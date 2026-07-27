-- CR 613 / CR 608.2h: the one place a Pawl.Type.Count is interpreted. A pure
-- fold -- enumerate the scope, keep by the Filter, aggregate -- that never
-- learns which effect or card produced the count.
--
-- Parameterized by the view builder rather than importing Pawl.Projection:
-- Projection imports Pawl.Quantity, which imports this module, and
-- Quantity.evaluate is called from INSIDE the layer fold. The caller supplies
-- characteristics as of whatever layers it has already applied, which is what
-- lets a count read the projection without the module cycle or the recursion.
module Pawl.Count where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Binding as Binding
import qualified Pawl.Filter as Filter
import qualified Pawl.Game as Game
import qualified Pawl.Type.Aggregation as Aggregation
import qualified Pawl.Type.Count as Count.Type
import qualified Pawl.Type.EventShape as EventShape
import qualified Pawl.Type.Filter as Filter.Type
import qualified Pawl.Type.GameEvent as GameEvent
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.PlayerRef as PlayerRef
import qualified Pawl.Type.PlayerRelation as PlayerRelation
import qualified Pawl.Type.ProjectedCharacteristics as PC
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Scope as Scope
import qualified Pawl.Type.ZoneChange as ZoneChange

-- The characteristics of a candidate, as of the layers the CALLER has already
-- applied. Nothing when the candidate has no view -- an unknown id, or an object
-- the caller's bound projection cannot describe.
type ViewOf = ObjectId -> Maybe Filter.View

-- Nothing when the count cannot be determined -- an unresolvable PlayerRef. It
-- propagates; CR 208.2a's "use 0 instead of that number" is a different rule
-- and is not implemented here (#65).
evaluate :: ViewOf -> Filter.Context -> GameState -> Count.Type.Count -> Maybe Integer
evaluate viewOf context gs (Count.Type.MkCount scope predicate aggregation) = case scope of
  Scope.InZone zone ref -> do
    pids <- playersFor context gs ref
    let ids = concatMap (\pid -> Game.zoneMembers zone pid gs) pids
        kept = Maybe.mapMaybe (keep predicate context . viewOf) ids
    Just (aggregate aggregation kept)
  -- CR 608.2i: the event log. Views come from each event's stored snapshot
  -- (CR 608.2h last-known information), never from a live object -- a token has
  -- no printed card at all (CR 111.3) and an animated land died as a creature.
  Scope.InHistory shape ->
    let views = Maybe.mapMaybe (snapshotView shape) (Foldable.toList (GameState.events gs))
        kept = Maybe.mapMaybe (keep predicate context . Just) views
     in Just (aggregate aggregation kept)

keep :: Filter.Type.Filter -> Filter.Context -> Maybe Filter.View -> Maybe Filter.View
keep predicate context mv = case mv of
  Nothing -> Nothing
  Just v -> if Filter.matches context v predicate then Just v else Nothing

-- CR 208.2a: Tarmogoyf counts card TYPES, so DistinctCardTypes is the size of
-- the union, not the length of the list.
aggregate :: Aggregation.Aggregation -> [Filter.View] -> Integer
aggregate aggregation views = case aggregation of
  Aggregation.Objects -> toInteger (length views)
  Aggregation.DistinctCardTypes -> toInteger (Set.size (Set.unions (fmap Filter.cardTypes views)))

-- CR 400.1: whose copy of the zone. Nothing when the reference cannot be
-- resolved -- a Relative with no perspective, or a slot that is unbound or bound
-- to something that is not a player.
--
-- `everyone` is every player in the map, INCLUDING one who has left the game, and
-- that is masked rather than correct. It is unobservable: CR 800.4a takes every
-- object a departing player owns out of the game, and no site can mint a new one
-- owned by them afterwards, so Game.zoneMembers returns [] for every zone of
-- theirs and a departed player contributes nothing to any fold here.
--
-- It is not fixed because there is nothing to observe, so no test could prove a
-- filter right. The obstacle this comment used to name -- that the status
-- predicate lived in Pawl.Departure, which this module cannot import -- is gone:
-- it is Game.stillPlaying now, and Pawl.Game is already imported here. Adding
-- `filter (\pid -> List.elem pid (Game.stillPlaying gs)) everyone` is a one-line
-- change whenever a card makes the difference visible.
playersFor :: Filter.Context -> GameState -> PlayerRef.PlayerRef -> Maybe [PlayerId]
playersFor context gs ref =
  let everyone = Map.keys (GameState.players gs)
   in case ref of
        PlayerRef.EachPlayer -> Just everyone
        PlayerRef.Relative relation -> do
          you <- Filter.perspective context
          case relation of
            PlayerRelation.You -> Just [you]
            -- Every other player. Not a two-player shortcut: in a free-for-all
            -- the players compete as individuals and every other player is an
            -- opponent by construction (CR 806.1). CR 102.3 makes a TEAMMATE not
            -- an opponent, which is the only reading this is wrong for, and pawl
            -- has no teams (#175).
            PlayerRelation.Opponent -> Just (filter (/= you) everyone)
        PlayerRef.InSlot name -> do
          src <- Filter.source context
          obj <- Game.lookupObject src gs
          recipient <- Map.lookup name (Binding.targetsOf (Object.bindings obj))
          case recipient of
            Recipient.ToPlayer pid -> Just [pid]
            Recipient.ToCreature _ -> Nothing
            Recipient.ToObject _ -> Nothing

-- CR 608.2h: the view of a past event, built from the snapshot the event
-- recorded rather than from any object that may no longer exist. identity is
-- Nothing (the object is gone, so IsSource cannot match) and controller is
-- Nothing (the snapshot does not record one).
snapshotView :: EventShape.EventShape -> GameEvent.GameEvent -> Maybe Filter.View
snapshotView shape event = case event of
  GameEvent.Moved zc snapshot -> case shape of
    EventShape.MovedBetween from to ->
      if ZoneChange.from zc == from && ZoneChange.to zc == to
        then
          Just
            Filter.MkView
              { Filter.cardTypes = PC.cardTypes snapshot,
                Filter.supertypes = Set.empty,
                Filter.colors = PC.colors snapshot,
                Filter.subtypes = PC.subtypes snapshot,
                Filter.power = PC.power snapshot,
                Filter.controller = Nothing,
                Filter.identity = Nothing,
                Filter.playerIdentity = Nothing,
                -- The snapshot records characteristics only, and combat status is
                -- not one (CR 109.3) -- so IsAttacking is vacuously False over a
                -- past event, the posture controller already takes here.
                Filter.attacking = False
              }
        else Nothing
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.StepBegan _ _ -> Nothing
  GameEvent.SpellCast _ -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
