-- CR 613 / CR 608.2h: the one place a Pawl.Types.Count is interpreted. A pure
-- fold -- enumerate the scope, keep by the Filter, aggregate -- that never
-- learns which effect or card produced the count.
--
-- Parameterized by the view builder AND by the per-object quantity reader,
-- rather than importing Pawl.Projection or Pawl.Quantity: Projection imports
-- Pawl.Quantity, which imports this module, and Quantity.evaluate is called from
-- INSIDE the layer fold. The caller supplies characteristics as of whatever
-- layers it has already applied, which is what lets a count read the projection
-- without the module cycle or the recursion.
module Pawl.Count where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Binding as Binding
import qualified Pawl.Filter as Filter
import qualified Pawl.Game as Game
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.ZoneChange as ZoneChange

-- The characteristics of a candidate, as of the layers the CALLER has already
-- applied. Nothing when the candidate has no view -- an unknown id, or an object
-- the caller's bound projection cannot describe.
type ViewOf = ObjectId -> Maybe Filter.View

-- Reads a per-object quantity off one candidate. INJECTED for exactly the
-- module-cycle reason ViewOf is: Pawl.Quantity imports this module, so nothing
-- here can call Pawl.Quantity.evaluate. Pawl.Quantity ties the knot at its own
-- Count arm, and it is the only module that does -- Pawl.Condition reaches this
-- fold only THROUGH Pawl.Quantity, since both sides of a Condition are
-- Quantities. Every aggregation but Aggregation.Greatest ignores it.
type QuantityOf quantity = ObjectId -> quantity -> Maybe Integer

-- Nothing when the count cannot be determined -- an unresolvable PlayerRef, or
-- (Aggregation.Greatest only) a maximum over a set that is empty or holds a
-- member with no value. It propagates; CR 208.2a's "use 0 instead of that
-- number" is a different rule and is not implemented here (#65).
evaluate :: ViewOf -> QuantityOf quantity -> Filter.Context -> GameState -> Count.Type.Count quantity -> Maybe Integer
evaluate viewOf quantityOf context gs (Count.Type.MkCount scope predicate aggregation) = case scope of
  Scope.InZone zone ref -> do
    pids <- playersFor context gs ref
    let ids = concatMap (\pid -> Game.zoneMembers zone pid gs) pids
        kept = Maybe.mapMaybe (\oid -> fmap ((,) (Just oid)) (keep predicate context (viewOf oid))) ids
    aggregate quantityOf aggregation kept
  -- CR 608.2i: the event log. Views come from each event's stored snapshot
  -- (CR 608.2h last-known information), never from a live object -- a token has
  -- no printed card at all (CR 111.3) and an animated land died as a creature.
  Scope.InHistory shape ->
    let views = Maybe.mapMaybe (snapshotView shape) (Foldable.toList (GameState.events gs))
        kept = fmap ((,) Nothing) (Maybe.mapMaybe (keep predicate context . Just) views)
     in aggregate quantityOf aggregation kept

keep :: Filter.Type.Filter -> Filter.Context -> Maybe Filter.View -> Maybe Filter.View
keep predicate context mv = case mv of
  Nothing -> Nothing
  Just v -> if Filter.matches context v predicate then Just v else Nothing

-- CR 208.2a: Tarmogoyf counts card TYPES, so DistinctCardTypes is the size of
-- the union, not the length of the list.
--
-- Each member carries the object it came from when there is one. An InHistory
-- member has none: its view is a CR 608.2h snapshot of a past event rather than
-- of anything on the battlefield now, so there is no object to read a per-object
-- quantity against and Greatest over that scope is undeterminable (#299).
aggregate :: QuantityOf quantity -> Aggregation.Aggregation quantity -> [(Maybe ObjectId, Filter.View)] -> Maybe Integer
aggregate quantityOf aggregation members = case aggregation of
  Aggregation.Objects -> Just (toInteger (length members))
  Aggregation.DistinctCardTypes -> Just (toInteger (Set.size (Set.unions (fmap (Filter.cardTypes . snd) members))))
  -- Total in both directions. A member whose quantity cannot be determined makes
  -- the whole maximum undeterminable rather than being dropped, which would
  -- report the maximum of a set the card never named; and an EMPTY matched set
  -- has no maximum at all. Nothing, NOT 0: no rule gives a maximum over nothing
  -- a value, CR 208.2a's "use 0 instead of that number" is scoped to a
  -- characteristic-defining ability and unimplemented here anyway (#65), and
  -- where the CR wants an empty maximum to be 0 it legislates it case by case
  -- (CR 714.2d, a Saga with no chapter abilities).
  Aggregation.Greatest quantity -> do
    values <- traverse (\(identity, _) -> identity >>= \oid -> quantityOf oid quantity) members
    case values of
      [] -> Nothing
      value : rest -> Just (Foldable.foldl' max value rest)

-- CR 400.1: whose copy of the zone. Nothing when the reference cannot be
-- resolved -- a Relative with no perspective, or a slot that is unbound or bound
-- to something that is not a player.
--
-- CR 102.1: "A player is one of the people in the game." A player who has left
-- keeps their row in GameState.players -- Player.status turns Departed, the key
-- stays -- so `everyone` is Game.stillPlaying rather than the map's keys, and
-- neither EachPlayer nor Opponent names a departed seat.
--
-- Unobservable HERE, unlike at Resolve.playerRefPlayers, and written anyway:
-- CR 800.4a took every object a departing player owned out of the game and no
-- site can mint a new one owned by them, so Game.zoneMembers already answered []
-- for each of their zones and they contributed nothing to any fold. The two
-- readings of a PlayerRef must not disagree about who a PlayerRef names, and the
-- first Scope that folds over PLAYERS rather than over their objects would
-- observe the difference immediately.
playersFor :: Filter.Context -> GameState -> PlayerRef.PlayerRef -> Maybe [PlayerId]
playersFor context gs ref =
  let everyone = Game.stillPlaying gs
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
                Filter.attacking = False,
                -- Nor is blocking one (CR 509.1g is combat status too), so
                -- IsBlocking is vacuously False here as well.
                Filter.blocking = False,
                -- Nor does the snapshot record what the object DID: it holds the
                -- characteristics the object last had (CR 608.2h), not the
                -- turn's event log, and this view is built from one event rather
                -- than from the game -- so AttackedThisTurn is vacuously False
                -- here, the posture attacking takes.
                Filter.attackedThisTurn = False,
                -- Nor is what a permanent was attached to (CR 109.3 names it
                -- explicitly), and the snapshot records no attachment either --
                -- so IsAttachedToCreature is vacuously False here too.
                Filter.attachedToCreature = False,
                -- Nor is what a permanent is represented by (CR 111.6: a token
                -- "isn't a card"), and the snapshot records characteristics only
                -- -- so IsToken is vacuously False over a past event, the third
                -- arm to take that posture here.
                Filter.token = False
              }
        else Nothing
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.StepBegan _ _ -> Nothing
  GameEvent.SpellCast _ -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  -- CR 702.29c's cycling records no characteristics snapshot -- the Moved event
  -- the same discard emits is what carries one -- so there is nothing here for an
  -- EventShape to match against.
  GameEvent.Discarded {} -> Nothing
  -- A reveal DOES carry a characteristics snapshot, unlike the two above, and is
  -- still Nothing here: the only EventShape is a shape of ZONE CHANGE, and CR
  -- 701.20b says a reveal is not one ("revealing a card doesn't cause it to
  -- leave the zone it's in"). The arm becomes a real view the day an EventShape
  -- names revealing (#162).
  GameEvent.Revealed _ _ -> Nothing
  -- The same reason as the reveal: an attacker being declared (CR 508.2b) is not
  -- a zone change, and every EventShape is a shape of one.
  GameEvent.AttackerDeclared _ -> Nothing
