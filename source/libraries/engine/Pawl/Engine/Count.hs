-- CR 613 / CR 608.2h: the one place a Pawl.Types.Count is interpreted. A pure
-- fold -- enumerate the scope, keep by the Filter, aggregate -- that never
-- learns which effect or card produced the count.
--
-- Parameterized by the view builder AND by the per-member quantity reader
-- rather than importing Pawl.Engine.Projection or Pawl.Engine.Quantity, both of
-- which sit above this module and call into the layer fold. The caller supplies
-- characteristics as of whatever layers it has already applied, which is what
-- lets a count read the projection without the module cycle or the recursion.
module Pawl.Engine.Count where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LastKnown as LastKnown
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
-- applied. Nothing when the candidate has no view -- an unknown id, or an
-- object the caller's bound projection cannot describe.
type ViewOf = ObjectId -> Maybe Filter.View

-- Reads a per-member quantity off one candidate. INJECTED for the same
-- module-cycle reason ViewOf is: Pawl.Engine.Quantity imports this module and
-- ties the knot at its own Count arm. Every aggregation but
-- Aggregation.Greatest ignores it.
--
-- BOTH the candidate's object and its view, because an InHistory candidate has
-- only the second: its view is a CR 608.2h snapshot of a past event, so a
-- reader demanding an object could never answer one. For an InZone
-- candidate the view IS `viewOf` of the id beside it, so the two agree.
type QuantityOf quantity = Maybe ObjectId -> Filter.View -> quantity -> Maybe Integer

-- Nothing when the count cannot be determined -- an unresolvable PlayerRef, or
-- (Aggregation.Greatest only) a maximum over a set that is empty or holds a
-- member with no value. It propagates, which is what every caller but one
-- wants: CR 208.2a's substituted 0 is a different rule, scoped to a
-- characteristic-defining ability and applied by
-- Pawl.Engine.Quantity.determine.
evaluate :: ViewOf -> QuantityOf quantity -> Filter.Context -> GameState -> Count.Type.Count quantity -> Maybe Integer
evaluate viewOf quantityOf context gs count = case Count.Type.scope count of
  Scope.InZone zone ref -> do
    pids <- playersFor context gs ref
    let ids = concatMap (\pid -> Game.zoneMembers zone pid gs) pids
        kept = Maybe.mapMaybe (\oid -> fmap ((,) (Just oid)) (keep predicate context (viewOf oid))) ids
    aggregate quantityOf aggregation kept
  -- CR 608.2i: the event log. Views come from each event's stored snapshot
  -- (CR 608.2h last-known information), never from a live object -- a token has
  -- no printed card at all (CR 111.3) and an animated land died as a creature.
  Scope.InHistory shape ->
    let views = Maybe.mapMaybe (snapshotView gs shape . snd) (Foldable.toList (GameState.events gs))
        kept = fmap ((,) Nothing) (Maybe.mapMaybe (keep predicate context . Just) views)
     in aggregate quantityOf aggregation kept
  where
    predicate = Count.Type.filter count
    aggregation = Count.Type.aggregation count

-- The binding slots the per-member quantity of a count reads, with the reader
-- INJECTED for the module-cycle reason QuantityOf is. Only Aggregation.Greatest
-- carries a quantity; the other two aggregate the matched set alone, and
-- neither the Scope nor the Filter holds a slot name.
slots :: (quantity -> Set slot) -> Count.Type.Count quantity -> Set slot
slots slotsOfQuantity count = case Count.Type.aggregation count of
  Aggregation.Objects -> Set.empty
  Aggregation.DistinctCardTypes -> Set.empty
  Aggregation.Greatest quantity -> slotsOfQuantity quantity

-- Does the per-member quantity of a count satisfy the predicate? slots' shape
-- with a Bool in place of a Set, and injected for the same module-cycle reason:
-- Pawl.Engine.Quantity.readsX is the one caller and this module cannot import
-- it. The two aggregations carrying no quantity answer False for the reason they
-- answer the empty set above -- there is nothing there to ask.
anyQuantity :: (quantity -> Bool) -> Count.Type.Count quantity -> Bool
anyQuantity predicate count = case Count.Type.aggregation count of
  Aggregation.Objects -> False
  Aggregation.DistinctCardTypes -> False
  Aggregation.Greatest quantity -> predicate quantity

keep :: Filter.Type.Filter Keyword.Type.Keyword -> Filter.Context -> Maybe Filter.View -> Maybe Filter.View
keep predicate context mv = case mv of
  Nothing -> Nothing
  Just v -> if Filter.matches context v predicate then Just v else Nothing

-- CR 208.2a: Tarmogoyf counts card TYPES, so DistinctCardTypes is the size of
-- the union, not the length of the list.
--
-- Each member carries the object it came from when there is one, and its view
-- either way. An InHistory member has no object -- its view is a CR 608.2h
-- snapshot of a past event rather than of anything on the battlefield now --
-- so Greatest hands the reader both and lets it answer from whichever it can.
aggregate :: QuantityOf quantity -> Aggregation.Aggregation quantity -> [(Maybe ObjectId, Filter.View)] -> Maybe Integer
aggregate quantityOf aggregation members = case aggregation of
  Aggregation.Objects -> Just (toInteger (length members))
  Aggregation.DistinctCardTypes -> Just (toInteger (Set.size (Set.unions (fmap (Filter.cardTypes . snd) members))))
  -- Total in both directions. A member whose quantity cannot be determined
  -- makes the whole maximum undeterminable rather than being dropped, which
  -- would report the maximum of a set the card never named; and an EMPTY
  -- matched set has no maximum. Nothing, NOT 0: no rule gives a maximum over
  -- nothing a value, and where the CR wants an empty maximum to be 0 it
  -- legislates it case by case (CR 714.2d). CR 208.2a is one such case, applied
  -- where it is scoped -- at the characteristic-defining ability that consumes
  -- this count, never here.
  -- Pawl.CountSpec's Rootha, Mastering the Moment group is what proves the
  -- objectless member really is read off its snapshot.
  Aggregation.Greatest quantity -> do
    values <- traverse (\(identity, view) -> quantityOf identity view quantity) members
    case values of
      [] -> Nothing
      value : rest -> Just (Foldable.foldl' max value rest)

-- CR 400.1: whose copy of the zone -- and, for Pawl.Engine.ManaCount, whose
-- mana pool, which CR 106.4 attaches to a player the same way. Nothing when the
-- reference cannot be resolved: a Relative with no perspective, or a slot that
-- is unbound or bound to something that is not a player.
--
-- CR 102.1: a departed player keeps their row in GameState.players (only
-- Player.status changes), so `everyone` is Game.stillPlaying rather than the
-- map's keys, and neither EachPlayer nor Opponent names a departed seat.
--
-- Unobservable HERE, unlike at Resolve.playerRefPlayers, and written anyway: CR
-- 800.4a already emptied every zone a departing player owned, so they
-- contributed nothing to any fold. The two readings of a PlayerRef must not
-- disagree, and the first Scope folding over PLAYERS rather than their objects
-- would observe it.
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
            -- every other player is an opponent by construction (CR 806.1).
            -- Only CR 102.3's teammates would break that, and pawl has no teams
            -- (#175).
            PlayerRelation.Opponent -> Just (filter (/= you) everyone)
        PlayerRef.InSlot name -> do
          src <- Filter.source context
          obj <- Game.lookupObject src gs
          recipient <- Map.lookup name (Binding.targetsOf (Object.bindings obj))
          case recipient of
            Recipient.ToPlayer pid -> Just [pid]
            Recipient.ToCreature _ -> Nothing
            Recipient.ToPlaneswalker _ -> Nothing
            Recipient.ToBattle _ -> Nothing
            Recipient.ToObject _ -> Nothing

-- CR 608.2h: the view of a past event, built from the snapshot the event
-- recorded rather than from any object that may no longer exist.
--
-- The snapshot fills the characteristic fields it records (see viewOfSnapshot
-- below): card types, colours, subtypes, keywords (CR 109.3 counts abilities
-- among an object's characteristics), power and mana value. Everything that is
-- not a characteristic is vacuously empty over a past event -- identity and
-- playerIdentity are Nothing, and combat status, attachment, tap status and
-- what the object did this turn are all False.
--
-- `controller` and `token` are the exceptions, and neither is a characteristic
-- (CR 109.3 / CR 111.6), so neither can ride the snapshot. Each arm answers
-- them for itself: CR 601.2a makes the player who cast a spell its controller,
-- and a move reads CR 608.2h's record filed under the id it left behind.
--
-- `supertypes` is the odd one out: it IS a characteristic and
-- ProjectedCharacteristics records it, but this view leaves it empty, so a
-- supertype filter over a past event answers False (#646).
snapshotView :: GameState -> EventShape.EventShape -> GameEvent.GameEvent -> Maybe Filter.View
snapshotView gs shape event = case event of
  GameEvent.Moved zc snapshot -> case shape of
    EventShape.MovedBetween from to ->
      if ZoneChange.from zc == from && ZoneChange.to zc == to
        then -- CR 608.2h: who controlled it and what KIND of object it was, read
        -- from the record the move funnel filed under the DEPARTED id as the
        -- object ceased -- the same pre-move state `snapshot` was taken
        -- against, and the route Event.departedFrom already takes back from a
        -- Moved event (CR 400.7 makes that id name nothing else, ever).
        --
        -- No record only where nothing departed: Event.recordTokenEntry's
        -- battlefield-to-battlefield pseudo-move for a new token, whose object
        -- is therefore still live and can be asked directly.
          let lastKnown = Map.lookup (ZoneChange.departed zc) (GameState.lastKnown gs)
           in Just
                ( viewOfSnapshot
                    (fmap LastKnown.controller lastKnown)
                    (maybe (Game.isToken (ZoneChange.object zc) gs) (Game.sourceIsToken . LastKnown.source) lastKnown)
                    snapshot
                )
        else Nothing
    EventShape.SpellCast -> Nothing
  GameEvent.DamageDealt _ -> Nothing
  -- CR 615.13's record names a recipient and an amount and snapshots no
  -- characteristics, so there is nothing for a Filter to look at.
  GameEvent.DamagePrevented _ _ -> Nothing
  GameEvent.StepBegan _ _ -> Nothing
  -- CR 601.2i's cast, read from the snapshot the event took as the spell became
  -- cast rather than off the stack: by the time a look-back count folds the log
  -- that spell has resolved or been countered, so the live object is gone.
  -- TriggerCondition.SpellCast is the other reader and does read it live, which
  -- it can -- CR 601.2i's trigger is checked while the spell is still there.
  GameEvent.SpellCast caster _spell snapshot -> case shape of
    -- CR 601.2a: "that player becomes its controller", so the caster the event
    -- recorded IS the view's controller and Filter.ControlledBy You answers "a
    -- spell you've cast". The spell's id is deliberately left out of the view
    -- for the reason the Moved arm leaves its own out: a look-back view is of a
    -- past event rather than of an object, and Filter.IsSource asking about one
    -- would be asking about an incarnation that no longer exists.
    -- CR 111.1 / 111.7: a token represents a PERMANENT and ceases to exist
    -- anywhere else, so nothing on the stack to be cast was ever one.
    EventShape.SpellCast -> Just (viewOfSnapshot (Just caster) False snapshot)
    EventShape.MovedBetween _ _ -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  -- CR 702.29c's cycling records no characteristics snapshot -- the Moved event
  -- the same discard emits is what carries one -- so there is nothing here for
  -- an EventShape to match against.
  GameEvent.Discarded {} -> Nothing
  -- A reveal DOES carry a characteristics snapshot, as the two arms above do,
  -- and is still Nothing here: no EventShape names revealing. This becomes a
  -- real view the day one does (#162).
  GameEvent.Revealed _ _ -> Nothing
  -- The same reason, with no snapshot to offer either: no EventShape names an
  -- attacker being declared (CR 508.2b).
  GameEvent.AttackerDeclared _ _ -> Nothing
  GameEvent.BlockerDeclared _ _ -> Nothing
  -- A countering (CR 701.6a) does move the spell, but this event is not that
  -- move: Event.counter records a Moved event alongside this one, and matching
  -- both would count one countering twice. It carries no snapshot either.
  -- Becomes a real view the day an EventShape names countering (#162).
  GameEvent.SpellCountered _ -> Nothing
  GameEvent.HalfUnlocked {} -> Nothing
  GameEvent.TurnedFaceUp _ -> Nothing
  GameEvent.PermanentSacrificed {} -> Nothing
  GameEvent.AbilityTriggered {} -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing
  GameEvent.LifeLost _ _ -> Nothing
  GameEvent.LifeGained _ _ -> Nothing
  -- CR 122.6's placement names an object by id and snapshots no characteristics,
  -- and no EventShape names it either.
  GameEvent.CountersPut {} -> Nothing
  GameEvent.CountersRemoved {} -> Nothing

-- The Filter.View a recorded snapshot yields, shared by every arm of
-- snapshotView above so that two shapes of event cannot disagree about what a
-- snapshot says. The `controller` and the tokenhood flag are the arm's to
-- supply, since they are the two fields no ProjectedCharacteristics carries
-- (CR 109.3 / CR 111.6) and the events differ on where each is recoverable
-- from.
viewOfSnapshot :: Maybe PlayerId -> Bool -> PC.ProjectedCharacteristics -> Filter.View
viewOfSnapshot mController isToken snapshot =
  Filter.MkView
    { Filter.cardTypes = PC.cardTypes snapshot,
      Filter.supertypes = Set.empty,
      Filter.colors = PC.colors snapshot,
      Filter.subtypes = PC.subtypes snapshot,
      Filter.keywords = Map.keysSet (PC.keywords snapshot),
      Filter.power = PC.power snapshot,
      -- CR 202.3 off the snapshot, which carries the number: a
      -- ProjectedCharacteristics records a mana value, so this reads what the
      -- object's was AT THE EVENT rather than throwing the question away.
      --
      -- Nothing means that object had no card behind it, exactly as it does
      -- live. Not implemented there: CR 202.3a's 0 for an ability on the stack
      -- (#674).
      Filter.manaValue = PC.manaValue snapshot,
      Filter.controller = mController,
      -- CR 108.3: an owner is read off an OBJECT, and a ProjectedCharacteristics
      -- carries none -- the position `supertypes` and `counters` below are in
      -- (#646). Unlike those two the fact does not change, and the Moved arm's
      -- CR 608.2h record would be the place to keep it, but no field of it holds
      -- one (#1069).
      Filter.owner = Nothing,
      Filter.identity = Nothing,
      Filter.playerIdentity = Nothing,
      Filter.attacking = False,
      Filter.blocking = False,
      Filter.attackedThisTurn = False,
      Filter.attachedToCreature = False,
      Filter.attachedToPermanent = False,
      Filter.canHostSubject = False,
      -- CR 111.6: "A token isn't a card", which is a fact about the OBJECT and
      -- not a characteristic, so the arm supplies it above.
      Filter.token = isToken,
      Filter.tapped = False,
      -- CR 122.1: a ProjectedCharacteristics records no counters -- CR 613.4c
      -- has already folded them into the power and toughness above -- so a past
      -- event carries none to read, the position `supertypes` is in (#646). The
      -- Moved arm's CR 608.2h record does carry them, and is not read for them:
      -- no card in the pool asks an event snapshot for a counter tally, so a
      -- quantity that does is still answered 0 (#993).
      Filter.counters = Map.empty,
      -- CR 701.54b: a designation, which a ProjectedCharacteristics does not carry
      -- and never could -- CR 109.3's characteristic list has no room for one. So a
      -- past event records none, the position `supertypes` and `counters` are in
      -- (#646). Nothing rather than a remembered player: an event snapshot is not
      -- an object, and "is your Ring-bearer" is a question about a permanent on the
      -- battlefield now (CR 701.54e), not about one at the moment of an event.
      Filter.ringBearerFor = Nothing
    }
