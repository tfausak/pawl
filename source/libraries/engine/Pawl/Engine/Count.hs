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
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.ManaAbility as ManaAbility
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.MovedBetween as MovedBetween
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.SpellWasCast as SpellWasCast
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
  Scope.InZone (InZone.MkInZone zone ref) -> do
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
  -- CR 102.1: the players themselves. Candidates come from the same
  -- playersFor the zone arm indexes by, so CR 800.4a's departed seat is
  -- uncountable here without a second reading of who is in the game -- and
  -- unlike there, that IS observable: a departed player's zones were emptied,
  -- so naming them cost nothing, while naming the player costs one.
  --
  -- Each candidate is seen through Filter.playerView rather than through the
  -- injected ViewOf, which answers about OBJECTS (CR 109.1) and has no id to
  -- be asked with here. So the members are objectless, as InHistory's are, and
  -- an Aggregation.Greatest over this scope folds a per-OBJECT quantity against
  -- a player, which CR 208.1 and CR 202.3 give no answer to.
  --
  -- A per-PLAYER quantity DOES answer, and reaches the candidate through that
  -- same view: Filter.playerView records the player's identity, and
  -- Pawl.Types.PlayerRef.Candidate is what a card writes to read it -- Malignus'
  -- "the highest life total among your opponents". So nothing here carries the
  -- candidate beside the view: the view already names it.
  Scope.OverPlayers ref -> do
    pids <- playersFor context gs ref
    -- The predicate is baked PER CANDIDATE (see bakePerspective): CR 110.2's
    -- comparison is answered here, where the board is, and the match below is the
    -- same pure one every other scope makes.
    let kept = fmap ((,) Nothing) (Maybe.mapMaybe (\pid -> keep (bakePerspective viewOf context gs pid predicate) context (Just (Filter.playerView pid))) pids)
    aggregate quantityOf aggregation kept
  where
    predicate = Count.Type.filter count
    aggregation = Count.Type.aggregation count

-- The binding slots the per-member quantity of a count reads, with the reader
-- INJECTED for the module-cycle reason QuantityOf is. Only Aggregation.Greatest
-- carries a quantity; the other two aggregate the matched set alone, and
-- neither the Scope nor the Filter holds a slot name.
slots :: (quantity -> Set slot) -> Count.Type.Count quantity -> Set slot
slots slotsOfQuantity count = case Count.Type.aggregation count of
  Aggregation.Members -> Set.empty
  Aggregation.DistinctCardTypes -> Set.empty
  Aggregation.Greatest quantity -> slotsOfQuantity quantity

-- Does the per-member quantity of a count satisfy the predicate? slots' shape
-- with a Bool in place of a Set, and injected for the same module-cycle reason:
-- Pawl.Engine.Quantity.readsX is the one caller and this module cannot import
-- it. The two aggregations carrying no quantity answer False for the reason they
-- answer the empty set above -- there is nothing there to ask.
anyQuantity :: (quantity -> Bool) -> Count.Type.Count quantity -> Bool
anyQuantity predicate count = case Count.Type.aggregation count of
  Aggregation.Members -> False
  Aggregation.DistinctCardTypes -> False
  Aggregation.Greatest quantity -> predicate quantity

-- The count with its per-member quantity REWRITTEN, injected for slots' reason:
-- Pawl.Engine.Quantity.bakeBound is the one caller and this module cannot import
-- it. The two aggregations carrying no quantity are returned untouched, there
-- being nothing there to rewrite -- anyQuantity's False, one type over.
mapQuantity :: (quantity -> quantity) -> Count.Type.Count quantity -> Count.Type.Count quantity
mapQuantity f count =
  count
    { Count.Type.aggregation = case Count.Type.aggregation count of
        Aggregation.Members -> Aggregation.Members
        Aggregation.DistinctCardTypes -> Aggregation.DistinctCardTypes
        Aggregation.Greatest quantity -> Aggregation.Greatest (f quantity)
    }

-- CR 110.2 / 109.5: answer every perspective-reframing atom in a predicate
-- against ONE candidate player, rewriting each to a trivially true or trivially
-- false predicate so the match itself stays the pure fold Pawl.Engine.Filter
-- performs. That module cannot answer the atom: it holds no game state, and
-- "controls more lands than you" is a question about the board rather than about
-- the candidate. This is Filter.bakeBound's shape, with a board where that one has
-- a binding map.
--
-- The candidate is held as "you" for the INNER count only; the outer context rides
-- through unchanged, so a nested atom reading the perspective still reads the real
-- one. That is the asymmetry the whole atom exists for.
--
-- Exhaustive rather than a catch-all, bakeBound's posture: a later atom that must
-- be answered against the board has to fail to compile here rather than silently
-- go unbaked and answer False.
bakePerspective :: ViewOf -> Filter.Context -> GameState -> PlayerId -> Filter.Type.Filter Keyword.Type.Keyword -> Filter.Type.Filter Keyword.Type.Keyword
bakePerspective viewOf context gs candidate predicate = case predicate of
  -- STRICTLY more, and False when no perspective frames the match (CR 109.5) --
  -- the vacuous posture every player-referencing atom takes.
  Filter.Type.ControlsMoreThanYou inner ->
    let theirs = controlledMatching viewOf context gs inner candidate
        yours = fmap (controlledMatching viewOf context gs inner) (Filter.perspective context)
     in truth (maybe False (theirs >) yours)
  -- CR 108.4 / 608.2h: is this candidate the player who controls the object the
  -- slot names? Baked here for ControlsMoreThanYou's reason -- projecting a
  -- controller is a question about the board -- and off the same view the fold
  -- reads everything else through, which is what carries CR 608.2h in: the caller
  -- that has already moved the object supplies a last-known-aware view, and
  -- Pawl.Engine.Filter.View.controller then answers for a permanent that is gone.
  --
  -- False when the slot names no object or the view cannot describe it, the
  -- vacuous posture above: an unanswerable atom admits no candidate rather than
  -- admitting every one.
  Filter.Type.IsControllerOfBound slot ->
    truth (Just candidate == (Map.lookup slot (Filter.slotObjects context) >>= viewOf >>= Filter.controller))
  Filter.Type.And fs -> Filter.Type.And (fmap recur fs)
  Filter.Type.Or fs -> Filter.Type.Or (fmap recur fs)
  Filter.Type.Not f -> Filter.Type.Not (recur f)
  Filter.Type.HasCardType _ -> predicate
  Filter.Type.HasSupertype _ -> predicate
  Filter.Type.HasColor _ -> predicate
  Filter.Type.HasSubtype _ -> predicate
  Filter.Type.HasKeyword _ -> predicate
  Filter.Type.HasKeywordFamily _ -> predicate
  Filter.Type.PowerAtLeast _ -> predicate
  Filter.Type.PowerAtMost _ -> predicate
  Filter.Type.PowerLessThanSource -> predicate
  Filter.Type.PowerGreaterThanSource -> predicate
  Filter.Type.ManaValueAtMost _ -> predicate
  Filter.Type.ManaValueIsEven -> predicate
  Filter.Type.ControlledBy _ -> predicate
  Filter.Type.ControlledByDefendingPlayer -> predicate
  Filter.Type.ControlledByBound _ -> predicate
  Filter.Type.ControlledByPlayer _ -> predicate
  Filter.Type.ControlledByRecipient -> predicate
  Filter.Type.OwnedBy _ -> predicate
  Filter.Type.IsSource -> predicate
  Filter.Type.IsBound _ -> predicate
  Filter.Type.IsPlayer _ -> predicate
  Filter.Type.IsAttacking -> predicate
  Filter.Type.IsBlocking -> predicate
  Filter.Type.AttackedThisTurn -> predicate
  Filter.Type.IsAttachedToCreature -> predicate
  Filter.Type.IsAttachedToPermanent -> predicate
  Filter.Type.IsAttachedToSource -> predicate
  Filter.Type.CanHostSubject -> predicate
  Filter.Type.IsToken -> predicate
  Filter.Type.IsTapped -> predicate
  Filter.Type.IsRingBearer -> predicate
  Filter.Type.HasDesignation _ -> predicate
  Filter.Type.HasCounters _ -> predicate
  Filter.Type.HasNonManaActivatedAbility -> predicate
  where
    recur = bakePerspective viewOf context gs candidate

-- A baked answer as a Filter. `And []` is the trivial predicate by
-- Pawl.Types.Filter's own note, so its negation is the trivially false one --
-- there is no Always atom to reach for and deliberately so.
truth :: Bool -> Filter.Type.Filter keyword
truth b = if b then Filter.Type.And [] else Filter.Type.Not (Filter.Type.And [])

-- CR 110.2: how many permanents that player CONTROLS match the filter.
--
-- Control is read off the projected view rather than off Game.zoneMembers, which
-- slices the shared battlefield by OWNER (#161): rule 110.2 makes the two come
-- apart, and control is what the card asks about. The view is the injected one
-- every other candidate is seen through, so a land animated or taken at CR 613's
-- layers counts as the layers leave it, and an object the caller's projection
-- cannot describe counts for nobody.
--
-- The inner filter is matched under the UNCHANGED context, so what it says about
-- CR 109.5's "you" or about the source is still said about the real ones. The
-- candidate's own board is expressed by the controller test here rather than by a
-- ControlledBy conjunct, which could only ever name a relation.
controlledMatching :: ViewOf -> Filter.Context -> GameState -> Filter.Type.Filter Keyword.Type.Keyword -> PlayerId -> Integer
controlledMatching viewOf context gs inner pid =
  let matching oid = case viewOf oid of
        Nothing -> False
        Just view -> Filter.controller view == Just pid && Filter.matches context view inner
   in toInteger (length (Prelude.filter matching (Set.toList (GameState.battlefield gs))))

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
  Aggregation.Members -> Just (toInteger (length members))
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
-- Observable through Scope.OverPlayers, which folds the players this returns
-- rather than their objects, and Pawl.CountSpec's Tyranid Invasion group is
-- what proves it. Through Scope.InZone it still is not: CR 800.4a already
-- emptied every zone a departing player owned, so naming a departed seat there
-- folds nothing either way.
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
          -- One recipient or none: a count reads a slot that names one player,
          -- and Binding.onlyOne is how every such reader declines a slot that
          -- names several (CR 601.2c).
          recipient <- Binding.onlyOne =<< Map.lookup name (Binding.targetsOf (Object.bindings obj))
          case recipient of
            Recipient.ToPlayer pid -> Just [pid]
            Recipient.ToCreature _ -> Nothing
            Recipient.ToPlaneswalker _ -> Nothing
            Recipient.ToBattle _ -> Nothing
            Recipient.ToObject _ -> Nothing
        -- InSlot's baked half, and answered exactly as the arm above answers a
        -- slot that names one player: the seat, with no roster test. Per the CR
        -- 102.1 note above a departed player keeps their row, so this can name one
        -- -- and the answer is defined rather than absent. What a departed seat
        -- can still be TRUE of is the reader's question: CR 725.4 takes the crown
        -- off a player as they leave, so Quantity.IsMonarch reads 0 for one and
        -- Garland's duration ends.
        PlayerRef.Specific pid -> Just [pid]
        -- The fold's own candidate, which this function cannot answer: it holds
        -- no view, and the candidate is a fact about the member being read
        -- rather than about the board. Pawl.Engine.Quantity answers it where the
        -- view is, so what reaches here is a reference in a position that has no
        -- candidate at all -- a Scope naming it, or a ManaCount -- and Nothing is
        -- the honest answer for those.
        PlayerRef.Candidate -> Nothing
        -- The controller of a bound OBJECT, which this function cannot answer
        -- either: CR 613.1b's layer 2 decides who controls a permanent, and
        -- projecting that needs a view this function is handed none of.
        -- Pawl.Engine.Quantity answers it where the view is, exactly as it
        -- answers the candidate above; what reaches here is the reference in a
        -- position that holds no view at all -- a Scope naming it, or a ManaCount
        -- -- and those go unanswered (#1441).
        PlayerRef.ControllerOfBound _ -> Nothing

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
  GameEvent.Moved (Moved.MkMoved zc snapshot) -> case shape of
    EventShape.MovedBetween (MovedBetween.MkMovedBetween from to) ->
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
  GameEvent.DamagePrevented {} -> Nothing
  GameEvent.StepBegan {} -> Nothing
  -- CR 601.2i's cast, read from the snapshot the event took as the spell became
  -- cast rather than off the stack: by the time a look-back count folds the log
  -- that spell has resolved or been countered, so the live object is gone.
  -- TriggerCondition.SpellCast is the other reader and does read it live, which
  -- it can -- CR 601.2i's trigger is checked while the spell is still there.
  GameEvent.SpellCast (SpellWasCast.MkSpellWasCast caster _spell snapshot) -> case shape of
    -- CR 601.2a: "that player becomes its controller", so the caster the event
    -- recorded IS the view's controller and Filter.ControlledBy You answers "a
    -- spell you've cast". The spell's id is deliberately left out of the view
    -- for the reason the Moved arm leaves its own out: a look-back view is of a
    -- past event rather than of an object, and Filter.IsSource asking about one
    -- would be asking about an incarnation that no longer exists.
    -- CR 111.1 / 111.7: a token represents a PERMANENT and ceases to exist
    -- anywhere else, so nothing on the stack to be cast was ever one.
    EventShape.SpellCast -> Just (viewOfSnapshot (Just caster) False snapshot)
    EventShape.MovedBetween {} -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  -- CR 702.29c's cycling records no characteristics snapshot -- the Moved event
  -- the same discard emits is what carries one -- so there is nothing here for
  -- an EventShape to match against.
  GameEvent.Discarded {} -> Nothing
  GameEvent.Drew {} -> Nothing
  -- A reveal DOES carry a characteristics snapshot, as the two arms above do,
  -- and is still Nothing here: no EventShape names revealing. This becomes a
  -- real view the day one does (#162).
  GameEvent.Revealed {} -> Nothing
  -- The same reason, with no snapshot to offer either: no EventShape names an
  -- attacker being declared (CR 508.2b).
  GameEvent.AttackerDeclared {} -> Nothing
  GameEvent.BlockerDeclared {} -> Nothing
  GameEvent.BlocksDeclared {} -> Nothing
  GameEvent.AttackerBlocked {} -> Nothing
  -- A countering (CR 701.6a) does move the spell, but this event is not that
  -- move: Event.counter records a Moved event alongside this one, and matching
  -- both would count one countering twice. It carries no snapshot either.
  -- Becomes a real view the day an EventShape names countering (#162).
  GameEvent.SpellCountered _ -> Nothing
  GameEvent.HalfUnlocked {} -> Nothing
  GameEvent.TurnedFaceUp _ -> Nothing
  GameEvent.BecameDesignated {} -> Nothing
  GameEvent.Evolved _ -> Nothing
  GameEvent.Mentored {} -> Nothing
  GameEvent.PermanentSacrificed {} -> Nothing
  GameEvent.AbilityTriggered {} -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing
  GameEvent.LifeLost {} -> Nothing
  GameEvent.LifeGained {} -> Nothing
  -- CR 122.6's placement names an object by id and snapshots no characteristics,
  -- and no EventShape names it either.
  GameEvent.CountersPut {} -> Nothing
  GameEvent.CountersRemoved {} -> Nothing
  -- A control change names two players and one object by id, snapshots no
  -- characteristics, and no EventShape names it.
  GameEvent.ControlChanged {} -> Nothing
  GameEvent.VentureMarkerEntered {} -> Nothing
  -- CR 601.2c's targeting names two objects by id and snapshots no
  -- characteristics, so no EventShape names it either.
  GameEvent.BecameTarget {} -> Nothing

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
      Filter.toughness = PC.toughness snapshot,
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
      Filter.attachedTo = Nothing,
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
      Filter.ringBearerFor = Nothing,
      Filter.designations = Set.empty,
      Filter.kicked = False,
      -- CR 602.1 / 605.1a off the snapshot, which is what it reads for `keywords`
      -- and `power` too -- so this answers what the object HAD at the event.
      --
      -- Rule 702's own abilities are minted on top, exactly as
      -- Pawl.Engine.Projection.abilitiesFromCharacteristics mints them: a
      -- ProjectedCharacteristics stores the printed and granted list only, so
      -- reading the field bare would answer differently here than live for a
      -- Vehicle with crew or a land with reinforce. CR 702.178a's grant condition
      -- is not re-asked -- there is no board at the event to ask it against, and
      -- no snapshot-shaped reader in the pool asks this question at all.
      Filter.nonManaActivatedAbility =
        not
          ( all
              ManaAbility.isManaAbility
              ( PC.activatedAbilities snapshot
                  <> Keyword.battlefieldAbilitiesOf (PC.keywords snapshot)
                  <> Keyword.handAbilitiesOf (Map.keysSet (PC.keywords snapshot))
              )
          )
    }
