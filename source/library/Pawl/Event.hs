-- The event pipeline (CR 603/614). This module owns the single zone-change
-- funnel and the sole casing on TriggerCondition; casing on ReplacementEffect
-- lives in Pawl.Replacement (CR 616.1's loop), which this module's changeZone
-- calls through rather than cases on directly.
-- changeZone lives here (not in Pawl.Game) so it can read the projection --
-- Projection imports Game, so a Game.changeZone that read the projection would
-- be an import cycle. See the plan's module dependency note.
module Pawl.Event where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Replacement as Replacement
import Pawl.Type.Card (Card)
import qualified Pawl.Type.CounterKind as CounterKind
import Pawl.Type.DamageEvent (DamageEvent)
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.DamageKind as DamageKind
import Pawl.Type.DelayedTrigger (DelayedTrigger)
import qualified Pawl.Type.DelayedTrigger as DelayedTrigger
import Pawl.Type.Game (Game)
import Pawl.Type.GameEvent (GameEvent)
import qualified Pawl.Type.GameEvent as GameEvent
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PendingTrigger (PendingTrigger)
import qualified Pawl.Type.PendingTrigger as PendingTrigger
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.ProjectedCharacteristics as PC
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.Source as Source
import Pawl.Type.StateCondition (StateCondition)
import qualified Pawl.Type.StateCondition as StateCondition
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Timestamp as Timestamp
import Pawl.Type.TriggerCondition (TriggerCondition)
import qualified Pawl.Type.TriggerCondition as TriggerCondition
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
import qualified Pawl.Type.TurnScope as TurnScope
import Pawl.Type.Zone (Zone)
import qualified Pawl.Type.Zone as Zone
import Pawl.Type.ZoneChange (ZoneChange)
import qualified Pawl.Type.ZoneChange as ZoneChange

-- CR 608.2i: append one entry to the turn-scoped log. The single APPEND point --
-- Engine.handoffTurn clears the log at turn end, Setup.emptyGame and the test
-- fixture Support.oneMountainState both seed it empty, and the test helper
-- Support.withEvent sets it directly; none of those append.
recordEvent :: GameEvent -> GameState -> GameState
recordEvent event gs = gs {GameState.events = GameState.events gs Seq.|> event}

-- The zone change an event describes, if it is one.
movedOf :: GameEvent -> Maybe ZoneChange
movedOf event = case event of
  GameEvent.Moved zc _ -> Just zc
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.StepBegan _ _ -> Nothing
  GameEvent.SpellCast _ -> Nothing
  GameEvent.BecameMonarch _ -> Nothing

-- The damage an event describes, if it is any.
damageOf :: GameEvent -> Maybe DamageEvent
damageOf event = case event of
  GameEvent.DamageDealt ev -> Just ev
  GameEvent.Moved _ _ -> Nothing
  GameEvent.StepBegan _ _ -> Nothing
  GameEvent.SpellCast _ -> Nothing
  GameEvent.BecameMonarch _ -> Nothing

-- The caster an event describes, if it is a cast (CR 601.2i).
castOf :: GameEvent -> Maybe PlayerId
castOf event = case event of
  GameEvent.SpellCast pid -> Just pid
  GameEvent.Moved _ _ -> Nothing
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.StepBegan _ _ -> Nothing
  GameEvent.BecameMonarch _ -> Nothing

-- CR 117.5: the events the trigger scan has not yet consumed.
unscannedEvents :: GameState -> [GameEvent]
unscannedEvents gs =
  Foldable.toList (Seq.drop (fromIntegral (GameState.scannedThrough gs)) (GameState.events gs))

-- CR 704.5h: the damage the state-based-action check has not yet consumed.
unscannedDamage :: GameState -> [DamageEvent]
unscannedDamage gs =
  Maybe.mapMaybe damageOf (Foldable.toList (Seq.drop (fromIntegral (GameState.damageScannedThrough gs)) (GameState.events gs)))

-- Insert a freshly-built object into `dest` under a new id and timestamp, and
-- return that id. The common tail of changeZone (a moved incarnation) and
-- createTokens (a token from nothing). `mkObj` receives the fresh timestamp so the
-- object records when it entered (CR 613.7d). The Moved event is emitted by the
-- CALLER: only it knows which state the CR 608.2h snapshot must be taken against.
placeObject :: PlayerId -> (Timestamp.Timestamp -> Object.Object) -> Zone -> Game ObjectId
placeObject pid mkObj dest = do
  gs <- State.get
  let (newId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj = mkObj ts
      gs3 = gs2 {GameState.objects = Map.insert newId obj (GameState.objects gs2)}
  State.put (Game.insertIntoZone dest pid newId gs3)
  pure newId

-- The single zone-change primitive (CR 400.7): the source object ceases; a NEW
-- object with a fresh id is created in the destination, carrying owner and
-- source forward and resetting per-incarnation state. No-op if the id is unknown.
changeZone :: ObjectId -> Zone -> Game ()
changeZone oid requestedDest = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just obj -> do
      let pid = Object.owner obj
          fromZone = Object.zone obj
          -- CR 608.2h: last known information -- the object as it exists in the
          -- zone it is LEAVING, projected against the PRE-MOVE state -- one board
          -- projection per zone change, forced eagerly (GameEvent.Moved's
          -- snapshot field is strict) rather than left as a thunk retaining the
          -- whole pre-move GameState for a turn. Measured on the tasty-bench
          -- suite, pre-log baseline (3cc3ecd) vs. this log with the strict field
          -- (goldfish /casting/fighting, 2p): 10.1/9.30/9.31 ms -> 10.6/9.73/9.72
          -- ms -- a ~4-5% move, within the benchmark's own run-to-run noise
          -- (~800 us stddev on a ~10 ms mean), not the large regression a
          -- captured pre-move GameState would cause. That is the price of an
          -- honest history (a token has no printed card to re-derive from,
          -- CR 111.1).
          snapshot = Projection.project oid gs
      -- CR 614.4: replacements exist before the event, so the loop reads them from
      -- the PRE-MOVE state. CR 614.6: the modified event is what actually happens.
      --
      -- `obj` and `snapshot` are both read from `gs`, BEFORE resolveZoneChange
      -- runs, and both are still used below, AFTER it returns -- only
      -- `ZoneChange.to settled` is read back from the settled event. This is
      -- sound despite Pawl.Replacement's AsCopy arm calling State.modify' (it
      -- stamps a copy snapshot when a permanent enters as a copy): the loop
      -- resolveZoneChange runs here is a WouldChangeZone loop, which `applies`
      -- restricts to ZoneChangeR candidates -- it cannot reach the EntryR arm
      -- that AsCopy lives under, because EntryR only matches a WouldEnter event
      -- (the entry loop nested inside changeZone below, at
      -- Replacement.runEntry, on the object's NEW id). And `gs` is an immutable
      -- value, not a reference into mutable state, so a State.modify' anywhere
      -- downstream cannot retroactively change what `snapshot` already
      -- captured. A future task extending EITHER loop to mutate state that
      -- `obj` or `snapshot` reads (e.g. a ZoneChangeR arm that edits the
      -- object's own fields) would need to re-derive these bindings from the
      -- state AFTER that loop, not carry the pre-loop ones across unexamined.
      resolved <- Replacement.resolveZoneChange (ZoneChange.MkZoneChange oid fromZone requestedDest)
      case resolved of
        -- CR 614.6: nothing survived the loop, so no zone change happens. No
        -- producer today -- no card in the pool cancels a zone change outright --
        -- but Maybe is what "the event does not happen" means on this path.
        Nothing -> pure ()
        Just settled -> do
          let dest = ZoneChange.to settled
              mkObj ts = obj {Object.zone = dest, Object.tapped = TapState.Untapped, Object.damage = 0, Object.sickness = Sickness.Sick, Object.bindings = Map.empty, Object.counters = Map.empty, Object.timestamp = ts}
          State.modify' $ \g ->
            let g1 = Game.removeFromZones pid oid g
             in g1 {GameState.objects = Map.delete oid (GameState.objects g1)}
          newId <- placeObject pid mkObj dest
          -- CR 614.1c-d: entry replacements apply to BATTLEFIELD entries and
          -- nowhere else. CR 616.1g: this loop is NESTED inside the zone change,
          -- which is how "an effect may apply to an event contained within another
          -- event" is expressed -- as call nesting, not as a field. A lone entry
          -- has no same-batch siblings (CR 614.12a; see Pawl.Replacement's
          -- applyReplacementsIn for why 614.12a, not 614.13a, is the cite).
          Monad.when (dest == Zone.Battlefield) (Replacement.runEntry Set.empty newId)
          -- CR 603.2g: record the RESOLVED event, carrying the NEW object's id --
          -- what an enters trigger scans. Recorded LAST, so the entry loop's
          -- choices are locked in before any trigger or SBA can observe the object.
          State.modify' (recordEvent (GameEvent.Moved (ZoneChange.MkZoneChange newId fromZone dest) snapshot))

-- The single destruction funnel (CR 701.8 / 702.12b): every destruction -- the
-- Destroy opcode and the CR 704.5g/h state-based actions -- flows through here.
--
-- CR 702.12b: an indestructible permanent can't be destroyed, and that gate
-- comes BEFORE the replacement loop, which is CR 614.7: "If a replacement
-- effect would replace an event, but that event never happens, the
-- replacement effect simply doesn't do anything" -- a regeneration shield is
-- neither applied nor consumed. Otherwise the would-be-destroyed event is
-- offered to CR 616.1; if it survives, the permanent is put into its owner's
-- graveyard via changeZone (so Rest in Peace's redirect and a token's CR
-- 704.5d cease-to-exist still compose). Ungated for CR 701.19c "can't be
-- regenerated" (#42).
destroy :: ObjectId -> Game ()
destroy oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just _ ->
      if Projection.hasKeyword Keyword.Indestructible oid gs
        then pure ()
        else do
          happens <- Replacement.resolveDestruction oid
          Monad.when happens (changeZone oid Zone.Graveyard)

-- The single counter-PLACEMENT funnel (CR 122.6: counters as markers on a
-- permanent -- not to be confused with `counter` below, CR 701.6's countering
-- of a spell). Before P5 the PutCounters opcode edited Object.counters in
-- place with no funnel at all, so there was nothing for a replacement to
-- intercept.
--
-- CR 122.6 makes this the right single seam: "Some spells and abilities refer to
-- counters being put on an object. This refers to putting counters on that object
-- while it's on the battlefield and also to an object that's given counters as it
-- enters the battlefield." A zero count after the loop puts nothing on.
putCounters :: ObjectId -> CounterKind.CounterKind -> Natural -> Game ()
putCounters oid kind n = do
  resolved <- Replacement.resolveCounters oid kind n
  case resolved of
    Nothing -> pure ()
    Just (target, settledKind, settledCount) ->
      Monad.when (settledCount > 0) $
        State.modify' $ \gs ->
          let bump obj = obj {Object.counters = Map.insertWith (+) settledKind settledCount (Object.counters obj)}
           in gs {GameState.objects = Map.adjust bump target (GameState.objects gs)}

-- The single spell-countering funnel (CR 701.6 -- not to be confused with
-- `putCounters` above, CR 122.6's placement of counter markers). A countered
-- spell is removed from the stack and put into its owner's graveyard (CR
-- 701.6a) via changeZone -- so Rest in Peace's redirect (graveyard->exile) and
-- CR 400.7's new incarnation still compose, exactly as they do for destroy.
-- Ungated for "can't be countered" (CR 701.6), and emits no distinct "was
-- countered" event (#43) -- the same posture as Event.destroy being ungated
-- for CR 701.19c.
counter :: ObjectId -> Game ()
counter oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just _ -> changeZone oid Zone.Graveyard

-- CR 701.21/701.21a: the single sacrifice funnel. The permanent is put into its
-- OWNER's graveyard through changeZone (so Rest in Peace's redirect and a token's
-- CR 704.5d cease-to-exist still compose), and -- unlike Event.destroy -- with no
-- indestructible gate (CR 702.12b) and no regeneration shield consulted (CR
-- 701.19a): CR 701.21a says sacrificing is not destroying. CR 701.21a also
-- restricts it to permanents on the battlefield, so anything else is a no-op.
--
-- CR 701.21a's other clause -- "A player can't sacrifice something that... [is] a
-- permanent they don't control" -- is not enforced here (#44). Not wrong today:
-- the only caller (Resolve's Sacrifice arm) reads a slot the engine itself
-- stamped (Binding.triggerSource, a triggered ability's own source, CR 113.7),
-- which is always controlled by whoever triggered it.
sacrifice :: ObjectId -> Game ()
sacrifice oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just obj -> case Object.zone obj of
      Zone.Battlefield -> changeZone oid Zone.Graveyard
      Zone.Library -> pure ()
      Zone.Hand -> pure ()
      Zone.Graveyard -> pure ()
      Zone.Stack -> pure ()
      Zone.Exile -> pure ()
      -- CR 408.1: a command-zone object is not a permanent, so it is never
      -- sacrificed.
      Zone.Command -> pure ()

-- CR 111.2: create `n` tokens with the given effect-defined characteristics under
-- `controller`'s control (its owner, CR 111.2), summoning-sick (CR 302.6). A token
-- is created from nothing -- it has no prior object to move, so changeZone cannot
-- mint it. Uses from = Battlefield (it appears there; to == from can never read as
-- a leave). Emits the enters event so ETB triggers (CR 603.6a) fire on the same
-- path a resolved permanent uses.
--
-- PLURAL since P5, and that is a rules requirement, not a convenience. CR 614.1
-- replacements scope to the CREATION EVENT, not to each token -- Doubling Season
-- says "if an effect would create ONE OR MORE tokens ... it creates twice that
-- many" (CR 614.16) -- so the count is settled once, up front. Then every token
-- is materialized, and only then does each run its OWN entry loop (CR 616.1g:
-- "one replacement or prevention effect may apply to an event, and another may
-- apply to an event contained within the first event" -- creating a token
-- CONTAINS that token entering); each token's entry loop is handed the whole
-- batch, which EXCLUDES its simultaneously-entering siblings from any copy
-- choice (CR 614.12a; see Pawl.Replacement's applyReplacementsIn for why
-- 614.12a, not 614.13a, is the cite).
--
-- The call nesting below (runEntry, called once per token, nested inside this
-- function which itself settles the CREATE event) implements CR 616.1g's
-- containment as DESIGN INTENT, but no test exercises it: every token card in
-- the pool has empty `replacementEffects`, so each token's entry loop finds no
-- candidates and returns immediately -- the nesting could be deleted and every
-- scenario in the test pool would still pass. CR 616.1g's own worked example
-- (a token copy of Voice of All) needs a token WITH an entry replacement to
-- exercise, and no such card is in this pool (#73).
createTokens :: PlayerId -> Card -> Natural -> Game [ObjectId]
createTokens controller card n = do
  resolved <- Replacement.resolveTokens controller card n
  case resolved of
    Nothing -> pure []
    Just (owner, tokenCard, count) -> do
      let mkObj ts =
            Object.MkObject
              { Object.owner = owner,
                Object.source = Source.OfToken tokenCard,
                Object.zone = Zone.Battlefield,
                Object.tapped = TapState.Untapped,
                Object.damage = 0,
                Object.sickness = Sickness.Sick,
                Object.bindings = Map.empty,
                Object.counters = Map.empty,
                Object.timestamp = ts
              }
      ids <- Monad.replicateM (fromIntegral count) (placeObject owner mkObj Zone.Battlefield)
      Monad.mapM_ (Replacement.runEntry (Set.fromList ids)) ids
      -- A token is created from nothing, so there is no prior incarnation to
      -- snapshot: its last known information IS what it is now (CR 111.3 makes the
      -- creating effect's stated values functionally printed values). Recorded
      -- AFTER every entry loop, so the events describe settled objects.
      Monad.mapM_ recordTokenEntry ids
      pure ids

recordTokenEntry :: ObjectId -> Game ()
recordTokenEntry newId = do
  placed <- State.get
  let snapshot = Projection.project newId placed
  State.modify' (recordEvent (GameEvent.Moved (ZoneChange.MkZoneChange newId Zone.Battlefield Zone.Battlefield) snapshot))

-- CR 121.2/121.3: the single-card draw. Move pid's top library card to their
-- hand; an empty library records the failed draw (CR 704.5b makes it a loss at
-- the next state-based-action check). The primitive shared by the draw step
-- (Engine.runTurnBasedActions), opening hands (Setup.newGame), and the Draw
-- effect (Resolve).
drawCard :: PlayerId -> Game ()
drawCard pid = do
  gs <- State.get
  case Game.zoneMembers Zone.Library pid gs of
    [] -> State.put gs {GameState.drewFromEmpty = Set.insert pid (GameState.drewFromEmpty gs)}
    top : _ -> changeZone top Zone.Hand

-- CR 603.2: does this condition fire on this event, for the permanent that bears
-- it? `bearer` is the object whose ability this is and `you` is its controller
-- -- CR 603.3a controls the triggered ability, and CR 109.5 is what makes "your"
-- (as in "your upkeep") mean that controller -- both are part of the match,
-- because the scan below visits EVERY permanent, not only the one an event
-- names. This module is the sole home of casing on TriggerCondition for RULES
-- purposes; Pawl.Codec also cases on every constructor, but only as the JSON
-- data boundary (encode/decode), not to decide game behaviour.
matchesTrigger :: ObjectId -> PlayerId -> TriggerCondition -> GameEvent -> Bool
matchesTrigger bearer you cond event = case cond of
  -- CR 603.6a: the bearer's own object entered the battlefield.
  TriggerCondition.SelfEnters -> case event of
    GameEvent.Moved zc _ -> ZoneChange.object zc == bearer && ZoneChange.to zc == Zone.Battlefield
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.BecameMonarch _ -> False
  -- CR 603.2b: this step began, on a turn the scope admits.
  TriggerCondition.StepBegins wanted scope -> case event of
    GameEvent.StepBegan began active ->
      began == wanted && case scope of
        TurnScope.EachTurn -> True
        TurnScope.ControllersTurn -> active == you
    GameEvent.Moved _ _ -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.BecameMonarch _ -> False
  -- CR 603.8: a state trigger is not an event trigger. It never matches an entry
  -- in the log; stateTriggers below is its whole story.
  TriggerCondition.StateIs _ -> False
  -- CR 510.1b / 510.2: the bearer dealt COMBAT damage to a PLAYER (the trigger
  -- pattern behind "whenever this creature deals combat damage to a player").
  -- Combat damage already records a DamageDealt event, so the match is a filter
  -- over the log, not new recording.
  TriggerCondition.SelfDealsCombatDamageToPlayer -> case event of
    GameEvent.DamageDealt ev ->
      DamageEvent.source ev == bearer
        && DamageEvent.kind ev == DamageKind.Combat
        && isPlayerRecipient (DamageEvent.target ev)
    GameEvent.Moved _ _ -> False
    GameEvent.StepBegan _ _ -> False
    GameEvent.SpellCast _ -> False
    GameEvent.BecameMonarch _ -> False
  -- CR 725.2: never matched via a card's bearer -- the monarch's crown-steal is
  -- an inherent ability of no object, so its real match lives in
  -- Pawl.Monarch.inherentMatch, not here.
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> False

-- Whether a damage recipient is a player (CR 120.1): a total discriminator over
-- Recipient, so the combat-damage-to-player trigger matcher stays non-partial.
isPlayerRecipient :: Recipient.Recipient -> Bool
isPlayerRecipient r = case r of
  Recipient.ToPlayer _ -> True
  Recipient.ToCreature _ -> False
  Recipient.ToObject _ -> False

-- CR 603.6a: "Each time an event puts one or more permanents onto the
-- battlefield, all permanents on the battlefield (including the newcomers)
-- are checked for any enters-the-battlefield triggers that match the event."
-- This WIDENS M3f's scan, which only ever inspected the object an enters
-- event named: a step trigger belongs to a permanent that has nothing to do
-- with the event at all, so every event is checked against every permanent
-- currently on the battlefield.
--
-- The candidate set -- the current battlefield permanents -- is the same for
-- every event; it is projected ONCE via Projection.projectAll, before the
-- event loop, not once per (event, permanent) pair. The naive alternative --
-- calling Projection.triggeredAbilitiesOf per (event, permanent) pair, as this
-- scan did before this task -- goes through Projection.project, which reruns
-- the whole-board `gather` fold on every call; that made settleForPriority's
-- trigger scan quadratic in board size. Projection.projectAll runs `gather`
-- exactly once and shares the result across the whole scan.
--
-- The battlefield is the ONLY scanned zone; an ability that functions from a
-- graveyard, hand or exile is never scanned (#45).
--
-- This scan derives its candidate set from BATTLEFIELD STATE AS IT THEN STANDS
-- at the CR 117.5 priority boundary, rather than checking against the state
-- immediately after each event -- so a permanent that enters and then dies to a
-- state-based action within the same settle loses its enters trigger (#46). Note
-- this is NOT a "look back in time" gap: that CR 603.10 term of art names the
-- opposite case, and 603.10's exception list (603.10a-g) is
-- leaves-the-battlefield, sacrifice, phase-out and similar, not enters.
--
-- Events outer, permanents inner (ascending by id): a deterministic canonical
-- order, which is what the CR 603.3b ordering prompt indexes into.
eventTriggers :: [GameEvent] -> GameState -> [PendingTrigger]
eventTriggers events gs =
  let projected = Projection.projectAll gs
      onBattlefield =
        Maybe.mapMaybe
          ( \oid -> case Map.lookup oid projected of
              -- Unreachable: projected (Projection.projectAll gs) is keyed on
              -- the same GameState.battlefield set this list walks, so every
              -- oid drawn from that set has an entry.
              Nothing -> Nothing
              -- CR 603.3a: controlled by whoever controls the source when it
              -- triggers. Projection.controllerOf reads control AT THE SCAN
              -- BOUNDARY, not at the moment the underlying event fired (#47) --
              -- unobservable today because nothing changes control between an
              -- event and the CR 117.5 boundary.
              Just pc -> fmap (\ctrl -> (oid, ctrl, PC.triggeredAbilities pc)) (Projection.controllerOf oid gs)
          )
          (Set.toAscList (GameState.battlefield gs))
      forOne event (oid, ctrl, abilities) =
        let fires ab = matchesTrigger oid ctrl (TriggeredAbility.condition ab) event
            pend ab = PendingTrigger.MkPendingTrigger oid ctrl ab Map.empty
         in map pend (filter fires abilities)
   in concatMap (\event -> concatMap (forOne event) onBattlefield) events

-- CR 603.8 / 603.4 / 611.2b: is this state condition currently true, for an
-- ability or effect whose controller is `you` and whose source object is
-- `source`? Reads the PROJECTION -- a subtype is CR 613 layer 4 and control is
-- layer 2, so Blood Moon and Act of Treason both change the answer. This
-- module is the sole home of casing on StateCondition; the two subtype arms
-- ignore `source`.
stateHolds :: PlayerId -> ObjectId -> StateCondition -> GameState -> Bool
stateHolds you source cond gs =
  let hasSubtype subtype oid = Set.member subtype (Projection.subtypesOf oid gs)
   in case cond of
        -- CR 109.5: "you" on a triggered ability's condition means the
        -- ability's controller (at the time it triggered) -- so "you control"
        -- is that player's projected-controlled permanents.
        StateCondition.YouControlNo subtype -> not (any (hasSubtype subtype) (Projection.controls you gs))
        -- Any player's -- the whole battlefield.
        StateCondition.NoPermanentsOfSubtype subtype -> not (any (hasSubtype subtype) (Set.toList (GameState.battlefield gs)))
        -- CR 611.2b / 613.1b / 400.7: the source object is still on the
        -- battlefield AND its projected controller is `you`.
        StateCondition.YouControlSource ->
          Set.member source (GameState.battlefield gs) && Projection.controllerOf source gs == Just you

-- CR 603.8: state triggers. Every battlefield permanent whose StateIs condition
-- is currently TRUE and which has no instance already on the stack.
--
-- Armedness is DERIVED, never stored. CR 603.8's second sentence -- "doesn't
-- trigger again until the ability has resolved, has been countered, or has
-- otherwise left the stack" -- names three outcomes that are all "no longer on
-- the stack", so an instance sitting there is the whole suppression rule and
-- there is no bookkeeping field to leak. There is no triggered-but-not-yet-placed
-- window to worry about: Engine.placePendingTriggers puts them on the stack
-- within the same settle step.
--
-- A trigger whose modes are all unfillable would be removed from the stack (CR
-- 603.3c) and re-trigger on the next settle pass while its condition held, which
-- would not terminate. No card in the pool can do that -- Barbarian Outcast's
-- single mode has no target slots and is always fillable -- and the first card
-- that could is the one that must revisit this.
stateTriggers :: GameState -> [PendingTrigger]
stateTriggers gs =
  let -- Suppression is scoped to (source, ability): `Object.source obj ==
      -- Source.OfTrigger srcId ab` compares BOTH the source object's id and
      -- the ability, so two permanents bearing the identical triggered
      -- ability (e.g. two Barbarian Outcasts) suppress independently -- one
      -- instance per source, not one for the whole board. A weaker
      -- comparison (ability only, dropping srcId) would wrongly suppress a
      -- second source's identical trigger.
      --
      -- Unreachable caveat: if a SINGLE source ever carried two textually
      -- identical StateIs abilities, this same comparison (Source equality)
      -- would conflate them into one Source.OfTrigger value and suppress the
      -- second as though it were an instance of the first. No card in the
      -- pool has two identical state triggers on one source today; the first
      -- that does must revisit this.
      alreadyOnStack srcId ab =
        let isInstance sid = case Game.lookupObject sid gs of
              -- A stack id whose object can't be found: fail CLOSED (treat
              -- as suppressing), not open. This runs inside the
              -- Engine.settleForPriority fixpoint, so a lost suppression
              -- (failing open) loops forever -- a hang, not a wrong answer --
              -- while failing closed costs at most one settle pass' worth of
              -- a legitimate new instance. Unreachable today: Resolve.cease
              -- removes the stack entry and its object together, so a stack
              -- id is never left dangling.
              Nothing -> True
              -- Source equality, so one source carrying two textually identical
              -- StateIs abilities would conflate them and suppress the second
              -- as though it were an instance of the first (#55).
              Just obj -> Object.source obj == Source.OfTrigger srcId ab
         in any isInstance (GameState.stack gs)
      forOne oid = case Projection.controllerOf oid gs of
        Nothing -> []
        -- CR 603.3a: a triggered ability is controlled by whoever controls
        -- its source; CR 109.5 is what makes "you" in the condition (e.g.
        -- YouControlNo's "you control no Swamps") mean that same controller.
        Just ctrl ->
          let live ab = case TriggeredAbility.condition ab of
                TriggerCondition.StateIs cond -> stateHolds ctrl oid cond gs && not (alreadyOnStack oid ab)
                TriggerCondition.SelfEnters -> False
                TriggerCondition.StepBegins _ _ -> False
                TriggerCondition.SelfDealsCombatDamageToPlayer -> False
                TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
              pend ab = PendingTrigger.MkPendingTrigger oid ctrl ab Map.empty
           in map pend (filter live (Projection.triggeredAbilitiesOf oid gs))
   in concatMap forOne (Set.toAscList (GameState.battlefield gs))

-- CR 603.7: delayed abilities whose trigger event is among these events. Each one
-- that fires is REMOVED from the store (CR 603.7b: "only once, the next time its
-- trigger event occurs"); the survivors are returned so the caller can store them
-- back. CR 603.7d-f: the controller travels with the entry, so a delayed ability
-- resolves under the player who controlled the spell that created it even if that
-- spell's source object is long gone.
--
-- `fires` matches only against EVENTS (`matchesTrigger`), never against live
-- game state, so a stored entry whose condition is TriggerCondition.StateIs would
-- never match here -- it would neither fire nor ever be evicted from the store.
-- Not a live gap: TriggerCondition is a closed type (Pawl.Type.TriggerCondition)
-- and no card in this pool arms a delayed ability with a StateIs condition (CR
-- 603.7's few state-triggered delayed abilities, e.g. "at the beginning of the
-- next end step" clauses, are all StepBegins in this pool). Noted because a later
-- P4 task touches state conditions again and should see this before adding one.
--
-- The surviving store this function returns is computed from the EVENT MATCH
-- (`fires`) alone, before gatherTriggers's CR 603.4 intervening-"if" filter
-- (`interveningHolds`) ever runs on the entries it produces -- so an entry whose
-- intervening "if" is false is removed here, spending CR 603.7b's one shot,
-- rather than staying armed for the trigger event's next occurrence (#48).
delayedPending :: [GameEvent] -> GameState -> ([PendingTrigger], Seq.Seq DelayedTrigger)
delayedPending events gs =
  let fires entry =
        let cond = TriggeredAbility.condition (DelayedTrigger.ability entry)
         in any (matchesTrigger (DelayedTrigger.source entry) (DelayedTrigger.controller entry) cond) events
      pend entry =
        PendingTrigger.MkPendingTrigger
          (DelayedTrigger.source entry)
          (DelayedTrigger.controller entry)
          (DelayedTrigger.ability entry)
          (DelayedTrigger.bindings entry)
      store = GameState.delayedTriggers gs
   in (map pend (Foldable.toList (Seq.filter fires store)), Seq.filter (not . fires) store)

-- Everything that has triggered and is not yet on the stack, from all three
-- sources, plus the delayed store as it stands afterwards. One function, so
-- Pawl.Engine never needs to know how many sources there are.
gatherTriggers :: [GameEvent] -> GameState -> ([PendingTrigger], Seq.Seq DelayedTrigger)
gatherTriggers events gs =
  let (fromDelayed, surviving) = delayedPending events gs
      all_ = eventTriggers events gs ++ stateTriggers gs ++ fromDelayed
   in (filter (interveningHolds gs) all_, surviving)

-- CR 603.4: "the ability doesn't trigger at all" when its intervening "if" is
-- false as the trigger event occurs. Checked HERE, at the gather -- not at
-- placement -- because "doesn't trigger" must be indistinguishable from "no
-- ability existed", including to the CR 117.5 settle loop's re-run flag.
interveningHolds :: GameState -> PendingTrigger -> Bool
interveningHolds gs pending =
  case TriggeredAbility.intervening (PendingTrigger.ability pending) of
    Nothing -> True
    Just cond -> stateHolds (PendingTrigger.controller pending) (PendingTrigger.source pending) cond gs
