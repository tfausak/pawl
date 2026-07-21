-- The event pipeline (CR 603/614). This module owns the single zone-change
-- funnel and, later, the sole casing on ReplacementEffect and TriggerCondition.
-- changeZone lives here (not in Pawl.Game) so it can read the projection --
-- Projection imports Game, so a Game.changeZone that read the projection would
-- be an import cycle. See the plan's module dependency note.
module Pawl.Event where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Binding as Binding
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Type.ActivePrevention as ActivePrevention
import Pawl.Type.Card (Card)
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.Combat as Combat
import Pawl.Type.DamageEvent (DamageEvent)
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.DamageKind as DamageKind
import qualified Pawl.Type.Duration as Duration
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
import Pawl.Type.Prevention (Prevention)
import qualified Pawl.Type.Prevention as Prevention
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.ProjectedCharacteristics as PC
import Pawl.Type.ReplacementEffect (ReplacementEffect)
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
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
-- Engine.handoffTurn clears the log at turn end, Setup.emptyGame seeds it empty,
-- and the test helper Support.withEvent sets it directly; none of those append.
recordEvent :: GameEvent -> GameState -> GameState
recordEvent event gs = gs {GameState.events = GameState.events gs Seq.|> event}

-- The zone change an event describes, if it is one.
movedOf :: GameEvent -> Maybe ZoneChange
movedOf event = case event of
  GameEvent.Moved zc _ -> Just zc
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.StepBegan _ _ -> Nothing

-- The damage an event describes, if it is any.
damageOf :: GameEvent -> Maybe DamageEvent
damageOf event = case event of
  GameEvent.DamageDealt ev -> Just ev
  GameEvent.Moved _ _ -> Nothing
  GameEvent.StepBegan _ _ -> Nothing

-- CR 117.5: the events the trigger scan has not yet consumed.
unscannedEvents :: GameState -> [GameEvent]
unscannedEvents gs =
  Foldable.toList (Seq.drop (fromIntegral (GameState.scannedThrough gs)) (GameState.events gs))

-- CR 704.5h: the damage the state-based-action check has not yet consumed.
unscannedDamage :: GameState -> [DamageEvent]
unscannedDamage gs =
  Maybe.mapMaybe damageOf (Foldable.toList (Seq.drop (fromIntegral (GameState.damageScannedThrough gs)) (GameState.events gs)))

-- CR 615.6: apply active prevention shields to a batch of damage events, dropping
-- each event a shield cancels -- a prevented event never happens (not marked, not
-- drained, never emitted). The cancel shape, as applyReplacements is the redirect
-- shape. This module is the sole home of casing on Prevention.
applyPreventions :: [ActivePrevention.ActivePrevention] -> [DamageEvent] -> [DamageEvent]
applyPreventions preventions = filter (not . prevented)
  where
    prevented ev = any (\p -> cancels (ActivePrevention.prevention p) ev) preventions

-- Does this prevention cancel this event? The Prevention case lives here.
cancels :: Prevention -> DamageEvent -> Bool
cancels p ev = case p of
  Prevention.PreventAllCombatDamage -> DamageEvent.kind ev == DamageKind.Combat

-- CR 514.2: at cleanup, drop until-end-of-turn preventions (the prevention analog
-- of Projection.dropEndOfTurnEffects). Indefinite preventions, if ever added, stay.
dropEndOfTurnPreventions :: GameState -> GameState
dropEndOfTurnPreventions gs =
  let keep p = ActivePrevention.duration p /= Duration.UntilEndOfTurn
   in gs {GameState.preventions = filter keep (GameState.preventions gs)}

-- CR 701.19a: regeneration shields last "this turn," so cleanup clears every one.
clearRegenerationShields :: GameState -> GameState
clearRegenerationShields gs = gs {GameState.regenerationShields = Map.empty}

-- Insert a freshly-built object into `dest` under a new id and timestamp, and
-- return that id. The common tail of changeZone (a moved incarnation) and
-- createToken (a token from nothing). `mkObj` receives the fresh timestamp so the
-- object records when it entered (CR 613.7d). The Moved event is emitted by the
-- CALLER: only it knows which state the CR 608.2h snapshot must be taken against.
placeObject :: PlayerId -> (Timestamp.Timestamp -> Object.Object) -> Zone -> GameState -> (ObjectId, GameState)
placeObject pid mkObj dest gs =
  let (newId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj = markCopyOnEnter dest (mkObj ts)
      gs3 = gs2 {GameState.objects = Map.insert newId obj (GameState.objects gs2)}
   in (newId, Game.insertIntoZone dest pid newId gs3)

-- CR 614.1c / 603.6d / 113.6h: an object with a "enters as a copy" ability is
-- marked as-enters-pending as it enters the battlefield, on ANY entry path. The
-- choice itself is a prompt, so it is drained at the CR 117.5 boundary
-- (Engine.drainAsEntersChoices), not here -- placeObject stays pure. Non-copyOnEnter
-- entries and non-battlefield destinations are untouched.
markCopyOnEnter :: Zone -> Object.Object -> Object.Object
markCopyOnEnter dest obj =
  if dest == Zone.Battlefield && maybe False Card.copyOnEnter (cardOfObject obj)
    then obj {Object.bindings = Binding.markPending (Object.bindings obj)}
    else obj

-- The card an object is built from, read from its source (P2; a card-less object --
-- an ability or trigger -- has none, and is never copyOnEnter).
cardOfObject :: Object.Object -> Maybe Card.Card
cardOfObject obj = case Object.source obj of
  Source.OfCard printing -> Just (Printing.card printing)
  Source.OfToken card -> Just card
  Source.OfAbility _ _ -> Nothing
  Source.OfTrigger _ _ -> Nothing

-- The single zone-change primitive (CR 400.7): the source object ceases; a NEW
-- object with a fresh id is created in the destination, carrying owner and
-- source forward and resetting per-incarnation state. No-op if the id is unknown.
changeZone :: ObjectId -> Zone -> GameState -> GameState
changeZone oid requestedDest gs = case Game.lookupObject oid gs of
  Nothing -> gs
  Just obj ->
    let pid = Object.owner obj
        fromZone = Object.zone obj
        -- CR 608.2h: last known information -- the object as it exists in the zone
        -- it is LEAVING, projected against the PRE-MOVE state. One board
        -- projection per zone change, forced eagerly (GameEvent.Moved's snapshot
        -- field is strict) rather than left as a thunk retaining the whole
        -- pre-move GameState for a turn. Measured on the tasty-bench suite,
        -- pre-log baseline (3cc3ecd) vs. this log with the strict field (goldfish
        -- /casting/fighting, 2p): 10.1/9.30/9.31 ms -> 10.6/9.73/9.72 ms -- a ~4-5%
        -- move, within the benchmark's own run-to-run noise (~800 us stddev on a
        -- ~10 ms mean), not the large regression a captured pre-move GameState
        -- would cause. That is the price of an honest history (a token has no
        -- printed card to re-derive from, CR 111.3).
        snapshot = Projection.project oid gs
        -- CR 614.4: replacements exist before the event; read them from the
        -- pre-move state. CR 614.6: the modified event is what actually happens.
        proposed = ZoneChange.MkZoneChange oid fromZone requestedDest
        resolved = applyReplacements (Projection.replacementsAffecting gs) proposed
        dest = ZoneChange.to resolved
        mkObj ts = obj {Object.zone = dest, Object.tapped = TapState.Untapped, Object.damage = 0, Object.sickness = Sickness.Sick, Object.bindings = Map.empty, Object.counters = Map.empty, Object.timestamp = ts}
        gs1 = Game.removeFromZones pid oid gs
        gs2 = gs1 {GameState.objects = Map.delete oid (GameState.objects gs1)}
        (newId, placed) = placeObject pid mkObj dest gs2
     in -- CR 603.2g: record the RESOLVED event, carrying the NEW object's id --
        -- what an enters trigger scans.
        recordEvent (GameEvent.Moved (ZoneChange.MkZoneChange newId fromZone dest) snapshot) placed

-- The single destruction funnel (CR 701.8 / 702.12b): every destruction -- the
-- Destroy opcode and the CR 704.5g/h state-based actions -- flows through here.
-- CR 702.12b: an indestructible permanent can't be destroyed (the event never
-- happens, so a shield is neither applied nor consumed, CR 614.7). CR 701.19a: a
-- regeneration shield replaces the destruction. Otherwise the permanent is put
-- into its owner's graveyard via changeZone (so Rest in Peace's redirect and a
-- token's CR 704.5d cease-to-exist still compose). CR 701.19c "can't be
-- regenerated" is deferred to Wrath (spec section 7): this funnel is ungated.
destroy :: ObjectId -> GameState -> GameState
destroy oid gs = case Game.lookupObject oid gs of
  Nothing -> gs
  Just _ ->
    if Projection.hasKeyword Keyword.Indestructible oid gs
      then gs
      else case Map.lookup oid (GameState.regenerationShields gs) of
        Just n | n > 0 -> regenerate oid gs
        _ -> changeZone oid Zone.Graveyard gs

-- CR 701.19a: consume one shield, remove all marked damage, tap the permanent,
-- and remove it from combat. The permanent stays on the battlefield (same id).
regenerate :: ObjectId -> GameState -> GameState
regenerate oid gs =
  let shields = Map.update (\n -> if n <= 1 then Nothing else Just (n - 1)) oid (GameState.regenerationShields gs)
      healTap obj = obj {Object.damage = 0, Object.tapped = TapState.Tapped}
      gs1 =
        gs
          { GameState.regenerationShields = shields,
            GameState.objects = Map.adjust healTap oid (GameState.objects gs)
          }
   in removeFromCombat oid gs1

-- CR 701.19a: if it is attacking or blocking, remove it from combat. Edits the
-- GameState.combat maps directly (Event must not import Pawl.Combat -- that would
-- cycle through Sba; see the plan's Global Constraints).
removeFromCombat :: ObjectId -> GameState -> GameState
removeFromCombat oid gs =
  let c = GameState.combat gs
      c1 =
        c
          { Combat.attackers = Map.delete oid (Combat.attackers c),
            Combat.blockers = Map.map (Set.delete oid) (Map.delete oid (Combat.blockers c))
          }
   in gs {GameState.combat = c1}

-- The single counter funnel (CR 701.6). A countered spell is removed from the
-- stack and put into its owner's graveyard (CR 701.6a) via changeZone -- so Rest
-- in Peace's redirect (graveyard->exile) and CR 400.7's new incarnation still
-- compose, exactly as they do for destroy. Ungated today: "can't be countered"
-- (CR 701.6) and a distinct "was countered" event are deferred (spec section 7),
-- as Event.destroy is ungated for CR 701.19c.
counter :: ObjectId -> GameState -> GameState
counter oid gs = case Game.lookupObject oid gs of
  Nothing -> gs
  Just _ -> changeZone oid Zone.Graveyard gs

-- CR 701.21/701.21a: the single sacrifice funnel. The permanent is put into its
-- OWNER's graveyard through changeZone (so Rest in Peace's redirect and a token's
-- CR 704.5d cease-to-exist still compose), and -- unlike Event.destroy -- with no
-- indestructible gate (CR 702.12b) and no regeneration shield consulted (CR
-- 701.19a): CR 701.21a says sacrificing is not destroying. CR 701.21a also
-- restricts it to permanents on the battlefield, so anything else is a no-op.
--
-- Named elision: CR 701.21a's other clause -- "A player can't sacrifice
-- something that... [is] a permanent they don't control" -- is not enforced
-- here. Not wrong today: the only caller (Resolve's Sacrifice arm) reads a
-- slot the engine itself stamped (Binding.triggerSource, a triggered
-- ability's own source, CR 113.7), which is always controlled by whoever
-- triggered it. Expires at the first effect that can name a permanent its
-- controller does not control -- an opponent-sacrifice effect (an edict,
-- e.g. Diabolic Edict).
sacrifice :: ObjectId -> GameState -> GameState
sacrifice oid gs = case Game.lookupObject oid gs of
  Nothing -> gs
  Just obj -> case Object.zone obj of
    Zone.Battlefield -> changeZone oid Zone.Graveyard gs
    Zone.Library -> gs
    Zone.Hand -> gs
    Zone.Graveyard -> gs
    Zone.Stack -> gs
    Zone.Exile -> gs

-- CR 111.2: create a token with the given effect-defined characteristics under
-- `controller`'s control (its owner, CR 111.2), summoning-sick (CR 302.6). A token
-- is created from nothing -- it has no prior object to move, so changeZone cannot
-- mint it. Uses from = Battlefield (it appears there; to == from can never read as
-- a leave). Emits the enters event so ETB triggers (CR 603.6a) fire on the same
-- path a resolved permanent uses. Does NOT consult replacements (Doubling Season
-- is future, spec section 8).
createToken :: PlayerId -> Card -> GameState -> GameState
createToken controller card gs =
  let mkObj ts =
        Object.MkObject
          { Object.owner = controller,
            Object.source = Source.OfToken card,
            Object.zone = Zone.Battlefield,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
      (newId, placed) = placeObject controller mkObj Zone.Battlefield gs
      -- A token is created from nothing, so there is no prior incarnation to
      -- snapshot: its last known information IS what it is now (CR 111.3 makes
      -- the creating effect's stated values functionally printed values).
      snapshot = Projection.project newId placed
   in recordEvent (GameEvent.Moved (ZoneChange.MkZoneChange newId Zone.Battlefield Zone.Battlefield) snapshot) placed

-- CR 121.2/121.3: the single-card draw. Move pid's top library card to their
-- hand; an empty library records the failed draw (CR 704.5b makes it a loss at
-- the next state-based-action check). The primitive shared by the draw step
-- (Engine.drawFor), opening hands (Setup.drawCard), and the Draw effect (Resolve).
drawCard :: PlayerId -> GameState -> GameState
drawCard pid gs = case Game.zoneMembers Zone.Library pid gs of
  [] -> gs {GameState.drewFromEmpty = Set.insert pid (GameState.drewFromEmpty gs)}
  top : _ -> changeZone top Zone.Hand gs

-- CR 614: rewrite the proposed zone change by each active replacement. CR 614.5:
-- a replacement gets ONE opportunity -- applied left-to-right, each sees the
-- running event; RedirectZoneChange's output destination no longer matches its
-- own `whenDestination`, so it cannot re-fire. This module is the sole home of
-- casing on ReplacementEffect.
applyReplacements :: [ReplacementEffect] -> ZoneChange -> ZoneChange
applyReplacements res zc = List.foldl' applyOne zc res

applyOne :: ZoneChange -> ReplacementEffect -> ZoneChange
applyOne zc re = case re of
  ReplacementEffect.RedirectZoneChange whenDest toDest ->
    if ZoneChange.to zc == whenDest
      then zc {ZoneChange.to = toDest}
      else zc

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
  -- CR 603.2b: this step began, on a turn the scope admits.
  TriggerCondition.StepBegins wanted scope -> case event of
    GameEvent.StepBegan began active ->
      began == wanted && case scope of
        TurnScope.EachTurn -> True
        TurnScope.ControllersTurn -> active == you
    GameEvent.Moved _ _ -> False
    GameEvent.DamageDealt _ -> False
  -- CR 603.8: a state trigger is not an event trigger. It never matches an entry
  -- in the log; stateTriggers below is its whole story.
  TriggerCondition.StateIs _ -> False

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
-- The battlefield is the ONLY scanned zone. An ability that functions from a
-- graveyard, hand or exile is a named deferral (the P4 spec, section 8), expiring
-- at the first such card.
--
-- A permanent that enters and then dies to a state-based action within the
-- same settle (Engine.settleForPriority runs Sba.performStateBasedActions
-- before placePendingTriggers) DOES currently lose its enters trigger:
-- Event.changeZone retires the id from GameState.objects on the move that
-- kills it (CR 400.7's "becomes a new object with no memory of... its
-- previous existence"), so by the time this scan runs, no candidate carries
-- that id. This is NOT a "look back in time" gap -- that CR 603.10 term of
-- art names the opposite case, where a trigger checks the state immediately
-- PRIOR to an event, and 603.10's own exception list (603.10a-g) is
-- leaves-the-battlefield, sacrifice, phase-out and similar triggers, not
-- enters. CR 603.10's NORMAL rule -- the one that applies to an enters
-- trigger -- is that objects existing immediately AFTER the event are
-- checked, and the entering permanent does exist immediately after the
-- Moved event that placed it. The actual defect is timing: this scan runs at
-- the CR 117.5 priority boundary and derives its candidate set from
-- BATTLEFIELD STATE AS IT THEN STANDS, rather than checking against the
-- state immediately after each event, so an id that has since been retired
-- is invisible to it. It is a named deferral of this phase (P4 spec section
-- 8's "enters-then-dies-same-settle" bullet); closing it needs the scan to
-- evaluate candidates against the state at the time of the event rather than
-- at the boundary.
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
              -- BOUNDARY, not at the moment the underlying event fired --
              -- pre-existing posture carried forward from M3f, unobservable
              -- today because nothing changes control between an event and
              -- the CR 117.5 boundary. Expires at the first effect that can
              -- change control between an event and the boundary.
              Just pc -> fmap (\ctrl -> (oid, ctrl, PC.triggeredAbilities pc)) (Projection.controllerOf oid gs)
          )
          (Set.toAscList (GameState.battlefield gs))
      forOne event (oid, ctrl, abilities) =
        let fires ab = matchesTrigger oid ctrl (TriggeredAbility.condition ab) event
            pend ab = PendingTrigger.MkPendingTrigger oid ctrl ab Map.empty
         in map pend (filter fires abilities)
   in concatMap (\event -> concatMap (forOne event) onBattlefield) events

-- CR 603.8 / 603.4: is this state condition currently true, for an ability whose
-- controller is `you`? Reads the PROJECTION -- a subtype is CR 613 layer 4 and
-- control is layer 2, so Blood Moon and Act of Treason both change the answer.
-- This module is the sole home of casing on StateCondition.
stateHolds :: PlayerId -> StateCondition -> GameState -> Bool
stateHolds you cond gs =
  let hasSubtype subtype oid = Set.member subtype (Projection.subtypesOf oid gs)
   in case cond of
        -- CR 109.5: "you" on a triggered ability's condition means the
        -- ability's controller (at the time it triggered) -- so "you control"
        -- is that player's projected-controlled permanents.
        StateCondition.YouControlNo subtype -> not (any (hasSubtype subtype) (Projection.controls you gs))
        -- Any player's -- the whole battlefield.
        StateCondition.NoPermanentsOfSubtype subtype -> not (any (hasSubtype subtype) (Set.toList (GameState.battlefield gs)))

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
              Just obj -> Object.source obj == Source.OfTrigger srcId ab
         in any isInstance (GameState.stack gs)
      forOne oid = case Projection.controllerOf oid gs of
        Nothing -> []
        -- CR 603.3a: a triggered ability is controlled by whoever controls
        -- its source; CR 109.5 is what makes "you" in the condition (e.g.
        -- YouControlNo's "you control no Swamps") mean that same controller.
        Just ctrl ->
          let live ab = case TriggeredAbility.condition ab of
                TriggerCondition.StateIs cond -> stateHolds ctrl cond gs && not (alreadyOnStack oid ab)
                TriggerCondition.SelfEnters -> False
                TriggerCondition.StepBegins _ _ -> False
              pend ab = PendingTrigger.MkPendingTrigger oid ctrl ab Map.empty
           in map pend (filter live (Projection.triggeredAbilitiesOf oid gs))
   in concatMap forOne (Set.toAscList (GameState.battlefield gs))

-- Everything that has triggered and is not yet on the stack. One function, so
-- Pawl.Engine never needs to know how many sources there are. Now runs an event
-- pass (eventTriggers) and a state pass (CR 603.8, stateTriggers); a delayed
-- pass (CR 603.7) is still owed at Task 6.
gatherTriggers :: [GameEvent] -> GameState -> [PendingTrigger]
gatherTriggers events gs = eventTriggers events gs ++ stateTriggers gs
