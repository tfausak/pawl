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
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.Prevention (Prevention)
import qualified Pawl.Type.Prevention as Prevention
import qualified Pawl.Type.Printing as Printing
import Pawl.Type.ReplacementEffect (ReplacementEffect)
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Timestamp as Timestamp
import Pawl.Type.TriggerCondition (TriggerCondition)
import qualified Pawl.Type.TriggerCondition as TriggerCondition
import Pawl.Type.TriggeredAbility (TriggeredAbility)
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
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

-- The single destruction funnel (CR 701.7 / 700.4): every destruction -- the
-- Destroy opcode and the CR 704.5g/h state-based actions -- flows through here.
-- CR 700.4: an indestructible permanent can't be destroyed (the event never
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

-- CR 603.6a: does this condition fire on this event? SelfEnters fires when the
-- bearer's object entered the battlefield -- so the event's destination is the
-- battlefield. This module is the sole home of casing on TriggerCondition.
matchesTrigger :: TriggerCondition -> ZoneChange -> Bool
matchesTrigger cond zc = case cond of
  TriggerCondition.SelfEnters -> ZoneChange.to zc == Zone.Battlefield

-- The battlefield/enters pass of the three-pass trigger scan (leaves-the-
-- battlefield and phase-step passes are future). For each event with to =
-- Battlefield, the newcomer (`object`) is checked for triggered abilities whose
-- condition matches; each becomes a pending trigger paired with its source id and
-- controller (CR 603.3a).
triggersFrom :: [ZoneChange] -> GameState -> [(ObjectId, PlayerId, TriggeredAbility Card)]
triggersFrom changes gs =
  let fromOne zc =
        let srcId = ZoneChange.object zc
         in case Projection.controllerOf srcId gs of
              Nothing -> []
              Just ctrl ->
                map
                  (\ab -> (srcId, ctrl, ab))
                  (filter (\ab -> matchesTrigger (TriggeredAbility.condition ab) zc) (Projection.triggeredAbilitiesOf srcId gs))
   in concatMap fromOne changes
