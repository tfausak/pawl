-- The event pipeline (CR 603/614/616). Owns the single zone-change funnel, CR
-- 616.1's loop and the `apply` that carries out a chosen replacement, plus the
-- sole casing on TriggerCondition.
--
-- The loop and the funnel share a module because the rules make them mutually
-- recursive, not because either is convenient here: a zone change raises its
-- event through the loop, because CR 614.1's replacement effects "watch for a
-- particular event that would happen" and a zone change is one of those events,
-- and applying a chosen rewrite can itself
-- change zones -- CR 614.1c's "as this permanent enters, sacrifice any number of
-- permanents" is a replacement whose application is a CR 701.21a sacrifice. The
-- SELECTION half -- which effects exist, which apply, how they bucket, who
-- chooses, how a row is spent -- stays in Pawl.Engine.Replacement, which this
-- module calls down into and which must never import this one.
--
-- changeZone lives here rather than in Pawl.Engine.Game so it can read the
-- projection -- Projection imports Game, so a Game.changeZone reading it would be
-- an import cycle.
module Pawl.Engine.Event where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Attach as Attach
import qualified Pawl.Engine.Battle as Battle
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Commander as Commander
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.EffectZone as EffectZone
import qualified Pawl.Engine.EntryRestriction as EntryRestriction
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.ManaRider as ManaRider
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.Replacement as Replacement
import qualified Pawl.Engine.SacrificeRestriction as SacrificeRestriction
import qualified Pawl.Engine.Saga as Saga
import qualified Pawl.Extra.Integer as Integer
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.AbilityTriggered as AbilityTriggered
import qualified Pawl.Types.ActiveUnregeneratable as ActiveUnregeneratable
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AsCopy as AsCopy
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.AttackerBlocked as AttackerBlocked
import qualified Pawl.Types.AttackerDeclared as AttackerDeclared
import qualified Pawl.Types.BattlefieldCandidate as BattlefieldCandidate
import qualified Pawl.Types.BecameAttached as BecameAttached
import qualified Pawl.Types.BecameAttacked as BecameAttacked
import qualified Pawl.Types.BecameBlocking as BecameBlocking
import qualified Pawl.Types.BecameDesignated as BecameDesignated
import qualified Pawl.Types.BecameTarget as BecameTarget
import Pawl.Types.Binding (Binding)
import qualified Pawl.Types.BlocksDeclared as BlocksDeclared
import Pawl.Types.CandidateId (CandidateId)
import Pawl.Types.Card (Card)
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.ClassLevelChange as ClassLevelChange
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat
import qualified Pawl.Types.CommandZoneDecision as CommandZoneDecision
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.ControlChanged as ControlChanged
import qualified Pawl.Types.ControllerBecomesTarget as ControllerBecomesTarget
import qualified Pawl.Types.CounterCause as CounterCause
import qualified Pawl.Types.CounterChange as CounterChange
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterR as CounterR
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Countering as Countering
import qualified Pawl.Types.CreatureBecomesBlockedByAtLeast as CreatureBecomesBlockedByAtLeast
import Pawl.Types.DamageEvent (DamageEvent)
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePrevented as DamagePrevented
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import Pawl.Types.DelayedTrigger (DelayedTrigger)
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.DestructionCause as DestructionCause
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Discarded as Discarded
import qualified Pawl.Types.Drew as Drew
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.EntryR as EntryR
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.EventGroup as EventGroup
import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import Pawl.Types.Game (Game)
import Pawl.Types.GameEvent (GameEvent)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.HalfUnlocked as HalfUnlocked
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.Mentored as Mentored
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.Onset (Onset)
import qualified Pawl.Types.Onset as Onset
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PendingEntryEffect as PendingEntryEffect
import Pawl.Types.PendingTrigger (PendingTrigger)
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import qualified Pawl.Types.PermanentBecomesDesignated as PermanentBecomesDesignated
import qualified Pawl.Types.PermanentSacrificed as PermanentSacrificed
import Pawl.Types.PhaseSelector (PhaseSelector)
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerAttacksWith as PlayerAttacksWith
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerDrawsNthCard as PlayerDrawsNthCard
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import Pawl.Types.Prevention (Prevention)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import Pawl.Types.ProposedEvent (ProposedEvent)
import qualified Pawl.Types.ProposedEvent as ProposedEvent
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import Pawl.Types.ReplacementCandidate (ReplacementCandidate)
import qualified Pawl.Types.ReplacementCandidate as ReplacementCandidate
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.RevealCause as RevealCause
import qualified Pawl.Types.Revealed as Revealed
import qualified Pawl.Types.SacrificeAnyNumber as SacrificeAnyNumber
import qualified Pawl.Types.SelfCountersReached as SelfCountersReached
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.SpellCast as SpellCast
import qualified Pawl.Types.SpellWasCast as SpellWasCast
import qualified Pawl.Types.StackObjectKind as StackObjectKind
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.StepBegins as StepBegins
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.TokenR as TokenR
import qualified Pawl.Types.Transformed as Transformed
import Pawl.Types.TriggerCondition (TriggerCondition)
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TriggeredAbilitySource as TriggeredAbilitySource
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.TurnUpR as TurnUpR
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite
import Pawl.Types.TurnWindow (TurnWindow)
import qualified Pawl.Types.TurnWindow as TurnWindow
import qualified Pawl.Types.VentureMarkerEntered as VentureMarkerEntered
import qualified Pawl.Types.WithCounters as WithCounters
import Pawl.Types.Zone (Zone)
import qualified Pawl.Types.Zone as Zone
import Pawl.Types.ZoneChange (ZoneChange)
import qualified Pawl.Types.ZoneChange as ZoneChange
import qualified Pawl.Types.ZoneChangeR as ZoneChangeR

-- CR 608.2i: append one entry to the turn-scoped log. The single APPEND point,
-- which is also what lets it be where CR 603.10's "immediately after an event" is
-- sampled: the caller has already applied the event to the board by the time it
-- gets here, so the pre-append state IS the board the rule asks about, and
-- GameState.battlefieldWhenTriggered files it under this event's group.
--
-- Sampled at EVERY record, not only at the one that opens a batch, and the
-- overwrite within a group is deliberate rather than waste: a group is CR 608.2f's
-- single event, its members are applied and recorded one at a time, and the rule
-- asks about the board after the whole thing -- so the last member's sample is the
-- right one and the earlier thunks are dropped unforced.
--
-- Cheap for that reason: the thunk a discarded sample left is never projected, and
-- the one that survives is projected at the scan, which would have projected the
-- board there anyway. What the change costs against a per-batch sample is one
-- projection per event GROUP instead of one per batch.
--
-- The group stamped on the entry is CR 704.3 / CR 608.2f's "single event": a
-- fresh one per record, unless a `simultaneously` bracket is open, in which case
-- every event in its body shares the one the bracket froze. Only advanced at
-- depth 0, which is what freezes it.
recordEvent :: GameEvent -> GameState -> GameState
recordEvent event gs =
  let group = GameState.nextEventGroup gs
   in gs
        { GameState.events = GameState.events gs Seq.|> LoggedEvent.MkLoggedEvent {LoggedEvent.group = group, LoggedEvent.event = event},
          GameState.nextEventGroup =
            if GameState.eventGroupDepth gs == 0
              then EventGroup.next group
              else group,
          GameState.battlefieldWhenTriggered =
            Map.insert group (battlefieldCandidates gs) (GameState.battlefieldWhenTriggered gs)
        }

-- CR 603.10's "objects that exist immediately after an event", taken of the
-- battlefield: every permanent standing on it, the player controlling it, and the
-- projection its abilities are read out of.
--
-- The whole battlefield rather than a diff or an overrides-only map, because all
-- three of the answers the scan wants can move and only a full reading tells
-- "unchanged" from "not there". The controller half is the one that used to be
-- kept alone, as a layer-2 overrides map; folding it in here costs nothing extra,
-- since the projection this walks is the same one the controller walk needs.
--
-- Projected ONCE for the whole board rather than per permanent, Projection.project
-- rerunning the whole-board gather fold on every call.
battlefieldCandidates :: GameState -> Map.Map ObjectId (BattlefieldCandidate.BattlefieldCandidate PC.ProjectedCharacteristics)
battlefieldCandidates gs =
  let projected = Projection.projectAll gs
      grants = Projection.controlGrants gs
   in Map.fromList
        ( Maybe.mapMaybe
            ( \oid -> case Map.lookup oid projected of
                -- Unreachable: projectAll is keyed on the same battlefield set this
                -- list walks, so every oid drawn from it has an entry.
                Nothing -> Nothing
                Just pc -> fmap (\ctrl -> (oid, BattlefieldCandidate.MkBattlefieldCandidate {BattlefieldCandidate.controller = ctrl, BattlefieldCandidate.characteristics = pc})) (Projection.controllerOfGiven grants Set.empty oid gs)
            )
            (Set.toAscList (GameState.battlefield gs))
        )

-- CR 704.3 / CR 608.2f: run `body` as ONE event, so every event it records
-- shares an EventGroup and CR 603.10a's look-back can tell "at the same time"
-- from "one after the other".
--
-- A BRACKET rather than a parameter, because the sites that know a body is one
-- event are not the sites that record its events: CR 704.3's state-based-action
-- pass drives half a dozen funnels, and CR 701.8's batched destruction runs a CR
-- 616.1 replacement loop per member. Everything either reaches is inside the one
-- event by the same rule.
--
-- Every caller must be able to cite the rule that makes its body a single event;
-- a bracket placed wider than such a rule reaches silently fuses two real events,
-- which is exactly the error this type exists to make impossible.
--
-- The OUTERMOST bracket wins. Nesting keeps the depth above zero, so an inner
-- bracket neither mints a group nor ends the outer one's -- the same posture CR
-- 616.1g's nested entry loops already take, where the outer event owns the inner
-- ones.
--
-- The group is spent on exit whether or not the body recorded anything, so a
-- bracket leaves a gap rather than leaking its group to the next event.
--
-- Not bracketed: CR 510.2's combat damage, CR 608.2f's other multi-object
-- resolution effects, token creation and CR 508.1's attacker declaration, so the
-- events each records are read as a sequence (#441).
simultaneously :: Game a -> Game a
simultaneously body = do
  State.modify' openEventGroup
  result <- body
  State.modify' closeEventGroup
  pure result

-- `simultaneously` for a body that is a pure function of the board rather than a
-- Game action. CR 800.4a's FIRST clause is the caller: leaving the game is not a
-- zone change, so what Pawl.Engine.Departure does there is delete objects and
-- record events, which needs no funnel -- and every object it takes leaves at the
-- same instant. That rule's fourth clause IS a zone change and takes the monadic
-- bracket above instead.
--
-- The two halves are shared with the bracket above rather than restated, so a
-- pure caller cannot come to disagree with a monadic one about what a group is.
simultaneouslyPure :: (GameState -> GameState) -> GameState -> GameState
simultaneouslyPure body = closeEventGroup . body . openEventGroup

-- | CR 701.27a, recorded: one GameEvent.Transformed per permanent that turned
-- over. The ONE writer of that event, shared by the two roads that turn a
-- permanent over -- Pawl.Engine.Resolve's CR 701.27a opcode and
-- Pawl.Engine.Daytime's CR 702.145c/f sweep, which reaches it as a function
-- argument because this module already imports that one.
--
-- Called with the ids that ACTUALLY turned (Pawl.Engine.Game.facesTurned), never
-- with the instruction's victims: CR 701.27c/d/f and CR 702.145b each leave an
-- instruction doing nothing, and CR 603.2 has no event to fire on then.
--
-- AFTER the write, which is what makes CR 701.27e's "immediately after it does
-- so" fall out twice over: the names sampled here are the turned permanent's, and
-- recordEvent's own sample of the battlefield sees the back face's abilities, so a
-- trigger printed on the face just turned to is among the candidates that fire.
--
-- ONE event group for the whole set (CR 608.2f): Moonmist transforms every Human
-- at once, and CR 603.10a must not be able to tell two of them apart.
recordTransformed :: [ObjectId] -> GameState -> GameState
recordTransformed oids gs = case oids of
  [] -> gs
  _ ->
    simultaneouslyPure
      ( \g0 ->
          List.foldl'
            (\g oid -> recordEvent (GameEvent.Transformed (Transformed.MkTransformed oid (Projection.namesOf oid g))) g)
            g0
            oids
      )
      gs

openEventGroup :: GameState -> GameState
openEventGroup gs = gs {GameState.eventGroupDepth = GameState.eventGroupDepth gs + 1}

closeEventGroup :: GameState -> GameState
closeEventGroup gs =
  let depth = GameState.eventGroupDepth gs
      left = if depth == 0 then 0 else depth - 1
   in gs
        { GameState.eventGroupDepth = left,
          GameState.nextEventGroup =
            if left == 0
              then EventGroup.next (GameState.nextEventGroup gs)
              else GameState.nextEventGroup gs
        }

-- CR 119.4: a player may pay an amount of life greater than 0 only if their life
-- total is at least that amount.
--
-- Lives HERE, beside recordEvent, and not in Pawl.Engine.Mana or
-- Pawl.Engine.Cost, because paying life is a recorded game event and this module
-- owns those. All three readers of CR 119.4 then share one reading of it: CR
-- 107.4f's Phyrexian symbol and CR 119.4's own PayLife cost component through
-- Pawl.Engine.Mana and Pawl.Engine.Cost, which import this module, and CR
-- 614.1c's as-enters "you may pay N life" in `apply` below, which could not
-- import Pawl.Engine.Mana at all -- that module imports this one.
--
-- CR 119.4b is answered BEFORE the lookup, not by the `>=` that would usually
-- absorb it: players can ALWAYS pay 0 life, whatever their total and even where
-- an effect says they can't pay life. So a player the map does not hold must not
-- turn a zero payment into an unpayable one -- and a cost with no Phyrexian
-- symbol and no PayLife component asks Pawl.Engine.Mana.payableResolutions' life
-- clause about 0 and nothing else, so that clause cannot change any answer such a
-- cost used to give.
canPayLife :: PlayerId -> Natural -> GameState -> Bool
canPayLife pid n gs =
  n == 0 || case Map.lookup pid (GameState.players gs) of
    Nothing -> False
    Just player -> Player.life player >= toInteger n

-- CR 119.4: the payment is subtracted from the player's life total. A direct
-- subtraction, and the CR 704.5a state-based action that may follow is the
-- existing one in Pawl.Engine.Sba -- paying to exactly 0 is a legal payment, not
-- a barred one.
payLife :: PlayerId -> Natural -> GameState -> GameState
payLife pid n gs =
  -- CR 119.4's own last clause, "in other words, the player loses that much
  -- life", is why the payment is recorded as a life loss like any other. CR
  -- 119.4b's always-payable 0 loses nothing and so records nothing.
  (if n == 0 then id else recordEvent (GameEvent.LifeLost (LifeChange.MkLifeChange pid n)))
    gs
      { GameState.players =
          Map.adjust (\p -> p {Player.life = Player.life p - toInteger n}) pid (GameState.players gs)
      }

-- CR 110.5b: stamp the tapped status onto an entering permanent, the write shared
-- by EntryRewrite.Tapped (CR 614.1d), by the declining half of both
-- EntryRewrite.PayLifeOrTapped and EntryRewrite.RevealOrTapped (CR 614.1c), and
-- by the taking half of EntryRewrite.AsCopy's `tapped` (Vesuva) -- which is why
-- it is one function: those sentences differ in what they charge and not in what
-- they leave on the board.
--
-- ENTERS TAPPED, not "enters, then is tapped". The status goes straight onto the
-- object rather than through `tap` below, so the permanent never transitions from
-- untapped to tapped and nothing watching for that can fire. That is CR 603.2e in
-- as many words: an ability that triggers when a permanent "becomes tapped"
-- doesn't trigger if the permanent enters the battlefield in that state. See the
-- arms in `apply` for why stamping the already-materialized incarnation is
-- observationally the same as minting it tapped.
enterTapped :: ObjectId -> Game ()
enterTapped oid =
  State.modify' $ \gs ->
    let stamp obj = obj {Object.tapped = TapState.Tapped}
     in gs {GameState.objects = Map.adjust stamp oid (GameState.objects gs)}

-- CR 701.26a: turn one permanent sideways from an upright position, and record
-- that it did. The single funnel every route that taps goes through --
-- Pawl.Engine.Cost.tapObject for a cost component, Pawl.Engine.Resolve's
-- Effect.Tap opcode, CR 508.1f's attacker declaration in
-- Pawl.Engine.Combat.declareAttackers, and CR 701.19a's regeneration in `apply`
-- below -- so that a card watching for a tap sees every one of them.
--
-- Rule 701.26a's SECOND sentence is the guard: "only untapped permanents can be
-- tapped", so an already-tapped permanent is left alone and nothing is recorded.
-- The write on its own is idempotent and the guard would be redundant for it; the
-- EVENT is not, and CR 603.2e's "they don't ... retrigger if it persists" is
-- exactly what a second record would break.
--
-- What does NOT come through here is a permanent entering the battlefield tapped
-- (`enterTapped` above, and Pawl.Engine.Resolve.putTapped) -- CR 603.2e's other
-- sentence, and the reason those two stay direct writes.
--
-- CR 110.5 makes tapped a PERMANENT's status, which every caller above already
-- has: a cost's tap candidates, an attacker, a regenerating permanent and the
-- Effect.Tap opcode's victims are all on the battlefield.
tap :: ObjectId -> Game ()
tap oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Just obj
      | Object.tapped obj == TapState.Untapped ->
          State.put
            . recordEvent (GameEvent.BecameTapped oid)
            $ gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)}
    _ -> pure ()

-- The zone change an event describes, if it is one.
movedOf :: GameEvent -> Maybe ZoneChange
movedOf event = case event of
  GameEvent.Moved (Moved.MkMoved zc _) -> Just zc
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.DamagePrevented {} -> Nothing
  GameEvent.StepBegan {} -> Nothing
  GameEvent.SpellCast {} -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  -- The Moved event emitted by the same discard is the zone change; this one
  -- says the move WAS a discard (CR 701.9a).
  GameEvent.Discarded {} -> Nothing
  GameEvent.Drew {} -> Nothing
  -- CR 701.20b: a reveal is never a zone change, even when the card is about to
  -- make one.
  GameEvent.Revealed {} -> Nothing
  GameEvent.AttackerDeclared {} -> Nothing
  GameEvent.BecameBlocking {} -> Nothing
  GameEvent.BlocksDeclared {} -> Nothing
  GameEvent.AttackerBlocked {} -> Nothing
  GameEvent.AttackerUnblocked _ -> Nothing
  -- The Moved event `counter` records alongside this one is rule 701.6a's zone
  -- change; this one only says the move WAS a countering. The Discarded case.
  GameEvent.SpellCountered _ -> Nothing
  GameEvent.HalfUnlocked {} -> Nothing
  GameEvent.TurnedFaceUp _ -> Nothing
  GameEvent.Transformed {} -> Nothing
  GameEvent.BecameDesignated {} -> Nothing
  GameEvent.Evolved _ -> Nothing
  GameEvent.Mentored {} -> Nothing
  GameEvent.Trained _ -> Nothing
  GameEvent.PermanentSacrificed {} -> Nothing
  GameEvent.AbilityTriggered {} -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing
  GameEvent.LifeLost {} -> Nothing
  GameEvent.LifeGained {} -> Nothing
  GameEvent.CountersPut {} -> Nothing
  GameEvent.CountersRemoved {} -> Nothing
  GameEvent.ControlChanged {} -> Nothing
  GameEvent.VentureMarkerEntered {} -> Nothing
  GameEvent.BecameTarget {} -> Nothing
  GameEvent.BecameAttached {} -> Nothing
  GameEvent.LeftTheGame _ -> Nothing
  GameEvent.Milled {} -> Nothing
  GameEvent.Scried _ -> Nothing
  GameEvent.Surveiled _ -> Nothing
  GameEvent.DiceRolled _ -> Nothing
  GameEvent.ClassLevelSet _ -> Nothing
  GameEvent.Plotted _ -> Nothing
  GameEvent.Explored _ -> Nothing
  GameEvent.Exerted _ -> Nothing
  GameEvent.BecameAttacked _ -> Nothing
  GameEvent.AttackersDeclared _ -> Nothing
  GameEvent.BecameTapped _ -> Nothing

-- The damage an event describes, if it is any.
damageOf :: GameEvent -> Maybe DamageEvent
damageOf event = case event of
  GameEvent.DamageDealt ev -> Just ev
  GameEvent.DamagePrevented {} -> Nothing
  GameEvent.Moved {} -> Nothing
  GameEvent.StepBegan {} -> Nothing
  GameEvent.SpellCast {} -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  GameEvent.Discarded {} -> Nothing
  GameEvent.Drew {} -> Nothing
  GameEvent.Revealed {} -> Nothing
  GameEvent.AttackerDeclared {} -> Nothing
  GameEvent.BecameBlocking {} -> Nothing
  GameEvent.BlocksDeclared {} -> Nothing
  GameEvent.AttackerBlocked {} -> Nothing
  GameEvent.AttackerUnblocked _ -> Nothing
  GameEvent.SpellCountered _ -> Nothing
  GameEvent.HalfUnlocked {} -> Nothing
  GameEvent.TurnedFaceUp _ -> Nothing
  GameEvent.Transformed {} -> Nothing
  GameEvent.BecameDesignated {} -> Nothing
  GameEvent.Evolved _ -> Nothing
  GameEvent.Mentored {} -> Nothing
  GameEvent.Trained _ -> Nothing
  GameEvent.PermanentSacrificed {} -> Nothing
  GameEvent.AbilityTriggered {} -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing
  GameEvent.LifeLost {} -> Nothing
  GameEvent.LifeGained {} -> Nothing
  GameEvent.CountersPut {} -> Nothing
  GameEvent.CountersRemoved {} -> Nothing
  GameEvent.ControlChanged {} -> Nothing
  GameEvent.VentureMarkerEntered {} -> Nothing
  GameEvent.BecameTarget {} -> Nothing
  GameEvent.BecameAttached {} -> Nothing
  GameEvent.LeftTheGame _ -> Nothing
  GameEvent.Milled {} -> Nothing
  GameEvent.Scried _ -> Nothing
  GameEvent.Surveiled _ -> Nothing
  GameEvent.DiceRolled _ -> Nothing
  GameEvent.ClassLevelSet _ -> Nothing
  GameEvent.Plotted _ -> Nothing
  GameEvent.Explored _ -> Nothing
  GameEvent.Exerted _ -> Nothing
  GameEvent.BecameAttacked _ -> Nothing
  GameEvent.AttackersDeclared _ -> Nothing
  GameEvent.BecameTapped _ -> Nothing

-- Who revealed what, if the event is a reveal (CR 701.20a).
revealOf :: GameEvent -> Maybe (PlayerId, PC.ProjectedCharacteristics)
revealOf event = case event of
  GameEvent.Revealed (Revealed.MkRevealed pid _ _ snapshot) -> Just (pid, snapshot)
  GameEvent.Moved {} -> Nothing
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.DamagePrevented {} -> Nothing
  GameEvent.StepBegan {} -> Nothing
  GameEvent.SpellCast {} -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  GameEvent.Discarded {} -> Nothing
  GameEvent.Drew {} -> Nothing
  GameEvent.AttackerDeclared {} -> Nothing
  GameEvent.BecameBlocking {} -> Nothing
  GameEvent.BlocksDeclared {} -> Nothing
  GameEvent.AttackerBlocked {} -> Nothing
  GameEvent.AttackerUnblocked _ -> Nothing
  GameEvent.SpellCountered _ -> Nothing
  GameEvent.HalfUnlocked {} -> Nothing
  GameEvent.TurnedFaceUp _ -> Nothing
  GameEvent.Transformed {} -> Nothing
  GameEvent.BecameDesignated {} -> Nothing
  GameEvent.Evolved _ -> Nothing
  GameEvent.Mentored {} -> Nothing
  GameEvent.Trained _ -> Nothing
  GameEvent.PermanentSacrificed {} -> Nothing
  GameEvent.AbilityTriggered {} -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing
  GameEvent.LifeLost {} -> Nothing
  GameEvent.LifeGained {} -> Nothing
  GameEvent.CountersPut {} -> Nothing
  GameEvent.CountersRemoved {} -> Nothing
  GameEvent.ControlChanged {} -> Nothing
  GameEvent.VentureMarkerEntered {} -> Nothing
  GameEvent.BecameTarget {} -> Nothing
  GameEvent.BecameAttached {} -> Nothing
  GameEvent.LeftTheGame _ -> Nothing
  GameEvent.Milled {} -> Nothing
  GameEvent.Scried _ -> Nothing
  GameEvent.Surveiled _ -> Nothing
  GameEvent.DiceRolled _ -> Nothing
  GameEvent.ClassLevelSet _ -> Nothing
  GameEvent.Plotted _ -> Nothing
  GameEvent.Explored _ -> Nothing
  GameEvent.Exerted _ -> Nothing
  GameEvent.BecameAttacked _ -> Nothing
  GameEvent.AttackersDeclared _ -> Nothing
  GameEvent.BecameTapped _ -> Nothing

-- CR 117.5: the events the trigger scan has not yet consumed, WITH the
-- EventGroup each belongs to. Only eventTriggers wants the groups; every other
-- reader takes unscannedEvents below.
--
-- The drop counts ELEMENTS, not groups, and must keep doing so: the two
-- watermarks drain this log at different cadences, so a group can be half
-- consumed.
unscannedGrouped :: GameState -> [LoggedEvent.LoggedEvent]
unscannedGrouped gs =
  Foldable.toList (Seq.drop (Natural.toIntSaturating (GameState.scannedThrough gs)) (GameState.events gs))

-- CR 117.5: the events the trigger scan has not yet consumed.
unscannedEvents :: GameState -> [GameEvent]
unscannedEvents = fmap LoggedEvent.event . unscannedGrouped

-- The events the STATE-BASED ACTION check has not yet consumed -- CR 704.5h's
-- "since the last state-based action check", and the same boundary CR 903.9a
-- names. Pawl.Engine.Sba hands these to Pawl.Engine.Commander.returnable, the
-- other reader.
unscannedSbaEvents :: GameState -> [GameEvent]
unscannedSbaEvents gs =
  fmap LoggedEvent.event (Foldable.toList (Seq.drop (Natural.toIntSaturating (GameState.damageScannedThrough gs)) (GameState.events gs)))

-- CR 704.5h: the damage the state-based-action check has not yet consumed.
unscannedDamage :: GameState -> [DamageEvent]
unscannedDamage gs =
  Maybe.mapMaybe damageOf (unscannedSbaEvents gs)

-- Insert a freshly-built object into `dest` under a new id and timestamp, and
-- return that id. The common tail of changeZone (a moved incarnation) and
-- createTokens (a token from nothing). `mkObj` receives the fresh timestamp so the
-- object records when it entered (CR 613.7d). The Moved event is emitted by the
-- CALLER: only it knows which state the CR 608.2h snapshot must be taken against.
--
-- `position` is CR 401.2's end for a LIBRARY destination and is inert for every
-- other one; see Game.insertIntoZone, which is the only thing that reads it.
placeObject :: PlayerId -> (Timestamp.Timestamp -> Object.Object) -> Zone -> LibraryPosition.LibraryPosition -> Game ObjectId
placeObject pid mkObj dest position = do
  gs <- State.get
  let (newId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj = mkObj ts
      gs3 = gs2 {GameState.objects = Map.insert newId obj (GameState.objects gs2)}
  State.put (Game.insertIntoZone dest position pid newId gs3)
  pure newId

-- CR 114.2: a player gets an emblem with the given abilities, put into the
-- command zone and both owned and controlled by them. CR 613.7a: its entry
-- timestamp is what the projection reads when ordering the continuous effect of
-- any static ability it carries.
--
-- Here rather than in Pawl.Engine.Resolve, because there are two minting sites
-- and only one of them is an opcode: Effect.CreateEmblem is what a CARD says,
-- and Pawl.Engine.Ring.tempt is what CR 701.54c says. An emblem built two ways
-- would be an emblem that could differ.
--
-- Inert per-incarnation fields (an emblem is never tapped, damaged or
-- countered): harmless, nothing reads them here. `enteredUnder = Nothing` is
-- what makes Projection.defaultControllerOf answer the owner, which is CR
-- 109.4c and so CR 114.2's last sentence.
createEmblem :: PlayerId -> Card -> Game ObjectId
createEmblem pid card = do
  -- An emblem's characteristics are effect-defined (CR 114.3), so its entry is
  -- minted here rather than coming from a deck -- interned once for the one
  -- object it backs, and carrying no print-level data because an emblem is not
  -- a card (CR 114.5).
  emblemId <- State.state (Game.intern (Printing.MkPrinting card))
  let mkObj ts =
        Object.MkObject
          { Object.owner = pid,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfEmblem emblemId,
            Object.zone = Zone.Command,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Settled pid,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.classLevel = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False,
            Object.phyrexianLifePaid = 0,
            Object.manaSpent = Mana.MkMana [],
            Object.announcedX = Nothing,
            Object.detainedUntil = Set.empty,
            Object.goadedBy = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty
          }
  placeObject pid mkObj Zone.Command LibraryPosition.defaultValue

-- CR 614: settle a proposed zone change. Nothing means the move does not happen.
-- The typed door changeZoneAttaching below uses, so the funnel itself never cases
-- on a ProposedEvent.
--
-- `asOf` is applyReplacementsIn's: Nothing for a lone move, Just the pre-batch
-- board when this move is one member of a CR 608.2f / 704.3 batch.
--
-- CR 903.9b's rules-based offer is asked here too, once the loop has settled a
-- destination -- see `offerCommandZone`.
--
-- Also reports CR 607.2b's link: the object whose replacement effect is what made
-- the destination exile, or Nothing when the move was headed there on its own
-- instruction. The caller files it once the arriving incarnation has an id.
resolveZoneChange :: Maybe GameState -> ZoneChange -> Game (Maybe ZoneChange, Maybe ObjectId)
resolveZoneChange asOf zc = do
  (outcome, _, exiledBy) <- applyReplacementsFully asOf Set.empty (ProposedEvent.WouldChangeZone zc)
  case outcome >>= Replacement.asZoneChange of
    Nothing -> pure (Nothing, exiledBy)
    Just settled -> do
      redirected <- offerCommandZone settled
      pure (Just redirected, exiledBy)

-- CR 903.9b: "if a commander would be put into its owner's hand or library from
-- anywhere, its owner may put it into the command zone instead". The question;
-- Pawl.Engine.Commander.commandZoneOffer is the condition, and says why the owner
-- is who is asked.
--
-- A RULES step in the funnel rather than a third segment of
-- Pawl.Engine.Replacement.collect, which holds a battlefield permanent's printed
-- static abilities and the floating rows a resolution installed -- both things a
-- CARD carries. Synthesizing a ReplacementEffect.ZoneChangeR from rule 903.9b
-- would put a rules-invented value into the structure whose whole point is that it
-- came from data, and rule 903.9b's "may" has no home on that type in any case:
-- every ZoneChangeR is unconditional, so the prompt would have to be asked from
-- `apply`'s generic arm under a guard on WHICH candidate this is.
-- Pawl.Engine.Cast.legendaryRestrictionOk takes the same posture toward CR 205.4e:
-- a restriction the rulebook states is asked by the engine, not modelled as
-- something a card carries.
--
-- AFTER the CR 616.1 loop and asked of the SETTLED destination, CR 614.6's
-- reading: a printed redirect that already moved this move off a hand or a library
-- is the event that happens, and rule 903.9b has nothing to say about it.
--
-- Not implemented: a place in CR 616.1's ordering, where CR 616.1e leaves the
-- affected player free to pick among the applicable effects and this offer instead
-- always goes last (#2266). Unobservable while no ZoneChangeR in data/cards/ matches a
-- hand or a library -- all six match a graveyard or the stack and redirect to exile
-- -- so no second candidate can be applicable to the same event; a printed redirect
-- naming a hand or a library as the destination it watches (Wheel of Sun and Moon
-- is the shape) would refute that.
--
-- No case on effect identity: the question is a proposed event's destination ZONE
-- and whether its subject is a commander.
offerCommandZone :: ZoneChange -> Game ZoneChange
offerCommandZone zc = do
  gs <- State.get
  case Commander.commandZoneOffer zc gs of
    Nothing -> pure zc
    Just owner -> do
      decision <- Game.choose (Prompt.ReturnCommander (Decide.deciderFor owner gs) owner (ZoneChange.departed zc))
      pure $ case decision of
        -- CR 614.6: the modified event is what happens, so the destination is
        -- rewritten rather than the card being moved twice. changeZoneAttaching
        -- places it under Object.owner, which is rule 903.9b's "its owner" for a
        -- stolen commander too.
        CommandZoneDecision.Returns -> zc {ZoneChange.to = Zone.Command}
        CommandZoneDecision.Leaves -> zc

-- CR 616.1's loop. `Nothing` means the event DOES NOT HAPPEN (CR 615.6, CR
-- 701.19a). A rewrite that cancels an event has already performed its own
-- consequences by the time it returns Nothing.
applyReplacements :: ProposedEvent -> Game (Maybe ProposedEvent)
applyReplacements = applyReplacementsIn Nothing Set.empty

-- CR 608.2f / 704.3: `asOf` is the board a BATCH's candidates are read from --
-- `Just` the state the batch began in, or `Nothing` for the live board. Only the
-- destroy funnel passes `Just` (`destroy`, `destroyInBatch` below), along with
-- the graveyard moves it and Pawl.Engine.Sba's put-into-graveyard batch make
-- through `changeZoneInBatch`. Everything else is a lone event and wants the
-- live board. CR 608.2f and CR 704.3 make a batch ONE event, so CR 614.4 asks
-- which effects existed before the BATCH, not before the member being processed
-- -- otherwise Rest in Peace animated by Opalescence and swept by Day of
-- Judgment answers according to an order CR 608.2f gives nobody the right to
-- decide.
--
-- `asOf` and `batch` both name a batch, and they are OPPOSITES: `asOf` widens
-- the candidate set to include effects of permanents the batch is itself
-- removing, while `batch` NARROWS the copy-target set to exclude permanents
-- entering beside the loop's subject. Deliberately not one parameter: different
-- batches and different readers. Effect.MoveToZone's CR 608.2f batch
-- (Pawl.Engine.Resolve) supplies both, and even there they hold different values
-- -- the pre-batch board against the ids that have already arrived out of it.
--
-- What `asOf` does NOT freeze: the FLOATING store stays live (see
-- Replacement.collect), because CR 614.3 has Replacement.consume spend a one-shot
-- as it applies and a frozen
-- store would hand a spent regeneration shield to the next member of the batch;
-- the loop still RE-COLLECTS every iteration, so CR 616.1f and CR 616.2 are
-- untouched; and `apply`'s writes and Replacement.choose's chooser lookup read the LIVE
-- state. A permanent that ENTERED after the batch began therefore contributes
-- nothing, which is CR 614.4 read the other way. No producer today, so that half
-- is unexercised.
--
-- `batch` is the set of ids entering the battlefield AT THE SAME TIME as this
-- loop's subject, NOT counting the subject itself -- every door subtracts it
-- before passing the set. THREE readers narrow by it, on three different rules.
-- The loop's own candidate filter is the first (CR 614.12; see `loop`), and two
-- of `apply`'s entry arms are the other two:
--
--   * COPY TARGETS. CR 614.12a puts the choice BEFORE the permanent enters, and
--     an as-enters copy may only copy a permanent already ON the battlefield
--     (Clone's "any creature", Copy Enchantment's "any enchantment"), so a sibling
--     entering in the same batch is not there yet at the moment the choice is
--     made. No rule states that exclusion outright: it follows from 614.12a's
--     timing plus the copy effect's own wording. CR 614.13a is the wrong cite for
--     it -- that rule is about an entry effect moving OTHER objects to a
--     different zone, and a copy target never changes zones.
--   * THE AS-ENTERS SACRIFICE (EntryRewrite.SacrificeAnyNumber). Here CR 614.13a
--     is exactly the rule, because a sacrificed permanent does change zones:
--     "You can't choose the object that will become that permanent or any other
--     object entering the battlefield at the same time as that object."
--
-- Both arms exclude the loop's own subject themselves -- legalCopyTargets'
-- `self`, and the sacrifice arm's `entering` -- rather than leaning on this set
-- being subject-free.
--
-- Every `changeZone` door but changeZoneEnteringIn handles one entering object at
-- a time and passes `Set.empty`. There are two non-empty cases, and they reach
-- the same invariant from opposite directions. `createTokens` below materializes
-- every token of a Create BEFORE running any of their entry loops (CR 614.16's
-- doubled count is settled once, up front), so it knows the whole set and passes
-- each token the rest of it. Effect.MoveToZone's CR 608.2f batch
-- (Pawl.Engine.Resolve) moves its
-- members through the funnel one after another, so it ACCUMULATES the set as
-- each arrives: a member still in its old zone is not on the battlefield for a
-- sweep to find, and one that has arrived is. Either way a later member's entry
-- loop would otherwise find its siblings already sitting on the battlefield.
--
-- A simultaneously-entering sibling can reach a later member's entry loop through
-- three channels; only the first needs this explicit exclusion:
--   1. Copy targets -- excluded by `batch`. Two boards observe it, one per
--      producer: kicked Rite of Replication on a Clone, where five token Clones
--      enter at once (Pawl.CopySpec's "none may copy a sibling"), and Rise of the
--      Dark Realms over a graveyard holding a Clone, where the batch is a zone
--      change (Pawl.CopySpec's "a reanimated Clone may not copy a creature
--      reanimated beside it"). Both fail without this exclusion.
--   2. Candidate collection -- excluded by `batch` in `loop`, which drops every
--      candidate a sibling SOURCES. Most entry replacements a PERMANENT carries
--      in this pool are CR 614.1c's self-only `IsSource` (Clone, Primal Plasma,
--      CR 306.5b's loyalty), which no sibling can satisfy, so the exclusion bites
--      on the other-objects forms: Kismet's CR 614.1d "enter tapped" and
--      Corpsejack Menace's CR 614.16 counter doubling. Pawl.ReplacementSpec's
--      "a Corpsejack Menace reanimated beside a modular creature doubles nothing"
--      is the proof, over a Rise of the Dark Realms batch; without it an Arcbound
--      Worker returned beside the Menace enters 2/2 instead of 1/1, and which
--      answer it got depended on the order CR 608.2f gives nobody the right to
--      decide.
--   3. Projection -- a sibling's STATIC ABILITIES would otherwise be visible to
--      every projection read a later member's entry loop makes: Kismet's filter
--      through `applies`, the copy arm's Projection.copiableCharacteristics and
--      isCreatureOf, Projection.controllerOf, and the SACRIFICE arm's offer, which
--      narrows the whole battlefield by a Filter and is the read this pool
--      observes. CR 614.12 does not sanction any of them: a simultaneously-
--      entering sibling's continuous effects do not already exist relative to it.
--      Excluded by GameState.enteringBeside, which runEntry writes for the span of
--      one member's loop and Pawl.Engine.Projection.abilitySources subtracts, so
--      the exclusion covers every read at once rather than one call site at a
--      time. Pawl.ReplacementSpec's "a Wood Elemental reanimated beside Ashaya
--      sacrifices nothing" is the proof: without it Ashaya, Soul of the Wild makes
--      a bystanding Goblin Piker a Forest land, and the Wood Elemental arriving
--      beside it eats the Piker.
applyReplacementsIn :: Maybe GameState -> Set ObjectId -> ProposedEvent -> Game (Maybe ProposedEvent)
applyReplacementsIn asOf batch event = do
  (outcome, _, _) <- applyReplacementsFully asOf batch event
  pure outcome

-- The same loop, answering CR 615.13's second question as well: WHICH prevention
-- effects applied on the way, and how much each of them prevented.
--
-- A separate entry rather than a wider applyReplacementsIn because only the
-- damage class can answer anything but the empty list -- CR 615.1 makes a
-- prevention effect a thing that watches a DAMAGE event -- so every other caller
-- would be threading a value it knows is empty.
applyReplacementsReporting :: Maybe GameState -> Set ObjectId -> ProposedEvent -> Game (Maybe ProposedEvent, [Prevention])
applyReplacementsReporting asOf batch event = do
  (outcome, prevented, _) <- applyReplacementsFully asOf batch event
  pure (outcome, prevented)

-- The loop itself, with both of the side answers its two classes of caller want:
-- CR 615.13's preventions, and CR 607.2b's "which object's replacement effect is
-- what exiled this". Each is empty or Nothing for every event class but one --
-- damage for the first, zone changes for the second -- which is why the three
-- entries above and around it exist rather than one wide return everywhere.
applyReplacementsFully :: Maybe GameState -> Set ObjectId -> ProposedEvent -> Game (Maybe ProposedEvent, [Prevention], Maybe ObjectId)
applyReplacementsFully asOf batch = loop asOf batch Set.empty [] Nothing

loop :: Maybe GameState -> Set ObjectId -> Set CandidateId -> [Prevention] -> Maybe ObjectId -> ProposedEvent -> Game (Maybe ProposedEvent, [Prevention], Maybe ObjectId)
loop asOf batch applied prevented exiledBy event = do
  gs <- State.get
  -- From scratch each iteration: collect against the CURRENT state (or, for a
  -- CR 608.2f batch, the state the batch began in), minus CR 614.5's
  -- already-applied set. Re-collecting is what makes CR 616.2 work.
  let unused candidate = not (Set.member (ReplacementCandidate.identity candidate) applied)
      -- CR 614.12: a permanent entering BESIDE this event's subject contributes
      -- nothing. The rule settles which effects apply from "continuous effects
      -- that already exist and would apply to the permanent", and a sibling's
      -- static abilities do not already exist -- they begin to apply once it is on
      -- the battlefield, which is the moment this event happens too. The subject's
      -- OWN abilities are untouched, because `batch` never holds it (see
      -- applyReplacementsIn).
      --
      -- Filtered by SOURCE, which reaches only the permanent segment of `collect`:
      -- a floating row's source is a spell or ability that has already changed
      -- zones and taken a new id, so no id in a batch of entering permanents can
      -- name one.
      notSibling candidate = not (Set.member (ReplacementCandidate.source candidate) batch)
      fresh = filter (\candidate -> unused candidate && notSibling candidate) (Replacement.applicable asOf gs event)
  case Replacement.highestBucket fresh of
    -- CR 616.1f / 614.6: no candidate remains, so the surviving event happens.
    [] -> pure (Just event, prevented, exiledBy)
    bucket -> do
      picked <- Replacement.choose gs event bucket
      case picked of
        -- Unreachable: highestBucket returns [] for an empty input, so `bucket`
        -- is non-empty and `choose` always picks. Total rather than partial.
        Nothing -> pure (Just event, prevented, exiledBy)
        Just candidate -> do
          -- CR 615.12: the chosen effect is a prevention effect and this damage
          -- can't be prevented (Spider-Punk), so it is APPLIED and prevents none
          -- of it. The event comes back undiminished and no shield is written down
          -- -- "existing damage prevention shields won't be reduced by damage
          -- that can't be prevented" -- while the recursive call below still
          -- records it as applied, which is CR 615.12a's "just once" and the
          -- reason this does not spin. What the application still owes is the
          -- rule's middle clause, "any additional effects they have will take
          -- place", which is `applyInertly`'s whole job.
          --
          -- CR 615.3's use count is skipped too, which no card notices: every
          -- prevention row pawl installs is Uses.Unlimited (Resolve.installDamageRow
          -- says why, and Fog's authored row says Unlimited as well), so the
          -- `consume` this bypasses would have been a no-op anyway.
          --
          -- A SEPARATE fold rather than a flag inside `apply`, because CR 615.12 is
          -- a fact about the (effect, event) PAIR and not about any one rewrite:
          -- `apply`'s arms answer "what does this rewrite do", and the rule stops
          -- the rewrite happening at all while leaving its additional effect
          -- standing. CR 614.1a's replacements never come here, so a Furnace of
          -- Rath still doubles unpreventable damage.
          --
          -- Not implemented: CR 615.5's authored rider, which a row CAN carry now
          -- but which this path still never queues -- `preventionBy` below reports
          -- Nothing off an undiminished event, so nothing reaches the rider
          -- (#1695).
          outcome <- case Replacement.inertPrevention gs candidate event of
            Just rewrite -> applyInertly candidate rewrite event
            Nothing -> apply batch candidate event
          -- CR 615.13: read OUTSIDE `apply`, from the event before and after, so
          -- no arm of that fold has to report anything and none can forget to.
          -- What makes it exact rather than a guess is Replacement.prevents: only a
          -- PREVENTION rewrite's shrinkage is prevention, where CR 614.1a's
          -- SetAmount and Scale shrink an event without preventing a point of it.
          let prevented1 = prevented <> Maybe.maybeToList (Replacement.preventionBy candidate event outcome)
          case outcome of
            Nothing -> pure (Nothing, prevented1, exiledBy)
            Just rewritten -> loop asOf batch (Set.insert (ReplacementCandidate.identity candidate) applied) prevented1 (exiledByAfter candidate event rewritten exiledBy) rewritten

-- CR 607.2b's link, read OUTSIDE `apply` from the event before and after --
-- `preventionBy`'s posture above, for its reason: no arm of that fold has to
-- report anything, so none can forget to. A rewrite that moved a proposed zone
-- change's destination INTO exile is the "replacement event caused by" the row's
-- own object that the rule links to, so the arriving card is that object's and
-- not the resolving ability's, which CR 607.2a's "as a result of an instruction
-- to exile them in the first ability" already excludes it from.
--
-- A later rewrite that moves the destination back OUT of exile clears the link:
-- no card arrives in exile for the rule to speak of. One that leaves an
-- already-exile destination alone changes nothing, since the earlier row is
-- still what caused the exile. Neither of those two arms is exercised, and not
-- by a claim about Magic: every ReplacementEffect.ZoneChangeR in data/cards/
-- names exile as its destination, so no board can stack two of them into a
-- rewrite chain that leaves it again. A printed row naming any other zone --
-- Wheel of Sun and Moon's "into its owner's library instead" is the shape --
-- would refute that and reach both arms. Only the first has a producer.
--
-- `offerCommandZone` is not a second way to reach them either: it runs after this
-- loop has finished, so no candidate is read off its answer, and rule 903.9b only
-- fires on a destination of hand or library.
--
-- No case on effect identity: the question is a proposed event's destination
-- ZONE, and the answer is the row's source object.
exiledByAfter :: ReplacementCandidate -> ProposedEvent -> ProposedEvent -> Maybe ObjectId -> Maybe ObjectId
exiledByAfter candidate before after exiledBy =
  case (fmap ZoneChange.to (Replacement.asZoneChange before), fmap ZoneChange.to (Replacement.asZoneChange after)) of
    (Just old, Just new)
      | new == Zone.Exile, old /= Zone.Exile -> Just (ReplacementCandidate.source candidate)
      | new /= Zone.Exile -> Nothing
    _ -> exiledBy

-- CR 615.12: apply one chosen PREVENTION effect to damage that can't be
-- prevented. The event comes back undiminished -- "those effects won't prevent
-- any damage" -- and what each arm below does is only the rule's middle clause,
-- "any additional effects they have will take place".
--
-- Separate from `apply` rather than a flag threaded through it, because the two
-- answer different questions: `apply`'s arms say what a rewrite DOES, and CR
-- 615.12 is a fact about the (effect, event) pair that stops the rewrite
-- happening at all. Only what survives the rule is here.
--
-- No `consume` and no `setShield` in any arm, which is CR 615.12's last sentence:
-- "existing damage prevention shields won't be reduced by damage that can't be
-- prevented". CR 615.3's use count is skipped with it, which no card notices --
-- `loop` above says why.
--
-- One arm per DamageRewrite constructor, `apply`'s discipline for `apply`'s
-- reason: a new prevention rewrite carrying an additional effect must break the
-- build here rather than silently losing it. The three that `Replacement.prevents`
-- refuses are unreachable, since `Replacement.inertPrevention` answers Just only
-- for a rewrite that prevents.
applyInertly :: ReplacementCandidate -> DamageRewrite.DamageRewrite -> ProposedEvent -> Game (Maybe ProposedEvent)
applyInertly candidate rewrite event = do
  case rewrite of
    -- CR 122.1c's "prevent that damage and remove a shield counter from it". The
    -- removal is the additional effect, and it is AMOUNT-INDEPENDENT -- the rule
    -- removes one counter per application whatever it prevented, here nothing --
    -- so it is the half of the clause that survives unpreventable damage.
    DamageRewrite.PreventRemovingShieldCounter -> removeCounters (ReplacementCandidate.source candidate) CounterKind.Shield 1
    -- CR 615.7's countdown shield has no additional effect of its own; its
    -- remaining amount is exactly the "existing damage prevention shield" the
    -- rule's last sentence protects.
    DamageRewrite.PreventNext _ -> pure ()
    -- Fog's blanket prevention likewise carries nothing beyond the prevention:
    -- CR 615.5's authored rider rides on the CANDIDATE rather than on the
    -- rewrite, so it is `loop`'s business above and not this fold's.
    DamageRewrite.PreventAll -> pure ()
    -- Unreachable: `Replacement.prevents` refuses these three, so no inert
    -- application ever reaches them. CR 614.1a's replacements are not preventions
    -- and are applied in full to unpreventable damage.
    DamageRewrite.SetAmount _ -> pure ()
    DamageRewrite.Scale _ -> pure ()
    DamageRewrite.Redirect _ -> pure ()
  pure (Just event)

-- CR 614.6: apply one chosen effect. Nothing means the event does not happen.
--
-- One arm per ReplacementEffect constructor, same shape as `applies`, so a new
-- constructor breaks the build HERE too. A wildcard fallback would defeat that:
-- an author who teaches `applies` a new arm but forgets this one gets a silent
-- no-op replacement. Every arm below either rewrites its paired event or, for a
-- pair `applies` already excludes, falls through to `pure (Just event)` --
-- unreachable in practice, but present so the match stays total per constructor
-- rather than total by wildcard.
--
-- The same discipline applies one level down, to each arm's inner SUM type
-- (DamageRewrite, DestructionRewrite, EntryRewrite, Scaling), never to the
-- pattern RECORDS, which are read for their fields rather than cased. An arm
-- must CASE on the inner sum, not bind it with `_`: `_` is exhaustive
-- UNCONDITIONALLY, so it raises no build failure and no warning when a new
-- constructor is added, silently treating a real rewrite as a no-op.
--
-- CounterR's and TokenR's arms delegate Scaling whole to `scale`, which is where
-- the exhaustive case lives -- a new Scaling constructor breaks `scale`'s build
-- and both arms' transitively, so casing it again inline would not strengthen
-- anything.
apply :: Set ObjectId -> ReplacementCandidate -> ProposedEvent -> Game (Maybe ProposedEvent)
apply batch candidate event =
  case (ReplacementCandidate.effect candidate, event) of
    (ReplacementEffect.ZoneChangeR (ZoneChangeR.MkZoneChangeR _ toDest), ProposedEvent.WouldChangeZone zc) -> do
      Replacement.consume (ReplacementCandidate.identity candidate)
      pure (Just (ProposedEvent.WouldChangeZone zc {ZoneChange.to = toDest}))
    -- Unreachable: `applies` admits ZoneChangeR only against WouldChangeZone.
    (ReplacementEffect.ZoneChangeR {}, _) -> pure (Just event)
    -- CR 707.5 / 614.1c / 614.12a: the entering object's controller chooses a
    -- permanent to copy, and its copiable characteristics are stamped as this
    -- object's copy snapshot. Writing to the COPIABLE layer (CR 613.1a) is what
    -- makes CR 707.2 fall out for free: a later Clone of this object copies the
    -- stamped values with no further machinery. Clone's "may" is real: Nothing
    -- leaves the object as its printed self (a 0/0, which CR 704.5f then buries).
    --
    -- The class match is on the OUTER tuple, so the INNER `case rewrite of` is
    -- what carries the exhaustiveness obligation -- a wildcard-bound `_` on the
    -- outer pattern would let a new EntryRewrite constructor fall through
    -- silently whenever it happened to pair with WouldEnter.
    (ReplacementEffect.EntryR (EntryR.MkEntryR _ rewrite), ProposedEvent.WouldEnter oid) -> case rewrite of
      EntryRewrite.AsCopy asCopy -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable: the object is materialized on the battlefield before this
          -- loop runs, so controllerOf falls back to its owner. Defensive: make no
          -- unprompted copy choice.
          Nothing -> pure (Just event)
          Just controller -> do
            let decider = Decide.deciderFor controller gs
            -- The eligible set is the CARD's, not this arm's (#1512): Clone says
            -- "any creature" and Copy Enchantment "any enchantment", and the
            -- battlefield walk that supplies "on the battlefield" is
            -- legalCopyTargets'. Read HERE, at CR 614.12a's moment, so an entry
            -- replacement applied earlier in the same batch is already visible.
            let legal = Replacement.legalCopyTargets batch (AsCopy.eligible asCopy) oid gs
            answer <-
              if null legal
                then -- With nothing eligible, declining is the only legal answer
                -- -- a forced selection rather than an elision of options a
                -- player could tell apart -- so the prompt is skipped rather
                -- than asked and overruled. RevealOrTapped's posture below.
                -- One candidate IS still asked: the card's "may" makes
                -- declining a real fork.
                  pure Nothing
                else Game.choose (Prompt.ChooseCopyTarget decider controller oid legal)
            -- FILTERED, NOT TRUSTED (#222). legalCopyTargets is the ONLY thing
            -- enforcing CR 614.12a's same-batch exclusion and the printed noun
            -- phrase, so honouring an unoffered answer would let a Clone copy a
            -- sibling token entering beside it, or a Copy Enchantment copy a
            -- creature.
            let chosen = case answer of
                  Just src | List.elem src legal -> Just src
                  _ -> Nothing
            case chosen of
              Nothing -> pure (Just event)
              Just src2 -> do
                State.modify' $ \g ->
                  -- CR 707.9: the exceptions are applied to the snapshot on the
                  -- way in, so they are part of the copy's own copiable values
                  -- (CR 707.9b) rather than an effect layered over them. Only on
                  -- this branch: a declined copy is no copying process, so its
                  -- exceptions do not happen either.
                  let stamped = Replacement.applyCopyExceptions (AsCopy.exceptions asCopy) (copiedSnapshot src2 g)
                      stamp o = o {Object.bindings = Binding.setCopy stamped (Object.bindings o)}
                   in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
                -- CR 614.1d, inside the same sentence: Vesuva enters TAPPED as a
                -- copy. On this branch alone -- a declined copy is a Vesuva that
                -- entered untapped, since the printed "may" governs both halves
                -- at once.
                Monad.when (AsCopy.tapped asCopy) (enterTapped oid)
                pure (Just event)
      -- CR 614.1c / 208.2b: Primal Plasma's choice of which printed
      -- power/toughness-and-keywords option to become. Written into the COPIABLE
      -- snapshot (applyEntryOption), which is what makes CR 616.2 fall out for
      -- free: a Clone that copies Primal Plasma also copies this ability (CR
      -- 707.5), and the loop's next iteration finds it newly applicable.
      EntryRewrite.ChoiceOf options -> do
        gs <- State.get
        case options of
          -- Malformed card data: an as-enters choice with nothing to choose
          -- from. No-op rather than a partial function, but still consumed -- a
          -- floating one-shot must not survive to apply again.
          [] -> do
            Replacement.consume (ReplacementCandidate.identity candidate)
            pure (Just event)
          first : rest -> do
            picked <-
              if null rest
                then -- One option is not a choice; where the rules leave
                -- nothing to ask, don't prompt.
                  pure first
                else case Projection.controllerOf oid gs of
                  -- Unreachable: the object is materialized on the
                  -- battlefield before this loop runs, so controllerOf falls
                  -- back to its owner. Defensive: make no unprompted choice.
                  Nothing -> pure first
                  Just controller -> do
                    let decider = Decide.deciderFor controller gs
                    answer <- Game.choose (Prompt.ChooseEntryOption decider controller oid options)
                    pure (Replacement.at options answer first)
            Replacement.consume (ReplacementCandidate.identity candidate)
            State.modify' (Replacement.applyEntryOption oid picked)
            pure (Just event)
      -- CR 614.1c: Painter's Servant's as-enters colour choice. Unlike ChoiceOf
      -- above, this is asked every time the entering object has a controller to
      -- ask: CR 105.1's five colours are always all legal and always
      -- distinguishable, so there is no one-option case to elide.
      --
      -- Written to Object.chosenColor, NOT to the copiable snapshot -- see
      -- EntryRewrite.ChooseColor.
      EntryRewrite.ChooseColor -> do
        gs <- State.get
        picked <- case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for ChoiceOf's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. A WEAKER fallback than
          -- ChoiceOf's, and the one place on this path the engine would decide
          -- something: there is no colour the card named to default to, so white
          -- is conjured. It stands only because the branch cannot be reached.
          Nothing -> pure Color.White
          Just controller -> do
            let decider = Decide.deciderFor controller gs
            Game.choose (Prompt.ChooseColor decider controller oid)
        Replacement.consume (ReplacementCandidate.identity candidate)
        State.modify' $ \g ->
          let stamp o = o {Object.chosenColor = Just picked}
           in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
        pure (Just event)
      -- CR 614.1c: Convincing Mirage's as-enters basic land type choice. Asked
      -- every time the entering object has a controller to ask, for
      -- ChooseColor's reason just above: CR 305.6's five basic land types are
      -- always all legal and always distinguishable, so there is no one-option
      -- case to elide.
      --
      -- Written to Object.chosenSubtype, NOT to the copiable snapshot -- see
      -- EntryRewrite.ChooseBasicLandType.
      EntryRewrite.ChooseBasicLandType -> do
        gs <- State.get
        picked <- case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for ChoiceOf's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. The same WEAKER fallback
          -- ChooseColor's arm carries, and for the same reason: no type the card
          -- named to default to, so Mountain is conjured.
          Nothing -> pure Subtype.Mountain
          Just controller -> do
            let decider = Decide.deciderFor controller gs
            Game.choose (Prompt.ChooseBasicLandType decider controller oid)
        Replacement.consume (ReplacementCandidate.identity candidate)
        State.modify' $ \g ->
          let stamp o = o {Object.chosenSubtype = Just picked}
           in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
        pure (Just event)
      -- CR 614.1c: Stuffy Doll's as-enters player choice. The two arms above ask
      -- unconditionally because rules 105.1 and 305.6 fix their offers; this one
      -- asks the BOARD who is available, so it can be elided in the one case those
      -- two never reach -- a single player still in the game, where CR 102.1's
      -- offer has one member and nothing is left to decide.
      --
      -- Written to Object.chosenPlayer, NOT to the copiable snapshot -- see
      -- EntryRewrite.ChoosePlayer.
      EntryRewrite.ChoosePlayer -> do
        gs <- State.get
        let candidates = Game.stillPlaying gs
        picked <- case (Projection.controllerOf oid gs, NonEmpty.nonEmpty candidates) of
          -- Nobody left to choose from, a board CR 104.2a has already ended the
          -- game on. Chooses nobody rather than conjuring a seat, the posture
          -- designateProtector takes for a battle with no legal protector.
          (_, Nothing) -> pure Nothing
          -- Unreachable, and defensive for ChoiceOf's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. Chooses NOBODY rather than
          -- conjuring a seat the way ChooseColor's arm conjures white, because a
          -- player is a real board object where a colour is not -- and CR 101.3
          -- already ignores the share of a later instruction that names nobody.
          (Nothing, _) -> pure Nothing
          (Just controller, Just offer)
            -- One candidate is one outcome, so the options are indistinguishable
            -- and the engine decides nothing by not asking.
            | null (NonEmpty.tail offer) -> pure (Just (NonEmpty.head offer))
            | otherwise -> do
                let decider = Decide.deciderFor controller gs
                answer <- Game.choose (Prompt.ChoosePlayer decider controller oid offer)
                -- Filters rather than trusts the answer, Battle.designateProtector's
                -- posture: an interpreter naming a player who is not in the game
                -- gets the head of the offer instead of an illegal designation.
                pure . Just $
                  if List.elem answer candidates
                    then answer
                    else NonEmpty.head offer
        Replacement.consume (ReplacementCandidate.identity candidate)
        State.modify' $ \g ->
          let stamp o = o {Object.chosenPlayer = picked}
           in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
        pure (Just event)
      -- CR 614.1c with CR 201.4: Null Chamber's as-enters name choices. Unlike
      -- the two arms above, this one has to settle WHO is asked before it can
      -- ask anything: the card names its controller and one opponent, and CR
      -- 101.4 puts their two simultaneous choices in APNAP order.
      --
      -- Written to Object.chosenNames, NOT to the copiable snapshot -- see
      -- EntryRewrite.ChooseCardNames.
      EntryRewrite.ChooseCardNames restriction -> do
        gs <- State.get
        picked <- case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for ChoiceOf's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. Names NOTHING rather than
          -- conjuring a name, which the two arms above cannot do -- their
          -- fallbacks pick from a fixed five, and CR 201.4's offer is every card
          -- in the Oracle card reference.
          Nothing -> pure Set.empty
          Just controller -> do
            -- CR 102.1 makes a player one of the people IN the game, and CR
            -- 104.3a lets one leave at any time -- so the offer is
            -- Game.stillPlaying and not GameState.turnOrder, which keeps a
            -- departed seat.
            let opponents = filter (/= controller) (Game.stillPlaying gs)
            opponent <- case opponents of
              -- CR 102.2: a two-player game leaves exactly one opponent, and
              -- one option is not a choice. The empty case is a game whose
              -- other seats have all left (CR 104.2a) -- nobody to ask, and no
              -- second name.
              [] -> pure Nothing
              [sole] -> pure (Just sole)
              first : second : rest -> do
                let offered = first NonEmpty.:| (second : rest)
                answer <- Game.choose (Prompt.ChooseOpponent (Decide.deciderFor controller gs) controller oid offered)
                -- FILTERED, NOT TRUSTED, the posture Sba.chooseLegendVictims
                -- takes: an answer naming somebody who is not an opponent would
                -- otherwise hand a second name to a player the card never asked,
                -- so it falls back to the head.
                pure (Just (if List.elem answer (NonEmpty.toList offered) then answer else first))
            -- CR 101.4: the active player chooses first, then the rest in turn
            -- order. Both names are chosen as one event, so the order is the
            -- rule's and not the card's reading order.
            let choosers = filter (\pid -> pid == controller || Just pid == opponent) (Game.apnapOrder gs)
                ask pid = Game.choose (Prompt.ChooseCardName (Decide.deciderFor pid gs) pid oid restriction)
            fmap Set.fromList (Monad.mapM ask choosers)
        Replacement.consume (ReplacementCandidate.identity candidate)
        State.modify' $ \g ->
          let stamp o = o {Object.chosenNames = picked}
           in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
        pure (Just event)
      -- CR 306.5b via CR 614.1c: this permanent enters with N counters. Through
      -- Event.putCounters, the CR 122.6 funnel, and NOT a direct write to
      -- Object.counters, because CR 614.16 makes a counter-scaling replacement
      -- apply even when the original event was not itself an effect -- so
      -- Doubling Season has to see these. That nested CR 616.1 loop is why the
      -- counters are placed here rather than folded into the entry event's own
      -- payload. Consumed like every other arm, so CR 614.5 keeps the loop's next
      -- iteration from placing them twice.
      --
      -- CR 614.1c also admits "a number of ... counters ... equal to [something]"
      -- (Undergrowth Scavenger), so the amount is a Quantity and is evaluated ONCE
      -- here (CR 608.2f) rather than inside the CR 122.6 funnel, which would
      -- re-read it per replacement iteration. CR 107.1b clamps a negative result
      -- to zero, which is Integer.toNaturalSaturating.
      --
      -- The permanent is already materialized on the battlefield when this loop
      -- runs (see runEntry), so the CR 613 projection answers for it. The Context
      -- is the ROW's, through Replacement.candidateContext: CR 109.5's "you" is
      -- the row's controller rather than the entrant's, and a floating row's
      -- captured slot bindings ride along, which is what a bare Filter.contextFor
      -- would have dropped; see #2141 for the four callers that still do.
      --
      -- Consumed unconditionally: CR 614.5 is about the row having applied, and a
      -- row whose amount would not evaluate has still applied. Consuming only on
      -- the evaluable branch loops.
      EntryRewrite.WithCounters (WithCounters.MkWithCounters kind quantity) -> do
        gs <- State.get
        let viewOf = Projection.viewWithLastKnown oid gs
            context = Replacement.candidateContext candidate
        Replacement.consume (ReplacementCandidate.identity candidate)
        case Quantity.evaluate viewOf context gs oid quantity of
          Nothing -> pure () -- unevaluable quantity: no counters (Resolve's PutCounters posture)
          Just n -> Monad.when (n > 0) (Monad.void (putOwnCountersIn batch oid kind (Integer.toNaturalSaturating n)))
        pure (Just event)
      -- CR 616.1b / 110.2: Gather Specimens. The entering object's CR 110.2
      -- DEFAULT controller becomes CR 109.5's "you" -- the candidate's
      -- controller, baked when the row was installed -- and that is a permanent
      -- change: the card's "this turn" bounds how long the REPLACEMENT is around
      -- to catch entries, never how long the creature stays yours.
      --
      -- Written to the object rather than to the surviving ProposedEvent, which
      -- is why WouldEnter still carries only an ObjectId. This engine
      -- materializes the entering permanent BEFORE running the entry loop (see
      -- runEntry, and CR 614.12), so the would-be controller is exactly
      -- Projection.controllerOf on the live board. That is also what makes CR
      -- 616.2 fall out: the loop's next iteration re-matches against a board
      -- where the control has already changed, which a value parked on the event
      -- would not show it. All five arms above land on the object for the same
      -- reason.
      --
      -- No prompt, and none is owed: CR 616.1b's rewrite has no choice in it,
      -- and the choice the rule DOES describe -- which of several
      -- control-modifying effects to apply -- is `choose`'s, one level up.
      --
      -- No roster check on `you`, and none is owed: CR 800.4a's second clause
      -- ends a floating control-on-entry row the moment its controller leaves
      -- (Departure.givesControlOnEntryTo), so no row that reaches here can name a
      -- departed player. Pawl.ReplacementSpec's "CR 800.4a a control-on-entry row
      -- ends when its controller leaves the game" is what proves it.
      EntryRewrite.UnderSourceControl -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        case ReplacementCandidate.controller candidate of
          -- CR 109.5 has no answer: a permanent-sourced instance whose source
          -- has left the board. Defensive, with no producer today, and it leaves
          -- the entry alone rather than guessing at a player.
          Nothing -> pure (Just event)
          Just you -> do
            State.modify' $ \gs ->
              let claim obj = obj {Object.enteredUnder = Just you}
               in gs {GameState.objects = Map.adjust claim oid (GameState.objects gs)}
            pure (Just event)
      -- CR 614.1c: Shimatsu the Bloodcloaked. "As this creature enters, sacrifice
      -- any number of permanents. This creature enters with that many +1/+1
      -- counters on it." -- one rewrite, because the count the second sentence
      -- uses is the answer to the first.
      --
      -- Wood Elemental is the same rewrite reading its count somewhere else: "As
      -- this creature enters, sacrifice any number of untapped Forests. Wood
      -- Elemental's power and toughness are each equal to the number of Forests
      -- sacrificed as it entered." No counters, so `kind` is Nothing and the count
      -- is left for the card's characteristic-defining ability (CR 208.2a) to read
      -- back out of Binding.sacrificedCount.
      --
      -- The only entry arm that PERFORMS a game action rather than stamping a
      -- value, which is why this module and not Pawl.Engine.Replacement holds
      -- `apply`: the sacrifice is `sacrifice` below, CR 701.21a's one funnel, and
      -- the counters go through `putCounters`, CR 122.6's. Both are the ordinary
      -- doors, so Rest in Peace redirects a sacrificed permanent and Doubling
      -- Season (CR 614.16) doubles the counters, with nothing written here to
      -- make either happen.
      --
      -- THE ENTERING OBJECT IS NOT A CANDIDATE, nor is a permanent entering
      -- beside it. CR 614.13a states it outright -- "you can't choose the object
      -- that will become that permanent or any other object entering the
      -- battlefield at the same time as that object" -- and that rule reaches
      -- this arm and not the copy arm beside it, because a sacrificed permanent
      -- CHANGES ZONES and a copy target does not (see applyReplacementsIn). This
      -- engine materializes the entering object before running the entry loop
      -- (see runEntry), so `sacrifice` would otherwise happily take it: the
      -- exclusion has to be written, not inherited.
      --
      -- CR 614.12b's COMBINED BUDGET across permanents entering simultaneously
      -- needs no check of its own, and this is where that falls out. The choice
      -- is paid for here, inside the entry loop, before the next member of the
      -- batch runs its own (createTokens' mapM_ over runEntry); the offer above
      -- is re-derived from the live board; so what an earlier choice spent is
      -- not there for a later one to choose again, which is CR 614.13b. No joint
      -- answer the rule allows is lost either -- the player answers each prompt
      -- as it comes, so any partition of the supply among the batch is reachable
      -- in this fixed order, and the rule only ever FORBIDS choices.
      --
      -- Kicked Rite of Replication on a Wood Elemental proves it
      -- (Pawl.ReplacementSpec, "One budget for simultaneous entry costs"): five
      -- token copies, one supply of three Forests. `many` counts what was
      -- CHOSEN, so paying any later than this leaves five 3/3s instead of one.
      --
      -- The argument rests on every entry cost in the pool being "any number",
      -- which is never unpayable. An entry cost whose amount is fixed by the
      -- choice and CAN be unpayable (Frankenstein's Monster's X) would need the
      -- forward check the rule literally describes; no EntryRewrite arm carries
      -- one (#1395).
      EntryRewrite.SacrificeAnyNumber (SacrificeAnyNumber.MkSacrificeAnyNumber criterion kind) -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for the arms above's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. Sacrifices nothing rather than
          -- guessing at a player, and so places no counters and records no count.
          Nothing -> pure (Just event)
          Just controller -> do
            -- The offer narrows the WHOLE battlefield by a Filter, so it is also
            -- where CR 614.12's other half bites: a bystander is judged with the
            -- batch's static abilities suppressed (GameState.enteringBeside),
            -- which is channel 3 of applyReplacementsIn's note and not this
            -- exclusion.
            let entering oid2 = oid2 == oid || Set.member oid2 batch
                offered = filter (not . entering) (Replacement.sacrificeCandidates controller (Just oid) criterion gs)
            chosen <-
              -- Where the rules leave nothing to ask, don't prompt: with no
              -- candidate the empty set is the only answer. ONE candidate is
              -- still asked, unlike Prompt.ChooseSacrifices' elision -- "any
              -- number" leaves two distinguishable answers there.
              if null offered
                then pure Set.empty
                else do
                  let decider = Decide.deciderFor controller gs
                  answer <- Game.choose (Prompt.ChooseAnyNumberToSacrifice decider controller oid offered)
                  -- FILTERED, NOT TRUSTED (#222): an answer naming a permanent
                  -- that was never offered would otherwise sacrifice it and pay
                  -- for a counter with it.
                  pure (Set.intersection answer (Set.fromList offered))
            Monad.mapM_ (sacrifice controller) (Set.toAscList chosen)
            -- "That many": the permanents CHOSEN, which is also the permanents
            -- sacrificed -- every member was on the battlefield under this
            -- player's control when it was offered, and nothing between there and
            -- here moves one.
            let many = Natural.length chosen
            -- Recorded on the entering permanent BEFORE the counters, and
            -- unconditionally, so a card that reads the count rather than
            -- spending it on counters has it (Wood Elemental). See
            -- Binding.sacrificedCount for why 0 is recorded rather than left
            -- absent.
            State.modify' $ \gs2 ->
              let note obj = obj {Object.bindings = Map.insert Binding.sacrificedCount (Binding.toAmount many) (Object.bindings obj)}
               in gs2 {GameState.objects = Map.adjust note oid (GameState.objects gs2)}
            Monad.mapM_ (\k -> putOwnCountersIn batch oid k many) kind
            pure (Just event)
      -- CR 702.155b / 714.3b: read ahead's two intrinsic abilities, applied as
      -- one rewrite -- choose a number between one and this Saga's final chapter
      -- number, then enter with that many lore counters.
      --
      -- THE BOUND is CR 714.2d's final chapter number, read off the entering
      -- permanent's own projection rather than off the row: the row is minted
      -- from the projection by Pawl.Engine.Saga.entryReplacementsOf, but a layer
      -- effect could have changed which chapter abilities the Saga has since, and
      -- rule 714.2d asks about the abilities it HAS.
      --
      -- The counters go through putOwnCountersIn, CR 122.6's funnel, so CR 614.16
      -- applies to them exactly as it does to riot's counter below.
      EntryRewrite.ReadAhead -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        let bound = Saga.finalChapterOf (Projection.project oid gs)
        picked <- case (Projection.controllerOf oid gs, bound) of
          -- CR 714.2d's abilityless Saga: "between one and 0" names an EMPTY
          -- range, so there is nothing to ask and no counter to place. No
          -- printing reaches it -- it needs a read-ahead Saga an effect has
          -- stripped of its chapter abilities -- and this is rule 714.2d read
          -- honestly rather than a question withheld.
          (_, 0) -> pure 0
          -- Unreachable, and defensive for the arms above's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. Chooses the FLOOR of rule
          -- 702.155b's range rather than conjuring a chapter, which is also
          -- Replay.defaultAnswer's answer.
          (Nothing, _) -> pure 1
          (Just controller, _)
            -- CR 702.155b's range holds one number, so nothing is left to decide
            -- -- EntryRewrite.ChoiceOf's elision, and no player's choice is being
            -- made. At two or more the chapters are distinguishable (entering on
            -- II skips I), so the prompt is owed.
            | bound == 1 -> pure 1
            | otherwise -> do
                let decider = Decide.deciderFor controller gs
                answer <- Game.choose (Prompt.ChooseReadAheadChapter decider controller oid bound)
                -- CLAMPED rather than trusted, Prompt.ChoosePaidEnergy's posture:
                -- rule 702.155b names a closed range and an answerer naming a
                -- chapter outside it would put the Saga past its final chapter
                -- (CR 704.5s) or leave it on none.
                pure (max 1 (min bound answer))
        Monad.when (picked > 0) (Monad.void (putOwnCountersIn batch oid CounterKind.Lore picked))
        pure (Just event)
      -- CR 702.136a: riot. "You may have this permanent enter with an additional
      -- +1/+1 counter on it. If you don't, it gains haste."
      --
      -- NEVER ELIDED. A +1/+1 counter and haste are two outcomes a player can
      -- tell apart on any board -- the whole reason the keyword exists -- so this
      -- prompt is raised every time the entering object has a controller to ask,
      -- the posture ChooseColor's arm takes and not ChoiceOf's one-option
      -- elision.
      --
      -- The counter goes through putCounters, CR 122.6's funnel, exactly as the
      -- WithCounters arm above does, so CR 614.16 applies to it and Doubling
      -- Season sees riot's counter.
      --
      -- The haste is a STORED continuous effect (CR 611.2) rather than a stamp on
      -- the object: rule 702.136a says the permanent "gains haste" and names no
      -- end, which is CR 611.2a's rest-of-the-game duration, and a stored effect
      -- is what puts the grant in CR 613.1f's layer 6 with a timestamp for
      -- Humility and every other ability-remover to be ordered against. Its
      -- source is the entering permanent itself, the object whose riot ability
      -- generated it (CR 113.7).
      --
      -- The timestamp is a FRESH one, taken here. CR 613.7a would give a static
      -- ability's continuous effect the timestamp of the object the ability is
      -- on, which for this one is the permanent that entered a moment ago and has
      -- the newest object timestamp on the board -- so the two coincide at every
      -- ordering question a card in this pool can ask.
      EntryRewrite.Riot -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for the arms above's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. Neither half is applied rather
          -- than one being chosen unasked -- the engine makes no player's choice,
          -- and both halves here are choices.
          Nothing -> pure (Just event)
          Just controller -> do
            let decider = Decide.deciderFor controller gs
            answer <- Game.choose (Prompt.ChooseRiot decider controller oid)
            case answer of
              OptionalDecision.Exercises -> Monad.void (putOwnCountersIn batch oid CounterKind.PlusOnePlusOne 1)
              OptionalDecision.Declines ->
                State.modify' $ \gs2 ->
                  -- CR 611.2a: "gains haste" with no stated end lasts until the
                  -- game does. Armed through Pawl.Engine.Expiry rather than naming
                  -- Expiry.Never here, the posture Resolve's storing arms take;
                  -- Indefinite always arms, so the Nothing branch is unreachable
                  -- and is written out only because arm is total over Duration.
                  -- No bindings: rule 702.136a's riot is a replacement's choice,
                  -- not a resolution, so its duration can name no slot.
                  case Expiry.arm Map.empty controller oid Duration.Indefinite gs2 of
                    Nothing -> gs2
                    Just expiry ->
                      let (ts, gs3) = Game.freshTimestamp gs2
                          eff =
                            ContinuousEffect.MkContinuousEffect
                              { ContinuousEffect.source = oid,
                                ContinuousEffect.timestamp = ts,
                                ContinuousEffect.expiry = expiry,
                                ContinuousEffect.modification = Modification.GainKeyword Keyword.Type.Haste,
                                -- CR 611.2c: a fixed set of one, settled here --
                                -- the permanent that entered, not whatever
                                -- matches a filter later.
                                ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
                              }
                       in gs3 {GameState.continuousEffects = eff : GameState.continuousEffects gs3}
            pure (Just event)
      -- CR 702.98a / 614.1c: unleash. "You may have this permanent enter with an
      -- additional +1/+1 counter on it." Riot's arm with the declining half
      -- deleted: rule 702.98a states no consequence for declining, so Declines
      -- writes nothing.
      --
      -- NEVER ELIDED, for riot's reason: the counter is what turns rule 702.98a's
      -- second static ability on, so the two answers are a bigger creature that
      -- cannot block against a smaller one that can.
      --
      -- Through putCounters, CR 122.6's funnel, exactly as riot's is, so CR 614.16
      -- applies and Doubling Season sees unleash's counter.
      EntryRewrite.Unleash -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for the reason riot's arm gives above.
          Nothing -> pure (Just event)
          Just controller -> do
            let decider = Decide.deciderFor controller gs
            answer <- Game.choose (Prompt.ChooseUnleash decider controller oid)
            case answer of
              OptionalDecision.Exercises -> Monad.void (putOwnCountersIn batch oid CounterKind.PlusOnePlusOne 1)
              OptionalDecision.Declines -> pure ()
            pure (Just event)
      -- CR 702.54a via CR 614.1c: bloodthirst N on Bloodrage Vampire. The
      -- WithCounters arm above with the kind fixed at +1/+1 by rule 702.54a, and
      -- through putCounters for that arm's reason -- CR 122.6's funnel is what
      -- makes CR 614.16 reach these, so Doubling Season sees bloodthirst's
      -- counters as it sees riot's.
      --
      -- NO CONDITION HERE. Rule 702.54a's "if an opponent was dealt damage this
      -- turn" is asked by Pawl.Engine.Replacement.admitsEntry, which is why the
      -- row reaching this point already means the condition held; see that
      -- function for why the question is asked there rather than here.
      --
      -- No prompt, and none is owed: rule 702.54a states no choice.
      EntryRewrite.Bloodthirst n -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        Monad.void (putOwnCountersIn batch oid CounterKind.PlusOnePlusOne n)
        pure (Just event)
      -- CR 614.1d / 110.5b: "This permanent enters tapped" (Zof Bloodbog's land,
      -- Headless Skaab's creature -- the arm gates on no card type). CR 110.5b
      -- has a permanent enter untapped "unless a spell or ability says otherwise",
      -- and this is the permanent's own ability saying otherwise -- which is why
      -- it is here and not in EntryRiders, the rider a SPELL or ability writes
      -- when it puts something onto the battlefield tapped. A land played as CR
      -- 305.1's special action passes through no effect at all, so only this route
      -- reaches it.
      --
      -- ENTERS TAPPED, not "enters, then is tapped", and the difference is the
      -- whole point of the arm: the status is stamped straight onto the object
      -- rather than routed through the tap funnel, so the permanent never
      -- transitions from untapped to tapped and nothing watching for that can
      -- fire -- which is CR 603.2e's own sentence about a permanent that enters in
      -- that state. Stamping the ALREADY-MATERIALIZED incarnation is observationally the same
      -- as minting it tapped, on UnderSourceControl's footing above: runEntry
      -- finishes before the Moved event is recorded, so no trigger scan and no
      -- state-based action can see the interim object.
      --
      -- No prompt, and none is owed: rule 614.1d's rewrite has no choice in it.
      EntryRewrite.Tapped -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        enterTapped oid
        pure (Just event)
      -- CR 614.1c with CR 119.4: "As this land enters, you may pay N life. If you
      -- don't, it enters tapped" (Razorgrass Field). The arm above's write, with a
      -- price on avoiding it -- so declining here leaves exactly the board Zof
      -- Bloodbog's unconditional sentence leaves, down to the same stamp.
      --
      -- NEVER ELIDED where the payment is possible. Life against an untapped land
      -- is a real fork on any board -- it is why the cycle is printed -- so the
      -- prompt is raised every time the entering object has a controller who can
      -- afford it.
      --
      -- Through payLife, CR 119.4's own door, and NOT a subtraction from the life
      -- total: rule 119.4's last clause makes the payment a life loss like any
      -- other, so a card watching for life loss sees this one.
      EntryRewrite.PayLifeOrTapped n -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for the arms above's reason: the object is
          -- materialized on the battlefield before this loop runs, so controllerOf
          -- falls back to its owner. Tapped rather than untapped, because with
          -- nobody to ask nobody paid -- which is the card's own stated default,
          -- "if you don't, it enters tapped".
          Nothing -> do
            enterTapped oid
            pure (Just event)
          Just controller -> do
            -- CR 119.4: a player may pay N life only if their life total is at
            -- least N. Below that, declining is the only legal answer -- a forced
            -- selection, not an elision of options a player could tell apart --
            -- so the prompt is skipped rather than asked and overruled. CR 119.4b
            -- keeps 0 payable at any total, so a zero amount is still asked.
            answer <-
              if canPayLife controller n gs
                then Game.choose (Prompt.ChoosePayLifeOnEntry (Decide.deciderFor controller gs) controller oid n)
                else pure OptionalDecision.Declines
            case answer of
              OptionalDecision.Exercises -> State.modify' (payLife controller n)
              OptionalDecision.Declines -> enterTapped oid
            pure (Just event)
      -- CR 614.1c with CR 701.20a: "As this land enters, you may reveal a Kithkin
      -- card from your hand. If you don't, this land enters tapped" (Rustic
      -- Clachan). The arm above with a different price -- showing a card instead
      -- of spending life -- and the same declining half, down to the same stamp.
      --
      -- Through `reveal`, CR 701.20a's own funnel, so the shown card reaches the
      -- public log with the projection a player at the table would see. Nothing
      -- moves and nothing changes (CR 701.20b), which is why the paying half is
      -- the reveal alone: this is not a cost, so no CR 118 payment and no
      -- rollback is involved.
      --
      -- NEVER ELIDED where a matching card is held. Showing a card nobody could
      -- have made you show, against a land that comes in tapped, is a real fork on
      -- any board -- it is why the cycle is printed.
      EntryRewrite.RevealOrTapped filter_ -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for the arm above's reason: the object is
          -- materialized on the battlefield before this loop runs, so controllerOf
          -- falls back to its owner. Tapped rather than untapped, because with
          -- nobody to ask nobody revealed -- the card's own stated default.
          Nothing -> do
            enterTapped oid
            pure (Just event)
          Just controller -> do
            -- The hand is read HERE, at CR 614.12a's moment, and not off any
            -- earlier snapshot: an entry replacement applied before this one can
            -- have moved a card (CR 614.13), and the offer must be what the
            -- player actually holds as the choice is made.
            let candidates = Replacement.revealableFromHand controller filter_ gs
            answer <- case NonEmpty.nonEmpty candidates of
              -- Holding nothing that matches, declining is the only legal answer
              -- -- a forced selection rather than an elision of options a player
              -- could tell apart -- so the prompt is skipped rather than asked and
              -- overruled.
              Nothing -> pure Nothing
              Just offered -> Game.choose (Prompt.ChooseRevealOnEntry (Decide.deciderFor controller gs) controller oid offered)
            -- FILTERED, NOT TRUSTED, AsCopy's posture above: this list is the only
            -- thing enforcing the printed criterion, so honouring an unoffered
            -- answer would let any card in hand keep the land untapped. A
            -- REGRESSION FENCE rather than proven behaviour -- the offer is the
            -- only thing an ordinary game answers from, so it takes a transcript
            -- naming a card that was never offered to reach the refusal.
            case answer of
              Just shown | List.elem shown candidates -> reveal RevealCause.Ordinary controller shown
              _ -> enterTapped oid
            pure (Just event)
      -- CR 712.13a via CR 702.145b's first static ability: "if it is night and
      -- this permanent is represented by a double-faced card, it enters
      -- transformed." The one producer CR 616.1d's bucket has.
      --
      -- WHICH face is Card.backFace, the same answer CR 712.14a's rider gets in
      -- changeZoneEntering: the card is read, never cased on.
      --
      -- Object.face and NOT Game.turnFaceOver, which is the write for CR 701.27a's
      -- transform: nothing turned over here. So Object.turnedOverAt stays Nothing,
      -- and CR 701.27f's "has already transformed since" (Pawl.Engine.Resolve's
      -- alreadyTurnedFor) still reads the permanent as one that has not.
      --
      -- Stamped on the ALREADY-MATERIALIZED incarnation, Tapped's footing above:
      -- runEntry finishes before the Moved event is recorded, so no trigger scan
      -- and no state-based action can see the interim front face.
      --
      -- WHICH enters-the-battlefield trigger fires does not currently tell this
      -- apart from the CR 702.145c sweep turning the permanent over a settle later
      -- (Pawl.Engine.Daytime.turnDue), and that is a defect in the scan rather
      -- than in this arm: the scan reads the permanent's abilities off the live
      -- board at settle, by which time the sweep has already run (#1548). What
      -- this arm fixes regardless is the face itself, which the rule is about.
      --
      -- Not implemented: CR 712.13a's second sentence, an instant or sorcery back
      -- face sending the spell to its owner's graveyard rather than the
      -- battlefield. The write is unguarded, where Game.turnFaceOver goes through
      -- Card.turnedOver and CR 701.27d's refusal with it, so such a face would be
      -- shown on the battlefield rather than merely fail to reach the graveyard
      -- (#1547).
      EntryRewrite.EntersTransformed -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case fmap Face.name (Game.cardOf oid gs >>= Card.backFace) of
          -- Unreachable: Replacement.admitsEntry admits this rewrite only where
          -- Card.backFace answers. Defensive: leave the front face up.
          Nothing -> pure (Just event)
          Just name -> do
            State.modify' $ \g ->
              g {GameState.objects = Map.adjust (\obj -> obj {Object.face = Just name}) oid (GameState.objects g)}
            pure (Just event)
      -- CR 614.1c: "As [this permanent] enters, [do something]" -- Monstrous
      -- War-Leech's "mill four cards". The one entry rewrite that runs an EFFECT
      -- rather than changing what the permanent is; see #1416.
      --
      -- QUEUED, not run here, and the module boundary is the reason: this module
      -- is below Pawl.Engine.Resolve and cannot run a card's effects, exactly as
      -- Pawl.Engine.Damage cannot run CR 615.5's rider. So the effects go onto
      -- GameState.pendingEntryEffects with the environment they need and
      -- Resolve.runEntryEffects performs them; see that field for where it drains
      -- and what the deferral costs.
      --
      -- CR 109.5's "you" is the ENTERING object's controller, read live off the
      -- board rather than off the candidate -- Bloodthirst's and
      -- SacrificeAnyNumber's posture, and what makes `readsApplier` answer False
      -- for this arm. Read NOW rather than at the drain, because a later rewrite
      -- in the same CR 616.1 loop may hand the permanent to someone else
      -- (UnderSourceControl).
      --
      -- NO CONDITION asked here. Rule 702.54a's is Bloodthirst's and lives in
      -- Replacement.admitsEntry; this arm's producer states its own on CR 604.2's
      -- clause, which Projection.replacementsOf asked before the row was
      -- collected.
      EntryRewrite.RunEffects effects -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for the reason riot's arm gives above: the
          -- object is materialized on the battlefield before runEntry, so
          -- controllerOf falls back to its owner. Runs nothing rather than
          -- picking a performer.
          Nothing -> pure (Just event)
          Just controller -> do
            State.modify' $ \g ->
              g
                { GameState.pendingEntryEffects =
                    GameState.pendingEntryEffects g
                      Seq.|> PendingEntryEffect.MkPendingEntryEffect
                        { PendingEntryEffect.object = oid,
                          PendingEntryEffect.controller = controller,
                          PendingEntryEffect.effects = effects
                        }
                }
            pure (Just event)
    -- Unreachable: `applies` admits EntryR only against WouldEnter.
    (ReplacementEffect.EntryR {}, _) -> pure (Just event)
    (ReplacementEffect.DamageR damageR@(DamageR.MkDamageR _ rewrite _), ProposedEvent.WouldDealDamage de) -> case rewrite of
      -- CR 615.6: a prevented event never happens -- it is not marked, not
      -- drained, and never recorded, so no deathtouch bit exists for the CR
      -- 704.5h SBA to read.
      DamageRewrite.PreventAll -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        pure Nothing
      -- CR 122.1c: "prevent that damage and remove a shield counter from it". The
      -- WHOLE event goes, whatever its amount -- "if a permanent that would be
      -- dealt damage has more than one shield counter on it, that damage is
      -- prevented and only one shield counter is removed" -- so this is
      -- PreventAll's arm plus the removal, and never PreventNext's arithmetic.
      --
      -- The counter comes off the DAMAGED permanent, which is this candidate's own
      -- source: Replacement.admitsRecipient admits the event only when the two are
      -- the same object.
      --
      -- NOT `consume`, for the reason `collect` gives: a permanent's candidate has
      -- no use to spend. The removal is what spends this one instead, since
      -- Projection.shieldOf mints the pair only while a counter is there and CR
      -- 616.1f re-collects.
      DamageRewrite.PreventRemovingShieldCounter -> do
        removeCounters (ReplacementCandidate.source candidate) CounterKind.Shield 1
        pure Nothing
      -- CR 615.7's shield covers as much of THIS event as it has left, and
      -- whatever it could not cover survives as a smaller event of the same
      -- source, recipient and riders. Nothing when it covered all of it, which
      -- is CR 615.6: a prevented event never happens.
      --
      -- No choice is made here, and none is owed: within one event CR 615.7
      -- leaves nothing to decide, since the prevention is neither optional nor
      -- divisible by anyone's say-so. The choice the rule DOES describe -- which
      -- of several simultaneous events the shield covers -- is asked one level
      -- up, in resolveDamageBatch.
      --
      -- NOT `consume`. That spends a row per APPLICATION, while CR 615.7's unit
      -- is the amount of damage rather than the number of events or sources
      -- dealing it. `setShield` writes the remainder back and drops the row at 0.
      DamageRewrite.PreventNext remaining -> do
        let amount = DamageEvent.amount de
            -- Both subtractions below are total on Natural: `prevented` is a min
            -- of the two operands, so it is no greater than either.
            prevented = min remaining amount
        Replacement.setShield (ReplacementCandidate.identity candidate) damageR (remaining - prevented)
        if prevented >= amount
          then pure Nothing
          else pure (Just (ProposedEvent.WouldDealDamage de {DamageEvent.amount = amount - prevented}))
      -- CR 614.1a's "instead" with a flat amount (Galvanic Blast). Only the
      -- AMOUNT is rewritten, and that is the rule rather than economy: a
      -- replaced damage event keeps its source, its recipient and every
      -- deal-time rider it was proposed with.
      DamageRewrite.SetAmount n -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        pure (Just (ProposedEvent.WouldDealDamage de {DamageEvent.amount = n}))
      -- CR 614.1a: Furnace of Rath's "it deals double that damage ... instead".
      -- Through the same `scale` the counter and token rewrites use, so a
      -- doubling means one thing across every event class.
      DamageRewrite.Scale scaling -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        pure (Just (ProposedEvent.WouldDealDamage de {DamageEvent.amount = Replacement.scale scaling (DamageEvent.amount de)}))
      -- CR 614.9: a redirection effect replaces the damage's RECIPIENT with
      -- another and nothing else -- "the same damage dealt to another battle,
      -- creature, planeswalker, or player". Every other field rides along for
      -- SetAmount's reason: a replaced damage event keeps its source, its amount
      -- and every deal-time rider it was proposed with.
      --
      -- The rule's guard says the effect DOES NOTHING, not that it is
      -- inapplicable -- the CR 615.12 distinction `inertPrevention` already
      -- draws -- so a dead destination consumes the candidate, is marked applied
      -- by the loop, and hands the event straight back. Returning Nothing here
      -- would DROP the damage, which is weaker than printed: the rule leaves it
      -- on its original recipient.
      DamageRewrite.Redirect dest -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        pure . Just $ case Replacement.redirectDestination gs dest of
          Nothing -> event
          Just live -> ProposedEvent.WouldDealDamage de {DamageEvent.target = live}
    -- Unreachable: `applies` admits DamageR only against WouldDealDamage.
    (ReplacementEffect.DamageR {}, _) -> pure (Just event)
    -- CR 701.19a / 122.1c: under either arm the DESTRUCTION does not happen, so
    -- nothing downstream of it (a put-into-graveyard, and therefore Rest in Peace's
    -- redirect) ever runs. What each does INSTEAD is all that separates them, and
    -- the two do not overlap: regeneration removes marked damage, taps the
    -- permanent and removes it from combat, where a shield counter's removal does
    -- none of those.
    (ReplacementEffect.DestructionR rewrite, ProposedEvent.WouldBeDestroyed oid _ _) -> case rewrite of
      DestructionRewrite.Regenerate -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        -- Rule 701.19a's three instructions in the order it prints them: remove
        -- all marked damage, tap it, then remove it from combat. The ORDER is
        -- observable now that the middle one records an event -- a trigger
        -- gathered off the tap reads the board as it stands, and doing the combat
        -- removal first would show it a creature already out of combat.
        --
        -- Three statements rather than one write because `tap` is a Game action;
        -- the funnel is what makes a regeneration a becomes-tapped event like any
        -- other route.
        State.modify' (\gs -> gs {GameState.objects = Map.adjust (\obj -> obj {Object.damage = 0}) oid (GameState.objects gs)})
        tap oid
        State.modify' (Game.removeFromCombat oid)
        pure Nothing
      -- CR 122.1c: "instead remove a shield counter from it". The destruction does
      -- not happen, and NONE of regeneration's own work above does either --
      -- "removing a shield counter in this way isn't the same as regenerating a
      -- creature", so the permanent keeps its marked damage, its tap state and its
      -- place in combat.
      --
      -- The counter comes off `oid`, which Replacement.applies has already made
      -- this candidate's own source. `consume` is skipped for the damage arm's
      -- reason.
      DestructionRewrite.RemoveShieldCounter -> do
        removeCounters oid CounterKind.Shield 1
        pure Nothing
    -- Unreachable: `applies` admits DestructionR only against WouldBeDestroyed.
    (ReplacementEffect.DestructionR _, _) -> pure (Just event)
    -- CR 122.6/614.16: Hardened Scales/Doubling Season scale a counter placement.
    (ReplacementEffect.CounterR (CounterR.MkCounterR _ scaling), ProposedEvent.WouldPutCounters cause oid kind n) -> do
      Replacement.consume (ReplacementCandidate.identity candidate)
      pure (Just (ProposedEvent.WouldPutCounters cause oid kind (Replacement.scale scaling n)))
    -- CR 122.1: the same rewrite against a player -- Vorinclex, Monstrous Raider's
    -- "on a permanent or player". Through the same `scale`, so the two recipients
    -- cannot disagree about what doubling or halving means.
    --
    -- The event SURVIVES at a scaled count of zero rather than being cancelled
    -- here, so CR 616.2's next iteration still sees it and CR 614.5 still spends
    -- this row; putPlayerCounters is what declines to write a zero.
    (ReplacementEffect.CounterR (CounterR.MkCounterR _ scaling), ProposedEvent.WouldPutPlayerCounters cause pid kind n) -> do
      Replacement.consume (ReplacementCandidate.identity candidate)
      pure (Just (ProposedEvent.WouldPutPlayerCounters cause pid kind (Replacement.scale scaling n)))
    -- Unreachable: `applies` admits CounterR only against the two counter events.
    (ReplacementEffect.CounterR {}, _) -> pure (Just event)
    -- CR 614.16: Doubling Season scales token creation.
    (ReplacementEffect.TokenR (TokenR.MkTokenR _ scaling), ProposedEvent.WouldCreateTokens pid card n) -> do
      Replacement.consume (ReplacementCandidate.identity candidate)
      pure (Just (ProposedEvent.WouldCreateTokens pid card (Replacement.scale scaling n)))
    -- Unreachable: `applies` admits TokenR only against WouldCreateTokens.
    (ReplacementEffect.TokenR {}, _) -> pure (Just event)
    -- CR 614.1b / 614.10: a skip is "instead of doing X, do nothing", so the
    -- step or phase simply does not begin. Nothing is done first, unlike
    -- DamageRewrite.PreventAll's sibling arm: a skip has no consequence of its
    -- own to perform before it cancels.
    --
    -- The obligation the doc above places on every arm -- case on the inner sum
    -- rather than bind it with `_` -- has nothing to bind here: PhaseR carries a
    -- pattern and no rewrite, because CR 614.1b leaves a skip only one possible
    -- outcome. The day a PhaseRewrite exists, this arm owes it a case.
    --
    -- CR 614.10a's arithmetic -- two skip effects mean two occurrences skipped,
    -- one per instance -- falls out of the floating store's SHAPE rather than
    -- out of care taken here. Two Fatigues prepend two ActiveReplacements, and a
    -- list of instances with distinct timestamps cannot coalesce the way a Set of
    -- patterns or a Boolean flag would; Replacement.consume deletes by (source,
    -- timestamp), so it spends exactly the one that applied; and returning
    -- Nothing ENDS the CR 616.1 loop, so no second skip can be spent on the same
    -- step.
    --
    -- The occurrence skipped is the one the PATTERN named, which for Stonehorn
    -- Dignitary is a whole combat phase rather than a step of one. Nothing here
    -- has to know that: Engine.runStep raises the phase question exactly once per
    -- phase, so a whole-phase skip gets exactly one chance to apply.
    --
    -- Eon Hub's PhaseR reaches the same arm and consumes nothing: it is a
    -- permanent's static ability, so its CandidateId is OfPermanent and `consume`
    -- is a no-op for it. It is the store, not this arm, that tells the two apart.
    (ReplacementEffect.PhaseR _, ProposedEvent.WouldBeginPhase _ _) -> do
      Replacement.consume (ReplacementCandidate.identity candidate)
      pure Nothing
    -- Unreachable: `applies` admits PhaseR only against WouldBeginPhase.
    (ReplacementEffect.PhaseR _, _) -> pure (Just event)
    -- CR 614.1e / 702.37b: megamorph's "as this permanent is turned face up, put
    -- a +1/+1 counter on it", applied WHILE the permanent turns over (CR 708.11)
    -- because FaceDown.performTurnFaceUp raises this event there and nowhere else.
    --
    -- The counters go through putCounters, the CR 122.6 funnel, exactly as the
    -- EntryR WithCounters arm's do -- so CR 614.16 applies and Hardened Scales
    -- sees a megamorph counter the way it sees a riot one. The amount is
    -- evaluated the same way too, though rule 702.37b states its own number and
    -- Pawl.CardSpec holds that no printing authors a turn-up counter rewrite, so
    -- the only quantity that reaches here today is that arm's Literal 1.
    --
    -- The event survives: turning face up is not replaced by the counter, only
    -- accompanied by it, so Just is returned and FaceDown.performTurnFaceUp goes on to
    -- record CR 708.7's event.
    (ReplacementEffect.TurnUpR (TurnUpR.MkTurnUpR _ rewrite), ProposedEvent.WouldTurnFaceUp oid _) -> case rewrite of
      TurnUpRewrite.WithCounters (WithCounters.MkWithCounters kind quantity) -> do
        gs <- State.get
        let viewOf = Projection.viewWithLastKnown oid gs
            context = Replacement.candidateContext candidate
        Replacement.consume (ReplacementCandidate.identity candidate)
        case Quantity.evaluate viewOf context gs oid quantity of
          Nothing -> pure ()
          Just n -> Monad.when (n > 0) (Monad.void (putOwnCounters oid kind (Integer.toNaturalSaturating n)))
        pure (Just event)
      -- CR 303.4k with CR 614.1e: Gift of Doom's "as this Aura is turned face
      -- up, you may attach it to a creature", applied WHILE the permanent turns
      -- over (CR 708.11) because FaceDown.performTurnFaceUp raises this event there and
      -- nowhere else -- and, decisively for this rule, AFTER it has written the
      -- face-up status. Every characteristic Attach.turnUpHosts reads is
      -- therefore the Aura's "as it would exist if it were face up"; see there.
      --
      -- CR 303.4k's "an object OR PLAYER" is narrowed to objects here, and by the
      -- rule's own conjunction rather than by this engine: the destinations are
      -- what the card's Filter admits, a Filter matches a battlefield permanent,
      -- and the only printing says "a creature".
      --
      -- The "may" is asked FIRST and separately, since declining and finding no
      -- legal host are different states a transcript must tell apart. Declining
      -- leaves the Aura unattached and CR 704.5m buries it on the next
      -- state-based pass, which is a real and quite bad outcome -- so this is
      -- never elided, the ChooseRiot posture.
      --
      -- The event survives either way: turning face up is not replaced by the
      -- attachment, only accompanied by it, so Just is returned and
      -- FaceDown.performTurnFaceUp goes on to record CR 708.7's event.
      TurnUpRewrite.MayAttachTo filter_ -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        gs <- State.get
        case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for the SacrificeAnyNumber arm's reason:
          -- the permanent is on the battlefield, so controllerOf falls back to
          -- its owner. Attaches nothing rather than guessing at a player.
          Nothing -> pure (Just event)
          Just controller -> do
            let hosts = Attach.turnUpHosts controller oid filter_ gs
            -- Where the rules leave nothing to ask, don't prompt: with no legal
            -- host the "may" has no exercisable side.
            Monad.unless (null hosts) $ do
              decision <- Game.choose (Prompt.ChooseTurnUpAttachment (Decide.deciderFor controller gs) controller oid)
              Monad.when (decision == OptionalDecision.Exercises) $ do
                chosen <- Attach.chooseHost controller oid hosts
                Monad.mapM_ (attach oid . Recipient.ToObject) chosen
            pure (Just event)
    -- Unreachable: `applies` admits TurnUpR only against WouldTurnFaceUp.
    (ReplacementEffect.TurnUpR {}, _) -> pure (Just event)

-- CR 707.2 / 202.3b: the copiable values a copy takes off the object it copies.
-- Projection.copiableCharacteristics answers all but one of them; the exception
-- is a mana value, and only when the copied object is a nonmodal double-faced
-- permanent with its back face up.
--
-- CR 202.3b's two sentences disagree about that permanent on purpose. Its first
-- calculates the object's OWN mana value "as though it had the mana cost of its
-- front face" -- Pawl.Engine.Card.manaCostFace, which is what
-- copiableCharacteristics already read -- and its second says "if a permanent or
-- spell is a copy of the back face of a nonmodal double-faced object ... the
-- mana value of the copy is 0". So the source and its copy report DIFFERENT
-- numbers, and the copy's cannot be derived from the snapshot alone: the front
-- face's cost is already folded into it and nothing left in the record says
-- which face it came off. The copied OBJECT is where that is knowable, which is
-- why the override lives at the stamp rather than in the projection.
--
-- Everything else in CR 202.3 rides through untouched, including the copy's own
-- printed cost being irrelevant (CR 707.2, the parenthetical's "even if the card
-- representing that copy is itself a double-faced card") -- the snapshot is the
-- COPIED object's throughout, so a Clone that is itself a Transforming card
-- would get 0 here for the same reason a Clone printed {3}{U} does.
--
-- A copy OF A COPY needs no arm: the source's snapshot already carries whatever
-- number this wrote when it entered, and its own card and face are then no
-- longer what it reports.
--
-- CR 202.3b's rule is about "a permanent OR SPELL", and BOTH halves come through
-- here: the entry replacement stamps the permanent's snapshot, and
-- Pawl.Engine.Resolve's CR 707.10 copy stamps the spell's off this same
-- function.
copiedSnapshot :: ObjectId -> GameState -> PC.ProjectedCharacteristics
copiedSnapshot src gs =
  let snapshot = Projection.copiableCharacteristics src gs
      backFace = case (Game.lookupObject src gs, Game.cardOf src gs) of
        (Just obj, Just card) -> Card.showsBackFace card (Object.face obj)
        _ -> False
   in if backFace then snapshot {PC.manaValue = Just 0} else snapshot

-- CR 608.2h: `copiedSnapshot` for an object that may already be gone -- the
-- record filed as it ceased, which is the same value this function would have
-- returned an instant earlier. Game.cardOfWithLastKnown is the twin fallback for
-- the card behind it, and a copy effect needs both.
--
-- A separate name rather than widening `copiedSnapshot`, for
-- Game.cardOfWithLastKnown's reason: the live reader is asked by the CR 614.1c
-- entry rewrite, whose subject is on the battlefield by construction, and
-- answering there for an object that is not would resurrect it.
copiedSnapshotWithLastKnown :: ObjectId -> GameState -> PC.ProjectedCharacteristics
copiedSnapshotWithLastKnown oid gs = case Projection.lastKnownOf oid gs of
  Just lk -> LastKnown.copiable lk
  Nothing -> copiedSnapshot oid gs

-- CR 614.1c / 614.12: run the entry loop for an object that has just been
-- materialized on the battlefield.
--
-- The object is in GameState.objects and its zone index BEFORE this runs,
-- because CR 614.12 asks for the permanent's characteristics AS IT WOULD EXIST
-- ON THE BATTLEFIELD -- a projection of the object in the state where it has
-- entered, so the cheapest correct implementation is to put it there and project
-- it normally. Nothing observes the interim object: this finishes before the
-- Moved event is recorded, so no trigger scan and no state-based action can see
-- it.
--
-- `Monad.void` discards the `Nothing` that means the event does not happen. Safe
-- here: every EntryR arm always returns `Just`, and only DamageR/DestructionR
-- ever return `Nothing`, neither of which pairs with WouldEnter -- the only
-- event this loop proposes.
--
-- Always the LIVE board (`Nothing`), even when the zone change containing this
-- entry belongs to a CR 608.2f batch: the entering object is not on the
-- pre-batch board at all, and CR 614.12 asks about now rather than about when
-- the containing event began. CR 616.1g recognizes an entry like this as an
-- event CONTAINED within another rather than a second member of the batch, but
-- speaks only to the ORDER the two events' effects are chosen in, not to which
-- board each collects from. That a contained event keeps its own footing is this
-- engine's reading, resting on CR 614.12; no rule states it outright.
runEntry :: Set ObjectId -> ObjectId -> Game ()
runEntry batch oid = do
  -- CR 113.6 / 614.12: for the span of this loop, the batch's OTHER members are
  -- materialized but not entered, so Pawl.Engine.Projection gathers no continuous
  -- effect from their static abilities (see GameState.enteringBeside). The
  -- subject's own are untouched, because `batch` never holds it. Saved and
  -- restored rather than cleared, since an entry rewrite can reach another entry
  -- -- the SacrificeAnyNumber arm below runs a sacrifice, and RunEffects runs a
  -- card's effects -- and the outer batch has to survive that.
  before <- State.gets GameState.enteringBeside
  State.modify' (\gs -> gs {GameState.enteringBeside = batch})
  Monad.void (applyReplacementsIn Nothing batch (ProposedEvent.WouldEnter oid))
  designateProtector oid
  State.modify' (\gs -> gs {GameState.enteringBeside = before})

-- CR 310.9a: "as a battle enters the battlefield, its controller chooses a player
-- to be its protector." Run for every entering object, and a no-op for all but a
-- battle.
--
-- NOT a replacement effect, and the contrast with the defense counters it enters
-- beside is the rules' own. CR 310.4b says outright that a battle "has the
-- intrinsic ability 'This permanent enters with a number of defense counters on it
-- equal to its printed defense number'" and that "this ability creates a
-- replacement effect (see rule 614.1c)"; CR 310.9a says none of that. It names no
-- ability, cites no rule 614, and reads as a bare instruction about entering. So
-- the counters go through the CR 616.1 loop above and this does not.
--
-- Modelling it as an EntryRewrite anyway was the first cut here, and the rules
-- were right: two intrinsic rows on one entering battle both land in
-- ReplacementBucket.Other, and CR 616.1e then has the controller ORDER them --
-- a prompt on every battle entry whose two answers reach the same board, which is
-- the engine inventing a decision the rules never offer.
--
-- After the loop rather than before it, which is the whole difference between the
-- two positions and is unobservable: nothing between them can see the object (see
-- runEntry's own note -- no Moved event yet, so no trigger scan and no
-- state-based action), and no replacement effect reads a protector. Doing it
-- second also means CR 614.12's "characteristics as it would exist on the
-- battlefield" have already settled, so a permanent that entered AS a copy of a
-- Siege (CR 707.5) designates by the copy's battle types rather than the card's.
designateProtector :: ObjectId -> Game ()
designateProtector oid = do
  gs <- State.get
  let pc = Projection.project oid gs
  Monad.when (Battle.isBattle pc) $ case Projection.controllerOf oid gs of
    -- Unreachable, and defensive for the entry rewrites' reason: the object is
    -- materialized on the battlefield before this runs, so controllerOf falls
    -- back to its owner. Designates NOBODY rather than conjuring a player -- CR
    -- 310.11 is exactly the rule for a battle with no protector, and repairs this
    -- at the next state-based action check.
    Nothing -> pure ()
    Just controller -> do
      picked <- Battle.designateProtector pc controller oid
      State.modify' $ \g ->
        let stamp o = o {Object.protector = picked}
         in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}

-- CR 709.5f / 709.5c: give this permanent the unlocked designation for one of
-- its halves -- the single door every unlock goes through, and the only writer of
-- Object.unlockedHalves after the entry designation the move itself writes (CR
-- 709.5d, changeZoneAttaching's mkObj).
--
-- IDEMPOTENT, and that is CR 709.5h rather than defensiveness: the trigger fires
-- "when that permanent IS GIVEN the appropriate unlocked designation", so a
-- permanent that already has it is given nothing and nothing fires. CR 709.5e's
-- special action and CR 709.5f's keyword action both choose a LOCKED half, so
-- neither reaches this with a door already open; an effect that unlocks without
-- choosing would.
--
-- The EVENT is recorded here rather than by the caller, so the two cannot drift:
-- CR 709.5h is a fact about the designation being given, and this is where it is
-- given.
--
-- CR 709.5g's LOCK -- taking a designation back away -- has no counterpart
-- function: no card in the pool locks a door (#924).
unlockHalf :: ObjectId -> CardName.CardName -> Game ()
unlockHalf oid half = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just obj ->
      Monad.unless (Set.member half (Object.unlockedHalves obj)) $ do
        let opened = Set.insert half (Object.unlockedHalves obj)
        State.modify' $ \g ->
          let open o = o {Object.unlockedHalves = opened}
           in g {GameState.objects = Map.adjust open oid (GameState.objects g)}
        State.modify' (recordEvent (GameEvent.HalfUnlocked (HalfUnlocked.MkHalfUnlocked oid half (fullyUnlockedAfter opened (Game.cardOf oid gs)))))

-- CR 709.5i's "fully unlocks", answered about the designations a permanent has
-- ONCE a write has landed: "such an ability triggers when that permanent has one
-- of the two unlocked designations and gets the other, or when it has neither
-- designation and gains both."
--
-- Taking the designation set rather than the object, so both writers of
-- Object.unlockedHalves -- unlockHalf above and changeZoneAttaching's mkObj
-- below, which is CR 709.5d's entry designation -- can hand over the set they are
-- about to store rather than re-reading a GameState that has or has not been
-- modified yet. THE SAME helper at both, so the two cannot drift apart about what
-- "fully" means; that shared call is the only cover the entry site has, since no
-- board can make its answer True (#962). Computed AT THE WRITE and
-- carried on GameEvent.HalfUnlocked for the reason that event's own comment
-- gives: by the time a trigger is matched the board has moved on.
--
-- ALL the halves and not merely two, though CR 709.5 knows only a left and a
-- right: the question the rule asks is whether any half is still locked (CR
-- 709.5c), and asking it of every face is the same answer for a two-faced card
-- and an honest one for anything else. `all` and not `any` is the whole content
-- of this function -- one designation on a two-door Room is exactly the case CR
-- 709.5i does not fire on.
--
-- False for a card with no shared type line, which is CR 709.5's own scope: a
-- card with one face would otherwise be "fully unlocked" by having no locked
-- halves, and it has no halves at all. Nothing calls this for one, but the guard
-- is the rule rather than defensiveness.
--
-- Nothing -- a designation written for an object whose card cannot be found --
-- answers False, there being no faces to compare against.
fullyUnlockedAfter :: Set CardName.CardName -> Maybe Card -> Bool
fullyUnlockedAfter halves card = case card of
  Nothing -> False
  Just c ->
    Card.hasSharedTypeLine c
      && all (\face -> Set.member (Face.name face) halves) (Card.Type.faces c)

-- CR 615: settle one proposed damage event. Nothing means it does not happen;
-- the second answer is CR 615.13's, one entry per prevention effect that applied
-- to THIS event and prevented some of it.
resolveDamage :: DamageEvent.DamageEvent -> Game (Maybe DamageEvent.DamageEvent, [Prevention])
resolveDamage de = do
  (outcome, prevented) <- applyReplacementsReporting Nothing Set.empty (ProposedEvent.WouldDealDamage de)
  pure (outcome >>= Replacement.asDamageEvent, prevented)

-- CR 608.2f / 510.2: settle a whole batch of SIMULTANEOUS damage events, and
-- answer the survivors. The typed door Pawl.Engine.Damage uses, so Damage never
-- cases on a ProposedEvent or on a ReplacementEffect.
--
-- Each event still runs its OWN CR 616.1 loop and the loop's unit is still one
-- event, which is what CR 614.5 and CR 615.10 both describe. Three things this
-- adds over calling resolveDamage per event, and all three are rules the BATCH is
-- the only place to state:
--
--   * CR 616.1's APNAP clause, because a lone event has one affected object and
--     so one chooser: only a batch can present choices to two players at once.
--     orderBatch settles that order before any of the batch is asked.
--   * CR 615.7's ORDER, because the shield is a single resource allocated across
--     the whole batch and the rule gives that choice to the shielded side -- CR
--     101.4c saying the same of CR 122.1c's shield counters.
--   * CR 615.13's GROUPING, because that rule fires an ability "each time a
--     prevention effect is applied to one or more simultaneous damage events",
--     so one instance reaching three of this batch's events is ONE prevention of
--     the total rather than three.
resolveDamageBatch :: [DamageEvent.DamageEvent] -> Game ([DamageEvent.DamageEvent], [Prevention])
resolveDamageBatch events = do
  ordered <- Replacement.orderBatch events
  settled <- Monad.mapM resolveDamage ordered
  pure (Maybe.mapMaybe fst settled, Replacement.groupPreventions (concatMap snd settled))

-- CR 701.8 / 614.8: settle a proposed destruction. `Just` is the object actually
-- destroyed -- which need not be the one asked about, since a rewrite may
-- redirect it; `Nothing` means a replacement took the event (regeneration), and
-- that rewrite has already done its own work.
--
-- `asOf` is applyReplacementsIn's, and the destroy funnel always supplies it: CR
-- 608.2f gives even a single Doom Blade a one-element batch, and when that batch
-- is itself part of a CR 704.3 pass the board is the pass's rather than the
-- batch's (Event.destroyInBatch).
resolveDestruction :: Maybe GameState -> DestructionCause.DestructionCause -> Regenerability.Regenerability -> ObjectId -> Game (Maybe ObjectId)
resolveDestruction asOf cause regenerability oid = do
  live <- State.get
  let settledRegenerability = strengthen (Maybe.fromMaybe live asOf) oid regenerability
  outcome <- applyReplacementsIn asOf Set.empty (ProposedEvent.WouldBeDestroyed oid settledRegenerability cause)
  pure (outcome >>= Replacement.asDestruction)

-- CR 701.19c: a standing prohibition (Hurr Jackal) forbids regeneration of the
-- permanent it names, so the destruction this funnel is about to propose is one
-- that can't be regenerated whatever its cause supplied. The rule's own reading
-- -- the shield is still created and simply "not applied" -- is what makes this
-- the right seam: Replacement.admits refuses the candidate, so CR 616.1 never
-- offers it and the shield is never spent.
--
-- One-directional. A prohibition can only turn Regenerable into
-- CantBeRegenerated; nothing in CR 701.19 permits the converse, so Terror's
-- clause survives an empty store.
--
-- Read off the board applyReplacementsIn is about to gather from -- `asOf` when
-- the caller supplied one (CR 704.3's whole pass, Event.destroyInBatch), the
-- live state otherwise -- so a CR 608.2f batch cannot see the prohibition and
-- the replacement pass disagree about which board they are on.
strengthen :: GameState -> ObjectId -> Regenerability.Regenerability -> Regenerability.Regenerability
strengthen gs oid regenerability =
  if any ((== oid) . ActiveUnregeneratable.object) (GameState.unregeneratables gs)
    then Regenerability.CantBeRegenerated
    else regenerability

-- The single counter-PLACEMENT funnel for an OBJECT recipient (CR 122.6: counters
-- as markers on a permanent -- not to be confused with `counter` below, CR 701.6's
-- countering of a spell; putPlayerCounters below is the player's). CR 122.6 makes
-- this the right single seam, since it covers both counters put on a permanent
-- already on the battlefield and counters an object is given as it enters. A zero
-- count after the loop puts nothing on.
--
-- Beside the other change-and-emit funnels of this module, and `apply`'s
-- EntryRewrite arms reach it through putOwnCounters for CR 122.6's as-it-enters
-- clause. A copy of the body anywhere else would be a second funnel, which is the
-- one thing a funnel must not have.
--
-- The CounterCause is the placement's PROVENANCE and nothing else -- who is putting
-- the counters, and whether an effect is what put them. Only a row reads it; see
-- Replacement.matchesPutter.
--
-- ANSWERS how many counters actually landed, which is not what was asked for: CR
-- 614.16 may have grown or erased the placement, and an object that is no longer
-- there takes none. Only the two rules that say "one or more +1/+1 counters" read
-- it -- CR 702.100b and CR 702.149c, at Pawl.Engine.Resolve's Effect.Evolve and
-- Effect.Train arms; every other caller places and moves on.
putCounters :: CounterCause.CounterCause -> ObjectId -> CounterKind.CounterKind Keyword.Type.Keyword -> Natural -> Game Natural
putCounters = putCountersIn Set.empty

-- putCounters with CR 614.12's same-batch exclusion: `batch` is the permanents
-- entering beside `oid`, and only an entry path has one to pass.
putCountersIn :: Set ObjectId -> CounterCause.CounterCause -> ObjectId -> CounterKind.CounterKind Keyword.Type.Keyword -> Natural -> Game Natural
putCountersIn batch cause oid kind n = do
  resolved <- resolveCounters batch cause oid kind n
  case resolved of
    Nothing -> pure 0
    Just (target, settledKind, settledCount)
      | settledCount == 0 -> pure 0
      | otherwise -> do
          gs <- State.get
          -- No write and no event for an object that is not there. Map.adjust on a
          -- missing id is a silent no-op, so proceeding would record a placement
          -- the state does not show. ONE lookup answers both questions -- whether
          -- the object exists, and how many counters of the kind it already had.
          case Game.lookupObject target gs of
            Nothing -> pure 0
            Just obj -> do
              -- CR 613.7c: the counters arriving get a timestamp, and the ones of
              -- that kind already there get the same one.
              ts <- State.state Game.freshTimestamp
              let before = Map.findWithDefault 0 settledKind (Object.counters obj)
                  bump o =
                    o
                      { Object.counters = Map.insertWith (+) settledKind settledCount (Object.counters o),
                        Object.counterTimestamps = Map.insert settledKind ts (Object.counterTimestamps o)
                      }
                  bumped g = g {GameState.objects = Map.adjust bump target (GameState.objects g)}
              -- CR 122.6's placement, recorded AFTER the write and from the SETTLED
              -- count, so a Doubling Season that turned one counter into two records
              -- the crossing the board actually saw. The before/after pair is what
              -- CR 714.2b's chapter ability reads.
              --
              -- Guarded by the same `settledCount > 0` the write is: an event
              -- recorded for a placement that did not happen would fire a chapter
              -- ability off nothing.
              State.modify' (recordEvent (GameEvent.CountersPut (CounterChange.MkCounterChange target settledKind before (before + settledCount))) . bumped)
              pure settledCount

-- CR 122.6a: putCounters with the rule's DEFAULT putter -- "if the effect doesn't
-- specify a player, the object's controller puts those counters on it". Every
-- placement onto the RECEIVING object's own account goes through here: the entry
-- rewrites of `apply` above (CR 614.1c-d), CR 702.37b's megamorph counter, and the
-- counters a spell's entry riders give. No printing in the pool exercises the
-- rule's exception by naming a player, so nothing needs to pass one.
--
-- An object with no controller is one that is not on the battlefield, and
-- putCounters places nothing on such an object anyway -- so answering 0 without
-- raising the event is the same answer, reached one step earlier.
putOwnCounters :: ObjectId -> CounterKind.CounterKind Keyword.Type.Keyword -> Natural -> Game Natural
putOwnCounters = putOwnCountersIn Set.empty

-- putOwnCounters with CR 614.12's same-batch exclusion; `batch` is
-- putCountersIn's.
putOwnCountersIn :: Set ObjectId -> ObjectId -> CounterKind.CounterKind Keyword.Type.Keyword -> Natural -> Game Natural
putOwnCountersIn batch oid kind n = do
  gs <- State.get
  case Projection.controllerOf oid gs of
    Nothing -> pure 0
    Just putter -> putCountersIn batch (CounterCause.ByEffect putter) oid kind n

-- CR 122: take counters off an object, recording a CountersRemoved event from
-- the before/after pair so a trigger can read the crossing. That event's other
-- producer is CR 120.3h's and CR 120.3c's damage to a battle or a planeswalker;
-- it belongs to no one rule in particular. Two costs come through this door as
-- well: CR 606.4's loyalty (Pawl.Engine.Cost's RemoveLoyaltyFromThis arm) and CR
-- 118.1's +1\/+1 removal (its RemovePlusOneCountersFromThis arm).
--
-- NO CR 614.16 loop, unlike putCounters above, and that asymmetry is the rule's
-- rather than a shortcut: 614.16 replaces a PLACEMENT -- "if an effect would put
-- one or more counters on a permanent" -- and no ReplacementEffect class in
-- Pawl.Types.ReplacementEffect pairs with a removal, so there is nothing for a
-- loop to offer this to.
--
-- SATURATING: removing more than are present leaves the kind absent from the map
-- rather than negative, which is what keeps Object.counters a tally of what is
-- there. CR 122 states no rule making an over-large removal fail.
--
-- This is not the only place counters leave an object -- CR 704.5q's
-- annihilation and CR 122.2's zone change do not route through here, so #900's
-- "record a removal event for every removal" is advanced by this and not closed.
removeCounters :: ObjectId -> CounterKind.CounterKind Keyword.Type.Keyword -> Natural -> Game ()
removeCounters oid kind n =
  Monad.when (n > 0) . State.modify' $ \gs ->
    -- ONE lookup answers both questions -- whether the object is there, and how
    -- many of the kind it has -- for putCounters' reason: Map.adjust on a
    -- missing id is a silent no-op, so proceeding would record a removal the
    -- state does not show.
    case Game.lookupObject oid gs of
      Nothing -> gs
      Just obj ->
        let before = Map.findWithDefault 0 kind (Object.counters obj)
            after = if n >= before then 0 else before - n
            drop_ o =
              o
                { Object.counters =
                    if after == 0
                      then Map.delete kind (Object.counters o)
                      else Map.insert kind after (Object.counters o)
                }
            dropped = gs {GameState.objects = Map.adjust drop_ oid (GameState.objects gs)}
         in -- Nothing to remove is nothing to record: an event for a removal that
            -- did not happen would fire a counter-watching trigger off nothing,
            -- which is the guard putCounters puts on its own write.
            if before == 0
              then gs
              else recordEvent (GameEvent.CountersRemoved (CounterChange.MkCounterChange oid kind before after)) dropped

-- CR 122.6: settle a proposed counter placement. Nothing means none are put on.
--
-- EVERY placement runs the CR 616.1 loop, whatever its cause, and the cause rides
-- the event into Replacement.matchesPutter -- which is where CR 614.16's "if an
-- effect would put" and Vorinclex's "if you would put" part company. A ByRule
-- placement used to skip the loop at this door, an equivalence that held only while
-- every representable counter replacement was one of rule 614.16's; Vorinclex is
-- not one, so the gate moved into the row filter (#847).
--
-- `batch` is applyReplacementsIn's: the permanents entering the battlefield
-- BESIDE the object being counted, empty for every placement that is not part of
-- an entry. A counter a permanent enters with is part of how it enters (CR
-- 614.1c), so CR 614.12 settles which effects may scale it, and a Corpsejack
-- Menace arriving in the same batch is not one of them.
resolveCounters :: Set ObjectId -> CounterCause.CounterCause -> ObjectId -> CounterKind.CounterKind Keyword.Type.Keyword -> Natural -> Game (Maybe (ObjectId, CounterKind.CounterKind Keyword.Type.Keyword, Natural))
resolveCounters batch cause oid kind n = do
  outcome <- applyReplacementsIn Nothing batch (ProposedEvent.WouldPutCounters cause oid kind n)
  pure (outcome >>= Replacement.asCounters)

-- CR 122.1 / 122.6: putCounters' player half -- the ONE place a player's counters
-- go up. Answers how many actually landed, which is not what was asked for: the
-- CR 616.1 loop may have grown the placement (Vorinclex, Monstrous Raider's
-- doubling), shrunk it or erased it. Nobody reads the answer yet; it is the shape
-- putCounters already has, and dropping it would make the two funnels disagree
-- about their own event.
--
-- No GameEvent is recorded, where putCounters records CR 122.6's CountersPut: that
-- event carries an ObjectId, and no trigger condition in
-- Pawl.Types.TriggerCondition watches a player's counters -- so an event minted
-- here would have no reader. The card that gives it one brings the condition and
-- the event together; this is where it is recorded.
putPlayerCounters :: CounterCause.CounterCause -> PlayerId -> PlayerCounterKind.PlayerCounterKind -> Natural -> Game Natural
putPlayerCounters cause pid kind n = do
  resolved <- resolvePlayerCounters cause pid kind n
  case resolved of
    Nothing -> pure 0
    Just (target, settledKind, settledCount)
      | settledCount == 0 -> pure 0
      | otherwise -> do
          -- Zero is the guard putCounters puts on its own write, and Scaling.Halve
          -- is what makes it reachable here: half of one counter, rounded down, is
          -- a replacement that removes the event.
          State.modify' $ \gs ->
            let bump p = p {Player.counters = Map.insertWith (+) settledKind settledCount (Player.counters p)}
             in gs {GameState.players = Map.adjust bump target (GameState.players gs)}
          pure settledCount

-- CR 122.1: resolveCounters for a player recipient, and read the same way -- the
-- cause rides the event and the rows decide which causes they reach.
resolvePlayerCounters :: CounterCause.CounterCause -> PlayerId -> PlayerCounterKind.PlayerCounterKind -> Natural -> Game (Maybe (PlayerId, PlayerCounterKind.PlayerCounterKind, Natural))
resolvePlayerCounters cause pid kind n = do
  outcome <- applyReplacements (ProposedEvent.WouldPutPlayerCounters cause pid kind n)
  pure (outcome >>= Replacement.asPlayerCounters)

-- CR 111.1: settle a proposed token creation. Nothing means none are created.
resolveTokens :: PlayerId -> Card -> Natural -> Game (Maybe (PlayerId, Card, Natural))
resolveTokens pid card n = do
  outcome <- applyReplacements (ProposedEvent.WouldCreateTokens pid card n)
  pure (outcome >>= Replacement.asTokens)

-- CR 500.11 / 614.10: settle whether a step or phase begins at all, on the turn
-- of `pid`. False means a skip took it, and proceeding past it is then the
-- caller's whole obligation -- CR 614.1b replaces a skipped step with nothing, so
-- there is no rewritten event to carry out. How far past it reaches is the
-- caller's too: one schedule entry for a PhaseSelector.Step, the phase's
-- remaining entries for a whole phase (Engine.runStep, Turn.dropRestOfPhase).
--
-- Answers a Bool rather than the settled event, unlike resolveDestruction, whose
-- `Just` had to carry an identity because a rewrite can redirect which object is
-- destroyed. Nothing can rewrite a WouldBeginPhase: PhaseR is the only effect the
-- class admits and it only ever cancels.
--
-- The typed door Pawl.Engine.Engine uses, so Engine never cases on a
-- ProposedEvent.
beginsPhase :: PhaseSelector -> PlayerId -> Game Bool
beginsPhase selector pid = do
  outcome <- applyReplacements (ProposedEvent.WouldBeginPhase selector pid)
  pure (Maybe.isJust (outcome >>= Replacement.asPhaseBegin))

-- The single zone-change primitive (CR 400.7): the source object ceases; a NEW
-- object with a fresh id is created in the destination, carrying owner and
-- source forward and resetting per-incarnation state. No-op if the id is unknown.
-- The Game () wrapper the ~30 existing callers use; changeZoneReturning below
-- carries the same body but hands back the freshly-minted incarnation id, which
-- Resolve's ExileUntilMonarch arm registers for its return sweep.
changeZone :: ObjectId -> Zone -> Game ()
changeZone oid requestedDest = Monad.void (changeZoneReturning oid requestedDest)

-- changeZoneReturning for a move whose effect says how the object ENTERS -- CR
-- 110.5b's tap state, CR 712.14a's transformed face, CR 708.3's face-down entry
-- and CR 110.2a's controller -- rather than taking the rules' defaults.
--
-- A separate door rather than a fifth parameter on changeZone, as changeZoneInBatch
-- is: the ~30 callers moving under the default have no tap state to name. Handed
-- to the funnel rather than applied after it, so a permanent an effect says is
-- tapped is never untapped for an instant, and so a permanent an effect says
-- enters under someone's control never belongs to its owner for an instant --
-- CR 614.1c's entry replacements run inside this call and read both.
--
-- CR 712.14a is the other rule this door is the only one for: "If a spell or
-- ability puts a double-faced card onto the battlefield 'transformed' or
-- 'converted', it enters the battlefield with its back face up." Which face that
-- is comes from the card's layout (Card.backFace), so the rider stays a Bool and
-- the engine never learns which card is entering. The rule's SECOND sentence is
-- the same refusal CR 712.14b gets below -- "if a player is instructed to put a
-- card that isn't a double-faced card onto the battlefield transformed or
-- converted, that card stays in its current zone" -- which is exactly a card
-- whose layout gives Card.backFace nothing to answer with.
--
-- CR 712.14a and NOT CR 712.13a, which is a different rule with a different
-- mechanism: an ability causing a double-faced SPELL already on the stack to
-- enter transformed is a replacement effect, CR 616.1d's own bucket, and no
-- rider on a move can express it -- EntryRewrite.EntersTransformed is that one,
-- applied by `apply` above.
--
-- The one door CR 712.14b applies to, and that is what the rule's own wording
-- picks out: "If a player is INSTRUCTED to put a modal double-faced card onto
-- the battlefield and its front face isn't a permanent card, the card stays in
-- its current zone." An instruction to put an object onto the battlefield is
-- exactly the move that names how it enters, so the other doors are out of the
-- rule's scope rather than exempted from it -- a permanent spell RESOLVING (CR
-- 712.13, Pawl.Engine.Stack) and a land PLAYED (CR 712.12, Pawl.Engine.Engine)
-- both reach the battlefield without any instruction to put them there.
--
-- Nothing, the same answer a CR 616.1 replacement that cancelled the move gives,
-- so every caller already handles it: "the card stays in its current zone" and
-- "nothing entered" are the same fact. Asked BEFORE the funnel, because CR
-- 712.14b is not a replacement effect -- there is no event for CR 616.1 to
-- choose among, and running the entry loop first would fire CR 614.1c's
-- as-enters abilities for a card that never enters.
changeZoneEntering :: ObjectId -> Zone -> LibraryPosition.LibraryPosition -> EntryRiders.EntryRiders Natural -> Maybe PlayerId -> Game (Maybe ObjectId)
changeZoneEntering = changeZoneEnteringIn Nothing Set.empty

-- changeZoneEntering for ONE MEMBER of a CR 608.2f batch: `asOf` and `batch` are
-- applyReplacementsIn's, and Pawl.Engine.Resolve's Effect.MoveToZone arm is the
-- only caller that supplies either. A separate door rather than two more
-- parameters on changeZoneEntering, changeZoneInBatch's shape one door over: the
-- lone moves have no batch to name.
changeZoneEnteringIn :: Maybe GameState -> Set ObjectId -> ObjectId -> Zone -> LibraryPosition.LibraryPosition -> EntryRiders.EntryRiders Natural -> Maybe PlayerId -> Game (Maybe ObjectId)
changeZoneEnteringIn asOf batch oid requestedDest position riders under = do
  gs <- State.get
  let mCard = Game.cardOf oid gs
      onto = requestedDest == Zone.Battlefield
      -- CR 712.14a's back face, asked only of a move the effect says is
      -- transformed. Nothing means one of two different things, which is why the
      -- rider is read alongside it below: no back face to turn to, or no
      -- instruction to turn at all.
      mBack = if EntryRiders.transformed riders then mCard >>= Card.backFace else Nothing
      refused =
        onto
          && ( maybe False Card.staysWhenPutOntoBattlefield mCard -- CR 712.14b
                 || (EntryRiders.transformed riders && Maybe.isNothing mBack) -- CR 712.14a
             )
      -- CR 712.14: "A double-faced card put onto the battlefield from a zone
      -- other than the stack enters the battlefield with its front face up by
      -- default", which Object.face records as Nothing (CR 712.8a).
      shown = if onto then fmap Face.name mBack else Nothing
      -- CR 110.2a: the effect states otherwise, so the player it instructed stops
      -- being the answer and CR 110.2's default -- the owner, which is what
      -- `Nothing` means to the funnel -- takes over. Undying and persist are what
      -- print it (CR 702.93a, CR 702.79a).
      under' = if EntryRiders.underOwner riders then Nothing else under
      -- CR 708.3: "objects that are put onto the battlefield face down are
      -- turned face down BEFORE they enter the battlefield". Handed to the
      -- funnel rather than written after it for the tap state's reason, and the
      -- rule's whole content is that ordering -- the funnel writes the status in
      -- mkObj, so the CR 614.1c entry loop, the CR 603.2g Moved event and every
      -- trigger scanning it see CR 708.2a's 2/2 with no abilities, and "the
      -- permanent's enters-the-battlefield abilities won't trigger (if
      -- triggered) or have any effect (if static)" needs no branch of its own.
      -- Manifest (CR 701.40a) is what prints it; Pawl.FaceDownSpec's Soul
      -- Summons group is the proof, with Thragtusk's "you gain 5 life" enters
      -- trigger as the card underneath.
      --
      -- BATTLEFIELD-ONLY, which is CR 110.5d's scope for the status and this
      -- door's own responsibility: mkObj's `facing` write serves the stack too
      -- (CR 708.4, changeZoneCasting), so the gate cannot live there. A card
      -- stating the rider on a move anywhere else says something no rule reads,
      -- which Pawl.CardSpec lints.
      -- CR 701.40a names the allower: manifest is the only rule in the pool that
      -- puts a card onto the battlefield face down, and "that permanent is a
      -- manifested permanent for as long as it remains face down" is what the
      -- reason records -- which is what opens CR 701.40b's turn-face-up procedure
      -- to it (Pawl.Engine.FaceDown.canTurnFaceUp). The rider is a Bool, so it
      -- names no other (gap #1668); see Pawl.Types.EntryRiders.
      facing = if onto && EntryRiders.faceDown riders then Facing.faceDown FaceDownReason.Manifested else Facing.FaceUp
  if refused
    then pure Nothing
    else changeZoneAttaching asOf batch oid requestedDest position Nothing (EntryRiders.tapped riders) (EntryRiders.counters riders) under' shown facing (EntryRiders.exiledFaceDown riders)

-- changeZoneReturning for a move that carries ONE NAMED HALF of the card into
-- its destination: CR 709.3's choice of which half of a split card is being
-- cast, which the rule makes "before putting it onto the stack", so the move is
-- what carries it. CR 712.11b words the modal double-faced card's version of the
-- same choice identically, and CR 712.11c its version of CR 709.3a, so both
-- layouts come through this door.
--
-- A HALF and not always a face that is up, which is CR 709.5d's use of the same
-- parameter: a Room permanent shows both halves at once, so changeZoneAttaching
-- spends the name on an unlocked designation and leaves Object.face empty. Every
-- other caller's half IS the face the object arrives showing.
--
-- The name is a MAYBE, because the third rule asking for this door does not
-- always have one to give: CR 712.13 carries a resolving double-faced spell's
-- face onto the battlefield, and Pawl.Engine.Stack asks that of every permanent
-- spell it resolves, most of which show nothing (Pawl.Engine.Card.enteringFace).
-- Nothing here is exactly changeZoneReturning.
--
-- A separate door rather than a seventh parameter on changeZone, as
-- changeZoneEntering is: the ~30 callers moving an object that shows whatever its
-- layout gives it (CR 709.4's combined view, a single-face card's one face) have
-- no face to name.
--
-- Handed to the funnel rather than written onto the object the move returned,
-- for the reason CR 709.3a states: "only that half is considered to be put onto
-- the stack", so the CR 400.7 incarnation must never exist without it -- and,
-- since only a writer inside the move knows where the move actually landed, a
-- CR 616.1 redirect to another zone drops the face instead of carrying it there.
-- See the `face` note in changeZoneAttaching's mkObj, and Pawl.CastSpec's "a cast
-- redirected off the stack keeps both halves" for the case that proves it.
changeZoneShowing :: ObjectId -> Zone -> Maybe CardName.CardName -> Game (Maybe ObjectId)
changeZoneShowing oid requestedDest shown = changeZoneAttaching Nothing Set.empty oid requestedDest LibraryPosition.defaultValue Nothing TapState.Untapped Map.empty Nothing shown Facing.FaceUp False

-- changeZoneShowing for a move that puts the object into its destination FACE
-- DOWN -- the CR 110.5b "unless a spell or ability says otherwise" that morph is.
--
-- The two rules that need it are the two ends of one cast. CR 708.4: "Objects
-- that are cast face down are turned face down BEFORE they are put onto the
-- stack, so effects that care about the characteristics of a spell will see only
-- the face-down spell's characteristics" -- so the facing is part of the move
-- and not a stamp applied to what it hands back, exactly as CR 709.3a's chosen
-- half is. And that rule's last sentence, "the permanent the spell becomes will
-- be a face-down permanent", which is the stack-to-battlefield move
-- Pawl.Engine.Stack makes. CR 708.3's permanent PUT onto the battlefield face
-- down does NOT come through here: it is a move whose effect says how the object
-- enters, riders and all, so it is changeZoneEntering's faceDown rider.
--
-- A separate door rather than an eighth parameter on changeZoneShowing, as
-- changeZoneEntering is: the ordinary move leaves CR 110.5b's default standing
-- and has no facing to name.
--
-- The face name goes along, and is not in tension with CR 708.2a's "no name":
-- Object.face records WHICH HALF of the card is underneath, not what the object
-- is called, and the substitution that empties the name reads it (faceOf) rather
-- than being stored. Turning the permanent face up is what makes it observable
-- again (CR 708.8).
changeZoneFaceDown :: ObjectId -> Zone -> Maybe CardName.CardName -> Game (Maybe ObjectId)
changeZoneFaceDown oid requestedDest shown = changeZoneAttaching Nothing Set.empty oid requestedDest LibraryPosition.defaultValue Nothing TapState.Untapped Map.empty Nothing shown (Facing.faceDown FaceDownReason.Morphed) False

-- CR 601.2a's move: the card goes onto the stack and "that player becomes its
-- controller". The caster is carried BY THE MOVE, for the reason CR 709.3a
-- states about the chosen half above -- the CR 400.7 incarnation must never exist
-- without it, or a reader inside the move (a CR 616.1 replacement, a trigger
-- scanned over the arrival) would see a spell controlled by whoever happens to
-- own the card.
--
-- One caller, Pawl.Engine.Cast, which is the only route a CARD takes onto the
-- stack. changeZoneEntering carries a player too, so an Effect.MoveToZone naming
-- the stack would stamp) one as well -- no) card in the pool does, and CR 800.4b's
-- "put onto the stack under the control of" says such a move would have a
-- controller to name if one did.
--
-- A CR 616.1 redirect that lands the move in any zone but the stack or the
-- battlefield drops the stamp, exactly as it drops the face, because CR 109.4
-- gives an object there no controller to record.
changeZoneCasting :: PlayerId -> ObjectId -> Zone -> Maybe CardName.CardName -> Facing.Facing -> Game (Maybe ObjectId)
changeZoneCasting caster oid requestedDest shown facing = changeZoneAttaching Nothing Set.empty oid requestedDest LibraryPosition.defaultValue Nothing TapState.Untapped Map.empty (Just caster) shown facing False

-- changeZone for one member of a batch of moves CR 608.2f or CR 704.3 processes
-- SIMULTANEOUSLY. `asOf` is the board the batch began in -- or, for a batch inside
-- a larger simultaneous event, that event's -- and is what its members' CR 616.1
-- loops collect replacement candidates from; see applyReplacementsIn above.
--
-- Not the only batch door: an Effect.MoveToZone batch goes through
-- changeZoneEnteringIn, which carries the entry riders, the library position and
-- the entry controller this one has no parameters for. This door carries the
-- batches whose members reach a graveyard, the command zone or exile --
-- Pawl.Engine.Sba's sweeps, the destroy funnel and CR 800.4a's fourth clause --
-- so nothing enters the battlefield and there is no `batch` set for CR 614.12a
-- to narrow.
--
-- A separate door rather than a fourth parameter on changeZone: a batch is the
-- rare case, and for a single move the board it begins on IS the live one.
changeZoneInBatch :: GameState -> ObjectId -> Zone -> Game ()
changeZoneInBatch asOf oid requestedDest = Monad.void (changeZoneInBatchReturning asOf oid requestedDest)

-- changeZoneInBatch, answering with the destination incarnation's id -- what
-- changeZoneReturning is to changeZone, and the same answer: Just the CR 400.7
-- id, Nothing when the move was cancelled or the id named no object. The destroy
-- funnel is the one caller, for CR 701.8b's "put into a graveyard this way".
changeZoneInBatchReturning :: GameState -> ObjectId -> Zone -> Game (Maybe ObjectId)
changeZoneInBatchReturning asOf oid requestedDest = changeZoneAttaching (Just asOf) Set.empty oid requestedDest LibraryPosition.defaultValue Nothing TapState.Untapped Map.empty Nothing Nothing Facing.FaceUp False

-- changeZoneReturning's body, returning the destination incarnation's id: Just
-- newId on a completed move (CR 400.7 minted a fresh id), Nothing when the id is
-- unknown or the CR 616.1 replacement loop cancelled the move (`resolved ==
-- Nothing`). changeZoneReturning itself is the `seed = Nothing` case below.
changeZoneReturning :: ObjectId -> Zone -> Game (Maybe ObjectId)
changeZoneReturning oid requestedDest = changeZoneAttaching Nothing Set.empty oid requestedDest LibraryPosition.defaultValue Nothing TapState.Untapped Map.empty Nothing Nothing Facing.FaceUp False

-- changeZoneReturning with an attachment seed. Per CR 303.4 attachment is a
-- property of entering, not a step after it: the CR 614.1c entry replacement loop
-- and the Moved event both run before this returns, so an Aura attached afterward
-- would be unattached during both.
--
-- The seed is also what the CR 701.3a attachment event further down is read off,
-- so Bramble Elemental's "whenever an Aura becomes attached to this creature"
-- turns on it -- Pawl.TriggerSpec's "CR 608.3c whole card" case is the proof.
-- The ORDERING against the entry loop and the Moved event is the rule's rather
-- than anything a card in data/cards reads.
--
-- Stack's Aura branch is the only caller supplying a seed. An Aura entering by any
-- other route is CR 303.4f's, and the body below asks its controller what it will
-- enchant rather than letting it enter unattached -- see there.
--
-- `asOf` and `batch` are applyReplacementsIn's, supplied by changeZoneInBatch and
-- by changeZoneEnteringIn; every other door names a lone move and passes Nothing
-- and the empty set. `batch` is read twice below: CR 303.4f's host sweep
-- subtracts it, and the CR 614.1c entry loop is handed it. `tapped` is CR
-- 110.5b's status, Untapped for every door but changeZoneEntering. `entering` is
-- CR 122.6a's counters the object enters the battlefield with, empty for every
-- door but that one and inert for every destination but the battlefield.
-- `under` is the arriving incarnation's default controller -- CR 110.2a's entry
-- controller on the battlefield, CR 405.4's caster on the stack -- Nothing for
-- every door but changeZoneEntering and changeZoneCasting, and Nothing on the
-- first of those too for a move whose effect names no player, which by CR 110.2
-- and CR 108.4a leaves the owner answering. `shown` is CR 709.3's chosen
-- half or CR 712.13's carried face, Nothing for every door but
-- changeZoneShowing. `facing` is CR 110.5's face-up/face-down status, FaceUp for
-- every door but changeZoneFaceDown (CR 708.4) and a changeZoneEntering whose
-- riders say otherwise (CR 708.3), which is CR 110.5b's default standing
-- everywhere else. `position` is CR 401.2's end of
-- a library, the default for every door but changeZoneEntering. `concealed` is
-- CR 406.3's "exiled face down", False for every door but changeZoneEntering and
-- inert for every destination but exile.
--
-- `position` needs no `dest == requestedDest` gate, unlike `face` and `facing`
-- below: it is inert everywhere but a library, so a CR 616.1 redirect AWAY from
-- one drops it for free, and a redirect INTO one from a move that named no
-- position carries the default -- which is the right answer, since nothing said
-- top.
changeZoneAttaching :: Maybe GameState -> Set ObjectId -> ObjectId -> Zone -> LibraryPosition.LibraryPosition -> Maybe Recipient.Recipient -> TapState.TapState -> Map.Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural -> Maybe PlayerId -> Maybe CardName.CardName -> Facing.Facing -> Bool -> Game (Maybe ObjectId)
changeZoneAttaching asOf batch oid requestedDest position seed tapped entering under shown facing concealed = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure Nothing
    -- CR 111.8: "A token that has left the battlefield can't move to another
    -- zone or come back onto the battlefield. If such a token would change
    -- zones, it remains in its current zone instead." Nothing is this funnel's
    -- word for a move that did not happen -- the object is never touched, so it
    -- is still in the zone it was in, which is what the rule asks for.
    --
    -- Only a WITHIN-ONE-RESOLUTION window reaches this: CR 704.5d removes such a
    -- token the next time state-based actions are checked, and until then a slot
    -- an earlier clause bound to the token's exile or graveyard incarnation
    -- still names it. Flicker of Fate is the pool's producer -- "exile target
    -- creature or enchantment, THEN RETURN IT to the battlefield" -- and
    -- Pawl.ZoneChangeSpec is where the token and the card twin are separated.
    --
    -- Asked BEFORE the CR 616.1 replacement loop rather than after it, which is
    -- CR 101.2: a "can't" beats whatever would allow the move, so there is no
    -- event left for a replacement to choose among. Ordering it after the loop
    -- would answer the same, and not on a claim about Magic: the predicate reads
    -- the object and its CURRENT zone, neither of which any ZoneChangeR rewrites
    -- -- they rewrite the destination -- and that loop leaves the board alone on
    -- this path (see resolveZoneChange, which restricts it to ZoneChangeR
    -- candidates, so the one state-writing arm is out of reach).
    Just obj | Game.tokenHasLeftTheBattlefield obj -> pure Nothing
    Just obj -> do
      let pid = Object.owner obj
          fromZone = Object.zone obj
          -- CR 704.8: inside a batch, last known information is read from the
          -- board the batch began on, not from the live one -- so a permanent
          -- leaving alongside others is not projected against a board its
          -- siblings have already left. Departure.depart reads its own record
          -- the same way for CR 800.4a's first clause.
          --
          -- Live for an id the batch board does not hold: destroyIn follows the
          -- object its replacement loop settled on, which CR 614.6 lets a
          -- rewrite redirect to one that board never held.
          lki = case asOf of
            Just before | Maybe.isJust (Game.lookupObject oid before) -> before
            _ -> gs
          -- CR 608.2h: last known information -- the object as it exists in the
          -- zone it is LEAVING, projected against the pre-move state. Forced
          -- eagerly (Moved's snapshot field is strict) rather than left as a thunk
          -- retaining the whole pre-move GameState for a turn. The price of an
          -- honest history: a token has no printed card to re-derive from (CR
          -- 111.1).
          snapshot = Projection.project oid lki
          -- CR 613.1b: the OTHER half of last known information, read from the
          -- same pre-move state. Control is not a characteristic (CR 109.3's
          -- list does not include it), so it cannot ride `snapshot`; it is kept
          -- because CR 603.3a asks "who controlled its source at the time it
          -- triggered" about sources that are already gone -- see
          -- eventTriggers below.
          --
          -- The `Object.owner` fallback is unreachable rather than a guess:
          -- Projection.controllerOfGiven's own base case returns
          -- `Just (Object.owner obj)` for any id that resolves, and `oid`
          -- resolves in `lki` (either it is `gs`, which this branch matched
          -- `Just obj` against, or the guard above found it). It is written as a
          -- fallback only because controllerOf's type is honest about ids that
          -- do not.
          --
          -- A second board walk on the same hot path as `snapshot` above
          -- (controllerOf rebuilds controlGrants, a walk of the battlefield).
          -- Measured on the tasty-bench suite, this commit's parent vs. this
          -- change (goldfish / casting / fighting / fighting-aura, 2p):
          -- 15.2/133/24.6/569 ms -> 15.5/134/25.2/575 ms -- every move inside
          -- one run-to-run stddev, so no gate was moved to buy it back.
          lastController = Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf oid lki)
      -- CR 614.4: replacements exist before the event, so the loop reads them from
      -- the PRE-MOVE state. CR 614.6: the modified event is what actually happens.
      --
      -- `obj` and `snapshot` are read before this runs and still used after it
      -- returns, which is sound despite Replacement's AsCopy arm calling
      -- State.modify': this is a WouldChangeZone loop, restricted to ZoneChangeR
      -- candidates, so it cannot reach the EntryR arm AsCopy lives under -- and
      -- `gs` and `lki` are immutable values, so no downstream modify' can change
      -- what `snapshot` captured. Extending either loop to mutate state these bindings
      -- read would mean re-deriving them after that loop.
      --
      -- Both ids are `oid` in the PROPOSED event: nothing has moved yet.
      (resolved, exiledBy) <- resolveZoneChange asOf (ZoneChange.MkZoneChange oid oid fromZone requestedDest)
      case resolved of
        -- CR 614.6: nothing survived the loop, so no zone change happens. No
        -- producer today -- no ReplacementEffect in data/cards cancels a zone
        -- change outright, where the entry PROHIBITION one case down is a CR
        -- 101.2 "can't" rather than a rule 614 replacement -- but Maybe is what
        -- "the event does not happen" means on this path.
        Nothing -> pure Nothing
        -- CR 101.2 with CR 400.4a: an effect in force states this object can't
        -- enter the battlefield, and CR 101.2 makes that "can't" beat whatever
        -- allowed or directed the entry. CR 400.4a is the rulebook's own answer
        -- to what happens next -- "it remains in its previous zone" -- stated
        -- there for a card type, and again in CR 701.40f for a prohibited
        -- manifest. Grafdigger's Cage is the pool's printing, and CR 613.11 is
        -- why the prohibition is asked here rather than in Pawl.Engine.Projection.
        --
        -- Nothing, this funnel's CR 614.6 cancel arm one case up and CR 303.4g's
        -- answer one case down: the object is never deleted and never re-minted
        -- (CR 400.7), so the card in the graveyard or the library is the SAME
        -- object it always was. That is what separates a refusal from a redirect,
        -- and Pawl.EntryRestrictionSpec's "each card remains in its previous zone,
        -- as the same object" is the proof.
        --
        -- BEFORE placeObject and before the CR 614.1c entry loop, changeZoneEntering's
        -- `refused` reason: running the loop first would fire as-enters abilities
        -- for a card that never enters. Before mkObj too, which is CR 701.40f's
        -- "if it was face up, it remains face up" -- the face-down status
        -- changeZoneEntering computed for a manifest is written in mkObj and
        -- nowhere earlier, so a refused manifest leaves the library card face up
        -- with nothing to undo (Pawl.EntryRestrictionSpec's Soul Summons case).
        --
        -- AFTER the CR 616.1 replacement loop and asked of the SETTLED
        -- destination, `unlocking`'s reading and CR 614.6's: a redirect that sends
        -- the object anywhere but the battlefield is the event that happens, and
        -- an entry prohibition has nothing to say about it.
        --
        -- `fromZone` is the "previous zone" both rules name, read off the object
        -- rather than off `settled`: no ZoneChangeR rewrites an event's origin.
        --
        -- Not implemented: CR 608.3e's permanent spell, which this arm leaves on
        -- the stack instead of putting into its owner's graveyard (#2065). No card
        -- reaches it -- the pool's one entry prohibition names the graveyard and
        -- the library, never the stack.
        Just settled
          | ZoneChange.to settled == Zone.Battlefield && EntryRestriction.prohibited oid fromZone gs ->
              pure Nothing
        Just settled -> do
          let dest = ZoneChange.to settled
              -- CR 110.2a: "If an effect instructs a player to put an object onto
              -- the battlefield, that object enters the battlefield under that
              -- player's control unless the effect states otherwise." That
              -- control is BASE STATE -- CR 110.2 makes the entry controller a
              -- permanent's default controller thereafter, which is what
              -- Projection.defaultControllerOf reads -- and not a CR 613.1b
              -- layer-2 effect, which is the distinction CR 800.4c draws and
              -- which decides whether CR 800.4a's second clause can end it
              -- (Pawl.DepartureSpec's Meandering Towershell case is the proof).
              --
              -- BATTLEFIELD ONLY, the rule's own scope (CR 110.2, CR 110.5d):
              -- Projection.controllerOf answers for an object in any zone, so an
              -- ungated write would give a graveyard card a controller.
              --
              -- Gated on the SETTLED destination rather than the requested one,
              -- so a CR 616.1 rewrite that redirects the move decides this too
              -- (CR 614.6: the modified event is what happens). Indistinguishable
              -- from gating on the request today, and not because of a claim
              -- about Magic: no ReplacementEffect.ZoneChangeR in data/cards/
              -- names the battlefield as its destination -- every one of them
              -- names exile (see exiledByAfter, which rests on the same fact) --
              -- and the one RULES-based redirect in this funnel, rule 903.9b's
              -- offerCommandZone above, answers Zone.Command.
              --
              -- CR 400.7: Object.newIncarnation is the whole forgetting -- the
              -- entry controller (CR 110.2), the as-enters choices (CR 614.1c),
              -- damage, counters, bindings and the rest all go back to their
              -- no-memory values there, and Setup's two hand-written moves into
              -- a library call the same function. What is set back here is only
              -- what this MOVE decides: the destination, CR 613.7d's moment of
              -- entry, CR 110.5b's "enters tapped" (meaningful only for a
              -- battlefield destination, CR 110.5a), CR 110.2a's entry
              -- controller, CR 701.3's attach-on-entry seed, and CR 709.3a's
              -- chosen half.
              --
              -- `face` is among what newIncarnation clears, which is right by
              -- default: whichever half CR 709.3b singled out belonged only to
              -- the incarnation that left, and CR 709.4 gives a split card its
              -- two halves combined everywhere but the stack. `shown` is the
              -- exception the rules name -- CR 709.3, where the choice of half
              -- is made BEFORE the card is put onto the stack, and CR 709.3a,
              -- where "only that half is considered to be put onto the stack".
              -- Set HERE rather than written onto the returned incarnation, so
              -- that no reader inside the move can see the CR 400.7 object
              -- without its half.
              --
              -- Gated on the move ARRIVING where it was headed, which is the
              -- reading only a writer inside the move can have: CR 709.3a's half
              -- is "considered to be put onto the stack", so a CR 616.1 redirect
              -- that settles on another destination (CR 614.6: the modified event
              -- is what happens) means the card was never put onto the stack at
              -- all, and CR 709.4 gives it its two halves combined wherever it
              -- did land. Pawl.CastSpec's "a cast redirected off the stack keeps
              -- both halves" is the proof, and fails under the pre-#781 ordering
              -- -- a stamp applied to whatever the move handed back cannot ask
              -- this question, because the caller named a destination the move
              -- was free to overrule.
              --
              -- The SETTLED destination against the REQUESTED one rather than
              -- against Zone.Stack: the rule is that the face describes the move
              -- the caller asked for, not that a face is only ever a stack half.
              -- CR 712.13's face carried OUT of the stack is that same shape with
              -- Battlefield in both slots -- "a resolving double-faced spell that
              -- becomes a permanent is put onto the battlefield with the same face
              -- up that was face up on the stack" -- and Pawl.Engine.Stack's
              -- permanent branch is what passes it, through
              -- Pawl.Engine.Card.enteringFace. Pawl.ModalDoubleFacedSpec's
              -- "casting the back face puts the artifact onto the battlefield"
              -- proves it for a modal card, and Pawl.BattleSpec's "she may then
              -- cast it TRANSFORMED and FREE" for a transforming one.
              -- CR 709.5d: "A permanent with a shared type line is given the
              -- 'left half unlocked' designation as it enters the battlefield if
              -- its left half was cast as a spell. It is given the 'right half
              -- unlocked' designation as it enters the battlefield if its right
              -- half was a cast as a spell." `shown` is that half --
              -- Pawl.Engine.Card.enteringFace carried it off the stack, exactly
              -- as CR 712.13's face is carried -- so a Room spends it on a
              -- DESIGNATION where a double-faced card spends it on `face`, and
              -- the two are mutually exclusive below.
              --
              -- Written INSIDE the move rather than stamped onto what it hands
              -- back, which is CR 709.5d's own "as it enters the battlefield":
              -- the CR 400.7 incarnation must never exist without the
              -- designation, so nothing in the CR 616.1 entry loop and nothing
              -- in the CR 709.5h trigger scan can see a Room whose doors are all
              -- shut when one of them was cast. CR 614.12 is what makes the
              -- first of those observable rather than merely tidy -- an entry
              -- replacement reads the object's characteristics "as it would
              -- exist on the battlefield", and a Room with every door shut has
              -- no text for such a replacement to be printed in. `face` above
              -- takes the same position for CR 709.3a's chosen half.
              --
              -- Gated on the BATTLEFIELD rather than on the move arriving where
              -- it was headed, which the two clauses of the rule name in as many
              -- words. A Room spell countered onto its owner's graveyard, or a
              -- CR 616.1 redirect that lands it anywhere but the battlefield, is
              -- not a permanent and has no designations (CR 709.5c).
              --
              -- Nothing to unlock when neither half was cast, which is CR
              -- 709.5d's own last sentence: "If it's entering the battlefield and
              -- neither half was cast as a spell, it enters with neither unlocked
              -- designation." A Room put onto the battlefield by an effect
              -- reaches this with `shown` Nothing and enters with both doors shut.
              unlocking = dest == Zone.Battlefield && maybe False Card.hasSharedTypeLine (Game.cardOf oid gs)
              -- CR 110.5's status the ARRIVING incarnation will carry. Named
              -- because two readers want it: mkObj's `facing` field below, whose
              -- comment has the reasoning, and the CR 303.4f gate further down,
              -- which must not ask about an Aura card entering FACE DOWN -- CR
              -- 708.2a leaves such a permanent with no subtypes and no enchant
              -- ability, so it is not an Aura the rule speaks about. That read
              -- cannot come off the projection here, which sees the object face up
              -- in the zone it is leaving.
              entryFacing = if dest == requestedDest then facing else Facing.FaceUp
              mkObj entrySeed ts =
                (Object.newIncarnation obj)
                  { Object.zone = dest,
                    Object.timestamp = ts,
                    Object.tapped = tapped,
                    Object.attachedTo = entrySeed,
                    -- CR 109.4: only the stack and the battlefield give an object
                    -- a controller, so those are the two destinations that keep
                    -- the caller's player and every other clears it. The stack
                    -- arm is CR 405.4's, written by changeZoneCasting alone; the
                    -- battlefield arm is CR 110.2a's.
                    Object.enteredUnder = if dest == Zone.Battlefield || dest == Zone.Stack then under else Nothing,
                    -- `unlocking` is what excludes a Room here: CR 709.5c gives
                    -- a permanent with a shared type line designations rather
                    -- than a face that is up, and both halves go on being halves
                    -- of the one permanent (CR 709.5b), so there is no half for
                    -- this field to single out. Leaving `shown` here as well
                    -- would give Game.resolveFaceFor two answers to choose
                    -- between, one of which stops being right the moment the
                    -- second door opens.
                    Object.face = if dest == requestedDest && not unlocking then shown else Nothing,
                    Object.unlockedHalves = if unlocking then foldMap Set.singleton shown else Set.empty,
                    -- CR 708.4 / 708.3: the object is turned face down BEFORE it
                    -- is put onto the stack or enters the battlefield, so this is
                    -- part of the move rather than a stamp on what the move
                    -- returned -- CR 709.3a's `face` above, one status over.
                    --
                    -- Gated on the move ARRIVING where it was headed, for
                    -- `face`'s reason and one of its own: CR 708.9 reveals a
                    -- face-down permanent to all players as it leaves the
                    -- battlefield, so a CR 616.1 redirect that lands the object
                    -- anywhere else must leave it face up.
                    Object.facing = entryFacing,
                    -- CR 406.3: an exiled card is kept face up unless the effect
                    -- exiled it face down, so this is part of the move for
                    -- `facing`'s reason -- the card is never face up in exile
                    -- for an instant, not even for the Moved event.
                    --
                    -- Gated on the destination as well as on the move arriving
                    -- there, which the two statuses above need only the second
                    -- half of: CR 406.3 is a rule about the EXILE ZONE, so a CR
                    -- 616.1 redirect into any other zone leaves an ordinary face
                    -- up card, and an effect that names the rider on a move
                    -- somewhere else says something no rule reads.
                    --
                    -- Nothing observes either gate today: the pool's one
                    -- face-down exile names exile, and no replacement redirects
                    -- it. Both are regression fences rather than proven
                    -- behaviour -- dropping them leaves the suite green.
                    Object.exiledFaceDown = concealed && dest == requestedDest && dest == Zone.Exile,
                    -- CR 107.3m: the one thing CR 400.7's forgetting carries
                    -- across, and the rule states it as an exception. The value
                    -- of X for a permanent's enters-the-battlefield replacement
                    -- effect is the value chosen for the spell that became it,
                    -- so the announcement is read off the DEPARTING object's
                    -- CR 601.2b binding -- which `newIncarnation` has just
                    -- cleared -- and written onto the arriving one.
                    --
                    -- Read here rather than passed in by Pawl.Engine.Stack,
                    -- because the funnel already holds the object that has the
                    -- binding, and the rule is about any object that entered
                    -- the battlefield as a resolving spell rather than about
                    -- one caller's route.
                    --
                    -- BATTLEFIELD ONLY, the rule's own scope: a countered spell
                    -- on its way to a graveyard becomes a card, and CR 107.3g
                    -- puts the X of a card outside the stack at 0. Nothing for
                    -- every move whose object bound no X, which is every move
                    -- but a resolving {X} permanent spell's.
                    --
                    -- That gate is a regression fence rather than proven
                    -- behaviour: the field's only reader is the entry
                    -- replacement mint, which asks it of a permanent, so
                    -- dropping the gate leaves the suite green.
                    Object.announcedX = if dest == Zone.Battlefield then Binding.amountOf Binding.variableX (Object.bindings obj) else Nothing,
                    -- CR 400.7d: "an ability of a permanent can reference
                    -- information about the spell that became that permanent as
                    -- it resolved, including WHAT COSTS WERE PAID to cast that
                    -- spell." Rule 702.33d's designation is exactly such a cost
                    -- record, and Monstrous War-Leech's CR 614.1c rewrite is the
                    -- ability that references it -- asked of the PERMANENT, since
                    -- the permanent is what enters.
                    --
                    -- So this is `announcedX` above's second instance rather than
                    -- a new idea: `newIncarnation` has just cleared the flag with
                    -- everything else CR 400.7 forgets, and the exception the rule
                    -- names is written back here, off the departing object.
                    --
                    -- BATTLEFIELD ONLY, and here the gate carries weight where
                    -- announcedX's does not: a kicked spell countered on its way
                    -- to a graveyard becomes a card, and rule 400.7d speaks only
                    -- about a permanent. Nothing else reads the flag off an object
                    -- outside the stack.
                    Object.kicked = Object.kicked obj && dest == Zone.Battlefield,
                    -- CR 400.7d a third time, and rule 702.150a is the ability
                    -- that references it: how many of the spell's Phyrexian mana
                    -- symbols were announced to be paid with life (CR 601.2b).
                    --
                    -- BATTLEFIELD ONLY, `kicked`'s gate and for its reason: rule
                    -- 702.150a is about a permanent entering, and a countered
                    -- spell on its way to a graveyard becomes a card no compleated
                    -- ability can be on.
                    Object.phyrexianLifePaid = if dest == Zone.Battlefield then Object.phyrexianLifePaid obj else 0,
                    -- CR 400.7d a fourth time, and this is the clause the rule
                    -- names last: "what mana was spent to pay those costs". CR
                    -- 107.4h's third sentence is what references it, so Berg
                    -- Strider's "if {S} was spent to cast this spell" is asked of
                    -- the permanent that spell became.
                    --
                    -- BATTLEFIELD ONLY, `kicked`'s gate and for its reason: rule
                    -- 400.7d speaks about a permanent, and a countered spell
                    -- becomes a card no such ability can be on.
                    Object.manaSpent = if dest == Zone.Battlefield then Object.manaSpent obj else Mana.MkMana []
                  }
              -- CR 604.2's override, handed over as the permanent leaves the
              -- battlefield. lingeringHandover below is the whole of it; this
              -- road gates on the zone the object is LEAVING, which is what
              -- makes an ordinary zone change pay nothing for the read.
              handover = if fromZone == Zone.Battlefield then lingeringHandover oid lastController gs else []
          -- CR 303.4f: an Aura entering the battlefield "by any means other than
          -- by resolving as an Aura spell", where "the effect putting it onto the
          -- battlefield doesn't specify the object or player the Aura will
          -- enchant". That second clause IS `seed` being Nothing: a door supplies
          -- one exactly when the effect it carries named a destination -- CR
          -- 303.4a's target for Pawl.Engine.Stack's Aura branch, which is also the
          -- only door a resolving Aura spell takes, and the fixed host for
          -- Pawl.Engine.Resolve.putFound's search destination (Auratouched Mage).
          -- So the rule's own exclusion is carried without this funnel asking what
          -- kind of move it is.
          --
          -- Gated on the SETTLED destination, `unlocking`'s reading one binding up:
          -- a CR 616.1 redirect that sends the Aura anywhere but the battlefield
          -- (CR 614.6: the modified event is what happens) must not raise the
          -- prompt.
          --
          -- Asked of `gs`, the PRE-MOVE board, which is where the Aura still is and
          -- where the hosts already are. Attach.hostsFor sweeps the battlefield and
          -- Attach.attachmentFor reads Projection.subtypesOf and Game.faceOf, both
          -- of which answer for an object in any zone.
          --
          -- MINUS `batch`, which is CR 608.2f: the members of a batch enter
          -- simultaneously, so a would-be host arriving beside this Aura is not on
          -- the battlefield at the moment CR 303.4f's choice is made, however the
          -- funnel happens to order the two moves. Replenish over a graveyard
          -- holding an Aura and an enchantment creature is the board
          -- (Pawl.AuraSpec). Subtracting the SET rather than sweeping the `asOf`
          -- board: the set names exactly "entered beside me", where that board
          -- would also re-offer a host the batch itself removed.
          --
          -- Filter.CanHostSubject ALONE, because CR 303.4f's "a legal object or
          -- player according to the Aura's enchant ability and any other applicable
          -- effects" is the whole restriction -- there is no card text to intersect
          -- it with, which is the difference from CR 303.4k's Attach.turnUpHosts.
          --
          -- The Aura test is the PROJECTION's subtypes (CR 205.3 -- CR 303.4 speaks
          -- about characteristics) rather than the printed type line
          -- Pawl.Engine.Card.isAura reads, and the two now differ: Cloudform GRANTS
          -- itself the Aura subtype (Modification.AddSubtype) and the enchant
          -- ability that goes with it (Modification.GainEnchant). Unobserved here
          -- all the same -- Cloudform grants both while already on the battlefield,
          -- so no board in this pool has a granted Aura ENTERING one.
          --
          -- `entryFacing` and not the projection is what answers CR 708.2a, and the
          -- distinction matters: the projection sees the object in the zone it is
          -- LEAVING, where an Aura card manifested off a library (CR 701.40a) is
          -- still face up and would trip this gate. A permanent entering face down
          -- has no subtypes and no enchant ability, so it is not an Aura CR 303.4f
          -- speaks about -- it enters unattached, and Sba.fallsOff leaves it alone,
          -- reading a projection the substituted face seeds. Pawl.AuraSpec's "a manifested
          -- Aura card is not an Aura" is the proof: Soul Summons manifests an Unholy
          -- Strength over two legal hosts, and dropping the conjunct raises a prompt.
          --
          -- Answering Nothing is CR 303.4g's "the Aura remains in its current
          -- zone": this funnel's CR 614.6 cancel arm above, one case over, so the
          -- object is never deleted and never re-minted (CR 400.7) and the graveyard
          -- card is the same object it always was.
          --
          -- Not implemented: CR 303.4g's other branch, for an Aura whose current
          -- zone is the stack or that is a token, and CR 303.4i's effect that names
          -- an attachment the Aura can't legally enchant (gap #1734).
          settledSeed <-
            if dest == Zone.Battlefield && entryFacing == Facing.FaceUp && Maybe.isNothing seed && Set.member Subtype.Aura (Projection.subtypesOf oid gs)
              then do
                -- CR 303.4f's "that player" is CR 110.2a's entry controller, which
                -- is `under` -- falling back to the owner when the effect named
                -- nobody, per CR 110.2 and CR 108.4a, exactly as the field's own
                -- note above says.
                let chooser = Maybe.fromMaybe pid under
                    hosts = filter (\h -> not (Set.member h batch)) (Attach.hostsFor chooser oid oid Filter.Type.CanHostSubject gs)
                chosen <- Attach.chooseHost chooser oid hosts
                -- THE TAG the Aura's own enchant slot produced, never a hand-built
                -- ToObject: Sba.stillLegalEnchant compares the (pool, tag) pair, so
                -- a ToObject stored where the slot offers a ToCreature falls through
                -- and CR 704.5m buries the Aura on the next pass.
                --
                -- attachmentFor answering Nothing collapses into CR 303.4g's
                -- "remains in its current zone" too, and is unreachable rather than
                -- a second reading: hostsFor's Filter.CanHostSubject conjunct is
                -- that same function, so every candidate it offered admits.
                pure (fmap Just (chosen >>= \h -> Attach.attachmentFor oid (Recipient.ToObject h) gs))
              else pure (Just seed)
          case settledSeed of
            Nothing -> pure Nothing
            Just entrySeed -> do
              State.modify' $ \g ->
                let g1 = Game.removeFromZones pid oid g
                 in g1
                      { GameState.objects = Map.delete oid (GameState.objects g1),
                        -- Stored as the permanent goes, in the same write that
                        -- removes it: nothing can observe a board where the
                        -- permanent has left and its effect has not yet been handed
                        -- over.
                        GameState.continuousEffects = handover <> GameState.continuousEffects g1,
                        -- CR 608.2h: the object ceases here, so this is the last
                        -- moment its information is known. Filed under the id it had
                        -- while it existed -- the id an ability on the stack still
                        -- carries as its source (CR 113.7) -- and from the same
                        -- `snapshot` the Moved event below records, so the two
                        -- readings of "what was it" cannot drift apart.
                        --
                        -- The counters come off `obj`, the PRE-MOVE object, and not
                        -- off the incarnation `mkObj` builds: CR 122.2 makes them
                        -- cease to exist on the zone change, so the last moment they
                        -- can be recorded is this one.
                        --
                        -- The COPIABLE snapshot is taken here for the counters'
                        -- reason: it is layer 1 (CR 613.1a) rather than the fold
                        -- `snapshot` is, and the copy binding and face it reads live
                        -- on `obj`, which is about to cease. No third board walk --
                        -- it reads that binding or the printed face and stops.
                        GameState.lastKnown = Map.insert oid (LastKnown.MkLastKnown snapshot lastController (Object.source obj) (Object.counters obj) (copiedSnapshot oid gs) (Object.attachedTo obj)) (GameState.lastKnown g1)
                      }
              newId <- placeObject pid (mkObj entrySeed) dest position
              -- CR 614.1c-d: entry replacements apply to BATTLEFIELD entries and
              -- nowhere else. CR 616.1g's nesting of one event inside another is
              -- expressed as call nesting rather than a field. `batch` is the
              -- same-batch siblings, empty for every door but changeZoneEnteringIn
              -- (CR 614.12a; see applyReplacementsIn for why 614.12a and not
              -- 614.13a).
              Monad.when (dest == Zone.Battlefield) $ do
                -- CR 122.6a: the counters the EFFECT says the object enters with --
                -- undying's and persist's "with a +1/+1 counter on it". Inside the
                -- move, before the entry loop and before the Moved event below, so
                -- nothing can see the permanent without them (the tap state's reason,
                -- one field over). Through putCounters, CR 122.6's funnel, so CR
                -- 614.16 applies and Doubling Season sees them, exactly as the
                -- EntryRewrite.WithCounters arm inside the loop does.
                --
                -- Before the loop rather than after it, which no card observes: no
                -- entry replacement in the pool reads a counter the entering object
                -- already has, and the two are simultaneous in the rules anyway.
                Monad.mapM_ (\(kind, n) -> Monad.void (putOwnCountersIn batch newId kind n)) (Map.toAscList entering)
                runEntry batch newId
              -- CR 709.5h: an ability that triggers on a door opening fires "regardless
              -- of whether it was given that designation while entering the
              -- battlefield or after entering the battlefield", so the entry
              -- designation `unlocking` wrote into mkObj above needs its event too.
              -- Recorded rather than routed through unlockHalf, which would find the
              -- door already open and record nothing: writing the designation inside
              -- the move is what CR 709.5d's "as it enters" asks for, and the event is
              -- what CR 709.5h asks for -- two rules, and the entry is the one place
              -- they are not the same write.
              --
              -- BEFORE the Moved event, so a Room's own "when you unlock this door"
              -- and its "when this enters" are gathered in one scan with the door's
              -- event first. The two are simultaneous and CR 603.3b lets their
              -- controller order them on the stack, so nothing observable rides on
              -- which is logged first.
              --
              -- CR 709.5i's flag is computed here too, through the same
              -- `fullyUnlockedAfter` unlockHalf uses, and against the designations
              -- `mkObj` actually wrote. Reading `shown` back rather than the stored
              -- object, so the two writers answer the question the same way from the
              -- same input. Always False today, and that is CR 709.5d rather than a
              -- shortcut: an entry gives at most ONE designation, so a two-door Room
              -- can never arrive fully unlocked. CR 709.5i's second branch -- a
              -- permanent that "has neither designation and gains both" -- is
              -- therefore unreachable and untested (#962).
              Monad.forM_ (if unlocking then Maybe.maybeToList shown else []) $ \half ->
                State.modify' (recordEvent (GameEvent.HalfUnlocked (HalfUnlocked.MkHalfUnlocked newId half (fullyUnlockedAfter (foldMap Set.singleton shown) (Game.cardOf oid gs)))))
              -- CR 603.2g: record the RESOLVED event, carrying the NEW object's id --
              -- what an enters trigger scans -- alongside the id it had in `fromZone`,
              -- which is the key `lastKnown` is filed under and so the only route back
              -- once CR 400.7 has minted a new incarnation (CR 603.10a's look-back
              -- reads it). Recorded LAST, so the entry loop's choices are locked in
              -- before any trigger or SBA can observe the object.
              -- CR 608.3c and CR 303.4f: a permanent that ARRIVES attached became
              -- attached, which is the half Event.attach cannot record -- there is
              -- no CR 701.3 move here, the seed goes on the incarnation `mkObj`
              -- mints and the id it names did not exist a moment ago. Bramble
              -- Elemental's "whenever an Aura becomes attached to this creature"
              -- fires for a cast Pacifism through this line and for Crown of the
              -- Ages through the other one.
              --
              -- Gated on the SETTLED destination, `unlocking`'s reading: a seed
              -- rides along on every move (mkObj writes the field whatever the
              -- zone), and only the battlefield has attachments. CR 614.6's
              -- redirect elsewhere leaves it unread and unrecorded.
              --
              -- Carries `newId`, the CR 400.7 incarnation, rather than the id the
              -- Aura spell had on the stack: that is the object a trigger scan
              -- will find attached.
              Monad.forM_ (if dest == Zone.Battlefield then entrySeed else Nothing) $ \host ->
                State.modify'
                  . recordEvent
                  $ GameEvent.BecameAttached
                    BecameAttached.MkBecameAttached
                      { BecameAttached.attachment = newId,
                        BecameAttached.host = host
                      }
              -- CR 607.2b: this move ended in exile because somebody's replacement
              -- effect sent it there, so the arriving card is linked to THAT
              -- object. Filed here, at the one place the CR 400.7 incarnation
              -- first has an id, and before the Moved event so nothing can read
              -- the arrival with the link missing.
              --
              -- Resolve.recordExiledWith's diff runs afterwards and would
              -- otherwise file this same card against whatever effect was
              -- running; its insertWith keeps the entry already present, which is
              -- what makes this write the one that stands.
              Monad.forM_ (if dest == Zone.Exile then exiledBy else Nothing) $ \linked ->
                State.modify' (\g -> g {GameState.exiledWith = Map.insert newId linked (GameState.exiledWith g)})
              State.modify' (recordEvent (GameEvent.Moved (Moved.MkMoved (ZoneChange.MkZoneChange oid newId fromZone dest) snapshot)))
              pure (Just newId)

-- CR 604.2 ends a static ability's continuous effect the moment its permanent
-- leaves the battlefield. A card whose own text overrides that -- Titania's
-- Song's "If this enchantment leaves the battlefield, this effect continues
-- until end of turn" -- needs the effect to outlive the ability generating it,
-- and the only way an effect outlives its source is to become a STORED one (CR
-- 611.2). So the abilities this permanent prints with a StaticAbility.lingers
-- duration are handed over here, as it goes.
--
-- Everything is read from `gs`, the board the permanent is still on and still
-- projecting from, so what is handed over is the effect that was actually
-- applying. CR 611.2c then fixes its set for good: the noncreature artifacts
-- Titania's Song names at THIS instant, never the live filter -- an artifact
-- that enters after the Song is gone is not animated. Pawl.ExpirySpec's
-- whole-card case proves both halves, and its Humility case is the control for
-- the gate: an ability with no such clause hands over nothing and ends at once.
--
-- The timestamp is the departing permanent's own, the one CR 613.7a already gave
-- this effect, rather than a fresh one: the card says "this effect CONTINUES",
-- so it is the same effect and nothing reorders it against its neighbours within
-- its layers. CR 613.7b's fresh timestamp belongs to an effect the resolution of
-- a spell or ability creates, and nothing resolved here.
--
-- ONE stored effect per PART, since a stored effect carries a single
-- modification. CR 613.6's "one set for the whole effect" survives that split by
-- construction: frozenStaticParts freezes every part of an ability to the same
-- objects, so asking each of them separately can only get the same answer back.
--
-- `lastController` is CR 109.5's "you" for the arming, read from the same
-- pre-departure board.
--
-- TWO roads reach this, because a permanent leaves the battlefield two ways:
-- changeZoneAttaching above for a zone change, and
-- Pawl.Engine.Departure.objectsLeaveWith for CR 800.4a's first clause, which
-- takes a departing player's objects out of the game without one. Neither gate
-- is written here: this function is asked about a permanent that has already
-- been decided to be leaving, and each caller decides that its own way -- the
-- funnel off Object.zone, the departure off GameState.battlefield membership,
-- which is CR 702.26b on that road: a phased-out permanent is treated as though
-- it does not exist, so it was generating no effect to continue -- CR 702.26k
-- still takes it out of the game with its owner.
--
-- Short-circuited on the cheap COPIABLE read (Projection.staticAbilitiesOf,
-- which is projection-free), so an ordinary zone change -- every one in the pool
-- but this card's -- never pays for the projection frozenStaticParts spends.
--
-- That read and not the printed face, because `n` is CR 613.6's index into the
-- list Projection.permanentParts tagged, and that walk reads the copiable list
-- (CR 707.2a). Indexing a different list here would join the wrong ability's
-- duration onto the frozen part, with no type error to say so.
lingeringHandover :: ObjectId -> PlayerId -> GameState -> [ContinuousEffect.ContinuousEffect Card]
lingeringHandover oid lastController gs =
  let lingering :: [(Natural, Duration.Duration)]
      lingering =
        [ (n, duration)
        | (n, sa) <- zip [0 ..] (Projection.staticAbilitiesOf oid gs),
          duration <- Maybe.maybeToList (StaticAbility.lingers sa)
        ]
   in if null lingering
        then []
        else
          [ ContinuousEffect.MkContinuousEffect
              { ContinuousEffect.source = oid,
                ContinuousEffect.timestamp = ts,
                ContinuousEffect.expiry = expiry,
                ContinuousEffect.modification = modification,
                ContinuousEffect.affected = Affected.TheseObjects frozen
              }
          | (n, ts, modification, frozen) <- Projection.frozenStaticParts oid gs,
            duration <- fmap snd (filter ((== n) . fst) lingering),
            -- No bindings to bake a CR 611.2b condition against: this duration is
            -- a PRINTED static ability's, and no resolution chose anything for it.
            expiry <- Maybe.maybeToList (Expiry.arm Map.empty lastController oid duration gs)
          ]

-- The single destruction funnel (CR 701.8 / 702.12b): the Destroy opcode and the
-- CR 704.5g/h state-based actions both flow through here.
--
-- Takes the WHOLE BATCH rather than one permanent at a time, because CR 608.2f
-- and CR 704.3 make the batch one simultaneous event. A lone destruction is the
-- one-element batch.
--
-- CR 702.12b's indestructible gate is judged for every member against the state
-- the batch began in, BEFORE any of them moves -- which is why this takes a list.
-- A permanent granting the others indestructible is still on the battlefield at
-- that moment even when it is itself in the batch, so it dies alone. Judging each
-- member against a board its predecessors had left would make the answer depend on
-- an order CR 608.2f gives nobody the right to decide.
--
-- The gate also precedes the replacement loop, per CR 614.7: an event that never
-- happens neither applies nor consumes a regeneration shield. Otherwise the
-- would-be-destroyed event is offered to CR 616.1, and a survivor goes to its
-- owner's graveyard via changeZone. Ungated for CR 701.19c (#42).
--
-- The door for a batch that is a whole event whose caller does not care what died;
-- destroyReturning is the same door for a caller that does, destroyInBatch for a
-- batch nested in a larger event, and destroyIn is the shared body.
--
-- This door and destroyReturning are the EFFECT's, which is why neither takes a
-- Pawl.Types.DestructionCause: a caller with a whole event of its own to destroy in
-- is a resolving spell or ability (CR 609.1), and CR 122.1c's shield reads that.
-- The rules' own destructions come through destroyInBatch, which asks.
destroy :: Regenerability.Regenerability -> [ObjectId] -> Game ()
destroy regenerability oids = Monad.void (destroyIn Nothing DestructionCause.ByEffect regenerability oids)

-- destroy, answering with the permanents it ACTUALLY destroyed (CR 701.8b), which
-- is emphatically not the batch it was handed: an indestructible member never
-- reaches the destruction event (CR 702.12b) and a regenerated one has it replaced
-- (CR 701.8c), so neither is in this answer.
--
-- Each surviving destruction reports the SETTLED object rather than the one asked
-- about, for the reason the graveyard move follows it -- a CR 616.1 rewrite may
-- redirect the destruction. Nothing in the pool does, so the lists are equal today.
--
-- PAIRED with the CR 400.7 incarnation the graveyard move minted, which is a
-- different object from the one destroyed and the only one a later clause can
-- name: "put into a graveyard this way" is a card in a graveyard, and the
-- permanent that was destroyed no longer exists. Nothing for a move the CR 616.1
-- loop cancelled, and the id alone says nothing about WHERE it landed -- a CR
-- 614 replacement may have sent it to exile instead (Rest in Peace), which is
-- why the reader asks the board rather than trusting this answer's presence.
--
-- A second door rather than a return type on `destroy`, the changeZoneReturning
-- posture: only the Destroy opcode's bound slots use the answer.
destroyReturning :: Regenerability.Regenerability -> [ObjectId] -> Game [(ObjectId, Maybe ObjectId)]
destroyReturning = destroyIn Nothing DestructionCause.ByEffect

-- destroy for a batch that is one PART of a larger simultaneous event, whose board
-- is `asOf`. CR 704.3's state-based-action check is that event, and Sba is the
-- only caller: its put-into-graveyard and destruction batches are a sequence only
-- in the implementation, so both stand on the board the pass began in -- an
-- animated Rest in Peace the pass itself buries still exiles the card of the
-- creature the pass destroys.
--
-- A separate door rather than a parameter on `destroy`, for changeZoneInBatch's
-- reason: every other caller has no larger event to name.
--
-- The one door that ASKS for the Pawl.Types.DestructionCause, because it is the one
-- whose caller may be a rule: CR 704.5g/h destroy with no effect involved, which is
-- what CR 122.1c's replacement does not reach.
destroyInBatch :: GameState -> DestructionCause.DestructionCause -> Regenerability.Regenerability -> [ObjectId] -> Game ()
destroyInBatch asOf cause regenerability oids = Monad.void (destroyIn (Just asOf) cause regenerability oids)

-- The shared body. Three readers of a board, and they do NOT all get the same one:
--
--   1. The CR 616.1 replacement loops -- the destruction's and the
--      put-into-graveyard that follows -- collect from `gs`, the containing
--      event's board. That is CR 608.2f / 704.3's "single event" reading: the
--      effects in force are the ones that existed before it, so an effect
--      belonging to a permanent the same event is removing still applies.
--   2. The CR 702.12b gate reads `gs` too. Indestructibility is a fact about the
--      permanent when the event's conditions were judged; letting an earlier part
--      of the same event change it would make the answer depend on an order CR
--      608.2f gives nobody the right to decide.
--   3. The existence filter reads `live`, NOT `gs`, per CR 614.7: an object an
--      earlier part of the event already put into a graveyard is not on the
--      battlefield to be destroyed, so no destruction event happens for it and no
--      regeneration shield may be offered one. The reachable shape is an Aura
--      named by CR 704.5m and CR 704.5g in the same pass.
--
-- CR 122.1c is the first DestructionR for which (1) is not vacuous: a shield
-- counter's replacement is minted from the permanent holding the counter
-- (Projection.shieldOf), so the destruction loop finds it on the FROZEN board rather
-- than on the live one. Every other DestructionR in the pool is a regeneration
-- shield in the floating store, which the frozen board does not hold at all.
--
-- The whole body is ONE event, which is what `simultaneously` stamps on
-- everything it records: CR 608.2f makes an action taken on multiple objects
-- simultaneous, and every caller here hands a batch that one effect named. That
-- is the same sentence the three readings above already rest on -- a batch judged
-- against one board, not a sequence -- so the bracket adds no claim they do not.
-- Day of Judgment's deaths are the case it decides, and CR 603.10a's own Example
-- is why they must not be a sequence.
destroyIn :: Maybe GameState -> DestructionCause.DestructionCause -> Regenerability.Regenerability -> [ObjectId] -> Game [(ObjectId, Maybe ObjectId)]
destroyIn asOf cause regenerability oids = simultaneously $ do
  live <- State.get
  let gs = Maybe.fromMaybe live asOf
      doomed = filter (\oid -> Maybe.isJust (Game.lookupObject oid live) && not (Projection.hasKeyword Keyword.Type.Indestructible oid gs)) oids
  fmap Maybe.catMaybes . Monad.forM doomed $ \oid -> do
    settled <- resolveDestruction (Just gs) cause regenerability oid
    case settled of
      -- CR 701.8c: a regeneration effect REPLACED the destruction, so nothing was
      -- destroyed here and this member is not in the answer.
      Nothing -> pure Nothing
      -- The graveyard move follows the SETTLED object, so a rewrite redirecting
      -- the destruction is honoured. changeZone is a no-op for an object already
      -- gone, which is what makes naming the batch's members up front safe.
      Just target -> do
        arrived <- changeZoneInBatchReturning gs target Zone.Graveyard
        pure (Just (target, arrived))

-- The single countering funnel (CR 701.6a -- not to be confused with putCounters
-- above, CR 122.6's counter markers).
--
-- Two endings, because that rule's last sentence is about a SPELL and its first
-- two about "a spell or ability". Which applies is decided by Game.isAbility, a
-- classification of the object's kind and never a question about the countering
-- effect:
--
--   * a SPELL goes to its owner's graveyard through the changeZone funnel, so
--     Rest in Peace's redirect and CR 400.7's new incarnation still compose;
--   * an ABILITY ceases (CR 608.2n). Not a zone change: an ability has no owner's
--     graveyard, nothing arrives anywhere, and there is no destination for CR 614
--     to replace.
--
-- The ability branch records NO event, so no trigger can watch it (#541). Widening
-- GameEvent.SpellCountered is the wrong direction -- its one reader asks about
-- countering A SPELL and must stay silent here. What a countered ability does
-- leave is counterReturning's answer, which is the RESOLUTION's own tally (Glen
-- Elendra's Answer's "countered this way") and not a look-back at the history.
--
-- TWO "can't be countered" gates, one per carrier. CR 101.2 makes either the whole
-- story: the countering effect resolves and does nothing. Neither is targeting
-- immunity -- neither rule grants shroud -- which is why the gates are here and
-- not in Target. They precede the zone change for destroy's CR 614.7 reason.
--
--   * CR 113.6g's, read off the SPELL's own card (Rending Volley), since there is
--     no battlefield projection of a spell. Self-referential by that rule's own
--     wording -- "an object's ability that states IT can't be countered" -- so it
--     is asked only on the spell branch: an ability on the stack has no card for
--     it to be printed on, and Game.faceOf answers Nothing for one.
--   * CR 611.1 / 613.11's, asked of Pawl.Engine.PlayerEffect (Spider-Punk). That
--     one is an ability of a BATTLEFIELD PERMANENT about other objects, so it is
--     gathered from the battlefield like any other player effect and reaches BOTH
--     of CR 701.6a's subjects -- which is why it, unlike the gate above, is asked
--     ahead of the branch split rather than inside one branch. It is asked of the
--     VICTIM's controller: CR 113.8 for an ability, CR 601.2a for a spell.
--
-- On the spell branch, records a SpellCountered ALONGSIDE the zone change's Moved
-- event, never instead of it: the Moved event is the CR 400.7 change and this one
-- is what the change WAS. That is what distinguishes a countered spell from one
-- that RESOLVED into the same graveyard (CR 608.2n), and what survives Rest in
-- Peace redirecting the destination.
--
-- Nothing is recorded on any of the paths that do NOT counter, which CR 603.2g
-- makes mandatory rather than tidy: an id with no object; any of the three
-- can't-be-countered gates, since through CR 101.2 such a spell was never
-- countered; and a move the CR 616.1 loop cancelled, which leaves the spell on
-- the stack. The ability branch is not one of them -- that countering really
-- happened, and its silence is #541.
--
-- `source` and `controller` are the countering spell or ability and its controller
-- (CR 405.4), taken from the caller rather than re-derived: by the time the CR
-- 117.5 scan reads this event the controller can no longer be asked for exactly
-- (see Pawl.Types.Countering).
counter :: ObjectId -> PlayerId -> ObjectId -> Game ()
counter source controller oid = Monad.void (counterOne source controller oid)

-- counter over a whole batch, answering with the objects it ACTUALLY countered
-- (CR 701.6a) -- which is emphatically not the batch it was handed: an id naming
-- no object, any can't-be-countered gate, and a move the CR 616.1 loop
-- cancelled each leave their victim out of the answer.
--
-- The VICTIMS as the caller named them, not the graveyard incarnations CR 400.7
-- mints, because every reader wants how many rather than which: Swift Silence's
-- "draw a card for each spell countered this way" and Glen Elendra's Answer's
-- "for each spell and ability countered this way", both of which
-- Pawl.Engine.Resolve binds as an amount. An ability leaves no new object at all
-- (CR 608.2n), so there is no second id to report for it -- which is why the
-- answer is the victims and not the incarnations.
--
-- A second door rather than a return type on `counter`, the destroyReturning
-- posture: only the Counter opcode's bound-count slot uses the answer.
counterReturning :: ObjectId -> PlayerId -> [ObjectId] -> Game [ObjectId]
counterReturning source controller = Monad.filterM (counterOne source controller)

-- The shared body of both doors: counter ONE object, answering whether it was.
counterOne :: ObjectId -> PlayerId -> ObjectId -> Game Bool
counterOne source controller oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure False
    -- CR 613.11's gate first, ahead of the branch split, because it is the one
    -- that reaches both of CR 701.6a's subjects.
    Just _ | protectedFromCountering oid gs -> pure False
    -- CR 106.6 through CR 101.2, and ahead of the branch split for CR 613.11's
    -- reason: rule 106.6 says an additional effect "affects the spell or
    -- ability that mana is spent on", so both of CR 701.6a's subjects are in
    -- reach. An ability records no payment today (#2007), which makes this
    -- vacuous for one rather than wrong.
    --
    -- The typed question again, so this module never sees a ManaRiderEffect
    -- constructor; Pawl.Engine.ManaRider is where the casing lives.
    Just _ | ManaRider.uncounterable oid gs -> pure False
    -- CR 608.2n, reached before the CR 113.6g gate because that gate asks about a
    -- spell's own card and an ability has none -- Game.faceOf answers Nothing for
    -- one, so asking first would fall through to the graveyard move by accident.
    Just _ | Game.isAbility oid gs -> do
      State.modify' (Game.cease oid)
      pure True
    Just _ -> case fmap Face.counterability (Game.faceOf oid gs) of
      Just Counterability.CantBeCountered -> pure False
      _ -> do
        moved <- changeZoneReturning oid Zone.Graveyard
        case moved of
          Nothing -> pure False
          Just _ -> do
            State.modify'
              . recordEvent
              $ GameEvent.SpellCountered
                Countering.MkCountering
                  { Countering.spell = oid,
                    Countering.source = source,
                    Countering.controller = controller
                  }
            pure True

-- CR 601.2c: record that everything just announced as a target BECAME one --
-- "the chosen objects and/or players each become a target of that spell. (Any
-- abilities that trigger when those objects ... become the target of a spell
-- trigger at this point ...)". CR 602.2b and CR 603.3d route an activated and a
-- triggered ability through the same rule, so all three announcement sites call
-- this and none of them knows what watches.
--
-- ONE EVENT PER SLOT PER RECIPIENT, which is rule 601.2c's own arity: that rule
-- lets one object be chosen once for each instance of the word "target" and
-- forbids it twice within one instance, so the per-slot sets are walked rather
-- than unioned and a spell naming the same permanent under two instances records
-- two.
--
-- Players are kept, CR 115.1 making a player a target in its own right: Dormant
-- Gomazoa watches its controller become one, and rule 601.2c names objects and
-- players in the same breath, so one event over a Recipient beats two events.
--
-- The KIND is passed in rather than derived. Rule 601.2c's parenthetical is about
-- spells (CR 112.1) while CR 602.2b and CR 603.3d bring abilities (CR 113.3) to
-- the same step, and only the caller knows which it is putting on the stack;
-- the matcher that later reads this has no GameState to ask.
--
-- CALLED AFTER THE ANNOUNCEMENT SUCCEEDS rather than at rule 601.2c's own
-- position in the sequence. The rule puts the trigger before the costs are paid
-- but holds the ability off the stack "until the spell has finished being cast",
-- and CR 601.2 rewinds the whole announcement if it does not -- so a trigger
-- recorded here and one recorded earlier differ only in a case where the earlier
-- one would have to be taken back.
becameTarget :: ObjectId -> StackObjectKind.StackObjectKind -> PlayerId -> Map.Map SlotName.SlotName (Set.Set Recipient.Recipient) -> Game ()
becameTarget source kind controller chosen =
  Foldable.for_ (concatMap Set.toAscList (Map.elems chosen)) $ \targeted ->
    State.modify'
      . recordEvent
      $ GameEvent.BecameTarget
        BecameTarget.MkBecameTarget
          { BecameTarget.targeted = targeted,
            BecameTarget.source = source,
            BecameTarget.kind = kind,
            BecameTarget.controller = controller
          }

-- CR 701.3b and CR 701.3c: store the attachment, restamp, and record it.
--
-- CR 303.4j for an Aura -- "the Aura doesn't move" -- and CR 701.3b's first
-- sentence for the rest. A FAILURE MODE, not a fizzle: the only thing that does
-- not happen is the move, and in particular the subject stays attached to its old
-- host rather than becoming unattached, so CR 704.5m has nothing to bury. Nothing
-- is recorded on that branch either, which is CR 701.3b in as many words -- the
-- permanent did not become attached.
--
-- CR 701.3b's SECOND sentence is checked here rather than left to the caller:
-- attaching a permanent to the object or player it is already attached to "does
-- nothing", so there is no restamp and no event. Two of the three callers cannot
-- reach it -- Attach.hostsFor never offers the current host -- and CR 702.6a's
-- equip, whose target is any creature its controller controls, can.
--
-- CR 701.3c: attaching to a DIFFERENT object gives it a new timestamp, which CR
-- 613.7 orders layer effects by.
--
-- LIVES HERE rather than in Pawl.Engine.Attach, which is where the legality
-- reading it asks (Attach.attachmentFor) still lives: Event imports Attach, so a
-- recordEvent call from there would invert the edge. All three call sites -- this
-- module's CR 303.4k rewrite and Pawl.Engine.Resolve's Attach and AttachTarget
-- opcodes -- already see this module.
--
-- The event carries the tag attachmentFor produced rather than the caller's
-- `destination`, so a reader sees the same Recipient Object.attachedTo holds.
attach :: ObjectId -> Recipient.Recipient -> Game ()
attach subject destination = do
  gs <- State.get
  case Attach.attachmentFor subject destination gs of
    Nothing -> pure ()
    Just attachment -> Monad.unless (fmap Object.attachedTo (Game.lookupObject subject gs) == Just (Just attachment)) $ do
      let (ts, gs1) = Game.freshTimestamp gs
          move o = o {Object.attachedTo = Just attachment, Object.timestamp = ts}
      State.put gs1 {GameState.objects = Map.adjust move subject (GameState.objects gs1)}
      State.modify'
        . recordEvent
        $ GameEvent.BecameAttached
          BecameAttached.MkBecameAttached
            { BecameAttached.attachment = subject,
              BecameAttached.host = attachment
            }

-- CR 611.1 / 613.11: does a rules-modifying continuous effect stop this spell or
-- ability from being countered (Spider-Punk, Prowling Serpopard)? The victim's
-- controller is the player the effect is anchored against -- CR 113.8 for an
-- ability on the stack, CR 601.2a for a spell -- and an object with no
-- controller is protected by nothing. The victim's own id rides along too, since
-- a narrowed effect names the victim's characteristics ("creature spells you
-- control can't be countered") and not only its controller.
--
-- The typed question, so this module never sees a PlayerEffect constructor;
-- Pawl.Engine.PlayerEffect.cantBeCountered is where the casing lives.
--
-- Projection.controllerOf, which is what every other reader of an affected
-- object's controller already asks (PlayerEffect.matchesObject, and
-- Replacement.chooserOf for CR 616.1's chooser). For a
-- SPELL that is a re-derivation rather than the stored fact CR 405.4 describes,
-- and it falls back to the owner (#83); a spell cast from a zone its owner does
-- not hold would therefore be read against the wrong player here.
protectedFromCountering :: ObjectId -> GameState -> Bool
protectedFromCountering oid gs =
  maybe False (\pid -> PlayerEffect.cantBeCountered pid oid gs) (Projection.controllerOf oid gs)

-- CR 701.21/701.21a: the single sacrifice funnel. The permanent goes to its
-- OWNER's graveyard through changeZone, and -- unlike destroy -- with no
-- indestructible gate and no regeneration shield consulted, since sacrificing is
-- not destroying. Restricted to permanents on the battlefield, so anything else is
-- a no-op.
--
-- TWO refusals live here, and the arms below cite each: CR 701.21a's "a permanent
-- they don't control", and CR 101.2's "can't" beating a rule or effect's "can" --
-- a permanent under a Pawl.Types.SacrificeRestriction. Both are asked at the
-- funnel, so a caller that named a victim without consulting
-- Pawl.Engine.Replacement.sacrificeCandidates cannot get past either.
--
-- CR 701.21a also forbids sacrificing a permanent you do not control, which is why
-- this takes the sacrificing player. Enforced here at the one funnel rather than
-- trusted from each caller: a cost payment, a triggered ability's own source and
-- `apply`'s CR 614.1c as-enters sacrifice are controlled by the paying player by
-- construction, but an edict's victim is a permanent a PLAYER named.
--
-- Records GameEvent.PermanentSacrificed, which is what makes CR 603.10a's
-- "abilities that trigger when a player sacrifices a permanent" expressible: CR
-- 700.4 makes a sacrifice a death, so the Moved event changeZone writes below is
-- the one a destruction writes too and cannot say what the move WAS. Written HERE
-- rather than at each caller for the reason the controller check is: this is the
-- one funnel.
--
-- BEFORE the zone change, and naming the PRE-MOVE id, which is CR 603.10a's
-- look-back. CR 701.21a's sacrifice is the game action, so a replacement that
-- redirects the move -- Rest in Peace exiling it instead -- does not un-sacrifice
-- the permanent, and an event recorded afterwards would either name an
-- incarnation the redirection never produced or not be recorded at all.
sacrifice :: PlayerId -> ObjectId -> Game ()
sacrifice pid oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just obj -> case Object.zone obj of
      -- CR 701.21a: "A player can't sacrifice something that isn't a permanent, or
      -- something that's a permanent they don't control." The zone case below is
      -- the first clause; this is the second. Enforced HERE, at the one funnel,
      -- rather than trusted from each caller -- the callers are a cost payment, a
      -- trigger's own source, `apply`'s CR 614.1c as-enters sacrifice, and an
      -- edict whose victim a player NAMED, and only the last of those could ever
      -- be wrong.
      Zone.Battlefield
        | Projection.controllerOf oid gs /= Just pid -> pure ()
        -- CR 101.2: "if a rule or effect allows or directs something to happen,
        -- and another effect states that it can't happen, the 'can't' effect
        -- takes precedence" -- Garland, Royal Kidnapper's "can't be sacrificed".
        -- CR 101.3 says what happens instead: "any part of an instruction that's
        -- impossible to perform is ignored", so nothing moves, no event is
        -- recorded, and no destruction is substituted.
        --
        -- The SECOND of the two gates, and it earns its place at the funnel:
        -- Effect.Sacrifice names a target or a bound group and consults no
        -- candidate list at all, which is how Lightning Skelemental's "at the
        -- beginning of the end step, sacrifice this creature" reaches here.
        -- Pawl.SacrificeRestrictionSpec proves that path goes through this arm
        -- and no other -- gating only the candidate list leaves it green.
        --
        -- The other two callers that name a victim outright answer CR 101.2
        -- before they get here as well, and for their own reasons:
        -- Pawl.Engine.Cost refuses the SacrificeThis cost so CR 118.3 is not
        -- broken, and Pawl.Engine.Sba drops a CR 704.5s Saga so CR 704.3's
        -- repeat terminates. This arm is the backstop under both.
        | SacrificeRestriction.prohibited oid gs -> pure ()
        | otherwise -> do
            State.modify' (recordEvent (GameEvent.PermanentSacrificed (PermanentSacrificed.MkPermanentSacrificed pid oid)))
            changeZone oid Zone.Graveyard
      Zone.Library -> pure ()
      Zone.Hand -> pure ()
      Zone.Graveyard -> pure ()
      Zone.Stack -> pure ()
      Zone.Exile -> pure ()
      -- CR 408.1: a command-zone object is not a permanent, so it is never
      -- sacrificed.
      Zone.Command -> pure ()

-- CR 111.2: create `n` tokens with the given effect-defined characteristics under
-- `controller`'s control, summoning-sick (CR 302.6). A token is created from
-- nothing, so changeZone cannot mint it. Uses from = Battlefield, where to == from
-- can never read as a leave, and emits the enters event so CR 603.6a triggers fire
-- on the path a resolved permanent uses.
--
-- Plural by rules requirement, not convenience: CR 614.1/614.16 replacements scope
-- to the CREATION EVENT rather than each token, so the count is settled once up
-- front. Every token is then materialized, and only then does each run its OWN
-- entry loop -- CR 616.1g's containment, since creating a token contains that
-- token entering. Each entry loop is handed the whole batch, which excludes
-- simultaneously-entering siblings from any copy choice (CR 614.12a; see
-- applyReplacementsIn for why 614.12a and not 614.13a).
--
-- A token whose text is GIVEN has empty `replacementEffects`, so its entry loop
-- returns immediately; a token copy of a Clone carries the copied permanent's own
-- `EntryR AsCopy`, which is CR 616.1g's worked example and what makes the nesting
-- observable. Pawl.CopySpec's "none may copy a sibling" is the proof: deleting
-- the loop below leaves five 0/0 Clones for CR 704.5f to bury.
--
-- CR 800.4b / 800.4d: no token is created for a player who has left the game. The
-- two sentences coincide by CR 111.2, which makes a token's owner and controller
-- the same player, so one guard satisfies both. Checked BEFORE resolveTokens: the
-- rule says no token is created, so nothing may be minted and nothing spent
-- getting there -- resolveTokens consumes CR 614.3 use counts.
--
-- Reading the PARAMETER rather than the post-entry controller is sufficient, not
-- merely convenient: the entry loop below can move a token with a CR 616.1b
-- control rewrite, but CR 800.4a's second clause ends such a row when its
-- controller leaves (Departure.givesControlOnEntryTo), so no surviving row can
-- rewrite the token onto a departed player.
--
-- Inline rather than delegating to a `createTokensFor` body: the project writes no
-- export lists, so a second top-level name would be a public door past the check.
--
-- The Maybe snapshot is CR 707.1's other kind of token -- one whose text is a
-- copy of a permanent rather than given (`copiedSnapshot`) -- and it comes
-- through this one funnel rather than a second so that CR 614.16's count
-- replacement, CR 800.4b's departed-player guard and each token's own CR 614.12
-- entry loop are the same code for both kinds.
--
-- `entering` is CR 122.6a's counters the effect says each token enters the
-- battlefield with -- CR 701.53a's incubate -- and it is passed beside `tapped`
-- rather than as a whole EntryRiders for the reason changeZoneAttaching keeps them
-- apart too: the record's other riders are answered elsewhere or are inert here
-- (CR 111.2 for `underOwner`, CR 508.4 for `attacking`, CR 712.14a's card-only
-- scope for `transformed`), and handing this funnel the record would read as
-- though it applied them.
createTokens :: PlayerId -> Card -> Maybe PC.ProjectedCharacteristics -> Natural -> TapState.TapState -> Map.Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural -> Game [ObjectId]
createTokens controller card copy n tapped entering = do
  gs <- State.get
  if List.notElem controller (Game.stillPlaying gs)
    then pure []
    else do
      resolved <- resolveTokens controller card n
      case resolved of
        Nothing -> pure []
        Just (owner, tokenCard, count) -> do
          -- Interned ONCE for the whole event, not once per token: `count`
          -- tokens created by one effect are copies of one set of
          -- effect-defined characteristics (CR 111.3), so they name one entry.
          tokenId <- State.state (Game.intern (Printing.MkPrinting tokenCard))
          let mkObj ts =
                Object.MkObject
                  { Object.owner = owner,
                    Object.enteredUnder = Nothing,
                    Object.source = Source.OfToken tokenId,
                    Object.zone = Zone.Battlefield,
                    -- CR 110.5b: untapped unless an effect says otherwise, which
                    -- is why the caller supplies this rather than the default
                    -- being taken and the token tapped after.
                    Object.tapped = tapped,
                    -- CR 110.5b: face up, for the same rule's reason. No effect
                    -- in the pool creates a token face down.
                    Object.facing = Facing.FaceUp,
                    Object.exiledFaceDown = False,
                    Object.damage = 0,
                    Object.sickness = Sickness.Sick,
                    -- CR 707.2 / 111.3: a token copy's copiable values are the
                    -- copied permanent's, stamped into the layer-1 snapshot the
                    -- projection starts from -- the same binding the CR 614.1c
                    -- entry rewrite writes, and the same read
                    -- (Projection.copiableCharacteristics) on the way out.
                    -- Written HERE rather than after the entry loop because CR
                    -- 614.12 asks for the characteristics the permanent would
                    -- have on the battlefield, and for this token those are the
                    -- copy's from the instant it exists.
                    Object.bindings = maybe Map.empty (\pc -> Binding.setCopy pc Map.empty) copy,
                    Object.counters = Map.empty,
                    Object.counterTimestamps = Map.empty,
                    Object.attachedTo = Nothing,
                    Object.chosenColor = Nothing,
                    Object.chosenSubtype = Nothing,
                    Object.chosenNames = Set.empty,
                    Object.chosenPlayer = Nothing,
                    Object.timestamp = ts,
                    Object.face = Nothing,
                    Object.turnedOverAt = Nothing,
                    Object.worldSince = Nothing,
                    Object.playableFromExile = Nothing,
                    Object.plotted = Nothing,
                    Object.foretold = Nothing,
                    Object.ringBearerFor = Nothing,
                    Object.protector = Nothing,
                    Object.ventureRoom = Nothing,
                    Object.classLevel = Nothing,
                    Object.unlockedHalves = Set.empty,
                    Object.designations = Set.empty,
                    Object.kicked = False,
                    Object.phyrexianLifePaid = 0,
                    Object.manaSpent = Mana.MkMana [],
                    Object.announcedX = Nothing,
                    Object.detainedUntil = Set.empty,
                    Object.goadedBy = Set.empty,
                    Object.doesNotUntapNext = False,
                    Object.exertedBy = Set.empty
                  }
          ids <- Monad.replicateM (Natural.toIntSaturating count) (placeObject owner mkObj Zone.Battlefield LibraryPosition.defaultValue)
          -- CR 122.6a: the counters the EFFECT says these tokens enter with, placed
          -- through putCounters -- CR 122.6's funnel, so CR 614.16 applies and
          -- Vorinclex sees them -- exactly as changeZoneEntering's door does for the
          -- move that carries the same rider.
          --
          -- The WHOLE BATCH is dressed before any token runs its entry loop, rather
          -- than each token being dressed and then entered: CR 614.12 checks "the
          -- characteristics of the permanent as it would exist on the battlefield",
          -- and every sibling of this batch is already there by then, so the counters
          -- they entered with belong to that reading too. No card observes the
          -- difference -- no entry replacement in the pool reads a counter the
          -- entering object already has -- so this buys the ordering rather than a
          -- passing test.
          let siblingsOf oid = Set.delete oid (Set.fromList ids)
          Monad.mapM_ (\oid -> Monad.mapM_ (\(kind, many) -> Monad.void (putOwnCountersIn (siblingsOf oid) oid kind many)) (Map.toAscList entering)) ids
          Monad.mapM_ (\oid -> runEntry (siblingsOf oid) oid) ids
          -- No prior incarnation to snapshot, so a token's last known information
          -- IS what it is now (CR 111.3). Recorded after every entry loop, so the
          -- events describe settled objects.
          Monad.mapM_ recordTokenEntry ids
          pure ids

-- Nothing departed, so `departed` is the token's own id. Harmless rather than a
-- fiction readers must know about: from == to == Battlefield already fails every
-- departure test (CR 603.6c), and a token has no `lastKnown` entry to find.
recordTokenEntry :: ObjectId -> Game ()
recordTokenEntry newId = do
  placed <- State.get
  let snapshot = Projection.project newId placed
  State.modify' (recordEvent (GameEvent.Moved (Moved.MkMoved (ZoneChange.MkZoneChange newId newId Zone.Battlefield Zone.Battlefield) snapshot)))

-- CR 121.1, one card at a time per CR 121.2. An empty library records the failed
-- draw, which CR 704.5b makes a loss at the next state-based-action check. Shared
-- by the draw step, opening hands and the Draw effect.
--
-- The tally and the event are for CR 121.2's "individual card draws": each draw
-- is its own event, carrying which of this player's draws this turn it was, so a
-- card can ask (CR 702.94a). Both happen only when the card actually moved --
-- an empty library draws nothing, and a CR 616.1 loop that cancelled the move
-- means no card reached the hand, so neither is a draw to count.
--
-- CR 121.9's reveal window closes the draw, and it is inside this funnel rather
-- than beside its callers because rule 121.9 says "as they draw it": every route
-- that draws -- the draw step, an opening hand, a Draw effect -- opens the same
-- window, and none of them learns that it did.
drawCard :: PlayerId -> Game ()
drawCard pid = Monad.void (drawCardReturning pid)

-- drawCardReturning is to drawCard what changeZoneReturning is to changeZone, and
-- for CR 121.1's "and reveal IT" (#1899): Just the id the card ARRIVED in the hand
-- under (CR 400.7), which is where a later clause of the same resolution names it.
--
-- Nothing where there is no such card -- an empty library (CR 104.3c is then the
-- whole of what happened) or a move a replacement effect cancelled -- so a caller
-- binding the answer binds nothing rather than binding a card that is not there.
drawCardReturning :: PlayerId -> Game (Maybe ObjectId)
drawCardReturning pid = do
  gs <- State.get
  case Game.zoneMembers Zone.Library pid gs of
    [] -> do
      State.put gs {GameState.drewFromEmpty = Set.insert pid (GameState.drewFromEmpty gs)}
      pure Nothing
    top : _ -> do
      moved <- changeZoneReturning top Zone.Hand
      case moved of
        Nothing -> pure Nothing
        Just drawn -> do
          nth <- State.state $ \g ->
            let tally = Map.insertWith (+) pid 1 (GameState.drawsThisTurn g)
             in (Map.findWithDefault 1 pid tally, g {GameState.drawsThisTurn = tally})
          State.modify' (recordEvent (GameEvent.Drew (Drew.MkDrew pid nth)))
          -- CR 702.94a's "if it's the FIRST card you've drawn this turn", asked
          -- off the ordinal this draw was just stamped with rather than off a
          -- second reading of the tally.
          Monad.when (nth == 1) (offerMiracleReveal pid drawn)
          pure (Just drawn)

-- CR 702.94a's static half, and CR 121.9's window: "you may reveal this card from
-- your hand as you draw it". Asked of the card that just arrived in the hand, and
-- only when it has a miracle ability to ask about -- CR 702.94a is the whole of
-- what puts a question here, so an ordinary draw of an ordinary card raises
-- nothing.
--
-- The FIRST-DRAW gate is the caller's, since it is the ordinal the draw already
-- carries. Dropping it would make pawl's miracle cards WEAKER than printed, which
-- is the direction that is never admissible.
--
-- CR 121.9's "may look at that card as they draw it before choosing" is satisfied
-- by the prompt naming the card. pawl has no per-player hidden-information filter
-- at all (#1412), so nothing here could have shown the player less
-- than the rule allows.
--
-- The reveal goes through the ordinary funnel with CR 702.94a's cause, which is
-- what makes the linked trigger (CR 603.11) findable: `eventTriggers`' hand source
-- reads that event and nothing else knows a miracle happened.
--
-- Not implemented: CR 702.94b's LASTING reveal -- the card stays revealed until it
-- leaves the hand or the ability leaves the stack -- which needs a per-object
-- revealed flag (#1408). Nor CR 121.8's face-down drawn card
-- (#1409).
offerMiracleReveal :: PlayerId -> ObjectId -> Game ()
offerMiracleReveal pid drawn = do
  gs <- State.get
  case Game.faceOf drawn gs of
    Just face | Maybe.isJust (Keyword.miracleCost (Face.keywords face)) -> do
      let decider = Decide.deciderFor pid gs
      decision <- Game.choose (Prompt.OfferedMiracleReveal decider pid drawn (Face.name face))
      case decision of
        OptionalDecision.Declines -> pure ()
        OptionalDecision.Exercises -> reveal RevealCause.ForMiracle pid drawn
    _ -> pure ()

-- The single discard funnel (CR 701.9a). `pid` is the discarding player, whom that
-- rule makes the card's owner either way, and `cause` is why (see DiscardCause).
--
-- The move goes through the CR 400.7 funnel, so a discarded card gets a new
-- incarnation and Rest in Peace's redirect composes. The EVENT is what this adds,
-- and it is not redundant with the Moved event: per CR 701.9c a redirected discard
-- is still a discard while its Moved event no longer reads hand-to-graveyard, so a
-- discard trigger reads this record and never the zone pair.
--
-- Recorded only when the move COMPLETED -- an unknown id or a cancelled CR 616.1
-- loop must record nothing (CR 603.2g). The id recorded is the one the funnel
-- MINTED, since CR 702.29c's abilities trigger from wherever the card winds up.
discard :: DiscardCause.DiscardCause -> PlayerId -> ObjectId -> Game ()
discard cause pid oid = Monad.void (discardReturning cause pid oid)

-- A second door rather than a return type on `discard`, the changeZoneReturning
-- and destroyReturning shape: the ~dozen callers that only perform the discard
-- keep their `Game ()`, and the one caller that binds "discarded this way" gets
-- the id without a `Monad.void` at every other site.
--
-- The answer is the id the CR 400.7 funnel MINTED, which is the same id the
-- Discarded event carries and for the same reason: the hand incarnation is gone
-- by the time anything reads the slot, so binding it would name an object no
-- reader of the destination zone can find. Nothing for a move that did not
-- complete -- an unknown id, or a CR 616.1 loop the player cancelled.
discardReturning :: DiscardCause.DiscardCause -> PlayerId -> ObjectId -> Game (Maybe ObjectId)
discardReturning cause pid oid = do
  moved <- changeZoneReturning oid Zone.Graveyard
  case moved of
    Nothing -> pure ()
    Just newId -> State.modify' (recordEvent (GameEvent.Discarded (Discarded.MkDiscarded pid newId cause)))
  pure moved

-- The single reveal funnel (CR 701.20a): `pid` shows `oid` to all players, which
-- here means appending what was shown to the public log. No-op for an unknown id.
-- Per CR 701.20b nothing moves and nothing changes, so the event is the whole
-- effect -- the rule, not a shortcut.
--
-- The snapshot is Projection.project -- the object's CR 613 characteristics and
-- deliberately not its printed card, since a reveal has to show what a player at
-- the table would see, on every axis a continuous effect reaches off the
-- battlefield. The readers that judge a card off the battlefield take that same
-- projection -- the explore that follows this very reveal, and rule 728.1's mill
-- tally (see #1911) -- so what is shown and what is judged cannot part
-- company.
--
-- The `cause` is CR 702.94a's "this way" (see RevealCause): every caller but the
-- draw funnel's miracle window shows a card for a reason no rule asks about
-- again, and passes Ordinary.
reveal :: RevealCause.RevealCause -> PlayerId -> ObjectId -> Game ()
reveal cause pid oid = do
  gs <- State.get
  Monad.when (Maybe.isJust (Game.lookupObject oid gs)) $
    State.modify' (recordEvent (GameEvent.Revealed (Revealed.MkRevealed pid oid cause (Projection.project oid gs))))

-- CR 508.3a / 608.2i: how many times this object has been declared as an attacker
-- this turn, read out of the turn-scoped event log. Only Combat.declareAttackers
-- appends the event, which keeps CR 508.4's creature put onto the battlefield
-- attacking -- one that never attacked -- out of the count.
declarationsOf :: ObjectId -> GameState -> Int
declarationsOf bearer gs =
  let declaredIt event = case event of
        GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared oid _ _) -> oid == bearer
        _ -> False
   in length (Seq.filter (declaredIt . LoggedEvent.event) (GameState.events gs))

-- Clarion Spirit's "your SECOND spell each turn": which of the turn's matching
-- casts this one is, counting from one.
--
-- The window is the whole event log, which is exactly "this turn" for
-- Pawl.Engine.PlayerEffect.castsThisTurn's reason -- Engine.handoffTurn clears it
-- at the handoff. So the TurnScope beside the ordinal narrows which turns the
-- ability watches and never which casts are counted; on a turn the scope refuses,
-- the condition has already answered False without reaching here.
--
-- Counted over the casts the SAME condition admits, since the printed sentence
-- puts the ordinal inside the description -- so the Filter and the zone are
-- applied to each earlier cast too. Each earlier one is read through
-- Count.snapshotView, the CR 608.2h snapshot its event recorded, because CR
-- 601.2a's stack incarnation is long gone for every cast but the one being
-- matched.
--
-- POSITIONAL rather than a count of the whole log. CR 601.2i files the cast
-- before CR 603.2 checks the condition against it, so the log already holds this
-- cast -- and it may hold LATER ones too, since one CR 117.5 boundary can cover
-- several casts. The walk therefore stops at this cast's own entry, which the
-- spell's id names uniquely (CR 400.7 mints it and nothing else ever bears it),
-- and counts it: the turn's second cast answers 2 whatever was cast after it.
-- An event that is not in the log at all -- a fixture appending one directly --
-- is read as the latest, which is what the whole walk then counts against.
--
-- No board tells the walk from a count of the WHOLE log: mutating it away leaves
-- the suite green, because every cast a player can make gets its own CR 117.5
-- scan and no card in the pool casts twice in one resolution -- though a clause
-- list holding two Effect.OfferCast would, and the DSL admits one. So the stop
-- is a fence resting on the rule's ordering rather than a behaviour a test
-- proves.
castOrdinal :: Filter.Context -> Filter.Type.Filter Keyword.Type.Keyword -> Maybe Zone -> ObjectId -> GameState -> Natural
castOrdinal context predicate fromZone spell gs =
  let isThisCast cast = SpellWasCast.spell cast == spell
      earlier = Seq.takeWhileL (maybe True (not . isThisCast) . Game.castOf . LoggedEvent.event) (GameState.events gs)
      counted entry = case Game.castOf (LoggedEvent.event entry) of
        Nothing -> False
        Just cast ->
          maybe True (\z -> SpellWasCast.zone cast == Just z) fromZone
            && maybe False (\view -> Filter.matches context view predicate) (Count.snapshotView gs EventShape.SpellCast (LoggedEvent.event entry))
   in 1 + Natural.length (Seq.filter counted earlier)

-- CR 102.1: does this turn belong to the scope? `active` is "the player whose
-- turn it is", and `own` is the seat the scope is read against -- the player
-- Pawl.Types.TurnScope deliberately names none of, since each reader supplies
-- its own: CR 109.5's "you" for a triggered ability (CR 603.3a), the CR 602.2
-- activator for an activated one.
--
-- OpponentsTurn is "not you" rather than an enumeration of opponents, which is
-- CR 102.2 in a two-player game and CR 806.1 in a Free-for-All: there "a group
-- of players compete as individuals against each other", so every other seat is
-- an opponent. Wrong only for CR 102.3's teams, which pawl has no format for
-- (#175).
turnScopeAdmits :: TurnScope.TurnScope -> PlayerId -> PlayerId -> Bool
turnScopeAdmits scope active own = case scope of
  TurnScope.EachTurn -> True
  TurnScope.ControllersTurn -> active == own
  TurnScope.OpponentsTurn -> active /= own

-- CR 122's removal as the two bearer-scoped counter-removal conditions read it:
-- the before/after pair of a GameEvent.CountersRemoved that took counters of
-- `wanted` off `bearer`, and Nothing for every other event.
--
-- ONE exhaustive case shared by TriggerCondition.SelfLastCounterRemoved and
-- TriggerCondition.SelfCountersRemoved rather than a copy each, because the two
-- ask the identical question of the identical constructor and differ only in what
-- they then do with the pair. A new GameEvent constructor still breaks the build
-- here, which is what the exhaustive list is for.
countersRemovedFrom :: ObjectId -> CounterKind.CounterKind Keyword.Type.Keyword -> GameEvent -> Maybe (Natural, Natural)
countersRemovedFrom bearer wanted event = case event of
  GameEvent.CountersRemoved (CounterChange.MkCounterChange oid kind before after)
    | oid == bearer && kind == wanted ->
        Just (before, after)
  GameEvent.CountersRemoved {} -> Nothing
  GameEvent.CountersPut {} -> Nothing
  GameEvent.ControlChanged {} -> Nothing
  GameEvent.VentureMarkerEntered {} -> Nothing
  GameEvent.BecameTarget {} -> Nothing
  GameEvent.BecameAttached {} -> Nothing
  GameEvent.LeftTheGame _ -> Nothing
  GameEvent.Milled {} -> Nothing
  GameEvent.Scried _ -> Nothing
  GameEvent.Surveiled _ -> Nothing
  GameEvent.DiceRolled _ -> Nothing
  GameEvent.ClassLevelSet _ -> Nothing
  GameEvent.Plotted _ -> Nothing
  GameEvent.Explored _ -> Nothing
  GameEvent.Exerted _ -> Nothing
  GameEvent.BecameAttacked _ -> Nothing
  GameEvent.AttackersDeclared _ -> Nothing
  GameEvent.BecameTapped _ -> Nothing
  GameEvent.Moved {} -> Nothing
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.DamagePrevented {} -> Nothing
  GameEvent.StepBegan {} -> Nothing
  GameEvent.SpellCast {} -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  GameEvent.Discarded {} -> Nothing
  GameEvent.Drew {} -> Nothing
  GameEvent.Revealed {} -> Nothing
  GameEvent.AttackerDeclared {} -> Nothing
  GameEvent.BecameBlocking {} -> Nothing
  GameEvent.BlocksDeclared {} -> Nothing
  GameEvent.AttackerBlocked {} -> Nothing
  GameEvent.AttackerUnblocked _ -> Nothing
  GameEvent.SpellCountered _ -> Nothing
  GameEvent.HalfUnlocked {} -> Nothing
  GameEvent.TurnedFaceUp _ -> Nothing
  GameEvent.Transformed {} -> Nothing
  GameEvent.BecameDesignated {} -> Nothing
  GameEvent.Evolved _ -> Nothing
  GameEvent.Mentored {} -> Nothing
  GameEvent.Trained _ -> Nothing
  GameEvent.PermanentSacrificed {} -> Nothing
  GameEvent.AbilityTriggered {} -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing
  GameEvent.LifeLost {} -> Nothing
  GameEvent.LifeGained {} -> Nothing

-- CR 603.2: does this condition fire on this event, for the permanent that bears
-- it? `bearer` is the object whose ability this is and `you` its controller (CR
-- 603.3a, CR 109.5); both are part of the match because the scan visits EVERY
-- permanent, not only the one an event names. The sole home of casing on
-- TriggerCondition for rules purposes -- Pawl.Codec also cases on every
-- constructor, but only at the JSON boundary.
--
-- The BINDINGLESS reading, which is every caller that has no environment to offer
-- and every condition but one. `matchesTriggerGiven` below is the same act with CR
-- 603.7c's captured environment in hand.
matchesTrigger :: GameState -> ObjectId -> PlayerId -> TriggerCondition -> GameEvent -> Bool
matchesTrigger = matchesTriggerGiven Map.empty

-- The same question against a slot environment, for the one condition whose subject
-- is an object named EARLIER rather than the bearer or a class of objects
-- (TriggerCondition.LoseControlOfBound). Projection.controllerOf's `-Given` shape:
-- the plain name above defaults the extra argument, so a caller with nothing to say
-- says nothing.
--
-- An empty map is not a special case -- it simply names no slot, so the one arm
-- that reads a slot finds none and answers False.
matchesTriggerGiven :: Map.Map SlotName.SlotName Binding -> GameState -> ObjectId -> PlayerId -> TriggerCondition -> GameEvent -> Bool
matchesTriggerGiven bindings gs bearer you cond event = case cond of
  -- CR 603.6a: the bearer's own object entered the battlefield.
  TriggerCondition.SelfEnters -> case event of
    GameEvent.Moved (Moved.MkMoved zc _) -> ZoneChange.object zc == bearer && ZoneChange.to zc == Zone.Battlefield
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 603.6a's "whenever a [type] enters": a permanent the Filter admits
  -- entered the battlefield. The bearer frames the match rather than being it --
  -- it is the Filter.Context's source (so `Not IsSource` is Soul Warden's
  -- "another"), and its controller is the perspective CR 109.5 gives "you" in
  -- "a creature YOU CONTROL enters".
  TriggerCondition.PermanentEnters f -> case event of
    GameEvent.Moved (Moved.MkMoved zc _)
      | ZoneChange.to zc == Zone.Battlefield ->
          -- Deliberately NOT the snapshot the Moved event carries: that is the
          -- object as it last existed in the zone it LEFT, and reading it here
          -- would answer CR 603.6b backwards. The entrant's characteristics come
          -- from the game as it stands, which is what CR 603.10 asks for.
          --
          -- viewWithLastKnown rather than viewOfObject, so an entrant that has
          -- already left again -- a creature entering as a 0/0 and buried by CR
          -- 704.5f before the CR 117.5 boundary -- is still read as it was on the
          -- battlefield (CR 608.2h) instead of vanishing from the match.
          --
          -- Recomputed per (bearer, entry event) pair rather than shared: this is
          -- handed the GameState and nothing else. Forced only inside this arm, so
          -- a board with no such ability pays nothing.
          --
          -- Nothing is an entrant that is gone AND filed no last known information,
          -- about which no Filter can honestly answer.
          let entrant = ZoneChange.object zc
           in case Projection.viewWithLastKnown entrant gs entrant of
                Nothing -> False
                Just view -> Filter.matches (Filter.contextFor (Just you) (Just bearer)) view f
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 603.2b: this step began, on a turn the scope admits.
  TriggerCondition.StepBegins (StepBegins.MkStepBegins wanted scope) -> case event of
    GameEvent.StepBegan (StepBegan.MkStepBegan began active) ->
      began == wanted && turnScopeAdmits scope active you
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 603.8: a state trigger is not an event trigger. It never matches an entry
  -- in the log; stateTriggers below is its whole story.
  TriggerCondition.StateIs _ -> False
  -- CR 510.1b / 510.2: the bearer dealt COMBAT damage to a PLAYER. Combat damage
  -- already records a DamageDealt event, so this is a filter over the log.
  TriggerCondition.SelfDealsCombatDamageToPlayer -> case event of
    GameEvent.DamageDealt ev ->
      DamageEvent.source ev == bearer
        && DamageEvent.kind ev == DamageKind.Combat
        && isPlayerRecipient (DamageEvent.target ev)
    GameEvent.Moved {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 120.3: the bearer was DEALT damage -- enrage's event. The arm above with the
  -- identity check moved from the event's SOURCE to its RECIPIENT.
  --
  -- Neither of that arm's other two tests, and deliberately: rule 120.3 is about
  -- damage being dealt however it was dealt, and Ripjaw Raptor's printed phrase
  -- qualifies it in no way, so a Prodigal Sorcerer's ping fires this exactly as
  -- combat damage does. This is the damage arm that breaks the local pattern.
  --
  -- Recipient.objectOf, not a ToCreature test: CR 120.3's recipient may be any
  -- permanent (a planeswalker, a battle), and the bearer's own id is what decides
  -- the match either way. Nothing is dealt damage while the id is a player's, which
  -- is what the Nothing arm falls through on.
  TriggerCondition.SelfIsDealtDamage -> case event of
    GameEvent.DamageDealt ev -> Recipient.objectOf (DamageEvent.target ev) == Just bearer
    GameEvent.Moved {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- The same event read by a BYSTANDER (CR 510.1b / 510.2): a permanent the Filter
  -- admits dealt combat damage to a player. The Filter reads the event's DAMAGER,
  -- the bearer contributing only CR 109.5's "you" and the Filter.Context's source
  -- -- which is what would make Filter.IsSource the self-scoped reading.
  --
  -- viewWithLastKnown, not fullView: CR 603.10's first sentence wants the damager
  -- as it existed immediately after the damage, and pawl scans the log after CR
  -- 704's pass, so a trampler that connected and died to its blocker in the same
  -- CR 510.2 event is already gone. CR 608.2h's record is what still answers "was
  -- it a Wolf". No board in the pool reaches that -- Tovolar's Wolves are vanilla
  -- and unblocked -- so this is a fence rather than a tested branch, as is the
  -- DamageKind test beside it: no card in the pool makes a Wolf or Werewolf deal
  -- NONCOMBAT damage while a Tovolar watches.
  TriggerCondition.PermanentDealsCombatDamageToPlayer f -> case event of
    GameEvent.DamageDealt ev ->
      DamageEvent.kind ev == DamageKind.Combat
        && isPlayerRecipient (DamageEvent.target ev)
        && ( let damager = DamageEvent.source ev
              in case Projection.viewWithLastKnown damager gs damager of
                   Nothing -> False
                   Just view -> Filter.matches (Filter.contextFor (Just you) (Just bearer)) view f
           )
    GameEvent.Moved {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 725.2: never matched via a card's bearer -- the monarch's crown-steal is
  -- an inherent ability of no object, so its real match lives in
  -- Pawl.Engine.Monarch.inherentMatch, not here.
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
  -- CR 702.179d: the same, one rule over. The speed-increase ability hangs on no
  -- object either, so Pawl.Engine.Speed.inherentPending is where it is matched.
  TriggerCondition.OpponentLostLifeDuringYourTurn -> False
  -- CR 603.12: a reflexive matches no log entry at all. Its trigger event
  -- happened during the resolution that CREATED it, which CR 603.12's own
  -- exception makes a question about the entry's existence rather than about the
  -- log -- so Event.delayedPending fires it with no event and this answers False
  -- for every one, StateIs' posture.
  TriggerCondition.Reflexive -> False
  -- CR 702.29c: the bearer IS the card that was cycled. The event carries the CR
  -- 400.7 incarnation, which is the object the scan offers as the bearer.
  --
  -- The CAUSE makes this narrower than the discard condition below, and is the
  -- whole of rule 702.29c's "to pay an activation cost of a cycling ability": an
  -- ordinary discard of a card that HAS cycling reaches the same graveyard through
  -- the same funnel and must fire nothing.
  TriggerCondition.SelfCycled -> case event of
    GameEvent.Discarded (Discarded.MkDiscarded _ oid DiscardCause.ToPayCyclingCost) -> oid == bearer
    GameEvent.Discarded (Discarded.MkDiscarded _ _ DiscardCause.Ordinary) -> False
    GameEvent.Drew {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 702.94a: the bearer IS the card that was revealed, and the reveal was
  -- miracle's own. SelfCycled's shape one rule over, cause and all -- and for the
  -- same reason: the same card shown by an ordinary reveal reaches the same log
  -- through the same funnel and must fire nothing. That is rule 702.94a's "THIS
  -- WAY", and CR 603.11 is what makes the two halves one ability.
  --
  -- The Ordinary arm is a REGRESSION FENCE rather than proven behaviour, and so
  -- is `revealedInHand`'s: the pool's ordinary hand reveals -- an activation cost
  -- paid from a hidden zone, and CR 614.1c's as-enters reveal (Rustic Clachan) --
  -- are on cards that carry no miracle, so no board tells the two readings apart
  -- and neither gate can be broken on its own while the other stands. Written
  -- because the rule says it.
  --
  -- The id in the event is the incarnation that reached the hand, which is the
  -- object the hand source offers as the bearer -- no CR 400.7 step separates
  -- them, since CR 701.20b moves nothing.
  --
  -- The FIRST-DRAW gate is NOT re-asked here. Rule 702.94a states it on the
  -- static half, so it decides whether the reveal happens at all
  -- (Event.offerMiracleReveal); a reveal that happened cannot have been the wrong
  -- draw, and re-reading GameState.drawsThisTurn at the scan would ask about the
  -- board one CR 117.5 boundary later.
  TriggerCondition.SelfRevealedForMiracle -> case event of
    GameEvent.Revealed (Revealed.MkRevealed _ oid RevealCause.ForMiracle _) -> oid == bearer
    GameEvent.Revealed (Revealed.MkRevealed _ _ RevealCause.Ordinary _) -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 701.9a: the bearer IS the card that was discarded. SelfCycled's shape
  -- above with the CAUSE dropped, which is the whole difference between the two:
  -- CR 702.29a makes cycling a discard, so "when you discard this card" fires on
  -- a cycle as well as on an ordinary discard, where rule 702.29c's "to pay an
  -- activation cost of a cycling ability" admits only the one cause.
  --
  -- The discarding player is not compared against anything. CR 701.9a moves the
  -- card from its OWNER's hand, and CR 113.8 makes that owner the controller of
  -- an ability of a card in a graveyard, so the two seats coincide by
  -- construction and there is no PlayerRelation to read.
  TriggerCondition.SelfDiscarded -> case event of
    GameEvent.Discarded (Discarded.MkDiscarded _ oid _) -> oid == bearer
    GameEvent.Drew {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 701.9a: a card was discarded, by a player the relation admits. The
  -- discarding player comes from the event; CR 109.5 fixes "you" as the
  -- ability's controller (CR 603.3a), and PlayerRelation.holds is what each arm
  -- MEANS -- Megrim's "an opponent" is every other player, CR 806.1 in a
  -- free-for-all and CR 102.2 in a two-player game, with CR 102.3's teams the one
  -- reading that is wrong for (#175). Every relation-carrying condition below
  -- reads it, so they cannot drift apart.
  --
  -- The bearer is NOT part of the match, unlike every Self- condition here: the
  -- enchantment watches someone else's hand and has nothing to do with the card
  -- that left it.
  --
  -- CR 702.29d -- "these abilities trigger only once when a card is cycled" --
  -- needs no clause of its own, and the DiscardCause is ignored for that reason
  -- rather than by omission. CR 702.29a makes cycling a discard, so a cycled
  -- card must fire this; the cycle is ONE Discarded event, so it fires it once.
  -- TriggerSpec's "CR 702.29d cycling a card fires the discard trigger exactly
  -- once" is the test that proves it.
  TriggerCondition.PlayerDiscards relation -> case event of
    GameEvent.Discarded (Discarded.MkDiscarded discarder _ _) -> PlayerRelation.holds relation you discarder
    GameEvent.Drew {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- The discard arm above narrowed by the CAUSE, which is the whole of the
  -- difference: CR 702.29a makes cycling a discard, so an ordinary discard
  -- reaches the same log through the same funnel and must fire nothing here.
  -- That cause is what rule 702.29c calls "to pay an activation cost of a
  -- cycling ability"; rule 702.29c itself defines only the self-scoped phrase,
  -- and what fixes the "you" of this watcher-scoped one is CR 603.3a, read
  -- through PlayerRelation.holds so a new relation is answered once.
  --
  -- The bearer is NOT part of the match, on PlayerDiscards' posture: Prickly
  -- Marmoset watches its controller's hand and has nothing to do with the card
  -- that left it.
  --
  -- CR 702.29d needs no clause: one cycle is one Discarded event, so this fires
  -- once by construction, exactly as the discard arm above does.
  TriggerCondition.PlayerCycles relation -> case event of
    GameEvent.Discarded (Discarded.MkDiscarded discarder _ DiscardCause.ToPayCyclingCost) -> PlayerRelation.holds relation you discarder
    GameEvent.Discarded (Discarded.MkDiscarded _ _ DiscardCause.Ordinary) -> False
    GameEvent.Drew {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 121.1: a card was DRAWN, by a player the relation admits, and it was that
  -- player's `nth` draw of the turn. The ordinal comes off the event, which
  -- Event.drawCard stamped from GameState.drawsThisTurn as the draw happened;
  -- CR 109.5 fixes "you" as the ability's controller (CR 603.3a), and an
  -- opponent is every other player for the reason PlayerDiscards gives.
  --
  -- The bearer is NOT part of the match, exactly as for PlayerDiscards: Erudite
  -- Wizard is a creature watching a library, and the card drawn is nothing to do
  -- with it.
  --
  -- Equality on the ordinal, which is what makes "your second card" fire once in
  -- a turn with five draws.
  TriggerCondition.PlayerDrawsNthCard (PlayerDrawsNthCard.MkPlayerDrawsNthCard relation nth) -> case event of
    GameEvent.Drew (Drew.MkDrew drawer ordinal) ->
      ordinal
        == nth
        && PlayerRelation.holds relation you drawer
    GameEvent.Discarded {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 725.1: a player BECAME the monarch. Matched against the event the
  -- crowning records, so every route through CR 725.1's "an effect instructs a
  -- player to become the monarch" fires it alike: Effect.BecomeMonarch records
  -- this event whichever MonarchTarget named the player, which covers an entry
  -- trigger's crown, a targeted crown and CR 725.2's stolen crown.
  --
  -- CR 725.4's departure reassignment records the same event, so the crown
  -- moving because the monarch left is matched alike. TriggerSpec's "CR 725.4 a
  -- departure crowns alice, and that crowning fires her edict" proves it.
  --
  -- The event carries exactly one player, which is CR 725.3 ("Only one player can
  -- be the monarch at a time") rather than a simplification -- so the relation is
  -- the whole comparison and there is no filter to apply. The bearer is NOT part
  -- of the match: Custodi Lich watches a designation, not itself.
  TriggerCondition.PlayerBecomesMonarch relation -> case event of
    GameEvent.BecameMonarch crowned -> PlayerRelation.holds relation you crowned
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 508.3a: the bearer was DECLARED as an attacker. Matched against the
  -- declaration event rather than Combat.attackers, which keeps that rule's last
  -- sentence true -- a creature put onto the battlefield attacking is in the
  -- record and has no event here.
  TriggerCondition.SelfAttacks frequency -> case event of
    GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared oid _ _) ->
      oid == bearer && case frequency of
        TriggerFrequency.EveryTime -> True
        -- "For the first time each turn". The declaration being matched is
        -- already in the log when the scan reaches here, so "the first time" is
        -- "the only one so far", and the log's clearing at turn handoff is what
        -- makes it "each turn". Counted per BEARER, and CR 400.7 mints a new
        -- object on a zone change, so a creature that left and returned attacks
        -- for the first time again.
        TriggerFrequency.FirstTimeEachTurn -> declarationsOf bearer gs <= 1
    -- The other declaration. CR 509.1a's blocker is not an attacker, and a
    -- creature can be both this combat.
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 702.149a: the bearer was declared as an attacker, and at least one OTHER
  -- attacking creature satisfies the Filter. SelfAttacks' event and its identity
  -- check, with an existential over the rest of the declaration added.
  --
  -- The companions come from Combat.attackers, which at this moment IS the
  -- declaration: CR 508.2b puts these triggers on the stack before any player gets
  -- priority, so the only creatures CR 508.4 could add have had no window to
  -- arrive. The event log would answer the same question one turn too widely: it
  -- keeps an earlier combat phase's declarations, where the combat record is
  -- cleared per phase.
  --
  -- The source's power is supplied here rather than left Nothing, which is what
  -- makes CR 702.149a's PowerGreaterThanSource evaluable at a trigger match at
  -- all; it is a thunk for the reason Target.admittedGiven's is. Both it and each
  -- candidate view read through the bearer's last known information (CR 608.2h),
  -- a REGRESSION FENCE rather than a live path: the bearer was declared an
  -- attacker a moment ago and nothing has had priority since.
  TriggerCondition.SelfAttacksWithAnother f -> case event of
    GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared oid _ _)
      | oid == bearer ->
          let viewOf = Projection.viewWithLastKnown bearer gs
              context = Filter.contextComparingPower (Just you) bearer (Filter.power =<< viewOf bearer)
              -- Rule 702.149a's "OTHER". Not independently observable while the
              -- Filter's comparison is strict -- nothing has power greater than
              -- its own -- so dropping it leaves the suite green; it is here
              -- because the rule says it, not because a test proves it.
              admits other = other /= bearer && maybe False (\view -> Filter.matches context view f) (viewOf other)
           in any admits (Map.keys (Combat.attackers (GameState.combat gs)))
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 506.5: a creature the Filter admits was declared as an attacker, and it
  -- was the ONLY one the declaration named. The same event SelfAttacks reads,
  -- with the count taken instead of the bearer's identity.
  --
  -- `count == 1` and never a floor, unlike SelfBlocksAtLeast's `>= n`: rule 506.5
  -- says "the ONLY creature declared as an attacker", which is one number.
  --
  -- The attacker's characteristics come from the game as it stands, which is CR
  -- 603.10's normal reading -- CR 508.1k has already made the creature attacking,
  -- and CR 508.2's triggers go on the stack before any player gets priority.
  -- viewWithLastKnown for PermanentEnters' reason.
  TriggerCondition.CreatureAttacksAlone f -> case event of
    GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared attacker _ count)
      | count == 1 ->
          case Projection.viewWithLastKnown attacker gs attacker of
            Nothing -> False
            Just view -> Filter.matches (Filter.contextFor (Just you) (Just bearer)) view f
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 508.3a's second sentence: some creature was declared as an attacker, and CR
  -- 508.5's defending player for it is the bearer's controller. SelfAttacks' event
  -- with the identity check moved from the ATTACKER to the DEFENDER.
  --
  -- "You or a planeswalker you control" needs no second test and no board read: CR
  -- 508.5/508.5a already resolve an attacked planeswalker to its controller, and
  -- Combat.declareAttackers stamped that player onto the event. Where
  -- SelfAttacksPlayerWithMostLife below has to consult Combat.attackers -- rule
  -- 702.105a says "the player", so an attacked planeswalker must not count -- this
  -- condition wants exactly the field the event carries.
  --
  -- No Filter over the attacker and no count: CR 508.1a admits only creatures, and
  -- this fires once per declared attacker (CR 508.3a), not once per declaration --
  -- which is PlayerAttacks (CR 508.3d) and AttachedPlayerIsAttacked (CR 508.3b)
  -- below, each against its own event.
  TriggerCondition.CreatureAttacksYou -> case event of
    GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared _ defending _) -> defending == you
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 508.3d: the player the payload names declared one or more attackers. The
  -- once-per-DECLARATION arity, matched against the once-per-declaration event --
  -- CreatureAttacksYou above reads the per-attacker one and
  -- AttachedPlayerIsAttacked below the per-target one, and no grouping happens
  -- here for their reason: this function sees one event at a time.
  --
  -- Rule 508.3d's "[a player]" as the card printed it, against CR 109.5's `you`.
  -- Read off the event, which Combat.declareAttackers stamps with CR 508.1's
  -- declaring player, rather than off GameState.activePlayer: the two agree
  -- today, CR 508.1 letting only the active player declare, but the rule asks
  -- who declared and the event is the record of that.
  --
  -- No bearer test, where SelfAttacks pins one: rule 508.3d's subject is a
  -- player, so the bearer only frames whose declaration this is --
  -- CreatureAttacksAlone's bystanding posture. A Boggart Prankster held out of
  -- combat still triggers on its controller's attack.
  --
  -- Pawl.EventTriggerSpec's Avatar Roku, Firebender group proves the relation is
  -- READ: its two boards -- an opponent declares, then Roku's own controller
  -- does -- falsify hardcoding You and hardcoding Opponent respectively. Roku's
  -- payload adds mana rather than targeting, which is why those boards see a
  -- difference where Boggart Prankster's "target attacking Goblin you control"
  -- cannot.
  --
  -- The You NARROWING is still a regression fence rather than a proven
  -- behaviour: answering True unconditionally leaves the whole suite green,
  -- because the corpus's one You producer is Boggart Prankster, and on any board
  -- where a non-controller declares, its trigger has no legal target and CR
  -- 603.3d removes it either way. What would observe it is a card printing
  -- "whenever you attack" whose payload does not need its controller to have an
  -- attacker.
  TriggerCondition.PlayerAttacks relation -> case event of
    GameEvent.AttackersDeclared attacker -> PlayerRelation.holds relation you attacker
    GameEvent.BecameTapped _ -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
  -- CR 508.3c: the player the payload names declared one or more attackers the
  -- Filter admits. The arm above narrowed, against the same once-per-DECLARATION
  -- event, because that is the arity every printing of "attacks with" takes --
  -- the quantifier is in the printed sentence ("one or more Birds"), so a
  -- declaration naming two Birds fires it once. CR 508.3a's per-attacker event
  -- sits below answering False: matching it here would fire once per declared
  -- Bird instead.
  --
  -- The creatures come from Combat.declaredAttackers rather than from the event,
  -- which carries only who declared, and NOT from Combat.attackers as
  -- SelfAttacksWithAnother reads it: CR 508.4 says a creature put onto the
  -- battlefield attacking never "attacked", and putOntoBattlefieldAttacking
  -- writes the second map and not this one. A REGRESSION FENCE rather than a
  -- proved behaviour: reading Combat.attackers here leaves the suite green,
  -- since no board reaches a declaration with such a creature already in combat.
  --
  -- Exact at this moment for SelfAttacksWithAnother's reason -- CR 508.2b puts
  -- every trigger from this declaration on the stack together, so no player has
  -- had priority since.
  --
  -- Rule 508.3c's "that player CONTROLS" is Combat.joinedUnder, CR 506.4's
  -- record of who controlled each combatant as it joined. Not independently
  -- observable: CR 508.1a lets only the active player declare, so every id in
  -- declaredAttackers joined under the declarer, and dropping the comparison
  -- leaves the suite green. It is here because the rule says it.
  --
  -- viewWithLastKnown and the Filter context framed by the bearer, exactly as
  -- SelfBlocksOneOrMore's arm below does it. Nothing is bound, so the context's
  -- empty slot map is honest here.
  TriggerCondition.PlayerAttacksWith (PlayerAttacksWith.MkPlayerAttacksWith relation f) -> case event of
    GameEvent.AttackersDeclared attacker
      | PlayerRelation.holds relation you attacker ->
          let combat = GameState.combat gs
              admits oid =
                Map.lookup oid (Combat.joinedUnder combat) == Just attacker
                  && maybe False (\view -> Filter.matches (Filter.contextFor (Just you) (Just bearer)) view f) (Projection.viewWithLastKnown oid gs oid)
           in any admits (Set.toList (Combat.declaredAttackers combat))
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
  -- CR 508.3b: the player this ability's source is attached to was attacked.
  -- CreatureAttacksYou's question asked once per DECLARATION instead, which is
  -- the whole of what separates them: this matches the grouped
  -- GameEvent.BecameAttacked, recorded once per distinct target, so the arity is
  -- the event's and no dedup happens here.
  --
  -- The subject comes from Object.attachedTo (CR 303.4m), read live rather than
  -- through last known information as AttachedCreatureDies reads it: that arm
  -- matches an event that may have taken the bearer with it, and a declaration of
  -- attackers moves nothing.
  --
  -- ONLY AttackTarget.OfPlayer matches. CR 508.1b lists player, planeswalker and
  -- battle separately and rule 508.3b asks about the one attacked, so a creature
  -- sent at a planeswalker the enchanted player controls leaves this silent --
  -- where CreatureAttacksYou, reading CR 508.5's defending player, would fire.
  TriggerCondition.AttachedPlayerIsAttacked -> case event of
    GameEvent.BecameAttacked payload ->
      case Recipient.playerOf =<< (Object.attachedTo =<< Game.lookupObject bearer gs) of
        Just enchanted -> BecameAttacked.target payload == AttackTarget.OfPlayer enchanted
        Nothing -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 508.3e: the player the payload names declared attackers, and at least one
  -- of them was sent at a PLAYER. AttachedPlayerIsAttacked's event and its
  -- per-TARGET arity, with the subject read off a relation instead of off the
  -- bearer's attachment -- so one declaration split across two opponents fires
  -- this TWICE where PlayerAttacks above fires once, which is what parts rule
  -- 508.3e from rule 508.3d.
  --
  -- Both sides come off the EVENT. Combat.declareAttackers stamps CR 508.1's
  -- declaring player onto it beside the target, which is what lets this arm ask
  -- rule 508.3e's question at all; reading GameState.activePlayer for the
  -- attacker would agree today, rule 508.1 letting only the active player
  -- declare, but the rule asks who declared.
  --
  -- The attacked side is UNQUALIFIED, rule 508.3e's "[another player]" as every
  -- printing of this shape leaves it -- Pawl.Types.TriggerCondition's arm says
  -- which cards would narrow it (#2281). A relation there would be nearly
  -- unobservable besides: CR 506.2 and CR 802.2 make every defending player an
  -- opponent of the attacker, so Opponent and AnyPlayer would pick the same
  -- seats on every legal declaration.
  --
  -- ONLY AttackTarget.OfPlayer, which is rule 508.3e's last sentence in as many
  -- words: "it won't trigger if a creature attacks a planeswalker or a battle".
  -- The other exclusion in that sentence -- a creature put onto the battlefield
  -- attacking -- holds by construction instead, CR 508.4 saying such a creature
  -- was never declared and Combat.putOntoBattlefieldAttacking recording no
  -- event.
  --
  -- No bearer test, PlayerAttacks' bystanding posture above: rule 508.3e's two
  -- subjects are both players, so a Seifer held out of combat still triggers on
  -- its controller's attack.
  TriggerCondition.PlayerAttacksPlayer relation -> case event of
    GameEvent.BecameAttacked payload -> case BecameAttacked.target payload of
      AttackTarget.OfPlayer _ -> PlayerRelation.holds relation you (BecameAttacked.attacker payload)
      AttackTarget.OfPlaneswalker _ -> False
      AttackTarget.OfBattle _ -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 702.105a: the bearer was declared attacking A PLAYER, and no player still in
  -- the game has more life than that one. SelfAttacks' event and its identity
  -- check, with the comparison added.
  --
  -- Whom the bearer attacked comes from Combat.attackers rather than from the
  -- event, which carries CR 508.5's DEFENDING player instead -- the same id for an
  -- attacked planeswalker or battle, where rule 702.105a names the player. Reading
  -- the record here is exact for SelfAttacksWithAnother's reason.
  --
  -- Non-strict, which is rule 702.105a's "or tied for most life", and over
  -- Game.stillPlaying rather than every seat the game began with: a player who has
  -- left (CR 800.4a) has no life total left to be beaten.
  TriggerCondition.SelfAttacksPlayerWithMostLife -> case event of
    GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared oid _ _)
      | oid == bearer ->
          case Map.lookup bearer (Combat.attackers (GameState.combat gs)) of
            Just (AttackTarget.OfPlayer attacked) ->
              let lifeOf pid = fmap Player.life (Map.lookup pid (GameState.players gs))
               in case lifeOf attacked of
                    Nothing -> False
                    Just theirs -> all (maybe True (<= theirs) . lifeOf) (Game.stillPlaying gs)
            _ -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 509.3a: the bearer was DECLARED as a blocker. SelfAttacks' mirror, and
  -- matched against GameEvent.BlocksDeclared for that arm's reason -- CR 509.4's
  -- creature put onto the battlefield blocking is in Combat.blockers, and the
  -- only event it records on the blocking side is a GameEvent.BecameBlocking
  -- this condition does not read, which is rule 509.3a's last sentence.
  --
  -- The attacking creatures the declaration named are neither compared nor bound:
  -- this condition is CR 509.3a's, which names none. SelfBlocksCreature's arm
  -- below is rule 509.3b's, which does.
  --
  -- The GROUPED event, which is rule 509.3a's "only once each combat for that
  -- creature, even if it blocks multiple creatures": a blocker declared against
  -- two attackers makes two BecameBlocking and one BlocksDeclared, so matching
  -- the pairwise one here would fire twice.
  TriggerCondition.SelfBlocks -> case event of
    GameEvent.BlocksDeclared (BlocksDeclared.MkBlocksDeclared blocker _) -> blocker == bearer
    GameEvent.BecameBlocking {} -> False
    -- The bearer BECOMING blocked is the other side of the same declaration and
    -- not this condition: CR 509.3a's creature is the one doing the blocking.
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 509.3b: the PAIRWISE event, which is that rule's "once for each attacking
  -- creature the creature with the ability blocks" -- and the difference from
  -- SelfBlocks above, together with the attacker eventBindings stamps under
  -- Binding.blockedCreature.
  --
  -- The Filter is a predicate over that ATTACKER, read from the game as it
  -- stands, which is rule 509.3f's "at the point blockers are declared" for
  -- SelfBecomesBlockedBy's reason below. viewWithLastKnown for that arm's reason
  -- too -- an attacker already gone (CR 608.2h) is still read as it was on the
  -- battlefield.
  TriggerCondition.SelfBlocksCreature f -> case event of
    GameEvent.BecameBlocking b
      | BecameBlocking.blocker b == bearer,
        -- CR 509.3b's last sentence: "It won't trigger if the creature is put
        -- onto the battlefield blocking." CR 509.3d's arm below is where that
        -- same event does fire, which is the whole reason it is recorded.
        --
        -- Proved rather than fenced, since Aetherplasm: it puts a creature
        -- CARD onto the battlefield blocking, so the arrival brings its own
        -- text. Pawl.CombatEffectSpec puts Loyal Sentry out that way against a
        -- 2/2 and reddens here when this guard is dropped, with the same Sentry
        -- DECLARED as the control leg where it does fire.
        not (BecameBlocking.putOntoBattlefield b) ->
          let attacker = BecameBlocking.attacker b
           in case Projection.viewWithLastKnown attacker gs attacker of
                Nothing -> False
                Just view -> Filter.matches (Filter.contextFor (Just you) (Just bearer)) view f
    GameEvent.BecameBlocking {} -> False
    -- CR 509.3a's grouped event is the once-per-combat one, and matching it here
    -- would lose a blocker's second attacker.
    GameEvent.BlocksDeclared {} -> False
    -- CR 509.3c's grouped event is the bearer BECOMING blocked, which is not a
    -- block by it.
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 509.3e: the bearer blocked at least `n` creatures. SelfBlocks with the
  -- count read, on the very same grouped event -- which is what makes rule
  -- 509.3e's "when blockers are declared" the moment this fires.
  --
  -- Rule 509.3e's "effects that add or remove blockers" also cause it to trigger,
  -- and no such effect is in the pool: the count is the declaration's (#1146).
  TriggerCondition.SelfBlocksAtLeast n -> case event of
    GameEvent.BlocksDeclared (BlocksDeclared.MkBlocksDeclared blocker count) -> blocker == bearer && count >= n
    GameEvent.BecameBlocking {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 509.3e: the bearer blocked at least one creature the Filter admits. The
  -- same grouped event SelfBlocks and SelfBlocksAtLeast read, so the printed "one
  -- or more" fires once for the whole declaration; SelfBlocksCreature's arm above
  -- is the per-attacker reading, and two admitted attackers tells them apart.
  --
  -- The attackers come from Combat.blockers rather than from the event, which
  -- carries a count and no ids -- the count being unfiltered, and this condition
  -- asking about a quality. That map is keyed by ATTACKER, so the bearer's own
  -- entries are the ones whose blocker set holds it. Exact at this moment for
  -- SelfAttacksWithAnother's reason: CR 509.2a puts these triggers on the stack
  -- before any player gets priority, so the record still holds the declaration
  -- that made the event.
  --
  -- viewWithLastKnown, and the Filter context framed by the bearer, exactly as
  -- SelfBecomesBlockedBy's arm below does it.
  TriggerCondition.SelfBlocksOneOrMore f -> case event of
    GameEvent.BlocksDeclared (BlocksDeclared.MkBlocksDeclared blocker _)
      | blocker == bearer ->
          let admits attacker = maybe False (\view -> Filter.matches (Filter.contextFor (Just you) (Just bearer)) view f) (Projection.viewWithLastKnown attacker gs attacker)
              blocked = [attacker | (attacker, blockers) <- Map.toList (Combat.blockers (GameState.combat gs)), Set.member bearer blockers]
           in any admits blocked
    GameEvent.BlocksDeclared {} -> False
    -- The PAIRWISE event is CR 509.3b's, and matching it here would fire once per
    -- attacker blocked rather than once for the declaration.
    GameEvent.BecameBlocking {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 509.3c: the bearer BECAME a blocked creature, which CR 509.1h makes the
  -- declaration's other product. SelfBlocks' arm above is the mirror.
  --
  -- One event per blocked attacker is what Combat.declareBlockers records, so
  -- matching it once is rule 509.3c's "only once each combat for that creature".
  -- A match on GameEvent.BecameBlocking's attacker would fire once per blocker
  -- instead; Pawl.TriggerSpec's two-blocker case is what tells the two apart.
  TriggerCondition.SelfBecomesBlocked -> case event of
    GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked oid _) -> oid == bearer
    -- The same declaration's other branch, and CR 509.1h makes the two exclusive
    -- for any one attacker.
    GameEvent.AttackerUnblocked _ -> False
    -- CR 509.4's creature put onto the battlefield blocking never "blocked", but
    -- that is not why this is False: a blocker's own declaration is CR 509.3a's
    -- event, whoever it was declared against.
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 509.3d: a creature the Filter admits became a blocking creature FOR the
  -- bearer. The pair on GameEvent.BecameBlocking is read from the ATTACKING
  -- side, which is what makes this fire once per blocker where
  -- SelfBecomesBlocked's arm above fires once per attacker.
  --
  -- BecameBlocking.putOntoBattlefield is deliberately not read: rule 509.3d's
  -- third sentence is "In addition, it will trigger if a creature is put onto
  -- the battlefield blocking that creature", the one form of CR 509.3 that CR
  -- 509.4's "never blocked" does not silence. CR 509.3b's arm above reads the
  -- flag, and that difference is the rule.
  --
  -- The blocker's characteristics come from the game as it stands, which is rule
  -- 509.3f's "at the point it becomes a blocking creature": CR 509.2a puts these
  -- triggers on the stack before any player gets priority, so nothing has had a
  -- window to change them. viewWithLastKnown for PermanentEnters' reason -- a
  -- blocker already gone (CR 608.2h) is still read as it was on the battlefield.
  TriggerCondition.SelfBecomesBlockedBy f -> case event of
    GameEvent.BecameBlocking (BecameBlocking.MkBecameBlocking {BecameBlocking.blocker = blocker, BecameBlocking.attacker = attacker})
      | attacker == bearer ->
          case Projection.viewWithLastKnown blocker gs blocker of
            Nothing -> False
            Just view -> Filter.matches (Filter.contextFor (Just you) (Just bearer)) view f
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    -- The GROUPED event is CR 509.3c's, and matching it here would collapse two
    -- blockers into one trigger.
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 509.3e read from the attacking side: the bearer became blocked, by at
  -- least one creature the Filter admits. The GROUPED event, which is the printed
  -- "one or more" -- the arm above fires once per blocker, and two admitted
  -- blockers tells the two apart.
  --
  -- The blockers come from Combat.blockers for SelfBlocksOneOrMore's reason:
  -- GameEvent.AttackerBlocked names the attacker and CR 508.5's defending player,
  -- and no blocker at all. The map is keyed by attacker, so the bearer's own entry
  -- is the whole answer here.
  TriggerCondition.SelfBecomesBlockedByOneOrMore f -> case event of
    GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked attacker _)
      | attacker == bearer ->
          let admits blocker = maybe False (\view -> Filter.matches (Filter.contextFor (Just you) (Just bearer)) view f) (Projection.viewWithLastKnown blocker gs blocker)
           in any admits (Set.toList (Map.findWithDefault Set.empty bearer (Combat.blockers (GameState.combat gs))))
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 509.3e read by a BYSTANDER on the attacking side: a creature attacking a
  -- player the PlayerRelation admits became blocked by at least `n` creatures.
  -- The arm above with its Filter traded for a count, and the identity check on
  -- the bearer dropped -- Seifer, Balamb Rival watches everybody's attackers,
  -- including its own controller's, so nothing here compares an id to the bearer.
  --
  -- TWO events, which is rule 509.3e's two producers. The GROUPED
  -- GameEvent.AttackerBlocked is the declaration's, so the whole declaration
  -- fires it once, and there `>=` and never `==` is rule 509.3e's last sentence.
  -- GameEvent.BecameBlocking is the arrival's, and the arm on it argues its own
  -- comparison; the two are read together below because the count is the same
  -- reading either way.
  --
  -- The count comes from Combat.blockers, which neither event carries, and is
  -- exact at this moment for the arm above's reason -- CR 509.2a puts these
  -- triggers on the stack before any player gets priority, so the record still
  -- holds the declaration and the arrivals that made the event.
  --
  -- Whom the attacker attacked comes from Combat.attackers and only
  -- AttackTarget.OfPlayer answers: CR 508.1b lists player, planeswalker and
  -- battle separately, so a creature sent at an opponent's planeswalker is not
  -- attacking that opponent -- where CR 508.5's defending player, which the event
  -- carries, would resolve to them. SelfAttacksPlayerWithMostLife reads the same
  -- record for the same reason.
  TriggerCondition.CreatureBecomesBlockedByAtLeast (CreatureBecomesBlockedByAtLeast.MkCreatureBecomesBlockedByAtLeast relation n) ->
    let attacksAdmittedPlayer attacker = case Map.lookup attacker (Combat.attackers (GameState.combat gs)) of
          Just (AttackTarget.OfPlayer attacked) -> PlayerRelation.holds relation you attacked
          _ -> False
        blockedBy attacker = Natural.length (Map.findWithDefault Set.empty attacker (Combat.blockers (GameState.combat gs)))
     in case event of
          GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked attacker _) -> attacksAdmittedPlayer attacker && blockedBy attacker >= n
          -- Rule 509.3e's second sentence -- "effects that add or remove
          -- blockers can also cause such abilities to trigger" -- for the one
          -- producer the pool has: a creature PUT ONTO THE BATTLEFIELD
          -- blocking an attacker that was ALREADY blocked, which is Flash
          -- Foliage's Saproling joining a declared blocker. That road records
          -- no GameEvent.AttackerBlocked at all --
          -- Combat.putOntoBattlefieldBlocking withholds it under CR 509.3c's
          -- "only if the attacking creature was an unblocked creature at that
          -- time" -- so the arm above never sees the arrival, and the guard
          -- there is CR 509.3c's own and must not be weakened to reach it.
          --
          -- Removing blockers cannot reach THIS form of the rule: the count
          -- only falls, and a floor is never crossed upwards by a departure.
          --
          -- The flag is load-bearing. Combat.declareBlockers records this same
          -- constructor once per declared PAIR with it clear, so an unguarded
          -- arm would answer an ordinary declaration the arm above has already
          -- answered, once more per blocker.
          --
          -- `== n` here where the arm above reads `>= n`, and the two are not
          -- in disagreement. The rule's "at least" is about how many creatures
          -- blocked when blockers were declared, which is one number the arm
          -- above reads once. An arrival adds exactly one blocker, so the count
          -- crosses the floor at most once; `>=` would fire again on the next
          -- arrival, where the attacker did not newly become blocked by that
          -- many creatures.
          --
          -- `n >= 2` is "GameEvent.AttackerBlocked was not recorded for this
          -- same arrival", stated in terms of the floor: that event rides an
          -- arrival only when the attacker had no blocker before it, which
          -- together with `== n` means n == 1. That conjunct is a REGRESSION
          -- FENCE rather than proved behaviour: Seifer, Balamb Rival is the only
          -- printing and reads two, so relaxing it to `n >= 0` leaves the whole
          -- trigger subtree green. The codec admits one, and both events would
          -- then answer the one arrival.
          --
          -- Not implemented: several creatures put onto the battlefield
          -- blocking one attacker at once, where the count jumps past the floor
          -- rather than landing on it (#2298).
          GameEvent.BecameBlocking (BecameBlocking.MkBecameBlocking {BecameBlocking.attacker = attacker, BecameBlocking.putOntoBattlefield = True}) ->
            attacksAdmittedPlayer attacker && n >= 2 && blockedBy attacker == n
          GameEvent.BecameBlocking {} -> False
          GameEvent.AttackerUnblocked _ -> False
          GameEvent.BlocksDeclared {} -> False
          GameEvent.AttackerDeclared {} -> False
          GameEvent.Moved {} -> False
          GameEvent.DamageDealt _ -> False
          GameEvent.StepBegan {} -> False
          GameEvent.SpellCast {} -> False
          GameEvent.DamagePrevented {} -> False
          GameEvent.BecameMonarch _ -> False
          GameEvent.Discarded {} -> False
          GameEvent.Drew {} -> False
          GameEvent.Revealed {} -> False
          GameEvent.SpellCountered _ -> False
          GameEvent.HalfUnlocked {} -> False
          GameEvent.TurnedFaceUp _ -> False
          GameEvent.Transformed {} -> False
          GameEvent.BecameDesignated {} -> False
          GameEvent.Evolved _ -> False
          GameEvent.Mentored {} -> False
          GameEvent.Trained _ -> False
          GameEvent.PermanentSacrificed {} -> False
          GameEvent.AbilityTriggered {} -> False
          GameEvent.LoyaltyAbilityActivated _ -> False
          GameEvent.LifeLost {} -> False
          GameEvent.LifeGained {} -> False
          GameEvent.CountersPut {} -> False
          GameEvent.CountersRemoved {} -> False
          GameEvent.ControlChanged {} -> False
          GameEvent.VentureMarkerEntered {} -> False
          GameEvent.BecameTarget {} -> False
          GameEvent.BecameAttached {} -> False
          GameEvent.LeftTheGame _ -> False
          GameEvent.Milled {} -> False
          GameEvent.Scried _ -> False
          GameEvent.Surveiled _ -> False
          GameEvent.DiceRolled _ -> False
          GameEvent.ClassLevelSet _ -> False
          GameEvent.Plotted _ -> False
          GameEvent.Explored _ -> False
          GameEvent.Exerted _ -> False
          GameEvent.BecameAttacked _ -> False
          GameEvent.AttackersDeclared _ -> False
          GameEvent.BecameTapped _ -> False
  -- CR 509.1h: the bearer became an UNBLOCKED creature, which the glossary's
  -- "attacks and isn't blocked" entry sends here. SelfBecomesBlocked's arm above
  -- is the other branch of the same turn-based action, and no attacker can
  -- produce both events in one declaration.
  --
  -- The event alone answers it. Reading Combat.blockers instead would answer for
  -- the board at scan time, and rule 509.1h's last sentence is what that gets
  -- wrong: an attacker whose blockers all left combat is still blocked.
  TriggerCondition.SelfAttacksUnblocked -> case event of
    GameEvent.AttackerUnblocked oid -> oid == bearer
    -- CR 509.1i's declaration events are the blocked side of the same action; an
    -- attacker this one names got a blocker, so it is not unblocked.
    GameEvent.AttackerDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 603.6: a zone-change trigger matched on BOTH ends of the move, library to
  -- graveyard. The bearer is the incarnation the card became on arrival per CR
  -- 400.7e, a graveyard being public (CR 400.2). The pair is also what makes CR
  -- 113.6k put this ability in the graveyard rather than on the battlefield.
  --
  -- `from` is the half that does the work: the same card discarded out of a hand
  -- or dying off the battlefield reaches the same graveyard and must not trigger.
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> case event of
    GameEvent.Moved (Moved.MkMoved zc _) ->
      ZoneChange.object zc == bearer
        && ZoneChange.from zc == Zone.Library
        && ZoneChange.to zc == Zone.Graveyard
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 603.6 with NO origin zone: the destination is the whole condition, so a
  -- discard, a mill, a countered spell and a death all match. `from` is
  -- deliberately unread, which is the one line separating this from the two
  -- conditions on either side of it.
  --
  -- Matched on `object`, the arriving incarnation, and NOT on `departed`: CR
  -- 603.6c's last sentence takes this out of the leaves-the-battlefield family,
  -- so CR 603.10a's look-back does not reach it and CR 603.10's normal reading --
  -- the objects that exist immediately after the event -- applies. That is also
  -- what makes the graveyard the one zone the scan has to find the bearer in,
  -- however far away the card started.
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> case event of
    GameEvent.Moved (Moved.MkMoved zc _) ->
      ZoneChange.object zc == bearer
        && ZoneChange.to zc == Zone.Graveyard
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 603.6c narrowed by CR 700.4's definition of "dies": the bearer was put into
  -- a graveyard from the battlefield. Both ends are load-bearing -- `from` keeps a
  -- permanent DISCARDED out of a hand silent, and `to` keeps one EXILED off the
  -- battlefield silent, the latter having left the battlefield without dying.
  -- SelfLeavesTheBattlefield below is the other condition, and naming this one
  -- after the printed word is what keeps them apart.
  --
  -- Matched on `departed`, NOT `object`: CR 603.10a makes leaves-the-battlefield
  -- abilities look back in time, so the bearer offered here is the permanent as it
  -- was immediately before the event, never the CR 400.7 incarnation.
  TriggerCondition.SelfDies -> case event of
    GameEvent.Moved (Moved.MkMoved zc _) ->
      ZoneChange.departed zc == bearer
        && ZoneChange.from zc == Zone.Battlefield
        && ZoneChange.to zc == Zone.Graveyard
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- The same rule and zone pair as SelfDies, watched by a BYSTANDER. The bearer
  -- frames the match rather than being it, as for PermanentEnters: it is the
  -- Filter.Context's source (so `Not IsSource` is "another"), and its controller
  -- is CR 109.5's "you".
  --
  -- Matched on `departed`, with characteristics from CR 608.2h last known
  -- information rather than a live read -- both CR 603.10a's look-back. The
  -- PermanentEnters arm makes the opposite choice, rightly, since CR 603.6b puts an
  -- entrant's continuous effects on the moment it is on the battlefield. A dead
  -- permanent has no live reading left, and reading the graveyard card instead
  -- would answer "you control" wrong rather than not at all: CR 108.4a substitutes
  -- the OWNER, so a stolen creature would be credited back to the player who no
  -- longer had it when it died.
  --
  -- viewWithLastKnown aimed at the deceased twice over, which is how it is asked
  -- for the snapshot: it takes the last-known branch only for the id it is
  -- anchored to, and only once that id is gone.
  --
  -- Nothing is a permanent that is gone AND filed no last known information, about
  -- which no Filter can honestly answer.
  TriggerCondition.PermanentDies f -> case event of
    GameEvent.Moved (Moved.MkMoved zc _)
      | ZoneChange.from zc == Zone.Battlefield && ZoneChange.to zc == Zone.Graveyard ->
          let deceased = ZoneChange.departed zc
           in case Projection.viewWithLastKnown deceased gs deceased of
                Nothing -> False
                Just view -> Filter.matches (Filter.contextFor (Just you) (Just bearer)) view f
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 603.2c's batch reading of the arm above (Vengeful Townsfolk's "whenever ONE
  -- OR MORE other creatures you control die"). Delegated rather than duplicated
  -- because the per-EVENT question is the same one: which deaths this condition
  -- admits is PermanentDies' answer, filter, look-back and all.
  --
  -- What makes it fire ONCE for a whole sweep is not here. This matcher's contract
  -- is that it sees one event at a time, so it cannot count occurrences of a CR
  -- 704.3 / CR 608.2f batch; `batchScoped` below marks the condition and
  -- eventTriggers keeps the first pending trigger per (bearer, ability) within each
  -- Pawl.Types.EventGroup. So this arm answering True for every member of the batch
  -- is deliberate, not a missing dedup: the arm and the dedup are two halves of one
  -- rule, and matchesTrigger alone is not the whole of it.
  TriggerCondition.PermanentsDie f -> matchesTriggerGiven bindings gs bearer you (TriggerCondition.PermanentDies f) event
  -- CR 700.4's "dies" once more, asked of the permanent the bearer is attached
  -- to: PermanentDies' battlefield-to-graveyard pair, matched on
  -- ZoneChange.departed for that arm's reason (CR 603.10a), against the host id
  -- Object.attachedTo records.
  --
  -- The host is read through Recipient.objectOf, AttachedCreatureMentors' route
  -- for CR 303.4's other destination: an Aura enchanting a PLAYER has no host id
  -- to compare and answers False, as does one attached to nothing.
  --
  -- No characteristic of the deceased is read, unlike PermanentDies -- the
  -- attachment link already says which permanent this is about, so there is
  -- nothing for last known information to answer.
  --
  -- LAST KNOWN INFORMATION where the bearer is gone (CR 608.2h), which is
  -- AttachedCreatureMentors' one point of departure and is load-bearing rather
  -- than defensive: CR 704.5m takes the Aura off the battlefield in the very SBA
  -- batch that buried its host, and CR 117.5 places triggers only after that
  -- batch settles, so by the time this is asked the live link is ALWAYS gone.
  -- The live read is kept ahead of it for the Equipment-shaped case, where
  -- CR 704.5n leaves the bearer standing.
  TriggerCondition.AttachedCreatureDies -> case event of
    GameEvent.Moved (Moved.MkMoved zc _)
      | ZoneChange.from zc == Zone.Battlefield && ZoneChange.to zc == Zone.Graveyard ->
          let hostOfBearer = case Game.lookupObject bearer gs of
                Just obj -> Object.attachedTo obj
                Nothing -> LastKnown.attachedTo =<< Map.lookup bearer (GameState.lastKnown gs)
           in (Recipient.objectOf =<< hostOfBearer) == Just (ZoneChange.departed zc)
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 701.26a's tap, asked of the permanent the bearer is attached to
  -- (Betrayal's "whenever enchanted creature becomes tapped"). The event names
  -- whichever permanent turned sideways; Object.attachedTo says whether that is
  -- this bearer's host.
  --
  -- CR 603.2e's exclusions are discharged where the event is MINTED, in `tap`
  -- above, rather than here: a permanent that entered tapped never transitioned,
  -- and rule 701.26a's "only untapped permanents can be tapped" makes a repeat tap
  -- no event at all. So this arm asks only whose tap it was.
  --
  -- The host is read through Recipient.objectOf, AttachedCreatureDies' route for
  -- CR 303.4's other destination: an Aura enchanting a PLAYER has no host id to
  -- compare and answers False, as does one attached to nothing.
  --
  -- LIVE only, where AttachedCreatureDies falls back on CR 608.2h last known
  -- information. That fallback is load-bearing there because CR 704.5m buries the
  -- Aura in the same SBA batch as its host; here the host is still standing -- it
  -- has just become tapped -- so the link is on the board to be read.
  TriggerCondition.AttachedCreatureBecomesTapped -> case event of
    GameEvent.BecameTapped tapped ->
      let hostOfBearer = Object.attachedTo =<< Game.lookupObject bearer gs
       in (Recipient.objectOf =<< hostOfBearer) == Just tapped
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
  -- CR 603.6c taken whole. The `from` half matches SelfDies'; the `to` half is
  -- where they part company, this one asking only that the destination be ANOTHER
  -- zone.
  --
  -- The `to /= Battlefield` guard is that rule's own word "another", and is
  -- load-bearing: recordTokenEntry files a battlefield-to-battlefield pseudo-move
  -- whose `departed` is the token's own id, so a token bearing this condition
  -- would fire on its own creation without it.
  --
  -- Matched on `departed` for SelfDies' reason (CR 603.10a).
  --
  -- CR 603.6c's OTHER trigger event is the second arm: a phased-in permanent
  -- leaving the game because its owner left it (CR 800.4a). No zone pair to
  -- check there -- the permanent was on the battlefield or the event would not
  -- have been recorded, and CR 702.26k's exclusion of a phased-out one is
  -- applied where the event is emitted, in Pawl.Engine.Departure.
  --
  -- SelfDies deliberately does NOT take the same arm: CR 700.4 makes "dies" a
  -- move to a graveyard, and leaving the game reaches no zone at all.
  TriggerCondition.SelfLeavesTheBattlefield -> case event of
    GameEvent.Moved (Moved.MkMoved zc _) ->
      ZoneChange.departed zc == bearer
        && ZoneChange.from zc == Zone.Battlefield
        && ZoneChange.to zc /= Zone.Battlefield
    GameEvent.LeftTheGame oid -> oid == bearer
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
  -- The SAME two trigger events read by a BYSTANDER: PermanentDies' Filter over
  -- CR 608.2h last known information, asked of the arm above's wider destination
  -- test and of its CR 800.4a leaving-the-game form alike.
  --
  -- viewWithLastKnown aimed at the departed id twice over, PermanentDies'
  -- posture and its reasons: CR 603.10a's look-back is what makes "you control"
  -- answerable about a permanent that is a card in a graveyard -- or in a hand,
  -- or nowhere at all -- by the time the scan runs. A permanent that filed no
  -- last known information is one no Filter can honestly answer about, so it
  -- matches nothing.
  TriggerCondition.PermanentLeavesTheBattlefield f ->
    let admits departed = case Projection.viewWithLastKnown departed gs departed of
          Nothing -> False
          Just view -> Filter.matches (Filter.contextFor (Just you) (Just bearer)) view f
     in case event of
          GameEvent.Moved (Moved.MkMoved zc _)
            | ZoneChange.from zc == Zone.Battlefield && ZoneChange.to zc /= Zone.Battlefield ->
                admits (ZoneChange.departed zc)
          GameEvent.Moved {} -> False
          GameEvent.LeftTheGame oid -> admits oid
          GameEvent.Milled {} -> False
          GameEvent.Scried _ -> False
          GameEvent.Surveiled _ -> False
          GameEvent.DiceRolled _ -> False
          GameEvent.ClassLevelSet _ -> False
          GameEvent.Plotted _ -> False
          GameEvent.Explored _ -> False
          GameEvent.Exerted _ -> False
          GameEvent.BecameAttacked _ -> False
          GameEvent.AttackersDeclared _ -> False
          GameEvent.BecameTapped _ -> False
          GameEvent.DamageDealt _ -> False
          GameEvent.StepBegan {} -> False
          GameEvent.SpellCast {} -> False
          GameEvent.DamagePrevented {} -> False
          GameEvent.BecameMonarch _ -> False
          GameEvent.Discarded {} -> False
          GameEvent.Drew {} -> False
          GameEvent.Revealed {} -> False
          GameEvent.AttackerDeclared {} -> False
          GameEvent.BecameBlocking {} -> False
          GameEvent.BlocksDeclared {} -> False
          GameEvent.AttackerBlocked {} -> False
          GameEvent.AttackerUnblocked _ -> False
          GameEvent.SpellCountered _ -> False
          GameEvent.HalfUnlocked {} -> False
          GameEvent.TurnedFaceUp _ -> False
          GameEvent.Transformed {} -> False
          GameEvent.BecameDesignated {} -> False
          GameEvent.Evolved _ -> False
          GameEvent.Mentored {} -> False
          GameEvent.Trained _ -> False
          GameEvent.PermanentSacrificed {} -> False
          GameEvent.AbilityTriggered {} -> False
          GameEvent.LoyaltyAbilityActivated _ -> False
          GameEvent.LifeLost {} -> False
          GameEvent.LifeGained {} -> False
          GameEvent.CountersPut {} -> False
          GameEvent.CountersRemoved {} -> False
          GameEvent.ControlChanged {} -> False
          GameEvent.VentureMarkerEntered {} -> False
          GameEvent.BecameTarget {} -> False
          GameEvent.BecameAttached {} -> False
  -- CR 702.55b/702.55c: SelfDies' zone pair, asked of the object the BEARER
  -- HAUNTS rather than of the bearer itself -- so the id compared against
  -- ZoneChange.departed is the one GameState.haunting files the bearer under, and
  -- a bearer that haunts nothing matches nothing.
  --
  -- Matched on `departed` for PermanentDies' reason (CR 603.10a): the haunt
  -- ability targeted the permanent as it was on the battlefield, and that is the
  -- id the link was written with.
  --
  -- NO characteristic of the deceased is read, where PermanentDies reads a whole
  -- Filter: rule 702.55b keeps "the creature it haunts" pointing at the object
  -- targeted "regardless of whether or not that object is still a creature", so a
  -- creature that was turned into a Treasure and then destroyed still fires this.
  TriggerCondition.HauntedCreatureDies -> case event of
    GameEvent.Moved (Moved.MkMoved zc _) ->
      ZoneChange.from zc == Zone.Battlefield
        && ZoneChange.to zc == Zone.Graveyard
        && Map.lookup bearer (GameState.haunting gs) == Just (ZoneChange.departed zc)
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 701.6a: a spell was countered, by a spell or ability whose controller the
  -- relation admits. The countering source's controller comes from the event,
  -- captured as the counter happened, and CR 109.5/603.3a fix "you" as the
  -- ability's controller.
  --
  -- The bearer is NOT part of the match -- the PlayerDiscards posture rather than
  -- any Self- condition's -- since the bearer is a permanent and the countering is
  -- done by a spell somewhere else.
  --
  -- Neither "can't be countered" gate needs a clause here -- CR 113.6g's on the
  -- spell, CR 613.11's on a permanent's static ability: a spell that can't be
  -- countered is not countered at all (CR 101.2), so `counter` records nothing
  -- and there is no event for this arm to see.
  TriggerCondition.SpellOrAbilityCounters relation -> case event of
    GameEvent.SpellCountered c -> PlayerRelation.holds relation you (Countering.controller c)
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 615.13: a prevention effect was applied and prevented some damage, and the
  -- damage it prevented was addressed to a player the relation admits. CR 109.5 /
  -- 603.3a fix "you" as the ability's controller, exactly as PlayerDiscards and
  -- SpellOrAbilityCounters do.
  --
  -- The bearer is NOT part of the match: Selfless Squire is a creature watching
  -- damage addressed to its controller, and CR 615.13 says nothing about which
  -- object the ability is on.
  --
  -- ONE fire per recorded event, and the record is already grouped per prevention
  -- effect per batch (Replacement.groupPreventions), which is where CR 615.13's
  -- "one or more simultaneous damage events" is honoured. Nothing here has to
  -- count.
  --
  -- Damage prevented to a PERMANENT is silence rather than a miss: the printed
  -- sentence says "to you", and the recipient the event carries is what
  -- distinguishes the two.
  TriggerCondition.DamageToPlayerPrevented relation -> case event of
    GameEvent.DamagePrevented (DamagePrevented.MkDamagePrevented _ recipient _) -> case recipient of
      Recipient.ToPlayer pid -> PlayerRelation.holds relation you pid
      Recipient.ToCreature _ -> False
      Recipient.ToPlaneswalker _ -> False
      Recipient.ToBattle _ -> False
      Recipient.ToObject _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 615.13's other reading: the damage was prevented THIS WAY -- by a
  -- prevention effect the BEARER's own card prints (Phyrexian Vindicator).
  -- Replacement.printedBy is that question, and it is the whole of the match:
  -- rule 615.13 says nothing about whom the damage was addressed to, and neither
  -- does the printed sentence.
  --
  -- The identity, never the recipient: the Vindicator's shield covers only
  -- itself, so a recipient test would be a second name for a fact the identity
  -- already settles -- and one prevention effect of another object covering the
  -- SAME recipient is exactly the case this arm has to answer False for.
  TriggerCondition.SelfPreventsDamage -> case event of
    GameEvent.DamagePrevented prevented -> Replacement.printedBy (DamagePrevented.by prevented) == Just bearer
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 119.9: a source caused a player the relation admits to gain life. The
  -- gaining player comes from the event; CR 109.5 / 603.3a fix "you" as the
  -- ability's controller, exactly as PlayerDiscards, SpellOrAbilityCounters and
  -- DamageToPlayerPrevented above do.
  --
  -- The bearer is NOT part of the match: Ajani's Pridemate is a creature watching
  -- its controller's life total, and CR 119.9 says nothing about which object the
  -- ability is on.
  --
  -- No zero check here. CR 119.9's "if a player gains 0 life, no life gain event
  -- has occurred" is enforced where the event is RECORDED -- Resolve's GainLife
  -- arm and Damage's lifelink pass both guard their own zero -- so a
  -- GameEvent.LifeGained in the log is by construction a gain of more than 0, and
  -- a second guard here would be a second place for that invariant to live.
  --
  -- LOSING life is not a near miss but a different event, and the LifeLost arm
  -- below is where that shows: one damage event can record both, and only the
  -- gain fires this.
  TriggerCondition.PlayerGainsLife relation -> case event of
    GameEvent.LifeGained (LifeChange.MkLifeChange pid _) -> PlayerRelation.holds relation you pid
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- A player the relation admits LOST life -- Exquisite Blood's "whenever an
  -- opponent loses life". The losing player comes from the event; CR 109.5 /
  -- 603.3a fix "you" as the ability's controller, exactly as PlayerGainsLife
  -- above does.
  --
  -- The bearer is NOT part of the match: Exquisite Blood is an enchantment
  -- watching somebody else's life total, and nothing about the condition names
  -- the object the ability is on.
  --
  -- Which life-total movements are a loss is settled at the RECORDING sites and
  -- not here, since the rules print no CR 119.9 for this direction: CR 119.3's
  -- instructed loss, CR 119.2 / 120.3a's damage, and CR 119.4's paid life all
  -- write GameEvent.LifeLost, while CR 120.3b's infect diversion, CR 615.6's
  -- prevented damage and damage taken by a permanent write none. See
  -- Pawl.Types.TriggerCondition.PlayerLosesLife.
  --
  -- No zero check either, for the reason the gain arm gives: every producer
  -- guards its own zero, so a GameEvent.LifeLost in the log is by construction a
  -- loss of more than 0.
  --
  -- GAINING life is a different event, not a signed version of this one: one
  -- damage event can record a loss and a lifelink gain together, and only the
  -- loss fires this.
  TriggerCondition.PlayerLosesLife relation -> case event of
    GameEvent.LifeLost (LifeChange.MkLifeChange pid _) -> PlayerRelation.holds relation you pid
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 714.2b: counters of this kind were put onto the BEARER, and the count
  -- crossed N going up. Both halves of the rule's sentence are here -- see
  -- Pawl.Types.TriggerCondition.SelfCountersReached for why the intervening "if"
  -- is not split off into TriggeredAbility.intervening.
  --
  -- `before < n` is what stops a chapter re-firing: History of Benalia going from
  -- two lore counters to three crosses III and nothing else, and a later counter
  -- taking it from three to four crosses none of its chapters again.
  --
  -- `n <= after` rather than `n == after`, because one placement can cross several
  -- thresholds at once (CR 714.2b says "at least N", not "exactly N"): a Saga
  -- given two lore counters while it has none fires chapters I and II together.
  -- Read ahead (CR 702.155a) is the mechanic that wants the equality instead, and
  -- then only on the turn the Saga entered -- Saga.chapterTriggers, which asks
  -- Saga.readAheadRestricted of the bearer.
  --
  -- The narrowing does NOT leak past Sagas even though this arm is generic over
  -- counter kind: readAheadRestricted gates on Keyword.ReadAhead, which rule
  -- 702.155a puts only on Saga cards, so the extra conjunct is inert for every
  -- other counter this condition watches. A CounterKind.Lore test here would
  -- restate that gate rather than add to it.
  --
  -- Projected LAZILY, and Saga.chapterTriggers asks `after == n` first, so the
  -- whole-board projection is forced only where the two readings could differ.
  TriggerCondition.SelfCountersReached (SelfCountersReached.MkSelfCountersReached wanted n) -> case event of
    GameEvent.CountersPut (CounterChange.MkCounterChange oid kind before after) ->
      oid == bearer && kind == wanted && Saga.chapterTriggers (Saga.readAheadRestricted (Projection.project bearer gs) gs bearer) before after n
    -- Rule 714.2b says "are PUT onto", so a removal crosses nothing: a Saga whose
    -- lore counters were taken off and put back fires its chapters again.
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 716.2a: the BEARER's class level crossed N going up. "When this Class
  -- becomes level N" is printed inside the level-N section, so CR 716.2a's static
  -- half is what grants it, and CR 603.10 is what lets it see its own arrival:
  -- "becomes level N" is not on that rule's exhaustive look-back list, so the
  -- abilities checked are the ones existing immediately AFTER the level changed.
  --
  -- `before < n` is what stops a later bar re-firing an earlier section's
  -- trigger: a Class going from level 2 to level 3 crosses 3 and nothing else.
  --
  -- `n <= after` rather than `n == after`, SelfCountersReached's reading of the
  -- same shape: one write can cross several thresholds, so a Class set straight
  -- from level 1 to level 3 fires both sections' triggers. No level bar can do
  -- that (CR 716.2a's "only if this Class is level N-1"), which is why the two
  -- readings coincide for every printing in `data/cards/` -- Effect.SetClassLevel
  -- is an opcode, not a bar, and this is the reading the rule states.
  --
  -- Saga.crossed says the same sentence for lore counters and is deliberately not
  -- reused: it is over Naturals, and its haddock ties it to CR 714.2b and to the
  -- agreement CR 704.5s's state-based action needs with it. A class level is a
  -- ClassLevel and has no state-based action to agree with.
  TriggerCondition.SelfBecomesClassLevel n -> case event of
    GameEvent.ClassLevelSet (ClassLevelChange.MkClassLevelChange oid before after) -> oid == bearer && before < n && n <= after
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 310.12b: the LAST counter of this kind came off the BEARER. The mirror of
  -- the arm above, and narrower in the way rule 310.12b is narrower than rule
  -- 714.2b: there is no threshold to cross, only a count that reached none.
  --
  -- `after == 0` alone, with no `before > 0` conjunct: GameEvent.CountersRemoved is
  -- recorded only where something actually came off, so an event that removed
  -- nothing is not in the log to match. That invariant is the record's, stated on
  -- the constructor, exactly as CountersPut's "before < after" is.
  TriggerCondition.SelfLastCounterRemoved wanted -> maybe False ((==) 0 . snd) (countersRemovedFrom bearer wanted event)
  -- "Whenever one or more [kind] counters are removed from this permanent"
  -- (Chandra, Fire Artisan): the arm above's any-amount mirror, dropping every
  -- read of the AFTER count. Three of four loyalty counters coming off matches
  -- here and not there, which is what keeps the two from collapsing.
  --
  -- No "one or more" conjunct either, and for the arm above's reason: the record
  -- exists only where something came off.
  TriggerCondition.SelfCountersRemoved wanted -> Maybe.isJust (countersRemovedFrom bearer wanted event)
  -- CR 601.2i's "any abilities that trigger when a spell is cast": a spell the
  -- Filter admits became cast. The bearer frames the match rather than being it,
  -- as for PermanentEnters -- it is the Filter.Context's source, and its
  -- controller is the perspective CR 109.5 gives "you" in "whenever YOU CAST an
  -- instant or sorcery spell".
  --
  -- The CASTER comes from the event and is handed to the view as the spell's
  -- controller, which is what makes Filter.ControlledBy You answer the printed
  -- "you". Not read off the object: Event.changeZone stamps Object.enteredUnder
  -- only for a battlefield entry, so a stack object falls back to its OWNER
  -- (Projection.defaultControllerOf) -- the same player for every cast in the
  -- pool today, and the wrong one the moment a card lets somebody cast a card
  -- they do not own. CR 601.2a settles it the other way: the player casting the
  -- spell is its controller.
  --
  -- The spell is read LIVE off the stack rather than from a snapshot, which is
  -- what separates this arm from PermanentDies'. CR 601.2i's trigger event is
  -- the spell BECOMING cast, and CR 601.2a leaves it on the stack "until it
  -- resolves, it's countered, or a rule or effect moves it elsewhere" -- none of
  -- which can have happened before the scan, since the cast is the last thing
  -- Cast.castSpell does. So no CR 608.2h fallback is reachable, and the `Nothing`
  -- below is the id naming nothing at all, about which no Filter can honestly
  -- answer.
  --
  -- The TurnScope is the second half, and it comes from the GAME STATE rather
  -- than from the event: GameEvent.SpellCast records the caster and the spell,
  -- never the turn, and CR 601.2i's trigger is checked in the same settle the
  -- cast happened in -- so the active player standing now is the one the cast
  -- happened under. Read against `you`, CR 109.5's controller of the ability (CR
  -- 603.3a), exactly as the StepBegins arm above reads its own.
  TriggerCondition.SpellCast (SpellCast.MkSpellCast f scope fromZone ordinal) -> case event of
    GameEvent.SpellCast (SpellWasCast.MkSpellWasCast caster spell _ castFrom) -> case Game.lookupObject spell gs of
      Nothing -> False
      Just _ ->
        turnScopeAdmits scope (GameState.activePlayer gs) you
          -- CR 601.2a's zone, read off the EVENT and not off the spell: rule
          -- 400.7 left the stack incarnation with no memory of it. A condition
          -- that names no zone admits every cast, which is what almost every
          -- printing writes.
          && maybe True (\z -> castFrom == Just z) fromZone
          && Filter.matches (Filter.contextFor (Just you) (Just bearer)) (Projection.viewOfSpell caster spell gs) f
          -- Clarion Spirit's "your SECOND spell each turn", asked LAST so the
          -- log walk happens only for a cast the rest of the condition already
          -- admits.
          && maybe True (castOrdinal (Filter.contextFor (Just you) (Just bearer)) f fromZone spell gs ==) ordinal
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 601.2i, self-scoped: the spell that became cast IS the bearer. A bare
  -- comparison of ids and no Filter at all, which is what separates this arm
  -- from SpellCast's above -- nothing about the spell is read, so no projection
  -- can come up empty and no CR 608.2h fallback is reachable.
  --
  -- No TurnScope either, so no turnScopeAdmits: CR 601.2i says nothing about
  -- whose turn it is, and no printing narrows its own cast by one.
  --
  -- The bearer is the STACK object, which is the same id GameEvent.SpellCast
  -- carries: CR 601.2a puts the card on the stack as it is cast and leaves it
  -- there, so eventTriggers' `spellCast` source offers exactly that incarnation.
  TriggerCondition.SelfCast -> case event of
    GameEvent.SpellCast (SpellWasCast.MkSpellWasCast _ spell _ _) -> spell == bearer
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 601.2c, self-scoped: the object that became a target IS the bearer, a bare
  -- comparison of ids in SelfCast's shape and for its reason -- nothing about the
  -- targeting spell is read, so no projection can come up empty.
  --
  -- The RELATION is read off the event's own controller field rather than off the
  -- board, which is what CR 405.4 asks for: the answer wanted is who controlled
  -- the targeting object as it was announced, and by the time a ward trigger is
  -- gathered that object may already have changed hands or left.
  --
  -- Rule 702.21a's own relation is Opponent; You is admitted because the
  -- condition is stated over a PlayerRelation, and a card printing the other half
  -- would read the same field.
  TriggerCondition.SelfBecomesTargeted relation -> case event of
    GameEvent.BecameTarget t ->
      Recipient.objectOf (BecameTarget.targeted t) == Just bearer
        && PlayerRelation.holds relation you (BecameTarget.controller t)
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.SpellCast {} -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
  -- CR 601.2c from the PLAYER's side, the sibling above one recipient over:
  -- Dormant Gomazoa's "whenever you become the target of a spell" and Amulet of
  -- Safekeeping's "whenever you become the target of a spell or ability an
  -- opponent controls". The recipient is compared with CR 109.5's "you" -- CR
  -- 603.3a's controller of the bearer as the ability triggered -- rather than
  -- with the bearer, which is the whole of what separates this from ward.
  --
  -- The KIND conjunct is the arm's rules content, and it is a Maybe: CR 112.1
  -- makes a spell a card on the stack, while CR 602.2b and CR 603.3d route an
  -- activated and a triggered ability through the same rule 601.2c. Gomazoa names
  -- Spell and Ravenous Rats' targeted discard must not untap it; Amulet names
  -- neither limb and takes both.
  --
  -- The RELATION is ward's, read off the same BecameTarget.controller field --
  -- CR 405.4's controller of the targeting object, which is who Amulet offers the
  -- {1} to. Gomazoa's AnyPlayer fires on its own spells as readily as on an
  -- opponent's.
  TriggerCondition.ControllerBecomesTarget c -> case event of
    GameEvent.BecameTarget t ->
      Recipient.playerOf (BecameTarget.targeted t) == Just you
        && maybe True (== BecameTarget.kind t) (ControllerBecomesTarget.kind c)
        && PlayerRelation.holds (ControllerBecomesTarget.relation c) you (BecameTarget.controller t)
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.SpellCast {} -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
  -- CR 709.5h: the bearer is the permanent that was given the designation, and
  -- the door named is the one it was given for. A bare comparison of an id and a
  -- name, in SelfEnters' shape and for its reason -- nothing about the entrant's
  -- characteristics is read, so there is no CR 608.2h fallback to reach for.
  --
  -- The half is checked as well as the bearer, which is the arm's whole content:
  -- CR 709.5h fires "when a player unlocks a PARTICULAR half", so a Room whose
  -- other door was the one that opened must not fire this ability.
  TriggerCondition.SelfHalfUnlocked half -> case event of
    GameEvent.HalfUnlocked (HalfUnlocked.MkHalfUnlocked oid name _) -> oid == bearer && name == half
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 708.7 through CR 603.2: the bearer is the permanent that turned over.
  -- SelfEnters' shape -- a bare comparison of ids, with nothing about the
  -- permanent's characteristics read, so no CR 608.2h fallback is reachable.
  --
  -- The BEARER check is what keeps one player's face-up Skirk Marauder from
  -- firing off a different permanent turning over; Pawl.FaceDownSpec seats a
  -- second face-down permanent on the same board to prove it.
  --
  -- Nothing here asks whether the permanent had ALREADY entered the battlefield.
  -- CR 708.7 leaves turning face up something only a permanent can be doing, and
  -- FaceDown.performTurnFaceUp is the sole writer of this event -- CR 708.3's
  -- face-down ENTRY writes a Moved event and never this one, which is what makes
  -- CR 708.8's last sentence fall out rather than needing a clause.
  TriggerCondition.SelfTurnedFaceUp -> case event of
    GameEvent.TurnedFaceUp oid -> oid == bearer
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 701.27e through CR 603.2: the bearer is the permanent that transformed,
  -- and the names the event carries are what it turned INTO. SelfTurnedFaceUp
  -- above is the neighbouring shape, plus the name -- and CR 701.27b is why
  -- they are two conditions rather than one, turning over and turning face up
  -- being different game actions.
  --
  -- Both halves are read off the EVENT, neither off the board. The board at the
  -- CR 117.5 scan is not the board CR 701.27e asks about: a permanent that
  -- transformed twice before the scan shows only its last face, which would
  -- answer this arm wrong on both events. Pawl.Types.Transformed carries the
  -- sample for that reason.
  --
  -- Set.member rather than equality because CR 709.4a admits several names and
  -- CR 708.2a admits none; a permanent with no name matches nothing, which is
  -- the rule's own answer rather than a guard.
  TriggerCondition.SelfTransformedInto name -> case event of
    GameEvent.Transformed (Transformed.MkTransformed oid names) -> oid == bearer && Set.member name names
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 708.7's OTHER written form, read by a bystander: "whenever a permanent is
  -- turned face up". PermanentEnters' shape against SelfEnters', and for its
  -- reason -- the bearer frames the match rather than being it. The bearer is the
  -- Filter.Context's source (so `Not IsSource` would be an "another"), and its
  -- controller is the perspective CR 109.5 gives "you" in Deathmist Raptor's "a
  -- permanent YOU CONTROL is turned face up".
  --
  -- NO comparison against the bearer at all, which is the whole difference from
  -- the arm above: Aven Farseer bears no morph ability and so can never be the
  -- permanent that turned over, and a bearer check here would make its ability
  -- dead text. Pawl.FaceDownSpec's "a permanent turning face up puts Aven
  -- Farseer's counter on the FARSEER" is what proves the scope, asserting the
  -- counter's landing place by object id on a board where the watcher and the
  -- subject are two permanents.
  --
  -- Read LIVE off the game as it stands, like PermanentEnters and unlike
  -- PermanentDies: CR 708.8 leaves the permanent on the battlefield with its
  -- normal copiable values restored, so there is nothing for CR 603.10a's
  -- look-back to recover and the live read is what CR 603.10's first sentence
  -- asks for. It is also the only read that can answer a narrowed form correctly
  -- -- CR 708.2 gives a face-down permanent only the characteristics its listing
  -- names and never the card's, so a Filter applied to the pre-turning object
  -- would decline every "a Dragon is turned face up" there is.
  --
  -- viewWithLastKnown rather than viewOfObject for PermanentEnters' reason: a
  -- permanent turned face up and gone again before the CR 117.5 boundary is still
  -- read as it was on the battlefield (CR 608.2h) instead of vanishing from the
  -- match. Nothing is a permanent that is gone AND filed no last known
  -- information, about which no Filter can honestly answer.
  TriggerCondition.PermanentTurnedFaceUp f -> case event of
    GameEvent.TurnedFaceUp oid -> case Projection.viewWithLastKnown oid gs oid of
      Nothing -> False
      Just view -> Filter.matches (Filter.contextFor (Just you) (Just bearer)) view f
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 702.112b: a permanent the Filter admits was given the renowned designation.
  -- PermanentTurnedFaceUp's arm above, line for line, and for its reasons: the
  -- permanent is read as it stands (viewWithLastKnown for CR 608.2h, a designation
  -- being no zone change), and the bearer contributes only CR 109.5's perspective
  -- and the Filter.Context's source -- which is what makes Filter.IsSource the
  -- self-scoped reading.
  -- CR 702.100b: the BEARER evolved. SelfEnters' arm -- a bare id comparison, no
  -- view and no Filter -- which is what makes it answerable for a creature that
  -- has since left: the marker is about an event, not about the object now.
  TriggerCondition.SelfEvolves -> case event of
    GameEvent.Evolved oid -> oid == bearer
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 702.134c: the creature the BEARER IS ATTACHED TO mentored another. The
  -- event's first id is rule 702.134c's "first creature", the mentor, and this arm
  -- asks only whether that is the bearer's host -- the pairing with the second
  -- creature is the event's, decided where the mentor ability resolved.
  --
  -- CR 301.5a's "equipped creature" read off Object.attachedTo, which is where the
  -- Equipment records it, through Recipient.objectOf for CR 303.4's other
  -- destination: an Equipment attached to nothing, or an Aura enchanting a player,
  -- has no host id to compare and answers False.
  --
  -- A LIVE read, PermanentTurnedFaceUp's posture: nothing here is a zone change, so
  -- CR 603.10a's look-back does not reach this condition, and the attachment as it
  -- stands when the trigger is gathered is CR 603.2's own reading of "equipped
  -- creature".
  TriggerCondition.AttachedCreatureMentors -> case event of
    GameEvent.Mentored (Mentored.MkMentored mentor _) ->
      (Recipient.objectOf =<< Object.attachedTo =<< Game.lookupObject bearer gs) == Just mentor
    GameEvent.Evolved _ -> False
    GameEvent.Trained _ -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 702.149c: the BEARER trained. SelfEvolves' arm above, line for line and for
  -- its reasons -- a bare id comparison, no view and no Filter, so a creature that
  -- has since left the battlefield is still answered about the event.
  TriggerCondition.SelfTrains -> case event of
    GameEvent.Trained oid -> oid == bearer
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    -- The event the RULE distinguishes this condition from: +1/+1 counters arriving
    -- say nothing about what put them, which is why rule 702.149c needs a marker at
    -- all.
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    -- Nor the declaration rule 702.149a's own trigger reads: training FIRES on an
    -- attack and this condition fires on that ability resolving, so an ability
    -- removed before it resolves (CR 608.2b) trains nothing.
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  TriggerCondition.PermanentBecomesDesignated (PermanentBecomesDesignated.MkPermanentBecomesDesignated wanted f) -> case event of
    -- The designations must MATCH, not merely both be present: Valeron Wardens'
    -- renown trigger must not fire when a creature you control becomes monstrous.
    GameEvent.BecameDesignated (BecameDesignated.MkBecameDesignated got oid)
      | got /= wanted -> False
      | otherwise -> case Projection.viewWithLastKnown oid gs oid of
          Nothing -> False
          Just view -> Filter.matches (Filter.contextFor (Just you) (Just bearer)) view f
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 709.5i: "such an ability triggers when that permanent has one of the two
  -- unlocked designations and gets the other, or when it has neither designation
  -- and gains both." The whole of that sentence is the flag the event carries,
  -- decided where the designation was written; this arm reads it and asks who
  -- controls the permanent.
  --
  -- NOT scoped to the bearer, which is where this parts company with
  -- SelfHalfUnlocked above: Balemurk Leech is a creature watching every Room on
  -- the board, so the bearer contributes only CR 109.5's perspective through
  -- `you`.
  --
  -- The controller is read off the LIVE board rather than off the event, since
  -- the event carries none. Projection.controllerOf falls back to the owner for a
  -- permanent that has since left, which is CR 109.5's own answer for an object
  -- outside the battlefield; nothing in the pool can move a Room between the
  -- designation and the CR 117.5 boundary that scans for this.
  --
  -- Whose ACTION opened the door is a different question, and not this one (#961).
  TriggerCondition.RoomFullyUnlocked relation -> case event of
    GameEvent.HalfUnlocked (HalfUnlocked.MkHalfUnlocked oid _ fully) ->
      fully && case Projection.controllerOf oid gs of
        Nothing -> False
        Just controller -> PlayerRelation.holds relation you controller
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 603.10a's sacrifice family: the log entry Event.sacrifice writes is the
  -- whole of the answer, which is exactly what makes the condition worth having.
  -- CR 700.4 makes every sacrifice a death, so the Moved event the same sacrifice
  -- records is indistinguishable from a destruction's or a mill's -- an arm that
  -- read the zone change instead would fire on both.
  --
  -- Nothing is compared. CR 701.21a's "a player" is every player, its own
  -- controller included, and "a permanent" names no quality, so neither the
  -- bearer nor `you` is consulted and the event's payload goes unread.
  TriggerCondition.PermanentSacrificed -> case event of
    GameEvent.PermanentSacrificed {} -> True
    GameEvent.AbilityTriggered {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.CountersPut {} -> False
    -- CR 700.4 again, from this side: a sacrifice DOES record a Moved event, and
    -- matching it here would answer twice for one sacrifice.
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 603.1b: "a triggered ability may have more than one trigger condition".
  -- `any`, because the printed sentence joins them with "and whenever": each
  -- clause is its own occasion for the ability to trigger. Two clauses matching
  -- the SAME event fire the ability once here, which is also the right answer --
  -- CR 603.2 matches an event against "a triggered ability's trigger event", and
  -- the ability is one ability.
  TriggerCondition.AnyOf conditions -> any (\c -> matchesTriggerGiven bindings gs bearer you c event) conditions
  -- CR 603.3b's second class, the one condition in this type whose event is
  -- another ability triggering: "whenever the final chapter ability of a Saga you
  -- control triggers".
  --
  -- The event carries the triggered ability's SOURCE, its CONTROLLER as it
  -- triggered (CR 603.3a) and its trigger CONDITION, and all three are read:
  --
  --   * the condition must be a chapter ability's (CR 714.2b, through
  --     Saga.chapterOfCondition, so this and the SelfCountersReached arm above
  --     cannot drift about what a chapter symbol is);
  --   * its chapter must be the source's FINAL chapter number (CR 714.2d), which
  --     is why the source's projection is read rather than the event alone;
  --   * the source must be a Saga with one or more chapter abilities
  --     (Saga.tracksLore, CR 714.1 / 704.5s's own phrase). CR 714.2d gives a
  --     permanent with no chapter abilities a final chapter number of 0, so
  --     without that conjunct a "{r0}" chapter ability on any permanent at all
  --     would match -- no card prints one, and the gate is still the one the
  --     rule states.
  --
  -- NOT self-scoped: Historian's Boon is an enchantment watching somebody else's
  -- permanent, so the bearer contributes only CR 109.5's perspective through
  -- `you`, which the PlayerRelation reads the event's controller against.
  --
  -- The source is read LIVE (Projection.project), which needs no CR 608.2h
  -- fallback and is not a shortcut: CR 704.5s's exemption keeps a Saga on the
  -- battlefield for exactly as long as a chapter ability of its own has triggered
  -- and not yet left the stack, so the Saga whose final chapter fired this event
  -- is still standing at the CR 117.5 boundary that scans for it. A Saga a
  -- replacement or another player's effect took away in the same batch projects
  -- as an object with no subtypes, which Saga.tracksLore declines -- the same
  -- silence CR 603.10 would give a look-back that found nothing (#1028).
  TriggerCondition.SagaFinalChapterTriggers relation -> case event of
    GameEvent.AbilityTriggered (AbilityTriggered.MkAbilityTriggered srcId controller fired) ->
      PlayerRelation.holds relation you controller
        && ( let pc = Projection.project srcId gs
              in Saga.tracksLore pc && Saga.chapterOfCondition fired == Just (Saga.finalChapterOf pc)
           )
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 603.7: "when you lose control of the creature" -- Ray of Command's third
  -- sentence. The slot is read out of CR 603.7c's captured environment, so the
  -- creature this asks about is the one the spell targeted rather than whatever
  -- creature happens to change hands.
  --
  -- Binding.objectSlots and not Binding.objectsOf: the printed sentence is about
  -- ONE creature, and a slot holding a group or several targets names none of them
  -- here rather than any one of them.
  --
  -- `before == you` and nothing about `after`, because losing control is the whole
  -- of what the sentence asks. Engine.sampleControl only mints the event when the
  -- two players differ, so no separate "and somebody else has it now" conjunct is
  -- needed for the match to mean a change.
  --
  -- The `== Just oid` conjunct is a REGRESSION FENCE rather than a proven
  -- behaviour: no board in the pool can tell it from `Map.member slot`. The only
  -- control changes a Ray of Command board sees are the reversions of the creatures
  -- its controller stole, all in one CR 514.2 sweep, and the ability taps its slot
  -- whichever of them matched -- CR 603.7b spending the one shot either way. A card
  -- whose controller can lose control of a DIFFERENT permanent while this entry is
  -- armed is what would observe it.
  TriggerCondition.LoseControlOfBound slot -> case event of
    GameEvent.ControlChanged (ControlChanged.MkControlChanged oid before _) ->
      Map.lookup slot (Binding.objectSlots bindings) == Just oid && before == you
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 309.4c: "when you move your venture marker into THIS room", so the
  -- dungeon card the marker is on must be the bearer and the room must be this
  -- ability's own. Not reached from here: eventTriggers' command-zone source is
  -- CR 114.4's and takes emblems alone, so a dungeon card is never offered and
  -- Pawl.Engine.Dungeon.roomPending is what gathers a room ability. A regression
  -- fence, written to agree with that gatherer rather than to differ. The two
  -- cannot disagree on any dungeon card: CR 309.4c gives every room ability the
  -- same trigger condition, which the rulebook supplies and the card does not
  -- print, so only the effect varies and neither collector reads it.
  TriggerCondition.RoomEntered room -> case event of
    GameEvent.VentureMarkerEntered (VentureMarkerEntered.MkVentureMarkerEntered _ oid entered) -> oid == bearer && entered == room
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 701.22d: this player scried. The relation reads the scryer against CR
  -- 109.5's "you", the ability's controller (CR 603.3a) --
  -- PlayerBecomesMonarch's shape, and Matoya, Archon Elder is the You form.
  --
  -- The EVENT and nothing else: how many cards moved, and whether any could,
  -- is CR 701.22a's business and CR 701.22d says explicitly that neither
  -- narrows this. Pawl.Engine.Resolve.scryOne records the event outside its
  -- own prompt guard for that sentence.
  TriggerCondition.PlayerScries relation -> case event of
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried scryer -> PlayerRelation.holds relation you scryer
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 701.25d, the arm above's twin and Matoya, Archon Elder's other branch.
  -- A surveil that put nothing into a graveyard matches, which is what a
  -- condition built on CR 701.25a's zone changes could not do.
  TriggerCondition.PlayerSurveils relation -> case event of
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled surveiller -> PlayerRelation.holds relation you surveiller
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 706.1: this player rolled a die, the relation reading the roller against
  -- CR 109.5's "you" as PlayerScries above does. Feywild Trickster is the You
  -- form.
  --
  -- The EVENT alone. What the die SHOWED is deliberately not a bar here: CR
  -- 706.7 has the planar die firing this very condition while every effect
  -- reading a numerical result ignores it, so a condition gated on the result
  -- would be the wrong shape rather than a stricter one (#934).
  --
  -- Not implemented: CR 706.6's ignored roll, which "is considered to have never
  -- happened" and triggers nothing -- nothing in data/cards ignores or rerolls a
  -- roll (#2083), so no recorded event is one this must skip.
  TriggerCondition.PlayerRollsDice relation -> case event of
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled roller -> PlayerRelation.holds relation you roller
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 702.170a / 702.170c: the bearer's own card became plotted. Self-scoped, so the
  -- match is the id and nothing else -- and the id the event carries is the
  -- CR 400.7 incarnation in exile, which is the bearer here because
  -- Event.eventTriggers finds this ability through its exile scan rather than
  -- on the battlefield (zonesTriggeredFrom below).
  TriggerCondition.SelfBecomesPlotted -> case event of
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted plotted -> plotted == bearer
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 701.44b: a permanent the Filter admits completed an explore.
  -- Wildgrowth Walker's "a creature you control" describes the EXPLORER, so
  -- the bearer only frames the match: it is the Filter.Context's source and
  -- its controller is CR 109.5's "you".
  --
  -- viewWithLastKnown aimed at the explorer twice over, PermanentDies' posture
  -- and CR 701.44c's instruction in as many words: a permanent that explored
  -- and has since left is read as it last was, so "you control" answers with
  -- the player who controlled it rather than CR 108.4a's owner substitute.
  --
  -- Nothing is an explorer that is gone AND filed no last known information,
  -- about which no Filter can honestly answer.
  TriggerCondition.PermanentExplores f -> case event of
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored explorer -> case Projection.viewWithLastKnown explorer gs explorer of
      Nothing -> False
      Just view -> Filter.matches (Filter.contextFor (Just you) (Just bearer)) view f
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 701.43d / 607.2h: the BEARER was exerted. SelfEvolves' arm above, line
  -- for line: CR 701.43a records the event only for the permanent actually
  -- exerted, so WHOSE exert it was is the whole question, and CR 607.2h's
  -- linkage needs nothing more because Pawl.Engine.Combat.declareAttackers
  -- records the event only where the static ability offered the cost.
  TriggerCondition.SelfExerted -> case event of
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted oid -> oid == bearer
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
  -- CR 701.3a read from the HOST: something became attached to the BEARER, and
  -- the Filter narrows WHAT. Two questions, and the split is the condition's
  -- shape -- a bare id comparison for the host (SelfEnters' arm) and a Filter
  -- read for the attachment (PermanentTurnedFaceUp's arm), which is why the
  -- constructor carries one payload and matches on two objects.
  --
  -- Recipient.objectOf and not equality on the whole Recipient: CR 701.3a's
  -- destination is tagged by the attaching permanent's own rules text
  -- (Pawl.Engine.Attach.attachmentFor), so an Aura arrives as a ToCreature and
  -- an Equipment as a ToCreature while nothing promises the bearer would be
  -- named the same way twice. A ToPlayer host has no object and declines here,
  -- which is right: the bearer of this condition is a permanent.
  --
  -- viewWithLastKnown for PermanentTurnedFaceUp's reason -- an attachment that
  -- is gone by the CR 117.5 boundary is still read as it was (CR 608.2h)
  -- instead of vanishing from the match.
  TriggerCondition.SelfBecomesAttachedBy f -> case event of
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached a ->
      Recipient.objectOf (BecameAttached.host a) == Just bearer
        && ( case Projection.viewWithLastKnown (BecameAttached.attachment a) gs (BecameAttached.attachment a) of
               Nothing -> False
               Just view -> Filter.matches (Filter.contextFor (Just you) (Just bearer)) view f
           )
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False

-- CR 603.3b: is this trigger condition "another ability triggering"? The
-- classification the rule's two-part placement turns on -- False puts a trigger
-- in the FIRST pass and True in the second, which is where
-- Pawl.Engine.Engine.placePendingTriggers reads it.
--
-- Exhaustive with no wildcard, for eventBindingSlots' reason and more sharply: a
-- wildcard would silently give every future condition the first pass, and a
-- condition in the wrong pass is a resolution order the player can see.
--
-- `any` for CR 603.1b's multi-condition ability, which is the conservative
-- reading rather than the exact one: rule 603.3b asks about the trigger
-- condition that actually fired, and a PendingTrigger does not record WHICH of an
-- AnyOf's clauses matched (#1027). No printing mixes the two classes in one
-- ability -- Historian's Boon writes its two sentences as two abilities.
reactsToAbilityTriggering :: TriggerCondition -> Bool
reactsToAbilityTriggering cond = case cond of
  -- The one condition of this class in the pool, and the reason the rule's second
  -- pass exists at all.
  TriggerCondition.SagaFinalChapterTriggers _ -> True
  TriggerCondition.AnyOf conditions -> any reactsToAbilityTriggering conditions
  -- CR 603.7's own event is a control change, not an ability triggering, so it
  -- belongs to CR 603.3b's first pass.
  TriggerCondition.LoseControlOfBound _ -> False
  -- CR 309.4c's event is a venture marker moving, not an ability triggering.
  TriggerCondition.RoomEntered _ -> False
  -- Four keyword actions, none of them an ability triggering: CR 701.22a,
  -- CR 701.25a, CR 702.170b and CR 701.44a each describe something a player
  -- or a permanent DOES, so all four take CR 603.3b's first pass.
  TriggerCondition.PlayerScries _ -> False
  TriggerCondition.PlayerSurveils _ -> False
  TriggerCondition.SelfBecomesPlotted -> False
  TriggerCondition.PermanentExplores _ -> False
  -- CR 706.1's roll is something a resolving effect INSTRUCTS a player to do,
  -- never an ability triggering, so it takes CR 603.3b's first pass as well.
  TriggerCondition.PlayerRollsDice _ -> False
  -- The same answer for the same reason: CR 701.43a's exert is a keyword action a
  -- PLAYER takes, and CR 508.1g puts it in a turn-based action rather than in a
  -- resolving ability.
  TriggerCondition.SelfExerted -> False
  -- And CR 701.3a's attach is a keyword ACTION too, taken by a resolving effect
  -- or by a permanent entering the battlefield -- never an ability triggering, so
  -- CR 603.3b's first pass again.
  TriggerCondition.SelfBecomesAttachedBy _ -> False
  -- CR 603.12's trigger event is the action the resolving ability instructed --
  -- a payment, for every printing in the pool -- and never another ability
  -- triggering, so a reflexive takes CR 603.3b's first pass.
  TriggerCondition.Reflexive -> False
  -- Everything else names something that happened to the board or to a player,
  -- which is CR 603.3b's first class in as many words.
  TriggerCondition.SelfEnters -> False
  TriggerCondition.PermanentEnters _ -> False
  TriggerCondition.StepBegins {} -> False
  -- CR 603.8: a state trigger's condition is a fact about the game state, and a
  -- game state is not an ability triggering.
  TriggerCondition.StateIs _ -> False
  TriggerCondition.SelfDealsCombatDamageToPlayer -> False
  TriggerCondition.SelfIsDealtDamage -> False
  TriggerCondition.PermanentDealsCombatDamageToPlayer _ -> False
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
  TriggerCondition.OpponentLostLifeDuringYourTurn -> False
  TriggerCondition.SelfCycled -> False
  TriggerCondition.SelfRevealedForMiracle -> False
  TriggerCondition.SelfDiscarded -> False
  TriggerCondition.PlayerDiscards _ -> False
  TriggerCondition.PlayerCycles _ -> False
  -- CR 121.1's draw is something that happens to a player, not an ability
  -- triggering.
  TriggerCondition.PlayerDrawsNthCard {} -> False
  -- CR 725.1's crowning is something that happens TO a player, which is CR
  -- 603.3b's first class in as many words -- a designation changing hands is not
  -- an ability triggering, whatever put the crown there.
  TriggerCondition.PlayerBecomesMonarch _ -> False
  TriggerCondition.SelfAttacks _ -> False
  TriggerCondition.SelfAttacksWithAnother _ -> False
  TriggerCondition.CreatureAttacksAlone _ -> False
  TriggerCondition.CreatureAttacksYou -> False
  TriggerCondition.AttachedPlayerIsAttacked -> False
  TriggerCondition.PlayerAttacks _ -> False
  TriggerCondition.PlayerAttacksWith {} -> False
  TriggerCondition.PlayerAttacksPlayer {} -> False
  TriggerCondition.SelfAttacksPlayerWithMostLife -> False
  TriggerCondition.SelfBlocks -> False
  TriggerCondition.SelfBlocksCreature _ -> False
  TriggerCondition.SelfBlocksAtLeast _ -> False
  TriggerCondition.SelfBlocksOneOrMore _ -> False
  TriggerCondition.SelfBecomesBlocked -> False
  TriggerCondition.SelfBecomesBlockedBy _ -> False
  TriggerCondition.SelfBecomesBlockedByOneOrMore _ -> False
  TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> False
  TriggerCondition.SelfAttacksUnblocked -> False
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> False
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> False
  TriggerCondition.SelfDies -> False
  TriggerCondition.PermanentDies _ -> False
  -- CR 603.3b's first class too, for the arm above's reason: a batch of deaths
  -- is not another ability triggering.
  TriggerCondition.PermanentsDie _ -> False
  TriggerCondition.SelfLeavesTheBattlefield -> False
  TriggerCondition.PermanentLeavesTheBattlefield _ -> False
  -- CR 702.55b watches a death, not another ability triggering.
  TriggerCondition.HauntedCreatureDies -> False
  -- CR 701.6a's countering is a spell or ability DOING something, not one
  -- triggering, so Baral takes the first pass like every other watcher.
  TriggerCondition.SpellOrAbilityCounters _ -> False
  TriggerCondition.DamageToPlayerPrevented _ -> False
  -- CR 603.3b again: a prevention is something that happens to a damage event,
  -- not an ability triggering.
  TriggerCondition.SelfPreventsDamage -> False
  TriggerCondition.PlayerGainsLife _ -> False
  TriggerCondition.PlayerLosesLife _ -> False
  -- CR 714.2b's own condition is a counter placement, which is what makes a
  -- chapter ability itself a FIRST-pass trigger -- the other half of the pair
  -- this whole classification exists to separate.
  TriggerCondition.SelfCountersReached {} -> False
  TriggerCondition.SelfBecomesClassLevel _ -> False
  TriggerCondition.SelfLastCounterRemoved _ -> False
  TriggerCondition.SelfCountersRemoved _ -> False
  TriggerCondition.SpellCast {} -> False
  TriggerCondition.SelfCast -> False
  TriggerCondition.SelfBecomesTargeted _ -> False
  TriggerCondition.ControllerBecomesTarget {} -> False
  TriggerCondition.SelfHalfUnlocked _ -> False
  TriggerCondition.RoomFullyUnlocked _ -> False
  TriggerCondition.SelfTurnedFaceUp -> False
  TriggerCondition.SelfTransformedInto _ -> False
  TriggerCondition.PermanentTurnedFaceUp _ -> False
  TriggerCondition.PermanentBecomesDesignated {} -> False
  TriggerCondition.SelfEvolves -> False
  -- CR 702.134c watches a mentor ability RESOLVING, which is neither of CR 603.3b's
  -- two classes' subjects read carelessly: an ability resolving is something the
  -- rules did, but the rule's second pass is for a condition that IS another
  -- ability TRIGGERING, and a resolution is a first-pass event like any other. A
  -- mentor ability's own trigger has long since been placed by the time this fires.
  TriggerCondition.AttachedCreatureMentors -> False
  -- Rule 702.149c watches a training ability RESOLVING, which is the same
  -- first-pass event the arm above argues rule 702.134c's is.
  TriggerCondition.SelfTrains -> False
  -- CR 700.4's zone change is a first-pass event too, and not an ability
  -- triggering.
  TriggerCondition.AttachedCreatureDies -> False
  -- CR 701.26a's tap is a first-pass event as well, and not an ability
  -- triggering.
  TriggerCondition.AttachedCreatureBecomesTapped -> False
  TriggerCondition.PermanentSacrificed -> False

-- CR 603.2: the bindings the EVENT contributes to a trigger it has just fired --
-- the environment in which the ability's "that player" / "that creature" is read.
-- Called only for a pair `matchesTrigger` already accepted, so an arm may assume
-- its condition's shape matched; a mismatched pair contributes nothing.
--
-- Separate from `matchesTrigger` rather than folded into a `Maybe bindings`
-- return, the two having different customers: a DELAYED ability matches several
-- events at once and carries the environment captured when it was armed (CR
-- 603.7c). The parallel for a sourceless inherent ability is
-- Monarch.inherentMatch, which has no bearer to scope a shared matcher to.
--
-- THE FIRST ARGUMENT IS NOT READ OFF THE EVENT, and is the one datum here that
-- is not: CR 400.7f's "the new object that each Aura enchanting that permanent
-- became in its owner's graveyard" is a fact about the BEARER's own zone change,
-- which the event that fired the trigger says nothing about. `eventTriggers`
-- computes it off the same CR 117.5 batch and hands it in; `delayedPending` has
-- no bearer departure to scan for and hands in Nothing. Kept here rather than
-- unioned in at the call site so that eventBindingSlots below stays the single
-- statement of which slots a condition makes available, which is what the card
-- lint reads.
eventBindings :: Maybe ObjectId -> TriggerCondition -> GameEvent -> Map.Map SlotName.SlotName Binding
eventBindings bearerBecame cond event = case (cond, event) of
  -- CR 603.2b's "that player": the active player, on whose turn the step began.
  -- Shizuko, Caller of Autumn's "at the beginning of each player's upkeep, THAT
  -- PLAYER adds {G}{G}{G}" is the reader, and the seat it names is nobody the
  -- ability already has -- CR 109.5's "you" is Shizuko's controller, and CR
  -- 603.2b's step is each player's in turn.
  --
  -- Bound for EVERY TurnScope, not only EachTurn. Under ControllersTurn the
  -- active player IS the controller, which makes the slot a redundant second name
  -- rather than a wrong one -- the posture the PlayerBecomesMonarch arm below
  -- takes for its own You case, and what eventBindingSlots' unconditional promise
  -- for this condition needs.
  --
  -- Unconditional given a match: every GameEvent.StepBegan carries a PlayerId,
  -- CR 500.1 giving every step exactly one turn to belong to.
  (TriggerCondition.StepBegins {}, GameEvent.StepBegan ev) ->
    Binding.setTriggerPlayer (StepBegan.player ev) Map.empty
  -- CR 702.70a's "that player": the player the bearer dealt combat damage to.
  --
  -- HOW MUCH, alongside it: Questing Beast's "it deals THAT MUCH damage to target
  -- planeswalker that player controls" counts the damage the event carried, under
  -- the same reserved slot CR 615.13's prevention and CR 119.9's life gain stamp
  -- (see Binding.eventAmount) and the same one the bystander arm below stamps.
  --
  -- Both stamped on the ToPlayer branch alone. The other four recipients are
  -- events this condition does not admit -- matchesTrigger requires
  -- isPlayerRecipient -- so claiming a slot there would name a match that never
  -- happened. Given a match both are unconditional, which is what
  -- eventBindingSlots' per-condition promise needs: every GameEvent.DamageDealt
  -- carries a DamageEvent.amount.
  (TriggerCondition.SelfDealsCombatDamageToPlayer, GameEvent.DamageDealt ev) ->
    case DamageEvent.target ev of
      Recipient.ToPlayer pid -> Binding.setTriggerPlayer pid (Binding.setEventAmount (DamageEvent.amount ev) Map.empty)
      Recipient.ToCreature _ -> Map.empty
      Recipient.ToPlaneswalker _ -> Map.empty
      Recipient.ToBattle _ -> Map.empty
      Recipient.ToObject _ -> Map.empty
  -- CR 603.2's "that much": how many counters actually came off, read off the
  -- event's own before/after pair. Chandra, Fire Artisan's "she deals that much
  -- damage" counts THAT and not the damage that caused it -- CR 306.8's removal
  -- saturates, so five damage to a four-loyalty planeswalker removes four -- and
  -- one CR 510.2 batch is one record, so two attackers taking two counters off
  -- between them stamp 2 once rather than 1 twice.
  --
  -- Unconditional given a match, which is what eventBindingSlots' per-condition
  -- promise needs: every GameEvent.CountersRemoved carries both counts, and the
  -- record exists only where `before` exceeds `after`.
  --
  -- Saturating, and the guard is nominal for that reason; a Natural difference
  -- has no other honest floor.
  (TriggerCondition.SelfCountersRemoved _, GameEvent.CountersRemoved change) ->
    Binding.setEventAmount (Natural.minusSaturating (CounterChange.before change) (CounterChange.after change)) Map.empty
  -- CR 510.2's "it": the permanent that dealt the combat damage, which Aragorn,
  -- Hornburg Hero's payload doubles the counters on. Read off the event's source,
  -- the same field matchesTrigger applied the Filter to, so the slot names exactly
  -- the permanent the condition admitted.
  --
  -- HOW MUCH, alongside it: Shroofus Sproutsire's "create that many 1/1 green
  -- Saproling creature tokens" counts the damage the event carried, under the same
  -- reserved slot CR 615.13's prevention and CR 119.9's life gain stamp (see
  -- Binding.eventAmount). The AMOUNT the event recorded and never the damager's
  -- power: CR 702.19b lets a trampler assign part of its power to a blocker, so
  -- the two come apart on exactly the board Pawl.TriggerSpec's shroofusSpec runs.
  --
  -- The DAMAGED PLAYER beside them, under the same `triggerPlayer` slot the
  -- self-scoped arm above stamps: Larceny's "whenever a creature you control deals
  -- combat damage to a player, THAT PLAYER discards a card" names a seat that is
  -- neither the bearer's controller nor the damager's.
  --
  -- All three unconditional given a match, which is what eventBindingSlots'
  -- per-condition promise needs: every GameEvent.DamageDealt carries a
  -- DamageEvent.source and a DamageEvent.amount, and matchesTrigger has already
  -- required isPlayerRecipient of the target -- so Recipient.playerOf's Nothing is
  -- unreachable for an event this condition admitted.
  (TriggerCondition.PermanentDealsCombatDamageToPlayer _, GameEvent.DamageDealt ev) ->
    maybe id Binding.setTriggerPlayer (Recipient.playerOf (DamageEvent.target ev)) (Binding.setCombatDamager (DamageEvent.source ev) (Binding.setEventAmount (DamageEvent.amount ev) Map.empty))
  -- CR 400.7e: a zone-change trigger can find the new object the card became in
  -- the zone it moved to, if that zone is public. CR 603.6c and CR 603.6e say it
  -- from the other side.
  --
  -- ZoneChange.object, NOT `departed`, which is the whole point of this arm:
  -- `departed` is what matchesTrigger matched the bearer against (CR 603.10a's
  -- look-back) and names an id CR 400.7 has deleted, so an effect handed it would
  -- move nothing. `object` is the card in the graveyard.
  --
  -- Bound ALONGSIDE the source, not instead of it: Engine.placeBorne stamps
  -- Binding.triggerSource over these and must keep stamping the departed id, that
  -- slot being CR 113.7a's source. One printed "it", two objects.
  --
  -- CR 400.7e's public-zone proviso holds by construction here, matchesTrigger's
  -- SelfDies arm having required `to == Graveyard`; the arm below is where it
  -- becomes a real test.
  (TriggerCondition.SelfDies, GameEvent.Moved (Moved.MkMoved zc _)) ->
    Binding.setBecame (ZoneChange.object zc) Map.empty
  -- The same rule and the same field, watched by a BYSTANDER: Promise of
  -- Tomorrow's "whenever a creature you control dies, exile IT". What differs
  -- from the arm above is only which object CR 113.7a's source is -- there the
  -- bearer and the deceased are two incarnations of one card, here the bearer is
  -- a third object entirely (an enchantment) and the source slot cannot reach
  -- the dead creature at all.
  --
  -- ZoneChange.object, NOT `departed`, and here the two really are two different
  -- cards' worth of trap: matchesTrigger's PermanentDies arm matches on
  -- `departed`, because CR 603.10a's look-back is what makes "you control"
  -- answerable off CR 608.2h last known information -- but CR 400.7 deleted that
  -- id, so a payload handed it would move nothing. `object` is the graveyard
  -- card the payload has to act on.
  --
  -- CR 400.7e's public-zone proviso holds BY CONSTRUCTION, needing no guard of
  -- the kind SelfLeavesTheBattlefield below carries: matchesTrigger's
  -- PermanentDies arm has already required the battlefield-to-graveyard pair, and
  -- CR 400.2 lists the graveyard among the public zones. That is what makes
  -- eventBindingSlots' unconditional promise for this condition honest.
  (TriggerCondition.PermanentDies _, GameEvent.Moved (Moved.MkMoved zc _)) ->
    Binding.setBecame (ZoneChange.object zc) Map.empty
  -- The same rule, with its proviso doing real work for the first time: CR 603.6c's
  -- wider condition accepts ANY destination, and CR 400.2 makes two of them hidden.
  --
  -- The binding is ABSENT for a hidden destination rather than present-but-useless:
  -- ZoneChange.object names a real card in that hand, and stamping it would hand
  -- the ability an object the rule forbids it to find. Absence is what CardSpec's
  -- slot lint reads and what Resolve's arms treat as "nothing to act on".
  --
  -- Classified by the ZONE, never by whether the card is currently visible -- CR
  -- 400.2 draws exactly that distinction.
  (TriggerCondition.SelfLeavesTheBattlefield, GameEvent.Moved (Moved.MkMoved zc _))
    | not (Game.isHiddenZone (ZoneChange.to zc)) ->
        Binding.setBecame (ZoneChange.object zc) Map.empty
  -- The bystander reading of that same arm, guarded the same way and for the same
  -- rule. Its OTHER event binds nothing and has no arm: CR 603.6c's
  -- leaving-the-game form reaches no zone at all, so there is no arriving
  -- incarnation for CR 400.7e to rescue.
  (TriggerCondition.PermanentLeavesTheBattlefield _, GameEvent.Moved (Moved.MkMoved zc _))
    | not (Game.isHiddenZone (ZoneChange.to zc)) ->
        Binding.setBecame (ZoneChange.object zc) Map.empty
  -- CR 400.7e again, read in the ENTRY direction: the object that moved is the
  -- entrant, and what it became is the permanent now on the battlefield --
  -- ZoneChange.object, the field the SelfDies arm reads for the same reason.
  --
  -- The SAME slot as that arm, CR 400.7e being one rule with two readings. What
  -- differs is which object CR 113.7a's source happens to be, a fact about the
  -- CONDITION rather than the slot: SelfDies matches the departing incarnation, so
  -- `triggerSource` and `became` are two incarnations of one card, while here the
  -- bearer is another permanent entirely. Two slots would have to be kept apart by
  -- every reader for a distinction no rule draws -- and Resolve, where the slot is
  -- read, never learns which condition placed the ability.
  --
  -- The public-zone proviso holds by construction here too, `to == Battlefield`
  -- having already been required.
  --
  -- Bound whatever the Filter admits, creature or not: whether the entrant can
  -- RECEIVE what the payload does is the payload's question (CR 120.1a for
  -- damage), and a binding that existed only for creatures would make the slot's
  -- presence depend on the entrant, which eventBindingSlots cannot express.
  (TriggerCondition.PermanentEnters _, GameEvent.Moved (Moved.MkMoved zc _)) ->
    Binding.setBecame (ZoneChange.object zc) Map.empty
  -- CR 708.7's "that creature": the permanent that was turned face up, which Pine
  -- Walker untaps. The bearer is a bystander here -- CR 113.7a's source slot names
  -- the WATCHER, and on Pine Walker's own board the two are different permanents --
  -- so the subject needs a name of its own.
  --
  -- THE SAME SLOT the zone-change arms above stamp, deliberately widened rather
  -- than a fresh one. Turning face up is NOT a zone change: CR 708.8 restores the
  -- permanent's copiable values and leaves it on the battlefield, so CR 400.7
  -- mints no new id and nothing here is an incarnation a card became. What carries
  -- the widening is that CR 400.7e's slot is the printed word "it"/"that
  -- creature" -- the thing the event names, which the ability's source is not --
  -- and Resolve, where the slot is read, never learns which condition placed the
  -- ability, so a second slot would be two names for one notion kept apart by
  -- every reader for a distinction no rule draws. It is also the only choice that
  -- can ever serve CR 603.1b's AnyOf, whose slots eventBindingSlots INTERSECTS: a
  -- fresh name would make the intersection with PermanentEnters empty forever
  -- (#963).
  --
  -- Unconditional given a match, which is what eventBindingSlots' per-condition
  -- promise needs: every GameEvent.TurnedFaceUp carries exactly one ObjectId, and
  -- it is the only thing the event carries. Bound whatever the Filter admitted,
  -- for the PermanentEnters arm's reason.
  --
  -- SelfTurnedFaceUp gets no such arm: there the subject IS the bearer, whom CR
  -- 113.7a's source slot already names.
  (TriggerCondition.PermanentTurnedFaceUp _, GameEvent.TurnedFaceUp oid) ->
    Binding.setBecame oid Map.empty
  -- "That player": the discarder, which CR 701.9a makes one player and the event
  -- carries directly. The same reserved slot CR 702.70a's poisonous uses, for the
  -- same reason -- a player the EVENT names, which CR 109.5's `you` cannot stand
  -- in for.
  (TriggerCondition.PlayerDiscards _, GameEvent.Discarded (Discarded.MkDiscarded discarder _ _)) ->
    Binding.setTriggerPlayer discarder Map.empty
  -- CR 702.86a's "defending player": CR 508.5 resolves that phrase through what
  -- the attacking creature is attacking, and Pawl.Engine.Combat.declareAttackers
  -- stamped the answer onto the event as the declaration was written down. The
  -- same reserved slot the discard and poisonous arms use, for the same reason --
  -- a player the EVENT names, whom CR 109.5's `you` cannot stand in for. Here it
  -- is not even an opponent by construction: the attacking creature's controller
  -- is `you`, and CR 506.2a picks the defender out of several opponents.
  --
  -- Read off the event rather than derived, which is what makes this arm possible
  -- at all: this function takes no game state, and both the planeswalker and the
  -- battle forms of CR 508.5 need the board.
  (TriggerCondition.SelfAttacks _, GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared _ defending _)) ->
    Binding.setTriggerPlayer defending Map.empty
  -- CR 702.83a's "that creature": the creature that attacked alone, which is the
  -- id the same event names -- and NOT the bearer, since rule 702.83a's condition
  -- watches every creature its controller has. The defending player the event
  -- also carries is not bound, because rule 702.83a names no player.
  (TriggerCondition.CreatureAttacksAlone _, GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared attacker _ _)) ->
    Binding.setAttackingCreature attacker Map.empty
  -- The same slot off the same event, for Marchesa's Decree's "that creature's
  -- controller" -- again not the bearer, which is a bystanding enchantment. CR
  -- 508.5's defending player goes unbound here where the SelfAttacks arm above
  -- binds it: matchesTrigger has already required that player to be CR 109.5's
  -- "you", so a slot would be a second name for a seat the ability has.
  (TriggerCondition.CreatureAttacksYou, GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared attacker _ _)) ->
    Binding.setAttackingCreature attacker Map.empty
  -- CR 508.3b's subject, under the same reserved slot every other "that player"
  -- takes: whom the Curse enchants, which matchesTrigger has already required the
  -- event to name. Bound rather than left to the ability's own attachment because
  -- the payload reads it as a player -- Curse of Vitality's "each opponent
  -- attacking that player" -- and Object.attachedTo is a Recipient.
  (TriggerCondition.AttachedPlayerIsAttacked, GameEvent.BecameAttacked (BecameAttacked.MkBecameAttacked _ (AttackTarget.OfPlayer attacked))) ->
    Binding.setTriggerPlayer attacked Map.empty
  -- CR 508.3e's SECOND subject, off the same event and under the same reserved
  -- slot: whom the declaration was aimed at, which is what "that player" means
  -- in Seifer, Balamb Rival's "goad target creature that player controls". The
  -- ATTACKING player the event also carries goes unbound, the CreatureAttacksYou
  -- arm above's reasoning -- no printing of this shape points back at it that
  -- CR 109.5's "you" cannot say, and #2154 is where the ones that do are
  -- tracked.
  --
  -- The narrowing to AttackTarget.OfPlayer is matchesTrigger's, re-stated here
  -- because this function is not given its answer: an event this condition
  -- rejected reaches no binding at all.
  (TriggerCondition.PlayerAttacksPlayer {}, GameEvent.BecameAttacked (BecameAttacked.MkBecameAttacked _ (AttackTarget.OfPlayer attacked))) ->
    Binding.setTriggerPlayer attacked Map.empty
  -- CR 509.3e's "that attacking creature", off the grouped blocking event: the
  -- attacker the event names, and again not the bearer, which is a bystander.
  -- CR 508.5's defending player rides that event too and goes unbound, the
  -- CreatureAttacksYou arm above's reasoning -- matchesTrigger has already
  -- required whom the attacker attacked to be a player the PlayerRelation
  -- admits.
  (TriggerCondition.CreatureBecomesBlockedByAtLeast {}, GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked attacker _)) ->
    Binding.setAttackingCreature attacker Map.empty
  -- The same slot off matchesTrigger's OTHER road into this condition (rule
  -- 509.3e's "effects that add or remove blockers"): the attacker
  -- GameEvent.BecameBlocking names, which is the creature that just became
  -- blocked by one more. Without this arm the trigger fires with an empty
  -- binding map and Seifer's "that attacking creature" resolves to nothing --
  -- a board indistinguishable from one where it never fired, which is why
  -- Pawl.ZoneTriggerSpec's representativeEvents lists BOTH events for this
  -- condition and intersects what they stamp.
  --
  -- The BLOCKER the event also names goes unbound: this form names a number
  -- rather than an object, and eventBindingSlots promises
  -- Binding.attackingCreature alone.
  --
  -- Unguarded on putOntoBattlefield where matchesTrigger is not, for the
  -- PlayerAttacksPlayer arm's converse reason: an event this condition
  -- rejected reaches no binding at all, so a declaration's clear flag has
  -- already been answered by the time anything asks here.
  (TriggerCondition.CreatureBecomesBlockedByAtLeast {}, GameEvent.BecameBlocking (BecameBlocking.MkBecameBlocking {BecameBlocking.attacker = attacker})) ->
    Binding.setAttackingCreature attacker Map.empty
  -- CR 702.130a's "defending player", the same phrase and the same reserved slot
  -- as the arm above -- CR 508.5 resolves it for an ability of an ATTACKING
  -- creature, which is what the bearer of this condition is. Read off the event
  -- for that arm's reason: every writer of it stamps CR 508.5's answer there --
  -- Combat.declareBlockers, Combat.becomeBlocked (CR 509.1h's effect) and
  -- Combat.putOntoBattlefieldBlocking (CR 509.4).
  (TriggerCondition.SelfBecomesBlocked, GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked _ defending)) ->
    Binding.setTriggerPlayer defending Map.empty
  -- CR 615.13's "that many": how much this prevention effect prevented, which is
  -- the whole reason the event carries a number. The first reserved slot holding
  -- an AMOUNT rather than a reference, read back by Quantity.InSlot off the stack
  -- object these bindings are stamped on (see Binding.eventAmount).
  --
  -- The recipient is NOT bound alongside it. Every payload this CONDITION
  -- carries acts on the ability's own source (Selfless Squire counters itself),
  -- and the player the recipient names here is CR 109.5's "you", already bound.
  (TriggerCondition.DamageToPlayerPrevented _, GameEvent.DamagePrevented (DamagePrevented.MkDamagePrevented _ _ amount)) ->
    Binding.setEventAmount amount Map.empty
  -- CR 615.13's "that much" once more, off the same event and into the same
  -- reserved slot: the Vindicator deals what its own prevention stopped. The
  -- recipient is not bound alongside it for the arm above's reason -- the payload
  -- acts on a target it chooses, never on whoever the prevented damage was
  -- addressed to.
  (TriggerCondition.SelfPreventsDamage, GameEvent.DamagePrevented (DamagePrevented.MkDamagePrevented _ _ amount)) ->
    Binding.setEventAmount amount Map.empty
  -- CR 119.9's "that much": how much life the gain was, which CR 603.2 makes part
  -- of the event that fired the trigger -- Sanguine Bond's "target opponent loses
  -- that much life". The SAME slot the prevention arm above stamps, one printed
  -- phrase and one number (see Binding.eventAmount).
  --
  -- The AMOUNT the event recorded, never the gainer's life total: CR 119.3
  -- adjusts a total by the gain, so the two coincide only on a board that started
  -- at nothing, and the printed word means the gain.
  --
  -- The gaining PLAYER alongside it, under the reserved slot the loss arm below
  -- and CR 701.9a's discard trigger already stamp: False Cure's "that player loses
  -- 2 life for each 1 life they gained" reads both halves of one event, and
  -- CR 603.2 makes both halves part of it. Bound whichever relation matched, for
  -- the reason the loss arm spells out -- under You the slot is a second name for
  -- CR 109.5's "you", a redundancy rather than a wrong answer, and
  -- eventBindingSlots answers per condition with no relation in hand.
  (TriggerCondition.PlayerGainsLife _, GameEvent.LifeGained (LifeChange.MkLifeChange pid amount)) ->
    Binding.setTriggerPlayer pid (Binding.setEventAmount amount Map.empty)
  -- The other direction's "that much" -- Exquisite Blood's "you gain that much
  -- life". The same slot and the same reading as the gain arm above, off an
  -- event CR 603.2 makes the number part of.
  --
  -- The AMOUNT the event recorded, never the loser's life total. Under the one
  -- relation a card in the pool uses the two are not even the same player's
  -- number: Exquisite Blood's controller is bound as "you" while the loss is an
  -- opponent's.
  --
  -- The LOSING player alongside it, under the reserved slot CR 701.9a's discard
  -- trigger already stamps: Mindcrank's "that player mills that many cards" reads
  -- both halves of one event, and CR 603.2 makes both halves part of it.
  --
  -- Bound whichever relation matched, and that is a statement about the EVENT
  -- rather than about the relation -- eventBindingSlots below answers per
  -- condition with no relation in hand, so a slot it promises has to hold for
  -- every relation the condition admits. Under You the loser is also CR 109.5's
  -- "you", so the slot is a second name for one player there; that is a
  -- redundancy, not a wrong answer, and the alternative -- binding it only under
  -- Opponent -- would make the promise depend on the relation.
  (TriggerCondition.PlayerLosesLife _, GameEvent.LifeLost (LifeChange.MkLifeChange pid amount)) ->
    Binding.setTriggerPlayer pid (Binding.setEventAmount amount Map.empty)
  -- CR 601.2i's "it": the spell that became cast, which the event names and
  -- which nothing else on the ability does. Presence of the Master's "whenever a
  -- player casts an enchantment spell, counter it" is the reader.
  --
  -- The STACK object, not a card in a hand or a library -- see GameEvent.SpellCast
  -- for why the two are different objects and why this is the one the rule is
  -- about. Guaranteed to be a real id when the binding is made: CR 601.2a leaves
  -- the spell on the stack "until it resolves, it's countered, or a rule or
  -- effect moves it elsewhere", and none of those can have happened before the
  -- gather, the cast being the last thing Pawl.Engine.Cast does. By RESOLUTION
  -- it can be gone -- the case CR 608.2h is about -- which is the payload's
  -- business rather than this function's: CR 701.6a's funnel no-ops on a dead id.
  --
  -- The CASTER alongside it, under the reserved slot CR 701.9a's discard trigger
  -- and CR 702.70a's poisonous already stamp: Kambal, Consul of Allocation's
  -- "that player loses 2 life" is the reader, and CR 112.2 makes that player the
  -- spell's controller -- "by default, the player who put it on the stack".
  --
  -- A SLOT rather than a reader that derives the controller from the bound
  -- spell, and CR 608.2h is the argument: the spell can be GONE by the time this
  -- ability resolves (another trigger counters it first), and a derivation would
  -- then have to fall back to last known information for a fact the event
  -- carried outright. The PlayerLosesLife arm binds both halves of its event for
  -- the same reason.
  --
  -- Bound whatever the Filter admitted, which is what eventBindingSlots'
  -- per-condition promise needs -- that function answers with no event and no
  -- Filter in hand, so a slot it names has to hold for every cast the condition
  -- can match. Both do: GameEvent.SpellCast carries an ObjectId and a PlayerId
  -- unconditionally, so no shape of the event withholds either.
  (TriggerCondition.SpellCast {}, GameEvent.SpellCast (SpellWasCast.MkSpellWasCast caster spell _ _)) ->
    Binding.setTriggerPlayer caster (Binding.setCastSpell spell Map.empty)
  -- CR 702.21a's "that spell or ability": the object whose announcement fired
  -- this, which ward counters and whose controller ward offers the cost to.
  -- Unconditional given a match, which is what eventBindingSlots' per-condition
  -- promise needs: every GameEvent.BecameTarget carries a source.
  --
  -- The TARGETED object is the bearer, already bound as CR 113.7a's source, so it
  -- gets no second name. Nor does the controller: Resolve.payerOf reads this very
  -- slot as "whoever controls that object", which is the whole of rule 702.21a's
  -- "unless that player pays".
  (TriggerCondition.SelfBecomesTargeted _, GameEvent.BecameTarget t) ->
    Binding.setTargetingObject (BecameTarget.source t) Map.empty
  -- The same slot off the same field, one recipient over: Amulet of Safekeeping's
  -- "counter THAT SPELL OR ABILITY unless its controller pays {1}" names the
  -- object that did the targeting, and here the targeted party is a player rather
  -- than the bearer, so nothing else on the ability reaches it.
  --
  -- Unconditional in the same sense as the arm above, and its controller likewise
  -- takes no slot of its own: Resolve.payerOf reads this slot as "whoever
  -- controls that object", which is CR 405.4's answer to "its controller".
  (TriggerCondition.ControllerBecomesTarget {}, GameEvent.BecameTarget t) ->
    Binding.setTargetingObject (BecameTarget.source t) Map.empty
  -- CR 509.3d's "that creature": the blocker whose declaration fired this, which
  -- rule 702.25a's payload gives -1/-1. Unconditional in the same sense as the
  -- arm above -- every GameEvent.BecameBlocking carries both ids -- so
  -- eventBindingSlots' per-condition promise holds with no event in hand.
  --
  -- The ATTACKER on the same event is the bearer, already bound as CR 113.7a's
  -- source, so it gets no second name.
  (TriggerCondition.SelfBecomesBlockedBy _, GameEvent.BecameBlocking (BecameBlocking.MkBecameBlocking {BecameBlocking.blocker = blocker})) ->
    Binding.setBlockingCreature blocker Map.empty
  -- CR 509.3b's "that creature": the ATTACKER on the very same declaration, which
  -- Loyal Sentry's payload destroys. The mirror of the arm above, and
  -- unconditional for the same reason; here it is the BLOCKER that is the bearer
  -- and so gets no second name.
  (TriggerCondition.SelfBlocksCreature _, GameEvent.BecameBlocking (BecameBlocking.MkBecameBlocking {BecameBlocking.attacker = attacker})) ->
    Binding.setBlockedCreature attacker Map.empty
  -- CR 702.134c's "that creature": the creature that was mentored, the event's
  -- second id -- Aegis of the Legion's shield counter goes on it. The MENTOR gets no
  -- slot: matchesTrigger has just proved it is the bearer's host, and no printed
  -- payload points at it.
  --
  -- Unconditional given a match, which is what eventBindingSlots' per-condition
  -- promise needs: every GameEvent.Mentored carries both ids.
  (TriggerCondition.AttachedCreatureMentors, GameEvent.Mentored (Mentored.MkMentored _ mentored)) ->
    Binding.setMentoredCreature mentored Map.empty
  -- CR 400.7f, the sibling of CR 400.7e's `became` arms above: an ability that
  -- triggers when an enchanted permanent leaves the battlefield "can find the new
  -- object that each Aura enchanting that permanent became in its owner's
  -- graveyard". Screams from Within's "return THIS CARD from your graveyard to
  -- the battlefield" is the reader, and this is the id that answers it -- CR
  -- 400.7 having already destroyed the battlefield id CR 113.7a's `triggerSource`
  -- slot carries. Binding.became's own comment draws the line: `triggerSource` is
  -- everything the ability says ABOUT itself, this slot everything it DOES to
  -- itself.
  --
  -- Both sentences of the rule, and one lookup serves them: the "at the same time
  -- the enchanted permanent left the battlefield" case is the wrath, where host
  -- and Aura share an EventGroup, and the CR 704.5m case is the ordinary one,
  -- where the Aura is buried by a LATER pass of the same CR 117.5 batch. The
  -- argument is computed over the whole scanned batch, which is exactly those two
  -- and no wider: an Aura that reached a graveyard at an EARLIER group than the
  -- host's death is offered by none of eventTriggers' candidate sources, so its
  -- trigger is never gathered and this is never asked about it.
  --
  -- The EVENT is not read -- see the first argument's note on the signature -- so
  -- the pattern is a wildcard rather than the Moved shape matchesTrigger accepted.
  --
  -- Nothing where the bearer did not reach a graveyard: an Equipment host dying
  -- under CR 704.5n leaves the bearer standing, and an effect that sent the Aura
  -- elsewhere in the same batch put it somewhere the rule cannot look. Both are
  -- CR 400.7f's own answer rather than a hole -- the payload finds nothing and
  -- moves nothing -- and eventBindingSlots below says why the floor is still the
  -- slot.
  (TriggerCondition.AttachedCreatureDies, _) ->
    maybe Map.empty (`Binding.setBecame` Map.empty) bearerBecame
  -- Nothing at all, stated rather than left to the fallthrough below: the
  -- attachment link already names the permanent that became tapped, and CR 109.5
  -- answers the Aura's "you" from Binding.triggerSource. There is no second
  -- object for the payload to name.
  (TriggerCondition.AttachedCreatureBecomesTapped, _) -> Map.empty
  -- CR 725.1's newly crowned player: Garland, Royal Kidnapper's "that player",
  -- whose creature the trigger then targets and whose crown its duration watches.
  -- Bound whichever relation matched, for the reason the PlayerLosesLife arm
  -- gives: eventBindingSlots answers per CONDITION with no relation in hand, so
  -- the slot has to hold for every relation this condition admits. Under You the
  -- crowned player is also CR 109.5's "you", which is a redundancy rather than a
  -- wrong answer.
  --
  -- Unconditional given a match: GameEvent.BecameMonarch carries a PlayerId
  -- outright, and CR 725.3 makes it exactly one.
  (TriggerCondition.PlayerBecomesMonarch _, GameEvent.BecameMonarch crowned) ->
    Binding.setTriggerPlayer crowned Map.empty
  -- CR 120.3's "that much", read by the damage's RECIPIENT: Coalhauler Swine's
  -- "whenever this creature is dealt damage, it deals that much damage to each
  -- player". The same reserved slot CR 615.13's prevention and CR 119.9's life
  -- gain stamp (see Binding.eventAmount), and the same reading -- the AMOUNT the
  -- event recorded, never the damager's power or the bearer's: CR 702.19b lets a
  -- trampler split its power across a blocker and a player, and CR 120.3 admits
  -- noncombat damage whose amount its source's power never named at all.
  --
  -- ONE event's amount, not a batch's: CR 510.2 deals a combat damage step's
  -- damage simultaneously and Pawl.Engine.Damage records a DamageDealt per
  -- surviving event, so two blockers stamp two triggers with their own numbers
  -- rather than one with the sum. Pinned by Pawl.TriggerSpec's two-blocker board.
  --
  -- Unconditional given a match, which is what eventBindingSlots' per-condition
  -- promise needs: every GameEvent.DamageDealt carries a DamageEvent.amount,
  -- whichever of CR 120.3's damage kinds it is.
  --
  -- The DAMAGER takes Binding.combatDamager beside it, which is CR 120.1's "an
  -- object that deals damage is the source of that damage" -- Belltower Sphinx's
  -- "that source's controller mills that many cards", read through
  -- PlayerRef.ControllerOfBound. Also unconditional given a match:
  -- DamageEvent.source is an ObjectId outright, so there is nothing to fail.
  --
  -- The slot is not combat-scoped here even though its name is: CR 120.1 makes
  -- every damage event name a source, and Belltower Sphinx's trigger admits a
  -- Prodigal Sorcerer's ping as readily as a blocker's. Pinned by
  -- Pawl.TriggerSpec's noncombat board.
  --
  -- The RECIPIENT needs no slot -- matchesTrigger has just proved it is the
  -- bearer, whom CR 113.7a's source slot already names.
  (TriggerCondition.SelfIsDealtDamage, GameEvent.DamageDealt ev) ->
    Binding.setCombatDamager (DamageEvent.source ev) (Binding.setEventAmount (DamageEvent.amount ev) Map.empty)
  -- CR 603.1b's multi-condition ability reaches this fallthrough and stamps
  -- nothing, which agrees with eventBindingSlots' intersection for the pool's one
  -- AnyOf and is pinned by Pawl.TriggerSpec against every event either branch
  -- admits. An AnyOf two of whose branches bind the SAME slot is not handled
  -- (#963).
  --
  -- So do the five CR 701/702 keyword-action conditions, deliberately: no card
  -- in the pool reads the scrying player, the plotted card or the explorer, and
  -- SelfExerted's "it" is the bearer, which CR 113.7a's source slot already
  -- names. eventBindingSlots claims nothing for any of them; see that function's
  -- arms.
  --
  -- And so does SelfDiscarded, for SelfCycled's reason: CR 701.9a's discarded
  -- card is the bearer, whom CR 113.7a's source slot already names, and its owner
  -- is CR 113.8's controller, whom Binding.setYou already names.
  --
  -- CR 603.12's Reflexive reaches it NECESSARILY rather than by choice: it
  -- admits no event at all, so delayedPending never calls this for one and there
  -- is nothing an arm could read. What such an ability knows comes from CR
  -- 603.7c's captured environment instead.
  _ -> Map.empty

-- Which slots eventBindings above can stamp for a condition, as a set. A
-- CLASSIFICATION of a rule 603 trigger condition -- the sibling of
-- zonesTriggeredFrom below, which asks the other structural question about the
-- same closed type -- so it never reaches an ability's payload and no reader of
-- it learns what any effect IS.
--
-- Its customer is the card lint (CardSpec's "every slot a triggered ability
-- reads is bound for its condition"): an effect naming CR 400.7e's `became` or
-- CR 702.70a's `thatPlayer` under a condition that binds neither would place its
-- trigger, miss the lookup and silently do nothing, which is the worst failure
-- mode card data has.
--
-- Exhaustive with no wildcard, deliberately unlike eventBindings' own
-- `_ -> Map.empty`: that case is over (condition, event) PAIRS, where a wildcard
-- is the only way to say "this pair does not match", while a new CONDITION here
-- must force a decision rather than defaulting to "binds nothing" -- the default
-- that would silently un-lint whatever slot the new condition binds.
--
-- A PARALLEL STATEMENT, PINNED BY A TEST. This says in one dimension what
-- eventBindings says in two, so the two can drift out of agreement. Deriving
-- this from that would mean fabricating a representative GameEvent per condition
-- inside the rules core, which is fixture work the engine has no other use for;
-- the agreement is therefore pinned from the test side instead, by TriggerSpec's
-- "CR 603.2 eventBindingSlots names exactly the keys eventBindings stamps for
-- EVERY event a condition admits", which runs every condition against the events
-- that genuinely fire it and intersects the Map.keysSet of each result against
-- the answer here.
--
-- Every slot named here is GUARANTEED given a match, the only reading that makes a
-- per-CONDITION set sound: the answer must hold for every event the condition
-- admits, the card lint having no event in hand. For most conditions the readings
-- coincide, matchesTrigger having already pinned the destination or recipient.
-- SelfLeavesTheBattlefield is where they come apart, and gets the floor.
eventBindingSlots :: TriggerCondition -> Set.Set SlotName.SlotName
eventBindingSlots cond = case cond of
  -- CR 309.4c names nothing but the room, and the room is in the condition rather
  -- than in the event's bindings. The dungeon card itself arrives under CR 113.7a's
  -- reserved source slot, which every borne trigger gets at placement.
  TriggerCondition.RoomEntered _ -> Set.empty
  -- Nothing, for all four keyword actions. CR 701.22d and CR 701.25d name a
  -- player and CR 702.170a and CR 701.44b an object, but no printed payload
  -- under any of them points at one: Matoya, Archon Elder draws, Aloe
  -- Alchemist targets a creature of its controller's choosing and Wildgrowth
  -- Walker grows itself. A card printing "that player" or "that creature" is
  -- what would earn a slot, and eventBindings has no arm for any of the four
  -- until one does.
  TriggerCondition.PlayerScries _ -> Set.empty
  TriggerCondition.PlayerSurveils _ -> Set.empty
  TriggerCondition.SelfBecomesPlotted -> Set.empty
  TriggerCondition.PermanentExplores _ -> Set.empty
  -- Nothing here either. CR 706.1's event names the roller, but Feywild
  -- Trickster's payload points at no one -- it creates a token for its own
  -- controller -- and a card printing "that player" is what would earn a slot.
  -- The roll's numerical RESULT is not a binding of this condition at all:
  -- Pawl.Engine.Resolve binds it at Pawl.Types.RollDie's own slot, during the
  -- roller's own resolution, for a later effect of THAT ability to read.
  TriggerCondition.PlayerRollsDice _ -> Set.empty
  -- Empty for the same reason, and CR 701.43d is what settles it: the linked
  -- trigger's "it" is the exerted permanent, which is already CR 113.7a's source
  -- slot, so a binding here would be a second name for one object. Glory-Bound
  -- Initiate reads it as Filter.IsSource.
  TriggerCondition.SelfExerted -> Set.empty
  -- Empty DELIBERATELY. CR 701.3a's event names two objects, and the bearer is
  -- one of them -- CR 113.7a's source slot already names the host. The other,
  -- the attachment, has no printed reader: Bramble Elemental says "create two
  -- 1\/1 green Saproling creature tokens" and names no "it". Enormous Energy
  -- Blade's "tap that creature" is the printing that earns a slot, and it reads
  -- the event from the other end (gap #1837).
  TriggerCondition.SelfBecomesAttachedBy _ -> Set.empty
  -- CR 603.6a's two written forms differ only in which object the bearer is.
  -- SelfEnters matches on `object == bearer`, so CR 113.7a's source slot already
  -- names the entrant and `became` would be a second name for one object.
  -- "Whenever a [type] enters" has no such luck.
  TriggerCondition.SelfEnters -> Set.empty
  TriggerCondition.PermanentEnters _ -> Set.singleton Binding.became
  -- CR 603.2b's step beginning names no OBJECT -- but it names the active player,
  -- and the active player is not what CR 109.5's `you` means, so "that player"
  -- needs a slot of its own. Shizuko, Caller of Autumn is the reader.
  --
  -- Unconditional, as this classification has to be: every GameEvent.StepBegan
  -- carries a PlayerId, whatever the TurnScope.
  TriggerCondition.StepBegins {} -> Set.singleton Binding.triggerPlayer
  -- CR 603.8: a state trigger matches a game STATE rather than an event
  -- (matchesTrigger's StateIs arm answers False for every event), so no event
  -- contributes anything to one.
  TriggerCondition.StateIs _ -> Set.empty
  -- CR 702.70a's "that player": the player the bearer dealt combat damage to.
  --
  -- CR 510.2's amount beside it, which Questing Beast's "that much" reads: the
  -- same slot CR 615.13's prevention and CR 119.9's life gain stamp. Guaranteed
  -- given a match -- every DamageDealt event carries an amount.
  TriggerCondition.SelfDealsCombatDamageToPlayer -> Set.fromList [Binding.eventAmount, Binding.triggerPlayer]
  -- CR 120.3's amount for enrage, which Coalhauler Swine's "it deals that much
  -- damage to each player" reads: the same slot CR 615.13's prevention and CR
  -- 119.9's life gain stamp, and guaranteed given a match -- every DamageDealt
  -- event carries an amount, whichever of CR 120.3's two damage kinds this
  -- unfiltered condition admitted.
  --
  -- Plus CR 120.1's source, which Belltower Sphinx's "that source's controller"
  -- reads, and equally guaranteed -- every DamageDealt event carries one. No slot
  -- for the recipient, who is the bearer. See the eventBindings arm above.
  TriggerCondition.SelfIsDealtDamage -> Set.fromList [Binding.combatDamager, Binding.eventAmount]
  -- CR 510.2's damager, which the bystander's form needs and the self-scoped one
  -- above does not: there the damager IS the bearer, already bound as CR 113.7a's
  -- source. Aragorn, Hornburg Hero's "double the number of +1/+1 counters on it"
  -- is the reader. Guaranteed given a match -- every DamageDealt event carries a
  -- source.
  --
  -- CR 510.2's amount beside its damager, which Shroofus Sproutsire's "that many"
  -- reads: the same slot CR 615.13's prevention and CR 119.9's life gain stamp, and
  -- guaranteed given a match for the same reason -- every DamageDealt event carries
  -- an amount.
  --
  -- CR 603.2's "that player" beside them, which Larceny's "that player discards a
  -- card" reads -- the same slot the self-scoped arm above stamps. Guaranteed
  -- given a match: matchesTrigger admits only a player recipient here.
  TriggerCondition.PermanentDealsCombatDamageToPlayer _ -> Set.fromList [Binding.combatDamager, Binding.eventAmount, Binding.triggerPlayer]
  -- CR 725.2's inherent ability is borne by no card, and its bindings come from
  -- Monarch.inherentMatch rather than eventBindings -- so a card declaring this
  -- condition would honestly get nothing from the event.
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> Set.empty
  -- CR 702.179d's ability is borne by no card either, and binds nothing at all --
  -- "your speed" is the controller's, whom Binding.setYou already names.
  TriggerCondition.OpponentLostLifeDuringYourTurn -> Set.empty
  -- CR 702.29c's cycled card is the bearer itself, already bound as CR 113.7's
  -- source.
  TriggerCondition.SelfCycled -> Set.empty
  -- CR 702.94a's revealed card is the bearer itself, already bound as CR 113.7's
  -- source, and the revealing player is its owner -- the same seat CR 113.8 makes
  -- the ability's controller, whom Binding.setYou already names.
  TriggerCondition.SelfRevealedForMiracle -> Set.empty
  -- CR 701.9a's discarded card is the bearer itself, already bound as CR 113.7's
  -- source, and the discarding player is its owner -- the same seat CR 113.8 makes
  -- the ability's controller, whom Binding.setYou already names.
  TriggerCondition.SelfDiscarded -> Set.empty
  -- CR 701.9a's discarding player, which is nobody the bearer already names --
  -- Megrim's "that player" is the opponent whose hand the card left.
  TriggerCondition.PlayerDiscards _ -> Set.singleton Binding.triggerPlayer
  -- NOTHING, where the cause-blind sibling above binds the discarder. Prickly
  -- Marmoset's payload says "this creature", which is CR 113.7's source slot
  -- the placement already stamps, and names no player; a printing under this
  -- condition that said "that player" is what would earn the slot. eventBindings
  -- has no arm for this condition, and the two must agree.
  TriggerCondition.PlayerCycles _ -> Set.empty
  -- NOTHING. The event names the drawing player, and CR 701.9a's `triggerPlayer`
  -- is the slot they would take, but no card in the pool reads them under this
  -- condition: Erudite Wizard's payload points only at its own bearer, and Faerie
  -- Mastermind's says "you". Ian Malcolm, Chaotician's "that player exiles" is the
  -- card that would add the slot, and it needs a PlayerRelation for "a player"
  -- that does not exist either.
  TriggerCondition.PlayerDrawsNthCard {} -> Set.empty
  -- CR 508.5's defending player, which the declaration event carries -- rule
  -- 702.86a's annihilator is the reader. The DECLARED attacker itself is the
  -- bearer, already bound as CR 113.7a's source, so it needs no slot of its own.
  --
  -- Unconditional, as this classification has to be: every AttackerDeclared event
  -- carries a PlayerId, so no shape of the event withholds it.
  TriggerCondition.SelfAttacks _ -> Set.singleton Binding.triggerPlayer
  -- NOTHING, unlike SelfAttacks above: rule 702.149a's payload names only "this
  -- creature", so neither the companion that qualified nor CR 508.5's defending
  -- player is pointed at afterwards.
  TriggerCondition.SelfAttacksWithAnother _ -> Set.empty
  -- CR 506.5's lone attacker, which the same event names -- rule 702.83a's
  -- exalted is the reader. Where SelfAttacks above needs a slot for the PLAYER
  -- and gets the creature free (it is the bearer), this one needs a slot for the
  -- CREATURE and wants no player: the bearer is a bystander.
  --
  -- Unconditional, as this classification has to be: every AttackerDeclared event
  -- carries an ObjectId, and matchesTrigger has already required the count to be
  -- one.
  TriggerCondition.CreatureAttacksAlone _ -> Set.singleton Binding.attackingCreature
  -- The attacker, CreatureAttacksAlone's slot above -- Marchesa's Decree's "that
  -- creature's controller". CR 508.5's defending player is NOT bound alongside it:
  -- the match has already pinned that player to be CR 109.5's "you".
  --
  -- Unconditional, as this classification has to be: every AttackerDeclared event
  -- carries an ObjectId.
  TriggerCondition.CreatureAttacksYou -> Set.singleton Binding.attackingCreature
  -- The PLAYER instead, where the arm above binds the attacker: rule 508.3b names
  -- a set of creatures rather than one, and Curse of Vitality's payload says
  -- "that player" and nothing about them.
  TriggerCondition.AttachedPlayerIsAttacked -> Set.singleton Binding.triggerPlayer
  -- NOTHING, and neither the attacker nor the player: rule 508.3d names a SET
  -- of creatures, so there is no one attacker to point at. Boggart Prankster's
  -- and Avatar Roku, Firebender's payloads target or say "you" rather than
  -- pointing at the declarer. That is also why this condition needs no arm in
  -- eventBindings.
  --
  -- Not implemented: the declaring player as a bound slot, which "that player"
  -- and "the attacking player" need (#2154). Both must move together with
  -- eventBindings, which Pawl.ZoneTriggerSpec pins against this.
  TriggerCondition.PlayerAttacks _ -> Set.empty
  -- The arm above's reason verbatim: rule 508.3c's subject is a player and the
  -- Filter names a SET of creatures, so there is nothing to point at either.
  TriggerCondition.PlayerAttacksWith {} -> Set.empty
  -- The ATTACKED player, where the two arms above bind nothing: rule 508.3e
  -- names a second player, and that is the one the printed payloads read --
  -- Seifer, Balamb Rival's "that player controls".
  --
  -- Unconditional, as this classification has to be, although matchesTrigger
  -- admits only AttackTarget.OfPlayer: every event this condition MATCHES
  -- carries a player, and eventBindings is consulted for no other.
  TriggerCondition.PlayerAttacksPlayer {} -> Set.singleton Binding.triggerPlayer
  -- NOTHING, for SelfAttacksWithAnother's reason: rule 702.105a's payload names
  -- only "this creature", so the attacked player is compared and then never
  -- pointed at. That is also why this condition needs no arm in eventBindings.
  TriggerCondition.SelfAttacksPlayerWithMostLife -> Set.empty
  -- Nothing, unlike SelfAttacks above: the blocker is the bearer, already bound
  -- as CR 113.7a's source, and the attacker the event also carries is what CR
  -- 509.3b's condition names rather than this one, below. CR 509.1a makes the
  -- blocker's controller the defending player, whom CR 109.5's `you` already
  -- names.
  TriggerCondition.SelfBlocks -> Set.empty
  TriggerCondition.SelfBlocksAtLeast _ -> Set.empty
  TriggerCondition.SelfBlocksOneOrMore _ -> Set.empty
  -- CR 509.3b's form is the one that DOES name the attacker, off the same event.
  -- Guaranteed rather than conditional, as SelfBecomesBlockedBy's is: every
  -- declaration carries both ids, and matchesTrigger has already pinned the
  -- blocker to the bearer.
  TriggerCondition.SelfBlocksCreature _ -> Set.singleton Binding.blockedCreature
  -- CR 508.5's defending player, which the becomes-blocked event carries for
  -- SelfAttacks' reason -- rule 702.130a's afflict is the reader. No BLOCKER: CR
  -- 509.3c names none, so GameEvent.AttackerBlocked carries none, and CR 509.3d's
  -- form below is the one that binds one. The blocked attacker itself is the
  -- bearer, already bound as CR 113.7a's source.
  --
  -- Unconditional, as this classification has to be: every AttackerBlocked event
  -- carries a PlayerId.
  TriggerCondition.SelfBecomesBlocked -> Set.singleton Binding.triggerPlayer
  -- CR 509.3d's form is the one that DOES name a blocker, and
  -- GameEvent.BecameBlocking carries it: rule 702.25a's "the blocking creature".
  -- Guaranteed rather than conditional -- every such event carries both ids, and
  -- matchesTrigger has already pinned the attacker to the bearer.
  TriggerCondition.SelfBecomesBlockedBy _ -> Set.singleton Binding.blockingCreature
  -- CR 509.3e names a SET of blockers rather than one, and no reader in the
  -- pool reaches into it: Serra Inquisitors' payload names only itself.
  TriggerCondition.SelfBecomesBlockedByOneOrMore _ -> Set.empty
  -- Seifer, Balamb Rival's "that attacking creature", the same reserved slot
  -- CreatureAttacksYou fills off its own event.
  TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> Set.singleton Binding.attackingCreature
  -- CR 509.1h's unblocked branch names no second object at all -- no blocker,
  -- and no defending player on GameEvent.AttackerUnblocked to bind one from.
  -- eventBindings therefore has no arm and falls through to the empty map.
  TriggerCondition.SelfAttacksUnblocked -> Set.empty
  -- CR 113.6k: the bearer of a library-to-graveyard trigger IS the arriving
  -- incarnation, so binding it again under `became` would be a second name for
  -- one object. Narcomoeba reads the source slot instead.
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> Set.empty
  -- The same answer for the same reason: with no look-back (CR 603.6c's last
  -- sentence), this condition's bearer already IS the arriving incarnation, so
  -- `became` would be a second name for one object. Serra Avatar's "shuffle IT"
  -- reads the source slot.
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> Set.empty
  -- CR 400.7e: the incarnation the card became, which CR 603.10a's look-back
  -- keeps out of the source slot.
  TriggerCondition.SelfDies -> Set.singleton Binding.became
  -- PermanentEnters' `became` pointed at the opposite zone change, the two being
  -- the same bystander shape: CR 400.7e supplies the name and CR 400.2 makes the
  -- graveyard public. Guaranteed rather than conditional, unlike
  -- SelfLeavesTheBattlefield below, because matchesTrigger has already pinned the
  -- destination to the graveyard. Promise of Tomorrow's "exile it" is the reader.
  TriggerCondition.PermanentDies _ -> Set.singleton Binding.became
  -- Empty where PermanentDies binds CR 400.7e's graveyard card, and NECESSARILY
  -- so: the trigger event is a whole CR 704.3 batch, which may have buried
  -- several cards, and one slot cannot name them all. Nothing in print asks for
  -- one either: Scryfall o:"one or more other creatures you control die",
  -- 2026-08-24, matches Vengeful Townsfolk and Vraan, Executioner Thane, whose
  -- payloads act on the bearer and on the players. A printing whose payload said
  -- "it" would refute this, and the lint would reject it (#505).
  TriggerCondition.PermanentsDie _ -> Set.empty
  -- The same slot and rule as SelfDies, but bound only for a PUBLIC destination
  -- (CR 400.7e's proviso over CR 400.2's hidden zones), so the guaranteed floor is
  -- empty. A card whose leaves-the-battlefield payload names `became` is therefore
  -- rejected by the lint (#505).
  TriggerCondition.SelfLeavesTheBattlefield -> Set.empty
  -- Empty for the arm above's reason, and for a second one: this condition's
  -- other event (CR 603.6c's leaving-the-game form) reaches no zone, so even a
  -- public destination is not guaranteed by a match.
  TriggerCondition.PermanentLeavesTheBattlefield _ -> Set.empty
  -- Nothing, where PermanentDies binds CR 400.7e's graveyard card: rule 702.55b's
  -- ability speaks about the creature it HAUNTS and never about the card that
  -- creature became, so no printing of haunt names the arrival. eventBindings has
  -- no arm for this condition either, which is what the empty floor pins.
  TriggerCondition.HauntedCreatureDies -> Set.empty
  -- CR 701.6a's countering names two objects and a player and this binds none of
  -- them -- eventBindings has no arm for it. Empty by decision rather than
  -- default: both ids are dead by the time the trigger resolves, and CR 400.7e
  -- would name the countered card in its owner's graveyard. A card that says
  -- "exile it instead" is the one that must bind `became` here.
  TriggerCondition.SpellOrAbilityCounters _ -> Set.empty
  -- CR 615.13's amount, guaranteed given a match: the event carries a Natural
  -- unconditionally, so unlike SelfLeavesTheBattlefield's `became` there is no
  -- shape of the event that withholds it.
  TriggerCondition.DamageToPlayerPrevented _ -> Set.singleton Binding.eventAmount
  -- The same slot the arm above declares, off the same event: this condition's
  -- payload reads "that much" too.
  TriggerCondition.SelfPreventsDamage -> Set.singleton Binding.eventAmount
  -- CR 119.9's amount, guaranteed given a match for the prevention arm's reason:
  -- GameEvent.LifeGained carries a Natural unconditionally, so no shape of the
  -- event withholds it. Sanguine Bond's "that much" is what reads it.
  --
  -- And the GAINING player, for the reason the loss arm below binds the loser:
  -- under False Cure's AnyPlayer relation that player is not the "you"
  -- Binding.setYou names, and "that player loses 2 life for each 1 life they
  -- gained" reads them. Guaranteed for the same reason the amount is --
  -- GameEvent.LifeGained carries a PlayerId unconditionally, so the promise holds
  -- under every relation.
  TriggerCondition.PlayerGainsLife _ -> Set.fromList [Binding.eventAmount, Binding.triggerPlayer]
  -- The loss condition's amount, guaranteed for the same reason:
  -- GameEvent.LifeLost carries a Natural unconditionally. Exquisite Blood's "you
  -- gain that much life" is what reads it.
  --
  -- And the LOSING player, which is what separates this from the gain arm above:
  -- under Exquisite Blood's Opponent relation that player is NOT the "you"
  -- Binding.setYou names, and Mindcrank's "that player mills that many cards"
  -- reads them. Guaranteed for the same reason the amount is -- GameEvent.LifeLost
  -- carries a PlayerId unconditionally, so the promise holds under either
  -- relation.
  TriggerCondition.PlayerLosesLife _ -> Set.fromList [Binding.eventAmount, Binding.triggerPlayer]
  -- CR 714.2b names one object -- the bearer -- which CR 113.7a's source slot
  -- already names, so `became` would be a second name for it. The counts the
  -- event carries are the CONDITION's, not the payload's: no chapter ability in
  -- print says "that many", and eventBindings has no arm for this condition.
  TriggerCondition.SelfCountersReached {} -> Set.empty
  TriggerCondition.SelfBecomesClassLevel _ -> Set.empty
  TriggerCondition.SelfLastCounterRemoved _ -> Set.empty
  -- CR 603.2's "that much", the one thing this condition binds that neither
  -- sibling does: Chandra, Fire Artisan reads the number of counters that came
  -- off. The bearer needs no slot, CR 113.7a's source slot already naming it.
  TriggerCondition.SelfCountersRemoved _ -> Set.singleton Binding.eventAmount
  -- CR 709.5h names the permanent and the half, and CR 113.7a's source slot
  -- already names the permanent. The HALF is not bound: no printing says "that
  -- door", so there is nothing for a payload to read it as.
  TriggerCondition.SelfHalfUnlocked _ -> Set.empty
  -- CR 709.5i names a permanent the bearer does not have to be, so a slot for it
  -- would be honest -- but no printing reads one ("each opponent loses 1 life"
  -- names nothing about the Room), and eventBindings stamps nothing for this
  -- condition, so claiming one would promise a slot that is never filled.
  TriggerCondition.RoomFullyUnlocked _ -> Set.empty
  -- The INTERSECTION, because this function answers the guaranteed FLOOR: a slot
  -- named here has to be bound for every event the condition admits, and an AnyOf
  -- admits every event any of its branches does. A UNION would promise a slot only
  -- one branch binds, and Pawl.Engine.Resolve would look it up on an event the
  -- other branch matched and silently do nothing.
  --
  -- Set.empty for the empty list, which is the floor read literally: an AnyOf
  -- with no branches matches no event, so there is no event for which a slot
  -- could fail to be bound -- but there is also no payload that could ever read
  -- one, and the empty intersection is the only answer a Set can give. No card
  -- writes one; Pawl.CardSpec's modal lint is what would notice the ability that
  -- can never fire.
  TriggerCondition.AnyOf conditions -> case fmap eventBindingSlots conditions of
    [] -> Set.empty
    slots : rest -> List.foldl' Set.intersection slots rest
  -- CR 708.7's event names the permanent and nothing else, and CR 113.7a's source
  -- slot already names it -- so this is a DELIBERATE empty rather than an arm
  -- nobody wrote. eventBindings' fallthrough would answer the same for a
  -- condition that had been forgotten, which is exactly why it is spelled out
  -- here: Pawl.TriggerSpec pins the two against each other.
  TriggerCondition.SelfTurnedFaceUp -> Set.empty
  -- The same deliberate empty, and for the same reason one step further on: CR
  -- 701.27e's event names the permanent that transformed, which CR 113.7a's
  -- source slot already names, and the face it turned into is a NAME rather than
  -- an object for a slot to hold.
  TriggerCondition.SelfTransformedInto _ -> Set.empty
  -- The SAME event read by a bystander, and here the answer is NOT empty. CR
  -- 113.7a's source slot names the WATCHER rather than the permanent that turned
  -- over, so the subject needs a slot of its own, and Pine Walker's "untap that
  -- creature" is the printing that reads it. The zone-change slot, widened; see
  -- eventBindings' arm for why it is that slot and not a new one.
  --
  -- ALWAYS bound and never sometimes, which is what Binding.became's own contract
  -- demands: GameEvent.TurnedFaceUp carries one ObjectId unconditionally, so
  -- unlike CR 400.7e's hidden-destination case (#505) there is no shape of this
  -- event that withholds it.
  TriggerCondition.PermanentTurnedFaceUp _ -> Set.singleton Binding.became
  -- A deliberate empty: Valeron Wardens draws a card and names no "it", so there
  -- is no subject to claim a slot for. The arm above is the worked example of what
  -- a card reading the designated permanent would take -- CR 400.7e's slot, since
  -- the event names one object and CR 113.7a's source names the watcher.
  TriggerCondition.PermanentBecomesDesignated {} -> Set.empty
  -- Empty too: rule 702.100b's event names the creature that evolved, and that is
  -- the bearer -- Renegade Krasis says "this creature", so there is no "it" to
  -- bind that Binding.triggerSource does not already answer.
  TriggerCondition.SelfEvolves -> Set.empty
  -- NOT empty, unlike SelfEvolves above, and the pair CR 702.134c names is why:
  -- neither the mentor nor the mentored creature is the bearer, so Aegis of the
  -- Legion's "that creature" has no other name to be read under. Guaranteed given a
  -- match, as this classification has to be: every Mentored event carries both ids.
  TriggerCondition.AttachedCreatureMentors -> Set.singleton Binding.mentoredCreature
  -- CR 400.7f's `became`, and only it. CR 303.4b's "enchanted creature" is the
  -- other permanent the event names, and it gets NO slot: Screams from Within's
  -- payload acts on the bearer alone, and a card that did name the host -- Reins
  -- of the Vinesteed's "that creature" -- would need a second slot here
  -- (gap #1893).
  --
  -- GUARANTEED given a match, on CR 704.5m rather than on the event: an Aura
  -- whose host has left the battlefield is attached to nothing, and that rule
  -- puts it into its owner's graveyard as a state-based action, which CR 117.5
  -- runs to completion before any trigger is placed. matchesTrigger's arm makes
  -- the same observation from the other side -- by the time the condition is
  -- asked, the live attachment is ALWAYS gone.
  --
  -- Two shapes escape it, and neither is a hole this classification has to widen
  -- for. An EQUIPMENT bearer stays on the battlefield under CR 704.5n, and no
  -- printing carries this condition on one (gap #1894 records the query and the
  -- card that would refute it). An effect that sends the Aura somewhere other
  -- than its owner's graveyard in the same batch puts it where CR 400.7f cannot
  -- look -- and there the rule's own answer is that the ability finds nothing, so
  -- the payload moving nothing is correct rather than the silent no-op this lint
  -- exists to catch. That is what separates this arm from
  -- SelfLeavesTheBattlefield's floor, where a BOUNCE is an ordinary printed
  -- destination and the slot's absence is an ordinary printed case (#505).
  TriggerCondition.AttachedCreatureDies -> Set.singleton Binding.became
  -- Empty, and for the opposite reason to the arm above: nothing MOVED, so there
  -- is no arrival for a payload to find. The tapped permanent is still the one
  -- Object.attachedTo names.
  TriggerCondition.AttachedCreatureBecomesTapped -> Set.empty
  -- Empty for SelfEvolves' reason and not for AttachedCreatureMentors' -- rule
  -- 702.149a's counter goes on the bearer, so Savior of Ollenbock's "this creature"
  -- is Binding.triggerSource and the event names nobody else.
  TriggerCondition.SelfTrains -> Set.empty
  -- CR 701.21a's event names a player and a permanent, and this claims NEITHER --
  -- a deliberate empty, decided rather than defaulted, since eventBindings' own
  -- fallthrough would answer the same for a condition nobody wrote an arm for.
  --
  -- The permanent is not bound because CR 603.10a's look-back keeps the pre-move
  -- id out of the graveyard: under a zone change `became` names the incarnation
  -- the move produced (CR 400.7e), and this event is recorded BEFORE the move, so
  -- the id it carries is the one that no longer exists. The player is not bound
  -- because Mayhem Devil's "deals 1 damage to any target" names nobody the event
  -- did.
  -- A card saying "that player" or "return it to its owner's hand" is what earns
  -- a slot here (#977).
  TriggerCondition.PermanentSacrificed -> Set.empty
  -- CR 601.2i's spell, the object the event names and nobody the bearer already
  -- does. Guaranteed given a match for the reason CR 615.13's amount is:
  -- GameEvent.SpellCast carries an ObjectId unconditionally, so no shape of the
  -- event withholds it, and unlike SelfLeavesTheBattlefield's `became` there is
  -- no CR 400.7e proviso to fail. Presence of the Master's "counter it" is what
  -- reads it.
  --
  -- And the CASTER, whom CR 112.2 makes the spell's controller: Kambal, Consul
  -- of Allocation's "that player loses 2 life" reads it, and under that card's
  -- Opponent-scoped Filter the player is not the "you" Binding.setYou names.
  -- Guaranteed for the reason the spell is -- GameEvent.SpellCast carries a
  -- PlayerId unconditionally -- so the promise holds for every cast the Filter
  -- can admit.
  TriggerCondition.SpellCast {} -> Set.fromList [Binding.castSpell, Binding.triggerPlayer]
  -- Nothing, a deliberate empty rather than a default: the spell the event names
  -- is the BEARER, which every ability already reaches as its own source, and the
  -- caster is CR 109.5's "you", whom Binding.setYou already names. A slot would
  -- be a second name for each. eventBindings has no arm for this condition and
  -- its fallthrough answers the same.
  TriggerCondition.SelfCast -> Set.empty
  -- CR 601.2c's targeting object, guaranteed given a match: every
  -- GameEvent.BecameTarget carries a source, so unlike SelfLeavesTheBattlefield's
  -- `became` there is no shape of the event that withholds it. The CONTROLLER
  -- rule 702.21a offers the cost to takes no slot of its own -- Resolve.payerOf
  -- reads a slot bound to an object as that object's controller, which is what
  -- "unless that player pays" asks for.
  TriggerCondition.SelfBecomesTargeted _ -> Set.singleton Binding.targetingObject
  -- CR 601.2c's targeting object again, the sibling directly above one recipient
  -- over: Amulet of Safekeeping's "counter that spell or ability" reads it, and
  -- with a PLAYER targeted there is no bearer-shaped slot that would already
  -- name it. Guaranteed for the sibling's reason -- every GameEvent.BecameTarget
  -- carries a source.
  --
  -- Claimed for the condition rather than for the printing, which is what a
  -- per-CONDITION set means: Dormant Gomazoa's "you may untap this creature"
  -- reads the bearer and never this slot, and a slot promised but unread is
  -- harmless where the reverse is the failure this lint exists to catch.
  TriggerCondition.ControllerBecomesTarget {} -> Set.singleton Binding.targetingObject
  -- CR 603.3b's second class binds NOTHING, a deliberate empty rather than a
  -- default: GameEvent.AbilityTriggered names the Saga and the player who
  -- controls the chapter ability, and Historian's Boon's "create a 4/4 white
  -- Angel" reads neither. A card printing "that Saga" or "that player" is what
  -- would earn a slot (#1029).
  TriggerCondition.SagaFinalChapterTriggers _ -> Set.empty
  -- CR 725.1's newly crowned player -- Garland, Royal Kidnapper watches an
  -- OPPONENT take the crown, so the seat is one nothing else on the ability
  -- names. Under the You relation it is also CR 109.5's "you", a second name for
  -- one player and no wrong answer; a per-CONDITION set cannot depend on the
  -- relation.
  --
  -- Unconditional: GameEvent.BecameMonarch carries a PlayerId outright.
  TriggerCondition.PlayerBecomesMonarch _ -> Set.singleton Binding.triggerPlayer
  -- Empty by decision: the permanent the event names is the one the condition's own
  -- SLOT already names, and Ray of Command's "tap it" reads that slot rather than
  -- anything the event bound. A slot for the player who GAINED control is what a
  -- card printing "that player" would earn; nothing prints one.
  TriggerCondition.LoseControlOfBound _ -> Set.empty
  -- Empty NECESSARILY rather than by choice: this condition admits no event, so
  -- there is none to read a slot out of. What a reflexive knows about the
  -- resolution that made it comes from CR 603.7c's captured environment instead,
  -- which Pawl.Engine.Resolve stamps into the entry as it is armed.
  TriggerCondition.Reflexive -> Set.empty

-- Whether a damage recipient is a player (CR 120.1): a total discriminator over
-- Recipient, so the combat-damage-to-player trigger matcher stays non-partial.
isPlayerRecipient :: Recipient.Recipient -> Bool
isPlayerRecipient r = case r of
  Recipient.ToPlayer _ -> True
  Recipient.ToCreature _ -> False
  Recipient.ToPlaneswalker _ -> False
  Recipient.ToBattle _ -> False
  Recipient.ToObject _ -> False

-- CR 603.10a: is this one of the conditions the game "looks back in time" for?
--
-- The rule states a CLOSED list of four families -- "leaves-the-battlefield
-- abilities, abilities that trigger when a player sacrifices a permanent,
-- abilities that trigger when a card leaves a graveyard, and abilities that
-- trigger when an object that all players can see is put into a hand or library"
-- -- and CR 603.10b through CR 603.10g add six more, none of which pawl has a
-- condition for. Everything else takes CR 603.10's first sentence.
--
-- A TOTAL case, with no wildcard, which is the whole reason this is a function
-- rather than a guard at the use site: a new condition must be READ against that
-- list, and a wildcard would silently give it whichever answer was convenient.
-- -Werror is what makes that a compile error rather than a rules bug.
--
-- Not a case on an EFFECT's identity. CR 603.10a enumerates trigger CONDITIONS,
-- the way rule 702 enumerates keywords, so asking which family a condition falls
-- in is reading the rulebook rather than reading a card.
--
-- What the answer decides is narrow: whether a bearer that left the battlefield
-- in the event's OWN group is offered for this ability. A bearer that left at a
-- later group is offered either way (CR 603.10's first sentence), and a
-- condition's own matcher still has the last word.
looksBack :: TriggerCondition -> Bool
looksBack condition = case condition of
  -- CR 603.10a is about a bearer that left the battlefield, and CR 309.2c keeps a
  -- dungeon card in the command zone until it leaves the game.
  TriggerCondition.RoomEntered _ -> False
  -- None of the four is on CR 603.10a's list, and none is a zone change at
  -- all: CR 701.22a moves cards within one library, CR 701.25a and CR 701.44a
  -- do move cards but their events say the keyword action COMPLETED rather
  -- than that anything left a zone, and CR 702.170b's exile is a card leaving
  -- a HAND. So CR 603.10's first sentence governs all four.
  TriggerCondition.PlayerScries _ -> False
  TriggerCondition.PlayerSurveils _ -> False
  TriggerCondition.SelfBecomesPlotted -> False
  TriggerCondition.PermanentExplores _ -> False
  -- Not on CR 603.10a's list, and CR 706.1's roll is no zone change: it moves
  -- no object at all, so CR 603.10's first sentence governs.
  TriggerCondition.PlayerRollsDice _ -> False
  -- The same answer once more, and the most plainly: CR 701.43c can only exert a
  -- permanent that is ON the battlefield, so nothing has changed zones.
  TriggerCondition.SelfExerted -> False
  -- CR 603.10a's list does not reach an attachment either. CR 701.3a moves a
  -- permanent ONTO another one without changing its zone, and the one route that
  -- is a zone change -- CR 608.3c's Aura spell arriving attached -- leaves both
  -- objects on the battlefield for a live read.
  TriggerCondition.SelfBecomesAttachedBy _ -> False
  -- CR 603.6c's two written forms, which CR 603.10a names first:
  -- leaves-the-battlefield abilities. CR 700.4 narrows the second to a
  -- graveyard, and narrowing the destination does not leave the family.
  TriggerCondition.SelfDies -> True
  TriggerCondition.PermanentDies _ -> True
  -- The batch reading of that same form is in the same family: CR 603.10a names
  -- leaves-the-battlefield abilities without counting them. Load-bearing rather
  -- than tidy -- it is what offers a bearer swept up in its own batch the
  -- group-mates that died beside it (CR 603.10a's own Example).
  TriggerCondition.PermanentsDie _ -> True
  TriggerCondition.SelfLeavesTheBattlefield -> True
  TriggerCondition.PermanentLeavesTheBattlefield _ -> True
  -- CR 603.10a's first family read off the HOST rather than the bearer: this
  -- triggers when a permanent leaves the battlefield, so the rule reaches the
  -- ability however the bearer is found.
  TriggerCondition.AttachedCreatureDies -> True
  -- Not on CR 603.10a's list at all: that rule names leaves-the-battlefield
  -- abilities, sacrifices, cards leaving a graveyard and objects put into a hand
  -- or library, and a permanent becoming tapped is none of them. It stays on the
  -- battlefield, so the ordinary CR 603.10 reading -- the board as it is now --
  -- is the right one.
  TriggerCondition.AttachedCreatureBecomesTapped -> False
  -- CR 603.10a's first family again, read off the event rather than off the
  -- bearer: this triggers when a permanent leaves the battlefield. Inert today --
  -- the bearer is a card in exile, which no look-back source can offer -- but a
  -- classification the rule decides, not the scan.
  TriggerCondition.HauntedCreatureDies -> True
  -- CR 603.10a's second family in as many words: "abilities that trigger when a
  -- player sacrifices a permanent".
  TriggerCondition.PermanentSacrificed -> True
  -- CR 603.1b: one ability, several conditions. It looks back if ANY of them
  -- does -- the ability is on the rule's list if the rule reaches it at all, and
  -- a condition that does not look back is unaffected, since its own matcher
  -- still has to admit the candidate.
  TriggerCondition.AnyOf conditions -> any looksBack conditions
  -- CR 603.6c's own last sentence puts this OUTSIDE the family: a card put into
  -- a graveyard from anywhere is not a leaves-the-battlefield ability, and CR
  -- 603.10's normal reading applies. The constructor's own Haddock argues it.
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> False
  -- Library to graveyard, which is on none of CR 603.10a's four families: a card
  -- leaving a LIBRARY is not a card leaving a graveyard, and the bearer this
  -- reads is the CR 400.7 incarnation that arrived.
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> False
  -- CR 708.8 leaves the permanent on the battlefield, so there is no departure
  -- for a look-back to recover and the live read is what CR 603.10's first
  -- sentence asks for. Both written forms.
  TriggerCondition.SelfTurnedFaceUp -> False
  TriggerCondition.PermanentTurnedFaceUp _ -> False
  -- CR 712.18 is the same claim about transforming, and states it outright: the
  -- permanent "doesn't become a new object", so there is no departure for CR
  -- 603.10a to look back at.
  TriggerCondition.SelfTransformedInto _ -> False
  -- CR 702.112b's designation is given to a permanent that stays where it is, so
  -- there is no departure here either.
  TriggerCondition.PermanentBecomesDesignated {} -> False
  -- Nor here: rule 702.100b's counters are put on a permanent on the battlefield.
  TriggerCondition.SelfEvolves -> False
  -- Nor here, for the same reason one rule over: rule 702.134a's counter goes on a
  -- creature that CR 508.1k has made an attacking creature, and a permanent leaving
  -- the battlefield is removed from combat (CR 506.4) rather than mentored.
  TriggerCondition.AttachedCreatureMentors -> False
  -- Nor here, and by the same sentence: rule 702.149a's counter goes on an
  -- attacking creature, which CR 506.4 has removed from combat if it left.
  TriggerCondition.SelfTrains -> False
  -- Entries, not departures (CR 603.6a). The rule's own CR 603.6a checks "all
  -- permanents on the battlefield (including the newcomers)" AFTER the event.
  TriggerCondition.SelfEnters -> False
  TriggerCondition.PermanentEnters _ -> False
  -- Turn structure, the stack, damage, life, counters and the rest: none names a
  -- zone change at all, so CR 603.10a's list cannot reach them.
  TriggerCondition.StepBegins {} -> False
  TriggerCondition.StateIs _ -> False
  TriggerCondition.SelfDealsCombatDamageToPlayer -> False
  TriggerCondition.SelfIsDealtDamage -> False
  TriggerCondition.PermanentDealsCombatDamageToPlayer _ -> False
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
  TriggerCondition.OpponentLostLifeDuringYourTurn -> False
  TriggerCondition.SelfCycled -> False
  TriggerCondition.SelfRevealedForMiracle -> False
  TriggerCondition.SelfDiscarded -> False
  TriggerCondition.PlayerDiscards _ -> False
  TriggerCondition.PlayerCycles _ -> False
  TriggerCondition.PlayerDrawsNthCard {} -> False
  TriggerCondition.SelfAttacks _ -> False
  TriggerCondition.SelfAttacksWithAnother _ -> False
  TriggerCondition.CreatureAttacksAlone _ -> False
  TriggerCondition.CreatureAttacksYou -> False
  TriggerCondition.AttachedPlayerIsAttacked -> False
  TriggerCondition.PlayerAttacks _ -> False
  TriggerCondition.PlayerAttacksWith {} -> False
  TriggerCondition.PlayerAttacksPlayer {} -> False
  TriggerCondition.SelfAttacksPlayerWithMostLife -> False
  TriggerCondition.SelfBlocks -> False
  TriggerCondition.SelfBlocksCreature _ -> False
  TriggerCondition.SelfBlocksAtLeast _ -> False
  TriggerCondition.SelfBlocksOneOrMore _ -> False
  TriggerCondition.SelfBecomesBlocked -> False
  TriggerCondition.SelfBecomesBlockedBy _ -> False
  TriggerCondition.SelfBecomesBlockedByOneOrMore _ -> False
  TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> False
  TriggerCondition.SelfAttacksUnblocked -> False
  TriggerCondition.SpellOrAbilityCounters _ -> False
  TriggerCondition.DamageToPlayerPrevented _ -> False
  TriggerCondition.SelfPreventsDamage -> False
  TriggerCondition.PlayerGainsLife _ -> False
  TriggerCondition.PlayerLosesLife _ -> False
  TriggerCondition.SelfCountersReached {} -> False
  TriggerCondition.SelfBecomesClassLevel _ -> False
  TriggerCondition.SelfLastCounterRemoved _ -> False
  TriggerCondition.SelfCountersRemoved _ -> False
  TriggerCondition.SpellCast {} -> False
  TriggerCondition.SelfCast -> False
  TriggerCondition.SelfBecomesTargeted _ -> False
  TriggerCondition.ControllerBecomesTarget {} -> False
  TriggerCondition.SelfHalfUnlocked _ -> False
  TriggerCondition.RoomFullyUnlocked _ -> False
  -- CR 603.3b's second class names no zone change at all -- its event is another
  -- ability triggering -- so CR 603.10a's four families cannot reach it. Its
  -- bearer is a permanent standing on the battlefield watching a Saga, and CR
  -- 603.10's first sentence is what reads it.
  TriggerCondition.SagaFinalChapterTriggers _ -> False
  -- CR 725.1's crowning names no zone change either -- it moves a DESIGNATION,
  -- not an object -- so none of CR 603.10a's four families reaches it.
  TriggerCondition.PlayerBecomesMonarch _ -> False
  -- CR 603.10a's look-back is for a bearer that left; this condition's subject is a
  -- permanent still on the battlefield, Engine.sampleControl sampling nowhere else,
  -- and its bearer is a CR 603.7 delayed entry the store keeps rather than the log.
  TriggerCondition.LoseControlOfBound _ -> False
  -- CR 603.10a's look-back is a question about which event fired the ability, and
  -- a reflexive is fired by none. Its bearer is a CR 603.7 delayed entry too.
  TriggerCondition.Reflexive -> False

-- CR 603.2c, the second sentence: does this condition's trigger event CONTAIN the
-- occurrences, or IS each occurrence its trigger event? "Whenever one or more
-- other creatures you control die" names the whole CR 704.3 / CR 608.2f batch, so
-- a sweep that buries three contains one occurrence of it; "whenever another
-- creature you control dies" names each death, so the same sweep contains three,
-- which is the rule's own Example.
--
-- Read by eventTriggers alone, and only to decide how many pending triggers one
-- Pawl.Types.EventGroup may yield per (bearer, ability). matchesTriggerGiven sees
-- one event at a time and so answers the same for both readings -- that is its
-- contract, and this predicate is what keeps it intact.
--
-- A total case over TriggerCondition and never a wildcard, for looksBack's reason:
-- the fork is one CR 603.2c forces on every zone-change and event-watching
-- condition, so a new one must be classified rather than defaulted. Everything but
-- the batch reading is per-occurrence, which is what the rule's first sentence
-- makes the default -- but a future "whenever one or more" printing on any other
-- event would answer True here, so the arms are written out rather than folded.
--
-- CR 603.1b lets one ability carry several conditions, and `any` makes such an
-- ability batch-scoped as a whole: a mixed AnyOf would fire at most once per group
-- rather than once per group plus once per member. Nothing in data/cards mixes
-- the two readings, and CardSpec's anyOfOffends already narrows what an AnyOf may
-- hold; a card that did would need this arm re-derived rather than reused.
batchScoped :: TriggerCondition -> Bool
batchScoped condition = case condition of
  TriggerCondition.RoomEntered _ -> False
  TriggerCondition.PlayerScries _ -> False
  TriggerCondition.PlayerSurveils _ -> False
  TriggerCondition.SelfBecomesPlotted -> False
  TriggerCondition.PermanentExplores _ -> False
  -- Per-occurrence, and today indistinguishable from the batch reading of
  -- Feywild Trickster's "one or more dice": CR 706.1's other half, how MANY
  -- dice, is unimplemented (#2085), so one Effect.RollDie records exactly one
  -- event and the ability can fire at most once either way. A card rolling
  -- several dice at once would have to answer True here.
  TriggerCondition.PlayerRollsDice _ -> False
  TriggerCondition.SelfExerted -> False
  TriggerCondition.SelfBecomesAttachedBy _ -> False
  TriggerCondition.SelfDies -> False
  TriggerCondition.PermanentDies _ -> False
  TriggerCondition.PermanentsDie _ -> True
  TriggerCondition.SelfLeavesTheBattlefield -> False
  TriggerCondition.PermanentLeavesTheBattlefield _ -> False
  TriggerCondition.AttachedCreatureDies -> False
  -- CR 603.2e names the MOMENT a permanent becomes tapped, and a moment holds one
  -- occurrence; no printing of that event says "one or more".
  TriggerCondition.AttachedCreatureBecomesTapped -> False
  TriggerCondition.HauntedCreatureDies -> False
  TriggerCondition.PermanentSacrificed -> False
  TriggerCondition.AnyOf conditions -> any batchScoped conditions
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> False
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> False
  TriggerCondition.SelfTurnedFaceUp -> False
  TriggerCondition.PermanentTurnedFaceUp _ -> False
  TriggerCondition.SelfTransformedInto _ -> False
  TriggerCondition.PermanentBecomesDesignated {} -> False
  TriggerCondition.SelfEvolves -> False
  TriggerCondition.AttachedCreatureMentors -> False
  TriggerCondition.SelfTrains -> False
  TriggerCondition.SelfEnters -> False
  TriggerCondition.PermanentEnters _ -> False
  TriggerCondition.StepBegins {} -> False
  TriggerCondition.StateIs _ -> False
  TriggerCondition.SelfDealsCombatDamageToPlayer -> False
  TriggerCondition.SelfIsDealtDamage -> False
  TriggerCondition.PermanentDealsCombatDamageToPlayer _ -> False
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
  TriggerCondition.OpponentLostLifeDuringYourTurn -> False
  TriggerCondition.SelfCycled -> False
  TriggerCondition.SelfRevealedForMiracle -> False
  TriggerCondition.SelfDiscarded -> False
  TriggerCondition.PlayerDiscards _ -> False
  TriggerCondition.PlayerCycles _ -> False
  TriggerCondition.PlayerDrawsNthCard {} -> False
  TriggerCondition.SelfAttacks _ -> False
  TriggerCondition.SelfAttacksWithAnother _ -> False
  TriggerCondition.CreatureAttacksAlone _ -> False
  TriggerCondition.CreatureAttacksYou -> False
  TriggerCondition.AttachedPlayerIsAttacked -> False
  TriggerCondition.PlayerAttacks _ -> False
  TriggerCondition.PlayerAttacksWith {} -> False
  TriggerCondition.PlayerAttacksPlayer {} -> False
  TriggerCondition.SelfAttacksPlayerWithMostLife -> False
  TriggerCondition.SelfBlocks -> False
  TriggerCondition.SelfBlocksCreature _ -> False
  -- FALSE despite naming batches in the RULES, which is the one group of answers
  -- here that is not what it looks like. Rule 509.3e's "one or more" and rule
  -- 508.3b's are already once-per-declaration, structurally: their events
  -- (GameEvent.BlocksDeclared, GameEvent.AttackerBlocked, GameEvent.BecameAttacked)
  -- are minted at that arity by Pawl.Engine.Combat, the one emitter that sees a
  -- whole declaration, so there is nothing left for this predicate to dedup and
  -- True would be a second dedup over an already-unique trigger. Deaths get the
  -- other treatment because they reach a graveyard from four places and no event
  -- carries the arity; GameEvent.BecameAttacked's own haddock draws that line.
  TriggerCondition.SelfBlocksAtLeast _ -> False
  TriggerCondition.SelfBlocksOneOrMore _ -> False
  TriggerCondition.SelfBecomesBlocked -> False
  TriggerCondition.SelfBecomesBlockedBy _ -> False
  TriggerCondition.SelfBecomesBlockedByOneOrMore _ -> False
  TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> False
  TriggerCondition.SelfAttacksUnblocked -> False
  TriggerCondition.SpellOrAbilityCounters _ -> False
  TriggerCondition.DamageToPlayerPrevented _ -> False
  -- Per PREVENTION, which is what the record already is: groupPreventions
  -- collapsed the batch to one entry per applying instance, so rule 615.13's "one
  -- or more simultaneous damage events" is spent before this is asked -- the
  -- DamageToPlayerPrevented arm above's reasoning, one identity over.
  TriggerCondition.SelfPreventsDamage -> False
  TriggerCondition.PlayerGainsLife _ -> False
  TriggerCondition.PlayerLosesLife _ -> False
  TriggerCondition.SelfCountersReached {} -> False
  TriggerCondition.SelfBecomesClassLevel _ -> False
  TriggerCondition.SelfLastCounterRemoved _ -> False
  TriggerCondition.SelfCountersRemoved _ -> False
  TriggerCondition.SpellCast {} -> False
  TriggerCondition.SelfCast -> False
  TriggerCondition.SelfBecomesTargeted _ -> False
  TriggerCondition.ControllerBecomesTarget {} -> False
  TriggerCondition.SelfHalfUnlocked _ -> False
  TriggerCondition.RoomFullyUnlocked _ -> False
  TriggerCondition.SagaFinalChapterTriggers _ -> False
  TriggerCondition.PlayerBecomesMonarch _ -> False
  TriggerCondition.LoseControlOfBound _ -> False
  TriggerCondition.Reflexive -> False

-- CR 603.6a: every event is checked against every permanent currently on the
-- battlefield, not only the object the event names -- a step trigger belongs to a
-- permanent with nothing to do with the event.
--
-- "Currently" is the word CR 603.10's first sentence corrects, and the battlefield
-- reading here is per EVENT GROUP for that reason: each group's permanents, their
-- abilities and their controllers are the ones GameState.battlefieldWhenTriggered
-- sampled as the group was recorded, never the board the scan finds. The sample is
-- one projectAll per group rather than per (event, permanent) pair --
-- Projection.project reruns the whole-board `gather` fold on every call, which made
-- this scan quadratic in board size.
--
-- The battlefield is not the only scanned zone -- every GRAVEYARD and the whole
-- EXILE zone are scanned for the abilities CR 113.6k puts there, a spell that
-- just became cast is offered from the STACK for the same rule, the card a player
-- revealed as they drew it is offered from their HAND for it too, and an EMBLEM is
-- offered from the command zone under CR 114.4. The rest of the command zone is
-- unscanned: the only other thing it holds is a dungeon card, whose room
-- abilities CR 309.4c mints rather than prints, leaving this scan nothing on a
-- face to read.
-- Pawl.Engine.Dungeon.roomPending gathers those.
--
-- Two holes are left in the BATTLEFIELD half of that reading, and last known
-- information fills both. A permanent that left WITHIN its own group is missing
-- from that group's sample, the sample being taken as the last of the group's
-- members is recorded and CR 704.3's whole destruction batch being one group; and
-- a group nothing sampled has no reading but the live board, which the departed
-- are not on either.
--
-- So each event ALSO contributes the permanents that left the battlefield at a
-- LATER EVENT GROUP in the same batch, read from CR 608.2h last known information
-- -- `laterGroups` below, which the sample outranks wherever both hold the same id.
-- Four things make that exact rather than approximate:
--
--   * The same reading, one event later. A permanent removed by a later event
--     existed immediately after this one, which is what the rule asks. It reaches
--     the event's own newcomer for free: a creature entering as a 0/0 and buried
--     by CR 704.5f leaves at a later group than its entry.
--   * No double fire, structurally: `lastKnown` is written by the zone change that
--     DELETES an id, and CR 400.7 mints a fresh id per move, so no id is in both.
--   * The right snapshot: `lastKnown` holds the permanent as it was on the
--     battlefield, continuous effects applied, which CR 603.10 demands.
--   * A canonical place in the order: candidates are a Map keyed by ObjectId and
--     traversed ascending, so extras sort in rather than being appended.
--
-- CR 603.10a is the other half of that rule, the exception rather than the normal
-- case, and it contributes twice. A DEPARTURE event contributes the permanent it
-- took off the battlefield (`leftBattlefield`), and every event contributes the
-- permanents that left at its OWN group (`sameGroup`) -- for a look-back
-- condition alone, since only those read "the appearance of objects immediately
-- prior to the event". For both, the last-known reading is what the rule asks for
-- rather than a repair for a late boundary.
--
-- Which conditions those are is `looksBack`, a total case over TriggerCondition
-- and never a wildcard: CR 603.10a states a closed list, so a condition the rule
-- has not been read against must be classified rather than defaulted.
--
-- The GRAVEYARD half has a hole of the same shape, and last known information
-- fills it the same way: a card the batch put into a graveyard and took back out
-- again is not in the graveyard the boundary scan walks, so `arrivedInGraveyard`
-- offers it to its own arrival event from CR 608.2h. The other direction of that
-- half is narrowed rather than widened: the graveyard the boundary scan walks
-- holds cards the batch put there, and `arrivedLater` withholds each of them from
-- every event of a strictly earlier group, which is the same per-event reading
-- `laterGroups` gives the battlefield.
--
-- Not reconstructed: a permanent that ENTERED later in the batch and left before
-- the boundary is still offered to the batch's earlier events (#441). Nor is a
-- departed graveyard card offered to any event but its own arrival: the three
-- conditions zonesTriggeredFrom sends to a graveyard are self-referential arrival
-- conditions whose only matching event IS that arrival, so the only ability that
-- could observe the difference is a CR 113.6m one -- Squee, Goblin Nabob's
-- upkeep, read from a graveyard (#1732).
--
-- Events outer, permanents inner (ascending by id): the deterministic canonical
-- order the CR 603.3b ordering prompt indexes into. Groups do not disturb it --
-- they decide which permanents an event is offered, never in what order.
--
-- CR 603.2c's batch conditions do not disturb it either, which is why they are a
-- DEDUP (`oncePerBatch` below) rather than a second pass: dropping every match
-- after a group's first leaves each surviving trigger at the position its earliest
-- matching event gave it, where appending the batch's triggers to the block would
-- have moved them.
eventTriggers :: [LoggedEvent.LoggedEvent] -> GameState -> [PendingTrigger]
eventTriggers events gs =
  let -- CR 702.70a: a keyword can BE a triggered ability, so a permanent's
      -- abilities are its printed-and-granted ones plus the ones rule 702 mints
      -- from its keywords. Derived from POST-LAYER counts, so Humility takes them
      -- away and a layer-6 grant adds them without special-casing. Shared by both
      -- candidate sources, so a live and a last-known permanent read alike.
      -- CR 310.12b's Siege ability is minted the same way and for the same reason,
      -- off the same finished projection: rule 310 gives it, no card prints it, and
      -- the scan below never learns which rule produced any of these.
      --
      -- Through Projection.mintedTriggeredAbilitiesOf rather than
      -- Keyword.triggeredAbilitiesOf directly, so CR 612.2a's text change reaches
      -- the words rule 702 writes -- the Spirit an afterlife trigger creates. Rule
      -- 310.12b's Siege ability names no subtype word, so it needs no such wrapper.
      abilitiesOf pc = PC.triggeredAbilities pc <> Projection.mintedTriggeredAbilitiesOf pc <> Battle.triggeredAbilitiesOf pc
      -- CR 113.6m's "functions ONLY in that zone", asked of a permanent read AS
      -- BEING ON THE BATTLEFIELD: a Squee, Goblin Nabob standing there does not
      -- see its own upkeep, because the ability that watches for it functions in
      -- the graveyard. The mirror of the filter
      -- Pawl.Engine.Activate.abilitiesForGiven puts on its battlefield arm.
      --
      -- Applied to BOTH readings that say "this permanent was on the
      -- battlefield": the live `onBattlefield` set, and the two group-scoped
      -- unions below (`laterGroups` and `sameGroup`), which recover a permanent
      -- that WAS on the battlefield at this event and has left by the CR 117.5
      -- boundary. Last known information (CR 608.2h) is how that permanent is
      -- read, not a statement about which zone it is being read IN, so the zone
      -- CR 113.6m compares against is the battlefield either way.
      --
      -- NOT applied to `leftBattlefield` -- CR 603.10a's look-back at the
      -- permanent THIS event removed. That is the same shape asking a different
      -- question, and CR 113.6m answers it differently: the rule's own "unless
      -- its trigger condition ... specifies that the object is put into that
      -- zone" exempts the dies triggers that arm serves, and THAT half of the
      -- clause is not implemented (#819) -- `enchantedObjectLeaves` below reads
      -- only its Aura half -- so filtering there would read the rule's first
      -- sentence without the exception that governs this arm.
      battlefieldAbilitiesOf pc = filter (functionsIn Zone.Battlefield) (abilitiesOf pc)
      -- CR 603.10's first sentence, per EVENT GROUP: the permanents that existed
      -- immediately after the event, with the abilities and the CR 603.3a
      -- controller each of them had THEN. The three readings the live board gets
      -- wrong are the three this recovers, and each has a board that tells them
      -- apart: a permanent that entered later in the batch was not there to be
      -- checked against an earlier event, one whose abilities were stripped after
      -- the event still triggered on it (Pawl.TriggerSpec's "abilities as of the
      -- event"), and one whose layer-2 controller changed after it (CR 514.2's
      -- "until end of turn" ending between CR 514.1's discard and CR 514.3a's
      -- placement) triggered under the old one.
      --
      -- The LIVE board answers for a group the sample does not name, which is the
      -- honest reading for an event this module never recorded: a fixture that
      -- appends to the log directly has no sampled board, and the game as it stands
      -- is the only one there is. Lazy, so a scan whose every group is sampled
      -- never projects it.
      liveBattlefield = battlefieldCandidates gs
      -- Out of the record and into the pair the rest of the scan's unions use.
      -- Only the STORED shape is named (#126); `candidates` below unions this
      -- with six sibling maps that are computed here and never serialized.
      onBattlefieldAt group =
        Map.map
          (\candidate -> (BattlefieldCandidate.controller candidate, battlefieldAbilitiesOf (BattlefieldCandidate.characteristics candidate)))
          (Map.findWithDefault liveBattlefield group (GameState.battlefieldWhenTriggered gs))
      -- The permanent this event took OFF the battlefield, read from
      -- CR 608.2h last known information -- both the abilities and the objects'
      -- appearance immediately prior to the event, which is what CR 603.10 says
      -- looking back means. Both live in the single `lastKnown` record, written
      -- from the pre-move state by the zone change that deleted the id, so the
      -- ability is read as it existed on the battlefield and CR 603.3a's
      -- controller is who controlled the permanent as it left.
      --
      -- Possible only because Moved names BOTH ids: `object` is the CR 400.7
      -- incarnation in the destination zone, which `lastKnown` knows nothing
      -- about, while `departed` is the key it files under.
      --
      -- Keyed by that departing id, which by construction no longer exists, so
      -- this source collides with no other -- one entry per id means one pass of
      -- `forOne` without leaning on Map.unions' bias.
      --
      -- EVERY battlefield departure contributes, not only the deaths: which
      -- destinations a condition accepts is the CONDITION's business, and keeping
      -- that out of the candidate source is what let CR 603.6c's wider "leaves the
      -- battlefield" arrive as a matcher arm alone.
      --
      -- The `to /= Battlefield` guard is CR 603.6c's own word "another": the
      -- pseudo-move recordTokenEntry emits for a new token is not a departure.
      --
      -- The departing id is what the placed trigger carries as its SOURCE (CR
      -- 113.7a). CR 603.6c's arriving incarnation is a SECOND slot rather than a
      -- different value in this one -- eventBindings binds it under `became`.
      --
      -- Empty for a permanent that ceased without a zone change running over it,
      -- which files no last known information. That hole is the two group-scoped
      -- unions' too.
      --
      -- Parameterized by which of the departed permanent's abilities to offer,
      -- because the two callers below want different sets out of one recovery:
      -- `leftBattlefield` is CR 603.10a's look-back at the event's own departure
      -- and takes them all, while `laterGroups` and `sameGroup` read a permanent
      -- that was standing on the battlefield when the event happened and so take
      -- only the ones CR 113.6m leaves functioning there.
      departedFrom pick event = case event of
        GameEvent.Moved (Moved.MkMoved zc _)
          | ZoneChange.from zc == Zone.Battlefield && ZoneChange.to zc /= Zone.Battlefield ->
              case Map.lookup (ZoneChange.departed zc) (GameState.lastKnown gs) of
                Nothing -> Map.empty
                Just lk ->
                  Map.singleton
                    (ZoneChange.departed zc)
                    (LastKnown.controller lk, pick (LastKnown.characteristics lk))
        -- CR 800.4a's removal, recovered the same way and from the same record:
        -- the permanent is not on the battlefield at the CR 117.5 boundary
        -- because it is not in the game at all, so last known information is the
        -- only reading of it there is. The id is its own -- nothing was minted
        -- for it to become -- and it too no longer exists, so this collides with
        -- no other source.
        GameEvent.LeftTheGame oid -> case Map.lookup oid (GameState.lastKnown gs) of
          Nothing -> Map.empty
          Just lk -> Map.singleton oid (LastKnown.controller lk, pick (LastKnown.characteristics lk))
        GameEvent.Milled {} -> Map.empty
        GameEvent.Scried _ -> Map.empty
        GameEvent.Surveiled _ -> Map.empty
        GameEvent.DiceRolled _ -> Map.empty
        GameEvent.ClassLevelSet _ -> Map.empty
        GameEvent.Plotted _ -> Map.empty
        GameEvent.Explored _ -> Map.empty
        GameEvent.Exerted _ -> Map.empty
        GameEvent.BecameAttacked _ -> Map.empty
        GameEvent.AttackersDeclared _ -> Map.empty
        GameEvent.BecameTapped _ -> Map.empty
        GameEvent.Moved {} -> Map.empty
        GameEvent.DamageDealt _ -> Map.empty
        GameEvent.DamagePrevented {} -> Map.empty
        GameEvent.StepBegan {} -> Map.empty
        GameEvent.SpellCast {} -> Map.empty
        GameEvent.BecameMonarch _ -> Map.empty
        GameEvent.Discarded {} -> Map.empty
        GameEvent.Drew {} -> Map.empty
        GameEvent.Revealed {} -> Map.empty
        GameEvent.AttackerDeclared {} -> Map.empty
        GameEvent.BecameBlocking {} -> Map.empty
        GameEvent.BlocksDeclared {} -> Map.empty
        GameEvent.AttackerBlocked {} -> Map.empty
        GameEvent.AttackerUnblocked _ -> Map.empty
        GameEvent.SpellCountered _ -> Map.empty
        GameEvent.HalfUnlocked {} -> Map.empty
        GameEvent.TurnedFaceUp _ -> Map.empty
        GameEvent.Transformed {} -> Map.empty
        GameEvent.BecameDesignated {} -> Map.empty
        GameEvent.Evolved _ -> Map.empty
        GameEvent.Mentored {} -> Map.empty
        GameEvent.Trained _ -> Map.empty
        GameEvent.PermanentSacrificed {} -> Map.empty
        GameEvent.AbilityTriggered {} -> Map.empty
        GameEvent.LoyaltyAbilityActivated _ -> Map.empty
        GameEvent.LifeLost {} -> Map.empty
        GameEvent.LifeGained {} -> Map.empty
        GameEvent.CountersPut {} -> Map.empty
        GameEvent.CountersRemoved {} -> Map.empty
        GameEvent.ControlChanged {} -> Map.empty
        GameEvent.VentureMarkerEntered {} -> Map.empty
        GameEvent.BecameTarget {} -> Map.empty
        GameEvent.BecameAttached {} -> Map.empty
      -- CR 603.10a's look-back at the permanent this event removed: every
      -- ability it had, unfiltered, for the reason `battlefieldAbilitiesOf`
      -- above gives.
      leftBattlefield = departedFrom abilitiesOf
      -- CR 400.7f's own datum, and the one thing `eventBindings` is told that it
      -- could not read off the event it was handed: for each permanent this batch
      -- put from the battlefield into a graveyard, the id it BECAME there.
      -- `departed` is the key because that is the id a borne trigger carries as
      -- its source (CR 113.7a), so `pend` below can ask it about the bearer.
      --
      -- Battlefield to GRAVEYARD alone, that being the only destination the rule
      -- names ("in its owner's graveyard"); every other departure contributes
      -- nothing, and a bearer with no entry gets Nothing.
      --
      -- The whole scanned batch rather than one group, because the rule's second
      -- sentence is the CR 704.5m burial, which happens at a strictly later SBA
      -- pass than the host's death. No wider than the rule even so: a permanent
      -- whose graveyard arrival was EARLIER than the event was not on the
      -- battlefield when it happened, so none of the candidate sources below
      -- offers its abilities and no trigger of its is ever gathered to ask about.
      becameInGraveyard =
        Map.fromList
          ( Maybe.mapMaybe
              ( ( \event -> case event of
                    GameEvent.Moved (Moved.MkMoved zc _)
                      | ZoneChange.from zc == Zone.Battlefield && ZoneChange.to zc == Zone.Graveyard ->
                          Just (ZoneChange.departed zc, ZoneChange.object zc)
                    _ -> Nothing
                )
                  . LoggedEvent.event
              )
              events
          )
      -- The batch cut into its CR 704.3 / CR 608.2f events. Groups are
      -- non-decreasing along the log (Event.recordEvent only mints a fresh one or
      -- repeats the frozen one), so the members of one event are contiguous and
      -- an adjacent grouping is exact rather than approximate.
      groups = List.groupBy (\a b -> LoggedEvent.group a == LoggedEvent.group b) events
      departuresIn block = Map.unions (fmap (departedFrom battlefieldAbilitiesOf . LoggedEvent.event) block)
      -- CR 603.10's first sentence, per EVENT GROUP: the permanents still on the
      -- battlefield when each event happened that have left by the CR 117.5
      -- boundary. Entry i is the union of `leftBattlefield` over the events at a
      -- strictly LATER group -- so neither an event's own departure nor a
      -- SIMULTANEOUS one is in its entry. That is not an optimisation: a
      -- permanent removed by this same event does not exist immediately after it,
      -- and reaches it only through the look-back below.
      --
      -- Strictly later GROUP, never strictly later index: two events of one group
      -- happened at the same time, so neither is "after" the other, and ordering
      -- them by the order the implementation happened to record them in is the
      -- order-dependence this whole binding exists to remove (#615).
      --
      -- A right scan over the groups rather than a lookup table: scanr shares each
      -- suffix's union with the one before, so the batch costs one pass, and a
      -- whole-board combat death batch that collapses into ONE group now costs one
      -- union rather than one per death. `drop 1` is the alignment, shifting
      -- scanr's "from i onward" to "from i+1 onward".
      --
      -- The controller and abilities here are the ones the permanent had as it
      -- LEFT, one moment after the event that triggered them rather than at it --
      -- so this is the SECOND reading of such a permanent, and loses to the first.
      -- `onBattlefieldAt` above already holds it, sampled at the event itself,
      -- because a permanent that departs at a later group was still standing when
      -- this group's sample was taken; Map.unions is left-biased and that entry
      -- comes first. What is left for this binding is the group the sample does not
      -- name, where last known information is the only reading there is.
      --
      -- CR 113.6m applies here and not to `leftBattlefield`: this permanent WAS
      -- on the battlefield when the event happened, so one of its abilities that
      -- functions only in a graveyard was no more watching then than it is now.
      -- The behaviour's proving case is Squee, Goblin Nabob leaving the battlefield
      -- after an upkeep began in the same batch, in Pawl.TriggerSpec's
      -- `bystanderZoneSpec` -- which reaches the same answer through the sample
      -- above, that being the reading that wins. The filter is kept identical here
      -- so the two cannot disagree on the fallback path.
      laterGroups = drop 1 (List.scanr (Map.union . departuresIn) Map.empty groups)
      -- The ids a graveyard arrival in this block minted, keyed by the ARRIVING
      -- incarnation -- ZoneChange.object, the key `inGraveyards` would hold them
      -- under, `arrivedInGraveyard` below arguing that the two ids coincide.
      arrivalsIn block = Set.fromList (Maybe.mapMaybe (arrivedInGraveyardAt . LoggedEvent.event) block)
      arrivedInGraveyardAt event = case movedOf event of
        Just zc | ZoneChange.to zc == Zone.Graveyard -> Just (ZoneChange.object zc)
        _ -> Nothing
      -- CR 603.10's first sentence on the ARRIVAL side, and `laterGroups`' mirror:
      -- a card that reached a graveyard at a STRICTLY LATER group did not exist
      -- immediately after this group's events, so it is no witness to them and is
      -- subtracted from `inGraveyards` below. Strictly later for `laterGroups`'
      -- reason -- two events of one group happened at the same time, so a card
      -- buried by an event of the SAME group did exist immediately after it, and
      -- keeping the two boundaries aligned is what stops the arrival and departure
      -- narrowings from disagreeing about simultaneity.
      --
      -- Subtracting from the live read rather than reconstructing each group's
      -- graveyard: a card that was in a graveyard BEFORE the batch has no arrival
      -- event in the log this scan reads, so it is in no entry here and survives
      -- every subtraction, which is the answer the rule wants. A Set and a right
      -- scan for `laterGroups`' reasons, `drop 1` being the same alignment.
      --
      -- `arrivedInGraveyard` is NOT narrowed by this: it is already per event and
      -- already scoped by the arrival it answers for, so subtracting the arrival
      -- from its own event would delete the case that source exists for.
      arrivedLater = drop 1 (List.scanr (Set.union . arrivalsIn) Set.empty groups)
      -- CR 603.10a, the other half of that rule: for a LOOK-BACK condition the
      -- board that matters is "the appearance of objects immediately prior to the
      -- event", on which every permanent this same event removed was still
      -- standing. So a bearer that departed in the event's OWN group is offered
      -- alongside the strictly-later ones.
      --
      -- CR 603.10a's own Example is this and nothing else: an artifact watching
      -- creatures die "triggers twice, even though the artifact goes to its
      -- owner's graveyard at the same time as the creatures". Meren of Clan Nel
      -- Toth dying to Day of Judgment beside a Goblin Piker is that Example with
      -- an experience counter in place of the life, and Pawl.TriggerSpec proves
      -- it for BOTH object-id orders.
      --
      -- Narrowed to the conditions that rule lists, by `looksBack` below, and
      -- narrowed HERE rather than at the match: admitting the bearer for a
      -- CR 603.10 first-sentence condition would say a permanent existed
      -- immediately after the event that removed it. The other direction is the
      -- one that would answer the sequential case wrong -- a Meren destroyed by
      -- one part of a resolution must not see creatures buried by a LATER part,
      -- which is a different group and so absent from this entry.
      --
      -- Not filtered against the entry above: an id departs at exactly one group,
      -- so the two maps are disjoint, and `leftBattlefield`'s unfiltered offer of
      -- the event's own departure wins over this one by Map.unions' left bias.
      sameGroup = fmap (Map.map (fmap (filter (looksBack . TriggeredAbility.condition))) . departuresIn) groups
      -- CR 702.29c: the card that was just cycled, wherever it landed. The
      -- candidate source that is neither on the battlefield nor a permanent that
      -- left it -- which is exactly what that rule asks for:
      -- "these abilities trigger from whatever zone the card winds up in after
      -- it's cycled", the graveyard for every printing today.
      --
      -- Abilities come from the PRINTED card rather than a projection, no pool
      -- effect changing the TRIGGERED abilities of a card in a graveyard (#1859) --
      -- Teferi, Mage of Zhalfir's grant off the battlefield mints none. Rule 702's
      -- minted abilities are not consulted either -- none functions from a
      -- graveyard.
      --
      -- The controller is the OWNER, CR 113.8's second clause: a card in a
      -- graveyard has no controller (CR 108.4).
      --
      -- Scoped to the CYCLING cause, not every discard, rule 702.29c speaking
      -- about cycling specifically. An ordinary discard's card reaches the
      -- graveyard too and is offered by `inGraveyards` under CR 113.6k.
      cycledCard event = case event of
        GameEvent.Discarded (Discarded.MkDiscarded _ oid DiscardCause.ToPayCyclingCost) -> case Game.lookupObject oid gs of
          Nothing -> Map.empty
          Just obj -> case Game.faceOf oid gs of
            Nothing -> Map.empty
            Just face -> Map.singleton oid (Object.owner obj, Face.triggeredAbilities face)
        GameEvent.Discarded (Discarded.MkDiscarded _ _ DiscardCause.Ordinary) -> Map.empty
        -- A draw names no object either. The card it puts in a hand may well bear
        -- an ability that triggers from there -- CR 702.94a's miracle -- but that
        -- one fires on the REVEAL rather than on the draw, and `revealedInHand`
        -- below is its source.
        GameEvent.Drew {} -> Map.empty
        GameEvent.Moved {} -> Map.empty
        GameEvent.DamageDealt _ -> Map.empty
        GameEvent.DamagePrevented {} -> Map.empty
        GameEvent.StepBegan {} -> Map.empty
        GameEvent.SpellCast {} -> Map.empty
        GameEvent.BecameMonarch _ -> Map.empty
        -- A reveal is not a cycling, whatever it showed. `revealedInHand` below is
        -- what hangs an ability on the card a reveal names.
        GameEvent.Revealed {} -> Map.empty
        GameEvent.AttackerDeclared {} -> Map.empty
        GameEvent.BecameBlocking {} -> Map.empty
        GameEvent.BlocksDeclared {} -> Map.empty
        GameEvent.AttackerBlocked {} -> Map.empty
        GameEvent.AttackerUnblocked _ -> Map.empty
        GameEvent.SpellCountered _ -> Map.empty
        GameEvent.HalfUnlocked {} -> Map.empty
        GameEvent.TurnedFaceUp _ -> Map.empty
        GameEvent.Transformed {} -> Map.empty
        GameEvent.BecameDesignated {} -> Map.empty
        GameEvent.Evolved _ -> Map.empty
        GameEvent.Mentored {} -> Map.empty
        GameEvent.Trained _ -> Map.empty
        GameEvent.PermanentSacrificed {} -> Map.empty
        GameEvent.AbilityTriggered {} -> Map.empty
        GameEvent.LoyaltyAbilityActivated _ -> Map.empty
        GameEvent.LifeLost {} -> Map.empty
        GameEvent.LifeGained {} -> Map.empty
        GameEvent.CountersPut {} -> Map.empty
        GameEvent.CountersRemoved {} -> Map.empty
        GameEvent.ControlChanged {} -> Map.empty
        GameEvent.VentureMarkerEntered {} -> Map.empty
        GameEvent.BecameTarget {} -> Map.empty
        GameEvent.BecameAttached {} -> Map.empty
        GameEvent.LeftTheGame _ -> Map.empty
        GameEvent.Milled {} -> Map.empty
        GameEvent.Scried _ -> Map.empty
        GameEvent.Surveiled _ -> Map.empty
        GameEvent.DiceRolled _ -> Map.empty
        GameEvent.ClassLevelSet _ -> Map.empty
        GameEvent.Plotted _ -> Map.empty
        GameEvent.Explored _ -> Map.empty
        GameEvent.Exerted _ -> Map.empty
        GameEvent.BecameAttacked _ -> Map.empty
        GameEvent.AttackersDeclared _ -> Map.empty
        GameEvent.BecameTapped _ -> Map.empty
      -- CR 113.6k and CR 113.6m: every card in every graveyard carrying at least
      -- one ability those rules put there. The one source that widens the SCANNED
      -- ZONE rather than recovering an object an event names, which is why the
      -- walk itself happens once outside the event loop; what an individual event
      -- may see of the answer is `arrivedLater`'s subtraction below.
      --
      -- Narrow by construction, which keeps a large graveyard cheap: membership is
      -- decided by `functionsIn` -- a total case over a closed condition type and a
      -- walk of the ability's own effects, no projection and no board walk. Cards
      -- contributing nothing are dropped rather than carried as empty entries.
      --
      -- Abilities come from the PRINTED card and the controller is the OWNER, for
      -- `cycledCard`'s reasons.
      --
      -- CR 603.10a does not apply to what this serves -- a card ENTERING a
      -- graveyard is on none of its look-back list -- so CR 603.10's normal first
      -- sentence governs: an event is checked against the objects that existed
      -- immediately after IT, not against the board at the end of the batch. This
      -- read is the whole graveyard as it stands, so `arrivedLater` below narrows
      -- it per event group. The card that arrived in a graveyard and is gone again
      -- by the boundary is the one this read cannot reach; `arrivedInGraveyard`
      -- below is its source.
      --
      -- Its `_ -> Nothing` arm is what keeps the two disjoint: `Game.lookupObject`
      -- fails for an id that has ceased, and a ceased id is exactly the one the
      -- other source answers for.
      graveyardCandidate oid = case (Game.lookupObject oid gs, Game.faceOf oid gs) of
        (Just obj, Just face) ->
          case filter (functionsIn Zone.Graveyard) (Face.triggeredAbilities face) of
            [] -> Nothing
            abilities -> Just (oid, (Object.owner obj, abilities))
        _ -> Nothing
      inGraveyards =
        Map.fromList
          (concatMap (Maybe.mapMaybe graveyardCandidate . Foldable.toList) (Map.elems (GameState.graveyard gs)))
      -- CR 603.10's first sentence again, for the card THIS event put into a
      -- graveyard that is gone by the CR 117.5 boundary: it existed in the
      -- graveyard immediately after the event, so its ability is checked, and
      -- CR 608.2h last known information is the only reading of it left. The
      -- graveyard twin of `leftBattlefield`, and per EVENT for the same reason --
      -- the arrival is what scopes it. Corpse Churn milling Narcomoeba and
      -- returning it in one resolution is the proving board, in
      -- Pawl.TriggerSpec's `graveyardTriggerSpec`.
      --
      -- Keyed by `ZoneChange.object`, where `departedFrom` keys by
      -- `ZoneChange.departed` -- the one place the graveyard source inverts the
      -- battlefield one. The bearer here is the CR 400.7 incarnation that ARRIVED
      -- in the graveyard, which is the id `matchesTriggerGiven` compares against
      -- for SelfPutIntoGraveyardFromLibrary and the id `inGraveyards` would have
      -- offered. It is also the id `lastKnown` files the later departure under,
      -- the graveyard card being what left; the two ids coincide by construction,
      -- and that is the hinge of this source.
      --
      -- ONLY the destination is gated. Which origins a condition accepts is the
      -- CONDITION's business -- SelfPutIntoGraveyardFromAnywhere is served from
      -- the same zone -- which is the posture `departedFrom`'s comment argues for.
      -- That gate is a regression fence rather than a proved behaviour: widening it
      -- to every non-battlefield destination leaves the suite green, since the key
      -- below is the ARRIVING id and only a graveyard arrival that has itself since
      -- departed has a `lastKnown` entry under it.
      --
      -- Abilities from the last known projection rather than a printed face: a
      -- ceased id has no face to look up, and `LastKnown` carries none. Identical
      -- to `inGraveyards`' printed read today, no pool effect changing the
      -- TRIGGERED abilities of a card in a graveyard (gap #1859). Not `abilitiesOf` either --
      -- nothing rule 702 or rule 310 mints functions from a graveyard, and
      -- `inGraveyards` does not consult them, so the two graveyard sources read
      -- alike.
      --
      -- The controller is `LastKnown.controller`, which for a graveyard card is
      -- the OWNER and so agrees with `inGraveyards` -- CR 113.8's second clause,
      -- CR 108.4 giving a card in a graveyard no controller. Not asserted by the
      -- read but true of the write: `changeZoneAttaching` computes it as
      -- Projection.controllerOf, which bottoms out at Object.enteredUnder and then
      -- the owner, and only a CR 613.1b layer-2 effect could move it -- those
      -- reach permanents (CR 110.2), never a card in a graveyard.
      --
      -- `functionsIn Zone.Graveyard` is CR 113.6k's own gate, as it is for
      -- `inGraveyards` and not for `leftBattlefield`: without it a departed Doomed
      -- Traveler would be offered its dies trigger from a graveyard, and a
      -- "whenever another creature dies" watcher would be offered ITS trigger for
      -- the very move that buried it -- CR 400.7 having minted a fresh id, the
      -- "another" test compares two different ids and passes. Proved by
      -- Pawl.ZoneTriggerSpec's "CR 113.6k a battlefield-only trigger on a card
      -- that arrived in a graveyard and left it is not offered": Come Back Wrong
      -- destroys a Meren of Clan Nel Toth and returns her in one resolution, and
      -- removing this filter hands her controller an experience counter for her
      -- own death.
      --
      -- Disjoint from `inGraveyards` by construction, not by Map.unions' bias: an
      -- id in `lastKnown` is one the same write deleted from GameState.objects,
      -- and CR 400.7 mints a fresh id per move, so `graveyardCandidate` drops it.
      -- A card that was in a graveyard before this batch is unreachable here too
      -- -- its arrival event is not in the log this scan reads.
      arrivedInGraveyard event = case movedOf event of
        Just zc
          | ZoneChange.to zc == Zone.Graveyard ->
              case Map.lookup (ZoneChange.object zc) (GameState.lastKnown gs) of
                Nothing -> Map.empty
                Just lk ->
                  case filter (functionsIn Zone.Graveyard) (PC.triggeredAbilities (LastKnown.characteristics lk)) of
                    [] -> Map.empty
                    abilities -> Map.singleton (ZoneChange.object zc) (LastKnown.controller lk, abilities)
        _ -> Map.empty
      -- CR 113.6k's third zone, `inGraveyards` one zone over: every card in exile
      -- carrying an ability that functions from there. Rule 702.55c is the
      -- sentence it exists for -- "triggered abilities of cards with haunt that
      -- refer to the haunted creature can trigger in the exile zone" -- and CR
      -- 113.6k's own example is the card that bears one.
      --
      -- FILTERED BY `functionsIn`, unlike the command zone's emblem source: there
      -- the rule at issue is CR 114.4, which is about the OBJECT, and every emblem
      -- ability would fail a condition test; here the rule at issue IS CR 113.6k,
      -- so the filter is the gate itself. Without it an exiled Doomed Traveler
      -- would be offered its dies trigger, and an exiled Desolation Twin its cast
      -- trigger, from a zone CR 113.6 says neither functions in.
      --
      -- ONE STANDING SCAN over the whole zone, computed outside the event loop for
      -- `inGraveyards`' reason: a haunting card sits in exile indefinitely and no
      -- event names it, so nothing narrower could find it.
      --
      -- Abilities come from the PRINTED card and the controller is the OWNER (CR
      -- 108.4a), also for `inGraveyards`' reasons: CR 108.4 gives a card in exile
      -- no controller at all, so Blind Hunter's "you gain 2 life" pays the player
      -- who owns the haunting card.
      -- Not implemented: a card exiled FACE DOWN is scanned here like any other,
      -- so its printed abilities are offered where CR 406.3a leaves it none
      -- (#1479).
      exileCandidate oid = case (Game.lookupObject oid gs, Game.faceOf oid gs) of
        (Just obj, Just face) ->
          case filter (functionsIn Zone.Exile) (Face.triggeredAbilities face) of
            [] -> Nothing
            abilities -> Just (oid, (Object.owner obj, abilities))
        _ -> Nothing
      inExile = Map.fromList (Maybe.mapMaybe exileCandidate (Set.toAscList (GameState.exile gs)))
      -- CR 113.6k's other zone: the spell that just became cast, offered from the
      -- STACK, where CR 601.2a leaves it. Desolation Twin's "when you cast this
      -- spell" is borne by an object that is on nobody's battlefield and in
      -- nobody's graveyard, so no source above can reach it.
      --
      -- Scoped to the CAST EVENT rather than computed once over GameState.stack,
      -- which is `cycledCard`'s shape. Not for want of a controller: CR 405.4's
      -- caster is stamped into Object.enteredUnder by changeZoneCasting, and
      -- Projection.defaultControllerOf reads it back, so a standing scan would
      -- find the caster rather than falling back to the owner. The narrower scope
      -- is what the WORK asks for -- SelfCast is the only condition
      -- zonesTriggeredFrom puts on the stack and it matches no other event, so a
      -- standing scan of every spell would answer alike at more cost. A future
      -- condition that functions on the stack and watches some other event is
      -- what would widen this.
      --
      -- Abilities come from the PRINTED card, for `cycledCard`'s reason (#1859).
      spellCast event = case event of
        GameEvent.SpellCast (SpellWasCast.MkSpellWasCast caster spell _ _) -> case Game.faceOf spell gs of
          Nothing -> Map.empty
          Just face -> case filter (functionsIn Zone.Stack) (Face.triggeredAbilities face) of
            [] -> Map.empty
            abilities -> Map.singleton spell (caster, abilities)
        GameEvent.Discarded {} -> Map.empty
        GameEvent.Drew {} -> Map.empty
        GameEvent.Moved {} -> Map.empty
        GameEvent.DamageDealt _ -> Map.empty
        GameEvent.DamagePrevented {} -> Map.empty
        GameEvent.StepBegan {} -> Map.empty
        GameEvent.BecameMonarch _ -> Map.empty
        GameEvent.Revealed {} -> Map.empty
        GameEvent.AttackerDeclared {} -> Map.empty
        GameEvent.BecameBlocking {} -> Map.empty
        GameEvent.BlocksDeclared {} -> Map.empty
        GameEvent.AttackerBlocked {} -> Map.empty
        GameEvent.AttackerUnblocked _ -> Map.empty
        GameEvent.SpellCountered _ -> Map.empty
        GameEvent.HalfUnlocked {} -> Map.empty
        GameEvent.TurnedFaceUp _ -> Map.empty
        GameEvent.Transformed {} -> Map.empty
        GameEvent.BecameDesignated {} -> Map.empty
        GameEvent.Evolved _ -> Map.empty
        GameEvent.Mentored {} -> Map.empty
        GameEvent.Trained _ -> Map.empty
        GameEvent.PermanentSacrificed {} -> Map.empty
        GameEvent.AbilityTriggered {} -> Map.empty
        GameEvent.LoyaltyAbilityActivated _ -> Map.empty
        GameEvent.LifeLost {} -> Map.empty
        GameEvent.LifeGained {} -> Map.empty
        GameEvent.CountersPut {} -> Map.empty
        GameEvent.CountersRemoved {} -> Map.empty
        GameEvent.ControlChanged {} -> Map.empty
        GameEvent.VentureMarkerEntered {} -> Map.empty
        GameEvent.BecameTarget {} -> Map.empty
        GameEvent.BecameAttached {} -> Map.empty
        GameEvent.LeftTheGame _ -> Map.empty
        GameEvent.Milled {} -> Map.empty
        GameEvent.Scried _ -> Map.empty
        GameEvent.Surveiled _ -> Map.empty
        GameEvent.DiceRolled _ -> Map.empty
        GameEvent.ClassLevelSet _ -> Map.empty
        GameEvent.Plotted _ -> Map.empty
        GameEvent.Explored _ -> Map.empty
        GameEvent.Exerted _ -> Map.empty
        GameEvent.BecameAttacked _ -> Map.empty
        GameEvent.AttackersDeclared _ -> Map.empty
        GameEvent.BecameTapped _ -> Map.empty
      -- CR 114.4 / CR 113.6p: "abilities of emblems function in the command zone".
      -- The third source that widens the SCANNED ZONE rather than recovering an
      -- object an event names, so it is computed once outside the event loop as
      -- `onBattlefield` and `inGraveyards` are.
      --
      -- NOT filtered through `functionsIn`, where `inGraveyards` is. CR 113.6k
      -- decides one CONDITION at a time and answers for the ability's usual zone;
      -- CR 114.4 is about the OBJECT, and says every ability of this one functions
      -- here. Filtering would drop them all: an emblem's "at the beginning of your
      -- upkeep" is a condition that triggers perfectly well from the battlefield,
      -- so CR 113.6's default sends it there and the emblem would never be asked.
      --
      -- EMBLEMS alone, where a graveyard walk takes the whole zone, for the reason
      -- Pawl.Engine.CombatRestriction.inForce narrows the same walk: the command
      -- zone also holds a commander and a dungeon card, whose abilities CR 113.6
      -- leaves functioning on the battlefield and CR 309.4c mints rather than
      -- prints. Asking Source.OfEmblem is reading the rulebook's own list (CR
      -- 113.6p), not an effect's identity.
      --
      -- The controller is the OWNER: CR 114.2 makes an emblem both owned and
      -- controlled by the player it was created for, and createEmblem leaves
      -- Object.enteredUnder empty so Projection.defaultControllerOf answers the
      -- same. CR 603.3a's control sample is not consulted, and cannot disagree:
      -- CR 110.2 gives a PERMANENT a controller, and CR 114.5 says an emblem is
      -- not one, so no layer-2 effect (CR 613.1b) has an emblem to move.
      --
      -- Abilities come from the emblem's own card, which is the whole of it (CR
      -- 114.3) -- no projection is involved, which is the posture
      -- Projection.gatherGiven's emblem walk takes for its static half: an emblem
      -- is not a creature, so the pool's CR 613.1f removers never reach it.
      emblemCandidate oid = case Game.lookupObject oid gs of
        Nothing -> Nothing
        Just obj -> case Object.source obj of
          Source.OfEmblem _ -> case Game.faceOf oid gs of
            Nothing -> Nothing
            Just face -> case Face.triggeredAbilities face of
              [] -> Nothing
              abilities -> Just (oid, (Object.owner obj, abilities))
          _ -> Nothing
      inCommand =
        Map.fromList
          (Maybe.mapMaybe emblemCandidate (Set.toAscList (GameState.command gs)))
      -- CR 113.6k's last zone: the card a player just revealed from their HAND as
      -- they drew it (CR 702.94a, CR 121.9). The ability is borne by an object that
      -- is on nobody's battlefield, in nobody's graveyard and on no stack -- rule
      -- 701.20b moved it nowhere -- so no source above can reach it.
      --
      -- Scoped to the reveal EVENT rather than computed once over every hand,
      -- which is `cycledCard`'s and `spellCast`'s shape rather than `inExile`'s.
      -- The reason is the rule: rule 702.94a's trigger fires on a reveal and
      -- nothing else, and no ability in the pool sits in a hand watching for
      -- something no event names -- where a haunting card sits in exile
      -- indefinitely and only a standing scan could find it. A standing walk of
      -- every hand would answer alike at the cost of a scan per event.
      --
      -- FILTERED BY `functionsIn`, like `inExile` and unlike the command zone's
      -- emblem source: the rule at issue here IS CR 113.6k. Without it a drawn
      -- Doomed Traveler would be offered its dies trigger from a hand.
      --
      -- The abilities are the PRINTED ones plus the ones rule 702 MINTS from the
      -- card's printed keywords -- and miracle's is entirely the latter, so
      -- dropping the mint would leave this source with nothing to find. Printed
      -- keywords rather than a projection's, for `cycledCard`'s reason (#1859).
      --
      -- The controller is the OWNER, CR 113.8's second clause, for `inGraveyards`'
      -- reason: CR 108.4 gives a card in a hand no controller. Rule 702.94a's
      -- reveal is one a player makes from their own hand, so the owner is also the
      -- revealer, and CR 109.5's "you" lands on the same seat either way.
      revealedInHand event = case event of
        GameEvent.Revealed (Revealed.MkRevealed _ oid RevealCause.ForMiracle _) -> case (Game.lookupObject oid gs, Game.faceOf oid gs) of
          (Just obj, Just face) ->
            case filter (functionsIn Zone.Hand) (Face.triggeredAbilities face <> Keyword.printedTriggeredAbilitiesOf (Face.keywords face)) of
              [] -> Map.empty
              abilities -> Map.singleton oid (Object.owner obj, abilities)
          _ -> Map.empty
        GameEvent.Revealed (Revealed.MkRevealed _ _ RevealCause.Ordinary _) -> Map.empty
        GameEvent.Discarded {} -> Map.empty
        GameEvent.Drew {} -> Map.empty
        GameEvent.Moved {} -> Map.empty
        GameEvent.DamageDealt _ -> Map.empty
        GameEvent.DamagePrevented {} -> Map.empty
        GameEvent.StepBegan {} -> Map.empty
        GameEvent.SpellCast {} -> Map.empty
        GameEvent.BecameMonarch _ -> Map.empty
        GameEvent.AttackerDeclared {} -> Map.empty
        GameEvent.BecameBlocking {} -> Map.empty
        GameEvent.BlocksDeclared {} -> Map.empty
        GameEvent.AttackerBlocked {} -> Map.empty
        GameEvent.AttackerUnblocked _ -> Map.empty
        GameEvent.SpellCountered _ -> Map.empty
        GameEvent.HalfUnlocked {} -> Map.empty
        GameEvent.TurnedFaceUp _ -> Map.empty
        GameEvent.Transformed {} -> Map.empty
        GameEvent.BecameDesignated {} -> Map.empty
        GameEvent.Evolved _ -> Map.empty
        GameEvent.Mentored {} -> Map.empty
        GameEvent.Trained _ -> Map.empty
        GameEvent.PermanentSacrificed {} -> Map.empty
        GameEvent.AbilityTriggered {} -> Map.empty
        GameEvent.LoyaltyAbilityActivated _ -> Map.empty
        GameEvent.LifeLost {} -> Map.empty
        GameEvent.LifeGained {} -> Map.empty
        GameEvent.CountersPut {} -> Map.empty
        GameEvent.CountersRemoved {} -> Map.empty
        GameEvent.ControlChanged {} -> Map.empty
        GameEvent.VentureMarkerEntered {} -> Map.empty
        GameEvent.BecameTarget {} -> Map.empty
        GameEvent.BecameAttached {} -> Map.empty
        GameEvent.LeftTheGame _ -> Map.empty
        GameEvent.Milled {} -> Map.empty
        GameEvent.Scried _ -> Map.empty
        GameEvent.Surveiled _ -> Map.empty
        GameEvent.DiceRolled _ -> Map.empty
        GameEvent.ClassLevelSet _ -> Map.empty
        GameEvent.Plotted _ -> Map.empty
        GameEvent.Explored _ -> Map.empty
        GameEvent.Exerted _ -> Map.empty
        GameEvent.BecameAttacked _ -> Map.empty
        GameEvent.AttackersDeclared _ -> Map.empty
        GameEvent.BecameTapped _ -> Map.empty
      forOne event (oid, (ctrl, abilities)) =
        let -- The bearer's own slot environment, so a condition naming a slot
            -- (TriggerCondition.LoseControlOfBound) is read the same way here as it
            -- is for a CR 603.7 delayed entry. Empty for a bearer that has since
            -- left, there being no object to ask -- whether it arrived here out of
            -- last known information or out of a sample taken while it stood.
            bindings = maybe Map.empty Object.bindings (Game.lookupObject oid gs)
            fires ab = matchesTriggerGiven bindings gs oid ctrl (TriggeredAbility.condition ab) event
            pend ab = PendingTrigger.MkPendingTrigger (TriggerSource.OfObject oid) ctrl ab (eventBindings (Map.lookup oid becameInGraveyard) (TriggeredAbility.condition ab) event) Nothing
            -- CR 603.2c's key, for `oncePerBatch` below: which ability of which
            -- bearer this pending trigger came from, or Nothing when the condition
            -- is per-occurrence and every member of the batch is its own trigger
            -- event. Keyed by the ability's POSITION in the bearer's list rather
            -- than by the ability itself, so a permanent printing the same batch
            -- condition twice keeps both -- no equality on TriggeredAbility is
            -- needed and none is assumed.
            key (index, ab) =
              if batchScoped (TriggeredAbility.condition ab)
                then Just (oid, index :: Natural)
                else Nothing
            keyed indexed = (key indexed, pend (snd indexed))
         in fmap keyed (filter (fires . snd) (zip [0 ..] abilities))
      -- Map.unions is left-biased, so the battlefield reading wins over a
      -- last-known one, a cycled card and a graveyard reading. That rules out a
      -- double fire: one entry per id means one pass of `forOne` per id.
      --
      -- Two of the first four genuinely overlap, and there the bias is load-bearing
      -- rather than belt and braces: a permanent that departs at a LATER group was
      -- still standing when this group's sample was taken, so it is in both
      -- `onBattlefield` and `later`, and the sample's at-the-event reading is the
      -- one CR 603.10 asks for. `leftBattlefield` and `same` cannot collide with
      -- the sample -- both name a permanent that had already gone when the sample
      -- was taken -- nor with each other, an id departing at exactly one group.
      -- `inGraveyards` genuinely overlaps `cycledCard` on purpose -- a card
      -- cycled into a graveyard is honestly a member of both -- and the winner
      -- offers that card's printed abilities unfiltered, a superset either way.
      -- `arrivedInGraveyard` overlaps nothing: it answers only for an id
      -- `lastKnown` holds, which is one no longer in GameState.objects and so in
      -- no player's graveyard and no player's hand, so its position beside
      -- `inGraveyards` is documentation rather than arbitration.
      -- `spellCast` overlaps nothing: CR 601.2a keeps its object on the stack,
      -- which no other source reads. Neither does `inCommand`: CR 114.1 puts an
      -- emblem into the command zone, and no rule or effect in pawl moves one
      -- anywhere else -- Event.createEmblem is its only writer. Nor does
      -- `inExile`: CR 400.1 makes exile a zone of its own, and an id in it is in
      -- no other. Nor does `revealedInHand`: CR 701.20b leaves the revealed card
      -- in the hand, which no other source reads.
      candidates onBattlefield event later same arrivedAfter = Map.toAscList (Map.unions [onBattlefield, leftBattlefield event, later, same, cycledCard event, spellCast event, revealedInHand event, Map.withoutKeys inGraveyards arrivedAfter, arrivedInGraveyard event, inCommand, inExile])
      scanOne onBattlefield later same arrivedAfter event = concatMap (forOne event) (candidates onBattlefield event later same arrivedAfter)
      -- CR 603.2c's second sentence, applied to ONE event group: a batch-scoped
      -- condition's trigger event is the whole group, so it triggers once however
      -- many of the group's members matched, where a per-occurrence condition
      -- triggers once per member (CR 603.2c's own Example, the sweeper that fires a
      -- "whenever A land is put into a graveyard" ability once per land).
      --
      -- The FIRST match wins rather than the last, which keeps the canonical order
      -- below intact: this drops later duplicates and reorders nothing, so a batch
      -- trigger sits exactly where its earliest matching event would have put it.
      -- Which member won is unobservable anyway -- eventBindingSlots gives a
      -- batch-scoped condition no slots, so every duplicate carries identical
      -- bindings.
      --
      -- Per GROUP and never per scan: several groups can share one CR 117.5 scan
      -- (GameState.scannedThrough is not bumped until the scan ends), and CR 704.3
      -- makes each state-based-action pass its own single event.
      -- Pawl.ZoneTriggerSpec's "CR 704.3 two death groups in one trigger scan are
      -- two trigger events" is what tells the two readings apart.
      oncePerBatch seen entries = case entries of
        [] -> []
        (k, trigger) : rest -> case k of
          Nothing -> trigger : oncePerBatch seen rest
          Just batch
            | Set.member batch seen -> oncePerBatch seen rest
            | otherwise -> trigger : oncePerBatch (Set.insert batch seen) rest
      -- The battlefield reading is per GROUP and so is hoisted out of the block:
      -- every event in one group happened at the same time, so they share it. A
      -- group with no events cannot occur -- List.groupBy yields no empty block.
      scanBlock block later same arrivedAfter = case block of
        [] -> []
        entry : _ -> oncePerBatch Set.empty (concatMap (scanOne (onBattlefieldAt (LoggedEvent.group entry)) later same arrivedAfter . LoggedEvent.event) block)
   in concat (List.zipWith4 scanBlock groups laterGroups sameGroup arrivedLater)

-- CR 113.6m, read off a TRIGGERED ability: "an ability whose cost or effect
-- specifies that it moves the object it's on out of a particular zone functions
-- only in that zone". The rule says "an ability" -- Pawl.Engine.Activate's
-- namesake is the same sentence read off an activated one, and this is the
-- triggered half.
--
-- No COST half. CR 602.1 gives an activated ability "a cost and an effect";
-- CR 603.1 gives a triggered one "a trigger condition and an effect", and no
-- cost at all -- so Pawl.Engine.Cost, the other half of
-- Activate.zoneFunctionedFrom, has nothing to be asked here. The CONDITION is
-- read instead, and only for the rule's own exception: `enchantedObjectLeaves`
-- below.
--
-- ALL MODES, in printed order, for Activate.zoneFunctionedFrom's reason: CR
-- 700.2 makes a modal ability's modes alternatives, so a zone stated by any of
-- them is a zone the ability can move its object out of.
--
-- Not a case on an effect's identity: Pawl.Engine.EffectZone answers the one
-- question, and this folds its answer. The condition case below is not one
-- either -- TriggerCondition is a closed-half type like Phase or Keyword, which
-- design.md section 1 puts on the rulebook's side of the line.
--
-- CR 113.6m's "unless" clause is read here in the one half a trigger condition
-- can satisfy -- the Aura half, `enchantedObjectLeaves` below. Screams from
-- Within's "when enchanted creature dies, return this card from your graveyard
-- to the battlefield" is the printing it decides: without the clause the effect
-- pins the ability to the graveyard, where the condition can never be checked,
-- and the card does nothing.
--
-- Not implemented: the clause's OTHER half, "a previous part of its cost or
-- effect specifies that the object is put into that zone", and the
-- delayed-triggered-ability sentence (#819). Neither needs a trigger condition,
-- so both belong to the fold below rather than here.
zoneFunctionedFrom :: TriggeredAbility.TriggeredAbility Card -> Maybe Zone
zoneFunctionedFrom ability =
  if enchantedObjectLeaves (TriggeredAbility.condition ability)
    then Nothing
    else
      Maybe.listToMaybe
        (Maybe.mapMaybe EffectZone.zoneFunctionedFrom (Modal.allEffects (TriggeredAbility.modal ability)))

-- CR 113.6m's Aura clause, asked of a trigger condition: does it specify "that
-- the object it enchants leaves the battlefield"? CR 700.4 makes a death one, so
-- the "dies" wording every printing uses is inside the clause.
--
-- CR 603.1b: one ability may have several conditions, and the rule's "its
-- trigger condition" is satisfied by any of them -- the exception is about what
-- the ability can be made to watch, and one watching condition is enough.
--
-- The rule's "if the object is an Aura" is NOT checked, for want of the object:
-- this reads an ability, and the card's type line is not in hand. Inert rather
-- than wrong today -- Scryfall `o:"enchanted creature dies" o:"from your
-- graveyard to the battlefield"` (2026-08-19) returns Journey to Eternity, Reins
-- of the Vinesteed and Screams from Within, all three Auras -- and an Equipment
-- printed with this condition and a graveyard-recursion effect is the card that
-- would refute it (gap #1894).
--
-- The `_` is a decision, not an omission: CR 113.6m's exception names exactly one
-- family of conditions, so a condition that says nothing about the enchanted
-- object's departure gets the rule's main sentence, which is what False means
-- here.
enchantedObjectLeaves :: TriggerCondition -> Bool
enchantedObjectLeaves condition = case condition of
  TriggerCondition.AttachedCreatureDies -> True
  -- False, stated rather than left to the wildcard: this watches the SAME
  -- attachment link, but for an event that leaves the enchanted permanent right
  -- where it was, so CR 113.6m's Aura clause has nothing to exempt.
  TriggerCondition.AttachedCreatureBecomesTapped -> False
  TriggerCondition.AnyOf conditions -> any enchantedObjectLeaves conditions
  _ -> False

-- CR 113.6, asked of one zone and one triggered ability: does it function from
-- there? Three sentences of that rule in precedence order.
--
-- CR 113.6m first, because it is the only one that can name a zone the condition
-- knows nothing about -- Squee, Goblin Nabob's "at the beginning of your upkeep"
-- triggers perfectly well from the battlefield, and only "return this card from
-- your graveyard" says otherwise. "Functions ONLY in that zone" is what makes
-- this an override rather than an addition.
--
-- CR 113.6k next, for a condition that cannot trigger from the battlefield at
-- all -- Narcomoeba's "put into your graveyard from your library".
--
-- CR 113.6's own default last: "abilities of all other objects usually function
-- only while that object is on the battlefield".
--
-- The two rules cannot presently disagree: no printing states an origin zone on
-- an ability whose condition already answers CR 113.6k, and if one did they
-- would both say graveyard. The order is written down so a future card meets a
-- decision rather than an accident.
functionsIn :: Zone -> TriggeredAbility.TriggeredAbility Card -> Bool
functionsIn zone ability = case zoneFunctionedFrom ability of
  Just named -> zone == named
  Nothing -> Set.member zone (zonesTriggeredFrom (TriggeredAbility.condition ability))

-- CR 113.6k, both sentences: "a trigger condition that can't trigger from the
-- battlefield functions in all zones it can trigger from. OTHER TRIGGER
-- CONDITIONS OF THE SAME TRIGGERED ABILITY MAY FUNCTION IN DIFFERENT ZONES."
-- Which is why the answer is a SET: rule 113.6k's own example is Absolver
-- Thrull's "when this creature enters or the creature it haunts dies", one
-- ability whose first condition functions from the battlefield and whose second
-- functions from exile, and no single zone describes it.
--
-- The graveyard, the stack, exile and the hand are the four non-battlefield
-- answers eventTriggers can act on, being the non-battlefield zones this rule
-- sends it to. The command zone IS scanned too, but by
-- a source CR 114.4 governs rather than this rule, so this function's one Command
-- answer -- CR 309.4c's room ability -- still goes unconsulted.
--
-- One of the three sentences `functionsIn` above reads, and the only one that
-- looks at the CONDITION -- so an ability whose effect already names its zone
-- never reaches this, and no arm below has to think about CR 113.6m.
--
-- A CLASSIFICATION of a trigger condition rather than an effect: it asks which
-- zone a rule 603 condition functions in and never reaches the ability's payload.
--
-- The default is `battlefield`, which is CR 113.6's own: abilities usually
-- function only while the object is on the battlefield. Every `battlefield` arm
-- below is that sentence, not an omission.
zonesTriggeredFrom :: TriggerCondition -> Set.Set Zone
zonesTriggeredFrom cond = case cond of
  -- CR 309.4c: "as long as a dungeon card is in the command zone, its abilities
  -- may trigger". The honest answer, and inert: eventTriggers' command-zone source
  -- is CR 114.4's and takes emblems alone, so nothing consults this arm --
  -- Pawl.Engine.Dungeon.roomPending is what gathers a room ability.
  TriggerCondition.RoomEntered _ -> Set.singleton Zone.Command
  -- CR 113.6's default for the three whose watcher is an ordinary permanent:
  -- Matoya, Archon Elder and Wildgrowth Walker are creatures, and neither the
  -- scry, the surveil nor the explore is a condition that cannot trigger from
  -- the battlefield, so CR 113.6k's exception does not apply.
  TriggerCondition.PlayerScries _ -> battlefield
  TriggerCondition.PlayerSurveils _ -> battlefield
  TriggerCondition.PermanentExplores _ -> battlefield
  -- CR 113.6's default again: Feywild Trickster is a creature, and nothing
  -- about rolling a die is a condition that cannot trigger from the
  -- battlefield.
  TriggerCondition.PlayerRollsDice _ -> battlefield
  -- CR 113.6's default, and CR 701.43c makes it the only possible answer rather
  -- than a default: an object that isn't on the battlefield can't be exerted, so
  -- the bearer is standing there when its own exert is recorded.
  TriggerCondition.SelfExerted -> battlefield
  -- CR 113.6's default, and CR 701.3a makes it the only possible answer for the
  -- exert arm's reason: the host of an attachment is a permanent, so the bearer
  -- is on the battlefield whenever this can match.
  TriggerCondition.SelfBecomesAttachedBy _ -> battlefield
  -- EXILE, and this arm is CR 113.6k's exception rather than its default:
  -- CR 702.170b's special action exiles the card as it becomes plotted, so
  -- the object bearing Aloe Alchemist's "when this card becomes plotted" is
  -- in exile at the moment it fires and can never be on the battlefield for
  -- it. Answering `battlefield` here would leave the trigger unreachable --
  -- eventTriggers finds this bearer through its exile scan, which is gated on
  -- exactly this answer.
  TriggerCondition.SelfBecomesPlotted -> Set.singleton Zone.Exile
  -- CR 603.6a is an enters-the-battlefield ability; its bearer is on the
  -- battlefield when it fires.
  TriggerCondition.SelfEnters -> battlefield
  TriggerCondition.PermanentEnters _ -> battlefield
  TriggerCondition.StepBegins {} -> battlefield
  -- CR 709.5c makes an unlocked designation something a permanent ON THE
  -- BATTLEFIELD has, so this condition cannot trigger from a graveyard at all.
  TriggerCondition.SelfHalfUnlocked _ -> battlefield
  -- CR 709.5c again, one object over: the permanent that became fully unlocked is
  -- on the battlefield, and CR 113.6 leaves the WATCHER where it usually is.
  -- Balemurk Leech is a creature and does nothing from a graveyard.
  TriggerCondition.RoomFullyUnlocked _ -> battlefield
  -- THE UNION, which is CR 113.6k's second sentence read literally: each condition
  -- of a multi-condition ability functions where it functions, and the ability is
  -- offered from every zone any of them reaches. Blind Hunter's ability is the
  -- producer -- battlefield for "when this creature enters", exile for "or the
  -- creature it haunts dies" -- and it is the rule's own example.
  --
  -- Offering the ability in both zones cannot double-fire it: `matchesTrigger`
  -- still has to admit the bearer, and the two conditions are about different ids
  -- in different zones, so at most one of them matches any event.
  --
  -- The empty list falls to CR 113.6's default rather than to the empty union,
  -- which would say the ability functions nowhere. No card writes one.
  TriggerCondition.AnyOf conditions -> case conditions of
    [] -> battlefield
    _ -> Set.unions (fmap zonesTriggeredFrom conditions)
  -- CR 708.7 is about a PERMANENT being turned face up, and CR 110.1 puts
  -- permanents on the battlefield alone, so CR 113.6k never reaches this.
  TriggerCondition.SelfTurnedFaceUp -> battlefield
  -- CR 701.27a transforms a PERMANENT, which CR 110.1 puts on the battlefield
  -- alone, so CR 113.6k never reaches this either.
  TriggerCondition.SelfTransformedInto _ -> battlefield
  -- CR 113.6's default, one object over: the WATCHER is an ordinary permanent
  -- doing its watching from the battlefield -- Aven Farseer is a creature -- so CR
  -- 113.6k's exception, which is for a condition that cannot trigger from the
  -- battlefield at all, does not apply.
  TriggerCondition.PermanentTurnedFaceUp _ -> battlefield
  -- The same default: CR 702.112b's "only permanents can be or become renowned"
  -- keeps the subject on the battlefield, and Valeron Wardens watches from it.
  TriggerCondition.PermanentBecomesDesignated {} -> battlefield
  -- The same default again: rule 702.100b's marker goes to a creature, and
  -- Renegade Krasis is the creature watching itself.
  TriggerCondition.SelfEvolves -> battlefield
  -- The same default a third time, from the Equipment's side: CR 301.5c unattaches
  -- an Equipment rather than moving it, so one that equips anything is on the
  -- battlefield, and Aegis of the Legion watches from there -- CR 113.6k's exception
  -- is for a condition that cannot trigger from the battlefield at all.
  TriggerCondition.AttachedCreatureMentors -> battlefield
  -- CR 113.6's default from the Aura's side: CR 303.4's Aura is itself a
  -- permanent on the battlefield, so its bearer watches from there, and CR
  -- 113.6k's exception -- for a condition that cannot trigger from the
  -- battlefield at all -- does not apply. What DOES apply is CR 113.6m's Aura
  -- clause, read by zoneFunctionedFrom above, which is why this arm is reached
  -- for Screams from Within at all.
  TriggerCondition.AttachedCreatureDies -> battlefield
  -- The same default for the same reason, and more plainly: an Aura enchanting a
  -- permanent is itself a permanent on the battlefield, and CR 113.6k's exception
  -- is for a condition that cannot trigger from there at all.
  TriggerCondition.AttachedCreatureBecomesTapped -> battlefield
  -- The same default from the training creature's own side: rule 702.149a's ability
  -- fires on an attack, so its bearer is on the battlefield and CR 113.6k's
  -- exception -- for a condition that cannot trigger from there at all -- does not
  -- apply.
  TriggerCondition.SelfTrains -> battlefield
  -- CR 113.6's default: an ability of a permanent functions only while that
  -- permanent is on the battlefield. CR 113.6k's exception is for a trigger
  -- condition that CANNOT trigger from the battlefield, and this one plainly can
  -- -- Mayhem Devil watches every sacrifice from the board it stands on.
  TriggerCondition.PermanentSacrificed -> battlefield
  -- CR 603.8's state triggers are not event triggers, so this scan is not their
  -- reader in any zone; stateTriggers below gathers them from the battlefield.
  TriggerCondition.StateIs _ -> battlefield
  TriggerCondition.SelfDealsCombatDamageToPlayer -> battlefield
  -- CR 113.6's default again, and the match's own shape on top of it: this arm
  -- compares the bearer against the event's RECIPIENT, and CR 120.3's recipient is
  -- a player or a permanent -- so a bearer anywhere but the battlefield can never
  -- be the one damaged.
  TriggerCondition.SelfIsDealtDamage -> battlefield
  TriggerCondition.PermanentDealsCombatDamageToPlayer _ -> battlefield
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> battlefield
  TriggerCondition.OpponentLostLifeDuringYourTurn -> battlefield
  -- CR 302.6 / 508.1a: only a permanent on the battlefield can be declared as an
  -- attacker, so CR 113.6k never reaches this.
  TriggerCondition.SelfAttacks _ -> battlefield
  TriggerCondition.SelfAttacksWithAnother _ -> battlefield
  TriggerCondition.CreatureAttacksAlone _ -> battlefield
  TriggerCondition.CreatureAttacksYou -> battlefield
  TriggerCondition.AttachedPlayerIsAttacked -> battlefield
  TriggerCondition.PlayerAttacks _ -> battlefield
  TriggerCondition.PlayerAttacksWith {} -> battlefield
  TriggerCondition.PlayerAttacksPlayer {} -> battlefield
  TriggerCondition.SelfAttacksPlayerWithMostLife -> battlefield
  TriggerCondition.SelfBlocks -> battlefield
  TriggerCondition.SelfBlocksCreature _ -> battlefield
  TriggerCondition.SelfBlocksAtLeast _ -> battlefield
  TriggerCondition.SelfBlocksOneOrMore _ -> battlefield
  TriggerCondition.SelfBecomesBlocked -> battlefield
  TriggerCondition.SelfBecomesBlockedBy _ -> battlefield
  TriggerCondition.SelfBecomesBlockedByOneOrMore _ -> battlefield
  TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> battlefield
  TriggerCondition.SelfAttacksUnblocked -> battlefield
  -- CR 702.29c: a cycling ability triggers from whatever zone the card winds up
  -- in, the graveyard for every printing in this pool, and a cycled card cannot be
  -- on the battlefield. eventTriggers' `cycledCard` is what actually serves it.
  TriggerCondition.SelfCycled -> Set.singleton Zone.Graveyard
  -- CR 113.6k's fourth zone: rule 702.94a's reveal happens FROM a hand and rule
  -- 701.20b leaves the card there, so this condition cannot trigger from the
  -- battlefield at all and the hand is the one zone it can. eventTriggers'
  -- `revealedInHand` is what serves it.
  TriggerCondition.SelfRevealedForMiracle -> Set.singleton Zone.Hand
  -- CR 113.6k's exception again, on SelfCycled's argument: CR 701.9a discards a
  -- card from a HAND, so this condition can never trigger from the battlefield,
  -- and the graveyard rule 701.9a moves the card to is the one zone the scan
  -- meets it in.
  --
  -- eventTriggers' `inGraveyards` is what serves it, gated on exactly this
  -- answer -- Pawl.TriggerSpec's Bartered Cow cases go red if this arm answers
  -- the battlefield. No candidate source of its own is owed: `cycledCard`
  -- recovers the card the CYCLING cause named, which rule 702.29c makes narrower
  -- than this condition rather than a gap under it.
  TriggerCondition.SelfDiscarded -> Set.singleton Zone.Graveyard
  -- CR 113.6's default: the bearer watches from the battlefield, so a card in a
  -- graveyard does not see an opponent discard.
  TriggerCondition.PlayerDiscards _ -> battlefield
  -- CR 113.6's default too, and NOT the graveyard SelfCycled answers above: the
  -- bearer here is a permanent, not the card that was cycled, so the zone the
  -- cycled card winds up in (rule 702.29c's second sentence) is nothing to do
  -- with where this ability functions. eventTriggers' `cycledCard` serves the
  -- self-scoped condition only.
  TriggerCondition.PlayerCycles _ -> battlefield
  -- CR 113.6's default again: Erudite Wizard watches its controller's draws from
  -- the battlefield. CR 702.94a's miracle answers a hand below, and it is a
  -- different condition -- it watches the REVEAL, not the draw.
  TriggerCondition.PlayerDrawsNthCard {} -> battlefield
  -- The condition this predicate exists for: a card cannot be put into a graveyard
  -- from a library while on the battlefield, so this can never trigger from there
  -- and the graveyard it lands in is the one zone it can.
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> Set.singleton Zone.Graveyard
  -- The graveyard for a NEARER reason than the library condition's, and the one
  -- that matters: this condition CAN follow a battlefield-to-graveyard move, but CR
  -- 603.6c's last sentence denies it the leaves-the-battlefield look-back, so
  -- the bearer is never the permanent on the battlefield -- it is always the card
  -- that arrived in the graveyard. Nothing it can trigger from is the
  -- battlefield, so CR 113.6k puts it in every zone it can, and the graveyard is
  -- where the scan meets it whatever zone the card came from.
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> Set.singleton Zone.Graveyard
  -- The mirror image, defaulting for a reason rather than by omission: a dies trigger CAN
  -- trigger from the battlefield, which CR 603.10a's look-back is what makes true
  -- of a permanent that is a graveyard card by the time the scan runs.
  -- `leftBattlefield` serves it from CR 608.2h; neither graveyard source may, or
  -- the ability would be read off the graveyard card and credited to its owner.
  TriggerCondition.SelfDies -> battlefield
  -- The same answer one step further: this condition's bearer is not the permanent
  -- that died at all, and watches from the battlefield.
  TriggerCondition.PermanentDies _ -> battlefield
  -- The batch reading watches from the battlefield too, and for the arm above's
  -- reason: its bearer is a bystander.
  TriggerCondition.PermanentsDie _ -> battlefield
  -- The same CR 603.10a answer as both dies conditions, and harder to miss here:
  -- the destination may be a hand or library, and an ability found in a GRAVEYARD
  -- could not be what fired for a permanent that went somewhere else.
  TriggerCondition.SelfLeavesTheBattlefield -> battlefield
  -- The same answer once more, and here it is the ONLY one CR 113.6k could give:
  -- the bearer is a bystander that never left the battlefield at all.
  TriggerCondition.PermanentLeavesTheBattlefield _ -> battlefield
  -- CR 113.6k's third zone, and rule 702.55c states it outright: "triggered
  -- abilities of cards with haunt that refer to the haunted creature can trigger
  -- in the exile zone". A permanent on the battlefield haunts nothing -- only a
  -- card Effect.ExileHaunting put in exile is in GameState.haunting at all -- so
  -- this condition cannot trigger from the battlefield, and exile is the one zone
  -- it can trigger from. eventTriggers' `inExile` is what serves it.
  TriggerCondition.HauntedCreatureDies -> Set.singleton Zone.Exile
  -- CR 113.6's default again: the bearer watches from the battlefield.
  TriggerCondition.SpellOrAbilityCounters _ -> battlefield
  -- The same default: Selfless Squire watches damage addressed to its controller from
  -- the battlefield, and a card in a graveyard sees nothing prevented.
  TriggerCondition.DamageToPlayerPrevented _ -> battlefield
  -- CR 113.6's default: the Vindicator's prevention ability functions on the
  -- battlefield, so the trigger paired with it watches from there too.
  TriggerCondition.SelfPreventsDamage -> battlefield
  -- CR 113.6's default once more: Ajani's Pridemate has to be on the battlefield
  -- to receive the counter its own ability puts on it.
  TriggerCondition.PlayerGainsLife _ -> battlefield
  -- And once more: Exquisite Blood is an enchantment, and CR 113.6 leaves its
  -- ability functioning only where the permanent is.
  TriggerCondition.PlayerLosesLife _ -> battlefield
  -- CR 122.1's first sentence puts counters on OBJECTS, and CR 714.3 keeps a
  -- Saga's lore counters on the permanent -- so CR 113.6's default holds and a
  -- chapter ability functions from the battlefield alone.
  TriggerCondition.SelfCountersReached {} -> battlefield
  TriggerCondition.SelfBecomesClassLevel _ -> battlefield
  TriggerCondition.SelfLastCounterRemoved _ -> battlefield
  TriggerCondition.SelfCountersRemoved _ -> battlefield
  -- CR 113.6's default: Young Pyromancer watches the stack from the battlefield,
  -- and a card in a graveyard sees nothing cast.
  TriggerCondition.SpellCast {} -> battlefield
  -- CR 113.6k, the second zone it reaches: CR 601.2a moves the object to the stack
  -- to cast it and leaves it there, so at CR 601.2i it is on the stack and not on
  -- the battlefield -- this condition cannot trigger from there at all. The stack
  -- is the one zone it can, and eventTriggers' `spellCast` is what serves it.
  TriggerCondition.SelfCast -> Set.singleton Zone.Stack
  -- CR 113.6's default, unlike SelfCast just above: rule 702.21a prints ward on a
  -- permanent, so the bearer watches the announcement from the battlefield. A
  -- spell on the stack can become a target too, and no card in the pool is one.
  TriggerCondition.SelfBecomesTargeted _ -> battlefield
  -- CR 113.6's default again: Dormant Gomazoa is a creature and Amulet of
  -- Safekeeping an artifact, both watching their controller from the
  -- battlefield. Nothing on the stack reads its controller becoming a target.
  TriggerCondition.ControllerBecomesTarget {} -> battlefield
  -- CR 113.6's default a last time: Historian's Boon is an enchantment watching
  -- the battlefield's Sagas, and a card in a graveyard sees no chapter fire.
  TriggerCondition.SagaFinalChapterTriggers _ -> battlefield
  -- CR 113.6's default once more: Custodi Lich is a creature and watches the
  -- crown from the battlefield, so the card sees no crowning from a graveyard.
  TriggerCondition.PlayerBecomesMonarch _ -> battlefield
  -- CR 113.6's default, and never actually consulted: this condition's only carrier
  -- is a CR 603.7 delayed entry, which Event.delayedPending gathers out of
  -- GameState.delayedTriggers rather than out of a zone. The default is right
  -- anyway -- the event it matches happens on the battlefield.
  TriggerCondition.LoseControlOfBound _ -> battlefield
  -- Never consulted either, and for the same reason: CR 603.12 routes a
  -- reflexive through rule 603.7, so its only carrier is a delayed entry
  -- Event.delayedPending gathers out of GameState.delayedTriggers. EMPTY rather
  -- than CR 113.6's default, which is the honest answer here where it is not for
  -- the arm above: a reflexive is created by a RESOLVING spell or ability and
  -- watches no zone at all, its source having been able to leave before it fires.
  TriggerCondition.Reflexive -> Set.empty
  where
    battlefield = Set.singleton Zone.Battlefield

-- CR 603.2b / 109.5: does this condition restrict the turn its event may occur
-- on to the ABILITY'S CONTROLLER's turn? True for "at the beginning of YOUR
-- <step>" and for nothing else.
--
-- A CLASSIFICATION of a trigger condition, the third of the same kind as
-- eventBindingSlots and zonesTriggeredFrom above.
--
-- Its customer is the card lint. Onset.FromYourNextTurn delivers both halves of
-- "your next turn" on its own, so this no longer guards the firing -- it guards
-- the DATA: a card arming that onset over an EachTurn condition would have its
-- printed "each" silently narrowed by the window.
--
-- Exhaustive with no wildcard, for eventBindingSlots' reason: a new condition must
-- force a decision rather than defaulting to False.
controllerTurnScoped :: TriggerCondition -> Bool
controllerTurnScoped cond = case cond of
  -- CR 309.4c names no turn at all.
  TriggerCondition.RoomEntered _ -> False
  -- None of the four keyword-action conditions names a turn: CR 701.22,
  -- CR 701.25, CR 702.170 and CR 701.44 each state when their event happens
  -- and say nothing about whose turn it is.
  TriggerCondition.PlayerScries _ -> False
  TriggerCondition.PlayerSurveils _ -> False
  TriggerCondition.SelfBecomesPlotted -> False
  TriggerCondition.PermanentExplores _ -> False
  -- CR 706.1 names no turn either.
  TriggerCondition.PlayerRollsDice _ -> False
  -- False for the SelfAttacks arm's reason below, which is exactly this case one
  -- rule earlier: CR 508.1g exerts on the ACTIVE player's turn, and CR 109.5's
  -- "you" is the ability's controller, so a stolen Glory-Bound Initiate is
  -- exerted on its thief's turn rather than on its owner's.
  TriggerCondition.SelfExerted -> False
  -- CR 701.3a names no turn: an Aura can be cast, and an Equipment equipped, on
  -- any turn its controller has priority for.
  TriggerCondition.SelfBecomesAttachedBy _ -> False
  -- One of the two arms carrying a TurnScope, and the one the lint below this
  -- was written for (CR 603.3a, CR 109.5).
  TriggerCondition.StepBegins (StepBegins.MkStepBegins _ TurnScope.ControllersTurn) -> True
  -- "Each <step>" admits every player's turn, the pairing the lint rejects.
  TriggerCondition.StepBegins (StepBegins.MkStepBegins _ TurnScope.EachTurn) -> False
  -- And "during an opponent's <step>" admits every turn but the controller's,
  -- which is not the controller's turn either. No card prints it, so the lint
  -- cannot reach this arm; answering it any other way would make the
  -- classification wrong for the sake of an unreachable case.
  TriggerCondition.StepBegins (StepBegins.MkStepBegins _ TurnScope.OpponentsTurn) -> False
  -- CR 702.179d's "during YOUR turn" is the same restriction StepBegins spells
  -- with a TurnScope, written into the condition itself because rule 702.179d
  -- states it there. No card bears this condition, so the lint this feeds cannot
  -- reach it; answering False anyway would make the classification wrong for the
  -- sake of an unreachable case.
  TriggerCondition.OpponentLostLifeDuringYourTurn -> True
  -- None of the rest is turn-scoped: each names an event that can happen on
  -- anybody's turn.
  TriggerCondition.SelfEnters -> False
  TriggerCondition.PermanentEnters _ -> False
  -- CR 709.5e restricts the special action to the taker's own turn, but CR
  -- 709.5f's keyword action and CR 709.5d's entry have no such restriction, and
  -- this classification is about the CONDITION rather than about how the
  -- designation came to be given.
  TriggerCondition.SelfHalfUnlocked _ -> False
  -- CR 709.5i says nothing about whose turn it is, for SelfHalfUnlocked's reason.
  TriggerCondition.RoomFullyUnlocked _ -> False
  -- `all`, because the lint this feeds asks whether the WHOLE ability is already
  -- narrowed to its controller's turn, and one clause that admits every turn is
  -- enough to make the answer no. Vacuously True for the empty list, which no card
  -- writes.
  TriggerCondition.AnyOf conditions -> all controllerTurnScoped conditions
  -- CR 702.37e offers the special action "any time you have priority", which is
  -- every turn and not only the controller's own.
  TriggerCondition.SelfTurnedFaceUp -> False
  -- CR 701.27a names no turn at all: an activated ability can transform a
  -- permanent whenever it can be activated, and CR 702.145c's sweep fires on
  -- whosever turn it becomes night.
  TriggerCondition.SelfTransformedInto _ -> False
  -- The same rule from the watcher's side, and doubly so: CR 702.37e lets any
  -- player take the action on any turn, and the watcher is not even the player
  -- taking it.
  TriggerCondition.PermanentTurnedFaceUp _ -> False
  -- CR 702.112a's ability fires on combat damage to a player, which any player's
  -- turn can carry, and the watcher's turn is not asked about at all.
  TriggerCondition.PermanentBecomesDesignated {} -> False
  -- Rule 702.100b names no turn either: a creature can evolve on anyone's.
  TriggerCondition.SelfEvolves -> False
  -- Rule 702.134c names none either. CR 508.1 does make every mentoring happen on
  -- the mentor's controller's turn, but that is a consequence of what mentor
  -- watches rather than a narrowing this condition states, and the Equipment's
  -- controller need not be that player.
  TriggerCondition.AttachedCreatureMentors -> False
  -- CR 303.4 names no turn either: an enchanted creature can die on anyone's.
  TriggerCondition.AttachedCreatureDies -> False
  -- Nor does CR 701.26a: an enchanted creature can be tapped on anyone's turn,
  -- and Betrayal's whole point is that the Aura's controller is not the tapping
  -- creature's.
  TriggerCondition.AttachedCreatureBecomesTapped -> False
  -- Rule 702.149c names no turn either, and the SelfAttacks arm below settles the
  -- consequence: CR 508.1a makes the training happen on the ACTIVE player's turn,
  -- which is not CR 109.5's "you" -- a stolen creature trains on its thief's turn.
  TriggerCondition.SelfTrains -> False
  -- CR 701.21a says nothing about whose turn it is, and neither does the printed
  -- "whenever a player sacrifices a permanent".
  TriggerCondition.PermanentSacrificed -> False
  TriggerCondition.StateIs _ -> False
  TriggerCondition.SelfDealsCombatDamageToPlayer -> False
  TriggerCondition.SelfIsDealtDamage -> False
  TriggerCondition.PermanentDealsCombatDamageToPlayer _ -> False
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
  TriggerCondition.SelfCycled -> False
  TriggerCondition.SelfRevealedForMiracle -> False
  TriggerCondition.SelfDiscarded -> False
  TriggerCondition.PlayerDiscards _ -> False
  TriggerCondition.PlayerCycles _ -> False
  TriggerCondition.PlayerDrawsNthCard {} -> False
  -- CR 508.1a makes this the ACTIVE player's turn, which is not the same thing:
  -- CR 109.5's "you" is the ability's controller, and a stolen creature attacks on
  -- its thief's turn.
  TriggerCondition.SelfAttacks _ -> False
  TriggerCondition.SelfAttacksWithAnother _ -> False
  TriggerCondition.CreatureAttacksAlone _ -> False
  -- CR 506.2 makes this one an OPPONENT's turn every time, which is not the
  -- controller's turn either -- StepBegins' OpponentsTurn arm above answers the
  -- same way for the same reason.
  TriggerCondition.CreatureAttacksYou -> False
  TriggerCondition.AttachedPlayerIsAttacked -> False
  -- The only arm around here that can answer True, and only on one relation: CR
  -- 508.1 lets only the active player declare attackers, so the declarer named
  -- by the event is always the active player. You therefore pins the event to CR
  -- 109.5's "you"'s own turn; Opponent pins it to somebody else's; AnyPlayer
  -- pins nothing. SelfAttacks answers False because it pins the CREATURE and not
  -- the declarer -- a stolen creature attacks on its thief's turn -- which is
  -- the comparison this arm does make.
  TriggerCondition.PlayerAttacks relation -> relation == PlayerRelation.You
  -- The arm above's comparison, over the same relation: rule 508.3c's Filter
  -- narrows WHICH creatures were declared and says nothing about whose turn it
  -- is.
  TriggerCondition.PlayerAttacksWith payload -> PlayerAttacksWith.player payload == PlayerRelation.You
  -- The same comparison over the ATTACKING side, which is the field rule
  -- 508.3e's event pins to the active player; the attacked side says nothing
  -- about whose turn it is.
  TriggerCondition.PlayerAttacksPlayer relation -> relation == PlayerRelation.You
  TriggerCondition.SelfAttacksPlayerWithMostLife -> False
  TriggerCondition.SelfBlocks -> False
  TriggerCondition.SelfBlocksCreature _ -> False
  TriggerCondition.SelfBlocksAtLeast _ -> False
  TriggerCondition.SelfBlocksOneOrMore _ -> False
  TriggerCondition.SelfBecomesBlocked -> False
  TriggerCondition.SelfBecomesBlockedBy _ -> False
  TriggerCondition.SelfBecomesBlockedByOneOrMore _ -> False
  TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> False
  TriggerCondition.SelfAttacksUnblocked -> False
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> False
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> False
  TriggerCondition.SelfDies -> False
  TriggerCondition.PermanentDies _ -> False
  TriggerCondition.PermanentsDie _ -> False
  TriggerCondition.SelfLeavesTheBattlefield -> False
  TriggerCondition.PermanentLeavesTheBattlefield _ -> False
  -- Rule 702.55b names no turn.
  TriggerCondition.HauntedCreatureDies -> False
  TriggerCondition.SpellOrAbilityCounters _ -> False
  -- Damage can be prevented on anybody's turn.
  TriggerCondition.DamageToPlayerPrevented _ -> False
  -- Damage can be prevented on anybody's turn, the arm above's reason.
  TriggerCondition.SelfPreventsDamage -> False
  -- Life can be gained on anybody's turn. CR 702.179d's loss condition above says
  -- "during your turn" and this one does not, which is the two rules' own
  -- difference rather than an omission here.
  TriggerCondition.PlayerGainsLife _ -> False
  -- And life can be LOST on anybody's turn. This is the arm where CR 702.179d's
  -- condition is closest to being duplicated and is not: that one reads the very
  -- same GameEvent.LifeLost but only during its controller's turn, which is the
  -- printed speed rule rather than anything a card's "whenever an opponent loses
  -- life" says.
  TriggerCondition.PlayerLosesLife _ -> False
  -- CR 122.6 puts counters on at any time, and a Saga can receive one on an
  -- opponent's turn: CR 714.3a's entry replacement fires whenever the Saga enters,
  -- which a flash effect or an opponent's Sneak Attack could make happen. Only CR
  -- 714.3c's turn-based action is the controller's own turn, and that is the
  -- action's restriction rather than this condition's.
  TriggerCondition.SelfCountersReached {} -> False
  TriggerCondition.SelfBecomesClassLevel _ -> False
  TriggerCondition.SelfLastCounterRemoved _ -> False
  TriggerCondition.SelfCountersRemoved _ -> False
  -- CR 601.2i says nothing about whose turn it is and CR 117.1a lets an instant
  -- be cast on anybody's, so the answer is the condition's own TurnScope --
  -- StepBegins' arms one more time, and for its reason (CR 603.3a, CR 109.5).
  -- Brineborn Cutthroat's OpponentsTurn is turn-scoped and is not the
  -- CONTROLLER's turn, which is the only thing this classification asks.
  TriggerCondition.SpellCast (SpellCast.MkSpellCast _ TurnScope.ControllersTurn _ _) -> True
  TriggerCondition.SpellCast (SpellCast.MkSpellCast _ TurnScope.EachTurn _ _) -> False
  TriggerCondition.SpellCast (SpellCast.MkSpellCast _ TurnScope.OpponentsTurn _ _) -> False
  -- The same rule with no TurnScope to read: a spell can be cast on anybody's
  -- turn, so its own cast trigger is not the controller's-turn kind either.
  TriggerCondition.SelfCast -> False
  -- The condition carries no TurnScope, and an opponent can target the bearer on
  -- anybody's turn.
  TriggerCondition.SelfBecomesTargeted _ -> False
  -- Its player-side sibling likewise: a spell can name its controller on anybody's
  -- turn.
  TriggerCondition.ControllerBecomesTarget {} -> False
  -- CR 714.3c's turn-based action falls on the Saga controller's own turn, but
  -- nothing restricts this CONDITION to it: CR 714.3a's entry replacement can put
  -- a Saga's last lore counter on during anybody's turn, and the watcher is not
  -- even the Saga's controller under the Opponent relation.
  TriggerCondition.SagaFinalChapterTriggers _ -> False
  -- The condition carries no TurnScope, and CR 725 restricts a crowning to no
  -- turn at all: CR 725.1's "an effect instructs a player to become the monarch"
  -- can resolve on anybody's turn, and CR 725.2's crown steal happens in the
  -- combat damage step of whoever is attacking. The watcher may or may not be
  -- that player, which is exactly what makes this not controller-scoped.
  TriggerCondition.PlayerBecomesMonarch _ -> False
  -- Carries no TurnScope at all: CR 514.2 ends the control effect Ray of Command's
  -- delayed ability watches on the turn the spell resolved, but that is the
  -- DURATION's doing rather than a restriction the condition states.
  TriggerCondition.LoseControlOfBound _ -> False
  -- Carries no TurnScope either, and CR 603.12 restricts a reflexive to no turn:
  -- it fires on whatever turn the ability that created it resolved on, which for
  -- an instant-speed creator is an opponent's as readily as its controller's.
  TriggerCondition.Reflexive -> False

-- CR 603.8: state triggers. For every battlefield permanent, each StateIs ability
-- it bears whose condition is currently TRUE and which has no instance of ITSELF
-- already on the stack -- counted, so an object carrying the same state-triggered
-- ability twice arms both (CR 603.2 makes each of them an ability in its own
-- right).
--
-- Armedness is DERIVED, never stored: CR 603.8's three outcomes are all "no longer
-- on the stack", so an instance sitting there is the whole suppression rule and
-- there is no bookkeeping field to leak. No triggered-but-not-yet-placed window
-- either, Engine.placePendingTriggers acting within the same settle step.
--
-- A trigger whose modes are all unfillable would be removed from the stack (CR
-- 603.3c) and re-trigger on the next settle pass while its condition held, which
-- would not terminate. No card in the pool can do that, and the first that could
-- is the one that must revisit this.
--
-- Not implemented: a state trigger borne by an EMBLEM, which CR 114.4 would have
-- function in the command zone -- this scan reads the battlefield alone, where
-- eventTriggers reads both (#1400).
stateTriggers :: GameState -> [PendingTrigger]
stateTriggers gs
  -- A stack id whose object can't be found: fail CLOSED, not open. This runs
  -- inside the settleForPriority fixpoint, so a lost suppression loops forever
  -- -- a hang, not a wrong answer -- while failing closed costs at most one
  -- settle pass. Unreachable: Game.cease removes the stack entry and its object
  -- together. Hoisted to the whole function because that is what the per-ability
  -- check it replaces amounted to: one unreadable stack entry suppressed every
  -- ability of every source.
  | any (\sid -> Maybe.isNothing (Game.lookupObject sid gs)) (GameState.stack gs) = []
  | otherwise = concatMap forOne (Set.toAscList (GameState.battlefield gs))
  where
    -- The same hoist eventTriggers' `grants` binding makes.
    grants = Projection.controlGrants gs
    -- CR 603.8's suppression, COUNTED rather than tested. Scoped to (source,
    -- ability), so two permanents bearing the identical triggered ability
    -- suppress independently -- one instance per source, not one for the whole
    -- board.
    --
    -- A count rather than an "is there one?" because CR 603.2 makes each ability
    -- its own ability: one object may carry two identical state-triggered
    -- abilities, and CR 603.8 holds each back only until THAT ability's own
    -- instance leaves the stack. Object.source cannot tell those two instances
    -- apart -- they are equal values -- but it does not have to. Which of N
    -- identical abilities a given instance came from is unobservable, so N live
    -- copies minus K instances already on the stack is the exact answer: it
    -- reproduces the single-ability behavior at N = 1, and lets one of a twin
    -- pair re-arm while the other's instance still sits there
    -- (TriggerSpec, "one instance leaving re-arms ITS ability").
    instancesOnStack srcId ab =
      let isInstance sid = case fmap Object.source (Game.lookupObject sid gs) of
            -- The SOURCE and the ABILITY, rather than the whole record: CR
            -- 603.7a's creation moment also rides on that arm, and an instance
            -- of this ability is an instance of it however it got here.
            Just (Source.OfTrigger triggered) -> TriggeredAbilitySource.source triggered == srcId && TriggeredAbilitySource.ability triggered == ab
            _ -> False
       in length (filter isInstance (GameState.stack gs))
    forOne oid = case Projection.controllerOfGiven grants Set.empty oid gs of
      Nothing -> []
      -- CR 603.3a / 109.5: the ability's controller is its source's, and that is
      -- what "you" in the condition means. Outside the layer fold, so the ViewOf
      -- is the FULL projection rather than the layer-bounded one.
      Just ctrl ->
        let live ab = liveCondition (TriggeredAbility.condition ab)
            liveCondition condition = case condition of
              TriggerCondition.StateIs cond ->
                Condition.holds (Projection.fullView gs) (Filter.contextFor (Just ctrl) (Just oid)) gs oid cond
              TriggerCondition.SelfEnters -> False
              -- CR 309.4c is an EVENT trigger too: the marker MOVING into the room
              -- is what fires it, not the marker sitting there.
              TriggerCondition.RoomEntered _ -> False
              -- CR 603.2 event triggers, all four: a scry, a surveil, a card
              -- becoming plotted and an explore are things that HAPPEN, each with
              -- its own log entry, and none of them is a CR 603.8 state that could
              -- be true standing still.
              TriggerCondition.PlayerScries _ -> False
              TriggerCondition.PlayerSurveils _ -> False
              TriggerCondition.SelfBecomesPlotted -> False
              TriggerCondition.PermanentExplores _ -> False
              -- CR 603.2 once more: a die roll is something that HAPPENS, with its own log
              -- entry, never a CR 603.8 state that could be true standing still.
              TriggerCondition.PlayerRollsDice _ -> False
              -- CR 603.2 again: being exerted is something that happens, with its
              -- own log entry, and CR 701.43b makes "already exerted" no bar to
              -- exerting again -- so there is no standing state to be true.
              TriggerCondition.SelfExerted -> False
              -- CR 603.2 once more: becoming attached is something that HAPPENS,
              -- with its own log entry. Standing attached is a state, but no
              -- condition here asks about it.
              TriggerCondition.SelfBecomesAttachedBy _ -> False
              -- CR 603.6a is an EVENT trigger, matched against the log; nothing
              -- about it is a CR 603.8 state.
              TriggerCondition.PermanentEnters _ -> False
              TriggerCondition.StepBegins {} -> False
              TriggerCondition.SelfDealsCombatDamageToPlayer -> False
              TriggerCondition.SelfIsDealtDamage -> False
              TriggerCondition.PermanentDealsCombatDamageToPlayer _ -> False
              TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
              TriggerCondition.OpponentLostLifeDuringYourTurn -> False
              TriggerCondition.SelfAttacks _ -> False
              TriggerCondition.SelfAttacksWithAnother _ -> False
              TriggerCondition.CreatureAttacksAlone _ -> False
              TriggerCondition.CreatureAttacksYou -> False
              TriggerCondition.AttachedPlayerIsAttacked -> False
              TriggerCondition.PlayerAttacks _ -> False
              TriggerCondition.PlayerAttacksWith {} -> False
              TriggerCondition.PlayerAttacksPlayer {} -> False
              TriggerCondition.SelfAttacksPlayerWithMostLife -> False
              TriggerCondition.SelfBlocks -> False
              TriggerCondition.SelfBlocksCreature _ -> False
              TriggerCondition.SelfBlocksAtLeast _ -> False
              TriggerCondition.SelfBlocksOneOrMore _ -> False
              TriggerCondition.SelfBecomesBlocked -> False
              TriggerCondition.SelfBecomesBlockedBy _ -> False
              TriggerCondition.SelfBecomesBlockedByOneOrMore _ -> False
              TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> False
              TriggerCondition.SelfAttacksUnblocked -> False
              TriggerCondition.SelfCycled -> False
              TriggerCondition.SelfRevealedForMiracle -> False
              TriggerCondition.SelfDiscarded -> False
              TriggerCondition.PlayerDiscards _ -> False
              TriggerCondition.PlayerCycles _ -> False
              TriggerCondition.PlayerDrawsNthCard {} -> False
              TriggerCondition.SelfPutIntoGraveyardFromLibrary -> False
              TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> False
              TriggerCondition.SelfDies -> False
              TriggerCondition.PermanentDies _ -> False
              TriggerCondition.PermanentsDie _ -> False
              TriggerCondition.SelfLeavesTheBattlefield -> False
              TriggerCondition.PermanentLeavesTheBattlefield _ -> False
              TriggerCondition.HauntedCreatureDies -> False
              TriggerCondition.SpellOrAbilityCounters _ -> False
              TriggerCondition.DamageToPlayerPrevented _ -> False
              TriggerCondition.SelfPreventsDamage -> False
              TriggerCondition.PlayerGainsLife _ -> False
              TriggerCondition.PlayerLosesLife _ -> False
              -- CR 603.3b's condition is an EVENT trigger too, and the event is
              -- another ability triggering: nothing about it is a state a settle
              -- could re-read.
              TriggerCondition.SagaFinalChapterTriggers _ -> False
              -- CR 603.2 event trigger, not a CR 603.8 state: this fires on a
              -- player BECOMING the monarch, and a settle re-reading "is the
              -- monarch" would fire it again every time until the crown moved.
              TriggerCondition.PlayerBecomesMonarch _ -> False
              -- CR 603.2 event trigger too, and not a state: it fires on control
              -- CHANGING, and a settle re-reading "somebody else controls it" would
              -- fire it again on every pass thereafter.
              TriggerCondition.LoseControlOfBound _ -> False
              -- CR 603.12 sends a reflexive through rule 603.7, and CR 603.8's
              -- state triggers are a different family: nothing about "when you
              -- do" is a state that could be standing true.
              TriggerCondition.Reflexive -> False
              -- CR 709.5h is an EVENT trigger: it fires on the permanent BEING
              -- GIVEN the designation, which CR 709.5c leaves it holding
              -- thereafter, so a state read would fire it again every time the
              -- board settles.
              TriggerCondition.SelfHalfUnlocked _ -> False
              -- CR 708.7 is an EVENT trigger for the same reason: it fires on the
              -- permanent BEING turned face up, and CR 708.8 leaves it face up
              -- thereafter -- so a state read would fire it again every settle,
              -- for as long as the permanent stayed on the battlefield.
              TriggerCondition.SelfTurnedFaceUp -> False
              -- CR 701.27a is an EVENT trigger for that reason exactly: CR
              -- 712.18 leaves the permanent on its new face thereafter, so a
              -- state read would fire it again on every settle.
              TriggerCondition.SelfTransformedInto _ -> False
              -- And the watcher's form is an EVENT trigger for the same reason,
              -- more plainly still: a board on which some permanent is face up
              -- says nothing about which of them was ever TURNED over, so there
              -- is no state here to read at all.
              TriggerCondition.PermanentTurnedFaceUp _ -> False
              -- CR 702.112b's designation is exactly that shape once more: the
              -- permanent keeps it, so a state read would fire every settle.
              TriggerCondition.PermanentBecomesDesignated {} -> False
              -- CR 702.100b is an EVENT trigger and leaves no state at all behind:
              -- the counters it put are indistinguishable from any others.
              TriggerCondition.SelfEvolves -> False
              -- CR 702.134c likewise, and one step further removed: what it fires
              -- on is a resolution, and the counter that resolution put is a
              -- counter like any other, so the board afterwards says nothing about
              -- which creature mentored which.
              TriggerCondition.AttachedCreatureMentors -> False
              -- CR 700.4's death is an EVENT, and the board afterwards cannot
              -- say which permanent an Aura in a graveyard used to enchant.
              TriggerCondition.AttachedCreatureDies -> False
              -- CR 701.26a's tap is an EVENT too. A tapped enchanted permanent is
              -- a state the board can read, which is exactly why this must be
              -- False: CR 603.2e says a "becomes" condition does not retrigger
              -- while the state persists, and a state trigger would do nothing but.
              TriggerCondition.AttachedCreatureBecomesTapped -> False
              -- CR 702.149c the same: it fires on a resolution, and the counter
              -- that resolution put is a counter like any other, so the board
              -- afterwards says nothing about which creature trained.
              TriggerCondition.SelfTrains -> False
              -- CR 701.21a is a game ACTION, so this is an event trigger too: it
              -- fires on the moment the permanent is sacrificed, and the board
              -- afterwards holds no state a read could recover.
              TriggerCondition.PermanentSacrificed -> False
              -- CR 714.2b is an EVENT trigger too: it fires on the moment counters
              -- are PUT ON, not on the count standing at or above N -- which is
              -- exactly the difference CR 603.8 draws, and the reason a Saga does
              -- not re-run its final chapter for as long as it sits there.
              TriggerCondition.SelfCountersReached {} -> False
              TriggerCondition.SelfBecomesClassLevel _ -> False
              TriggerCondition.SelfLastCounterRemoved _ -> False
              TriggerCondition.SelfCountersRemoved _ -> False
              TriggerCondition.SpellCast {} -> False
              TriggerCondition.SelfCast -> False
              TriggerCondition.SelfBecomesTargeted _ -> False
              TriggerCondition.ControllerBecomesTarget {} -> False
              -- CR 709.5i is an EVENT trigger, for CR 709.5h's reason one arm up:
              -- it fires on the LAST designation arriving, and CR 709.5c leaves
              -- the permanent holding both thereafter, so a state read would fire
              -- it again on every settle.
              TriggerCondition.RoomFullyUnlocked _ -> False
              -- `any`, which is matchesTrigger's AnyOf arm read into this scan:
              -- an ability with a CR 603.8 clause is a state trigger, whatever
              -- else it also has. Never True today -- Pawl.CardSpec's lint
              -- forbids a StateIs inside an AnyOf, precisely so that an ability
              -- cannot be gathered by this scan and by the event scan at once --
              -- so what this arm really says is that the classification stays
              -- coherent if that lint is ever relaxed.
              TriggerCondition.AnyOf conditions -> any liveCondition conditions
            lives = filter live (Projection.triggeredAbilitiesOf oid gs)
            -- Each live copy against the copies of itself that came earlier in
            -- the list, which gives it a 1-based ordinal among its equals: the
            -- j-th copy is armed exactly when fewer than j instances of it are
            -- already on the stack. That is the N-minus-K subtraction
            -- instancesOnStack describes, written without ever needing an Ord on
            -- a triggered ability.
            armed (before, ab) = 1 + length (filter (ab ==) before) > instancesOnStack oid ab
            pend ab = PendingTrigger.MkPendingTrigger (TriggerSource.OfObject oid) ctrl ab Map.empty Nothing
         in fmap (pend . snd) (filter armed (zip (List.inits lives) lives))

-- CR 603.7: delayed abilities whose trigger event is among these events. An entry
-- that TRIGGERS is REMOVED from the store (CR 603.7b) unless it carries a stated
-- duration, which is that rule's own exception -- one of Expiry's sweeps ends
-- those instead. The survivors are returned so the caller can store them back. CR
-- 603.7d-f: the controller travels with the entry, so a delayed ability resolves
-- under whoever controlled the spell that created it even once that spell's source
-- is gone.
--
-- `matching` matches its condition only against EVENTS, never live game state -- the
-- turn number `armed` reads is CR 603.7a's arming gate, which can only withhold a
-- match. So a stored entry whose condition is StateIs would never fire, and
-- without a stated duration would never leave the store. Not a live gap: no card
-- in this pool arms a delayed ability with a StateIs condition.
--
-- CR 603.4 is applied HERE rather than in gatherTriggers, because the surviving
-- store depends on it: "the ability triggers only if [the condition] is [true];
-- otherwise it does nothing", and CR 603.7b bounds how many times the ability
-- TRIGGERS, not how many occurrences of its event it watches. So an entry whose
-- intervening "if" is false at the occurrence has not triggered, nothing is spent
-- against 603.7b's one shot, and it stays armed for the next occurrence. No other
-- 603.7 subrule evicts an entry, so triggering and a stated duration remain its
-- only two exits.
delayedPending :: [GameEvent] -> GameState -> ([PendingTrigger], Seq.Seq DelayedTrigger)
delayedPending events gs =
  let -- CR 603.7a's floor is the watermark's job, and is all an ordinary entry
      -- needs. This is the card's OWN further restriction: an ability printed "on
      -- your next turn" fires on that one turn and no other, whatever its
      -- condition matches. Read against the LIVE turn number, so an entry with no
      -- onset is untouched.
      armed entry = case DelayedTrigger.window entry of
        TurnWindow.AnyTurn -> True
        -- The named turn has not begun, so no occurrence counts -- including one
        -- in the turn that armed the ability, which is why the onset exists.
        TurnWindow.ControllersNextTurn -> False
        -- EQUALITY, not a floor: CR 603.7a is a claim about ONE named turn, so the
        -- window has an upper end and not merely a lower one.
        TurnWindow.OnTurn n -> n == GameState.turnNumber gs
      -- WHICH events fired the entry, rather than merely whether one did: the
      -- payload reads CR 603.2's event through eventBindings below, so the match
      -- has to hand each event forward.
      matching entry =
        let cond = TriggeredAbility.condition (DelayedTrigger.ability entry)
         in -- The entry's own bindings, which is CR 603.7c's captured environment:
            -- TriggerCondition.LoseControlOfBound asks about an object named as the
            -- arming spell resolved, and the store is the only thing that still
            -- remembers it.
            if armed entry
              then filter (matchesTriggerGiven (DelayedTrigger.bindings entry) gs (DelayedTrigger.source entry) (DelayedTrigger.controller entry) cond) events
              else []
      -- CR 603.7b's exception, read through CR 603.2c. A stated duration lifts the
      -- one shot, and 603.2c then applies unmodified -- "it can trigger repeatedly
      -- if one event contains multiple occurrences" -- so every occurrence in the
      -- batch fires the entry once. Centaur Peacemaker's "each player gains 4 life"
      -- is that batch for False Cure, and TriggerSpec's three-seat board proves the
      -- count.
      --
      -- Without a duration, `take 1` is CR 603.7b's first sentence and is CORRECT
      -- for occurrences the engine records in sequence: the ability triggers the
      -- NEXT time its event occurs, which is the earliest match in the batch. The
      -- controller's choice the rule's second sentence gives is not implemented
      -- (#1711); it applies only to occurrences that are SIMULTANEOUS, and reaching
      -- it needs both an event whose occurrences share an EventGroup (#1726) and a
      -- delayedPending that takes grouped events rather than this flat list.
      firedBy entry
        | Maybe.isJust (DelayedTrigger.expiry entry) = matching entry
        | otherwise = take 1 (matching entry)
      pend entry event =
        PendingTrigger.MkPendingTrigger
          (TriggerSource.OfObject (DelayedTrigger.source entry))
          (DelayedTrigger.controller entry)
          (DelayedTrigger.ability entry)
          -- CR 603.2's event slots over CR 603.7c's captured environment, the way an
          -- object's trigger gets them in eventTriggers -- False Cure's "that
          -- player" is the seat that just gained, not one the arming spell named.
          -- Map.union is left-biased, so a name the arming environment happens to
          -- share is read as THIS firing's, which is what the printed word means.
          -- Nothing for CR 400.7f's bearer arrival: this scan looks for events a
          -- delayed entry watches, not for a bearer's own departure, and the
          -- entry's captured environment (CR 603.7c) is where what it knows about
          -- its own object comes from.
          (Map.union (eventBindings Nothing (TriggeredAbility.condition (DelayedTrigger.ability entry)) event) (DelayedTrigger.bindings entry))
          -- CR 603.7a: what tells the ability this becomes apart from one its
          -- source simply has, once it is on the stack.
          (Just (DelayedTrigger.createdAt entry))
      store = GameState.delayedTriggers gs
      -- CR 603.12's exception to all of the above, and the ONE place the reflexive
      -- form differs from an ordinary CR 603.7 entry: it is "checked immediately
      -- after being created" and triggers "based on whether the trigger event or
      -- events occurred earlier during the resolution of the spell or ability
      -- that created them" -- which is a question about the resolution that is
      -- over, not about this batch's log. Pawl.Engine.Resolve appends such an
      -- entry only from the CR 118.12 pay-gate branch that actually ran, so the
      -- entry's EXISTENCE is the affirmative answer and no event is needed. It
      -- therefore fires at the first gather after it was armed, which CR 603.3
      -- makes the next time a player would receive priority.
      --
      -- `armed` still gates it, so the data cannot say two things at once: an
      -- onset would name a turn CR 603.12's "immediately" has already denied, and
      -- the entry would sit unfired rather than firing early. No card can reach
      -- that -- CardSpec's onset lint needs a controller-scoped condition and this
      -- one is not -- so the guard is a fence, not a live branch.
      reflexive entry = TriggeredAbility.condition (DelayedTrigger.ability entry) == TriggerCondition.Reflexive
      -- One firing, whatever the batch holds. CR 603.12a's second sentence wants
      -- exactly that for a cost payable several times, and `spent` below retires
      -- the entry immediately after, an expiry-less entry having CR 603.7b's one
      -- shot. Not implemented: that rule's FIRST sentence, once per occurrence
      -- (#2121).
      bare entry =
        PendingTrigger.MkPendingTrigger
          (TriggerSource.OfObject (DelayedTrigger.source entry))
          (DelayedTrigger.controller entry)
          (DelayedTrigger.ability entry)
          (DelayedTrigger.bindings entry)
          -- CR 603.12: a reflexive ability follows CR 603.7, so it carries the
          -- creation moment too. No card exercises the pairing -- nothing
          -- reflexive transforms -- so this is CR 701.27f as the rule states it
          -- rather than a behaviour a test pins.
          (Just (DelayedTrigger.createdAt entry))
      -- CR 603.2 plus CR 603.4: the event matched AND the intervening "if" held,
      -- which together are what "triggered" means. Per occurrence, since CR 603.4
      -- asks about the moment the event occurs. AFTER firedBy rather than inside
      -- it, so an entry with no stated duration still commits to the earliest
      -- match and then triggers or does not: this docstring's CR 603.4 paragraph
      -- has the entry survive an occurrence whose "if" was false, not skip past it
      -- to a later one in the same batch.
      triggered entry
        | reflexive entry = filter (interveningHolds gs) [bare entry | armed entry]
        | otherwise = filter (interveningHolds gs) (fmap (pend entry) (firedBy entry))
      -- Triggering spends the one shot only for an entry with no stated duration.
      spent entry = not (null (triggered entry)) && Maybe.isNothing (DelayedTrigger.expiry entry)
   in (concatMap triggered store, Seq.filter (not . spent) store)

-- CR 603.7a: the printed Onset as the game first stores it. The delayed-trigger
-- twin of Expiry.arm, deliberately blind to the board -- unlike a duration, an
-- onset has nothing to bake in when the ability is created, "your next turn" being
-- a boundary that has not happened yet. settleOnsets supplies the number.
armOnset :: Onset -> TurnWindow
armOnset onset = case onset of
  Onset.Immediately -> TurnWindow.AnyTurn
  Onset.FromYourNextTurn -> TurnWindow.ControllersNextTurn

-- CR 603.7a: a turn has BEGUN, so settle every delayed entry waiting for one and
-- drop every entry whose turn is now over. Engine.beginTurnOf calls this once the
-- new turn's number and active player are in place, and only for a turn that
-- actually begins -- CR 614.10a read on the turn axis, so CR 800.4k's turn a
-- departed seat never begins is walked past without settling anything.
--
-- The turn handoff is the ONLY moment either transition can be made, which is what
-- makes this a boundary sweep rather than something derived on demand. Two of
-- them, in this order:
--
-- 1. WAITING -> THIS TURN, for an entry whose controller is the player whose turn
--    this is. The number is sampled here because nothing in GameState remembers
--    which player each earlier turn belonged to, so the question cannot be
--    answered later. Sampled once and thereafter only ever cleared.
--
-- 2. THIS TURN -> GONE, for an entry whose settled turn is behind us. Its trigger
--    event cannot occur again, so CR 603.7a has already decided the matter.
--    Dropping it is hygiene: an entry that can never fire and states no duration
--    would otherwise outlive the game.
--
-- Ordered settle-then-drop within one pass: an entry settled onto THIS turn
-- carries this turn's number, and the drop removes only numbers strictly behind.
--
-- CR 603.7b's one shot is untouched -- this ends entries by the CALENDAR, and
-- triggering still ends them in delayedPending. An entry with a stated duration is
-- dropped here too, and rightly: a duration keeps an ability armed for its event's
-- next occurrence, not for a turn its printed text never named.
settleOnsets :: GameState -> GameState
settleOnsets gs =
  let settled entry = case DelayedTrigger.window entry of
        -- The turn that is beginning IS the one the printed phrase named exactly
        -- when it belongs to the entry's controller (CR 603.7d-f).
        TurnWindow.ControllersNextTurn
          | DelayedTrigger.controller entry == GameState.activePlayer gs ->
              entry {DelayedTrigger.window = TurnWindow.OnTurn (GameState.turnNumber gs)}
        -- Anyone else's turn, including an intervening opponent's: still waiting.
        TurnWindow.ControllersNextTurn -> entry
        TurnWindow.AnyTurn -> entry
        -- Already settled, and this is a later turn -- `live` is what ends it.
        TurnWindow.OnTurn _ -> entry
      live entry = case DelayedTrigger.window entry of
        TurnWindow.AnyTurn -> True
        TurnWindow.ControllersNextTurn -> True
        TurnWindow.OnTurn n -> n >= GameState.turnNumber gs
   in gs {GameState.delayedTriggers = Seq.filter live (fmap settled (GameState.delayedTriggers gs))}

-- Everything that has triggered and is not yet on the stack, from all three
-- sources, plus the delayed store as it stands afterwards. One function, so
-- Pawl.Engine.Engine never needs to know how many sources there are.
--
-- Takes the GROUPED events, because eventTriggers is the one gatherer whose
-- answer depends on which of them were simultaneous (CR 603.10a). The other two
-- take the events alone.
gatherTriggers :: [LoggedEvent.LoggedEvent] -> GameState -> ([PendingTrigger], Seq.Seq DelayedTrigger)
gatherTriggers grouped gs =
  let events = fmap LoggedEvent.event grouped
      -- Already CR 603.4-filtered: delayedPending has to run the check itself,
      -- since which entries survive in its second component depends on the
      -- answer. Running it over these again would be a redundant no-op, the
      -- GameState being the same one, so only the other two are filtered here.
      (fromDelayed, surviving) = delayedPending events gs
      undecided = eventTriggers grouped gs <> stateTriggers gs
   in (filter (interveningHolds gs) undecided <> fromDelayed, surviving)

-- | CR 603.3b: the abilities a round of GameEvent.AbilityTriggered records fires,
-- for Pawl.Engine.Engine.reactions to fold into the same batch.
--
-- `gatherTriggers`' event half alone. The other two sources are deliberately not
-- re-run: CR 603.8's state triggers were gathered once for this batch already and
-- match no event at all, so a second call would duplicate every one of them; and
-- the CR 603.7 delayed store's watermark is spent by the same one call, so
-- re-running it would consume entries against events they never matched.
reactionTriggers :: [LoggedEvent.LoggedEvent] -> GameState -> [PendingTrigger]
reactionTriggers events gs = filter (interveningHolds gs) (eventTriggers events gs)

-- CR 603.4: the ability doesn't trigger at all when its intervening "if" is false
-- as the trigger event occurs. Checked at the gather rather than at placement,
-- because "doesn't trigger" must be indistinguishable from "no ability existed",
-- including to the CR 117.5 settle loop's re-run flag.
--
-- A SOURCELESS pending trigger never reaches this -- gatherTriggers and
-- delayedPending are the only callers, and all three gatherers hang their
-- triggers on an object, the inherent ones being merged in afterwards by
-- Pawl.Engine.Engine. The arm answers True
-- rather than failing because an inherent ability's own gatherer owns CR 603.4:
-- rule 725.2's pair has no intervening "if" at all, and CR 702.179d's does,
-- checked inside Pawl.Engine.Speed.inherentPending. A fourth gatherer must do the
-- same; there is no subject object to hand this function, so routing one here
-- would mean giving Condition.holds the ability object Pawl.Engine.Stack's CR
-- 608.2a re-check uses, which does not exist until placement.
--
-- CR 608.2h supplies the view rather than fullView, which for a look-back trigger
-- is the difference between reading the clause and reading nothing: CR 603.10a
-- makes the source the permanent as it was immediately before the event, whose id
-- CR 400.7 has since deleted -- and fullView describes a deleted id as an object
-- with no characteristics, quietly answering False to every clause. Stack's CR
-- 608.2a re-check reads the same way, and the two must agree or a trigger would be
-- placed and then removed for disagreeing with itself.
--
-- The trigger's own bindings ride in as the context's slot objects, which is
-- what lets CR 702.100a's "if THAT CREATURE's power is greater" read the
-- entrant through Quantity.AgainstSlot rather than the bearer: an intervening
-- "if" may be about the event's object and not only about the source. Stack's
-- re-check reads the same slots off the placed ability, for the reason the view
-- above must match.
--
-- Which is why the view is the UNSCOPED viewWithLastKnownAnywhere: CR 608.2h is
-- owed to every object the clause reads, and a slot naming an object that has
-- since left would otherwise be described as one with no characteristics.
--
-- Nothing OBSERVES that at this end of the rule, and scoping the view back to the
-- source here leaves the suite green: evolve is the only ability whose "if" reads
-- a slot, and rule 702.100a's entrant is on the battlefield by construction while
-- its own entry is being gathered. So this is a fence keeping the two checks
-- reading alike, not a proved behaviour -- the proved one is Stack's re-check.
interveningHolds :: GameState -> PendingTrigger -> Bool
interveningHolds gs pending =
  case (TriggeredAbility.intervening (PendingTrigger.ability pending), PendingTrigger.source pending) of
    (Nothing, _) -> True
    (Just _, TriggerSource.Sourceless) -> True
    (Just cond, TriggerSource.OfObject oid) ->
      Condition.holds
        (Projection.viewWithLastKnownAnywhere gs)
        -- CR 303.4b's host rides in beside the slots, for the reason they do:
        -- Ray of Frost's "if enchanted creature is red" is about the SOURCE's
        -- attachment rather than about the event, and Stack's CR 608.2a re-check
        -- supplies the same field so the two checks cannot disagree.
        ((Filter.contextWithSlots (Just (PendingTrigger.controller pending)) (Just oid) (Binding.slotObjects (PendingTrigger.bindings pending))) {Filter.sourceAttachedTo = Projection.hostOf oid gs})
        gs
        oid
        cond
