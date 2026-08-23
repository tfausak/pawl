-- Pawl.Engine.Trigger's machinery, and the part of Pawl.Engine.Event that
-- feeds it: the turn-scoped event log and its per-reader watermarks, the CR
-- 603.2b step-beginning event, the CR 603.6a scan, CR 608.2i turn history, CR
-- 603.7 delayed triggers and their object slots, the CR 603.3b ordering prompt,
-- and the CR 603.4 / 608.2a intervening "if".
--
-- The triggers themselves live in three sibling modules --
-- Pawl.KeywordTriggerSpec (CR 702 keywords), Pawl.ZoneTriggerSpec (zone
-- changes) and Pawl.EventTriggerSpec (everything else). All four report under
-- the same describe name, so the split is invisible to a tasty pattern.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Pawl.TriggerSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Saga as Saga
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.AttackerDeclared as AttackerDeclared
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Create as Create
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.EventGroup as EventGroup
import qualified Pawl.Types.Expiry as Expiry.Type
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.InherentTriggerSource as InherentTriggerSource
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.StepBegins as StepBegins
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerEntry as TriggerEntry
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TriggeredAbilitySource as TriggeredAbilitySource
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.TurnWindow as TurnWindow
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- Barbarian Outcast on N Swamps, the two loaded printings each test case
-- supplies.
outcastBoard :: Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
outcastBoard barbarianOutcast swamp swamps =
  S.addCreature barbarianOutcast S.alice (S.landsInPlay swamp swamps)

-- alice casts Tidal Wave off three Islands and lets it resolve.
castWave :: Printing.Printing -> Printing.Printing -> GameState.GameState
castWave tidalWave island =
  let resolveAll g = snd (Engine.runGamePure S.identityAnswer g Engine.priorityLoop)
      (gs, oid) = S.handOne tidalWave (S.landsInPlay island 3)
   in resolveAll (snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice oid)))

-- CR 608.2i: the log records; it is never emptied by a reader.
logSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
logSpec s registry =
  Spec.describe s "EventLog" $ do
    -- CR 400.7 / 603.2g: a zone change appends a Moved event carrying the
    -- RESOLVED destination. Pawl.EventSpec's "CR 603.2g: the emitted event
    -- records the RESOLVED destination (exile)" covers this same accessor
    -- (S.zoneChangesOf / ZoneChange.to) more strongly, through a Rest in
    -- Peace redirect -- no separate case needed here.
    -- CR 608.2h: the snapshot is the object as it last existed in the zone it
    -- LEFT. Re-deriving from the printed card would be wrong for an animated
    -- land and impossible for a token (CR 111.1).
    Spec.it s "CR 608.2h a Moved event snapshots the object it moved" $ do
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      let (piker, gs) = S.addCreature pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
          expected = Projection.project piker gs
          after = S.runPure S.identityAnswer gs (Event.changeZone piker Zone.Graveyard)
      case S.eventsOf after of
        GameEvent.Moved (Moved.MkMoved _ snapshot) : _ -> Spec.assertEqWith s "snapshot from the origin zone" snapshot expected
        _ -> Spec.assertFailure s "expected exactly one Moved event"
    -- CR 704.5h's window is "since the last SBA check": the check CONSUMES by
    -- bumping a watermark, and the record survives.
    Spec.it s "CR 704 the SBA check advances the damage watermark but keeps the record" $ do
      typhoidRats <- S.printingOf s registry "Typhoid Rats"
      ogreSentry <- S.printingOf s registry "Ogre Sentry"
      let (gs, _, _) = S.combatBoardOf [typhoidRats] [ogreSentry]
          fought = S.fightWith S.aggressiveAnswer gs
          after = S.settleSba fought
      Spec.assertEqWith s "nothing left unscanned for damage" (Event.unscannedDamage after) []
      Spec.assertBool s (not (null (S.damageEventsOf after))) "the damage events are still recorded"
    Spec.it s "CR 117.5 the trigger scan advances its watermark but keeps the record" $ do
      restInPeace <- S.printingOf s registry "Rest in Peace"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, gs) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
          (pikerId, gs1) = S.addCreature piker S.bob gs
          moved = S.runPure S.identityAnswer gs1 (Event.changeZone pikerId Zone.Hand)
          scanned = snd (Engine.runGamePure S.identityAnswer moved Engine.placePendingTriggers)
      Spec.assertEqWith s "nothing left unscanned" (Event.unscannedEvents scanned) []
      Spec.assertBool s (not (null (S.zoneChangesOf scanned))) "the zone change is still recorded"
    -- The turn is the log's scope (CR 608.2i). Clearing at cleanup would be
    -- wrong: cleanup is still part of THIS turn.
    Spec.it s "the log and both watermarks are cleared at turn handoff" $ do
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      let (piker, gs) = S.addCreature pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
          moved = S.runPure S.identityAnswer gs (Event.changeZone piker Zone.Graveyard)
          after = snd (Engine.runGamePure S.identityAnswer moved Engine.handoffTurn)
      Spec.assertEqWith s "log empty" (GameState.events after) Seq.empty
      Spec.assertEqWith s "scan watermark reset" (GameState.scannedThrough after) 0
      Spec.assertEqWith s "damage watermark reset" (GameState.damageScannedThrough after) 0
    -- CR 514.3 (partial): an event emitted by the cleanup step must be scanned
    -- BEFORE handoffTurn clears the log, or its trigger is lost outright.
    Spec.it s "advance settles before handing off, so no unscanned event is discarded" $ do
      restInPeace <- S.printingOf s registry "Rest in Peace"
      let (ripId, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
          entered = ZoneChange.MkZoneChange ripId ripId Zone.Stack Zone.Battlefield
          gs1 = S.withEvents [GameEvent.Moved (Moved.MkMoved entered (Projection.project ripId gs0))] gs0
          ending = gs1 {GameState.remaining = Seq.empty}
          after = snd (Engine.runGamePure S.identityAnswer ending Engine.advance)
          isTrigger oid = case Game.lookupObject oid after of
            Just obj -> case Object.source obj of
              Source.OfTrigger _ -> True
              _ -> False
            Nothing -> False
      Spec.assertBool s (any isTrigger (GameState.stack after)) "the pending trigger reached the stack"
      Spec.assertEqWith s "the log was cleared afterwards" (GameState.events after) Seq.empty

-- CR 603.2b / 603.6a: a step begins, and EVERY permanent is checked.
scanSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
scanSpec s registry =
  Spec.describe s "Scan" $ do
    Spec.it s "CR 603.2b running a step records that it began, on the active player's turn" $ do
      let gs = (Setup.emptyGame S.bothPlayers) {GameState.phase = Phase.Ending EndingStep.EndStep, GameState.activePlayer = S.alice}
          after = snd (Engine.runGamePure S.identityAnswer gs Engine.runStep)
          began ev = case ev of
            GameEvent.StepBegan (StepBegan.MkStepBegan p pid) -> Just (p, pid)
            _ -> Nothing
      Spec.assertEqWith s "the end step's beginning is recorded exactly once" (Maybe.mapMaybe began (S.eventsOf after)) [(Phase.Ending EndingStep.EndStep, S.alice)]
    Spec.it s "CR 603.2b StepBegins matches its own step and no other" $ do
      let bearer = ObjectId.MkObjectId 1
          cond = TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Ending EndingStep.EndStep) TurnScope.EachTurn)
      Spec.assertBool s (Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice cond (GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Ending EndingStep.EndStep) S.alice))) "the end step matches"
      Spec.assertBool s (not (Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice cond (GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Beginning BeginningStep.Upkeep) S.alice)))) "the upkeep does not"
    -- CR 603.2b's "that player": the step's own turn names the active player,
    -- and Shizuko, Caller of Autumn's "each player's upkeep, THAT PLAYER adds"
    -- reads them back off the reserved slot. Three seats, so the slot can hold
    -- somebody who is neither the controller nor the one opponent; the falsifier
    -- is an implementation that leaves the slot empty and lets the payload fall
    -- back to CR 109.5's "you".
    Spec.it s "CR 603.2b the active player rides a step trigger in the reserved slot" $ do
      let upkeepOf pid = GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Beginning BeginningStep.Upkeep) pid)
          cond = TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.EachTurn)
          boundOf pid = Binding.targetsOf (Event.eventBindings cond (upkeepOf pid))
      Spec.assertEqWith s "carol's upkeep binds carol under thatPlayer" (boundOf S.carol) (Map.singleton Binding.triggerPlayer (Set.singleton (Recipient.ToPlayer S.carol)))
      Spec.assertEqWith s "and bob's binds bob" (boundOf S.bob) (Map.singleton Binding.triggerPlayer (Set.singleton (Recipient.ToPlayer S.bob)))
      -- The same slot under the OTHER TurnScope, which eventBindingSlots'
      -- per-condition promise needs: there the seat is redundant with CR 109.5's
      -- "you", but the classification claims it unconditionally.
      Spec.assertEqWith
        s
        "a ControllersTurn step binds it too"
        (Binding.targetsOf (Event.eventBindings (TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn)) (upkeepOf S.alice)))
        (Map.singleton Binding.triggerPlayer (Set.singleton (Recipient.ToPlayer S.alice)))
    -- CR 603.3a / 109.5: "your upkeep" is the ABILITY CONTROLLER's (603.3a
    -- controls the ability; 109.5 makes "your" mean that controller), so the
    -- scope is read against the bearer's controller, not the card.
    Spec.it s "CR 603.3a ControllersTurn matches only the bearer's controller's turn" $ do
      let bearer = ObjectId.MkObjectId 1
          cond = TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn)
      Spec.assertBool s (Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice cond (GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Beginning BeginningStep.Upkeep) S.alice))) "alice's upkeep matches for alice"
      Spec.assertBool s (not (Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice cond (GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Beginning BeginningStep.Upkeep) S.bob)))) "bob's upkeep does not"
    -- The widening falsifier: the scan now visits every battlefield permanent,
    -- so SelfEnters must ask whether the event is about THIS permanent. Rest in
    -- Peace is on the battlefield and a DIFFERENT object entered.
    Spec.it s "CR 603.6a a SelfEnters trigger does not fire on another object's entry" $ do
      restInPeace <- S.printingOf s registry "Rest in Peace"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
          (pikerId, gs1) = S.addCreature piker S.bob gs0
          entered = ZoneChange.MkZoneChange pikerId pikerId Zone.Stack Zone.Battlefield
          gs2 = S.withEvents [GameEvent.Moved (Moved.MkMoved entered (Projection.project pikerId gs1))] gs1
      Spec.assertEqWith s "no trigger" (length (fst (Event.gatherTriggers (Event.unscannedGrouped gs2) gs2))) 0
    Spec.it s "CR 603.6a a SelfEnters trigger still fires on its own entry" $ do
      restInPeace <- S.printingOf s registry "Rest in Peace"
      let (ripId, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
          entered = ZoneChange.MkZoneChange ripId ripId Zone.Stack Zone.Battlefield
          gs1 = S.withEvents [GameEvent.Moved (Moved.MkMoved entered (Projection.project ripId gs0))] gs0
      case fst (Event.gatherTriggers (Event.unscannedGrouped gs1) gs1) of
        [pt] -> do
          Spec.assertEqWith s "source is RiP" (PendingTrigger.source pt) (TriggerSource.OfObject ripId)
          Spec.assertEqWith s "controller is alice" (PendingTrigger.controller pt) S.alice
        other -> Spec.assertFailure s ("expected exactly one pending trigger, got " <> show (length other))
    Spec.it s "a graveyard-bound event yields no enters trigger" $ do
      restInPeace <- S.printingOf s registry "Rest in Peace"
      let (ripId, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
          toGrave = ZoneChange.MkZoneChange ripId ripId Zone.Battlefield Zone.Graveyard
          gs1 = S.withEvents [GameEvent.Moved (Moved.MkMoved toGrave (Projection.project ripId gs0))] gs0
      Spec.assertEqWith s "no triggers" (length (fst (Event.gatherTriggers (Event.unscannedGrouped gs1) gs1))) 0
    Spec.it s "SelfEnters matches only a battlefield destination" $ do
      let bearer = ObjectId.MkObjectId 1
          movedTo zone = GameEvent.Moved (Moved.MkMoved (ZoneChange.MkZoneChange bearer bearer Zone.Stack zone) S.emptyCharacteristics)
      Spec.assertBool s (Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice TriggerCondition.SelfEnters (movedTo Zone.Battlefield)) "enters battlefield matches"
      Spec.assertBool s (not (Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice TriggerCondition.SelfEnters (movedTo Zone.Graveyard))) "enters graveyard does not"
    -- CR 508.3a plus Aurelia, the Warleader's "for the first time each turn".
    -- The declaration being matched is already in the log when the scan runs,
    -- so "the first time" is "this is the only one so far".
    Spec.it s "SelfAttacks FirstTimeEachTurn matches only the first declaration" $ do
      let bearer = ObjectId.MkObjectId 1
          declared = GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared bearer S.bob 1)
          gsWith events = S.withEvents events (Setup.emptyGame S.bothPlayers)
          matches frequency events =
            Event.matchesTrigger (gsWith events) bearer S.alice (TriggerCondition.SelfAttacks frequency) declared
      Spec.assertBool s (matches TriggerFrequency.FirstTimeEachTurn [declared]) "the first declaration matches"
      Spec.assertBool s (not (matches TriggerFrequency.FirstTimeEachTurn [declared, declared])) "a second declaration this turn does not"
      -- Hanweir Garrison's shape is untouched by the narrowing.
      Spec.assertBool s (matches TriggerFrequency.EveryTime [declared]) "EveryTime matches the first"
      Spec.assertBool s (matches TriggerFrequency.EveryTime [declared, declared]) "EveryTime matches the second too"
      -- The count is per bearer, not per turn: two creatures declared
      -- together are each attacking for the first time.
      Spec.assertBool s (matches TriggerFrequency.FirstTimeEachTurn [GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared (ObjectId.MkObjectId 2) S.bob 1), declared]) "another creature's declaration does not spend this one's first time"
      -- CR 508.3a's last sentence, unchanged by the frequency: a
      -- non-declaration event never matches.
      Spec.assertBool s (not (Event.matchesTrigger (gsWith [declared]) bearer S.alice (TriggerCondition.SelfAttacks TriggerFrequency.FirstTimeEachTurn) (GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Combat CombatStep.DeclareAttackers) S.alice)))) "a step beginning is not an attack"
    -- Pins the canonical emission order this module's `eventTriggers` comment
    -- documents ("events outer, permanents inner, ascending by id"), which a
    -- later task's CR 603.3b ordering prompt indexes into. Two RiP bearers
    -- enter via two separate events recorded in the same order their ids
    -- were assigned; the resulting PendingTrigger.source list must follow
    -- that same ascending order.
    Spec.it s "CR 603.6a two SelfEnters triggers emit in ascending ObjectId order" $ do
      restInPeace <- S.printingOf s registry "Rest in Peace"
      let (rip1, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
          (rip2, gs1) = S.addCreature restInPeace S.alice gs0
          entered1 = ZoneChange.MkZoneChange rip1 rip1 Zone.Stack Zone.Battlefield
          entered2 = ZoneChange.MkZoneChange rip2 rip2 Zone.Stack Zone.Battlefield
          gs2 =
            S.withEvents
              [ GameEvent.Moved (Moved.MkMoved entered1 (Projection.project rip1 gs1)),
                GameEvent.Moved (Moved.MkMoved entered2 (Projection.project rip2 gs1))
              ]
              gs1
          triggers = fst (Event.gatherTriggers (Event.unscannedGrouped gs2) gs2)
      Spec.assertBool s (rip1 < rip2) "rip1 has the lower id"
      Spec.assertEqWith s "both triggers fired" (length triggers) 2
      Spec.assertEqWith s "sources in ascending ObjectId order" (fmap PendingTrigger.source triggers) (fmap TriggerSource.OfObject [rip1, rip2])
    -- The PERMANENTS-INNER half of that same order guarantee. Every SelfEnters
    -- test above has exactly one bearer matching each event, so inner order
    -- can never affect the output -- SelfEnters alone cannot discriminate
    -- events-outer-permanents-inner from any other traversal. A StepBegins
    -- bearer can: ONE StepBegan event matches MANY permanents at once. Two
    -- Khabál Ghouls (CR 603.2b, "at the beginning of each end step") on the
    -- battlefield, one end-step event -- the PendingTrigger.source list must
    -- come out in ascending ObjectId order.
    Spec.it s "CR 603.2b two StepBegins triggers from one event emit in ascending ObjectId order" $ do
      khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
      let (ghoul1, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
          (ghoul2, gs1) = S.addCreature khabalGhoul S.alice gs0
          event = GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Ending EndingStep.EndStep) S.alice)
          triggers = fst (Event.gatherTriggers [LoggedEvent.MkLoggedEvent {LoggedEvent.group = EventGroup.first, LoggedEvent.event = event}] gs1)
      Spec.assertBool s (ghoul1 < ghoul2) "ghoul1 has the lower id"
      Spec.assertEqWith s "both triggers fired" (length triggers) 2
      Spec.assertEqWith s "sources in ascending ObjectId order" (fmap PendingTrigger.source triggers) (fmap TriggerSource.OfObject [ghoul1, ghoul2])
    -- CR 603.10, FIRST sentence -- the normal rule, not the "looks back in
    -- time" exception list that follows it: "objects that exist immediately
    -- after an event are checked to see if the event matched any trigger
    -- conditions". Ravenous Rats existed immediately after the event that put
    -- it onto the battlefield, so its CR 603.6a entry trigger fired -- even
    -- though CR 704.5f then buried it as a 0/0 before the CR 117.5 boundary's
    -- trigger scan ran.
    --
    -- bob holds TWO cards, so "discarded once" is distinguishable from
    -- "discarded twice"; the companion test below is the no-double-fire half.
    Spec.it s "CR 603.10 whole cards: under Night of Souls' Betrayal, Ravenous Rats dies as it enters and STILL makes bob discard" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      ravenousRats <- S.printingOf s registry "Ravenous Rats"
      night <- S.printingOf s registry "Night of Souls' Betrayal"
      let (_, base1) = S.addCreature night S.alice (S.landsInPlay swamp 2)
          (_, base2) = S.addHandCard piker S.bob base1
          (_, base3) = S.addHandCard piker S.bob base2
          (gs, spellId) = S.handOne ravenousRats base3
          bobBefore = S.handSize S.bob gs
          cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
          settled = snd (Engine.runGamePure S.identityAnswer cast Engine.priorityLoop)
      Spec.assertEqWith s "CR 704.5f buried the 0/0" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Ravenous Rats") S.alice settled) 0
      Spec.assertEqWith s "in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice settled)) 1
      Spec.assertEqWith s "and bob still discarded, exactly once" (S.handSize S.bob settled) (bobBefore - 1)
    -- The no-double-fire half. Same board minus the -1/-1, so the Rats is on
    -- the battlefield at the CR 117.5 boundary AND named by an unscanned
    -- entry event -- the two candidate sources the scan draws from. A count,
    -- not a boolean: a Rats counted twice discards two of bob's two cards.
    Spec.it s "CR 603.6a whole cards: a Ravenous Rats that survives its entry triggers exactly once" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      ravenousRats <- S.printingOf s registry "Ravenous Rats"
      let (_, base1) = S.addHandCard piker S.bob (S.landsInPlay swamp 2)
          (_, base2) = S.addHandCard piker S.bob base1
          (gs, spellId) = S.handOne ravenousRats base2
          bobBefore = S.handSize S.bob gs
          cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
          settled = snd (Engine.runGamePure S.identityAnswer cast Engine.priorityLoop)
      Spec.assertEqWith s "the Rats survived" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Ravenous Rats") S.alice settled) 1
      Spec.assertEqWith s "nothing in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice settled)) 0
      Spec.assertEqWith s "bob discarded exactly one" (S.handSize S.bob settled) (bobBefore - 1)

-- CR 701.21: sacrificing is its own keyword action -- NOT a destruction.
sacrificeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
sacrificeSpec s registry =
  Spec.describe s "Sacrifice" $ do
    Spec.it s "CR 701.21a a sacrificed permanent goes to its owner's graveyard" $ do
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      let (piker, gs) = S.addCreature pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
          after = S.runPure S.identityAnswer gs (Event.sacrifice S.bob piker)
      Spec.assertEqWith s "off the battlefield" (S.creaturesInPlay S.bob after) 0
      Spec.assertEqWith s "in bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    -- The test above sacrifices a permanent the same player owns and
    -- controls, so it never exercises owner-relativity: CR 701.21a says
    -- "its CONTROLLER moves it... to its OWNER's graveyard." Here bob owns
    -- and alice controls (S.giveControl installs the layer-2 SetController
    -- effect), so the result must land in bob's graveyard, not alice's.
    Spec.it s "CR 701.21a a sacrifice lands in the OWNER's graveyard even when a different player controls it" $ do
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      let (piker, gs0) = S.addCreature pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
          gs = S.giveControl piker S.alice gs0
          -- ALICE is the sacrificing player, because she controls it (CR
          -- 701.21a); bob merely owns it, which is what the assertions below
          -- separate.
          after = S.runPure S.identityAnswer gs (Event.sacrifice S.alice piker)
      Spec.assertEqWith s "off bob's battlefield" (S.creaturesInPlay S.bob after) 0
      Spec.assertEqWith s "in bob's (owner's) graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
      Spec.assertEqWith s "not in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0
    -- CR 701.21a: "sacrificing a permanent doesn't destroy it", so neither CR
    -- 702.12b's indestructible gate nor CR 701.19a's shield applies.
    Spec.it s "CR 701.21a an indestructible permanent can still be sacrificed" $ do
      darksteelMyr <- S.printingOf s registry "Darksteel Myr"
      let (myr, gs) = S.addCreature darksteelMyr S.bob (Setup.emptyGame S.bothPlayers)
          after = S.runPure S.identityAnswer gs (Event.sacrifice S.bob myr)
      Spec.assertEqWith s "gone from the battlefield" (S.creaturesInPlay S.bob after) 0
    Spec.it s "CR 701.21a sacrificing neither consults nor consumes a regeneration shield" $ do
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      let (piker, gs0) = S.addCreature pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
          gs = S.addRegenShield piker gs0
          after = S.runPure S.identityAnswer gs (Event.sacrifice S.bob piker)
      Spec.assertEqWith s "still sacrificed" (S.creaturesInPlay S.bob after) 0
      Spec.assertEqWith s "the shield's source is untouched" (fmap ActiveReplacement.source (GameState.replacements after)) [piker]
    Spec.it s "only a battlefield permanent can be sacrificed (CR 701.21a)" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (card, gs) = S.addLibraryCard piker S.bob (Setup.emptyGame S.bothPlayers)
          after = S.runPure S.identityAnswer gs (Event.sacrifice S.bob card)
      Spec.assertEqWith s "the library card is untouched" after gs
    -- CR 701.21a's second clause, which had no enforcement before #44: "A player
    -- can't sacrifice ... a permanent they don't control." Bob controls it;
    -- alice asking is refused outright rather than quietly honoured.
    Spec.it s "CR 701.21a a player cannot sacrifice a permanent they do not control" $ do
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      let (piker, gs) = S.addCreature pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
          byAlice = S.runPure S.identityAnswer gs (Event.sacrifice S.alice piker)
          byBob = S.runPure S.identityAnswer gs (Event.sacrifice S.bob piker)
      Spec.assertEqWith s "alice's attempt changes nothing at all" byAlice gs
      -- The discriminating half: the same call from the controller works, so the
      -- refusal above is the guard and not an unrelated no-op.
      Spec.assertEqWith s "bob's own sacrifice goes through" (S.creaturesInPlay S.bob byBob) 0
    -- CR 113.7: "this creature" is a slot read, filled at placement.
    Spec.it s "CR 113.7 a placed trigger binds its source into the reserved self slot" $ do
      restInPeace <- S.printingOf s registry "Rest in Peace"
      let (ripId, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
          entered = ZoneChange.MkZoneChange ripId ripId Zone.Stack Zone.Battlefield
          gs1 = S.withEvents [GameEvent.Moved (Moved.MkMoved entered (Projection.project ripId gs0))] gs0
          placed = snd (Engine.runGamePure S.identityAnswer gs1 Engine.placePendingTriggers)
          bindingsOn oid = maybe Map.empty Object.bindings (Game.lookupObject oid placed)
          selfOf oid = Map.lookup Binding.triggerSource (Binding.targetsOf (bindingsOn oid))
      Spec.assertEqWith s "the trigger names its source" (fmap selfOf (GameState.stack placed)) [Just (Set.singleton (Recipient.ToObject ripId))]

-- CR 603.10a's sacrifice family, end to end. Mayhem Devil {1}{B}{R} Creature --
-- Devil 3/3: "Whenever a player sacrifices a permanent, this creature deals 1
-- damage to any target."
--
-- THE PAIR is the point, and neither half means anything alone. CR 700.4 makes
-- "dies" mean "is put into a graveyard from the battlefield", so every sacrifice
-- IS a death and writes the same battlefield-to-graveyard Moved event a
-- destruction writes. An implementation that emitted the sacrifice event from the
-- generic zone-change funnel would pass the firing case and fail only the silent
-- one -- which is exactly the confusion #386 describes.
--
-- THREE SEATS. The Devil fires on ANY player's sacrifice, its own controller's
-- included, so alice has to be the sacrificing player -- a two-handed board where
-- bob sacrificed would leave a wrong "opponent only" reading passing. carol is
-- the third seat, and she absorbs the Fire-Eater's damage so that the Devil's 1
-- lands alone on bob: pointing the two sources at the SAME player would let a
-- single total agree for the wrong reason.
--
-- Ghitu Fire-Eater ({T}, Sacrifice this creature: it deals damage equal to its
-- power to any target) is the sacrificing card, chosen because its cost
-- sacrifices ITSELF -- so nothing is prompted about WHICH permanent, and the
-- fixture stays free of an answerer choice that could drift.
--
-- ONE TUPLE of all three totals, per assertion. bob's exact 19 is also the
-- no-double-fire pin: an event recorded both before and after the move, or a
-- condition that matched the Moved event alongside its own, would take him to 18.
mayhemDevilSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mayhemDevilSpec s registry =
  let -- Answers every target slot with `who`. Split across the two runs below:
      -- the Fire-Eater's own target is chosen as the ability is ACTIVATED, and
      -- the Devil's trigger's when the priority loop places it, so two answerers
      -- aim the two damage sources at two different players.
      aimAt :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      aimAt who p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer who))) sets
        _ -> S.identityAnswer p
      board devil extra =
        let (_, withDevil) = S.addCreature devil S.alice S.threePlayerGame
            (extraId, withExtra) = S.addCreature extra S.alice withDevil
         in ( extraId,
              withExtra
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            )
      lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs)
   in Spec.describe s "PermanentSacrificed" $ do
        Spec.it s "CR 603.10a Mayhem Devil fires on a sacrifice, including its own controller's" $ do
          mayhemDevil <- S.printingOf s registry "Mayhem Devil"
          ghituFireEater <- S.printingOf s registry "Ghitu Fire-Eater"
          case Face.activatedAbilities (S.combinedFace ghituFireEater) of
            [] -> Spec.assertFailure s "Ghitu Fire-Eater should declare one activated ability"
            ability : _ -> do
              let (fireEater, gs) = board mayhemDevil ghituFireEater
                  -- The Fire-Eater's own damage goes to carol; the cost paid here
                  -- is the CR 701.21a sacrifice under test.
                  activated = S.runPure (aimAt S.carol) gs (Activate.activateAbility S.alice fireEater ability)
                  -- The Devil's trigger is gathered and placed by the loop, so its
                  -- target is answered here -- bob, alone.
                  after = S.runPure (aimAt S.bob) activated Engine.priorityLoop
              Spec.assertEqWith s "everyone starts at 20" (lives gs) (Just 20, Just 20, Just 20)
              Spec.assertBool s (not (Set.member fireEater (GameState.battlefield activated))) "the cost really sacrificed the Fire-Eater"
              Spec.assertEqWith s "CR 701.21a: bob takes the Devil's 1, carol the Fire-Eater's 2" (lives after) (Just 20, Just 19, Just 18)
        -- The half that makes the half above mean something. Same board, same
        -- Devil, a permanent that DIES without being sacrificed: CR 700.4 makes
        -- the zone change identical, so only an engine that records the sacrifice
        -- as a sacrifice can stay silent here.
        Spec.it s "CR 700.4 a plain death is not a sacrifice, and fires nothing" $ do
          mayhemDevil <- S.printingOf s registry "Mayhem Devil"
          piker <- S.printingOf s registry "Goblin Piker"
          let (pikerId, gs) = board mayhemDevil piker
              killed = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [pikerId])
              after = S.runPure (aimAt S.bob) killed Engine.priorityLoop
          -- Positive control: the Piker really left, so the silence below is the
          -- condition's answer rather than a fixture that destroyed nothing.
          Spec.assertEqWith s "the Piker really died" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.alice after) 0
          Spec.assertEqWith s "and it landed in a graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
          Spec.assertEqWith s "CR 700.4: nobody's life total moved" (lives after) (Just 20, Just 20, Just 20)

-- Barbarian Outcast {1}{R} Creature -- Human Barbarian Beast 2/2:
-- "When you control no Swamps, sacrifice this creature." CR 603.8's own example
-- shape ("a player controlling no permanents of a particular card type"), chosen
-- by the rulebook to illustrate the rule.
stateTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
stateTriggerSpec s registry =
  let triggerIds gs = filter (isTriggerObject gs) (GameState.stack gs)
      isTriggerObject gs oid = case Game.lookupObject oid gs of
        Just obj -> case Object.source obj of
          Source.OfTrigger _ -> True
          _ -> False
        Nothing -> False
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
   in Spec.describe s "StateTrigger" $ do
        -- THE flooding falsifier. CR 603.8's second sentence exists to prevent
        -- exactly this: a state trigger that re-fires at every boundary.
        --
        -- What this test can and can't observe: it can only ever see 0 or 1
        -- here, never 2+. If suppression were absent, placePendingTriggers
        -- would keep reporting the same source as newly-triggered on every
        -- iteration, and Engine.settleForPriority (which loops until
        -- nothing new triggers) would recurse without terminating -- a
        -- broken suppression HANGS the test suite rather than failing this
        -- assertion with some higher count. This test still earns its keep
        -- by discriminating 0 (no trigger at all) from 1 (correctly armed
        -- once); it does not, and cannot, discriminate "one instance" from
        -- "flooding". The two-source test below is a separate discriminator,
        -- pinning that suppression is scoped per SOURCE rather than per
        -- ability (a bug that would wrongly suppress a second source, not
        -- flood -- so it fails this same 0/1 shape rather than hanging).
        Spec.it s "CR 603.8 a true state condition puts EXACTLY ONE instance on the stack" $ do
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          swamp <- S.printingOf s registry "Swamp"
          let (_, gs) = outcastBoard barbarianOutcast swamp 0
              settled = settle gs
          Spec.assertEqWith s "one trigger, not one per boundary" (length (triggerIds settled)) 1
        Spec.it s "CR 603.8 re-settling while the instance is on the stack adds no second copy" $ do
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          swamp <- S.printingOf s registry "Swamp"
          let (_, gs) = outcastBoard barbarianOutcast swamp 0
              twice = settle (settle gs)
          Spec.assertEqWith s "still exactly one" (length (triggerIds twice)) 1
        Spec.it s "CR 603.8 the condition being FALSE means no trigger at all" $ do
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          swamp <- S.printingOf s registry "Swamp"
          let (_, gs) = outcastBoard barbarianOutcast swamp 1
              settled = settle gs
          Spec.assertEqWith s "no trigger while a Swamp is out" (length (triggerIds settled)) 0
        Spec.it s "CR 603.8 losing the last Swamp makes the condition true and fires it" $ do
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          swamp <- S.printingOf s registry "Swamp"
          let (_, gs) = outcastBoard barbarianOutcast swamp 1
              quiet = settle gs
              swampOid = case Game.zoneMembers Zone.Battlefield S.alice quiet of
                ids -> case filter (\oid -> Set.member Subtype.Swamp (Projection.subtypesOf oid quiet)) ids of
                  swampId : _ -> swampId
                  [] -> ObjectId.MkObjectId 999
              gone = settle (S.runPure S.identityAnswer quiet (Event.destroy Regenerability.Regenerable [swampOid]))
          Spec.assertEqWith s "the Swamp's death arms it" (length (triggerIds gone)) 1
        -- CR 603.8: "doesn't trigger again until the ability has resolved, has
        -- been countered, or has otherwise left the stack" -- all three are
        -- "no longer on the stack", which is why armedness is derived.
        Spec.it s "CR 603.8 an instance leaving the stack re-arms the trigger" $ do
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          swamp <- S.printingOf s registry "Swamp"
          let (_, gs) = outcastBoard barbarianOutcast swamp 0
              settled = settle gs
              removed = case triggerIds settled of
                abilId : _ -> Game.cease abilId settled
                [] -> settled
              again = settle removed
          Spec.assertEqWith s "a fresh instance" (length (triggerIds again)) 1
        -- IMPORTANT-2 (review): Event.stateTriggers' instancesOnStack count
        -- keys on BOTH the source object's id and the ability (the two fields of
        -- `TriggeredAbilitySource`). Every test above uses exactly one
        -- Barbarian Outcast, so all of them would still pass a weaker
        -- implementation that compared only the TriggeredAbility and ignored
        -- srcId -- and that weaker version would wrongly suppress a second,
        -- otherwise-independent source. This is the one that catches it: put
        -- one Outcast's instance on the stack first, THEN let a second,
        -- identical Outcast (same controller, same 0-Swamp board) get its own
        -- chance to trigger. Under a source-scoped comparison the second fires;
        -- under an ability-only comparison it is (wrongly) suppressed by the
        -- first's presence on the stack.
        Spec.it s "CR 603.8 a second identical source still triggers -- suppression is per-source, not per-ability" $ do
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          swamp <- S.printingOf s registry "Swamp"
          let (_, gs0) = outcastBoard barbarianOutcast swamp 0
              settledFirst = settle gs0
              (_, gs1) = S.addCreature barbarianOutcast S.alice settledFirst
              settledBoth = settle gs1
          Spec.assertEqWith s "two instances, one per source" (length (triggerIds settledBoth)) 2
        -- M-4 (review): the state trigger's Condition.holds reads the PROJECTION
        -- -- CR 613 layer 4 for a subtype -- not Face.typeLine. Pin it with no real Swamp card
        -- anywhere: alice controls only a Mountain, so the Outcast triggers;
        -- adding an AddLandSubtype Swamp modification (the Urborg shape) to
        -- that same Mountain must turn the trigger off.
        Spec.it s "CR 613 layer 4: an added Swamp subtype (no real Swamp card) suppresses the trigger" $ do
          mountain <- S.printingOf s registry "Mountain"
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          let gs0 = S.landsInPlay mountain 1
              mountainId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
                i : _ -> i
                [] -> ObjectId.MkObjectId 999
              (_, gs1) = S.addCreature barbarianOutcast S.alice gs0
              before = settle gs1
              withUrborg = S.withEffect mountainId (Modification.AddLandSubtype Subtype.Swamp) gs1
              after = settle withUrborg
          Spec.assertEqWith s "no real Swamp yet: triggers" (length (triggerIds before)) 1
          Spec.assertEqWith s "projected Swamp subtype (still a Mountain card): stops triggering" (length (triggerIds after)) 0
        -- M-4 (review): the state trigger's Condition.holds reads projected
        -- CONTROL -- CR 613 layer 2 -- not Object.owner. Pin it: bob owns and controls the only Swamp, so
        -- alice's Outcast triggers; giving alice control of bob's Swamp (a
        -- layer-2 SetController effect, S.giveControl) must turn it off even
        -- though bob still OWNS that Swamp.
        Spec.it s "CR 110.2/613 layer 2: gaining control of the opponent's Swamp suppresses the trigger" $ do
          swamp <- S.printingOf s registry "Swamp"
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          let gs0 = Setup.emptyGame S.bothPlayers
              (swampId, gs1) = S.addCreature swamp S.bob gs0
              (_, gs2) = S.addCreature barbarianOutcast S.alice gs1
              before = settle gs2
              gs3 = S.giveControl swampId S.alice gs2
              after = settle gs3
          Spec.assertEqWith s "alice controls no Swamps yet: triggers" (length (triggerIds before)) 1
          Spec.assertEqWith s "alice now controls the Swamp: stops triggering" (length (triggerIds after)) 0
        -- The whole card, at gameplay level: the trigger resolves and the
        -- Outcast sacrifices itself (CR 701.21a, through Event.sacrifice).
        Spec.it s "CR 701.21 the resolved trigger sacrifices the Outcast into its owner's graveyard" $ do
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          swamp <- S.printingOf s registry "Swamp"
          let (outcast, gs) = outcastBoard barbarianOutcast swamp 0
              settled = settle gs
              resolved = snd (Engine.runGamePure S.identityAnswer settled Stack.resolveTop)
          Spec.assertBool s (not (Set.member outcast (GameState.battlefield resolved))) "the Outcast is off the battlefield"
          Spec.assertEqWith s "and in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 1
        -- CR 603.2 / 603.8: ONE object carrying TWO identical state-triggered
        -- abilities. Each ability is its own ability, so each triggers, and CR
        -- 603.8's suppression ("a state-triggered ability doesn't trigger again
        -- until the ability has resolved...") is scoped to *the* ability -- it
        -- says nothing about a different ability that happens to read the same.
        --
        -- Synthetic Twofold Outcast (synthetic-twofold-outcast.json) is
        -- Barbarian Outcast with its one line printed twice and a life payment
        -- bolted on: "When you control no Swamps, you lose 1 life. Sacrifice
        -- this creature." x2. No printed card has two identical trigger lines
        -- (checked against Scryfall's whole oracle-cards bulk export, funny and
        -- alchemy sets included: zero hits), and nothing in CR 603 forbids one.
        --
        -- The life payment is what makes this test DISCRIMINATE. Counting stack
        -- objects alone would be weak; two identical abilities produce two
        -- indistinguishable trigger objects, so the effect COUNT is the real
        -- evidence. Alice is at 20; both instances resolving costs 2, one
        -- instance costs 1. The sacrifice is what makes it terminate: with the
        -- source off the battlefield the condition can never re-arm, so this is
        -- not the CR 603.8 loop a bare "lose 1 life" would be.
        Spec.it s "CR 603.2/603.8 two identical state triggers on ONE permanent each trigger" $ do
          twofoldOutcast <- S.printingOf s registry "Synthetic Twofold Outcast"
          swamp <- S.printingOf s registry "Swamp"
          let (_, gs) = outcastBoard twofoldOutcast swamp 0
              settled = settle gs
              resolveTop g = snd (Engine.runGamePure S.identityAnswer g Stack.resolveTop)
              both = resolveTop (resolveTop settled)
          Spec.assertEqWith s "two instances, one per ability" (length (triggerIds settled)) 2
          Spec.assertEqWith s "both resolved, so two life paid" (S.lifeOf S.alice both) (Just 18)
          Spec.assertEqWith s "and the stack is empty again" (length (triggerIds both)) 0
        -- CR 603.8's second half, on the same two-ability source: "A
        -- state-triggered ability doesn't trigger again until THE ABILITY has
        -- resolved, has been countered, or has otherwise left the stack." The
        -- rule is about the ability whose instance left -- an instance of the
        -- OTHER, identical ability is not that ability's instance and must not
        -- hold it back.
        --
        -- Game.cease is the "otherwise left the stack" path the sibling
        -- single-Outcast test above uses, and it is the only one that reaches
        -- this: resolving an instance sacrifices the source, which takes it off
        -- the battlefield and out of Event.stateTriggers' scan entirely.
        Spec.it s "CR 603.8 one instance leaving re-arms ITS ability, not held back by the twin's instance" $ do
          twofoldOutcast <- S.printingOf s registry "Synthetic Twofold Outcast"
          swamp <- S.printingOf s registry "Swamp"
          let (_, gs) = outcastBoard twofoldOutcast swamp 0
              settled = settle gs
              removed = case triggerIds settled of
                abilId : _ -> Game.cease abilId settled
                [] -> settled
              again = settle removed
          Spec.assertEqWith s "one of the two is gone" (length (triggerIds removed)) 1
          Spec.assertEqWith s "and the freed ability triggers again, alongside the survivor" (length (triggerIds again)) 2

-- Answers the Hack: it targets `oid` and swaps `from` for `to`. Everything else
-- falls through to the identity answer.
answerHackAt :: ObjectId.ObjectId -> Subtype.Subtype -> Subtype.Subtype -> Prompt.Prompt r -> r
answerHackAt oid from to p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  Prompt.ChooseLandTypeSwap {} -> (from, to)
  _ -> S.identityAnswer p

-- alice controls a Swamp and a Barbarian Outcast; BOB holds the Magical Hack and
-- the Island that pays for it. Whose Island it is decides the whole test: one on
-- ALICE's side would satisfy the swapped condition all by itself and the fired
-- trigger would prove nothing. bob casting it is legal for the same reason a
-- Hack can be aimed at any permanent -- CR 612.1's swap is a property of the
-- OBJECT, not of who changed its text.
--
-- With `hacked`, bob casts the Hack at the Outcast (Swamp -> Island) and it
-- resolves before the board is settled. Returns the Outcast's id and the state
-- at the CR 117.5 boundary, triggers placed and unresolved.
outcastHackBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m (ObjectId.ObjectId, GameState.GameState)
outcastHackBoard s registry hacked = do
  barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
  swamp <- S.printingOf s registry "Swamp"
  island <- S.printingOf s registry "Island"
  magicalHack <- S.printingOf s registry "Magical Hack"
  let gs0 = Setup.emptyGame S.bothPlayers
      (_, gs1) = S.addCreature swamp S.alice gs0
      (_, gs2) = S.addCreature island S.bob gs1
      (outcastId, gs3) = S.addCreature barbarianOutcast S.alice gs2
      (hackId, gs4) = S.addHandCard magicalHack S.bob gs3
      gs5 = gs4 {GameState.priority = Just S.bob}
      hackIt g = S.runPure (answerHackAt outcastId Subtype.Swamp Subtype.Island) g (do S.cast S.bob hackId; Stack.resolveTop)
      settle g = snd (Engine.runGamePure S.identityAnswer g Engine.settleForPriority)
  pure (outcastId, settle (if hacked then hackIt gs5 else gs5))

-- CR 612.1 reaching a TRIGGERED ability, end to end through the real engine.
--
-- Barbarian Outcast {1}{R} Creature -- Human Barbarian Beast 2/2, "When you
-- control no Swamps, sacrifice this creature." (checked against Scryfall) and
-- Magical Hack are the whole board -- no card had to be added for this.
--
-- CR 612.1: a text-changing effect "can apply to any words or symbols printed on
-- that object, but generally affects only that object's rules text (which
-- appears in its text box)". A triggered ability is printed in that text box
-- exactly as an activated ability is, so a hacked Outcast asks about ISLANDS.
--
-- The reader this proves out is Pawl.Engine.Event.stateTriggers, which takes the
-- ability from Projection.triggeredAbilitiesOf (the projection's post-layer
-- list) and hands its CR 603.8 condition to Condition.holds -- so the swap has
-- to land in the projection, at CR 613.1c layer 3, to be seen here.
textChangedTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
textChangedTriggerSpec s registry =
  let isTriggerObject gs oid = case Game.lookupObject oid gs of
        Just obj -> case Object.source obj of
          Source.OfTrigger _ -> True
          _ -> False
        Nothing -> False
      triggerIds gs = filter (isTriggerObject gs) (GameState.stack gs)
      resolveTop gs = snd (Engine.runGamePure S.identityAnswer gs Stack.resolveTop)
   in Spec.describe s "TextChangedTriggeredAbility" $ do
        -- The control, and what keeps the case below from passing vacuously:
        -- unhacked, the printed word stands, alice's Swamp answers it, and the
        -- Outcast is never in any danger.
        Spec.it s "CR 603.8 whole card: an unhacked Outcast asks about SWAMPS and stays" $ do
          (outcastId, board) <- outcastHackBoard s registry False
          Spec.assertEqWith s "alice controls a Swamp: no trigger" (length (triggerIds board)) 0
          Spec.assertBool s (Set.member outcastId (GameState.battlefield (resolveTop board))) "the Outcast is still on the battlefield"
        -- The swap, at gameplay level. alice's board did not move -- one Swamp,
        -- no Islands -- but the Outcast's own text now reads "no Islands", which
        -- is true, so it fires and sacrifices itself.
        Spec.it s "CR 612.1 whole card: a hacked Outcast asks about ISLANDS, fires and sacrifices itself" $ do
          (outcastId, board) <- outcastHackBoard s registry True
          Spec.assertEqWith s "alice controls no Islands: it triggers" (length (triggerIds board)) 1
          let resolved = resolveTop board
          Spec.assertBool s (not (Set.member outcastId (GameState.battlefield resolved))) "the Outcast is off the battlefield"
          Spec.assertEqWith s "and in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 1

-- Khabál Ghoul {2}{B} Creature -- Zombie 1/1: "At the beginning of each end step,
-- put a +1/+1 counter on this creature for each creature that died this turn."
-- Scryfall's only ruling on the card is the design in one sentence: the count
-- "includes creature tokens ... as well as creatures put into a graveyard before
-- Khabál Ghoul entered the battlefield."
historySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
historySpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs =
        Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice)) (gs {GameState.phase = endStep})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      countersOn oid gs = maybe 0 (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
   in Spec.describe s "TurnHistory" $ do
        -- The drained-queue falsifier: the deaths are SCANNED past before the end
        -- step's trigger ever exists, and must still be counted.
        Spec.it s "CR 608.2i deaths the trigger scan already passed are still counted" $ do
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          piker <- S.printingOf s registry "Goblin Piker"
          let (ghoul, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
              (p1, gs1) = S.addCreature piker S.bob gs0
              (p2, gs2) = S.addCreature piker S.bob gs1
              dead = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs2 (Event.destroy Regenerability.Regenerable [p1])) (Event.destroy Regenerability.Regenerable [p2])
              scanned = settle dead
              atEnd = resolveAll (settle (beginEndStep scanned))
          Spec.assertEqWith s "two +1/+1 counters" (countersOn ghoul atEnd) 2
          Spec.assertEqWith s "a 3/3" (Projection.powerOf ghoul atEnd) (Just 3)
        -- CR 111.1 / 608.2h: a token has NO printed card, so an implementation
        -- that re-derived card types from print instead of from the event's
        -- snapshot would read zero here.
        Spec.it s "CR 111.1 a token creature that died counts, though it has no printed card" $ do
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          piker <- S.printingOf s registry "Goblin Piker"
          let (ghoul, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
              (tok, gs1) = S.addToken (Printing.card piker) S.bob gs0
              dead = S.settleSba (S.runPure S.identityAnswer gs1 (Event.destroy Regenerability.Regenerable [tok]))
              atEnd = resolveAll (settle (beginEndStep dead))
          Spec.assertEqWith s "the token is counted" (countersOn ghoul atEnd) 1
        Spec.it s "a creature that left the battlefield for HAND did not die (CR 700.4)" $ do
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          piker <- S.printingOf s registry "Goblin Piker"
          let (ghoul, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
              (p1, gs1) = S.addCreature piker S.bob gs0
              bounced = S.runPure S.identityAnswer gs1 (Event.changeZone p1 Zone.Hand)
              atEnd = resolveAll (settle (beginEndStep bounced))
          Spec.assertEqWith s "a bounce is not a death" (countersOn ghoul atEnd) 0
        -- CR 608.2i: "look back in time" effects don't require the counted
        -- objects to currently exist, or the counting object to have existed
        -- at the time. Scryfall's ruling says this explicitly: the count
        -- "includes ... creatures put into a graveyard before Khabál Ghoul
        -- entered the battlefield." This test cannot fail against today's
        -- `Pawl.Engine.Count.evaluate`, whose `Scope.InHistory` arm folds the
        -- whole of `GameState.events` and is handed no `ObjectId` at all --
        -- `Pawl.Engine.Quantity.evaluate` has the Ghoul's id but does not
        -- forward it into that call -- so there is no way to scope the fold to
        -- the Ghoul's own lifetime. It is a regression gate on the ruling,
        -- pinned ahead of that signature ever gaining one.
        Spec.it s "CR 608.2i a creature that died before the Ghoul entered is still counted" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          let (p1, gs0) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
              dead = S.runPure S.identityAnswer gs0 (Event.destroy Regenerability.Regenerable [p1])
              (ghoul, gs1) = S.addCreature khabalGhoul S.alice (settle dead)
              atEnd = resolveAll (settle (beginEndStep gs1))
          Spec.assertEqWith s "one +1/+1 counter" (countersOn ghoul atEnd) 1
        -- CR 608.2i: the history's scope is ONE turn.
        Spec.it s "the count resets at turn handoff, not at the trigger scan" $ do
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          piker <- S.printingOf s registry "Goblin Piker"
          let (ghoul, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
              (p1, gs1) = S.addCreature piker S.bob gs0
              dead = S.runPure S.identityAnswer gs1 (Event.destroy Regenerability.Regenerable [p1])
              nextTurn = snd (Engine.runGamePure S.identityAnswer dead Engine.handoffTurn)
              atEnd = resolveAll (settle (beginEndStep nextTurn))
          Spec.assertEqWith s "last turn's death does not count" (countersOn ghoul atEnd) 0
        -- CR 603.2b: the step trigger belongs to a permanent with nothing to do
        -- with the event -- Task 2's widened scan, at gameplay level.
        Spec.it s "CR 603.2b the end step's beginning is what fires it" $ do
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          let (_, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
              quiet = settle gs0
              fired = settle (beginEndStep quiet)
              isTrigger oid = case Game.lookupObject oid fired of
                Just obj -> case Object.source obj of
                  Source.OfTrigger _ -> True
                  _ -> False
                Nothing -> False
          Spec.assertEqWith s "nothing before the step began" (GameState.stack quiet) []
          Spec.assertEqWith s "one trigger once it did" (length (filter isTrigger (GameState.stack fired))) 1

-- Furious Spinesplitter {2}{R/G}{R/G} Creature -- Ogre Warrior 3/3 with trample:
-- "At the beginning of your end step, put a +1/+1 counter on this creature for
-- each opponent who was dealt damage this turn." Khabál Ghoul's window over CR
-- 120.1's damage instead of over CR 700.4's death -- and the two measurements are
-- built differently on purpose. The Ghoul's is a Scope.InHistory fold over the
-- objects a Filter kept; this is Quantity.PlayersDealtDamageThisTurn, because CR
-- 120.3a's recipient is a PLAYER, who has no Filter view, and because what the
-- card counts is players rather than events.
--
-- ONE BOARD throughout -- three seats, alice's Spinesplitter, an Ogre Sentry of
-- bob's -- and every case below differs from the others in nothing but what was
-- dealt to whom before the end step. Three seats because a two-player board
-- collapses "an opponent" onto the only other player, so a reading that counted
-- every damaged player could not be told from one that counted opponents.
--
-- Every amount is distinct (1, 3, 4, 5, 6, 7) so that no sum coincides with a count
-- of players: the two-opponent case is 2 against a damage sum of 7, and the
-- twice-at-one-opponent case is 1 against a sum of 8 and an event count of 2.
--
-- The damage goes in through Damage.applyDamage rather than off a spell. That is
-- the funnel which records the event, and it is the level Khabál Ghoul's group
-- reaches for Event.destroy at. Everything downstream of the record -- the trigger,
-- the quantity, the counters -- is the card's own, driven through the real scan and
-- the real resolution.
spinesplitterSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spinesplitterSpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      -- The ACTIVE player's end step, so that after the handoffs below this is the
      -- step the turn actually reached rather than alice's forced back onto it.
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep (GameState.activePlayer gs))) (gs {GameState.phase = endStep})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      countersOn oid gs = maybe 0 (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      -- Settled FIRST, so the damage is scanned past before the end step's trigger
      -- exists -- the drained-queue falsifier Khabál Ghoul's group opens with.
      atEnd gs = resolveAll (settle (beginEndStep (settle gs)))
      hit src target amount gs =
        S.runPure
          S.identityAnswer
          gs
          (Damage.applyDamage [DamageEvent.MkDamageEvent src target amount False False False 0 Nothing DamageKind.Noncombat])
   in Spec.describe s "Furious Spinesplitter" $ do
        Spec.it s "CR 120.1 a counter for each OPPONENT dealt damage, not for each damage event" $ do
          spinesplitter <- S.printingOf s registry "Furious Spinesplitter"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (spine, gs0) = S.addCreature spinesplitter S.alice S.threePlayerGame
              (bobsSentry, base) = S.addCreature sentry S.bob gs0
              quiet = atEnd base
              atBob = atEnd (hit spine (Recipient.ToPlayer S.bob) 3 base)
              atBoth = atEnd (hit spine (Recipient.ToPlayer S.carol) 4 (hit spine (Recipient.ToPlayer S.bob) 3 base))
              atBobTwice = atEnd (hit spine (Recipient.ToPlayer S.bob) 5 (hit spine (Recipient.ToPlayer S.bob) 3 base))
              atAlice = atEnd (hit spine (Recipient.ToPlayer S.alice) 7 base)
              atBobsSentry = atEnd (hit spine (Recipient.ToCreature bobsSentry) 1 base)
              bobPaid = atEnd (Event.payLife S.bob 6 base)
          Spec.assertEqWith s "CR 702.19: the printed trample is there" (Map.member Keyword.Type.Trample (Projection.keywordsOf spine base)) True
          Spec.assertEqWith s "nobody was dealt damage, so no counter" (countersOn spine quiet) 0
          Spec.assertEqWith s "one opponent was, so one" (countersOn spine atBob) 1
          Spec.assertEqWith s "both opponents were, so two" (countersOn spine atBoth) 2
          Spec.assertEqWith s "one opponent hit twice is still one opponent" (countersOn spine atBobTwice) 1
          -- CR 102.2 / 109.5: "your opponents" is every player but you, so the
          -- ability's own controller is never among them.
          Spec.assertEqWith s "damage to alice herself is not damage to an opponent" (countersOn spine atAlice) 0
          -- CR 120.3a names the PLAYER recipient; a creature bob controls is not bob.
          Spec.assertEqWith s "damage to an opponent's creature is not damage to them" (countersOn spine atBobsSentry) 0
          Spec.assertEqWith s "and the 3/3 Sentry survived the 1 damage, so the board is otherwise the same" (Set.member bobsSentry (GameState.battlefield atBobsSentry)) True
          -- CR 119.4's life loss is not CR 120.1's damage, and the log records the
          -- two separately. Without this the reader could be looking at
          -- GameEvent.LifeLost and pass every assertion above, since CR 120.3a's
          -- damage to a player files one of those too.
          Spec.assertEqWith s "CR 119.4 life lost without damage is not damage" (countersOn spine bobPaid) 0
        -- CR 608.2i: the window is THIS turn. Without this, a lifetime tally passes
        -- every assertion above. The turn goes all the way round to alice again, so
        -- the only difference from the two-opponent case is which turn it is.
        Spec.it s "CR 608.2i the damage is THIS turn's: it resets at the handoff" $ do
          spinesplitter <- S.printingOf s registry "Furious Spinesplitter"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let (spine, gs0) = S.addCreature spinesplitter S.alice S.threePlayerGame
              (_, base) = S.addCreature sentry S.bob gs0
              damaged = hit spine (Recipient.ToPlayer S.carol) 4 (hit spine (Recipient.ToPlayer S.bob) 3 base)
              handoff gs = S.runPure S.identityAnswer gs Engine.handoffTurn
              roundAgain = handoff (handoff (handoff damaged))
          Spec.assertEqWith s "the turn came back to alice" (GameState.activePlayer roundAgain) S.alice
          Spec.assertEqWith s "on the turn it happened, two" (countersOn spine (atEnd damaged)) 2
          Spec.assertEqWith s "a turn cycle later, none" (countersOn spine (atEnd roundAgain)) 0

-- Tidal Wave {2}{U} Instant: "Create a 5/5 blue Wall creature token with defender.
-- Sacrifice it at the beginning of the next end step." CR 603.7c's object-bound
-- delayed ability -- "it" must survive the resolution that armed it. Synthetic
-- Deferred Rally joins it for the one shape Tidal Wave cannot reach: a delayed
-- ability with an intervening "if".
delayedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
delayedSpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice)) (gs {GameState.phase = endStep})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      walls gs = filter (\oid -> Set.member Subtype.Wall (Projection.subtypesOf oid gs)) (Set.toList (GameState.battlefield gs))
      -- Answers Prompt.ChooseBoundToken with the LAST token minted, recording
      -- every candidate list so a test can assert whether the prompt was issued
      -- at all. Naming the last is what makes the assertion discriminating:
      -- binding the FIRST is exactly what the engine used to do silently.
      chooseLastToken :: Prompt.Prompt r -> State.State [[ObjectId.ObjectId]] r
      chooseLastToken p = case p of
        Prompt.ChooseBoundToken _ _ _ candidates -> do
          State.modify' (<> [NonEmpty.toList candidates])
          pure (NonEmpty.last candidates)
        _ -> pure (S.identityAnswer p)
      -- Answers Prompt.ChooseBoundToken with an object that was never minted, so
      -- the engine's filter is what decides the binding. Id 999 names nothing --
      -- the same posture S.noSource takes.
      -- Stamp an expiry onto every armed delayed ability, so the CR 603.7b
      -- stated-duration mechanism can be exercised on a real armed entry.
      withExpiry expiry gs =
        gs
          { GameState.delayedTriggers =
              fmap (\entry -> entry {DelayedTrigger.expiry = expiry}) (GameState.delayedTriggers gs)
          }
      chooseUnmintedToken :: Prompt.Prompt r -> r
      chooseUnmintedToken p = case p of
        Prompt.ChooseBoundToken {} -> ObjectId.MkObjectId 999
        _ -> S.identityAnswer p
      -- alice casts the spell in hand and resolves it under chooseLastToken,
      -- handing back the board alongside the candidate lists it was asked about.
      castUnderChoice gs oid =
        State.runState
          ( Engine.runGame chooseLastToken gs $ do
              S.cast S.alice oid
              Engine.priorityLoop
          )
          []
   in Spec.describe s "DelayedTrigger" $ do
        Spec.it s "CR 111.3 the spell mints a 5/5 Wall with defender and arms one delayed ability" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          let after = castWave tidalWave island
          case walls after of
            [wall] -> do
              Spec.assertEqWith s "5 power" (Projection.powerOf wall after) (Just 5)
              Spec.assertEqWith s "5 toughness" (Projection.toughnessOf wall after) (Just 5)
              Spec.assertBool s (Projection.hasKeyword Keyword.Type.Defender wall after) "defender"
              Spec.assertEqWith s "one delayed ability waiting" (Seq.length (GameState.delayedTriggers after)) 1
            other -> Spec.assertFailure s ("expected exactly one Wall token, got " <> show (length other))
        -- CR 603.7b: "only once, the next time its trigger event occurs".
        Spec.it s "CR 603.7 the token is sacrificed at the beginning of the next end step" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          let after = resolveAll (settle (beginEndStep (castWave tidalWave island)))
          Spec.assertEqWith s "no Wall left" (walls after) []
          Spec.assertEqWith s "the store is empty" (Seq.length (GameState.delayedTriggers after)) 0
        -- CR 603.7b's other half: "unless it has a stated duration, such as
        -- 'this turn.'" Tidal Wave's entry is reused with an expiry stamped on
        -- it, so the mechanism is tested without inventing a card; Full
        -- Throttle is the card that actually prints one.
        Spec.it s "CR 603.7b a stated-duration delayed ability stays armed after firing" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          let armed = castWave tidalWave island
              stated = withExpiry (Just Expiry.Type.AtCleanup) armed
              began = [GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice)]
              (firedOnce, survivors) = Event.delayedPending began stated
              (firedAgain, _) = Event.delayedPending began stated {GameState.delayedTriggers = survivors}
          Spec.assertEqWith s "it fired" (length firedOnce) 1
          Spec.assertEqWith s "and stayed armed" (Seq.length survivors) 1
          Spec.assertEqWith s "so the next end step fires it again" (length firedAgain) 1
        Spec.it s "CR 603.7b without a stated duration firing still spends it" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          let armed = castWave tidalWave island
              (fired, survivors) = Event.delayedPending [GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice)] armed
          Spec.assertEqWith s "it fired" (length fired) 1
          Spec.assertEqWith s "and was evicted" (Seq.length survivors) 0
        -- Synthetic Deferred Rally {W} Instant: "At the beginning of the next
        -- end step, if you control a creature, you gain 2 life." A LABELED
        -- CRUTCH (#851): every printed delayed ability with an intervening "if"
        -- asks whether a named card was cast, played or is still in some zone,
        -- and neither Quantity nor Filter can name a card. CR 603.4 and
        -- CR 603.7b meeting on one ability: the first end step's event matches,
        -- but the intervening "if" is false, so the ability does not TRIGGER --
        -- and CR 603.7b bounds how many times it triggers, not how many events
        -- it watches, so nothing was spent and it is still waiting for the next
        -- end step.
        --
        -- The STORE is where the difference shows, and the two assertions on it
        -- are the discriminating pair: still armed after the false occurrence,
        -- spent after the true one. The life total alone would not discriminate
        -- in the first half -- an entry that wrongly triggered at the first end
        -- step still gains nothing, because Pawl.Engine.Stack's CR 608.2a
        -- re-check removes it from the stack for the same false condition, so
        -- the one shot would be spent invisibly.
        Spec.it s "CR 603.4 a false intervening \"if\" leaves the delayed ability armed for the next end step" $ do
          rally <- S.printingOf s registry "Synthetic Deferred Rally"
          plains <- S.printingOf s registry "Plains"
          piker <- S.printingOf s registry "Goblin Piker"
          let (gs0, oid) = S.handOne rally (S.landsInPlay plains 1)
              armed = resolveAll (snd (Engine.runGamePure S.identityAnswer gs0 (S.cast S.alice oid)))
              firstEnd = resolveAll (settle (beginEndStep armed))
              withCreature = snd (S.addCreature piker S.alice firstEnd)
              secondEnd = resolveAll (settle (beginEndStep withCreature))
          Spec.assertEqWith s "the resolution armed it" (Seq.length (GameState.delayedTriggers armed)) 1
          Spec.assertEqWith s "no life gained while the condition is false" (S.lifeOf S.alice firstEnd) (Just 20)
          Spec.assertEqWith s "and the entry is still armed" (Seq.length (GameState.delayedTriggers firstEnd)) 1
          Spec.assertEqWith s "the next end step fires it" (S.lifeOf S.alice secondEnd) (Just 22)
          Spec.assertEqWith s "and that spends it" (Seq.length (GameState.delayedTriggers secondEnd)) 0
        -- CR 514.2: "all 'until end of turn' and 'this turn' effects end"
        -- during the cleanup step -- which is what ends the stated duration,
        -- and the reason an armed entry cannot outlive the turn that made it.
        Spec.it s "CR 514.2 cleanup drops a stated-duration delayed ability" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          let armed = castWave tidalWave island
              swept expiry = GameState.delayedTriggers (Expiry.dropAtCleanup (withExpiry expiry armed))
          Spec.assertEqWith s "an 'this turn' entry is gone" (Seq.length (swept (Just Expiry.Type.AtCleanup))) 0
          Spec.assertEqWith s "an end-of-game entry stays" (Seq.length (swept (Just Expiry.Type.Never))) 1
          -- CR 603.7b's one shot is spent by FIRING, not by time, so an entry
          -- on no duration at all must survive every sweep.
          Spec.assertEqWith s "and a one-shot entry stays" (Seq.length (swept Nothing)) 1
        Spec.it s "CR 603.7b a second end step does not re-fire it" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          let once = resolveAll (settle (beginEndStep (castWave tidalWave island)))
              again = settle (beginEndStep once)
          Spec.assertEqWith s "nothing on the stack" (GameState.stack again) []
        -- CR 603.7a: a delayed ability does not trigger on an event that
        -- happened BEFORE it was created. Falls out of the watermark for free.
        Spec.it s "CR 603.7a armed during an end step, it waits for the NEXT one" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          let (gs0, oid) = S.handOne tidalWave (S.landsInPlay island 3)
              inEndStep = settle (beginEndStep gs0)
              cast = resolveAll (snd (Engine.runGamePure S.identityAnswer inEndStep (S.cast S.alice oid)))
              sameStep = settle cast
              nextStep = resolveAll (settle (beginEndStep sameStep))
          Spec.assertEqWith s "still alive during the step it was armed in" (length (walls sameStep)) 1
          Spec.assertEqWith s "sacrificed at the next end step" (walls nextStep) []
        -- CR 603.7c: the ability still triggers and is still consumed even when
        -- the object it remembers is gone.
        Spec.it s "CR 603.7c with the token already gone the ability does nothing and is consumed" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          let armed = castWave tidalWave island
              killed = case walls armed of
                wall : _ -> S.settleSba (S.runPure S.identityAnswer armed (Event.destroy Regenerability.Regenerable [wall]))
                [] -> armed
              after = resolveAll (settle (beginEndStep killed))
          Spec.assertEqWith s "no Wall" (walls after) []
          Spec.assertEqWith s "the store is still emptied" (Seq.length (GameState.delayedTriggers after)) 0
          Spec.assertEqWith s "nothing stuck on the stack" (GameState.stack after) []
        -- IMPORTANT-1 (fix pass 1): Engine.placeOne merges a delayed ability's
        -- OWN placement-time bindings (its chosen modes/targets, chosen just now)
        -- with the environment CAPTURED when the ability was armed, under
        -- Map.union -- left-biased, so the argument ORDER decides which side
        -- wins a collision on a reserved slot such as Binding.chosenModes. The
        -- two DO collide in practice: Pawl.Engine.Cast builds an arming spell's
        -- bindings through the same Binding.fromChoices that stamps chosenModes
        -- whenever the spell chooses a mode, so a modal arming spell's captured
        -- environment carries a "modes" entry that belongs to the SPELL, not to
        -- the delayed ability being placed.
        --
        -- Tidal Wave cannot exercise this: both it and its one delayed ability
        -- have exactly one mode, so both always choose mode 0 and the two
        -- possible union orders are indistinguishable through it (the earlier
        -- Tidal Wave tests above pass under EITHER order). This test instead
        -- calls Engine.placeOne directly with a hand-built PendingTrigger whose
        -- CAPTURED bindings carry a chosenModes entry for a mode index (7) the
        -- ability being placed does not even have -- standing in for "whatever
        -- mode a modal arming spell happened to choose". The ability itself has
        -- one legal mode (index 0, forced/unprompted), so a correct placement
        -- can only ever stamp {0} for ITS OWN choice. Under the pre-fix
        -- direction (`Map.union captured placementTime`) the captured {7} would
        -- win instead, and this assertion would fail.
        Spec.it s "CR 603.7c placement-time's own chosen mode wins a collision with the captured environment" $ do
          let onlyMode = Mode.MkMode {Mode.clauses = Seq.empty, Mode.targetSlots = Map.empty}
              ability =
                TriggeredAbility.MkTriggeredAbility
                  { TriggeredAbility.condition = TriggerCondition.SelfEnters,
                    TriggeredAbility.modal = Modal.MkModal {Modal.modes = Seq.singleton onlyMode, Modal.selection = ModeSelection.ChooseExactly 1},
                    TriggeredAbility.intervening = Nothing,
                    TriggeredAbility.limit = TriggerLimit.Unlimited
                  }
              -- Stands in for a modal arming spell's own captured chosenModes --
              -- built with the SAME Binding.fromChoices Cast.castSpell uses, so
              -- the collision is the real production shape, not a fabricated one.
              captured = Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 7))
              pending = PendingTrigger.MkPendingTrigger (TriggerSource.OfObject (ObjectId.MkObjectId 0)) S.alice ability captured
              after = snd (Engine.runGamePure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Engine.placeOne pending))
              placedModes = case GameState.stack after of
                placedId : _ -> case Game.lookupObject placedId after of
                  Just obj -> Binding.modesOf (Object.bindings obj)
                  Nothing -> Seq.empty
                [] -> Seq.empty
          Spec.assertEqWith s "the ability's own mode (0), not the captured spell's mode (7)" placedModes (Seq.singleton (ModeIndex.MkModeIndex 0))
        -- CR 800.4d: "If a triggered ability that would be controlled by a
        -- player who has left the game would be put onto the stack, it isn't
        -- put on the stack." CR 800.4d's own example is a delayed ability, and
        -- so is this: Tidal Wave's "Sacrifice it at the beginning of the next
        -- end step", armed by bob, whose controller is baked in at arming
        -- (CR 603.7d) and so survives him. CR 603.7b still spends its one shot:
        -- the example's Hypnotic Specter "never returns to the battlefield."
        --
        -- Three seats, because at two the departure ends the game before an end
        -- step can arrive.
        Spec.it s "CR 800.4d a departed player's delayed ability triggers, is consumed, and is not put on the stack" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          let (_, l1) = S.addCreature island S.bob S.threePlayerGame
              (_, l2) = S.addCreature island S.bob l1
              (_, l3) = S.addCreature island S.bob l2
              (waveId, l4) = S.addHandCard tidalWave S.bob l3
              ready = l4 {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.bob, GameState.priority = Just S.bob}
              cast = S.runPure S.identityAnswer ready (S.cast S.bob waveId)
              armed = S.runPure S.identityAnswer cast Engine.priorityLoop
              gone = S.departs Departure.Type.Conceded S.bob armed
              began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice)) (gone {GameState.phase = endStep})
              (placedAny, placed) = S.runPureWith S.identityAnswer began Engine.placePendingTriggers
              (controlAny, control) = S.runPureWith S.identityAnswer (Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice)) (armed {GameState.phase = endStep})) Engine.placePendingTriggers
          Spec.assertEqWith s "the fixture really armed one delayed ability" (Seq.length (GameState.delayedTriggers armed)) 1
          Spec.assertEqWith s "bob's ability is not put on the stack" (GameState.stack placed) []
          Spec.assertEqWith s "CR 603.7b: it still triggered, so its one shot is spent" (Seq.length (GameState.delayedTriggers placed)) 0
          Spec.assertEqWith s "with bob still in the game the SAME ability IS placed -- the filter is what did it" (length (GameState.stack control)) 1
          Spec.assertEqWith s "nothing reached the stack, so placePendingTriggers honestly reports it placed nothing" placedAny False
          Spec.assertEqWith s "with bob still in the game, something genuinely got placed" controlAny True
        -- CR 614.16 meets CR 603.7c. Doubling Season ("If an effect would
        -- create one or more tokens under your control, it creates twice that
        -- many of those tokens instead") scales Tidal Wave's Create at
        -- RESOLUTION, so two Walls are minted where CR 603.7c's "it" names one
        -- particular object. CR 707.10e is the codified analogue and settles
        -- that the leftover is a CHOICE, not something the engine may decide:
        -- where a replacement causes a copy to target more than one object,
        -- "the copy's controller chooses one of them to be the new target",
        -- and its Frontline Heroism / Anointed Procession example is this exact
        -- shape -- two tokens created, "the copy targets one of those tokens of
        -- your choice."
        --
        -- Discriminating: the answerer names the LAST minted token, and the
        -- unfixed engine bound the first, so the wrong Wall would be the one
        -- that died and the surviving assertion would fail too.
        Spec.it s "CR 614.16/603.7c a doubled Create asks which minted token \"it\" names" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          doublingSeason <- S.printingOf s registry "Doubling Season"
          let (_, base) = S.addCreature doublingSeason S.alice (S.landsInPlay island 3)
              (gs, waveId) = S.handOne tidalWave base
              ((_, armed), asked) = castUnderChoice gs waveId
              after = resolveAll (settle (beginEndStep armed))
          Spec.assertEqWith s "the replacement really doubled the Create" (length (walls armed)) 2
          case asked of
            [[unchosen, chosen]] -> do
              Spec.assertEqWith s "the token named by \"it\" was sacrificed, and only it" (walls after) [unchosen]
              Spec.assertBool s (Set.notMember chosen (GameState.battlefield after)) "the chosen Wall is off the battlefield"
            _ -> Spec.assertFailure s ("expected one prompt offering two tokens, got " <> show asked)
        -- The companion, and the elision this pairs with: without the
        -- replacement the Create mints exactly one token, so "it" has only one
        -- possible referent and there is nothing to ask. Where the rules leave
        -- nothing to ask, don't prompt.
        Spec.it s "CR 603.7c one minted token is no choice, so nothing is asked" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          let (gs, waveId) = S.handOne tidalWave (S.landsInPlay island 3)
              ((_, armed), asked) = castUnderChoice gs waveId
              after = resolveAll (settle (beginEndStep armed))
          Spec.assertEqWith s "one Wall minted" (length (walls armed)) 1
          Spec.assertEqWith s "no binding prompt was issued" asked []
          Spec.assertEqWith s "and it is still sacrificed at the end step" (walls after) []
        -- FILTERED, NOT TRUSTED, the posture Sba.chooseLegendVictims takes for
        -- CR 704.5j: an answer naming something that was never minted would
        -- otherwise leave CR 603.7c's "it" pointing at nothing and the delayed
        -- ability would sacrifice neither Wall. The slot is bound either way,
        -- deterministically to the first token.
        Spec.it s "CR 603.7c an answer naming an unminted object falls back to the first token" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          doublingSeason <- S.printingOf s registry "Doubling Season"
          let (_, base) = S.addCreature doublingSeason S.alice (S.landsInPlay island 3)
              (gs, waveId) = S.handOne tidalWave base
              cast = S.runPure chooseUnmintedToken gs (S.cast S.alice waveId)
              armed = S.runPure chooseUnmintedToken cast Engine.priorityLoop
              after = resolveAll (settle (beginEndStep armed))
          case walls armed of
            [firstWall, secondWall] -> do
              Spec.assertEqWith s "only the second minted Wall is left" (walls after) [secondWall]
              Spec.assertBool s (Set.notMember firstWall (GameState.battlefield after)) "the first minted Wall was bound, and it is gone"
            other -> Spec.assertFailure s ("expected two Wall tokens, got " <> show (length other))

-- Thatcher Revolt {2}{R} Sorcery: "Create three 1/1 red Human creature tokens
-- with haste. Sacrifice those tokens at the beginning of the next end step."
-- delayedSpec's plural sibling: the card refers back to EVERY token it made at
-- once, so the Create's slot holds a GROUP rather than one object, and the
-- delayed ability sacrifices all of it.
tokenSetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
tokenSetSpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice)) (gs {GameState.phase = endStep})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      humans gs = filter (\oid -> Set.member Subtype.Human (Projection.subtypesOf oid gs)) (Set.toList (GameState.battlefield gs))
      -- Records every Prompt.ChooseBoundToken the resolution issues, so a test
      -- can assert the plural binding asks NOTHING -- "them" names every minted
      -- token, and where the rules leave nothing to ask, don't prompt.
      recordPrompts :: Prompt.Prompt r -> State.State [[ObjectId.ObjectId]] r
      recordPrompts p = case p of
        Prompt.ChooseBoundToken _ _ _ candidates -> do
          State.modify' (<> [NonEmpty.toList candidates])
          pure (NonEmpty.head candidates)
        _ -> pure (S.identityAnswer p)
      castUnderPrompts gs oid =
        State.runState
          ( Engine.runGame recordPrompts gs $ do
              S.cast S.alice oid
              Engine.priorityLoop
          )
          []
      -- alice casts Thatcher Revolt from hand onto the given board and lets it
      -- resolve, handing back the board alongside any prompts it issued.
      castRevolt revolt base = uncurry castUnderPrompts (S.handOne revolt base)
      boardOf mountain = S.landsInPlay mountain 3
   in Spec.describe s "DelayedTrigger token set" $ do
        Spec.it s "CR 111.3 the spell mints three 1/1 Humans with haste and arms one delayed ability" $ do
          revolt <- S.printingOf s registry "Thatcher Revolt"
          mountain <- S.printingOf s registry "Mountain"
          let ((_, armed), asked) = castRevolt revolt (boardOf mountain)
          case humans armed of
            tokens@[_, _, _] -> do
              Spec.assertEqWith s "each is 1/1" (fmap (`Projection.powerOf` armed) tokens) [Just 1, Just 1, Just 1]
              Spec.assertBool s (all (\oid -> Projection.hasKeyword Keyword.Type.Haste oid armed) tokens) "each has haste"
              Spec.assertEqWith s "one delayed ability waiting" (Seq.length (GameState.delayedTriggers armed)) 1
            other -> Spec.assertFailure s ("expected exactly three Human tokens, got " <> show (length other))
          -- The second invariant: a group binding names every token, so unlike
          -- CR 603.7c's singular "it" there is no candidate to choose between.
          Spec.assertEqWith s "no binding prompt was issued" asked []
        -- Binding one of the three would be the engine choosing; binding all
        -- three is what "those tokens" says.
        Spec.it s "CR 111.1 \"those tokens\" names every minted token, so all three are sacrificed" $ do
          revolt <- S.printingOf s registry "Thatcher Revolt"
          mountain <- S.printingOf s registry "Mountain"
          let ((_, armed), _) = castRevolt revolt (boardOf mountain)
              after = resolveAll (settle (beginEndStep armed))
          Spec.assertEqWith s "three were minted" (length (humans armed)) 3
          Spec.assertEqWith s "and none is left" (humans after) []
          Spec.assertEqWith s "the store is empty" (Seq.length (GameState.delayedTriggers after)) 0
        -- CR 614.16 meets the plural binding, and this is where it differs from
        -- CR 603.7c's singular "it": a replacement that multiplies the count just
        -- makes the set bigger. "Those tokens" still names all of them, so there
        -- is nothing to ask and nothing survives -- where the singular case must
        -- prompt (see delayedSpec's doubled Tidal Wave).
        Spec.it s "CR 614.16 a doubled Create binds all six, unprompted" $ do
          revolt <- S.printingOf s registry "Thatcher Revolt"
          mountain <- S.printingOf s registry "Mountain"
          doublingSeason <- S.printingOf s registry "Doubling Season"
          let (_, base) = S.addCreature doublingSeason S.alice (boardOf mountain)
              ((_, armed), asked) = castRevolt revolt base
              after = resolveAll (settle (beginEndStep armed))
          Spec.assertEqWith s "the replacement really doubled the Create" (length (humans armed)) 6
          Spec.assertEqWith s "still nothing to ask" asked []
          Spec.assertEqWith s "and all six are sacrificed" (humans after) []
        -- CR 603.7c's "no longer in the zone it's expected to be in": one token
        -- already gone does not spare the others, and the ability is still spent.
        Spec.it s "CR 603.7c one token already gone leaves the rest sacrificed" $ do
          revolt <- S.printingOf s registry "Thatcher Revolt"
          mountain <- S.printingOf s registry "Mountain"
          let ((_, armed), _) = castRevolt revolt (boardOf mountain)
              killed = case humans armed of
                token : _ -> S.settleSba (S.runPure S.identityAnswer armed (Event.destroy Regenerability.Regenerable [token]))
                [] -> armed
              after = resolveAll (settle (beginEndStep killed))
          Spec.assertEqWith s "two were left to sacrifice" (length (humans killed)) 2
          Spec.assertEqWith s "and both are gone" (humans after) []
          Spec.assertEqWith s "the store is still emptied" (Seq.length (GameState.delayedTriggers after)) 0
          Spec.assertEqWith s "nothing stuck on the stack" (GameState.stack after) []
        -- The positive control for the three "nothing to ask" assertions above.
        -- They are claims about the ANSWERER as much as about the engine: a
        -- recordPrompts that matched the wrong constructor or dropped its
        -- State.modify' would report an empty list for every case in this group
        -- and every one of them would pass. So the SAME answerer is pointed at
        -- Tidal Wave under Doubling Season, the singular case that must prompt.
        Spec.it s "the recorder does see a prompt when the singular case asks for one" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          doublingSeason <- S.printingOf s registry "Doubling Season"
          let (_, base) = S.addCreature doublingSeason S.alice (S.landsInPlay island 3)
              (_, asked) = uncurry castUnderPrompts (S.handOne tidalWave base)
          Spec.assertEqWith s "one prompt, offering the two minted Walls" (fmap length asked) [2]

-- Salt Road Skirmish {3}{B} Sorcery: "Destroy target creature. Create two 1/1 red
-- Warrior creature tokens. They gain haste until end of turn. Sacrifice them at
-- the beginning of the next end step."
--
-- tokenSetSpec's other half. There the group binding was WRITTEN by a Create and
-- read by the one opcode that understood it; here a THIRD sentence reads the same
-- group through ObjectRef.InSlot, which is what makes the binding a general
-- reference rather than Sacrifice's private channel. "They" and "them" are the
-- same two tokens, named by two different opcodes in one resolution.
tokenGroupReadSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
tokenGroupReadSpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice)) (gs {GameState.phase = endStep})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      warriors gs = filter (\oid -> Set.member Subtype.Warrior (Projection.subtypesOf oid gs)) (Set.toList (GameState.battlefield gs))
      -- alice casts the Skirmish off four Swamps with bob's creature the only
      -- one on the battlefield, so S.identityAnswer's lowest-legal-recipient
      -- pick is that creature and nothing else can be targeted by accident.
      board swamp victim = S.addCreature victim S.bob (S.landsInPlay swamp 4)
      castSkirmish skirmish base =
        let (gs, oid) = S.handOne skirmish base
         in resolveAll (snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice oid)))
   in Spec.describe s "Group read through InSlot" $ do
        -- THE PROVING CASE. Before ObjectRef.InSlot could see a group binding,
        -- "they gain haste" resolved against an empty slot and granted haste to
        -- nobody, while the two tokens still appeared and were still sacrificed
        -- -- so only the keyword assertion discriminates.
        Spec.it s "CR 613.1f \"they gain haste\" reaches BOTH minted tokens" $ do
          skirmish <- S.printingOf s registry "Salt Road Skirmish"
          swamp <- S.printingOf s registry "Swamp"
          rats <- S.printingOf s registry "Typhoid Rats"
          let (_, base) = board swamp rats
              after = castSkirmish skirmish base
          case warriors after of
            tokens@[_, _] ->
              Spec.assertEqWith
                s
                "each of the two has haste"
                (fmap (\oid -> Projection.hasKeyword Keyword.Type.Haste oid after) tokens)
                [True, True]
            other -> Spec.assertFailure s ("expected exactly two Warrior tokens, got " <> show (length other))
        Spec.it s "CR 701.8 the same resolution still destroys its target" $ do
          skirmish <- S.printingOf s registry "Salt Road Skirmish"
          swamp <- S.printingOf s registry "Swamp"
          rats <- S.printingOf s registry "Typhoid Rats"
          let (victim, base) = board swamp rats
              after = castSkirmish skirmish base
          Spec.assertBool s (Set.notMember victim (GameState.battlefield after)) "the targeted creature is gone"
          Spec.assertEqWith s "and two Warriors stand" (length (warriors after)) 2
        -- The group is read by TWO opcodes in one resolution, and the second
        -- must still find it: ModifyTarget consuming the slot would leave the
        -- delayed ability with nothing to sacrifice.
        Spec.it s "CR 603.7c and the delayed ability still sacrifices both" $ do
          skirmish <- S.printingOf s registry "Salt Road Skirmish"
          swamp <- S.printingOf s registry "Swamp"
          rats <- S.printingOf s registry "Typhoid Rats"
          let (_, base) = board swamp rats
              armed = castSkirmish skirmish base
              after = resolveAll (settle (beginEndStep armed))
          Spec.assertEqWith s "two before the end step" (length (warriors armed)) 2
          Spec.assertEqWith s "none after it" (warriors after) []
        -- CR 608.2b: "if all its targets ... are now illegal, the spell doesn't
        -- resolve". The card's own Gatherer ruling spells out what that costs
        -- here -- "it won't resolve and none of its effects will happen" -- so
        -- the tokens the LATER sentences would have made are never minted and
        -- the group is never bound. Also the negative control for the group
        -- read: no group, and the haste sentence still has to be harmless.
        Spec.it s "CR 608.2b an illegal target takes the whole spell, tokens and all" $ do
          skirmish <- S.printingOf s registry "Salt Road Skirmish"
          swamp <- S.printingOf s registry "Swamp"
          rats <- S.printingOf s registry "Typhoid Rats"
          let (victim, base) = board swamp rats
              (gs, oid) = S.handOne skirmish base
              onStack = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice oid))
              -- The target leaves before the Skirmish resolves, which is the only
              -- way to make it illegal without a second card.
              gone = S.settleSba (S.runPure S.identityAnswer onStack (Event.destroy Regenerability.Regenerable [victim]))
              after = resolveAll gone
          Spec.assertBool s (Set.notMember victim (GameState.battlefield after)) "the target really left"
          Spec.assertEqWith s "no Warriors were minted" (warriors after) []
          Spec.assertEqWith s "and nothing was armed" (Seq.length (GameState.delayedTriggers after)) 0
          -- Otherwise "no Warriors" would also be true of a spell still sitting
          -- on the stack, which is a different bug wearing the same result.
          Spec.assertEqWith s "the spell left the stack rather than stalling on it" (GameState.stack after) []

-- Feral Lightning {3}{R}{R}{R} Sorcery: "Create three 3/1 red Elemental creature
-- tokens with haste. Exile them at the beginning of the next end step."
--
-- tokenGroupReadSpec's third reader, and the one that is not an ObjectRef sweep:
-- Effect.MoveToZone's ObjectRef.InSlot arm branches by hand rather than going
-- through objectRefObjects (it has to -- an incarnation an earlier effect of the
-- same resolution bound is invisible to `chosen`), so the group read had to be
-- written there too. "Them" is all three tokens, moved as one batch.
--
-- THE OBSERVABLE IS THE BATTLEFIELD, not the exile zone: CR 111.7 makes a token
-- in any other zone cease to exist, so a token that reached exile is not there to
-- be counted. What discriminates is that all three leave and that nothing else
-- does.
tokenGroupMoveSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
tokenGroupMoveSpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice)) (gs {GameState.phase = endStep})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      elementals gs = filter (\oid -> Set.member Subtype.Elemental (Projection.subtypesOf oid gs)) (Set.toList (GameState.battlefield gs))
      -- Six Mountains for the {3}{R}{R}{R}, and TWO bystanders that are not
      -- Elementals: one alice's own, so "them" is told from "every creature you
      -- control", and one bob's, so it is told from "every creature".
      board mountain piker rats =
        let (pikerId, gs1) = S.addCreature piker S.alice (S.landsInPlay mountain 6)
            (ratsId, gs2) = S.addCreature rats S.bob gs1
         in (pikerId, ratsId, gs2)
      castLightning lightning base =
        let (gs, oid) = S.handOne lightning base
         in resolveAll (snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice oid)))
   in Spec.describe s "Group move through InSlot" $ do
        Spec.it s "CR 111.3 the spell mints three 3/1 Elementals and arms one delayed ability" $ do
          lightning <- S.printingOf s registry "Feral Lightning"
          mountain <- S.printingOf s registry "Mountain"
          piker <- S.printingOf s registry "Goblin Piker"
          rats <- S.printingOf s registry "Typhoid Rats"
          let (_, _, base) = board mountain piker rats
              armed = castLightning lightning base
          case elementals armed of
            tokens@[_, _, _] -> do
              Spec.assertEqWith s "each is 3/1" (fmap (\oid -> (Projection.powerOf oid armed, Projection.toughnessOf oid armed)) tokens) [(Just 3, Just 1), (Just 3, Just 1), (Just 3, Just 1)]
              Spec.assertBool s (all (\oid -> Projection.hasKeyword Keyword.Type.Haste oid armed) tokens) "each has haste"
              Spec.assertEqWith s "one delayed ability waiting" (Seq.length (GameState.delayedTriggers armed)) 1
            other -> Spec.assertFailure s ("expected exactly three Elemental tokens, got " <> show (length other))
        -- THE PROVING CASE. Before Effect.MoveToZone's InSlot arm read a group
        -- binding, the slot held no single object (Pawl.Engine.Resolve.slotOne
        -- answers Nothing for a group) and "exile them" moved NOBODY, while the
        -- three tokens were still minted and the ability was still spent -- so
        -- only the after-count discriminates.
        Spec.it s "CR 406.2 \"exile them\" moves all three, and only them" $ do
          lightning <- S.printingOf s registry "Feral Lightning"
          mountain <- S.printingOf s registry "Mountain"
          piker <- S.printingOf s registry "Goblin Piker"
          rats <- S.printingOf s registry "Typhoid Rats"
          let (pikerId, ratsId, base) = board mountain piker rats
              armed = castLightning lightning base
              after = resolveAll (settle (beginEndStep armed))
          Spec.assertEqWith s "three were minted" (length (elementals armed)) 3
          Spec.assertEqWith s "and none is left" (elementals after) []
          Spec.assertBool s (Set.member pikerId (GameState.battlefield after)) "alice's own non-token creature stayed"
          Spec.assertBool s (Set.member ratsId (GameState.battlefield after)) "bob's creature stayed"
          Spec.assertEqWith s "the store is empty" (Seq.length (GameState.delayedTriggers after)) 0
          Spec.assertEqWith s "nothing stuck on the stack" (GameState.stack after) []
        -- CR 603.7c's "no longer in the zone it's expected to be in": one token
        -- already gone does not spare the other two.
        Spec.it s "CR 603.7c one token already gone leaves the rest exiled" $ do
          lightning <- S.printingOf s registry "Feral Lightning"
          mountain <- S.printingOf s registry "Mountain"
          piker <- S.printingOf s registry "Goblin Piker"
          rats <- S.printingOf s registry "Typhoid Rats"
          let (pikerId, _, base) = board mountain piker rats
              armed = castLightning lightning base
              killed = case elementals armed of
                token : _ -> S.settleSba (S.runPure S.identityAnswer armed (Event.destroy Regenerability.Regenerable [token]))
                [] -> armed
              after = resolveAll (settle (beginEndStep killed))
          Spec.assertEqWith s "two were left to exile" (length (elementals killed)) 2
          Spec.assertEqWith s "and both are gone" (elementals after) []
          Spec.assertBool s (Set.member pikerId (GameState.battlefield after)) "the bystander is still untouched"

-- Harried Dronesmith {3}{R} Creature -- Human Artificer 2/3: "At the beginning of
-- combat on your turn, create a 1/1 colorless Thopter artifact creature token with
-- flying. It gains haste until end of turn. Sacrifice it at the beginning of your
-- next end step."
--
-- tokenGroupReadSpec's SINGULAR, and the whole reason it is a separate group: a
-- Create binds one token into the binding's target field and several into its
-- objects group (Pawl.Engine.Resolve.bindSlot and bindObjectsSlot), and the two
-- are read back by different routes. Salt Road Skirmish's "they" reads the group,
-- which has always been live; this card's "it" reads the single target, which the
-- ability path once fixed before its effect fold began -- so CR 608.2c's "in the
-- order written" was violated for the second sentence only, and only when the
-- sentence before it made exactly one token.
--
-- ATTACKING is the discriminating observable, not the keyword. CR 302.6 keeps a
-- creature that has not been controlled continuously since the turn began from
-- attacking, and CR 702.10b is the only thing that lifts it; the token entered
-- during this very combat phase, so it is on the attackers menu if and only if
-- the haste grant landed on it. Asserting Projection.hasKeyword alone would be a
-- claim about a stored effect rather than about the game.
--
-- The token is authored with FLYING only. Printing haste on its face would make
-- every assertion below pass with the grant deleted entirely.
singleTokenSlotReadSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
singleTokenSlotReadSpec s registry =
  let thopters gs = filter (\oid -> Set.member Subtype.Thopter (Projection.subtypesOf oid gs)) (Set.toList (GameState.battlefield gs))
      -- alice, active, sitting in her own beginning of combat step with the
      -- Dronesmith and a Goblin Piker both SETTLED (S.addCreature stamps
      -- Sickness.Settled), so neither of the two creatures she already had is
      -- summoning-sick and a bug that kept EVERY creature off the menu could not
      -- hide inside assertion 2. The rest of the turn is scheduled so Engine.runStep
      -- can walk it.
      board dronesmith piker =
        let (dronesmithId, gs1) = S.addCreature dronesmith S.alice (Setup.emptyGame S.bothPlayers)
            (pikerId, gs2) = S.addCreature piker S.alice gs1
         in ( dronesmithId,
              pikerId,
              gs2
                { GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice,
                  GameState.phase = Phase.Combat CombatStep.BeginningOfCombat,
                  GameState.remaining =
                    Seq.fromList
                      [ Phase.Combat CombatStep.DeclareAttackers,
                        Phase.Combat CombatStep.DeclareBlockers,
                        Phase.Combat CombatStep.CombatDamage,
                        Phase.Combat CombatStep.EndOfCombat,
                        Phase.PostcombatMain,
                        Phase.Ending EndingStep.EndStep,
                        Phase.Ending EndingStep.Cleanup
                      ]
                }
            )
      -- The real turn loop, one whole step at a time: Engine.runStep is what
      -- writes the CR 603.2b StepBegan record this trigger matches, runs the
      -- priority round that resolves it, and advances the schedule.
      step gs = snd (Engine.runGamePure S.identityAnswer gs Engine.runStep)
      -- Run steps until the cleanup step is scheduled, which is one step PAST the
      -- end step -- so the delayed ability has fired and resolved. Bounded, so a
      -- schedule bug cannot loop forever.
      throughEndStep =
        let go n gs =
              if n <= (0 :: Int) || GameState.phase gs == Phase.Ending EndingStep.Cleanup
                then gs
                else go (n - 1) (step gs)
         in go 10
      dronesmithBoard = do
        dronesmith <- S.printingOf s registry "Harried Dronesmith"
        piker <- S.printingOf s registry "Goblin Piker"
        pure (board dronesmith piker)
   in Spec.describe s "Single-token slot read through InSlot" $ do
        -- CR 603.2b then CR 111.1: the step began, the ability triggered, and one
        -- token stands. The characteristics are all here because the token is
        -- authored in this card's own text and nothing else asserts them.
        Spec.it s "CR 111.1 the combat trigger mints one 1/1 colorless flying Thopter artifact creature" $ do
          (_, _, gs) <- dronesmithBoard
          let after = step gs
          case thopters after of
            [token] -> do
              Spec.assertEqWith s "1/1" (S.powerToughnessOf token after) (Just (1, 1))
              Spec.assertBool s (Projection.hasKeyword Keyword.Type.Flying token after) "flying"
              Spec.assertEqWith s "colorless" (Projection.colorsOf token after) Set.empty
              Spec.assertEqWith
                s
                "an artifact creature"
                (Projection.cardTypesOf token after)
                (Set.fromList [CardType.Artifact, CardType.Creature])
              Spec.assertEqWith s "under alice's control" (Projection.controllerOf token after) (Just S.alice)
              Spec.assertEqWith s "and untapped" (fmap Object.tapped (Game.lookupObject token after)) (Just TapState.Untapped)
              Spec.assertEqWith s "the same resolution armed the delayed ability" (Seq.length (GameState.delayedTriggers after)) 1
            other -> Spec.assertFailure s ("expected exactly one Thopter token, got " <> show (length other))
        -- THE PROVING CASE. "It gains haste until end of turn" is the
        -- sentence after the one that made the token, on a TRIGGERED ability, and
        -- the token's slot is read through ObjectRef.InSlot. With the ability
        -- path's pre-fold snapshot, that read comes back empty, ModifyTarget
        -- stores nothing, and CR 302.6 keeps the token home.
        --
        -- The Piker is asserted onto the same menu deliberately: it makes an
        -- answer of "every creature alice controls" and an answer of "none" both
        -- fail, so the Thopter's membership is the only thing that can carry
        -- this.
        Spec.it s "CR 702.10b \"it gains haste\" reaches the one token, so it can attack the turn it entered" $ do
          (_, pikerId, gs) <- dronesmithBoard
          let after = step gs
              menu = Combat.legalAttackers S.alice after
          Spec.assertEqWith s "the beginning of combat step is over" (GameState.phase after) (Phase.Combat CombatStep.DeclareAttackers)
          case thopters after of
            [token] -> do
              Spec.assertBool s (Projection.hasKeyword Keyword.Type.Haste token after) "the grant landed on the token"
              Spec.assertBool s (List.elem token menu) "so CR 302.6 does not keep it out of the attackers menu"
              Spec.assertBool s (List.elem pikerId menu) "and the Piker that was already there is on the same menu"
            other -> Spec.assertFailure s ("expected exactly one Thopter token, got " <> show (length other))
        -- The regression guard on the group reader's LIVE read, which the fix must
        -- not disturb: the third sentence's delayed ability reads the same slot at
        -- alice's own end step (CR 603.7 / 603.7b), and the onset is immediate, so
        -- "your next end step" is this turn's.
        Spec.it s "CR 603.7b the delayed ability sacrifices the token at alice's end step" $ do
          (_, _, gs) <- dronesmithBoard
          let armed = step gs
              after = throughEndStep armed
          Spec.assertEqWith s "one Thopter before the end step" (length (thopters armed)) 1
          Spec.assertEqWith s "none after it" (thopters after) []
          Spec.assertEqWith s "and the store is empty" (Seq.length (GameState.delayedTriggers after)) 0
          Spec.assertEqWith s "with nothing stuck on the stack" (GameState.stack after) []

-- CR 603.3b: "puts each triggered ability they control ... on the stack in any
-- order they choose". The centerpiece: two triggers, one controller, and an
-- order that changes the answer.
orderingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
orderingSpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice)) (gs {GameState.phase = endStep})
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      countersOn oid gs = maybe 0 (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      -- alice has Khabál Ghoul out and casts Tidal Wave, so both a delayed
      -- sacrifice and a step trigger are pending at the same end step.
      boardOf tidalWave khabalGhoul island =
        let (gs0, waveId) = S.handOne tidalWave (S.landsInPlay island 3)
            (ghoul, gs1) = S.addCreature khabalGhoul S.alice gs0
            cast = resolveAll (snd (Engine.runGamePure S.identityAnswer gs1 (S.cast S.alice waveId)))
         in (ghoul, beginEndStep cast)
      -- The source of the OTHER pending trigger: Tidal Wave's delayed ability,
      -- whose source is the resolved spell's id rather than any permanent.
      otherThan ghoul gs =
        let sources = fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped gs) gs))
         in case filter (/= TriggerSource.OfObject ghoul) sources of
              src : _ -> src
              [] -> TriggerSource.OfObject ghoul
      -- An answerer that puts a chosen source LAST on the stack, so it resolves
      -- FIRST (CR 603.3b's answer is the order they are PUT on the stack). The
      -- two triggers here come from two different sources, so the entry's SOURCE
      -- is enough to name one of them; Pawl.KeywordTriggerSpec's battleCrySpec
      -- is where the ability half of the entry (#61) is what does the naming.
      orderLast :: TriggerSource.TriggerSource -> Prompt.Prompt r -> r
      orderLast wanted p = case p of
        Prompt.OrderTriggers _ _ entries ->
          let indexed = zip [0 ..] entries
              pick keep = fmap fst (filter (\entry -> (TriggerEntry.source (snd entry) == wanted) == keep) indexed)
           in pick False <> pick True
        _ -> S.identityAnswer p
      -- Counts how many times the ordering prompt was asked, answering canonically.
      countingAnswer :: Prompt.Prompt r -> State.State Int r
      countingAnswer p = case p of
        Prompt.OrderTriggers _ _ entries -> do
          State.modify' (+ 1)
          pure (zipWith const [0 ..] entries)
        _ -> pure (S.identityAnswer p)
      -- alice has a permanent that watches creatures enter, plus lands and a
      -- token-maker in hand. Casting and resolving the maker is one event, so CR
      -- 603.6a fires the watcher once per entrant.
      watcherBoard watcher land n maker =
        let (gs0, makerId) = S.handOne maker (S.landsInPlay land n)
            (_, gs1) = S.addCreature watcher S.alice gs0
         in (gs1, makerId)
      -- Cast it, run priority out, and report how many times CR 603.3b's order
      -- was asked for along the way.
      castCounting gs makerId =
        let ((_, after), asked) = State.runState (Engine.runGame countingAnswer gs (S.cast S.alice makerId >> Engine.priorityLoop)) 0
         in (after, asked)
   in Spec.describe s "TriggerOrdering" $ do
        Spec.it s "CR 603.3b two triggers under one controller ask for an order, exactly once" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          let (_, gs) = boardOf tidalWave khabalGhoul island
              (_, asked) = State.runState (Engine.runGame countingAnswer gs Engine.settleForPriority) 0
          Spec.assertEqWith s "asked once" asked 1
        -- THE ELISION, and its negative, one permanent apart. Dragon Fodder's
        -- single Create of two tokens is one event (CR 603.6a), so each board's
        -- watcher contributes two EQUAL entries under one controller.
        --
        -- Soul Warden's payload reads no slot, so both orders gain the same 2
        -- life and there is nothing to decide. The life assertion is what stops
        -- `asked == 0` passing because the triggers never fired.
        Spec.it s "CR 603.3b a batch of interchangeable triggers is not asked about (Soul Warden)" $ do
          soulWarden <- S.printingOf s registry "Soul Warden"
          mountain <- S.printingOf s registry "Mountain"
          dragonFodder <- S.printingOf s registry "Dragon Fodder"
          let (gs, fodderId) = watcherBoard soulWarden mountain 2 dragonFodder
              (after, asked) = castCounting gs fodderId
          Spec.assertEqWith s "not asked" asked 0
          Spec.assertEqWith s "and both triggers resolved: 1 life each" (S.lifeOf S.alice after) (fmap (+ 2) (S.lifeOf S.alice gs))
          Spec.assertEqWith s "with both goblins still standing" (length (S.tokensOf after)) 2
        -- Aether Flash is Warstorm Surge's shape and the reason entry equality
        -- alone is not the test: the two entries are equal, their bindings name
        -- different creatures, and CR 117.3b hands priority back between the two
        -- resolutions -- so which goblin was shot first is observable.
        Spec.it s "CR 117.3b the same-shaped batch IS asked about when the payload reads the entrant (Aether Flash)" $ do
          aetherFlash <- S.printingOf s registry "Aether Flash"
          mountain <- S.printingOf s registry "Mountain"
          dragonFodder <- S.printingOf s registry "Dragon Fodder"
          let (gs, fodderId) = watcherBoard aetherFlash mountain 2 dragonFodder
              (after, asked) = castCounting gs fodderId
          Spec.assertEqWith s "asked once" asked 1
          Spec.assertEqWith s "and both triggers resolved: 2 damage kills a 1/1 (CR 704.5g)" (S.tokensOf after) []
        -- The control: one entrant, so one trigger, which the older `length mine
        -- < 2` guard already elided. Without it, "Soul Warden asks nothing"
        -- cannot be told apart from "the batch was never two".
        Spec.it s "CR 603.3b one trigger is elided by count alone (Soul Warden, one token)" $ do
          soulWarden <- S.printingOf s registry "Soul Warden"
          island <- S.printingOf s registry "Island"
          tidalWave <- S.printingOf s registry "Tidal Wave"
          let (gs, waveId) = watcherBoard soulWarden island 3 tidalWave
              (after, asked) = castCounting gs waveId
          Spec.assertEqWith s "not asked" asked 0
          Spec.assertEqWith s "and the one trigger resolved" (S.lifeOf S.alice after) (fmap (+ 1) (S.lifeOf S.alice gs))
          Spec.assertEqWith s "off one token" (length (S.tokensOf after)) 1
        -- Sacrifice resolves FIRST: the Wall token dies, and CR 608.2h has the
        -- Ghoul count it when its own effect is applied. The token has NO printed
        -- card (CR 111.1) and its death happened at a boundary the scan already
        -- passed -- so a re-derived type line or a drained queue both read zero.
        --
        -- orderLast's argument is the source PUT LAST on the stack, i.e. the one
        -- that RESOLVES FIRST (CR 603.3b, see orderLast's own comment above): for
        -- the sacrifice to resolve first, the OTHER (non-Ghoul) trigger is the one
        -- named -- not the Ghoul itself.
        Spec.it s "CR 608.2h sacrificing first makes the Ghoul count the token" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          let (ghoul, gs) = boardOf tidalWave khabalGhoul island
              after = snd (Engine.runGamePure (orderLast (otherThan ghoul gs)) gs Engine.priorityLoop)
          Spec.assertEqWith s "the token was counted" (countersOn ghoul after) 1
        -- The Ghoul resolves FIRST: the token is still alive, so it is not
        -- counted. Same board, same cards, opposite answer -- which is what makes
        -- the ordering a genuine choice rather than a formality.
        Spec.it s "CR 608.2h counting first means the token is still alive and is not counted" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          let (ghoul, gs) = boardOf tidalWave khabalGhoul island
              after = snd (Engine.runGamePure (orderLast (TriggerSource.OfObject ghoul)) gs Engine.priorityLoop)
          Spec.assertEqWith s "nothing counted" (countersOn ghoul after) 0
        -- M-1 (review): permute's reject-not-repair guard, pinned directly. The
        -- centerpiece above only ever answers with a valid permutation (via
        -- orderLast/countingAnswer), and the canonical-answer tests elsewhere use
        -- the identity -- so nothing exercises the fallback branch. "Rejected"
        -- means the input list comes back verbatim: nothing dropped, nothing
        -- duplicated.
        Spec.it s "permute applies a genuine permutation" $ do
          Spec.assertEqWith s "reordered" (Game.permute "abc" [2, 1, 0]) "cba"
        Spec.it s "permute rejects a short answer, keeping the canonical order" $ do
          Spec.assertEqWith s "unchanged" (Game.permute "abc" [1, 0]) "abc"
        Spec.it s "permute rejects a duplicate index, keeping the canonical order" $ do
          Spec.assertEqWith s "unchanged" (Game.permute "abc" [0, 0, 1]) "abc"
        Spec.it s "permute rejects an out-of-range index, keeping the canonical order" $ do
          Spec.assertEqWith s "unchanged" (Game.permute "abc" [0, 1, 5]) "abc"
        -- M-2 (review): apnapPlayers rotates the turn order to start at the active
        -- player and filters to controllers with a pending trigger -- genuinely new
        -- behaviour versus M3f's apnapOrder, which never consulted turn order at
        -- all, and untested where two DIFFERENT players each control a trigger.
        -- Barbarian Outcast's state trigger (CR 603.8) needs no event, so one
        -- Outcast under EACH player, both controlling no Swamps, gives two
        -- controllers with one trigger apiece -- fewer than two each, so no
        -- ordering prompt is asked and the test isolates the cross-controller walk.
        Spec.it s "CR 101.4/603.3b the active player's trigger is placed first (bottom of stack)" $ do
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          let gs0 = Setup.emptyGame S.bothPlayers
              (_, gs1) = S.addCreature barbarianOutcast S.alice gs0
              (_, gs2) = S.addCreature barbarianOutcast S.bob gs1
              placed = snd (Engine.runGamePure S.identityAnswer gs2 Engine.placePendingTriggers)
              controllerOf oid = fmap Object.owner (Game.lookupObject oid placed)
              stack = GameState.stack placed
          case stack of
            [top, bottom] -> do
              Spec.assertEqWith s "the OTHER player's trigger is on top -- placed second" (controllerOf top) (Just S.bob)
              Spec.assertEqWith s "the active player's (alice's) trigger is at the bottom -- placed first" (controllerOf bottom) (Just S.alice)
            other -> Spec.assertFailure s ("expected exactly two triggers on the stack, got " <> show (length other))
        -- The same walk with a third seat and a departure. Barbarian Outcast's
        -- state trigger (CR 603.8) needs no event, so one Outcast under each of
        -- alice, bob and carol -- none controlling a Swamp -- gives three
        -- controllers with one trigger apiece. The two ordering assertions are
        -- the APNAP rotation itself: it starts at the active player and takes the
        -- seats still in the game, so carol's trigger is placed after alice's.
        -- Unobservable at two players, where a departure ends the game before any
        -- trigger is gathered.
        --
        -- Bob's trigger is absent for a different reason than it once was: CR
        -- 800.4a's first clause removes his Outcast with him, so the trigger
        -- never exists to be filtered. Engine.apnapPlayers still filters his seat
        -- out of the rotation -- see the still-playing filter there -- and the
        -- assertion on his Outcast below is what keeps this case honest about
        -- which rule did what.
        Spec.it s "CR 101.4/603.3b APNAP orders the two remaining players' triggers starting at the active player, and a departed seat's permanent is gone with it" $ do
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          let gs0 = Setup.emptyGame S.threePlayers
              (_, gs1) = S.addCreature barbarianOutcast S.alice gs0
              (bobsOutcast, gs2) = S.addCreature barbarianOutcast S.bob gs1
              (_, gs3) = S.addCreature barbarianOutcast S.carol gs2
              gone = S.departs Departure.Type.Conceded S.bob gs3
              placed = snd (Engine.runGamePure S.identityAnswer gone Engine.placePendingTriggers)
              controllerOf oid = fmap Object.owner (Game.lookupObject oid placed)
          Spec.assertBool s (Maybe.isJust (Game.lookupObject bobsOutcast gs3)) "the fixture really gave bob one"
          Spec.assertEqWith s "CR 800.4a: bob's Outcast left the game with him, so it has no trigger to place" (Game.lookupObject bobsOutcast gone) Nothing
          case GameState.stack placed of
            [top, bottom] -> do
              Spec.assertEqWith s "carol's trigger is on top -- placed second" (controllerOf top) (Just S.carol)
              Spec.assertEqWith s "the active player's (alice's) is at the bottom -- placed first" (controllerOf bottom) (Just S.alice)
            other -> Spec.assertFailure s ("expected exactly two triggers on the stack, got " <> show (length other))

-- CR 725.2: the monarch's two inherent abilities "have no source and are
-- controlled by the player who was the monarch at the time the abilities
-- triggered" -- triggered abilities in every other respect, so CR 603.3b's
-- own-order choice covers them exactly as it covers an object's trigger. The
-- collision is reachable from the pool: Palace Jailer crowns its controller, and
-- Khabál Ghoul triggers "at the beginning of each end step", so one player's end
-- step fires her Ghoul's trigger and the monarch's inherent draw in one batch.
monarchOrderingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
monarchOrderingSpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice)) (gs {GameState.phase = endStep})
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      settleWith :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      settleWith answer gs = S.runPure answer gs Engine.settleForPriority
      -- alice's Palace Jailer enters and crowns her (its first entry trigger,
      -- BecomeMonarch TheController) on top of whatever board the caller built;
      -- the state is then wound to the beginning of her end step. bob's Goblin
      -- Piker is the Jailer's SECOND entry trigger's exile victim -- without one
      -- that mode is unfillable and CR 603.3c would take the trigger back off the
      -- stack -- and alice's library holds the card the monarch's draw takes.
      crownAndEndStep palaceJailer piker base =
        let (_, gs1) = S.addCreature piker S.bob base
            (_, gs2) = S.addLibraryCard piker S.alice gs1
            (jailer, gs3) = S.addCreature palaceJailer S.alice gs2
            entered = ZoneChange.MkZoneChange jailer jailer Zone.Stack Zone.Battlefield
         in beginEndStep (resolveAll (S.withEvents [GameEvent.Moved (Moved.MkMoved entered (Projection.project jailer gs3))] gs3))
      -- Records every ordering payload's SOURCES, in order, answering
      -- canonically. The sources are what this group is about (CR 725.2's
      -- absent one beside a borne one); Pawl.KeywordTriggerSpec's battleCrySpec
      -- asserts on the ability half of the same entries.
      recordPayloads :: Prompt.Prompt r -> State.State [[TriggerSource.TriggerSource]] r
      recordPayloads p = case p of
        Prompt.OrderTriggers _ _ entries -> do
          State.modify' (<> [fmap TriggerEntry.source entries])
          pure (zipWith const [0 ..] entries)
        _ -> pure (S.identityAnswer p)
      -- Puts the SOURCELESS entry -- CR 725.2's inherent draw, named by what it
      -- is rather than by where it sits -- at the front of the permutation, so it
      -- goes on the stack FIRST and, the stack being LIFO, resolves LAST. That is
      -- the direction the old two-pass placement could not express: it appended
      -- the inherent trigger after the ordered batch, i.e. always on top, always
      -- resolving first.
      sourcelessFirst :: Prompt.Prompt r -> r
      sourcelessFirst p = case p of
        Prompt.OrderTriggers _ _ entries ->
          let indexed = zip [0 ..] entries
              pick keep = fmap fst (filter (\entry -> (TriggerEntry.source (snd entry) == TriggerSource.Sourceless) == keep) indexed)
           in pick True <> pick False
        _ -> S.identityAnswer p
      inherentController placed oid = case fmap Object.source (Game.lookupObject oid placed) of
        Just (Source.OfInherentTrigger inherent) -> Just (InherentTriggerSource.controller inherent)
        _ -> Nothing
      triggerSourceOf placed oid = case fmap Object.source (Game.lookupObject oid placed) of
        Just (Source.OfTrigger triggered) -> Just (TriggeredAbilitySource.source triggered)
        _ -> Nothing
   in Spec.describe s "MonarchTriggerOrdering" $ do
        -- The collision itself: both triggers reach ONE CR 603.3b choice.
        Spec.it s "CR 603.3b/725.2 the inherent end-step draw is offered in the same ordering choice as the Ghoul's trigger" $ do
          palaceJailer <- S.printingOf s registry "Palace Jailer"
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          piker <- S.printingOf s registry "Goblin Piker"
          let (ghoul, base) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
              gs = crownAndEndStep palaceJailer piker base
              (_, asked) = State.runState (Engine.runGame recordPayloads gs Engine.settleForPriority) []
          Spec.assertEqWith s "alice really holds the crown" (GameState.monarch gs) (Just S.alice)
          Spec.assertEqWith s "one ordering choice, offering the Ghoul's trigger and the sourceless inherent draw together" asked [[TriggerSource.OfObject ghoul, TriggerSource.Sourceless]]
        -- The order is HONOURED, in the direction the old two-pass placement
        -- could not reach: the inherent draw goes on the stack first, so it sits
        -- at the BOTTOM and resolves last.
        Spec.it s "CR 603.3b/725.2 putting the inherent draw on the stack first leaves it at the bottom, resolving last" $ do
          palaceJailer <- S.printingOf s registry "Palace Jailer"
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          piker <- S.printingOf s registry "Goblin Piker"
          let (ghoul, base) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
              gs = crownAndEndStep palaceJailer piker base
              placed = settleWith sourcelessFirst gs
          case GameState.stack placed of
            [top, bottom] -> do
              Spec.assertEqWith s "the inherent draw is at the bottom -- placed first, resolves last" (inherentController placed bottom) (Just S.alice)
              Spec.assertEqWith s "the Ghoul's trigger is on top -- placed second, resolves first" (triggerSourceOf placed top) (Just ghoul)
            other -> Spec.assertFailure s ("expected exactly two triggers on the stack, got " <> show (length other))
        -- Same board, opposite answer: the inherent draw goes on the stack
        -- last, so it is on top and resolves first. This is what the old engine
        -- forced unconditionally; here it is a choice.
        Spec.it s "CR 603.3b/725.2 putting the inherent draw on the stack last leaves it on top, resolving first" $ do
          palaceJailer <- S.printingOf s registry "Palace Jailer"
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          piker <- S.printingOf s registry "Goblin Piker"
          let (ghoul, base) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
              gs = crownAndEndStep palaceJailer piker base
              placed = settleWith S.identityAnswer gs
          case GameState.stack placed of
            [top, bottom] -> do
              Spec.assertEqWith s "the inherent draw is on top -- placed second, resolves first" (inherentController placed top) (Just S.alice)
              Spec.assertEqWith s "the Ghoul's trigger is at the bottom -- placed first, resolves last" (triggerSourceOf placed bottom) (Just ghoul)
            other -> Spec.assertFailure s ("expected exactly two triggers on the stack, got " <> show (length other))
        -- Both still resolve, whichever order was chosen: the merge must not
        -- lose the inherent trigger's placement, only relocate it.
        Spec.it s "CR 725.2 the monarch still draws when her own trigger is ordered last" $ do
          palaceJailer <- S.printingOf s registry "Palace Jailer"
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, base) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
              gs = crownAndEndStep palaceJailer piker base
              after = snd (Engine.runGamePure sourcelessFirst gs Engine.priorityLoop)
          Spec.assertEqWith s "alice drew the one card in her library" (length (Game.zoneMembers Zone.Hand S.alice after)) 1
        -- The companion elision: with only the inherent trigger in the batch
        -- there is nothing to order, and where the rules leave nothing to ask,
        -- don't prompt.
        Spec.it s "CR 603.3b the inherent draw alone is one trigger, so nothing is asked" $ do
          palaceJailer <- S.printingOf s registry "Palace Jailer"
          piker <- S.printingOf s registry "Goblin Piker"
          let gs = crownAndEndStep palaceJailer piker (Setup.emptyGame S.bothPlayers)
              (_, asked) = State.runState (Engine.runGame recordPayloads gs Engine.settleForPriority) []
              after = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
          Spec.assertEqWith s "alice really holds the crown" (GameState.monarch gs) (Just S.alice)
          Spec.assertEqWith s "no ordering choice was offered" asked []
          Spec.assertEqWith s "and she still drew" (length (Game.zoneMembers Zone.Hand S.alice after)) 1
        -- And the mirror: the Ghoul's trigger alone, with no monarch at all, is
        -- also one trigger and also asks nothing.
        Spec.it s "CR 603.3b the Ghoul's trigger alone, with no monarch, asks nothing" $ do
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          let (_, base) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
              gs = beginEndStep base
              (_, asked) = State.runState (Engine.runGame recordPayloads gs Engine.settleForPriority) []
          Spec.assertEqWith s "no monarch, so no inherent trigger exists" (GameState.monarch gs) Nothing
          Spec.assertEqWith s "no ordering choice was offered" asked []

-- Sarcomancy {B} Enchantment: "When this enchantment enters, create a 2/2 black
-- Zombie creature token. At the beginning of your upkeep, if there are no Zombies
-- on the battlefield, this enchantment deals 1 damage to you."
interveningSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
interveningSpec s registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      beginUpkeep gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice)) (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      zombies gs = filter (\oid -> Set.member Subtype.Zombie (Projection.subtypesOf oid gs)) (Set.toList (GameState.battlefield gs))
      -- Sarcomancy enters and its ETB resolves, so a Zombie token is out.
      withZombie sarcomancy =
        let (sarcId, gs0) = S.addCreature sarcomancy S.alice (Setup.emptyGame S.bothPlayers)
            entered = ZoneChange.MkZoneChange sarcId sarcId Zone.Stack Zone.Battlefield
            gs1 = S.withEvents [GameEvent.Moved (Moved.MkMoved entered (Projection.project sarcId gs0))] gs0
         in (sarcId, resolveAll (settle gs1))
   in Spec.describe s "InterveningIf" $ do
        Spec.it s "CR 603.6a the enters trigger makes a 2/2 black Zombie token" $ do
          sarcomancy <- S.printingOf s registry "Sarcomancy"
          let (_, after) = withZombie sarcomancy
          case zombies after of
            [tok] -> do
              Spec.assertEqWith s "2 power" (Projection.powerOf tok after) (Just 2)
              Spec.assertEqWith s "black" (Projection.colorsOf tok after) (Set.singleton Color.Black)
            other -> Spec.assertFailure s ("expected exactly one Zombie token, got " <> show (length other))
        -- CR 603.4: with the condition FALSE, the ability does not trigger AT ALL
        -- -- nothing reaches the stack.
        Spec.it s "CR 603.4 with a Zombie out, the upkeep ability does not trigger" $ do
          sarcomancy <- S.printingOf s registry "Sarcomancy"
          let (_, board) = withZombie sarcomancy
              atUpkeep = settle (beginUpkeep board)
          Spec.assertEqWith s "nothing on the stack" (GameState.stack atUpkeep) []
          Spec.assertEqWith s "no life lost" (S.lifeOf S.alice atUpkeep) (Just 20)
        Spec.it s "CR 603.4 with no Zombie, it triggers and deals 1 to its controller" $ do
          sarcomancy <- S.printingOf s registry "Sarcomancy"
          let (_, board) = withZombie sarcomancy
              killed = case zombies board of
                tok : _ -> S.settleSba (S.runPure S.identityAnswer board (Event.destroy Regenerability.Regenerable [tok]))
                [] -> board
              after = resolveAll (settle (beginUpkeep killed))
          Spec.assertEqWith s "alice took 1" (S.lifeOf S.alice after) (Just 19)
        -- CR 608.2a: the case that distinguishes an intervening "if" from a plain
        -- condition. The ability triggered legitimately; a Zombie appearing in
        -- RESPONSE makes it do nothing on resolution.
        Spec.it s "CR 608.2a a Zombie made in response makes the trigger resolve doing nothing" $ do
          sarcomancy <- S.printingOf s registry "Sarcomancy"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, board) = withZombie sarcomancy
              killed = case zombies board of
                tok : _ -> S.settleSba (S.runPure S.identityAnswer board (Event.destroy Regenerability.Regenerable [tok]))
                [] -> board
              onStack = settle (beginUpkeep killed)
              -- The Zombie arrives under BOB's control, which is exactly the
              -- point: CR 603.4's clause is "no Zombies on the battlefield",
              -- not "no Zombies you control".
              responded = snd (S.addToken (zombieTokenOf sarcomancy piker) S.bob onStack)
              after = resolveAll responded
          Spec.assertBool s (not (null (GameState.stack onStack))) "the trigger really was on the stack"
          Spec.assertEqWith s "no damage on resolution" (S.lifeOf S.alice after) (Just 20)

-- CR 603.4 / 303.4b: an intervening "if" that reads the SOURCE's own host, which is
-- Filter.IsHostOfSource in the position Pawl.Engine.Event.interveningHolds answers.
--
-- Ray of Frost ({1}{U} Enchantment -- Aura, "Flash / Enchant creature / When this
-- Aura enters, if enchanted creature is red, tap it. / As long as enchanted
-- creature is red, it loses all abilities. / Enchanted creature doesn't untap
-- during its controller's untap step.", checked against Scryfall 2026-08-17). Its
-- first sentence is the only intervening "if" in the pool about an attachment, and
-- its "tap it" is the only effect naming a host.
--
-- Bird Maiden ({2}{R} Creature -- Human Bird 1/2) and Aven Squire ({1}{W} Creature
-- -- Bird Soldier 1/1) are the two hosts, so the two boards differ in the host's
-- COLOUR and in nothing else.
enchantedHostTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
enchantedHostTriggerSpec s registry =
  let settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      tapStateOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)
      -- The Aura on the battlefield attached to `host`, with the entry event CR
      -- 603.6a's scan reads -- Sarcomancy's fixture above with an attachment, which
      -- is what the clause is about.
      entering ray host gs0 =
        let (rayId, gs1) = S.addCreature ray S.alice gs0
            attached = S.attach rayId host gs1
            entered = ZoneChange.MkZoneChange rayId rayId Zone.Stack Zone.Battlefield
         in S.withEvents [GameEvent.Moved (Moved.MkMoved entered (Projection.project rayId attached))] attached
   in Spec.describe s "EnchantedHostTrigger" $ do
        Spec.it s "CR 603.4 the clause holds on a RED host, so the trigger taps it" $ do
          ray <- S.printingOf s registry "Ray of Frost"
          maiden <- S.printingOf s registry "Bird Maiden"
          let (maidenId, board) = S.addCreature maiden S.alice (Setup.emptyGame S.bothPlayers)
              onStack = settle (entering ray maidenId board)
              after = resolveAll onStack
          Spec.assertEqWith s "the ability triggered" (length (GameState.stack onStack)) 1
          Spec.assertEqWith s "and tapped the creature it enchants" (tapStateOf maidenId after) (Just TapState.Tapped)
        -- THE NEGATIVE, asserted on the STACK rather than on the tap state: a
        -- trigger that reached the stack and then found the host untappable would
        -- leave the same board behind, and CR 608.2a's re-check would mask it.
        Spec.it s "CR 603.4 the clause fails on a WHITE host, so nothing triggers" $ do
          ray <- S.printingOf s registry "Ray of Frost"
          squire <- S.printingOf s registry "Aven Squire"
          let (squireId, board) = S.addCreature squire S.alice (Setup.emptyGame S.bothPlayers)
              onStack = settle (entering ray squireId board)
              after = resolveAll onStack
          Spec.assertEqWith s "nothing reached the stack" (length (GameState.stack onStack)) 0
          Spec.assertEqWith s "and the creature is untapped" (tapStateOf squireId after) (Just TapState.Untapped)
        -- The atom rather than the colour: a red creature is on the board in BOTH
        -- readings, and only the one the Aura enchants can decide the clause. An
        -- IsHostOfSource that matched every candidate would tap here too.
        Spec.it s "CR 303.4b a red creature the Aura does NOT enchant decides nothing" $ do
          ray <- S.printingOf s registry "Ray of Frost"
          maiden <- S.printingOf s registry "Bird Maiden"
          squire <- S.printingOf s registry "Aven Squire"
          let (maidenId, g1) = S.addCreature maiden S.alice (Setup.emptyGame S.bothPlayers)
              (squireId, board) = S.addCreature squire S.alice g1
              onStack = settle (entering ray squireId board)
              after = resolveAll onStack
          Spec.assertEqWith s "nothing reached the stack" (length (GameState.stack onStack)) 0
          Spec.assertEqWith s "neither creature is tapped" (tapStateOf squireId after, tapStateOf maidenId after) (Just TapState.Untapped, Just TapState.Untapped)

-- The 2/2 black Zombie Sarcomancy's own ETB mints, read back out of the card data
-- so the "in response" fixture makes the same object the card would.
zombieTokenOf :: Printing.Printing -> Printing.Printing -> Card.Type.Card
zombieTokenOf sarcomancy pikerFallback =
  let created effect = case effect of
        Effect.Create (Create.MkCreate _ card _ _ _) -> Just card
        _ -> Nothing
      abilityEffects = concatMap (Modal.allEffects . TriggeredAbility.modal) (Face.triggeredAbilities (S.combinedFace sarcomancy))
   in case Maybe.mapMaybe created abilityEffects of
        card : _ -> card
        [] -> Printing.card pikerFallback

-- The Ten Rings {8} Legendary Artifact: "Your maximum hand size is ten. At the
-- beginning of your end step, if you have fewer than ten cards in hand, draw
-- cards equal to the difference." The maximum-hand-size half is
-- Pawl.PlayerEffectSpec's; this is the second line.
--
-- The pool's first card whose DRAW COUNT is a subtraction -- Plus of Literal 10
-- and Negate of the hand count -- and its first Comparison.AtMost, CR 603.4's
-- "fewer than ten" being a hand count of at most nine.
--
-- Four cards held on the drawing board, so three wrong readings of "the
-- difference" miss ten and are caught: a flat draw of ten ends on fourteen, a
-- draw of the hand size ends on eight, and a draw of one ends on five. The
-- boards differ in the HAND alone -- same seats, same permanent, same library.
tenRingsDrawSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
tenRingsDrawSpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice)) (gs {GameState.phase = endStep})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      stockHand n printing gs = List.foldl' (\g _ -> snd (S.addHandCard printing S.alice g)) gs [1 .. n :: Int]
      -- alice with The Ten Rings out, `held` Plains in hand and twenty more in
      -- her library. The library is stocked well past the six the positive board
      -- draws, so CR 104.3c decks nobody before the assertion runs.
      board tenRings plains held =
        let (_, gs0) = S.addCreature tenRings S.alice (Setup.emptyGame S.bothPlayers)
         in List.foldl' (\g _ -> snd (S.addLibraryCard plains S.alice g)) (stockHand held plains gs0) [1 .. 20 :: Int]
   in Spec.describe s "TheTenRingsDraw" $ do
        Spec.it s "CR 603.2b/121.1 four cards in hand draws the difference, six, to reach ten" $ do
          tenRings <- S.printingOf s registry "The Ten Rings"
          plains <- S.printingOf s registry "Plains"
          let atEnd = settle (beginEndStep (board tenRings plains 4))
          Spec.assertEqWith s "the ability triggered" (length (GameState.stack atEnd)) 1
          Spec.assertEqWith s "and drew up to ten" (S.handSize S.alice (resolveAll atEnd)) 10
        -- THE NEGATIVE, the same board with only the hand changed. Ten cards is
        -- not "fewer than ten", so CR 603.4 keeps the ability off the stack
        -- entirely. Asserted on the STACK rather than on the hand, because a
        -- threshold read one too high would trigger and then draw a difference of
        -- zero -- which no hand size can tell from not triggering at all.
        Spec.it s "CR 603.4 ten cards in hand is not fewer than ten, so nothing triggers" $ do
          tenRings <- S.printingOf s registry "The Ten Rings"
          plains <- S.printingOf s registry "Plains"
          let atEnd = settle (beginEndStep (board tenRings plains 10))
          Spec.assertEqWith s "nothing on the stack" (length (GameState.stack atEnd)) 0
          Spec.assertEqWith s "and the hand is untouched" (S.handSize S.alice (resolveAll atEnd)) 10
        -- CR 608.2h: "the difference" is information the effect requires, so it is
        -- determined once, as the effect is APPLIED rather than as the ability
        -- triggered. Three cards arrive while it waits on the stack, so the draw is
        -- three rather than the six a trigger-time hand of four would give -- ten
        -- either way is impossible, since that reading ends on thirteen.
        Spec.it s "CR 608.2h cards gained in response shrink the draw, which is read on resolution" $ do
          tenRings <- S.printingOf s registry "The Ten Rings"
          plains <- S.printingOf s registry "Plains"
          let atEnd = settle (beginEndStep (board tenRings plains 4))
              responded = stockHand 3 plains atEnd
          Spec.assertEqWith s "the ability really is waiting on the stack" (length (GameState.stack atEnd)) 1
          Spec.assertEqWith s "seven held when it resolves, so three drawn" (S.handSize S.alice (resolveAll responded)) 10

-- CR 603.6a's SECOND written form -- "Whenever a [type] enters, . . ." -- and
-- Soul Warden {W} Creature -- Human Cleric 1/1, "Whenever another creature
-- enters, you gain 1 life", the card that proves it. Its effect names nothing
-- about the entering creature, so these cases isolate the trigger CONDITION;
-- its "another" is Filter.Not Filter.IsSource inside the condition's own
-- Filter, never a second exclusion mechanism (#163).
permanentEntersSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
permanentEntersSpec s registry =
  let anyCreature = Filter.Type.HasCardType CardType.Creature
      anotherCreature = Filter.Type.And [anyCreature, Filter.Type.Not Filter.Type.IsSource]
      enters oid = GameEvent.Moved (Moved.MkMoved (ZoneChange.MkZoneChange oid oid Zone.Stack Zone.Battlefield) S.emptyCharacteristics)
      sourcesOf gs = fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped gs) gs))
   in Spec.describe s "PermanentEnters" $ do
        -- The gameplay-level proof, cast to resolution: alice's second Soul
        -- Warden enters and the FIRST one's trigger resolves for exactly 1
        -- life. Exactly one life, not two, is the "another" falsifier -- the
        -- newcomer is checked against its own entry (the case below proves
        -- the scan does check it) and its own Filter is what declines.
        Spec.it s "CR 603.6a whole cards: a second Soul Warden entering gains alice exactly 1 life" $ do
          plains <- S.printingOf s registry "Plains"
          soulWarden <- S.printingOf s registry "Soul Warden"
          let (_, base) = S.addCreature soulWarden S.alice (S.landsInPlay plains 1)
              (gs, spellId) = S.handOne soulWarden base
              cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
              settled = snd (Engine.runGamePure S.identityAnswer cast Engine.priorityLoop)
          Spec.assertEqWith s "both Wardens are on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Soul Warden") S.alice settled) 2
          Spec.assertEqWith s "alice gained exactly 1" (S.lifeOf S.alice settled) (fmap (+ 1) (S.lifeOf S.alice gs))
          Spec.assertEqWith s "bob gained nothing" (S.lifeOf S.bob settled) (S.lifeOf S.bob gs)
        -- CR 603.6a: "all permanents on the battlefield (INCLUDING THE
        -- NEWCOMERS) are checked for any enters-the-battlefield triggers that
        -- match the event." The newcomer really is offered its own entry; a
        -- bare "a creature enters" admits it, and only Soul Warden's printed
        -- "another" turns it away. This is why the constructor is not called
        -- OtherEnters.
        Spec.it s "CR 603.6a including the newcomers: a permanent is checked against its own entry" $ do
          soulWarden <- S.printingOf s registry "Soul Warden"
          let (oid, gs) = S.addCreature soulWarden S.alice (Setup.emptyGame S.bothPlayers)
          Spec.assertBool s (Event.matchesTrigger gs oid S.alice (TriggerCondition.PermanentEnters anyCreature) (enters oid)) "\"a creature enters\" admits the newcomer itself"
          Spec.assertBool s (not (Event.matchesTrigger gs oid S.alice (TriggerCondition.PermanentEnters anotherCreature) (enters oid))) "\"another creature enters\" does not"
        -- The live-reading falsifier. `enters` above hands the event a
        -- deliberately EMPTY ProjectedCharacteristics -- no card types at all
        -- -- because that snapshot is CR 608.2h last known information for the
        -- zone the object LEFT, and matching against it would answer CR 603.6b
        -- backwards: "continuous effects that modify characteristics of a
        -- permanent do so the moment the permanent is on the battlefield (and
        -- not before then)". CR 603.10 says the same of an event's objects.
        -- The Piker is a Creature only in the live projection, so a matcher
        -- reading the snapshot fires nothing here.
        Spec.it s "CR 603.6b/603.10 the entrant is read live, not from the Moved event's snapshot" $ do
          soulWarden <- S.printingOf s registry "Soul Warden"
          piker <- S.printingOf s registry "Goblin Piker"
          let (warden, gs0) = S.addCreature soulWarden S.alice (Setup.emptyGame S.bothPlayers)
              (pikerId, gs1) = S.addCreature piker S.bob gs0
          Spec.assertEqWith s "no card types in the snapshot" (PC.cardTypes S.emptyCharacteristics) Set.empty
          Spec.assertBool s (Event.matchesTrigger gs1 warden S.alice (TriggerCondition.PermanentEnters anotherCreature) (enters pikerId)) "and the trigger still fires"
        -- CR 608.2h: an entrant that is already gone by the CR 117.5 boundary
        -- -- here moved straight on to the graveyard -- is read from last known
        -- information, which for a permanent that left the battlefield is the
        -- battlefield reading. The event happened; the trigger is not lost with
        -- the object. Same fallback eventTriggers' own group-scoped unions take for
        -- the bearer side.
        Spec.it s "CR 608.2h a creature that enters and leaves again still fires the trigger" $ do
          soulWarden <- S.printingOf s registry "Soul Warden"
          piker <- S.printingOf s registry "Goblin Piker"
          let (warden, gs0) = S.addCreature soulWarden S.alice (Setup.emptyGame S.bothPlayers)
              (handCard, gs1) = S.addHandCard piker S.bob gs0
              entered = S.runPure S.identityAnswer gs1 (Event.changeZone handCard Zone.Battlefield)
              newIds = fmap ZoneChange.object (filter ((==) Zone.Battlefield . ZoneChange.to) (S.zoneChangesOf entered))
          case newIds of
            [newId] -> do
              let gone = S.runPure S.identityAnswer entered (Event.changeZone newId Zone.Graveyard)
              Spec.assertEqWith s "the entrant is off the battlefield" (Game.lookupObject newId gone) Nothing
              Spec.assertEqWith s "the Warden still triggered, once" (sourcesOf gone) [TriggerSource.OfObject warden]
            other -> Spec.assertFailure s ("expected exactly one battlefield entry, got " <> show (length other))
        -- The type half of the Filter: a LAND entering is not a creature
        -- entering. Plains has no ability of its own, so nothing else can
        -- stand in for the silence.
        Spec.it s "CR 603.6a a noncreature permanent entering fires nothing" $ do
          soulWarden <- S.printingOf s registry "Soul Warden"
          plains <- S.printingOf s registry "Plains"
          let (_, gs0) = S.addCreature soulWarden S.alice (Setup.emptyGame S.bothPlayers)
              (landId, gs1) = S.addCreature plains S.alice gs0
              gs2 = S.withEvents [GameEvent.Moved (Moved.MkMoved (ZoneChange.MkZoneChange landId landId Zone.Stack Zone.Battlefield) (Projection.project landId gs1))] gs1
          Spec.assertEqWith s "no trigger" (sourcesOf gs2) []
        -- The destination half: CR 603.6a is an ENTERS-THE-BATTLEFIELD
        -- ability, so a creature card moving to a graveyard is not it.
        Spec.it s "CR 603.6a only a battlefield destination fires it" $ do
          soulWarden <- S.printingOf s registry "Soul Warden"
          piker <- S.printingOf s registry "Goblin Piker"
          let (warden, gs0) = S.addCreature soulWarden S.alice (Setup.emptyGame S.bothPlayers)
              (pikerId, gs1) = S.addCreature piker S.bob gs0
              toGrave = GameEvent.Moved (Moved.MkMoved (ZoneChange.MkZoneChange pikerId pikerId Zone.Battlefield Zone.Graveyard) S.emptyCharacteristics)
          Spec.assertBool s (not (Event.matchesTrigger gs1 warden S.alice (TriggerCondition.PermanentEnters anotherCreature) toGrave)) "a graveyard-bound move does not match"
        -- CR 603.6a: "EACH TIME an event puts one or more permanents onto the
        -- battlefield" -- one bearer, two entering creatures, two triggers. A
        -- count, not a boolean, so "fires once per entrant" is distinguishable
        -- from "fires once per batch".
        Spec.it s "CR 603.6a one Soul Warden fires once per entering creature" $ do
          soulWarden <- S.printingOf s registry "Soul Warden"
          piker <- S.printingOf s registry "Goblin Piker"
          let (warden, gs0) = S.addCreature soulWarden S.alice (Setup.emptyGame S.bothPlayers)
              (first, gs1) = S.addCreature piker S.bob gs0
              (second, gs2) = S.addCreature piker S.bob gs1
              gs3 =
                S.withEvents
                  [ GameEvent.Moved (Moved.MkMoved (ZoneChange.MkZoneChange first first Zone.Stack Zone.Battlefield) (Projection.project first gs2)),
                    GameEvent.Moved (Moved.MkMoved (ZoneChange.MkZoneChange second second Zone.Stack Zone.Battlefield) (Projection.project second gs2))
                  ]
                  gs2
          Spec.assertEqWith s "twice, both from the one Warden" (sourcesOf gs3) (replicate 2 (TriggerSource.OfObject warden))

-- CR 603.7's arming gate, through Meandering Towershell -- the pool's one card
-- whose delayed ability is printed "on your NEXT turn" (Pawl.Types.Onset).
--
-- The gate is NOT vacuous, and this group's whole point is to prove it. The
-- ability's own condition is StepBegins (Combat DeclareAttackers)
-- ControllersTurn, and a TurnScope cannot tell one of the controller's turns
-- from another -- so an extra combat phase in the SAME turn (Relentless Assault,
-- and its siblings Aggravated Assault, Full Throttle and Aurelia) has a declare
-- attackers step that begins on alice's turn and would fire the return early.
towershellOnsetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
towershellOnsetSpec s registry = Spec.describe s "DelayedOnset" $ do
  let boardOf = do
        towershell <- S.printingOf s registry "Meandering Towershell"
        mountain <- S.printingOf s registry "Mountain"
        island <- S.printingOf s registry "Island"
        assault <- S.printingOf s registry "Relentless Assault"
        pure (towershellAssaultBoard towershell mountain island assault)
      towershellName = CardName.MkCardName $ Text.pack "Meandering Towershell"
  Spec.it s "CR 603.7 a second declare attackers step THIS turn does not fire it" $ do
    (gs, spell) <- boardOf
    let -- alice attacks with the Towershell; its trigger exiles it and arms the
        -- return, gated to a later turn.
        atMain = runToTurnStep 1 Phase.PostcombatMain S.aggressiveAnswer gs
        armed = GameState.delayedTriggers atMain
        -- "After this main phase, there is an additional combat phase followed
        -- by an additional main phase" (CR 500.8).
        cast = snd (Engine.runGamePure S.identityAnswer atMain (S.cast S.alice spell))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        atExtra = runToTurnStep 1 (Phase.Combat CombatStep.DeclareAttackers) S.aggressiveAnswer resolved
        afterExtra = snd (Engine.runGamePure S.aggressiveAnswer atExtra Engine.runStep)
    Spec.assertEqWith s "the return is armed" (length armed) 1
    -- Waiting for a BOUNDARY, not for a turn number: which turn alice's next one
    -- is cannot be known here (Pawl.Types.TurnWindow), and this same turn is not
    -- it whatever number it carries.
    Spec.assertEqWith
      s
      "and gated to alice's next turn, which has not begun"
      (fmap DelayedTrigger.window (Foldable.toList armed))
      [TurnWindow.ControllersNextTurn]
    Spec.assertEqWith s "the extra combat's declare attackers step really happened" (GameState.phase atExtra) (Phase.Combat CombatStep.DeclareAttackers)
    Spec.assertEqWith s "it is still in exile" (S.countOnBattlefieldByName towershellName S.alice afterExtra) 0
    Spec.assertEqWith s "and still armed, unspent" (length (GameState.delayedTriggers afterExtra)) 1
  -- The control that stops the case above from passing for the wrong reason: a
  -- gate that never opened would satisfy every assertion in it. The SAME line of
  -- play -- extra combat phase included -- returns the Towershell on alice's
  -- next turn.
  Spec.it s "CR 603.7b and the same line of play returns it on alice's next turn" $ do
    (gs, spell) <- boardOf
    let atMain = runToTurnStep 1 Phase.PostcombatMain S.aggressiveAnswer gs
        cast = snd (Engine.runGamePure S.identityAnswer atMain (S.cast S.alice spell))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        atNextTurn = runToTurnStep 3 (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer resolved
    Spec.assertEqWith s "alice's next turn is turn 3" (GameState.turnNumber atNextTurn) 3
    Spec.assertEqWith s "and there it does return" (S.countOnBattlefieldByName towershellName S.alice atNextTurn) 1
    Spec.assertEqWith s "spending the one shot (CR 603.7b)" (length (GameState.delayedTriggers atNextTurn)) 0

-- CR 603.7a's last sentence -- "Other events that happen earlier may make the
-- trigger event impossible" -- with both halves already in the pool: Meandering
-- Towershell's return, armed for alice's NEXT turn, and Stonehorn Dignitary
-- taking that very turn's combat phase away before it arrives.
--
-- The printed phrase names ONE turn. A gate that only said "not before turn n"
-- would leave the entry armed past it and fire the return on the FOLLOWING turn
-- of alice's -- a turn the card does not name -- which is what the second case
-- here discriminates.
towershellSkipSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
towershellSkipSpec s registry = Spec.describe s "DelayedOnsetSkipped" $ do
  let boardOf = do
        towershell <- S.printingOf s registry "Meandering Towershell"
        island <- S.printingOf s registry "Island"
        plains <- S.printingOf s registry "Plains"
        stonehorn <- S.printingOf s registry "Stonehorn Dignitary"
        pure (towershellStonehornBoard towershell island plains stonehorn)
      towershellName = CardName.MkCardName $ Text.pack "Meandering Towershell"
      -- CR 603.2b: the combat steps of pid's turn that actually BEGAN. A skipped
      -- phase leaves none, which is CR 614.6's "if an event is replaced, it never
      -- happens" -- and is why this is read before the turn ends, since
      -- Engine.handoffTurn clears the log.
      combatStepsOf pid gs =
        [ph | GameEvent.StepBegan (StepBegan.MkStepBegan ph@(Phase.Combat _) who) <- S.eventsOf gs, who == pid]
  -- The control that makes the case below discriminating: the same board and the
  -- same line of play with the Dignitary never cast. S.aggressiveAnswer takes no
  -- action at all, and S.fightAnswer is exactly it plus casting what it can
  -- afford -- so the ONE difference between the two runs is that combat phase.
  Spec.it s "CR 603.7 with alice's next turn intact the Towershell returns on it" $ do
    gs <- boardOf
    let turn3 = runToTurnStep 3 Phase.PostcombatMain S.aggressiveAnswer gs
    -- All five of CR 506.1's steps, because the returning Towershell is "put
    -- onto the battlefield attacking" -- CR 508.8's other half, which is what
    -- keeps the declare blockers and combat damage steps from being skipped.
    Spec.assertEqWith s "alice's turn 3 ran all five combat steps (CR 506.1)" (length (combatStepsOf S.alice turn3)) 5
    Spec.assertEqWith s "and the Towershell returned there" (S.countOnBattlefieldByName towershellName S.alice turn3) 1
    Spec.assertEqWith s "spending the one shot (CR 603.7b)" (length (GameState.delayedTriggers turn3)) 0
  -- THE PROVING CASE. bob's Dignitary, cast on turn 2, takes alice's turn-3
  -- combat phase away -- so the declare attackers step the return watches for
  -- never happens on the one turn "your next turn" named, and CR 603.7a's "other
  -- events that happen earlier may make the trigger event impossible" is the
  -- whole of what follows: the ability never triggers.
  Spec.it s "CR 603.7a a skipped combat phase on the named turn makes the return impossible, not late" $ do
    gs <- boardOf
    let turn3 = runToTurnStep 3 Phase.PostcombatMain S.fightAnswer gs
        turn5 = runToTurnStep 5 Phase.PostcombatMain S.fightAnswer turn3
    Spec.assertEqWith s "no combat step of alice's turn 3 began (CR 500.11)" (combatStepsOf S.alice turn3) []
    Spec.assertEqWith s "so the Towershell did not return on the turn its ability named" (S.countOnBattlefieldByName towershellName S.alice turn3) 0
    -- CR 614.10a spends the skip on ONE occurrence, so alice's turn 5 has a
    -- combat phase of its own -- and with it the very step the return watches
    -- for. That is what makes the assertions below about the ABILITY rather than
    -- about the board: the trigger event does occur here, and an entry left armed
    -- would fire on it and return the Towershell on a turn the card never named.
    -- Not all five steps: alice declares no attackers, so CR 508.8 skips the
    -- declare blockers and combat damage steps.
    Spec.assertBool
      s
      (List.elem (Phase.Combat CombatStep.DeclareAttackers) (combatStepsOf S.alice turn5))
      "alice's turn 5 declare attackers step happens (CR 614.10a)"
    Spec.assertEqWith s "and the return never fires at all" (S.countOnBattlefieldByName towershellName S.alice turn5) 0
    Spec.assertEqWith s "nor is it left in the store forever" (length (GameState.delayedTriggers turn5)) 0

-- alice at her declare attackers step on turn 1 with one Meandering Towershell;
-- bob has four untapped Plains (exactly Stonehorn Dignitary's {3}{W}) and the
-- Dignitary in hand, and nothing else. Both libraries hold Islands so the draw
-- steps of the five turns these cases run through cannot empty one (CR 104.3c).
towershellStonehornBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  GameState.GameState
towershellStonehornBoard towershell island plains stonehorn =
  let (base, _, _) = S.combatBoardOf [towershell] []
      withLands = List.foldl' (\g _ -> snd (S.addCreature plains S.bob g)) base [1 :: Int .. 4]
      stock pid g = List.foldl' (\h _ -> snd (S.addLibraryCard island pid h)) g [1 :: Int .. 8]
   in snd (S.addHandCard stonehorn S.bob (stock S.bob (stock S.alice withLands)))

-- alice at her declare attackers step with one Meandering Towershell, four
-- untapped Mountains (exactly Relentless Assault's {2}{R}{R}) and the Assault in
-- hand; bob defends. Both libraries hold Islands so the draw steps of the turns
-- these cases run through cannot empty one (CR 104.3c).
--
-- The Islands are in LIBRARIES and never on the battlefield, which matters for
-- this card: CR 702.14c's islandwalk reads the lands the DEFENDING PLAYER
-- CONTROLS, and a library is not the battlefield -- so nothing here turns on
-- evasion.
towershellAssaultBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, ObjectId.ObjectId)
towershellAssaultBoard towershell mountain island assault =
  let (base, _, _) = S.combatBoardOf [towershell] []
      withLands = List.foldl' (\g _ -> snd (S.addCreature mountain S.alice g)) base [1 :: Int .. 4]
      stock pid g = List.foldl' (\h _ -> snd (S.addLibraryCard island pid h)) g [1 :: Int .. 8]
      (withCard, spell) = S.handOne assault (stock S.bob (stock S.alice withLands))
   in ( withCard
          { GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice,
            GameState.phase = GameState.phase base,
            GameState.combat = GameState.combat base,
            GameState.remaining = GameState.remaining base
          },
        spell
      )

-- Run whole steps until the board is at `phase` on turn `turn`, WITHOUT running
-- that step. Bounded so a bug cannot loop forever; stops on a finished game.
-- Pawl.CombatSpec has the same helper for the same card, kept local to each
-- group per the suite's convention.
runToTurnStep :: Natural -> Phase.Phase -> (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runToTurnStep turn phase answer gs0 =
  let go n g =
        if n <= (0 :: Int)
          || Maybe.isJust (GameState.result g)
          || (GameState.turnNumber g == turn && GameState.phase g == phase)
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 64 gs0

-- CR 603.3b's TWO-PART placement, verbatim: "First, each player, in APNAP order,
-- puts each triggered ability they control with a trigger condition that isn't
-- another ability triggering on the stack in any order they choose. ... Second,
-- each player, in APNAP order, puts all remaining triggered abilities they
-- control on the stack in any order they choose."
--
-- The pool's one producer of the second class, and the only printed card of the
-- shape: Historian's Boon, "{3}{W} Enchantment -- Whenever the final chapter
-- ability of a Saga you control triggers, create a 4/4 white Angel creature token
-- with flying and vigilance", paired with History of Benalia.
--
-- Two lore counters on the Saga and alice's precombat main phase: CR 714.3c's
-- turn-based action puts the third on, chapter III triggers (CR 714.2b), and the
-- Boon's second ability triggers off THAT triggering. Chapter III is first-class
-- and the Boon second-class, so the Boon goes on the stack above it and resolves
-- first, whatever order alice would have preferred.
--
-- WHAT THIS ASSERTS AND WHAT IT DOES NOT. The resulting BOARD is the same either
-- way -- chapter III pumps Knights alice controls and the Boon's token is an
-- Angel -- so the assertion is on stack order and on what alice was asked, both
-- of which a player sees, and not on a board difference. A Saga whose final
-- chapter read "creatures you control" would give a stronger discriminator; none
-- is in the pool.
--
-- `orderReversing` is what keeps the stack assertion from passing for the wrong
-- reason. Under a single APNAP pass alice controls both triggers and IS asked,
-- and this answerer names chapter III LAST -- which puts it on TOP and makes it
-- resolve first, the exact inversion. Under the rule's two passes she controls
-- one trigger in each and is asked nothing at all, so the answerer is inert. That
-- is why the recorded prompt list is expected EMPTY rather than merely free of
-- mixed batches: CR 603.3b's own-order choice is offered inside a pass.
secondPlacementPassSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
secondPlacementPassSpec s registry =
  let boardOf benalia boon =
        let (sagaId, base) = S.addCreature benalia S.alice (Setup.emptyGame S.bothPlayers)
            (boonId, withBoon) = S.addCreature boon S.alice base
            withCounters = S.addCounter CounterKind.Lore 2 sagaId withBoon
            ready =
              withCounters
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
         in (sagaId, boonId, S.runPure S.identityAnswer ready (Engine.runTurnBasedActions Phase.PrecombatMain))
      -- Answers CR 603.3b's ordering prompt with the offered indices REVERSED --
      -- the canonical order names the chapter ability first, so this names it
      -- last, and the answer is the order the triggers are PUT on the stack --
      -- while recording every batch it was offered.
      orderReversing :: Prompt.Prompt r -> State.State [[TriggerEntry.TriggerEntry]] r
      orderReversing p = case p of
        Prompt.OrderTriggers _ _ entries -> do
          State.modify' (<> [entries])
          pure (reverse (zipWith const [0 ..] entries))
        _ -> pure (S.identityAnswer p)
      -- The stack from the TOP down, named by each triggered ability's source (CR
      -- 113.7). Anything on the stack that is not a triggered ability is dropped,
      -- and nothing here puts one there.
      triggerSourcesOnStack gs =
        Maybe.mapMaybe
          ( \sid -> case fmap Object.source (Game.lookupObject sid gs) of
              Just (Source.OfTrigger triggered) -> Just (TriggeredAbilitySource.source triggered)
              _ -> Nothing
          )
          (GameState.stack gs)
      angelToken = CardName.MkCardName (Text.pack "Angel Token")
   in Spec.describe s "SecondPlacementPass" $ do
        Spec.it s "CR 603.3b the ability triggered by another ability's triggering is placed SECOND, so it resolves first" $ do
          benalia <- S.printingOf s registry "History of Benalia"
          boon <- S.printingOf s registry "Historian's Boon"
          let (sagaId, boonId, advanced) = boardOf benalia boon
              ((_, settled), asked) = State.runState (Engine.runGame orderReversing advanced Engine.settleForPriority) []
          Spec.assertEqWith s "the turn-based action put the third lore counter on" (S.counterOf CounterKind.Lore sagaId advanced) 3
          -- Both triggered, and the Boon's is ABOVE chapter III -- the whole of CR
          -- 603.3b's second pass, read off the stack the player sees.
          Spec.assertEqWith s "the Boon's trigger sits on top of the Saga's chapter III" (triggerSourcesOnStack settled) [boonId, sagaId]
          -- Not merely "an ability of the Saga": the one underneath must be the
          -- FINAL chapter, since that is what the Boon's condition names.
          Spec.assertEqWith s "and the one underneath is chapter III" (chaptersOnStackFrom sagaId settled) [3]
          -- The secondary check, and secondary because it can hold vacuously: with
          -- one trigger in each pass alice has nothing to order, so she is asked
          -- nothing. Under a single pass she is asked once, with both entries.
          Spec.assertEqWith s "alice was never offered the two abilities in one ordering" (fmap length asked) []
        Spec.it s "CR 603.3b the Boon's Angel really arrives, and CR 704.5s still takes the Saga" $ do
          benalia <- S.printingOf s registry "History of Benalia"
          boon <- S.printingOf s registry "Historian's Boon"
          let (sagaId, _, advanced) = boardOf benalia boon
              ((_, resolved), _) = State.runState (Engine.runGame orderReversing advanced Engine.priorityLoop) []
          Spec.assertEqWith s "one Angel token" (S.countOnBattlefieldByName angelToken S.alice resolved) 1
          -- The whole printed payload, so the card's data is exercised and not
          -- merely its name: "a 4/4 white Angel creature token with flying and
          -- vigilance". Chapter III creates nothing, so the one token is it.
          case S.tokensOf resolved of
            [token] -> do
              Spec.assertEqWith s "4/4" (S.powerToughnessOf token resolved) (Just (4, 4))
              Spec.assertEqWith s "an Angel" (Projection.subtypesOf token resolved) (Set.singleton Subtype.Angel)
              Spec.assertBool s (Map.member Keyword.Type.Flying (Projection.keywordsOf token resolved)) "with flying"
              Spec.assertBool s (Map.member Keyword.Type.Vigilance (Projection.keywordsOf token resolved)) "and vigilance"
            other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
          Spec.assertBool s (not (S.onBattlefield sagaId resolved)) "and the Saga's story is told, so CR 704.5s sacrifices it"

-- The chapter numbers of `oid`'s own chapter abilities currently on the stack (CR
-- 714.2, CR 704.5s).
chaptersOnStackFrom :: ObjectId.ObjectId -> GameState.GameState -> [Natural]
chaptersOnStackFrom oid gs =
  let from sid = case fmap Object.source (Game.lookupObject sid gs) of
        Just (Source.OfTrigger triggered) | TriggeredAbilitySource.source triggered == oid -> Saga.chapterOf (TriggeredAbilitySource.ability triggered)
        _ -> Nothing
   in Maybe.mapMaybe from (GameState.stack gs)

-- CR 118.12's gate answered by SEVERAL seats, and read back plurally. The
-- answer is bound under Pawl.Engine.Binding.gatePlayers, which holds every
-- payer whose answer selected the clause's branch; PlayerRef.EachInSlot is the
-- read that takes them all, where PlayerRef.InSlot takes one and so reads
-- NOTHING out of a slot holding two.
--
-- Bellowing Mauler, {4}{B} Creature -- Ogre Warrior 4/6, whose entire text box
-- is "At the beginning of your end step, each player loses 4 life unless they
-- sacrifice a nontoken creature of their choice." CR 118.12a rewrites that
-- "unless" into the offer, and CR 119.3 is the life loss.
--
-- THREE SEATS, each on a different limb of the rule, because two cannot tell
-- the readings apart -- with two seats both declining, "the seats that did not
-- pay" and "the whole table" name the same pair:
--
--   alice CAN pay (the Mauler is itself a nontoken creature) and DECLINES,
--   bob PAYS, sacrificing one of his two Pikers,
--   carol controls only a TOKEN creature, so CR 118.3 never offers her the cost.
--
-- The observable is the LIFE TRIPLE, and it separates three implementations:
-- EachInSlot gives (16, 20, 16); InSlot's singular read gives (20, 20, 20),
-- since Binding.onlyOne answers Nothing for the two seats in the slot; a naive
-- EachPlayer gives (16, 16, 16), sweeping up the seat that paid. Nothing else
-- on the board changes a life total, and no seat drops near zero, so no
-- state-based action moves the reading between the resolution and the read.
maulerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
maulerSpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      -- alice's own end step: the trigger's scope is TurnScope.ControllersTurn
      -- and she controls the Mauler.
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice)) (gs {GameState.phase = endStep, GameState.activePlayer = S.alice})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs)
      -- Payer-keyed: only bob pays. An answerer that ignored the payer would
      -- collapse the board onto one answer and prove nothing. The Decider is
      -- checked beside the player because CR 723.1 can part them; nothing here
      -- controls anybody, so they must agree.
      onlyBobPays :: Prompt.Prompt r -> r
      onlyBobPays p = case p of
        Prompt.ChooseToPay (Decider.MkDecider d) player _ _ _ _
          | d == S.bob && player == S.bob ->
              PaymentDecision.Pays
        _ -> S.identityAnswer p
      boardOf = do
        mauler <- S.printingOf s registry "Bellowing Mauler"
        piker <- S.printingOf s registry "Goblin Piker"
        let (maulerId, g0) = S.addCreature mauler S.alice S.threePlayerGame
            (bobFirst, g1) = S.addCreature piker S.bob g0
            (bobSecond, g2) = S.addCreature piker S.bob g1
            -- A token COPY of the same creature: identical but for CR 111.1's
            -- tokenness, which is the one thing the cost's filter excludes.
            (carolToken, g3) = S.addToken (Printing.card piker) S.carol g2
        pure (maulerId, bobFirst, bobSecond, carolToken, settle (beginEndStep g3))
   in Spec.describe s "CR 118.12 a gate offered to the whole table" $ do
        Spec.it s "CR 118.12a only the seats that did not pay lose the life" $ do
          (maulerId, bobFirst, bobSecond, carolToken, onStack) <- boardOf
          let after = S.runPure onlyBobPays onStack Stack.resolveTop
          -- THE BEHAVIOUR, first: alice declined and carol was never offered, so
          -- both lost 4; bob paid, so he did not.
          Spec.assertEqWith s "CR 119.3: alice and carol lost 4 and bob, who paid, did not" (lives after) (Just 16, Just 20, Just 16)
          -- CR 701.21a: bob's payment really moved one of his own creatures, and
          -- took exactly one of the two.
          Spec.assertEqWith s "one of bob's two Pikers was sacrificed" (length (filter (\oid -> S.onBattlefield oid after) [bobFirst, bobSecond])) 1
          -- The seats that did not pay paid nothing: alice kept the creature she
          -- could have sacrificed, and carol's token was never a candidate.
          Spec.assertBool s (S.onBattlefield maulerId after && S.onBattlefield carolToken after) "alice's Mauler and carol's token both stayed"
          -- The controls, read off the board BEFORE the resolution so no
          -- implementation of the read can reach them: the trigger really fired,
          -- and the three seats really start level.
          Spec.assertEqWith s "CR 603.2b: its end-step trigger is on the stack" (length (GameState.stack onStack)) 1
          Spec.assertEqWith s "all three seats start at 20" (lives onStack) (Just 20, Just 20, Just 20)
        -- The pair board, differing in exactly ONE thing: bob declines too. It
        -- tells "bob was asked and his own answer spared him" apart from "bob is
        -- excluded for some other reason", and shows the whole table CAN be in
        -- the slot at once.
        Spec.it s "CR 118.12a with nobody paying, every seat loses the life" $ do
          (_, bobFirst, bobSecond, _, onStack) <- boardOf
          let after = S.runPure S.identityAnswer onStack Stack.resolveTop
          Spec.assertEqWith s "CR 119.3: all three seats lost 4" (lives after) (Just 16, Just 16, Just 16)
          Spec.assertEqWith s "and bob kept both Pikers" (length (filter (\oid -> S.onBattlefield oid after) [bobFirst, bobSecond])) 2

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Trigger" $ do
  logSpec s registry
  scanSpec s registry
  permanentEntersSpec s registry
  sacrificeSpec s registry
  mayhemDevilSpec s registry
  stateTriggerSpec s registry
  textChangedTriggerSpec s registry
  historySpec s registry
  spinesplitterSpec s registry
  delayedSpec s registry
  tokenSetSpec s registry
  tokenGroupReadSpec s registry
  tokenGroupMoveSpec s registry
  singleTokenSlotReadSpec s registry
  towershellOnsetSpec s registry
  towershellSkipSpec s registry
  orderingSpec s registry
  secondPlacementPassSpec s registry
  monarchOrderingSpec s registry
  interveningSpec s registry
  enchantedHostTriggerSpec s registry
  tenRingsDrawSpec s registry
  maulerSpec s registry
