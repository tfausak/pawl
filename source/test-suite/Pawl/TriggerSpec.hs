-- Covers M4.5 P4 Task 1: the turn-scoped event log (Pawl.Type.GameEvent,
-- GameState's log and watermarks) -- append-only recording, watermark-based
-- consumption per reader (trigger scan, SBA damage check), and the log's
-- turn-scoped clearing at handoff. Task 2 adds the CR 603.2b step-beginning
-- event and the CR 603.6a widened scan (every battlefield permanent, not just
-- an enters event's newcomer) -- `scanTests` below. Later P4 tasks add
-- coverage HERE for state triggers (CR 603.8), delayed triggers (CR 603.7),
-- intervening "if" (CR 603.4 / 608.2a), and the CR 603.3b ordering prompt --
-- none of that is implemented yet.
module Pawl.TriggerSpec where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Pawl.Binding as Binding
import qualified Pawl.Cards as Cards
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Sba as Sba
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.GameEvent as GameEvent
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.PendingTrigger as PendingTrigger
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.TriggerCondition as TriggerCondition
import qualified Pawl.Type.TurnScope as TurnScope
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChange as ZoneChange
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- CR 608.2i: the log records; it is never emptied by a reader.
logTests :: Cards.Cards -> Tasty.TestTree
logTests cards =
  Tasty.testGroup
    "EventLog"
    [ -- CR 400.7 / 603.2g: a zone change appends a Moved event carrying the
      -- RESOLVED destination. Pawl.EventSpec's "CR 603.2g: the emitted event
      -- records the RESOLVED destination (exile)" covers this same accessor
      -- (S.zoneChangesOf / ZoneChange.to) more strongly, through a Rest in
      -- Peace redirect -- no separate case needed here.
      -- CR 608.2h: the snapshot is the object as it last existed in the zone it
      -- LEFT. Re-deriving from the printed card would be wrong for an animated
      -- land and impossible for a token (CR 111.3).
      HU.testCase "CR 608.2h a Moved event snapshots the object it moved" $
        let (piker, gs) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
            expected = Projection.project piker gs
            after = Event.changeZone piker Zone.Graveyard gs
         in case Foldable.toList (GameState.events after) of
              GameEvent.Moved _ snapshot : _ -> HU.assertEqual "snapshot from the origin zone" expected snapshot
              _ -> HU.assertFailure "expected exactly one Moved event",
      -- CR 704.5h's window is "since the last SBA check": the check CONSUMES by
      -- bumping a watermark, and the record survives.
      HU.testCase "CR 704 the SBA check advances the damage watermark but keeps the record" $
        let (gs, _, _) = S.combatBoardOf [Cards.typhoidRatsPrinting cards] [Cards.ogreSentryPrinting cards]
            fought = S.fightWith S.aggressiveAnswer gs
            after = Sba.checkStateBasedActions fought
         in do
              HU.assertEqual "nothing left unscanned for damage" [] (Event.unscannedDamage after)
              HU.assertBool "the damage events are still recorded" (not (null (S.damageEventsOf after))),
      HU.testCase "CR 117.5 the trigger scan advances its watermark but keeps the record" $
        let (_, gs) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (piker, gs1) = S.addPiker cards S.bob gs
            moved = Event.changeZone piker Zone.Hand gs1
            scanned = snd (Engine.runGamePure S.identityAnswer moved Engine.placePendingTriggers)
         in do
              HU.assertEqual "nothing left unscanned" [] (Event.unscannedEvents scanned)
              HU.assertBool "the zone change is still recorded" (not (null (S.zoneChangesOf scanned))),
      -- The turn is the log's scope (CR 608.2i). Clearing at cleanup would be
      -- wrong: cleanup is still part of THIS turn.
      HU.testCase "the log and both watermarks are cleared at turn handoff" $
        let (piker, gs) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
            moved = Event.changeZone piker Zone.Graveyard gs
            after = snd (Engine.runGamePure S.identityAnswer moved Engine.handoffTurn)
         in do
              HU.assertEqual "log empty" Seq.empty (GameState.events after)
              HU.assertEqual "scan watermark reset" 0 (GameState.scannedThrough after)
              HU.assertEqual "damage watermark reset" 0 (GameState.damageScannedThrough after),
      -- CR 514.3 (partial): an event emitted by the cleanup step must be scanned
      -- BEFORE handoffTurn clears the log, or its trigger is lost outright.
      HU.testCase "advance settles before handing off, so no unscanned event is discarded" $
        let (ripId, gs0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            entered = ZoneChange.MkZoneChange ripId Zone.Stack Zone.Battlefield
            gs1 = S.withEvent (GameEvent.Moved entered (Projection.project ripId gs0)) gs0
            ending = gs1 {GameState.remaining = Seq.empty}
            after = snd (Engine.runGamePure S.identityAnswer ending Engine.advance)
            isTrigger oid = case Game.lookupObject oid after of
              Just obj -> case Object.source obj of
                Source.OfTrigger _ _ -> True
                _ -> False
              Nothing -> False
         in do
              HU.assertBool "the pending trigger reached the stack" (any isTrigger (GameState.stack after))
              HU.assertEqual "the log was cleared afterwards" Seq.empty (GameState.events after)
    ]

-- CR 603.2b / 603.6a: a step begins, and EVERY permanent is checked.
scanTests :: Cards.Cards -> Tasty.TestTree
scanTests cards =
  Tasty.testGroup
    "Scan"
    [ HU.testCase "CR 603.2b running a step records that it began, on the active player's turn" $
        let gs = (Setup.emptyGame S.bothPlayers) {GameState.phase = Phase.Ending EndingStep.EndStep, GameState.activePlayer = S.alice}
            after = snd (Engine.runGamePure S.identityAnswer gs Engine.runStep)
            began ev = case ev of
              GameEvent.StepBegan p pid -> Just (p, pid)
              _ -> Nothing
         in HU.assertEqual
              "the end step's beginning is recorded exactly once"
              [(Phase.Ending EndingStep.EndStep, S.alice)]
              (Maybe.mapMaybe began (Foldable.toList (GameState.events after))),
      HU.testCase "CR 603.2b StepBegins matches its own step and no other" $
        let bearer = ObjectId.MkObjectId 1
            cond = TriggerCondition.StepBegins (Phase.Ending EndingStep.EndStep) TurnScope.EachTurn
         in do
              HU.assertBool "the end step matches" $
                Event.matchesTrigger bearer S.alice cond (GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice)
              HU.assertBool "the upkeep does not" $
                not (Event.matchesTrigger bearer S.alice cond (GameEvent.StepBegan (Phase.Beginning BeginningStep.Upkeep) S.alice)),
      -- CR 603.3a / 109.5: "your upkeep" is the ABILITY CONTROLLER's (603.3a
      -- controls the ability; 109.5 makes "your" mean that controller), so the
      -- scope is read against the bearer's controller, not the card.
      HU.testCase "CR 603.3a ControllersTurn matches only the bearer's controller's turn" $
        let bearer = ObjectId.MkObjectId 1
            cond = TriggerCondition.StepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn
         in do
              HU.assertBool "alice's upkeep matches for alice" $
                Event.matchesTrigger bearer S.alice cond (GameEvent.StepBegan (Phase.Beginning BeginningStep.Upkeep) S.alice)
              HU.assertBool "bob's upkeep does not" $
                not (Event.matchesTrigger bearer S.alice cond (GameEvent.StepBegan (Phase.Beginning BeginningStep.Upkeep) S.bob)),
      -- The widening falsifier: the scan now visits every battlefield permanent,
      -- so SelfEnters must ask whether the event is about THIS permanent. Rest in
      -- Peace is on the battlefield and a DIFFERENT object entered.
      HU.testCase "CR 603.6a a SelfEnters trigger does not fire on another object's entry" $
        let (_, gs0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (piker, gs1) = S.addPiker cards S.bob gs0
            entered = ZoneChange.MkZoneChange piker Zone.Stack Zone.Battlefield
            gs2 = S.withEvent (GameEvent.Moved entered (Projection.project piker gs1)) gs1
         in HU.assertEqual "no trigger" 0 (length (Event.gatherTriggers (Event.unscannedEvents gs2) gs2)),
      HU.testCase "CR 603.6a a SelfEnters trigger still fires on its own entry" $
        let (ripId, gs0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            entered = ZoneChange.MkZoneChange ripId Zone.Stack Zone.Battlefield
            gs1 = S.withEvent (GameEvent.Moved entered (Projection.project ripId gs0)) gs0
         in case Event.gatherTriggers (Event.unscannedEvents gs1) gs1 of
              [pt] -> do
                HU.assertEqual "source is RiP" ripId (PendingTrigger.source pt)
                HU.assertEqual "controller is alice" S.alice (PendingTrigger.controller pt)
              other -> HU.assertFailure ("expected exactly one pending trigger, got " <> show (length other)),
      HU.testCase "a graveyard-bound event yields no enters trigger" $
        let (ripId, gs0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            toGrave = ZoneChange.MkZoneChange ripId Zone.Battlefield Zone.Graveyard
            gs1 = S.withEvent (GameEvent.Moved toGrave (Projection.project ripId gs0)) gs0
         in HU.assertEqual "no triggers" 0 (length (Event.gatherTriggers (Event.unscannedEvents gs1) gs1)),
      HU.testCase "SelfEnters matches only a battlefield destination" $
        let bearer = ObjectId.MkObjectId 1
            movedTo zone = GameEvent.Moved (ZoneChange.MkZoneChange bearer Zone.Stack zone) S.emptyCharacteristics
         in do
              HU.assertBool "enters battlefield matches" $
                Event.matchesTrigger bearer S.alice TriggerCondition.SelfEnters (movedTo Zone.Battlefield)
              HU.assertBool "enters graveyard does not" $
                not (Event.matchesTrigger bearer S.alice TriggerCondition.SelfEnters (movedTo Zone.Graveyard)),
      -- Pins the canonical emission order this module's `eventTriggers` comment
      -- documents ("events outer, permanents inner, ascending by id"), which a
      -- later task's CR 603.3b ordering prompt indexes into. Two RiP bearers
      -- enter via two separate events recorded in the same order their ids
      -- were assigned; the resulting PendingTrigger.source list must follow
      -- that same ascending order.
      HU.testCase "CR 603.6a two SelfEnters triggers emit in ascending ObjectId order" $
        let (rip1, gs0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (rip2, gs1) = S.addCreature (Cards.restInPeacePrinting cards) S.alice gs0
            entered1 = ZoneChange.MkZoneChange rip1 Zone.Stack Zone.Battlefield
            entered2 = ZoneChange.MkZoneChange rip2 Zone.Stack Zone.Battlefield
            gs2 = S.withEvent (GameEvent.Moved entered1 (Projection.project rip1 gs1)) gs1
            gs3 = Event.recordEvent (GameEvent.Moved entered2 (Projection.project rip2 gs1)) gs2
            triggers = Event.gatherTriggers (Event.unscannedEvents gs3) gs3
         in do
              HU.assertBool "rip1 has the lower id" (rip1 < rip2)
              HU.assertEqual "both triggers fired" 2 (length triggers)
              HU.assertEqual "sources in ascending ObjectId order" [rip1, rip2] (map PendingTrigger.source triggers)
    ]

-- CR 701.21: sacrificing is its own keyword action -- NOT a destruction.
sacrificeTests :: Cards.Cards -> Tasty.TestTree
sacrificeTests cards =
  Tasty.testGroup
    "Sacrifice"
    [ HU.testCase "CR 701.21a a sacrificed permanent goes to its owner's graveyard" $
        let (piker, gs) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
            after = Event.sacrifice piker gs
         in do
              HU.assertEqual "off the battlefield" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "in bob's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      -- CR 701.21a: "sacrificing a permanent doesn't destroy it", so neither CR
      -- 702.12b's indestructible gate nor CR 701.19a's shield applies.
      HU.testCase "CR 701.21a an indestructible permanent can still be sacrificed" $
        let (myr, gs) = S.addCreature (Cards.darksteelMyrPrinting cards) S.bob (Setup.emptyGame S.bothPlayers)
            after = Event.sacrifice myr gs
         in HU.assertEqual "gone from the battlefield" 0 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 701.21a sacrificing neither consults nor consumes a regeneration shield" $
        let (piker, gs0) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
            gs = S.addRegenShield piker gs0
            after = Event.sacrifice piker gs
         in do
              HU.assertEqual "still sacrificed" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "the shield is untouched" (Just 1) (Map.lookup piker (GameState.regenerationShields after)),
      HU.testCase "only a battlefield permanent can be sacrificed (CR 701.21a)" $
        let (card, gs) = S.addLibraryCard (Cards.pikerPrinting cards) S.bob (Setup.emptyGame S.bothPlayers)
            after = Event.sacrifice card gs
         in HU.assertEqual "the library card is untouched" gs after,
      -- CR 113.7 / 603.7c: "this creature" is a slot read, filled at placement.
      HU.testCase "CR 113.7 a placed trigger binds its source into the reserved self slot" $
        let (ripId, gs0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            entered = ZoneChange.MkZoneChange ripId Zone.Stack Zone.Battlefield
            gs1 = S.withEvent (GameEvent.Moved entered (Projection.project ripId gs0)) gs0
            placed = snd (Engine.runGamePure S.identityAnswer gs1 Engine.placePendingTriggers)
            bindingsOn oid = maybe Map.empty Object.bindings (Game.lookupObject oid placed)
            selfOf oid = Map.lookup Binding.triggerSource (Binding.targetsOf (bindingsOn oid))
         in HU.assertEqual
              "the trigger names its source"
              [Just (Recipient.ToObject ripId)]
              (map selfOf (GameState.stack placed))
    ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Pawl.TriggerSpec" [logTests cards, scanTests cards, sacrificeTests cards]
