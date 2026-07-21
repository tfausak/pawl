-- Covers M4.5 P4 Task 1: the turn-scoped event log (Pawl.Type.GameEvent,
-- GameState's log and watermarks) -- append-only recording, watermark-based
-- consumption per reader (trigger scan, SBA damage check), and the log's
-- turn-scoped clearing at handoff. Task 2 adds the CR 603.2b step-beginning
-- event and the CR 603.6a widened scan (every battlefield permanent, not just
-- an enters event's newcomer) -- `scanTests` below. Task 4 adds CR 603.8
-- state triggers -- `stateTriggerTests` below. Later P4 tasks still owe
-- coverage HERE for delayed triggers (CR 603.7), intervening "if" (CR 603.4 /
-- 608.2a), and the CR 603.3b ordering prompt -- none of that is implemented
-- yet.
module Pawl.TriggerSpec where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Binding as Binding
import qualified Pawl.Cards as Cards
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Sba as Sba
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.GameEvent as GameEvent
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.PendingTrigger as PendingTrigger
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Subtype as Subtype
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
              HU.assertEqual "sources in ascending ObjectId order" [rip1, rip2] (map PendingTrigger.source triggers),
      -- The PERMANENTS-INNER half of that same order guarantee. Every SelfEnters
      -- test above has exactly one bearer matching each event, so inner order
      -- can never affect the output -- SelfEnters alone cannot discriminate
      -- events-outer-permanents-inner from any other traversal. A StepBegins
      -- bearer can: ONE StepBegan event matches MANY permanents at once. Two
      -- Khabál Ghouls (CR 603.2b, "at the beginning of each end step") on the
      -- battlefield, one end-step event -- the PendingTrigger.source list must
      -- come out in ascending ObjectId order.
      HU.testCase "CR 603.2b two StepBegins triggers from one event emit in ascending ObjectId order" $
        let (ghoul1, gs0) = S.addCreature (Cards.khabalGhoulPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (ghoul2, gs1) = S.addCreature (Cards.khabalGhoulPrinting cards) S.alice gs0
            event = GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice
            triggers = Event.gatherTriggers [event] gs1
         in do
              HU.assertBool "ghoul1 has the lower id" (ghoul1 < ghoul2)
              HU.assertEqual "both triggers fired" 2 (length triggers)
              HU.assertEqual "sources in ascending ObjectId order" [ghoul1, ghoul2] (map PendingTrigger.source triggers)
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
      -- The test above sacrifices a permanent the same player owns and
      -- controls, so it never exercises owner-relativity: CR 701.21a says
      -- "its CONTROLLER moves it... to its OWNER's graveyard." Here bob owns
      -- and alice controls (S.giveControl installs the layer-2 SetController
      -- effect), so the result must land in bob's graveyard, not alice's.
      HU.testCase "CR 701.21a a sacrifice lands in the OWNER's graveyard even when a different player controls it" $
        let (piker, gs0) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
            gs = S.giveControl piker S.alice gs0
            after = Event.sacrifice piker gs
         in do
              HU.assertEqual "off bob's battlefield" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "in bob's (owner's) graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after))
              HU.assertEqual "not in alice's graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
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
      -- CR 113.7: "this creature" is a slot read, filled at placement.
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

-- Barbarian Outcast {1}{R} Creature -- Human Barbarian Beast 2/2:
-- "When you control no Swamps, sacrifice this creature." CR 603.8's own example
-- shape ("a player controlling no permanents of a particular card type"), chosen
-- by the rulebook to illustrate the rule.
stateTriggerTests :: Cards.Cards -> Tasty.TestTree
stateTriggerTests cards =
  let outcastBoard swamps =
        let (oid, gs) = S.addCreature (Cards.barbarianOutcastPrinting cards) S.alice (S.landsInPlay (Cards.swampPrinting cards) swamps)
         in (oid, gs)
      triggerIds gs = filter (isTriggerObject gs) (GameState.stack gs)
      isTriggerObject gs oid = case Game.lookupObject oid gs of
        Just obj -> case Object.source obj of
          Source.OfTrigger _ _ -> True
          _ -> False
        Nothing -> False
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
   in Tasty.testGroup
        "StateTrigger"
        [ -- THE flooding falsifier. CR 603.8's second sentence exists to prevent
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
          HU.testCase "CR 603.8 a true state condition puts EXACTLY ONE instance on the stack" $
            let (_, gs) = outcastBoard 0
                settled = settle gs
             in HU.assertEqual "one trigger, not one per boundary" 1 (length (triggerIds settled)),
          HU.testCase "CR 603.8 re-settling while the instance is on the stack adds no second copy" $
            let (_, gs) = outcastBoard 0
                twice = settle (settle gs)
             in HU.assertEqual "still exactly one" 1 (length (triggerIds twice)),
          HU.testCase "CR 603.8 the condition being FALSE means no trigger at all" $
            let (_, gs) = outcastBoard 1
                settled = settle gs
             in HU.assertEqual "no trigger while a Swamp is out" 0 (length (triggerIds settled)),
          HU.testCase "CR 603.8 losing the last Swamp makes the condition true and fires it" $
            let (_, gs) = outcastBoard 1
                quiet = settle gs
                swamp = case Game.zoneMembers Zone.Battlefield S.alice quiet of
                  ids -> case filter (\oid -> Set.member Subtype.Swamp (Projection.subtypesOf oid quiet)) ids of
                    s : _ -> s
                    [] -> ObjectId.MkObjectId 999
                gone = settle (Event.destroy swamp quiet)
             in HU.assertEqual "the Swamp's death arms it" 1 (length (triggerIds gone)),
          -- CR 603.8: "doesn't trigger again until the ability has resolved, has
          -- been countered, or has otherwise left the stack" -- all three are
          -- "no longer on the stack", which is why armedness is derived.
          HU.testCase "CR 603.8 an instance leaving the stack re-arms the trigger" $
            let (_, gs) = outcastBoard 0
                settled = settle gs
                removed = case triggerIds settled of
                  abilId : _ -> Resolve.cease abilId settled
                  [] -> settled
                again = settle removed
             in HU.assertEqual "a fresh instance" 1 (length (triggerIds again)),
          -- IMPORTANT-2 (review): the suppression check in Event.stateTriggers
          -- compares BOTH the source object's id and the ability (`Object.source
          -- obj == Source.OfTrigger srcId ab`). Every test above uses exactly one
          -- Barbarian Outcast, so all of them would still pass a weaker
          -- implementation that compared only the TriggeredAbility and ignored
          -- srcId -- and that weaker version would wrongly suppress a second,
          -- otherwise-independent source. This is the one that catches it: put
          -- one Outcast's instance on the stack first, THEN let a second,
          -- identical Outcast (same controller, same 0-Swamp board) get its own
          -- chance to trigger. Under a source-scoped comparison the second fires;
          -- under an ability-only comparison it is (wrongly) suppressed by the
          -- first's presence on the stack.
          HU.testCase "CR 603.8 a second identical source still triggers -- suppression is per-source, not per-ability" $
            let (_, gs0) = outcastBoard 0
                settledFirst = settle gs0
                (_, gs1) = S.addCreature (Cards.barbarianOutcastPrinting cards) S.alice settledFirst
                settledBoth = settle gs1
             in HU.assertEqual "two instances, one per source" 2 (length (triggerIds settledBoth)),
          -- M-4 (review): stateHolds reads the PROJECTION -- CR 613 layer 4 for a
          -- subtype -- not Card.typeLine. Pin it with no real Swamp card
          -- anywhere: alice controls only a Mountain, so the Outcast triggers;
          -- adding an AddLandSubtype Swamp modification (the Urborg shape) to
          -- that same Mountain must turn the trigger off.
          HU.testCase "CR 613 layer 4: an added Swamp subtype (no real Swamp card) suppresses the trigger" $
            let gs0 = S.mountainsInPlay cards 1
                mountainId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
                  i : _ -> i
                  [] -> ObjectId.MkObjectId 999
                (_, gs1) = S.addCreature (Cards.barbarianOutcastPrinting cards) S.alice gs0
                before = settle gs1
                withUrborg = S.withEffect mountainId (Modification.AddLandSubtype Subtype.Swamp) gs1
                after = settle withUrborg
             in do
                  HU.assertEqual "no real Swamp yet: triggers" 1 (length (triggerIds before))
                  HU.assertEqual "projected Swamp subtype (still a Mountain card): stops triggering" 0 (length (triggerIds after)),
          -- M-4 (review): stateHolds reads projected CONTROL -- CR 613 layer 2 --
          -- not Object.owner. Pin it: bob owns and controls the only Swamp, so
          -- alice's Outcast triggers; giving alice control of bob's Swamp (a
          -- layer-2 SetController effect, S.giveControl) must turn it off even
          -- though bob still OWNS that Swamp.
          HU.testCase "CR 110.2/613 layer 2: gaining control of the opponent's Swamp suppresses the trigger" $
            let gs0 = Setup.emptyGame S.bothPlayers
                (swampId, gs1) = S.addCreature (Cards.swampPrinting cards) S.bob gs0
                (_, gs2) = S.addCreature (Cards.barbarianOutcastPrinting cards) S.alice gs1
                before = settle gs2
                gs3 = S.giveControl swampId S.alice gs2
                after = settle gs3
             in do
                  HU.assertEqual "alice controls no Swamps yet: triggers" 1 (length (triggerIds before))
                  HU.assertEqual "alice now controls the Swamp: stops triggering" 0 (length (triggerIds after)),
          -- The whole card, at gameplay level: the trigger resolves and the
          -- Outcast sacrifices itself (CR 701.21a, through Event.sacrifice).
          HU.testCase "CR 701.21 the resolved trigger sacrifices the Outcast into its owner's graveyard" $
            let (outcast, gs) = outcastBoard 0
                settled = settle gs
                resolved = snd (Engine.runGamePure S.identityAnswer settled Stack.resolveTop)
             in do
                  HU.assertBool "the Outcast is off the battlefield" (not (Set.member outcast (GameState.battlefield resolved)))
                  HU.assertEqual "and in alice's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice resolved))
        ]

-- Khabál Ghoul {2}{B} Creature -- Zombie 1/1: "At the beginning of each end step,
-- put a +1/+1 counter on this creature for each creature that died this turn."
-- Scryfall's only ruling on the card is the design in one sentence: the count
-- "includes creature tokens ... as well as creatures put into a graveyard before
-- Khabál Ghoul entered the battlefield."
historyTests :: Cards.Cards -> Tasty.TestTree
historyTests cards =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs =
        Event.recordEvent (GameEvent.StepBegan endStep S.alice) (gs {GameState.phase = endStep})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      countersOn oid gs = maybe 0 (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
   in Tasty.testGroup
        "TurnHistory"
        [ -- The drained-queue falsifier: the deaths are SCANNED past before the end
          -- step's trigger ever exists, and must still be counted.
          HU.testCase "CR 608.2i deaths the trigger scan already passed are still counted" $
            let (ghoul, gs0) = S.addCreature (Cards.khabalGhoulPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
                (p1, gs1) = S.addPiker cards S.bob gs0
                (p2, gs2) = S.addPiker cards S.bob gs1
                dead = Event.destroy p2 (Event.destroy p1 gs2)
                scanned = settle dead
                atEnd = resolveAll (settle (beginEndStep scanned))
             in do
                  HU.assertEqual "two +1/+1 counters" 2 (countersOn ghoul atEnd)
                  HU.assertEqual "a 3/3" (Just 3) (Projection.powerOf ghoul atEnd),
          -- CR 111.3 / 608.2h: a token has NO printed card, so an implementation
          -- that re-derived card types from print instead of from the event's
          -- snapshot would read zero here.
          HU.testCase "CR 111.3 a token creature that died counts, though it has no printed card" $
            let (ghoul, gs0) = S.addCreature (Cards.khabalGhoulPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
                (tok, gs1) = S.addToken (Printing.card (Cards.pikerPrinting cards)) S.bob gs0
                dead = Sba.checkStateBasedActions (Event.destroy tok gs1)
                atEnd = resolveAll (settle (beginEndStep dead))
             in HU.assertEqual "the token is counted" 1 (countersOn ghoul atEnd),
          HU.testCase "a creature that left the battlefield for HAND did not die (CR 700.4)" $
            let (ghoul, gs0) = S.addCreature (Cards.khabalGhoulPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
                (p1, gs1) = S.addPiker cards S.bob gs0
                bounced = Event.changeZone p1 Zone.Hand gs1
                atEnd = resolveAll (settle (beginEndStep bounced))
             in HU.assertEqual "a bounce is not a death" 0 (countersOn ghoul atEnd),
          -- CR 608.2i: "look back in time" effects don't require the counted
          -- objects to currently exist, or the counting object to have existed
          -- at the time. Scryfall's ruling says this explicitly: the count
          -- "includes ... creatures put into a graveyard before Khabál Ghoul
          -- entered the battlefield." This test cannot fail against today's
          -- `Pawl.Quantity.countOf`, which takes no `ObjectId` at all and so
          -- has no way to scope the fold to the Ghoul's own lifetime -- it is
          -- a regression gate on the ruling, pinned ahead of that signature
          -- ever gaining one.
          HU.testCase "CR 608.2i a creature that died before the Ghoul entered is still counted" $
            let (p1, gs0) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
                dead = Event.destroy p1 gs0
                (ghoul, gs1) = S.addCreature (Cards.khabalGhoulPrinting cards) S.alice (settle dead)
                atEnd = resolveAll (settle (beginEndStep gs1))
             in HU.assertEqual "one +1/+1 counter" 1 (countersOn ghoul atEnd),
          -- CR 608.2i: the history's scope is ONE turn.
          HU.testCase "the count resets at turn handoff, not at the trigger scan" $
            let (ghoul, gs0) = S.addCreature (Cards.khabalGhoulPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
                (p1, gs1) = S.addPiker cards S.bob gs0
                dead = Event.destroy p1 gs1
                nextTurn = snd (Engine.runGamePure S.identityAnswer dead Engine.handoffTurn)
                atEnd = resolveAll (settle (beginEndStep nextTurn))
             in HU.assertEqual "last turn's death does not count" 0 (countersOn ghoul atEnd),
          -- CR 603.2b: the step trigger belongs to a permanent with nothing to do
          -- with the event -- Task 2's widened scan, at gameplay level.
          HU.testCase "CR 603.2b the end step's beginning is what fires it" $
            let (_, gs0) = S.addCreature (Cards.khabalGhoulPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
                quiet = settle gs0
                fired = settle (beginEndStep quiet)
                isTrigger oid = case Game.lookupObject oid fired of
                  Just obj -> case Object.source obj of
                    Source.OfTrigger _ _ -> True
                    _ -> False
                  Nothing -> False
             in do
                  HU.assertEqual "nothing before the step began" [] (GameState.stack quiet)
                  HU.assertEqual "one trigger once it did" 1 (length (filter isTrigger (GameState.stack fired)))
        ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Pawl.TriggerSpec" [logTests cards, scanTests cards, sacrificeTests cards, stateTriggerTests cards, historyTests cards]
