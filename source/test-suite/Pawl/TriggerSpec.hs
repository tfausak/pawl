-- Covers M4.5 P4 in full. The turn-scoped event log (Pawl.Type.GameEvent,
-- GameState's log and watermarks) -- append-only recording, watermark-based
-- consumption per reader (trigger scan, SBA damage check), and the log's
-- turn-scoped clearing at handoff -- `logTests`. The CR 603.2b step-beginning
-- event and the CR 603.6a widened scan (every battlefield permanent, not just
-- an enters event's newcomer) -- `scanTests`. The `Sacrifice` opcode and its
-- reserved trigger-source slot, CR 701.21 -- `sacrificeTests`. CR 603.8 state
-- triggers -- `stateTriggerTests`. CR 608.2i turn history (Khabál Ghoul's
-- "died this turn") -- `historyTests`. CR 603.7 delayed triggered abilities
-- -- `delayedTests`. The CR 603.3b ordering prompt -- `orderingTests`. The CR
-- 603.4 / 608.2a intervening "if" -- `interveningTests`. Also Pawl.Keyword: CR
-- 702.70 poisonous, the keyword whose rule text IS a triggered ability, and the
-- reserved "that player" slot the scan stamps for it -- `poisonousTests`.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Pawl.TriggerSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Binding as Binding
import qualified Pawl.Cast as Cast
import qualified Pawl.Departure as Departure
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Keyword as Keyword
import qualified Pawl.Modal as Modal
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.ActiveReplacement as ActiveReplacement
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.DamageKind as DamageKind
import qualified Pawl.Type.Departure as Departure.Type
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.GameEvent as GameEvent
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword.Type
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeIndex as ModeIndex
import qualified Pawl.Type.ModeSelection as ModeSelection
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.PendingTrigger as PendingTrigger
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.TriggerCondition as TriggerCondition
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
import qualified Pawl.Type.TurnScope as TurnScope
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChange as ZoneChange
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

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
   in resolveAll (snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid)))

-- CR 608.2i: the log records; it is never emptied by a reader.
logTests :: Registry.Type.Registry -> Tasty.TestTree
logTests registry =
  Tasty.testGroup
    "EventLog"
    [ -- CR 400.7 / 603.2g: a zone change appends a Moved event carrying the
      -- RESOLVED destination. Pawl.EventSpec's "CR 603.2g: the emitted event
      -- records the RESOLVED destination (exile)" covers this same accessor
      -- (S.zoneChangesOf / ZoneChange.to) more strongly, through a Rest in
      -- Peace redirect -- no separate case needed here.
      -- CR 608.2h: the snapshot is the object as it last existed in the zone it
      -- LEFT. Re-deriving from the printed card would be wrong for an animated
      -- land and impossible for a token (CR 111.1).
      HU.testCase "CR 608.2h a Moved event snapshots the object it moved" $ do
        pikerPrinting <- Registry.printing registry "Goblin Piker"
        let (piker, gs) = S.addCreature pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
            expected = Projection.project piker gs
            after = S.runPure S.identityAnswer gs (Event.changeZone piker Zone.Graveyard)
        case Foldable.toList (GameState.events after) of
          GameEvent.Moved _ snapshot : _ -> HU.assertEqual "snapshot from the origin zone" expected snapshot
          _ -> HU.assertFailure "expected exactly one Moved event",
      -- CR 704.5h's window is "since the last SBA check": the check CONSUMES by
      -- bumping a watermark, and the record survives.
      HU.testCase "CR 704 the SBA check advances the damage watermark but keeps the record" $ do
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        ogreSentry <- Registry.printing registry "Ogre Sentry"
        let (gs, _, _) = S.combatBoardOf [typhoidRats] [ogreSentry]
            fought = S.fightWith S.aggressiveAnswer gs
            after = S.settleSba fought
        HU.assertEqual "nothing left unscanned for damage" [] (Event.unscannedDamage after)
        HU.assertBool "the damage events are still recorded" (not (null (S.damageEventsOf after))),
      HU.testCase "CR 117.5 the trigger scan advances its watermark but keeps the record" $ do
        restInPeace <- Registry.printing registry "Rest in Peace"
        piker <- Registry.printing registry "Goblin Piker"
        let (_, gs) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
            (pikerId, gs1) = S.addCreature piker S.bob gs
            moved = S.runPure S.identityAnswer gs1 (Event.changeZone pikerId Zone.Hand)
            scanned = snd (Engine.runGamePure S.identityAnswer moved Engine.placePendingTriggers)
        HU.assertEqual "nothing left unscanned" [] (Event.unscannedEvents scanned)
        HU.assertBool "the zone change is still recorded" (not (null (S.zoneChangesOf scanned))),
      -- The turn is the log's scope (CR 608.2i). Clearing at cleanup would be
      -- wrong: cleanup is still part of THIS turn.
      HU.testCase "the log and both watermarks are cleared at turn handoff" $ do
        pikerPrinting <- Registry.printing registry "Goblin Piker"
        let (piker, gs) = S.addCreature pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
            moved = S.runPure S.identityAnswer gs (Event.changeZone piker Zone.Graveyard)
            after = snd (Engine.runGamePure S.identityAnswer moved Engine.handoffTurn)
        HU.assertEqual "log empty" Seq.empty (GameState.events after)
        HU.assertEqual "scan watermark reset" 0 (GameState.scannedThrough after)
        HU.assertEqual "damage watermark reset" 0 (GameState.damageScannedThrough after),
      -- CR 514.3 (partial): an event emitted by the cleanup step must be scanned
      -- BEFORE handoffTurn clears the log, or its trigger is lost outright.
      HU.testCase "advance settles before handing off, so no unscanned event is discarded" $ do
        restInPeace <- Registry.printing registry "Rest in Peace"
        let (ripId, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
            entered = ZoneChange.MkZoneChange ripId Zone.Stack Zone.Battlefield
            gs1 = S.withEvents [GameEvent.Moved entered (Projection.project ripId gs0)] gs0
            ending = gs1 {GameState.remaining = Seq.empty}
            after = snd (Engine.runGamePure S.identityAnswer ending Engine.advance)
            isTrigger oid = case Game.lookupObject oid after of
              Just obj -> case Object.source obj of
                Source.OfTrigger _ _ -> True
                _ -> False
              Nothing -> False
        HU.assertBool "the pending trigger reached the stack" (any isTrigger (GameState.stack after))
        HU.assertEqual "the log was cleared afterwards" Seq.empty (GameState.events after)
    ]

-- CR 603.2b / 603.6a: a step begins, and EVERY permanent is checked.
scanTests :: Registry.Type.Registry -> Tasty.TestTree
scanTests registry =
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
      HU.testCase "CR 603.6a a SelfEnters trigger does not fire on another object's entry" $ do
        restInPeace <- Registry.printing registry "Rest in Peace"
        piker <- Registry.printing registry "Goblin Piker"
        let (_, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
            (pikerId, gs1) = S.addCreature piker S.bob gs0
            entered = ZoneChange.MkZoneChange pikerId Zone.Stack Zone.Battlefield
            gs2 = S.withEvents [GameEvent.Moved entered (Projection.project pikerId gs1)] gs1
        HU.assertEqual "no trigger" 0 (length (fst (Event.gatherTriggers (Event.unscannedEvents gs2) gs2))),
      HU.testCase "CR 603.6a a SelfEnters trigger still fires on its own entry" $ do
        restInPeace <- Registry.printing registry "Rest in Peace"
        let (ripId, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
            entered = ZoneChange.MkZoneChange ripId Zone.Stack Zone.Battlefield
            gs1 = S.withEvents [GameEvent.Moved entered (Projection.project ripId gs0)] gs0
        case fst (Event.gatherTriggers (Event.unscannedEvents gs1) gs1) of
          [pt] -> do
            HU.assertEqual "source is RiP" ripId (PendingTrigger.source pt)
            HU.assertEqual "controller is alice" S.alice (PendingTrigger.controller pt)
          other -> HU.assertFailure ("expected exactly one pending trigger, got " <> show (length other)),
      HU.testCase "a graveyard-bound event yields no enters trigger" $ do
        restInPeace <- Registry.printing registry "Rest in Peace"
        let (ripId, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
            toGrave = ZoneChange.MkZoneChange ripId Zone.Battlefield Zone.Graveyard
            gs1 = S.withEvents [GameEvent.Moved toGrave (Projection.project ripId gs0)] gs0
        HU.assertEqual "no triggers" 0 (length (fst (Event.gatherTriggers (Event.unscannedEvents gs1) gs1))),
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
      HU.testCase "CR 603.6a two SelfEnters triggers emit in ascending ObjectId order" $ do
        restInPeace <- Registry.printing registry "Rest in Peace"
        let (rip1, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
            (rip2, gs1) = S.addCreature restInPeace S.alice gs0
            entered1 = ZoneChange.MkZoneChange rip1 Zone.Stack Zone.Battlefield
            entered2 = ZoneChange.MkZoneChange rip2 Zone.Stack Zone.Battlefield
            gs2 =
              S.withEvents
                [ GameEvent.Moved entered1 (Projection.project rip1 gs1),
                  GameEvent.Moved entered2 (Projection.project rip2 gs1)
                ]
                gs1
            triggers = fst (Event.gatherTriggers (Event.unscannedEvents gs2) gs2)
        HU.assertBool "rip1 has the lower id" (rip1 < rip2)
        HU.assertEqual "both triggers fired" 2 (length triggers)
        HU.assertEqual "sources in ascending ObjectId order" [rip1, rip2] (fmap PendingTrigger.source triggers),
      -- The PERMANENTS-INNER half of that same order guarantee. Every SelfEnters
      -- test above has exactly one bearer matching each event, so inner order
      -- can never affect the output -- SelfEnters alone cannot discriminate
      -- events-outer-permanents-inner from any other traversal. A StepBegins
      -- bearer can: ONE StepBegan event matches MANY permanents at once. Two
      -- Khabál Ghouls (CR 603.2b, "at the beginning of each end step") on the
      -- battlefield, one end-step event -- the PendingTrigger.source list must
      -- come out in ascending ObjectId order.
      HU.testCase "CR 603.2b two StepBegins triggers from one event emit in ascending ObjectId order" $ do
        khabalGhoul <- Registry.printing registry "Khabál Ghoul"
        let (ghoul1, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
            (ghoul2, gs1) = S.addCreature khabalGhoul S.alice gs0
            event = GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice
            triggers = fst (Event.gatherTriggers [event] gs1)
        HU.assertBool "ghoul1 has the lower id" (ghoul1 < ghoul2)
        HU.assertEqual "both triggers fired" 2 (length triggers)
        HU.assertEqual "sources in ascending ObjectId order" [ghoul1, ghoul2] (fmap PendingTrigger.source triggers)
    ]

-- CR 701.21: sacrificing is its own keyword action -- NOT a destruction.
sacrificeTests :: Registry.Type.Registry -> Tasty.TestTree
sacrificeTests registry =
  Tasty.testGroup
    "Sacrifice"
    [ HU.testCase "CR 701.21a a sacrificed permanent goes to its owner's graveyard" $ do
        pikerPrinting <- Registry.printing registry "Goblin Piker"
        let (piker, gs) = S.addCreature pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
            after = S.runPure S.identityAnswer gs (Event.sacrifice piker)
        HU.assertEqual "off the battlefield" 0 (S.creaturesInPlay S.bob after)
        HU.assertEqual "in bob's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      -- The test above sacrifices a permanent the same player owns and
      -- controls, so it never exercises owner-relativity: CR 701.21a says
      -- "its CONTROLLER moves it... to its OWNER's graveyard." Here bob owns
      -- and alice controls (S.giveControl installs the layer-2 SetController
      -- effect), so the result must land in bob's graveyard, not alice's.
      HU.testCase "CR 701.21a a sacrifice lands in the OWNER's graveyard even when a different player controls it" $ do
        pikerPrinting <- Registry.printing registry "Goblin Piker"
        let (piker, gs0) = S.addCreature pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
            gs = S.giveControl piker S.alice gs0
            after = S.runPure S.identityAnswer gs (Event.sacrifice piker)
        HU.assertEqual "off bob's battlefield" 0 (S.creaturesInPlay S.bob after)
        HU.assertEqual "in bob's (owner's) graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after))
        HU.assertEqual "not in alice's graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      -- CR 701.21a: "sacrificing a permanent doesn't destroy it", so neither CR
      -- 702.12b's indestructible gate nor CR 701.19a's shield applies.
      HU.testCase "CR 701.21a an indestructible permanent can still be sacrificed" $ do
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        let (myr, gs) = S.addCreature darksteelMyr S.bob (Setup.emptyGame S.bothPlayers)
            after = S.runPure S.identityAnswer gs (Event.sacrifice myr)
        HU.assertEqual "gone from the battlefield" 0 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 701.21a sacrificing neither consults nor consumes a regeneration shield" $ do
        pikerPrinting <- Registry.printing registry "Goblin Piker"
        let (piker, gs0) = S.addCreature pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
            gs = S.addRegenShield piker gs0
            after = S.runPure S.identityAnswer gs (Event.sacrifice piker)
        HU.assertEqual "still sacrificed" 0 (S.creaturesInPlay S.bob after)
        HU.assertEqual
          "the shield's source is untouched"
          [piker]
          (fmap ActiveReplacement.source (GameState.replacements after)),
      HU.testCase "only a battlefield permanent can be sacrificed (CR 701.21a)" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (card, gs) = S.addLibraryCard piker S.bob (Setup.emptyGame S.bothPlayers)
            after = S.runPure S.identityAnswer gs (Event.sacrifice card)
        HU.assertEqual "the library card is untouched" gs after,
      -- CR 113.7: "this creature" is a slot read, filled at placement.
      HU.testCase "CR 113.7 a placed trigger binds its source into the reserved self slot" $ do
        restInPeace <- Registry.printing registry "Rest in Peace"
        let (ripId, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
            entered = ZoneChange.MkZoneChange ripId Zone.Stack Zone.Battlefield
            gs1 = S.withEvents [GameEvent.Moved entered (Projection.project ripId gs0)] gs0
            placed = snd (Engine.runGamePure S.identityAnswer gs1 Engine.placePendingTriggers)
            bindingsOn oid = maybe Map.empty Object.bindings (Game.lookupObject oid placed)
            selfOf oid = Map.lookup Binding.triggerSource (Binding.targetsOf (bindingsOn oid))
        HU.assertEqual
          "the trigger names its source"
          [Just (Recipient.ToObject ripId)]
          (fmap selfOf (GameState.stack placed))
    ]

-- Barbarian Outcast {1}{R} Creature -- Human Barbarian Beast 2/2:
-- "When you control no Swamps, sacrifice this creature." CR 603.8's own example
-- shape ("a player controlling no permanents of a particular card type"), chosen
-- by the rulebook to illustrate the rule.
stateTriggerTests :: Registry.Type.Registry -> Tasty.TestTree
stateTriggerTests registry =
  let triggerIds gs = filter (isTriggerObject gs) (GameState.stack gs)
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
          HU.testCase "CR 603.8 a true state condition puts EXACTLY ONE instance on the stack" $ do
            barbarianOutcast <- Registry.printing registry "Barbarian Outcast"
            swamp <- Registry.printing registry "Swamp"
            let (_, gs) = outcastBoard barbarianOutcast swamp 0
                settled = settle gs
            HU.assertEqual "one trigger, not one per boundary" 1 (length (triggerIds settled)),
          HU.testCase "CR 603.8 re-settling while the instance is on the stack adds no second copy" $ do
            barbarianOutcast <- Registry.printing registry "Barbarian Outcast"
            swamp <- Registry.printing registry "Swamp"
            let (_, gs) = outcastBoard barbarianOutcast swamp 0
                twice = settle (settle gs)
            HU.assertEqual "still exactly one" 1 (length (triggerIds twice)),
          HU.testCase "CR 603.8 the condition being FALSE means no trigger at all" $ do
            barbarianOutcast <- Registry.printing registry "Barbarian Outcast"
            swamp <- Registry.printing registry "Swamp"
            let (_, gs) = outcastBoard barbarianOutcast swamp 1
                settled = settle gs
            HU.assertEqual "no trigger while a Swamp is out" 0 (length (triggerIds settled)),
          HU.testCase "CR 603.8 losing the last Swamp makes the condition true and fires it" $ do
            barbarianOutcast <- Registry.printing registry "Barbarian Outcast"
            swamp <- Registry.printing registry "Swamp"
            let (_, gs) = outcastBoard barbarianOutcast swamp 1
                quiet = settle gs
                swampOid = case Game.zoneMembers Zone.Battlefield S.alice quiet of
                  ids -> case filter (\oid -> Set.member Subtype.Swamp (Projection.subtypesOf oid quiet)) ids of
                    s : _ -> s
                    [] -> ObjectId.MkObjectId 999
                gone = settle (S.runPure S.identityAnswer quiet (Event.destroy swampOid))
            HU.assertEqual "the Swamp's death arms it" 1 (length (triggerIds gone)),
          -- CR 603.8: "doesn't trigger again until the ability has resolved, has
          -- been countered, or has otherwise left the stack" -- all three are
          -- "no longer on the stack", which is why armedness is derived.
          HU.testCase "CR 603.8 an instance leaving the stack re-arms the trigger" $ do
            barbarianOutcast <- Registry.printing registry "Barbarian Outcast"
            swamp <- Registry.printing registry "Swamp"
            let (_, gs) = outcastBoard barbarianOutcast swamp 0
                settled = settle gs
                removed = case triggerIds settled of
                  abilId : _ -> Resolve.cease abilId settled
                  [] -> settled
                again = settle removed
            HU.assertEqual "a fresh instance" 1 (length (triggerIds again)),
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
          HU.testCase "CR 603.8 a second identical source still triggers -- suppression is per-source, not per-ability" $ do
            barbarianOutcast <- Registry.printing registry "Barbarian Outcast"
            swamp <- Registry.printing registry "Swamp"
            let (_, gs0) = outcastBoard barbarianOutcast swamp 0
                settledFirst = settle gs0
                (_, gs1) = S.addCreature barbarianOutcast S.alice settledFirst
                settledBoth = settle gs1
            HU.assertEqual "two instances, one per source" 2 (length (triggerIds settledBoth)),
          -- M-4 (review): the state trigger's Condition.holds reads the PROJECTION
          -- -- CR 613 layer 4 for a subtype -- not Card.typeLine. Pin it with no real Swamp card
          -- anywhere: alice controls only a Mountain, so the Outcast triggers;
          -- adding an AddLandSubtype Swamp modification (the Urborg shape) to
          -- that same Mountain must turn the trigger off.
          HU.testCase "CR 613 layer 4: an added Swamp subtype (no real Swamp card) suppresses the trigger" $ do
            mountain <- Registry.printing registry "Mountain"
            barbarianOutcast <- Registry.printing registry "Barbarian Outcast"
            let gs0 = S.landsInPlay mountain 1
                mountainId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
                  i : _ -> i
                  [] -> ObjectId.MkObjectId 999
                (_, gs1) = S.addCreature barbarianOutcast S.alice gs0
                before = settle gs1
                withUrborg = S.withEffect mountainId (Modification.AddLandSubtype Subtype.Swamp) gs1
                after = settle withUrborg
            HU.assertEqual "no real Swamp yet: triggers" 1 (length (triggerIds before))
            HU.assertEqual "projected Swamp subtype (still a Mountain card): stops triggering" 0 (length (triggerIds after)),
          -- M-4 (review): the state trigger's Condition.holds reads projected
          -- CONTROL -- CR 613 layer 2 -- not Object.owner. Pin it: bob owns and controls the only Swamp, so
          -- alice's Outcast triggers; giving alice control of bob's Swamp (a
          -- layer-2 SetController effect, S.giveControl) must turn it off even
          -- though bob still OWNS that Swamp.
          HU.testCase "CR 110.2/613 layer 2: gaining control of the opponent's Swamp suppresses the trigger" $ do
            swamp <- Registry.printing registry "Swamp"
            barbarianOutcast <- Registry.printing registry "Barbarian Outcast"
            let gs0 = Setup.emptyGame S.bothPlayers
                (swampId, gs1) = S.addCreature swamp S.bob gs0
                (_, gs2) = S.addCreature barbarianOutcast S.alice gs1
                before = settle gs2
                gs3 = S.giveControl swampId S.alice gs2
                after = settle gs3
            HU.assertEqual "alice controls no Swamps yet: triggers" 1 (length (triggerIds before))
            HU.assertEqual "alice now controls the Swamp: stops triggering" 0 (length (triggerIds after)),
          -- The whole card, at gameplay level: the trigger resolves and the
          -- Outcast sacrifices itself (CR 701.21a, through Event.sacrifice).
          HU.testCase "CR 701.21 the resolved trigger sacrifices the Outcast into its owner's graveyard" $ do
            barbarianOutcast <- Registry.printing registry "Barbarian Outcast"
            swamp <- Registry.printing registry "Swamp"
            let (outcast, gs) = outcastBoard barbarianOutcast swamp 0
                settled = settle gs
                resolved = snd (Engine.runGamePure S.identityAnswer settled Stack.resolveTop)
            HU.assertBool "the Outcast is off the battlefield" (not (Set.member outcast (GameState.battlefield resolved)))
            HU.assertEqual "and in alice's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice resolved))
        ]

-- Khabál Ghoul {2}{B} Creature -- Zombie 1/1: "At the beginning of each end step,
-- put a +1/+1 counter on this creature for each creature that died this turn."
-- Scryfall's only ruling on the card is the design in one sentence: the count
-- "includes creature tokens ... as well as creatures put into a graveyard before
-- Khabál Ghoul entered the battlefield."
historyTests :: Registry.Type.Registry -> Tasty.TestTree
historyTests registry =
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
          HU.testCase "CR 608.2i deaths the trigger scan already passed are still counted" $ do
            khabalGhoul <- Registry.printing registry "Khabál Ghoul"
            piker <- Registry.printing registry "Goblin Piker"
            let (ghoul, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
                (p1, gs1) = S.addCreature piker S.bob gs0
                (p2, gs2) = S.addCreature piker S.bob gs1
                dead = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs2 (Event.destroy p1)) (Event.destroy p2)
                scanned = settle dead
                atEnd = resolveAll (settle (beginEndStep scanned))
            HU.assertEqual "two +1/+1 counters" 2 (countersOn ghoul atEnd)
            HU.assertEqual "a 3/3" (Just 3) (Projection.powerOf ghoul atEnd),
          -- CR 111.1 / 608.2h: a token has NO printed card, so an implementation
          -- that re-derived card types from print instead of from the event's
          -- snapshot would read zero here.
          HU.testCase "CR 111.1 a token creature that died counts, though it has no printed card" $ do
            khabalGhoul <- Registry.printing registry "Khabál Ghoul"
            piker <- Registry.printing registry "Goblin Piker"
            let (ghoul, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
                (tok, gs1) = S.addToken (Printing.card piker) S.bob gs0
                dead = S.settleSba (S.runPure S.identityAnswer gs1 (Event.destroy tok))
                atEnd = resolveAll (settle (beginEndStep dead))
            HU.assertEqual "the token is counted" 1 (countersOn ghoul atEnd),
          HU.testCase "a creature that left the battlefield for HAND did not die (CR 700.4)" $ do
            khabalGhoul <- Registry.printing registry "Khabál Ghoul"
            piker <- Registry.printing registry "Goblin Piker"
            let (ghoul, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
                (p1, gs1) = S.addCreature piker S.bob gs0
                bounced = S.runPure S.identityAnswer gs1 (Event.changeZone p1 Zone.Hand)
                atEnd = resolveAll (settle (beginEndStep bounced))
            HU.assertEqual "a bounce is not a death" 0 (countersOn ghoul atEnd),
          -- CR 608.2i: "look back in time" effects don't require the counted
          -- objects to currently exist, or the counting object to have existed
          -- at the time. Scryfall's ruling says this explicitly: the count
          -- "includes ... creatures put into a graveyard before Khabál Ghoul
          -- entered the battlefield." This test cannot fail against today's
          -- `Pawl.Quantity.countOf`, which takes no `ObjectId` at all and so
          -- has no way to scope the fold to the Ghoul's own lifetime -- it is
          -- a regression gate on the ruling, pinned ahead of that signature
          -- ever gaining one.
          HU.testCase "CR 608.2i a creature that died before the Ghoul entered is still counted" $ do
            piker <- Registry.printing registry "Goblin Piker"
            khabalGhoul <- Registry.printing registry "Khabál Ghoul"
            let (p1, gs0) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
                dead = S.runPure S.identityAnswer gs0 (Event.destroy p1)
                (ghoul, gs1) = S.addCreature khabalGhoul S.alice (settle dead)
                atEnd = resolveAll (settle (beginEndStep gs1))
            HU.assertEqual "one +1/+1 counter" 1 (countersOn ghoul atEnd),
          -- CR 608.2i: the history's scope is ONE turn.
          HU.testCase "the count resets at turn handoff, not at the trigger scan" $ do
            khabalGhoul <- Registry.printing registry "Khabál Ghoul"
            piker <- Registry.printing registry "Goblin Piker"
            let (ghoul, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
                (p1, gs1) = S.addCreature piker S.bob gs0
                dead = S.runPure S.identityAnswer gs1 (Event.destroy p1)
                nextTurn = snd (Engine.runGamePure S.identityAnswer dead Engine.handoffTurn)
                atEnd = resolveAll (settle (beginEndStep nextTurn))
            HU.assertEqual "last turn's death does not count" 0 (countersOn ghoul atEnd),
          -- CR 603.2b: the step trigger belongs to a permanent with nothing to do
          -- with the event -- Task 2's widened scan, at gameplay level.
          HU.testCase "CR 603.2b the end step's beginning is what fires it" $ do
            khabalGhoul <- Registry.printing registry "Khabál Ghoul"
            let (_, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
                quiet = settle gs0
                fired = settle (beginEndStep quiet)
                isTrigger oid = case Game.lookupObject oid fired of
                  Just obj -> case Object.source obj of
                    Source.OfTrigger _ _ -> True
                    _ -> False
                  Nothing -> False
            HU.assertEqual "nothing before the step began" [] (GameState.stack quiet)
            HU.assertEqual "one trigger once it did" 1 (length (filter isTrigger (GameState.stack fired)))
        ]

-- Tidal Wave {2}{U} Instant: "Create a 5/5 blue Wall creature token with defender.
-- Sacrifice it at the beginning of the next end step." CR 603.7c's object-bound
-- delayed ability -- "it" must survive the resolution that armed it.
delayedTests :: Registry.Type.Registry -> Tasty.TestTree
delayedTests registry =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan endStep S.alice) (gs {GameState.phase = endStep})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      walls gs = filter (\oid -> Set.member Subtype.Wall (Projection.subtypesOf oid gs)) (Set.toList (GameState.battlefield gs))
   in Tasty.testGroup
        "DelayedTrigger"
        [ HU.testCase "CR 111.3 the spell mints a 5/5 Wall with defender and arms one delayed ability" $ do
            tidalWave <- Registry.printing registry "Tidal Wave"
            island <- Registry.printing registry "Island"
            let after = castWave tidalWave island
            case walls after of
              [wall] -> do
                HU.assertEqual "5 power" (Just 5) (Projection.powerOf wall after)
                HU.assertEqual "5 toughness" (Just 5) (Projection.toughnessOf wall after)
                HU.assertBool "defender" (Projection.hasKeyword Keyword.Type.Defender wall after)
                HU.assertEqual "one delayed ability waiting" 1 (Seq.length (GameState.delayedTriggers after))
              other -> HU.assertFailure ("expected exactly one Wall token, got " <> show (length other)),
          -- CR 603.7b: "only once, the next time its trigger event occurs".
          HU.testCase "CR 603.7 the token is sacrificed at the beginning of the next end step" $ do
            tidalWave <- Registry.printing registry "Tidal Wave"
            island <- Registry.printing registry "Island"
            let after = resolveAll (settle (beginEndStep (castWave tidalWave island)))
            HU.assertEqual "no Wall left" [] (walls after)
            HU.assertEqual "the store is empty" 0 (Seq.length (GameState.delayedTriggers after)),
          HU.testCase "CR 603.7b a second end step does not re-fire it" $ do
            tidalWave <- Registry.printing registry "Tidal Wave"
            island <- Registry.printing registry "Island"
            let once = resolveAll (settle (beginEndStep (castWave tidalWave island)))
                again = settle (beginEndStep once)
            HU.assertEqual "nothing on the stack" [] (GameState.stack again),
          -- CR 603.7a: a delayed ability does not trigger on an event that
          -- happened BEFORE it was created. Falls out of the watermark for free.
          HU.testCase "CR 603.7a armed during an end step, it waits for the NEXT one" $ do
            tidalWave <- Registry.printing registry "Tidal Wave"
            island <- Registry.printing registry "Island"
            let (gs0, oid) = S.handOne tidalWave (S.landsInPlay island 3)
                inEndStep = settle (beginEndStep gs0)
                cast = resolveAll (snd (Engine.runGamePure S.identityAnswer inEndStep (Cast.castSpell S.alice oid)))
                sameStep = settle cast
                nextStep = resolveAll (settle (beginEndStep sameStep))
            HU.assertEqual "still alive during the step it was armed in" 1 (length (walls sameStep))
            HU.assertEqual "sacrificed at the next end step" [] (walls nextStep),
          -- CR 603.7c: the ability still triggers and is still consumed even when
          -- the object it remembers is gone.
          HU.testCase "CR 603.7c with the token already gone the ability does nothing and is consumed" $ do
            tidalWave <- Registry.printing registry "Tidal Wave"
            island <- Registry.printing registry "Island"
            let armed = castWave tidalWave island
                killed = case walls armed of
                  wall : _ -> S.settleSba (S.runPure S.identityAnswer armed (Event.destroy wall))
                  [] -> armed
                after = resolveAll (settle (beginEndStep killed))
            HU.assertEqual "no Wall" [] (walls after)
            HU.assertEqual "the store is still emptied" 0 (Seq.length (GameState.delayedTriggers after))
            HU.assertEqual "nothing stuck on the stack" [] (GameState.stack after),
          -- IMPORTANT-1 (fix pass 1): Engine.placeOne merges a delayed ability's
          -- OWN placement-time bindings (its chosen modes/targets, chosen just now)
          -- with the environment CAPTURED when the ability was armed, under
          -- Map.union -- left-biased, so the argument ORDER decides which side
          -- wins a collision on a reserved slot such as Binding.chosenModes. The
          -- two DO collide in practice: Pawl.Cast builds an arming spell's
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
          HU.testCase "CR 603.7c placement-time's own chosen mode wins a collision with the captured environment" $
            let onlyMode = Mode.MkMode {Mode.effects = Seq.empty, Mode.targetSpecs = Map.empty}
                ability =
                  TriggeredAbility.MkTriggeredAbility
                    { TriggeredAbility.condition = TriggerCondition.SelfEnters,
                      TriggeredAbility.modal = Modal.MkModal {Modal.modes = Seq.singleton onlyMode, Modal.selection = ModeSelection.ChooseExactly 1},
                      TriggeredAbility.intervening = Nothing
                    }
                -- Stands in for a modal arming spell's own captured chosenModes --
                -- built with the SAME Binding.fromChoices Cast.castSpell uses, so
                -- the collision is the real production shape, not a fabricated one.
                captured = Binding.fromChoices Map.empty Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 7))
                pending = PendingTrigger.MkPendingTrigger (ObjectId.MkObjectId 0) S.alice ability captured
                after = snd (Engine.runGamePure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Engine.placeOne pending))
                placedModes = case GameState.stack after of
                  placedId : _ -> case Game.lookupObject placedId after of
                    Just obj -> Binding.modesOf (Object.bindings obj)
                    Nothing -> Set.empty
                  [] -> Set.empty
             in HU.assertEqual "the ability's own mode (0), not the captured spell's mode (7)" (Set.singleton (ModeIndex.MkModeIndex 0)) placedModes,
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
          HU.testCase "CR 800.4d a departed player's delayed ability triggers, is consumed, and is not put on the stack" $ do
            tidalWave <- Registry.printing registry "Tidal Wave"
            island <- Registry.printing registry "Island"
            let (_, l1) = S.addCreature island S.bob S.threePlayerGame
                (_, l2) = S.addCreature island S.bob l1
                (_, l3) = S.addCreature island S.bob l2
                (waveId, l4) = S.addHandCard tidalWave S.bob l3
                ready = l4 {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.bob, GameState.priority = Just S.bob}
                cast = S.runPure S.identityAnswer ready (Cast.castSpell S.bob waveId)
                armed = S.runPure S.identityAnswer cast Engine.priorityLoop
                gone = Departure.depart Departure.Type.Conceded S.bob armed
                began = Event.recordEvent (GameEvent.StepBegan endStep S.alice) (gone {GameState.phase = endStep})
                (placedAny, placed) = S.runPureWith S.identityAnswer began Engine.placePendingTriggers
                (controlAny, control) = S.runPureWith S.identityAnswer (Event.recordEvent (GameEvent.StepBegan endStep S.alice) (armed {GameState.phase = endStep})) Engine.placePendingTriggers
            HU.assertEqual "the fixture really armed one delayed ability" 1 (Seq.length (GameState.delayedTriggers armed))
            HU.assertEqual "bob's ability is not put on the stack" [] (GameState.stack placed)
            HU.assertEqual "CR 603.7b: it still triggered, so its one shot is spent" 0 (Seq.length (GameState.delayedTriggers placed))
            HU.assertEqual "with bob still in the game the SAME ability IS placed -- the filter is what did it" 1 (length (GameState.stack control))
            HU.assertEqual "nothing reached the stack, so placePendingTriggers honestly reports it placed nothing" False placedAny
            HU.assertEqual "with bob still in the game, something genuinely got placed" True controlAny
        ]

-- CR 603.3b: "that player puts them on the stack in any order they choose". The
-- centerpiece: two triggers, one controller, and an order that changes the answer.
orderingTests :: Registry.Type.Registry -> Tasty.TestTree
orderingTests registry =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan endStep S.alice) (gs {GameState.phase = endStep})
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      countersOn oid gs = maybe 0 (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      -- alice has Khabál Ghoul out and casts Tidal Wave, so both a delayed
      -- sacrifice and a step trigger are pending at the same end step.
      boardOf tidalWave khabalGhoul island =
        let (gs0, waveId) = S.handOne tidalWave (S.landsInPlay island 3)
            (ghoul, gs1) = S.addCreature khabalGhoul S.alice gs0
            cast = resolveAll (snd (Engine.runGamePure S.identityAnswer gs1 (Cast.castSpell S.alice waveId)))
         in (ghoul, beginEndStep cast)
      -- The source of the OTHER pending trigger: Tidal Wave's delayed ability,
      -- whose source is the resolved spell's id rather than any permanent.
      otherThan ghoul gs =
        let sources = fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedEvents gs) gs))
         in case filter (/= ghoul) sources of
              oid : _ -> oid
              [] -> ghoul
      -- An answerer that puts a chosen source LAST on the stack, so it resolves
      -- FIRST (CR 603.3b's answer is the order they are PUT on the stack).
      orderLast :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      orderLast wanted p = case p of
        Prompt.OrderTriggers _ _ sources ->
          let indexed = zip [0 :: Int ..] sources
              pick keep = fmap (fromIntegral . fst) (filter (\entry -> (snd entry == wanted) == keep) indexed)
           in pick False <> pick True
        _ -> S.identityAnswer p
      -- Counts how many times the ordering prompt was asked, answering canonically.
      countingAnswer :: Prompt.Prompt r -> State.State Int r
      countingAnswer p = case p of
        Prompt.OrderTriggers _ _ sources -> do
          State.modify' (+ 1)
          pure (fmap fromIntegral (take (length sources) [0 :: Int ..]))
        _ -> pure (S.identityAnswer p)
   in Tasty.testGroup
        "TriggerOrdering"
        [ HU.testCase "CR 603.3b two triggers under one controller ask for an order, exactly once" $ do
            tidalWave <- Registry.printing registry "Tidal Wave"
            island <- Registry.printing registry "Island"
            khabalGhoul <- Registry.printing registry "Khabál Ghoul"
            let (_, gs) = boardOf tidalWave khabalGhoul island
                (_, asked) = State.runState (Engine.runGame countingAnswer gs Engine.settleForPriority) 0
            HU.assertEqual "asked once" 1 asked,
          -- Sacrifice resolves FIRST: the Wall token dies, and CR 608.2h has the
          -- Ghoul count it when its own effect is applied. The token has NO printed
          -- card (CR 111.1) and its death happened at a boundary the scan already
          -- passed -- so a re-derived type line or a drained queue both read zero.
          --
          -- orderLast's argument is the source PUT LAST on the stack, i.e. the one
          -- that RESOLVES FIRST (CR 603.3b, see orderLast's own comment above): for
          -- the sacrifice to resolve first, the OTHER (non-Ghoul) trigger is the one
          -- named -- not the Ghoul itself.
          HU.testCase "CR 608.2h sacrificing first makes the Ghoul count the token" $ do
            tidalWave <- Registry.printing registry "Tidal Wave"
            island <- Registry.printing registry "Island"
            khabalGhoul <- Registry.printing registry "Khabál Ghoul"
            let (ghoul, gs) = boardOf tidalWave khabalGhoul island
                after = snd (Engine.runGamePure (orderLast (otherThan ghoul gs)) gs Engine.priorityLoop)
            HU.assertEqual "the token was counted" 1 (countersOn ghoul after),
          -- The Ghoul resolves FIRST: the token is still alive, so it is not
          -- counted. Same board, same cards, opposite answer -- which is what makes
          -- the ordering a genuine choice rather than a formality.
          HU.testCase "CR 608.2h counting first means the token is still alive and is not counted" $ do
            tidalWave <- Registry.printing registry "Tidal Wave"
            island <- Registry.printing registry "Island"
            khabalGhoul <- Registry.printing registry "Khabál Ghoul"
            let (ghoul, gs) = boardOf tidalWave khabalGhoul island
                after = snd (Engine.runGamePure (orderLast ghoul) gs Engine.priorityLoop)
            HU.assertEqual "nothing counted" 0 (countersOn ghoul after),
          -- M-1 (review): permute's reject-not-repair guard, pinned directly. The
          -- centerpiece above only ever answers with a valid permutation (via
          -- orderLast/countingAnswer), and the canonical-answer tests elsewhere use
          -- the identity -- so nothing exercises the fallback branch. "Rejected"
          -- means the input list comes back verbatim: nothing dropped, nothing
          -- duplicated.
          HU.testCase "permute applies a genuine permutation" $
            HU.assertEqual "reordered" "cba" (Engine.permute "abc" [2, 1, 0]),
          HU.testCase "permute rejects a short answer, keeping the canonical order" $
            HU.assertEqual "unchanged" "abc" (Engine.permute "abc" [1, 0]),
          HU.testCase "permute rejects a duplicate index, keeping the canonical order" $
            HU.assertEqual "unchanged" "abc" (Engine.permute "abc" [0, 0, 1]),
          HU.testCase "permute rejects an out-of-range index, keeping the canonical order" $
            HU.assertEqual "unchanged" "abc" (Engine.permute "abc" [0, 1, 5]),
          -- M-2 (review): apnapPlayers rotates the turn order to start at the active
          -- player and filters to controllers with a pending trigger -- genuinely new
          -- behaviour versus M3f's apnapOrder, which never consulted turn order at
          -- all, and untested where two DIFFERENT players each control a trigger.
          -- Barbarian Outcast's state trigger (CR 603.8) needs no event, so one
          -- Outcast under EACH player, both controlling no Swamps, gives two
          -- controllers with one trigger apiece -- fewer than two each, so no
          -- ordering prompt is asked and the test isolates the cross-controller walk.
          HU.testCase "CR 101.4/603.3b the active player's trigger is placed first (bottom of stack)" $ do
            barbarianOutcast <- Registry.printing registry "Barbarian Outcast"
            let gs0 = Setup.emptyGame S.bothPlayers
                (_, gs1) = S.addCreature barbarianOutcast S.alice gs0
                (_, gs2) = S.addCreature barbarianOutcast S.bob gs1
                placed = snd (Engine.runGamePure S.identityAnswer gs2 Engine.placePendingTriggers)
                controllerOf oid = fmap Object.owner (Game.lookupObject oid placed)
                stack = GameState.stack placed
            case stack of
              [top, bottom] -> do
                HU.assertEqual "the OTHER player's trigger is on top -- placed second" (Just S.bob) (controllerOf top)
                HU.assertEqual "the active player's (alice's) trigger is at the bottom -- placed first" (Just S.alice) (controllerOf bottom)
              other -> HU.assertFailure ("expected exactly two triggers on the stack, got " <> show (length other)),
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
          HU.testCase "CR 101.4/603.3b APNAP orders the two remaining players' triggers starting at the active player, and a departed seat's permanent is gone with it" $ do
            barbarianOutcast <- Registry.printing registry "Barbarian Outcast"
            let gs0 = Setup.emptyGame S.threePlayers
                (_, gs1) = S.addCreature barbarianOutcast S.alice gs0
                (bobsOutcast, gs2) = S.addCreature barbarianOutcast S.bob gs1
                (_, gs3) = S.addCreature barbarianOutcast S.carol gs2
                gone = Departure.depart Departure.Type.Conceded S.bob gs3
                placed = snd (Engine.runGamePure S.identityAnswer gone Engine.placePendingTriggers)
                controllerOf oid = fmap Object.owner (Game.lookupObject oid placed)
            HU.assertBool "the fixture really gave bob one" (Maybe.isJust (Game.lookupObject bobsOutcast gs3))
            HU.assertEqual "CR 800.4a: bob's Outcast left the game with him, so it has no trigger to place" Nothing (Game.lookupObject bobsOutcast gone)
            case GameState.stack placed of
              [top, bottom] -> do
                HU.assertEqual "carol's trigger is on top -- placed second" (Just S.carol) (controllerOf top)
                HU.assertEqual "the active player's (alice's) is at the bottom -- placed first" (Just S.alice) (controllerOf bottom)
              other -> HU.assertFailure ("expected exactly two triggers on the stack, got " <> show (length other))
        ]

-- Sarcomancy {B} Enchantment: "When this enchantment enters, create a 2/2 black
-- Zombie creature token. At the beginning of your upkeep, if there are no Zombies
-- on the battlefield, this enchantment deals 1 damage to you."
interveningTests :: Registry.Type.Registry -> Tasty.TestTree
interveningTests registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      beginUpkeep gs = Event.recordEvent (GameEvent.StepBegan upkeep S.alice) (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      zombies gs = filter (\oid -> Set.member Subtype.Zombie (Projection.subtypesOf oid gs)) (Set.toList (GameState.battlefield gs))
      -- Sarcomancy enters and its ETB resolves, so a Zombie token is out.
      withZombie sarcomancy =
        let (sarcId, gs0) = S.addCreature sarcomancy S.alice (Setup.emptyGame S.bothPlayers)
            entered = ZoneChange.MkZoneChange sarcId Zone.Stack Zone.Battlefield
            gs1 = S.withEvents [GameEvent.Moved entered (Projection.project sarcId gs0)] gs0
         in (sarcId, resolveAll (settle gs1))
   in Tasty.testGroup
        "InterveningIf"
        [ HU.testCase "CR 603.6a the enters trigger makes a 2/2 black Zombie token" $ do
            sarcomancy <- Registry.printing registry "Sarcomancy"
            let (_, after) = withZombie sarcomancy
            case zombies after of
              [tok] -> do
                HU.assertEqual "2 power" (Just 2) (Projection.powerOf tok after)
                HU.assertEqual "black" (Set.singleton Color.Black) (Projection.colorsOf tok after)
              other -> HU.assertFailure ("expected exactly one Zombie token, got " <> show (length other)),
          -- CR 603.4: with the condition FALSE, the ability does not trigger AT ALL
          -- -- nothing reaches the stack.
          HU.testCase "CR 603.4 with a Zombie out, the upkeep ability does not trigger" $ do
            sarcomancy <- Registry.printing registry "Sarcomancy"
            let (_, board) = withZombie sarcomancy
                atUpkeep = settle (beginUpkeep board)
            HU.assertEqual "nothing on the stack" [] (GameState.stack atUpkeep)
            HU.assertEqual "no life lost" (Just 20) (S.lifeOf S.alice atUpkeep),
          HU.testCase "CR 603.4 with no Zombie, it triggers and deals 1 to its controller" $ do
            sarcomancy <- Registry.printing registry "Sarcomancy"
            let (_, board) = withZombie sarcomancy
                killed = case zombies board of
                  tok : _ -> S.settleSba (S.runPure S.identityAnswer board (Event.destroy tok))
                  [] -> board
                after = resolveAll (settle (beginUpkeep killed))
            HU.assertEqual "alice took 1" (Just 19) (S.lifeOf S.alice after),
          -- CR 608.2a: the case that distinguishes an intervening "if" from a plain
          -- condition. The ability triggered legitimately; a Zombie appearing in
          -- RESPONSE makes it do nothing on resolution.
          HU.testCase "CR 608.2a a Zombie made in response makes the trigger resolve doing nothing" $ do
            sarcomancy <- Registry.printing registry "Sarcomancy"
            piker <- Registry.printing registry "Goblin Piker"
            let (_, board) = withZombie sarcomancy
                killed = case zombies board of
                  tok : _ -> S.settleSba (S.runPure S.identityAnswer board (Event.destroy tok))
                  [] -> board
                onStack = settle (beginUpkeep killed)
                -- The Zombie arrives under BOB's control, which is exactly the
                -- point: CR 603.4's clause is "no Zombies on the battlefield",
                -- not "no Zombies you control".
                responded = snd (S.addToken (zombieTokenOf sarcomancy piker) S.bob onStack)
                after = resolveAll responded
            HU.assertBool "the trigger really was on the stack" (not (null (GameState.stack onStack)))
            HU.assertEqual "no damage on resolution" (Just 20) (S.lifeOf S.alice after)
        ]

-- The 2/2 black Zombie Sarcomancy's own ETB mints, read back out of the card data
-- so the "in response" fixture makes the same object the card would.
zombieTokenOf :: Printing.Printing -> Printing.Printing -> Card.Type.Card
zombieTokenOf sarcomancy pikerFallback =
  let created effect = case effect of
        Effect.Create _ card _ -> Just card
        _ -> Nothing
      abilityEffects = concatMap (Modal.allEffects . TriggeredAbility.modal) (Card.Type.triggeredAbilities (Printing.card sarcomancy))
   in case Maybe.mapMaybe created abilityEffects of
        card : _ -> card
        [] -> Printing.card pikerFallback

-- CR 702.70: poisonous -- the first keyword whose rule text IS a triggered
-- ability, so it is minted by Pawl.Keyword and gathered by the same
-- Pawl.Event.eventTriggers scan a printed trigger goes through, with the damaged
-- player carried across in the reserved Binding.triggerPlayer slot.
poisonousTests :: Registry.Type.Registry -> Tasty.TestTree
poisonousTests registry =
  let -- Hang `n` Auras off `host`, each owned by alice. Attached directly rather
      -- than cast: the cast path is proved once, by the whole-card test below.
      hang printing n host gs =
        foldl
          (\g _ -> let (aura, g1) = S.addCreature printing S.alice g in S.attach aura host g1)
          gs
          (replicate n ())
      -- alice attacks with one `attacking` creature wearing `n` copies of the
      -- `aura`; bob defends with one creature per printing in `theirs`.
      board attacking aura n theirs = case S.combatBoardOf [attacking] theirs of
        (gs, attacker : _, blockers) -> Just (hang aura n attacker gs, attacker, blockers)
        _ -> Nothing
   in Tasty.testGroup
        "Poisonous"
        [ -- CR 702.70b: "If a creature has multiple instances of poisonous, each
          -- triggers separately." So the count is a MULTIPLICITY, not a sum --
          -- the opposite of CR 702.164b's toxic, which sums its N values into one
          -- rider. The falsifier is a mint that collapses the count to one
          -- ability.
          HU.testCase "CR 702.70b each instance of poisonous is its own ability" $ do
            HU.assertEqual
              "poisonous 1 held twice is two abilities"
              [Keyword.poisonous 1, Keyword.poisonous 1]
              (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Poisonous 1) 2))
            HU.assertEqual
              "and poisonous 3 once is one"
              [Keyword.poisonous 3]
              (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Poisonous 3) 1)),
          -- Rule 702.70 is the only keyword in the pool that mints an ability;
          -- every other one is read where it matters (Projection.hasKeyword, the
          -- infect/toxic damage riders), so it must mint nothing here.
          HU.testCase "CR 702.164 toxic mints no triggered ability" $
            HU.assertEqual
              "toxic is a damage rider, not a trigger"
              []
              (Keyword.triggeredAbilitiesOf (Map.fromList [(Keyword.Type.Toxic 2, 1), (Keyword.Type.Flying, 1), (Keyword.Type.Infect, 1)])),
          -- CR 702.70a's "that player": the trigger's own event names them, and
          -- the scan stamps them under the reserved slot as it gathers. The
          -- falsifier is an implementation that hands the poison to the ability's
          -- controller (Binding.you) instead.
          HU.testCase "CR 603.2 the damaged player rides the trigger in the reserved slot" $
            let ev = GameEvent.DamageDealt (DamageEvent.MkDamageEvent (ObjectId.MkObjectId 7) (Recipient.ToPlayer S.bob) 2 False False 0 DamageKind.Combat)
                bindings = Event.eventBindings TriggerCondition.SelfDealsCombatDamageToPlayer ev
             in HU.assertEqual
                  "bob is bound under thatPlayer"
                  (Map.singleton Binding.triggerPlayer (Recipient.ToPlayer S.bob))
                  (Binding.targetsOf bindings),
          -- The proving test. CR 702.70a: "Whenever this creature deals combat
          -- damage to a player, that player gets N poison counters." bob is dealt
          -- the Piker's two damage AND gets three poison -- poisonous is not
          -- infect (CR 702.90b), so the life still goes.
          HU.testCase "CR 702.70a Snake Cult Initiation gives the damaged player three poison" $ do
            piker <- Registry.printing registry "Goblin Piker"
            initiation <- Registry.printing registry "Snake Cult Initiation"
            case board piker initiation 1 [] of
              Nothing -> HU.assertFailure "fixture should have an attacker"
              Just (gs, attacker, _) -> do
                HU.assertBool "the enchanted creature has poisonous 3" (Projection.hasKeyword (Keyword.Type.Poisonous 3) attacker gs)
                let after = S.runCombat S.aggressiveAnswer gs
                HU.assertEqual "bob has three poison" 3 (S.playerCounterOf PlayerCounterKind.Poison S.bob after)
                HU.assertEqual "and lost the two life as well" (Just 18) (S.lifeOf S.bob after)
                HU.assertEqual "alice, who controls the ability, gets none" 0 (S.playerCounterOf PlayerCounterKind.Poison S.alice after),
          -- What separates poisonous from infect and toxic: it is a TRIGGERED
          -- ability, so the poison arrives when the ability resolves, not as the
          -- damage is dealt. `fightWith` deals combat damage without ever reaching
          -- a priority boundary, so nothing has been gathered yet.
          HU.testCase "CR 702.70a the poison rides the stack, not the damage" $ do
            piker <- Registry.printing registry "Goblin Piker"
            initiation <- Registry.printing registry "Snake Cult Initiation"
            case board piker initiation 1 [] of
              Nothing -> HU.assertFailure "fixture should have an attacker"
              Just (gs, _, _) -> do
                let fought = S.fightWith S.aggressiveAnswer gs
                HU.assertEqual "damage is dealt" (Just 18) (S.lifeOf S.bob fought)
                HU.assertEqual "but no poison until the trigger resolves" 0 (S.playerCounterOf PlayerCounterKind.Poison S.bob fought),
          -- CR 702.70b at the board level: two Auras are two poisonous 3
          -- abilities, so two triggers and six counters. The falsifier is a
          -- projection that keeps keywords in a set -- the second grant collapses
          -- into the first and bob takes three.
          HU.testCase "CR 702.70b two Snake Cult Initiations trigger separately for six poison" $ do
            piker <- Registry.printing registry "Goblin Piker"
            initiation <- Registry.printing registry "Snake Cult Initiation"
            case board piker initiation 2 [] of
              Nothing -> HU.assertFailure "fixture should have an attacker"
              Just (gs, _, _) -> do
                let after = S.runCombat S.aggressiveAnswer gs
                HU.assertEqual "bob has six poison" 6 (S.playerCounterOf PlayerCounterKind.Poison S.bob after),
          -- CR 702.70a is scoped to combat damage dealt TO A PLAYER: a blocked
          -- creature deals its damage to the blocker, so the ability never
          -- triggers and the blocker (not being a player) gets nothing either.
          HU.testCase "CR 702.70a a blocked creature poisons nobody" $ do
            piker <- Registry.printing registry "Goblin Piker"
            initiation <- Registry.printing registry "Snake Cult Initiation"
            case board piker initiation 1 [piker] of
              Nothing -> HU.assertFailure "fixture should have an attacker"
              Just (gs, _, _) -> do
                let after = S.runCombat S.aggressiveAnswer gs
                HU.assertEqual "bob has no poison" 0 (S.playerCounterOf PlayerCounterKind.Poison S.bob after)
                HU.assertEqual "and lost no life" (Just 20) (S.lifeOf S.bob after),
          -- CR 613.1f / 613 layer 6: the ability is derived from the POST-LAYER
          -- keywords, so Humility's LoseAllAbilities (a later timestamp, so it
          -- applies after the Aura's grant) takes it away with no arm of its own.
          -- The falsifier is a mint that reads the PRINTED keywords or the Aura's
          -- own static ability instead of the projection.
          HU.testCase "CR 613 Humility strips poisonous along with everything else" $ do
            piker <- Registry.printing registry "Goblin Piker"
            initiation <- Registry.printing registry "Snake Cult Initiation"
            humility <- Registry.printing registry "Humility"
            case board piker initiation 1 [] of
              Nothing -> HU.assertFailure "fixture should have an attacker"
              Just (gs0, attacker, _) -> do
                let gs = S.withHumility humility gs0
                HU.assertBool "the keyword is gone" (not (Projection.hasKeyword (Keyword.Type.Poisonous 3) attacker gs))
                let after = S.runCombat S.aggressiveAnswer gs
                HU.assertEqual "so bob takes no poison" 0 (S.playerCounterOf PlayerCounterKind.Poison S.bob after)
                HU.assertEqual "only the 1/1's one damage" (Just 19) (S.lifeOf S.bob after),
          -- CR 702.70a's "that player" is whoever was DEALT the damage. In a
          -- multiplayer game (CR 800.1) that is not derivable from the ability's
          -- controller, since CR 506.2a has the attacking player choose which
          -- opponent becomes the defending player. The two runs differ only in
          -- the answer to
          -- Prompt.ChooseDefender, so a "give it to the opponent" implementation
          -- cannot pass both.
          HU.testCase "CR 702.70a the poison follows whichever opponent was attacked" $ do
            piker <- Registry.printing registry "Goblin Piker"
            initiation <- Registry.printing registry "Snake Cult Initiation"
            case S.threePlayerCombat [piker] [] [] of
              (_, [], _, _) -> HU.assertFailure "fixture should have an attacker"
              (base, attacker : _, _, _) -> do
                let gs = hang initiation 1 attacker base
                    hitBob = S.runCombat (S.attackTo S.bob) gs
                    hitCarol = S.runCombat (S.attackTo S.carol) gs
                HU.assertEqual "bob, attacked, has three poison" 3 (S.playerCounterOf PlayerCounterKind.Poison S.bob hitBob)
                HU.assertEqual "carol, untouched, has none" 0 (S.playerCounterOf PlayerCounterKind.Poison S.carol hitBob)
                HU.assertEqual "and the other way round" 3 (S.playerCounterOf PlayerCounterKind.Poison S.carol hitCarol)
                HU.assertEqual "bob untouched this time" 0 (S.playerCounterOf PlayerCounterKind.Poison S.bob hitCarol),
          -- The whole card, through the real cast path (design.md section 4): pay
          -- {3}{B}, target the Piker, let the Aura enter attached (CR 303.4), then
          -- attack. Everything above hangs the Aura on by fiat.
          HU.testCase "CR 702.70 whole card: cast Snake Cult Initiation, attack, and bob is poisoned" $ do
            piker <- Registry.printing registry "Goblin Piker"
            swamp <- Registry.printing registry "Swamp"
            initiation <- Registry.printing registry "Snake Cult Initiation"
            case S.combatBoardOf [piker] [] of
              (_, [], _) -> HU.assertFailure "fixture should have an attacker"
              (gs0, attacker : _, _) -> do
                let withSwamps = foldl (\g _ -> snd (S.addCreature swamp S.alice g)) gs0 (replicate 4 ())
                    (spellId, inHand) = S.addHandCard initiation S.alice withSwamps
                    cast = S.runPure S.aggressiveAnswer inHand {GameState.priority = Just S.alice} (Cast.castSpell S.alice spellId)
                    resolved = S.runPure S.aggressiveAnswer cast Stack.resolveTop
                    after = S.runCombat S.aggressiveAnswer resolved
                HU.assertBool "the Aura granted poisonous 3" (Projection.hasKeyword (Keyword.Type.Poisonous 3) attacker resolved)
                HU.assertEqual "bob has three poison" 3 (S.playerCounterOf PlayerCounterKind.Poison S.bob after)
                HU.assertEqual "and took the Piker's two" (Just 18) (S.lifeOf S.bob after)
        ]

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry = Tasty.testGroup "Pawl.TriggerSpec" [logTests registry, scanTests registry, sacrificeTests registry, stateTriggerTests registry, historyTests registry, delayedTests registry, orderingTests registry, interveningTests registry, poisonousTests registry]
