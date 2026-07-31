-- Covers M4.5 P4 in full. The turn-scoped event log (Pawl.Types.GameEvent,
-- GameState's log and watermarks) -- append-only recording, watermark-based
-- consumption per reader (trigger scan, SBA damage check), and the log's
-- turn-scoped clearing at handoff -- `logTests`. The CR 603.2b step-beginning
-- event and the CR 603.6a widened scan (every battlefield permanent, not just
-- an enters event's newcomer) -- `scanTests`. The `Sacrifice` opcode and its
-- reserved trigger-source slot, CR 701.21 -- `sacrificeTests`. CR 603.6a's
-- OTHER written form, "whenever a [type] enters", with Soul Warden --
-- `permanentEntersTests`. CR 603.8 state
-- triggers -- `stateTriggerTests`. CR 608.2i turn history (Khabál Ghoul's
-- "died this turn") -- `historyTests`. CR 603.7 delayed triggered abilities
-- -- `delayedTests`. The CR 603.3b ordering prompt -- `orderingTests`, and its
-- CR 725.2 sourceless case (the monarch's inherent triggers ordered WITH the
-- batch) -- `monarchOrderingTests`. The CR 603.4 / 608.2a intervening "if" --
-- `interveningTests`. Also Pawl.Keyword: CR
-- 702.70 poisonous, the keyword whose rule text IS a triggered ability, and the
-- reserved "that player" slot the scan stamps for it -- `poisonousTests`. CR
-- 113.6k's non-battlefield scan -- the graveyard, with Tome Scour milling
-- Narcomoeba -- `graveyardTriggerTests`. CR 400.7e's OTHER reference inside a
-- look-back trigger, the card it became in the first zone it went to, with
-- Endless Cockroaches -- `becameSlotTests`, which also pins
-- Event.eventBindingSlots (the per-condition slot set the card lint asks)
-- against the keys eventBindings actually stamps. CR 603.4's intervening "if"
-- read against a source that no longer exists (CR 608.2h), with Deathknell Berserker
-- -- `lookBackInterveningTests`. CR 603.10's first sentence for a BYSTANDER -- a
-- permanent that was on the battlefield when some OTHER event in the same batch
-- happened and is gone by the CR 117.5 boundary -- with Lightning Skelemental
-- and Khabál Ghoul -- `bystanderTests`. The same CR 400.7e slot read from the ENTRY
-- direction, where the entrant is a different card from the bearer, with Aether
-- Flash -- `aetherFlashTests`.
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
import qualified Pawl.Activate as Activate
import qualified Pawl.Binding as Binding
import qualified Pawl.Cast as Cast
import qualified Pawl.Cost as Cost
import qualified Pawl.Departure as Departure
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Expiry as Expiry
import qualified Pawl.Game as Game
import qualified Pawl.Keyword as Keyword
import qualified Pawl.Mana as Mana
import qualified Pawl.Modal as Modal
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Comparison as Comparison
-- Aliased Condition.Type, not Condition, per the project-wide convention
-- (CardSpec's note): the evaluator module Pawl.Condition may later be imported
-- and must not collide.
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Expiry as Expiry.Type
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Registry as Registry.Type
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange
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
            entered = ZoneChange.MkZoneChange ripId ripId Zone.Stack Zone.Battlefield
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
                Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice cond (GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice)
              HU.assertBool "the upkeep does not" $
                not (Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice cond (GameEvent.StepBegan (Phase.Beginning BeginningStep.Upkeep) S.alice)),
      -- CR 603.3a / 109.5: "your upkeep" is the ABILITY CONTROLLER's (603.3a
      -- controls the ability; 109.5 makes "your" mean that controller), so the
      -- scope is read against the bearer's controller, not the card.
      HU.testCase "CR 603.3a ControllersTurn matches only the bearer's controller's turn" $
        let bearer = ObjectId.MkObjectId 1
            cond = TriggerCondition.StepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn
         in do
              HU.assertBool "alice's upkeep matches for alice" $
                Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice cond (GameEvent.StepBegan (Phase.Beginning BeginningStep.Upkeep) S.alice)
              HU.assertBool "bob's upkeep does not" $
                not (Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice cond (GameEvent.StepBegan (Phase.Beginning BeginningStep.Upkeep) S.bob)),
      -- The widening falsifier: the scan now visits every battlefield permanent,
      -- so SelfEnters must ask whether the event is about THIS permanent. Rest in
      -- Peace is on the battlefield and a DIFFERENT object entered.
      HU.testCase "CR 603.6a a SelfEnters trigger does not fire on another object's entry" $ do
        restInPeace <- Registry.printing registry "Rest in Peace"
        piker <- Registry.printing registry "Goblin Piker"
        let (_, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
            (pikerId, gs1) = S.addCreature piker S.bob gs0
            entered = ZoneChange.MkZoneChange pikerId pikerId Zone.Stack Zone.Battlefield
            gs2 = S.withEvents [GameEvent.Moved entered (Projection.project pikerId gs1)] gs1
        HU.assertEqual "no trigger" 0 (length (fst (Event.gatherTriggers (Event.unscannedEvents gs2) gs2))),
      HU.testCase "CR 603.6a a SelfEnters trigger still fires on its own entry" $ do
        restInPeace <- Registry.printing registry "Rest in Peace"
        let (ripId, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
            entered = ZoneChange.MkZoneChange ripId ripId Zone.Stack Zone.Battlefield
            gs1 = S.withEvents [GameEvent.Moved entered (Projection.project ripId gs0)] gs0
        case fst (Event.gatherTriggers (Event.unscannedEvents gs1) gs1) of
          [pt] -> do
            HU.assertEqual "source is RiP" (TriggerSource.OfObject ripId) (PendingTrigger.source pt)
            HU.assertEqual "controller is alice" S.alice (PendingTrigger.controller pt)
          other -> HU.assertFailure ("expected exactly one pending trigger, got " <> show (length other)),
      HU.testCase "a graveyard-bound event yields no enters trigger" $ do
        restInPeace <- Registry.printing registry "Rest in Peace"
        let (ripId, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
            toGrave = ZoneChange.MkZoneChange ripId ripId Zone.Battlefield Zone.Graveyard
            gs1 = S.withEvents [GameEvent.Moved toGrave (Projection.project ripId gs0)] gs0
        HU.assertEqual "no triggers" 0 (length (fst (Event.gatherTriggers (Event.unscannedEvents gs1) gs1))),
      HU.testCase "SelfEnters matches only a battlefield destination" $
        let bearer = ObjectId.MkObjectId 1
            movedTo zone = GameEvent.Moved (ZoneChange.MkZoneChange bearer bearer Zone.Stack zone) S.emptyCharacteristics
         in do
              HU.assertBool "enters battlefield matches" $
                Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice TriggerCondition.SelfEnters (movedTo Zone.Battlefield)
              HU.assertBool "enters graveyard does not" $
                not (Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice TriggerCondition.SelfEnters (movedTo Zone.Graveyard)),
      -- CR 508.3a plus Aurelia, the Warleader's "for the first time each turn".
      -- The declaration being matched is already in the log when the scan runs,
      -- so "the first time" is "this is the only one so far".
      HU.testCase "SelfAttacks FirstTimeEachTurn matches only the first declaration" $
        let bearer = ObjectId.MkObjectId 1
            declared = GameEvent.AttackerDeclared bearer
            gsWith events = S.withEvents events (Setup.emptyGame S.bothPlayers)
            matches frequency events =
              Event.matchesTrigger (gsWith events) bearer S.alice (TriggerCondition.SelfAttacks frequency) declared
         in do
              HU.assertBool "the first declaration matches" $
                matches TriggerFrequency.FirstTimeEachTurn [declared]
              HU.assertBool "a second declaration this turn does not" $
                not (matches TriggerFrequency.FirstTimeEachTurn [declared, declared])
              -- Hanweir Garrison's shape is untouched by the narrowing.
              HU.assertBool "EveryTime matches the first" $
                matches TriggerFrequency.EveryTime [declared]
              HU.assertBool "EveryTime matches the second too" $
                matches TriggerFrequency.EveryTime [declared, declared]
              -- The count is per bearer, not per turn: two creatures declared
              -- together are each attacking for the first time.
              HU.assertBool "another creature's declaration does not spend this one's first time" $
                matches TriggerFrequency.FirstTimeEachTurn [GameEvent.AttackerDeclared (ObjectId.MkObjectId 2), declared]
              -- CR 508.3a's last sentence, unchanged by the frequency: a
              -- non-declaration event never matches.
              HU.assertBool "a step beginning is not an attack" $
                not
                  ( Event.matchesTrigger
                      (gsWith [declared])
                      bearer
                      S.alice
                      (TriggerCondition.SelfAttacks TriggerFrequency.FirstTimeEachTurn)
                      (GameEvent.StepBegan (Phase.Combat CombatStep.DeclareAttackers) S.alice)
                  ),
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
            entered1 = ZoneChange.MkZoneChange rip1 rip1 Zone.Stack Zone.Battlefield
            entered2 = ZoneChange.MkZoneChange rip2 rip2 Zone.Stack Zone.Battlefield
            gs2 =
              S.withEvents
                [ GameEvent.Moved entered1 (Projection.project rip1 gs1),
                  GameEvent.Moved entered2 (Projection.project rip2 gs1)
                ]
                gs1
            triggers = fst (Event.gatherTriggers (Event.unscannedEvents gs2) gs2)
        HU.assertBool "rip1 has the lower id" (rip1 < rip2)
        HU.assertEqual "both triggers fired" 2 (length triggers)
        HU.assertEqual "sources in ascending ObjectId order" (fmap TriggerSource.OfObject [rip1, rip2]) (fmap PendingTrigger.source triggers),
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
        HU.assertEqual "sources in ascending ObjectId order" (fmap TriggerSource.OfObject [ghoul1, ghoul2]) (fmap PendingTrigger.source triggers),
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
      HU.testCase "CR 603.10 whole cards: under Night of Souls' Betrayal, Ravenous Rats dies as it enters and STILL makes bob discard" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        ravenousRats <- Registry.printing registry "Ravenous Rats"
        night <- Registry.printing registry "Night of Souls' Betrayal"
        let (_, base1) = S.addCreature night S.alice (S.landsInPlay swamp 2)
            (_, base2) = S.addHandCard piker S.bob base1
            (_, base3) = S.addHandCard piker S.bob base2
            (gs, spellId) = S.handOne ravenousRats base3
            bobBefore = S.handSize S.bob gs
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            settled = snd (Engine.runGamePure S.identityAnswer cast Engine.priorityLoop)
        HU.assertEqual "CR 704.5f buried the 0/0" 0 (S.countOnBattlefieldByName (Text.pack "Ravenous Rats") S.alice settled)
        HU.assertEqual "in alice's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice settled))
        HU.assertEqual "and bob still discarded, exactly once" (bobBefore - 1) (S.handSize S.bob settled),
      -- The no-double-fire half. Same board minus the -1/-1, so the Rats is on
      -- the battlefield at the CR 117.5 boundary AND named by an unscanned
      -- entry event -- the two candidate sources the scan draws from. A count,
      -- not a boolean: a Rats counted twice discards two of bob's two cards.
      HU.testCase "CR 603.6a whole cards: a Ravenous Rats that survives its entry triggers exactly once" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        ravenousRats <- Registry.printing registry "Ravenous Rats"
        let (_, base1) = S.addHandCard piker S.bob (S.landsInPlay swamp 2)
            (_, base2) = S.addHandCard piker S.bob base1
            (gs, spellId) = S.handOne ravenousRats base2
            bobBefore = S.handSize S.bob gs
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            settled = snd (Engine.runGamePure S.identityAnswer cast Engine.priorityLoop)
        HU.assertEqual "the Rats survived" 1 (S.countOnBattlefieldByName (Text.pack "Ravenous Rats") S.alice settled)
        HU.assertEqual "nothing in alice's graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.alice settled))
        HU.assertEqual "bob discarded exactly one" (bobBefore - 1) (S.handSize S.bob settled)
    ]

-- CR 701.21: sacrificing is its own keyword action -- NOT a destruction.
sacrificeTests :: Registry.Type.Registry -> Tasty.TestTree
sacrificeTests registry =
  Tasty.testGroup
    "Sacrifice"
    [ HU.testCase "CR 701.21a a sacrificed permanent goes to its owner's graveyard" $ do
        pikerPrinting <- Registry.printing registry "Goblin Piker"
        let (piker, gs) = S.addCreature pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
            after = S.runPure S.identityAnswer gs (Event.sacrifice S.bob piker)
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
            -- ALICE is the sacrificing player, because she controls it (CR
            -- 701.21a); bob merely owns it, which is what the assertions below
            -- separate.
            after = S.runPure S.identityAnswer gs (Event.sacrifice S.alice piker)
        HU.assertEqual "off bob's battlefield" 0 (S.creaturesInPlay S.bob after)
        HU.assertEqual "in bob's (owner's) graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after))
        HU.assertEqual "not in alice's graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      -- CR 701.21a: "sacrificing a permanent doesn't destroy it", so neither CR
      -- 702.12b's indestructible gate nor CR 701.19a's shield applies.
      HU.testCase "CR 701.21a an indestructible permanent can still be sacrificed" $ do
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        let (myr, gs) = S.addCreature darksteelMyr S.bob (Setup.emptyGame S.bothPlayers)
            after = S.runPure S.identityAnswer gs (Event.sacrifice S.bob myr)
        HU.assertEqual "gone from the battlefield" 0 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 701.21a sacrificing neither consults nor consumes a regeneration shield" $ do
        pikerPrinting <- Registry.printing registry "Goblin Piker"
        let (piker, gs0) = S.addCreature pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
            gs = S.addRegenShield piker gs0
            after = S.runPure S.identityAnswer gs (Event.sacrifice S.bob piker)
        HU.assertEqual "still sacrificed" 0 (S.creaturesInPlay S.bob after)
        HU.assertEqual
          "the shield's source is untouched"
          [piker]
          (fmap ActiveReplacement.source (GameState.replacements after)),
      HU.testCase "only a battlefield permanent can be sacrificed (CR 701.21a)" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (card, gs) = S.addLibraryCard piker S.bob (Setup.emptyGame S.bothPlayers)
            after = S.runPure S.identityAnswer gs (Event.sacrifice S.bob card)
        HU.assertEqual "the library card is untouched" gs after,
      -- CR 701.21a's second clause, which had no enforcement before #44: "A player
      -- can't sacrifice ... a permanent they don't control." Bob controls it;
      -- alice asking is refused outright rather than quietly honoured.
      HU.testCase "CR 701.21a a player cannot sacrifice a permanent they do not control" $ do
        pikerPrinting <- Registry.printing registry "Goblin Piker"
        let (piker, gs) = S.addCreature pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
            byAlice = S.runPure S.identityAnswer gs (Event.sacrifice S.alice piker)
            byBob = S.runPure S.identityAnswer gs (Event.sacrifice S.bob piker)
        HU.assertEqual "alice's attempt changes nothing at all" gs byAlice
        -- The discriminating half: the same call from the controller works, so the
        -- refusal above is the guard and not an unrelated no-op.
        HU.assertEqual "bob's own sacrifice goes through" 0 (S.creaturesInPlay S.bob byBob),
      -- CR 113.7: "this creature" is a slot read, filled at placement.
      HU.testCase "CR 113.7 a placed trigger binds its source into the reserved self slot" $ do
        restInPeace <- Registry.printing registry "Rest in Peace"
        let (ripId, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
            entered = ZoneChange.MkZoneChange ripId ripId Zone.Stack Zone.Battlefield
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
                gone = settle (S.runPure S.identityAnswer quiet (Event.destroy Regenerability.Regenerable [swampOid]))
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
                dead = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs2 (Event.destroy Regenerability.Regenerable [p1])) (Event.destroy Regenerability.Regenerable [p2])
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
                dead = S.settleSba (S.runPure S.identityAnswer gs1 (Event.destroy Regenerability.Regenerable [tok]))
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
                dead = S.runPure S.identityAnswer gs0 (Event.destroy Regenerability.Regenerable [p1])
                (ghoul, gs1) = S.addCreature khabalGhoul S.alice (settle dead)
                atEnd = resolveAll (settle (beginEndStep gs1))
            HU.assertEqual "one +1/+1 counter" 1 (countersOn ghoul atEnd),
          -- CR 608.2i: the history's scope is ONE turn.
          HU.testCase "the count resets at turn handoff, not at the trigger scan" $ do
            khabalGhoul <- Registry.printing registry "Khabál Ghoul"
            piker <- Registry.printing registry "Goblin Piker"
            let (ghoul, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
                (p1, gs1) = S.addCreature piker S.bob gs0
                dead = S.runPure S.identityAnswer gs1 (Event.destroy Regenerability.Regenerable [p1])
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
              Cast.castSpell S.alice oid
              Engine.priorityLoop
          )
          []
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
          -- CR 603.7b's other half: "unless it has a stated duration, such as
          -- 'this turn.'" Tidal Wave's entry is reused with an expiry stamped on
          -- it, so the mechanism is tested without inventing a card; Full
          -- Throttle is the card that actually prints one.
          HU.testCase "CR 603.7b a stated-duration delayed ability stays armed after firing" $ do
            tidalWave <- Registry.printing registry "Tidal Wave"
            island <- Registry.printing registry "Island"
            let armed = castWave tidalWave island
                stated = withExpiry (Just Expiry.Type.AtCleanup) armed
                began = [GameEvent.StepBegan endStep S.alice]
                (firedOnce, survivors) = Event.delayedPending began stated
                (firedAgain, _) = Event.delayedPending began stated {GameState.delayedTriggers = survivors}
            HU.assertEqual "it fired" 1 (length firedOnce)
            HU.assertEqual "and stayed armed" 1 (Seq.length survivors)
            HU.assertEqual "so the next end step fires it again" 1 (length firedAgain),
          HU.testCase "CR 603.7b without a stated duration firing still spends it" $ do
            tidalWave <- Registry.printing registry "Tidal Wave"
            island <- Registry.printing registry "Island"
            let armed = castWave tidalWave island
                (fired, survivors) = Event.delayedPending [GameEvent.StepBegan endStep S.alice] armed
            HU.assertEqual "it fired" 1 (length fired)
            HU.assertEqual "and was evicted" 0 (Seq.length survivors),
          -- CR 514.2: "all 'until end of turn' and 'this turn' effects end"
          -- during the cleanup step -- which is what ends the stated duration,
          -- and the reason an armed entry cannot outlive the turn that made it.
          HU.testCase "CR 514.2 cleanup drops a stated-duration delayed ability" $ do
            tidalWave <- Registry.printing registry "Tidal Wave"
            island <- Registry.printing registry "Island"
            let armed = castWave tidalWave island
                swept expiry = GameState.delayedTriggers (Expiry.dropAtCleanup (withExpiry expiry armed))
            HU.assertEqual "an 'this turn' entry is gone" 0 (Seq.length (swept (Just Expiry.Type.AtCleanup)))
            HU.assertEqual "an end-of-game entry stays" 1 (Seq.length (swept (Just Expiry.Type.Never)))
            -- CR 603.7b's one shot is spent by FIRING, not by time, so an entry
            -- on no duration at all must survive every sweep.
            HU.assertEqual "and a one-shot entry stays" 1 (Seq.length (swept Nothing)),
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
                  wall : _ -> S.settleSba (S.runPure S.identityAnswer armed (Event.destroy Regenerability.Regenerable [wall]))
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
            let onlyMode = Mode.MkMode {Mode.effects = Seq.empty, Mode.targetSpecs = Map.empty, Mode.optionality = Optionality.Mandatory}
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
                pending = PendingTrigger.MkPendingTrigger (TriggerSource.OfObject (ObjectId.MkObjectId 0)) S.alice ability captured
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
            HU.assertEqual "with bob still in the game, something genuinely got placed" True controlAny,
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
          HU.testCase "CR 614.16/603.7c a doubled Create asks which minted token \"it\" names" $ do
            tidalWave <- Registry.printing registry "Tidal Wave"
            island <- Registry.printing registry "Island"
            doublingSeason <- Registry.printing registry "Doubling Season"
            let (_, base) = S.addCreature doublingSeason S.alice (S.landsInPlay island 3)
                (gs, waveId) = S.handOne tidalWave base
                ((_, armed), asked) = castUnderChoice gs waveId
                after = resolveAll (settle (beginEndStep armed))
            HU.assertEqual "the replacement really doubled the Create" 2 (length (walls armed))
            case asked of
              [[unchosen, chosen]] -> do
                HU.assertEqual "the token named by \"it\" was sacrificed, and only it" [unchosen] (walls after)
                HU.assertBool "the chosen Wall is off the battlefield" (Set.notMember chosen (GameState.battlefield after))
              _ -> HU.assertFailure ("expected one prompt offering two tokens, got " <> show asked),
          -- The companion, and the elision this pairs with: without the
          -- replacement the Create mints exactly one token, so "it" has only one
          -- possible referent and there is nothing to ask. Where the rules leave
          -- nothing to ask, don't prompt.
          HU.testCase "CR 603.7c one minted token is no choice, so nothing is asked" $ do
            tidalWave <- Registry.printing registry "Tidal Wave"
            island <- Registry.printing registry "Island"
            let (gs, waveId) = S.handOne tidalWave (S.landsInPlay island 3)
                ((_, armed), asked) = castUnderChoice gs waveId
                after = resolveAll (settle (beginEndStep armed))
            HU.assertEqual "one Wall minted" 1 (length (walls armed))
            HU.assertEqual "no binding prompt was issued" [] asked
            HU.assertEqual "and it is still sacrificed at the end step" [] (walls after),
          -- FILTERED, NOT TRUSTED, the posture Sba.chooseLegendVictims takes for
          -- CR 704.5j: an answer naming something that was never minted would
          -- otherwise leave CR 603.7c's "it" pointing at nothing and the delayed
          -- ability would sacrifice neither Wall. The slot is bound either way,
          -- deterministically to the first token.
          HU.testCase "CR 603.7c an answer naming an unminted object falls back to the first token" $ do
            tidalWave <- Registry.printing registry "Tidal Wave"
            island <- Registry.printing registry "Island"
            doublingSeason <- Registry.printing registry "Doubling Season"
            let (_, base) = S.addCreature doublingSeason S.alice (S.landsInPlay island 3)
                (gs, waveId) = S.handOne tidalWave base
                cast = S.runPure chooseUnmintedToken gs (Cast.castSpell S.alice waveId)
                armed = S.runPure chooseUnmintedToken cast Engine.priorityLoop
                after = resolveAll (settle (beginEndStep armed))
            case walls armed of
              [firstWall, secondWall] -> do
                HU.assertEqual "only the second minted Wall is left" [secondWall] (walls after)
                HU.assertBool "the first minted Wall was bound, and it is gone" (Set.notMember firstWall (GameState.battlefield after))
              other -> HU.assertFailure ("expected two Wall tokens, got " <> show (length other))
        ]

-- CR 603.3b: "puts each triggered ability they control ... on the stack in any
-- order they choose". The centerpiece: two triggers, one controller, and an
-- order that changes the answer.
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
         in case filter (/= TriggerSource.OfObject ghoul) sources of
              src : _ -> src
              [] -> TriggerSource.OfObject ghoul
      -- An answerer that puts a chosen source LAST on the stack, so it resolves
      -- FIRST (CR 603.3b's answer is the order they are PUT on the stack).
      orderLast :: TriggerSource.TriggerSource -> Prompt.Prompt r -> r
      orderLast wanted p = case p of
        Prompt.OrderTriggers _ _ sources ->
          let indexed = zip [0 ..] sources
              pick keep = fmap fst (filter (\entry -> (snd entry == wanted) == keep) indexed)
           in pick False <> pick True
        _ -> S.identityAnswer p
      -- Counts how many times the ordering prompt was asked, answering canonically.
      countingAnswer :: Prompt.Prompt r -> State.State Int r
      countingAnswer p = case p of
        Prompt.OrderTriggers _ _ sources -> do
          State.modify' (+ 1)
          pure (zipWith const [0 ..] sources)
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
                after = snd (Engine.runGamePure (orderLast (TriggerSource.OfObject ghoul)) gs Engine.priorityLoop)
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

-- CR 725.2: the monarch's two inherent abilities "have no source and are
-- controlled by the player who was the monarch at the time the abilities
-- triggered" -- triggered abilities in every other respect, so CR 603.3b's
-- own-order choice covers them exactly as it covers an object's trigger. The
-- collision is reachable from the pool: Palace Jailer crowns its controller, and
-- Khabál Ghoul triggers "at the beginning of each end step", so one player's end
-- step fires her Ghoul's trigger and the monarch's inherent draw in one batch.
monarchOrderingTests :: Registry.Type.Registry -> Tasty.TestTree
monarchOrderingTests registry =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan endStep S.alice) (gs {GameState.phase = endStep})
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
         in beginEndStep (resolveAll (S.withEvents [GameEvent.Moved entered (Projection.project jailer gs3)] gs3))
      -- Records every ordering payload offered, verbatim, answering canonically.
      recordPayloads :: Prompt.Prompt r -> State.State [[TriggerSource.TriggerSource]] r
      recordPayloads p = case p of
        Prompt.OrderTriggers _ _ sources -> do
          State.modify' (<> [sources])
          pure (zipWith const [0 ..] sources)
        _ -> pure (S.identityAnswer p)
      -- Puts the SOURCELESS entry -- CR 725.2's inherent draw, named by what it
      -- is rather than by where it sits -- at the front of the permutation, so it
      -- goes on the stack FIRST and, the stack being LIFO, resolves LAST. That is
      -- the direction the old two-pass placement could not express: it appended
      -- the inherent trigger after the ordered batch, i.e. always on top, always
      -- resolving first.
      sourcelessFirst :: Prompt.Prompt r -> r
      sourcelessFirst p = case p of
        Prompt.OrderTriggers _ _ sources ->
          let indexed = zip [0 ..] sources
              pick keep = fmap fst (filter (\entry -> (snd entry == TriggerSource.Sourceless) == keep) indexed)
           in pick True <> pick False
        _ -> S.identityAnswer p
      inherentController placed oid = case fmap Object.source (Game.lookupObject oid placed) of
        Just (Source.OfInherentTrigger pid _) -> Just pid
        _ -> Nothing
      triggerSourceOf placed oid = case fmap Object.source (Game.lookupObject oid placed) of
        Just (Source.OfTrigger src _) -> Just src
        _ -> Nothing
   in Tasty.testGroup
        "MonarchTriggerOrdering"
        [ -- The collision itself: both triggers reach ONE CR 603.3b choice.
          HU.testCase "CR 603.3b/725.2 the inherent end-step draw is offered in the same ordering choice as the Ghoul's trigger" $ do
            palaceJailer <- Registry.printing registry "Palace Jailer"
            khabalGhoul <- Registry.printing registry "Khabál Ghoul"
            piker <- Registry.printing registry "Goblin Piker"
            let (ghoul, base) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
                gs = crownAndEndStep palaceJailer piker base
                (_, asked) = State.runState (Engine.runGame recordPayloads gs Engine.settleForPriority) []
            HU.assertEqual "alice really holds the crown" (Just S.alice) (GameState.monarch gs)
            HU.assertEqual
              "one ordering choice, offering the Ghoul's trigger and the sourceless inherent draw together"
              [[TriggerSource.OfObject ghoul, TriggerSource.Sourceless]]
              asked,
          -- The order is HONOURED, in the direction the old two-pass placement
          -- could not reach: the inherent draw goes on the stack first, so it sits
          -- at the BOTTOM and resolves last.
          HU.testCase "CR 603.3b/725.2 putting the inherent draw on the stack first leaves it at the bottom, resolving last" $ do
            palaceJailer <- Registry.printing registry "Palace Jailer"
            khabalGhoul <- Registry.printing registry "Khabál Ghoul"
            piker <- Registry.printing registry "Goblin Piker"
            let (ghoul, base) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
                gs = crownAndEndStep palaceJailer piker base
                placed = settleWith sourcelessFirst gs
            case GameState.stack placed of
              [top, bottom] -> do
                HU.assertEqual "the inherent draw is at the bottom -- placed first, resolves last" (Just S.alice) (inherentController placed bottom)
                HU.assertEqual "the Ghoul's trigger is on top -- placed second, resolves first" (Just ghoul) (triggerSourceOf placed top)
              other -> HU.assertFailure ("expected exactly two triggers on the stack, got " <> show (length other)),
          -- Same board, opposite answer: the inherent draw goes on the stack
          -- last, so it is on top and resolves first. This is what the old engine
          -- forced unconditionally; here it is a choice.
          HU.testCase "CR 603.3b/725.2 putting the inherent draw on the stack last leaves it on top, resolving first" $ do
            palaceJailer <- Registry.printing registry "Palace Jailer"
            khabalGhoul <- Registry.printing registry "Khabál Ghoul"
            piker <- Registry.printing registry "Goblin Piker"
            let (ghoul, base) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
                gs = crownAndEndStep palaceJailer piker base
                placed = settleWith S.identityAnswer gs
            case GameState.stack placed of
              [top, bottom] -> do
                HU.assertEqual "the inherent draw is on top -- placed second, resolves first" (Just S.alice) (inherentController placed top)
                HU.assertEqual "the Ghoul's trigger is at the bottom -- placed first, resolves last" (Just ghoul) (triggerSourceOf placed bottom)
              other -> HU.assertFailure ("expected exactly two triggers on the stack, got " <> show (length other)),
          -- Both still resolve, whichever order was chosen: the merge must not
          -- lose the inherent trigger's placement, only relocate it.
          HU.testCase "CR 725.2 the monarch still draws when her own trigger is ordered last" $ do
            palaceJailer <- Registry.printing registry "Palace Jailer"
            khabalGhoul <- Registry.printing registry "Khabál Ghoul"
            piker <- Registry.printing registry "Goblin Piker"
            let (_, base) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
                gs = crownAndEndStep palaceJailer piker base
                after = snd (Engine.runGamePure sourcelessFirst gs Engine.priorityLoop)
            HU.assertEqual "alice drew the one card in her library" 1 (length (Game.zoneMembers Zone.Hand S.alice after)),
          -- The companion elision: with only the inherent trigger in the batch
          -- there is nothing to order, and where the rules leave nothing to ask,
          -- don't prompt.
          HU.testCase "CR 603.3b the inherent draw alone is one trigger, so nothing is asked" $ do
            palaceJailer <- Registry.printing registry "Palace Jailer"
            piker <- Registry.printing registry "Goblin Piker"
            let gs = crownAndEndStep palaceJailer piker (Setup.emptyGame S.bothPlayers)
                (_, asked) = State.runState (Engine.runGame recordPayloads gs Engine.settleForPriority) []
                after = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
            HU.assertEqual "alice really holds the crown" (Just S.alice) (GameState.monarch gs)
            HU.assertEqual "no ordering choice was offered" [] asked
            HU.assertEqual "and she still drew" 1 (length (Game.zoneMembers Zone.Hand S.alice after)),
          -- And the mirror: the Ghoul's trigger alone, with no monarch at all, is
          -- also one trigger and also asks nothing.
          HU.testCase "CR 603.3b the Ghoul's trigger alone, with no monarch, asks nothing" $ do
            khabalGhoul <- Registry.printing registry "Khabál Ghoul"
            let (_, base) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
                gs = beginEndStep base
                (_, asked) = State.runState (Engine.runGame recordPayloads gs Engine.settleForPriority) []
            HU.assertEqual "no monarch, so no inherent trigger exists" Nothing (GameState.monarch gs)
            HU.assertEqual "no ordering choice was offered" [] asked
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
            entered = ZoneChange.MkZoneChange sarcId sarcId Zone.Stack Zone.Battlefield
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
                  tok : _ -> S.settleSba (S.runPure S.identityAnswer board (Event.destroy Regenerability.Regenerable [tok]))
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
                  tok : _ -> S.settleSba (S.runPure S.identityAnswer board (Event.destroy Regenerability.Regenerable [tok]))
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
        Effect.Create _ card _ _ -> Just card
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

-- CR 702.29c: "'When you cycle this card' means 'When you discard this card to
-- pay an activation cost of a cycling ability.' These abilities trigger from
-- whatever zone the card winds up in after it's cycled."
--
-- Windcaller Aven is the card: a {4}{U}{U} 4/3 with flying, Cycling {U}, and
-- "When you cycle this card, target creature gains flying until end of turn".
-- The trigger is mandatory and its effect is Serpent's Gift's exact shape, so
-- the only new thing any test below can be passing on is the trigger itself.
cyclingTriggerTests :: Registry.Type.Registry -> Tasty.TestTree
cyclingTriggerTests registry =
  Tasty.testGroup
    "CyclingTrigger"
    [ -- The whole card: cycle the Aven for {U}, its trigger targets the Piker as
      -- it is placed (CR 603.3d), and the Piker is flying once it resolves.
      HU.testCase "CR 702.29c whole card: cycling Windcaller Aven grants flying" $ do
        aven <- Registry.printing registry "Windcaller Aven"
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        let (creature, g0) = S.addCreature piker S.alice (S.landsInPlay island 1)
            (g1, avenId) = S.handOne aven g0
            gs = g1 {GameState.priority = Just S.alice}
        HU.assertBool "the Piker does not start with flying" (not (Projection.hasKeyword Keyword.Type.Flying creature gs))
        case Activate.abilitiesFor avenId gs of
          [ability] -> do
            let cycled = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice avenId ability)
                -- The settle PLACES the trigger and stamps its target (CR
                -- 603.3d); resolving it is the next thing to happen, and it is on
                -- top of the draw it was triggered alongside.
                placed = S.runPure S.identityAnswer cycled Engine.settleForPriority
                after = S.runPure S.identityAnswer placed Stack.resolveTop
            HU.assertEqual "the Aven is in the graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice cycled))
            HU.assertBool "the trigger is on the stack, above the draw" (length (GameState.stack placed) == 2)
            HU.assertBool "and the Piker has flying once it resolves" (Projection.hasKeyword Keyword.Type.Flying creature after)
          abilities -> HU.assertFailure ("expected one cycling ability, got " <> show (length abilities)),
      -- "These abilities trigger from whatever zone the card winds up in": the
      -- trigger's source is the graveyard incarnation, which CR 400.7 makes a
      -- DIFFERENT object from the card that was in hand. The scan finds it in
      -- neither of the two places it looked before this rule -- the battlefield,
      -- and a permanent that just left it.
      HU.testCase "CR 702.29c the trigger fires from the graveyard, off a new incarnation" $ do
        aven <- Registry.printing registry "Windcaller Aven"
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        let (_, g0) = S.addCreature piker S.alice (S.landsInPlay island 1)
            (g1, avenId) = S.handOne aven g0
            gs = g1 {GameState.priority = Just S.alice}
        case Activate.abilitiesFor avenId gs of
          [ability] -> do
            let cycled = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice avenId ability)
                placed = S.runPure S.identityAnswer cycled Engine.placePendingTriggers
            HU.assertEqual "the id that was in hand is gone" Nothing (Game.lookupObject avenId placed)
            HU.assertEqual "the draw and the trigger are both on the stack" 2 (length (GameState.stack placed))
          abilities -> HU.assertFailure ("expected one cycling ability, got " <> show (length abilities)),
      -- The discriminating twin, and the reason CR 702.29c needs an event of its
      -- own rather than matching the zone change the discard already records: an
      -- ORDINARY discard of the same card, through the same CR 400.7 funnel, is
      -- not cycling and fires nothing.
      HU.testCase "CR 702.29c discarding the Aven without cycling fires nothing" $ do
        aven <- Registry.printing registry "Windcaller Aven"
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        let (creature, g0) = S.addCreature piker S.alice (S.landsInPlay island 1)
            (g1, _) = S.handOne aven g0
            gs = g1 {GameState.priority = Just S.alice}
            -- The same card, the same graveyard, one component over: a cost that
            -- discards a card of the player's choice rather than this one.
            discarded = S.runPure S.identityAnswer gs (Cost.payComponent S.alice S.noSource (CostComponent.DiscardCards 1))
            after = S.runPure S.identityAnswer discarded Engine.settleForPriority
        HU.assertEqual "the Aven really did reach the graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice discarded))
        HU.assertEqual "nothing was put on the stack" [] (GameState.stack after)
        HU.assertBool "and the Piker never gained flying" (not (Projection.hasKeyword Keyword.Type.Flying creature after)),
      -- The other control: cycling a card that has no cycling trigger fires
      -- nothing, so the trigger is the Aven's and not the act of cycling.
      HU.testCase "CR 702.29c cycling a card with no such trigger fires nothing" $ do
        mauler <- Registry.printing registry "Barkhide Mauler"
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        let (_, g0) = S.addCreature piker S.alice (S.landsInPlay forest 2)
            (g1, maulerId) = S.handOne mauler g0
            gs = g1 {GameState.priority = Just S.alice}
        case Activate.abilitiesFor maulerId gs of
          [ability] -> do
            let cycled = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice maulerId ability)
                placed = S.runPure S.identityAnswer cycled Engine.placePendingTriggers
            HU.assertEqual "only the draw is on the stack" 1 (length (GameState.stack placed))
          abilities -> HU.assertFailure ("expected one cycling ability, got " <> show (length abilities))
    ]

-- CR 603.6a's SECOND written form -- "Whenever a [type] enters, . . ." -- and
-- Soul Warden {W} Creature -- Human Cleric 1/1, "Whenever another creature
-- enters, you gain 1 life", the card that proves it. Its effect names nothing
-- about the entering creature, so these cases isolate the trigger CONDITION;
-- its "another" is Filter.Not Filter.IsSource inside the condition's own
-- Filter, never a second exclusion mechanism (#163).
permanentEntersTests :: Registry.Type.Registry -> Tasty.TestTree
permanentEntersTests registry =
  let anyCreature = Filter.Type.HasCardType CardType.Creature
      anotherCreature = Filter.Type.And [anyCreature, Filter.Type.Not Filter.Type.IsSource]
      enters oid = GameEvent.Moved (ZoneChange.MkZoneChange oid oid Zone.Stack Zone.Battlefield) S.emptyCharacteristics
      sourcesOf gs = fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedEvents gs) gs))
   in Tasty.testGroup
        "PermanentEnters"
        [ -- The gameplay-level proof, cast to resolution: alice's second Soul
          -- Warden enters and the FIRST one's trigger resolves for exactly 1
          -- life. Exactly one life, not two, is the "another" falsifier -- the
          -- newcomer is checked against its own entry (the case below proves
          -- the scan does check it) and its own Filter is what declines.
          HU.testCase "CR 603.6a whole cards: a second Soul Warden entering gains alice exactly 1 life" $ do
            plains <- Registry.printing registry "Plains"
            soulWarden <- Registry.printing registry "Soul Warden"
            let (_, base) = S.addCreature soulWarden S.alice (S.landsInPlay plains 1)
                (gs, spellId) = S.handOne soulWarden base
                cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
                settled = snd (Engine.runGamePure S.identityAnswer cast Engine.priorityLoop)
            HU.assertEqual "both Wardens are on the battlefield" 2 (S.countOnBattlefieldByName (Text.pack "Soul Warden") S.alice settled)
            HU.assertEqual "alice gained exactly 1" (fmap (+ 1) (S.lifeOf S.alice gs)) (S.lifeOf S.alice settled)
            HU.assertEqual "bob gained nothing" (S.lifeOf S.bob gs) (S.lifeOf S.bob settled),
          -- CR 603.6a: "all permanents on the battlefield (INCLUDING THE
          -- NEWCOMERS) are checked for any enters-the-battlefield triggers that
          -- match the event." The newcomer really is offered its own entry; a
          -- bare "a creature enters" admits it, and only Soul Warden's printed
          -- "another" turns it away. This is why the constructor is not called
          -- OtherEnters.
          HU.testCase "CR 603.6a including the newcomers: a permanent is checked against its own entry" $ do
            soulWarden <- Registry.printing registry "Soul Warden"
            let (oid, gs) = S.addCreature soulWarden S.alice (Setup.emptyGame S.bothPlayers)
            HU.assertBool "\"a creature enters\" admits the newcomer itself" $
              Event.matchesTrigger gs oid S.alice (TriggerCondition.PermanentEnters anyCreature) (enters oid)
            HU.assertBool "\"another creature enters\" does not" $
              not (Event.matchesTrigger gs oid S.alice (TriggerCondition.PermanentEnters anotherCreature) (enters oid)),
          -- The live-reading falsifier. `enters` above hands the event a
          -- deliberately EMPTY ProjectedCharacteristics -- no card types at all
          -- -- because that snapshot is CR 608.2h last known information for the
          -- zone the object LEFT, and matching against it would answer CR 603.6b
          -- backwards: "continuous effects that modify characteristics of a
          -- permanent do so the moment the permanent is on the battlefield (and
          -- not before then)". CR 603.10 says the same of an event's objects.
          -- The Piker is a Creature only in the live projection, so a matcher
          -- reading the snapshot fires nothing here.
          HU.testCase "CR 603.6b/603.10 the entrant is read live, not from the Moved event's snapshot" $ do
            soulWarden <- Registry.printing registry "Soul Warden"
            piker <- Registry.printing registry "Goblin Piker"
            let (warden, gs0) = S.addCreature soulWarden S.alice (Setup.emptyGame S.bothPlayers)
                (pikerId, gs1) = S.addCreature piker S.bob gs0
            HU.assertEqual "no card types in the snapshot" Set.empty (PC.cardTypes S.emptyCharacteristics)
            HU.assertBool "and the trigger still fires" $
              Event.matchesTrigger gs1 warden S.alice (TriggerCondition.PermanentEnters anotherCreature) (enters pikerId),
          -- CR 608.2h: an entrant that is already gone by the CR 117.5 boundary
          -- -- here moved straight on to the graveyard -- is read from last known
          -- information, which for a permanent that left the battlefield is the
          -- battlefield reading. The event happened; the trigger is not lost with
          -- the object. Same fallback eventTriggers' own `bystanders` takes for
          -- the bearer side.
          HU.testCase "CR 608.2h a creature that enters and leaves again still fires the trigger" $ do
            soulWarden <- Registry.printing registry "Soul Warden"
            piker <- Registry.printing registry "Goblin Piker"
            let (warden, gs0) = S.addCreature soulWarden S.alice (Setup.emptyGame S.bothPlayers)
                (handCard, gs1) = S.addHandCard piker S.bob gs0
                entered = S.runPure S.identityAnswer gs1 (Event.changeZone handCard Zone.Battlefield)
                newIds = fmap ZoneChange.object (filter ((==) Zone.Battlefield . ZoneChange.to) (S.zoneChangesOf entered))
            case newIds of
              [newId] -> do
                let gone = S.runPure S.identityAnswer entered (Event.changeZone newId Zone.Graveyard)
                HU.assertEqual "the entrant is off the battlefield" Nothing (Game.lookupObject newId gone)
                HU.assertEqual "the Warden still triggered, once" [TriggerSource.OfObject warden] (sourcesOf gone)
              other -> HU.assertFailure ("expected exactly one battlefield entry, got " <> show (length other)),
          -- The type half of the Filter: a LAND entering is not a creature
          -- entering. Plains has no ability of its own, so nothing else can
          -- stand in for the silence.
          HU.testCase "CR 603.6a a noncreature permanent entering fires nothing" $ do
            soulWarden <- Registry.printing registry "Soul Warden"
            plains <- Registry.printing registry "Plains"
            let (_, gs0) = S.addCreature soulWarden S.alice (Setup.emptyGame S.bothPlayers)
                (landId, gs1) = S.addCreature plains S.alice gs0
                gs2 = S.withEvents [GameEvent.Moved (ZoneChange.MkZoneChange landId landId Zone.Stack Zone.Battlefield) (Projection.project landId gs1)] gs1
            HU.assertEqual "no trigger" [] (sourcesOf gs2),
          -- The destination half: CR 603.6a is an ENTERS-THE-BATTLEFIELD
          -- ability, so a creature card moving to a graveyard is not it.
          HU.testCase "CR 603.6a only a battlefield destination fires it" $ do
            soulWarden <- Registry.printing registry "Soul Warden"
            piker <- Registry.printing registry "Goblin Piker"
            let (warden, gs0) = S.addCreature soulWarden S.alice (Setup.emptyGame S.bothPlayers)
                (pikerId, gs1) = S.addCreature piker S.bob gs0
                toGrave = GameEvent.Moved (ZoneChange.MkZoneChange pikerId pikerId Zone.Battlefield Zone.Graveyard) S.emptyCharacteristics
            HU.assertBool "a graveyard-bound move does not match" $
              not (Event.matchesTrigger gs1 warden S.alice (TriggerCondition.PermanentEnters anotherCreature) toGrave),
          -- CR 603.6a: "EACH TIME an event puts one or more permanents onto the
          -- battlefield" -- one bearer, two entering creatures, two triggers. A
          -- count, not a boolean, so "fires once per entrant" is distinguishable
          -- from "fires once per batch".
          HU.testCase "CR 603.6a one Soul Warden fires once per entering creature" $ do
            soulWarden <- Registry.printing registry "Soul Warden"
            piker <- Registry.printing registry "Goblin Piker"
            let (warden, gs0) = S.addCreature soulWarden S.alice (Setup.emptyGame S.bothPlayers)
                (first, gs1) = S.addCreature piker S.bob gs0
                (second, gs2) = S.addCreature piker S.bob gs1
                gs3 =
                  S.withEvents
                    [ GameEvent.Moved (ZoneChange.MkZoneChange first first Zone.Stack Zone.Battlefield) (Projection.project first gs2),
                      GameEvent.Moved (ZoneChange.MkZoneChange second second Zone.Stack Zone.Battlefield) (Projection.project second gs2)
                    ]
                    gs2
            HU.assertEqual "twice, both from the one Warden" (replicate 2 (TriggerSource.OfObject warden)) (sourcesOf gs3)
        ]

-- CR 113.6k: "A trigger condition that can't trigger from the battlefield
-- functions in all zones it can trigger from." Narcomoeba's "When this card is
-- put into your graveyard from your library" is such a condition -- the bearer
-- is in a graveyard when it fires, never on the battlefield -- so the scan has
-- to look somewhere other than the battlefield to find it.
--
-- The proving pair is Tome Scour ("target player mills five cards") and
-- Narcomoeba; Soul Warden rides along in the same graveyard as the control,
-- because its CR 603.6a trigger functions ONLY on the battlefield and so must
-- stay silent even when a creature enters right in front of it.
graveyardTriggerTests :: Registry.Type.Registry -> Tasty.TestTree
graveyardTriggerTests registry =
  let -- alice: one Island in play (Tome Scour's {U}), Tome Scour in hand, and a
      -- three-card library of Narcomoeba, Soul Warden and a Goblin Piker. Five
      -- mills a three-card library empty (CR 701.17b), so every one of them
      -- lands in the graveyard in one event batch and the scan has to pick the
      -- one card whose ability functions there.
      milledBoard = do
        island <- Registry.printing registry "Island"
        tomeScour <- Registry.printing registry "Tome Scour"
        narcomoeba <- Registry.printing registry "Narcomoeba"
        soulWarden <- Registry.printing registry "Soul Warden"
        piker <- Registry.printing registry "Goblin Piker"
        let base = S.landsInPlay island 1
            (_, g1) = S.addLibraryCard narcomoeba S.alice base
            (_, g2) = S.addLibraryCard soulWarden S.alice g1
            (_, g3) = S.addLibraryCard piker S.alice g2
            (g4, spellId) = S.handOne tomeScour g3
        pure (g4 {GameState.priority = Just S.alice}, spellId)
      -- Takes every "may". There is exactly one in this scenario -- Narcomoeba's
      -- -- so this is not a blanket yes standing in for a specific answer.
      takeOptional :: Prompt.Prompt r -> r
      takeOptional p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      -- Cast Tome Scour at alice herself (S.identityAnswer's ChooseTargets picks
      -- the least id in each set, and alice is player 0), resolve it, settle so
      -- any trigger reaches the stack, then resolve that trigger.
      millSelf :: (forall r. Prompt.Prompt r -> r) -> (GameState.GameState, ObjectId.ObjectId) -> (GameState.GameState, GameState.GameState)
      millSelf answer (gs, spellId) =
        let cast = S.runPure answer gs (Cast.castSpell S.alice spellId)
            milled = S.runPure answer cast Stack.resolveTop
            placed = S.runPure answer milled Engine.settleForPriority
         in (placed, S.runPure answer placed Stack.resolveTop)
      namesIn zone pid gs =
        Set.fromList (Maybe.mapMaybe (\oid -> fmap Card.Type.name (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs))
      narcomoebaName = Text.pack "Narcomoeba"
   in Tasty.testGroup
        "GraveyardTrigger"
        [ -- The gameplay-level proof, cast to resolution.
          HU.testCase "CR 113.6k whole card: Tome Scour mills Narcomoeba and its trigger puts it onto the battlefield" $ do
            board <- milledBoard
            let (placed, after) = millSelf takeOptional board
            HU.assertEqual "the trigger reached the stack" 1 (length (GameState.stack placed))
            HU.assertBool "Narcomoeba is on the battlefield" (Set.member narcomoebaName (namesIn Zone.Battlefield S.alice after))
            HU.assertBool "and no longer in the graveyard" (not (Set.member narcomoebaName (namesIn Zone.Graveyard S.alice after)))
            -- The control, in the same graveyard: Soul Warden's "whenever
            -- another creature enters" functions only on the battlefield (CR
            -- 113.6's default), and a creature entered right in front of it.
            HU.assertEqual "the Soul Warden in the graveyard gained nothing" (Just 20) (S.lifeOf S.alice after),
          -- CR 603.5: the "may" is a real choice, and declining is the other
          -- half of it. The trigger still went on the stack and still resolved.
          HU.testCase "CR 603.5 declining the may leaves Narcomoeba in the graveyard" $ do
            board <- milledBoard
            let (placed, after) = millSelf S.identityAnswer board
            HU.assertEqual "the trigger reached the stack anyway" 1 (length (GameState.stack placed))
            HU.assertBool "Narcomoeba is still in the graveyard" (Set.member narcomoebaName (namesIn Zone.Graveyard S.alice after))
            HU.assertBool "and not on the battlefield" (not (Set.member narcomoebaName (namesIn Zone.Battlefield S.alice after)))
            HU.assertEqual "and the ability left the stack -- a declined may is not a fizzle" 0 (length (GameState.stack after)),
          -- "from your library" doing real work, half one: the same card moved
          -- out of a HAND reaches the same graveyard and must not trigger.
          HU.testCase "CR 113.6k Narcomoeba put into the graveyard from the HAND does not trigger" $ do
            narcomoeba <- Registry.printing registry "Narcomoeba"
            let (handCard, gs) = S.addHandCard narcomoeba S.alice (Setup.emptyGame S.bothPlayers)
                buried = S.runPure S.identityAnswer gs (Event.changeZone handCard Zone.Graveyard)
            HU.assertEqual "nothing triggered" [] (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedEvents buried) buried)))
            HU.assertBool "it is in the graveyard" (Set.member narcomoebaName (namesIn Zone.Graveyard S.alice buried)),
          -- "from your library" doing real work, half two: dying is a move to
          -- the same graveyard from the battlefield, and is not this trigger.
          HU.testCase "CR 113.6k Narcomoeba dying from the BATTLEFIELD does not trigger" $ do
            narcomoeba <- Registry.printing registry "Narcomoeba"
            let (creature, gs) = S.addCreature narcomoeba S.alice (Setup.emptyGame S.bothPlayers)
                died = S.runPure S.identityAnswer gs (Event.changeZone creature Zone.Graveyard)
            HU.assertEqual "nothing triggered" [] (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedEvents died) died)))
            HU.assertBool "it is in the graveyard" (Set.member narcomoebaName (namesIn Zone.Graveyard S.alice died)),
          -- The zone half, isolated from the mill: a graveyard card whose only
          -- trigger functions on the battlefield (CR 113.6's default) is not
          -- scanned into firing by an event it would have seen from play.
          HU.testCase "CR 113.6 a battlefield-only trigger in a graveyard is not scanned" $ do
            soulWarden <- Registry.printing registry "Soul Warden"
            piker <- Registry.printing registry "Goblin Piker"
            let (wardenCard, gs0) = S.addLibraryCard soulWarden S.alice (Setup.emptyGame S.bothPlayers)
                buried = S.runPure S.identityAnswer gs0 (Event.changeZone wardenCard Zone.Graveyard)
                (pikerCard, gs1) = S.addHandCard piker S.alice buried
                entered = S.runPure S.identityAnswer gs1 (Event.changeZone pikerCard Zone.Battlefield)
            HU.assertBool "the Warden is in the graveyard" (Set.member (Text.pack "Soul Warden") (namesIn Zone.Graveyard S.alice entered))
            HU.assertEqual "and a creature entering fires nothing" [] (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedEvents entered) entered)))
        ]

-- CR 603.6c: leaves-the-battlefield abilities "trigger when a permanent moves
-- from the battlefield to another zone ... written as, but aren't limited to,
-- 'When [this object] leaves the battlefield, . . .' or 'Whenever [something]
-- is put into a graveyard from the battlefield, . . . .'" Doomed Traveler
-- prints the second of those in its abbreviated form: CR 700.4 says "the term
-- dies means 'is put into a graveyard from the battlefield.'"
--
-- CR 603.10a is what makes it more than a tenth condition: "Some zone-change
-- triggers look back in time. These are leaves-the-battlefield abilities ...",
-- so the match is against the game as it was IMMEDIATELY BEFORE the event. By
-- the time the scan runs, the Traveler is a card in a graveyard with a fresh id
-- (CR 400.7) and nothing is on the battlefield to find -- which is what makes
-- the token appearing at all the discriminating assertion here.
diesTriggerTests :: Registry.Type.Registry -> Tasty.TestTree
diesTriggerTests registry =
  let -- alice: one Mountain (Lightning Bolt's {R}), a Doomed Traveler in play,
      -- and the Bolt in hand. S.identityAnswer targets the least Recipient, and
      -- Recipient.ToCreature sorts before Recipient.ToPlayer, so the one
      -- creature on the board is the target without a bespoke interpreter.
      boltBoard = do
        mountain <- Registry.printing registry "Mountain"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        doomedTraveler <- Registry.printing registry "Doomed Traveler"
        let (_, withTraveler) = S.addCreature doomedTraveler S.alice (S.landsInPlay mountain 1)
        pure (S.handOne lightningBolt withTraveler)
      -- Cast the Bolt, resolve it (3 damage marked on a 1/1), settle -- CR
      -- 704.5g's state-based action destroys it and the CR 117.5 settle's OWN
      -- trigger scan must see that death -- then resolve the trigger.
      boltIt (gs, spellId) =
        let cast = S.runPure S.identityAnswer gs (Cast.castSpell S.alice spellId)
            damaged = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer damaged Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      namesIn zone pid gs =
        Set.fromList (Maybe.mapMaybe (\oid -> fmap Card.Type.name (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs))
      spiritsOf pid gs =
        filter
          -- CR 111.4: Doomed Traveler does not specify the token's name, so the
          -- name is its subtype plus the word "Token".
          (\oid -> fmap Card.Type.name (Game.cardOf oid gs) == Just (Text.pack "Spirit Token"))
          (Game.zoneMembers Zone.Battlefield pid gs)
      travelerName = Text.pack "Doomed Traveler"
   in Tasty.testGroup
        "DiesTrigger"
        [ -- The gameplay-level proof, cast to resolution, through a real
          -- removal spell and the state-based action it sets up.
          HU.testCase "CR 603.6c whole card: Lightning Bolt kills Doomed Traveler and its dies trigger makes a flying Spirit" $ do
            board <- boltBoard
            let (settled, after) = boltIt board
            -- The trigger was gathered in the SAME settle that ran the SBA
            -- (Engine.settleForPriority: performStateBasedActions, then
            -- placePendingTriggers, then loop).
            HU.assertEqual "the trigger reached the stack in that settle" 1 (length (GameState.stack settled))
            -- And it did so with the Traveler already gone: an implementation
            -- matching against the live battlefield would find nothing here.
            HU.assertBool "the Traveler is in the graveyard by then" (Set.member travelerName (namesIn Zone.Graveyard S.alice settled))
            HU.assertBool "and not on the battlefield" (not (Set.member travelerName (namesIn Zone.Battlefield S.alice settled)))
            case spiritsOf S.alice after of
              [spirit] -> do
                HU.assertEqual "power" (Just 1) (Projection.powerOf spirit after)
                HU.assertEqual "toughness" (Just 1) (Projection.toughnessOf spirit after)
                HU.assertEqual "white" (Set.singleton Color.White) (Projection.colorsOf spirit after)
                HU.assertEqual "Spirit" (Set.singleton Subtype.Spirit) (Projection.subtypesOf spirit after)
                HU.assertBool "with flying" (Projection.hasKeyword Keyword.Type.Flying spirit after)
              other -> HU.assertFailure ("expected exactly one Spirit token, got " <> show (length other)),
          -- CR 700.4 doing real work: "dies" is NARROWER than CR 603.6c's
          -- leaves-the-battlefield. The same permanent moved from the
          -- battlefield to EXILE has left the battlefield and has not died.
          HU.testCase "CR 700.4 a Traveler exiled from the battlefield does not trigger" $ do
            doomedTraveler <- Registry.printing registry "Doomed Traveler"
            let (traveler, gs) = S.addCreature doomedTraveler S.alice (Setup.emptyGame S.bothPlayers)
                exiled = S.runPure S.identityAnswer gs (Event.changeZone traveler Zone.Exile)
            HU.assertEqual "nothing triggered" [] (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedEvents exiled) exiled)))
            HU.assertBool "it is in exile" (Set.member travelerName (namesIn Zone.Exile S.alice exiled)),
          -- The other half of "from the battlefield": the same card discarded
          -- reaches the same graveyard and has not died (CR 700.4).
          HU.testCase "CR 700.4 a Traveler discarded from the HAND does not trigger" $ do
            doomedTraveler <- Registry.printing registry "Doomed Traveler"
            let (traveler, gs) = S.addHandCard doomedTraveler S.alice (Setup.emptyGame S.bothPlayers)
                discarded = S.runPure S.identityAnswer gs (Event.changeZone traveler Zone.Graveyard)
            HU.assertEqual "nothing triggered" [] (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedEvents discarded) discarded))),
          -- Self-scoped: SOME OTHER creature dying is not this Traveler's
          -- death, even though the Traveler is right there to see it.
          HU.testCase "CR 603.6c another creature dying does not fire the Traveler's trigger" $ do
            doomedTraveler <- Registry.printing registry "Doomed Traveler"
            piker <- Registry.printing registry "Goblin Piker"
            let (_, withTraveler) = S.addCreature doomedTraveler S.alice (Setup.emptyGame S.bothPlayers)
                (pikerId, gs) = S.addCreature piker S.alice withTraveler
                died = S.runPure S.identityAnswer gs (Event.changeZone pikerId Zone.Graveyard)
            HU.assertEqual "nothing triggered" [] (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedEvents died) died))),
          -- CR 603.3a through CR 603.10a's look-back: "the player who controlled
          -- the ability's source at the time it triggered" is read from the game
          -- as it was immediately BEFORE the death, so a Traveler bob owns but
          -- alice has stolen with Control Magic hands ALICE the Spirit. Reading
          -- the graveyard card's owner instead would answer bob.
          HU.testCase "CR 603.3a the trigger is controlled by whoever controlled the Traveler as it died" $ do
            doomedTraveler <- Registry.printing registry "Doomed Traveler"
            controlMagic <- Registry.printing registry "Control Magic"
            let (traveler, withTraveler) = S.addCreature doomedTraveler S.bob (Setup.emptyGame S.bothPlayers)
                (aura, withAura) = S.addCreature controlMagic S.alice withTraveler
                stolen = S.attach aura traveler withAura
                died = S.runPure S.identityAnswer stolen (Event.changeZone traveler Zone.Graveyard)
            HU.assertEqual "alice controlled it as it died" (Just S.alice) (Projection.controllerOf traveler stolen)
            HU.assertEqual "so the trigger is hers, not its owner's" [S.alice] (fmap PendingTrigger.controller (fst (Event.gatherTriggers (Event.unscannedEvents died) died)))
        ]

-- One event per trigger condition, chosen to be an event that GENUINELY fires
-- that condition (Event.matchesTrigger's own arms are the spec), so
-- eventBindings is exercised through its matching arm rather than through its
-- `_ -> Map.empty` fallthrough. A pair that did not match would pin nothing:
-- both sides would read empty for every condition.
--
-- Exhaustive with no wildcard, which is half of what keeps the pin honest -- a
-- new TriggerCondition fails to compile here. The other half, the list below, is
-- hand-kept and cannot be forced; add the new constructor there too.
representativeEvent :: TriggerCondition.TriggerCondition -> GameEvent.GameEvent
representativeEvent cond =
  let departed = ObjectId.MkObjectId 1
      arrived = ObjectId.MkObjectId 2
      moved from to = GameEvent.Moved (ZoneChange.MkZoneChange departed arrived from to) S.emptyCharacteristics
      combatDamage =
        GameEvent.DamageDealt
          (DamageEvent.MkDamageEvent departed (Recipient.ToPlayer S.bob) 2 False False 0 DamageKind.Combat)
   in case cond of
        TriggerCondition.SelfEnters -> moved Zone.Stack Zone.Battlefield
        TriggerCondition.PermanentEnters _ -> moved Zone.Stack Zone.Battlefield
        TriggerCondition.StepBegins phase _ -> GameEvent.StepBegan phase S.alice
        -- CR 603.8: a state trigger matches a game STATE, so no log entry fires
        -- it at all (Event.matchesTrigger's StateIs arm answers False for every
        -- event). Any event is therefore as representative as any other.
        TriggerCondition.StateIs _ -> GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice
        TriggerCondition.SelfDealsCombatDamageToPlayer -> combatDamage
        TriggerCondition.CreatureDealtCombatDamageToMonarch -> combatDamage
        TriggerCondition.SelfCycled -> GameEvent.Cycled departed
        TriggerCondition.SelfAttacks _ -> GameEvent.AttackerDeclared departed
        TriggerCondition.SelfPutIntoGraveyardFromLibrary -> moved Zone.Library Zone.Graveyard
        TriggerCondition.SelfDies -> moved Zone.Battlefield Zone.Graveyard

-- Every TriggerCondition, one inhabitant each. The payloads are arbitrary:
-- eventBindings and eventBindingSlots both ignore them, which is itself part of
-- what the pin asserts.
everyTriggerCondition :: [TriggerCondition.TriggerCondition]
everyTriggerCondition =
  [ TriggerCondition.SelfEnters,
    TriggerCondition.PermanentEnters Filter.Type.IsSource,
    TriggerCondition.StepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.EachTurn,
    TriggerCondition.StateIs (Condition.Type.MkCondition (Quantity.Type.Literal 0) Comparison.Exactly (Quantity.Type.Literal 0)),
    TriggerCondition.SelfDealsCombatDamageToPlayer,
    TriggerCondition.CreatureDealtCombatDamageToMonarch,
    TriggerCondition.SelfCycled,
    TriggerCondition.SelfAttacks TriggerFrequency.EveryTime,
    TriggerCondition.SelfPutIntoGraveyardFromLibrary,
    TriggerCondition.SelfDies
  ]

-- CR 603.6c's penultimate sentence -- "An ability that attempts to do something
-- to the card that left the battlefield checks for it only in the first zone
-- that it went to" -- said positively by CR 400.7e: "Abilities that trigger when
-- an object moves from one zone to another ... can find the new object that it
-- became in the zone it moved to when the ability triggered, if that zone is a
-- public zone."
--
-- Endless Cockroaches, {1}{B}{B} Creature -- Insect 1/1, "When this creature
-- dies, return it to its owner's hand." Two different objects hide inside that
-- one printed word "it": the ability's SOURCE (CR 113.7a -- the permanent that
-- died, which CR 603.10a's look-back reads from CR 608.2h last known
-- information) and the CARD it became in the graveyard, which is what the
-- effect has to move. CR 400.7 minted a fresh id for the second, so the two are
-- not interchangeable and they are not one slot.
--
-- Also the home of the pin on Event.eventBindingSlots (the last case): that
-- classification restates in one dimension what eventBindings says in two, so
-- the two are compared here rather than trusted to agree.
--
-- The structural twin is Narcomoeba's `MoveToZone "self" Battlefield` in
-- graveyardTriggerTests: same opcode, same slot shape. There "self" IS the
-- arriving card, because SelfPutIntoGraveyardFromLibrary matches on the
-- ARRIVING incarnation; here it is not, because CR 603.10a makes this condition
-- match on the DEPARTING one. That contrast is why there are two slots.
becameSlotTests :: Registry.Type.Registry -> Tasty.TestTree
becameSlotTests registry =
  let -- alice: one Mountain (Lightning Bolt's {R}), the Cockroaches in play, and
      -- the Bolt in hand. S.identityAnswer targets the least Recipient, and
      -- Recipient.ToCreature sorts before Recipient.ToPlayer, so the one
      -- creature on the board is the target without a bespoke interpreter.
      roachBoard = do
        mountain <- Registry.printing registry "Mountain"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        cockroaches <- Registry.printing registry "Endless Cockroaches"
        let (roachId, withRoaches) = S.addCreature cockroaches S.alice (S.landsInPlay mountain 1)
        pure (roachId, S.handOne lightningBolt withRoaches)
      -- Cast the Bolt, resolve it (3 damage marked on a 1/1), settle -- CR
      -- 704.5g destroys it and the same CR 117.5 settle's trigger scan sees the
      -- death -- then resolve the trigger.
      boltIt (gs, spellId) =
        let cast = S.runPure S.identityAnswer gs (Cast.castSpell S.alice spellId)
            damaged = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer damaged Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      namesIn zone pid gs =
        Set.fromList (Maybe.mapMaybe (\oid -> fmap Card.Type.name (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs))
      roachName = Text.pack "Endless Cockroaches"
   in Tasty.testGroup
        "CR 400.7e the card it became"
        [ -- The gameplay-level proof, cast to resolution. The discriminating
          -- assertion is the HAND: an effect reading the trigger's source would
          -- name the dead battlefield id and move nothing, leaving the card in
          -- the graveyard where the state-based action put it.
          HU.testCase "CR 603.6c whole card: Lightning Bolt kills Endless Cockroaches and its dies trigger returns the card to hand" $ do
            (_, board) <- roachBoard
            let (settled, after) = boltIt board
            HU.assertEqual "the trigger reached the stack in that settle" 1 (length (GameState.stack settled))
            HU.assertBool "the card is in the graveyard when the trigger is placed" (Set.member roachName (namesIn Zone.Graveyard S.alice settled))
            HU.assertBool "and in hand once it resolves" (Set.member roachName (namesIn Zone.Hand S.alice after))
            HU.assertBool "no longer in the graveyard" (not (Set.member roachName (namesIn Zone.Graveyard S.alice after)))
            -- The Bolt itself is the graveyard's only remaining tenant, so the
            -- assertion above cannot be passing because the graveyard is read
            -- from the wrong player's zone.
            HU.assertEqual "only the Bolt is left there" (Set.singleton (Text.pack "Lightning Bolt")) (namesIn Zone.Graveyard S.alice after),
          -- The two slots, side by side on the placed trigger. CR 113.7a's
          -- source is the id that DIED and no longer resolves; CR 400.7e's
          -- "became" is the graveyard card, which does.
          HU.testCase "CR 113.7a the self slot keeps the departed id while became names the graveyard card" $ do
            (roachId, board) <- roachBoard
            let (settled, _) = boltIt board
                bindingsOn oid = maybe Map.empty Object.bindings (Game.lookupObject oid settled)
                slots = concatMap (Map.toList . Binding.targetsOf . bindingsOn) (GameState.stack settled)
                slotFor name = lookup name slots
            HU.assertEqual "self is the permanent that died" (Just (Recipient.ToObject roachId)) (slotFor Binding.triggerSource)
            HU.assertBool "and that id is gone (CR 400.7)" (Maybe.isNothing (Game.lookupObject roachId settled))
            case slotFor Binding.became of
              Just (Recipient.ToObject graveyardId) -> do
                HU.assertBool "became is a different id" (graveyardId /= roachId)
                HU.assertEqual "and it is the graveyard card" (Just roachName) (fmap Card.Type.name (Game.cardOf graveyardId settled))
                -- The spent Bolt is in that graveyard too, so membership is the
                -- assertion rather than the whole zone.
                HU.assertBool "in alice's graveyard" (elem graveyardId (Game.zoneMembers Zone.Graveyard S.alice settled))
              other -> HU.assertFailure ("expected became to name an object, got " <> show other),
          -- eventBindings in isolation, so the binding is pinned to the rule
          -- rather than to one card's payload. CR 400.7e's "the new object that
          -- it became in the zone it moved to" is ZoneChange.object, never
          -- ZoneChange.departed, which is what matchesTrigger matched on.
          HU.testCase "CR 400.7e eventBindings binds the ARRIVING id, not the departed one" $
            let departed = ObjectId.MkObjectId 1
                arrived = ObjectId.MkObjectId 2
                died = GameEvent.Moved (ZoneChange.MkZoneChange departed arrived Zone.Battlefield Zone.Graveyard) S.emptyCharacteristics
             in HU.assertEqual
                  "became names the graveyard incarnation"
                  (Map.singleton Binding.became (Binding.toObject arrived))
                  (Event.eventBindings TriggerCondition.SelfDies died),
          -- A condition that is not a look-back gets no such slot: Narcomoeba's
          -- bearer IS the arriving card, so binding it again would be a second
          -- name for the same object.
          HU.testCase "CR 113.6k a library-to-graveyard trigger binds nothing" $
            let oid = ObjectId.MkObjectId 1
                milled = GameEvent.Moved (ZoneChange.MkZoneChange oid oid Zone.Library Zone.Graveyard) S.emptyCharacteristics
             in HU.assertEqual
                  "no became slot"
                  Map.empty
                  (Event.eventBindings TriggerCondition.SelfPutIntoGraveyardFromLibrary milled),
          -- The pin on Event.eventBindingSlots, the per-CONDITION slot set the
          -- card lint asks (CardSpec's "every slot a triggered ability reads is
          -- bound for its condition"). That function is a second statement of
          -- what eventBindings already says, and eventBindings cases on
          -- (condition, event) PAIRS, so nothing in the types keeps the two
          -- agreeing: a new binding arm added there and forgotten here would
          -- silently un-lint the new slot. Comparing the keys eventBindings
          -- actually produces against what the classification claims is what
          -- makes the drift a failing test.
          HU.testCase "CR 603.2 eventBindingSlots names exactly the keys eventBindings stamps" $
            mapM_
              ( \cond ->
                  HU.assertEqual
                    ("the slots bound for " <> show cond)
                    (Map.keysSet (Event.eventBindings cond (representativeEvent cond)))
                    (Event.eventBindingSlots cond)
              )
              everyTriggerCondition
        ]

-- CR 603.4's intervening "if" on a LOOK-BACK trigger, which is the one shape
-- where the clause has to be read against an object that no longer exists:
-- "When the trigger event occurs, the ability checks whether the stated
-- condition is true. The ability triggers only if it is." CR 608.2a repeats the
-- check as the ability resolves.
--
-- Deathknell Berserker, {1}{B} Creature -- Elf Berserker 2/2: "When this
-- creature dies, if its power was 3 or greater, create a 2/2 black Zombie
-- Berserker creature token." Both readings of "its power" that are available
-- without CR 608.2h are wrong -- the id is gone, so a live projection describes
-- nothing at all, and the card sitting in the graveyard has its printed 2.
-- Only last known information answers 3.
--
-- Bad Moon supplies the third power, and it does so through the LAYERS
-- (CR 613.4c, layer 7c), which is what makes the graveyard card's printed value
-- visibly the wrong answer rather than merely a different route to the same one.
lookBackInterveningTests :: Registry.Type.Registry -> Tasty.TestTree
lookBackInterveningTests registry =
  let berserkerBoard withBadMoon = do
        mountain <- Registry.printing registry "Mountain"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        berserker <- Registry.printing registry "Deathknell Berserker"
        badMoon <- Registry.printing registry "Bad Moon"
        let lands = S.landsInPlay mountain 1
            moonAdded = if withBadMoon then snd (S.addCreature badMoon S.alice lands) else lands
            (berserkerId, withBerserker) = S.addCreature berserker S.alice moonAdded
        pure (berserkerId, S.handOne lightningBolt withBerserker)
      -- The Bolt targets the least Recipient, and S.addCreature hands out
      -- ascending ids, so Bad Moon (added first) would sort before the
      -- Berserker if it were a legal target -- it is an enchantment, and
      -- Lightning Bolt's pool is AnyTarget, so the Berserker is the only
      -- creature and the Bolt finds it.
      boltIt (gs, spellId) =
        let cast = S.runPure S.identityAnswer gs (Cast.castSpell S.alice spellId)
            damaged = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer damaged Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      tokensOf pid gs =
        filter
          -- CR 111.4: the name is BOTH subtypes plus "Token", which is exactly
          -- the rule's own Dwarven Reinforcements example.
          (\oid -> fmap Card.Type.name (Game.cardOf oid gs) == Just (Text.pack "Zombie Berserker Token"))
          (Game.zoneMembers Zone.Battlefield pid gs)
   in Tasty.testGroup
        "CR 603.4 an intervening if over last known information"
        [ HU.testCase "CR 603.4 with Bad Moon the Berserker died at power 3 and its trigger fires" $ do
            (berserkerId, board) <- berserkerBoard True
            let (settled, after) = boltIt board
            HU.assertEqual "it was a 3/3 while it lived" (Just 3) (Projection.powerOf berserkerId (fst board))
            HU.assertEqual "the trigger reached the stack" 1 (length (GameState.stack settled))
            case tokensOf S.alice after of
              [token] -> do
                -- Printed 2/2, and 3/3 on this board: the token is black, so
                -- the same Bad Moon that made its maker a 3/3 pumps it in turn
                -- (CR 613.4c, layer 7c). Asserting the projection rather than
                -- the printed pair is what keeps the two facts from being
                -- confused for one another.
                HU.assertEqual "printed 2/2" (Just (Power.MkPower (Quantity.Type.Literal 2)), Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (maybe (Nothing, Nothing) (\c -> (Card.Type.power c, Card.Type.toughness c)) (Game.cardOf token after))
                HU.assertEqual "3/3 under Bad Moon" (Just 3, Just 3) (Projection.powerOf token after, Projection.toughnessOf token after)
                HU.assertEqual "black" (Set.singleton Color.Black) (Projection.colorsOf token after)
                HU.assertEqual "Zombie Berserker" (Set.fromList [Subtype.Zombie, Subtype.Berserker]) (Projection.subtypesOf token after)
              other -> HU.assertFailure ("expected exactly one token, got " <> show (length other)),
          -- CR 603.4's "otherwise it does nothing" -- and "does nothing" means
          -- the ability never reaches the stack at all, not that it resolves to
          -- no effect.
          HU.testCase "CR 603.4 without Bad Moon it died at power 2 and does not trigger at all" $ do
            (berserkerId, board) <- berserkerBoard False
            let (settled, after) = boltIt board
            HU.assertEqual "a 2/2 while it lived" (Just 2) (Projection.powerOf berserkerId (fst board))
            HU.assertEqual "nothing reached the stack" [] (GameState.stack settled)
            HU.assertEqual "and no token was made" [] (tokensOf S.alice after)
        ]

-- Radiant Fountain, a Land: "When this land enters, you gain 2 life. / {T}: Add
-- {C}." A nonbasic land whose whole text box is one triggered ability and one
-- activated one, which is what makes it the pool's witness for CR 305.7's
-- "It loses all abilities generated from its rules text" reaching a TRIGGER.
--
-- The entry is staged the way Pawl.TriggerSpec's other entry fixtures stage it:
-- the permanent is placed, its Moved event recorded, and the scan run at the next
-- settle. CR 603.6a checks every battlefield permanent against the event, and it
-- reads each one's PROJECTION -- so a Blood Moon that has already made the
-- Fountain a Mountain leaves nothing there to trigger.
strippedTriggerTests :: Registry.Type.Registry -> Tasty.TestTree
strippedTriggerTests registry =
  let settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      entering oid gs =
        let moved = ZoneChange.MkZoneChange oid oid Zone.Stack Zone.Battlefield
         in resolveAll (settle (S.withEvents [GameEvent.Moved moved (Projection.project oid gs)] gs))
   in Tasty.testGroup
        "CR 305.7 strips a triggered ability"
        [ HU.testCase "CR 603.6a Radiant Fountain's entry trigger gains its controller 2 life" $ do
            radiantFountain <- Registry.printing registry "Radiant Fountain"
            let (fountainId, gs) = S.addCreature radiantFountain S.alice (Setup.emptyGame S.bothPlayers)
                after = entering fountainId gs
            HU.assertEqual "20 + 2" (Just 22) (S.lifeOf S.alice after)
            HU.assertBool "and it taps for colorless" (ManaType.Colorless `elem` Mana.manaTypesOf fountainId after),
          HU.testCase "CR 305.7 under Blood Moon the same entry triggers nothing" $ do
            radiantFountain <- Registry.printing registry "Radiant Fountain"
            bloodMoon <- Registry.printing registry "Blood Moon"
            let (_, withMoon) = S.addCreature bloodMoon S.alice (Setup.emptyGame S.bothPlayers)
                (fountainId, gs) = S.addCreature radiantFountain S.alice withMoon
                after = entering fountainId gs
            HU.assertBool "it entered as a Mountain" (Set.member Subtype.Mountain (Projection.subtypesOf fountainId after))
            HU.assertEqual "nothing reached the stack" [] (GameState.stack after)
            HU.assertEqual "and no life was gained" (Just 20) (S.lifeOf S.alice after)
            -- CR 305.7's last clause, on the same board: the printed mana ability
            -- goes and the new basic land type's replaces it.
            HU.assertBool "red instead" (ManaType.Colored Color.Red `elem` Mana.manaTypesOf fountainId after)
            HU.assertBool "colorless gone" (ManaType.Colorless `notElem` Mana.manaTypesOf fountainId after)
        ]

-- CR 702.19b: assign each blocker exactly its lethal threshold and let the
-- excess trample through to the defending player. S.aggressiveAnswer cannot be
-- used here -- its AssignCombatDamage arm dumps the whole amount onto the first
-- CREATURE recipient it finds, so nothing would ever reach a player and the
-- trigger under test would never have an event to match.
tramplingAnswer :: Prompt.Prompt r -> r
tramplingAnswer p = case p of
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    let blockerEntries = Map.toList (Map.filterWithKey (\r _ -> S.isCreatureRecipient r) thresholds)
        spent = sum (fmap snd blockerEntries)
        leftover = if n >= spent then n - spent else 0
        toBlockers = Map.fromList blockerEntries
     in case filter (not . S.isCreatureRecipient) (Map.keys thresholds) of
          d : _ -> Map.insert d leftover toBlockers
          [] -> toBlockers
  _ -> S.aggressiveAnswer p

-- CR 603.10's FIRST sentence for a BYSTANDER: "objects that exist immediately
-- after an event are checked to see if the event matched any trigger
-- conditions". The scan runs once, at the CR 117.5 boundary, after CR 704.3's
-- state-based actions -- so a permanent that was on the battlefield when some
-- OTHER event in the same batch happened, and is gone by the time the scan
-- looks, has to be recovered from CR 608.2h last known information exactly as
-- CR 603.10a's look-back already recovers a departure event's own permanent.
--
-- Lightning Skelemental is the card: {B}{R}{R} Creature -- Elemental Skeleton
-- 6/1, "Trample, haste / Whenever this creature deals combat damage to a
-- player, that player discards two cards. / At the beginning of the end step,
-- sacrifice this creature." Its 1 toughness and CR 702.19b's trample are what
-- put the trigger's event and the bearer's death in ONE batch: the excess
-- reaches bob while the blocker's damage kills the Skelemental at the very next
-- CR 704.5g check, before any player gets priority.
bystanderTests :: Registry.Type.Registry -> Tasty.TestTree
bystanderTests registry =
  Tasty.testGroup
    "Bystander"
    [ -- The proving test. bob holds THREE cards, so "discarded once" (one left)
      -- is distinguishable from "discarded twice" (none) and from "not at all"
      -- (three).
      HU.testCase "CR 603.10 whole cards: Lightning Skelemental dies to its blocker and STILL makes bob discard two" $ do
        skelemental <- Registry.printing registry "Lightning Skelemental"
        piker <- Registry.printing registry "Goblin Piker"
        case S.combatBoardOf [skelemental] [piker] of
          (base, [attacker], [blocker]) -> do
            let gs = List.foldl' (\g _ -> snd (S.addHandCard piker S.bob g)) base [(), (), ()]
                after = S.runCombat tramplingAnswer gs
            HU.assertEqual "bob starts with three cards" 3 (S.handSize S.bob gs)
            HU.assertEqual "CR 702.19b: five trampled through to bob" (Just 15) (S.lifeOf S.bob after)
            HU.assertBool "CR 704.5g: the Piker's two killed the 6/1" (not (S.onBattlefield attacker after))
            HU.assertBool "and the Piker died to its one" (not (S.onBattlefield blocker after))
            HU.assertEqual "the Skelemental is in alice's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after))
            HU.assertEqual "and bob discarded two, exactly once" 1 (S.handSize S.bob after)
          _ -> HU.assertFailure "fixture should give alice one attacker and bob one blocker",
      -- The control leg, which passes with or without the bystander recovery:
      -- unblocked, the Skelemental is still on the battlefield at the boundary,
      -- so `onBattlefield` carries it and the same trigger fires from the
      -- ordinary candidate source. It is what makes the card data and the
      -- reserved "that player" slot innocent when the leg above fails.
      HU.testCase "CR 510.1b control: an UNBLOCKED Skelemental survives and makes bob discard two the ordinary way" $ do
        skelemental <- Registry.printing registry "Lightning Skelemental"
        piker <- Registry.printing registry "Goblin Piker"
        case S.combatBoardOf [skelemental] [] of
          (base, [attacker], []) -> do
            let gs = List.foldl' (\g _ -> snd (S.addHandCard piker S.bob g)) base [(), (), ()]
                after = S.runCombat tramplingAnswer gs
            HU.assertBool "the Skelemental is still on the battlefield" (S.onBattlefield attacker after)
            HU.assertEqual "bob took all six" (Just 14) (S.lifeOf S.bob after)
            HU.assertEqual "and discarded two" 1 (S.handSize S.bob after)
          _ -> HU.assertFailure "fixture should give alice one attacker and bob no blockers",
      -- The OTHER shape the rule reaches, at the gather rather than through a
      -- whole turn: a CR 603.2b step trigger whose bearer is gone by the
      -- boundary. Khabál Ghoul ("At the beginning of each end step, put a +1/+1
      -- counter on Khabál Ghoul for each creature that died this turn") is the
      -- bearer; the end step's beginning and the Ghoul's own death are two
      -- events in one unscanned batch, and the step event comes FIRST, so
      -- nothing about the Ghoul's own departure event can be what recovers it.
      HU.testCase "CR 603.10 a StepBegins bearer that dies later in the same batch still triggers" $ do
        khabalGhoul <- Registry.printing registry "Khabál Ghoul"
        let (ghoul, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
            began = S.withEvents [GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice] gs0
            dead = S.runPure S.identityAnswer began (Event.destroy Regenerability.Regenerable [ghoul])
            triggers = fst (Event.gatherTriggers (Event.unscannedEvents dead) dead)
        HU.assertEqual "the Ghoul really did leave the battlefield" Nothing (Game.lookupObject ghoul dead)
        HU.assertEqual "its step trigger still fired" [TriggerSource.OfObject ghoul] (fmap PendingTrigger.source triggers)
        HU.assertEqual "under alice, who controlled it as it left (CR 603.3a)" [S.alice] (fmap PendingTrigger.controller triggers),
      -- The discriminating twin: a bearer that left the battlefield BEFORE the
      -- step began did not exist immediately after that event, and gets nothing.
      -- Same board, same two events, opposite order.
      HU.testCase "CR 603.10 a bearer that had already left before the event does NOT trigger" $ do
        khabalGhoul <- Registry.printing registry "Khabál Ghoul"
        let (ghoul, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
            dead = S.runPure S.identityAnswer gs0 (Event.destroy Regenerability.Regenerable [ghoul])
            began = S.runPure S.identityAnswer dead (State.modify' (Event.recordEvent (GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice)))
            triggers = fst (Event.gatherTriggers (Event.unscannedEvents began) began)
        HU.assertEqual "the Ghoul is gone" Nothing (Game.lookupObject ghoul began)
        HU.assertEqual "and nothing triggered" [] (fmap PendingTrigger.source triggers)
    ]

-- CR 400.7e's slot read from the OTHER direction of a zone change: an entry.
-- "Abilities that trigger when an object moves from one zone to another ... can
-- find the new object that it became in the zone it moved to when the ability
-- triggered, if that zone is a public zone" -- and CR 400.2 lists the
-- battlefield among the public zones, so an enters trigger's payload may name
-- the entrant with no proviso to check.
--
-- Aether Flash, {2}{R}{R} Enchantment, "Whenever a creature enters, this
-- enchantment deals 2 damage to it." Soul Warden proved the CONDITION
-- (permanentEntersTests above); its "you gain 1 life" names nothing about the
-- creature that entered. This is the first card whose EFFECT refers back to the
-- entrant.
--
-- The contrast with becameSlotTests is the point of reusing one slot name.
-- There the bearer and the entrant are two incarnations of ONE card, and
-- `became` is the second of them; here the bearer is the enchantment and the
-- entrant is a different card entirely. CR 400.7e distinguishes neither
-- situation: it names "the new object that IT became", where "it" is whatever
-- moved, and the moved object being the bearer is a fact about the condition
-- rather than about the slot.
--
-- Goblin Piker ({1}{R} Creature -- Goblin Warrior 2/1) and Ogre Sentry ({1}{R}
-- Creature -- Ogre Warrior 3/3, defender) are the pair: identical costs and
-- colors, so two Mountains cast either, and the ONLY difference the test can be
-- reading is the toughness the 2 damage is measured against.
aetherFlashTests :: Registry.Type.Registry -> Tasty.TestTree
aetherFlashTests registry =
  let -- alice: Aether Flash already on the battlefield and two Mountains, with
      -- one creature card in hand. Casting it is the only thing on offer, so
      -- S.identityAnswer needs no bespoke interpreter.
      flashBoard creature = do
        mountain <- Registry.printing registry "Mountain"
        aetherFlash <- Registry.printing registry "Aether Flash"
        entrant <- Registry.printing registry creature
        let (flashId, withFlash) = S.addCreature aetherFlash S.alice (S.landsInPlay mountain 2)
        pure (flashId, S.handOne entrant withFlash)
      castIt (gs, spellId) =
        let cast = S.runPure S.identityAnswer gs (Cast.castSpell S.alice spellId)
         in S.runPure S.identityAnswer cast Engine.priorityLoop
      namesIn zone pid gs =
        fmap Card.Type.name (Maybe.mapMaybe (\oid -> Game.cardOf oid gs) (Game.zoneMembers zone pid gs))
      damageEventsIn gs = Maybe.mapMaybe Event.damageOf (Foldable.toList (GameState.events gs))
      -- CR 120.3e's marked damage on the one battlefield permanent with this
      -- name. Nothing if it is not there, or if there is more than one of it.
      markedOn name gs =
        case filter (\oid -> fmap Card.Type.name (Game.cardOf oid gs) == Just name) (Set.toList (GameState.battlefield gs)) of
          [oid] -> fmap Object.damage (Game.lookupObject oid gs)
          _ -> Nothing
      pikerName = Text.pack "Goblin Piker"
      sentryName = Text.pack "Ogre Sentry"
   in Tasty.testGroup
        "CR 400.7e the entrant an enters trigger names"
        [ -- The gameplay-level proof, cast to resolution. The discriminating
          -- assertion is the GRAVEYARD: an ability whose `became` slot went
          -- unbound would resolve, find nothing under it and silently deal no
          -- damage, leaving a live 2/1 on the battlefield.
          HU.testCase "CR 603.6a whole card: a Goblin Piker enters and Aether Flash's 2 damage kills it (CR 704.5g)" $ do
            (flashId, board) <- flashBoard "Goblin Piker"
            let after = castIt board
            HU.assertEqual "the Piker is not on the battlefield" 0 (S.countOnBattlefieldByName pikerName S.alice after)
            HU.assertEqual "it is in the graveyard, once" [pikerName] (namesIn Zone.Graveyard S.alice after)
            -- Falsifiers. The damage went to the creature, not to a player
            -- (CR 120.1a admits only battles, creatures and planeswalkers), and
            -- Aether Flash did not damage itself into the graveyard either.
            HU.assertEqual "alice's life is untouched" (Just 20) (S.lifeOf S.alice after)
            HU.assertEqual "bob's too" (Just 20) (S.lifeOf S.bob after)
            HU.assertBool "and the enchantment is still on the battlefield" (Set.member flashId (GameState.battlefield after))
            HU.assertEqual "exactly one damage event, of 2" [2] (fmap DamageEvent.amount (damageEventsIn after)),
          -- The control, differing only in the entrant's toughness: 2 damage
          -- marked on a 3/3 is not lethal (CR 704.5g compares the total marked
          -- against toughness), so the creature stays and CARRIES the mark. The
          -- marked damage is what proves the effect landed at all -- without it
          -- "still on the battlefield" would also be what a no-op looks like.
          HU.testCase "CR 704.5g the control: an Ogre Sentry survives the same 2 damage, marked" $ do
            (_, board) <- flashBoard "Ogre Sentry"
            let after = castIt board
            HU.assertEqual "the Sentry is on the battlefield" 1 (S.countOnBattlefieldByName sentryName S.alice after)
            HU.assertEqual "the graveyard is empty" [] (namesIn Zone.Graveyard S.alice after)
            HU.assertEqual "with 2 damage marked on it" (Just 2) (markedOn sentryName after),
          -- eventBindings in isolation, so the binding is pinned to CR 400.7e
          -- rather than to Aether Flash's payload. The entrant is
          -- ZoneChange.object -- for an ENTRY the arriving incarnation is what
          -- the event is about, so `departed` would be the pre-move id of a card
          -- that is not on the battlefield at all.
          HU.testCase "CR 400.7e eventBindings binds the ENTRANT under became" $
            let castCard = ObjectId.MkObjectId 1
                entered = ObjectId.MkObjectId 2
                entry = GameEvent.Moved (ZoneChange.MkZoneChange castCard entered Zone.Stack Zone.Battlefield) S.emptyCharacteristics
             in HU.assertEqual
                  "became names the permanent that entered"
                  (Map.singleton Binding.became (Binding.toObject entered))
                  (Event.eventBindings (TriggerCondition.PermanentEnters (Filter.Type.HasCardType CardType.Creature)) entry),
          -- CR 603.6a's "EACH TIME an event puts one or more permanents onto
          -- the battlefield" met with a per-entrant payload: Dragon Fodder
          -- ({1}{R} Sorcery, "create two 1/1 red Goblin creature tokens") makes
          -- two entrants in one event, so one Aether Flash places two triggers
          -- and each has to name ITS OWN. Both tokens dying is what says so --
          -- two triggers sharing one binding would kill one token twice and
          -- leave the other standing.
          --
          -- A token is also the one entrant whose Moved event is
          -- battlefield-to-battlefield (Event.recordTokenEntry's pseudo-move,
          -- where `departed` and `object` are the same fresh id), so this is the
          -- shape where reading either field would look identical. It is here as
          -- the reminder that the fields agree for a token and only for a token.
          -- CR 704.5d then removes the dead tokens from the graveyard, so the
          -- damage's proof is the DamageDealt log, not a graveyard census.
          HU.testCase "CR 603.6a two tokens enter together and each trigger names its own" $ do
            mountain <- Registry.printing registry "Mountain"
            aetherFlash <- Registry.printing registry "Aether Flash"
            dragonFodder <- Registry.printing registry "Dragon Fodder"
            let (_, withFlash) = S.addCreature aetherFlash S.alice (S.landsInPlay mountain 2)
                (gs, spellId) = S.handOne dragonFodder withFlash
                after = castIt (gs, spellId)
            HU.assertEqual "no Goblin token survived" 0 (S.countOnBattlefieldByName (Text.pack "Goblin Token") S.alice after)
            HU.assertEqual "two damage events of 2, one per token" [2, 2] (fmap DamageEvent.amount (damageEventsIn after))
            HU.assertEqual
              "and they were dealt to two different objects"
              2
              (Set.size (Set.fromList (fmap DamageEvent.target (damageEventsIn after)))),
          -- CR 608.2h, the case Aether Flash makes reachable with no second
          -- card: "if the effect requires information from a specific object
          -- ... the effect uses the current information of that object if it's
          -- in the public zone it was expected to be in". Two Aether Flashes,
          -- one 2/1 entrant, two triggers -- and the first one's damage kills it
          -- at the next state-based-action check, so the second resolves with
          -- its entrant already gone from the battlefield it was expected to be
          -- on. CR 400.7 minted a fresh id for the graveyard card, so the effect
          -- does not follow it there.
          HU.testCase "CR 608.2h a second Aether Flash resolves with the entrant already dead, and deals nothing" $ do
            mountain <- Registry.printing registry "Mountain"
            aetherFlash <- Registry.printing registry "Aether Flash"
            piker <- Registry.printing registry "Goblin Piker"
            let (_, oneFlash) = S.addCreature aetherFlash S.alice (S.landsInPlay mountain 2)
                (_, twoFlashes) = S.addCreature aetherFlash S.alice oneFlash
                (gs, spellId) = S.handOne piker twoFlashes
                cast = S.runPure S.identityAnswer gs (Cast.castSpell S.alice spellId)
                -- The creature spell resolves and enters; the settle's CR 117.5
                -- scan places both triggers.
                entered = S.runPure S.identityAnswer cast Stack.resolveTop
                placed = S.runPure S.identityAnswer entered Engine.settleForPriority
                -- One trigger resolves for 2, and the settle after it is where
                -- CR 704.5g destroys the 2/1.
                hit = S.runPure S.identityAnswer placed Stack.resolveTop
                buried = S.runPure S.identityAnswer hit Engine.settleForPriority
                -- The second trigger, resolving against an id CR 400.7 deleted.
                after = S.runPure S.identityAnswer buried Stack.resolveTop
            HU.assertEqual "both triggers were placed" 2 (length (GameState.stack placed))
            HU.assertEqual "the Piker is dead before the second resolves" 0 (S.countOnBattlefieldByName pikerName S.alice buried)
            HU.assertEqual "one damage event so far" [2] (fmap DamageEvent.amount (damageEventsIn buried))
            HU.assertEqual "the second trigger did resolve" [] (GameState.stack after)
            HU.assertEqual "and dealt nothing: still one damage event" [2] (fmap DamageEvent.amount (damageEventsIn after))
            HU.assertEqual "the card is in the graveyard once, not twice" [pikerName] (namesIn Zone.Graveyard S.alice after)
        ]

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry = Tasty.testGroup "Pawl.TriggerSpec" [logTests registry, scanTests registry, permanentEntersTests registry, sacrificeTests registry, stateTriggerTests registry, historyTests registry, delayedTests registry, orderingTests registry, monarchOrderingTests registry, interveningTests registry, poisonousTests registry, cyclingTriggerTests registry, graveyardTriggerTests registry, diesTriggerTests registry, becameSlotTests registry, lookBackInterveningTests registry, strippedTriggerTests registry, bystanderTests registry, aetherFlashTests registry]
