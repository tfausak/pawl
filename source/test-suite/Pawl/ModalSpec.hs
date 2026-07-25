{-# LANGUAGE GADTs #-}

-- Covers modal casting: Pawl.Cast mode selection, Pawl.Resolve chosen-mode
-- resolution.
module Pawl.ModalSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Activate as Activate
import qualified Pawl.Binding as Binding
import qualified Pawl.Cast as Cast
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Modal as Modal
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Target as Target
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.Effect as Effect
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Filter may later be imported and must not collide.
import qualified Pawl.Type.Filter as Filter.Type
import qualified Pawl.Type.GameEvent as GameEvent
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Modal as ModalT
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeIndex as ModeIndex
import qualified Pawl.Type.ModeSelection as ModeSelection
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Pool as Pool
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChange as ZoneChange
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Chaos Charm's three modes, in printed order (CR 700.2 / data/cards/chaos-charm.json):
--   0. destroy target Wall
--   1. deal 1 damage to target creature
--   2. target creature gains haste until end of turn
-- An answerer that always chooses `idx` and, when asked for a target, aims
-- every slot at `recipient` -- Chaos Charm's modes each have exactly one slot,
-- so a single fixed recipient is unambiguous. Everything else defers to
-- S.identityAnswer.
chooseModeAt :: ModeIndex.ModeIndex -> Recipient.Recipient -> Prompt.Prompt r -> r
chooseModeAt idx recipient p = case p of
  Prompt.ChooseModes {} -> Set.singleton idx
  Prompt.ChooseTargets _ _ _ sets -> fmap (const recipient) sets
  _ -> S.identityAnswer p

-- Rejects a ChooseModes prompt outright -- used to prove a non-modal cast
-- never issues one (CR 700.2a: a forced selection is not asked). `error` here
-- is a deliberately unreachable branch, not library code.
neverAskModes :: Prompt.Prompt r -> r
neverAskModes p = case p of
  Prompt.ChooseModes {} -> error "ChooseModes prompt issued for a non-modal spell"
  _ -> S.identityAnswer p

gateTests :: Registry.Type.Registry -> Tasty.TestTree
gateTests registry =
  Tasty.testGroup
    "Gate"
    [ HU.testCase "CR 608.2c mode 1 (damage) deals 1 to the chosen creature" $ do
        chaosCharm <- Registry.printing registry "Chaos Charm"
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs0, oid) = S.handOne chaosCharm (S.landsInPlay mountain 1)
            (pikerOid, gs1) = S.addCreature piker S.bob gs0
            answer :: Prompt.Prompt r -> r
            answer = chooseModeAt (ModeIndex.MkModeIndex 1) (Recipient.ToCreature pikerOid)
            cast = snd (Engine.runGamePure answer gs1 (Cast.castSpell S.alice oid))
            after = snd (Engine.runGamePure answer cast Stack.resolveTop)
        HU.assertEqual "1 damage marked" (Just 1) (S.damageOf pikerOid after),
      HU.testCase "CR 608.2c mode 2 (haste) grants haste to the chosen (summoning-sick) creature" $ do
        chaosCharm <- Registry.printing registry "Chaos Charm"
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs0, oid) = S.handOne chaosCharm (S.landsInPlay mountain 1)
            (creatureId, gs1) = S.addCreature piker S.alice gs0
            sick = gs1 {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) creatureId (GameState.objects gs1)}
            answer :: Prompt.Prompt r -> r
            answer = chooseModeAt (ModeIndex.MkModeIndex 2) (Recipient.ToCreature creatureId)
            cast = snd (Engine.runGamePure answer sick (Cast.castSpell S.alice oid))
            after = snd (Engine.runGamePure answer cast Stack.resolveTop)
        HU.assertBool "projected keywords include Haste" (Projection.hasKeyword Keyword.Haste creatureId after),
      HU.testCase "CR 608.2c mode 0 (destroy Wall) destroys the chosen Wall" $ do
        chaosCharm <- Registry.printing registry "Chaos Charm"
        mountain <- Registry.printing registry "Mountain"
        wallOfStone <- Registry.printing registry "Wall of Stone"
        let (gs0, oid) = S.handOne chaosCharm (S.landsInPlay mountain 1)
            (wallId, gs1) = S.addCreature wallOfStone S.bob gs0
            answer :: Prompt.Prompt r -> r
            answer = chooseModeAt (ModeIndex.MkModeIndex 0) (Recipient.ToCreature wallId)
            cast = snd (Engine.runGamePure answer gs1 (Cast.castSpell S.alice oid))
            after = snd (Engine.runGamePure answer cast Stack.resolveTop)
        HU.assertBool "no longer on the battlefield" (not (Set.member wallId (GameState.battlefield after)))
        HU.assertEqual "in bob's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after))
    ]

-- CR 700.2a: an illegal mode can't be chosen, so a mode with even one
-- unfillable slot is excluded wholesale -- the falsifier for the M3a
-- all-slots-fillable engine, which would have called Chaos Charm uncastable
-- the moment ANY mode (here, the Wall mode with no Wall in play) had an
-- unfillable slot.
falsifierTests :: Registry.Type.Registry -> Tasty.TestTree
falsifierTests registry =
  Tasty.testGroup
    "Falsifier"
    [ HU.testCase "CR 700.2c/601.2c castable via the damage/haste modes with no Wall on the board" $ do
        chaosCharm <- Registry.printing registry "Chaos Charm"
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs0, oid) = S.handOne chaosCharm (S.landsInPlay mountain 1)
            (_, gs1) = S.addCreature piker S.bob gs0
        HU.assertBool "castable" (Cast.castable S.alice oid gs1)
        HU.assertEqual
          "the Wall mode (0) is absent from the fillable set"
          (Set.fromList [ModeIndex.MkModeIndex 1, ModeIndex.MkModeIndex 2])
          (Target.fillableModes oid Map.empty (Card.Type.spell (Printing.card chaosCharm)) gs1)
    ]

-- CR 601.2c/700.2c: only the CHOSEN mode's slots are ever prompted or stamped
-- on the stack object -- an unchosen mode's slot name never appears at all.
onlyChosenModeTests :: Registry.Type.Registry -> Tasty.TestTree
onlyChosenModeTests registry =
  Tasty.testGroup
    "OnlyChosenModeTargets"
    [ HU.testCase "casting the damage mode binds the 'creature' slot, never 'wall'" $ do
        chaosCharm <- Registry.printing registry "Chaos Charm"
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs0, oid) = S.handOne chaosCharm (S.landsInPlay mountain 1)
            (pikerOid, gs1) = S.addCreature piker S.bob gs0
            answer :: Prompt.Prompt r -> r
            answer = chooseModeAt (ModeIndex.MkModeIndex 1) (Recipient.ToCreature pikerOid)
            cast = snd (Engine.runGamePure answer gs1 (Cast.castSpell S.alice oid))
        case Game.zoneMembers Zone.Stack S.alice cast of
          [] -> HU.assertFailure "Chaos Charm never reached the stack"
          stackId : _ -> case Game.lookupObject stackId cast of
            Nothing -> HU.assertFailure "the cast stack object vanished"
            Just obj ->
              let keys = Map.keysSet (Object.bindings obj)
               in do
                    HU.assertBool "has the 'creature' slot" (Set.member (SlotName.MkSlotName (Text.pack "creature")) keys)
                    HU.assertBool "does not have the 'wall' slot" (not (Set.member (SlotName.MkSlotName (Text.pack "wall")) keys))
    ]

fizzleTests :: Registry.Type.Registry -> Tasty.TestTree
fizzleTests registry =
  Tasty.testGroup
    "Fizzle"
    [ HU.testCase "CR 608.2b the damage mode fizzles when its only target leaves before resolution" $ do
        chaosCharm <- Registry.printing registry "Chaos Charm"
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
        let (gs0, oid) = S.handOne chaosCharm (S.landsInPlay mountain 1)
            (pikerOid, gs1) = S.addCreature piker S.bob gs0
            answer :: Prompt.Prompt r -> r
            answer = chooseModeAt (ModeIndex.MkModeIndex 1) (Recipient.ToCreature pikerOid)
            cast = snd (Engine.runGamePure answer gs1 (Cast.castSpell S.alice oid))
            -- CR 400.7: leaving the battlefield mints a new incarnation, so
            -- pikerOid's chosen recipient no longer names a legal target.
            gone = S.runPure S.identityAnswer cast (Event.changeZone pikerOid Zone.Graveyard)
            after = snd (Engine.runGamePure answer gone Stack.resolveTop)
        HU.assertEqual "Chaos Charm in alice's graveyard, unresolved" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after))
        HU.assertEqual "no damage was dealt" [] (S.damageEventsOf after)
        HU.assertEqual "stack empty" 0 (length (GameState.stack after))
    ]

forcedTests :: Registry.Type.Registry -> Tasty.TestTree
forcedTests registry =
  Tasty.testGroup
    "ForcedNoPrompt"
    [ HU.testCase "CR 700.2a casting a non-modal spell (Lightning Bolt) never issues ChooseModes" $ do
        -- No creature on the battlefield, so neverAskModes's identityAnswer
        -- fallback picks ToPlayer alice via Set.lookupMin (a self-Bolt, the same
        -- shape as ResolveSpec's "CR 120.3a a Bolt at a player drains life
        -- without marking"). The point of this test is that ChooseModes is never
        -- reached at all -- if it were, neverAskModes's error would fire.
        mountain <- Registry.printing registry "Mountain"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let (gs0, oid) = S.boltInHand mountain lightningBolt 1 Phase.PrecombatMain
            cast = snd (Engine.runGamePure neverAskModes gs0 (Cast.castSpell S.alice oid))
            after = snd (Engine.runGamePure neverAskModes cast Stack.resolveTop)
        HU.assertEqual "alice at 17 (Bolt resolved, forced/unprompted mode selection)" (Just 17) (S.lifeOf S.alice after)
    ]

-- M4h task 1: Aether Channeler's "another nonland permanent" slot as data --
-- Pool.Permanents narrowed by Not (HasCardType Land), with CR 601.2c's "another"
-- as the Not IsSource conjunct (#163). This proves the spec and the exclusion in
-- isolation; the wiring to a consumer is a later M4h task.
nonlandPermanentTargetTests :: Registry.Type.Registry -> Tasty.TestTree
nonlandPermanentTargetTests registry =
  Tasty.testGroup
    "M4h NonlandPermanentTarget"
    [ HU.testCase "NonlandPermanentTarget excludes lands (CR 109.2/110.4)" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mindslaver <- Registry.printing registry "Mindslaver"
        mountain <- Registry.printing registry "Mountain"
        let gs = S.boardWithCreatureArtifactLand piker mindslaver mountain
            got = Target.legalRecipients S.noSource (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.Not (Filter.Type.HasCardType CardType.Land)))) gs
        HU.assertEqual
          "two nonland permanents, no land"
          (Set.fromList [Recipient.ToObject (S.creatureId gs), Recipient.ToObject (S.artifactId gs)])
          got,
      HU.testCase "Not IsSource drops the source (CR \"another\")" $ do
        piker <- Registry.printing registry "Goblin Piker"
        mindslaver <- Registry.printing registry "Mindslaver"
        mountain <- Registry.printing registry "Mountain"
        let gs = S.boardWithCreatureArtifactLand piker mindslaver mountain
            nonlandOther = Filter.Type.And [Filter.Type.Not (Filter.Type.HasCardType CardType.Land), Filter.Type.Not Filter.Type.IsSource]
            specs = Map.singleton (SlotName.MkSlotName (Text.pack "x")) (TargetSpec.MkTargetSpec Pool.Permanents (Just nonlandOther))
            got = Target.legalSets (S.creatureId gs) specs gs
        HU.assertEqual
          "source excluded from its own set"
          (Map.singleton (SlotName.MkSlotName (Text.pack "x")) (Set.singleton (Recipient.ToObject (S.artifactId gs))))
          got
    ]

-- M4h task 2: the mode-scoped reader folds, lifted off Pawl.Card onto
-- Pawl.Modal (a card-free Modal card -> ... shape shared by the spell and,
-- later, both ability types). Card.Type.Card just fixes the ambiguous `card`
-- type parameter -- these two Modes never mention a card value.
modalReaderTests :: Tasty.TestTree
modalReaderTests =
  Tasty.testGroup
    "M4h Modal reader"
    [ HU.testCase "modesEffects reads only chosen modes, ModeIndex order" $ do
        let m =
              ModalT.MkModal
                ( Seq.fromList
                    [ Mode.MkMode (Seq.singleton (Effect.Draw (Quantity.Literal 1))) Map.empty,
                      Mode.MkMode (Seq.singleton (Effect.Draw (Quantity.Literal 2))) Map.empty
                    ]
                )
                (ModeSelection.ChooseExactly 1) ::
                ModalT.Modal Card.Type.Card
            chosen = Set.singleton (ModeIndex.MkModeIndex 1)
        HU.assertEqual "only mode 1's effect" [Effect.Draw (Quantity.Literal 2)] (Modal.modesEffects chosen m)
        HU.assertEqual "selectionCount is the ChooseExactly count" 1 (Modal.selectionCount m)
    ]

-- M4h task 4: CR 602.2b -- the activation path prompts ChooseModes exactly like
-- Cast.castSpell does, gated by a synthetic modal activated ability (no real one
-- exists in the pool yet: data/cards/synthetic-modal-activated.json, a {2} 2/2
-- whose lone {1} ability is ChooseExactly 1 over two CreatureTarget modes --
-- deal 1 damage, or put a +1/+1 counter). Both modes' target sets are nonempty
-- with just the activator and one victim on the battlefield (CreatureTarget
-- does not self-exclude), so `legal = {0,1}` and `count = 1`: a real prompt,
-- not the forced/single-mode path.
activationModalTests :: Registry.Type.Registry -> Tasty.TestTree
activationModalTests registry =
  Tasty.testGroup
    "M4h activation modal (CR 602.2b)"
    [ HU.testCase "activating a modal ability prompts the mode; only the chosen mode resolves" $ do
        syntheticModalActivated <- Registry.printing registry "Synthetic Modal Activator"
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
        case Card.Type.activatedAbilities (Printing.card syntheticModalActivated) of
          [ability] ->
            let gs0 = S.landsInPlay mountain 1
                (srcId, gs1) = S.addCreature syntheticModalActivated S.alice gs0
                (victimId, gs2) = S.addCreature piker S.bob gs1
                answer :: Prompt.Prompt r -> r
                answer = chooseModeAt (ModeIndex.MkModeIndex 0) (Recipient.ToCreature victimId)
                activated = snd (Engine.runGamePure answer gs2 (Activate.activateAbility S.alice srcId ability))
                resolved = snd (Engine.runGamePure answer activated Stack.resolveTop)
             in case Game.lookupObject victimId resolved of
                  Nothing -> HU.assertFailure "the victim vanished"
                  Just obj -> do
                    HU.assertEqual "victim took 1 damage (mode 0 only)" (Just 1) (S.damageOf victimId resolved)
                    HU.assertEqual "no +1/+1 counter (mode 1 never resolved)" Nothing (Map.lookup CounterKind.PlusOnePlusOne (Object.counters obj))
          _ -> HU.assertFailure "the fixture must have exactly one activated ability",
      HU.testCase "CR 608.2b the chosen mode fizzles when its only target leaves before resolution" $ do
        syntheticModalActivated <- Registry.printing registry "Synthetic Modal Activator"
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
        case Card.Type.activatedAbilities (Printing.card syntheticModalActivated) of
          [ability] ->
            let gs0 = S.landsInPlay mountain 1
                (srcId, gs1) = S.addCreature syntheticModalActivated S.alice gs0
                (victimId, gs2) = S.addCreature piker S.bob gs1
                answer :: Prompt.Prompt r -> r
                answer = chooseModeAt (ModeIndex.MkModeIndex 0) (Recipient.ToCreature victimId)
                activated = snd (Engine.runGamePure answer gs2 (Activate.activateAbility S.alice srcId ability))
                -- CR 400.7: leaving the battlefield mints a new incarnation, so
                -- victimId's chosen recipient no longer names a legal target.
                gone = S.runPure S.identityAnswer activated (Event.changeZone victimId Zone.Graveyard)
                resolved = snd (Engine.runGamePure answer gone Stack.resolveTop)
             in do
                  HU.assertEqual "no damage was dealt" [] (S.damageEventsOf resolved)
                  HU.assertEqual "stack empty" 0 (length (GameState.stack resolved))
          _ -> HU.assertFailure "the fixture must have exactly one activated ability"
    ]

-- M4h task 5: CR 700.2b/603.3d -- the TRIGGER-placement path (Engine.placeOne)
-- prompts the mode and, for the chosen mode(s), the targets, exactly like
-- Cast.castSpell does for a spell. Gated by Aether Channeler (a {2}{U} 3/3 Human
-- Wizard whose ETB is ChooseExactly 1 over: create a 1/1 flying Bird, return
-- ANOTHER nonland permanent to hand, or draw a card). Aether Channeler enters
-- via S.addCreature and the SelfEnters trigger is fed to Engine.placePendingTriggers
-- through a hand-built enters event (the same shape EventSpec uses), then the
-- placed ability resolves off the stack.
-- Put Aether Channeler on the battlefield and craft its enters event, so
-- placePendingTriggers sees the SelfEnters trigger pending.
triggerModalEtb :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
triggerModalEtb acPrinting pid gs0 =
  let (acId, gs1) = S.addCreature acPrinting pid gs0
      entered = ZoneChange.MkZoneChange acId Zone.Stack Zone.Battlefield
   in (acId, S.withEvents [GameEvent.Moved entered (Projection.project acId gs1)] gs1)

triggerModalOf :: Printing.Printing -> Maybe (ModalT.Modal Card.Type.Card)
triggerModalOf acPrinting = case Card.Type.triggeredAbilities (Printing.card acPrinting) of
  [ab] -> Just (TriggeredAbility.modal ab)
  _ -> Nothing

triggerModalTests :: Registry.Type.Registry -> Tasty.TestTree
triggerModalTests registry =
  Tasty.testGroup
    "M4h trigger modal (CR 700.2b/603.3d)"
    [ HU.testCase "create mode ({0}) makes a 1/1 flying Bird; nothing bounced or drawn" $ do
        aetherChanneler <- Registry.printing registry "Aether Channeler"
        let (acId, gs) = triggerModalEtb aetherChanneler S.alice (Setup.emptyGame S.bothPlayers)
            answer :: Prompt.Prompt r -> r
            answer = chooseModeAt (ModeIndex.MkModeIndex 0) (Recipient.ToObject acId)
            placed = snd (Engine.runGamePure answer gs Engine.placePendingTriggers)
            resolved = snd (Engine.runGamePure answer placed Stack.resolveTop)
            newObjs = Set.toList (Set.delete acId (GameState.battlefield resolved))
        HU.assertEqual "alice's hand is still empty (nothing drawn)" 0 (length (Game.zoneMembers Zone.Hand S.alice resolved))
        HU.assertEqual "Aether Channeler still on the battlefield (nothing bounced)" 1 (S.countOnBattlefieldByName (Text.pack "Aether Channeler") S.alice resolved)
        case newObjs of
          [tokId] -> do
            HU.assertEqual "the token is named Bird" (Just (Text.pack "Bird")) (fmap Card.Type.name (Game.cardOf tokId resolved))
            HU.assertBool "the Bird has flying (projected)" (Projection.hasKeyword Keyword.Flying tokId resolved)
          _ -> HU.assertFailure "expected exactly one new (Bird token) permanent",
      HU.testCase "bounce mode ({1}) returns another nonland permanent to its owner's hand (CR 601.2c)" $ do
        aetherChanneler <- Registry.printing registry "Aether Channeler"
        piker <- Registry.printing registry "Goblin Piker"
        let (_, gs1) = triggerModalEtb aetherChanneler S.alice (Setup.emptyGame S.bothPlayers)
            (victimId, gs2) = S.addCreature piker S.bob gs1
            answer :: Prompt.Prompt r -> r
            answer = chooseModeAt (ModeIndex.MkModeIndex 1) (Recipient.ToObject victimId)
            placed = snd (Engine.runGamePure answer gs2 Engine.placePendingTriggers)
            resolved = snd (Engine.runGamePure answer placed Stack.resolveTop)
            boundSlots = case GameState.stack placed of
              abilId : _ -> case Game.lookupObject abilId placed of
                Just obj -> Just (Map.keysSet (Binding.targetsOf (Object.bindings obj)))
                Nothing -> Nothing
              [] -> Nothing
        HU.assertEqual "victim is in bob's hand" 1 (length (Game.zoneMembers Zone.Hand S.bob resolved))
        HU.assertBool "victim no longer on the battlefield" (not (Set.member victimId (GameState.battlefield resolved)))
        -- M4.5 P4 Task 3 (CR 113.7): EVERY placed trigger now also carries
        -- its source under the reserved Binding.triggerSource ("self") slot
        -- -- Engine.placeOne stamps it unconditionally, not only for a
        -- Sacrifice-using card, and Binding.targetsOf projects it right
        -- alongside a real chosen target (the same field). So "permanent"
        -- (the real chosen target) is no longer the only bound slot.
        --
        -- M4.5 P4 Task 8 (CR 109.5): Engine.placeOne now ALSO stamps the
        -- ability's controller under the reserved Binding.you ("you") slot,
        -- unconditionally and for the same reason -- so it joins "self" here
        -- too, whether or not this card's own text ever reads it.
        HU.assertEqual
          "the 'permanent' slot and the reserved self/you slots are bound"
          (Just (Set.fromList [SlotName.MkSlotName (Text.pack "permanent"), Binding.triggerSource, Binding.you]))
          boundSlots,
      HU.testCase "draw mode ({2}) draws exactly one; no token made" $ do
        aetherChanneler <- Registry.printing registry "Aether Channeler"
        piker <- Registry.printing registry "Goblin Piker"
        let (acId, gs1) = triggerModalEtb aetherChanneler S.alice (Setup.emptyGame S.bothPlayers)
            (_, gs2) = S.addLibraryCard piker S.alice gs1
            answer :: Prompt.Prompt r -> r
            answer = chooseModeAt (ModeIndex.MkModeIndex 2) (Recipient.ToObject acId)
            placed = snd (Engine.runGamePure answer gs2 Engine.placePendingTriggers)
            resolved = snd (Engine.runGamePure answer placed Stack.resolveTop)
        HU.assertEqual "alice drew one card" 1 (length (Game.zoneMembers Zone.Hand S.alice resolved))
        HU.assertEqual "no new permanent (mode 0 never resolved)" (Set.singleton acId) (GameState.battlefield resolved),
      HU.testCase "bounce mode ({1}) excludes Aether Channeler itself (CR \"another\")" $ do
        aetherChanneler <- Registry.printing registry "Aether Channeler"
        let (acId, gs) = triggerModalEtb aetherChanneler S.alice (Setup.emptyGame S.bothPlayers)
        case triggerModalOf aetherChanneler of
          Nothing -> HU.assertFailure "Aether Channeler must have exactly one triggered ability"
          Just modal ->
            HU.assertEqual
              "with Aether Channeler the only nonland permanent, only modes 0 and 2 are fillable"
              (Set.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 2])
              (Target.fillableModes acId Map.empty modal gs),
      -- CR 603.3c/700.2b: "If no mode is chosen, the ability is removed from
      -- the stack." This is the trigger-only rule -- a SPELL that can't choose
      -- is simply never offered (CR 601.2c elides the cast entirely, M4g), but
      -- a TRIGGER is already placed on the stack before modes are chosen
      -- (CR 603.3d), so an unfillable trigger must be taken back OFF the
      -- stack. Gated by a new synthetic fixture: Synthetic Modal Trigger's ETB
      -- is ChooseExactly 1 over two NonlandPermanentTarget (self-excluding,
      -- CR "another") modes; with the fixture as the ONLY nonland permanent on
      -- the board, both modes' legal target sets are empty, so NEITHER mode is
      -- fillable and the trigger must be removed rather than forced or
      -- prompted (CreatureTarget would be self-fillable here and could never
      -- exercise this path -- spec sec 9).
      HU.testCase "no legal mode removes the trigger from the stack (CR 603.3c)" $ do
        smtPrinting <- Registry.printing registry "Synthetic Modal Trigger"
        mountain <- Registry.printing registry "Mountain"
        let gs0 = S.landsInPlay mountain 2
            (smtId, gs1) = S.addCreature smtPrinting S.alice gs0
            entered = ZoneChange.MkZoneChange smtId Zone.Stack Zone.Battlefield
            gs2 = S.withEvents [GameEvent.Moved entered (Projection.project smtId gs1)] gs1
            answer :: Prompt.Prompt r -> r
            answer = S.identityAnswer
            placed = snd (Engine.runGamePure answer gs2 Engine.placePendingTriggers)
            stackHasTrigger =
              any
                ( \abilId -> case Game.lookupObject abilId placed of
                    Just obj -> case Object.source obj of
                      Source.OfTrigger _ _ -> True
                      _ -> False
                    Nothing -> False
                )
                (GameState.stack placed)
        HU.assertBool "the trigger was removed from the stack, not left lingering" (not stackHasTrigger)
        HU.assertEqual "Synthetic Modal Trigger still on the battlefield (nothing bounced or exiled)" 1 (S.countOnBattlefieldByName (Text.pack "Synthetic Modal Trigger") S.alice placed)
        HU.assertEqual "alice's hand is still empty (nothing bounced)" 0 (length (Game.zoneMembers Zone.Hand S.alice placed))
        HU.assertEqual "nothing was exiled" 0 (length (Game.zoneMembers Zone.Exile S.alice placed))
    ]

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Modal"
    [ gateTests registry,
      falsifierTests registry,
      onlyChosenModeTests registry,
      fizzleTests registry,
      forcedTests registry,
      nonlandPermanentTargetTests registry,
      modalReaderTests,
      activationModalTests registry,
      triggerModalTests registry
    ]
