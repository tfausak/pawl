-- Covers Pawl.Mana: mana payment and castability.
module Pawl.ManaSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Card as Card
import qualified Pawl.Cast as Cast
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.AbilityCost as AbilityCost
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Mana as Mana.Type
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.ManaUnit as ManaUnit
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Cast `creature` off `nLands` copies of `land`, then resolve it.
resolvedCreature :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
resolvedCreature land creature nLands =
  let (base, oid) = S.handOne creature (S.landsInPlay land nLands)
      afterCast = snd (Engine.runGamePure S.identityAnswer base (Cast.castSpell S.alice oid))
   in snd (Engine.runGamePure S.identityAnswer afterCast Stack.resolveTop)

castabilityTests :: Tasty.TestTree
castabilityTests =
  Tasty.testGroup
    "Castability"
    [ HU.testCase "War Mammoth is cast off four Forests and resolves onto the battlefield" $
        let gs = resolvedCreature Card.forestPrinting Card.warMammothPrinting 4
         in do
              HU.assertEqual "stack empty" 0 (length (GameState.stack gs))
              HU.assertEqual "one creature in play" 1 (S.creaturesInPlay S.alice gs)
              HU.assertEqual "lands tapped" 4 (S.tappedCount S.alice gs),
      HU.testCase "Typhoid Rats is cast off one Swamp and resolves onto the battlefield" $
        let gs = resolvedCreature Card.swampPrinting Card.typhoidRatsPrinting 1
         in do
              HU.assertEqual "stack empty" 0 (length (GameState.stack gs))
              HU.assertEqual "one creature in play" 1 (S.creaturesInPlay S.alice gs)
              HU.assertEqual "lands tapped" 1 (S.tappedCount S.alice gs)
    ]

pikerCost :: ManaCost.ManaCost
pikerCost = ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)]

poolSize :: PlayerId.PlayerId -> GameState.GameState -> Int
poolSize pid gs = case Mana.poolOf pid gs of
  Mana.Type.MkMana units -> length units

manaTests :: Tasty.TestTree
manaTests =
  Tasty.testGroup
    "Mana"
    [ HU.testCase "CR 305.6 a Mountain's red mana ability comes from its subtype" $
        HU.assertEqual
          "red"
          (Just (ManaType.Colored Color.Red))
          (Mana.subtypeMana Subtype.Mountain),
      HU.testCase "a Goblin grants no mana ability" $
        HU.assertEqual "none" Nothing (Mana.subtypeMana Subtype.Goblin),
      HU.testCase "CR 305.6 Island taps blue, Plains taps white" $ do
        HU.assertEqual "island" (Just (ManaType.Colored Color.Blue)) (Mana.subtypeMana Subtype.Island)
        HU.assertEqual "plains" (Just (ManaType.Colored Color.White)) (Mana.subtypeMana Subtype.Plains),
      HU.testCase "an empty pool starts empty" $
        HU.assertEqual "empty" 0 (poolSize S.alice (S.mountainsInPlay 2)),
      HU.testCase "tapping a Mountain taps it and adds one red unit" $
        let gs = S.mountainsInPlay 1
         in case Game.zoneMembers Zone.Battlefield S.alice gs of
              [] -> HU.assertFailure "fixture should have one Mountain"
              oid : _ -> do
                let after = Mana.tapForMana oid gs
                HU.assertEqual "tapped" 1 (S.tappedCount S.alice after)
                HU.assertEqual
                  "pool"
                  (Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Red}])
                  (Mana.poolOf S.alice after),
      HU.testCase "two Mountains can pay {1}{R}" $
        HU.assertBool "affordable" (Mana.canPay S.alice pikerCost (S.mountainsInPlay 2)),
      HU.testCase "one Mountain cannot pay {1}{R}" $
        HU.assertBool "unaffordable" (not (Mana.canPay S.alice pikerCost (S.mountainsInPlay 1))),
      HU.testCase "no Mountains cannot pay {1}{R}" $
        HU.assertBool "unaffordable" (not (Mana.canPay S.alice pikerCost (S.mountainsInPlay 0))),
      HU.testCase "paying {1}{R} taps exactly two of three Mountains and leaves no float" $
        case Mana.payCost S.alice pikerCost (S.mountainsInPlay 3) of
          Nothing -> HU.assertFailure "three Mountains should pay {1}{R}"
          Just after -> do
            HU.assertEqual "tapped" 2 (S.tappedCount S.alice after)
            HU.assertEqual "no float" 0 (poolSize S.alice after),
      HU.testCase "CR 500.4 mana pools empty" $
        let gs = S.mountainsInPlay 1
         in case Game.zoneMembers Zone.Battlefield S.alice gs of
              [] -> HU.assertFailure "fixture should have one Mountain"
              oid : _ ->
                HU.assertEqual "emptied" 0 (poolSize S.alice (Mana.emptyManaPools (Mana.tapForMana oid gs))),
      HU.testCase "CR 305.6/305.7 an Urborg'd Mountain taps for black too" $
        let base = Setup.emptyGame S.bothPlayers
            (mountainId, g1) = S.addCreature Card.mountainPrinting S.alice base
            (_, gs) = S.addCreature Card.urborgPrinting S.alice g1
         in -- Urborg adds Swamp to all lands, so the Mountain taps for black too.
            do
              HU.assertBool "black available" (ManaType.Colored Color.Black `elem` Mana.manaTypesOf mountainId gs)
              HU.assertBool "red still available" (ManaType.Colored Color.Red `elem` Mana.manaTypesOf mountainId gs),
      HU.testCase "CR 305.6/305.7 a Blood Moon'd Urborg taps for red only" $
        let base = Setup.emptyGame S.bothPlayers
            (urborgId, g1) = S.addCreature Card.urborgPrinting S.alice base
            (_, gs) = S.addCreature Card.bloodMoonPrinting S.alice g1
         in do
              HU.assertBool "red available" (ManaType.Colored Color.Red `elem` Mana.manaTypesOf urborgId gs)
              HU.assertBool "black not available (stripped)" (ManaType.Colored Color.Black `notElem` Mana.manaTypesOf urborgId gs),
      HU.testCase "CR 605.1a a {T}: Add {G} ability is a mana ability" $
        let ab =
              ActivatedAbility.MkActivatedAbility
                { ActivatedAbility.cost = AbilityCost.MkAbilityCost {AbilityCost.additional = []},
                  ActivatedAbility.effects = [Effect.AddMana (ManaType.Colored Color.Green)],
                  ActivatedAbility.targetSpecs = Map.empty
                }
         in HU.assertBool "mana ability" (Mana.isManaAbility ab),
      HU.testCase "CR 605.1a an ability that targets is NOT a mana ability" $
        let ab =
              ActivatedAbility.MkActivatedAbility
                { ActivatedAbility.cost = AbilityCost.MkAbilityCost {AbilityCost.additional = []},
                  ActivatedAbility.effects = [Effect.AddMana (ManaType.Colored Color.Green)],
                  ActivatedAbility.targetSpecs = Map.singleton (SlotName.MkSlotName (Text.pack "x")) TargetSpec.AnyTarget
                }
         in HU.assertBool "targets -> not mana" (not (Mana.isManaAbility ab)),
      HU.testCase "CR 605.1a a damage ability is NOT a mana ability" $
        let ab =
              ActivatedAbility.MkActivatedAbility
                { ActivatedAbility.cost = AbilityCost.MkAbilityCost {AbilityCost.additional = []},
                  ActivatedAbility.effects = [Effect.DealDamage (SlotName.MkSlotName (Text.pack "x")) (Quantity.Literal 1)],
                  ActivatedAbility.targetSpecs = Map.singleton (SlotName.MkSlotName (Text.pack "x")) TargetSpec.AnyTarget
                }
         in HU.assertBool "no mana produced -> not mana" (not (Mana.isManaAbility ab)),
      HU.testCase "CR 605 a settled Llanowar Elves is a green mana source" $
        let (elfId, gs) = S.addCreature Card.llanowarElvesPrinting S.alice (Setup.emptyGame S.bothPlayers)
         in do
              HU.assertBool "taps green" (elem (ManaType.Colored Color.Green) (Mana.manaTypesOf elfId gs))
              HU.assertBool "is a mana source" (elem elfId (Mana.manaSources S.alice gs)),
      HU.testCase "CR 302.6 a summoning-sick Llanowar Elves is NOT a mana source" $
        let (elfId, g0) = S.addCreature Card.llanowarElvesPrinting S.alice (Setup.emptyGame S.bothPlayers)
            sick = g0 {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) elfId (GameState.objects g0)}
         in HU.assertBool "sick elf excluded" (notElem elfId (Mana.manaSources S.alice sick))
    ]

tests :: Tasty.TestTree
tests = Tasty.testGroup "Mana" [manaTests, castabilityTests]
