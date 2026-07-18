-- Covers Pawl.Mana: mana payment and castability.
module Pawl.ManaSpec where

import qualified Pawl.Card as Card
import qualified Pawl.Cast as Cast
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Mana as Mana.Type
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.ManaUnit as ManaUnit
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Cast `creature` off `nLands` copies of `land`, then resolve it.
resolvedCreature :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
resolvedCreature land creature nLands =
  let (base, oid) = S.handOne creature (S.landsInPlay land nLands)
      afterCast = snd (Engine.runGamePure S.identityAnswer base (Cast.castSpell S.alice oid))
   in Stack.resolveTop afterCast

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
                HU.assertEqual "emptied" 0 (poolSize S.alice (Mana.emptyManaPools (Mana.tapForMana oid gs)))
    ]

tests :: Tasty.TestTree
tests = Tasty.testGroup "Mana" [manaTests, castabilityTests]
