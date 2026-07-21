-- Covers: Pawl.Projection (an object's CR 613 layer-5 colour), Pawl.Target
-- (NonblackCreatureTarget) and the P3a colour gates (Doom Blade, Crimson Wisps,
-- Aphotic Wisps, Bad Moon). Gameplay-level: each card is cast or resolved through
-- the stack and the resulting game state is asserted on.
module Pawl.ColorSpec where

import qualified Data.Set as Set
import qualified Pawl.Cards as Cards
import qualified Pawl.Projection as Projection
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.Color as Color
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Color"
    [ HU.testCase "CR 202.2 a mono-black card's colour is black" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (ratsId, gs) = S.addCreature (Cards.typhoidRatsPrinting cards) S.alice gs0
         in HU.assertEqual "black" (Set.singleton Color.Black) (Projection.colorsOf ratsId gs),
      HU.testCase "CR 202.2 a generic-plus-red cost is red, and generic contributes nothing" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (pikerId, gs) = S.addPiker cards S.alice gs0
         in HU.assertEqual "red" (Set.singleton Color.Red) (Projection.colorsOf pikerId gs),
      HU.testCase "CR 202.2b an object with no coloured mana symbols is colourless" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (myrId, gs) = S.addCreature (Cards.darksteelMyrPrinting cards) S.alice gs0
         in HU.assertEqual "colourless" Set.empty (Projection.colorsOf myrId gs),
      HU.testCase "CR 202.1 a land has no mana cost, so it is colourless" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (mtnId, gs) = S.addCreature (Cards.mountainPrinting cards) S.alice gs0
         in HU.assertEqual "colourless" Set.empty (Projection.colorsOf mtnId gs),
      HU.testCase "CR 702.114a devoid makes an object colourless despite a black mana cost" $
        -- THE FALSIFIER for "an object's colours are the coloured symbols in its
        -- mana cost": this card's cost is {1}{B} and it is colourless.
        let gs0 = Setup.emptyGame S.bothPlayers
            (droneId, gs) = S.addCreature (Cards.devoidDronePrinting cards) S.alice gs0
         in HU.assertEqual "colourless" Set.empty (Projection.colorsOf droneId gs)
    ]
