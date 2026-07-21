-- Covers: Pawl.Projection (an object's CR 613 layer-5 colour), Pawl.Target
-- (NonblackCreatureTarget) and the P3a colour gates (Doom Blade, Crimson Wisps,
-- Aphotic Wisps, Bad Moon). Gameplay-level: each card is cast or resolved through
-- the stack and the resulting game state is asserted on.
module Pawl.ColorSpec where

import qualified Data.Set as Set
import qualified Pawl.Cards as Cards
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Append a stored continuous effect affecting exactly `oid`. Object id 996 is a
-- stand-in source: nothing in these tests reads the source's own characteristics.
withEffect :: ObjectId.ObjectId -> Modification.Modification -> GameState.GameState -> GameState.GameState
withEffect oid m gs =
  let (ts, gs1) = Game.freshTimestamp gs
      eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = ObjectId.MkObjectId 996,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.duration = Duration.UntilEndOfTurn,
            ContinuousEffect.modification = m,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
          }
   in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}

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
      HU.testCase "CR 202.1b a land has no mana cost, so it is colourless" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (mtnId, gs) = S.addCreature (Cards.mountainPrinting cards) S.alice gs0
         in HU.assertEqual "colourless" Set.empty (Projection.colorsOf mtnId gs),
      HU.testCase "CR 702.114a devoid makes an object colourless despite a black mana cost" $
        -- THE FALSIFIER for "an object's colours are the coloured symbols in its
        -- mana cost": this card's cost is {1}{B} and it is colourless.
        let gs0 = Setup.emptyGame S.bothPlayers
            (droneId, gs) = S.addCreature (Cards.devoidDronePrinting cards) S.alice gs0
         in HU.assertEqual "colourless" Set.empty (Projection.colorsOf droneId gs),
      HU.testCase "CR 105.3 a new colour REPLACES all previous colours" $
        -- THE FALSIFIER for implementing "becomes red" as an ADD: the Rats are
        -- black, and after the effect they are red and NOT black.
        let gs0 = Setup.emptyGame S.bothPlayers
            (ratsId, board) = S.addCreature (Cards.typhoidRatsPrinting cards) S.alice gs0
            gs = withEffect ratsId (Modification.SetColor (Set.singleton Color.Red)) board
         in HU.assertEqual "red only" (Set.singleton Color.Red) (Projection.colorsOf ratsId gs),
      HU.testCase "CR 105.3 an effect may make a coloured object colourless" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (ratsId, board) = S.addCreature (Cards.typhoidRatsPrinting cards) S.alice gs0
            gs = withEffect ratsId (Modification.SetColor Set.empty) board
         in HU.assertEqual "colourless" Set.empty (Projection.colorsOf ratsId gs),
      HU.testCase "CR 613.1e a layer-5 colour change beats the CR 702.114a devoid seed" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (droneId, board) = S.addCreature (Cards.devoidDronePrinting cards) S.alice gs0
            gs = withEffect droneId (Modification.SetColor (Set.singleton Color.Black)) board
         in HU.assertEqual "black" (Set.singleton Color.Black) (Projection.colorsOf droneId gs)
    ]
