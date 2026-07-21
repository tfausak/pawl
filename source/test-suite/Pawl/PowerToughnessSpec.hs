-- Covers: Pawl.Projection (CR 613 layer 7 -- 7a characteristic-defined P/T, the
-- CR 608.2h freeze that 7b's stored effects owe, and 7d P/T switching), Pawl.Quantity
-- (the counting quantity) and the P3b gates (Tarmogoyf, Inner Calm Outer Strength,
-- Twisted Image). Gameplay-level: each card is cast or resolved through the stack and
-- the resulting game state is asserted on.
module Pawl.PowerToughnessSpec where

import qualified Pawl.Cards as Cards
import qualified Pawl.Projection as Projection
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.CountSpec as CountSpec
import qualified Pawl.Type.ProjectedCharacteristics as PC
import qualified Pawl.Type.Quantity as Quantity.Type
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "PowerToughness"
    [ HU.testCase "CR 604.3 the seed carries the CDA as QUANTITIES, with the printed star substituted" $
        -- CR 707.2a: a copy acquires the ABILITY, so what the seed (and therefore
        -- the copiable value) holds must be unevaluated. Tarmogoyf's printed box is
        -- \*/1+*, so the pair is <count> and 1+<count>.
        let gs0 = Setup.emptyGame S.bothPlayers
            (goyfId, gs) = S.addCreature (Cards.tarmogoyfPrinting cards) S.alice gs0
            count = Quantity.Type.Count CountSpec.CardTypesInAllGraveyards
         in HU.assertEqual
              "the CDA pair"
              (Just (count, Quantity.Type.Plus (Quantity.Type.Literal 1) count))
              (PC.characteristicPT (Projection.baseCharacteristics goyfId gs)),
      HU.testCase "CR 613.4a no P/T value exists before layer 7a applies one" $
        -- The seed evaluates the printed Star, which is deliberately Nothing.
        let gs0 = Setup.emptyGame S.bothPlayers
            (goyfId, gs) = S.addCreature (Cards.tarmogoyfPrinting cards) S.alice gs0
            seeded = Projection.baseCharacteristics goyfId gs
         in do
              HU.assertEqual "no seeded power" Nothing (PC.power seeded)
              HU.assertEqual "no seeded toughness" Nothing (PC.toughness seeded),
      HU.testCase "an ordinary card has no CDA" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (pikerId, gs) = S.addPiker cards S.alice gs0
         in HU.assertEqual "none" Nothing (PC.characteristicPT (Projection.baseCharacteristics pikerId gs))
    ]
