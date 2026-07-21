-- Covers: Pawl.Projection (CR 613 layer 7 -- 7a characteristic-defined P/T, the
-- CR 608.2h freeze that 7b's stored effects owe, and 7d P/T switching), Pawl.Quantity
-- (the counting quantity) and the P3b gates (Tarmogoyf, Inner Calm Outer Strength,
-- Twisted Image). Gameplay-level: each card is cast or resolved through the stack and
-- the resulting game state is asserted on.
module Pawl.PowerToughnessSpec where

import qualified Pawl.Cards as Cards
import qualified Pawl.Cast as Cast
import qualified Pawl.Engine as Engine
import qualified Pawl.Projection as Projection
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.CountSpec as CountSpec
import qualified Pawl.Type.CounterKind as CounterKind
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
         in HU.assertEqual "none" Nothing (PC.characteristicPT (Projection.baseCharacteristics pikerId gs)),
      HU.testCase "CR 613.4a Tarmogoyf's P/T is recomputed, not fixed at entry" $
        -- THE FALSIFIER for evaluating a printed * once, at the seed or at entry:
        -- nothing touches the Goyf, and its P/T moves because a graveyard did.
        -- Empty graveyards -> 0 card types -> 0/1. Fog resolves and is put into
        -- its owner's graveyard (CR 608.2n), adding the Instant type.
        --
        -- Fog, NOT Lightning Bolt: Bolt targets, S.identityAnswer would aim it at
        -- the only creature on the board, and 3 damage would kill the 0/1 Goyf
        -- being measured. Fog has no target and no effect outside combat.
        let base = S.landsInPlay (Cards.forestPrinting cards) 1
            (goyfId, board) = S.addCreature (Cards.tarmogoyfPrinting cards) S.alice base
            (gs, fogId) = S.handOne (Cards.fogPrinting cards) board
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice fogId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "before: no card types in any graveyard, so 0 power" (Just 0) (Projection.powerOf goyfId board)
              HU.assertEqual "before: 0+1 toughness" (Just 1) (Projection.toughnessOf goyfId board)
              HU.assertEqual "after: one card type (Instant), so 1 power" (Just 1) (Projection.powerOf goyfId after)
              HU.assertEqual "after: 1+1 toughness" (Just 2) (Projection.toughnessOf goyfId after),
      HU.testCase "CR 208.2a 2007-10-01 the CDA works in all zones, and a Goyf in a graveyard counts itself" $
        -- Gatherer ruling on Tarmogoyf (WotC, 2007-10-01): "The ability that
        -- defines Tarmogoyf's power and toughness works in all zones, not just
        -- the battlefield. If Tarmogoyf is in your graveyard, it will count
        -- itself." CR 604.3 says a CDA functions in all zones, and CR 208.2a
        -- repeats it for P/T. This is the assertion that a gather-based
        -- implementation cannot make: gather only walks the battlefield.
        let gs0 = Setup.emptyGame S.bothPlayers
            (goyfId, gs) = S.addGraveyardCard (Cards.tarmogoyfPrinting cards) S.alice gs0
         in do
              HU.assertEqual "the Goyf in the graveyard is a creature card, so power 1" (Just 1) (Projection.powerOf goyfId gs)
              HU.assertEqual "1+1 toughness" (Just 2) (Projection.toughnessOf goyfId gs),
      HU.testCase "CR 613.4a/613.4c layer 7a runs before 7c, so a counter adds to the CDA" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, withCard) = S.addGraveyardCard (Cards.lightningBoltPrinting cards) S.alice gs0
            (goyfId, board) = S.addCreature (Cards.tarmogoyfPrinting cards) S.alice withCard
            gs = S.addCounter CounterKind.PlusOnePlusOne 1 goyfId board
         in do
              HU.assertEqual "1 card type + 1 counter" (Just 2) (Projection.powerOf goyfId gs)
              HU.assertEqual "1+1 toughness + 1 counter" (Just 3) (Projection.toughnessOf goyfId gs),
      HU.testCase "CR 604.3 Humility removes the CDA, and a Humility'd Tarmogoyf is 1/1" $
        -- NON-DISTINGUISHING BY CONSTRUCTION, and deliberately kept anyway.
        -- Humility is layer 6 (LoseAllAbilities) AND layer 7b (base P/T 1/1), and
        -- 7b overwrites 7a either way -- so this test passes whether or not
        -- LoseAllAbilities clears characteristicPT. It is here because "a
        -- Humility'd Tarmogoyf is 1/1" is a real ruling worth pinning, not because
        -- it proves the clearing.
        --
        -- What WOULD distinguish: a "loses all abilities" card that does not also
        -- set P/T. The Aura family (Darksteel Mutation and kin) is blocked on
        -- Attach; Soul Sculptor needs layer-4 card-type REPLACEMENT; Dress Down
        -- needs Flash, a beginning-of-end-step trigger (P4) and Sacrifice. See the
        -- P3b spec, section 8.
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, withBolt) = S.addGraveyardCard (Cards.lightningBoltPrinting cards) S.alice gs0
            (goyfId, board) = S.addCreature (Cards.tarmogoyfPrinting cards) S.alice withBolt
            gs = S.withHumility cards board
         in do
              HU.assertEqual "1 power" (Just 1) (Projection.powerOf goyfId gs)
              HU.assertEqual "1 toughness" (Just 1) (Projection.toughnessOf goyfId gs),
      HU.testCase "CR 604.3 LoseAllAbilities clears the CDA from the projected characteristics" $
        -- The clearing itself, asserted directly on the projection rather than
        -- through P/T -- the only channel through which it IS observable today.
        let gs0 = Setup.emptyGame S.bothPlayers
            (goyfId, board) = S.addCreature (Cards.tarmogoyfPrinting cards) S.alice gs0
            gs = S.withHumility cards board
         in HU.assertEqual "no CDA survives layer 6" Nothing (PC.characteristicPT (Projection.project goyfId gs))
    ]
