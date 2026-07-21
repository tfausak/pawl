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
         in HU.assertEqual "no CDA survives layer 6" Nothing (PC.characteristicPT (Projection.project goyfId gs)),
      HU.testCase "CR 608.2h a resolved pump is FROZEN and does not shrink with the hand" $
        -- THE FALSIFIER for re-evaluating a stored quantity: CR 608.2h says the
        -- answer is determined only once, when the effect is applied. Alice
        -- resolves the pump with two cards left in hand (+2/+2), then casts one of
        -- them -- her hand is now one card, and the pump must NOT follow it down.
        let base = S.landsInPlay (Cards.forestPrinting cards) 4
            (pikerId, board) = S.addPiker cards S.alice base
            -- handOne FIRST (it replaces the hand and sets up the phase), then
            -- addHandCard for the extras.
            (h1, icId) = S.handOne (Cards.innerCalmPrinting cards) board
            (ggId, h2) = S.addHandCard (Cards.giantGrowthPrinting cards) S.alice h1
            (_, gs) = S.addHandCard (Cards.forestPrinting cards) S.alice h2
            -- Casting Inner Calm moves it from hand to the stack, leaving two.
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice icId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
            -- Now the hand shrinks. Giant Growth is only CAST, not resolved, so it
            -- contributes no pump of its own -- the only thing that changed is the
            -- number Inner Calm counted.
            shrunk = snd (Engine.runGamePure S.identityAnswer after (Cast.castSpell S.alice ggId))
         in do
              HU.assertEqual "two cards left in hand at resolution" 2 (S.handSize S.alice after)
              HU.assertEqual "the 2/1 Piker is pumped to 4" (Just 4) (Projection.powerOf pikerId after)
              HU.assertEqual "and to 3 toughness" (Just 3) (Projection.toughnessOf pikerId after)
              HU.assertEqual "the hand is down to one card" 1 (S.handSize S.alice shrunk)
              HU.assertEqual "THE FREEZE: still +2, not +1" (Just 4) (Projection.powerOf pikerId shrunk)
              HU.assertEqual "and still +2 toughness" (Just 3) (Projection.toughnessOf pikerId shrunk),
      HU.testCase "CR 611.2 the freeze does NOT reach a static ability's continuous effect" $
        -- Opalescence's SetBasePowerToughness carries ManaValue, and CR 611.2 scopes
        -- the freeze to effects created by a spell's RESOLUTION. A static ability's
        -- effect is regenerated from the permanent every projection and evaluated
        -- per AFFECTED object. Were ManaValue frozen against the wrong object, this
        -- would evaluate against Opalescence itself (mana value 4, from {2}{W}{W}),
        -- not Bad Moon's own (mana value 2, from {1}{B}).
        --
        -- The expected 3/3, not the naively-expected 2/2: Opalescence's layer 7b
        -- sets Bad Moon's base to 2/2 (its own mana value), but layer 4 also makes
        -- Bad Moon itself a black creature -- and Bad Moon's own oracle text,
        -- "Black creatures get +1/+1", carries no "other" exclusion (unlike a lord
        -- effect), so its layer 7c ModifyPowerToughness applies to ITSELF too.
        -- Verified directly (not from recall): docs/rules.txt has no CR 613 clause
        -- excluding a source from its own static ability. 3/3 is still nowhere
        -- near the 5/5 a ManaValue-against-Opalescence leak would produce (4/4
        -- base + Bad Moon's own +1/+1), so the falsifier still distinguishes.
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, withOpal) = S.addCreature (Cards.opalescencePrinting cards) S.alice gs0
            (moonId, gs) = S.addCreature (Cards.badMoonPrinting cards) S.alice withOpal
         in do
              HU.assertEqual "Bad Moon's own mana value (2) plus its own +1/+1, not Opalescence's mana value (4)" (Just 3) (Projection.powerOf moonId gs)
              HU.assertEqual "and its toughness is 3" (Just 3) (Projection.toughnessOf moonId gs),
      HU.testCase "CR 608.2h the count is the CASTER's hand, not the target's controller's" $
        -- The second half of the same bug: applyModification used to evaluate a
        -- stored quantity against the AFFECTED object, so a player-scoped count
        -- would read the wrong player. Alice holds two cards after casting; bob
        -- holds none, and it is bob's creature being pumped.
        let base = S.landsInPlay (Cards.forestPrinting cards) 4
            (bobsPiker, board) = S.addPiker cards S.bob base
            (h1, icId) = S.handOne (Cards.innerCalmPrinting cards) board
            (_, h2) = S.addHandCard (Cards.giantGrowthPrinting cards) S.alice h1
            (_, gs) = S.addHandCard (Cards.forestPrinting cards) S.alice h2
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice icId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "bob holds nothing" 0 (S.handSize S.bob after)
              HU.assertEqual "the pump is alice's two, not bob's zero" (Just 4) (Projection.powerOf bobsPiker after)
    ]
