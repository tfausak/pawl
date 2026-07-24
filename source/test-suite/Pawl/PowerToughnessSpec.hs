-- Covers: Pawl.Projection (CR 613 layer 7 -- 7a characteristic-defined P/T, the
-- CR 608.2h freeze that 7b's stored effects owe, and 7d P/T switching), Pawl.Quantity
-- (the counting quantity) and the P3b gates (Tarmogoyf, Inner Calm Outer Strength,
-- Twisted Image). Gameplay-level: each card is cast or resolved through the stack and
-- the resulting game state is asserted on.
module Pawl.PowerToughnessSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Cards as Cards
import qualified Pawl.Cast as Cast
import qualified Pawl.Engine as Engine
import qualified Pawl.Projection as Projection
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.Aggregation as Aggregation
import qualified Pawl.Type.Count as Count.Type
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.Filter as Filter.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.PlayerRef as PlayerRef
import qualified Pawl.Type.ProjectedCharacteristics as PC
import qualified Pawl.Type.Quantity as Quantity.Type
import qualified Pawl.Type.Scope as Scope
import qualified Pawl.Type.Zone as Zone
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
            -- CR 208.2a: Tarmogoyf's shape -- distinct card types over every
            -- graveyard.
            count =
              Quantity.Type.Count
                ( Count.Type.MkCount
                    (Scope.InZone Zone.Graveyard PlayerRef.EachPlayer)
                    (Filter.Type.And [])
                    Aggregation.DistinctCardTypes
                )
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
              HU.assertEqual "the pump is alice's two, not bob's zero" (Just 4) (Projection.powerOf bobsPiker after),
      HU.testCase "CR 613.4d the switch takes the value AFTER layers 7a-7c" $
        -- THE ORDERING FALSIFIER, and the reason Tarmogoyf and Twisted Image are in
        -- the same phase: a symmetric fixture (a +1/+1 counter, Giant Growth)
        -- COMMUTES with the switch and proves nothing. Tarmogoyf's N/N+1 is the
        -- pool's only asymmetric P/T, so it is the only thing that can witness the
        -- order. Two card types in graveyards -> a 2/3 -> switched, a 3/2.
        -- Switching before 7a would switch Nothing/Nothing and the CDA would then
        -- write 2/3 straight back over it.
        let base = S.landsInPlay (Cards.islandPrinting cards) 1
            (_, g1) = S.addGraveyardCard (Cards.lightningBoltPrinting cards) S.alice base
            (_, g2) = S.addGraveyardCard (Cards.pikerPrinting cards) S.alice g1
            (goyfId, board) = S.addCreature (Cards.tarmogoyfPrinting cards) S.alice g2
            (gs, tiId) = S.handOne (Cards.twistedImagePrinting cards) board
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice tiId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "before: a 2/3" (Just 2) (Projection.powerOf goyfId board)
              HU.assertEqual "before: toughness 3" (Just 3) (Projection.toughnessOf goyfId board)
              HU.assertEqual "after: power is the old toughness" (Just 3) (Projection.powerOf goyfId after)
              HU.assertEqual "after: toughness is the old power" (Just 2) (Projection.toughnessOf goyfId after),
      HU.testCase "CR 613.4d a switched CDA still tracks the graveyards" $
        -- After the switch, a THIRD distinct card type lands in a graveyard, and
        -- the Goyf's CDA (still recomputed live every projection, CR 604.3) must
        -- pick it up underneath the still-active switch. Divination (Sorcery), not
        -- Giant Growth: Giant Growth is an Instant, the same type Lightning Bolt
        -- already contributed, so it would add no NEW type and this fixture would
        -- silently test nothing beyond the previous case.
        let base = S.landsInPlay (Cards.islandPrinting cards) 1
            (_, g1) = S.addGraveyardCard (Cards.lightningBoltPrinting cards) S.alice base
            (_, g2) = S.addGraveyardCard (Cards.pikerPrinting cards) S.alice g1
            (goyfId, board) = S.addCreature (Cards.tarmogoyfPrinting cards) S.alice g2
            (gs, tiId) = S.handOne (Cards.twistedImagePrinting cards) board
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice tiId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
            (_, later) = S.addGraveyardCard (Cards.divinationPrinting cards) S.bob after
         in do
              HU.assertEqual "a third card type: 3/4 switched is 4/3, power" (Just 4) (Projection.powerOf goyfId later)
              HU.assertEqual "and toughness" (Just 3) (Projection.toughnessOf goyfId later),
      HU.testCase "CR 613.4d 2021-03-19 the switch applies last regardless of WHEN it began" $
        -- Gatherer ruling on Twisted Image (WotC, 2021-03-19): "Effects that switch
        -- a creature's power and toughness apply after all other effects,
        -- REGARDLESS OF WHEN THOSE EFFECTS BEGAN TO APPLY. For instance, if you
        -- target a 1/2 creature then give it +2/+0 later in the turn, it's a 2/3
        -- creature, not a 4/1 creature."
        --
        -- The switch is installed FIRST (earlier timestamp) and the pump SECOND, so
        -- a timestamp-ordered implementation would switch then pump. Layer order
        -- (CR 613.4c before 613.4d) must beat timestamp order. The pump is +2/+0 --
        -- ASYMMETRIC, per the ruling's own example, because a symmetric one cannot
        -- tell the two orders apart.
        --
        -- Goblin Piker is 2/1. Correct: 7c gives 4/1, 7d switches to 1/4.
        -- Timestamp-ordered: switch gives 1/2, then the pump gives 3/2.
        let gs0 = Setup.emptyGame S.bothPlayers
            (pikerId, board) = S.addPiker cards S.alice gs0
            switched = S.withEffect pikerId Modification.SwitchPowerToughness board
            gs = S.withEffect pikerId (Modification.ModifyPowerToughness (Quantity.Type.Literal 2) (Quantity.Type.Literal 0)) switched
         in do
              HU.assertEqual "power is the pumped toughness" (Just 1) (Projection.powerOf pikerId gs)
              HU.assertEqual "toughness is the pumped power" (Just 4) (Projection.toughnessOf pikerId gs),
      HU.testCase "CR 613.4d 2021-03-19 two switches return the object to normal" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (pikerId, board) = S.addPiker cards S.alice gs0
            once = S.withEffect pikerId Modification.SwitchPowerToughness board
            twice = S.withEffect pikerId Modification.SwitchPowerToughness once
         in do
              HU.assertEqual "once: the 2/1 is a 1/2" (Just 1) (Projection.powerOf pikerId once)
              HU.assertEqual "twice: back to 2" (Just 2) (Projection.powerOf pikerId twice)
              HU.assertEqual "twice: back to 1 toughness" (Just 1) (Projection.toughnessOf pikerId twice),
      HU.testCase "CR 704.5g 2021-03-19 nonlethal damage becomes lethal after a switch" $
        -- Gatherer ruling on Twisted Image (WotC, 2021-03-19): "Because damage
        -- remains marked on a creature until the damage is removed as the turn
        -- ends, nonlethal damage dealt to a creature may become lethal if you
        -- switch its power and toughness during that turn." Damage marking
        -- (CR 514.2) and the CR 704.5g lethal-damage state-based action have both
        -- existed since M1b; this is the ruling as a scenario.
        --
        -- A 2/3 Tarmogoyf with 2 damage marked survives. Switched to 3/2, the same
        -- 2 damage is lethal.
        let base = S.landsInPlay (Cards.islandPrinting cards) 1
            (_, g1) = S.addGraveyardCard (Cards.lightningBoltPrinting cards) S.alice base
            (_, g2) = S.addGraveyardCard (Cards.pikerPrinting cards) S.alice g1
            (goyfId, g3) = S.addCreature (Cards.tarmogoyfPrinting cards) S.alice g2
            -- Twisted Image draws a card, and this is the one test here that runs
            -- settleForPriority (it needs the SBA sweep). Setup.emptyGame leaves
            -- libraries EMPTY, so without this alice would lose to CR 704.5b
            -- mid-assertion rather than the Goyf dying to CR 704.5g.
            (_, g4) = S.addLibraryCard (Cards.forestPrinting cards) S.alice g3
            board = S.markDamage goyfId 2 g4
            (gs, tiId) = S.handOne (Cards.twistedImagePrinting cards) board
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice tiId))
            after = snd (Engine.runGamePure S.identityAnswer cast (Stack.resolveTop >> Engine.settleForPriority))
         in do
              HU.assertBool "the 2/3 with 2 damage was alive" (Set.member goyfId (GameState.battlefield board))
              HU.assertBool "the switched 3/2 with 2 damage is dead" (not (Set.member goyfId (GameState.battlefield after))),
      HU.testCase "CR 208.2a Nightmare counts the Swamps you control" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, gs1) = S.addCreature (Cards.swampPrinting cards) S.alice gs0
            (_, gs2) = S.addCreature (Cards.swampPrinting cards) S.alice gs1
            (_, gs3) = S.addCreature (Cards.swampPrinting cards) S.bob gs2
            (nightId, gs) = S.addCreature (Cards.nightmarePrinting cards) S.alice gs3
         in do
              HU.assertEqual "power" (Just 2) (Projection.powerOf nightId gs)
              HU.assertEqual "toughness" (Just 2) (Projection.toughnessOf nightId gs),
      HU.testCase "CR 613.1d Urborg makes every land a Swamp, and Nightmare counts them" $
        -- THE FALSIFIER for a printed-type count. Nothing touches the Nightmare;
        -- its power moves because a layer-4 type-changer entered.
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, gs1) = S.addCreature (Cards.mountainPrinting cards) S.alice gs0
            (_, gs2) = S.addCreature (Cards.forestPrinting cards) S.alice gs1
            (nightId, gs3) = S.addCreature (Cards.nightmarePrinting cards) S.alice gs2
            (_, gs) = S.addCreature (Cards.urborgPrinting cards) S.alice gs3
         in HU.assertEqual "both lands are Swamps, plus Urborg itself (IncludesSource): 3" (Just 3) (Projection.powerOf nightId gs),
      HU.testCase "CR 613.5 a Swamp entering or leaving moves the P/T on the next projection" $
        -- CR 613.5: continuous application is automatic -- nothing touches
        -- Nightmare when a Swamp enters or leaves, only the next projection's
        -- read of the battlefield.
        let gs0 = Setup.emptyGame S.bothPlayers
            (nightId, g1) = S.addCreature (Cards.nightmarePrinting cards) S.alice gs0
            (_, g2) = S.addCreature (Cards.swampPrinting cards) S.alice g1
            (swamp2, g3) = S.addCreature (Cards.swampPrinting cards) S.alice g2
            g4 =
              g3
                { GameState.objects = Map.delete swamp2 (GameState.objects g3),
                  GameState.battlefield = Set.delete swamp2 (GameState.battlefield g3)
                }
         in do
              HU.assertEqual "one Swamp: power 1" (Just 1) (Projection.powerOf nightId g2)
              HU.assertEqual "a second Swamp enters: power 2" (Just 2) (Projection.powerOf nightId g3)
              HU.assertEqual "it leaves again: power back to 1" (Just 1) (Projection.powerOf nightId g4),
      HU.testCase "CR 613.1d/305.7 Blood Moon strips Urborg's own Swamp-granting ability" $
        -- Urborg enters first (earlier timestamp) and swamps itself; Blood Moon
        -- enters second and SETS its subtype to Mountain. CR 305.7: a land whose
        -- subtype is SET to a basic type loses every ability generated by its
        -- rules text -- including Urborg's OWN "every land is a Swamp" ability.
        -- Pawl.Projection.staticAbilitiesLive/liveGiven excludes that ability
        -- from `gather` entirely (reading BASE characteristics, not a timestamp
        -- race), so Urborg ends up a bare Mountain with nothing left for
        -- Nightmare to count.
        let gs0 = Setup.emptyGame S.bothPlayers
            (nightId, g1) = S.addCreature (Cards.nightmarePrinting cards) S.alice gs0
            (_, g2) = S.addCreature (Cards.urborgPrinting cards) S.alice g1
            (_, gs) = S.addCreature (Cards.bloodMoonPrinting cards) S.alice g2
         in HU.assertEqual "Urborg is a bare Mountain now, no Swamps to count" (Just 0) (Projection.powerOf nightId gs),
      HU.testCase "CR 613.7/305.7 the outcome does not flip with entry order (#11)" $
        -- THE FALSIFIER for a naive per-layer TIMESTAMP-ONLY implementation: the
        -- previous test's order reversed -- Blood Moon enters FIRST, Urborg
        -- SECOND. A plain CR 613.7 timestamp fold would apply Urborg's LATER
        -- AddLandSubtype Swamp after Blood Moon's SetLandSubtype Mountain and
        -- get a Swamp back (Mountain AND Swamp). Pawl instead gates Urborg's own
        -- ability on CR 305.7 liveness read against BASE characteristics
        -- (Pawl.Projection.staticAbilitiesLive), which does not depend on
        -- timestamps at all, so the result matches the previous test regardless
        -- of order. General same-layer applies-to dependency reordering (CR
        -- 613.8b) is otherwise unimplemented (#11); this specific pair does not
        -- exercise that gap -- it is carried by the dedicated CR 305.7 liveness
        -- mechanism instead.
        let gs0 = Setup.emptyGame S.bothPlayers
            (nightId, g1) = S.addCreature (Cards.nightmarePrinting cards) S.alice gs0
            (_, g2) = S.addCreature (Cards.bloodMoonPrinting cards) S.alice g1
            (_, gs) = S.addCreature (Cards.urborgPrinting cards) S.alice g2
         in HU.assertEqual "still 0: order does not matter" (Just 0) (Projection.powerOf nightId gs),
      HU.testCase "CR 604.3 Humility erases the CDA, and a Humility'd Nightmare is 1/1" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, g1) = S.addCreature (Cards.swampPrinting cards) S.alice gs0
            (_, g2) = S.addCreature (Cards.swampPrinting cards) S.alice g1
            (nightId, g3) = S.addCreature (Cards.nightmarePrinting cards) S.alice g2
            gs = S.withHumility cards g3
         in do
              HU.assertEqual "no CDA survives layer 6" Nothing (PC.characteristicPT (Projection.project nightId gs))
              HU.assertEqual "1 power" (Just 1) (Projection.powerOf nightId gs)
              HU.assertEqual "1 toughness" (Just 1) (Projection.toughnessOf nightId gs)
    ]
