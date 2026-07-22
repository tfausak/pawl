-- Covers Pawl.PlayerEffect and Pawl.Cost, plus the four types they case on
-- (Pawl.Type.PlayerEffect, PlayerScope, SpellCriterion, PlayerStaticAbility) and
-- the stored carrier Pawl.Type.ActivePlayerEffect. CR 613.10/613.11: the
-- continuous effects that affect PLAYERS and the RULES OF THE GAME, which sit
-- outside the CR 613 layer system entirely.
--
-- The five gate cards: Rule of Law, Thalia Guardian of Thraben, Sapphire
-- Medallion, Reliquary Tower and Silence.
module Pawl.PlayerEffectSpec where

import qualified Pawl.Action as Action
import qualified Pawl.Cards as Cards
import qualified Pawl.Cast as Cast
import qualified Pawl.Cost as Cost
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.PlayerEffect as PlayerEffect
import qualified Pawl.Support as S
import qualified Pawl.Type.Action as Action.Type
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.GameEvent as GameEvent
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Rule of Law {2}{W} Enchantment: "Each player can't cast more than one spell
-- each turn." alice has nine untapped Plains (mana is never the reason a cast is
-- unavailable) and three Rule of Law cards in hand, in her own precombat main
-- phase with an empty stack.
ruleOfLawBoard :: Cards.Cards -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
ruleOfLawBoard cards =
  let base = S.landsInPlay (Cards.plainsPrinting cards) 9
      (a, gs1) = S.addHandCard (Cards.ruleOfLawPrinting cards) S.alice base
      (b, gs2) = S.addHandCard (Cards.ruleOfLawPrinting cards) S.alice gs1
      (c, gs3) = S.addHandCard (Cards.ruleOfLawPrinting cards) S.alice gs2
   in ( a,
        b,
        c,
        gs3
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

ruleOfLawTests :: Cards.Cards -> Tasty.TestTree
ruleOfLawTests cards =
  let resolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop
      (a, b, _, board) = ruleOfLawBoard cards
      -- Cast Rule of Law itself, and let it resolve onto the battlefield.
      afterFirst = resolveAll (S.runPure S.identityAnswer board (Cast.castSpell S.alice a))
   in Tasty.testGroup
        "RuleOfLaw"
        [ HU.testCase "before any spell is cast, both cards are castable" $
            do
              HU.assertBool "not prohibited" (not (PlayerEffect.prohibitsCasting S.alice board))
              HU.assertBool "a offered" (elem (Action.Type.Cast a) (Action.legalActions S.alice board))
              HU.assertBool "b offered" (elem (Action.Type.Cast b) (Action.legalActions S.alice board)),
          -- Ruling: "Rule of Law looks at the entire turn to see if a player has
          -- cast a spell, even if Rule of Law wasn't on the battlefield when that
          -- spell was cast. Notably, you can't cast Rule of Law and then cast
          -- another spell during the same turn." THE FALSIFIER: the spell that
          -- used up the allowance is Rule of Law itself, cast BEFORE the effect
          -- existed. Any per-effect watermark or counter fails here.
          HU.testCase "CR 601.3 casting Rule of Law itself uses up the turn's one spell" $
            do
              HU.assertBool "alice is now prohibited" (PlayerEffect.prohibitsCasting S.alice afterFirst)
              HU.assertEqual
                "no cast is offered at all"
                []
                (filter isCast (Action.legalActions S.alice afterFirst)),
          -- The limit is counted PER PLAYER: bob has cast nothing this turn, so
          -- EachPlayer does not prohibit him.
          HU.testCase "CR 109.5 the EachPlayer scope still counts each player's own casts" $
            HU.assertBool "bob is not prohibited" (not (PlayerEffect.prohibitsCasting S.bob afterFirst)),
          -- CR 608.2i: the log is cleared at turn handoff, so "this turn" is
          -- exactly the log's own extent.
          HU.testCase "CR 608.2i the restriction lifts on the next turn" $
            let handoff gs = S.runPure S.identityAnswer gs Engine.handoffTurn
                nextOwnTurn =
                  (handoff (handoff afterFirst))
                    { GameState.phase = Phase.PrecombatMain,
                      GameState.priority = Just S.alice
                    }
             in do
                  HU.assertEqual "alice is active again" S.alice (GameState.activePlayer nextOwnTurn)
                  HU.assertBool "not prohibited" (not (PlayerEffect.prohibitsCasting S.alice nextOwnTurn))
                  HU.assertBool "b offered again" (elem (Action.Type.Cast b) (Action.legalActions S.alice nextOwnTurn)),
          -- Ruling: "If you cast a spell that was countered, you can't cast
          -- another spell during the same turn." The counted event is the CAST.
          HU.testCase "CR 601.2i a countered spell still counted" $
            let (_, x, _, plain) = ruleOfLawBoard cards
                onBoard = snd (S.addCreature (Cards.ruleOfLawPrinting cards) S.alice plain)
                cast = S.runPure S.identityAnswer onBoard (Cast.castSpell S.alice x)
             in case GameState.stack cast of
                  [] -> HU.assertFailure "expected the spell on the stack"
                  top : _ ->
                    let countered = S.runPure S.identityAnswer cast (Event.counter top)
                     in do
                          HU.assertEqual "the stack is empty again" [] (GameState.stack countered)
                          HU.assertBool "still prohibited" (PlayerEffect.prohibitsCasting S.alice countered),
          -- The effect is RE-DERIVED from the battlefield on every read, so there
          -- is no stored state to unwind when its source leaves.
          HU.testCase "CR 604.2 destroying Rule of Law lifts the restriction in the same turn" $
            let (_, _, z, plain) = ruleOfLawBoard cards
                (rol, onBoard) = S.addCreature (Cards.ruleOfLawPrinting cards) S.alice plain
                castOne = S.withEvent (GameEvent.SpellCast S.alice) onBoard
                gone = S.runPure S.identityAnswer castOne (Event.destroy rol)
             in do
                  HU.assertBool "prohibited while it stands" (PlayerEffect.prohibitsCasting S.alice castOne)
                  HU.assertBool "not prohibited once it is gone" (not (PlayerEffect.prohibitsCasting S.alice gone))
                  HU.assertBool "and a cast is offered again" (elem (Action.Type.Cast z) (Action.legalActions S.alice gone)),
          -- CR 601.3's prohibit half applies to EVERY cast, including a
          -- Panglacial Wurm cast from the library: the Panglacial permission
          -- (Cast.permitsCastWhileSearching) excepts only the timing half, not
          -- the prohibition half. Seven Forests pay the Wurm's {5}{G}{G}.
          HU.testCase "CR 601.3 Rule of Law also prohibits casting Panglacial Wurm from the library" $
            let base = S.landsInPlay (Cards.forestPrinting cards) 7
                (_, withRuleOfLaw) = S.addCreature (Cards.ruleOfLawPrinting cards) S.alice base
                (_, gs) = S.addLibraryCard (Cards.panglacialWurmPrinting cards) S.alice withRuleOfLaw
                castOne = S.withEvent (GameEvent.SpellCast S.alice) gs
             in do
                  -- Positive control: without it, the negative assertion below
                  -- could pass merely because the Wurm was never offered at all.
                  HU.assertEqual
                    "before any cast, the Wurm is offered from the library"
                    1
                    (length (Cast.castableWhileSearching S.alice gs))
                  HU.assertEqual
                    "Rule of Law's one-spell limit blocks the library cast too"
                    []
                    (Cast.castableWhileSearching S.alice castOne)
        ]

isCast :: Action.Type.Action -> Bool
isCast action = case action of
  Action.Type.Cast _ -> True
  Action.Type.Play _ -> False
  Action.Type.Activate _ _ -> False
  Action.Type.Pass -> False

-- The CR 601.2f arithmetic, with no board at all. The unit half of the cost
-- axis; the gate cards below are the gameplay half.
red :: ManaSymbol.ManaSymbol
red = ManaSymbol.OfType (ManaType.Colored Color.Red)

blue :: ManaSymbol.ManaSymbol
blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)

adjustmentTests :: Tasty.TestTree
adjustmentTests =
  Tasty.testGroup
    "Adjustments"
    [ HU.testCase "no adjustments is the identity on a printed cost" $
        HU.assertEqual
          "unchanged"
          (ManaCost.MkManaCost [ManaSymbol.Generic 1, red])
          (Cost.applyAdjustments ([], []) (ManaCost.MkManaCost [ManaSymbol.Generic 1, red])),
      HU.testCase "CR 601.2f an increase adds generic mana" $
        HU.assertEqual
          "{R} taxed by {1} is {1}{R}"
          (ManaCost.MkManaCost [ManaSymbol.Generic 1, red])
          (Cost.applyAdjustments ([1], []) (ManaCost.MkManaCost [red])),
      -- CR 118.7a: "Effects that reduce a cost by an amount of generic mana
      -- affect only the generic mana component of that cost. They can't affect
      -- the colored or colorless mana components."
      HU.testCase "CR 118.7a a reduction with no generic component to take is lost" $
        HU.assertEqual
          "{U} reduced by {1} is still {U}"
          (ManaCost.MkManaCost [blue])
          (Cost.applyAdjustments ([], [1]) (ManaCost.MkManaCost [blue])),
      HU.testCase "CR 118.7a a reduction takes only the generic component" $
        HU.assertEqual
          "{2}{U} reduced by {1} is {1}{U}"
          (ManaCost.MkManaCost [ManaSymbol.Generic 1, blue])
          (Cost.applyAdjustments ([], [1]) (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue])),
      HU.testCase "CR 601.2f the total can't be reduced below {0}" $
        HU.assertEqual
          "{1} reduced by {3} is {0}"
          (ManaCost.MkManaCost [])
          (Cost.applyAdjustments ([], [3]) (ManaCost.MkManaCost [ManaSymbol.Generic 1])),
      -- THE ORDER TEST, in the small. Increase first gives {1}{U}, which the
      -- reduction takes back to {U}. Reduce first loses the reduction to CR
      -- 118.7a's empty generic component, and the increase then leaves {1}{U}.
      HU.testCase "CR 601.2f every increase applies before any reduction" $
        HU.assertEqual
          "{U} +{1} -{1} is {U}"
          (ManaCost.MkManaCost [blue])
          (Cost.applyAdjustments ([1], [1]) (ManaCost.MkManaCost [blue]))
    ]

-- Thalia, Guardian of Thraben {1}{W} Legendary Creature -- Human Soldier 2/1:
-- "First strike / Noncreature spells cost {1} more to cast."
thaliaTests :: Cards.Cards -> Tasty.TestTree
thaliaTests cards =
  let -- alice controls Thalia and `n` untapped Mountains; her hand holds one
      -- Lightning Bolt ({R} instant -- noncreature) and one Goblin Piker
      -- ({1}{R} creature).
      board n =
        let base = S.landsInPlay (Cards.mountainPrinting cards) n
            (_, gs1) = S.addCreature (Cards.thaliaPrinting cards) S.alice base
            (bolt, gs2) = S.addHandCard (Cards.lightningBoltPrinting cards) S.alice gs1
            (piker, gs3) = S.addHandCard (Cards.pikerPrinting cards) S.alice gs2
         in ( bolt,
              piker,
              gs3
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            )
      untaxed n =
        let base = S.landsInPlay (Cards.mountainPrinting cards) n
            (bolt, gs1) = S.addHandCard (Cards.lightningBoltPrinting cards) S.alice base
         in (bolt, gs1 {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice})
   in Tasty.testGroup
        "Thalia"
        [ HU.testCase "CR 601.2f a noncreature spell's total cost is one more" $
            let (bolt, _, gs) = board 3
             in HU.assertEqual
                  "{R} becomes {1}{R}"
                  (ManaCost.MkManaCost [ManaSymbol.Generic 1, red])
                  (Cost.total S.alice bolt (ManaCost.MkManaCost [red]) gs),
          -- Ruling: "Thalia's ability affects each spell that's not a creature
          -- spell, including your own." SpellCriterion reads the PROJECTION.
          HU.testCase "CR 613.11 a creature spell is unaffected" $
            let (_, piker, gs) = board 3
             in HU.assertEqual
                  "{1}{R} stays {1}{R}"
                  (ManaCost.MkManaCost [ManaSymbol.Generic 1, red])
                  (Cost.total S.alice piker (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]) gs),
          -- BOTH sites, one scenario. Taxing castability but not payment lets
          -- the player underpay; taxing payment but not castability offers a
          -- cast that cannot be afforded, and there is no mid-announcement
          -- rewind (#56) -- that is a wedged game, not a rejected action.
          HU.testCase "CR 601.2f castability is measured against the total cost" $
            let (boltOne, _, oneLand) = board 1
                (boltTwo, _, twoLands) = board 2
                (_, pikerTwo, twoLandsAgain) = board 2
             in do
                  HU.assertBool "one Mountain is not enough for a taxed Bolt" (not (Cast.castable S.alice boltOne oneLand))
                  HU.assertBool "two Mountains are" (Cast.castable S.alice boltTwo twoLands)
                  HU.assertBool "and an untaxed creature spell needs only its printed two" (Cast.castable S.alice pikerTwo twoLandsAgain),
          HU.testCase "CR 601.2f payment spends the total cost" $
            let (bolt, _, gs) = board 3
                paid = S.runPure S.identityAnswer gs (Cast.castSpell S.alice bolt)
                (boltU, gsU) = untaxed 3
                paidU = S.runPure S.identityAnswer gsU (Cast.castSpell S.alice boltU)
             in do
                  HU.assertEqual "taxed: two lands tapped" 2 (S.tappedCount S.alice paid)
                  HU.assertEqual "untaxed: one land tapped" 1 (S.tappedCount S.alice paidU),
          -- The EachPlayer scope: Thalia's controller is taxed (every assertion
          -- above is alice's own spell) and so is her opponent.
          HU.testCase "CR 611.1 the opponent is taxed too" $
            let (_, _, gs) = board 3
                (bobBolt, withBob) = S.addHandCard (Cards.lightningBoltPrinting cards) S.bob gs
             in HU.assertEqual
                  "bob's {R} is also {1}{R}"
                  (ManaCost.MkManaCost [ManaSymbol.Generic 1, red])
                  (Cost.total S.bob bobBolt (ManaCost.MkManaCost [red]) withBob)
        ]

-- Sapphire Medallion {2} Artifact: "Blue spells you cast cost {1} less to cast."
medallionTests :: Cards.Cards -> Tasty.TestTree
medallionTests cards =
  let -- alice controls a Sapphire Medallion and `n` untapped Islands; her hand
      -- holds Unsummon ({U} instant), Divination ({2}{U} sorcery) and
      -- Lightning Bolt ({R} instant).
      board n =
        let base = S.landsInPlay (Cards.islandPrinting cards) n
            (_, gs1) = S.addCreature (Cards.sapphireMedallionPrinting cards) S.alice base
            (unsummon, gs3) = S.addHandCard (Cards.unsummonPrinting cards) S.alice gs1
            (divination, gs4) = S.addHandCard (Cards.divinationPrinting cards) S.alice gs3
            (bolt, gs5) = S.addHandCard (Cards.lightningBoltPrinting cards) S.alice gs4
         in ( unsummon,
              divination,
              bolt,
              gs5
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            )
      -- Thalia AND the Medallion, and one Island. The CR 601.2f order test.
      bothBoard =
        let base = S.landsInPlay (Cards.islandPrinting cards) 1
            (_, gs1) = S.addCreature (Cards.sapphireMedallionPrinting cards) S.alice base
            (_, gs2) = S.addCreature (Cards.thaliaPrinting cards) S.alice gs1
            (unsummon, gs3) = S.addHandCard (Cards.unsummonPrinting cards) S.alice gs2
         in ( unsummon,
              gs3
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            )
   in Tasty.testGroup
        "SapphireMedallion"
        [ -- Ruling: "The ability can't reduce the amount of colored mana you pay
          -- for a spell. It reduces only the generic mana component of that
          -- cost." THE HEADLINE FALSIFIER: subtracting from the mana value would
          -- make this spell free.
          HU.testCase "CR 118.7a a {U} spell still costs {U}" $
            let (unsummon, _, _, gs) = board 2
             in HU.assertEqual
                  "unchanged"
                  (ManaCost.MkManaCost [blue])
                  (Cost.total S.alice unsummon (ManaCost.MkManaCost [blue]) gs),
          HU.testCase "CR 118.7a a {2}{U} spell costs {1}{U}" $
            let (_, divination, _, gs) = board 2
             in HU.assertEqual
                  "one generic off"
                  (ManaCost.MkManaCost [ManaSymbol.Generic 1, blue])
                  (Cost.total S.alice divination (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue]) gs),
          HU.testCase "a red spell fails the effect's colour criterion, so it is unaffected" $
            let (_, _, bolt, gs) = board 2
             in HU.assertEqual
                  "unchanged"
                  (ManaCost.MkManaCost [red])
                  (Cost.total S.alice bolt (ManaCost.MkManaCost [red]) gs),
          -- Divination is {2}{U}: three mana printed, two after the discount. Two
          -- Islands is exactly the amount that tells the two apart.
          HU.testCase "CR 601.2f the discount is observable at the castability gate" $
            let (_, divination, _, withMedallion) = board 2
                bareBoard =
                  let base = S.landsInPlay (Cards.islandPrinting cards) 2
                      (d, gs1) = S.addHandCard (Cards.divinationPrinting cards) S.alice base
                   in ( d,
                        gs1
                          { GameState.phase = Phase.PrecombatMain,
                            GameState.activePlayer = S.alice,
                            GameState.priority = Just S.alice
                          }
                      )
                (bareDivination, bare) = bareBoard
             in do
                  HU.assertBool "castable for {1}{U} with two Islands" (Cast.castable S.alice divination withMedallion)
                  HU.assertBool "and not castable for {2}{U} without the Medallion" (not (Cast.castable S.alice bareDivination bare)),
          -- CR 611.1 / 109.5: the You scope is the effect's controller.
          HU.testCase "CR 109.5 the You scope does not discount an opponent's spell" $
            let (_, _, _, gs) = board 2
                (bobDivination, withBob) = S.addHandCard (Cards.divinationPrinting cards) S.bob gs
             in HU.assertEqual
                  "bob pays full price"
                  (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue])
                  (Cost.total S.bob bobDivination (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue]) withBob),
          -- Ruling: "If there are additional costs to cast a spell, or if the
          -- cost to cast a spell is increased by an effect (such as the one
          -- created by Thalia, Guardian of Thraben's ability), apply those
          -- increases before applying cost reductions." THE ORDER TEST, and it
          -- names a cost with NO generic component on purpose: the two orders
          -- agree wherever the CR 601.2f floor does not bind.
          HU.testCase "CR 601.2f Thalia then the Medallion leaves a {U} spell at exactly {U}" $
            let (unsummon, gs) = bothBoard
             in do
                  HU.assertEqual
                    "increase first, then reduce"
                    (ManaCost.MkManaCost [blue])
                    (Cost.total S.alice unsummon (ManaCost.MkManaCost [blue]) gs)
                  HU.assertBool "so one Island is enough" (Cast.castable S.alice unsummon gs)
        ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Pawl.PlayerEffectSpec" [ruleOfLawTests cards, adjustmentTests, thaliaTests cards, medallionTests cards]
