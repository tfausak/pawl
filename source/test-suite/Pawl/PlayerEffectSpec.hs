-- Covers Pawl.PlayerEffect and Pawl.Cost, plus the types they case on
-- (Pawl.Types.PlayerEffect, PlayerScope, PlayerStaticAbility) and
-- the stored carrier Pawl.Types.ActivePlayerEffect. The spell match runs through
-- the identity-blind Pawl.Filter over a Pawl.Types.Filter. CR 613.10/613.11: the
-- continuous effects that affect PLAYERS and the RULES OF THE GAME, which sit
-- outside the CR 613 layer system entirely.
--
-- The six gate cards: Rule of Law, Thalia Guardian of Thraben, Sapphire
-- Medallion, Edgewalker, Reliquary Tower and Silence. Humility and Opalescence
-- join them for CR 604.2's "and has the ability" -- the one place this axis does
-- meet the CR 613 layer system.
module Pawl.PlayerEffectSpec where

import qualified Data.List as List
import Numeric.Natural (Natural)
import qualified Pawl.Action as Action
import qualified Pawl.Cast as Cast
import qualified Pawl.Cost as Cost
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Expiry as Expiry
import qualified Pawl.Game as Game
import qualified Pawl.PlayerEffect as PlayerEffect
import qualified Pawl.Registry as Registry
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator Pawl.Filter already claims the alias Filter.

import qualified Pawl.Registry as Registry.Type
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Expiry as Expiry.Type
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerEffect as PlayerEffect.Type
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Rule of Law {2}{W} Enchantment: "Each player can't cast more than one spell
-- each turn." alice has nine untapped Plains (mana is never the reason a cast is
-- unavailable) and three Rule of Law cards in hand, in her own precombat main
-- phase with an empty stack. Loaded fresh inside each case that needs it --
-- equivalent because loading is deterministic and cached (batch-recipe.md).
ruleOfLawBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
ruleOfLawBoard plains ruleOfLaw =
  let base = S.landsInPlay plains 9
      (a, gs1) = S.addHandCard ruleOfLaw S.alice base
      (b, gs2) = S.addHandCard ruleOfLaw S.alice gs1
      (c, gs3) = S.addHandCard ruleOfLaw S.alice gs2
   in ( a,
        b,
        c,
        gs3
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Cast Rule of Law itself, and let it resolve onto the battlefield.
ruleOfLawAfterFirst :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
ruleOfLawAfterFirst plains ruleOfLaw =
  let resolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop
      (a, b, _, board) = ruleOfLawBoard plains ruleOfLaw
   in (a, b, resolveAll (S.runPure S.identityAnswer board (Cast.castSpell S.alice a)))

ruleOfLawTests :: Registry.Type.Registry -> Tasty.TestTree
ruleOfLawTests registry =
  Tasty.testGroup
    "RuleOfLaw"
    [ HU.testCase "before any spell is cast, both cards are castable" $ do
        plains <- Registry.printing registry "Plains"
        ruleOfLaw <- Registry.printing registry "Rule of Law"
        let (a, b, _, board) = ruleOfLawBoard plains ruleOfLaw
        HU.assertBool "not prohibited" (not (PlayerEffect.prohibitsCasting S.alice board))
        HU.assertBool "a offered" (elem (Action.Type.Cast a) (Action.legalActions S.alice board))
        HU.assertBool "b offered" (elem (Action.Type.Cast b) (Action.legalActions S.alice board)),
      -- Ruling: "Rule of Law looks at the entire turn to see if a player has
      -- cast a spell, even if Rule of Law wasn't on the battlefield when that
      -- spell was cast. Notably, you can't cast Rule of Law and then cast
      -- another spell during the same turn." THE FALSIFIER: the spell that
      -- used up the allowance is Rule of Law itself, cast BEFORE the effect
      -- existed. Any per-effect watermark or counter fails here.
      HU.testCase "CR 601.3 casting Rule of Law itself uses up the turn's one spell" $ do
        plains <- Registry.printing registry "Plains"
        ruleOfLaw <- Registry.printing registry "Rule of Law"
        let (_, _, afterFirst) = ruleOfLawAfterFirst plains ruleOfLaw
        HU.assertBool "alice is now prohibited" (PlayerEffect.prohibitsCasting S.alice afterFirst)
        HU.assertEqual
          "no cast is offered at all"
          []
          (filter isCast (Action.legalActions S.alice afterFirst)),
      -- The limit is counted PER PLAYER: bob has cast nothing this turn, so
      -- EachPlayer does not prohibit him.
      HU.testCase "CR 109.5 the EachPlayer scope still counts each player's own casts" $ do
        plains <- Registry.printing registry "Plains"
        ruleOfLaw <- Registry.printing registry "Rule of Law"
        let (_, _, afterFirst) = ruleOfLawAfterFirst plains ruleOfLaw
        HU.assertBool "bob is not prohibited" (not (PlayerEffect.prohibitsCasting S.bob afterFirst)),
      -- Engine.handoffTurn clears the event log at the turn handoff, so
      -- "this turn" (castsThisTurn's fold over the log) is exactly the
      -- log's own extent -- CR 608.2i is the "look back in time" rule and
      -- says nothing about the log being cleared, so this is an
      -- implementation fact rather than a rules citation.
      HU.testCase "the restriction lifts on the next turn" $ do
        plains <- Registry.printing registry "Plains"
        ruleOfLaw <- Registry.printing registry "Rule of Law"
        let (_, b, afterFirst) = ruleOfLawAfterFirst plains ruleOfLaw
            handoff gs = S.runPure S.identityAnswer gs Engine.handoffTurn
            nextOwnTurn =
              (handoff (handoff afterFirst))
                { GameState.phase = Phase.PrecombatMain,
                  GameState.priority = Just S.alice
                }
        HU.assertEqual "alice is active again" S.alice (GameState.activePlayer nextOwnTurn)
        HU.assertBool "not prohibited" (not (PlayerEffect.prohibitsCasting S.alice nextOwnTurn))
        HU.assertBool "b offered again" (elem (Action.Type.Cast b) (Action.legalActions S.alice nextOwnTurn)),
      -- Ruling: "If you cast a spell that was countered, you can't cast
      -- another spell during the same turn." The counted event is the CAST.
      HU.testCase "CR 601.2i a countered spell still counted" $ do
        plains <- Registry.printing registry "Plains"
        ruleOfLaw <- Registry.printing registry "Rule of Law"
        let (_, x, _, plain) = ruleOfLawBoard plains ruleOfLaw
            onBoard = snd (S.addCreature ruleOfLaw S.alice plain)
            cast = S.runPure S.identityAnswer onBoard (Cast.castSpell S.alice x)
        case GameState.stack cast of
          [] -> HU.assertFailure "expected the spell on the stack"
          top : _ -> do
            let countered = S.runPure S.identityAnswer cast (Event.counter top)
            HU.assertEqual "the stack is empty again" [] (GameState.stack countered)
            HU.assertBool "still prohibited" (PlayerEffect.prohibitsCasting S.alice countered),
      -- The effect is RE-DERIVED from the battlefield on every read, so there
      -- is no stored state to unwind when its source leaves.
      HU.testCase "CR 604.2 destroying Rule of Law lifts the restriction in the same turn" $ do
        plains <- Registry.printing registry "Plains"
        ruleOfLaw <- Registry.printing registry "Rule of Law"
        let (_, _, z, plain) = ruleOfLawBoard plains ruleOfLaw
            (rol, onBoard) = S.addCreature ruleOfLaw S.alice plain
            castOne = S.withEvents [GameEvent.SpellCast S.alice] onBoard
            gone = S.runPure S.identityAnswer castOne (Event.destroy Regenerability.Regenerable [rol])
        HU.assertBool "prohibited while it stands" (PlayerEffect.prohibitsCasting S.alice castOne)
        HU.assertBool "not prohibited once it is gone" (not (PlayerEffect.prohibitsCasting S.alice gone))
        HU.assertBool "and a cast is offered again" (elem (Action.Type.Cast z) (Action.legalActions S.alice gone)),
      -- CR 601.3's prohibit half applies to EVERY cast, including a
      -- Panglacial Wurm cast from the library: the Panglacial permission
      -- (Cast.permitsCastWhileSearching) excepts only the timing half, not
      -- the prohibition half. Seven Forests pay the Wurm's {5}{G}{G}.
      HU.testCase "CR 601.3 Rule of Law also prohibits casting Panglacial Wurm from the library" $ do
        forest <- Registry.printing registry "Forest"
        ruleOfLaw <- Registry.printing registry "Rule of Law"
        panglacialWurm <- Registry.printing registry "Panglacial Wurm"
        let base = S.landsInPlay forest 7
            (_, withRuleOfLaw) = S.addCreature ruleOfLaw S.alice base
            (_, gs) = S.addLibraryCard panglacialWurm S.alice withRuleOfLaw
            castOne = S.withEvents [GameEvent.SpellCast S.alice] gs
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

white :: ManaSymbol.ManaSymbol
white = ManaSymbol.OfType (ManaType.Colored Color.White)

black :: ManaSymbol.ManaSymbol
black = ManaSymbol.OfType (ManaType.Colored Color.Black)

-- A reduction by an amount of GENERIC mana (CR 118.7a) -- the Medallion's shape,
-- and the only shape a reduction had before Edgewalker.
generic :: Natural -> ManaCost.ManaCost
generic n = ManaCost.MkManaCost [ManaSymbol.Generic n]

-- Cost.total via a bare ManaCost, component-free -- this suite predates P8's
-- Cost/CostComponent generalization and exercises only the mana half
-- (costAdjustments), so the wrap-and-unwrap stays local here instead of
-- rewriting every assertion below to a full Cost.Type.MkCost literal.
totalManaCost :: PlayerId.PlayerId -> ObjectId.ObjectId -> ManaCost.ManaCost -> GameState.GameState -> Maybe ManaCost.ManaCost
totalManaCost pid oid manaCost gs = Cost.Type.mana (Cost.total pid oid (Cost.Type.MkCost (Just manaCost) []) gs)

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
          (Cost.applyAdjustments ([], [generic 1]) (ManaCost.MkManaCost [blue])),
      HU.testCase "CR 118.7a a reduction takes only the generic component" $
        HU.assertEqual
          "{2}{U} reduced by {1} is {1}{U}"
          (ManaCost.MkManaCost [ManaSymbol.Generic 1, blue])
          (Cost.applyAdjustments ([], [generic 1]) (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue])),
      HU.testCase "CR 601.2f the total can't be reduced below {0}" $
        HU.assertEqual
          "{1} reduced by {3} is {0}"
          (ManaCost.MkManaCost [])
          (Cost.applyAdjustments ([], [generic 3]) (ManaCost.MkManaCost [ManaSymbol.Generic 1])),
      -- THE ORDER TEST, in the small. Increase first gives {1}{U}, which the
      -- reduction takes back to {U}. Reduce first loses the reduction to CR
      -- 118.7a's empty generic component, and the increase then leaves {1}{U}.
      HU.testCase "CR 601.2f every increase applies before any reduction" $
        HU.assertEqual
          "{U} +{1} -{1} is {U}"
          (ManaCost.MkManaCost [blue])
          (Cost.applyAdjustments ([1], [generic 1]) (ManaCost.MkManaCost [blue])),
      -- CR 118.7's typed half, which CR 118.7a's generic half above cannot
      -- reach: a reduction that NAMES a mana type takes that type's symbols.
      HU.testCase "CR 118.7 a typed reduction takes the cost's matching symbols" $
        HU.assertEqual
          "{1}{W}{B} reduced by {W}{B} is {1}"
          (ManaCost.MkManaCost [ManaSymbol.Generic 1])
          (Cost.applyAdjustments ([], [ManaCost.MkManaCost [white, black]]) (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black])),
      HU.testCase "CR 118.7 one reducing symbol takes exactly one matching symbol" $
        HU.assertEqual
          "{W}{W} reduced by {W} is {W}"
          (ManaCost.MkManaCost [white])
          (Cost.applyAdjustments ([], [ManaCost.MkManaCost [white]]) (ManaCost.MkManaCost [white, white])),
      -- THE HEADLINE FALSIFIER for the typed half, and the one place pawl
      -- deliberately does not do what CR 118.7b-d would (#309). Edgewalker's own
      -- reminder text is the assertion: "if you cast a Cleric spell with mana
      -- cost {1}{W}, it costs {1} to cast" -- so the {B} half, which the cost
      -- cannot satisfy, takes NOTHING rather than one generic mana.
      HU.testCase "an excess typed reduction is dropped, not spilled onto generic" $
        HU.assertEqual
          "{1}{W} reduced by {W}{B} is {1}"
          (ManaCost.MkManaCost [ManaSymbol.Generic 1])
          (Cost.applyAdjustments ([], [ManaCost.MkManaCost [white, black]]) (ManaCost.MkManaCost [ManaSymbol.Generic 1, white])),
      -- Ruling: "If you have more than one of these on the battlefield, the cost
      -- reduction is cumulative." Cumulative, and still bounded by what the cost
      -- actually has to give.
      HU.testCase "two typed reductions pool, and the second finds nothing left to take" $
        HU.assertEqual
          "{1}{W}{B} reduced by {W}{B} twice is {1}"
          (ManaCost.MkManaCost [ManaSymbol.Generic 1])
          (Cost.applyAdjustments ([], [ManaCost.MkManaCost [white, black], ManaCost.MkManaCost [white, black]]) (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black])),
      -- The two halves of ONE reduction, on the two halves of one cost: CR
      -- 118.7a routes the {1} to the generic component and the {U} takes the
      -- blue symbol, and neither reaches into the other's component.
      HU.testCase "CR 118.7a a mixed reduction splits by component" $
        HU.assertEqual
          "{2}{U} reduced by {1}{U} is {1}"
          (ManaCost.MkManaCost [ManaSymbol.Generic 1])
          (Cost.applyAdjustments ([], [ManaCost.MkManaCost [ManaSymbol.Generic 1, blue]]) (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue]))
    ]

-- alice controls Thalia and `n` untapped Mountains; her hand holds one
-- Lightning Bolt ({R} instant -- noncreature) and one Goblin Piker ({1}{R}
-- creature). Loaded fresh inside each case that needs it -- equivalent
-- because loading is deterministic and cached (batch-recipe.md).
thaliaBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
thaliaBoard mountain thalia lightningBolt piker n =
  let base = S.landsInPlay mountain n
      (_, gs1) = S.addCreature thalia S.alice base
      (bolt, gs2) = S.addHandCard lightningBolt S.alice gs1
      (pikerId, gs3) = S.addHandCard piker S.alice gs2
   in ( bolt,
        pikerId,
        gs3
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

thaliaUntaxed :: Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
thaliaUntaxed mountain lightningBolt n =
  let base = S.landsInPlay mountain n
      (bolt, gs1) = S.addHandCard lightningBolt S.alice base
   in (bolt, gs1 {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice})

-- Thalia, Guardian of Thraben {1}{W} Legendary Creature -- Human Soldier 2/1:
-- "First strike / Noncreature spells cost {1} more to cast."
thaliaTests :: Registry.Type.Registry -> Tasty.TestTree
thaliaTests registry =
  Tasty.testGroup
    "Thalia"
    [ HU.testCase "CR 601.2f a noncreature spell's total cost is one more" $ do
        mountain <- Registry.printing registry "Mountain"
        thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        piker <- Registry.printing registry "Goblin Piker"
        let (bolt, _, gs) = thaliaBoard mountain thalia lightningBolt piker 3
        HU.assertEqual
          "{R} becomes {1}{R}"
          (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]))
          (totalManaCost S.alice bolt (ManaCost.MkManaCost [red]) gs),
      -- Ruling: "Thalia's ability affects each spell that's not a creature
      -- spell, including your own." The Filter reads the PROJECTION.
      HU.testCase "a creature spell fails the effect's criterion, so it is unaffected" $ do
        mountain <- Registry.printing registry "Mountain"
        thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        piker <- Registry.printing registry "Goblin Piker"
        let (_, pikerId, gs) = thaliaBoard mountain thalia lightningBolt piker 3
        HU.assertEqual
          "{1}{R} stays {1}{R}"
          (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]))
          (totalManaCost S.alice pikerId (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]) gs),
      -- BOTH sites, one scenario. Taxing castability but not payment lets
      -- the player underpay; taxing payment but not castability offers a
      -- cast that cannot be afforded, and there is no mid-announcement
      -- rewind (#56) -- that is a wedged game, not a rejected action.
      HU.testCase "CR 601.2f castability is measured against the total cost" $ do
        mountain <- Registry.printing registry "Mountain"
        thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        piker <- Registry.printing registry "Goblin Piker"
        let (boltOne, _, oneLand) = thaliaBoard mountain thalia lightningBolt piker 1
            (boltTwo, _, twoLands) = thaliaBoard mountain thalia lightningBolt piker 2
            (_, pikerTwo, twoLandsAgain) = thaliaBoard mountain thalia lightningBolt piker 2
        HU.assertBool "one Mountain is not enough for a taxed Bolt" (not (Cast.castable S.alice boltOne oneLand))
        HU.assertBool "two Mountains are" (Cast.castable S.alice boltTwo twoLands)
        HU.assertBool "and an untaxed creature spell needs only its printed two" (Cast.castable S.alice pikerTwo twoLandsAgain),
      HU.testCase "CR 601.2f payment spends the total cost" $ do
        mountain <- Registry.printing registry "Mountain"
        thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        piker <- Registry.printing registry "Goblin Piker"
        let (bolt, _, gs) = thaliaBoard mountain thalia lightningBolt piker 3
            paid = S.runPure S.identityAnswer gs (Cast.castSpell S.alice bolt)
            (boltU, gsU) = thaliaUntaxed mountain lightningBolt 3
            paidU = S.runPure S.identityAnswer gsU (Cast.castSpell S.alice boltU)
        HU.assertEqual "taxed: two lands tapped" 2 (S.tappedCount S.alice paid)
        HU.assertEqual "untaxed: one land tapped" 1 (S.tappedCount S.alice paidU),
      -- The EachPlayer scope: Thalia's controller is taxed (every assertion
      -- above is alice's own spell) and so is her opponent.
      HU.testCase "CR 611.1 the opponent is taxed too" $ do
        mountain <- Registry.printing registry "Mountain"
        thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        piker <- Registry.printing registry "Goblin Piker"
        let (_, _, gs) = thaliaBoard mountain thalia lightningBolt piker 3
            (bobBolt, withBob) = S.addHandCard lightningBolt S.bob gs
        HU.assertEqual
          "bob's {R} is also {1}{R}"
          (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]))
          (totalManaCost S.bob bobBolt (ManaCost.MkManaCost [red]) withBob),
      -- Cast.castableWhileSearching's cost half (CR 601.2f, via Cost.total),
      -- untested until now: Panglacial Wurm is a CREATURE spell, so Thalia's
      -- NoncreatureSpell criterion (matched against the projection, per
      -- Projection.cardTypesOf) does not admit it, and its total cost is
      -- unaffected. Rule of Law -- in the SAME GameState, never cast -- is
      -- the positive control: it IS a noncreature spell, so its total cost
      -- IS taxed here, which is what proves Thalia's effect is actually live
      -- rather than the Wurm assertion passing because nothing was ever
      -- taxed. Exactly seven Forests pay the Wurm's printed {5}{G}{G} with no
      -- slack (CastSpec's own "too little mana" case shows fewer is not
      -- enough), so if the tax wrongly reached the Wurm, castableWhileSearching
      -- would offer nothing here.
      HU.testCase "CR 601.2f a library-cast creature spell is unaffected by Thalia's noncreature tax" $ do
        forest <- Registry.printing registry "Forest"
        thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
        panglacialWurm <- Registry.printing registry "Panglacial Wurm"
        ruleOfLaw <- Registry.printing registry "Rule of Law"
        let green = ManaSymbol.OfType (ManaType.Colored Color.Green)
            base = S.landsInPlay forest 7
            (_, withThalia) = S.addCreature thalia S.alice base
            (wurm, withWurm) = S.addLibraryCard panglacialWurm S.alice withThalia
            (rol, gs) = S.addHandCard ruleOfLaw S.alice withWurm
        HU.assertEqual
          "positive control: Rule of Law, a noncreature spell, IS taxed here"
          (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3, white]))
          (totalManaCost S.alice rol (ManaCost.MkManaCost [ManaSymbol.Generic 2, white]) gs)
        HU.assertEqual
          "the Wurm's total cost is untouched"
          (Just (ManaCost.MkManaCost [ManaSymbol.Generic 5, green, green]))
          (totalManaCost S.alice wurm (ManaCost.MkManaCost [ManaSymbol.Generic 5, green, green]) gs)
        HU.assertEqual
          "exactly seven Forests still afford it, so castableWhileSearching offers it"
          [wurm]
          (Cast.castableWhileSearching S.alice gs)
    ]

-- alice controls a Sapphire Medallion and `n` untapped Islands; her hand
-- holds Unsummon ({U} instant), Divination ({2}{U} sorcery) and Lightning
-- Bolt ({R} instant). Loaded fresh inside each case that needs it --
-- equivalent because loading is deterministic and cached (batch-recipe.md).
medallionBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
medallionBoard island sapphireMedallion unsummonPrinting divinationPrinting lightningBolt n =
  let base = S.landsInPlay island n
      (_, gs1) = S.addCreature sapphireMedallion S.alice base
      (unsummon, gs3) = S.addHandCard unsummonPrinting S.alice gs1
      (divination, gs4) = S.addHandCard divinationPrinting S.alice gs3
      (bolt, gs5) = S.addHandCard lightningBolt S.alice gs4
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
medallionBothBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
medallionBothBoard island sapphireMedallion thalia unsummonPrinting =
  let base = S.landsInPlay island 1
      (_, gs1) = S.addCreature sapphireMedallion S.alice base
      (_, gs2) = S.addCreature thalia S.alice gs1
      (unsummon, gs3) = S.addHandCard unsummonPrinting S.alice gs2
   in ( unsummon,
        gs3
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Sapphire Medallion {2} Artifact: "Blue spells you cast cost {1} less to cast."
medallionTests :: Registry.Type.Registry -> Tasty.TestTree
medallionTests registry =
  Tasty.testGroup
    "SapphireMedallion"
    [ -- Ruling: "The ability can't reduce the amount of colored mana you pay
      -- for a spell. It reduces only the generic mana component of that
      -- cost." THE HEADLINE FALSIFIER: subtracting from the mana value would
      -- make this spell free.
      HU.testCase "CR 118.7a a {U} spell still costs {U}" $ do
        island <- Registry.printing registry "Island"
        sapphireMedallion <- Registry.printing registry "Sapphire Medallion"
        unsummonPrinting <- Registry.printing registry "Unsummon"
        divinationPrinting <- Registry.printing registry "Divination"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let (unsummon, _, _, gs) = medallionBoard island sapphireMedallion unsummonPrinting divinationPrinting lightningBolt 2
        HU.assertEqual
          "unchanged"
          (Just (ManaCost.MkManaCost [blue]))
          (totalManaCost S.alice unsummon (ManaCost.MkManaCost [blue]) gs),
      HU.testCase "CR 118.7a a {2}{U} spell costs {1}{U}" $ do
        island <- Registry.printing registry "Island"
        sapphireMedallion <- Registry.printing registry "Sapphire Medallion"
        unsummonPrinting <- Registry.printing registry "Unsummon"
        divinationPrinting <- Registry.printing registry "Divination"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let (_, divination, _, gs) = medallionBoard island sapphireMedallion unsummonPrinting divinationPrinting lightningBolt 2
        HU.assertEqual
          "one generic off"
          (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, blue]))
          (totalManaCost S.alice divination (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue]) gs),
      HU.testCase "a red spell fails the effect's colour criterion, so it is unaffected" $ do
        island <- Registry.printing registry "Island"
        sapphireMedallion <- Registry.printing registry "Sapphire Medallion"
        unsummonPrinting <- Registry.printing registry "Unsummon"
        divinationPrinting <- Registry.printing registry "Divination"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let (_, _, bolt, gs) = medallionBoard island sapphireMedallion unsummonPrinting divinationPrinting lightningBolt 2
        HU.assertEqual
          "unchanged"
          (Just (ManaCost.MkManaCost [red]))
          (totalManaCost S.alice bolt (ManaCost.MkManaCost [red]) gs),
      -- Divination is {2}{U}: three mana printed, two after the discount. Two
      -- Islands is exactly the amount that tells the two apart.
      HU.testCase "CR 601.2f the discount is observable at the castability gate" $ do
        island <- Registry.printing registry "Island"
        sapphireMedallion <- Registry.printing registry "Sapphire Medallion"
        unsummonPrinting <- Registry.printing registry "Unsummon"
        divinationPrinting <- Registry.printing registry "Divination"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let (_, divination, _, withMedallion) = medallionBoard island sapphireMedallion unsummonPrinting divinationPrinting lightningBolt 2
            bareBoard =
              let base = S.landsInPlay island 2
                  (d, gs1) = S.addHandCard divinationPrinting S.alice base
               in ( d,
                    gs1
                      { GameState.phase = Phase.PrecombatMain,
                        GameState.activePlayer = S.alice,
                        GameState.priority = Just S.alice
                      }
                  )
            (bareDivination, bare) = bareBoard
        HU.assertBool "castable for {1}{U} with two Islands" (Cast.castable S.alice divination withMedallion)
        HU.assertBool "and not castable for {2}{U} without the Medallion" (not (Cast.castable S.alice bareDivination bare)),
      -- CR 611.1 / 109.5: the You scope is the effect's controller.
      HU.testCase "CR 109.5 the You scope does not discount an opponent's spell" $ do
        island <- Registry.printing registry "Island"
        sapphireMedallion <- Registry.printing registry "Sapphire Medallion"
        unsummonPrinting <- Registry.printing registry "Unsummon"
        divinationPrinting <- Registry.printing registry "Divination"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let (_, _, _, gs) = medallionBoard island sapphireMedallion unsummonPrinting divinationPrinting lightningBolt 2
            (bobDivination, withBob) = S.addHandCard divinationPrinting S.bob gs
        HU.assertEqual
          "bob pays full price"
          (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue]))
          (totalManaCost S.bob bobDivination (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue]) withBob),
      -- Ruling: "If there are additional costs to cast a spell, or if the
      -- cost to cast a spell is increased by an effect (such as the one
      -- created by Thalia, Guardian of Thraben's ability), apply those
      -- increases before applying cost reductions." THE ORDER TEST, and it
      -- names a cost with NO generic component on purpose: the two orders
      -- agree wherever the CR 601.2f floor does not bind.
      HU.testCase "CR 601.2f Thalia then the Medallion leaves a {U} spell at exactly {U}" $ do
        island <- Registry.printing registry "Island"
        sapphireMedallion <- Registry.printing registry "Sapphire Medallion"
        thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
        unsummonPrinting <- Registry.printing registry "Unsummon"
        let (unsummon, gs) = medallionBothBoard island sapphireMedallion thalia unsummonPrinting
        HU.assertEqual
          "increase first, then reduce"
          (Just (ManaCost.MkManaCost [blue]))
          (totalManaCost S.alice unsummon (ManaCost.MkManaCost [blue]) gs)
        HU.assertBool "so one Island is enough" (Cast.castable S.alice unsummon gs)
    ]

-- Humility {2}{W}{W} Enchantment: "All creatures lose all abilities and have
-- base power and toughness 1/1." CR 604.2: a static ability's continuous effect
-- is active only "as long as the permanent with the ability remains on the
-- battlefield AND HAS THE ABILITY", so a CR 613.1f layer-6 removal takes the
-- player-affecting half of a card's text with it -- the axis Pawl.Projection
-- already gates for the projected characteristics (abilitiesRemoved).
--
-- CR 613.6's rescue ("if an effect starts to apply in one layer ... it will
-- continue to be applied ... even if the ability generating the effect is
-- removed") cannot reach a player ability: CR 613.10/613.11 apply these effects
-- AFTER the seven layers have run, so one never starts to apply before layer 6
-- and the cut is unconditional. Same shape as the layer-7-only static ability
-- gatherStatic drops.
humilityTests :: Registry.Type.Registry -> Tasty.TestTree
humilityTests registry =
  Tasty.testGroup
    "Humility"
    [ -- THE PROVING CASE. Thalia is a creature, so Humility reaches her with no
      -- animator in the way, and her tax is the only thing standing between the
      -- Bolt and its printed cost.
      HU.testCase "CR 604.2 Humility takes Thalia's ability, so her tax stops applying" $ do
        mountain <- Registry.printing registry "Mountain"
        thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        piker <- Registry.printing registry "Goblin Piker"
        humility <- Registry.printing registry "Humility"
        let (bolt, _, taxed) = thaliaBoard mountain thalia lightningBolt piker 3
            humbled = S.withHumility humility taxed
        HU.assertEqual
          "control: with Thalia's ability intact, {R} is {1}{R}"
          (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]))
          (totalManaCost S.alice bolt (ManaCost.MkManaCost [red]) taxed)
        HU.assertEqual
          "under Humility the printed {R} is the whole cost"
          (Just (ManaCost.MkManaCost [red]))
          (totalManaCost S.alice bolt (ManaCost.MkManaCost [red]) humbled),
      -- The same statement at the two gameplay sites the Thalia group tests,
      -- with ONE Mountain -- the amount that tells the taxed and untaxed costs
      -- apart.
      HU.testCase "CR 601.2f castability and payment both drop the stripped tax" $ do
        mountain <- Registry.printing registry "Mountain"
        thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        piker <- Registry.printing registry "Goblin Piker"
        humility <- Registry.printing registry "Humility"
        let (bolt, _, oneLand) = thaliaBoard mountain thalia lightningBolt piker 1
            humbled = S.withHumility humility oneLand
            paid = S.runPure S.identityAnswer humbled (Cast.castSpell S.alice bolt)
        HU.assertBool "control: one Mountain cannot pay the taxed Bolt" (not (Cast.castable S.alice bolt oneLand))
        HU.assertBool "under Humility one Mountain is enough" (Cast.castable S.alice bolt humbled)
        HU.assertEqual "and paying it taps exactly that one" 1 (S.tappedCount S.alice paid),
      -- THE DISCRIMINATOR against "Humility silences every player ability".
      -- Humility's affected set is "each creature", and Sapphire Medallion is an
      -- artifact -- nothing animates it here, so its discount is untouched.
      HU.testCase "CR 613.1f Humility reaches only creatures, so an artifact's ability stands" $ do
        island <- Registry.printing registry "Island"
        sapphireMedallion <- Registry.printing registry "Sapphire Medallion"
        unsummonPrinting <- Registry.printing registry "Unsummon"
        divinationPrinting <- Registry.printing registry "Divination"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        humility <- Registry.printing registry "Humility"
        let (_, divination, _, gs) = medallionBoard island sapphireMedallion unsummonPrinting divinationPrinting lightningBolt 2
            humbled = S.withHumility humility gs
        HU.assertEqual
          "the Medallion still discounts {2}{U} to {1}{U}"
          (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, blue]))
          (totalManaCost S.alice divination (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue]) humbled),
      -- The animated case: Rule of Law is an enchantment, so Humility alone
      -- leaves it alone. Opalescence's CR 613.1d layer-4 AddCardType is what
      -- brings it inside "each creature" -- and abilitiesRemoved judges the
      -- affected set against the layers 1-5 partial, which is where that
      -- animation already is. Opalescence itself is spared by its own "each
      -- other enchantment", so it keeps animating.
      HU.testCase "CR 613.1d Opalescence animates Rule of Law into Humility's set" $ do
        ruleOfLaw <- Registry.printing registry "Rule of Law"
        humility <- Registry.printing registry "Humility"
        opalescence <- Registry.printing registry "Opalescence"
        let base = Setup.emptyGame S.bothPlayers
            (_, withRuleOfLaw) = S.addCreature ruleOfLaw S.alice base
            withHumility = S.withHumility humility withRuleOfLaw
            (_, withOpalescence) = S.addCreature opalescence S.alice withHumility
            castOne = S.withEvents [GameEvent.SpellCast S.alice]
        HU.assertBool
          "control: Humility alone does not reach an enchantment"
          (PlayerEffect.prohibitsCasting S.alice (castOne withHumility))
        HU.assertBool
          "once animated, Rule of Law loses the ability and the limit lifts"
          (not (PlayerEffect.prohibitsCasting S.alice (castOne withOpalescence)))
    ]

-- alice controls `copies` Edgewalkers and `n` untapped Plains; her hand holds
-- one more Edgewalker ({1}{W}{B} Human Cleric) and one Goblin Piker ({1}{R}
-- Goblin Warrior -- no Cleric anywhere on it). Loaded fresh inside each case
-- that needs it -- equivalent because loading is deterministic and cached
-- (batch-recipe.md).
edgewalkerBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> Int -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
edgewalkerBoard plains edgewalker piker copies n =
  let base = S.landsInPlay plains n
      put g _ = snd (S.addCreature edgewalker S.alice g)
      withCopies = List.foldl' put base [1 .. copies]
      (spell, gs1) = S.addHandCard edgewalker S.alice withCopies
      (pikerId, gs2) = S.addHandCard piker S.alice gs1
   in ( spell,
        pikerId,
        gs2
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Edgewalker {1}{W}{B} Creature -- Human Cleric 2/2: "Cleric spells you cast
-- cost {W}{B} less to cast. This effect reduces only the amount of colored mana
-- you pay."
--
-- The card the typed half of a reduction exists for, and the one that pins the
-- excess as dropped rather than spilled (#309). Edgewalker is itself a Cleric,
-- so the spell it discounts is another copy of itself and the pool needs no
-- second Cleric to make the point.
edgewalkerTests :: Registry.Type.Registry -> Tasty.TestTree
edgewalkerTests registry =
  Tasty.testGroup
    "Edgewalker"
    [ HU.testCase "CR 118.7 a Cleric spell loses one white and one black symbol" $ do
        plains <- Registry.printing registry "Plains"
        edgewalker <- Registry.printing registry "Edgewalker"
        piker <- Registry.printing registry "Goblin Piker"
        let (spell, _, gs) = edgewalkerBoard plains edgewalker piker 1 3
        HU.assertEqual
          "{1}{W}{B} becomes {1}"
          (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1]))
          (totalManaCost S.alice spell (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black]) gs),
      HU.testCase "a spell with no Cleric subtype fails the effect's criterion, so it is unaffected" $ do
        plains <- Registry.printing registry "Plains"
        edgewalker <- Registry.printing registry "Edgewalker"
        piker <- Registry.printing registry "Goblin Piker"
        let (_, pikerId, gs) = edgewalkerBoard plains edgewalker piker 1 3
        HU.assertEqual
          "{1}{R} stays {1}{R}"
          (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]))
          (totalManaCost S.alice pikerId (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]) gs),
      -- THE HEADLINE FALSIFIER, at the board. Ruling: "If you have more than one
      -- of these on the battlefield, the cost reduction is cumulative" -- so two
      -- Edgewalkers really do offer {W}{B}{W}{B}. The cost has one white and one
      -- black to give, and the second pair strands: under CR 118.7b-d it would
      -- go on to eat the {1} and leave the spell free, and Edgewalker's card
      -- text stops it (#309).
      HU.testCase "a second Edgewalker's stranded halves leave the generic component alone" $ do
        plains <- Registry.printing registry "Plains"
        edgewalker <- Registry.printing registry "Edgewalker"
        piker <- Registry.printing registry "Goblin Piker"
        let (spell, _, gs) = edgewalkerBoard plains edgewalker piker 2 3
        HU.assertEqual
          "{1}{W}{B} is still {1}, not {0}"
          (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1]))
          (totalManaCost S.alice spell (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black]) gs),
      -- BOTH cost sites, one scenario, exactly as the Thalia group tests them.
      -- Three Plains produce white mana and nothing else, so they can never pay
      -- a {B}: what makes the discounted spell castable is that the reduction
      -- removed the black SYMBOL, not that it removed an amount.
      HU.testCase "CR 601.2f castability is measured against the total cost" $ do
        plains <- Registry.printing registry "Plains"
        edgewalker <- Registry.printing registry "Edgewalker"
        piker <- Registry.printing registry "Goblin Piker"
        let (discounted, _, withEdgewalker) = edgewalkerBoard plains edgewalker piker 1 3
            (undiscounted, _, bare) = edgewalkerBoard plains edgewalker piker 0 3
        HU.assertBool "three Plains cannot pay a printed {1}{W}{B}" (not (Cast.castable S.alice undiscounted bare))
        HU.assertBool "but they can pay the discounted {1}" (Cast.castable S.alice discounted withEdgewalker),
      HU.testCase "CR 601.2f payment spends the total cost" $ do
        plains <- Registry.printing registry "Plains"
        edgewalker <- Registry.printing registry "Edgewalker"
        piker <- Registry.printing registry "Goblin Piker"
        let (spell, _, gs) = edgewalkerBoard plains edgewalker piker 1 3
            paid = S.runPure S.identityAnswer gs (Cast.castSpell S.alice spell)
        HU.assertEqual "one Plains tapped, not three" 1 (S.tappedCount S.alice paid),
      -- CR 611.1 / 109.5: the You scope is the effect's controller, and bob
      -- controls no Edgewalker.
      HU.testCase "CR 109.5 the You scope does not discount an opponent's Cleric spell" $ do
        plains <- Registry.printing registry "Plains"
        edgewalker <- Registry.printing registry "Edgewalker"
        piker <- Registry.printing registry "Goblin Piker"
        let (_, _, gs) = edgewalkerBoard plains edgewalker piker 1 3
            (bobEdgewalker, withBob) = S.addHandCard edgewalker S.bob gs
        HU.assertEqual
          "bob pays full price"
          (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black]))
          (totalManaCost S.bob bobEdgewalker (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black]) withBob)
    ]

-- alice holds nine Plains cards; the board is otherwise empty unless a
-- printing is named. Loaded fresh inside each case that needs it --
-- equivalent because loading is deterministic and cached (batch-recipe.md).
reliquaryHandOfNine :: Printing.Printing -> [Printing.Printing] -> GameState.GameState
reliquaryHandOfNine plains extra =
  let gs0 = Setup.emptyGame S.bothPlayers
      put g printing = snd (S.addCreature printing S.alice g)
      withExtra = List.foldl' put gs0 extra
      add g _ = snd (S.addHandCard plains S.alice g)
   in List.foldl' add withExtra [1 .. 9 :: Int]

reliquaryCleanup :: GameState.GameState -> GameState.GameState
reliquaryCleanup gs = S.runPure S.identityAnswer gs (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup))

-- Reliquary Tower, a Land: "You have no maximum hand size. / {T}: Add {C}."
reliquaryTowerTests :: Registry.Type.Registry -> Tasty.TestTree
reliquaryTowerTests registry =
  Tasty.testGroup
    "ReliquaryTower"
    [ HU.testCase "CR 402.2 the maximum hand size is normally seven" $ do
        plains <- Registry.printing registry "Plains"
        HU.assertEqual "seven" (Just 7) (PlayerEffect.maximumHandSize S.alice (reliquaryHandOfNine plains [])),
      HU.testCase "CR 514.1 nine cards at cleanup discards down to seven" $ do
        plains <- Registry.printing registry "Plains"
        let after = reliquaryCleanup (reliquaryHandOfNine plains [])
        HU.assertEqual "hand" 7 (S.handSize S.alice after)
        HU.assertEqual "two discarded" 2 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      HU.testCase "CR 402.2 Reliquary Tower removes the maximum entirely" $ do
        plains <- Registry.printing registry "Plains"
        reliquaryTower <- Registry.printing registry "Reliquary Tower"
        HU.assertEqual
          "no maximum"
          Nothing
          (PlayerEffect.maximumHandSize S.alice (reliquaryHandOfNine plains [reliquaryTower])),
      HU.testCase "CR 514.1 with Reliquary Tower nothing is discarded and nothing is asked" $ do
        plains <- Registry.printing registry "Plains"
        reliquaryTower <- Registry.printing registry "Reliquary Tower"
        let after = reliquaryCleanup (reliquaryHandOfNine plains [reliquaryTower])
        HU.assertEqual "hand keeps nine" 9 (S.handSize S.alice after)
        HU.assertEqual "nothing discarded" 0 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      -- CR 109.5: the You scope. bob does not share alice's Tower.
      HU.testCase "CR 109.5 the opponent still has a maximum hand size" $ do
        plains <- Registry.printing registry "Plains"
        reliquaryTower <- Registry.printing registry "Reliquary Tower"
        HU.assertEqual
          "seven"
          (Just 7)
          (PlayerEffect.maximumHandSize S.bob (reliquaryHandOfNine plains [reliquaryTower])),
      -- CR 305.7: a land whose subtype is SET to a basic type loses its
      -- rules-text abilities. Reliquary Tower is nonbasic, and Blood Moon is
      -- in the pool -- so this axis composes with the layer system without
      -- being part of it.
      HU.testCase "CR 305.7 Blood Moon strips the ability off the Tower" $ do
        plains <- Registry.printing registry "Plains"
        reliquaryTower <- Registry.printing registry "Reliquary Tower"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let board = reliquaryHandOfNine plains [reliquaryTower, bloodMoon]
        HU.assertEqual "seven again" (Just 7) (PlayerEffect.maximumHandSize S.alice board)
    ]

-- Seed a stored player effect keyed to a REAL battlefield object, not
-- S.addPlayerEffect's stand-in id 998 -- so a S.youControlSource condition
-- check has something to genuinely hold or fail against. Mirrors
-- ExpirySpec's whileEffect, adapted to ActivePlayerEffect.
addPlayerEffectAt ::
  ObjectId.ObjectId ->
  Expiry.Type.Expiry ->
  PlayerScope.PlayerScope ->
  PlayerEffect.Type.PlayerEffect ->
  PlayerId.PlayerId ->
  GameState.GameState ->
  GameState.GameState
addPlayerEffectAt source expiry scope effect controller gs =
  let (ts, gs1) = Game.freshTimestamp gs
      active =
        ActivePlayerEffect.MkActivePlayerEffect
          { ActivePlayerEffect.source = source,
            ActivePlayerEffect.controller = controller,
            ActivePlayerEffect.timestamp = ts,
            ActivePlayerEffect.expiry = expiry,
            ActivePlayerEffect.scope = scope,
            ActivePlayerEffect.effect = effect
          }
   in gs1 {GameState.playerEffects = active : GameState.playerEffects gs1}

-- A real permanent on the battlefield, so the effect's condition can
-- genuinely hold rather than being false by construction. Loaded fresh
-- inside each case that needs it -- equivalent because loading is
-- deterministic and cached (batch-recipe.md).
storedConditional :: Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
storedConditional piker =
  let (srcId, withSrc) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
      conditional =
        addPlayerEffectAt
          srcId
          (Expiry.Type.While S.alice S.youControlSource)
          PlayerScope.Opponents
          PlayerEffect.Type.CantCastSpells
          S.alice
          withSrc
   in (srcId, conditional)

-- The STORED carrier: a player effect that outlives the object that made it.
-- Hand-built here, exactly as ExpirySpec hand-builds a ContinuousEffect, so the
-- carrier and its sweeps are proven before an opcode can produce one.
storedTests :: Registry.Type.Registry -> Tasty.TestTree
storedTests registry =
  let base = Setup.emptyGame S.bothPlayers
      silenced =
        S.addPlayerEffect
          Expiry.Type.AtCleanup
          PlayerScope.Opponents
          PlayerEffect.Type.CantCastSpells
          S.alice
          base
   in Tasty.testGroup
        "Stored"
        [ HU.testCase "CR 611.1 a stored effect applies through its scope" $
            do
              HU.assertBool "bob is prohibited" (PlayerEffect.prohibitsCasting S.bob silenced)
              HU.assertBool "alice is not" (not (PlayerEffect.prohibitsCasting S.alice silenced)),
          HU.testCase "CR 514.2 the cleanup sweep drops an AtCleanup player effect" $
            let after = Expiry.dropAtCleanup silenced
             in do
                  HU.assertEqual "one stored before" 1 (length (GameState.playerEffects silenced))
                  HU.assertEqual "none after" [] (GameState.playerEffects after)
                  HU.assertBool "and bob may cast again" (not (PlayerEffect.prohibitsCasting S.bob after)),
          HU.testCase "CR 514.2 the cleanup sweep keeps a Never player effect" $
            let forever = S.addPlayerEffect Expiry.Type.Never PlayerScope.Opponents PlayerEffect.Type.CantCastSpells S.alice base
             in HU.assertEqual "survives" 1 (length (GameState.playerEffects (Expiry.dropAtCleanup forever))),
          -- THE DISCRIMINATING SHAPE: two entries, keyed to the two different
          -- players, on the SAME handoff. An indiscriminate sweep (one that
          -- dropped every stored player effect) would pass the old
          -- single-entry version of this test; here it would wrongly drop
          -- bob's still-live entry too.
          HU.testCase "CR 611.2a the handoff sweep drops only the entry keyed to the player whose turn began" $
            let forBob = S.addPlayerEffect (Expiry.Type.AtTurnOf S.bob) PlayerScope.Opponents PlayerEffect.Type.CantCastSpells S.alice base
                armed = S.addPlayerEffect (Expiry.Type.AtTurnOf S.alice) PlayerScope.Opponents PlayerEffect.Type.CantCastSpells S.alice forBob
                bobsTurn = S.runPure S.identityAnswer armed Engine.handoffTurn
             in do
                  HU.assertEqual "bob is active" S.bob (GameState.activePlayer bobsTurn)
                  HU.assertEqual
                    "the bob-keyed entry ended; the alice-keyed entry survives"
                    [Expiry.Type.AtTurnOf S.alice]
                    (fmap ActivePlayerEffect.expiry (GameState.playerEffects bobsTurn)),
          -- The POSITIVE case: while the condition genuinely holds, the sweep
          -- must leave the effect in place and report that nothing changed.
          -- Without this, "deletes on failure" is indistinguishable from
          -- "empties the list unconditionally".
          HU.testCase "CR 611.2b the conditional sweep keeps a player effect whose condition still holds" $ do
            piker <- Registry.printing registry "Goblin Piker"
            let (_, conditional) = storedConditional piker
                (changed, swept) = Engine.runGamePure S.identityAnswer conditional Expiry.sweepConditional
            HU.assertBool "still prohibited while the source stands" (PlayerEffect.prohibitsCasting S.bob conditional)
            HU.assertBool "the sweep reports no change" (not changed)
            HU.assertEqual "still stored" 1 (length (GameState.playerEffects swept))
            HU.assertBool "still prohibited after a no-op sweep" (PlayerEffect.prohibitsCasting S.bob swept),
          HU.testCase "CR 611.2b the conditional sweep deletes a player effect whose condition has failed" $ do
            piker <- Registry.printing registry "Goblin Piker"
            let (srcId, conditional) = storedConditional piker
                gone = S.runPure S.identityAnswer conditional (Event.destroy Regenerability.Regenerable [srcId])
                (changed, swept) = Engine.runGamePure S.identityAnswer gone Expiry.sweepConditional
            HU.assertBool "the sweep reports a change" changed
            HU.assertEqual "deleted, not masked" [] (GameState.playerEffects swept)
            HU.assertBool "no longer prohibited" (not (PlayerEffect.prohibitsCasting S.bob swept))
        ]

-- The board is BOB's turn on purpose: the "only casting is stopped" ruling
-- names playing a land and activating an ability, and both are only
-- available to the active player or need his own permanents. alice casts
-- Silence at instant speed during his main phase, which is the card's real
-- use. Loaded fresh inside each case that needs it -- equivalent because
-- loading is deterministic and cached (batch-recipe.md).
silenceBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
silenceBoard plains silence mountain prodigalSorcerer piker =
  let gs0 = Setup.emptyGame S.bothPlayers
      -- alice: two Plains, two Silences in hand.
      (_, gs1) = S.addCreature plains S.alice gs0
      (_, gs2) = S.addCreature plains S.alice gs1
      (silenceId, gs3) = S.addHandCard silence S.alice gs2
      (silence2Id, gs4) = S.addHandCard silence S.alice gs3
      -- bob: two Mountains, a Prodigal Sorcerer (a NON-mana activated
      -- ability), a Goblin Piker in hand to cast and a Mountain in hand to
      -- play.
      (_, gs5) = S.addCreature mountain S.bob gs4
      (_, gs6) = S.addCreature mountain S.bob gs5
      (_, gs7) = S.addCreature prodigalSorcerer S.bob gs6
      (pikerId, gs8) = S.addHandCard piker S.bob gs7
      (landId, gs9) = S.addHandCard mountain S.bob gs8
   in ( silenceId,
        silence2Id,
        pikerId,
        landId,
        gs9
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.bob,
            GameState.priority = Just S.bob
          }
      )

-- CR 800.1: the three-seat Silence board. alice has two Plains and a Silence in
-- hand; bob and carol each have TWO Mountains (Goblin Piker is {1}{R}, two mana,
-- so one Mountain each -- as the brief originally specified -- cannot afford it;
-- the two-seat silenceBoard above gives bob two Mountains for the same reason)
-- and a creature in hand to cast. The SEAT COUNT is the whole point -- at two
-- players "the other player" and "every other player" are the same set, so
-- silenceBoard cannot tell the readings apart.
threeSeatSilenceBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
threeSeatSilenceBoard plains silence mountain piker =
  let gs0 = Setup.emptyGame S.threePlayers
      (_, gs1) = S.addCreature plains S.alice gs0
      (_, gs2) = S.addCreature plains S.alice gs1
      (silenceId, gs3) = S.addHandCard silence S.alice gs2
      (_, gs4) = S.addCreature mountain S.bob gs3
      (_, gs5) = S.addCreature mountain S.bob gs4
      (bobsPiker, gs6) = S.addHandCard piker S.bob gs5
      (_, gs7) = S.addCreature mountain S.carol gs6
      (_, gs8) = S.addCreature mountain S.carol gs7
      (carolsPiker, gs9) = S.addHandCard piker S.carol gs8
   in ( silenceId,
        bobsPiker,
        carolsPiker,
        gs9 {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
      )

-- alice casts Silence and it resolves.
silenceAfter :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, GameState.GameState)
silenceAfter plains silence mountain prodigalSorcerer piker =
  let (silenceId, silence2Id, pikerId, landId, before) = silenceBoard plains silence mountain prodigalSorcerer piker
      resolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop
      after = resolveAll (S.runPure S.identityAnswer before (Cast.castSpell S.alice silenceId))
   in (silenceId, silence2Id, pikerId, landId, before, after)

isSilenceActivate :: Action.Type.Action -> Bool
isSilenceActivate action = case action of
  Action.Type.Activate _ _ -> True
  Action.Type.Cast _ -> False
  Action.Type.Play _ -> False
  Action.Type.Pass -> False

-- Silence {W} Instant: "Your opponents can't cast spells this turn."
silenceTests :: Registry.Type.Registry -> Tasty.TestTree
silenceTests registry =
  Tasty.testGroup
    "Silence"
    [ HU.testCase "before Silence resolves, bob may cast his creature" $ do
        plains <- Registry.printing registry "Plains"
        silence <- Registry.printing registry "Silence"
        mountain <- Registry.printing registry "Mountain"
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        piker <- Registry.printing registry "Goblin Piker"
        let (_, _, pikerId, _, before, _) = silenceAfter plains silence mountain prodigalSorcerer piker
        HU.assertBool "offered" (elem (Action.Type.Cast pikerId) (Action.legalActions S.bob before)),
      -- CR 611.2c, THE FALSIFIER: nothing bob owns is a spell when Silence
      -- resolves -- the stack holds only Silence itself. Freeze the affected
      -- set and this card does literally nothing.
      HU.testCase "CR 611.2c the effect reaches a spell that did not exist when it began" $ do
        plains <- Registry.printing registry "Plains"
        silence <- Registry.printing registry "Silence"
        mountain <- Registry.printing registry "Mountain"
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        piker <- Registry.printing registry "Goblin Piker"
        let (_, _, _, _, _, after) = silenceAfter plains silence mountain prodigalSorcerer piker
        HU.assertEqual "one stored effect" 1 (length (GameState.playerEffects after))
        HU.assertBool "bob is prohibited" (PlayerEffect.prohibitsCasting S.bob after)
        HU.assertEqual
          "and no cast is offered"
          []
          (filter isCast (Action.legalActions S.bob after)),
      -- CR 109.5: "your opponents" is scoped off Silence's controller, which
      -- is baked into the stored effect because its source is in a graveyard.
      HU.testCase "CR 109.5 the Opponents scope spares the caster" $ do
        plains <- Registry.printing registry "Plains"
        silence <- Registry.printing registry "Silence"
        mountain <- Registry.printing registry "Mountain"
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        piker <- Registry.printing registry "Goblin Piker"
        let (_, silence2Id, _, _, _, after) = silenceAfter plains silence mountain prodigalSorcerer piker
        HU.assertBool "alice is not prohibited" (not (PlayerEffect.prohibitsCasting S.alice after))
        HU.assertBool "and may cast her second Silence" (Cast.castable S.alice silence2Id after),
      -- Ruling: "The only thing Silence stops is casting spells. Your
      -- opponents can still activate abilities ... they can still play lands,
      -- and so on."
      HU.testCase "CR 601.3 only casting is stopped" $ do
        plains <- Registry.printing registry "Plains"
        silence <- Registry.printing registry "Silence"
        mountain <- Registry.printing registry "Mountain"
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        piker <- Registry.printing registry "Goblin Piker"
        let (_, _, _, landId, _, after) = silenceAfter plains silence mountain prodigalSorcerer piker
        HU.assertBool "bob may still play a land" (elem (Action.Type.Play landId) (Action.legalActions S.bob after))
        HU.assertBool "and still activate an ability" (any isSilenceActivate (Action.legalActions S.bob after)),
      HU.testCase "CR 514.2 the prohibition ends at cleanup" $ do
        plains <- Registry.printing registry "Plains"
        silence <- Registry.printing registry "Silence"
        mountain <- Registry.printing registry "Mountain"
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        piker <- Registry.printing registry "Goblin Piker"
        let (_, _, _, _, _, after) = silenceAfter plains silence mountain prodigalSorcerer piker
            ended = S.runPure S.identityAnswer after (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup))
        HU.assertEqual "nothing stored" [] (GameState.playerEffects ended)
        HU.assertBool "bob may cast again" (not (PlayerEffect.prohibitsCasting S.bob ended)),
      -- CR 806.1: in a free-for-all the players compete as individuals, so the
      -- card's your-opponents is EVERY other player, not the next seat. This is
      -- the first Silence fixture that can tell those apart.
      HU.testCase "CR 806.1 at three seats Silence stops BOTH opponents, and still spares the caster" $ do
        plains <- Registry.printing registry "Plains"
        silence <- Registry.printing registry "Silence"
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
        let (silenceId, bobsPiker, carolsPiker, before) = threeSeatSilenceBoard plains silence mountain piker
            -- Goblin Piker is a creature, so CR 302.1's timing applies: it is
            -- offered only to the ACTIVE player (Cast.sorcerySpeed). `before` is
            -- alice's own main phase (she needs no such window: Silence is an
            -- instant), so bob and carol's positive controls are checked against a
            -- copy with the activePlayer field flipped to each of them in turn --
            -- nothing else about the board changes. Directly poking activePlayer via
            -- record update to stage a hypothetical turn already appears above
            -- (thaliaBoard, ruleOfLawBoard's nextOwnTurn).
            bobsTurn = before {GameState.activePlayer = S.bob}
            carolsTurn = before {GameState.activePlayer = S.carol}
            resolved = S.runPure S.identityAnswer (S.runPure S.identityAnswer before (Cast.castSpell S.alice silenceId)) Engine.priorityLoop
            resolvedBobsTurn = resolved {GameState.activePlayer = S.bob}
            resolvedCarolsTurn = resolved {GameState.activePlayer = S.carol}
        -- The fixture really is three-seat and both opponents really could cast,
        -- given their own main phase.
        HU.assertEqual "three seats" 3 (length (GameState.turnOrder before))
        HU.assertBool "bob could cast before it resolved" (elem (Action.Type.Cast bobsPiker) (Action.legalActions S.bob bobsTurn))
        HU.assertBool "carol could cast before it resolved" (elem (Action.Type.Cast carolsPiker) (Action.legalActions S.carol carolsTurn))
        HU.assertEqual "one stored effect" 1 (length (GameState.playerEffects resolved))
        -- THE DISCRIMINATOR. carol is the far seat: an Opponents scope resolved as
        -- "the next player in turn order" prohibits bob and leaves carol free, and
        -- that is the reading the doc comments claimed was in here.
        HU.assertBool "bob is prohibited" (PlayerEffect.prohibitsCasting S.bob resolved)
        HU.assertBool "carol is prohibited too" (PlayerEffect.prohibitsCasting S.carol resolved)
        HU.assertEqual
          "and nothing is offered to either, even on their own main phase"
          []
          (filter isCast (Action.legalActions S.bob resolvedBobsTurn) <> filter isCast (Action.legalActions S.carol resolvedCarolsTurn))
        -- CR 109.5: the scope is resolved off the effect's controller.
        HU.assertBool "alice is not prohibited" (not (PlayerEffect.prohibitsCasting S.alice resolved))
    ]

-- Loaded fresh inside each case that needs it -- equivalent because loading
-- is deterministic and cached (batch-recipe.md).
matchesSpellBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
matchesSpellBoard lightningBolt piker =
  let base = Setup.emptyGame S.bothPlayers
      (bolt, withBolt) = S.spellOnStack lightningBolt S.alice base
      (pikerId, gs) = S.spellOnStack piker S.alice withBolt
   in (bolt, pikerId, gs)

-- The spell-match half of the cost-adjustment axis, now expressed as a Filter
-- over the PROJECTED view (CR 613.1d layer 4 for a card type, CR 613.1e layer 5
-- for a colour) rather than the retired SpellCriterion. A noncreature spell is
-- Filter.Not (Filter.HasCardType Creature); a coloured spell is Filter.HasColor.
matchesSpellTests :: Registry.Type.Registry -> Tasty.TestTree
matchesSpellTests registry =
  let noncreature = Filter.Type.Not (Filter.Type.HasCardType CardType.Creature)
   in Tasty.testGroup
        "matchesSpell"
        [ HU.testCase "CR 613.1d Thalia's noncreature criterion admits an instant" $ do
            lightningBolt <- Registry.printing registry "Lightning Bolt"
            piker <- Registry.printing registry "Goblin Piker"
            let (bolt, _, gs) = matchesSpellBoard lightningBolt piker
            HU.assertBool "Lightning Bolt is a noncreature spell" (PlayerEffect.matchesSpell noncreature bolt gs),
          HU.testCase "CR 613.1d a creature spell fails the noncreature criterion" $ do
            lightningBolt <- Registry.printing registry "Lightning Bolt"
            piker <- Registry.printing registry "Goblin Piker"
            let (_, pikerId, gs) = matchesSpellBoard lightningBolt piker
            HU.assertBool "Goblin Piker is a creature spell" (not (PlayerEffect.matchesSpell noncreature pikerId gs)),
          HU.testCase "CR 613.1e a colour criterion admits a matching-colour spell" $ do
            lightningBolt <- Registry.printing registry "Lightning Bolt"
            piker <- Registry.printing registry "Goblin Piker"
            let (bolt, _, gs) = matchesSpellBoard lightningBolt piker
            HU.assertBool "Lightning Bolt is red" (PlayerEffect.matchesSpell (Filter.Type.HasColor Color.Red) bolt gs),
          HU.testCase "CR 613.1e a colour criterion rejects a non-matching colour" $ do
            lightningBolt <- Registry.printing registry "Lightning Bolt"
            piker <- Registry.printing registry "Goblin Piker"
            let (bolt, _, gs) = matchesSpellBoard lightningBolt piker
            HU.assertBool "Lightning Bolt is not blue" (not (PlayerEffect.matchesSpell (Filter.Type.HasColor Color.Blue) bolt gs))
        ]

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry = Tasty.testGroup "Pawl.PlayerEffectSpec" [ruleOfLawTests registry, adjustmentTests, thaliaTests registry, medallionTests registry, humilityTests registry, edgewalkerTests registry, reliquaryTowerTests registry, storedTests registry, silenceTests registry, matchesSpellTests registry]
