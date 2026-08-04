-- Covers Pawl.Engine.PlayerEffect and Pawl.Engine.Cost, plus the types they case on
-- (Pawl.Types.PlayerEffect, PlayerScope, PlayerStaticAbility) and
-- the stored carrier Pawl.Types.ActivePlayerEffect. The spell match runs through
-- the identity-blind Pawl.Engine.Filter over a Pawl.Types.Filter. CR 613.10/613.11: the
-- continuous effects that affect PLAYERS and the RULES OF THE GAME, which sit
-- outside the CR 613 layer system entirely.
--
-- The six gate cards: Rule of Law, Thalia Guardian of Thraben, Sapphire
-- Medallion, Edgewalker, Reliquary Tower and Silence. Humility, Opalescence and
-- Titania's Song join them for CR 604.2's "and has the ability" -- the one place
-- this axis does meet the CR 613 layer system.
module Pawl.PlayerEffectSpec where

import qualified Data.List as List
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator Pawl.Engine.Filter already claims the alias Filter.

import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
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
   in (a, b, resolveAll (S.runPure S.identityAnswer board (S.cast S.alice a)))

ruleOfLawSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ruleOfLawSpec s registry =
  Spec.describe s "RuleOfLaw" $ do
    Spec.it s "before any spell is cast, both cards are castable" $ do
      plains <- S.printingOf s registry "Plains"
      ruleOfLaw <- S.printingOf s registry "Rule of Law"
      let (a, b, _, board) = ruleOfLawBoard plains ruleOfLaw
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice board)) "not prohibited"
      Spec.assertBool s (elem (Action.Type.Cast a (S.printingName ruleOfLaw)) (Action.legalActions S.alice board)) "a offered"
      Spec.assertBool s (elem (Action.Type.Cast b (S.printingName ruleOfLaw)) (Action.legalActions S.alice board)) "b offered"

    -- Ruling: "Rule of Law looks at the entire turn to see if a player has
    -- cast a spell, even if Rule of Law wasn't on the battlefield when that
    -- spell was cast. Notably, you can't cast Rule of Law and then cast
    -- another spell during the same turn." THE FALSIFIER: the spell that
    -- used up the allowance is Rule of Law itself, cast BEFORE the effect
    -- existed. Any per-effect watermark or counter fails here.
    Spec.it s "CR 601.3 casting Rule of Law itself uses up the turn's one spell" $ do
      plains <- S.printingOf s registry "Plains"
      ruleOfLaw <- S.printingOf s registry "Rule of Law"
      let (_, _, afterFirst) = ruleOfLawAfterFirst plains ruleOfLaw
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.alice afterFirst) "alice is now prohibited"
      Spec.assertEqWith
        s
        "no cast is offered at all"
        (filter isCast (Action.legalActions S.alice afterFirst))
        []

    -- The limit is counted PER PLAYER: bob has cast nothing this turn, so
    -- EachPlayer does not prohibit him.
    Spec.it s "CR 109.5 the EachPlayer scope still counts each player's own casts" $ do
      plains <- S.printingOf s registry "Plains"
      ruleOfLaw <- S.printingOf s registry "Rule of Law"
      let (_, _, afterFirst) = ruleOfLawAfterFirst plains ruleOfLaw
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.bob afterFirst)) "bob is not prohibited"

    -- Engine.handoffTurn clears the event log at the turn handoff, so
    -- "this turn" (castsThisTurn's fold over the log) is exactly the
    -- log's own extent -- CR 608.2i is the "look back in time" rule and
    -- says nothing about the log being cleared, so this is an
    -- implementation fact rather than a rules citation.
    Spec.it s "the restriction lifts on the next turn" $ do
      plains <- S.printingOf s registry "Plains"
      ruleOfLaw <- S.printingOf s registry "Rule of Law"
      let (_, b, afterFirst) = ruleOfLawAfterFirst plains ruleOfLaw
          handoff gs = S.runPure S.identityAnswer gs Engine.handoffTurn
          nextOwnTurn =
            (handoff (handoff afterFirst))
              { GameState.phase = Phase.PrecombatMain,
                GameState.priority = Just S.alice
              }
      Spec.assertEqWith s "alice is active again" (GameState.activePlayer nextOwnTurn) S.alice
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice nextOwnTurn)) "not prohibited"
      Spec.assertBool s (elem (Action.Type.Cast b (S.printingName ruleOfLaw)) (Action.legalActions S.alice nextOwnTurn)) "b offered again"

    -- Ruling: "If you cast a spell that was countered, you can't cast
    -- another spell during the same turn." The counted event is the CAST.
    Spec.it s "CR 601.2i a countered spell still counted" $ do
      plains <- S.printingOf s registry "Plains"
      ruleOfLaw <- S.printingOf s registry "Rule of Law"
      let (_, x, _, plain) = ruleOfLawBoard plains ruleOfLaw
          onBoard = snd (S.addCreature ruleOfLaw S.alice plain)
          cast = S.runPure S.identityAnswer onBoard (S.cast S.alice x)
      case GameState.stack cast of
        [] -> Spec.assertFailure s "expected the spell on the stack"
        top : _ -> do
          let countered = S.runPure S.identityAnswer cast (Event.counter S.noSource S.bob top)
          Spec.assertEqWith s "the stack is empty again" (GameState.stack countered) []
          Spec.assertBool s (PlayerEffect.prohibitsCasting S.alice countered) "still prohibited"

    -- The effect is RE-DERIVED from the battlefield on every read, so there
    -- is no stored state to unwind when its source leaves.
    Spec.it s "CR 604.2 destroying Rule of Law lifts the restriction in the same turn" $ do
      plains <- S.printingOf s registry "Plains"
      ruleOfLaw <- S.printingOf s registry "Rule of Law"
      let (_, _, z, plain) = ruleOfLawBoard plains ruleOfLaw
          (rol, onBoard) = S.addCreature ruleOfLaw S.alice plain
          castOne = S.withEvents [GameEvent.SpellCast S.alice] onBoard
          gone = S.runPure S.identityAnswer castOne (Event.destroy Regenerability.Regenerable [rol])
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.alice castOne) "prohibited while it stands"
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice gone)) "not prohibited once it is gone"
      Spec.assertBool s (elem (Action.Type.Cast z (S.printingName ruleOfLaw)) (Action.legalActions S.alice gone)) "and a cast is offered again"

    -- CR 601.3's prohibit half applies to EVERY cast, including a
    -- Panglacial Wurm cast from the library: the Panglacial permission
    -- (Cast.permitsCastWhileSearching) excepts only the timing half, not
    -- the prohibition half. Seven Forests pay the Wurm's {5}{G}{G}.
    Spec.it s "CR 601.3 Rule of Law also prohibits casting Panglacial Wurm from the library" $ do
      forest <- S.printingOf s registry "Forest"
      ruleOfLaw <- S.printingOf s registry "Rule of Law"
      panglacialWurm <- S.printingOf s registry "Panglacial Wurm"
      let base = S.landsInPlay forest 7
          (_, withRuleOfLaw) = S.addCreature ruleOfLaw S.alice base
          (_, gs) = S.addLibraryCard panglacialWurm S.alice withRuleOfLaw
          castOne = S.withEvents [GameEvent.SpellCast S.alice] gs
      -- Positive control: without it, the negative assertion below
      -- could pass merely because the Wurm was never offered at all.
      Spec.assertEqWith
        s
        "before any cast, the Wurm is offered from the library"
        (length (Cast.castableWhileSearching S.alice gs))
        1
      Spec.assertEqWith
        s
        "Rule of Law's one-spell limit blocks the library cast too"
        (Cast.castableWhileSearching S.alice castOne)
        []

isCast :: Action.Type.Action -> Bool
isCast action = case action of
  Action.Type.Cast {} -> True
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

adjustmentSpec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
adjustmentSpec s =
  Spec.describe s "Adjustments" $ do
    Spec.it s "no adjustments is the identity on a printed cost" $
      Spec.assertEqWith
        s
        "unchanged"
        (Cost.applyAdjustments ([], []) (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1, red])

    Spec.it s "CR 601.2f an increase adds generic mana" $
      Spec.assertEqWith
        s
        "{R} taxed by {1} is {1}{R}"
        (Cost.applyAdjustments ([1], []) (ManaCost.MkManaCost [red]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1, red])

    -- CR 118.7a: "Effects that reduce a cost by an amount of generic mana
    -- affect only the generic mana component of that cost. They can't affect
    -- the colored or colorless mana components."
    Spec.it s "CR 118.7a a reduction with no generic component to take is lost" $
      Spec.assertEqWith
        s
        "{U} reduced by {1} is still {U}"
        (Cost.applyAdjustments ([], [generic 1]) (ManaCost.MkManaCost [blue]))
        (ManaCost.MkManaCost [blue])

    Spec.it s "CR 118.7a a reduction takes only the generic component" $
      Spec.assertEqWith
        s
        "{2}{U} reduced by {1} is {1}{U}"
        (Cost.applyAdjustments ([], [generic 1]) (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1, blue])

    Spec.it s "CR 601.2f the total can't be reduced below {0}" $
      Spec.assertEqWith
        s
        "{1} reduced by {3} is {0}"
        (Cost.applyAdjustments ([], [generic 3]) (ManaCost.MkManaCost [ManaSymbol.Generic 1]))
        (ManaCost.MkManaCost [])

    -- THE ORDER TEST, in the small. Increase first gives {1}{U}, which the
    -- reduction takes back to {U}. Reduce first loses the reduction to CR
    -- 118.7a's empty generic component, and the increase then leaves {1}{U}.
    Spec.it s "CR 601.2f every increase applies before any reduction" $
      Spec.assertEqWith
        s
        "{U} +{1} -{1} is {U}"
        (Cost.applyAdjustments ([1], [generic 1]) (ManaCost.MkManaCost [blue]))
        (ManaCost.MkManaCost [blue])

    -- CR 118.7's typed half, which CR 118.7a's generic half above cannot
    -- reach: a reduction that NAMES a mana type takes that type's symbols.
    Spec.it s "CR 118.7 a typed reduction takes the cost's matching symbols" $
      Spec.assertEqWith
        s
        "{1}{W}{B} reduced by {W}{B} is {1}"
        (Cost.applyAdjustments ([], [ManaCost.MkManaCost [white, black]]) (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1])

    Spec.it s "CR 118.7 one reducing symbol takes exactly one matching symbol" $
      Spec.assertEqWith
        s
        "{W}{W} reduced by {W} is {W}"
        (Cost.applyAdjustments ([], [ManaCost.MkManaCost [white]]) (ManaCost.MkManaCost [white, white]))
        (ManaCost.MkManaCost [white])

    -- THE HEADLINE FALSIFIER for the typed half, and the one place pawl
    -- deliberately does not do what CR 118.7b-d would (#309). Edgewalker's own
    -- reminder text is the assertion: "if you cast a Cleric spell with mana
    -- cost {1}{W}, it costs {1} to cast" -- so the {B} half, which the cost
    -- cannot satisfy, takes NOTHING rather than one generic mana.
    Spec.it s "an excess typed reduction is dropped, not spilled onto generic" $
      Spec.assertEqWith
        s
        "{1}{W} reduced by {W}{B} is {1}"
        (Cost.applyAdjustments ([], [ManaCost.MkManaCost [white, black]]) (ManaCost.MkManaCost [ManaSymbol.Generic 1, white]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1])

    -- Ruling: "If you have more than one of these on the battlefield, the cost
    -- reduction is cumulative." Cumulative, and still bounded by what the cost
    -- actually has to give.
    Spec.it s "two typed reductions pool, and the second finds nothing left to take" $
      Spec.assertEqWith
        s
        "{1}{W}{B} reduced by {W}{B} twice is {1}"
        (Cost.applyAdjustments ([], [ManaCost.MkManaCost [white, black], ManaCost.MkManaCost [white, black]]) (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1])

    -- The two halves of ONE reduction, on the two halves of one cost: CR
    -- 118.7a routes the {1} to the generic component and the {U} takes the
    -- blue symbol, and neither reaches into the other's component.
    Spec.it s "CR 118.7a a mixed reduction splits by component" $
      Spec.assertEqWith
        s
        "{2}{U} reduced by {1}{U} is {1}"
        (Cost.applyAdjustments ([], [ManaCost.MkManaCost [ManaSymbol.Generic 1, blue]]) (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1])

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
thaliaSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
thaliaSpec s registry =
  Spec.describe s "Thalia" $ do
    Spec.it s "CR 601.2f a noncreature spell's total cost is one more" $ do
      mountain <- S.printingOf s registry "Mountain"
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (bolt, _, gs) = thaliaBoard mountain thalia lightningBolt piker 3
      Spec.assertEqWith
        s
        "{R} becomes {1}{R}"
        (totalManaCost S.alice bolt (ManaCost.MkManaCost [red]) gs)
        (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]))

    -- Ruling: "Thalia's ability affects each spell that's not a creature
    -- spell, including your own." The Filter reads the PROJECTION.
    Spec.it s "a creature spell fails the effect's criterion, so it is unaffected" $ do
      mountain <- S.printingOf s registry "Mountain"
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, pikerId, gs) = thaliaBoard mountain thalia lightningBolt piker 3
      Spec.assertEqWith
        s
        "{1}{R} stays {1}{R}"
        (totalManaCost S.alice pikerId (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]) gs)
        (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]))

    -- BOTH sites, one scenario. Taxing castability but not payment lets
    -- the player underpay; taxing payment but not castability offers a
    -- cast that cannot be afforded, and there is no mid-announcement
    -- rewind (#56) -- that is a wedged game, not a rejected action.
    Spec.it s "CR 601.2f castability is measured against the total cost" $ do
      mountain <- S.printingOf s registry "Mountain"
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (boltOne, _, oneLand) = thaliaBoard mountain thalia lightningBolt piker 1
          (boltTwo, _, twoLands) = thaliaBoard mountain thalia lightningBolt piker 2
          (_, pikerTwo, twoLandsAgain) = thaliaBoard mountain thalia lightningBolt piker 2
      Spec.assertBool s (not (S.castable S.alice boltOne oneLand)) "one Mountain is not enough for a taxed Bolt"
      Spec.assertBool s (S.castable S.alice boltTwo twoLands) "two Mountains are"
      Spec.assertBool s (S.castable S.alice pikerTwo twoLandsAgain) "and an untaxed creature spell needs only its printed two"

    Spec.it s "CR 601.2f payment spends the total cost" $ do
      mountain <- S.printingOf s registry "Mountain"
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (bolt, _, gs) = thaliaBoard mountain thalia lightningBolt piker 3
          paid = S.runPure S.identityAnswer gs (S.cast S.alice bolt)
          (boltU, gsU) = thaliaUntaxed mountain lightningBolt 3
          paidU = S.runPure S.identityAnswer gsU (S.cast S.alice boltU)
      Spec.assertEqWith s "taxed: two lands tapped" (S.tappedCount S.alice paid) 2
      Spec.assertEqWith s "untaxed: one land tapped" (S.tappedCount S.alice paidU) 1

    -- The EachPlayer scope: Thalia's controller is taxed (every assertion
    -- above is alice's own spell) and so is her opponent.
    Spec.it s "CR 611.1 the opponent is taxed too" $ do
      mountain <- S.printingOf s registry "Mountain"
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, _, gs) = thaliaBoard mountain thalia lightningBolt piker 3
          (bobBolt, withBob) = S.addHandCard lightningBolt S.bob gs
      Spec.assertEqWith
        s
        "bob's {R} is also {1}{R}"
        (totalManaCost S.bob bobBolt (ManaCost.MkManaCost [red]) withBob)
        (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]))

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
    Spec.it s "CR 601.2f a library-cast creature spell is unaffected by Thalia's noncreature tax" $ do
      forest <- S.printingOf s registry "Forest"
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      panglacialWurm <- S.printingOf s registry "Panglacial Wurm"
      ruleOfLaw <- S.printingOf s registry "Rule of Law"
      let green = ManaSymbol.OfType (ManaType.Colored Color.Green)
          base = S.landsInPlay forest 7
          (_, withThalia) = S.addCreature thalia S.alice base
          (wurm, withWurm) = S.addLibraryCard panglacialWurm S.alice withThalia
          (rol, gs) = S.addHandCard ruleOfLaw S.alice withWurm
      Spec.assertEqWith
        s
        "positive control: Rule of Law, a noncreature spell, IS taxed here"
        (totalManaCost S.alice rol (ManaCost.MkManaCost [ManaSymbol.Generic 2, white]) gs)
        (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3, white]))
      Spec.assertEqWith
        s
        "the Wurm's total cost is untouched"
        (totalManaCost S.alice wurm (ManaCost.MkManaCost [ManaSymbol.Generic 5, green, green]) gs)
        (Just (ManaCost.MkManaCost [ManaSymbol.Generic 5, green, green]))
      Spec.assertEqWith
        s
        "exactly seven Forests still afford it, so castableWhileSearching offers it"
        (Cast.castableWhileSearching S.alice gs)
        [wurm]

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
medallionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
medallionSpec s registry =
  Spec.describe s "SapphireMedallion" $ do
    -- Ruling: "The ability can't reduce the amount of colored mana you pay
    -- for a spell. It reduces only the generic mana component of that
    -- cost." THE HEADLINE FALSIFIER: subtracting from the mana value would
    -- make this spell free.
    Spec.it s "CR 118.7a a {U} spell still costs {U}" $ do
      island <- S.printingOf s registry "Island"
      sapphireMedallion <- S.printingOf s registry "Sapphire Medallion"
      unsummonPrinting <- S.printingOf s registry "Unsummon"
      divinationPrinting <- S.printingOf s registry "Divination"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let (unsummon, _, _, gs) = medallionBoard island sapphireMedallion unsummonPrinting divinationPrinting lightningBolt 2
      Spec.assertEqWith
        s
        "unchanged"
        (totalManaCost S.alice unsummon (ManaCost.MkManaCost [blue]) gs)
        (Just (ManaCost.MkManaCost [blue]))

    Spec.it s "CR 118.7a a {2}{U} spell costs {1}{U}" $ do
      island <- S.printingOf s registry "Island"
      sapphireMedallion <- S.printingOf s registry "Sapphire Medallion"
      unsummonPrinting <- S.printingOf s registry "Unsummon"
      divinationPrinting <- S.printingOf s registry "Divination"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let (_, divination, _, gs) = medallionBoard island sapphireMedallion unsummonPrinting divinationPrinting lightningBolt 2
      Spec.assertEqWith
        s
        "one generic off"
        (totalManaCost S.alice divination (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue]) gs)
        (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, blue]))

    Spec.it s "a red spell fails the effect's colour criterion, so it is unaffected" $ do
      island <- S.printingOf s registry "Island"
      sapphireMedallion <- S.printingOf s registry "Sapphire Medallion"
      unsummonPrinting <- S.printingOf s registry "Unsummon"
      divinationPrinting <- S.printingOf s registry "Divination"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let (_, _, bolt, gs) = medallionBoard island sapphireMedallion unsummonPrinting divinationPrinting lightningBolt 2
      Spec.assertEqWith
        s
        "unchanged"
        (totalManaCost S.alice bolt (ManaCost.MkManaCost [red]) gs)
        (Just (ManaCost.MkManaCost [red]))

    -- Divination is {2}{U}: three mana printed, two after the discount. Two
    -- Islands is exactly the amount that tells the two apart.
    Spec.it s "CR 601.2f the discount is observable at the castability gate" $ do
      island <- S.printingOf s registry "Island"
      sapphireMedallion <- S.printingOf s registry "Sapphire Medallion"
      unsummonPrinting <- S.printingOf s registry "Unsummon"
      divinationPrinting <- S.printingOf s registry "Divination"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
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
      Spec.assertBool s (S.castable S.alice divination withMedallion) "castable for {1}{U} with two Islands"
      Spec.assertBool s (not (S.castable S.alice bareDivination bare)) "and not castable for {2}{U} without the Medallion"

    -- CR 611.1 / 109.5: the You scope is the effect's controller.
    Spec.it s "CR 109.5 the You scope does not discount an opponent's spell" $ do
      island <- S.printingOf s registry "Island"
      sapphireMedallion <- S.printingOf s registry "Sapphire Medallion"
      unsummonPrinting <- S.printingOf s registry "Unsummon"
      divinationPrinting <- S.printingOf s registry "Divination"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let (_, _, _, gs) = medallionBoard island sapphireMedallion unsummonPrinting divinationPrinting lightningBolt 2
          (bobDivination, withBob) = S.addHandCard divinationPrinting S.bob gs
      Spec.assertEqWith
        s
        "bob pays full price"
        (totalManaCost S.bob bobDivination (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue]) withBob)
        (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue]))

    -- Ruling: "If there are additional costs to cast a spell, or if the
    -- cost to cast a spell is increased by an effect (such as the one
    -- created by Thalia, Guardian of Thraben's ability), apply those
    -- increases before applying cost reductions." THE ORDER TEST, and it
    -- names a cost with NO generic component on purpose: the two orders
    -- agree wherever the CR 601.2f floor does not bind.
    Spec.it s "CR 601.2f Thalia then the Medallion leaves a {U} spell at exactly {U}" $ do
      island <- S.printingOf s registry "Island"
      sapphireMedallion <- S.printingOf s registry "Sapphire Medallion"
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      unsummonPrinting <- S.printingOf s registry "Unsummon"
      let (unsummon, gs) = medallionBothBoard island sapphireMedallion thalia unsummonPrinting
      Spec.assertEqWith
        s
        "increase first, then reduce"
        (totalManaCost S.alice unsummon (ManaCost.MkManaCost [blue]) gs)
        (Just (ManaCost.MkManaCost [blue]))
      Spec.assertBool s (S.castable S.alice unsummon gs) "so one Island is enough"

-- Humility {2}{W}{W} Enchantment: "All creatures lose all abilities and have
-- base power and toughness 1/1." CR 604.2: a static ability's continuous effect
-- is active only "as long as the permanent with the ability remains on the
-- battlefield AND HAS THE ABILITY", so a CR 613.1f layer-6 removal takes the
-- player-affecting half of a card's text with it -- the axis Pawl.Engine.Projection
-- already gates for the projected characteristics (abilitiesRemoved).
--
-- CR 613.6's rescue ("if an effect starts to apply in one layer ... it will
-- continue to be applied ... even if the ability generating the effect is
-- removed") cannot reach a player ability: CR 613.10/613.11 apply these effects
-- AFTER the seven layers have run, so one never starts to apply before layer 6
-- and the cut is unconditional. Same shape as the layer-7-only static ability
-- gatherStatic drops.
humilitySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
humilitySpec s registry =
  Spec.describe s "Humility" $ do
    -- THE PROVING CASE. Thalia is a creature, so Humility reaches her with no
    -- animator in the way, and her tax is the only thing standing between the
    -- Bolt and its printed cost.
    Spec.it s "CR 604.2 Humility takes Thalia's ability, so her tax stops applying" $ do
      mountain <- S.printingOf s registry "Mountain"
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      humility <- S.printingOf s registry "Humility"
      let (bolt, _, taxed) = thaliaBoard mountain thalia lightningBolt piker 3
          humbled = S.withHumility humility taxed
      Spec.assertEqWith
        s
        "control: with Thalia's ability intact, {R} is {1}{R}"
        (totalManaCost S.alice bolt (ManaCost.MkManaCost [red]) taxed)
        (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]))
      Spec.assertEqWith
        s
        "under Humility the printed {R} is the whole cost"
        (totalManaCost S.alice bolt (ManaCost.MkManaCost [red]) humbled)
        (Just (ManaCost.MkManaCost [red]))

    -- The same statement at the two gameplay sites the Thalia group tests,
    -- with ONE Mountain -- the amount that tells the taxed and untaxed costs
    -- apart.
    Spec.it s "CR 601.2f castability and payment both drop the stripped tax" $ do
      mountain <- S.printingOf s registry "Mountain"
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      humility <- S.printingOf s registry "Humility"
      let (bolt, _, oneLand) = thaliaBoard mountain thalia lightningBolt piker 1
          humbled = S.withHumility humility oneLand
          paid = S.runPure S.identityAnswer humbled (S.cast S.alice bolt)
      Spec.assertBool s (not (S.castable S.alice bolt oneLand)) "control: one Mountain cannot pay the taxed Bolt"
      Spec.assertBool s (S.castable S.alice bolt humbled) "under Humility one Mountain is enough"
      Spec.assertEqWith s "and paying it taps exactly that one" (S.tappedCount S.alice paid) 1

    -- THE DISCRIMINATOR against "Humility silences every player ability".
    -- Humility's affected set is "each creature", and Sapphire Medallion is an
    -- artifact -- nothing animates it here, so its discount is untouched.
    Spec.it s "CR 613.1f Humility reaches only creatures, so an artifact's ability stands" $ do
      island <- S.printingOf s registry "Island"
      sapphireMedallion <- S.printingOf s registry "Sapphire Medallion"
      unsummonPrinting <- S.printingOf s registry "Unsummon"
      divinationPrinting <- S.printingOf s registry "Divination"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      humility <- S.printingOf s registry "Humility"
      let (_, divination, _, gs) = medallionBoard island sapphireMedallion unsummonPrinting divinationPrinting lightningBolt 2
          humbled = S.withHumility humility gs
      Spec.assertEqWith
        s
        "the Medallion still discounts {2}{U} to {1}{U}"
        (totalManaCost S.alice divination (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue]) humbled)
        (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, blue]))

    -- The animated case: Rule of Law is an enchantment, so Humility alone
    -- leaves it alone. Opalescence's CR 613.1d layer-4 AddCardType is what
    -- brings it inside "each creature" -- and abilitiesRemoved judges the
    -- affected set at CR 613.6's decision point, which for Humility is layer 6,
    -- so the partial it reads already has that animation. Opalescence itself is
    -- spared by its own "each other enchantment", so it keeps animating.
    Spec.it s "CR 613.1d Opalescence animates Rule of Law into Humility's set" $ do
      ruleOfLaw <- S.printingOf s registry "Rule of Law"
      humility <- S.printingOf s registry "Humility"
      opalescence <- S.printingOf s registry "Opalescence"
      let base = Setup.emptyGame S.bothPlayers
          (_, withRuleOfLaw) = S.addCreature ruleOfLaw S.alice base
          withHumility = S.withHumility humility withRuleOfLaw
          (_, withOpalescence) = S.addCreature opalescence S.alice withHumility
          castOne = S.withEvents [GameEvent.SpellCast S.alice]
      Spec.assertBool
        s
        (PlayerEffect.prohibitsCasting S.alice (castOne withHumility))
        "control: Humility alone does not reach an enchantment"
      Spec.assertBool
        s
        (not (PlayerEffect.prohibitsCasting S.alice (castOne withOpalescence)))
        "once animated, Rule of Law loses the ability and the limit lifts"

-- alice controls a Sapphire Medallion and two untapped Islands, with Divination
-- ({2}{U}) in hand; the fourth component is the same board with bob's Titania's
-- Song added, so a case can assert against both. Loaded fresh inside each case
-- that needs it -- equivalent because loading is deterministic and cached
-- (batch-recipe.md).
titaniasSongBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, GameState.GameState)
titaniasSongBoard island sapphireMedallion divinationPrinting titaniasSong =
  let base = S.landsInPlay island 2
      (medallion, gs1) = S.addCreature sapphireMedallion S.alice base
      (divination, gs2) = S.addHandCard divinationPrinting S.alice gs1
      bare =
        gs2
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      (_, sung) = S.addCreature titaniasSong S.bob bare
   in (medallion, divination, bare, sung)

-- Titania's Song {3}{G} Enchantment: "Each noncreature artifact loses all
-- abilities and becomes an artifact creature with power and toughness each equal
-- to its mana value."
--
-- The card CR 613.6's LOWEST-LAYER reading exists for (#326), and the only one in
-- the pool: its one static ability pairs an ability-removing part (CR 613.1f,
-- layer 6) with a type-changing one (CR 613.1d, layer 4), and its affected set
-- reads the very card type its layer-4 part writes. CR 613.6 -- "if an effect
-- starts to apply in one layer and/or sublayer, it will continue to be applied to
-- the same set of objects in each other applicable layer and/or sublayer" -- fixes
-- that set at LAYER 4, where a Sapphire Medallion is still a noncreature artifact.
-- Judged at layer 6 instead, the Medallion has already been animated by the
-- Song's own layer-4 part and no longer matches "noncreature artifact", so the
-- removal would miss it. Humility cannot state this: layer 6 is its lowest layer,
-- so the two readings agree on it by coincidence.
titaniasSongSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
titaniasSongSpec s registry =
  Spec.describe s "TitaniasSong" $ do
    -- THE PROVING CASE for #326. The middle assertion is what makes the last one
    -- discriminate: the layer fold itself puts the Medallion inside the Song's
    -- set (it is a 2/2 for its mana value), so CR 613.6 leaves CR 604.2's "and
    -- has the ability" no room to disagree -- the discount has to be gone.
    Spec.it s "CR 613.6 the Song's set is fixed at layer 4, so the Medallion it animates loses its discount" $ do
      island <- S.printingOf s registry "Island"
      sapphireMedallion <- S.printingOf s registry "Sapphire Medallion"
      divinationPrinting <- S.printingOf s registry "Divination"
      titaniasSong <- S.printingOf s registry "Titania's Song"
      let (medallion, divination, bare, sung) = titaniasSongBoard island sapphireMedallion divinationPrinting titaniasSong
          printed = ManaCost.MkManaCost [ManaSymbol.Generic 2, blue]
      Spec.assertEqWith
        s
        "control: on its own the Medallion discounts {2}{U} to {1}{U}"
        (totalManaCost S.alice divination printed bare)
        (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, blue]))
      Spec.assertEqWith
        s
        "CR 613.1d / 613.4b: the Song does animate the Medallion, to its mana value"
        (S.powerToughnessOf medallion sung)
        (Just (2, 2))
      Spec.assertEqWith
        s
        "CR 604.2: and having lost the ability, it discounts nothing"
        (totalManaCost S.alice divination printed sung)
        (Just printed)

    -- THE DISCRIMINATOR against "Titania's Song silences every player ability".
    -- Its set is "each noncreature artifact"; Thalia is a creature, so her tax
    -- stands -- the mirror of the Humility group's Sapphire Medallion case.
    Spec.it s "CR 613.1f the Song reaches only noncreature artifacts, so a creature's ability stands" $ do
      mountain <- S.printingOf s registry "Mountain"
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      titaniasSong <- S.printingOf s registry "Titania's Song"
      let (bolt, _, taxed) = thaliaBoard mountain thalia lightningBolt piker 3
          (_, sung) = S.addCreature titaniasSong S.bob taxed
      Spec.assertEqWith
        s
        "the printed {R} is still taxed to {1}{R}"
        (totalManaCost S.alice bolt (ManaCost.MkManaCost [red]) sung)
        (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]))

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
edgewalkerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
edgewalkerSpec s registry =
  Spec.describe s "Edgewalker" $ do
    Spec.it s "CR 118.7 a Cleric spell loses one white and one black symbol" $ do
      plains <- S.printingOf s registry "Plains"
      edgewalker <- S.printingOf s registry "Edgewalker"
      piker <- S.printingOf s registry "Goblin Piker"
      let (spell, _, gs) = edgewalkerBoard plains edgewalker piker 1 3
      Spec.assertEqWith
        s
        "{1}{W}{B} becomes {1}"
        (totalManaCost S.alice spell (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black]) gs)
        (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1]))

    Spec.it s "a spell with no Cleric subtype fails the effect's criterion, so it is unaffected" $ do
      plains <- S.printingOf s registry "Plains"
      edgewalker <- S.printingOf s registry "Edgewalker"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, pikerId, gs) = edgewalkerBoard plains edgewalker piker 1 3
      Spec.assertEqWith
        s
        "{1}{R} stays {1}{R}"
        (totalManaCost S.alice pikerId (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]) gs)
        (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]))

    -- THE HEADLINE FALSIFIER, at the board. Ruling: "If you have more than one
    -- of these on the battlefield, the cost reduction is cumulative" -- so two
    -- Edgewalkers really do offer {W}{B}{W}{B}. The cost has one white and one
    -- black to give, and the second pair strands: under CR 118.7b-d it would
    -- go on to eat the {1} and leave the spell free, and Edgewalker's card
    -- text stops it (#309).
    Spec.it s "a second Edgewalker's stranded halves leave the generic component alone" $ do
      plains <- S.printingOf s registry "Plains"
      edgewalker <- S.printingOf s registry "Edgewalker"
      piker <- S.printingOf s registry "Goblin Piker"
      let (spell, _, gs) = edgewalkerBoard plains edgewalker piker 2 3
      Spec.assertEqWith
        s
        "{1}{W}{B} is still {1}, not {0}"
        (totalManaCost S.alice spell (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black]) gs)
        (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1]))

    -- BOTH cost sites, one scenario, exactly as the Thalia group tests them.
    -- Three Plains produce white mana and nothing else, so they can never pay
    -- a {B}: what makes the discounted spell castable is that the reduction
    -- removed the black SYMBOL, not that it removed an amount.
    Spec.it s "CR 601.2f castability is measured against the total cost" $ do
      plains <- S.printingOf s registry "Plains"
      edgewalker <- S.printingOf s registry "Edgewalker"
      piker <- S.printingOf s registry "Goblin Piker"
      let (discounted, _, withEdgewalker) = edgewalkerBoard plains edgewalker piker 1 3
          (undiscounted, _, bare) = edgewalkerBoard plains edgewalker piker 0 3
      Spec.assertBool s (not (S.castable S.alice undiscounted bare)) "three Plains cannot pay a printed {1}{W}{B}"
      Spec.assertBool s (S.castable S.alice discounted withEdgewalker) "but they can pay the discounted {1}"

    Spec.it s "CR 601.2f payment spends the total cost" $ do
      plains <- S.printingOf s registry "Plains"
      edgewalker <- S.printingOf s registry "Edgewalker"
      piker <- S.printingOf s registry "Goblin Piker"
      let (spell, _, gs) = edgewalkerBoard plains edgewalker piker 1 3
          paid = S.runPure S.identityAnswer gs (S.cast S.alice spell)
      Spec.assertEqWith s "one Plains tapped, not three" (S.tappedCount S.alice paid) 1

    -- CR 611.1 / 109.5: the You scope is the effect's controller, and bob
    -- controls no Edgewalker.
    Spec.it s "CR 109.5 the You scope does not discount an opponent's Cleric spell" $ do
      plains <- S.printingOf s registry "Plains"
      edgewalker <- S.printingOf s registry "Edgewalker"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, _, gs) = edgewalkerBoard plains edgewalker piker 1 3
          (bobEdgewalker, withBob) = S.addHandCard edgewalker S.bob gs
      Spec.assertEqWith
        s
        "bob pays full price"
        (totalManaCost S.bob bobEdgewalker (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black]) withBob)
        (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black]))

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
reliquaryTowerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
reliquaryTowerSpec s registry =
  Spec.describe s "ReliquaryTower" $ do
    Spec.it s "CR 402.2 the maximum hand size is normally seven" $ do
      plains <- S.printingOf s registry "Plains"
      Spec.assertEqWith s "seven" (PlayerEffect.maximumHandSize S.alice (reliquaryHandOfNine plains [])) (Just 7)

    Spec.it s "CR 514.1 nine cards at cleanup discards down to seven" $ do
      plains <- S.printingOf s registry "Plains"
      let after = reliquaryCleanup (reliquaryHandOfNine plains [])
      Spec.assertEqWith s "hand" (S.handSize S.alice after) 7
      Spec.assertEqWith s "two discarded" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 2

    Spec.it s "CR 402.2 Reliquary Tower removes the maximum entirely" $ do
      plains <- S.printingOf s registry "Plains"
      reliquaryTower <- S.printingOf s registry "Reliquary Tower"
      Spec.assertEqWith
        s
        "no maximum"
        (PlayerEffect.maximumHandSize S.alice (reliquaryHandOfNine plains [reliquaryTower]))
        Nothing

    Spec.it s "CR 514.1 with Reliquary Tower nothing is discarded and nothing is asked" $ do
      plains <- S.printingOf s registry "Plains"
      reliquaryTower <- S.printingOf s registry "Reliquary Tower"
      let after = reliquaryCleanup (reliquaryHandOfNine plains [reliquaryTower])
      Spec.assertEqWith s "hand keeps nine" (S.handSize S.alice after) 9
      Spec.assertEqWith s "nothing discarded" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0

    -- CR 109.5: the You scope. bob does not share alice's Tower.
    Spec.it s "CR 109.5 the opponent still has a maximum hand size" $ do
      plains <- S.printingOf s registry "Plains"
      reliquaryTower <- S.printingOf s registry "Reliquary Tower"
      Spec.assertEqWith
        s
        "seven"
        (PlayerEffect.maximumHandSize S.bob (reliquaryHandOfNine plains [reliquaryTower]))
        (Just 7)

    -- CR 305.7: a land whose subtype is SET to a basic type loses its
    -- rules-text abilities. Reliquary Tower is nonbasic, and Blood Moon is
    -- in the pool -- so this axis composes with the layer system without
    -- being part of it.
    Spec.it s "CR 305.7 Blood Moon strips the ability off the Tower" $ do
      plains <- S.printingOf s registry "Plains"
      reliquaryTower <- S.printingOf s registry "Reliquary Tower"
      bloodMoon <- S.printingOf s registry "Blood Moon"
      let board = reliquaryHandOfNine plains [reliquaryTower, bloodMoon]
      Spec.assertEqWith s "seven again" (PlayerEffect.maximumHandSize S.alice board) (Just 7)

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
storedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
storedSpec s registry =
  Spec.describe s "Stored" $ do
    let base = Setup.emptyGame S.bothPlayers
        silenced =
          S.addPlayerEffect
            Expiry.Type.AtCleanup
            PlayerScope.Opponents
            PlayerEffect.Type.CantCastSpells
            S.alice
            base

    Spec.it s "CR 611.1 a stored effect applies through its scope" $
      do
        Spec.assertBool s (PlayerEffect.prohibitsCasting S.bob silenced) "bob is prohibited"
        Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice silenced)) "alice is not"

    Spec.it s "CR 514.2 the cleanup sweep drops an AtCleanup player effect" $
      let after = Expiry.dropAtCleanup silenced
       in do
            Spec.assertEqWith s "one stored before" (length (GameState.playerEffects silenced)) 1
            Spec.assertEqWith s "none after" (GameState.playerEffects after) []
            Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.bob after)) "and bob may cast again"

    Spec.it s "CR 514.2 the cleanup sweep keeps a Never player effect" $
      let forever = S.addPlayerEffect Expiry.Type.Never PlayerScope.Opponents PlayerEffect.Type.CantCastSpells S.alice base
       in Spec.assertEqWith s "survives" (length (GameState.playerEffects (Expiry.dropAtCleanup forever))) 1

    -- THE DISCRIMINATING SHAPE: two entries, keyed to the two different
    -- players, on the SAME handoff. An indiscriminate sweep (one that
    -- dropped every stored player effect) would pass the old
    -- single-entry version of this test; here it would wrongly drop
    -- bob's still-live entry too.
    Spec.it s "CR 611.2a the handoff sweep drops only the entry keyed to the player whose turn began" $
      let forBob = S.addPlayerEffect (Expiry.Type.AtTurnOf S.bob) PlayerScope.Opponents PlayerEffect.Type.CantCastSpells S.alice base
          armed = S.addPlayerEffect (Expiry.Type.AtTurnOf S.alice) PlayerScope.Opponents PlayerEffect.Type.CantCastSpells S.alice forBob
          bobsTurn = S.runPure S.identityAnswer armed Engine.handoffTurn
       in do
            Spec.assertEqWith s "bob is active" (GameState.activePlayer bobsTurn) S.bob
            Spec.assertEqWith
              s
              "the bob-keyed entry ended; the alice-keyed entry survives"
              (fmap ActivePlayerEffect.expiry (GameState.playerEffects bobsTurn))
              [Expiry.Type.AtTurnOf S.alice]

    -- The POSITIVE case: while the condition genuinely holds, the sweep
    -- must leave the effect in place and report that nothing changed.
    -- Without this, "deletes on failure" is indistinguishable from
    -- "empties the list unconditionally".
    Spec.it s "CR 611.2b the conditional sweep keeps a player effect whose condition still holds" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, conditional) = storedConditional piker
          (changed, swept) = Engine.runGamePure S.identityAnswer conditional Expiry.sweepConditional
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.bob conditional) "still prohibited while the source stands"
      Spec.assertBool s (not changed) "the sweep reports no change"
      Spec.assertEqWith s "still stored" (length (GameState.playerEffects swept)) 1
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.bob swept) "still prohibited after a no-op sweep"

    Spec.it s "CR 611.2b the conditional sweep deletes a player effect whose condition has failed" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (srcId, conditional) = storedConditional piker
          gone = S.runPure S.identityAnswer conditional (Event.destroy Regenerability.Regenerable [srcId])
          (changed, swept) = Engine.runGamePure S.identityAnswer gone Expiry.sweepConditional
      Spec.assertBool s changed "the sweep reports a change"
      Spec.assertEqWith s "deleted, not masked" (GameState.playerEffects swept) []
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.bob swept)) "no longer prohibited"

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
      after = resolveAll (S.runPure S.identityAnswer before (S.cast S.alice silenceId))
   in (silenceId, silence2Id, pikerId, landId, before, after)

isSilenceActivate :: Action.Type.Action -> Bool
isSilenceActivate action = case action of
  Action.Type.Activate _ _ -> True
  Action.Type.Cast {} -> False
  Action.Type.Play _ -> False
  Action.Type.Pass -> False

-- Silence {W} Instant: "Your opponents can't cast spells this turn."
silenceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
silenceSpec s registry =
  Spec.describe s "Silence" $ do
    Spec.it s "before Silence resolves, bob may cast his creature" $ do
      plains <- S.printingOf s registry "Plains"
      silence <- S.printingOf s registry "Silence"
      mountain <- S.printingOf s registry "Mountain"
      prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, _, pikerId, _, before, _) = silenceAfter plains silence mountain prodigalSorcerer piker
      Spec.assertBool s (elem (Action.Type.Cast pikerId (S.printingName piker)) (Action.legalActions S.bob before)) "offered"

    -- CR 611.2c, THE FALSIFIER: nothing bob owns is a spell when Silence
    -- resolves -- the stack holds only Silence itself. Freeze the affected
    -- set and this card does literally nothing.
    Spec.it s "CR 611.2c the effect reaches a spell that did not exist when it began" $ do
      plains <- S.printingOf s registry "Plains"
      silence <- S.printingOf s registry "Silence"
      mountain <- S.printingOf s registry "Mountain"
      prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, _, _, _, _, after) = silenceAfter plains silence mountain prodigalSorcerer piker
      Spec.assertEqWith s "one stored effect" (length (GameState.playerEffects after)) 1
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.bob after) "bob is prohibited"
      Spec.assertEqWith
        s
        "and no cast is offered"
        (filter isCast (Action.legalActions S.bob after))
        []

    -- CR 109.5: "your opponents" is scoped off Silence's controller, which
    -- is baked into the stored effect because its source is in a graveyard.
    Spec.it s "CR 109.5 the Opponents scope spares the caster" $ do
      plains <- S.printingOf s registry "Plains"
      silence <- S.printingOf s registry "Silence"
      mountain <- S.printingOf s registry "Mountain"
      prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, silence2Id, _, _, _, after) = silenceAfter plains silence mountain prodigalSorcerer piker
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice after)) "alice is not prohibited"
      Spec.assertBool s (S.castable S.alice silence2Id after) "and may cast her second Silence"

    -- Ruling: "The only thing Silence stops is casting spells. Your
    -- opponents can still activate abilities ... they can still play lands,
    -- and so on."
    Spec.it s "CR 601.3 only casting is stopped" $ do
      plains <- S.printingOf s registry "Plains"
      silence <- S.printingOf s registry "Silence"
      mountain <- S.printingOf s registry "Mountain"
      prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, _, _, landId, _, after) = silenceAfter plains silence mountain prodigalSorcerer piker
      Spec.assertBool s (elem (Action.Type.Play landId) (Action.legalActions S.bob after)) "bob may still play a land"
      Spec.assertBool s (any isSilenceActivate (Action.legalActions S.bob after)) "and still activate an ability"

    Spec.it s "CR 514.2 the prohibition ends at cleanup" $ do
      plains <- S.printingOf s registry "Plains"
      silence <- S.printingOf s registry "Silence"
      mountain <- S.printingOf s registry "Mountain"
      prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, _, _, _, _, after) = silenceAfter plains silence mountain prodigalSorcerer piker
          ended = S.runPure S.identityAnswer after (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup))
      Spec.assertEqWith s "nothing stored" (GameState.playerEffects ended) []
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.bob ended)) "bob may cast again"

    -- CR 806.1: in a free-for-all the players compete as individuals, so the
    -- card's your-opponents is EVERY other player, not the next seat. This is
    -- the first Silence fixture that can tell those apart.
    Spec.it s "CR 806.1 at three seats Silence stops BOTH opponents, and still spares the caster" $ do
      plains <- S.printingOf s registry "Plains"
      silence <- S.printingOf s registry "Silence"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
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
          resolved = S.runPure S.identityAnswer (S.runPure S.identityAnswer before (S.cast S.alice silenceId)) Engine.priorityLoop
          resolvedBobsTurn = resolved {GameState.activePlayer = S.bob}
          resolvedCarolsTurn = resolved {GameState.activePlayer = S.carol}
      -- The fixture really is three-seat and both opponents really could cast,
      -- given their own main phase.
      Spec.assertEqWith s "three seats" (length (GameState.turnOrder before)) 3
      Spec.assertBool s (elem (Action.Type.Cast bobsPiker (S.printingName piker)) (Action.legalActions S.bob bobsTurn)) "bob could cast before it resolved"
      Spec.assertBool s (elem (Action.Type.Cast carolsPiker (S.printingName piker)) (Action.legalActions S.carol carolsTurn)) "carol could cast before it resolved"
      Spec.assertEqWith s "one stored effect" (length (GameState.playerEffects resolved)) 1
      -- THE DISCRIMINATOR. carol is the far seat: an Opponents scope resolved as
      -- "the next player in turn order" prohibits bob and leaves carol free, and
      -- that is the reading the doc comments claimed was in here.
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.bob resolved) "bob is prohibited"
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.carol resolved) "carol is prohibited too"
      Spec.assertEqWith
        s
        "and nothing is offered to either, even on their own main phase"
        (filter isCast (Action.legalActions S.bob resolvedBobsTurn) <> filter isCast (Action.legalActions S.carol resolvedCarolsTurn))
        []
      -- CR 109.5: the scope is resolved off the effect's controller.
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice resolved)) "alice is not prohibited"

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
matchesSpellSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
matchesSpellSpec s registry =
  Spec.describe s "matchesSpell" $ do
    let noncreature = Filter.Type.Not (Filter.Type.HasCardType CardType.Creature)

    Spec.it s "CR 613.1d Thalia's noncreature criterion admits an instant" $ do
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (bolt, _, gs) = matchesSpellBoard lightningBolt piker
      Spec.assertBool s (PlayerEffect.matchesSpell noncreature bolt gs) "Lightning Bolt is a noncreature spell"

    Spec.it s "CR 613.1d a creature spell fails the noncreature criterion" $ do
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, pikerId, gs) = matchesSpellBoard lightningBolt piker
      Spec.assertBool s (not (PlayerEffect.matchesSpell noncreature pikerId gs)) "Goblin Piker is a creature spell"

    Spec.it s "CR 613.1e a colour criterion admits a matching-colour spell" $ do
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (bolt, _, gs) = matchesSpellBoard lightningBolt piker
      Spec.assertBool s (PlayerEffect.matchesSpell (Filter.Type.HasColor Color.Red) bolt gs) "Lightning Bolt is red"

    Spec.it s "CR 613.1e a colour criterion rejects a non-matching colour" $ do
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (bolt, _, gs) = matchesSpellBoard lightningBolt piker
      Spec.assertBool s (not (PlayerEffect.matchesSpell (Filter.Type.HasColor Color.Blue) bolt gs)) "Lightning Bolt is not blue"

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.PlayerEffect" $ do
  ruleOfLawSpec s registry
  adjustmentSpec s
  thaliaSpec s registry
  medallionSpec s registry
  humilitySpec s registry
  titaniasSongSpec s registry
  edgewalkerSpec s registry
  reliquaryTowerSpec s registry
  storedSpec s registry
  silenceSpec s registry
  matchesSpellSpec s registry
