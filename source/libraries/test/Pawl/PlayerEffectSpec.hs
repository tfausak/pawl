{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.PlayerEffect and Pawl.Engine.Cost, plus the types they case on
-- (Pawl.Types.PlayerEffect, PlayerScope, AffectedPlayers, PlayerStaticAbility) and
-- the stored carrier Pawl.Types.ActivePlayerEffect. The spell match runs through
-- the identity-blind Pawl.Engine.Filter over a Pawl.Types.Filter. CR 613.10/613.11: the
-- continuous effects that affect PLAYERS and the RULES OF THE GAME, which sit
-- outside the CR 613 layer system entirely.
--
-- The seven gate cards: Rule of Law, Thalia Guardian of Thraben, Sapphire
-- Medallion, Edgewalker, Reliquary Tower, Silence and Null Chamber. Synthetic
-- Phyrexian Discount, Synthetic Snow Discount, Synthetic Monocolored Hybrid
-- Discount and Synthetic Hybrid Discount are SYNTHETIC, one per reduction CR
-- 118.7e-g describes and no card prints. Khabál
-- Ghoul, Withered Wretch and Sol Ring are the costs the last two are aimed at.
--
-- Patrician Geist is the reduction side's zone axis: CR 601.2a's zone a spell was
-- CAST FROM, which Filter.WasCastFrom reads and which no other card in this
-- module names.
--
-- Blossoming Calm and Synthetic Conditional Silence are the stored carrier's
-- other two durations (CR 611.2a's "until your next turn", CR 611.2b's "for as
-- long as"), the second synthetic for the reason its own group states.
-- Humility,
-- Opalescence and Titania's Song join them for CR 604.2's "and has the ability"
-- -- the one place this axis does meet the CR 613 layer system.
--
-- Exploration and Azusa, Lost but Seeking are the CR 305.2 pair, and they are a
-- PAIR on purpose: they grant different numbers of extra land plays, which is
-- what tells a real count apart from a boolean-plus-one.
--
-- Null Chamber also brings CR 201.4's card-name choice and CR 614.1c's entry
-- replacement in with it, so the group covers Pawl.Engine.Replacement's
-- EntryRewrite.ChooseCardNames arm and Pawl.Engine.Action.playableLands as well.
-- Conjurer's Ban is the same two prohibitions on the STORED carrier, which is
-- what makes the pair worth having: its name is chosen by CR 608.2c during the
-- resolution rather than by CR 614.1c as a permanent enters, and its source is in
-- a graveyard before the rows it stored are read, so it is the only card that
-- reaches CR 608.2h's last-known road into Pawl.Engine.PlayerEffect.chosenNamesOf.
--
-- Runed Halo is the card-name choice's other shape -- CR 614.1c with ONE chooser
-- rather than Null Chamber's two -- and the pool's only card that gives a PLAYER
-- a rule 702.16 protection ability, so it carries rule 702.16b's and rule
-- 702.16c's player halves in with it (Pawl.Engine.Target.targetable and
-- Pawl.Engine.Sba.fallsOff). Curse of Vitality is the enchant-player Aura on the
-- other side of both.
--
-- Artificial Evolution and Magical Hack join Edgewalker for CR 612.1, the second
-- rule reaching this axis from outside it: the word naming which spells a player
-- static ability discounts is printed text like any other, so a text change moves
-- the discount off it. Magical Hack reaches the STORED carrier too, at Synthetic
-- Conditional Silence on the stack, where the word it swaps is the one CR
-- 611.2b's duration counts.
--
-- The Ten Rings and Sea Gate Restoration are the CR 613.11 TIMESTAMP pair, and
-- they are a pair on purpose: a set maximum hand size and a removed one disagree,
-- so which of them entered later decides the answer -- and one rides each carrier,
-- which is where pawl's order used to come apart (Reliquary Tower is the third
-- card in the group, and its ruling is the authority for the reading).
--
-- Minamo Scrollkeeper and Gnat Miser are the ADJUSTING pair on that same axis,
-- and a pair for two reasons at once: they move the number in opposite
-- directions, which is what tells two constructors from one signed delta, and
-- Gnat Miser reduces each OPPONENT's maximum where every other card here writes
-- its own controller's.
--
-- Void Winnower brings CR 601.3a's LOOKAHEAD, and Molten Disaster is the second
-- half of that pair: a prohibition on even mana values, against an {X} spell
-- whose mana value is even only while it sits in a hand (CR 202.3e).
--
-- Cease-Fire is the TARGETED seat, and the reason it is a three-seat fixture:
-- its restriction is stored against one player the spell chose (CR 601.2c)
-- rather than against a scope, which two seats cannot tell from "your opponent".
--
-- Spider-Punk brings CR 701.6a onto the axis, with Cancel and Stifle as the two
-- counterers it has to stop -- the one place this file reaches
-- Pawl.Engine.Event's countering funnel. Prowling Serpopard is its NARROWED
-- counterpart, and the pair is the point: one board, one Cancel, and the filter
-- alone deciding whether the victim spell survives.
--
-- Oppressive Rays is CR 601.2f's ACTIVATION side, on a criterion no other group
-- here has: CR 303.4b's "enchanted", answered off the source the row carries.
-- Brothers of Fire is the taxed ability, and the group runs two of them so that
-- "the tax reached this object" is told apart from "the tax reached everything".
--
-- Scout's Warning is CR 601.1a's PLAY-scoped sibling of Vedalken Orrery's
-- CastAsThoughItHadFlash, and the one producer whose criterion a LAND card can
-- match: Dryad Arbor is a creature land, so its play (never a cast, CR 305.1)
-- is what Pawl.Engine.Action.landTimingOk has to reach. It also carries the
-- pool's only Expiry.WhenUsed grant -- CR 611.2a's "or until you play a
-- matching card, whichever comes first" -- so Mountain beside it is the
-- Filter's own negative and Goblin Piker proves the same grant widens a CAST.
module Pawl.PlayerEffectSpec where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator Pawl.Engine.Filter already claims the alias Filter.
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.AppliedReduction as AppliedReduction
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostAdjustments as CostAdjustments
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterName as CounterName
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Expiry as Expiry.Type
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Hybrid as Hybrid
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerEffect as PlayerEffect.Type
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.SpellWasCast as SpellWasCast
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.VariableChoice as VariableChoice
import qualified Pawl.Types.While as While
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

-- The name every assertion below about a QUALITY-FREE prohibition passes to
-- prohibitsCasting. CR 601.3's "can't cast spells" (Silence) and "can't cast
-- more than one spell" (Rule of Law) do not depend on WHICH spell, so their
-- arms ignore this; naming a card in the test's own hand would suggest a
-- dependence those rules do not have. Null Chamber's arm, which does depend on
-- the name, passes a real card's name in nullChamberSpec below -- to this
-- function directly, and through Cast.castable and Action.legalActions.
anySpell :: CardName.CardName
anySpell = CardName.MkCardName (Text.pack "any spell")

-- The object those same assertions pass beside anySpell above, and a made-up one
-- for that name's own reason: an arm that reads neither the spell's name nor its
-- characteristics must not be handed a real card, or the assertion would suggest
-- a dependence the rule does not have. Nothing dereferences it -- only
-- PlayerEffect.CantCastMatching reads the object, and no board below carries one
-- -- so an id no fixture mints is the honest argument. That arm is asked about a
-- real proposal instead, through Cast.castable, in Pawl.SpecialActionSpec's
-- Damping Engine cases.
anySpellId :: ObjectId.ObjectId
anySpellId = ObjectId.MkObjectId 999999

ruleOfLawSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ruleOfLawSpec s registry =
  Spec.describe s "RuleOfLaw" $ do
    Spec.it s "before any spell is cast, both cards are castable" $ do
      plains <- S.printingOf s registry "Plains"
      ruleOfLaw <- S.printingOf s registry "Rule of Law"
      let (a, b, _, board) = ruleOfLawBoard plains ruleOfLaw
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell VariableChoice.Announced board)) "not prohibited"
      Spec.assertBool s (elem (Action.Type.Cast a (S.printingName ruleOfLaw) Facing.FaceUp) (Action.legalActions S.alice board)) "a offered"
      Spec.assertBool s (elem (Action.Type.Cast b (S.printingName ruleOfLaw) Facing.FaceUp) (Action.legalActions S.alice board)) "b offered"

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
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell VariableChoice.Announced afterFirst) "alice is now prohibited"
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
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell VariableChoice.Announced afterFirst)) "bob is not prohibited"

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
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell VariableChoice.Announced nextOwnTurn)) "not prohibited"
      Spec.assertBool s (elem (Action.Type.Cast b (S.printingName ruleOfLaw) Facing.FaceUp) (Action.legalActions S.alice nextOwnTurn)) "b offered again"

    -- Ruling: "If you cast a spell that was countered, you can't cast
    -- another spell during the same turn." The counted event is the CAST.
    Spec.it s "CR 601.2i a countered spell still counted" $ do
      plains <- S.printingOf s registry "Plains"
      ruleOfLaw <- S.printingOf s registry "Rule of Law"
      let (_, x, _, plain) = ruleOfLawBoard plains ruleOfLaw
          onBoard = snd (S.addPermanent ruleOfLaw S.alice plain)
          cast = S.runPure S.identityAnswer onBoard (S.cast S.alice x)
      case GameState.stack cast of
        [] -> Spec.assertFailure s "expected the spell on the stack"
        top : _ -> do
          let countered = S.runPure S.identityAnswer cast (Event.counter S.noSource S.bob top)
          Spec.assertEqWith s "the stack is empty again" (GameState.stack countered) []
          Spec.assertBool s (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell VariableChoice.Announced countered) "still prohibited"

    -- The effect is RE-DERIVED from the battlefield on every read, so there
    -- is no stored state to unwind when its source leaves.
    Spec.it s "CR 604.2 destroying Rule of Law lifts the restriction in the same turn" $ do
      plains <- S.printingOf s registry "Plains"
      ruleOfLaw <- S.printingOf s registry "Rule of Law"
      let (_, _, z, plain) = ruleOfLawBoard plains ruleOfLaw
          (rol, onBoard) = S.addPermanent ruleOfLaw S.alice plain
          castOne = S.withEvents [GameEvent.SpellCast (SpellWasCast.MkSpellWasCast S.alice S.noSource S.emptyCharacteristics (Just Zone.Hand))] onBoard
          gone = S.runPure S.identityAnswer castOne (Event.destroy Regenerability.Regenerable [rol])
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell VariableChoice.Announced castOne) "prohibited while it stands"
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell VariableChoice.Announced gone)) "not prohibited once it is gone"
      Spec.assertBool s (elem (Action.Type.Cast z (S.printingName ruleOfLaw) Facing.FaceUp) (Action.legalActions S.alice gone)) "and a cast is offered again"

    -- CR 601.3's prohibit half applies to EVERY cast, including a
    -- Panglacial Wurm cast from the library: the Panglacial permission
    -- (Cast.permitsCastWhileSearching) excepts only the timing half, not
    -- the prohibition half. Seven Forests pay the Wurm's {5}{G}{G}.
    Spec.it s "CR 601.3 Rule of Law also prohibits casting Panglacial Wurm from the library" $ do
      forest <- S.printingOf s registry "Forest"
      ruleOfLaw <- S.printingOf s registry "Rule of Law"
      panglacialWurm <- S.printingOf s registry "Panglacial Wurm"
      let base = S.landsInPlay forest 7
          (_, withRuleOfLaw) = S.addPermanent ruleOfLaw S.alice base
          (_, gs) = S.addLibraryCard panglacialWurm S.alice withRuleOfLaw
          castOne = S.withEvents [GameEvent.SpellCast (SpellWasCast.MkSpellWasCast S.alice S.noSource S.emptyCharacteristics (Just Zone.Hand))] gs
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
  Action.Type.Play {} -> False
  Action.Type.Activate _ _ -> False
  Action.Type.TurnFaceUp {} -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.Plot _ -> False
  Action.Type.Foretell _ -> False
  Action.Type.PutCompanionIntoHand -> False
  Action.Type.Ignore _ _ -> False
  Action.Type.EndEffect _ -> False
  Action.Type.ActivateManaAbility _ -> False
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

green :: ManaSymbol.ManaSymbol
green = ManaSymbol.OfType (ManaType.Colored Color.Green)

-- CR 107.4c's {C}, the mana type that is not a colour. CR 118.7b names it
-- alongside the colours; CR 118.7d is the arm that is about it alone, where CR
-- 118.7c is about the coloured symbols above.
colorless :: ManaSymbol.ManaSymbol
colorless = ManaSymbol.OfType ManaType.Colorless

-- CR 107.4f's {G/P}. A Color rather than a ManaType, since every Phyrexian
-- symbol is coloured.
phyrexianGreen :: ManaSymbol.ManaSymbol
phyrexianGreen = ManaSymbol.Phyrexian Color.Green

-- CR 601.2f's adjustments as this suite's assertions state them: the increases,
-- the reductions each floored at zero and each SPILLING what the cost cannot use
-- (CR 118.7b-d), and no added components -- the floor is Heartstone's sentence
-- and Pawl.Types.CostAdjustments.components is Brutal Suppression's, and no
-- spell-cost effect states either (the activation side of both is proved against
-- the card in Pawl.ActivateSpec).
adjustments :: [Natural] -> [ManaCost.ManaCost] -> CostAdjustments.CostAdjustments
adjustments increases reductions =
  CostAdjustments.MkCostAdjustments
    { CostAdjustments.increases = increases,
      CostAdjustments.reductions = fmap (\reduction -> AppliedReduction.MkAppliedReduction reduction 0 False) reductions,
      CostAdjustments.components = []
    }

-- `adjustments` with every reduction CONFINED to the coloured mana paid, which
-- is Edgewalker's "This effect reduces only the amount of colored mana you pay"
-- (CR 101.1) and the one thing that stops CR 118.7b-d's spill. The pair of
-- helpers is what lets a case assert the two readings against one cost.
confinedAdjustments :: [ManaCost.ManaCost] -> CostAdjustments.CostAdjustments
confinedAdjustments reductions =
  (adjustments [] reductions)
    { CostAdjustments.reductions = fmap (\reduction -> AppliedReduction.MkAppliedReduction reduction 0 True) reductions
    }

-- A reduction by an amount of GENERIC mana (CR 118.7a) -- the Medallion's shape,
-- and the only shape a reduction had before Edgewalker.
generic :: Natural -> ManaCost.ManaCost
generic n = ManaCost.MkManaCost [ManaSymbol.Generic n]

-- Cost.total via a bare ManaCost, component-free -- this suite predates P8's
-- Cost/CostComponent generalization and exercises only the mana half
-- (spellCostAdjustments), so the wrap-and-unwrap stays local here instead of
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
        (Cost.applyAdjustments (adjustments [] []) (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1, red])

    Spec.it s "CR 601.2f an increase adds generic mana" $
      Spec.assertEqWith
        s
        "{R} taxed by {1} is {1}{R}"
        (Cost.applyAdjustments (adjustments [1] []) (ManaCost.MkManaCost [red]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1, red])

    -- CR 118.7a: "Effects that reduce a cost by an amount of generic mana
    -- affect only the generic mana component of that cost. They can't affect
    -- the colored or colorless mana components."
    Spec.it s "CR 118.7a a reduction with no generic component to take is lost" $
      Spec.assertEqWith
        s
        "{U} reduced by {1} is still {U}"
        (Cost.applyAdjustments (adjustments [] [generic 1]) (ManaCost.MkManaCost [blue]))
        (ManaCost.MkManaCost [blue])

    Spec.it s "CR 118.7a a reduction takes only the generic component" $
      Spec.assertEqWith
        s
        "{2}{U} reduced by {1} is {1}{U}"
        (Cost.applyAdjustments (adjustments [] [generic 1]) (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1, blue])

    Spec.it s "CR 601.2f the total can't be reduced below {0}" $
      Spec.assertEqWith
        s
        "{1} reduced by {3} is {0}"
        (Cost.applyAdjustments (adjustments [] [generic 3]) (ManaCost.MkManaCost [ManaSymbol.Generic 1]))
        (ManaCost.MkManaCost [])

    -- THE ORDER TEST, in the small. Increase first gives {1}{U}, which the
    -- reduction takes back to {U}. Reduce first loses the reduction to CR
    -- 118.7a's empty generic component, and the increase then leaves {1}{U}.
    Spec.it s "CR 601.2f every increase applies before any reduction" $
      Spec.assertEqWith
        s
        "{U} +{1} -{1} is {U}"
        (Cost.applyAdjustments (adjustments [1] [generic 1]) (ManaCost.MkManaCost [blue]))
        (ManaCost.MkManaCost [blue])

    -- CR 118.7's typed half, which CR 118.7a's generic half above cannot
    -- reach: a reduction that NAMES a mana type takes that type's symbols.
    Spec.it s "CR 118.7 a typed reduction takes the cost's matching symbols" $
      Spec.assertEqWith
        s
        "{1}{W}{B} reduced by {W}{B} is {1}"
        (Cost.applyAdjustments (adjustments [] [ManaCost.MkManaCost [white, black]]) (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1])

    Spec.it s "CR 118.7 one reducing symbol takes exactly one matching symbol" $
      Spec.assertEqWith
        s
        "{W}{W} reduced by {W} is {W}"
        (Cost.applyAdjustments (adjustments [] [ManaCost.MkManaCost [white]]) (ManaCost.MkManaCost [white, white]))
        (ManaCost.MkManaCost [white])

    -- THE HEADLINE FALSIFIER for the typed half, in its two readings. CR 118.7b:
    -- the {B} half is an amount of coloured mana this cost does not require, so
    -- it comes off the generic component instead and {1}{W} pays nothing at all.
    Spec.it s "CR 118.7b an excess typed reduction spills onto the generic component" $
      Spec.assertEqWith
        s
        "{1}{W} reduced by {W}{B} is {0}"
        (Cost.applyAdjustments (adjustments [] [ManaCost.MkManaCost [white, black]]) (ManaCost.MkManaCost [ManaSymbol.Generic 1, white]))
        (ManaCost.MkManaCost [])

    -- THE OTHER READING, on the same cost and the same reduction, which is what
    -- makes the pair prove the flag rather than the arithmetic: Edgewalker's
    -- "This effect reduces only the amount of colored mana you pay" is card text
    -- CR 101.1 lets override CR 118.7b, and its own reminder text is the
    -- assertion -- "if you cast a Cleric spell with mana cost {1}{W}, it costs
    -- {1} to cast".
    Spec.it s "CR 101.1 a reduction confined to coloured mana drops the excess instead" $
      Spec.assertEqWith
        s
        "{1}{W} reduced by a confined {W}{B} is {1}"
        (Cost.applyAdjustments (confinedAdjustments [ManaCost.MkManaCost [white, black]]) (ManaCost.MkManaCost [ManaSymbol.Generic 1, white]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1])

    -- Ruling: "If you have more than one of these on the battlefield, the cost
    -- reduction is cumulative." Cumulative, and still bounded by what the cost
    -- actually has to give -- the second pair finds no white and no black left,
    -- so CR 118.7c sends both symbols at the {1} and CR 601.2f floors the rest.
    Spec.it s "CR 118.7c two typed reductions pool, and the second spills onto the generic component" $
      Spec.assertEqWith
        s
        "{1}{W}{B} reduced by {W}{B} twice is {0}"
        (Cost.applyAdjustments (adjustments [] [ManaCost.MkManaCost [white, black], ManaCost.MkManaCost [white, black]]) (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black]))
        (ManaCost.MkManaCost [])

    -- The same pooling under Edgewalker's own sentence, which is what two
    -- Edgewalkers really do: the second pair strands with nothing to take and
    -- nothing to spill onto.
    Spec.it s "CR 101.1 two confined typed reductions leave the generic component alone" $
      Spec.assertEqWith
        s
        "{1}{W}{B} reduced by a confined {W}{B} twice is {1}"
        (Cost.applyAdjustments (confinedAdjustments [ManaCost.MkManaCost [white, black], ManaCost.MkManaCost [white, black]]) (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1])

    -- CR 118.7c's own wording, which CR 118.7b's arm does not reach: the cost
    -- HAS a mana component of that colour and the reduction EXCEEDS it, so the
    -- colour goes to nothing and the DIFFERENCE comes off the generic component.
    -- {2}{W} reduced by {W}{W} keeps one generic mana; dropping the excess would
    -- leave {2}.
    Spec.it s "CR 118.7c a reduction exceeding the cost's colour spills the difference" $
      Spec.assertEqWith
        s
        "{2}{W} reduced by {W}{W} is {1}"
        (Cost.applyAdjustments (adjustments [] [ManaCost.MkManaCost [white, white]]) (ManaCost.MkManaCost [ManaSymbol.Generic 2, white]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1])

    -- CR 118.7d, the colourless half of the same sentence, which is a DIFFERENT
    -- mana type and not a colour: {2}{C} reduced by {C}{C} loses its {C} and one
    -- generic mana. Nothing in Pawl.Engine.Cost reads the two arms apart -- both
    -- are an OfType -- and this case is what says so.
    Spec.it s "CR 118.7d the same holds of a colourless component" $
      Spec.assertEqWith
        s
        "{2}{C} reduced by {C}{C} is {1}"
        (Cost.applyAdjustments (adjustments [] [ManaCost.MkManaCost [colorless, colorless]]) (ManaCost.MkManaCost [ManaSymbol.Generic 2, colorless]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1])

    -- CR 118.7a is NOT the mirror image: a GENERIC reduction that exceeds the
    -- generic component stops there rather than spilling the other way onto the
    -- coloured symbols. The pair with the case above is what keeps the spill
    -- one-directional.
    Spec.it s "CR 118.7a a generic reduction never spills onto a coloured symbol" $
      Spec.assertEqWith
        s
        "{1}{W} reduced by {2} is {W}"
        (Cost.applyAdjustments (adjustments [] [generic 2]) (ManaCost.MkManaCost [ManaSymbol.Generic 1, white]))
        (ManaCost.MkManaCost [white])

    -- CR 118.7f, in the small: the two SIDES of the cancellation read a
    -- Phyrexian symbol differently. A reduction written {G/P} names green, and
    -- a cost written {G/P} names nothing yet -- so the same pair of symbols
    -- cancels in one direction and not the other. The board-level group below
    -- is what proves each half against a card.
    Spec.it s "CR 118.7f a Phyrexian reduction takes one mana of its colour" $
      Spec.assertEqWith
        s
        "{1}{G} reduced by {G/P} is {1}"
        (Cost.applyAdjustments (adjustments [] [ManaCost.MkManaCost [phyrexianGreen]]) (ManaCost.MkManaCost [ManaSymbol.Generic 1, green]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1])

    Spec.it s "a Phyrexian symbol in the COST is not what a reduction of its colour takes" $
      Spec.assertEqWith
        s
        "{G/P} reduced by {G} is still {G/P}"
        (Cost.applyAdjustments (adjustments [] [ManaCost.MkManaCost [green]]) (ManaCost.MkManaCost [phyrexianGreen]))
        (ManaCost.MkManaCost [phyrexianGreen])

    -- CR 118.7e, in the small: WHICH TWO THINGS the payer is choosing between.
    -- "If a colored or colorless half is chosen, the cost is reduced by one mana
    -- of that type. If a generic half is chosen, the cost is reduced by an
    -- amount of generic mana equal to that half's number" -- so a colour/colour
    -- symbol offers two OfTypes and a monocolored one offers an OfType against
    -- CR 107.4e's {2}. Cost.announceReductions puts exactly this list on the
    -- wire, and the board-level groups below prove each half against a card.
    Spec.it s "CR 118.7e a hybrid reduction offers its two halves" $ do
      Spec.assertEqWith
        s
        "{W/U} offers {W} and {U}"
        (Cost.reductionHalvesOf (ManaSymbol.Hybrid (Hybrid.MkHybrid (ManaType.Colored Color.White) (ManaType.Colored Color.Blue))))
        (Just [white, blue])
      Spec.assertEqWith
        s
        "{2/B} offers {B} and {2}"
        (Cost.reductionHalvesOf (ManaSymbol.MonocoloredHybrid (ManaType.Colored Color.Black)))
        (Just [black, ManaSymbol.Generic 2])

    -- The symbols CR 118.7e does NOT reach, and each for its own reason: a
    -- printed amount of generic mana and a plain coloured symbol have one half
    -- to begin with, CR 118.7f gives a Phyrexian reduction its colour outright,
    -- and CR 118.7g makes an {S} reduction generic mana. A Just here would put a
    -- prompt in front of a player with nothing to decide.
    Spec.it s "a symbol with no halves is not asked about" $ do
      Spec.assertEqWith s "{1}" (Cost.reductionHalvesOf (ManaSymbol.Generic 1)) Nothing
      Spec.assertEqWith s "{G}" (Cost.reductionHalvesOf green) Nothing
      Spec.assertEqWith s "{G/P}" (Cost.reductionHalvesOf phyrexianGreen) Nothing
      Spec.assertEqWith s "{S}" (Cost.reductionHalvesOf ManaSymbol.Snow) Nothing

    -- Pawl.Types.ManaSymbol calls `Hybrid t t` degenerate rather than illegal,
    -- and no card prints one. Both halves are the same symbol, so there is
    -- nothing to observe about the answer and CR 118.7e's prompt is elided --
    -- the one elision this rule permits.
    Spec.it s "a hybrid of one type offers one half, not the same one twice" $
      Spec.assertEqWith
        s
        "{W/W} offers {W}"
        (Cost.reductionHalvesOf (ManaSymbol.Hybrid (Hybrid.MkHybrid (ManaType.Colored Color.White) (ManaType.Colored Color.White))))
        (Just [white])

    -- Both halves of a colour/colour hybrid really do bite a cost that prints
    -- both colours, which the board groups below cannot show: their cost prints
    -- one of the two, deliberately, so that the count tells the answers apart.
    -- These are the symbols announceReductions leaves behind, cancelled by
    -- applyAdjustments' ordinary typed path.
    Spec.it s "CR 118.7e either half of a {W/B} takes its own symbol" $ do
      Spec.assertEqWith
        s
        "{1}{W}{B} reduced by the white half is {1}{B}"
        (Cost.applyAdjustments (adjustments [] [ManaCost.MkManaCost [white]]) (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1, black])
      Spec.assertEqWith
        s
        "{1}{W}{B} reduced by the black half is {1}{W}"
        (Cost.applyAdjustments (adjustments [] [ManaCost.MkManaCost [black]]) (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1, white])

    -- CR 118.7g, in the small, and the other arm where the two sides part
    -- company. A reduction written {S} is an amount of GENERIC mana, so it
    -- comes off the generic component exactly as a {1} would.
    Spec.it s "CR 118.7g an {S} reduction takes that much generic mana" $
      Spec.assertEqWith
        s
        "{2}{U} reduced by {S} is {1}{U}"
        (Cost.applyAdjustments (adjustments [] [ManaCost.MkManaCost [ManaSymbol.Snow]]) (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1, blue])

    -- CR 107.4h's half of the same split: "Effects that reduce the amount of
    -- generic mana you pay don't affect {S} costs." So an {S} in a COST is no
    -- part of the generic component a reduction comes off -- were it counted
    -- there, the {1} below would survive the reduction and re-emit as generic.
    -- {1}{S} is Adarkar Windform's activation cost, not a printed mana cost:
    -- Magic's three {S} mana costs are Arcum's Astrolabe's {S}, Icehide Golem's
    -- {S} and Wowzer, the Aspirational's {C}{W}{U}{B}{R}{G}{S}, and none of them
    -- carries the generic component that makes the two readings differ.
    Spec.it s "CR 107.4h a generic reduction does not affect an {S} in the cost" $
      Spec.assertEqWith
        s
        "{1}{S} reduced by {1} is {S}"
        (Cost.applyAdjustments (adjustments [] [generic 1]) (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.Snow]))
        (ManaCost.MkManaCost [ManaSymbol.Snow])

    -- The two halves of ONE reduction, on the two halves of one cost: CR
    -- 118.7a routes the {1} to the generic component and the {U} takes the
    -- blue symbol, and neither reaches into the other's component.
    Spec.it s "CR 118.7a a mixed reduction splits by component" $
      Spec.assertEqWith
        s
        "{2}{U} reduced by {1}{U} is {1}"
        (Cost.applyAdjustments (adjustments [] [ManaCost.MkManaCost [ManaSymbol.Generic 1, blue]]) (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1])

-- alice controls Thalia and `n` untapped Mountains; her hand holds one
-- Lightning Bolt ({R} instant -- noncreature) and one Goblin Piker ({1}{R}
-- creature). Loaded fresh inside each case that needs it -- equivalent
-- because loading is deterministic and cached (batch-recipe.md).
thaliaBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
thaliaBoard mountain thalia lightningBolt piker n =
  let base = S.landsInPlay mountain n
      (_, gs1) = S.addPermanent thalia S.alice base
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

-- thaliaBoard, minus the Piker and plus each printing in `extras` on alice's
-- battlefield after Thalia (so `extras` take the later timestamps). Returns the
-- Bolt and Thalia. Draws nothing and advances no turn, as thaliaBoard does not.
thaliaWith :: Printing.Printing -> Printing.Printing -> Printing.Printing -> [Printing.Printing] -> Int -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
thaliaWith mountain thalia lightningBolt extras n =
  let base = S.landsInPlay mountain n
      (thaliaId, gs1) = S.addPermanent thalia S.alice base
      gs2 = List.foldl' (\g p -> snd (S.addPermanent p S.alice g)) gs1 extras
      (bolt, gs3) = S.addHandCard lightningBolt S.alice gs2
   in ( bolt,
        thaliaId,
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
    -- cast that cannot be afforded, and nothing REPAIRS a cast partway --
    -- that is a wedged game, not a rejected action.
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
      let base = S.landsInPlay forest 7
          (_, withThalia) = S.addPermanent thalia S.alice base
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
        (fmap fst (Cast.castableWhileSearching S.alice gs))
        [wurm]

    -- The proving test for Projection.liveAfterLayers, CR 305.7's CR
    -- 613.10/613.11 half; Projection.setSubtypeStripped is the static-ability
    -- half beside it (see #391). Ashaya adds the card type Land to Thalia at
    -- layer 4; Blood Moon then depends on that (CR 613.8a) and SETS her subtype
    -- to Mountain, which by CR 305.7 takes every ability generated by her rules
    -- text -- the tax included. The old gate asked Blood Moon's "nonbasic land"
    -- filter against Thalia's BASE characteristics, where she is only a
    -- creature, so the tax survived.
    --
    -- Both controls are load-bearing and neither may lift the tax. Ashaya alone
    -- ADDS a land type, and CR 305.7's last sentence keeps the rules text of a
    -- land that gains types in addition to its own. Blood Moon alone never names
    -- Thalia at all. Only the conjunction strips.
    --
    -- EXACT COSTS rather than S.castable, deliberately: the stripped board's
    -- Thalia and Ashaya are Mountains, so CR 305.6 hands each a mana ability and
    -- (the fixtures placing permanents Settled) they tap for it. A castability
    -- pair could not hold available mana equal across the two boards, so it would
    -- pass for a reason other than the tax. These four are equalities on a
    -- ManaCost and never consult available mana.
    Spec.it s "CR 305.7 Ashaya animates Thalia into a Mountain, so Blood Moon takes her tax" $ do
      mountain <- S.printingOf s registry "Mountain"
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
      bloodMoon <- S.printingOf s registry "Blood Moon"
      let (bolt1, _, p1) = thaliaWith mountain thalia lightningBolt [] 3
          (bolt2, _, p2) = thaliaWith mountain thalia lightningBolt [ashaya] 3
          (bolt3, _, p3) = thaliaWith mountain thalia lightningBolt [bloodMoon] 3
          (boltN, thaliaN, nn) = thaliaWith mountain thalia lightningBolt [ashaya, bloodMoon] 3
          printed = ManaCost.MkManaCost [red]
          taxed = Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, red])
          untaxed = Just printed
      Spec.assertEqWith s "control: Thalia alone taxes" (totalManaCost S.alice bolt1 printed p1) taxed
      Spec.assertEqWith s "control: Ashaya ADDS a land type (CR 305.7's last sentence), so Thalia keeps her text" (totalManaCost S.alice bolt2 printed p2) taxed
      Spec.assertEqWith s "control: Blood Moon alone does not name a creature" (totalManaCost S.alice bolt3 printed p3) taxed
      Spec.assertEqWith s "both: Thalia is a nonbasic land SET to Mountain, so CR 305.7 takes her ability" (totalManaCost S.alice boltN printed nn) untaxed
      -- Anchors, so a failure above says which half moved.
      Spec.assertBool s (Set.member CardType.Land (Projection.cardTypesOf thaliaN nn)) "Thalia is a land"
      Spec.assertBool s (Set.member Subtype.Mountain (Projection.subtypesOf thaliaN nn)) "and a Mountain"
      Spec.assertBool s (Projection.isCreatureOf thaliaN nn) "and still a creature (CR 305.7: setting a subtype removes no card type)"

-- alice controls a Sapphire Medallion and `n` untapped Islands; her hand
-- holds Unsummon ({U} instant), Divination ({2}{U} sorcery) and Lightning
-- Bolt ({R} instant). Loaded fresh inside each case that needs it --
-- equivalent because loading is deterministic and cached (batch-recipe.md).
medallionBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
medallionBoard island sapphireMedallion unsummonPrinting divinationPrinting lightningBolt n =
  let base = S.landsInPlay island n
      (_, gs1) = S.addPermanent sapphireMedallion S.alice base
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
      (_, gs1) = S.addPermanent sapphireMedallion S.alice base
      (_, gs2) = S.addPermanent thalia S.alice gs1
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

-- alice on `n` untapped Islands with one Think Twice in her graveyard and one in
-- her hand -- the same card in the two zones the case tells apart, so the boards
-- it compares differ in the zone and in nothing else. The Patrician Geist is
-- added by each case rather than here, so it is the only other difference any
-- board below carries.
--
-- Her library is stocked because Think Twice draws and CR 104.3c would lose her
-- the game out from under the assertions.
geistBoard :: Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
geistBoard island thinkTwice n =
  let (yardId, gs1) = S.addGraveyardCard thinkTwice S.alice (S.landsInPlay island n)
      (handId, gs2) = S.addHandCard thinkTwice S.alice gs1
      (_, gs3) = S.addLibraryCard island S.alice gs2
   in ( yardId,
        handId,
        gs3
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Patrician Geist {2}{U} Creature -- Spirit Knight 2/2 (MID 69): "Flying / Other
-- Spirits you control get +1/+1. / Spells you cast from your graveyard cost {1}
-- less to cast." Oracle text verified against Scryfall.
--
-- The third sentence is the point: it is the reduction half of the CR 601.2a
-- cast-from-zone atom Aven Interrupter taxes with (Pawl.CastSpec), spelled
-- `And [WasCastFrom Graveyard, OwnedBy You]`.
--
-- The OwnedBy conjunct is a REGRESSION FENCE rather than a proven behaviour, and
-- CR 400.3 is what makes it exact: a card in a graveyard is in its owner's, so
-- "your graveyard" is the spell's owner being the caster. A board that
-- discriminates it now exists -- Tinybones, the Pickpocket casts a card out of
-- the graveyard of the player it damaged (Pawl.CastSpec's Pickpocket group) --
-- but no case in the tree drives the Geist's reduction on such a cast, so
-- dropping the conjunct still leaves the suite green.
--
-- Think Twice ({1}{U} Instant, "Draw a card." / "Flashback {2}{U}") is the spell,
-- for the reason Pawl.CastSpec's tax group gives: the two zones it can be cast
-- from off one board are what make the reduction discriminating.
patricianGeistSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
patricianGeistSpec s registry =
  Spec.describe s "PatricianGeist" $ do
    -- MANA TAPPED, not a cost read: #2363's content is that a cost filter over
    -- the wrong zone leaves Pawl.Engine.Cast.castable and Pawl.Engine.Cast
    -- .castSpell pricing one cast differently, so the payment is where the
    -- reduction has to show.
    Spec.it s "CR 601.2f a flashback cast out of her own graveyard costs {1} less, and her cast from hand does not" $ do
      island <- S.printingOf s registry "Island"
      geist <- S.printingOf s registry "Patrician Geist"
      thinkTwice <- S.printingOf s registry "Think Twice"
      let (yardId, handId, open) = geistBoard island thinkTwice 5
          withGeist = snd (S.addPermanent geist S.alice open)
          -- The Geist is itself a permanent alice controls, so only her LANDS
          -- may be counted: it enters untapped and stays that way.
          tappedAfter gs oid = S.tappedCount S.alice (S.runPure S.identityAnswer gs (S.cast S.alice oid))
      Spec.assertEqWith s "CR 702.34a the flashback cost is {2}{U}, so three Islands with no Geist out" (tappedAfter open yardId) 3
      Spec.assertEqWith s "and two with it, CR 118.7a taking the reduction off the generic component alone" (tappedAfter withGeist yardId) 2
      Spec.assertEqWith s "the sentence names her graveyard and not her hand: the {1}{U} hand cast taps two either way" (tappedAfter open handId, tappedAfter withGeist handId) (2, 2)

    -- Two Islands is exactly the amount that tells the two prices apart, the
    -- shape the Medallion's gate case above takes -- and it is the half #2363
    -- says a wrong reader gets wrong in the opposite direction from the payment.
    Spec.it s "CR 601.2f the reduction is observable at the castability gate too" $ do
      island <- S.printingOf s registry "Island"
      geist <- S.printingOf s registry "Patrician Geist"
      thinkTwice <- S.printingOf s registry "Think Twice"
      let (yardId, _, open) = geistBoard island thinkTwice 2
          withGeist = snd (S.addPermanent geist S.alice open)
      Spec.assertBool s (S.castable S.alice yardId withGeist) "castable for {1}{U} with two Islands"
      Spec.assertBool s (not (S.castable S.alice yardId open)) "and not for {2}{U} without the Geist"

    -- CR 611.1 / 109.5 again, the Medallion's own negative one zone over: the You
    -- scope is the effect's controller, so bob's flashback out of bob's graveyard
    -- pays full price off the same board.
    Spec.it s "CR 109.5 the You scope does not discount an opponent's cast from their own graveyard" $ do
      island <- S.printingOf s registry "Island"
      geist <- S.printingOf s registry "Patrician Geist"
      thinkTwice <- S.printingOf s registry "Think Twice"
      let (_, _, open) = geistBoard island thinkTwice 5
          withGeist = snd (S.addPermanent geist S.alice open)
          (bobYard, seated) = S.addGraveyardCard thinkTwice S.bob (S.landsFor island S.bob 5 withGeist)
          (_, stocked) = S.addLibraryCard island S.bob seated
      Spec.assertEqWith
        s
        "bob taps three for the same flashback alice would pay two for"
        (S.tappedCount S.bob (S.runPure S.identityAnswer stocked (S.cast S.bob bobYard)))
        3

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
          (_, withRuleOfLaw) = S.addPermanent ruleOfLaw S.alice base
          withHumility = S.withHumility humility withRuleOfLaw
          (_, withOpalescence) = S.addPermanent opalescence S.alice withHumility
          castOne = S.withEvents [GameEvent.SpellCast (SpellWasCast.MkSpellWasCast S.alice S.noSource S.emptyCharacteristics (Just Zone.Hand))]
      Spec.assertBool
        s
        (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell VariableChoice.Announced (castOne withHumility))
        "control: Humility alone does not reach an enchantment"
      Spec.assertBool
        s
        (not (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell VariableChoice.Announced (castOne withOpalescence)))
        "once animated, Rule of Law loses the ability and the limit lifts"

-- alice controls a Sapphire Medallion and two untapped Islands, with Divination
-- ({2}{U}) in hand; the fourth component is the same board with bob's Titania's
-- Song added, so a case can assert against both. Loaded fresh inside each case
-- that needs it -- equivalent because loading is deterministic and cached
-- (batch-recipe.md).
titaniasSongBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, GameState.GameState)
titaniasSongBoard island sapphireMedallion divinationPrinting titaniasSong =
  let base = S.landsInPlay island 2
      (medallion, gs1) = S.addPermanent sapphireMedallion S.alice base
      (divination, gs2) = S.addHandCard divinationPrinting S.alice gs1
      bare =
        gs2
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      (_, sung) = S.addPermanent titaniasSong S.bob bare
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
          (_, sung) = S.addPermanent titaniasSong S.bob taxed
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
      put g _ = snd (S.addPermanent edgewalker S.alice g)
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
-- The card the typed half of a reduction exists for, and the pool's one printing
-- of CR 101.1's confinement -- the excess dropped rather than spilled, against
-- CR 118.7b-d's default. Edgewalker is itself a Cleric, so the spell it
-- discounts is another copy of itself and this group needs no second Cleric.
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
    -- text stops it. The Adjustments group asserts both readings of exactly
    -- this cost against each other.
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

-- alice controls `copies` Synthetic Phyrexian Discounts and `n` untapped
-- Forests; her hand holds one Longtusk Cub ({1}{G} green Cat) and one Mutagenic
-- Growth ({G/P} green instant). Loaded fresh inside each case that needs it --
-- equivalent because loading is deterministic and cached (batch-recipe.md).
phyrexianDiscountBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> Int -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
phyrexianDiscountBoard forest discount cub growth copies n =
  let base = S.landsInPlay forest n
      put g _ = snd (S.addPermanent discount S.alice g)
      withCopies = List.foldl' put base [1 .. copies]
      (cubId, gs1) = S.addHandCard cub S.alice withCopies
      (growthId, gs2) = S.addHandCard growth S.alice gs1
   in ( cubId,
        growthId,
        gs2
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Synthetic Phyrexian Discount {2} Artifact: "Green spells you cast cost {G/P}
-- less to cast."
--
-- SYNTHETIC, and the only synthetic in this file. CR 118.7f exists to say what
-- a reduction written with a Phyrexian mana symbol does, so nothing in the
-- rules forbids the printing -- the pool merely lacks one. All 41 cards whose
-- oracle text carries a Phyrexian symbol (Scryfall, 2026-08-05) spend it in a
-- COST, never as the amount of a reduction, and none of the 539 cards saying
-- "less to cast" names one.
--
-- Green because Mutagenic Growth's whole cost is {G/P}, and the third case below
-- aims this reduction straight at it: the two sides of the
-- cancellation read the same symbol differently, and matching the colours is
-- what makes that visible rather than merely stipulated.
phyrexianDiscountSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
phyrexianDiscountSpec s registry =
  Spec.describe s "SyntheticPhyrexianDiscount" $ do
    -- THE HEADLINE FALSIFIER, and it separates all three readings of the
    -- reduction at once. CR 118.7f takes "one mana of that symbol's color", so
    -- {1}{G} loses its {G} and keeps its {1}. Reading the symbol as generic
    -- would leave {G}; reading it as no type at all -- what the arm did before
    -- -- would leave {1}{G} untouched.
    Spec.it s "CR 118.7f a reduction written {G/P} takes one green mana off the cost" $ do
      forest <- S.printingOf s registry "Forest"
      discount <- S.printingOf s registry "Synthetic Phyrexian Discount"
      cub <- S.printingOf s registry "Longtusk Cub"
      growth <- S.printingOf s registry "Mutagenic Growth"
      let (cubId, _, gs) = phyrexianDiscountBoard forest discount cub growth 1 2
      Spec.assertEqWith
        s
        "{1}{G} becomes {1}"
        (totalManaCost S.alice cubId (ManaCost.MkManaCost [ManaSymbol.Generic 1, green]) gs)
        (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1]))

    -- The control the case above needs: without the artifact the same spell is
    -- full price, so the {G} really did leave because of the reduction.
    Spec.it s "without the reducer the same spell is full price" $ do
      forest <- S.printingOf s registry "Forest"
      discount <- S.printingOf s registry "Synthetic Phyrexian Discount"
      cub <- S.printingOf s registry "Longtusk Cub"
      growth <- S.printingOf s registry "Mutagenic Growth"
      let (cubId, _, gs) = phyrexianDiscountBoard forest discount cub growth 0 2
      Spec.assertEqWith
        s
        "{1}{G} stays {1}{G}"
        (totalManaCost S.alice cubId (ManaCost.MkManaCost [ManaSymbol.Generic 1, green]) gs)
        (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, green]))

    -- THE OTHER SIDE of the same symbol, and the reason CR 118.7f is a rule
    -- about REDUCTIONS only. Mutagenic Growth's printed {G/P} is a symbol its
    -- controller has not yet announced a route for (CR 601.2b precedes CR
    -- 601.2f), so there is no green mana in the cost for a green reduction to
    -- cancel -- Edgewalker's ruling read this way round, "if you choose to pay
    -- such a cost with {W} or {B}, Edgewalker can reduce that part of the
    -- cost". The reduction here is spent by nothing.
    --
    -- CR 118.7b turns the unspent green into one GENERIC mana, and this cost has
    -- no generic component for CR 118.7a to take it off, so {G/P} is the answer
    -- whether the stranded symbol spills or not.
    Spec.it s "an unannounced Phyrexian symbol in the COST offers nothing to cancel" $ do
      forest <- S.printingOf s registry "Forest"
      discount <- S.printingOf s registry "Synthetic Phyrexian Discount"
      cub <- S.printingOf s registry "Longtusk Cub"
      growth <- S.printingOf s registry "Mutagenic Growth"
      let (_, growthId, gs) = phyrexianDiscountBoard forest discount cub growth 1 2
      Spec.assertEqWith
        s
        "{G/P} stays {G/P}"
        (totalManaCost S.alice growthId (ManaCost.MkManaCost [phyrexianGreen]) gs)
        (Just (ManaCost.MkManaCost [phyrexianGreen]))

    -- NO colour-criterion case here, deliberately. A non-green spell prints no
    -- {G} either, so a {G/P} reduction would take nothing from it whatever the
    -- Filter said -- the assertion would pass for the wrong reason, and it did
    -- under mutation. Pawl.Engine.Filter's colour atom is proved by the
    -- SapphireMedallion group instead, and the mutation that DOES discriminate
    -- here (this card's HasColor Green flipped to Red) breaks the three cases
    -- that assert a discount.

    -- BOTH cost sites, one scenario, exactly as the Edgewalker group tests
    -- them. One Forest is the amount that tells {1} apart from {1}{G}.
    Spec.it s "CR 601.2f castability is measured against the total cost" $ do
      forest <- S.printingOf s registry "Forest"
      discount <- S.printingOf s registry "Synthetic Phyrexian Discount"
      cub <- S.printingOf s registry "Longtusk Cub"
      growth <- S.printingOf s registry "Mutagenic Growth"
      let (discounted, _, withDiscount) = phyrexianDiscountBoard forest discount cub growth 1 1
          (undiscounted, _, bare) = phyrexianDiscountBoard forest discount cub growth 0 1
      Spec.assertBool s (not (S.castable S.alice undiscounted bare)) "one Forest cannot pay a printed {1}{G}"
      Spec.assertBool s (S.castable S.alice discounted withDiscount) "but it can pay the discounted {1}"

    Spec.it s "CR 601.2f payment spends the total cost" $ do
      forest <- S.printingOf s registry "Forest"
      discount <- S.printingOf s registry "Synthetic Phyrexian Discount"
      cub <- S.printingOf s registry "Longtusk Cub"
      growth <- S.printingOf s registry "Mutagenic Growth"
      let (cubId, _, gs) = phyrexianDiscountBoard forest discount cub growth 1 2
          paid = S.runPure S.identityAnswer gs (S.cast S.alice cubId)
      Spec.assertEqWith s "one Forest tapped, not two" (S.tappedCount S.alice paid) 1

-- alice controls `copies` Synthetic Snow Discounts and `n` untapped Forests; her
-- hand holds one Longtusk Cub ({1}{G} green Cat) and one Goblin Piker ({1}{R}
-- red Goblin). Loaded fresh inside each case that needs it -- equivalent because
-- loading is deterministic and cached (batch-recipe.md).
snowDiscountBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> Int -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
snowDiscountBoard forest discount cub piker copies n =
  let base = S.landsInPlay forest n
      put g _ = snd (S.addPermanent discount S.alice g)
      withCopies = List.foldl' put base [1 .. copies]
      (cubId, gs1) = S.addHandCard cub S.alice withCopies
      (pikerId, gs2) = S.addHandCard piker S.alice gs1
   in ( cubId,
        pikerId,
        gs2
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Synthetic Snow Discount {2} Artifact: "Green spells you cast cost {S} less to
-- cast."
--
-- SYNTHETIC, and the file's second. CR 118.7g exists to say what a reduction
-- written with snow mana symbols does, so nothing in the rules forbids the
-- printing -- the pool merely lacks one. All 46 cards whose oracle text carries
-- an {S} (Scryfall, 2026-08-05, digital-only printings included) spend it in a
-- cost or measure how much of it was spent, never as the amount of a reduction,
-- and no card's text contains "{S} less".
--
-- The criterion is green, and which criterion it is does not matter to CR
-- 118.7g: what the rule turns the {S} into is GENERIC mana, which is no more
-- particular about the spell than about the cost. What DOES matter is the spell
-- the reduction is aimed at, and green puts Longtusk Cub's {1}{G} in hand -- a
-- cost with a generic component, where 0-vs-1 is directly visible. Aim the same
-- reduction at a cost without one and both readings agree, which is the trap
-- this group's cases are shaped to avoid.
snowDiscountSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
snowDiscountSpec s registry =
  Spec.describe s "SyntheticSnowDiscount" $ do
    -- THE HEADLINE FALSIFIER. CR 118.7g reduces the cost by one GENERIC mana,
    -- so {1}{G} loses its {1} and keeps its {G}. Reading the {S} as nothing at
    -- all -- what the arm did before -- leaves {1}{G} untouched, and reading it
    -- as a typed symbol would have to name a type CR 107.4h says it has not
    -- got.
    Spec.it s "CR 118.7g a reduction written {S} takes one generic mana off the cost" $ do
      forest <- S.printingOf s registry "Forest"
      discount <- S.printingOf s registry "Synthetic Snow Discount"
      cub <- S.printingOf s registry "Longtusk Cub"
      piker <- S.printingOf s registry "Goblin Piker"
      let (cubId, _, gs) = snowDiscountBoard forest discount cub piker 1 2
      Spec.assertEqWith
        s
        "{1}{G} becomes {G}"
        (totalManaCost S.alice cubId (ManaCost.MkManaCost [ManaSymbol.Generic 1, green]) gs)
        (Just (ManaCost.MkManaCost [green]))

    -- The control the case above needs: without the artifact the same spell is
    -- full price, so the {1} really did leave because of the reduction.
    Spec.it s "without the reducer the same spell is full price" $ do
      forest <- S.printingOf s registry "Forest"
      discount <- S.printingOf s registry "Synthetic Snow Discount"
      cub <- S.printingOf s registry "Longtusk Cub"
      piker <- S.printingOf s registry "Goblin Piker"
      let (cubId, _, gs) = snowDiscountBoard forest discount cub piker 0 2
      Spec.assertEqWith
        s
        "{1}{G} stays {1}{G}"
        (totalManaCost S.alice cubId (ManaCost.MkManaCost [ManaSymbol.Generic 1, green]) gs)
        (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, green]))

    -- The colour criterion, and unlike the Phyrexian group's it DISCRIMINATES:
    -- a generic reduction does not care what the cost prints, so {1}{R} would
    -- lose its {1} just as {1}{G} does if the Filter let the reduction reach
    -- it. Flipping this card's HasColor Green to Red breaks this case and the
    -- headline both.
    Spec.it s "a red spell fails the effect's criterion, so its generic component survives" $ do
      forest <- S.printingOf s registry "Forest"
      discount <- S.printingOf s registry "Synthetic Snow Discount"
      cub <- S.printingOf s registry "Longtusk Cub"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, pikerId, gs) = snowDiscountBoard forest discount cub piker 1 2
      Spec.assertEqWith
        s
        "{1}{R} stays {1}{R}"
        (totalManaCost S.alice pikerId (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]) gs)
        (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]))

    -- BOTH cost sites, one scenario, exactly as the Edgewalker and Phyrexian
    -- groups test them. One Forest is the amount that tells {G} apart from
    -- {1}{G}.
    Spec.it s "CR 601.2f castability is measured against the total cost" $ do
      forest <- S.printingOf s registry "Forest"
      discount <- S.printingOf s registry "Synthetic Snow Discount"
      cub <- S.printingOf s registry "Longtusk Cub"
      piker <- S.printingOf s registry "Goblin Piker"
      let (discounted, _, withDiscount) = snowDiscountBoard forest discount cub piker 1 1
          (undiscounted, _, bare) = snowDiscountBoard forest discount cub piker 0 1
      Spec.assertBool s (not (S.castable S.alice undiscounted bare)) "one Forest cannot pay a printed {1}{G}"
      Spec.assertBool s (S.castable S.alice discounted withDiscount) "but it can pay the discounted {G}"

    Spec.it s "CR 601.2f payment spends the total cost" $ do
      forest <- S.printingOf s registry "Forest"
      discount <- S.printingOf s registry "Synthetic Snow Discount"
      cub <- S.printingOf s registry "Longtusk Cub"
      piker <- S.printingOf s registry "Goblin Piker"
      let (cubId, _, gs) = snowDiscountBoard forest discount cub piker 1 2
          paid = S.runPure S.identityAnswer gs (S.cast S.alice cubId)
      Spec.assertEqWith s "one Forest tapped, not two" (S.tappedCount S.alice paid) 1

-- Answers CR 118.7e's Prompt.ChooseReductionHalf with `half` whenever it is on
-- offer, and defers everything else to S.identityAnswer -- the `announces` shape
-- Pawl.ManaSpec uses for CR 118.13a's announcements.
--
-- The "whenever it is on offer" is what makes the pairs below discriminating: an
-- interpreter that named a half the symbol does not have would silently get the
-- first one instead, and the two cases would stop disagreeing.
takesHalf :: ManaSymbol.ManaSymbol -> Prompt.Prompt r -> r
takesHalf half p = case p of
  Prompt.ChooseReductionHalf _ _ _ _ offers ->
    if elem half offers then half else NonEmpty.head offers
  _ -> S.identityAnswer p

-- alice controls `copies` reducers and `n` untapped lands of one printing; her
-- hand holds one `spell` and one Sol Ring ({1} colourless Artifact). Shared by
-- the two hybrid-reduction groups below, which differ in which hybrid symbol
-- their reducer is written with and in which black spell that symbol is aimed
-- at. The land is a Swamp everywhere but the one gate case that needs a board
-- producing no black mana. Loaded fresh inside each case that needs it --
-- equivalent because loading is deterministic and cached (batch-recipe.md).
hybridDiscountBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> Int -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
hybridDiscountBoard land discount spell solRing copies n =
  let base = S.landsInPlay land n
      put g _ = snd (S.addPermanent discount S.alice g)
      withCopies = List.foldl' put base [1 .. copies]
      (spellId, gs1) = S.addHandCard spell S.alice withCopies
      (ringId, gs2) = S.addHandCard solRing S.alice gs1
   in ( spellId,
        ringId,
        gs2
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Synthetic Monocolored Hybrid Discount {2} Artifact: "Black spells you cast
-- cost {2/B} less to cast."
--
-- SYNTHETIC, and the file's third. CR 118.7e exists to say what a reduction
-- written with a hybrid mana symbol does, so nothing in the rules forbids the
-- printing -- the pool merely lacks one. Of the 38,542 cards in Scryfall's
-- Oracle Cards bulk (2026-08-06, which carries un-set, playtest and digital-only
-- printings), not one states a reduction whose amount contains a hybrid symbol;
-- the three whose text puts a hybrid symbol in the same sentence as "less"
-- (Fiend Artisan, Eagle's Rescue, Reaping Willow) all spend it in an activation
-- cost beside a "mana value N or less".
--
-- {2/B} is CR 118.7e's own worked example, and the shape whose two halves differ
-- in the NUMBER of mana they take: "one black mana" against "two generic mana".
-- Khabál Ghoul's {2}{B} is the cost both halves bite -- it has a black symbol
-- for the one and a generic component of two for the other -- which is what
-- makes the pair below disagree by a whole land. THREE SWAMPS is the board that
-- tells the three readings apart: full price taps three, the {B} half taps two,
-- the {2} half taps one.
--
-- CR 118.7b-c cannot reach this group: the {2} half is generic mana and the {B}
-- half finds a black symbol waiting for it, so neither half is ever the excess
-- that rule spills onto the generic component.
monocoloredHybridDiscountSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
monocoloredHybridDiscountSpec s registry =
  Spec.describe s "SyntheticMonocoloredHybridDiscount" $ do
    -- THE HEADLINE FALSIFIER, and it is a GAMEPLAY-level one because CR 118.7e's
    -- choice is only made on the path that pays: taking the {2} half leaves
    -- {2}{B} as {B}, one Swamp. Reading the symbol as nothing at all -- what the
    -- arm did before -- leaves all three tapped.
    Spec.it s "CR 118.7e a {2/B} reduction taken as {2} takes two generic mana off the cost" $ do
      swamp <- S.printingOf s registry "Swamp"
      discount <- S.printingOf s registry "Synthetic Monocolored Hybrid Discount"
      ghoul <- S.printingOf s registry "Khabál Ghoul"
      solRing <- S.printingOf s registry "Sol Ring"
      let (ghoulId, _, gs) = hybridDiscountBoard swamp discount ghoul solRing 1 3
          paid = S.runPure (takesHalf (ManaSymbol.Generic 2)) gs (S.cast S.alice ghoulId)
      Spec.assertEqWith s "one Swamp tapped, not three" (S.tappedCount S.alice paid) 1

    -- THE OTHER HALF of the same symbol, on the same board, and the pair is what
    -- proves the ANSWER is what decides: CR 118.7e's coloured half takes one
    -- black mana, so {2}{B} becomes {2} and two Swamps pay it. An engine that
    -- picked a half for the player could not make both cases pass.
    Spec.it s "CR 118.7e the same reduction taken as {B} takes one black mana instead" $ do
      swamp <- S.printingOf s registry "Swamp"
      discount <- S.printingOf s registry "Synthetic Monocolored Hybrid Discount"
      ghoul <- S.printingOf s registry "Khabál Ghoul"
      solRing <- S.printingOf s registry "Sol Ring"
      let (ghoulId, _, gs) = hybridDiscountBoard swamp discount ghoul solRing 1 3
          paid = S.runPure (takesHalf black) gs (S.cast S.alice ghoulId)
      Spec.assertEqWith s "two Swamps tapped, not one and not three" (S.tappedCount S.alice paid) 2

    -- The control both cases above need: without the artifact the same spell is
    -- full price whatever the interpreter would have answered, so the mana
    -- really did leave because of the reduction.
    Spec.it s "without the reducer the same spell is full price" $ do
      swamp <- S.printingOf s registry "Swamp"
      discount <- S.printingOf s registry "Synthetic Monocolored Hybrid Discount"
      ghoul <- S.printingOf s registry "Khabál Ghoul"
      solRing <- S.printingOf s registry "Sol Ring"
      let (ghoulId, _, gs) = hybridDiscountBoard swamp discount ghoul solRing 0 3
          paid = S.runPure (takesHalf (ManaSymbol.Generic 2)) gs (S.cast S.alice ghoulId)
      Spec.assertEqWith s "three Swamps tapped" (S.tappedCount S.alice paid) 3

    -- The colour criterion, and it DISCRIMINATES here where the Phyrexian
    -- group's could not: the {2} half is generic mana, which does not care what
    -- the cost prints, so Sol Ring's {1} would go to {0} and tap nothing at all
    -- if the Filter let the reduction reach a colourless spell.
    Spec.it s "a colourless spell fails the effect's criterion, so it pays in full" $ do
      swamp <- S.printingOf s registry "Swamp"
      discount <- S.printingOf s registry "Synthetic Monocolored Hybrid Discount"
      ghoul <- S.printingOf s registry "Khabál Ghoul"
      solRing <- S.printingOf s registry "Sol Ring"
      let (_, ringId, gs) = hybridDiscountBoard swamp discount ghoul solRing 1 3
          paid = S.runPure (takesHalf (ManaSymbol.Generic 2)) gs (S.cast S.alice ringId)
      Spec.assertEqWith s "one Swamp tapped, not none" (S.tappedCount S.alice paid) 1

    -- CR 118.7e AT THE GATE. Two Swamps cannot pay Khabál Ghoul's printed
    -- {2}{B}, and either half of the reduction brings it into range: the {B}
    -- half leaves {2} and the {2} half leaves {B}. The control differs in ONE
    -- thing, the reducer's presence.
    Spec.it s "CR 118.7e a hybrid reduction makes a spell castable that full price does not" $ do
      swamp <- S.printingOf s registry "Swamp"
      discount <- S.printingOf s registry "Synthetic Monocolored Hybrid Discount"
      ghoul <- S.printingOf s registry "Khabál Ghoul"
      solRing <- S.printingOf s registry "Sol Ring"
      let (discounted, _, withDiscount) = hybridDiscountBoard swamp discount ghoul solRing 1 2
          (undiscounted, _, bare) = hybridDiscountBoard swamp discount ghoul solRing 0 2
      Spec.assertBool s (not (S.castable S.alice undiscounted bare)) "two Swamps cannot pay a printed {2}{B}"
      Spec.assertBool s (S.castable S.alice discounted withDiscount) "but they can pay it reduced"

    -- WHICH half the gate has to reach for. One Swamp pays only the {2} half's
    -- {B}: the {B} half leaves {2}, which one Swamp cannot pay. So a gate that
    -- resolved the symbol one fixed way -- or took the first resolution -- would
    -- answer False here, where CR 118.7e gives the choice to the payer and makes
    -- the honest question whether SOME resolution pays.
    Spec.it s "CR 118.7e the gate reaches the half that pays and not merely the first" $ do
      swamp <- S.printingOf s registry "Swamp"
      discount <- S.printingOf s registry "Synthetic Monocolored Hybrid Discount"
      ghoul <- S.printingOf s registry "Khabál Ghoul"
      solRing <- S.printingOf s registry "Sol Ring"
      let (discounted, _, withDiscount) = hybridDiscountBoard swamp discount ghoul solRing 1 1
          (undiscounted, _, bare) = hybridDiscountBoard swamp discount ghoul solRing 0 1
      Spec.assertBool s (not (S.castable S.alice undiscounted bare)) "one Swamp cannot pay a printed {2}{B}"
      Spec.assertBool s (S.castable S.alice discounted withDiscount) "but it can pay the {2} half's {B}"

    -- THE OTHER DIRECTION, and the reason the gate cannot pick a half either:
    -- two Radiant Fountains make {C}{C}, which pays the {B} half's leftover {2}
    -- and cannot pay the {2} half's leftover {B}. With the case above, no fixed
    -- half answers both.
    Spec.it s "CR 118.7e the gate reaches the coloured half too" $ do
      fountain <- S.printingOf s registry "Radiant Fountain"
      discount <- S.printingOf s registry "Synthetic Monocolored Hybrid Discount"
      ghoul <- S.printingOf s registry "Khabál Ghoul"
      solRing <- S.printingOf s registry "Sol Ring"
      let (discounted, _, withDiscount) = hybridDiscountBoard fountain discount ghoul solRing 1 2
          (undiscounted, _, bare) = hybridDiscountBoard fountain discount ghoul solRing 0 2
      Spec.assertBool s (not (S.castable S.alice undiscounted bare)) "colourless mana cannot pay a printed {2}{B}"
      Spec.assertBool s (S.castable S.alice discounted withDiscount) "but it can pay the {B} half's {2}"

    -- The negative the two cases above need, and it differs from them in the
    -- mana alone: with no Swamps at all neither half is payable, so a gate that
    -- answered True whenever a resolution EXISTS fails here.
    Spec.it s "CR 118.7e the gate offers no cast that no half can pay" $ do
      swamp <- S.printingOf s registry "Swamp"
      discount <- S.printingOf s registry "Synthetic Monocolored Hybrid Discount"
      ghoul <- S.printingOf s registry "Khabál Ghoul"
      solRing <- S.printingOf s registry "Sol Ring"
      let (ghoulId, _, gs) = hybridDiscountBoard swamp discount ghoul solRing 1 0
      Spec.assertBool s (not (S.castable S.alice ghoulId gs)) "no mana pays either half"

    -- THE GATE AND THE PAYMENT AGREE, which is the pair that matters: on the
    -- board the gate now says yes to, both of CR 118.7e's answers complete, and
    -- they tap different numbers of Swamps. A gate more permissive than the
    -- payment would leave one of these two casts unpaid.
    Spec.it s "CR 118.7e a cast the gate allows is one both halves can pay" $ do
      swamp <- S.printingOf s registry "Swamp"
      discount <- S.printingOf s registry "Synthetic Monocolored Hybrid Discount"
      ghoul <- S.printingOf s registry "Khabál Ghoul"
      solRing <- S.printingOf s registry "Sol Ring"
      let (ghoulId, _, gs) = hybridDiscountBoard swamp discount ghoul solRing 1 2
          takenAsGeneric = S.runPure (takesHalf (ManaSymbol.Generic 2)) gs (S.cast S.alice ghoulId)
          takenAsBlack = S.runPure (takesHalf black) gs (S.cast S.alice ghoulId)
      Spec.assertBool s (S.castable S.alice ghoulId gs) "the gate allows the cast"
      Spec.assertEqWith s "the {2} half taps one Swamp" (S.tappedCount S.alice takenAsGeneric) 1
      Spec.assertEqWith s "the {B} half taps both" (S.tappedCount S.alice takenAsBlack) 2

-- Synthetic Hybrid Discount {2} Artifact: "Black spells you cast cost {W/B}
-- less to cast."
--
-- SYNTHETIC, and the file's fourth, for the reason the group above gives: the
-- same Scryfall sweep finds no printing whose reduction amount is a hybrid
-- symbol of either shape.
--
-- CR 107.4e's COLOUR/COLOUR half ({W/B}), whose two ways are two colours rather
-- than a colour against a number. That is what makes it a different question
-- from the group above and not a relabelling of it, and why CR 118.7e's prompt
-- answers with the resulting SYMBOL: {2/B}'s halves are an OfType and a Generic,
-- {W/B}'s are two OfTypes.
--
-- Aimed at Withered Wretch's {B}{B}, which prints one of the two colours and not
-- the other, and NO GENERIC COMPONENT. Both of those are deliberate. Printing
-- one colour is what lets the count tell the two answers apart -- a cost
-- printing both would take one mana either way. Printing no generic component
-- isolates CR 118.7e's choice from CR 118.7b's spill: a stranded {W} becomes one
-- GENERIC mana, and this cost has none for CR 118.7a to take it off, so the
-- white half leaves {B}{B} alone whichever way the spill is read.
--
-- The SPILL itself is the last case below, on Khabál Ghoul's {2}{B} instead --
-- the same reduction and the same white half, aimed at a cost that does have a
-- generic component for CR 118.7b to reach.
hybridDiscountSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
hybridDiscountSpec s registry =
  Spec.describe s "SyntheticHybridDiscount" $ do
    -- THE HEADLINE FALSIFIER for CR 107.4e's colour/colour half: the black half
    -- of {W/B} takes one black mana, so {B}{B} becomes {B} and one Swamp pays
    -- it. Reading the symbol as nothing -- what the arm did before -- taps both.
    Spec.it s "CR 118.7e a {W/B} reduction taken as {B} takes one black mana off the cost" $ do
      swamp <- S.printingOf s registry "Swamp"
      discount <- S.printingOf s registry "Synthetic Hybrid Discount"
      wretch <- S.printingOf s registry "Withered Wretch"
      solRing <- S.printingOf s registry "Sol Ring"
      let (wretchId, _, gs) = hybridDiscountBoard swamp discount wretch solRing 1 2
          paid = S.runPure (takesHalf black) gs (S.cast S.alice wretchId)
      Spec.assertEqWith s "one Swamp tapped, not two" (S.tappedCount S.alice paid) 1

    -- THE ENGINE DOES NOT PICK THE BETTER HALF. CR 118.7e gives the choice to
    -- the player paying with no condition attached, so a payer who names the
    -- white half of {W/B} against a cost printing no {W} gets a reduction that
    -- takes nothing -- and pays both Swamps. This case fails if the engine
    -- silently takes the half the cost can use, which is exactly what a
    -- payability filter on the offers would have made it do.
    Spec.it s "CR 118.7e the same reduction taken as {W} finds no white mana to take" $ do
      swamp <- S.printingOf s registry "Swamp"
      discount <- S.printingOf s registry "Synthetic Hybrid Discount"
      wretch <- S.printingOf s registry "Withered Wretch"
      solRing <- S.printingOf s registry "Sol Ring"
      let (wretchId, _, gs) = hybridDiscountBoard swamp discount wretch solRing 1 2
          paid = S.runPure (takesHalf white) gs (S.cast S.alice wretchId)
      Spec.assertEqWith s "two Swamps tapped" (S.tappedCount S.alice paid) 2

    -- The control the headline needs: with no reducer out, two Swamps is what
    -- the spell costs whatever the interpreter would have answered.
    Spec.it s "without the reducer the same spell is full price" $ do
      swamp <- S.printingOf s registry "Swamp"
      discount <- S.printingOf s registry "Synthetic Hybrid Discount"
      wretch <- S.printingOf s registry "Withered Wretch"
      solRing <- S.printingOf s registry "Sol Ring"
      let (wretchId, _, gs) = hybridDiscountBoard swamp discount wretch solRing 0 2
          paid = S.runPure (takesHalf black) gs (S.cast S.alice wretchId)
      Spec.assertEqWith s "two Swamps tapped" (S.tappedCount S.alice paid) 2

    -- CR 118.7b AT THE BOARD, and the file's proof of the spill. Khabál Ghoul
    -- ({2}{B} Creature -- Zombie, "At the beginning of each end step, put a
    -- +1/+1 counter on this creature for each creature that died this turn." --
    -- checked against Scryfall, 2026-08-20) is black, so the same artifact
    -- discounts it, and its {2} is the generic component Withered Wretch above
    -- deliberately lacks. Taking the WHITE half leaves a reduction of one white
    -- mana against a cost requiring none, which CR 118.7b turns into one generic
    -- mana: {2}{B} becomes {1}{B} and two Swamps pay it. Dropping the stranded
    -- half instead leaves {2}{B} and taps all three.
    Spec.it s "CR 118.7b a stranded {W} half comes off the generic component" $ do
      swamp <- S.printingOf s registry "Swamp"
      discount <- S.printingOf s registry "Synthetic Hybrid Discount"
      ghoul <- S.printingOf s registry "Khabál Ghoul"
      solRing <- S.printingOf s registry "Sol Ring"
      let (ghoulId, _, gs) = hybridDiscountBoard swamp discount ghoul solRing 1 3
          paid = S.runPure (takesHalf white) gs (S.cast S.alice ghoulId)
      Spec.assertEqWith s "two Swamps tapped, not three" (S.tappedCount S.alice paid) 2

    -- The control that case needs, the two boards differing only in whether the
    -- reducer is out: the full {2}{B} taps all three Swamps.
    Spec.it s "without the reducer the same Ghoul is full price" $ do
      swamp <- S.printingOf s registry "Swamp"
      discount <- S.printingOf s registry "Synthetic Hybrid Discount"
      ghoul <- S.printingOf s registry "Khabál Ghoul"
      solRing <- S.printingOf s registry "Sol Ring"
      let (ghoulId, _, gs) = hybridDiscountBoard swamp discount ghoul solRing 0 3
          paid = S.runPure (takesHalf white) gs (S.cast S.alice ghoulId)
      Spec.assertEqWith s "three Swamps tapped" (S.tappedCount S.alice paid) 3

-- Aims the text changer's one target slot at `oid` -- the SpellsAndPermanents
-- pool's recipient shape, which both changers below print -- and answers whichever
-- family's swap prompt the changer asks with (from, to). BOTH prompts are
-- answered because the group casts an Artificial Evolution in one case and a
-- Magical Hack in another; a changer asks only its own.
swapAt :: ObjectId.ObjectId -> Subtype.Subtype -> Subtype.Subtype -> Prompt.Prompt r -> r
swapAt oid from to p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  Prompt.ChooseCreatureTypeSwap {} -> (from, to)
  Prompt.ChooseLandTypeSwap {} -> (from, to)
  _ -> S.identityAnswer p

-- alice controls one Edgewalker, one untapped Plains and one untapped Island;
-- her hand holds a second Edgewalker ({1}{W}{B} Human Cleric), a Whipstitched
-- Zombie ({1}{B} Zombie) and the text changer `changerName`. With `swap`, she
-- casts that changer at the Edgewalker ON THE BATTLEFIELD -- the Island pays the
-- {U} -- and it resolves before anything is measured; without it the board is
-- otherwise identical, which is what makes the two comparable.
--
-- Returns the state, the Edgewalker printing the discount, the Cleric spell and
-- the Zombie spell. Loaded fresh inside each case that needs it -- equivalent
-- because loading is deterministic and cached (batch-recipe.md).
textChangedEdgewalkerBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  String ->
  Maybe (Subtype.Subtype, Subtype.Subtype) ->
  m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
textChangedEdgewalkerBoard s registry changerName swap = do
  plains <- S.printingOf s registry "Plains"
  island <- S.printingOf s registry "Island"
  edgewalker <- S.printingOf s registry "Edgewalker"
  zombie <- S.printingOf s registry "Whipstitched Zombie"
  changer <- S.printingOf s registry changerName
  let (_, g1) = S.addPermanent island S.alice (S.landsInPlay plains 1)
      (walkerId, g2) = S.addPermanent edgewalker S.alice g1
      (clericSpell, g3) = S.addHandCard edgewalker S.alice g2
      (zombieSpell, g4) = S.addHandCard zombie S.alice g3
      (changerId, g5) = S.addHandCard changer S.alice g4
      ready =
        g5
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      after = case swap of
        Nothing -> ready
        Just (from, to) ->
          S.runPure (swapAt walkerId from to) ready $ do
            S.cast S.alice changerId
            Stack.resolveTop
  pure (after, walkerId, clericSpell, zombieSpell)

-- Answers CR 118.7e's half with `half` and CR 601.2f's order with the total
-- `cost`, deferring everything else to S.identityAnswer. TWO prompts in one
-- cast, and both have to be answered for the pair below to be about the order
-- rather than about the half.
takesHalfAndCost :: ManaSymbol.ManaSymbol -> ManaCost.ManaCost -> Prompt.Prompt r -> r
takesHalfAndCost half cost p = case p of
  Prompt.ChooseReducedCost _ _ _ offers ->
    if elem cost offers then cost else NonEmpty.head offers
  _ -> takesHalf half p

-- alice controls one Edgewalker, one Synthetic Monocolored Hybrid Discount and
-- two untapped Swamps; her hand holds a Cabal Evangel ({1}{B} Creature -- Human
-- Cleric 2/2, vanilla -- checked against Scryfall, 2026-08-20). Loaded fresh
-- inside each case that needs it -- equivalent because loading is deterministic
-- and cached (batch-recipe.md).
mixedReductionBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
mixedReductionBoard swamp edgewalker discount evangel =
  let base = S.landsInPlay swamp 2
      (_, g1) = S.addPermanent edgewalker S.alice base
      (_, g2) = S.addPermanent discount S.alice g1
      (evangelId, g3) = S.addHandCard evangel S.alice g2
   in ( evangelId,
        g3
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- CR 601.2f's "if multiple cost reductions apply, the player may apply them in
-- any order", where the order is observable BECAUSE the two reducers disagree
-- about CR 101.1's coloured-mana confinement.
--
-- Cabal Evangel is a black Cleric with a generic component, so both reducers
-- match it: Edgewalker's {W}{B} confined to the coloured mana paid, and the
-- Synthetic Monocolored Hybrid Discount's {2/B} taken as {B}, which is not.
-- Against {1}{B} the two orders part company -- Edgewalker first takes the black
-- symbol, leaving the unconfined {B} nothing to take and CR 118.7b to spill it
-- onto the {1}, for {0}; the unconfined one first takes the black symbol, and
-- Edgewalker's own sentence then strands both its halves, for {1}.
--
-- That is the whole reason `reductionOrders` prunes on the confinement as well
-- as on the floor. Two reducers agreeing on both commute, and pruning them costs
-- the payer nothing; these two do not, and pruning them would be pawl choosing.
mixedReductionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mixedReductionSpec s registry =
  Spec.describe s "MixedConfinementReductions" $ do
    -- THE HEADLINE FALSIFIER: the payer names {0} and pays no mana at all. An
    -- engine that pruned the order away would fold in the gathered order and
    -- never ask, so one of this pair would come out with the other's count.
    Spec.it s "CR 601.2f the payer may apply the confined reduction first, for {0}" $ do
      swamp <- S.printingOf s registry "Swamp"
      edgewalker <- S.printingOf s registry "Edgewalker"
      discount <- S.printingOf s registry "Synthetic Monocolored Hybrid Discount"
      evangel <- S.printingOf s registry "Cabal Evangel"
      let (evangelId, gs) = mixedReductionBoard swamp edgewalker discount evangel
          paid = S.runPure (takesHalfAndCost black (ManaCost.MkManaCost [])) gs (S.cast S.alice evangelId)
      Spec.assertEqWith s "no Swamp tapped" (S.tappedCount S.alice paid) 0
      -- The anti-vacuity check: an untapped board also describes a cast that
      -- never happened.
      Spec.assertEqWith s "and the Evangel left the hand" (S.handSize S.alice paid) 0

    -- THE OTHER ORDER, same board and same half, and the pair is what proves the
    -- ANSWER decides: applying the unconfined {B} first leaves Edgewalker's
    -- {W}{B} with nothing to take and nothing to spill onto, so the {1} stands.
    Spec.it s "CR 601.2f or the unconfined one first, for {1}" $ do
      swamp <- S.printingOf s registry "Swamp"
      edgewalker <- S.printingOf s registry "Edgewalker"
      discount <- S.printingOf s registry "Synthetic Monocolored Hybrid Discount"
      evangel <- S.printingOf s registry "Cabal Evangel"
      let (evangelId, gs) = mixedReductionBoard swamp edgewalker discount evangel
          paid = S.runPure (takesHalfAndCost black (ManaCost.MkManaCost [ManaSymbol.Generic 1])) gs (S.cast S.alice evangelId)
      Spec.assertEqWith s "one Swamp tapped" (S.tappedCount S.alice paid) 1
      Spec.assertEqWith s "and the Evangel left the hand" (S.handSize S.alice paid) 0

-- CR 612.1 reaching the FILTER a player static ability's effect carries.
--
-- Edgewalker's "Cleric spells you cast cost {W}{B} less to cast" names which
-- spells it discounts with a creature type word, and CR 612.1 gives a
-- text-changing effect "any words or symbols printed on that object" -- so an
-- Artificial Evolution ({U} Instant, "Change the text of target spell or
-- permanent by replacing all instances of one creature type with another. The
-- new creature type can't be Wall." -- checked against Scryfall, 2026-08-05)
-- resolved at the Edgewalker moves the discount off Clerics and onto the new
-- word.
--
-- Cleric -> Zombie rather than Cleric -> Wizard, which is the same swap with a
-- word the discount cannot be seen through: Edgewalker reduces by {W}{B}, and
-- its own sentence confines that to the coloured mana paid (CR 101.1), so a
-- stranded half never reaches the generic component and only a white or black
-- spell shows the difference at all. Every Wizard in the pool is mono-blue; Whipstitched Zombie ({1}{B}
-- Creature -- Zombie 2/2, "At the beginning of your upkeep, sacrifice this
-- creature unless you pay {B}." -- checked against Scryfall, 2026-08-05) is the
-- black Zombie that makes the new word observable, and its upkeep trigger never
-- fires here because nothing in the group reaches an upkeep.
--
-- BOTH HALVES are asserted every time. "The Zombie spell is discounted" passes
-- vacuously against a reader that discounts everything, and "the Cleric spell is
-- not" passes vacuously against one that discounts nothing.
textChangedEdgewalkerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
textChangedEdgewalkerSpec s registry = Spec.describe s "TextChangedEdgewalker" $ do
  -- The premise the two changed cases are read against: with no text changer the
  -- printed word stands, so the Cleric spell is discounted and the Zombie is not.
  Spec.it s "CR 118.7 the printed filter discounts Clerics and not Zombies" $ do
    (gs, _, clericSpell, zombieSpell) <- textChangedEdgewalkerBoard s registry "Artificial Evolution" Nothing
    Spec.assertEqWith
      s
      "the Cleric spell's {1}{W}{B} becomes {1}"
      (totalManaCost S.alice clericSpell (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black]) gs)
      (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1]))
    Spec.assertEqWith
      s
      "and the Zombie spell's {1}{B} is untouched"
      (totalManaCost S.alice zombieSpell (ManaCost.MkManaCost [ManaSymbol.Generic 1, black]) gs)
      (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, black]))

  Spec.it s "CR 612.1 an evolved Edgewalker discounts Zombies and no longer discounts Clerics" $ do
    (gs, walkerId, clericSpell, zombieSpell) <-
      textChangedEdgewalkerBoard s registry "Artificial Evolution" (Just (Subtype.Cleric, Subtype.Zombie))
    -- The anti-vacuity check, first: every assertion below would also hold of an
    -- Evolution that never resolved onto the Edgewalker at all.
    Spec.assertEqWith s "the Evolution resolved onto the Edgewalker" (Projection.textChangesAffecting walkerId gs) [(Subtype.Cleric, Subtype.Zombie)]
    Spec.assertEqWith
      s
      "the Zombie spell's {1}{B} becomes {1}"
      (totalManaCost S.alice zombieSpell (ManaCost.MkManaCost [ManaSymbol.Generic 1, black]) gs)
      (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1]))
    Spec.assertEqWith
      s
      "and the Cleric spell pays its printed {1}{W}{B}"
      (totalManaCost S.alice clericSpell (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black]) gs)
      (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black]))

  -- CR 612.2's family gate, at this read point: a text-changing effect "changes
  -- only those words that are used in the correct way", and the rule's own
  -- examples are "a land type word used as a land type" and "a creature type word
  -- used as a creature type". Magical Hack ({U} Instant, "Change the
  -- text of target spell or permanent by replacing all instances of one basic
  -- land type with another." -- checked against Scryfall, 2026-08-05) can only
  -- name a basic land type, so the pair it imposes reaches no creature type
  -- position and the discount stays on Clerics.
  Spec.it s "CR 612.2 a land-type pair leaves the creature-type filter alone" $ do
    (gs, walkerId, clericSpell, zombieSpell) <-
      textChangedEdgewalkerBoard s registry "Magical Hack" (Just (Subtype.Swamp, Subtype.Island))
    Spec.assertEqWith s "the Hack resolved onto the Edgewalker" (Projection.textChangesAffecting walkerId gs) [(Subtype.Swamp, Subtype.Island)]
    Spec.assertEqWith
      s
      "the Cleric spell is discounted exactly as printed"
      (totalManaCost S.alice clericSpell (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black]) gs)
      (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1]))
    Spec.assertEqWith
      s
      "and the Zombie spell is still no business of the Edgewalker's"
      (totalManaCost S.alice zombieSpell (ManaCost.MkManaCost [ManaSymbol.Generic 1, black]) gs)
      (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, black]))

  -- The whole-card case: CR 601.2f measures castability and payment against the
  -- TOTAL cost, so the rewritten filter decides which spell alice can actually
  -- cast. One Plains is her only untapped mana once the Evolution has tapped the
  -- Island, and a Plains can never pay a {B}: what makes the Zombie castable is
  -- that the reduction removed the black SYMBOL.
  Spec.it s "CR 601.2f whole cards: the rewritten filter decides a real cast" $ do
    (evolved, _, evolvedCleric, evolvedZombie) <-
      textChangedEdgewalkerBoard s registry "Artificial Evolution" (Just (Subtype.Cleric, Subtype.Zombie))
    (printed, _, printedCleric, printedZombie) <- textChangedEdgewalkerBoard s registry "Artificial Evolution" Nothing
    Spec.assertBool s (not (S.castable S.alice printedZombie printed)) "unevolved, the Zombie spell's {B} cannot be paid"
    Spec.assertBool s (S.castable S.alice evolvedZombie evolved) "evolved, the discounted Zombie spell can be cast"
    Spec.assertBool s (S.castable S.alice printedCleric printed) "unevolved, the discounted Cleric spell can be cast"
    Spec.assertBool s (not (S.castable S.alice evolvedCleric evolved)) "evolved, the Cleric spell's {W}{B} cannot be paid"
    -- And the payment really is the reduced one: the Island the Evolution tapped,
    -- plus the single Plains that pays the discounted {1}.
    let paid = S.runPure S.identityAnswer evolved (S.cast S.alice evolvedZombie)
    Spec.assertEqWith s "one Plains tapped on top of the Evolution's Island" (S.tappedCount S.alice paid) 2

-- alice holds nine Plains cards; the board is otherwise empty unless a
-- printing is named. Loaded fresh inside each case that needs it --
-- equivalent because loading is deterministic and cached (batch-recipe.md).
reliquaryHandOfNine :: Printing.Printing -> [Printing.Printing] -> GameState.GameState
reliquaryHandOfNine plains extra =
  let gs0 = Setup.emptyGame S.bothPlayers
      put g printing = snd (S.addPermanent printing S.alice g)
      withExtra = List.foldl' put gs0 extra
      add g _ = snd (S.addHandCard plains S.alice g)
   in List.foldl' add withExtra [1 .. 9 :: Int]

-- The CR 613.10 cases' board: the same nine Plains in hand, plus a Reliquary
-- Tower and Zhao, the Moon Slayer, who carries one counter of each of `kinds`.
-- The runs differ in NOTHING but those counters.
zhaoHandOfNine :: Printing.Printing -> Printing.Printing -> Printing.Printing -> [CounterKind.CounterKind Keyword.Keyword] -> GameState.GameState
zhaoHandOfNine plains reliquaryTower zhao kinds =
  let gs0 = Setup.emptyGame S.bothPlayers
      (_, g1) = S.addPermanent reliquaryTower S.alice gs0
      (zhaoId, g2) = S.addPermanent zhao S.alice g1
      g3 = List.foldl' (\g k -> S.addCounter k 1 zhaoId g) g2 kinds
      add g _ = snd (S.addHandCard plains S.alice g)
   in List.foldl' add g3 [1 .. 9 :: Int]

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

    -- The same strip with a CR 604.2 clause on the stripper, which is what
    -- Projection.setLandSubtypeEffects used to ignore: this reader is outside the
    -- layer fold (CR 613.10), so it asks liveAfterLayers, and liveAfterLayers is
    -- handed the list of subtype-setting effects the gate builds. Wired open, an
    -- ability whose clause was false still stripped the Tower here while
    -- gatherStatic dropped it from the fold -- the two halves of one rule
    -- disagreeing.
    --
    -- The pair differs in ONE thing, the counter on Zhao. CR 122.1 makes a
    -- counter's identity its name, so it is that name the clause reads and
    -- nothing else about the board moves.
    --
    -- Zhao, the Moon Slayer is the printed card that pairs an "as long as"
    -- clause (CR 604.2) with a land-subtype set (CR 305.7); before it landed the
    -- corpus stood in a synthetic for exactly that shape.
    Spec.it s "CR 604.2/305.7 with no counter on Zhao the clause is false, and the Tower keeps its ability" $ do
      plains <- S.printingOf s registry "Plains"
      reliquaryTower <- S.printingOf s registry "Reliquary Tower"
      zhao <- S.printingOf s registry "Zhao, the Moon Slayer"
      let board = zhaoHandOfNine plains reliquaryTower zhao []
      Spec.assertEqWith s "no maximum" (PlayerEffect.maximumHandSize S.alice board) Nothing

    -- The wrong KIND of counter, which is what tells a kind lookup apart from a
    -- "does this permanent have any counters" read: +1/+1 is a kind the engine
    -- itself places and reads, so a bug that counts every kind is guaranteed to
    -- see it.
    Spec.it s "CR 122.1 a +1/+1 counter on Zhao is not a conqueror counter, and the Tower keeps its ability" $ do
      plains <- S.printingOf s registry "Plains"
      reliquaryTower <- S.printingOf s registry "Reliquary Tower"
      zhao <- S.printingOf s registry "Zhao, the Moon Slayer"
      let board = zhaoHandOfNine plains reliquaryTower zhao [CounterKind.PlusOnePlusOne]
      Spec.assertEqWith s "no maximum" (PlayerEffect.maximumHandSize S.alice board) Nothing

    Spec.it s "CR 604.2/305.7 a conqueror counter turns the clause on, and the Tower is stripped" $ do
      plains <- S.printingOf s registry "Plains"
      reliquaryTower <- S.printingOf s registry "Reliquary Tower"
      zhao <- S.printingOf s registry "Zhao, the Moon Slayer"
      let board = zhaoHandOfNine plains reliquaryTower zhao [CounterKind.Named (CounterName.UnsafeMkCounterName (Text.pack "conqueror"))]
      Spec.assertEqWith s "seven again" (PlayerEffect.maximumHandSize S.alice board) (Just 7)

-- alice's board with BOTH maximum-hand-size effects live, built in one of the two
-- orders: `ringsFirst` decides whether The Ten Rings' printed CR 613.7a effect is
-- older or newer than Sea Gate Restoration's stored CR 613.7b one.
--
-- The two CARRIERS are the point, and no board of two permanents can replace them.
-- A permanent takes a fresh ObjectId every time it arrives (Event.placeObject), so
-- GameState.battlefield's Set walks two printed carriers in the very order their
-- timestamps give, and a board of two printed effects cannot tell the readings
-- apart. Printed against STORED can: the gather concatenates the two lists, so
-- without the sort every stored effect is read last whatever its stamp.
--
-- Five Plains in hand, ten in the library: the sorcery draws hand-plus-one, so
-- alice ends on eleven cards -- one over The Ten Rings' maximum, which is what
-- makes the cleanup discard tell the two readings apart. The library is stocked
-- past the draw so CR 104.3c decks nobody.
ringsAndRestoration :: Bool -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId)
ringsAndRestoration ringsFirst plains island tenRings restoration =
  let gs0 = Setup.emptyGame S.bothPlayers
      add printing g _ = snd (S.addPermanent printing S.alice g)
      stockHand g _ = snd (S.addHandCard plains S.alice g)
      stockLibrary g _ = snd (S.addLibraryCard plains S.alice g)
      -- Seven Islands, because the sorcery costs {4}{U}{U}{U} and is CAST rather
      -- than placed on the stack: a modal double-faced card put there by hand has
      -- no chosen half, and CR 712.11b makes that choice part of casting it.
      withLands = List.foldl' (add island) gs0 [1 .. 7 :: Int]
      stocked = List.foldl' stockLibrary (List.foldl' stockHand withLands [1 .. 5 :: Int]) [1 .. 10 :: Int]
      (spellId, withSpell) = S.addHandCard restoration S.alice stocked
      ready =
        withSpell
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      frontName = CardName.MkCardName (Text.pack "Sea Gate Restoration")
      cast g = S.runPure S.identityAnswer g (Cast.castSpell S.manaPerformer S.alice spellId frontName Facing.FaceUp)
      resolve g = S.runPure S.identityAnswer g Stack.resolveTop
      rings = S.addPermanent tenRings S.alice
   in if ringsFirst
        then let (ringsId, withRings) = rings ready in (resolve (cast withRings), ringsId)
        else let (ringsId, withRings) = rings (resolve (cast ready)) in (withRings, ringsId)

-- The Ten Rings, a Legendary Artifact: "Your maximum hand size is ten." Its
-- other line, the end-step draw to ten, is Pawl.TriggerSpec's.
--
-- Sea Gate Restoration, the front face of a modal double-faced card: "Draw cards
-- equal to the number of cards in your hand plus one. You have no maximum hand
-- size for the rest of the game." Only the front face is cast here; the back
-- face's "you may pay 3 life" is exercised in Pawl.ReplacementSpec.
--
-- Together they are the pair CR 613.11's timestamp order decides, one on each
-- carrier: a SET maximum and a REMOVED one disagree, and Reliquary Tower's own
-- ruling says the later of the two wins. Both orders are built, so neither answer
-- can be reached by a fold that ignores the order.
theTenRingsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
theTenRingsSpec s registry =
  Spec.describe s "TheTenRings" $ do
    Spec.it s "CR 402.2 alone it sets the maximum to ten" $ do
      plains <- S.printingOf s registry "Plains"
      tenRings <- S.printingOf s registry "The Ten Rings"
      Spec.assertEqWith s "ten" (PlayerEffect.maximumHandSize S.alice (reliquaryHandOfNine plains [tenRings])) (Just 10)

    -- CR 514.1: nine cards is under the ten it allows, so the cleanup discard
    -- that trims a default hand of nine to seven trims nothing here.
    Spec.it s "CR 514.1 nine cards at cleanup discards nothing" $ do
      plains <- S.printingOf s registry "Plains"
      tenRings <- S.printingOf s registry "The Ten Rings"
      let after = reliquaryCleanup (reliquaryHandOfNine plains [tenRings])
      Spec.assertEqWith s "hand keeps nine" (S.handSize S.alice after) 9
      Spec.assertEqWith s "nothing discarded" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0

    -- CR 109.5: the You scope, as for the Tower. bob's hand is still CR 402.2's
    -- seven.
    Spec.it s "CR 109.5 the opponent's maximum is untouched" $ do
      plains <- S.printingOf s registry "Plains"
      tenRings <- S.printingOf s registry "The Ten Rings"
      Spec.assertEqWith s "seven" (PlayerEffect.maximumHandSize S.bob (reliquaryHandOfNine plains [tenRings])) (Just 7)

    -- The fixture's own claim, asserted rather than assumed: the stored effect
    -- really is the OLDER of the two on this board, so the gather has to move it
    -- ahead of the printed one it is concatenated behind. Without this the case
    -- below could pass on a board where the two orders agreed.
    Spec.it s "CR 613.7 the stored effect is older than the printed one" $ do
      plains <- S.printingOf s registry "Plains"
      tenRings <- S.printingOf s registry "The Ten Rings"
      restoration <- S.printingOf s registry "Sea Gate Restoration"
      island <- S.printingOf s registry "Island"
      let (board, ringsId) = ringsAndRestoration False plains island tenRings restoration
      Spec.assertEqWith s "one stored effect" (fmap ActivePlayerEffect.effect (GameState.playerEffects board)) [PlayerEffect.Type.NoMaximumHandSize]
      Spec.assertEqWith
        s
        "and it began before The Ten Rings entered"
        (fmap (\stored -> Just (ActivePlayerEffect.timestamp stored) < fmap Object.timestamp (Game.lookupObject ringsId board)) (GameState.playerEffects board))
        [True]

    Spec.it s "CR 613.11 The Ten Rings entering after Sea Gate Restoration resolved sets the maximum to ten" $ do
      plains <- S.printingOf s registry "Plains"
      tenRings <- S.printingOf s registry "The Ten Rings"
      restoration <- S.printingOf s registry "Sea Gate Restoration"
      island <- S.printingOf s registry "Island"
      let (board, _) = ringsAndRestoration False plains island tenRings restoration
      Spec.assertEqWith s "the sorcery drew hand-plus-one" (S.handSize S.alice board) 11
      Spec.assertEqWith s "ten" (PlayerEffect.maximumHandSize S.alice board) (Just 10)
      let after = reliquaryCleanup board
      Spec.assertEqWith s "CR 514.1 discards down to ten" (S.handSize S.alice after) 10
      -- The resolved sorcery is the graveyard's other card (CR 608.2n).
      Spec.assertEqWith s "one card discarded" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 2

    -- THE CONTROL, the same board with the two built in the other order and
    -- nothing else changed: same seats, same five Plains held, same ten stocked,
    -- same two cards.
    Spec.it s "CR 613.11 Sea Gate Restoration resolving after The Ten Rings entered removes the maximum" $ do
      plains <- S.printingOf s registry "Plains"
      tenRings <- S.printingOf s registry "The Ten Rings"
      restoration <- S.printingOf s registry "Sea Gate Restoration"
      island <- S.printingOf s registry "Island"
      let (board, _) = ringsAndRestoration True plains island tenRings restoration
      Spec.assertEqWith s "the sorcery drew hand-plus-one" (S.handSize S.alice board) 11
      Spec.assertEqWith s "no maximum" (PlayerEffect.maximumHandSize S.alice board) Nothing
      let after = reliquaryCleanup board
      Spec.assertEqWith s "CR 514.1 discards nothing" (S.handSize S.alice after) 11
      Spec.assertEqWith s "and only the resolved sorcery is in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1

-- The ADJUSTING arms' board: alice controls `extra`, BOTH players hold `held`
-- Plains, and `active` is whose cleanup step runs. Two hands, because the
-- reducing card here is scoped to opponents and CR 514.1 trims the active player
-- alone -- one hand could not show both halves of that.
--
-- The two hands are stocked to the SAME size on purpose, so the two cleanup runs
-- differ in nothing but which seat is active and a scope read backwards has to
-- show up as a different discard rather than as the same one from the other seat.
adjustedHands :: Printing.Printing -> [Printing.Printing] -> Int -> PlayerId.PlayerId -> GameState.GameState
adjustedHands plains extra held active =
  let gs0 = (Setup.emptyGame S.bothPlayers) {GameState.activePlayer = active}
      put g printing = snd (S.addPermanent printing S.alice g)
      withExtra = List.foldl' put gs0 extra
      add pid g _ = snd (S.addHandCard plains pid g)
      forAlice = List.foldl' (add S.alice) withExtra [1 .. held]
   in List.foldl' (add S.bob) forAlice [1 .. held]

-- Minamo Scrollkeeper, a Creature: "Defender / Your maximum hand size is
-- increased by one." The INCREASING arm's producer, and the reason it is this
-- printing rather than Trusted Advisor's "increased by two": both print the same
-- shape, and this one prints nothing else that needs a trigger.
--
-- An adjustment is not a set, and CR 613.11's timestamp order is what tells them
-- apart: laid over The Ten Rings' ten the answer is eleven, and laid under it the
-- ten wins outright -- so both orders are built. Over Reliquary Tower there is no
-- number left to raise at all.
minamoScrollkeeperSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
minamoScrollkeeperSpec s registry =
  Spec.describe s "MinamoScrollkeeper" $ do
    Spec.it s "CR 402.2 alone it raises the maximum to eight" $ do
      plains <- S.printingOf s registry "Plains"
      scrollkeeper <- S.printingOf s registry "Minamo Scrollkeeper"
      Spec.assertEqWith s "eight" (PlayerEffect.maximumHandSize S.alice (reliquaryHandOfNine plains [scrollkeeper])) (Just 8)

    -- The gameplay-level case: the same nine cards the bare board discards two
    -- of, discarding ONE here.
    Spec.it s "CR 514.1 nine cards at cleanup discards down to eight" $ do
      plains <- S.printingOf s registry "Plains"
      scrollkeeper <- S.printingOf s registry "Minamo Scrollkeeper"
      let after = reliquaryCleanup (reliquaryHandOfNine plains [scrollkeeper])
      Spec.assertEqWith s "hand" (S.handSize S.alice after) 8
      Spec.assertEqWith s "one discarded" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1

    -- CR 109.5: the You scope, as for the Tower and the Rings.
    Spec.it s "CR 109.5 the opponent's maximum is untouched" $ do
      plains <- S.printingOf s registry "Plains"
      scrollkeeper <- S.printingOf s registry "Minamo Scrollkeeper"
      Spec.assertEqWith s "seven" (PlayerEffect.maximumHandSize S.bob (reliquaryHandOfNine plains [scrollkeeper])) (Just 7)

    -- CR 613.11 / Reliquary Tower's ruling: an adjustment applied after a removal
    -- adjusts nothing, because the removal left no number. A reading that
    -- restarted from CR 402.2's seven, or from the Tower's absent maximum treated
    -- as a number, would answer eight here.
    Spec.it s "CR 613.11 an increase after Reliquary Tower still leaves no maximum" $ do
      plains <- S.printingOf s registry "Plains"
      reliquaryTower <- S.printingOf s registry "Reliquary Tower"
      scrollkeeper <- S.printingOf s registry "Minamo Scrollkeeper"
      let board = reliquaryHandOfNine plains [reliquaryTower, scrollkeeper]
          after = reliquaryCleanup board
      Spec.assertEqWith s "CR 514.1 discards nothing" (S.handSize S.alice after) 9
      Spec.assertEqWith s "nothing discarded" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0
      Spec.assertEqWith s "no maximum" (PlayerEffect.maximumHandSize S.alice board) Nothing

    -- CR 613.11 with the Scrollkeeper LATER: it raises the ten The Ten Rings set.
    Spec.it s "CR 613.11 an increase after The Ten Rings raises the ten it set" $ do
      plains <- S.printingOf s registry "Plains"
      tenRings <- S.printingOf s registry "The Ten Rings"
      scrollkeeper <- S.printingOf s registry "Minamo Scrollkeeper"
      let board = adjustedHands plains [tenRings, scrollkeeper] 12 S.alice
          after = reliquaryCleanup board
      Spec.assertEqWith s "CR 514.1 discards down to eleven" (S.handSize S.alice after) 11
      Spec.assertEqWith s "one discarded" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
      Spec.assertEqWith s "eleven" (PlayerEffect.maximumHandSize S.alice board) (Just 11)

    -- THE ORDER CONTROL, the same two permanents entering the other way round and
    -- nothing else changed: The Ten Rings SETS, so it overwrites the eight the
    -- Scrollkeeper had already made of seven rather than composing with it.
    Spec.it s "CR 613.11 The Ten Rings entering after the increase sets the maximum to ten outright" $ do
      plains <- S.printingOf s registry "Plains"
      tenRings <- S.printingOf s registry "The Ten Rings"
      scrollkeeper <- S.printingOf s registry "Minamo Scrollkeeper"
      let board = adjustedHands plains [scrollkeeper, tenRings] 12 S.alice
          after = reliquaryCleanup board
      Spec.assertEqWith s "CR 514.1 discards down to ten" (S.handSize S.alice after) 10
      Spec.assertEqWith s "two discarded" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 2
      Spec.assertEqWith s "ten" (PlayerEffect.maximumHandSize S.alice board) (Just 10)

-- Gnat Miser, a Creature: "Each opponent's maximum hand size is reduced by one."
-- The REDUCING arm's producer, and the OPPONENTS scope on the same axis -- the
-- two things the increase above cannot show. Chosen over Thought Nibbler's
-- self-scoped "reduced by two" because it brings the scope with it and prints no
-- keyword at all.
--
-- The direction is not a sign on one constructor, and this group is where that is
-- observable: alice's Scrollkeeper takes her to eight on the very board where
-- alice's Gnat Miser takes bob to six.
gnatMiserSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
gnatMiserSpec s registry =
  Spec.describe s "GnatMiser" $ do
    Spec.it s "CR 402.2 each opponent's maximum drops to six" $ do
      plains <- S.printingOf s registry "Plains"
      gnatMiser <- S.printingOf s registry "Gnat Miser"
      Spec.assertEqWith s "six" (PlayerEffect.maximumHandSize S.bob (adjustedHands plains [gnatMiser] 8 S.alice)) (Just 6)

    -- CR 102.2: "each OPPONENT" is the other player, never the controller. The
    -- scope read backwards would take alice to six and leave bob alone.
    Spec.it s "CR 102.2 the controller's own maximum is untouched" $ do
      plains <- S.printingOf s registry "Plains"
      gnatMiser <- S.printingOf s registry "Gnat Miser"
      Spec.assertEqWith s "seven" (PlayerEffect.maximumHandSize S.alice (adjustedHands plains [gnatMiser] 8 S.alice)) (Just 7)

    -- The gameplay-level scope pair: ONE board, eight cards in each hand, and
    -- only who is active differs. alice trims to CR 402.2's seven, bob to six.
    Spec.it s "CR 514.1 alice's own cleanup trims her to seven, not six" $ do
      plains <- S.printingOf s registry "Plains"
      gnatMiser <- S.printingOf s registry "Gnat Miser"
      let after = reliquaryCleanup (adjustedHands plains [gnatMiser] 8 S.alice)
      Spec.assertEqWith s "hand" (S.handSize S.alice after) 7
      Spec.assertEqWith s "one discarded" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1

    Spec.it s "CR 514.1 bob's cleanup trims him to six" $ do
      plains <- S.printingOf s registry "Plains"
      gnatMiser <- S.printingOf s registry "Gnat Miser"
      let after = reliquaryCleanup (adjustedHands plains [gnatMiser] 8 S.bob)
      Spec.assertEqWith s "hand" (S.handSize S.bob after) 6
      Spec.assertEqWith s "two discarded" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 2

    -- THE DIRECTION CONTROL: both producers on one board, and the two seats move
    -- opposite ways. One constructor carrying a signed delta could still spell
    -- this, but it could not keep the reduction's floor below -- so the pair is
    -- read together with the zero case.
    Spec.it s "CR 402.2 an increase and a reduction on one board move the two seats opposite ways" $ do
      plains <- S.printingOf s registry "Plains"
      gnatMiser <- S.printingOf s registry "Gnat Miser"
      scrollkeeper <- S.printingOf s registry "Minamo Scrollkeeper"
      let board = adjustedHands plains [gnatMiser, scrollkeeper] 8 S.alice
      Spec.assertEqWith s "alice is up one" (PlayerEffect.maximumHandSize S.alice board) (Just 8)
      Spec.assertEqWith s "bob is down one" (PlayerEffect.maximumHandSize S.bob board) (Just 6)

    -- CR 107.1b: eight Misers would take seven to minus one, and the calculation
    -- yields zero instead. Eight rather than seven so the floor is applied to a
    -- number already AT it, which is where a subtraction would go wrong twice.
    Spec.it s "CR 107.1b eight Gnat Misers floor the opponent's maximum at zero" $ do
      plains <- S.printingOf s registry "Plains"
      gnatMiser <- S.printingOf s registry "Gnat Miser"
      let board = adjustedHands plains (replicate 8 gnatMiser) 3 S.bob
          after = reliquaryCleanup board
      Spec.assertEqWith s "CR 514.1 bob discards his whole hand" (S.handSize S.bob after) 0
      Spec.assertEqWith s "three discarded" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 3
      Spec.assertEqWith s "zero" (PlayerEffect.maximumHandSize S.bob board) (Just 0)

-- Seed a stored player effect keyed to a REAL battlefield object, not
-- S.addPlayerEffect's stand-in id 998 -- so a S.youControlSource condition
-- check has something to genuinely hold or fail against. Mirrors
-- ExpirySpec's whileEffect, adapted to ActivePlayerEffect.
addPlayerEffectAt ::
  ObjectId.ObjectId ->
  Expiry.Type.Expiry ->
  AffectedPlayers.AffectedPlayers PlayerId.PlayerId ->
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
  let (srcId, withSrc) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
      conditional =
        addPlayerEffectAt
          srcId
          (Expiry.Type.While (While.MkWhile S.alice S.youControlSource))
          (AffectedPlayers.Scoped PlayerScope.Opponents)
          PlayerEffect.Type.CantCastSpells
          S.alice
          withSrc
   in (srcId, conditional)

-- The STORED carrier: a player effect that outlives the object that made it.
-- Hand-built here, exactly as ExpirySpec hand-builds a ContinuousEffect. Every
-- expiry now has a card driving it end to end (the Silence, Blossoming Calm and
-- Synthetic Conditional Silence groups); these stay because they discriminate
-- shapes no single card reaches -- two AtTurnOf entries keyed to two players on
-- one handoff being the sharpest.
storedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
storedSpec s registry =
  Spec.describe s "Stored" $ do
    let base = Setup.emptyGame S.bothPlayers
        silenced =
          S.addPlayerEffect
            Expiry.Type.AtCleanup
            (AffectedPlayers.Scoped PlayerScope.Opponents)
            PlayerEffect.Type.CantCastSpells
            S.alice
            base

    Spec.it s "CR 611.1 a stored effect applies through its scope" $
      do
        Spec.assertBool s (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell VariableChoice.Announced silenced) "bob is prohibited"
        Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell VariableChoice.Announced silenced)) "alice is not"

    Spec.it s "CR 514.2 the cleanup sweep drops an AtCleanup player effect" $
      let after = Expiry.dropAtCleanup silenced
       in do
            Spec.assertEqWith s "one stored before" (length (GameState.playerEffects silenced)) 1
            Spec.assertEqWith s "none after" (GameState.playerEffects after) []
            Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell VariableChoice.Announced after)) "and bob may cast again"

    Spec.it s "CR 514.2 the cleanup sweep keeps a Never player effect" $
      let forever = S.addPlayerEffect Expiry.Type.Never (AffectedPlayers.Scoped PlayerScope.Opponents) PlayerEffect.Type.CantCastSpells S.alice base
       in Spec.assertEqWith s "survives" (length (GameState.playerEffects (Expiry.dropAtCleanup forever))) 1

    -- THE DISCRIMINATING SHAPE: two entries, keyed to the two different
    -- players, on the SAME handoff. An indiscriminate sweep (one that
    -- dropped every stored player effect) would pass the old
    -- single-entry version of this test; here it would wrongly drop
    -- bob's still-live entry too.
    Spec.it s "CR 611.2a the handoff sweep drops only the entry keyed to the player whose turn began" $
      let forBob = S.addPlayerEffect (Expiry.Type.AtTurnOf S.bob) (AffectedPlayers.Scoped PlayerScope.Opponents) PlayerEffect.Type.CantCastSpells S.alice base
          armed = S.addPlayerEffect (Expiry.Type.AtTurnOf S.alice) (AffectedPlayers.Scoped PlayerScope.Opponents) PlayerEffect.Type.CantCastSpells S.alice forBob
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
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell VariableChoice.Announced conditional) "still prohibited while the source stands"
      Spec.assertBool s (not changed) "the sweep reports no change"
      Spec.assertEqWith s "still stored" (length (GameState.playerEffects swept)) 1
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell VariableChoice.Announced swept) "still prohibited after a no-op sweep"

    Spec.it s "CR 611.2b the conditional sweep deletes a player effect whose condition has failed" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (srcId, conditional) = storedConditional piker
          gone = S.runPure S.identityAnswer conditional (Event.destroy Regenerability.Regenerable [srcId])
          (changed, swept) = Engine.runGamePure S.identityAnswer gone Expiry.sweepConditional
      Spec.assertBool s changed "the sweep reports a change"
      Spec.assertEqWith s "deleted, not masked" (GameState.playerEffects swept) []
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell VariableChoice.Announced swept)) "no longer prohibited"

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
      (_, gs1) = S.addPermanent plains S.alice gs0
      (_, gs2) = S.addPermanent plains S.alice gs1
      (silenceId, gs3) = S.addHandCard silence S.alice gs2
      (silence2Id, gs4) = S.addHandCard silence S.alice gs3
      -- bob: two Mountains, a Prodigal Sorcerer (a NON-mana activated
      -- ability), a Goblin Piker in hand to cast and a Mountain in hand to
      -- play.
      (_, gs5) = S.addPermanent mountain S.bob gs4
      (_, gs6) = S.addPermanent mountain S.bob gs5
      (_, gs7) = S.addPermanent prodigalSorcerer S.bob gs6
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
      (_, gs1) = S.addPermanent plains S.alice gs0
      (_, gs2) = S.addPermanent plains S.alice gs1
      (silenceId, gs3) = S.addHandCard silence S.alice gs2
      (_, gs4) = S.addPermanent mountain S.bob gs3
      (_, gs5) = S.addPermanent mountain S.bob gs4
      (bobsPiker, gs6) = S.addHandCard piker S.bob gs5
      (_, gs7) = S.addPermanent mountain S.carol gs6
      (_, gs8) = S.addPermanent mountain S.carol gs7
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
  Action.Type.Play {} -> False
  Action.Type.TurnFaceUp {} -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.Plot _ -> False
  Action.Type.Foretell _ -> False
  Action.Type.PutCompanionIntoHand -> False
  Action.Type.Ignore _ _ -> False
  Action.Type.EndEffect _ -> False
  Action.Type.ActivateManaAbility _ -> False
  Action.Type.Pass -> False

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.PlayerEffect" $ do
  ruleOfLawSpec s registry
  adjustmentSpec s
  thaliaSpec s registry
  medallionSpec s registry
  patricianGeistSpec s registry
  humilitySpec s registry
  titaniasSongSpec s registry
  edgewalkerSpec s registry
  phyrexianDiscountSpec s registry
  snowDiscountSpec s registry
  monocoloredHybridDiscountSpec s registry
  hybridDiscountSpec s registry
  mixedReductionSpec s registry
  textChangedEdgewalkerSpec s registry
  reliquaryTowerSpec s registry
  theTenRingsSpec s registry
  minamoScrollkeeperSpec s registry
  gnatMiserSpec s registry
  storedSpec s registry
