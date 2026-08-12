{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.PlayerEffect and Pawl.Engine.Cost, plus the types they case on
-- (Pawl.Types.PlayerEffect, PlayerScope, PlayerStaticAbility) and
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
--
-- Artificial Evolution and Magical Hack join Edgewalker for CR 612.1, the second
-- rule reaching this axis from outside it: the word naming which spells a player
-- static ability discounts is printed text like any other, so a text change moves
-- the discount off it.
--
-- The Ten Rings and Sea Gate Restoration are the CR 613.11 TIMESTAMP pair, and
-- they are a pair on purpose: a set maximum hand size and a removed one disagree,
-- so which of them entered later decides the answer -- and one rides each carrier,
-- which is where pawl's order used to come apart (Reliquary Tower is the third
-- card in the group, and its ruling is the authority for the reading).
--
-- Void Winnower brings CR 601.3a's LOOKAHEAD, and Molten Disaster is the second
-- half of that pair: a prohibition on even mana values, against an {X} spell
-- whose mana value is even only while it sits in a hand (CR 202.3e).
--
-- Spider-Punk brings CR 701.6a onto the axis, with Cancel and Stifle as the two
-- counterers it has to stop -- the one place this file reaches
-- Pawl.Engine.Event's countering funnel. Prowling Serpopard is its NARROWED
-- counterpart, and the pair is the point: one board, one Cancel, and the filter
-- alone deciding whether the victim spell survives.
module Pawl.PlayerEffectSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
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
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.BeginningStep as BeginningStep
-- Aliased Card.Type, per the project-wide convention (CardSpec): the logic
-- module Pawl.Engine.Card may later be imported and must not collide.
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostAdjustments as CostAdjustments
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Expiry as Expiry.Type
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerEffect as PlayerEffect.Type
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

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
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell board)) "not prohibited"
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
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell afterFirst) "alice is now prohibited"
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
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell afterFirst)) "bob is not prohibited"

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
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell nextOwnTurn)) "not prohibited"
      Spec.assertBool s (elem (Action.Type.Cast b (S.printingName ruleOfLaw) Facing.FaceUp) (Action.legalActions S.alice nextOwnTurn)) "b offered again"

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
          Spec.assertBool s (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell countered) "still prohibited"

    -- The effect is RE-DERIVED from the battlefield on every read, so there
    -- is no stored state to unwind when its source leaves.
    Spec.it s "CR 604.2 destroying Rule of Law lifts the restriction in the same turn" $ do
      plains <- S.printingOf s registry "Plains"
      ruleOfLaw <- S.printingOf s registry "Rule of Law"
      let (_, _, z, plain) = ruleOfLawBoard plains ruleOfLaw
          (rol, onBoard) = S.addCreature ruleOfLaw S.alice plain
          castOne = S.withEvents [GameEvent.SpellCast S.alice S.noSource S.emptyCharacteristics] onBoard
          gone = S.runPure S.identityAnswer castOne (Event.destroy Regenerability.Regenerable [rol])
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell castOne) "prohibited while it stands"
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell gone)) "not prohibited once it is gone"
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
          (_, withRuleOfLaw) = S.addCreature ruleOfLaw S.alice base
          (_, gs) = S.addLibraryCard panglacialWurm S.alice withRuleOfLaw
          castOne = S.withEvents [GameEvent.SpellCast S.alice S.noSource S.emptyCharacteristics] gs
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
  Action.Type.TurnFaceUp _ -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.Ignore _ -> False
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

-- CR 107.4f's {G/P}. A Color rather than a ManaType, since every Phyrexian
-- symbol is coloured.
phyrexianGreen :: ManaSymbol.ManaSymbol
phyrexianGreen = ManaSymbol.Phyrexian Color.Green

-- CR 601.2f's adjustments as this suite's assertions state them: the increases,
-- the reductions, and no floor -- Pawl.Types.CostAdjustments.minimumMana is
-- Heartstone's sentence, and no spell-cost reducer states it (the activation side
-- is proved against the card in Pawl.ActivateSpec).
adjustments :: [Natural] -> [ManaCost.ManaCost] -> CostAdjustments.CostAdjustments
adjustments increases reductions =
  CostAdjustments.MkCostAdjustments
    { CostAdjustments.increases = increases,
      CostAdjustments.reductions = reductions,
      CostAdjustments.minimumMana = 0
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

    -- THE HEADLINE FALSIFIER for the typed half, and the one place pawl
    -- deliberately does not do what CR 118.7b-d would (#309). Edgewalker's own
    -- reminder text is the assertion: "if you cast a Cleric spell with mana
    -- cost {1}{W}, it costs {1} to cast" -- so the {B} half, which the cost
    -- cannot satisfy, takes NOTHING rather than one generic mana.
    Spec.it s "an excess typed reduction is dropped, not spilled onto generic" $
      Spec.assertEqWith
        s
        "{1}{W} reduced by {W}{B} is {1}"
        (Cost.applyAdjustments (adjustments [] [ManaCost.MkManaCost [white, black]]) (ManaCost.MkManaCost [ManaSymbol.Generic 1, white]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1])

    -- Ruling: "If you have more than one of these on the battlefield, the cost
    -- reduction is cumulative." Cumulative, and still bounded by what the cost
    -- actually has to give.
    Spec.it s "two typed reductions pool, and the second finds nothing left to take" $
      Spec.assertEqWith
        s
        "{1}{W}{B} reduced by {W}{B} twice is {1}"
        (Cost.applyAdjustments (adjustments [] [ManaCost.MkManaCost [white, black], ManaCost.MkManaCost [white, black]]) (ManaCost.MkManaCost [ManaSymbol.Generic 1, white, black]))
        (ManaCost.MkManaCost [ManaSymbol.Generic 1])

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
        (Cost.reductionHalvesOf (ManaSymbol.Hybrid (ManaType.Colored Color.White) (ManaType.Colored Color.Blue)))
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
        (Cost.reductionHalvesOf (ManaSymbol.Hybrid (ManaType.Colored Color.White) (ManaType.Colored Color.White)))
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

-- thaliaBoard, minus the Piker and plus each printing in `extras` on alice's
-- battlefield after Thalia (so `extras` take the later timestamps). Returns the
-- Bolt and Thalia. Draws nothing and advances no turn, as thaliaBoard does not.
thaliaWith :: Printing.Printing -> Printing.Printing -> Printing.Printing -> [Printing.Printing] -> Int -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
thaliaWith mountain thalia lightningBolt extras n =
  let base = S.landsInPlay mountain n
      (thaliaId, gs1) = S.addCreature thalia S.alice base
      gs2 = List.foldl' (\g p -> snd (S.addCreature p S.alice g)) gs1 extras
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
        (fmap fst (Cast.castableWhileSearching S.alice gs))
        [wurm]

    -- The proving test for Projection.liveAfterLayers (#391). Ashaya adds the
    -- card type Land to Thalia at layer 4; Blood Moon then depends on that (CR
    -- 613.8a) and SETS her subtype to Mountain, which by CR 305.7 takes every
    -- ability generated by her rules text -- the tax included. The old gate
    -- asked Blood Moon's "nonbasic land" filter against Thalia's BASE
    -- characteristics, where she is only a creature, so the tax survived.
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
          castOne = S.withEvents [GameEvent.SpellCast S.alice S.noSource S.emptyCharacteristics]
      Spec.assertBool
        s
        (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell (castOne withHumility))
        "control: Humility alone does not reach an enchantment"
      Spec.assertBool
        s
        (not (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell (castOne withOpalescence)))
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

-- alice controls `copies` Synthetic Phyrexian Discounts and `n` untapped
-- Forests; her hand holds one Longtusk Cub ({1}{G} green Cat) and one Mutagenic
-- Growth ({G/P} green instant). Loaded fresh inside each case that needs it --
-- equivalent because loading is deterministic and cached (batch-recipe.md).
phyrexianDiscountBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> Int -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
phyrexianDiscountBoard forest discount cub growth copies n =
  let base = S.landsInPlay forest n
      put g _ = snd (S.addCreature discount S.alice g)
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
-- Green because Mutagenic Growth's {G/P} is the pool's one Phyrexian COST, and
-- the third case below aims this reduction straight at it: the two sides of the
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
    -- cost". The reduction here is spent by nothing and dropped (#309).
    --
    -- Not hostage to that gap: CR 118.7b would instead turn the unspent green
    -- into one GENERIC mana, and this cost has no generic component for CR
    -- 118.7a to take it off, so {G/P} is the answer under either reading.
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
      put g _ = snd (S.addCreature discount S.alice g)
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
      put g _ = snd (S.addCreature discount S.alice g)
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
-- CR 118.7b-c cannot reach this group, so none of it is hostage to #309: the {2}
-- half is generic mana and the {B} half finds a black symbol waiting for it, so
-- neither half is ever the excess that pawl drops and the rule would spill.
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
-- printing both would take one mana either way. Printing no generic component is
-- what keeps the white-half case out of #309's way: CR 118.7b would turn a
-- stranded {W} into one generic mana and CR 118.7a says generic reductions reach
-- only the generic component, so pawl DROPPING it and the rule SPILLING it give
-- the same two Swamps here. Aim the same reduction at a cost with a generic
-- component and the two readings part company, which is the trap this group is
-- shaped to avoid.
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
  let (_, g1) = S.addCreature island S.alice (S.landsInPlay plains 1)
      (walkerId, g2) = S.addCreature edgewalker S.alice g1
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
-- pawl drops a stranded coloured reduction rather than spilling it onto the
-- generic component (#309), so only a white or black spell shows the difference
-- at all. Every Wizard in the pool is mono-blue; Whipstitched Zombie ({1}{B}
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
      add printing g _ = snd (S.addCreature printing S.alice g)
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
      cast g = S.runPure S.identityAnswer g (Cast.castSpell S.alice spellId frontName Facing.FaceUp)
      resolve g = S.runPure S.identityAnswer g Stack.resolveTop
      rings = S.addCreature tenRings S.alice
   in if ringsFirst
        then let (ringsId, withRings) = rings ready in (resolve (cast withRings), ringsId)
        else let (ringsId, withRings) = rings (resolve (cast ready)) in (withRings, ringsId)

-- The Ten Rings, a Legendary Artifact: "Your maximum hand size is ten." Its
-- end-step draw-to-ten ability is not implemented (#1239).
--
-- Sea Gate Restoration, the front face of a modal double-faced card: "Draw cards
-- equal to the number of cards in your hand plus one. You have no maximum hand
-- size for the rest of the game." Its back face Sea Gate, Reborn always enters
-- tapped -- the "you may pay 3 life" that buys it in untapped is not implemented
-- (#1240).
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
            PlayerScope.Opponents
            PlayerEffect.Type.CantCastSpells
            S.alice
            base

    Spec.it s "CR 611.1 a stored effect applies through its scope" $
      do
        Spec.assertBool s (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell silenced) "bob is prohibited"
        Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell silenced)) "alice is not"

    Spec.it s "CR 514.2 the cleanup sweep drops an AtCleanup player effect" $
      let after = Expiry.dropAtCleanup silenced
       in do
            Spec.assertEqWith s "one stored before" (length (GameState.playerEffects silenced)) 1
            Spec.assertEqWith s "none after" (GameState.playerEffects after) []
            Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell after)) "and bob may cast again"

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
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell conditional) "still prohibited while the source stands"
      Spec.assertBool s (not changed) "the sweep reports no change"
      Spec.assertEqWith s "still stored" (length (GameState.playerEffects swept)) 1
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell swept) "still prohibited after a no-op sweep"

    Spec.it s "CR 611.2b the conditional sweep deletes a player effect whose condition has failed" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (srcId, conditional) = storedConditional piker
          gone = S.runPure S.identityAnswer conditional (Event.destroy Regenerability.Regenerable [srcId])
          (changed, swept) = Engine.runGamePure S.identityAnswer gone Expiry.sweepConditional
      Spec.assertBool s changed "the sweep reports a change"
      Spec.assertEqWith s "deleted, not masked" (GameState.playerEffects swept) []
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell swept)) "no longer prohibited"

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
  Action.Type.Play {} -> False
  Action.Type.TurnFaceUp _ -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.Ignore _ -> False
  Action.Type.ActivateManaAbility _ -> False
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
      Spec.assertBool s (elem (Action.Type.Cast pikerId (S.printingName piker) Facing.FaceUp) (Action.legalActions S.bob before)) "offered"

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
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell after) "bob is prohibited"
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
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell after)) "alice is not prohibited"
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
      Spec.assertBool s (elem (Action.Type.Play landId Nothing) (Action.legalActions S.bob after)) "bob may still play a land"
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
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell ended)) "bob may cast again"

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
      Spec.assertBool s (elem (Action.Type.Cast bobsPiker (S.printingName piker) Facing.FaceUp) (Action.legalActions S.bob bobsTurn)) "bob could cast before it resolved"
      Spec.assertBool s (elem (Action.Type.Cast carolsPiker (S.printingName piker) Facing.FaceUp) (Action.legalActions S.carol carolsTurn)) "carol could cast before it resolved"
      Spec.assertEqWith s "one stored effect" (length (GameState.playerEffects resolved)) 1
      -- THE DISCRIMINATOR. carol is the far seat: an Opponents scope resolved as
      -- "the next player in turn order" prohibits bob and leaves carol free, and
      -- that is the reading the doc comments claimed was in here.
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell resolved) "bob is prohibited"
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.carol anySpellId anySpell resolved) "carol is prohibited too"
      Spec.assertEqWith
        s
        "and nothing is offered to either, even on their own main phase"
        (filter isCast (Action.legalActions S.bob resolvedBobsTurn) <> filter isCast (Action.legalActions S.carol resolvedCarolsTurn))
        []
      -- CR 109.5: the scope is resolved off the effect's controller.
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell resolved)) "alice is not prohibited"

-- CR 611.2a's board: three seats, and the SEAT COUNT is load-bearing twice
-- over. "Until your next turn" has to pass two other seats before it ends, so a
-- two-player board cannot tell "the next turn" from "your next turn"; and CR
-- 702.11c's opponents are two players, not one.
--
-- alice has one Plains and Blossoming Calm in hand. bob has one Mountain and a
-- Lightning Bolt. carol has nothing -- she is a seat to pass and a rival target,
-- both of which she is by existing. Mana is held EQUAL across every case below,
-- because each one casts the same Bolt off the same untapped Mountain: the only
-- thing that ever differs is whether alice's stored effect is still there.
--
-- Loaded fresh inside each case that needs it -- equivalent because loading is
-- deterministic and cached (batch-recipe.md).
blossomingCalmBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
blossomingCalmBoard plains calm mountain bolt =
  let gs0 = Setup.emptyGame S.threePlayers
      (_, gs1) = S.addCreature plains S.alice gs0
      (calmId, gs2) = S.addHandCard calm S.alice gs1
      (_, gs3) = S.addCreature mountain S.bob gs2
      (boltId, gs4) = S.addHandCard bolt S.bob gs3
   in ( calmId,
        boltId,
        gs4
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- alice casts Blossoming Calm and it resolves.
blossomingCalmAfter :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
blossomingCalmAfter calmId before =
  S.runPure S.identityAnswer (S.runPure S.identityAnswer before (S.cast S.alice calmId)) Engine.priorityLoop

-- bob casts his Bolt with an answerer that aims at alice whenever the engine
-- offers her, so "alice was never offered" is the only way the damage can land
-- anywhere else -- TargetSpec's prefersBob, pointed the other way.
--
-- The phase and priority are restated rather than inherited, so a board that has
-- been handed off two seats is cast on under exactly the conditions the
-- un-handed-off one was.
blossomingCalmBolt :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
blossomingCalmBolt boltId gs =
  let prefersAlice :: Prompt.Prompt r -> r
      prefersAlice p = case p of
        Prompt.ChooseTargets _ _ _ sets -> S.preferring (== Recipient.ToPlayer S.alice) sets
        _ -> S.identityAnswer p
      staged = gs {GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.bob}
   in S.runPure prefersAlice staged (S.cast S.bob boltId >> Stack.resolveTop)

-- CR 611.2a's turn boundary, as Engine.handoffTurn -- the call every "until your
-- next turn" duration is ended by (Expiry.dropAtTurnOf), and the one a test can
-- make without running whole turns and decking the fixture (CR 104.3c).
blossomingCalmHandoff :: GameState.GameState -> GameState.GameState
blossomingCalmHandoff gs = S.runPure S.identityAnswer gs Engine.handoffTurn

-- Blossoming Calm {W} Instant: "You gain hexproof until your next turn. You gain
-- 2 life." The stored player-effect carrier's turn-relative expiry, end to end:
-- Pawl.Engine.Resolve stamps Expiry.AtTurnOf off the resolution's controller
-- (CR 109.5) and Expiry.dropAtTurnOf ends it at alice's seat, two handoffs later.
--
-- Not implemented: the card's third line is rebound (CR 702.88), which pawl has
-- no representation for (#877). The omission runs against the controller -- pawl's
-- Blossoming Calm is cast once where the printed one is cast twice -- so nothing
-- here is weaker than printed.
blossomingCalmSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
blossomingCalmSpec s registry =
  Spec.describe s "Blossoming Calm" $ do
    -- THE CONTROL TWIN. Same seats, same Mountain, same Bolt, same answerer --
    -- the only difference from the case below is that alice never cast her
    -- instant. Without this, "alice took no damage" could mean the Bolt was
    -- never cast at all.
    Spec.it s "with no Calm cast, bob's Bolt reaches alice" $ do
      plains <- S.printingOf s registry "Plains"
      calm <- S.printingOf s registry "Blossoming Calm"
      mountain <- S.printingOf s registry "Mountain"
      bolt <- S.printingOf s registry "Lightning Bolt"
      let (_, boltId, before) = blossomingCalmBoard plains calm mountain bolt
          burned = blossomingCalmBolt boltId before
      Spec.assertEqWith s "alice takes the Bolt" (S.lifeOf S.alice burned) (Just 17)
      Spec.assertEqWith s "bob is untouched" (S.lifeOf S.bob burned) (Just 20)
      Spec.assertEqWith s "and so is carol" (S.lifeOf S.carol burned) (Just 20)

    -- CR 611.1: the resolution stores the effect, and CR 702.11c's player
    -- hexproof takes alice out of the Bolt's candidate set. The life gain is the
    -- second clause of the same spell, and it is asserted for its own sake: it is
    -- what shows the spell RESOLVED rather than fizzling somewhere earlier.
    Spec.it s "CR 702.11c once it resolves, alice has gained 2 and bob's Bolt cannot reach her" $ do
      plains <- S.printingOf s registry "Plains"
      calm <- S.printingOf s registry "Blossoming Calm"
      mountain <- S.printingOf s registry "Mountain"
      bolt <- S.printingOf s registry "Lightning Bolt"
      let (calmId, boltId, before) = blossomingCalmBoard plains calm mountain bolt
          resolved = blossomingCalmAfter calmId before
          burned = blossomingCalmBolt boltId resolved
      Spec.assertEqWith s "one stored player effect" (length (GameState.playerEffects resolved)) 1
      Spec.assertEqWith s "and it ends at alice's next turn" (fmap ActivePlayerEffect.expiry (GameState.playerEffects resolved)) [Expiry.Type.AtTurnOf S.alice]
      Spec.assertEqWith s "alice gained 2" (S.lifeOf S.alice resolved) (Just 22)
      Spec.assertEqWith s "and takes nothing from the Bolt" (S.lifeOf S.alice burned) (Just 22)
      Spec.assertEqWith s "which landed on bob, the lowest candidate left" (S.lifeOf S.bob burned) (Just 17)

    -- HEXPROOF, NOT SHROUD, on the stored carrier: CR 702.11c names only
    -- opponents, so alice remains a legal target for her own spells and carol
    -- remains a legal target for everyone. An implementation that read the
    -- payload as EachPlayer passes the case above and fails this one.
    Spec.it s "CR 702.11c the stored effect stops alice's opponents and nobody else" $ do
      plains <- S.printingOf s registry "Plains"
      calm <- S.printingOf s registry "Blossoming Calm"
      mountain <- S.printingOf s registry "Mountain"
      bolt <- S.printingOf s registry "Lightning Bolt"
      let (calmId, _, before) = blossomingCalmBoard plains calm mountain bolt
          resolved = blossomingCalmAfter calmId before
      case S.spellTargetSpec bolt of
        Nothing -> Spec.assertFailure s "Lightning Bolt should declare a target slot"
        Just theSpec -> do
          let legalFor who = Target.legalRecipients (Just who) S.noSource theSpec
          Spec.assertBool s (Set.member (Recipient.ToPlayer S.alice) (legalFor S.bob before)) "before the Calm, bob may bolt alice"
          Spec.assertBool s (not (Set.member (Recipient.ToPlayer S.alice) (legalFor S.bob resolved))) "after it, he may not"
          Spec.assertBool s (not (Set.member (Recipient.ToPlayer S.alice) (legalFor S.carol resolved))) "and neither may carol -- both opponents, not just the next seat"
          Spec.assertBool s (Set.member (Recipient.ToPlayer S.alice) (legalFor S.alice resolved)) "but alice may still target herself"
          Spec.assertBool s (Set.member (Recipient.ToPlayer S.carol) (legalFor S.bob resolved)) "and carol is targetable as ever"

    -- CR 514.2 is the wrong sweep for this duration, and this is where an
    -- UntilEndOfTurn mis-arming would show: the effect has to outlive the
    -- cleanup of the very turn it was cast in.
    Spec.it s "CR 514.2 the hexproof outlives the cleanup of the turn it was cast in" $ do
      plains <- S.printingOf s registry "Plains"
      calm <- S.printingOf s registry "Blossoming Calm"
      mountain <- S.printingOf s registry "Mountain"
      bolt <- S.printingOf s registry "Lightning Bolt"
      let (calmId, boltId, before) = blossomingCalmBoard plains calm mountain bolt
          swept = Expiry.dropAtCleanup (blossomingCalmAfter calmId before)
          burned = blossomingCalmBolt boltId swept
      Spec.assertEqWith s "still stored" (length (GameState.playerEffects swept)) 1
      Spec.assertEqWith s "and alice still takes nothing" (S.lifeOf S.alice burned) (Just 22)

    -- THE UNIT'S POINT. Two handoffs pass and the effect survives both; the
    -- third begins alice's own turn and ends it. A duration keyed to the next
    -- turn, or to the victim rather than to CR 109.5's "you", would end at the
    -- first handoff -- which is why the assertion is made at every seat rather
    -- than only at the last.
    Spec.it s "CR 611.2a it survives bob's turn and carol's turn, and ends as alice's next turn begins" $ do
      plains <- S.printingOf s registry "Plains"
      calm <- S.printingOf s registry "Blossoming Calm"
      mountain <- S.printingOf s registry "Mountain"
      bolt <- S.printingOf s registry "Lightning Bolt"
      let (calmId, boltId, before) = blossomingCalmBoard plains calm mountain bolt
          resolved = blossomingCalmAfter calmId before
          bobsTurn = blossomingCalmHandoff resolved
          carolsTurn = blossomingCalmHandoff bobsTurn
          alicesTurn = blossomingCalmHandoff carolsTurn
      Spec.assertEqWith s "bob's turn begins" (GameState.activePlayer bobsTurn) S.bob
      Spec.assertEqWith s "and the effect is still stored" (length (GameState.playerEffects bobsTurn)) 1
      Spec.assertEqWith s "alice takes nothing on bob's turn" (S.lifeOf S.alice (blossomingCalmBolt boltId bobsTurn)) (Just 22)
      Spec.assertEqWith s "carol's turn begins" (GameState.activePlayer carolsTurn) S.carol
      Spec.assertEqWith s "and the effect is still stored" (length (GameState.playerEffects carolsTurn)) 1
      Spec.assertEqWith s "alice takes nothing on carol's turn either" (S.lifeOf S.alice (blossomingCalmBolt boltId carolsTurn)) (Just 22)
      Spec.assertEqWith s "alice's own next turn begins" (GameState.activePlayer alicesTurn) S.alice
      Spec.assertEqWith s "and the effect is gone" (GameState.playerEffects alicesTurn) []
      Spec.assertEqWith s "so the same Bolt now reaches her" (S.lifeOf S.alice (blossomingCalmBolt boltId alicesTurn)) (Just 19)

-- CR 611.2b's board, and the SWAMP is the only thing about it that varies. alice
-- has an Island (which pays for the spell) and, on the holding board, a Swamp
-- (which the condition counts); bob and carol each have two Mountains and a
-- Goblin Piker, so both opponents can genuinely cast before the spell resolves.
--
-- Paying and gating are deliberately split across two lands: with one land doing
-- both, "the condition holds" and "she had mana" would be the same fact, and the
-- never-starts case below could not hold mana equal while removing the Swamp.
--
-- The Swamp's id is S.noSource on the board that has no Swamp: the one case built
-- that way never names it, and there is nothing on the battlefield for it to
-- collide with.
--
-- Loaded fresh inside each case that needs it -- equivalent because loading is
-- deterministic and cached (batch-recipe.md).
conditionalSilenceBoard :: Bool -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
conditionalSilenceBoard withSwamp island swamp hush mountain piker =
  let gs0 = Setup.emptyGame S.threePlayers
      (_, gs1) = S.addCreature island S.alice gs0
      (swampId, gs2) =
        if withSwamp
          then S.addCreature swamp S.alice gs1
          else (S.noSource, gs1)
      (hushId, gs3) = S.addHandCard hush S.alice gs2
      (_, gs4) = S.addCreature mountain S.bob gs3
      (_, gs5) = S.addCreature mountain S.bob gs4
      (bobsPiker, gs6) = S.addHandCard piker S.bob gs5
      (_, gs7) = S.addCreature mountain S.carol gs6
      (_, gs8) = S.addCreature mountain S.carol gs7
      (carolsPiker, gs9) = S.addHandCard piker S.carol gs8
   in ( hushId,
        swampId,
        bobsPiker,
        carolsPiker,
        gs9
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- alice casts it and it resolves.
conditionalSilenceAfter :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
conditionalSilenceAfter hushId before =
  S.runPure S.identityAnswer (S.runPure S.identityAnswer before (S.cast S.alice hushId)) Engine.priorityLoop

-- Goblin Piker is a creature, so CR 302.1 offers it only to the ACTIVE player.
-- The board is alice's own main phase, so each opponent's cast is read off a copy
-- with activePlayer flipped to them and nothing else changed -- threeSeatSilenceBoard's
-- device.
conditionalSilenceCasts :: PlayerId.PlayerId -> GameState.GameState -> [Action.Type.Action]
conditionalSilenceCasts who gs = filter isCast (Action.legalActions who (gs {GameState.activePlayer = who}))

-- SYNTHETIC. "Synthetic Conditional Silence" {U} Instant: "For as long as you
-- control a Swamp, your opponents can't cast spells." CR 611.2b's duration on the
-- stored player-effect carrier (Pawl.Types.ActivePlayerEffect), which no printed
-- card reaches: a "for as long as" effect that changes what a PLAYER may do is
-- printed as a static ability on a permanent, and that rides the other carrier
-- (Pawl.Types.PlayerStaticAbility) -- Grand Abolisher, Rule of Law and Damping
-- Engine are all statics. Every printed spell or ability that stores a
-- player-axis effect states a TURN-relative duration instead (Silence, Blossoming
-- Calm, Hope of Ghirapur, Academic Probation). Nothing in CR 611.2b confines the
-- duration to one carrier, so the card is legitimate and only unprinted.
conditionalSilenceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
conditionalSilenceSpec s registry =
  Spec.describe s "Synthetic Conditional Silence" $ do
    -- THE CONTROL TWIN: both opponents really could cast, so a later empty list
    -- is the prohibition and not an unaffordable Piker.
    Spec.it s "before it resolves, both opponents may cast" $ do
      island <- S.printingOf s registry "Island"
      swamp <- S.printingOf s registry "Swamp"
      hush <- S.printingOf s registry "Synthetic Conditional Silence"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, _, bobsPiker, carolsPiker, before) = conditionalSilenceBoard True island swamp hush mountain piker
      Spec.assertBool s (elem (Action.Type.Cast bobsPiker (S.printingName piker) Facing.FaceUp) (conditionalSilenceCasts S.bob before)) "bob is offered his Piker"
      Spec.assertBool s (elem (Action.Type.Cast carolsPiker (S.printingName piker) Facing.FaceUp) (conditionalSilenceCasts S.carol before)) "and carol hers"

    -- CR 611.2b: the duration began, so the effect is stored -- keyed to CR
    -- 109.5's "you", which Expiry.arm bakes in because the sweep that re-reads
    -- the condition has no resolution left to read a controller off.
    Spec.it s "CR 611.2b it is stored while the condition holds, and stops both opponents" $ do
      island <- S.printingOf s registry "Island"
      swamp <- S.printingOf s registry "Swamp"
      hush <- S.printingOf s registry "Synthetic Conditional Silence"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      let (hushId, _, _, _, before) = conditionalSilenceBoard True island swamp hush mountain piker
          resolved = conditionalSilenceAfter hushId before
      case fmap ActivePlayerEffect.expiry (GameState.playerEffects resolved) of
        [Expiry.Type.While who _] -> Spec.assertEqWith s "the duration is keyed to its controller" who S.alice
        other -> Spec.assertFailure s ("expected one conditional player effect, got " <> show other)
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell resolved) "bob is prohibited"
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.carol anySpellId anySpell resolved) "carol is prohibited too"
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice anySpellId anySpell resolved)) "alice is not"
      Spec.assertEqWith s "and nothing is offered to either" (conditionalSilenceCasts S.bob resolved <> conditionalSilenceCasts S.carol resolved) []

    -- THE POSITIVE HALF of the sweep. Without it, "deletes when the condition
    -- fails" is indistinguishable from "deletes at the first settle".
    Spec.it s "CR 611.2b a sweep with the Swamp still there changes nothing" $ do
      island <- S.printingOf s registry "Island"
      swamp <- S.printingOf s registry "Swamp"
      hush <- S.printingOf s registry "Synthetic Conditional Silence"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      let (hushId, _, _, _, before) = conditionalSilenceBoard True island swamp hush mountain piker
          resolved = conditionalSilenceAfter hushId before
          (changed, swept) = Engine.runGamePure S.identityAnswer resolved Expiry.sweepConditional
      Spec.assertBool s (not changed) "the sweep reports no change"
      Spec.assertEqWith s "still stored" (length (GameState.playerEffects swept)) 1
      Spec.assertEqWith s "and bob is still stopped" (conditionalSilenceCasts S.bob swept) []

    -- THE NEGATIVE, on the same board with exactly one difference: the Swamp
    -- changes hands (CR 613.1b), so alice no longer controls one and CR 611.2b's
    -- period is over. The effect is DELETED, and Engine.settleForPriority is what
    -- runs the sweep in a live game.
    Spec.it s "CR 611.2b when the Swamp changes hands the effect is deleted" $ do
      island <- S.printingOf s registry "Island"
      swamp <- S.printingOf s registry "Swamp"
      hush <- S.printingOf s registry "Synthetic Conditional Silence"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      let (hushId, swampId, bobsPiker, _, before) = conditionalSilenceBoard True island swamp hush mountain piker
          resolved = conditionalSilenceAfter hushId before
          stolen = S.giveControl swampId S.bob resolved
          settled = S.runPure S.identityAnswer stolen Engine.settleForPriority
      Spec.assertEqWith s "one stored before the Swamp moved" (length (GameState.playerEffects resolved)) 1
      Spec.assertEqWith s "none after" (GameState.playerEffects settled) []
      Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.bob anySpellId anySpell settled)) "bob is no longer prohibited"
      Spec.assertBool s (elem (Action.Type.Cast bobsPiker (S.printingName piker) Facing.FaceUp) (conditionalSilenceCasts S.bob settled)) "and his Piker is offered again"

    -- CR 611.2b's first sentence: a duration that never STARTS means the effect
    -- does nothing at all. The board differs from the holding one by the Swamp
    -- alone -- the Island that pays for the spell is on both -- so this is the
    -- Nothing arm of Expiry.arm and not an unaffordable cast.
    Spec.it s "CR 611.2b with no Swamp the duration never starts and nothing is stored" $ do
      island <- S.printingOf s registry "Island"
      swamp <- S.printingOf s registry "Swamp"
      hush <- S.printingOf s registry "Synthetic Conditional Silence"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      let (hushId, _, bobsPiker, _, before) = conditionalSilenceBoard False island swamp hush mountain piker
          -- Stack.resolveTop and NOT the priority loop the other cases use. A
          -- settle runs Expiry.sweepConditional, which deletes an effect whose
          -- condition is already false -- so a loop cannot tell "the duration
          -- never started" from "it started and was swept an instant later", and
          -- an arm that stored the effect unconditionally would leave this case
          -- green. The bare resolution can tell them apart.
          resolved = S.runPure S.identityAnswer (S.runPure S.identityAnswer before (S.cast S.alice hushId)) Stack.resolveTop
          settled = S.runPure S.identityAnswer resolved Engine.settleForPriority
      Spec.assertBool s (notElem hushId (GameState.stack resolved)) "the spell really did resolve"
      Spec.assertEqWith s "nothing stored" (GameState.playerEffects resolved) []
      Spec.assertBool s (elem (Action.Type.Cast bobsPiker (S.printingName piker) Facing.FaceUp) (conditionalSilenceCasts S.bob settled)) "so bob may cast"

-- Loaded fresh inside each case that needs it -- equivalent because loading
-- is deterministic and cached (batch-recipe.md).
matchesObjectBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
matchesObjectBoard lightningBolt piker =
  let base = Setup.emptyGame S.bothPlayers
      (bolt, withBolt) = S.spellOnStack lightningBolt S.alice base
      (pikerId, gs) = S.spellOnStack piker S.alice withBolt
   in (bolt, pikerId, gs)

-- The spell-match half of the cost-adjustment axis, now expressed as a Filter
-- over the PROJECTED view (CR 613.1d layer 4 for a card type, CR 613.1e layer 5
-- for a colour) rather than the retired SpellCriterion. A noncreature spell is
-- Filter.Not (Filter.HasCardType Creature); a coloured spell is Filter.HasColor.
matchesObjectSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
matchesObjectSpec s registry =
  Spec.describe s "matchesObject" $ do
    let noncreature = Filter.Type.Not (Filter.Type.HasCardType CardType.Creature)

    Spec.it s "CR 613.1d Thalia's noncreature criterion admits an instant" $ do
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (bolt, _, gs) = matchesObjectBoard lightningBolt piker
      Spec.assertBool s (PlayerEffect.matchesObject noncreature bolt gs) "Lightning Bolt is a noncreature spell"

    Spec.it s "CR 613.1d a creature spell fails the noncreature criterion" $ do
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, pikerId, gs) = matchesObjectBoard lightningBolt piker
      Spec.assertBool s (not (PlayerEffect.matchesObject noncreature pikerId gs)) "Goblin Piker is a creature spell"

    Spec.it s "CR 613.1e a colour criterion admits a matching-colour spell" $ do
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (bolt, _, gs) = matchesObjectBoard lightningBolt piker
      Spec.assertBool s (PlayerEffect.matchesObject (Filter.Type.HasColor Color.Red) bolt gs) "Lightning Bolt is red"

    Spec.it s "CR 613.1e a colour criterion rejects a non-matching colour" $ do
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (bolt, _, gs) = matchesObjectBoard lightningBolt piker
      Spec.assertBool s (not (PlayerEffect.matchesObject (Filter.Type.HasColor Color.Blue) bolt gs)) "Lightning Bolt is not blue"

-- Null Chamber {3}{W} World Enchantment: "As this enchantment enters, you and an
-- opponent each choose a card name other than a basic land card name. Spells
-- with the chosen names can't be cast and lands with the chosen names can't be
-- played."
--
-- alice has eight untapped Plains and four Mountains (mana is never the reason a
-- cast is unavailable, before or after the Chamber's own {3}{W} is paid) and the
-- Chamber in hand, in her own precombat main phase with an empty stack.
nullChamberBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
nullChamberBoard plains mountain nullChamber =
  let addMountain board _ = snd (S.addCreature mountain S.alice board)
      lands = List.foldl' addMountain (S.landsInPlay plains 8) [1 .. 4 :: Int]
      (gs, oid) = S.handOne nullChamber lands
   in (oid, gs)

-- The same board at THREE seats, which is the only shape where "an opponent" is
-- a choice at all (CR 102.2 leaves a two-player game one opponent). Four Plains
-- pay the Chamber's {3}{W}; nothing else is in play.
threeSeatBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
threeSeatBoard plains nullChamber =
  let addLand seat _ = snd (S.addCreature plains S.alice seat)
      lands = List.foldl' addLand (Setup.emptyGame S.threePlayers) [1 .. 4 :: Int]
      (oid, seated) = S.addHandCard nullChamber S.alice lands
   in ( oid,
        seated
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- CR 201.4 answered PER CHOOSER, which is the whole of what makes Null Chamber
-- worth testing: `pick` is asked WHO is choosing, so a case can put the
-- controller's name and the opponent's on different cards. `opponent` settles
-- the card's other open choice -- which opponent is asked at all -- and is never
-- reached at two seats. Everything else is the shared interpreter.
chamberAnswer :: PlayerId.PlayerId -> (PlayerId.PlayerId -> CardName.CardName) -> Prompt.Prompt r -> r
chamberAnswer opponent pick p = case p of
  Prompt.ChooseCardName _ chooser _ _ -> pick chooser
  Prompt.ChooseOpponent {} -> opponent
  _ -> S.identityAnswer p

-- chamberAnswer, also RECORDING each name ask as the (chooser, restriction) pair
-- it arrived as. Both halves are invisible from the finished board:
-- Object.chosenNames is a set and has forgotten CR 101.4's order, and CR 201.4a's
-- restriction is never written down at all, since the engine does not check the
-- answer against it (#663). Reading the prompt is the only way to see either.
recordingChamberAnswer ::
  PlayerId.PlayerId ->
  (PlayerId.PlayerId -> CardName.CardName) ->
  Prompt.Prompt r ->
  State.State [(PlayerId.PlayerId, Filter.Type.Filter Keyword.Keyword)] r
recordingChamberAnswer opponent pick p = case p of
  Prompt.ChooseCardName _ chooser _ restriction -> do
    State.modify' (<> [(chooser, restriction)])
    pure (pick chooser)
  _ -> pure (chamberAnswer opponent pick p)

-- CR 201.4a's restriction as Null Chamber prints it: "other than a basic land
-- card name", which is a supertype and a card type together (CR 205.4a: a basic
-- land card is the one carrying both).
nonBasicLandName :: Filter.Type.Filter Keyword.Keyword
nonBasicLandName = Filter.Type.Not (Filter.Type.And [Filter.Type.HasSupertype Supertype.Basic, Filter.Type.HasCardType CardType.Land])

-- Cast the Chamber and let it resolve, answering both name choices.
--
-- CAST rather than S.addCreature, because the choice happens only on the entry
-- path (Event.runEntry): a Chamber placed straight onto the battlefield
-- has an empty chosenNames and prohibits nothing.
castChamber :: PlayerId.PlayerId -> (PlayerId.PlayerId -> CardName.CardName) -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
castChamber opponent pick gs oid =
  let answer :: Prompt.Prompt r -> r
      answer = chamberAnswer opponent pick
      cast = snd (Engine.runGamePure answer gs (S.cast S.alice oid))
   in snd (Engine.runGamePure answer cast Stack.resolveTop)

-- The one object that reached the battlefield between two states -- the Chamber
-- itself, whose id the cast never handed back (CR 400.7 mints a new one).
enteredOne :: GameState.GameState -> GameState.GameState -> Maybe ObjectId.ObjectId
enteredOne before after = case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield before)) of
  [oid] -> Just oid
  _ -> Nothing

nullChamberSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
nullChamberSpec s registry =
  Spec.describe s "NullChamber" $ do
    -- CR 614.1c: "As [this permanent] enters . . ." is a replacement effect, and
    -- CR 614.12a makes its choice happen before the permanent enters. TWO
    -- choices, by two players, which is what no other as-enters card in the pool
    -- does -- Painter's Servant and Convincing Mirage each ask their controller
    -- and nobody else.
    Spec.it s "CR 614.1c both the controller and an opponent name a card as it enters" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      nullChamber <- S.printingOf s registry "Null Chamber"
      piker <- S.printingOf s registry "Goblin Piker"
      cancel <- S.printingOf s registry "Cancel"
      let (oid, board) = nullChamberBoard plains mountain nullChamber
          picks pid = if pid == S.alice then S.printingName piker else S.printingName cancel
          after = castChamber S.bob picks board oid
      case enteredOne board after >>= \chamber -> Game.lookupObject chamber after of
        Nothing -> Spec.assertFailure s "Null Chamber did not reach the battlefield"
        Just chamber ->
          Spec.assertEqWith
            s
            "both names, and only those two"
            (Object.chosenNames chamber)
            (Set.fromList [S.printingName piker, S.printingName cancel])

    -- CR 101.4: "If multiple players would make choices . . . at the same time,
    -- the active player . . . makes any choices required, then the next player
    -- in turn order". Both names are chosen as one event, so the order is the
    -- rule's.
    --
    -- NOT yet a discriminating test of CR 101.4 against "the controller first":
    -- the only way a permanent enters in this pool is its controller casting it,
    -- and a sorcery-speed enchantment is cast on its controller's own turn, so
    -- the two orders name the same player. A card that put a permanent onto the
    -- battlefield under another player's control would separate them.
    Spec.it s "CR 101.4 the active player is asked to name a card first" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      nullChamber <- S.printingOf s registry "Null Chamber"
      piker <- S.printingOf s registry "Goblin Piker"
      cancel <- S.printingOf s registry "Cancel"
      let (oid, board) = nullChamberBoard plains mountain nullChamber
          picks pid = if pid == S.alice then S.printingName piker else S.printingName cancel
          asked =
            State.execState
              (Engine.runGame (recordingChamberAnswer S.bob picks) board (S.cast S.alice oid >> Stack.resolveTop))
              []
      Spec.assertEqWith s "alice is active" (GameState.activePlayer board) S.alice
      Spec.assertEqWith s "alice names first, then bob" (fmap fst asked) [S.alice, S.bob]

    -- CR 201.4a: "If a player is instructed to choose a card name with certain
    -- characteristics, the player must choose the name of a card whose Oracle
    -- text matches those characteristics." Null Chamber's characteristics are
    -- "other than a basic land card name", and the engine does NOT check the
    -- answer against them (#663) -- so what is provable, and what the card is
    -- carried for, is that the restriction reaches the player being asked,
    -- unaltered, for BOTH choosers.
    --
    -- Nothing else in this group would notice its loss: the restriction is never
    -- written to the board, so authoring Filter.And [] -- the trivial predicate,
    -- which forbids nothing -- would leave every other case here green.
    Spec.it s "CR 201.4a the printed restriction reaches both choosers" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      nullChamber <- S.printingOf s registry "Null Chamber"
      piker <- S.printingOf s registry "Goblin Piker"
      cancel <- S.printingOf s registry "Cancel"
      let (oid, board) = nullChamberBoard plains mountain nullChamber
          picks pid = if pid == S.alice then S.printingName piker else S.printingName cancel
          asked =
            State.execState
              (Engine.runGame (recordingChamberAnswer S.bob picks) board (S.cast S.alice oid >> Stack.resolveTop))
              []
      Spec.assertEqWith s "the card's own restriction, on both asks" (fmap snd asked) [nonBasicLandName, nonBasicLandName]

    -- CR 613.10 / PlayerScope.EachPlayer: neither printed prohibition names a
    -- player -- "Spells with the chosen names can't be cast and lands with the
    -- chosen names can't be played" -- so both are SYMMETRIC, and reach the
    -- Chamber's controller and its opponents alike.
    --
    -- THE DISCRIMINATING CASE for that, and the only one: every other case in
    -- this group asks about alice, who controls the Chamber, so narrowing either
    -- ability's scope to PlayerScope.You would leave them all green. Both halves
    -- are asked of bob here, each with its own before/after pair so that "bob
    -- cannot do it" cannot be satisfied by bob never having been able to.
    --
    -- A Lightning Bolt rather than a creature, because CR 304.1 lets bob cast an
    -- instant on alice's turn: what keeps it off his list after the Chamber
    -- lands is the name and not CR 307.1's sorcery-speed window.
    Spec.it s "CR 613.10 both prohibitions reach the opponent, not only the controller" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      nullChamber <- S.printingOf s registry "Null Chamber"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      ashBarrens <- S.printingOf s registry "Ash Barrens"
      let (oid, alices) = nullChamberBoard plains mountain nullChamber
          (_, bobHasMana) = S.addCreature mountain S.bob alices
          (bobsBolt, bobHasBolt) = S.addHandCard lightningBolt S.bob bobHasMana
          (bobsBarrens, before) = S.addHandCard ashBarrens S.bob bobHasBolt
          -- alice names the spell, bob names the land: each prohibition is then
          -- carried by a name its own chooser did not pick, which is the same
          -- symmetry read on the other axis.
          picks pid = if pid == S.alice then S.printingName lightningBolt else S.printingName ashBarrens
          after = castChamber S.bob picks before oid
          casts = Action.legalActions S.bob
      Spec.assertBool s (elem (Action.Type.Cast bobsBolt (S.printingName lightningBolt) Facing.FaceUp) (casts before)) "bob may cast his Bolt before the Chamber lands"
      Spec.assertBool s (notElem (Action.Type.Cast bobsBolt (S.printingName lightningBolt) Facing.FaceUp) (casts after)) "and may not once it has"
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.bob anySpellId (S.printingName lightningBolt) after) "bob is prohibited by alice's name"
      Spec.assertBool s (elem (bobsBarrens, Nothing) (Action.playableLands S.bob before)) "bob's land is playable before the Chamber lands"
      Spec.assertBool s (notElem (bobsBarrens, Nothing) (Action.playableLands S.bob after)) "and not once it has"

    -- CR 601.3's prohibit half, now carrying a QUALITY: "no rule or effect
    -- prohibits" is asked of one named spell rather than of casting in general.
    -- The Lightning Bolt is the falsifier -- a blanket prohibition, or one that
    -- compared nothing, would take it away too.
    Spec.it s "CR 601.3 a spell with the chosen name can't be cast, and its neighbour still can" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      nullChamber <- S.printingOf s registry "Null Chamber"
      piker <- S.printingOf s registry "Goblin Piker"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      cancel <- S.printingOf s registry "Cancel"
      let (oid, board) = nullChamberBoard plains mountain nullChamber
          picks pid = if pid == S.alice then S.printingName piker else S.printingName cancel
          after = castChamber S.bob picks board oid
          (pikerId, withPiker) = S.addHandCard piker S.alice after
          (boltId, gs) = S.addHandCard lightningBolt S.alice withPiker
          offered = Action.legalActions S.alice gs
      Spec.assertBool s (notElem (Action.Type.Cast pikerId (S.printingName piker) Facing.FaceUp) offered) "the named Piker is not offered"
      Spec.assertBool s (elem (Action.Type.Cast boltId (S.printingName lightningBolt) Facing.FaceUp) offered) "the unnamed Bolt still is"

    -- The OPPONENT's name binds the Chamber's controller, which is the half a
    -- one-chooser reading of the card would lose: bob names the Piker, and it is
    -- alice who may no longer cast one.
    Spec.it s "CR 601.3 the opponent's chosen name prohibits the controller too" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      nullChamber <- S.printingOf s registry "Null Chamber"
      piker <- S.printingOf s registry "Goblin Piker"
      cancel <- S.printingOf s registry "Cancel"
      let (oid, board) = nullChamberBoard plains mountain nullChamber
          -- The reverse of the case above: alice names something she is not
          -- holding, bob names the card she is.
          picks pid = if pid == S.alice then S.printingName cancel else S.printingName piker
          after = castChamber S.bob picks board oid
          (pikerId, gs) = S.addHandCard piker S.alice after
      Spec.assertBool s (PlayerEffect.prohibitsCasting S.alice anySpellId (S.printingName piker) gs) "alice is prohibited by bob's name"
      Spec.assertBool s (notElem (Action.Type.Cast pikerId (S.printingName piker) Facing.FaceUp) (Action.legalActions S.alice gs)) "and no cast is offered"

    -- CR 305.1: playing a land is a SPECIAL ACTION that never uses the stack, so
    -- the land half of the card is a different gate from the cast half --
    -- Action.playableLands rather than Cast.castable.
    --
    -- The Plains is the falsifier, and it is also why the named land has to be a
    -- nonbasic one: the card forbids naming a basic land card and CR 201.4a is
    -- what makes that restriction binding, so a basic land
    -- is the one land this card can never stop.
    Spec.it s "CR 305.1 a land with the chosen name can't be played, and a basic land still can" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      nullChamber <- S.printingOf s registry "Null Chamber"
      ashBarrens <- S.printingOf s registry "Ash Barrens"
      cancel <- S.printingOf s registry "Cancel"
      let (oid, board) = nullChamberBoard plains mountain nullChamber
          picks pid = if pid == S.alice then S.printingName ashBarrens else S.printingName cancel
          after = castChamber S.bob picks board oid
          (barrensId, withBarrens) = S.addHandCard ashBarrens S.alice after
          (plainsId, gs) = S.addHandCard plains S.alice withBarrens
          playable = Action.playableLands S.alice gs
      Spec.assertBool s (notElem (barrensId, Nothing) playable) "the named Ash Barrens is not playable"
      Spec.assertBool s (elem (plainsId, Nothing) playable) "the Plains still is"
      Spec.assertBool s (elem (Action.Type.Play plainsId Nothing) (Action.legalActions S.alice gs)) "and the Plains is offered"
      Spec.assertBool s (notElem (Action.Type.Play barrensId Nothing) (Action.legalActions S.alice gs)) "while the Barrens is not"

    -- CR 604.2: the effect is re-derived from the battlefield on every read, so
    -- destroying the Chamber lifts both halves with nothing to unwind.
    --
    -- The names go with it too -- CR 400.7 mints a new incarnation in the
    -- graveyard and Event.changeZone empties its chosenNames -- but that is a
    -- separate fact and NOT what this case observes: `applying` walks only the
    -- battlefield, so both prohibitions would lift here even if the names had
    -- survived the move.
    Spec.it s "CR 604.2 destroying the Chamber lifts both prohibitions" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      nullChamber <- S.printingOf s registry "Null Chamber"
      piker <- S.printingOf s registry "Goblin Piker"
      ashBarrens <- S.printingOf s registry "Ash Barrens"
      let (oid, board) = nullChamberBoard plains mountain nullChamber
          picks pid = if pid == S.alice then S.printingName piker else S.printingName ashBarrens
          after = castChamber S.bob picks board oid
          (pikerId, withPiker) = S.addHandCard piker S.alice after
          (barrensId, gs) = S.addHandCard ashBarrens S.alice withPiker
      case enteredOne board after of
        Nothing -> Spec.assertFailure s "Null Chamber did not reach the battlefield"
        Just chamber -> do
          let gone = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [chamber])
          Spec.assertBool s (PlayerEffect.prohibitsCasting S.alice anySpellId (S.printingName piker) gs) "prohibited while it stands"
          Spec.assertBool s (not (PlayerEffect.prohibitsCasting S.alice anySpellId (S.printingName piker) gone)) "not prohibited once it is gone"
          Spec.assertBool s (elem (Action.Type.Cast pikerId (S.printingName piker) Facing.FaceUp) (Action.legalActions S.alice gone)) "and the cast is offered again"
          Spec.assertBool s (elem (barrensId, Nothing) (Action.playableLands S.alice gone)) "and the land may be played again"

    -- REJECT-NOT-REPAIR on the opponent answer, which only a three-seat board
    -- can reach: an answer naming somebody who is not an opponent -- here the
    -- Chamber's own controller -- falls back to the head of the offered list,
    -- the posture Sba.chooseLegendVictims takes toward an out-of-group legend.
    --
    -- THE FALSIFIER is the second name. An unfiltered answer would make alice
    -- both choosers, and since she is asked once the Chamber would enter with
    -- ONE name -- so the card would quietly prohibit half of what it says.
    Spec.it s "CR 102.2 an answer naming no opponent falls back to the head of the offer" $ do
      plains <- S.printingOf s registry "Plains"
      nullChamber <- S.printingOf s registry "Null Chamber"
      piker <- S.printingOf s registry "Goblin Piker"
      cancel <- S.printingOf s registry "Cancel"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let (oid, board) = threeSeatBoard plains nullChamber
          picks pid
            | pid == S.alice = S.printingName piker
            | pid == S.bob = S.printingName cancel
            | otherwise = S.printingName lightningBolt
          -- alice controls the Chamber, so naming her names no opponent at all.
          after = castChamber S.alice picks board oid
      case enteredOne board after >>= \chamber -> Game.lookupObject chamber after of
        Nothing -> Spec.assertFailure s "Null Chamber did not reach the battlefield"
        Just chamber ->
          Spec.assertEqWith
            s
            "alice's name and bob's, bob being the head of [bob, carol]"
            (Object.chosenNames chamber)
            (Set.fromList [S.printingName piker, S.printingName cancel])

    -- "An opponent" is a choice the card leaves open and no rule assigns, so
    -- pawl gives it to the ability's controller -- CR 109.5's "you", the player
    -- the card's other half already names -- and at three seats that choice is
    -- real. The third player is asked NOTHING, which a reading of "you and an
    -- opponent" as the whole table (or as PlayerScope.EachPlayer, which
    -- coincides with the card at two seats) would get wrong.
    Spec.it s "CR 102.2 at three seats the controller picks which opponent names a card" $ do
      plains <- S.printingOf s registry "Plains"
      nullChamber <- S.printingOf s registry "Null Chamber"
      piker <- S.printingOf s registry "Goblin Piker"
      cancel <- S.printingOf s registry "Cancel"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let (oid, board) = threeSeatBoard plains nullChamber
          -- Each seat names a different card, so chosenNames says exactly who
          -- was asked.
          picks pid
            | pid == S.alice = S.printingName piker
            | pid == S.bob = S.printingName cancel
            | otherwise = S.printingName lightningBolt
          after = castChamber S.carol picks board oid
      case enteredOne board after >>= \chamber -> Game.lookupObject chamber after of
        Nothing -> Spec.assertFailure s "Null Chamber did not reach the battlefield"
        Just chamber ->
          Spec.assertEqWith
            s
            "alice's name and carol's, and nothing bob named"
            (Object.chosenNames chamber)
            (Set.fromList [S.printingName piker, S.printingName lightningBolt])

-- `active` is the active player in their own precombat main phase with an empty
-- stack (CR 305.1's window) holding FIVE Mountains, while `grantors` are already
-- on the battlefield under ALICE's control.
--
-- Five is deliberately more than any case below plays. Every "and no more"
-- assertion is otherwise satisfiable by an empty hand, which is the trap this
-- whole group is built to avoid: each case checks the leftover hand as well as
-- the lands that landed.
--
-- The grantors go under alice while the HAND is the argument's, so the one case
-- that makes bob active reads alice's Exploration against bob's land plays --
-- CR 109.5's You scope with the two players actually pulled apart.
landDropBoard :: Printing.Printing -> [Printing.Printing] -> PlayerId.PlayerId -> GameState.GameState
landDropBoard mountain grantors active =
  let put g printing = snd (S.addCreature printing S.alice g)
      withGrantors = List.foldl' put (Setup.emptyGame S.bothPlayers) grantors
      add g _ = snd (S.addHandCard mountain active g)
      withHand = List.foldl' add withGrantors [1 .. 5 :: Int]
   in withHand
        { GameState.phase = Phase.PrecombatMain,
          GameState.activePlayer = active,
          GameState.priority = Just active
        }

-- Take every land play the board allows and stop. S.playLandAnswer plays a land
-- whenever one is offered and passes otherwise, so the loop halts exactly when
-- CR 305.2a's comparison refuses -- the whole gate, through the real priority
-- loop, rather than a direct call to Action.legalActions.
playEveryLand :: GameState.GameState -> GameState.GameState
playEveryLand gs = S.runPure S.playLandAnswer gs Engine.priorityLoop

-- alice's next turn, as far as CR 305.2 can see it: her untap step, which is
-- where Engine.runTurnBasedActions resets the per-turn tally -- "during their
-- turn" in CR 305.2 has to start over somewhere, and that is the first moment of
-- the new one.
nextTurnFor :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
nextTurnFor pid gs =
  let untap = Phase.Beginning BeginningStep.Untap
      untapped = S.runPure S.identityAnswer (gs {GameState.activePlayer = pid, GameState.phase = untap}) (Engine.runTurnBasedActions untap)
   in untapped {GameState.phase = Phase.PrecombatMain, GameState.priority = Just pid, GameState.passes = 0}

-- CR 601.3b / Vedalken Orrery {4} Artifact: "You may cast spells as though they
-- had flash."
--
-- One board, built twice. alice holds a Goblin Piker -- a creature card, so CR
-- 302.1 and CR 117.1a's second sentence give it the sorcery-speed window -- and a
-- Mountain, behind nine untapped Mountains so that mana is never the reason a
-- cast is unavailable. It is BOB's precombat main phase and the stack is empty,
-- so alice's own sorcery-speed window is shut. `extra` goes onto the battlefield
-- under alice, and the Orrery is the only thing the two boards ever differ by.
-- Returns the Piker in hand, the ids of `extra` in the order given, and the board.
orreryBoard :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
orreryBoard mountain piker extra =
  let (base, oid) = S.pikerInHand mountain piker 9 Phase.PrecombatMain
      withLand = snd (S.addHandCard mountain S.alice base)
      put (ids, g) printing = let (i, g1) = S.addCreature printing S.alice g in (ids <> [i], g1)
      (extraIds, withExtra) = List.foldl' put ([], withLand) extra
   in ( oid,
        extraIds,
        withExtra
          { GameState.activePlayer = S.bob,
            GameState.priority = Just S.alice
          }
      )

-- The same board back on ALICE's turn, which is the control every refusal below
-- is measured against: it is what says the Piker is affordable, offered and
-- unblocked by anything the Orrery is not responsible for.
orreryOnOwnTurn :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> (ObjectId.ObjectId, GameState.GameState)
orreryOnOwnTurn mountain piker extra =
  let (oid, _, board) = orreryBoard mountain piker extra
   in (oid, board {GameState.activePlayer = S.alice})

-- CR 109.5's You scope from the other seat: it is ALICE's turn, BOB holds
-- priority with a Piker of his own, and both players have nine untapped
-- Mountains. `owner` is who controls the Orrery, and is the only thing that
-- varies.
orreryScopeBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> PlayerId.PlayerId -> (ObjectId.ObjectId, GameState.GameState)
orreryScopeBoard mountain piker orrery owner =
  let base = S.landsInPlay mountain 9
      withBobsLands = List.foldl' (\g _ -> snd (S.addCreature mountain S.bob g)) base [1 .. 9 :: Int]
      (bobsPiker, withBobsPiker) = S.addHandCard piker S.bob withBobsLands
      withOrrery = snd (S.addCreature orrery owner withBobsPiker)
   in ( bobsPiker,
        withOrrery
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.bob
          }
      )

-- CR 307.5's window, on the same axis and carried by something else entirely:
-- alice controls a Goblin Piker to equip, a Bonesplitter to equip it with
-- (data/cards/bonesplitter.json declares the equip ability SorcerySpeed) and
-- whatever `extra` names, with nine untapped Mountains for the {1}. `active` is
-- whose turn it is. Returns the Equipment.
equipBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> [Printing.Printing] -> PlayerId.PlayerId -> (ObjectId.ObjectId, GameState.GameState)
equipBoard mountain piker bonesplitter extra active =
  let base = S.landsInPlay mountain 9
      withPiker = snd (S.addCreature piker S.alice base)
      (equipment, withEquipment) = S.addCreature bonesplitter S.alice withPiker
      withExtra = List.foldl' (\g printing -> snd (S.addCreature printing S.alice g)) withEquipment extra
   in ( equipment,
        withExtra
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = active,
            GameState.priority = Just S.alice
          }
      )

isActivateOf :: ObjectId.ObjectId -> Action.Type.Action -> Bool
isActivateOf oid action = case action of
  Action.Type.Activate o _ -> o == oid
  Action.Type.Cast {} -> False
  Action.Type.Play {} -> False
  Action.Type.TurnFaceUp _ -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.Ignore _ -> False
  Action.Type.ActivateManaAbility _ -> False
  Action.Type.Pass -> False

isPlay :: Action.Type.Action -> Bool
isPlay action = case action of
  Action.Type.Play {} -> True
  Action.Type.Cast {} -> False
  Action.Type.Activate _ _ -> False
  Action.Type.TurnFaceUp _ -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.Ignore _ -> False
  Action.Type.ActivateManaAbility _ -> False
  Action.Type.Pass -> False

-- Exploration {G} Enchantment: "You may play an additional land on each of your
-- turns." Azusa, Lost but Seeking {2}{G} Legendary Creature -- Human Monk: "You
-- may play two additional lands on each of your turns."
--
-- TWO producers with DIFFERENT numbers, and that is the point of the group
-- rather than redundancy: one card cannot tell a real count from a
-- boolean-plus-one, because both readings answer "two". Azusa's three is what
-- separates them, and the two of them together answer four, which separates a
-- SUM from a maximum.
extraLandDropsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
extraLandDropsSpec s registry =
  Spec.describe s "ExtraLandDrops" $ do
    -- The control. CR 305.2's "normally one", played through the same loop, so
    -- every raised number below is measured against a baseline this group
    -- established rather than an assumed one.
    Spec.it s "CR 305.2 with no effect a player plays one land and no more" $ do
      mountain <- S.printingOf s registry "Mountain"
      let board = landDropBoard mountain [] S.alice
          after = playEveryLand board
      Spec.assertEqWith s "the allowance is one" (PlayerEffect.landPlaysAllowed S.alice board) 1
      Spec.assertEqWith s "one Mountain landed" (S.countOnBattlefieldByName (S.printingName mountain) S.alice after) 1
      Spec.assertEqWith s "and FOUR are still in hand -- the gate refused, an empty hand did not" (S.handSize S.alice after) 4
      Spec.assertEqWith s "no further land play is offered" (filter isPlay (Action.legalActions S.alice after)) []

    -- Exploration's one extra. A gate that ignored the effect answers one here.
    Spec.it s "CR 305.2 Exploration raises the allowance to two" $ do
      mountain <- S.printingOf s registry "Mountain"
      exploration <- S.printingOf s registry "Exploration"
      let board = landDropBoard mountain [exploration] S.alice
          after = playEveryLand board
      Spec.assertEqWith s "the allowance is two" (PlayerEffect.landPlaysAllowed S.alice board) 2
      Spec.assertEqWith s "two Mountains landed" (S.countOnBattlefieldByName (S.printingName mountain) S.alice after) 2
      Spec.assertEqWith s "three are still in hand" (S.handSize S.alice after) 3
      Spec.assertEqWith s "and the third is refused" (filter isPlay (Action.legalActions S.alice after)) []

    -- THE DISCRIMINATOR between a count and a boolean. A gate written as "one,
    -- or two if any effect grants extra lands" passes the Exploration case above
    -- and stops at two here; only a gate that reads Azusa's NUMBER reaches
    -- three.
    Spec.it s "CR 305.2 Azusa's two additional lands make three, not two" $ do
      mountain <- S.printingOf s registry "Mountain"
      azusa <- S.printingOf s registry "Azusa, Lost but Seeking"
      let board = landDropBoard mountain [azusa] S.alice
          after = playEveryLand board
      Spec.assertEqWith s "the allowance is three" (PlayerEffect.landPlaysAllowed S.alice board) 3
      Spec.assertEqWith s "three Mountains landed" (S.countOnBattlefieldByName (S.printingName mountain) S.alice after) 3
      Spec.assertEqWith s "two are still in hand" (S.handSize S.alice after) 2
      Spec.assertEqWith s "and the fourth is refused" (filter isPlay (Action.legalActions S.alice after)) []

    -- CR 305.2 says continuous effects INCREASE the number, so two of them
    -- compose: one plus one plus two. Nothing here is redundant -- CR 702.18b
    -- and CR 702.11h make multiple instances redundant for a KEYWORD, and CR
    -- 305.2 states no such rule. A maximum rather than a sum answers three.
    Spec.it s "CR 305.2 Exploration and Azusa together add up to four" $ do
      mountain <- S.printingOf s registry "Mountain"
      exploration <- S.printingOf s registry "Exploration"
      azusa <- S.printingOf s registry "Azusa, Lost but Seeking"
      let board = landDropBoard mountain [exploration, azusa] S.alice
          after = playEveryLand board
      Spec.assertEqWith s "the allowance is four" (PlayerEffect.landPlaysAllowed S.alice board) 4
      Spec.assertEqWith s "four Mountains landed" (S.countOnBattlefieldByName (S.printingName mountain) S.alice after) 4
      Spec.assertEqWith s "one is still in hand" (S.handSize S.alice after) 1
      Spec.assertEqWith s "and the fifth is refused" (filter isPlay (Action.legalActions S.alice after)) []

    -- CR 109.5: the You scope. alice's Exploration is alice's, and bob playing
    -- lands on his own turn is still held to CR 305.2's one.
    Spec.it s "CR 109.5 one player's Exploration does not raise another's allowance" $ do
      mountain <- S.printingOf s registry "Mountain"
      exploration <- S.printingOf s registry "Exploration"
      let board = landDropBoard mountain [exploration] S.bob
          after = playEveryLand board
      Spec.assertEqWith s "alice's allowance is two" (PlayerEffect.landPlaysAllowed S.alice board) 2
      Spec.assertEqWith s "bob's is still one" (PlayerEffect.landPlaysAllowed S.bob board) 1
      Spec.assertEqWith s "one Mountain landed for bob" (S.countOnBattlefieldByName (S.printingName mountain) S.bob after) 1
      Spec.assertEqWith s "four are still in his hand" (S.handSize S.bob after) 4
      Spec.assertEqWith s "and his second is refused" (filter isPlay (Action.legalActions S.bob after)) []

    -- CR 305.2's allowance is PER TURN, and the raised one refills like the
    -- normal one: CR 703.4c's untap step clears the tally, and the next turn
    -- gets two again rather than nothing or a running total.
    Spec.it s "CR 305.2 the raised allowance refills each turn" $ do
      mountain <- S.printingOf s registry "Mountain"
      exploration <- S.printingOf s registry "Exploration"
      let firstTurn = playEveryLand (landDropBoard mountain [exploration] S.alice)
          untapped = nextTurnFor S.alice firstTurn
          secondTurn = playEveryLand untapped
      Spec.assertEqWith s "two played on the first turn" (GameState.landsPlayed firstTurn) (Map.singleton S.alice 2)
      Spec.assertEqWith s "the untap step clears the tally" (GameState.landsPlayed untapped) Map.empty
      Spec.assertEqWith s "two more on the second turn" (GameState.landsPlayed secondTurn) (Map.singleton S.alice 2)
      Spec.assertEqWith s "four Mountains in play" (S.countOnBattlefieldByName (S.printingName mountain) S.alice secondTurn) 4
      Spec.assertEqWith s "and the fifth is still in hand, refused" (S.handSize S.alice secondTurn) 1

    -- CR 604.2: the grant is re-read from the battlefield on every look, so
    -- destroying Exploration between the second land and the third takes the
    -- extra play back. The already-played tally is untouched by that, which is
    -- CR 305.2b's comparison landing on "equal" and refusing.
    Spec.it s "CR 604.2 destroying Exploration mid-turn drops the allowance back to one" $ do
      mountain <- S.printingOf s registry "Mountain"
      exploration <- S.printingOf s registry "Exploration"
      let (explorationId, board) = S.addCreature exploration S.alice (Setup.emptyGame S.bothPlayers)
          add g _ = snd (S.addHandCard mountain S.alice g)
          withHand = List.foldl' add board [1 .. 5 :: Int]
          ready = withHand {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
          gone = S.runPure S.identityAnswer (playEveryLand ready) (Event.destroy Regenerability.Regenerable [explorationId])
      Spec.assertEqWith s "two lands were played while it stood" (GameState.landsPlayed gone) (Map.singleton S.alice 2)
      Spec.assertEqWith s "the allowance is back to one" (PlayerEffect.landPlaysAllowed S.alice gone) 1
      Spec.assertEqWith s "and CR 305.2b refuses a third" (filter isPlay (Action.legalActions S.alice gone)) []

vedalkenOrrerySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
vedalkenOrrerySpec s registry =
  Spec.describe s "VedalkenOrrery" $ do
    -- The control. Without it, every refusal below would also be true of a board
    -- where the Piker was simply unaffordable or unoffered.
    Spec.it s "CR 117.1a on alice's own turn the creature spell is castable already" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, board) = orreryOnOwnTurn mountain piker []
      Spec.assertBool s (S.castable S.alice oid board) "castable"
      Spec.assertBool s (any (S.isCastOf oid) (Action.legalActions S.alice board)) "offered"

    Spec.it s "CR 117.1a on the opponent's turn it is not" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, _, board) = orreryBoard mountain piker []
      Spec.assertBool s (not (S.castable S.alice oid board)) "not castable"
      Spec.assertBool s (not (any (S.isCastOf oid) (Action.legalActions S.alice board))) "not offered"

    -- The whole card, on the board that just refused: CR 601.3b's permission is
    -- read beside Cast.instantSpeed, so the sorcery-speed window opens for a card
    -- that has no flash of its own.
    Spec.it s "CR 601.3b with Vedalken Orrery it is castable on the opponent's turn" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      orrery <- S.printingOf s registry "Vedalken Orrery"
      let (oid, _, board) = orreryBoard mountain piker [orrery]
      Spec.assertBool s (S.castable S.alice oid board) "castable"
      Spec.assertBool s (any (S.isCastOf oid) (Action.legalActions S.alice board)) "offered"

    -- The gameplay half, driven through the priority loop rather than by calling
    -- Cast.castSpell: S.castAnswer takes whatever Cast action it is OFFERED, so
    -- the two runs differ in the Orrery and in nothing that a test wrote by hand.
    -- Without it alice is offered no cast at all and simply passes.
    Spec.it s "CR 601.3b the offered cast resolves and the creature enters on the opponent's turn" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      orrery <- S.printingOf s registry "Vedalken Orrery"
      let (_, _, board) = orreryBoard mountain piker [orrery]
          (_, _, bare) = orreryBoard mountain piker []
          play gs = S.runPure S.castAnswer gs Engine.priorityLoop
          after = play board
      Spec.assertEqWith s "bob is still the active player" (GameState.activePlayer after) S.bob
      Spec.assertEqWith s "the Piker is on the battlefield" (S.countOnBattlefieldByName (S.printingName piker) S.alice after) 1
      Spec.assertEqWith s "and without the Orrery it never left her hand" (S.countOnBattlefieldByName (S.printingName piker) S.alice (play bare)) 0
      Spec.assertEqWith s "which is where it still is" (S.handSize S.alice (play bare)) 2

    -- CR 702.8a's keyword is untouched: the card the Orrery let through never
    -- gained flash, and nothing was written onto it.
    Spec.it s "CR 702.8a the Piker still has no flash of its own" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      orrery <- S.printingOf s registry "Vedalken Orrery"
      let (oid, _, board) = orreryBoard mountain piker [orrery]
      Spec.assertBool s (not (Cast.instantSpeed (S.combinedFace piker))) "no flash on the card"
      Spec.assertBool s (PlayerEffect.mayCastAsThoughItHadFlash S.alice oid board) "the permission is the player's"

    -- CR 604.2: the permission is gathered live off the battlefield, so removing
    -- the Orrery shuts the window again with nothing to unwind.
    Spec.it s "CR 604.2 with the Orrery gone the window shuts again" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      orrery <- S.printingOf s registry "Vedalken Orrery"
      let (oid, extras, board) = orreryBoard mountain piker [orrery]
          without = board {GameState.battlefield = foldr Set.delete (GameState.battlefield board) extras}
      Spec.assertBool s (S.castable S.alice oid board) "castable with it"
      Spec.assertBool s (not (S.castable S.alice oid without)) "not castable without it"

    -- CR 305.1: playing a land is a special action and is never a cast, so a
    -- permission about the timing of a CAST does not reach the Mountain in
    -- alice's hand. Action.legalActions gates a land play on being the active
    -- player, and the Orrery leaves that alone.
    Spec.it s "CR 305.1 the land in hand is still unplayable on the opponent's turn" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      orrery <- S.printingOf s registry "Vedalken Orrery"
      let (_, _, board) = orreryBoard mountain piker [orrery]
          (_, ownTurn) = orreryOnOwnTurn mountain piker [orrery]
      Spec.assertBool s (any isPlay (Action.legalActions S.alice ownTurn)) "playable on her own turn"
      Spec.assertBool s (not (any isPlay (Action.legalActions S.alice board))) "not on bob's"

    -- CR 109.5 / PlayerScope.You: the Orrery says "you", so alice's does nothing
    -- for bob. The pair differs only in who controls it.
    Spec.it s "CR 109.5 alice's Orrery does not widen bob's window" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      orrery <- S.printingOf s registry "Vedalken Orrery"
      let (bobsPiker, board) = orreryScopeBoard mountain piker orrery S.alice
      Spec.assertBool s (not (S.castable S.bob bobsPiker board)) "not castable"

    Spec.it s "CR 109.5 bob's own Orrery does" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      orrery <- S.printingOf s registry "Vedalken Orrery"
      let (bobsPiker, board) = orreryScopeBoard mountain piker orrery S.bob
      Spec.assertBool s (S.castable S.bob bobsPiker board) "castable"

    -- CR 307.5: the reason the permission is read BESIDE Cast.instantSpeed and
    -- not inside it, nor inside Turn.sorcerySpeedWindow under it. Bonesplitter's
    -- equip ability is sorcery-speed, and the Orrery is not about abilities at
    -- all. Three boards triangulate it: the ability is genuinely offered, the
    -- opponent's turn genuinely takes it away, and the Orrery does not give it
    -- back.
    Spec.it s "CR 307.5 the equip ability is offered on alice's own turn" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      bonesplitter <- S.printingOf s registry "Bonesplitter"
      let (equipment, board) = equipBoard mountain piker bonesplitter [] S.alice
      Spec.assertBool s (any (isActivateOf equipment) (Action.legalActions S.alice board)) "offered"

    Spec.it s "CR 307.5 and not on bob's" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      bonesplitter <- S.printingOf s registry "Bonesplitter"
      let (equipment, board) = equipBoard mountain piker bonesplitter [] S.bob
      Spec.assertBool s (not (any (isActivateOf equipment) (Action.legalActions S.alice board))) "not offered"

    Spec.it s "CR 307.5 Vedalken Orrery does not give it back" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      bonesplitter <- S.printingOf s registry "Bonesplitter"
      orrery <- S.printingOf s registry "Vedalken Orrery"
      let (equipment, board) = equipBoard mountain piker bonesplitter [orrery] S.bob
          (onOwnTurn, ownBoard) = equipBoard mountain piker bonesplitter [orrery] S.alice
      Spec.assertBool s (not (any (isActivateOf equipment) (Action.legalActions S.alice board))) "still not offered"
      Spec.assertBool s (any (isActivateOf onOwnTurn) (Action.legalActions S.alice ownBoard)) "and still offered on her own turn"

-- ONE board for both halves of CR 701.6a's "a spell or ability": alice has a
-- SPELL of the caller's choosing on the stack and a settled Prodigal Sorcerer
-- whose {T} ABILITY can join it, and bob holds a Cancel for the first and a
-- Stifle for the second. `permanents` is the only difference between a run that
-- counters and a run that does not.
--
-- Shared by the Spider-Punk and Prowling Serpopard groups below, which is why
-- both the protecting permanents and the victim spell are parameters: the
-- unfiltered arm and the filtered one differ only in which victim survives.
--
-- bob's three Islands pay for whichever of the two he casts; the runs branch
-- from this state and never share mana.
counteringBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  [(PlayerId.PlayerId, Printing.Printing)] ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
counteringBoard island cancel stifle sorcerer victim permanents =
  let withLands = List.foldl' (\g _ -> snd (S.addCreature island S.bob g)) (Setup.emptyGame S.bothPlayers) [1 .. (3 :: Int)]
      (srcId, withSorcerer) = S.addCreature sorcerer S.alice withLands
      -- CR 302.6: settled, so the Sorcerer's {T} may be activated at all.
      settled = S.runPure S.identityAnswer withSorcerer (Engine.settleAll S.alice)
      addPermanent (ids, g) (who, p) = let (oid, g') = S.addCreature p who g in (oid : ids, g')
      (permanentIds, withPermanents) = List.foldl' addPermanent ([], settled) permanents
      (victimId, onStack) = S.spellOnStack victim S.alice withPermanents
      (cancelId, withCancel) = S.addHandCard cancel S.bob onStack
      (stifleId, gs) = S.addHandCard stifle S.bob withCancel
   in (victimId, srcId, cancelId, stifleId, permanentIds, gs)

-- Every target prompt answers with this object, and CR 603.5's "may" is always
-- exercised -- so a silence below is the rule and never a declined option.
counteringAnswer :: ObjectId.ObjectId -> Prompt.Prompt r -> r
counteringAnswer oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

-- Prodigal Sorcerer's "any target" is aimed at ALICE, so the effect that must
-- not occur when the ability is countered is her own life total; Stifle's only
-- legal target is the ability, which the default interpreter picks.
counteringAtAlice :: Prompt.Prompt r -> r
counteringAtAlice p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.alice))) sets
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

counteringAtAbility :: Prompt.Prompt r -> r
counteringAtAbility p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

-- bob casts his Cancel at alice's spell and lets it resolve.
cancelRun :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
cancelRun victimId cancelId gs =
  let answer :: Prompt.Prompt r -> r
      answer = counteringAnswer victimId
      cast = S.runPure answer gs (S.cast S.bob cancelId)
      resolved = S.runPure answer cast Stack.resolveTop
   in S.runPure answer resolved Engine.settleForPriority

-- alice activates her Sorcerer at herself, bob casts his Stifle at the ability,
-- and the stack is emptied down to the spell underneath. The first component is
-- the state once the Stifle has resolved, the second once the ability under it
-- has had its chance to resolve too.
abilityRun ::
  ObjectId.ObjectId ->
  ActivatedAbility.ActivatedAbility Card.Type.Card ->
  ObjectId.ObjectId ->
  GameState.GameState ->
  (GameState.GameState, GameState.GameState)
abilityRun srcId ability stifleId gs =
  let activated = S.runPure counteringAtAlice (gs {GameState.priority = Just S.alice}) (Activate.activateAbility S.alice srcId ability)
      cast = S.runPure counteringAtAbility activated (S.cast S.bob stifleId)
      stifleResolved = S.runPure counteringAtAbility cast Stack.resolveTop
      placed = S.runPure counteringAtAbility stifleResolved Engine.settleForPriority
   in (placed, S.runPure counteringAtAlice placed Stack.resolveTop)

-- Spider-Punk {1}{R} Legendary Creature -- Spider Human Hero 2/1 (Marvel's
-- ONE board for Yawgmoth's Will, built once and branched. alice has six untapped
-- Swamps -- three for the Will's {2}{B} and three left over, so no assertion
-- below can turn on mana -- the Will in hand, and a Sign in Blood ({B}{B}, no
-- flashback and no casting permission of its own) in her graveyard. bob holds
-- exactly the same six Swamps and the same card in HIS graveyard, which is what
-- makes the CR 109.5 scope observable: the two seats differ in the grant and in
-- nothing else. Both libraries are stocked, since Sign in Blood draws and CR
-- 104.3c would otherwise decide the game before an assertion ran.
--
-- Returns the Will, alice's graveyard card, bob's, and the board.
willBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
willBoard swamp will signInBlood =
  let lands = S.landsFor swamp S.bob 6 (S.landsInPlay swamp 6)
      stock pid gs = List.foldl' (\g _ -> snd (S.addLibraryCard swamp pid g)) gs [1 :: Int .. 3]
      stocked = stock S.bob (stock S.alice lands)
      (willId, withWill) = S.addHandCard will S.alice stocked
      (hers, withHers) = S.addGraveyardCard signInBlood S.alice withWill
      (his, withHis) = S.addGraveyardCard signInBlood S.bob withHers
   in ( willId,
        hers,
        his,
        withHis
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- The same board with the Will cast and resolved.
willResolved :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
willResolved willId gs = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (S.cast S.alice willId)) Stack.resolveTop

-- CR 601.3 / Yawgmoth's Will {2}{B} Sorcery: "Until end of turn, you may play
-- lands and cast spells from your graveyard. If a card would be put into your
-- graveyard from anywhere this turn, exile that card instead."
--
-- The PLAYER-scoped half of CR 601.3's allow clause, where flashback (CastSpec's
-- Firebolt group) is the object-scoped half: the card that becomes castable
-- carries no permission of its own and never learns one.
--
-- The play-lands half of the first sentence is NOT implemented -- a land is
-- played and never cast (CR 305.1), and no effect can grant a zone permission on
-- the play side (#1364). pawl's Yawgmoth's Will is therefore STRICTER than
-- printed, which the last case here pins.
yawgmothsWillSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
yawgmothsWillSpec s registry =
  let board = do
        swamp <- S.printingOf s registry "Swamp"
        will <- S.printingOf s registry "Yawgmoth's Will"
        signInBlood <- S.printingOf s registry "Sign in Blood"
        pure (willBoard swamp will signInBlood)
   in Spec.describe s "YawgmothsWill" $ do
        -- The control. Without it every refusal below would also be true of a
        -- board where Sign in Blood was simply unaffordable or the timing wrong.
        Spec.it s "CR 601.3 before the Will resolves the graveyard card is not castable" $ do
          (willId, hers, _, gs) <- board
          Spec.assertBool s (S.castable S.alice willId gs) "the Will itself is castable"
          Spec.assertBool s (not (S.castable S.alice hers gs)) "but the card in the graveyard is not"
          Spec.assertBool s (not (any (S.isCastOf hers) (Action.legalActions S.alice gs))) "and not offered"

        -- The whole card, end to end: graveyard -> stack -> EXILE. The exile is
        -- the second sentence, and the card would be back in the graveyard
        -- without it.
        Spec.it s "CR 601.3 with the Will resolved the graveyard card is cast, resolves and is exiled" $ do
          (willId, hers, _, gs) <- board
          let after = willResolved willId gs
          Spec.assertBool s (S.castable S.alice hers after) "castable from the graveyard"
          Spec.assertBool s (any (S.isCastOf hers) (Action.legalActions S.alice after)) "and offered"
          let cast = S.runPure S.identityAnswer after (S.cast S.alice hers)
              resolved = S.runPure S.identityAnswer cast Stack.resolveTop
          Spec.assertEqWith s "it drew and cost 2 life" (S.lifeOf S.alice resolved) (Just 18)
          Spec.assertEqWith s "it is not back in the graveyard" (Game.zoneMembers Zone.Graveyard S.alice resolved) []
          Spec.assertEqWith s "it was exiled instead, beside the Will" (length (Game.zoneMembers Zone.Exile S.alice resolved)) 2

        -- The ruling: "It will exile itself since it goes to the graveyard after
        -- its effect starts." Both sentences of the card in one assertion --
        -- the replacement is already in force when CR 608.2n moves the Will.
        Spec.it s "CR 608.2n / 614.1a the Will exiles itself" $ do
          (willId, hers, _, gs) <- board
          will <- S.printingOf s registry "Yawgmoth's Will"
          let after = willResolved willId gs
          -- By NAME, not by id: CR 400.7 makes the card leaving the stack a new
          -- object, so `willId` names nothing once it has moved.
          Spec.assertEqWith s "the graveyard holds only what was already there" (Game.zoneMembers Zone.Graveyard S.alice after) [hers]
          Spec.assertEqWith s "and the Will is in exile" (fmap (\o -> S.soleFaceName o after) (Game.zoneMembers Zone.Exile S.alice after)) [S.printingName will]

        -- CR 109.5 / PlayerScope.You: alice's Will does nothing for bob, whose
        -- board is hers in every other respect. Asked of the typed question as
        -- well as of the gate, because CR 307.1's sorcery window is shut for bob
        -- on alice's turn and would refuse his cast on its own.
        Spec.it s "CR 109.5 the You scope does not reach bob's graveyard" $ do
          (willId, hers, his, gs) <- board
          let after = willResolved willId gs
              bobsTurn = after {GameState.activePlayer = S.bob, GameState.priority = Just S.bob}
          Spec.assertBool s (PlayerEffect.mayCastFromGraveyard S.alice hers after) "alice has the permission"
          Spec.assertBool s (not (PlayerEffect.mayCastFromGraveyard S.bob his after)) "bob does not"
          Spec.assertBool s (not (S.castable S.bob his bobsTurn)) "and it is not castable even in his own main phase"
          Spec.assertBool s (not (any (S.isCastOf his) (Action.legalActions S.bob bobsTurn))) "nor offered"

        -- The permission names a ZONE, not a TIME, which is the flashback ruling
        -- one rule over ("you can cast a sorcery using flashback only when you
        -- could normally cast a sorcery"). Read beside Cast.instantSpeed rather
        -- than inside it, so the sorcery window still has to be open.
        Spec.it s "CR 117.1a the grant does not lift the sorcery timing restriction" $ do
          (willId, hers, _, gs) <- board
          let after = willResolved willId gs
              upkeep = after {GameState.phase = Phase.Beginning BeginningStep.Upkeep}
          Spec.assertBool s (S.castable S.alice hers after) "castable in her own main phase"
          Spec.assertBool s (not (S.castable S.alice hers upkeep)) "not in her upkeep"

        -- CR 514.2: "until end of turn" is the stored CR 611.2c carrier's expiry,
        -- so the grant dies at cleanup and the same board refuses the same cast.
        Spec.it s "CR 514.2 the permission ends at cleanup" $ do
          (willId, hers, _, gs) <- board
          let after = willResolved willId gs
              ended = S.runPure S.identityAnswer after (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup))
          Spec.assertEqWith s "one stored effect while it lasts" (length (GameState.playerEffects after)) 1
          Spec.assertEqWith s "nothing stored afterwards" (GameState.playerEffects ended) []
          Spec.assertBool s (not (PlayerEffect.mayCastFromGraveyard S.alice hers ended)) "the permission is gone"
          Spec.assertBool s (not (S.castable S.alice hers ended)) "and the cast is refused again"

        -- CR 305.1: the play-lands half of the same sentence has no carrier, so a
        -- land in the graveyard stays unplayable (#1364). pawl's card is stricter
        -- than printed here, and this is what says so.
        Spec.it s "CR 305.1 a land in the graveyard is still not playable" $ do
          (willId, _, _, gs) <- board
          swamp <- S.printingOf s registry "Swamp"
          let (landId, withLand) = S.addGraveyardCard swamp S.alice gs
              after = willResolved willId withLand
          Spec.assertBool s (notElem (Action.Type.Play landId Nothing) (Action.legalActions S.alice after)) "not offered as a land play"

-- Spider-Man, 92), "Spells and abilities can't be countered". Run four ways off
-- counteringBoard above, with a Goblin Piker as the victim spell.
--
-- All four of the card's printed clauses are in its file now, and only this one
-- is read here: nothing on this board prevents damage, no other Spider enters,
-- and S.addCreature inserts Spider-Punk into the battlefield directly rather
-- than raising an entry event, so CR 702.136a's riot has no CR 614.1c
-- replacement to be. CR 615.12's clause is proved in Pawl.ReplacementSpec's
-- "Spider-Punk (CR 615.12)" group instead, where a Mending Hands shield gives it
-- something to defeat.
spiderPunkSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spiderPunkSpec s registry =
  let withAbility act = do
        island <- S.printingOf s registry "Island"
        cancel <- S.printingOf s registry "Cancel"
        stifle <- S.printingOf s registry "Stifle"
        sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
        piker <- S.printingOf s registry "Goblin Piker"
        punk <- S.printingOf s registry "Spider-Punk"
        case Face.activatedAbilities (S.combinedFace sorcerer) of
          [] -> Spec.assertFailure s "Prodigal Sorcerer should declare one activated ability"
          ability : _ -> act (counteringBoard island cancel stifle sorcerer piker) punk piker ability
   in Spec.describe s "SpiderPunk" $ do
        -- The CONTROL for the spell half. Without it every refusal below would
        -- also be true of a board where the Cancel never resolved at all.
        Spec.it s "CR 701.6a without Spider-Punk bob's Cancel counters alice's spell"
          . withAbility
          $ \board _ piker _ -> do
            let (victimId, _, cancelId, _, _, gs) = board []
                after = cancelRun victimId cancelId gs
            Spec.assertEqWith s "the stack is empty" (GameState.stack after) []
            Spec.assertEqWith s "the spell is in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
            Spec.assertEqWith s "and never reached the battlefield" (S.countOnBattlefieldByName (S.printingName piker) S.alice after) 0

        -- The SPELL half. CR 611.1's third clause makes Spider-Punk's sentence
        -- a rules-modifying continuous effect, and CR 101.2 makes its "can't"
        -- win: the Cancel resolves, does nothing, and CR 608.2n puts it into
        -- bob's graveyard while the spell it named stays on the stack.
        Spec.it s "CR 701.6a / 613.11 with Spider-Punk the same Cancel counters nothing"
          . withAbility
          $ \board punk _ _ -> do
            let (victimId, _, cancelId, _, _, gs) = board [(S.alice, punk)]
                after = cancelRun victimId cancelId gs
            Spec.assertEqWith s "alice's spell is still on the stack, alone" (GameState.stack after) [victimId]
            Spec.assertEqWith s "alice's graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0
            Spec.assertEqWith s "and the spent Cancel is bob's only graveyard card" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1

        -- The CONTROL for the ability half, and CR 113.9's reason it needs its
        -- own: an ability on the stack is not a spell, so nothing the spell
        -- case proves carries over.
        Spec.it s "CR 113.9 without Spider-Punk bob's Stifle counters alice's ability"
          . withAbility
          $ \board _ _ ability -> do
            let (victimId, srcId, _, stifleId, _, gs) = board []
                (placed, after) = abilityRun srcId ability stifleId gs
            Spec.assertEqWith s "the ability is gone, leaving only the Piker spell" (GameState.stack placed) [victimId]
            Spec.assertEqWith s "alice took no damage, so it never resolved" (S.lifeOf S.alice after) (Just 20)
            Spec.assertEqWith s "and bob's graveyard holds the spent Stifle alone" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1

        -- THE case Spider-Punk is in the pool for, and the half no card could
        -- reach before:
        -- Spider-Punk's clause is an ability of a BATTLEFIELD PERMANENT about
        -- other objects, where Pawl.Types.Counterability is CR 113.6g's
        -- self-referential ability of the spell itself and can say nothing
        -- about an ability at all. The ability survives the Stifle and
        -- resolves, so alice takes the 1 damage she aimed at herself.
        Spec.it s "CR 701.6a / 113.9 with Spider-Punk the ability survives the Stifle and resolves"
          . withAbility
          $ \board punk _ ability -> do
            let (victimId, srcId, _, stifleId, _, gs) = board [(S.alice, punk)]
                (placed, after) = abilityRun srcId ability stifleId gs
            Spec.assertEqWith s "the ability is still on the stack, above the Piker spell" (length (GameState.stack placed)) 2
            Spec.assertEqWith s "the spent Stifle is bob's only graveyard card" (length (Game.zoneMembers Zone.Graveyard S.bob placed)) 1
            Spec.assertEqWith s "and resolving it deals alice the 1 damage" (S.lifeOf S.alice after) (Just 19)
            Spec.assertEqWith s "leaving the Piker spell alone on the stack" (GameState.stack after) [victimId]

        -- PlayerScope.EachPlayer, and the case that tells it from
        -- PlayerScope.You: Spider-Punk's sentence has no possessive, so BOB's
        -- copy protects ALICE's spell from bob's own Cancel.
        Spec.it s "CR 109.5 EachPlayer: bob's own Spider-Punk protects alice's spell"
          . withAbility
          $ \board punk _ _ -> do
            let (victimId, _, cancelId, _, _, gs) = board [(S.bob, punk)]
                after = cancelRun victimId cancelId gs
            Spec.assertEqWith s "alice's spell is still on the stack" (GameState.stack after) [victimId]
            Spec.assertEqWith s "and alice's graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0

        -- CR 604.2: gathered live off the battlefield on every read, so
        -- destroying Spider-Punk lifts the protection in the same turn with
        -- nothing to unwind.
        Spec.it s "CR 604.2 destroying Spider-Punk makes the spell counterable again"
          . withAbility
          $ \board punk _ _ -> do
            let (victimId, _, cancelId, _, punkIds, gs) = board [(S.alice, punk)]
                gone = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable punkIds)
            Spec.assertBool s (PlayerEffect.cantBeCountered S.alice victimId gs) "protected while it stands"
            Spec.assertEqWith s "so the same Cancel counters nothing while it stands" (GameState.stack (cancelRun victimId cancelId gs)) [victimId]
            Spec.assertBool s (not (PlayerEffect.cantBeCountered S.alice victimId gone)) "not protected once it is gone"
            -- The stack is the readout, not the graveyard's size: the destroyed
            -- Spider-Punk is in that graveyard too, so a bare count could not
            -- tell a countered spell from an uncountered one.
            Spec.assertEqWith s "and the Cancel now counters, emptying the stack" (GameState.stack (cancelRun victimId cancelId gone)) []
            Spec.assertEqWith s "leaving the spell beside the destroyed Spider-Punk" (length (Game.zoneMembers Zone.Graveyard S.alice (cancelRun victimId cancelId gone))) 2

        -- CR 113.6g's carrier is untouched, which is what keeps the two apart:
        -- Spider-Punk's OWN card says nothing about being countered, and the
        -- protection it hands out comes from the CR 613.11 axis alone.
        Spec.it s "CR 113.6g Spider-Punk's own card field is Counterable" $ do
          punk <- S.printingOf s registry "Spider-Punk"
          Spec.assertEqWith s "the card field" (Face.counterability (S.combinedFace punk)) Counterability.Counterable

-- Prowling Serpopard {1}{G}{G} Creature -- Cat Snake 4/3 (Amonkhet, 180),
-- "This spell can't be countered. Creature spells you control can't be
-- countered." BOTH of the card's sentences, on the two different carriers the
-- rules give them:
--
--   * "This spell can't be countered" is CR 113.6g's self-referential ability,
--     which functions on the stack and rides the card as
--     Pawl.Types.Counterability.
--   * "Creature spells you control can't be countered" is an ability of a
--     BATTLEFIELD PERMANENT about OTHER objects, so CR 611.1's third clause
--     makes it a rules-modifying continuous effect on the CR 613.11 player
--     axis.
--
-- The second sentence is why the card is in THIS file and not only among the CR
-- 113.6g cards: it NARROWS by the victim spell's own qualities, which
-- Spider-Punk's unfiltered "Spells and abilities can't be countered" does not.
-- The whole group therefore turns on the same Cancel counting differently for a
-- CREATURE spell and a NONCREATURE one on one board -- an assertion no
-- unfiltered arm can pass.
prowlingSerpopardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
prowlingSerpopardSpec s registry =
  let withVictim name act = do
        island <- S.printingOf s registry "Island"
        cancel <- S.printingOf s registry "Cancel"
        stifle <- S.printingOf s registry "Stifle"
        sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
        victim <- S.printingOf s registry name
        cat <- S.printingOf s registry "Prowling Serpopard"
        case Face.activatedAbilities (S.combinedFace sorcerer) of
          [] -> Spec.assertFailure s "Prodigal Sorcerer should declare one activated ability"
          ability : _ -> act (counteringBoard island cancel stifle sorcerer victim) cat ability
   in Spec.describe s "ProwlingSerpopard" $ do
        -- The CONTROL for the creature half.
        Spec.it s "CR 701.6a without Prowling Serpopard bob's Cancel counters alice's creature spell"
          . withVictim "Goblin Piker"
          $ \board _ _ -> do
            let (victimId, _, cancelId, _, _, gs) = board []
            Spec.assertEqWith s "the stack is empty" (GameState.stack (cancelRun victimId cancelId gs)) []

        -- The clause the card is in the pool for, in the direction the filter
        -- ADMITS: a creature spell alice controls matches CR 613.11's effect and
        -- CR 101.2 makes its "can't" win.
        Spec.it s "CR 701.6a / 613.11 with Prowling Serpopard alice's creature spell survives"
          . withVictim "Goblin Piker"
          $ \board cat _ -> do
            let (victimId, _, cancelId, _, _, gs) = board [(S.alice, cat)]
                after = cancelRun victimId cancelId gs
            Spec.assertEqWith s "alice's creature spell is still on the stack, alone" (GameState.stack after) [victimId]
            Spec.assertEqWith s "alice's graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0

        -- The CONTROL for the noncreature half, so the refusal below is a
        -- statement about the FILTER rather than about a Cancel that never
        -- resolved.
        Spec.it s "CR 701.6a without Prowling Serpopard bob's Cancel counters alice's noncreature spell"
          . withVictim "Lightning Bolt"
          $ \board _ _ -> do
            let (victimId, _, cancelId, _, _, gs) = board []
            Spec.assertEqWith s "the stack is empty" (GameState.stack (cancelRun victimId cancelId gs)) []

        -- THE case #788 is about, and the one an unfiltered arm CANNOT pass:
        -- the very same Serpopard, on the very same board, leaves alice's
        -- noncreature spell counterable, because CR 613.11's effect names only
        -- creature spells.
        Spec.it s "CR 701.6a / 613.11 the same Serpopard leaves alice's noncreature spell counterable"
          . withVictim "Lightning Bolt"
          $ \board cat _ -> do
            let (victimId, _, cancelId, _, _, gs) = board [(S.alice, cat)]
                after = cancelRun victimId cancelId gs
            Spec.assertEqWith s "the Cancel counters it, emptying the stack" (GameState.stack after) []
            -- The stack is the readout and the graveyard only corroborates it:
            -- alice's graveyard holds the countered spell and nothing else, so
            -- the Serpopard on the battlefield is not being counted here.
            Spec.assertEqWith s "leaving the countered spell in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1

        -- PlayerScope.You, and the case that tells it from Spider-Punk's
        -- EachPlayer: "creature spells YOU control", so BOB's copy protects
        -- nothing of alice's.
        Spec.it s "CR 109.5 You: bob's own Prowling Serpopard does not protect alice's creature spell"
          . withVictim "Goblin Piker"
          $ \board cat _ -> do
            let (victimId, _, cancelId, _, _, gs) = board [(S.bob, cat)]
            Spec.assertEqWith s "the Cancel still counters, emptying the stack" (GameState.stack (cancelRun victimId cancelId gs)) []

        -- CR 113.9 / 701.6a's OTHER subject. An ability on the stack has no
        -- card behind it -- Game.faceOf answers Nothing for one -- so a Filter
        -- naming a CARD TYPE can never match it, and alice's Prodigal Sorcerer
        -- ability is Stifled with the Serpopard standing. That is the whole of
        -- the answer to the wrinkle a filtered arm has and the unfiltered one
        -- does not: this card's protection reaches spells only.
        Spec.it s "CR 113.9 with Prowling Serpopard alice's activated ability is still counterable"
          . withVictim "Goblin Piker"
          $ \board cat ability -> do
            let (victimId, srcId, _, stifleId, _, gs) = board [(S.alice, cat)]
                (placed, after) = abilityRun srcId ability stifleId gs
            Spec.assertEqWith s "the ability is gone, leaving only the creature spell" (GameState.stack placed) [victimId]
            Spec.assertEqWith s "alice took no damage, so it never resolved" (S.lifeOf S.alice after) (Just 20)

        -- CR 113.6g, the card's FIRST sentence, on the carrier that is not the
        -- player axis at all: a Prowling Serpopard SPELL is uncounterable with
        -- NO Serpopard on the battlefield, which no CR 613.11 effect could
        -- explain.
        Spec.it s "CR 113.6g a Prowling Serpopard spell can't be countered with none on the battlefield"
          . withVictim "Prowling Serpopard"
          $ \board cat _ -> do
            let (victimId, _, cancelId, _, _, gs) = board []
            Spec.assertEqWith s "the card field" (Face.counterability (S.combinedFace cat)) Counterability.CantBeCountered
            Spec.assertEqWith s "and the spell is still on the stack" (GameState.stack (cancelRun victimId cancelId gs)) [victimId]

-- Jared Carthalion, True Heir {R}{G}{W} Legendary Creature -- Human Warrior 3/3
-- (Commander Legends, 281): "When Jared Carthalion enters, target opponent
-- becomes the monarch. You can't become the monarch this turn." One trigger
-- carrying both sentences, which is how the card prints them.
--
-- The card is in the pool for the second sentence, and it is the ONLY printing
-- that restricts who may be crowned -- which makes it the sole producer of CR
-- 725.4's "the next player in turn order who can become the monarch". CR 725.1
-- and CR 725.3 gate nobody, so on the ordinary route it is CR 101.2 that makes
-- the "can't" win.
--
-- Its third sentence -- "If damage would be dealt to Jared Carthalion while
-- you're the monarch, prevent that damage and put that many +1/+1 counters on
-- it" -- is omitted from data/cards/jared-carthalion-true-heir.json: a STATIC
-- prevention ability cannot carry CR 615.5's rider (#1105). The omission takes
-- both a shield and its counters away from Jared's own controller, so pawl's card
-- is strictly weaker than the printed one.
--
-- Two seats and no departure, which is all the primary observable needs. Every
-- case runs on one board -- alice's Jared, her Palace Jailer and her Goblin Piker
-- on the battlefield, nobody crowned -- and differs only in which enters-the-
-- battlefield event is fed to the trigger gatherer. That is what makes each
-- negative a statement about the restriction rather than about a board that could
-- not crown anyone anyway.
--
-- Palace Jailer ("When Palace Jailer enters, you become the monarch") is the
-- second route on purpose: MonarchTarget.TheController, where Jared's own first
-- clause is MonarchTarget.InSlot and CR 725.2's steal is ControllerOfSource. All
-- three read one gate, so no case here passes through an ungated route.
jaredBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
jaredBoard jared jailer piker =
  let (jaredId, gs1) = S.addCreature jared S.alice (Setup.emptyGame S.bothPlayers)
      (jailerId, gs2) = S.addCreature jailer S.alice gs1
      (pikerId, gs3) = S.addCreature piker S.alice gs2
   in (jaredId, jailerId, pikerId, gs3)

-- One permanent's CR 603.6a entry, gathered and resolved. The permanent is already
-- on the battlefield, so this feeds the event alone -- the same staging
-- ExpirySpec's monarch group uses.
etbResolved :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
etbResolved oid gs =
  let entered = ZoneChange.MkZoneChange oid oid Zone.Stack Zone.Battlefield
      withEvent = S.withEvents [GameEvent.Moved entered (Projection.project oid gs)] gs
   in S.runPure S.identityAnswer (S.runPure S.identityAnswer withEvent Engine.settleForPriority) Engine.priorityLoop

-- CR 725.2's crown steal, as the event it triggers off: `attacker` deals combat
-- damage to bob, who must be the monarch for the inherent ability to match.
damageToTheMonarch :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
damageToTheMonarch attacker gs =
  let dmg = DamageEvent.MkDamageEvent attacker (Recipient.ToPlayer S.bob) 2 False False False 0 Nothing DamageKind.Combat
      withEvent = S.withEvents [GameEvent.DamageDealt dmg] gs
   in S.runPure S.identityAnswer (S.runPure S.identityAnswer withEvent Engine.settleForPriority) Engine.priorityLoop

jaredSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
jaredSpec s registry =
  let board = do
        jared <- S.printingOf s registry "Jared Carthalion, True Heir"
        jailer <- S.printingOf s registry "Palace Jailer"
        piker <- S.printingOf s registry "Goblin Piker"
        pure (jaredBoard jared jailer piker)
   in Spec.describe s "JaredCarthalionTrueHeir" $ do
        -- The card's own first clause, which is also what puts a monarch on the
        -- board for everything below: CR 601.2c's target slot, re-read at
        -- resolution, crowning the ONLY opponent two seats offer.
        Spec.it s "CR 725.1 his enters trigger crowns the targeted opponent, and stores the restriction on his controller" $ do
          (jaredId, _, _, gs) <- board
          let after = etbResolved jaredId gs
          Spec.assertEqWith s "bob is the monarch" (GameState.monarch after) (Just S.bob)
          Spec.assertEqWith s "one stored CR 611.2c effect" (fmap ActivePlayerEffect.effect (GameState.playerEffects after)) [PlayerEffect.Type.CantBecomeMonarch]
          Spec.assertEqWith s "scoped to its controller" (fmap ActivePlayerEffect.scope (GameState.playerEffects after)) [PlayerScope.You]
          Spec.assertEqWith s "who is alice" (fmap ActivePlayerEffect.controller (GameState.playerEffects after)) [S.alice]
          Spec.assertBool s (PlayerEffect.prohibitsBecomingMonarch S.alice after) "so alice can't become the monarch"
          Spec.assertBool s (not (PlayerEffect.prohibitsBecomingMonarch S.bob after)) "and bob still can"

        -- THE CONTROL for the case below, on the same board: with Jared's trigger
        -- never fed, the Jailer's "you become the monarch" crowns alice. Without
        -- this, the refusal below could be a Jailer whose ETB never resolved.
        Spec.it s "CR 725.1 with no restriction standing, Palace Jailer's enters trigger crowns alice" $ do
          (_, jailerId, _, gs) <- board
          Spec.assertEqWith s "alice takes the crown" (GameState.monarch (etbResolved jailerId gs)) (Just S.alice)

        -- THE PRIMARY OBSERVABLE. Two seats, no departure: an
        -- Effect.BecomeMonarch aimed at a restricted player does nothing, and CR
        -- 725.3's "the current monarch ceases to be the monarch" never fires
        -- either -- bob keeps the crown rather than the game losing it.
        Spec.it s "CR 101.2 / 725.1 the restriction stops Palace Jailer's TheController crowning outright" $ do
          (jaredId, jailerId, _, gs) <- board
          let restricted = etbResolved jaredId gs
              after = etbResolved jailerId restricted
          Spec.assertEqWith s "bob keeps the crown" (GameState.monarch after) (Just S.bob)
          Spec.assertEqWith s "and no crowning of alice was recorded" (filter (== GameEvent.BecameMonarch S.alice) (S.eventsOf after)) []

        -- CR 611.2a/514.2: the duration is the stored carrier's expiry and
        -- nothing else, so the SAME Jailer trigger crowns alice once the turn has
        -- ended. This is what says the restriction is "this turn" rather than
        -- permanent.
        Spec.it s "CR 514.2 the restriction ends at cleanup, and then the same crowning lands" $ do
          (jaredId, jailerId, _, gs) <- board
          let restricted = etbResolved jaredId gs
              ended = S.runPure S.identityAnswer restricted (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup))
          Spec.assertEqWith s "nothing stored" (GameState.playerEffects ended) []
          Spec.assertBool s (not (PlayerEffect.prohibitsBecomingMonarch S.alice ended)) "alice may be crowned again"
          Spec.assertEqWith s "so the Jailer's ETB now crowns her" (GameState.monarch (etbResolved jailerId ended)) (Just S.alice)

        -- THE CONTROL for CR 725.2's route, with bob crowned by the fixture
        -- instead of by Jared's trigger: an unrestricted alice takes the crown off
        -- a creature's combat damage.
        Spec.it s "CR 725.2 with no restriction standing, combat damage to the monarch hands alice the crown" $ do
          (_, _, pikerId, gs) <- board
          Spec.assertEqWith s "alice steals it" (GameState.monarch (damageToTheMonarch pikerId (S.withMonarch S.bob gs))) (Just S.alice)

        -- The vacuity trap this issue was filed with: CR 725.2's inherent ability
        -- is SOURCELESS and reaches the crown through MonarchTarget
        -- .ControllerOfSource, a different arm from the case above. The gate is
        -- read at the one place all three arms meet, so the steal is stopped too
        -- -- and the ability still triggers and still resolves, it just crowns
        -- nobody.
        Spec.it s "CR 101.2 / 725.2 the restriction stops the sourceless crown steal as well" $ do
          (jaredId, _, pikerId, gs) <- board
          let restricted = etbResolved jaredId gs
          Spec.assertEqWith s "bob was crowned by Jared's own trigger" (GameState.monarch restricted) (Just S.bob)
          Spec.assertEqWith s "and keeps the crown through alice's combat damage" (GameState.monarch (damageToTheMonarch pikerId restricted)) (Just S.bob)

-- CR 601.3a / Void Winnower {9} Creature -- Eldrazi: "Your opponents can't cast
-- spells with even mana values. (Zero is even.)"
--
-- ONE board, built twice, and `extra` is the only thing the two ever differ by.
-- alice and bob each have nine untapped Mountains, so mana is never why a cast is
-- missing, and each holds a Goblin Piker ({1}{R}, mana value 2 -- EVEN). bob also
-- holds a Lightning Bolt ({R}, mana value 1 -- ODD) and a Molten Disaster
-- ({X}{R}{R}), whose mana value is 2 in his hand by CR 202.3e and either parity
-- once X is chosen.
--
-- The three cards bob holds are the discriminating set: the Bolt differs from the
-- Piker in PARITY alone, and the Disaster differs from the Piker in the VARIABLE
-- alone -- same seat, same mana, same moment, same even mana value. alice's own
-- Piker is the SCOPE control, since no EachPlayer reading of the ability could
-- leave it castable.
--
-- Returns (alice's Piker, bob's Piker, bob's Bolt, bob's Disaster, board).
voidWinnowerBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  [Printing.Printing] ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
voidWinnowerBoard mountain piker bolt disaster extra =
  let base = S.landsInPlay mountain 9
      withBobsLands = List.foldl' (\g _ -> snd (S.addCreature mountain S.bob g)) base [1 .. 9 :: Int]
      (alicesPiker, gs1) = S.addHandCard piker S.alice withBobsLands
      (bobsPiker, gs2) = S.addHandCard piker S.bob gs1
      (bobsBolt, gs3) = S.addHandCard bolt S.bob gs2
      (bobsDisaster, gs4) = S.addHandCard disaster S.bob gs3
      put g printing = snd (S.addCreature printing S.alice g)
   in ( alicesPiker,
        bobsPiker,
        bobsBolt,
        bobsDisaster,
        (List.foldl' put gs4 extra) {GameState.phase = Phase.PrecombatMain}
      )

-- Whatever that player may do, asked in their own precombat main phase with an
-- empty stack -- so a sorcery, a creature spell and an instant are all inside CR
-- 307.1's window and timing is never the reason one is missing.
askedOf :: PlayerId.PlayerId -> GameState.GameState -> [Action.Type.Action]
askedOf pid gs = Action.legalActions pid (gs {GameState.activePlayer = pid, GameState.priority = Just pid})

voidWinnowerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
voidWinnowerSpec s registry =
  Spec.describe s "VoidWinnower" $ do
    -- CR 601.3a's quality-bearing prohibition on the axis a NAME cannot answer:
    -- the two cards refused and allowed here are told apart by their mana value
    -- and by nothing else.
    Spec.it s "CR 601.3a an opponent's even spell is refused and their odd one is not" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      bolt <- S.printingOf s registry "Lightning Bolt"
      disaster <- S.printingOf s registry "Molten Disaster"
      winnower <- S.printingOf s registry "Void Winnower"
      let (alicesPiker, bobsPiker, bobsBolt, _, board) = voidWinnowerBoard mountain piker bolt disaster [winnower]
          (_, barePiker, _, _, bare) = voidWinnowerBoard mountain piker bolt disaster []
      Spec.assertBool s (not (any (S.isCastOf bobsPiker) (askedOf S.bob board))) "the mana value 2 spell is refused"
      Spec.assertBool s (any (S.isCastOf bobsBolt) (askedOf S.bob board)) "the mana value 1 spell, off the same lands, is not"
      Spec.assertBool s (any (S.isCastOf alicesPiker) (askedOf S.alice board)) "and the Winnower's own controller may cast that same card"
      Spec.assertBool s (any (S.isCastOf barePiker) (askedOf S.bob bare)) "the pair: with the Winnower gone bob's Piker is castable"

    -- CR 601.3a's LOOKAHEAD, and the pair is the whole case: both spells have a
    -- mana value of 2 in bob's hand (CR 202.3e), both are refused by a reading
    -- that stops at the board, and the {X} one is offered anyway because a choice
    -- bob has not yet made could take it out of the prohibited class.
    Spec.it s "CR 601.3a an {X} spell with an even mana value in hand may still be begun" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      bolt <- S.printingOf s registry "Lightning Bolt"
      disaster <- S.printingOf s registry "Molten Disaster"
      winnower <- S.printingOf s registry "Void Winnower"
      let (_, bobsPiker, _, bobsDisaster, board) = voidWinnowerBoard mountain piker bolt disaster [winnower]
      Spec.assertBool s (PlayerEffect.matchesObject Filter.Type.ManaValueIsEven bobsPiker board) "the fixed spell's mana value is even"
      Spec.assertBool s (PlayerEffect.matchesObject Filter.Type.ManaValueIsEven bobsDisaster board) "and so is the {X} spell's, while it sits in hand"
      Spec.assertBool s (not (any (S.isCastOf bobsPiker) (askedOf S.bob board))) "the fixed one is refused"
      Spec.assertBool s (any (S.isCastOf bobsDisaster) (askedOf S.bob board)) "and the {X} one is offered"

    -- The search's REACH, which no card in the pool pins: Void Winnower's own
    -- criterion is answered at the second sample, so a lookahead that only ever
    -- looked one step would pass every case above. A threshold criterion is the
    -- shape that needs the climb -- an {X}{R}{R} card escapes "mana value 5 or
    -- less" only at X = 4 -- and Pawl.Engine.Filter.manaValueThresholds is what
    -- tells the search how far to walk. Asked of the same real card in the same
    -- hand; only the criterion is written by the test.
    Spec.it s "CR 601.3a the search walks past every literal the criterion names" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      bolt <- S.printingOf s registry "Lightning Bolt"
      disaster <- S.printingOf s registry "Molten Disaster"
      let (_, bobsPiker, _, bobsDisaster, board) = voidWinnowerBoard mountain piker bolt disaster []
          cheap = Filter.Type.ManaValueAtMost 5
      Spec.assertBool s (PlayerEffect.matchesObject cheap bobsDisaster board) "the {X} spell is inside the class as it sits in hand"
      Spec.assertBool s (PlayerEffect.choiceCouldEscape cheap bobsDisaster board) "and a large enough X takes it out"
      Spec.assertBool s (not (PlayerEffect.choiceCouldEscape cheap bobsPiker board)) "while the fixed spell beside it has no choice to make"

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.PlayerEffect" $ do
  ruleOfLawSpec s registry
  nullChamberSpec s registry
  adjustmentSpec s
  thaliaSpec s registry
  medallionSpec s registry
  humilitySpec s registry
  titaniasSongSpec s registry
  edgewalkerSpec s registry
  phyrexianDiscountSpec s registry
  snowDiscountSpec s registry
  monocoloredHybridDiscountSpec s registry
  hybridDiscountSpec s registry
  textChangedEdgewalkerSpec s registry
  reliquaryTowerSpec s registry
  theTenRingsSpec s registry
  storedSpec s registry
  silenceSpec s registry
  blossomingCalmSpec s registry
  conditionalSilenceSpec s registry
  extraLandDropsSpec s registry
  matchesObjectSpec s registry
  vedalkenOrrerySpec s registry
  yawgmothsWillSpec s registry
  voidWinnowerSpec s registry
  spiderPunkSpec s registry
  prowlingSerpopardSpec s registry
  jaredSpec s registry
