{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Cost and the three types it cases on (Pawl.Type.Cost,
-- Pawl.Type.CostComponent, Pawl.Type.Payment), plus the two prompts the axis
-- adds. CR 118: what a cost IS, what it takes to pay one, and the alternative
-- and additional costs that change the answer.
--
-- The three gate cards: Greed (an amount-bearing component), Village Rites (a
-- mandatory spell-side additional cost) and Fireblast (an alternative cost with
-- no mana in it at all).
module Pawl.CostSpec where

import qualified Pawl.Activate as Activate
import qualified Pawl.Cards as Cards
import qualified Pawl.Cost as Cost
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.Cost as Cost.Type
import qualified Pawl.Type.CostComponent as CostComponent
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.Payment as Payment
import qualified Pawl.Type.Printing as Printing
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- The single activated ability of a printing. Total: the fallback is unreachable
-- in these fixtures. Duplicated per this suite's convention of group-local
-- helpers (ActivateSpec and ReplacementSpec each carry their own).
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card
theAbility p = case Card.Type.activatedAbilities (Printing.card p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Card.Type.spell (Printing.card p))

doorTests :: Cards.Cards -> Tasty.TestTree
doorTests cards =
  Tasty.testGroup
    "Door"
    [ -- CR 118.3's own second example: "a permanent that's already tapped can't
      -- be tapped to pay a cost" (CR 107.5 says the same for the {T} symbol).
      HU.testCase "CR 107.5 TapThis is payable only while the permanent is untapped" $
        let (oid, gs) = S.addCreature (Cards.prodigalSorcererPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            tapped = S.tapObject oid gs
         in do
              HU.assertBool "untapped pays" (Cost.canPayComponent S.alice oid CostComponent.TapThis gs)
              HU.assertBool "tapped does not" (not (Cost.canPayComponent S.alice oid CostComponent.TapThis tapped)),
      -- CR 701.21a: "A player can't sacrifice something that isn't a permanent,
      -- or something that's a permanent they don't control."
      HU.testCase "CR 701.21a SacrificeThis needs a permanent this player controls" $
        let (onField, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (inHand, gs1) = S.addHandCard (Cards.pikerPrinting cards) S.alice gs0
         in do
              HU.assertBool "a controlled permanent pays" (Cost.canPayComponent S.alice onField CostComponent.SacrificeThis gs1)
              HU.assertBool "a card in hand does not" (not (Cost.canPayComponent S.alice inHand CostComponent.SacrificeThis gs1))
              HU.assertBool "another player's permanent does not" (not (Cost.canPayComponent S.bob onField CostComponent.SacrificeThis gs1)),
      -- CR 118.6 vs CR 118.5a: the distinction the Maybe carries. Nothing is an
      -- unpayable cost; an empty ManaCost is {0} and is payable.
      HU.testCase "CR 118.6 an unpayable cost can never be paid" $
        let gs = S.mountainsInPlay cards 5
         in HU.assertBool
              "Nothing is unpayable"
              (not (Cost.canPay S.alice S.noSource (Cost.Type.MkCost Nothing []) gs)),
      HU.testCase "CR 118.5a a {0} cost is payable" $
        let gs = Setup.emptyGame S.bothPlayers
         in HU.assertBool
              "an empty ManaCost is {0}"
              (Cost.canPay S.alice S.noSource (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) gs),
      -- CR 118.6a: "If an unpayable cost is increased by an effect or an
      -- additional cost is imposed, the cost is still unpayable." total maps over
      -- the Maybe, so there is no special case to get wrong.
      HU.testCase "CR 118.6a Thalia's increase leaves an unpayable cost unpayable" $
        let base = S.mountainsInPlay cards 5
            (_, gs) = S.addCreature (Cards.thaliaPrinting cards) S.alice base
            (bolt, withBolt) = S.addHandCard (Cards.lightningBoltPrinting cards) S.alice gs
         in HU.assertEqual
              "still Nothing"
              Nothing
              (Cost.Type.mana (Cost.total S.alice bolt (Cost.Type.MkCost Nothing []) withBolt)),
      -- The classification Pawl.Activate reads instead of matching a constructor.
      HU.testCase "CR 302.6 requiresTapSymbol classifies a cost, and Greed's counterpart proves it" $
        let elves = ActivatedAbility.cost (theAbility (Cards.llanowarElvesPrinting cards))
            skeletons = ActivatedAbility.cost (theAbility (Cards.drudgeSkeletonsPrinting cards))
         in do
              HU.assertBool "Llanowar Elves' {T} cost requires the tap symbol" (Cost.requiresTapSymbol elves)
              HU.assertBool "Drudge Skeletons' {B} regenerate cost does not" (not (Cost.requiresTapSymbol skeletons)),
      -- Departure 1: Pawl.Activate does NOT route an ability cost through
      -- Cost.total. PlayerEffect.matchesSpell classifies an OBJECT, not a spell,
      -- so a noncreature PERMANENT matches SpellCriterion.NoncreatureSpell --
      -- and Thalia taxes noncreature SPELLS, never abilities. Four Mountains
      -- must still afford Mindslaver's printed {4}; a fifth would be needed if
      -- the tax wrongly reached the activation (#90).
      HU.testCase "CR 613.11 Thalia does not tax a noncreature permanent's activated ability" $
        let base = S.mountainsInPlay cards 4
            (slaver, gs1) = S.addCreature (Cards.mindslaverPrinting cards) S.alice base
            (_, gs2) = S.addCreature (Cards.thaliaPrinting cards) S.alice gs1
         in HU.assertBool
              "four Mountains still pay {4}"
              (Activate.activatable S.alice slaver (theAbility (Cards.mindslaverPrinting cards)) gs2),
      -- Departure 2: an Unpaid payment is a complete no-op, never a partial one.
      HU.testCase "CR 118.6 paying an unpayable cost changes nothing" $
        let gs = S.mountainsInPlay cards 3
            (outcome, after) = S.runPureWith S.identityAnswer gs (Cost.pay S.alice S.noSource (Cost.Type.MkCost Nothing []))
         in do
              HU.assertEqual "Unpaid" Payment.Unpaid outcome
              HU.assertEqual "no land tapped" 0 (S.tappedCount S.alice after)
    ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Pawl.Cost" [doorTests cards]
