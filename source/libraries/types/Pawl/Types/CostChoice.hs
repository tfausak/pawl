module Pawl.Types.CostChoice where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Keyword as Keyword

-- | CR 118.8 / 601.2b: one printed additional cost whose payment offers the
-- payer a CHOICE -- Caustic Exhale's "behold a Dragon or pay {1}".
--
-- A list of whole Costs and not of Pawl.Types.CostComponents, because a branch
-- can be MANA: CR 601.2f splits a cost into a mana part and the non-mana
-- components, and "or pay {1}" lives in the first. Only a Cost holds both
-- halves, which is why this cannot ride Pawl.Types.Face.additionalCosts.
--
-- The choice is made at CR 601.2b, as an announcement, and not at CR 601.2h as
-- a payment: Pawl.Engine.Cost.candidateCostsGiven offers one candidate cost per
-- option, so Prompt.ChooseCost asks it beside every other cost the caster is
-- choosing among and the total CR 601.2f locks in is already settled. An option
-- the board cannot pay is not offered (CR 118.3), which is the same narrowing
-- every other candidate gets.
--
-- NONEMPTY because a choice with no options would be a cost with no way to pay
-- it, which is CR 118.6's unpayable cost written by accident rather than by a
-- card. Two or more is what makes it a choice at all. One option is a mandatory
-- additional cost, which belongs here only when it pays MANA that
-- Face.additionalCosts has no part for -- Water Whip's waterbend {5} -- and
-- Pawl.CardSpec is what holds a card to that.
--
-- The options are in PRINTED order, which is the order Prompt.ChooseCost offers
-- them in.
--
-- MONOMORPHIC in the keyword where Cost is parametric, Pawl.Types.AlternativeCost's
-- reason: the Filter a component reaches is already fixed at Pawl.Types.Keyword,
-- so there is no parameter left to thread.
newtype CostChoice = MkCostChoice
  { unwrap :: NonEmpty.NonEmpty (Cost.Cost Keyword.Keyword)
  }
  deriving (Eq, Ord, Show)
