module Pawl.Types.Cost where

import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.ManaCost as ManaCost

-- | CR 118.1: a cost is an action or payment necessary to take another action.
-- ONE type for both carriers -- a spell's cost (CR 601.2f) and an activated
-- ability's activation cost (CR 602.1a) -- because the rules make them the same
-- thing: a mana part plus the non-mana components.
--
-- `mana` carries CR 118.6's distinction in the type, and the two cases are NOT
-- interchangeable. Nothing is an UNPAYABLE cost (CR 118.6), the same fact
-- Face.manaCost's Maybe carries for CR 202.1. Just (MkManaCost []) is {0}, which
-- is real and payable (CR 118.5, CR 118.5a) -- ManaCost is a list of symbols and
-- the empty list IS {0}. Scryfall spells the difference: Ancestral Vision's
-- mana_cost is '', Ornithopter's is '{0}'.
--
-- PARAMETRIC in the keyword for the components it carries, for Pawl.Types.Filter's
-- reason alone -- CR 702.29e's cycling carries a Cost, and a Filter can name a
-- Keyword. Every caller writes `Cost Keyword.Keyword`.
data Cost keyword = MkCost
  { mana :: Maybe ManaCost.ManaCost,
    components :: [CostComponent.CostComponent keyword]
  }
  deriving (Eq, Ord, Show)
