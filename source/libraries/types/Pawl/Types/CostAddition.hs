module Pawl.Types.CostAddition where

import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CostScale as CostScale
import qualified Pawl.Types.Keyword as Keyword

-- | CR 118.8's additional cost applied from another effect: the components it
-- adds, a list because one sentence can name several actions, and how many
-- times they join the cost (Drought's "for each black mana symbol").
data CostAddition = MkCostAddition
  { components :: [CostComponent.CostComponent Keyword.Keyword],
    scale :: CostScale.CostScale
  }
  deriving (Eq, Ord, Show)
