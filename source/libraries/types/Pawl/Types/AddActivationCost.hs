module Pawl.Types.AddActivationCost where

import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | The payload of Pawl.Types.PlayerEffect's AddActivationCost arm (#1305).
--
-- The Filter matches the ability's SOURCE PERMANENT, as
-- Pawl.Types.ReduceActivationCost's does; the components are a list because one
-- sentence can name several actions.
data AddActivationCost = MkAddActivationCost
  { whichAbilities :: Filter.Filter Keyword.Keyword,
    components :: [CostComponent.CostComponent Keyword.Keyword]
  }
  deriving (Eq, Ord, Show)
