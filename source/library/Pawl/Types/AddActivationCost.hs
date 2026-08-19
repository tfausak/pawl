module Pawl.Types.AddActivationCost where

import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CostScale as CostScale
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | The payload of Pawl.Types.PlayerEffect's AddActivationCost arm (#1305).
--
-- The Filter matches the ability's SOURCE PERMANENT, as
-- Pawl.Types.ReduceActivationCost's does; the components are a list because one
-- sentence can name several actions.
--
-- The scale is how many times those components join the cost (#1417): Brutal
-- Suppression writes Once, Drought's "for each black mana symbol in their
-- activation costs" writes PerColoredSymbol. It is a DEFAULTED wire key
-- (Pawl.Codec.AddActivationCost), so an unscaled sentence writes nothing.
data AddActivationCost = MkAddActivationCost
  { whichAbilities :: Filter.Filter Keyword.Keyword,
    components :: [CostComponent.CostComponent Keyword.Keyword],
    scale :: CostScale.CostScale
  }
  deriving (Eq, Ord, Show)
