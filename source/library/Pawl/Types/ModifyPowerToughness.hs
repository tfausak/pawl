module Pawl.Types.ModifyPowerToughness where

import qualified Pawl.Types.Quantity as Quantity

-- | The payload of Pawl.Types.Modification's ModifyPowerToughness arm (#1305):
-- layer 7c's signed deltas, Giant Growth's +3/+3.
data ModifyPowerToughness = MkModifyPowerToughness
  { power :: Quantity.Quantity,
    toughness :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
