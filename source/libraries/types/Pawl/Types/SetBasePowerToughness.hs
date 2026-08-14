module Pawl.Types.SetBasePowerToughness where

import qualified Pawl.Types.Quantity as Quantity

-- | The payload of Pawl.Types.Modification's SetBasePowerToughness arm (#1305):
-- layer 7b's replacement box. Both are Quantity, so Opalescence can set them to
-- the object's mana value.
data SetBasePowerToughness = MkSetBasePowerToughness
  { power :: Quantity.Quantity,
    toughness :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
