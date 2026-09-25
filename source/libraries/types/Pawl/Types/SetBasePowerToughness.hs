module Pawl.Types.SetBasePowerToughness where

import qualified Pawl.Types.Quantity as Quantity

-- | The payload of Pawl.Types.Modification's SetBasePowerToughness arm (#1305):
-- layer 7b's replacement box. Both are Quantity, so Opalescence can set them to
-- the object's mana value. CR 613.4b sets "power and/or toughness", so either
-- may be absent and is then left alone (CR 701.12g's exchange sets one).
data SetBasePowerToughness = MkSetBasePowerToughness
  { power :: Maybe Quantity.Quantity,
    toughness :: Maybe Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
