module Pawl.Types.PowerToughnessIs where

import qualified Pawl.Types.Reference as Reference

-- | CR 208.1 / 613.4: an object's projected power and toughness.
data PowerToughnessIs = MkPowerToughnessIs
  { object :: Reference.Reference,
    power :: Integer,
    toughness :: Integer
  }
  deriving (Eq, Ord, Show)
