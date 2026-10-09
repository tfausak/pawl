module Pawl.Types.PowerToughnessSticker where

import qualified Numeric.Natural as Natural

-- | CR 123.8: a power and toughness sticker.
data PowerToughnessSticker = MkPowerToughnessSticker
  { -- | CR 123.3c / 107.17a.
    tickets :: Natural.Natural,
    power :: Integer,
    toughness :: Integer
  }
  deriving (Eq, Ord, Show)
