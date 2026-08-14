module Pawl.Types.SetPowerToughness where

-- | CR 707.9b's copy exception: the power and toughness the copy takes instead
-- of the copied object's.

-- BOTH fields are an Integer and they are NOT interchangeable, so they are named
-- rather than positional: Quicksilver Gargantuan is a 7/7, and a swap would be
-- invisible on a square body and wrong on every other.
data SetPowerToughness = MkSetPowerToughness
  { power :: Integer,
    toughness :: Integer
  }
  deriving (Eq, Ord, Show)
