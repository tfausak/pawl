module Pawl.Types.Measure where

-- | A numeric characteristic a Pawl.Types.Filter comparison reads off an
-- object's projected view (Pawl.Engine.Filter.measureOf).
data Measure
  = -- | CR 208.1: power.
    Power
  | -- | CR 208.1: toughness.
    Toughness
  | -- | CR 202.3: mana value.
    ManaValue
  deriving (Bounded, Enum, Eq, Ord, Show)
