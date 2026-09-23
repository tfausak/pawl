module Pawl.Types.RollAdjustment where

-- | CR 706.2b's second step: which way a RollModifier.IncreaseOrDecrease moves
-- the result, chosen as the modifier is applied.
data RollAdjustment
  = Increase
  | Decrease
  deriving (Bounded, Enum, Eq, Ord, Show)
