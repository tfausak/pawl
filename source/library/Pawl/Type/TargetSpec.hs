module Pawl.Type.TargetSpec where

-- What a target slot may legally hold. Classification data, never a predicate
-- function. AnyTarget is "any target": a creature or a player (CR 115.4's
-- damageable set, minus the card types that do not exist yet -- planeswalkers
-- and battles grow this).
data TargetSpec
  = AnyTarget
  deriving (Eq, Ord, Show)
