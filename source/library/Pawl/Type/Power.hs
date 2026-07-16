module Pawl.Type.Power where

import Pawl.Type.Quantity (Quantity)

newtype Power = MkPower Quantity
  deriving (Eq, Ord, Show)
