module Pawl.Types.Power where

import Pawl.Types.Quantity (Quantity)

newtype Power = MkPower Quantity
  deriving (Eq, Ord, Show)
