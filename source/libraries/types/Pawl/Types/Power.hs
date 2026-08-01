module Pawl.Types.Power where

import Pawl.Types.Quantity (Quantity)

newtype Power = MkPower
  { unwrap :: Quantity
  }
  deriving (Eq, Ord, Show)
