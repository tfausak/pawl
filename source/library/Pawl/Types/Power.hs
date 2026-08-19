module Pawl.Types.Power where

import qualified Pawl.Types.Quantity as Quantity

newtype Power = MkPower
  { unwrap :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
