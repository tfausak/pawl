module Pawl.Types.Toughness where

import Pawl.Types.Quantity (Quantity)

newtype Toughness = MkToughness
  { unwrap :: Quantity
  }
  deriving (Eq, Ord, Show)
