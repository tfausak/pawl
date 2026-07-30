module Pawl.Types.Toughness where

import Pawl.Types.Quantity (Quantity)

newtype Toughness = MkToughness Quantity
  deriving (Eq, Ord, Show)
