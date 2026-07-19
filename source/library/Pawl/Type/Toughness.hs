module Pawl.Type.Toughness where

import Pawl.Type.Quantity (Quantity)

newtype Toughness = MkToughness Quantity
  deriving (Eq, Ord, Show)
