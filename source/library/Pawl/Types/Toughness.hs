module Pawl.Types.Toughness where

import qualified Pawl.Types.Quantity as Quantity

newtype Toughness = MkToughness
  { unwrap :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
