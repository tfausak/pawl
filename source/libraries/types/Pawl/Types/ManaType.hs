module Pawl.Types.ManaType where

import Pawl.Types.Color (Color)

-- | CR 106.1: mana is either one of the five colors or colorless.
data ManaType
  = Colored Color
  | Colorless
  deriving (Eq, Ord, Show)
