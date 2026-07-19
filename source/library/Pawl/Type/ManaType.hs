{-# LANGUAGE DeriveLift #-}

module Pawl.Type.ManaType where

import Language.Haskell.TH.Syntax (Lift)
import Pawl.Type.Color (Color)

-- CR 106.1: mana is either one of the five colors or colorless.
data ManaType
  = Colored Color
  | Colorless
  deriving (Eq, Lift, Ord, Show)
