{-# LANGUAGE DeriveLift #-}

module Pawl.Type.ManaSymbol where

import Language.Haskell.TH.Syntax (Lift)
import Numeric.Natural (Natural)
import Pawl.Type.ManaType (ManaType)

-- CR 107.4. Grows: hybrid, Phyrexian, snow, variable (X).
data ManaSymbol
  = Generic Natural
  | OfType ManaType
  deriving (Eq, Lift, Ord, Show)
