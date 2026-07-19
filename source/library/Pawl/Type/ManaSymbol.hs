module Pawl.Type.ManaSymbol where

import Numeric.Natural (Natural)
import Pawl.Type.ManaType (ManaType)

-- CR 107.4. Grows: hybrid, Phyrexian, snow, variable (X).
data ManaSymbol
  = Generic Natural
  | OfType ManaType
  | -- CR 107.3 / 601.2b: the {X} symbol in a cost. Contributes the chosen value
    -- of X once chosen (0 before, for the CR 601.2b castability floor and for a
    -- mana value off the stack, CR 202.3b).
    Variable
  deriving (Eq, Ord, Show)
