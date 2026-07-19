module Pawl.Type.AbilityCost where

import Pawl.Type.AdditionalCost (AdditionalCost)
import Pawl.Type.ManaCost (ManaCost)

-- The cost of an activated ability (CR 602.1). A mana part (CR 602.1b) plus the
-- non-mana additional costs. Nothing = no mana symbol in the cost (every M3e
-- ability); Mindslaver's {4} is the first Just.
data AbilityCost = MkAbilityCost
  { mana :: Maybe ManaCost,
    additional :: [AdditionalCost]
  }
  deriving (Eq, Ord, Show)
