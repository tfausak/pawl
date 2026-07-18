module Pawl.Type.Effect where

import Pawl.Type.Quantity (Quantity)
import Pawl.Type.SlotName (SlotName)

-- The ISA (design.md section 1): first-order, non-recursive, no functions in
-- any field. The ONLY module that may case on a constructor is Pawl.Resolve --
-- the rules core asks classifications, never identities.
data Effect
  = DealDamage SlotName Quantity
  deriving (Eq, Ord, Show)
