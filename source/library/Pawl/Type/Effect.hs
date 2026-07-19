module Pawl.Type.Effect where

import Pawl.Type.Duration (Duration)
import Pawl.Type.Modification (Modification)
import Pawl.Type.Quantity (Quantity)
import Pawl.Type.SlotName (SlotName)

-- The ISA (design.md section 1): first-order, non-recursive, no functions in
-- any field. The ONLY module that may case on a constructor is Pawl.Resolve --
-- the rules core asks classifications, never identities.
data Effect
  = DealDamage SlotName Quantity
  | -- CR 611: create a continuous effect on the slot's target for a duration.
    -- Giant Growth and Serpent's Gift are this one opcode, differing only in the
    -- Modification (layer 7c vs 6). Resolve stores it; it never cases on the
    -- Modification.
    ModifyTarget Duration Modification SlotName
  | -- CR 612: rewrite basic-land-type words in the target spell or permanent. The
    -- SlotName is the target slot; the two basic land types are read from the
    -- caster's binding (Object.chosenSubtypes) and baked into a stored
    -- ChangeSubtypeWord continuous effect. Resolve stores it; Projection applies it.
    ChangeText SlotName
  deriving (Eq, Ord, Show)
