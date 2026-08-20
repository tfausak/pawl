module Pawl.Types.SetClassLevel where

import qualified Pawl.Types.ClassLevel as ClassLevel
import qualified Pawl.Types.SlotName as SlotName

-- | CR 716.2a's first half: "[Cost]: This Class's level becomes N."
--
-- Pawl.Types.Designate's shape, and for its reason -- CR 716.2b makes a level a
-- designation, so this writes a mark on the slot's object rather than creating a
-- CR 613 modification. It differs only in carrying a number where that carries a
-- Pawl.Types.Designation.
--
-- BECOMES rather than increments: rule 716.2a states an absolute, and the "only
-- if this Class is level N-1" half of the same sentence is an
-- ActivatedAbility.condition on the bar rather than anything this effect checks.
data SetClassLevel = MkSetClassLevel
  { level :: ClassLevel.ClassLevel,
    slot :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
