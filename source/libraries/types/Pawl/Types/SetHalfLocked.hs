module Pawl.Types.SetHalfLocked where

import qualified Pawl.Types.SlotName as SlotName

-- | CR 709.5f and CR 709.5g together: lock or unlock one half of the slot's
-- permanent. The half is not named here -- both rules make it a choice taken
-- while the effect is applied (CR 608.2d), asked through
-- Pawl.Types.Prompt.ChooseHalf.
data SetHalfLocked = MkSetHalfLocked
  { -- | The state the chosen half ends in: True is CR 709.5g's lock, False CR
    -- 709.5f's unlock. The RESULT rather than the verb, so the two rules read as
    -- one write with two settings rather than as two writes.
    locked :: Bool,
    slot :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
