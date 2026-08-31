module Pawl.Types.SetHalfLocked where

import qualified Pawl.Types.SlotName as SlotName

-- | CR 709.5f and CR 709.5g together: lock or unlock halves of the slot's
-- permanent. Which half is not named here -- both rules make it a choice taken
-- while the effect is applied (CR 608.2d), asked through
-- Pawl.Types.Prompt.ChooseHalf.
data SetHalfLocked = MkSetHalfLocked
  { -- | Whether the instruction names EVERY half the setting admits rather than
    -- one: "unlock each locked door" against rule 709.5f's "unlock". True leaves
    -- nothing to choose, so no prompt is raised, and is the only route to CR
    -- 709.5i's second branch, where a permanent with neither designation gains
    -- both.
    every :: Bool,
    -- | The state the chosen half ends in: True is CR 709.5g's lock, False CR
    -- 709.5f's unlock. The RESULT rather than the verb, so the two rules read as
    -- one write with two settings rather than as two writes.
    locked :: Bool,
    slot :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
