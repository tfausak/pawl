module Pawl.Types.ControlPlayer where

import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's ControlPlayerThisResolution arm (CR
-- 723.2): which slot names the player controlled, and rule 723.7's restriction
-- that rides with the control.
--
-- No duration field: the arm IS rule 723.2's duration, where
-- ControlPlayerNextTurn is rule 723.1's. Two opcodes rather than one carrying a
-- Pawl.Types.ControlDuration, because rule 723.1's control is SCHEDULED for a
-- later turn (CR 723.1b) and rule 723.2's takes hold immediately -- the two
-- write different fields of the game state.
data ControlPlayer = MkControlPlayer
  { slot :: SlotName.SlotName,
    -- | Word of Command's mana clause; see Pawl.Types.PlayerControl.
    manaFromLandsOnly :: Bool
  }
  deriving (Eq, Ord, Show)
