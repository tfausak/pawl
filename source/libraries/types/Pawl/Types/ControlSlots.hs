module Pawl.Types.ControlSlots where

import qualified Pawl.Types.SlotName as SlotName

-- | The two slots ControlSides.BetweenSlots exchanges control between (CR
-- 701.12b), one permanent each -- Spawnbroker's "target creature you control and
-- target creature ... an opponent controls", whose second slot's bound reads the
-- first, so the two cannot be one slot of count two.
data ControlSlots = MkControlSlots
  { first :: SlotName.SlotName,
    second :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
