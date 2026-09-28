module Pawl.Types.SlotPerPlayer where

import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

-- | CR 601.2c: "for each opponent, ... up to one target ... that player
-- controls" (Riptide Gearhulk) -- one target slot announced once per player the
-- relation names, with `slot` naming that player for the slot's own Filter.
data SlotPerPlayer = MkSlotPerPlayer
  { players :: PlayerRelation.PlayerRelation,
    slot :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
