module Pawl.Types.EntryAttack where

import qualified Pawl.Types.SlotName as SlotName

-- | What a creature put onto the battlefield attacking is attacking (CR 508.4).
data EntryAttack
  = -- | CR 508.4: its controller chooses as it enters.
    Chosen
  | -- | CR 508.4's parenthetical: whatever the object in the slot was attacking
    -- (CR 702.49c).
    SameAs SlotName.SlotName
  deriving (Eq, Ord, Show)
