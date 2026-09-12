module Pawl.Types.EntryAttack where

import qualified Pawl.Types.SlotName as SlotName

-- | What a creature put onto the battlefield attacking is attacking (CR 508.4).
data EntryAttack
  = -- | CR 508.4: its controller chooses as it enters.
    Chosen
  | -- | CR 508.4's parenthetical: whatever the object in the slot was attacking
    -- (CR 702.49c).
    SameAs SlotName.SlotName
  | -- | CR 702.116a: Chosen narrowed to one seat -- "that player or a planeswalker
    -- they control", where the slot holds the player. A RESTRICTION and not a
    -- specification: the controller still chooses (CR 508.4), among the subjects
    -- CR 508.4 already allows that belong to that one player.
    UnderPlayer SlotName.SlotName
  deriving (Eq, Ord, Show)
