module Pawl.Types.EquipTarget where

-- | What an equip ability attaches its Equipment to. Not a Bool, for
-- Pawl.Types.CostScale's reason.
data EquipTarget
  = -- | CR 702.6a: target creature you control.
    Creature
  | -- | CR 702.6e: target planeswalker you control, as though it were a creature.
    Planeswalker
  deriving (Bounded, Enum, Eq, Ord, Show)
