module Pawl.Types.RoomHalf where

-- | CR 709.5c: which half of a permanent with a shared type line an unlocked
-- designation names -- "left half unlocked" or "right half unlocked". A
-- POSITION rather than a half's name, since the designation stays the
-- permanent's own when a copy effect changes which card's halves it has.
data RoomHalf
  = LeftHalf
  | RightHalf
  deriving (Bounded, Enum, Eq, Ord, Show)
