module Pawl.Types.RoomIndex where

import qualified Numeric.Natural as Natural

-- | CR 309.4: which room of a dungeon card, counted from the topmost. A dungeon's
-- rooms are a printed sequence connected by arrows, and CR 309.4b makes the room
-- NAMES flavor text that does not affect game play -- so the ordinal is the only
-- identifier the rules will support, exactly as Pawl.Types.ModeIndex argues for a
-- mode.
--
-- The ordinal is load-bearing twice over: CR 309.4a starts the venture marker on
-- the TOPMOST room, and CR 309.6 removes the dungeon once the marker sits on the
-- BOTTOMMOST one. Both are positions, not names.
--
-- A newtype, not a bare Natural, so an arrow's destination cannot be confused with
-- a count.
newtype RoomIndex = MkRoomIndex
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)

-- | CR 309.4a: the topmost room, where a venture marker starts.
topmost :: RoomIndex
topmost = MkRoomIndex 0
