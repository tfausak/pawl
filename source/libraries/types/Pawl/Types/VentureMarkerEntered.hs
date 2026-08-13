module Pawl.Types.VentureMarkerEntered where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.RoomIndex as RoomIndex

-- | CR 309.4c: a player moved their venture marker into a room -- the player, the
-- dungeon card the marker is on, and which room.

-- The dungeon's id is carried as well as the room because the room index alone
-- names nothing: two players may be in room 1 of two different dungeons, and CR
-- 309.4c makes each room ability the dungeon card's own.
data VentureMarkerEntered = MkVentureMarkerEntered
  { player :: PlayerId.PlayerId,
    dungeon :: ObjectId.ObjectId,
    room :: RoomIndex.RoomIndex
  }
  deriving (Eq, Ord, Show)
