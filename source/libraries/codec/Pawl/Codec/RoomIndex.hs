module Pawl.Codec.RoomIndex where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.RoomIndex as RoomIndex

codec :: Codec.Codec RoomIndex.RoomIndex
codec = Common.wrapper Common.natural RoomIndex.MkRoomIndex RoomIndex.unwrap
