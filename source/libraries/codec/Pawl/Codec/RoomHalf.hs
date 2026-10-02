module Pawl.Codec.RoomHalf where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.RoomHalf as RoomHalf

codec :: Codec.Codec RoomHalf.RoomHalf
codec = Arm.enum
