module Pawl.Codec.SubtypeFamily where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily

codec :: Codec.Codec SubtypeFamily.SubtypeFamily
codec = Arm.enum
