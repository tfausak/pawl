module Pawl.Codec.Color where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Color as Color

codec :: Codec.Codec Color.Color
codec = Arm.enum
