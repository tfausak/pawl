module Pawl.Codec.Gift where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Gift as Gift

codec :: Codec.Codec Gift.Gift
codec = Arm.enum
