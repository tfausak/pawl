module Pawl.Codec.DifferentIn where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.DifferentIn as DifferentIn

codec :: Codec.Codec DifferentIn.DifferentIn
codec = Arm.enum
