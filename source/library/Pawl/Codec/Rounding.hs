module Pawl.Codec.Rounding where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Rounding as Rounding

codec :: Codec.Codec Rounding.Rounding
codec = Arm.enum
