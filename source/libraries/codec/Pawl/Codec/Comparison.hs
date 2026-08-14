module Pawl.Codec.Comparison where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Comparison as Comparison

codec :: Codec.Codec Comparison.Comparison
codec = Arm.enum
