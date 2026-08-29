module Pawl.Codec.MoveDuration where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.MoveDuration as MoveDuration

codec :: Codec.Codec MoveDuration.MoveDuration
codec = Arm.enum
