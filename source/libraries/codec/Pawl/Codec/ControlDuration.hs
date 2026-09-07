module Pawl.Codec.ControlDuration where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ControlDuration as ControlDuration

codec :: Codec.Codec ControlDuration.ControlDuration
codec = Arm.enum
