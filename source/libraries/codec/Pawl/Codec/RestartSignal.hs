module Pawl.Codec.RestartSignal where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.RestartSignal as RestartSignal

codec :: Codec.Codec RestartSignal.RestartSignal
codec = Arm.enum
