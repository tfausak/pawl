module Pawl.Codec.ControlClock where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ControlClock as ControlClock

codec :: Codec.Codec ControlClock.ControlClock
codec = Arm.enum
