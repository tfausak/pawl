module Pawl.Codec.ExtraPhase where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ExtraPhase as ExtraPhase

codec :: Codec.Codec ExtraPhase.ExtraPhase
codec = Arm.enum
