module Pawl.Codec.EndingStep where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.EndingStep as EndingStep

codec :: Codec.Codec EndingStep.EndingStep
codec = Arm.enum
