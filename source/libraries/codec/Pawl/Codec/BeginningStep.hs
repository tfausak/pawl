module Pawl.Codec.BeginningStep where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.BeginningStep as BeginningStep

codec :: Codec.Codec BeginningStep.BeginningStep
codec = Arm.enum
