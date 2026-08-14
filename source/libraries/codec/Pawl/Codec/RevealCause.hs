module Pawl.Codec.RevealCause where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.RevealCause as RevealCause

codec :: Codec.Codec RevealCause.RevealCause
codec = Arm.enum
