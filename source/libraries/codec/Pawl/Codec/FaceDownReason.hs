module Pawl.Codec.FaceDownReason where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.FaceDownReason as FaceDownReason

codec :: Codec.Codec FaceDownReason.FaceDownReason
codec = Arm.enum
