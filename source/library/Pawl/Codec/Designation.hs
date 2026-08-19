module Pawl.Codec.Designation where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Designation as Designation

codec :: Codec.Codec Designation.Designation
codec = Arm.enum
