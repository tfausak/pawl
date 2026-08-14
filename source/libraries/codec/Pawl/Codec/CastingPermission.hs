module Pawl.Codec.CastingPermission where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CastingPermission as CastingPermission

codec :: Codec.Codec CastingPermission.CastingPermission
codec = Arm.enum
