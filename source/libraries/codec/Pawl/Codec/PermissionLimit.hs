module Pawl.Codec.PermissionLimit where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PermissionLimit as PermissionLimit

codec :: Codec.Codec PermissionLimit.PermissionLimit
codec = Arm.enum
