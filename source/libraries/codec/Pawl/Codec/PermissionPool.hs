module Pawl.Codec.PermissionPool where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PermissionPool as PermissionPool

codec :: Codec.Codec PermissionPool.PermissionPool
codec = Arm.enum
