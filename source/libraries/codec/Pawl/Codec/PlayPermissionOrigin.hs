module Pawl.Codec.PlayPermissionOrigin where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PlayPermissionOrigin as PlayPermissionOrigin

codec :: Codec.Codec PlayPermissionOrigin.PlayPermissionOrigin
codec = Arm.enum
