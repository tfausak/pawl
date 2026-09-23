module Pawl.Codec.PermissionVerb where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PermissionVerb as PermissionVerb

codec :: Codec.Codec PermissionVerb.PermissionVerb
codec = Arm.enum
