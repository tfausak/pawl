module Pawl.Codec.DamageKind where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.DamageKind as DamageKind

codec :: Codec.Codec DamageKind.DamageKind
codec = Arm.enum
