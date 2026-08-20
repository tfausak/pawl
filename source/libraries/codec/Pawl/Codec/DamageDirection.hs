module Pawl.Codec.DamageDirection where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.DamageDirection as DamageDirection

codec :: Codec.Codec DamageDirection.DamageDirection
codec = Arm.enum
