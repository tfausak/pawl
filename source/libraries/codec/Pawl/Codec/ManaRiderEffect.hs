module Pawl.Codec.ManaRiderEffect where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ManaRiderEffect as ManaRiderEffect

codec :: Codec.Codec ManaRiderEffect.ManaRiderEffect
codec = Arm.enum
