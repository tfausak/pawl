module Pawl.Codec.CastObligation where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CastObligation as CastObligation

codec :: Codec.Codec CastObligation.CastObligation
codec = Arm.enum
