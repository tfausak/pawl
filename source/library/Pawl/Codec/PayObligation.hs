module Pawl.Codec.PayObligation where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PayObligation as PayObligation

codec :: Codec.Codec PayObligation.PayObligation
codec = Arm.enum
