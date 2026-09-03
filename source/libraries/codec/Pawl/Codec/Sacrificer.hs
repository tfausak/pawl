module Pawl.Codec.Sacrificer where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Sacrificer as Sacrificer

codec :: Codec.Codec Sacrificer.Sacrificer
codec = Arm.enum
