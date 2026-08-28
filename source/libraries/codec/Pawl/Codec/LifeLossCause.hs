module Pawl.Codec.LifeLossCause where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.LifeLossCause as LifeLossCause

codec :: Codec.Codec LifeLossCause.LifeLossCause
codec = Arm.enum
