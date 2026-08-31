module Pawl.Codec.RequiredDefender where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.RequiredDefender as RequiredDefender

codec :: Codec.Codec RequiredDefender.RequiredDefender
codec = Arm.enum
