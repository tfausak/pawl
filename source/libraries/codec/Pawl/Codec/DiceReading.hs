module Pawl.Codec.DiceReading where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.DiceReading as DiceReading

codec :: Codec.Codec DiceReading.DiceReading
codec = Arm.enum
