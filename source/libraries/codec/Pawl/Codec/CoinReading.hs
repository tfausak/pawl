module Pawl.Codec.CoinReading where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CoinReading as CoinReading

codec :: Codec.Codec CoinReading.CoinReading
codec = Arm.enum
