module Pawl.Codec.CoinFace where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CoinFace as CoinFace

codec :: Codec.Codec CoinFace.CoinFace
codec = Arm.enum
