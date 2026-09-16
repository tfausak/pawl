module Pawl.Codec.CoinFlipRewrite where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CoinFlipRewrite as CoinFlipRewrite

codec :: Codec.Codec CoinFlipRewrite.CoinFlipRewrite
codec = Arm.enum
