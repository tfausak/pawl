module Pawl.Codec.Rounding where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Rounding as Rounding

codec :: Codec.Codec Rounding.Rounding
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Up" Rounding.Up,
      Arm.nullary "Down" Rounding.Down
    ]
  where
    encode r = Common.nullary $ case r of
      Rounding.Up -> "Up"
      Rounding.Down -> "Down"
