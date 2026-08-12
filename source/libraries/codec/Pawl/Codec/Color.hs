module Pawl.Codec.Color where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Color as Color

codec :: Codec.Codec Color.Color
codec =
  Arm.tagged
    encode
    [ Arm.nullary "White" Color.White,
      Arm.nullary "Blue" Color.Blue,
      Arm.nullary "Black" Color.Black,
      Arm.nullary "Red" Color.Red,
      Arm.nullary "Green" Color.Green
    ]
  where
    encode c = Common.nullary $ case c of
      Color.White -> "White"
      Color.Blue -> "Blue"
      Color.Black -> "Black"
      Color.Red -> "Red"
      Color.Green -> "Green"
