module Pawl.Codec.Daytime where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Daytime as Daytime

codec :: Codec.Codec Daytime.Daytime
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Day" Daytime.Day,
      Arm.nullary "Night" Daytime.Night
    ]
  where
    encode d = Common.nullary $ case d of
      Daytime.Day -> "Day"
      Daytime.Night -> "Night"
