module Pawl.Codec.DrawRewrite where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.DrawRewrite as DrawRewrite

codec :: Codec.Codec DrawRewrite.DrawRewrite
codec =
  Arm.tagged
    [ Arm.payload "GainLife" Common.natural DrawRewrite.GainLife (\(DrawRewrite.GainLife y) -> Just y)
    ]
