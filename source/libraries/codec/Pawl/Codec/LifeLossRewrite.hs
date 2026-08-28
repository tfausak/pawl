module Pawl.Codec.LifeLossRewrite where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.LifeLossRewrite as LifeLossRewrite

codec :: Codec.Codec LifeLossRewrite.LifeLossRewrite
codec =
  Arm.tagged
    [ Arm.payload "LeaveAtLeast" Common.natural LifeLossRewrite.LeaveAtLeast (\(LifeLossRewrite.LeaveAtLeast y) -> Just y)
    ]
