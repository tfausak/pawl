module Pawl.Codec.DiscardCause where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.DiscardCause as DiscardCause

codec :: Codec.Codec DiscardCause.DiscardCause
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Ordinary" DiscardCause.Ordinary,
      Arm.nullary "ToPayCyclingCost" DiscardCause.ToPayCyclingCost
    ]
  where
    encode c = Common.nullary $ case c of
      DiscardCause.Ordinary -> "Ordinary"
      DiscardCause.ToPayCyclingCost -> "ToPayCyclingCost"
