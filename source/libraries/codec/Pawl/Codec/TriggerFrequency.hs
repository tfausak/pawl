module Pawl.Codec.TriggerFrequency where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency

codec :: Codec.Codec TriggerFrequency.TriggerFrequency
codec =
  Arm.tagged
    encode
    [ Arm.nullary "EveryTime" TriggerFrequency.EveryTime,
      Arm.nullary "FirstTimeEachTurn" TriggerFrequency.FirstTimeEachTurn
    ]
  where
    encode f = Common.nullary $ case f of
      TriggerFrequency.EveryTime -> "EveryTime"
      TriggerFrequency.FirstTimeEachTurn -> "FirstTimeEachTurn"
