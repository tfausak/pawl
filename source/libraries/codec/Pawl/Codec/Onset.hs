module Pawl.Codec.Onset where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Onset as Onset

codec :: Codec.Codec Onset.Onset
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Immediately" Onset.Immediately,
      Arm.nullary "FromYourNextTurn" Onset.FromYourNextTurn
    ]
  where
    encode o = Common.nullary $ case o of
      Onset.Immediately -> "Immediately"
      Onset.FromYourNextTurn -> "FromYourNextTurn"
