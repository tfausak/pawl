module Pawl.Codec.ExtraPhase where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ExtraPhase as ExtraPhase

codec :: Codec.Codec ExtraPhase.ExtraPhase
codec =
  Arm.tagged
    encode
    [ Arm.nullary "ExtraCombat" ExtraPhase.ExtraCombat,
      Arm.nullary "ExtraMain" ExtraPhase.ExtraMain
    ]
  where
    encode e = Common.nullary $ case e of
      ExtraPhase.ExtraCombat -> "ExtraCombat"
      ExtraPhase.ExtraMain -> "ExtraMain"
