module Pawl.Codec.BeginningStep where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.BeginningStep as BeginningStep

codec :: Codec.Codec BeginningStep.BeginningStep
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Untap" BeginningStep.Untap,
      Arm.nullary "Upkeep" BeginningStep.Upkeep,
      Arm.nullary "DrawStep" BeginningStep.DrawStep
    ]
  where
    encode s = Common.nullary $ case s of
      BeginningStep.Untap -> "Untap"
      BeginningStep.Upkeep -> "Upkeep"
      BeginningStep.DrawStep -> "DrawStep"
