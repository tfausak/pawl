module Pawl.Codec.Phase where

import qualified Pawl.Codec.BeginningStep as BeginningStep
import qualified Pawl.Codec.CombatStep as CombatStep
import qualified Pawl.Codec.EndingStep as EndingStep
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Phase as Phase

codec :: Codec.Codec Phase.Phase
codec =
  Arm.tagged
    encode
    [ Arm.payload "Beginning" BeginningStep.codec Phase.Beginning,
      Arm.nullary "PrecombatMain" Phase.PrecombatMain,
      Arm.payload "Combat" CombatStep.codec Phase.Combat,
      Arm.nullary "PostcombatMain" Phase.PostcombatMain,
      Arm.payload "Ending" EndingStep.codec Phase.Ending
    ]
  where
    encode p = case p of
      Phase.Beginning st -> Common.tagged "Beginning" . Just $ Codec.encode BeginningStep.codec st
      Phase.PrecombatMain -> Common.nullary "PrecombatMain"
      Phase.Combat st -> Common.tagged "Combat" . Just $ Codec.encode CombatStep.codec st
      Phase.PostcombatMain -> Common.nullary "PostcombatMain"
      Phase.Ending st -> Common.tagged "Ending" . Just $ Codec.encode EndingStep.codec st
