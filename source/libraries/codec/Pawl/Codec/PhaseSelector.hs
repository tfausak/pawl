module Pawl.Codec.PhaseSelector where

import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.PhaseSelector as PhaseSelector

codec :: Codec.Codec PhaseSelector.PhaseSelector
codec =
  Arm.tagged
    encode
    [ Arm.payload "Step" Phase.codec PhaseSelector.Step,
      Arm.nullary "BeginningPhase" PhaseSelector.BeginningPhase,
      Arm.nullary "CombatPhase" PhaseSelector.CombatPhase,
      Arm.nullary "EndingPhase" PhaseSelector.EndingPhase
    ]
  where
    encode selector = case selector of
      PhaseSelector.Step p -> Common.tagged "Step" . Just $ Codec.encode Phase.codec p
      PhaseSelector.BeginningPhase -> Common.nullary "BeginningPhase"
      PhaseSelector.CombatPhase -> Common.nullary "CombatPhase"
      PhaseSelector.EndingPhase -> Common.nullary "EndingPhase"
