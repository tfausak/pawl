module Pawl.Codec.PhaseSelector where

import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PhaseSelector as PhaseSelector

codec :: Codec.Codec PhaseSelector.PhaseSelector
codec =
  Arm.tagged
    [ Arm.payload "Step" Phase.codec PhaseSelector.Step (\x -> case x of PhaseSelector.Step y -> Just y; _ -> Nothing),
      Arm.nullary "BeginningPhase" PhaseSelector.BeginningPhase,
      Arm.nullary "CombatPhase" PhaseSelector.CombatPhase,
      Arm.nullary "EndingPhase" PhaseSelector.EndingPhase
    ]
