module Pawl.Codec.Phase where

import qualified Pawl.Codec.BeginningStep as BeginningStep
import qualified Pawl.Codec.CombatStep as CombatStep
import qualified Pawl.Codec.EndingStep as EndingStep
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Phase as Phase

codec :: Codec.Codec Phase.Phase
codec =
  Arm.tagged
    [ Arm.payload "Beginning" BeginningStep.codec Phase.Beginning (\x -> case x of Phase.Beginning y -> Just y; _ -> Nothing),
      Arm.nullary "PrecombatMain" Phase.PrecombatMain,
      Arm.payload "Combat" CombatStep.codec Phase.Combat (\x -> case x of Phase.Combat y -> Just y; _ -> Nothing),
      Arm.nullary "PostcombatMain" Phase.PostcombatMain,
      Arm.payload "Ending" EndingStep.codec Phase.Ending (\x -> case x of Phase.Ending y -> Just y; _ -> Nothing)
    ]
