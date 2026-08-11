module Pawl.Codec.CombatStep where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CombatStep as CombatStep

codec :: Codec.Codec CombatStep.CombatStep
codec =
  Arm.tagged
    encode
    [ Arm.nullary "BeginningOfCombat" CombatStep.BeginningOfCombat,
      Arm.nullary "DeclareAttackers" CombatStep.DeclareAttackers,
      Arm.nullary "DeclareBlockers" CombatStep.DeclareBlockers,
      Arm.nullary "CombatDamage" CombatStep.CombatDamage,
      Arm.nullary "EndOfCombat" CombatStep.EndOfCombat
    ]
  where
    encode s = Common.nullary $ case s of
      CombatStep.BeginningOfCombat -> "BeginningOfCombat"
      CombatStep.DeclareAttackers -> "DeclareAttackers"
      CombatStep.DeclareBlockers -> "DeclareBlockers"
      CombatStep.CombatDamage -> "CombatDamage"
      CombatStep.EndOfCombat -> "EndOfCombat"
