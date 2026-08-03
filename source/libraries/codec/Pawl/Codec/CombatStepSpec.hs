{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CombatStepSpec where

import qualified Pawl.Codec.CombatStep as CombatStep
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CombatStep as CombatStep

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CombatStep" $ do
  Spec.it s "BeginningOfCombat" $
    Common.assertJsonCodec
      s
      CombatStep.toJson
      CombatStep.fromJson
      CombatStep.BeginningOfCombat
      """ {"type":"BeginningOfCombat"} """
  Spec.it s "DeclareAttackers" $
    Common.assertJsonCodec
      s
      CombatStep.toJson
      CombatStep.fromJson
      CombatStep.DeclareAttackers
      """ {"type":"DeclareAttackers"} """
  Spec.it s "DeclareBlockers" $
    Common.assertJsonCodec
      s
      CombatStep.toJson
      CombatStep.fromJson
      CombatStep.DeclareBlockers
      """ {"type":"DeclareBlockers"} """
  Spec.it s "CombatDamage" $
    Common.assertJsonCodec
      s
      CombatStep.toJson
      CombatStep.fromJson
      CombatStep.CombatDamage
      """ {"type":"CombatDamage"} """
  Spec.it s "EndOfCombat" $
    Common.assertJsonCodec
      s
      CombatStep.toJson
      CombatStep.fromJson
      CombatStep.EndOfCombat
      """ {"type":"EndOfCombat"} """
