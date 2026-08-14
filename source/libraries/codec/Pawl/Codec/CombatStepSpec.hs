{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CombatStepSpec where

import qualified Pawl.Codec.CombatStep as CombatStep
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CombatStep as CombatStep

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CombatStep" $ do
  Spec.it s "BeginningOfCombat" $
    Common.assertCodec
      s
      CombatStep.codec
      CombatStep.BeginningOfCombat
      """ {"type":"BeginningOfCombat"} """
  Spec.it s "DeclareAttackers" $
    Common.assertCodec
      s
      CombatStep.codec
      CombatStep.DeclareAttackers
      """ {"type":"DeclareAttackers"} """
  Spec.it s "DeclareBlockers" $
    Common.assertCodec
      s
      CombatStep.codec
      CombatStep.DeclareBlockers
      """ {"type":"DeclareBlockers"} """
  Spec.it s "CombatDamage" $
    Common.assertCodec
      s
      CombatStep.codec
      CombatStep.CombatDamage
      """ {"type":"CombatDamage"} """
  Spec.it s "EndOfCombat" $
    Common.assertCodec
      s
      CombatStep.codec
      CombatStep.EndOfCombat
      """ {"type":"EndOfCombat"} """
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s CombatStep.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s CombatStep.codec
