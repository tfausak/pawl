{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.BeginningStepSpec where

import qualified Pawl.Codec.BeginningStep as BeginningStep
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BeginningStep as BeginningStep

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.BeginningStep" $ do
  Spec.it s "Untap" $
    Common.assertCodec
      s
      BeginningStep.codec
      BeginningStep.Untap
      """ {"type":"Untap"} """
  Spec.it s "Upkeep" $
    Common.assertCodec
      s
      BeginningStep.codec
      BeginningStep.Upkeep
      """ {"type":"Upkeep"} """
  Spec.it s "DrawStep" $
    Common.assertCodec
      s
      BeginningStep.codec
      BeginningStep.DrawStep
      """ {"type":"DrawStep"} """
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s BeginningStep.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s BeginningStep.codec
