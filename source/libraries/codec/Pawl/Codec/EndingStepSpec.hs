{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.EndingStepSpec where

import qualified Pawl.Codec.EndingStep as EndingStep
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.EndingStep as EndingStep

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EndingStep" $ do
  Spec.it s "EndStep" $
    Common.assertCodec
      s
      EndingStep.codec
      EndingStep.EndStep
      """ {"type":"EndStep"} """
  Spec.it s "Cleanup" $
    Common.assertCodec
      s
      EndingStep.codec
      EndingStep.Cleanup
      """ {"type":"Cleanup"} """
  Spec.it s "has a schema" $
    Common.assertHasSchema s EndingStep.codec
