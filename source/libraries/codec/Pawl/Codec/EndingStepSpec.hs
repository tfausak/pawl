module Pawl.Codec.EndingStepSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.EndingStep as EndingStep
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.EndingStep as EndingStep

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EndingStep" $ do
  Spec.it s "EndStep" $
    Common.assertJsonCodec s EndingStep.toJson EndingStep.fromJson EndingStep.EndStep "{\"type\":\"EndStep\"}"
  Spec.it s "Cleanup" $
    Common.assertJsonCodec s EndingStep.toJson EndingStep.fromJson EndingStep.Cleanup "{\"type\":\"Cleanup\"}"
