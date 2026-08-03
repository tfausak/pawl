module Pawl.Codec.BeginningStepSpec where

import qualified Pawl.Codec.BeginningStep as BeginningStep
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BeginningStep as BeginningStep

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.BeginningStep" $ do
  Spec.it s "Untap" $
    Common.assertJsonCodec
      s
      BeginningStep.toJson
      BeginningStep.fromJson
      BeginningStep.Untap
      "{\"type\":\"Untap\"}"
  Spec.it s "Upkeep" $
    Common.assertJsonCodec
      s
      BeginningStep.toJson
      BeginningStep.fromJson
      BeginningStep.Upkeep
      "{\"type\":\"Upkeep\"}"
  Spec.it s "DrawStep" $
    Common.assertJsonCodec
      s
      BeginningStep.toJson
      BeginningStep.fromJson
      BeginningStep.DrawStep
      "{\"type\":\"DrawStep\"}"
