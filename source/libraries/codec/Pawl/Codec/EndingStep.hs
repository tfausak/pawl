module Pawl.Codec.EndingStep where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.EndingStep as EndingStep

codec :: Codec.Codec EndingStep.EndingStep
codec =
  Arm.tagged
    encode
    [ Arm.nullary "EndStep" EndingStep.EndStep,
      Arm.nullary "Cleanup" EndingStep.Cleanup
    ]
  where
    encode s = Common.nullary $ case s of
      EndingStep.EndStep -> "EndStep"
      EndingStep.Cleanup -> "Cleanup"
