module Pawl.Codec.StepBeganSpec where

import qualified Pawl.Codec.StepBegan as StepBegan
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.StepBegan as StepBegan

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.StepBegan" $ do
  -- CR 500.1.
  Spec.it s "MkStepBegan, both keys" $
    Common.assertCodec
      s
      StepBegan.codec
      ( StepBegan.MkStepBegan
          { StepBegan.phase = Phase.Beginning BeginningStep.Upkeep,
            StepBegan.player = PlayerId.MkPlayerId 1
          }
      )
      " {\"phase\":{\"type\":\"Beginning\",\"value\":{\"type\":\"Upkeep\"}},\"player\":1} "
  Spec.it s "has a schema" $ Common.assertHasSchema s StepBegan.codec
