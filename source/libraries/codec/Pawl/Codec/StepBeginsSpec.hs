{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.StepBeginsSpec where

import qualified Pawl.Codec.StepBegins as StepBegins
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.StepBegins as StepBegins
import qualified Pawl.Types.TurnScope as TurnScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.StepBegins" $ do
  -- CR 603.2's unscoped "at the beginning of the end step".
  Spec.it s "MkStepBegins, an unscoped end step" $
    Common.assertCodec
      s
      StepBegins.codec
      ( StepBegins.MkStepBegins
          { StepBegins.phase = Phase.Ending EndingStep.EndStep,
            StepBegins.scope = TurnScope.EachTurn
          }
      )
      """ {"phase":{"type":"Ending","value":{"type":"EndStep"}},"scope":{"type":"EachTurn"}} """
  -- CR 603.2a's "your upkeep". The scope is REQUIRED, not defaulted: "at the
  -- beginning of the upkeep" and "at the beginning of YOUR upkeep" are
  -- different cards, so there is no default an absent key could mean.
  Spec.it s "MkStepBegins, the controller's upkeep" $
    Common.assertCodec
      s
      StepBegins.codec
      ( StepBegins.MkStepBegins
          { StepBegins.phase = Phase.Beginning BeginningStep.Upkeep,
            StepBegins.scope = TurnScope.ControllersTurn
          }
      )
      """ {"phase":{"type":"Beginning","value":{"type":"Upkeep"}},"scope":{"type":"ControllersTurn"}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s StepBegins.codec
