module Pawl.Codec.PhaseSpec where

import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Phase as Phase

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Phase" $ do
  Spec.it s "Beginning" $
    Common.assertCodec
      s
      Phase.codec
      (Phase.Beginning BeginningStep.Upkeep)
      " {\"type\":\"Beginning\",\"value\":{\"type\":\"Upkeep\"}} "
  Spec.it s "PrecombatMain" $
    Common.assertCodec
      s
      Phase.codec
      Phase.PrecombatMain
      " {\"type\":\"PrecombatMain\"} "
  Spec.it s "Combat" $
    Common.assertCodec
      s
      Phase.codec
      (Phase.Combat CombatStep.DeclareBlockers)
      " {\"type\":\"Combat\",\"value\":{\"type\":\"DeclareBlockers\"}} "
  Spec.it s "PostcombatMain" $
    Common.assertCodec
      s
      Phase.codec
      Phase.PostcombatMain
      " {\"type\":\"PostcombatMain\"} "
  Spec.it s "Ending" $
    Common.assertCodec
      s
      Phase.codec
      (Phase.Ending EndingStep.EndStep)
      " {\"type\":\"Ending\",\"value\":{\"type\":\"EndStep\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Phase.codec
