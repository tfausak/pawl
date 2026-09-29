module Pawl.Codec.PhaseSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.JsonCodec.Codec as Codec
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
  Spec.describe s "flat" $ do
    Spec.it s "a step is its own name" $
      Common.assertCodec s Phase.flat (Phase.Combat CombatStep.DeclareBlockers) " \"DeclareBlockers\" "
    Spec.it s "a phase with no steps is its own name" $
      Common.assertCodec s Phase.flat Phase.PostcombatMain " \"PostcombatMain\" "
    -- Every phase and step, where the literals above are representative: the
    -- names are derived per step type, so two that collided would decode alike.
    Spec.it s "round trips every phase and step, each to its own name" $ do
      let phases =
            fmap Phase.Beginning [minBound .. maxBound]
              <> [Phase.PrecombatMain]
              <> fmap Phase.Combat [minBound .. maxBound]
              <> [Phase.PostcombatMain]
              <> fmap Phase.Ending [minBound .. maxBound]
          encoded = fmap (Common.render . Codec.encode Phase.flat) phases
      Spec.assertEq s (traverse (Codec.decode Phase.flat . Codec.encode Phase.flat) phases) (Right phases)
      Spec.assertEq s (length (Set.fromList encoded)) (length phases)
    Spec.it s "has a schema" $
      Common.assertHasSchema s Phase.flat
