{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.PhaseSelectorSpec where

import qualified Pawl.Codec.PhaseSelector as PhaseSelector
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhaseSelector as PhaseSelector

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PhaseSelector" $ do
  Spec.it s "Step" $
    Common.assertCodec
      s
      PhaseSelector.codec
      (PhaseSelector.Step (Phase.Beginning BeginningStep.DrawStep))
      """ {"type":"Step","value":{"type":"Beginning","value":{"type":"DrawStep"}}} """
  Spec.it s "BeginningPhase" $
    Common.assertCodec
      s
      PhaseSelector.codec
      PhaseSelector.BeginningPhase
      """ {"type":"BeginningPhase"} """
  Spec.it s "CombatPhase" $
    Common.assertCodec
      s
      PhaseSelector.codec
      PhaseSelector.CombatPhase
      """ {"type":"CombatPhase"} """
  Spec.it s "EndingPhase" $
    Common.assertCodec
      s
      PhaseSelector.codec
      PhaseSelector.EndingPhase
      """ {"type":"EndingPhase"} """
  Spec.it s "has a schema" $
    Common.assertHasSchema s PhaseSelector.codec
