module Pawl.Codec.ActivationTimingSpec where

import qualified Pawl.Codec.ActivationTiming as ActivationTiming
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActivationTiming as ActivationTiming
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.TurnScope as TurnScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ActivationTiming" $ do
  Spec.it s "AnyTime" $
    Common.assertJsonCodec s ActivationTiming.toJson ActivationTiming.fromJson ActivationTiming.AnyTime "{\"type\":\"AnyTime\"}"
  Spec.it s "SorcerySpeed" $
    Common.assertJsonCodec s ActivationTiming.toJson ActivationTiming.fromJson ActivationTiming.SorcerySpeed "{\"type\":\"SorcerySpeed\"}"
  -- Desert's own rider (CR 511.1), a stepless phase alongside it (CR 500.1):
  -- Pawl.Types.Phase spans both, so the arm has to carry both.
  Spec.it s "DuringPhase, Desert's end-of-combat rider" $
    Common.assertJsonCodec
      s
      ActivationTiming.toJson
      ActivationTiming.fromJson
      (ActivationTiming.DuringPhase (Phase.Combat CombatStep.EndOfCombat) TurnScope.EachTurn)
      "{\"type\":\"DuringPhase\",\"value\":[{\"type\":\"Combat\",\"value\":{\"type\":\"EndOfCombat\"}},{\"type\":\"EachTurn\"}]}"
  -- Llanowar Augur's "Activate only during your upkeep", the arm's second axis:
  -- the SAME phase under each scope, so a codec that dropped the scope would
  -- collapse this and the previous case's phase into one.
  Spec.it s "DuringPhase, Llanowar Augur's controller's-turn upkeep" $
    Common.assertJsonCodec
      s
      ActivationTiming.toJson
      ActivationTiming.fromJson
      (ActivationTiming.DuringPhase (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn)
      "{\"type\":\"DuringPhase\",\"value\":[{\"type\":\"Beginning\",\"value\":{\"type\":\"Upkeep\"}},{\"type\":\"ControllersTurn\"}]}"
