{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ActivationRestrictionSpec where

import qualified Pawl.Codec.ActivationRestriction as ActivationRestriction
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActivationRestriction as ActivationRestriction
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.TurnScope as TurnScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ActivationRestriction" $ do
  Spec.it s "SorcerySpeed" $
    Common.assertJsonCodec
      s
      ActivationRestriction.toJson
      ActivationRestriction.fromJson
      ActivationRestriction.SorcerySpeed
      """ {"type":"SorcerySpeed"} """
  -- A stepped window (CR 511.1) beside a phase one (CR 500.1):
  -- Pawl.Types.PhaseSelector spans both, so the arm has to carry both.
  Spec.it s "DuringPhase, Desert's end-of-combat rider" $
    Common.assertJsonCodec
      s
      ActivationRestriction.toJson
      ActivationRestriction.fromJson
      (ActivationRestriction.DuringPhase (PhaseSelector.Step (Phase.Combat CombatStep.EndOfCombat)) TurnScope.EachTurn)
      """ {"type":"DuringPhase","value":[{"type":"Step","value":{"type":"Combat","value":{"type":"EndOfCombat"}}},{"type":"EachTurn"}]} """
  -- The arm's second axis: the SAME window under each scope, so a codec that
  -- dropped the scope would collapse this and the previous case into one.
  Spec.it s "DuringPhase, Llanowar Augur's controller's-turn upkeep" $
    Common.assertJsonCodec
      s
      ActivationRestriction.toJson
      ActivationRestriction.fromJson
      (ActivationRestriction.DuringPhase (PhaseSelector.Step (Phase.Beginning BeginningStep.Upkeep)) TurnScope.ControllersTurn)
      """ {"type":"DuringPhase","value":[{"type":"Step","value":{"type":"Beginning","value":{"type":"Upkeep"}}},{"type":"ControllersTurn"}]} """
  -- The PhaseSelector's stepless arm, with one printed producer in the pool
  -- (#520).
  Spec.it s "DuringPhase, Jade Statue's combat-phase rider" $
    Common.assertJsonCodec
      s
      ActivationRestriction.toJson
      ActivationRestriction.fromJson
      (ActivationRestriction.DuringPhase PhaseSelector.CombatPhase TurnScope.EachTurn)
      """ {"type":"DuringPhase","value":[{"type":"CombatPhase"},{"type":"EachTurn"}]} """
  -- CR 602.5's second clause, and the arm that made this type a list: Kongming's
  -- Contraptions prints it beside the DuringPhase above.
  Spec.it s "AttackedThisStep" $
    Common.assertJsonCodec
      s
      ActivationRestriction.toJson
      ActivationRestriction.fromJson
      ActivationRestriction.AttackedThisStep
      """ {"type":"AttackedThisStep"} """
