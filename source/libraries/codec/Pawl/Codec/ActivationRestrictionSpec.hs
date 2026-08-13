{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ActivationRestrictionSpec where

import qualified Pawl.Codec.ActivationRestriction as ActivationRestriction
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActivationRestriction as ActivationRestriction
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.DuringPhase as DuringPhase
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.TurnScope as TurnScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ActivationRestriction" $ do
  Spec.it s "SorcerySpeed" $
    Common.assertCodec
      s
      ActivationRestriction.codec
      ActivationRestriction.SorcerySpeed
      """ {"type":"SorcerySpeed"} """
  -- A stepped window (CR 511.1) beside a phase one (CR 500.1):
  -- Pawl.Types.PhaseSelector spans both, so the arm has to carry both.
  Spec.it s "DuringPhase, Desert's end-of-combat rider" $
    Common.assertCodec
      s
      ActivationRestriction.codec
      ( ActivationRestriction.DuringPhase
          DuringPhase.MkDuringPhase
            { DuringPhase.window = PhaseSelector.Step (Phase.Combat CombatStep.EndOfCombat),
              DuringPhase.scope = TurnScope.EachTurn
            }
      )
      """ {"type":"DuringPhase","value":{"window":{"type":"Step","value":{"type":"Combat","value":{"type":"EndOfCombat"}}},"scope":{"type":"EachTurn"}}} """
  -- The arm's second axis: the SAME window under each scope, so a codec that
  -- dropped the scope would collapse this and the previous case into one.
  Spec.it s "DuringPhase, Llanowar Augur's controller's-turn upkeep" $
    Common.assertCodec
      s
      ActivationRestriction.codec
      ( ActivationRestriction.DuringPhase
          DuringPhase.MkDuringPhase
            { DuringPhase.window = PhaseSelector.Step (Phase.Beginning BeginningStep.Upkeep),
              DuringPhase.scope = TurnScope.ControllersTurn
            }
      )
      """ {"type":"DuringPhase","value":{"window":{"type":"Step","value":{"type":"Beginning","value":{"type":"Upkeep"}}},"scope":{"type":"ControllersTurn"}}} """
  -- The PhaseSelector's stepless arm, with one printed producer in the pool
  -- (#520).
  Spec.it s "DuringPhase, Jade Statue's combat-phase rider" $
    Common.assertCodec
      s
      ActivationRestriction.codec
      ( ActivationRestriction.DuringPhase
          DuringPhase.MkDuringPhase
            { DuringPhase.window = PhaseSelector.CombatPhase,
              DuringPhase.scope = TurnScope.EachTurn
            }
      )
      """ {"type":"DuringPhase","value":{"window":{"type":"CombatPhase"},"scope":{"type":"EachTurn"}}} """
  -- CR 602.5's second clause, and the arm that made this type a list: Kongming's
  -- Contraptions prints it beside the DuringPhase above.
  Spec.it s "AttackedThisStep" $
    Common.assertCodec
      s
      ActivationRestriction.codec
      ActivationRestriction.AttackedThisStep
      """ {"type":"AttackedThisStep"} """
  Spec.it s "has a schema" $ Common.assertHasSchema s ActivationRestriction.codec
