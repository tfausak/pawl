module Pawl.Codec.DuringPhaseSpec where

import qualified Pawl.Codec.DuringPhase as DuringPhase
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.DuringPhase as DuringPhase
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.TurnScope as TurnScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DuringPhase" $ do
  -- Desert's end-of-combat rider: a window naming a STEP nests inside
  -- PhaseSelector's own Step tag.
  Spec.it s "MkDuringPhase, a step window" $
    Common.assertCodec
      s
      DuringPhase.codec
      ( DuringPhase.MkDuringPhase
          { DuringPhase.window = PhaseSelector.Step (Phase.Combat CombatStep.EndOfCombat),
            DuringPhase.scope = TurnScope.EachTurn
          }
      )
      " {\"window\":{\"type\":\"Step\",\"value\":{\"type\":\"Combat\",\"value\":{\"type\":\"EndOfCombat\"}}},\"scope\":{\"type\":\"EachTurn\"}} "
  -- Jade Statue's whole-phase window, and the other turn scope. BOTH keys are
  -- required: there is no scope-less form, so a payload naming only a window is
  -- a decode failure rather than a silent EachTurn.
  Spec.it s "MkDuringPhase, a whole-phase window on the controller's turns" $
    Common.assertCodec
      s
      DuringPhase.codec
      ( DuringPhase.MkDuringPhase
          { DuringPhase.window = PhaseSelector.CombatPhase,
            DuringPhase.scope = TurnScope.ControllersTurn
          }
      )
      " {\"window\":{\"type\":\"CombatPhase\"},\"scope\":{\"type\":\"ControllersTurn\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DuringPhase.codec
