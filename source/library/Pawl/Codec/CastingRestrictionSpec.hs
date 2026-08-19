module Pawl.Codec.CastingRestrictionSpec where

import qualified Pawl.Codec.CastingRestriction as CastingRestriction
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CastingRestriction as CastingRestriction
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.DuringPhase as DuringPhase
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.TurnScope as TurnScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CastingRestriction" $ do
  -- Rally the Troops' "only during the declare attackers step" (CR 500.1), cast
  -- by the DEFENDING player, so its scope is every turn.
  Spec.it s "DuringPhase, Rally the Troops' declare attackers step on any turn" $
    Common.assertCodec
      s
      CastingRestriction.codec
      ( CastingRestriction.DuringPhase
          DuringPhase.MkDuringPhase
            { DuringPhase.window = PhaseSelector.Step (Phase.Combat CombatStep.DeclareAttackers),
              DuringPhase.scope = TurnScope.EachTurn
            }
      )
      " {\"type\":\"DuringPhase\",\"value\":{\"window\":{\"type\":\"Step\",\"value\":{\"type\":\"Combat\",\"value\":{\"type\":\"DeclareAttackers\"}}},\"scope\":{\"type\":\"EachTurn\"}}} "
  -- Necrologia's "only during your end step" (CR 512.1, CR 109.5): a different
  -- window AND a different scope, so neither key is defaulted past.
  Spec.it s "DuringPhase, Necrologia's own end step" $
    Common.assertCodec
      s
      CastingRestriction.codec
      ( CastingRestriction.DuringPhase
          DuringPhase.MkDuringPhase
            { DuringPhase.window = PhaseSelector.Step (Phase.Ending EndingStep.EndStep),
              DuringPhase.scope = TurnScope.ControllersTurn
            }
      )
      " {\"type\":\"DuringPhase\",\"value\":{\"window\":{\"type\":\"Step\",\"value\":{\"type\":\"Ending\",\"value\":{\"type\":\"EndStep\"}}},\"scope\":{\"type\":\"ControllersTurn\"}}} "
  -- The third scope, and a whole-phase window: no card prints it on a CAST yet,
  -- so this is the codec's own coverage of the axis the arm gained.
  Spec.it s "DuringPhase, a whole-phase window on an opponent's turn" $
    Common.assertCodec
      s
      CastingRestriction.codec
      ( CastingRestriction.DuringPhase
          DuringPhase.MkDuringPhase
            { DuringPhase.window = PhaseSelector.CombatPhase,
              DuringPhase.scope = TurnScope.OpponentsTurn
            }
      )
      " {\"type\":\"DuringPhase\",\"value\":{\"window\":{\"type\":\"CombatPhase\"},\"scope\":{\"type\":\"OpponentsTurn\"}}} "
  Spec.it s "AttackedThisStep" $
    Common.assertCodec
      s
      CastingRestriction.codec
      CastingRestriction.AttackedThisStep
      " {\"type\":\"AttackedThisStep\"} "
  -- Curtain of Light's "only during combat after blockers are declared"
  -- (CR 506.7b).
  Spec.it s "AfterBlockersDeclared" $
    Common.assertCodec
      s
      CastingRestriction.codec
      CastingRestriction.AfterBlockersDeclared
      " {\"type\":\"AfterBlockersDeclared\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s CastingRestriction.codec
