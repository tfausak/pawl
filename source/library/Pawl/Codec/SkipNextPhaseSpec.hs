module Pawl.Codec.SkipNextPhaseSpec where

import qualified Pawl.Codec.SkipNextPhase as SkipNextPhase
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SkipNextPhase as SkipNextPhase

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SkipNextPhase" $ do
  -- CR 500.8: the players named each skip their next matching phase.
  Spec.it s "MkSkipNextPhase, both keys" $
    Common.assertCodec
      s
      SkipNextPhase.codec
      ( SkipNextPhase.MkSkipNextPhase
          { SkipNextPhase.player = PlayerRef.Relative PlayerRelation.You,
            SkipNextPhase.selector = PhaseSelector.CombatPhase
          }
      )
      " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"selector\":{\"type\":\"CombatPhase\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s SkipNextPhase.codec
