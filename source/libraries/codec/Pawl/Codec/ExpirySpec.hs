module Pawl.Codec.ExpirySpec where

import qualified Pawl.Codec.Expiry as Expiry
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AfterTurn as AfterTurn
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.While as While

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Expiry" $ do
  -- CR 514.2.
  Spec.it s "AtCleanup" $
    Common.assertCodec
      s
      Expiry.codec
      Expiry.AtCleanup
      " {\"type\":\"AtCleanup\"} "
  -- CR 611.2a: no sweep ends it.
  Spec.it s "Never" $
    Common.assertCodec
      s
      Expiry.codec
      Expiry.Never
      " {\"type\":\"Never\"} "
  -- CR 611.2b, baked with the concrete PlayerId CR 109.5's "you" resolves to.
  Spec.it s "While carries its player and condition" $
    Common.assertCodec
      s
      Expiry.codec
      ( Expiry.While
          While.MkWhile
            { While.player = PlayerId.MkPlayerId 0,
              While.condition = Condition.Compares (Compares.MkCompares (Quantity.Literal 0) Comparison.Exactly (Quantity.Literal 0))
            }
      )
      " {\"type\":\"While\",\"value\":{\"player\":0,\"condition\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"Literal\",\"value\":0},\"comparison\":{\"type\":\"Exactly\"},\"threshold\":{\"type\":\"Literal\",\"value\":0}}}}} "
  -- CR 611.2a, as a concrete player.
  Spec.it s "AtTurnOf carries its player" $
    Common.assertCodec
      s
      Expiry.codec
      (Expiry.AtTurnOf (PlayerId.MkPlayerId 1))
      " {\"type\":\"AtTurnOf\",\"value\":1} "
  -- CR 611.2a's other phrasing, which needs the turn as well as the player: the
  -- effect ends at the end of the first turn of theirs numbered above it.
  Spec.it s "AtEndOfTurnOf carries its player and turn" $
    Common.assertCodec
      s
      Expiry.codec
      ( Expiry.AtEndOfTurnOf
          AfterTurn.MkAfterTurn
            { AfterTurn.player = PlayerId.MkPlayerId 1,
              AfterTurn.turn = 3
            }
      )
      " {\"type\":\"AtEndOfTurnOf\",\"value\":{\"player\":1,\"turn\":3}} "
  -- CR 500.5, carrying the PhaseSelector window. Both grains -- a stepless
  -- phase and a step -- because Pawl.Engine.Expiry.dropAtEndOf tells them apart
  -- by EQUALITY, so a codec that collapsed them would let the end of a combat
  -- step end an effect stored against the whole combat phase.
  Spec.it s "AtEndOf carries its window" $ do
    Common.assertCodec
      s
      Expiry.codec
      (Expiry.AtEndOf PhaseSelector.CombatPhase)
      " {\"type\":\"AtEndOf\",\"value\":{\"type\":\"CombatPhase\"}} "
    Common.assertCodec
      s
      Expiry.codec
      (Expiry.AtEndOf (PhaseSelector.Step (Phase.Combat CombatStep.EndOfCombat)))
      " {\"type\":\"AtEndOf\",\"value\":{\"type\":\"Step\",\"value\":{\"type\":\"Combat\",\"value\":{\"type\":\"EndOfCombat\"}}}} "
    Spec.assertBool
      s
      (Codec.encode Expiry.codec (Expiry.AtEndOf PhaseSelector.CombatPhase) /= Codec.encode Expiry.codec (Expiry.AtEndOf (PhaseSelector.Step (Phase.Combat CombatStep.EndOfCombat))))
      "the phase and its own end-of-combat step encode differently"
