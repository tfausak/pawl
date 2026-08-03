module Pawl.Codec.ExpirySpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Expiry as Expiry
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Expiry" $ do
  -- CR 514.2: "all 'until end of turn' and 'this turn' effects end" during the
  -- cleanup step.
  Spec.it s "AtCleanup" $
    Common.assertJsonCodec s Expiry.toJson Expiry.fromJson Expiry.AtCleanup "{\"type\":\"AtCleanup\"}"
  -- CR 611.2a: "lasts until the end of the game". No sweep ends it.
  Spec.it s "Never" $
    Common.assertJsonCodec s Expiry.toJson Expiry.fromJson Expiry.Never "{\"type\":\"Never\"}"
  -- CR 611.2b: "for as long as ...", baked with the concrete PlayerId CR 109.5's
  -- "you" resolves to.
  Spec.it s "While carries its player and condition" $
    Common.assertJsonCodec
      s
      Expiry.toJson
      Expiry.fromJson
      (Expiry.While (PlayerId.MkPlayerId 0) (Condition.MkCondition (Quantity.Literal 0) Comparison.Exactly (Quantity.Literal 0)))
      "{\"type\":\"While\",\"value\":[0,{\"measured\":{\"type\":\"Literal\",\"value\":0},\"comparison\":{\"type\":\"Exactly\"},\"threshold\":{\"type\":\"Literal\",\"value\":0}}]}"
  -- CR 611.2a: "until your next turn", as a concrete player.
  Spec.it s "AtTurnOf carries its player" $
    Common.assertJsonCodec s Expiry.toJson Expiry.fromJson (Expiry.AtTurnOf (PlayerId.MkPlayerId 1)) "{\"type\":\"AtTurnOf\",\"value\":1}"
  -- CR 500.5: "as a step or phase ends", carrying the PhaseSelector window --
  -- Duration.UntilEndOfCombat's stored form. Both of CR 500.5's grains, a
  -- stepless phase and a step of one, because Pawl.Engine.Expiry.dropAtEndOf
  -- tells them apart by EQUALITY -- a codec that collapsed them would let the
  -- end of combat step end an effect stored against the whole combat phase.
  Spec.it s "AtEndOf carries its window" $ do
    Common.assertJsonCodec s Expiry.toJson Expiry.fromJson (Expiry.AtEndOf PhaseSelector.CombatPhase) "{\"type\":\"AtEndOf\",\"value\":{\"type\":\"CombatPhase\"}}"
    Common.assertJsonCodec
      s
      Expiry.toJson
      Expiry.fromJson
      (Expiry.AtEndOf (PhaseSelector.Step (Phase.Combat CombatStep.EndOfCombat)))
      "{\"type\":\"AtEndOf\",\"value\":{\"type\":\"Step\",\"value\":{\"type\":\"Combat\",\"value\":{\"type\":\"EndOfCombat\"}}}}"
    Spec.assertBool
      s
      (Expiry.toJson (Expiry.AtEndOf PhaseSelector.CombatPhase) /= Expiry.toJson (Expiry.AtEndOf (PhaseSelector.Step (Phase.Combat CombatStep.EndOfCombat))))
      "the phase and its own end-of-combat step encode differently"
