{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.PhasePatternSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.PhasePattern as PhasePattern
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhasePattern as PhasePattern
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerId as PlayerId

-- PhaseSelector's own coverage is in Pawl.Codec.PhaseSelectorSpec; the three
-- cases here are MkPhasePattern's own axis -- a symmetric `whosePhase =
-- Nothing`, a baked `Just`, and a whole-phase selector.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PhasePattern" $ do
  Spec.it s "Eon Hub's symmetric whosePhase = Nothing" $
    Common.assertJsonCodec
      s
      PhasePattern.toJson
      PhasePattern.fromJson
      PhasePattern.MkPhasePattern {PhasePattern.whichPhase = PhaseSelector.Step (Phase.Beginning BeginningStep.Upkeep), PhasePattern.whosePhase = Nothing}
      """ {"whichPhase":{"type":"Step","value":{"type":"Beginning","value":{"type":"Upkeep"}}}} """
  -- The only Just Resolve's SkipNextPhase arm produces, never authored on a
  -- card (#437).
  Spec.it s "Fatigue's player-scoped whosePhase = Just" $
    Common.assertJsonCodec
      s
      PhasePattern.toJson
      PhasePattern.fromJson
      PhasePattern.MkPhasePattern {PhasePattern.whichPhase = PhaseSelector.Step (Phase.Beginning BeginningStep.DrawStep), PhasePattern.whosePhase = Just (PlayerId.MkPlayerId 1)}
      """ {"whichPhase":{"type":"Step","value":{"type":"Beginning","value":{"type":"DrawStep"}}},"whosePhase":1} """
  -- CR 500.1: the whole-phase arm, once Resolve has baked the player its
  -- resolution named.
  Spec.it s "Stonehorn Dignitary's whole-phase selector" $
    Common.assertJsonCodec
      s
      PhasePattern.toJson
      PhasePattern.fromJson
      PhasePattern.MkPhasePattern {PhasePattern.whichPhase = PhaseSelector.CombatPhase, PhasePattern.whosePhase = Just (PlayerId.MkPlayerId 1)}
      """ {"whichPhase":{"type":"CombatPhase"},"whosePhase":1} """
