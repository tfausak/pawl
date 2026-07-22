-- CR 611.2: the life cycle of a stored effect's duration. This module is the
-- ONLY module that may case on Pawl.Type.Expiry -- the standing Pawl.Resolve
-- has over Effect, Pawl.Projection over Modification and Pawl.Event over
-- TriggerCondition. It owns the transformation from the PRINTED Duration to the
-- STORED Expiry (`arm`) and every sweep that ends one, over BOTH carriers:
-- GameState.continuousEffects and GameState.replacements share one expiry
-- vocabulary, so they share one sweep.
module Pawl.Expiry where

import qualified Pawl.Type.ActiveReplacement as ActiveReplacement
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import Pawl.Type.Duration (Duration)
import qualified Pawl.Type.Duration as Duration
import Pawl.Type.Expiry (Expiry)
import qualified Pawl.Type.Expiry as Expiry
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState

-- CR 611.2: the moment a duration BEGINS. Total for the two fixed points; the
-- shapes that can fail to start (CR 611.2b) arrive with their own arms.
arm :: Duration -> Maybe Expiry
arm duration = case duration of
  Duration.UntilEndOfTurn -> Just Expiry.AtCleanup
  Duration.Indefinite -> Just Expiry.Never

-- CR 514.2: during the cleanup step, "all 'until end of turn' and 'this turn'
-- effects end". Delete-and-recompute (design.md 2.5): dropping the stored entry
-- makes the next projection revert -- nothing is explicitly undone. One sweep
-- over both carriers, replacing Projection.dropEndOfTurnEffects and
-- Event.dropEndOfTurnReplacements, which existed only because the two lists
-- lived in two modules.
dropAtCleanup :: GameState -> GameState
dropAtCleanup gs =
  let survives expiry = case expiry of
        Expiry.AtCleanup -> False
        Expiry.Never -> True
        Expiry.While _ _ -> True
        Expiry.AtTurnOf _ -> True
      keepEffect eff = survives (ContinuousEffect.expiry eff)
      keepReplacement active = survives (ActiveReplacement.expiry active)
   in gs
        { GameState.continuousEffects = filter keepEffect (GameState.continuousEffects gs),
          GameState.replacements = filter keepReplacement (GameState.replacements gs)
        }
