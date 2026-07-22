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
import Pawl.Type.PlayerId (PlayerId)

-- CR 611.2: the moment a duration BEGINS. `controller` is the effect's
-- controller, which is CR 109.5's "you" for every duration that names a player.
arm :: PlayerId -> Duration -> Maybe Expiry
arm controller duration = case duration of
  Duration.UntilEndOfTurn -> Just Expiry.AtCleanup
  Duration.Indefinite -> Just Expiry.Never
  Duration.UntilYourNextTurn -> Just (Expiry.AtTurnOf controller)

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

-- CR 611.2a: "until your next turn" ends as that player's turn begins. Run at
-- the turn handoff, AFTER activePlayer has been updated, so "a turn began and
-- its active player is p" IS "p's next turn began" -- including when p created
-- the effect on their own turn (the handoff is the only caller, so this never
-- runs during the creating turn) and including extra turns. No per-effect
-- watermark is needed and none is stored.
--
-- Dropping here is observably identical to dropping "as the turn begins": CR
-- 500.12 (no game events occur between turns), CR 502.4 (no priority during
-- untap) and CR 704.3 (no state-based-action check without a player about to
-- receive priority) leave nothing that could observe the difference. The first
-- observation point is the upkeep step (CR 503.1).
dropAtHandoff :: GameState -> GameState
dropAtHandoff gs =
  let survives expiry = case expiry of
        Expiry.AtTurnOf pid -> pid /= GameState.activePlayer gs
        Expiry.AtCleanup -> True
        Expiry.Never -> True
        Expiry.While _ _ -> True
      keepEffect eff = survives (ContinuousEffect.expiry eff)
      keepReplacement active = survives (ActiveReplacement.expiry active)
   in gs
        { GameState.continuousEffects = filter keepEffect (GameState.continuousEffects gs),
          GameState.replacements = filter keepReplacement (GameState.replacements gs)
        }
