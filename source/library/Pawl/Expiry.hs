-- CR 611.2: the life cycle of a stored effect's duration. This module is the
-- ONLY module that may case on Pawl.Type.Expiry -- the standing Pawl.Resolve
-- has over Effect, Pawl.Projection over Modification and Pawl.Event over
-- TriggerCondition. It owns the transformation from the PRINTED Duration to the
-- STORED Expiry (`arm`) and every sweep that ends one, over THREE carriers:
-- GameState.continuousEffects, GameState.replacements and
-- GameState.playerEffects share one expiry vocabulary, so they share one sweep.
module Pawl.Expiry where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Pawl.Condition as Condition
import qualified Pawl.Filter as Filter
import qualified Pawl.Projection as Projection
import qualified Pawl.Type.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Type.ActiveReplacement as ActiveReplacement
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import Pawl.Type.Duration (Duration)
import qualified Pawl.Type.Duration as Duration
import Pawl.Type.Expiry (Expiry)
import qualified Pawl.Type.Expiry as Expiry
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)

-- CR 611.2: the moment a duration BEGINS. `controller` is the effect's
-- controller -- CR 109.5's "you" -- and `source` is the object the effect comes
-- from. Nothing means the duration never started, so per CR 611.2b the effect
-- does nothing and is never stored at all.
--
-- CR 611.2b's second sentence -- "if that duration ends before the moment the
-- effect would first be applied and doesn't begin again during that spell or
-- ability's resolution" -- is vacuous here: this runs once, at the point the
-- effect would be stored, and no opcode both ends and restarts a condition
-- mid-resolution.
arm :: PlayerId -> ObjectId -> Duration -> GameState -> Maybe Expiry
arm controller source duration gs = case duration of
  Duration.UntilEndOfTurn -> Just Expiry.AtCleanup
  Duration.Indefinite -> Just Expiry.Never
  Duration.UntilYourNextTurn -> Just (Expiry.AtTurnOf controller)
  Duration.ForAsLongAs cond ->
    if Condition.holds (Projection.fullView gs) (Filter.MkContext (Just controller) (Just source)) gs source cond
      then Just (Expiry.While controller cond)
      else Nothing

-- CR 514.2: during the cleanup step, "all 'until end of turn' and 'this turn'
-- effects end". Delete-and-recompute (design.md 2.5): dropping the stored entry
-- makes the next projection revert -- nothing is explicitly undone. One sweep
-- over three carriers: the per-carrier sweeps this absorbed existed only
-- because the lists lived in different modules, not because they differed.
dropAtCleanup :: GameState -> GameState
dropAtCleanup gs =
  let survives expiry = case expiry of
        Expiry.AtCleanup -> False
        Expiry.Never -> True
        Expiry.While _ _ -> True
        Expiry.AtTurnOf _ -> True
      keepEffect eff = survives (ContinuousEffect.expiry eff)
      keepReplacement active = survives (ActiveReplacement.expiry active)
      keepPlayerEffect active = survives (ActivePlayerEffect.expiry active)
   in gs
        { GameState.continuousEffects = filter keepEffect (GameState.continuousEffects gs),
          GameState.replacements = filter keepReplacement (GameState.replacements gs),
          GameState.playerEffects = filter keepPlayerEffect (GameState.playerEffects gs)
        }

-- CR 611.2b: drop every While whose condition has stopped holding. The effect is
-- DELETED, not masked: 611.2b's duration is one continuous period, so an effect
-- that has ended must stay ended even if the condition becomes true again --
-- CR 611.2b: "It doesn't start and immediately stop again, and it doesn't last
-- forever." Reports whether it changed anything, so Engine.settleForPriority
-- knows to run again.
--
-- CR 704.3 fixes the coarsest moment anything can OBSERVE the condition --
-- "whenever a player would get priority" -- and settleForPriority runs at
-- exactly the points where the board can change, so checking here is
-- indistinguishable from checking continuously.
--
-- `filter` only ever removes elements and preserves the survivors' relative
-- order, so a LENGTH compare is exactly equivalent to the deep structural `/=`
-- this used before -- and cheaper: no need to walk every kept element's Eq
-- instance on a settle that changed nothing (the common case). `State.put` is
-- skipped on that same common case, so a no-op sweep doesn't even rewrite the
-- GameState.
sweepConditional :: Game Bool
sweepConditional = do
  gs <- State.get
  let survives source expiry = case expiry of
        Expiry.While you cond -> Condition.holds (Projection.fullView gs) (Filter.MkContext (Just you) (Just source)) gs source cond
        Expiry.AtCleanup -> True
        Expiry.Never -> True
        Expiry.AtTurnOf _ -> True
      keepEffect eff = survives (ContinuousEffect.source eff) (ContinuousEffect.expiry eff)
      keepReplacement active = survives (ActiveReplacement.source active) (ActiveReplacement.expiry active)
      keepPlayerEffect active = survives (ActivePlayerEffect.source active) (ActivePlayerEffect.expiry active)
      keptEffects = filter keepEffect (GameState.continuousEffects gs)
      keptReplacements = filter keepReplacement (GameState.replacements gs)
      keptPlayerEffects = filter keepPlayerEffect (GameState.playerEffects gs)
      changed =
        length keptEffects /= length (GameState.continuousEffects gs)
          || length keptReplacements /= length (GameState.replacements gs)
          || length keptPlayerEffects /= length (GameState.playerEffects gs)
  Monad.when changed $
    State.put
      gs
        { GameState.continuousEffects = keptEffects,
          GameState.replacements = keptReplacements,
          GameState.playerEffects = keptPlayerEffects
        }
  pure changed

-- CR 611.2a: "until your next turn" ends as that player's turn begins.
--
-- Takes the player EXPLICITLY rather than reading GameState.activePlayer,
-- because CR 800.4m needs this to fire for a seat whose turn does not begin:
-- "When a player leaves the game, any continuous effects with durations that
-- last until that player's next turn ... will last until that turn would have
-- begun. They neither expire immediately nor last indefinitely." Engine's turn
-- handoff walks the seating order and calls this at every seat it passes, so a
-- departed player's durations end at their seat rather than never.
--
-- Dropping at the handoff is observably identical to dropping "as the turn
-- begins": CR 500.12 (no game events occur between turns), CR 502.4 (no priority
-- during untap) and CR 704.3 (no state-based-action check without a player about
-- to receive priority) leave nothing that could observe the difference. The
-- first observation point is the upkeep step (CR 503.1).
--
-- One sweep over the three carriers, as the neighbouring sweeps do. AtCleanup,
-- Never and While are untouched.
dropAtTurnOf :: PlayerId -> GameState -> GameState
dropAtTurnOf pid gs =
  let survives expiry = case expiry of
        Expiry.AtTurnOf p -> p /= pid
        Expiry.AtCleanup -> True
        Expiry.Never -> True
        Expiry.While _ _ -> True
      keepEffect eff = survives (ContinuousEffect.expiry eff)
      keepReplacement active = survives (ActiveReplacement.expiry active)
      keepPlayerEffect active = survives (ActivePlayerEffect.expiry active)
   in gs
        { GameState.continuousEffects = filter keepEffect (GameState.continuousEffects gs),
          GameState.replacements = filter keepReplacement (GameState.replacements gs),
          GameState.playerEffects = filter keepPlayerEffect (GameState.playerEffects gs)
        }
