module Pawl.Types.RestartSignal where

-- | CR 727.4: whether a restart has replaced the game underneath the control flow
-- that is still running.
--
-- A restart resolves from the stack, so it happens several frames deep -- inside
-- a resolution, inside Engine.priorityLoop, inside Engine.runStep -- all of them
-- mid-way through playing a game that CR 727.1 has already ended, so none may
-- take its next action on the rebuilt state. Setup.restartGame leaves that state
-- where CR 727.4 wants it, and this signal is how the enclosing frames learn to
-- unwind to it rather than through it.
--
-- Raised by Setup.restartGame; lowered by Engine.runStep as the rebuilt turn 1
-- begins. Not a Bool: the two states are "this game is being played" and "this
-- game has been replaced", which is an outcome, not a predicate.
--
-- Scoped per GameState, so CR 727.6 falls out for free: a subgame runs in its own
-- StateT frame with its own signal, and restarting a subgame leaves the main game
-- untouched.
data RestartSignal
  = Playing
  | Restarted
  deriving (Bounded, Enum, Eq, Ord, Show)
