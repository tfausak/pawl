module Pawl.Types.EndTurnSignal where

-- | CR 724.1: whether an effect has ended the turn underneath the control flow
-- that is still running the step it ended.
--
-- Ending the turn resolves from the stack, so it happens several frames deep --
-- inside a resolution, inside Engine.priorityLoop, inside Engine.runStep -- and
-- CR 724.1d has already ended the step those frames are running. CR 724.1f is
-- what makes the signal load-bearing rather than a schedule rewrite: no player
-- gets priority during this process, and the abilities that triggered during it
-- go onto the stack in the CLEANUP step, so priorityLoop may neither settle nor
-- grant another round once this is raised.
--
-- Raised by Resolve's Effect.EndTurn arm; lowered by Engine.runStep as the
-- cleanup step CR 724.1d jumped to begins. Not a Bool, for
-- Pawl.Types.RestartSignal's reason: the two states are an outcome -- "this step
-- is running" and "an effect ended the turn" -- rather than a predicate.
--
-- Scoped per GameState like RestartSignal, so a CR 729.1 subgame runs in its own
-- StateT frame with its own signal.
data EndTurnSignal
  = Running
  | Ended
  deriving (Bounded, Enum, Eq, Ord, Show)
