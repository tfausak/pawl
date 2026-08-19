module Pawl.Types.Game where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Pawl.Types.Asked as Asked
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Program as Program

-- The engine's monad: game state over a program that suspends on questions.
--
-- The instruction is Asked rather than Prompt, so a suspension carries the game
-- that raised it (#153). StateT folds the Program from the OUTSIDE, which means
-- the interpreter never sees the state at a suspension unless the instruction
-- brings it -- and Pawl.Engine.Game.ask is the one place that puts it there.
type Game = State.StateT GameState.GameState (Program.Program Asked.Asked)
