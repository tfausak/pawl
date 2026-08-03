module Pawl.Types.Game where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Program as Program
import qualified Pawl.Types.Prompt as Prompt

type Game = State.StateT GameState.GameState (Program.Program Prompt.Prompt)
