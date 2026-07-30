module Pawl.Types.Game where

import Control.Monad.Trans.State.Strict (StateT)
import Pawl.Types.GameState (GameState)
import Pawl.Types.Program (Program)
import Pawl.Types.Prompt (Prompt)

type Game a = StateT GameState (Program Prompt) a
