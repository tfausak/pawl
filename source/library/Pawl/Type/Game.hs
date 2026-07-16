module Pawl.Type.Game where

import Control.Monad.Trans.State.Strict (StateT)
import Pawl.Type.GameState (GameState)
import Pawl.Type.Program (Program)
import Pawl.Type.Prompt (Prompt)

type Game a = StateT GameState (Program Prompt) a
