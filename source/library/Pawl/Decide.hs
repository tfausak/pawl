module Pawl.Decide where

import Pawl.Type.Decider (Decider)
import qualified Pawl.Type.Decider as Decider
import Pawl.Type.GameState (GameState)
import Pawl.Type.PlayerId (PlayerId)

-- Who actually decides for a player (CR 722). M0: always themselves. The
-- GameState argument is where control effects will be consulted later.
deciderFor :: PlayerId -> GameState -> Decider
deciderFor pid _ = Decider.MkDecider pid
