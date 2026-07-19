module Pawl.Decide where

import Pawl.Type.Decider (Decider)
import qualified Pawl.Type.Decider as Decider
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import Pawl.Type.PlayerId (PlayerId)

-- Who actually decides for a player (CR 722/723). A player controlled during
-- their turn (CR 723.1) has their decisions made by the controller; everyone
-- else decides for themselves. The active-player guard is what makes a single
-- activeControl Maybe correct: only the active player is ever controlled during
-- their controlled turn (CR 723.3).
deciderFor :: PlayerId -> GameState -> Decider
deciderFor pid gs = case GameState.activeControl gs of
  Just decider | pid == GameState.activePlayer gs -> decider
  _ -> Decider.MkDecider pid
