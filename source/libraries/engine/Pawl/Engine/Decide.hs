module Pawl.Engine.Decide where

import Pawl.Types.Decider (Decider)
import qualified Pawl.Types.Decider as Decider
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.PlayerId (PlayerId)

-- Who actually decides for a player (CR 723). A player controlled during
-- their turn (CR 723.1) has their decisions made by the controller; everyone
-- else decides for themselves. The active-player guard is what makes a single
-- activeControl Maybe correct: only the active player is ever controlled during
-- their controlled turn (CR 723.3).
deciderFor :: PlayerId -> GameState -> Decider
deciderFor pid gs = case GameState.activeControl gs of
  Just decider | pid == GameState.activePlayer gs -> decider
  _ -> Decider.MkDecider pid
