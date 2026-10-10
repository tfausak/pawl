module Pawl.Types.PlayerActed where

import qualified Pawl.Types.PlayerAction as PlayerAction

-- | CR 603.2: a player performed a Pawl.Types.PlayerAction, beside the player
-- the `player` parameter names -- the actor itself on Pawl.Types.GameEvent's
-- PlayerActed, and a Pawl.Types.PlayerRelation to CR 109.5's "you" on
-- Pawl.Types.TriggerCondition's PlayerActs.
data PlayerActed player = MkPlayerActed
  { action :: PlayerAction.PlayerAction,
    player :: player
  }
  deriving (Eq, Ord, Show)
