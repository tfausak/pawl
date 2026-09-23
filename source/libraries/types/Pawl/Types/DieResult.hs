module Pawl.Types.DieResult where

import qualified Numeric.Natural as Natural

-- | CR 706.2: one die's result, after every modifier, beside the player the
-- `player` parameter names -- the roller itself on Pawl.Types.GameEvent's
-- DieResultSettled, and a Pawl.Types.PlayerRelation to CR 109.5's "you" on
-- Pawl.Types.TriggerCondition's PlayerRollsResult (Night Shift of the Living
-- Dead's "whenever you roll a 6").
data DieResult player = MkDieResult
  { roller :: player,
    result :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
