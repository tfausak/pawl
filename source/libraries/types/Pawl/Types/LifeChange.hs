module Pawl.Types.LifeChange where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 118.2 \/ 118.3: a player's life total moved, and by how much.

-- Shared by GameEvent's LifeLost and LifeGained as expediency, not because losing
-- and gaining life are the same event: the two are read separately and either
-- would take its own record the moment it grew a field.
data LifeChange = MkLifeChange
  { player :: PlayerId.PlayerId,
    amount :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
