module Pawl.Types.CreatureBecomesBlockedByAtLeast where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | CR 509.3e over CR 508.1b's attack targets: whom the attacking creature has
-- to be attacking, and how many creatures have to block it -- Seifer, Balamb
-- Rival's "a creature attacking one of your opponents becomes blocked by two or
-- more creatures".
data CreatureBecomesBlockedByAtLeast = MkCreatureBecomesBlockedByAtLeast
  { attacked :: PlayerRelation.PlayerRelation,
    blockers :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
