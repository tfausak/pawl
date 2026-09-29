module Pawl.Types.BlockersAre where

import qualified Data.Set as Set
import qualified Pawl.Types.Reference as Reference

-- | CR 509.1h: whether an attacking creature is blocked, and by which creatures
-- still blocking it; Nothing when it is unblocked.
data BlockersAre = MkBlockersAre
  { attacker :: Reference.Reference,
    blockers :: Maybe (Set.Set Reference.Reference)
  }
  deriving (Eq, Ord, Show)
