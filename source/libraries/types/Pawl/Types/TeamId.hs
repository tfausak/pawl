module Pawl.Types.TeamId where

import qualified Numeric.Natural as Natural

-- | CR 808.1: which team a player is on, in a game played between teams.
--
-- An opaque index and not a name: rule 102.3 asks only whether two players are
-- on the same one, so nothing in the CR distinguishes two teams beyond their
-- membership.
newtype TeamId = MkTeamId
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
