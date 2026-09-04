module Pawl.Types.PlayerAttacksWith where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | CR 508.3c: which player's declaration fires the ability, which quality a
-- creature they declared has to have, and how many such creatures the
-- declaration has to name -- Hermes, Overseer of Elpis' "whenever you attack
-- with one or more Birds" at a floor of one, Military Intelligence's "whenever
-- you attack with two or more creatures" at two.
--
-- A record for Pawl.Types.CreatureBecomesBlockedByAtLeast's reason: the printed
-- form pairs a subject with a narrowing and a count, and the threshold is a
-- field beside the other two rather than a re-spelling of every pattern.
data PlayerAttacksWith = MkPlayerAttacksWith
  { player :: PlayerRelation.PlayerRelation,
    filter :: Filter.Filter Keyword.Keyword,
    attackers :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
