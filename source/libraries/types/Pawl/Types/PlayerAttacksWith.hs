module Pawl.Types.PlayerAttacksWith where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | CR 508.3c: which player's declaration fires the ability, and which quality
-- a creature they declared has to have -- Hermes, Overseer of Elpis' "whenever
-- you attack with one or more Birds".
--
-- A record for Pawl.Types.CreatureBecomesBlockedByAtLeast's reason: the printed
-- form pairs a subject with a narrowing, and the threshold sibling (Aurelia,
-- the Law Above's "with three or more creatures") wants a third field beside
-- these two rather than a re-spelling of every pattern (#2226).
data PlayerAttacksWith = MkPlayerAttacksWith
  { player :: PlayerRelation.PlayerRelation,
    filter :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
