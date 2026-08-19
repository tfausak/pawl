module Pawl.Types.CantBeBlockedBy where

import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | CR 509.1b's PAIRWISE restriction: which attackers are restricted, which
-- blockers are barred from them, and CR 508.1c's "unless" gate.

-- Its own record rather than [[AffectedUnless]] plus a field, because the two
-- creature-naming halves are not interchangeable: 'affected' is the set of
-- attackers restricted and 'blockers' describes what may not block them. That
-- asymmetry is also why the second key is spelled @blockers@ and not a second
-- @affected@.
data CantBeBlockedBy = MkCantBeBlockedBy
  { affected :: Affected.Affected,
    blockers :: Filter.Filter Keyword.Keyword,
    -- | Nothing is the unconditional restriction. Elided rather than written
    -- null.
    unless :: Maybe Condition.Condition
  }
  deriving (Eq, Ord, Show)
