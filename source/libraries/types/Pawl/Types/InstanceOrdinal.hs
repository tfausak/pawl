module Pawl.Types.InstanceOrdinal where

import qualified Numeric.Natural as Natural

-- | CR 702.136b: which of several EQUAL replacement abilities on one source this
-- is -- "if a permanent has multiple instances of riot, each works separately".
--
-- An ORDINAL among equals, deliberately not a list index. Pawl.Types.CandidateId
-- says why at length: index identity breaks CR 616.2, because a Clone acquires
-- and loses whole abilities between iterations and would renumber a surviving
-- one. This counts only rows equal in (source, effect), so it is immune to that
-- shuffle. Pawl.Engine.Replacement.collect assigns it.
--
-- A newtype, not a bare Natural, so the reference is typed and cannot be
-- confused with the counts it sits beside.
newtype InstanceOrdinal = MkInstanceOrdinal
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
