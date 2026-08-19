module Pawl.Types.CounterChange where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 122: counters of one kind arrived on or left an object, with the count
-- BEFORE and the count AFTER.

-- The pair rather than the amount, because CR 714.2b's chapter ability asks
-- whether the number "was less than N and became at least N" -- a THRESHOLD
-- CROSSING neither the amount alone nor the resulting total alone can answer. A
-- Saga going from one lore counter to three crosses two thresholds at once.
--
-- Shared by GameEvent's CountersPut and CountersRemoved as expediency, not
-- because putting and removing are one event: they are read separately, and
-- either takes its own record the moment it grows a field.
--
-- BOTH counts are a Natural, which is why they are named: a swap would invert
-- every threshold this exists to answer.
data CounterChange = MkCounterChange
  { object :: ObjectId.ObjectId,
    kind :: CounterKind.CounterKind Keyword.Keyword,
    before :: Natural.Natural,
    after :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
