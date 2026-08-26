module Pawl.Types.CounterPlacement where

import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | A CR 122.6 placement PATTERN: which counter kind, and which permanents
-- count. Two fields rather than a bare Filter, because the printed form names
-- both -- "whenever one or more -1\/-1 counters are put on one or more
-- creatures" -- and neither is recoverable from the other.
--
-- Shared by BOTH of CR 603.2c's readings, which is why the type is named after
-- the pattern rather than after either constructor:
-- Pawl.Types.TriggerCondition's PermanentGetsCounters names ONE permanent and
-- fires once per permanent, and its PermanentsGetCounters names a set and fires
-- once for the set. What differs is the scope, which is the constructor's, not
-- the pattern's -- the kind and the Filter are the same question either way.
data CounterPlacement = MkCounterPlacement
  { kind :: CounterKind.CounterKind Keyword.Keyword,
    permanents :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
