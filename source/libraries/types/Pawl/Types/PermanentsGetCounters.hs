module Pawl.Types.PermanentsGetCounters where

import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | CR 603.2c's batch reading of a CR 122.6 placement: which counter kind, and
-- which permanents count. Two fields rather than a bare Filter, because the
-- printed form names both -- "whenever one or more -1\/-1 counters are put on one
-- or more creatures" -- and neither is recoverable from the other.
data PermanentsGetCounters = MkPermanentsGetCounters
  { kind :: CounterKind.CounterKind Keyword.Keyword,
    permanents :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
