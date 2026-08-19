module Pawl.Types.WithCounters where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword

-- | CR 614.1c's as-enters rewrite: which counters the permanent enters with, and
-- how many.
data WithCounters = MkWithCounters
  { kind :: CounterKind.CounterKind Keyword.Keyword,
    amount :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
