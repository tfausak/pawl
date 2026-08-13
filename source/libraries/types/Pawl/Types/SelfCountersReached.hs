module Pawl.Types.SelfCountersReached where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword

-- | CR 714.2's chapter trigger and its kin: which counter kind, and the count at
-- which the ability fires.
data SelfCountersReached = MkSelfCountersReached
  { kind :: CounterKind.CounterKind Keyword.Keyword,
    amount :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
