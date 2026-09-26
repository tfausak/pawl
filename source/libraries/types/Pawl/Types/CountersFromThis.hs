module Pawl.Types.CountersFromThis where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CounterKind as CounterKind

-- | The payload of Pawl.Types.CostComponent's RemoveCountersFromThis arm: CR
-- 118.1's removal as a cost, taking this many counters of one kind off the
-- permanent the cost is on. Barkhide Troll's +1\/+1 counter and Hickory
-- Woodlot's depletion counter are the printings.
--
-- PARAMETRIC in the keyword for the CounterKind it carries, exactly as
-- Pawl.Types.CostComponent is.
data CountersFromThis keyword = MkCountersFromThis
  { kind :: CounterKind.CounterKind keyword,
    count :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
