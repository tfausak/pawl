module Pawl.Types.RemovePlusOneCounters where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CounterSpread as CounterSpread
import qualified Pawl.Types.Filter as Filter

-- | The payload of Pawl.Types.CostComponent's RemovePlusOneCounters arm: CR
-- 118.1's removal as a cost aimed at permanents OTHER than the one the cost is
-- on, spent by taking this many +1\/+1 counters off the permanents the Filter
-- admits -- off one the payer chooses (Zameck Guildmage), or divided among
-- several as the payer pays (Novijen Sages), per the spread.
--
-- The count is COUNTERS, where Pawl.Types.TapPermanents' is objects.
--
-- The counter KIND is not a field: the printed cost states the +1\/+1 counter,
-- so there is no second kind for a card to spell. Pawl.Types.CostComponent's
-- RemoveCountersFromThis carries one because Hickory Woodlot's depletion
-- counter is such a second kind.
--
-- PARAMETRIC in the keyword for the Filter it carries, exactly as
-- Pawl.Types.CostComponent is.
data RemovePlusOneCounters keyword = MkRemovePlusOneCounters
  { count :: Natural.Natural,
    whichPermanent :: Filter.Filter keyword,
    spread :: CounterSpread.CounterSpread
  }
  deriving (Eq, Ord, Show)
