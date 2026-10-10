module Pawl.Types.CountersFromPermanents where

import qualified Pawl.Types.CostAmount as CostAmount
import qualified Pawl.Types.CounterSpread as CounterSpread
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.WhichCounters as WhichCounters

-- | The payload of Pawl.Types.CostComponent's RemoveCounters arm: CR 118.1's
-- removal as a cost aimed at permanents OTHER than the one the cost is on,
-- spent by taking this many counters of the kind given off the permanents the
-- Filter admits -- off one the payer chooses (Zameck Guildmage), or divided
-- among several as the payer pays (Novijen Sages), per the spread.
--
-- The count is COUNTERS, where Pawl.Types.TapPermanents' is objects.
--
-- PARAMETRIC in the keyword for the Filter it carries, exactly as
-- Pawl.Types.CostComponent is.
data CountersFromPermanents keyword = MkCountersFromPermanents
  { count :: CostAmount.CostAmount,
    kind :: WhichCounters.WhichCounters keyword,
    whichPermanent :: Filter.Filter keyword,
    spread :: CounterSpread.CounterSpread
  }
  deriving (Eq, Ord, Show)
