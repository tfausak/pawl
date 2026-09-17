module Pawl.Types.RemovePlusOneCounters where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter

-- | The payload of Pawl.Types.CostComponent's RemovePlusOneCounters arm: CR
-- 118.1's removal as a cost aimed at ANOTHER permanent, spent by taking this
-- many +1\/+1 counters off ONE permanent the payer chooses out of the ones the
-- Filter admits. Zameck Guildmage's "Remove a +1\/+1 counter from a creature you
-- control" is the printing.
--
-- The count is COUNTERS, where Pawl.Types.TapPermanents' is objects: this
-- component names one permanent and says how many counters come off it.
--
-- A cost that spreads the removal over SEVERAL permanents -- Novijen Sages'
-- "Remove two +1\/+1 counters from among creatures you control" -- is a
-- different shape, the payer dividing the count as they pay, and is not
-- implemented (#3813).
--
-- The counter KIND is not a field. Pawl.Types.CostComponent's sibling arm
-- RemovePlusOneCountersFromThis fixes CounterKind.PlusOnePlusOne the same way
-- and for the same reason: the printed cost states the +1\/+1 counter, so there
-- is no second kind for a card to spell.
--
-- PARAMETRIC in the keyword for the Filter it carries, exactly as
-- Pawl.Types.CostComponent is.
data RemovePlusOneCounters keyword = MkRemovePlusOneCounters
  { count :: Natural.Natural,
    whichPermanent :: Filter.Filter keyword
  }
  deriving (Eq, Ord, Show)
