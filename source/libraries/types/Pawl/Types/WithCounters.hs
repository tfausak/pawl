module Pawl.Types.WithCounters where

import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Quantity as Quantity

-- | CR 614.1c's as-enters rewrite: which counters the permanent enters with, and
-- how many.
--
-- The amount is a Quantity rather than a literal because CR 614.1c admits
-- "enters with a number of +1/+1 counters on it equal to ..." (Undergrowth
-- Scavenger). Pawl.Types.PutCounters, the resolution-time mirror of this
-- payload, has always been a Quantity; so is Pawl.Types.EntryRiders' count.
data WithCounters = MkWithCounters
  { kind :: CounterKind.CounterKind Keyword.Keyword,
    amount :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
