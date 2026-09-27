module Pawl.Types.CounterSpread where

-- | How a removal of counters from permanents the payer chooses spreads them
-- (CR 118.1 as a cost, CR 608.2d as an effect): off ONE permanent, or divided
-- among several (CR 601.2h), a fixed count or one the payer settles.
data CounterSpread
  = -- | Zameck Guildmage: "Remove a +1\/+1 counter from a creature you control".
    FromOne
  | -- | Novijen Sages: "Remove two +1\/+1 counters from among creatures you
    -- control", the division the payer's own.
    FromAmong
  | -- | Ooze Flux: "Remove one or more +1\/+1 counters from among creatures you
    -- control", the count a floor and how many the payer's own.
    FromAmongAtLeast
  | -- | The count a cap and none an answer: Pawl.Types.RemovalCount's UpTo, as
    -- the mixed-kind division prompt reads it. No printed cost states it.
    FromAmongAtMost
  deriving (Bounded, Enum, Eq, Ord, Show)
