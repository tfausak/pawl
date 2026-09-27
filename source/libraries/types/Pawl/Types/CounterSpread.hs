module Pawl.Types.CounterSpread where

-- | How a cost that removes counters from permanents the payer chooses spreads
-- them (CR 118.1 as a cost): off ONE permanent, or divided among several as the
-- payer pays (CR 601.2h), a fixed count or one the payer settles.
data CounterSpread
  = -- | Zameck Guildmage: "Remove a +1\/+1 counter from a creature you control".
    FromOne
  | -- | Novijen Sages: "Remove two +1\/+1 counters from among creatures you
    -- control", the division the payer's own.
    FromAmong
  | -- | Ooze Flux: "Remove one or more +1\/+1 counters from among creatures you
    -- control", the count a floor and how many the payer's own.
    FromAmongAtLeast
  deriving (Bounded, Enum, Eq, Ord, Show)
