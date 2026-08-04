module Pawl.Types.DiscardCause where

-- | CR 701.9a: why a discard happened. Carried by Pawl.Types.GameEvent's Discarded
-- so that ONE logged discard answers both of the questions the rules ask about
-- it -- "was a card discarded?" and "was a card cycled?" -- rather than two log
-- entries describing one action.
--
-- That is CR 702.29d's doing. CR 702.29a makes cycling a discard, and CR 702.29d
-- makes an ability that triggers whenever a player "cycles or discards" a card
-- trigger only once when a card is cycled. One event with two descriptions
-- makes that hold by construction; two events would make it a rule every reader
-- had to remember.
data DiscardCause
  = -- | CR 701.9a's plain discard, whatever asked for it: an effect, a cost
    -- component naming cards, or CR 514.1's cleanup step.
    Ordinary
  | -- | CR 702.29c: discarding this card to pay a cycling ability's activation
    -- cost. Recorded by Pawl.Engine.Cost's DiscardThis component -- see that arm
    -- for what it cannot see.
    ToPayCyclingCost
  deriving (Eq, Ord, Show)
