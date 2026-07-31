module Pawl.Types.DiscardCause where

-- CR 701.9a: why a discard happened. Carried by Pawl.Types.GameEvent's Discarded
-- so that ONE logged discard answers both of the questions the rules ask about
-- it -- "was a card discarded?" and "was a card cycled?" -- rather than two log
-- entries describing one action.
--
-- That is CR 702.29d's doing. CR 702.29a makes cycling a discard ("'Cycling
-- [cost]' means '[Cost], Discard this card: Draw a card'"), and CR 702.29d
-- bounds what that costs a discard trigger: "Some cards have abilities that
-- trigger whenever a player 'cycles or discards' a card. These abilities trigger
-- only once when a card is cycled." One event with two descriptions makes that
-- hold by construction; two events would make it a rule every reader had to
-- remember.
data DiscardCause
  = -- CR 701.9a's plain discard, whatever asked for it: a "discard a card"
    -- effect, a cost component naming cards, or CR 514.1's cleanup step.
    Ordinary
  | -- CR 702.29c: "'When you cycle this card' means 'When you discard this card
    -- to pay an activation cost of a cycling ability.'" Recorded by Pawl.Engine.Cost's
    -- DiscardThis component -- see that arm for what it cannot see.
    ToPayCyclingCost
  deriving (Eq, Ord, Show)
