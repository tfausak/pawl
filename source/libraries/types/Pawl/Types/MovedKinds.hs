module Pawl.Types.MovedKinds where

import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Quantity as Quantity

-- | WHICH counters a CR 122.5 move carries, and how many of each.
--
-- The printed text goes four ways and rule 122.5 answers them differently.
-- Explorer's Cache says "move a +1/+1 counter", which settles the kind on the
-- card, so nothing is asked -- that is 'Named'. Agent's Toolkit says "move a
-- counter", naming none, which leaves WHICH counter moves to the player
-- (Pawl.Types.Prompt's ChooseMovedCounter) -- that is 'Chosen'. Fate Transfer
-- says "move all counters", which names no kind and asks nothing either, because
-- every kind crosses -- that is 'Every'. Resourceful Defense says "move any
-- number of counters", which names neither a kind nor a count and asks for BOTH
-- at once (Pawl.Types.Prompt's ChooseMovedCounters) -- that is 'AnyNumber'. The
-- others are not that one narrowed: rule 122.5's second impossibility ("the
-- first object doesn't have the appropriate kind of counter on it") is the whole
-- of the check under 'Named', is vacuous under 'Chosen' and 'AnyNumber' since
-- the kinds ON the object are what is offered, and under 'Every' is what empties
-- the batch rather than what forbids it.
--
-- The count rides on the two arms that HAVE one rather than on a field beside
-- them, because 'Every' and 'AnyNumber' have none to carry: "all counters" is a
-- tally PER KIND and a Quantity is one number, and "any number" is the player's
-- answer rather than the card's, so a field would have to be ignored under both
-- and a card could write a count that means nothing.
--
-- 'Chosen' and 'AnyNumber' are two arms and not one because they ask different
-- questions: 'Chosen' settles the count on the card and asks WHICH KIND, where
-- 'AnyNumber' settles nothing and asks WHICH COUNTERS, so a single answer type
-- would leave one of the two over-specified. 'Chosen' therefore takes its whole
-- count out of the one kind picked, which is exact for a card that PRINTS a
-- count, since every printing that prints one prints "a counter" -- Scryfall
-- @oracle:\/(^|[^a-z])move [^.]*counter\/@, 2026-08-30, over every printing ever
-- released: the kindless moves carrying more than one counter all say "all
-- counters" ('Every') or "any number of counters" \/ "one or more counters"
-- ('AnyNumber'). A printing saying "move two counters", where the player could
-- take one +1\/+1 counter and one shield counter, would refute that.
data MovedKinds
  = -- | Fate Transfer's "move all counters": every kind on the first object, the
    -- whole tally of each (CR 122.5).
    Every
  | -- | Explorer's Cache's "move a +1\/+1 counter": the kind the card names, that
    -- many of it (CR 122.5).
    Named (CounterKind.CounterKind Keyword.Keyword) Quantity.Quantity
  | -- | Agent's Toolkit's "move a counter": one kind the player picks, that many
    -- of it (CR 122.5).
    Chosen Quantity.Quantity
  | -- | Resourceful Defense's "move any number of counters": however many of
    -- however many kinds the player picks (CR 122.5).
    AnyNumber
  deriving (Eq, Ord, Show)

-- | The count the card asks for, for a caller that reads every Quantity an
-- effect holds. 'Nothing' under 'Every', which asks for a tally per kind rather
-- than a number, and under 'AnyNumber', whose count is the player's answer.
quantityOf :: MovedKinds -> Maybe Quantity.Quantity
quantityOf x = case x of
  Every -> Nothing
  Named _ quantity -> Just quantity
  Chosen quantity -> Just quantity
  AnyNumber -> Nothing
