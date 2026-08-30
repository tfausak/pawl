module Pawl.Types.MovedKinds where

import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Quantity as Quantity

-- | WHICH counters a CR 122.5 move carries, and how many of each.
--
-- The printed text goes six ways and rule 122.5 answers them differently.
-- Explorer's Cache says "move a +1/+1 counter", which settles the kind on the
-- card, so nothing is asked -- that is 'Named'. Agent's Toolkit says "move a
-- counter", naming none, which leaves WHICH counter moves to the player
-- (Pawl.Types.Prompt's ChooseMovedCounter) -- that is 'Chosen'. Fate Transfer
-- says "move all counters", which names no kind and asks nothing either, because
-- every kind crosses -- that is 'Every'. Resourceful Defense says "move any
-- number of counters", which names neither a kind nor a count and asks for BOTH
-- at once (Pawl.Types.Prompt's ChooseMovedCounters) -- that is 'AnyNumber'.
-- Spike Cannibal says "move all +1/+1 counters", which names the kind on the
-- card and asks nothing either, because the whole tally of that one kind crosses
-- -- that is 'EveryOfKind', 'Named' and 'Every' each taken half way. Goldberry,
-- River-Daughter says "move a counter of each kind not on Goldberry",
-- which names no kind and asks nothing either, because what the DESTINATION
-- already bears is what settles which kinds cross -- that is 'EachAbsentKind',
-- the one arm reading the second object for anything but rule 122.5's own
-- impossibilities. The others are not that one narrowed: rule 122.5's second
-- impossibility ("the first object doesn't have the appropriate kind of counter
-- on it") is the whole of the check under 'Named', is vacuous under 'Chosen' and
-- 'AnyNumber' since the kinds ON the object are what is offered, and under
-- 'Every', 'EveryOfKind' and 'EachAbsentKind' is what empties the batch rather
-- than what forbids it.
--
-- The count rides on the two arms that HAVE one rather than on a field beside
-- them, because the other four have none to carry: "all counters" and "all +1/+1
-- counters" are a tally read off the first object and a Quantity is one number,
-- "any number" is the player's answer rather than the card's, and "a counter of
-- each kind" fixes the count at one per kind on the wording itself, so a field
-- would have to be ignored under all four and a card could write a count that
-- means nothing.
--
-- 'Chosen' and 'AnyNumber' are two arms and not one because they ask different
-- questions: 'Chosen' settles the count on the card and asks WHICH KIND, where
-- 'AnyNumber' settles nothing and asks WHICH COUNTERS, so a single answer type
-- would leave one of the two over-specified. 'Chosen' therefore takes its whole
-- count out of the one kind picked, which is exact for a card that PRINTS a
-- count, since every printing that prints one prints "a counter" -- Scryfall
-- @oracle:\/(^|[^a-z])move [^.]*counter\/@, 2026-08-30, over every printing ever
-- released: the kindless moves carrying more than one counter say "all counters"
-- (Fate Transfer, Nexus Mentality, The Ozolith -- 'Every'), "any number of
-- counters" (Resourceful Defense, Slippery Bogbonder -- 'AnyNumber'), "one or
-- more counters" (Goldberry, River-Daughter's second ability, which is that arm
-- with zero excluded and is not written today, #2702) or "a counter of each kind
-- not on Goldberry" (her first -- 'EachAbsentKind'), and NONE of them prints a
-- number. A printing saying "move two counters", where the player could take one
-- +1\/+1 counter and one shield counter, would refute that.
--
-- 'EveryOfKind' is not 'Named' with a clever count. Black Panther, Wakandan
-- King's "all +1\/+1 counters" IS written that way -- a Quantity.AgainstSlot
-- reading the counters on the object a slot holds -- and that spelling needs a
-- slot to aim at, which a first side naming a whole GROUP of objects does not
-- have. The tally is per first object there, so the arm has to read it as it
-- performs each pair rather than evaluate one number for the sentence.
data MovedKinds
  = -- | Fate Transfer's "move all counters": every kind on the first object, the
    -- whole tally of each (CR 122.5).
    Every
  | -- | Explorer's Cache's "move a +1\/+1 counter": the kind the card names, that
    -- many of it (CR 122.5).
    Named (CounterKind.CounterKind Keyword.Keyword) Quantity.Quantity
  | -- | Spike Cannibal's "move all +1\/+1 counters": the kind the card names, the
    -- whole tally of it on each first object (CR 122.5).
    EveryOfKind (CounterKind.CounterKind Keyword.Keyword)
  | -- | Agent's Toolkit's "move a counter": one kind the player picks, that many
    -- of it (CR 122.5).
    Chosen Quantity.Quantity
  | -- | Resourceful Defense's "move any number of counters": however many of
    -- however many kinds the player picks (CR 122.5).
    AnyNumber
  | -- | Goldberry, River-Daughter's "move a counter of each kind not on
    -- Goldberry": one counter of each kind the DESTINATION does not already bear
    -- (CR 122.5).
    EachAbsentKind
  deriving (Eq, Ord, Show)

-- | The count the card asks for, for a caller that reads every Quantity an
-- effect holds. 'Nothing' under 'Every' and 'EveryOfKind', which ask for a tally
-- read off the first object rather than a number, under 'AnyNumber', whose count
-- is the player's answer, and under 'EachAbsentKind', whose one per kind the card
-- never writes down.
quantityOf :: MovedKinds -> Maybe Quantity.Quantity
quantityOf x = case x of
  Every -> Nothing
  Named _ quantity -> Just quantity
  EveryOfKind _ -> Nothing
  Chosen quantity -> Just quantity
  AnyNumber -> Nothing
  EachAbsentKind -> Nothing
