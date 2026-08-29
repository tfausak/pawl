module Pawl.Types.MovedKinds where

import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Quantity as Quantity

-- | WHICH counters a CR 122.5 move carries, and how many of each.
--
-- The printed text goes three ways and rule 122.5 answers them differently.
-- Explorer's Cache says "move a +1/+1 counter", which settles the kind on the
-- card, so nothing is asked -- that is 'Named'. Agent's Toolkit says "move a
-- counter", naming none, which leaves WHICH counter moves to the player
-- (Pawl.Types.Prompt's ChooseMovedCounter) -- that is 'Chosen'. Fate Transfer
-- says "move all counters", which names no kind and asks nothing either, because
-- every kind crosses -- that is 'Every'. The first two are not the third
-- narrowed: rule 122.5's second impossibility ("the first object doesn't have
-- the appropriate kind of counter on it") is the whole of the check under
-- 'Named', is vacuous under 'Chosen' since the kinds ON the object are what is
-- offered, and under 'Every' is what empties the batch rather than what forbids
-- it.
--
-- The count rides on the two arms that HAVE one rather than on a field beside
-- them, because 'Every' has none to carry: "all counters" is a tally PER KIND
-- and a Quantity is one number, so a field would have to be ignored under
-- 'Every' and a card could write a count that means nothing.
data MovedKinds
  = -- | Fate Transfer's "move all counters": every kind on the first object, the
    -- whole tally of each (CR 122.5).
    Every
  | -- | Explorer's Cache's "move a +1\/+1 counter": the kind the card names, that
    -- many of it (CR 122.5).
    Named (CounterKind.CounterKind Keyword.Keyword) Quantity.Quantity
  | -- | Agent's Toolkit's "move a counter": one kind the player picks, that many
    -- of it (CR 122.5).
    --
    -- Not implemented: the count comes out of the ONE kind picked, so a printing
    -- letting a player take two counters of different kinds is unwritable
    -- (gap #2607).
    Chosen Quantity.Quantity
  deriving (Eq, Ord, Show)

-- | The count the card asks for, for a caller that reads every Quantity an
-- effect holds. 'Nothing' under 'Every', which asks for a tally per kind rather
-- than a number.
quantityOf :: MovedKinds -> Maybe Quantity.Quantity
quantityOf x = case x of
  Every -> Nothing
  Named _ quantity -> Just quantity
  Chosen quantity -> Just quantity
