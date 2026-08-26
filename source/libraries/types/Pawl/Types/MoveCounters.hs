module Pawl.Types.MoveCounters where

import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's MoveCounters arm (CR 122.5).
--
-- TWO SLOTS, not PutCounters' ObjectRef: rule 122.5 defines a move between an
-- object and "a second object", one on each side, and its own list of
-- impossibilities -- "the first and second objects are the same object" -- is
-- stated about a pair. An ObjectRef describing a set would have no pair for that
-- clause to be about.
--
-- `kind` is Maybe and not a bare CounterKind because the printed text goes both
-- ways and rule 122.5 answers them differently. Agent's Toolkit says "move a
-- counter", naming none, which leaves WHICH counter moves to the player
-- (Pawl.Types.Prompt's ChooseMovedCounter) -- that is Nothing. Explorer's Cache
-- says "move a +1/+1 counter", which settles it on the card, so nothing is asked
-- -- that is Just. The two are not the same question narrowed: rule 122.5's
-- second impossibility ("the first object doesn't have the appropriate kind of
-- counter on it") is vacuous under Nothing, since the kinds ON the object are
-- what is offered, and is the whole of the check under Just.
--
-- NO count field: the count is one. Rule 122.5 states every one of its
-- impossibilities about "a counter" singular, so a move of several counters is a
-- decision about atomicity this record does not make yet (gap #2321).
data MoveCounters = MkMoveCounters
  { from :: SlotName.SlotName,
    kind :: Maybe (CounterKind.CounterKind Keyword.Keyword),
    to :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
