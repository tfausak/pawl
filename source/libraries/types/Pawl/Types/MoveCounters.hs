module Pawl.Types.MoveCounters where

import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Quantity as Quantity
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
-- `quantity` is HOW MANY counters of that kind cross, and ONE batch is what
-- crosses -- Black Panther, Wakandan King's "move all +1/+1 counters", written
-- as the count of that kind on the object the `from` slot names. Rule 122.5
-- never speaks of more than one counter, so it does not settle the batch on its
-- own; what does is that each of its four impossibilities is an object-level or
-- kind-level property (the two objects being one, the appropriate kind absent,
-- the destination refusing counters, the wrong zone) and NONE of them varies
-- with the count, so the all-or-nothing it states about one counter carries to a
-- batch of one kind between one pair with nothing further to decide. CR 614.16
-- then makes the placement half a single replaceable event -- "if an effect
-- would put one or more counters on a permanent" -- and CR 122.7 reads a batch
-- put the same way, which is the call Pawl.Types.PutCounters' own `quantity`
-- already makes. CR 609.3 covers the one count-sensitive shortfall, a count
-- larger than the object has: "it does only as much as possible".
--
-- Not implemented: a move that names no kind carries counters of ONE kind, the
-- one the player picks, so neither "move all counters" (Fate Transfer, The
-- Ozolith) nor a count spread across kinds can be written (gap #2465).
data MoveCounters = MkMoveCounters
  { from :: SlotName.SlotName,
    kind :: Maybe (CounterKind.CounterKind Keyword.Keyword),
    quantity :: Quantity.Quantity,
    -- | How many counters rule 122.5 ACTUALLY moved, written back for a later
    -- effect of the same resolution to read as Quantity.InSlot -- Black Panther,
    -- Wakandan King's "if one or more +1\/+1 counters are moved this way, you
    -- gain that much life and draw a card". Pawl.Types.Destroy's `slot` in every
    -- respect, including that it is bound even when nothing moved: zero is an
    -- answer, where an unbound slot would leave the rider's gate unevaluable
    -- instead. Nothing for every move nothing looks back at.
    slot :: Maybe SlotName.SlotName,
    to :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
