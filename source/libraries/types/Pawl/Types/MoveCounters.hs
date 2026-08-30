module Pawl.Types.MoveCounters where

import qualified Pawl.Types.MovedKinds as MovedKinds
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's MoveCounters arm (CR 122.5).
--
-- TWO SLOTS, not PutCounters' ObjectRef: rule 122.5 defines a move between an
-- object and "a second object", one on each side, and its own list of
-- impossibilities -- "the first and second objects are the same object" -- is
-- stated about a pair. An ObjectRef describing a set would have no pair for that
-- clause to be about.
--
-- Not implemented: the printed text that DOES name a set on the first side --
-- Spike Cannibal's "from all creatures", Aetherborn Marauder's "from other
-- permanents you control", Slippery Bogbonder's "from among creatures you
-- control", Oozeavite's "from other creatures you control" (#2704).
--
-- `kinds` is WHICH counters cross and how many of each, which the printed text
-- spells five ways; Pawl.Types.MovedKinds is where the five are set out.
--
-- ONE batch per kind is what crosses. Rule 122.5 never speaks of more than one
-- counter, so it does not settle the batch on its own; what does is that each of
-- its four impossibilities is an object-level or kind-level property (the two
-- objects being one, the appropriate kind absent, the destination refusing
-- counters, the wrong zone) and NONE of them varies with the count, so the
-- all-or-nothing it states about one counter carries to a batch of one kind
-- between one pair with nothing further to decide. CR 614.16 then makes the
-- placement half a single replaceable event -- "if an effect would put one or
-- more counters on a permanent" -- and CR 122.7 reads a batch put the same way,
-- which is the call Pawl.Types.PutCounters' own `quantity` already makes. CR
-- 609.3 covers the one count-sensitive shortfall, a count larger than the object
-- has: "it does only as much as possible".
data MoveCounters = MkMoveCounters
  { from :: SlotName.SlotName,
    kinds :: MovedKinds.MovedKinds,
    -- | How many counters rule 122.5 ACTUALLY moved, summed over every kind that
    -- crossed, written back for a later effect of the same resolution to read as
    -- Quantity.InSlot -- Black Panther, Wakandan King's "if one or more +1\/+1
    -- counters are moved this way, you gain that much life and draw a card".
    -- Pawl.Types.Destroy's `slot` in every respect, including that it is bound
    -- even when nothing moved: zero is an answer, where an unbound slot would
    -- leave the rider's gate unevaluable instead. Nothing for every move nothing
    -- looks back at.
    slot :: Maybe SlotName.SlotName,
    to :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
