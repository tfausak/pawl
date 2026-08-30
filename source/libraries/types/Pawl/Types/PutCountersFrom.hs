module Pawl.Types.PutCountersFrom where

import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's PutCountersFrom arm (CR 122.8).
--
-- ONE SLOT on the reading side, where Pawl.Types.PutCounters carries a kind and
-- a Quantity: rule 122.8 names no kind and no count at all -- "the same number
-- of each kind of counter the first object had" -- so what crosses is a whole
-- per-kind tally, which no Quantity can spell.
--
-- NOT Pawl.Types.MoveCounters with MovedKinds.Every, though the printed text
-- reads like one: rule 122.8 says in as many words that "the player doesn't move
-- counters from one object to the other", and CR 122.2 is why -- the counters
-- ceased to exist when the object left the battlefield, so there is nothing left
-- to remove. CR 122.5's fourth impossibility ("either object is no longer in the
-- correct zone") would decline the move outright.
--
-- `from` is a SlotName and not an ObjectRef, unlike `ref` below: rule 122.8 is
-- stated about "one object's counters", one object, and every printing reads it
-- off a slot the ability already holds -- the source itself under Iron
-- Apprentice's "when this creature dies".
--
-- Not implemented: rule 122.8's second sentence, an ability that "specifies what
-- kind(s) of counters to place", so only the whole tally can be written
-- (#2698).
data PutCountersFrom = MkPutCountersFrom
  { from :: SlotName.SlotName,
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
