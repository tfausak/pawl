module Pawl.Types.PutCountersFrom where

import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's PutCountersFrom arm (CR 122.8).
--
-- NO QUANTITY, where Pawl.Types.PutCounters carries one: rule 122.8 names no
-- count at all -- "the same number of each kind of counter the first object
-- had" -- so what crosses is a per-kind tally, which no Quantity can spell.
-- `kind` is the one thing the card may settle, rule 122.8's second sentence:
-- 'Nothing' is Iron Apprentice's "put those counters", every kind the first
-- object had, and 'Just' is Selfless Police Captain's "put its +1\/+1
-- counters", that kind's tally alone.
--
-- One kind and not a set, though the rule says "kind(s)": no printing names two.
-- Scryfall @oracle:\/put (its|those) [^ ]+ counters\/@ with
-- @unique=cards&include_extras=true@, 2026-08-31, returns Selfless Police
-- Captain and Joraga Peach, each naming +1\/+1 alone. A printing saying "put its
-- +1\/+1 and shield counters on target creature" would refute that.
--
-- NOT Pawl.Types.MoveCounters with MovedKinds.Every or MovedKinds.EveryOfKind,
-- though the printed text reads like one: rule 122.8 says in as many words that
-- "the player doesn't move counters from one object to the other", and CR 122.2
-- is why -- the counters ceased to exist when the object left the battlefield, so
-- there is nothing left to remove. CR 122.5's fourth impossibility ("either
-- object is no longer in the correct zone") would decline the move outright.
--
-- `from` is a SlotName and not an ObjectRef, unlike `ref` below: rule 122.8 is
-- stated about "one object's counters", one object, and every printing reads it
-- off a slot the ability already holds -- the source itself under Iron
-- Apprentice's "when this creature dies".
data PutCountersFrom = MkPutCountersFrom
  { from :: SlotName.SlotName,
    kind :: Maybe (CounterKind.CounterKind Keyword.Keyword),
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
