module Pawl.Types.Counter where

import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's Counter arm (#1507).
--
-- CR 701.6's keyword action, and nothing to do with CR 122's counters -- the
-- collision is the rulebook's, which spends the same word on both. The kinds of
-- counter a permanent carries are Pawl.Types.CounterKind; this is the verb.
--
-- The bound slot is where the count of what CR 701.6a actually countered is
-- written, for a later effect of the same resolution to read as
-- Quantity.InSlot -- Swift Silence's "draw a card for each spell countered this
-- way". Destroy's field, for Destroy's reason, and it has to be the FUNNEL's
-- answer rather than the sweep's: a spell CR 113.6g or CR 613.11 protects was
-- named and was not countered.
--
-- The count is of everything COUNTERED, which CR 701.6a makes "a spell or
-- ability" -- Swift Silence's rider says "each SPELL countered this way", and
-- the two agree because its own ref names only spells (CR 109.2b). Glen
-- Elendra's Answer says "each spell and ability countered this way" and reads
-- the count unnarrowed, its ref being ObjectRef.EachOnStack. A card that
-- counters both and counts only one kind would need the narrowing here.
--
-- THE RESOLUTION'S OWN count, not a look-back at GameState.events: "this way"
-- is CR 608.2's one resolution, so a countering by anything else is not in it.
-- The events that resolution recorded are a different question (#541).
data Counter = MkCounter
  { ref :: ObjectRef.ObjectRef,
    slot :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
