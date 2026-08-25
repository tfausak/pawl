module Pawl.Types.MoveCounters where

import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's MoveCounters arm (CR 122.5).
--
-- TWO SLOTS, not PutCounters' ObjectRef: rule 122.5 defines a move between an
-- object and "a second object", one on each side, and its own list of
-- impossibilities -- "the first and second objects are the same object" -- is
-- stated about a pair. An ObjectRef describing a set would have no pair for that
-- clause to be about.
--
-- NO kind field and NO count field, and both absences are the pool's rather than
-- a shortcut. The one printing this opcode exists for, Agent's Toolkit, says
-- "move a counter", so the KIND is the player's choice (Pawl.Types.Prompt's
-- ChooseMovedCounter) and the count is one. A move naming a kind, or naming
-- several counters, is a field this record does not carry yet (gap #2321).
data MoveCounters = MkMoveCounters
  { from :: SlotName.SlotName,
    to :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
