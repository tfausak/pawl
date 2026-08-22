module Pawl.Types.RollDie where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's RollDie arm (CR 706.1): which die to
-- roll, and the slot the result is bound at for a later effect of the same
-- resolution to read as Pawl.Types.Quantity's InSlot (CR 706.4).
--
-- `sides` is CR 706.1a's N -- a dN has N equally likely outcomes numbered 1 to
-- N -- so it is the whole description of the die, and the range the answer is
-- filtered back against.
--
-- With no modifier in the pool, CR 706.2's natural result and result coincide,
-- so the one number this record binds is both.
--
-- Construct with BRACE syntax everywhere. A count or a table added later is a
-- new field, and positional construction absorbs a new field in argument order
-- with nothing red (#2009, #2021).
data RollDie = MkRollDie
  { sides :: Natural.Natural,
    slot :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
