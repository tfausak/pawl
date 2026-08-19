module Pawl.Types.RemoveCounters where

import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's RemoveCounters arm (#1305).
--
-- PutCounters' mirror, but NOT a shared record with it: this names a slot where
-- that names an ObjectRef, so the two coincide in two fields out of three and
-- not in the third. Sharing is only ever for a shape that actually coincides.
data RemoveCounters = MkRemoveCounters
  { kind :: CounterKind.CounterKind Keyword.Keyword,
    quantity :: Quantity.Quantity,
    slot :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
