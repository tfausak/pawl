module Pawl.Types.Destroy where

import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's Destroy arm (#1305).
--
-- The bound slot is where the count of what CR 701.8 actually destroyed is
-- written, for a later effect of the same resolution to read as
-- Quantity.InSlot. Absent for a destruction nothing looks back at, which is
-- every destruction in the pool but the one that counts.
data Destroy = MkDestroy
  { ref :: ObjectRef.ObjectRef,
    regenerability :: Regenerability.Regenerability,
    slot :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
