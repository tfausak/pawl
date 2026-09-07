module Pawl.Types.Designate where

import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | CR 701.60 / CR 702.112: give the slot's object this designation.
data Designate = MkDesignate
  { designation :: Designation.Designation,
    slot :: SlotName.SlotName,
    -- | CR 701.37c: the value the mark is set WITH -- "monstrosity X", whose X
    -- other abilities of that permanent may refer to. Nothing for a mark set
    -- with no number, which is every printing but a "Monstrosity X", so the key
    -- is elided rather than written.
    value :: Maybe Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
