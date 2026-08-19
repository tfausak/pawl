module Pawl.Types.Designate where

import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.SlotName as SlotName

-- | CR 701.60 / CR 702.112: give the slot's object this designation.
data Designate = MkDesignate
  { designation :: Designation.Designation,
    slot :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
