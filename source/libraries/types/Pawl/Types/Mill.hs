module Pawl.Types.Mill where

import qualified Pawl.Types.MillTally as MillTally
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity

-- | The payload of Pawl.Types.Effect's Mill arm (#1305).
--
-- The MillTally is "and remember how many of them counted", for a later effect
-- of the same resolution to read as Quantity.InSlot -- CR 728.1's "for each
-- nonland card milled this way". Absent for a mill nothing looks back at, which
-- is every mill in the pool but rule 728.1's.
data Mill = MkMill
  { player :: PlayerRef.PlayerRef,
    quantity :: Quantity.Quantity,
    tally :: Maybe MillTally.MillTally
  }
  deriving (Eq, Ord, Show)
