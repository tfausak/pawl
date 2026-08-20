module Pawl.Types.Mill where

import qualified Pawl.Types.MillTally as MillTally
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's Mill arm (#1305).
--
-- The MillTally is "and remember how many of them counted", for a later effect
-- of the same resolution to read as Quantity.InSlot -- CR 728.1's "for each
-- nonland card milled this way". Absent for a mill nothing looks back at.
--
-- The `slot` is the OTHER kind of looking back, CR 701.17c's: an effect naming
-- the milled cards themselves finds them in the graveyard they moved to, which
-- is Midnight Tilling's "then you may return a permanent card from among them to
-- your hand". The tally remembers HOW MANY matched and this remembers WHICH
-- cards, so a card wanting both writes both.
--
-- The ids bound are the ones the move ARRIVED at (CR 400.7), since rule 701.17c
-- names the card in the zone it moved to; the tally counts the pre-move ids
-- instead, because it asks after characteristics the library card had. Absent for
-- every mill nothing looks back at.
data Mill = MkMill
  { player :: PlayerRef.PlayerRef,
    quantity :: Quantity.Quantity,
    tally :: Maybe MillTally.MillTally,
    slot :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
