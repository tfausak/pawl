module Pawl.Types.CardsPutIntoZone where

import qualified Data.Set as Set
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Zone as Zone

-- | Which arriving cards fire a "whenever one or more cards are put into [a
-- zone]" ability (CR 603.2c), and from where -- Dutiful Knowledge Seeker's "into
-- a library from anywhere".
--
-- The arrival-side twin of Pawl.Types.CardLeavesZone: the zone the card reached
-- is fixed and the zone it left may be any. The Filter is read against the card
-- as it last existed in the zone it left (CR 603.10a's look-back); the zones are
-- no characteristic of the card, so they come from the event.
data CardsPutIntoZone = MkCardsPutIntoZone
  { filter :: Filter.Filter Keyword.Keyword,
    -- | The zones it may have come from (CR 400.7); empty admits any but `to`.
    from :: Set.Set Zone.Zone,
    -- | The zone it was put into.
    to :: Zone.Zone
  }
  deriving (Eq, Ord, Show)
