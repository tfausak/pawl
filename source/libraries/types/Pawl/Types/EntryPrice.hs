module Pawl.Types.EntryPrice where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | What CR 614.1c's "as this enters, you may [price]. If you don't, it enters
-- tapped" asks for -- the payload of Pawl.Types.EntryRewrite's OrTapped arm.
data EntryPrice
  = -- | CR 119.4: pay N life (Razorgrass Field).
    PayLife Natural.Natural
  | -- | CR 701.20a: reveal a matching card from your hand (Rustic Clachan). Not a
    -- cost, since a reveal changes no zone.
    Reveal (Filter.Filter Keyword.Keyword)
  deriving (Eq, Ord, Show)
