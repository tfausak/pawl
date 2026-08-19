module Pawl.Types.Reinforce where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Cost as Cost

-- | The payload of Pawl.Types.Keyword's Reinforce arm (#1305): CR 702.77a's
-- "Reinforce N--[cost]".
--
-- PARAMETRIC in the keyword for Pawl.Types.Cycling's reason: the Cost can name a
-- Keyword and Keyword names this. Only @Reinforce Keyword.Keyword@ is ever
-- written.
--
-- The two fields are independent halves of the printing: the cost is what is
-- paid, and the amount is how many +1/+1 counters the minted ability puts on.
data Reinforce keyword = MkReinforce
  { amount :: Natural.Natural,
    cost :: Cost.Cost keyword
  }
  deriving (Eq, Ord, Show)
