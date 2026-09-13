module Pawl.Types.DevourCount where

import qualified Numeric.Natural as Natural

-- | CR 702.82a's N in "devour N": how many +1\/+1 counters each permanent
-- sacrificed to devour buys.
data DevourCount
  = -- | CR 702.82a: a printed number (Thunder-Thrash Elder's devour 3).
    Fixed Natural.Natural
  | -- | CR 702.82b: N restated as the number of permanents devoured this way, so
    -- the counters are that count squared (Thromok the Insatiable's devour X).
    Devoured
  deriving (Eq, Ord, Show)
