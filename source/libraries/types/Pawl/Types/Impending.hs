module Pawl.Types.Impending where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Cost as Cost

-- | The payload of Pawl.Types.Keyword's Impending arm: CR 702.176a's
-- "Impending N--[cost]".
--
-- PARAMETRIC in the keyword for Pawl.Types.Reinforce's reason. `counters` is the
-- N time counters the permanent enters with, `cost` the alternative cost.
data Impending keyword = MkImpending
  { counters :: Natural.Natural,
    cost :: Cost.Cost keyword
  }
  deriving (Eq, Ord, Show)
