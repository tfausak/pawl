module Pawl.Types.While where

import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 611.2b's "for as long as" expiry: the condition to re-check, and the
-- player it is checked as.

-- The PlayerId is CR 109.5's "you", BAKED at arming time rather than re-derived:
-- the effect outlives the resolution that made it, so the seat the condition
-- reads has to travel with it.
data While = MkWhile
  { player :: PlayerId.PlayerId,
    condition :: Condition.Condition
  }
  deriving (Eq, Ord, Show)
