module Pawl.Types.Claim where

import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Types.ClaimAxis as ClaimAxis
import qualified Pawl.Types.ObjectId as ObjectId

-- | What one payment SPENDS out of a pool of objects: which resource it draws on
-- (Pawl.Types.ClaimAxis), which objects are in that pool for it, and how many of
-- them it takes. CR 118.3's "fully" is the rule that makes this worth naming --
-- two payments cannot both have the one object, whether they are two components
-- of one cost (Pawl.Engine.Cost.jointlyPayable) or the costs of two mana
-- abilities the same payment activates (Pawl.Engine.Mana.payableResolutionsGiven).
--
-- A POOL and a COUNT rather than the objects themselves, because which object a
-- payment will take is not decided until it is paid: every object in the pool
-- serves the claim equally, so what a claim contends for is the pool's SIZE.
--
-- The AXIS is what two claims must share to contend at all: a sacrifice and a
-- tapping may both name one creature and both be paid, so they are not two claims
-- on one pool.
data Claim = MkClaim
  { axis :: ClaimAxis.ClaimAxis,
    pool :: Set.Set ObjectId.ObjectId,
    count :: Natural
  }
  deriving (Eq, Ord, Show)
