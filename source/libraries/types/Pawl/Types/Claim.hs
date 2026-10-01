module Pawl.Types.Claim where

import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Types.ClaimAxis as ClaimAxis
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Threshold as Threshold

-- | What one payment SPENDS out of a pool of objects: which resource it draws on
-- (Pawl.Types.ClaimAxis), which objects are in that pool for it, and how many of
-- them it takes. CR 118.3's "fully" is the rule that makes this worth naming --
-- two payments cannot both have the one object, whether they are two components
-- of one cost (Pawl.Engine.Cost.jointlyPayable) or the costs of two mana
-- abilities the same payment activates (Pawl.Engine.Mana.payableResolutionsGiven).
--
-- A POOL and a COUNT rather than the objects themselves, because which object a
-- payment will take is not decided until it is paid. Without a threshold every
-- object in the pool serves the claim equally, so what it contends for is the
-- pool's SIZE; with one, only selections reaching the total serve it.
--
-- The AXIS is what two claims must share to contend at all: a sacrifice and a
-- tapping may both name one creature and both be paid, so they are not two claims
-- on one pool.
data Claim = MkClaim
  { axis :: ClaimAxis.ClaimAxis,
    pool :: Set.Set ObjectId.ObjectId,
    -- | How many objects the claim takes, or with a threshold how many disjoint
    -- selections of them, each reaching its total.
    count :: Natural,
    -- | The total each selection must reach, where the claim is CR 702.122a's or
    -- CR 701.59a's aggregate rather than a number of objects.
    threshold :: Maybe Threshold.Threshold
  }
  deriving (Eq, Ord, Show)
