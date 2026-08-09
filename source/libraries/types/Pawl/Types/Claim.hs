module Pawl.Types.Claim where

import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Zone as Zone

-- | What one payment takes OUT of a zone: which zone it draws from, which
-- objects are in that pool for it, and how many of them it takes. CR 118.3's
-- "fully" is the rule that makes this worth naming -- two payments cannot both
-- take the one object, whether they are two components of one cost
-- (Pawl.Engine.Cost.jointlyPayable) or the costs of two mana abilities the same
-- payment activates (Pawl.Engine.Mana.payableResolutionsGiven).
--
-- A POOL and a COUNT rather than the objects themselves, because which object a
-- payment will take is not decided until it is paid: every object in the pool
-- serves the claim equally, so what a claim contends for is the pool's SIZE.
--
-- The ZONE alone keys the contention even though a hand and a graveyard are
-- per-player (CR 400.3, CR 108.4): every claim pawl builds is on one player's
-- own copy of the zone, so two claims on one zone are two claims on one pool.
data Claim = MkClaim
  { zone :: Zone.Zone,
    pool :: Set.Set ObjectId.ObjectId,
    count :: Natural
  }
  deriving (Eq, Ord, Show)
