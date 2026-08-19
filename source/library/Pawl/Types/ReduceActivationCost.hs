module Pawl.Types.ReduceActivationCost where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost

-- | The payload of Pawl.Types.PlayerEffect's ReduceActivationCost arm (#1305).
--
-- The FLOOR is carried rather than assumed, because it is card text (CR 101.1)
-- and not a rule: Heartstone says "This effect can't reduce the mana in that
-- cost to less than one mana" and so carries 1, while Blossoming Tortoise's
-- "Activated abilities of lands you control cost {1} less to activate" does not
-- say it and carries 0. See Pawl.Types.CostAdjustments.reductions for what zero
-- means, why a floor never raises a cost, and why the two kinds cannot share one
-- floor over the pool.
data ReduceActivationCost = MkReduceActivationCost
  { whichAbilities :: Filter.Filter Keyword.Keyword,
    reduction :: ManaCost.ManaCost,
    floor :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
