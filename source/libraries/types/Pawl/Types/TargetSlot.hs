module Pawl.Types.TargetSlot where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.TargetCount as TargetCount

-- | What a target slot may hold: a closed Pool of candidate recipients (CR 115),
-- narrowed by an open Filter (Nothing = the whole pool, e.g. bare "target
-- creature"). This retires the whole hand-carved family of colour- and
-- type-restricted target constructors (#40): each is now one data value.
--
-- CR 601.2c's "another" is not a third field: it is Filter.Not Filter.IsSource
-- inside the Filter, which is what makes the exclusion agree with the Pool's own
-- recipient tagging. A separate field was applied by deleting a
-- Recipient.ToObject, which never matched the ToCreature tags a Creatures pool
-- produces, so "another target creature" did not exclude itself.
--
-- HOW MANY the slot takes is the `count` field (CR 601.2c), which covers CR
-- 115.6's "up to one", every larger count, and "any number of target ..." with
-- one range -- the last by naming no maximum. On the slot and not
-- on the mode, because a card makes the call per slot -- Explosive Entry's
-- artifact and creature slots are separately optional.
--
-- `filter` shadows the Prelude's, for the reason Pawl.Types.Count's does.
data TargetSlot = MkTargetSlot
  { pool :: Pool.Pool,
    filter :: Maybe (Filter.Filter Keyword.Keyword),
    count :: TargetCount.TargetCount
  }
  deriving (Eq, Ord, Show)

-- The ordinary "target creature" slot: exactly one recipient. Named because
-- almost every slot in the engine and the corpus is one, so writing the count out
-- at each would bury the handful that are not.
required :: Pool.Pool -> Maybe (Filter.Filter Keyword.Keyword) -> TargetSlot
required p f = MkTargetSlot p f TargetCount.one

-- CR 115.6 / 601.2c's "up to N targets": a slot the caster may fill any number of
-- times up to N, the empty answer included.
upTo :: Natural.Natural -> Pool.Pool -> Maybe (Filter.Filter Keyword.Keyword) -> TargetSlot
upTo n p f = MkTargetSlot p f (TargetCount.upTo n)

-- CR 601.2c's "any number of target ...": the same slot with no printed ceiling,
-- so the board's candidates are the only bound.
anyNumber :: Pool.Pool -> Maybe (Filter.Filter Keyword.Keyword) -> TargetSlot
anyNumber p f = MkTargetSlot p f TargetCount.anyNumber
