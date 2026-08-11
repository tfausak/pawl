module Pawl.Types.TargetSpec where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.TargetRequirement as TargetRequirement

-- | What a target slot may hold: a closed Pool of candidate recipients (CR 115),
-- narrowed by an open Filter (Nothing = the whole pool, e.g. bare "target
-- creature"). This retires the whole hand-carved family of colour- and
-- type-restricted specs (#40): each is now one data value.
--
-- CR 601.2c's "another" is not a third field: it is Filter.Not Filter.IsSource
-- inside the Filter, which is what makes the exclusion agree with the Pool's own
-- recipient tagging. A separate field was applied by deleting a
-- Recipient.ToObject, which never matched the ToCreature tags a Creatures pool
-- produces, so "another target creature" did not exclude itself.
--
-- CR 115.6's "up to one" is the `requirement` field: whether this slot MUST be
-- filled. On the spec and not on the mode, because a card makes the two calls
-- independently -- Explosive Entry's artifact and creature slots are separately
-- optional.
--
-- `filter` shadows the Prelude's, for the reason Pawl.Types.Count's does.
data TargetSpec = MkTargetSpec
  { pool :: Pool.Pool,
    filter :: Maybe (Filter.Filter Keyword.Keyword),
    requirement :: TargetRequirement.TargetRequirement
  }
  deriving (Eq, Ord, Show)

-- The ordinary "target creature" spec: a slot that must be filled. Named because
-- almost every spec in the engine and the corpus is one, so writing the
-- requirement out at each would bury the handful that are not.
required :: Pool.Pool -> Maybe (Filter.Filter Keyword.Keyword) -> TargetSpec
required p f = MkTargetSpec p f TargetRequirement.Required

-- CR 115.6's "up to one target": a slot the caster may leave empty.
upToOne :: Pool.Pool -> Maybe (Filter.Filter Keyword.Keyword) -> TargetSpec
upToOne p f = MkTargetSpec p f TargetRequirement.UpToOne
