module Pawl.Types.TargetSpec where

import Pawl.Types.Filter (Filter)
import Pawl.Types.Pool (Pool)

-- | What a target slot may hold: a closed Pool of candidate recipients (CR 115),
-- narrowed by an open Filter (Nothing = the whole pool, e.g. bare "target
-- creature"). This retires the whole hand-carved family of colour- and
-- type-restricted specs (#40): each is now one data value.
--
-- CR 601.2c's "another" is not a third field: it is Filter.Not Filter.IsSource
-- inside the Filter, which is the same relation the predicate language already
-- expresses. Folding it in is what makes the exclusion agree with the Pool's
-- own recipient tagging -- a separate field was applied by deleting a
-- Recipient.ToObject, which never matched the ToCreature tags a Creatures pool
-- produces, so "another target creature" did not exclude itself.
data TargetSpec = MkTargetSpec Pool (Maybe Filter)
  deriving (Eq, Ord, Show)
