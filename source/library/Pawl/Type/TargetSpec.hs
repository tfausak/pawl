module Pawl.Type.TargetSpec where

import Pawl.Type.Exclusion (Exclusion)
import Pawl.Type.Filter (Filter)
import Pawl.Type.Pool (Pool)

-- What a target slot may hold: a closed Pool of candidate recipients (CR 115),
-- narrowed by an open Filter (Nothing = the whole pool, e.g. bare "target
-- creature"), with an Exclusion saying whether the source itself is a legal
-- target ("another"). This retires the whole hand-carved family of colour- and
-- type-restricted specs (#40): each is now one data value.
data TargetSpec = MkTargetSpec Pool (Maybe Filter) Exclusion
  deriving (Eq, Ord, Show)
