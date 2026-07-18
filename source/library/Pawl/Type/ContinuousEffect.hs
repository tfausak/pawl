module Pawl.Type.ContinuousEffect where

import Pawl.Type.Affected (Affected)
import Pawl.Type.Duration (Duration)
import Pawl.Type.Modification (Modification)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Timestamp (Timestamp)

-- A stored, resolution-generated continuous effect (CR 611.2), held in
-- GameState.continuousEffects. `timestamp` orders it within its layer (CR 613.7);
-- `duration` decides when cleanup drops it (CR 514.2); `affected` is its fixed
-- set (CR 611.2c). Static-ability effects are NOT stored here -- they are
-- re-derived from Card.staticAbilities each projection.
data ContinuousEffect = MkContinuousEffect
  { source :: ObjectId,
    timestamp :: Timestamp,
    duration :: Duration,
    modification :: Modification,
    affected :: Affected
  }
  deriving (Eq, Ord, Show)
