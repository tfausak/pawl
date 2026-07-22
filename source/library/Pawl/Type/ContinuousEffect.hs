module Pawl.Type.ContinuousEffect where

import Pawl.Type.Affected (Affected)
import Pawl.Type.Expiry (Expiry)
import Pawl.Type.Modification (Modification)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Timestamp (Timestamp)

-- A stored, resolution-generated continuous effect (CR 611.2), held in
-- GameState.continuousEffects. `timestamp` orders it within its layer (CR 613.7);
-- `expiry` decides when a sweep drops it (Pawl.Expiry; CR 514.2, 611.2a, 611.2b);
-- `affected` is its fixed set (CR 611.2c). Static-ability effects are NOT stored
-- here -- they are re-derived from Card.staticAbilities each projection.
data ContinuousEffect = MkContinuousEffect
  { source :: ObjectId,
    timestamp :: Timestamp,
    expiry :: Expiry,
    modification :: Modification,
    affected :: Affected
  }
  deriving (Eq, Ord, Show)
