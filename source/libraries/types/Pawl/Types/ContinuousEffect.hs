module Pawl.Types.ContinuousEffect where

import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Timestamp as Timestamp

-- | A stored, resolution-generated continuous effect (CR 611.2), held in
-- GameState.continuousEffects. `timestamp` orders it within its layer (CR 613.7);
-- `expiry` decides when a sweep drops it (Pawl.Engine.Expiry; CR 514.2, 611.2a, 611.2b);
-- `affected` is its fixed set (CR 611.2c). Static-ability effects are NOT stored
-- here -- they are re-derived from Card.staticAbilities each projection.
data ContinuousEffect = MkContinuousEffect
  { source :: ObjectId.ObjectId,
    timestamp :: Timestamp.Timestamp,
    expiry :: Expiry.Expiry,
    modification :: Modification.Modification,
    affected :: Affected.Affected
  }
  deriving (Eq, Ord, Show)
