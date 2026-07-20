module Pawl.Type.ActivePrevention where

import Pawl.Type.Duration (Duration)
import Pawl.Type.Prevention (Prevention)

-- A floating, resolution-generated prevention effect (CR 615.3), held in
-- GameState.preventions. `duration` decides when cleanup drops it (CR 514.2) --
-- the prevention analog of ContinuousEffect for the event pipeline rather than
-- the projection. No timestamp (Fog needs no ordering; CR 615.7's multi-source
-- choice is deferred) and no source (CR 615.13 "prevented" triggers are deferred).
data ActivePrevention = MkActivePrevention
  { prevention :: Prevention,
    duration :: Duration
  }
  deriving (Eq, Ord, Show)
