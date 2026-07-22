module Pawl.Type.ActiveReplacement where

import Pawl.Type.Duration (Duration)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.ReplacementEffect (ReplacementEffect)
import Pawl.Type.Timestamp (Timestamp)
import Pawl.Type.Uses (Uses)

-- CR 614.3 / 615.3: a floating, resolution-generated replacement effect, held in
-- GameState.replacements. The event-pipeline analog of ContinuousEffect: the
-- projection re-derives a permanent's static replacement abilities live, while
-- these are stored because the object that made them may be long gone.
--
-- `duration` decides when cleanup drops it (CR 514.2). `uses` is CR 614.3's
-- "until they're used up". `source` and `timestamp` are new here and are exactly
-- the two fields #58 recorded as missing: CR 615.13's "prevented" triggers and CR
-- 615.7's multi-source choice are no longer STRUCTURALLY blocked, only
-- card-blocked.
--
-- `timestamp` doubles as this instance's CR 614.5 identity (Pawl.Type.CandidateId):
-- GameState.nextTimestamp is monotone, so no two floating replacements share one.
data ActiveReplacement = MkActiveReplacement
  { effect :: ReplacementEffect,
    source :: ObjectId,
    timestamp :: Timestamp,
    duration :: Duration,
    uses :: Uses
  }
  deriving (Eq, Ord, Show)
