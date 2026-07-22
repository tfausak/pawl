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
-- "until they're used up". `source` and `timestamp` are new here. #58 recorded
-- their ABSENCE as one blocker on CR 615.13's "prevented" triggers and CR
-- 615.7's multi-source choice, and that particular blocker is gone: there is
-- now a source and a timestamp to report.
--
-- But a SEPARATE structural blocker remains for both rules, and these two
-- fields do not touch it: Pawl.Damage.applyDamage runs each DamageEvent in a
-- simultaneous batch through its own independent CR 616.1 loop (Pawl.Replacement
-- loop), one event at a time. CR 615.7 allocates ONE shield across the whole
-- batch, with the recipient choosing which event it prevents -- a choice a
-- per-event loop has no batch to make it over. CR 615.13 fires once per BATCH
-- ("each time a prevention effect is applied to one or more simultaneous damage
-- events"), which a per-event loop has no way to observe. Both rules stay
-- card-blocked (#58) until that batch shape changes, not merely field-blocked.
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
