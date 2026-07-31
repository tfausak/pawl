module Pawl.Types.ActiveReplacement where

import Pawl.Types.Expiry (Expiry)
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.ReplacementEffect (ReplacementEffect)
import Pawl.Types.Timestamp (Timestamp)
import Pawl.Types.Uses (Uses)

-- CR 614.3 / 615.3: a floating, resolution-generated replacement effect, held in
-- GameState.replacements. The event-pipeline analog of ContinuousEffect: the
-- projection re-derives a permanent's static replacement abilities live, while
-- these are stored because the object that made them may be long gone.
--
-- `expiry` decides when a sweep drops it (Pawl.Engine.Expiry; CR 514.2). No card in the
-- pool arms a floating replacement to anything but AtCleanup or Never, so the
-- conditional (CR 611.2b) and turn-relative (CR 611.2a) expiries reach this
-- carrier only through hand-built test fixtures, and AtTurnOf on a replacement
-- has no test at all (#84). `uses` is CR 614.3's
-- "until they're used up". `source` and `timestamp` are new here. #58 recorded
-- their ABSENCE as one blocker on CR 615.13's "prevented" triggers and CR
-- 615.7's multi-source choice, and that particular blocker is gone: there is
-- now a source and a timestamp to report.
--
-- CR 615.7 genuinely needs the BATCH SHAPE to change: it allocates ONE shield
-- across simultaneous sources, with the recipient choosing which EVENT it
-- prevents -- a choice with no batch to be made over as long as
-- Pawl.Engine.Damage.applyDamage keeps running each DamageEvent through its own
-- independent CR 616.1 loop (Pawl.Engine.Replacement.loop), one event at a time.
--
-- CR 615.13's blocker is NARROWER than that. Pawl.Engine.Damage.applyDamage already
-- holds the whole batch -- it is the one thing calling resolveDamage once per
-- event -- so the per-event unit does not need to change. What blocks 615.13 is
-- that resolveDamage :: DamageEvent -> Game (Maybe DamageEvent) reports only
-- whether an event survived, discarding WHICH candidate applied; applyDamage
-- therefore has nothing to group by when 615.13 asks it to fire "each time a
-- prevention effect is applied to one or more simultaneous damage events."
-- Widening that return type to also report the applying candidate would unblock
-- 615.13 without touching the per-event batch shape 615.7 still needs. Both stay
-- card-blocked (#58) until their respective blocker is addressed.
--
-- `timestamp` doubles as this instance's CR 614.5 identity (Pawl.Types.CandidateId):
-- GameState.nextTimestamp is monotone, so no two floating replacements share one.
data ActiveReplacement = MkActiveReplacement
  { effect :: ReplacementEffect,
    source :: ObjectId,
    timestamp :: Timestamp,
    expiry :: Expiry,
    uses :: Uses
  }
  deriving (Eq, Ord, Show)
