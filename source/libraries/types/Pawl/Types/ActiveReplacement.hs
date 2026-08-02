module Pawl.Types.ActiveReplacement where

import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Uses as Uses

-- | CR 614.3 / 615.3: a floating, resolution-generated replacement effect, held in
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
--
-- `origin` is CR 614.15's question, and this carrier is the only one that can be
-- asked it: a self-replacement is "an effect of a resolving spell or ability",
-- which is exactly what a floating, resolution-generated row is. The projection's
-- side of `collect` -- a permanent's static replacement abilities -- has no such
-- field and needs none, because CR 614.15 rules those out in its first sentence
-- ("self-replacement effects ... are not continuous effects").
data ActiveReplacement = MkActiveReplacement
  { effect :: ReplacementEffect.ReplacementEffect,
    source :: ObjectId.ObjectId,
    -- | CR 109.5's "you", BAKED as the row is installed rather than re-derived
    -- from `source`. The same posture Pawl.Types.ContinuousEffect and
    -- Pawl.Types.PlayerEffect already take, and for the same reason: the source
    -- of a floating replacement is a resolving spell, and CR 608.2m has put it
    -- in its owner's graveyard -- as a NEW object with a NEW id (CR 400.7) --
    -- long before the row is ever consulted, so there is nothing left on the
    -- board to ask. Gather Specimens is the card that makes this observable:
    -- both halves of its text ("an OPPONENT's control", "YOUR control") are
    -- relative to a player its own source can no longer name.
    --
    -- The projection's side of Pawl.Engine.Replacement.collect needs no such
    -- field: a permanent's static replacement ability has its source sitting on
    -- the battlefield, so CR 109.5's "you" is read live -- which is also the
    -- rules' answer, since a stolen Furnace of Rath's "you" follows the theft.
    controller :: PlayerId.PlayerId,
    timestamp :: Timestamp.Timestamp,
    expiry :: Expiry.Expiry,
    uses :: Uses.Uses,
    origin :: ReplacementOrigin.ReplacementOrigin
  }
  deriving (Eq, Ord, Show)
