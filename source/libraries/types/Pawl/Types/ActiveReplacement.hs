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
-- `expiry` decides when a sweep drops it (Pawl.Engine.Expiry; CR 514.2). No card
-- in the pool arms a floating replacement to anything but AtCleanup or Never, so
-- the conditional (CR 611.2b) and turn-relative (CR 611.2a) expiries reach this
-- carrier only through hand-built test fixtures, and AtTurnOf on a replacement
-- has no test at all (#84). `uses` is CR 614.3's used-up count, and a CR 615.7
-- shield is the one row that does not use it: its remaining amount rides
-- DamageRewrite.PreventNext instead, because 615.7 counts damage where this field
-- counts applications.
--
-- CR 615.7's multi-source choice is asked over
-- Pawl.Engine.Replacement.resolveDamageBatch's batch: each DamageEvent still runs
-- its own CR 616.1 loop, and what the shielded player decides is the ORDER those
-- loops run in.
--
-- Not implemented: CR 615.13's "prevented" triggers.
-- Pawl.Engine.Replacement.resolveDamage reports only whether an event survived,
-- discarding WHICH candidate applied and how much it prevented, so nothing can
-- fire on a prevention (#612).
--
-- `timestamp` doubles as this instance's CR 614.5 identity:
-- GameState.nextTimestamp is monotone, so no two floating replacements share one.
--
-- `origin` is CR 614.15's question, and this carrier is the only one that can be
-- asked it: a self-replacement is an effect of a resolving spell or ability,
-- which is exactly what a floating, resolution-generated row is. The projection's
-- side of `collect` needs no such field, because CR 614.15's first sentence rules
-- static replacement abilities out.
data ActiveReplacement = MkActiveReplacement
  { effect :: ReplacementEffect.ReplacementEffect,
    source :: ObjectId.ObjectId,
    -- | CR 109.5's "you", BAKED as the row is installed rather than re-derived
    -- from `source`, the same posture Pawl.Types.ContinuousEffect and
    -- Pawl.Types.PlayerEffect take: the source of a floating replacement is a
    -- resolving spell, and CR 608.2n has put it in its owner's graveyard as a NEW
    -- object (CR 400.7) long before the row is consulted. Gather Specimens makes
    -- it observable -- both halves of its text are relative to a player its own
    -- source can no longer name.
    --
    -- The projection's side of Pawl.Engine.Replacement.collect needs no such
    -- field: a permanent's static replacement ability has its source on the
    -- battlefield, so "you" is read live -- which is also the rules' answer, since
    -- a stolen Furnace of Rath's "you" follows the theft.
    controller :: PlayerId.PlayerId,
    timestamp :: Timestamp.Timestamp,
    expiry :: Expiry.Expiry,
    uses :: Uses.Uses,
    origin :: ReplacementOrigin.ReplacementOrigin
  }
  deriving (Eq, Ord, Show)
