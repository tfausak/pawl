module Pawl.Types.ActiveReplacement where

import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PreventionRider as PreventionRider
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Uses as Uses

-- | CR 614.3 / 615.3: a floating, resolution-generated replacement effect, held in
-- GameState.replacements. The event-pipeline analog of ContinuousEffect: the
-- projection re-derives a permanent's static replacement abilities live, while
-- these are stored because the object that made them may be long gone.
--
-- `expiry` decides when a sweep drops it (Pawl.Engine.Expiry; CR 514.2). CR
-- 611.2a's turn-relative expiry has a printed producer here -- Dovin, Hand of
-- Control's -1, run end to end by Pawl.ExpirySpec's DovinHandOfControl group.
-- CR 611.2b's conditional one still has none: every other card in the pool arms
-- a floating replacement to AtCleanup or Never, so Expiry.While reaches this
-- carrier only through a hand-built fixture (#84). `uses` is CR 614.3's used-up count, and the
-- prevention shields are the rows that do not use it: a CR 615.7 shield's
-- remaining amount rides DamageRewrite.PreventNext instead, because 615.7 counts
-- damage where this field counts applications, and an unbounded one (CR 615.3)
-- has nothing to count at all.
--
-- CR 615.7's multi-source choice is asked over
-- Pawl.Engine.Event.resolveDamageBatch's batch: each DamageEvent still runs
-- its own CR 616.1 loop, and what the shielded player (or the shielded
-- permanent's controller) decides is the ORDER those loops run in -- among the
-- events their own shield contests, which CR 616.1's APNAP clause has already
-- grouped together (Pawl.Engine.Replacement.orderBatch).
--
-- `timestamp` doubles as this instance's CR 614.5 identity:
-- GameState.nextTimestamp is monotone, so no two floating replacements share one.
-- That identity is also what CR 615.13's preventions are GROUPED by
-- (Pawl.Engine.Replacement.groupPreventions), so a shield spent across two
-- simultaneous events fires a "when damage is prevented" ability once. Not
-- implemented: nothing carries it further than the grouping, so a trigger cannot
-- be keyed to the prevention that fired it -- "prevented this way" (#687).
--
-- `origin` is CR 614.15's question, and this carrier is the only one that can be
-- asked it: a self-replacement is an effect of a resolving spell or ability,
-- which is exactly what a floating, resolution-generated row is. The projection's
-- side of `collect` needs no such field, because CR 614.15 scopes a
-- self-replacement effect to a resolving spell or ability, ruling static
-- replacement abilities out.
data ActiveReplacement = MkActiveReplacement
  { effect :: ReplacementEffect.ReplacementEffect (Effect.Effect Card.Card),
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
    origin :: ReplacementOrigin.ReplacementOrigin,
    -- | CR 615.5's additional effect for a FLOATING row: Nothing on every row
    -- but a shield installed by one of the two prevention opcodes whose rider is
    -- non-empty. Copied forward to Pawl.Types.ReplacementCandidate and then to
    -- Pawl.Types.Prevention, since the row may be spent and dropped in the very
    -- application that fires it.
    --
    -- A whole Pawl.Types.PreventionRider rather than the bare program a card
    -- writes (DamageR.riders), because this carrier is the one that must
    -- snapshot: the spell that installed the row is gone, so its chosen targets
    -- and its CR 109.5 "you" cannot be re-derived. A permanent's static ability
    -- keeps both live and needs no such field.
    rider :: Maybe PreventionRider.PreventionRider
  }
  deriving (Eq, Ord, Show)
