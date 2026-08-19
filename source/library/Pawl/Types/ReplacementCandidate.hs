module Pawl.Types.ReplacementCandidate where

import qualified Pawl.Types.CandidateId as CandidateId
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PreventionRider as PreventionRider
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.Uses as Uses

-- | One replacement effect instance as the CR 616.1 loop sees it: what it does,
-- where it comes from and whose it is (CR 109.5's "you", which every
-- ControllerRelation pattern reads), which instance it is (CR 614.5), how long
-- it lasts (CR 614.3), and whether it is one of CR 614.15's self-replacement
-- effects (CR 616.1a's step). `source` is derivable from `identity` but is kept
-- explicit -- every applicability test and the ChooseReplacement payload read it
-- directly.
--
-- `origin` rides here rather than on ReplacementEffect because CR 614.15 is about
-- which ability CREATED an effect, not about what it does. It is what lets
-- bucketOf answer CR 616.1a without the rules core asking what an effect IS.
data ReplacementCandidate = MkReplacementCandidate
  { identity :: CandidateId.CandidateId,
    effect :: ReplacementEffect.ReplacementEffect (Effect.Effect Card.Card),
    source :: ObjectId.ObjectId,
    -- | CR 109.5's "you" for this instance, from whichever of the two segments
    -- can answer it: a permanent's static ability derives it from `source`'s
    -- current controller, and a floating row carries it baked (see
    -- Pawl.Types.ActiveReplacement). Nothing only where a permanent-sourced
    -- instance's source has left the board, which every pattern reading it then
    -- treats as matching nothing rather than everything.
    --
    -- NOT derivable from `source` at the point of use, which is the whole reason
    -- it is a field: Gather Specimens' row outlives the spell that installed it.
    controller :: Maybe PlayerId.PlayerId,
    -- | CR 614.3's "until they're used up or their duration has expired", for the
    -- one segment that has such a thing: a FLOATING row's own (expiry, uses).
    --
    -- Nothing for a permanent's static replacement ability, which has no lifetime
    -- to carry -- it is re-derived from the battlefield on every iteration and
    -- Pawl.Engine.Replacement.consume is a no-op for it. That is not a missing
    -- value standing in for one: two such candidates alike in `effect` really are
    -- interchangeable, because applying either spends nothing.
    --
    -- One field rather than two, so no candidate can carry half a lifetime.
    -- Present because CR 616.1's choice turns on it: two rows equal in `effect`
    -- but unequal here are NOT interchangeable, since `consume` spends only the
    -- one that applied and the other outlives it by a different rule (CR 614.10a's
    -- "the other will remain until another occurrence can be skipped").
    lifetime :: Maybe (Expiry.Expiry, Uses.Uses),
    origin :: ReplacementOrigin.ReplacementOrigin,
    -- | CR 615.5's additional effect, from whichever of the two segments carries
    -- one: copied off a FLOATING row, or built by
    -- Pawl.Engine.Replacement.collect from a permanent's DamageR.riders, whose
    -- environment is the live board rather than a snapshot. Nothing wherever the
    -- effect has no rider, which is every replacement but a prevention that
    -- prints one.
    rider :: Maybe PreventionRider.PreventionRider
  }
  deriving (Eq, Ord, Show)
