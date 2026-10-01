module Pawl.Types.ActiveUntapProhibition where

import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Timestamp as Timestamp

-- | CR 502.3 / 611.1: a stored, resolution-generated UNTAP PROHIBITION, held in
-- GameState.untapProhibitions. Wall of Stolen Identity's "it doesn't untap
-- during its controller's untap step for as long as you control this creature"
-- is the producer.
--
-- Read at Pawl.Engine.UntapRestriction.doesNotUntap, which unions it into that
-- module's answer beside the printed carrier's rows, so Engine.untapAll never
-- learns which road a prohibition took.
--
-- OUTSIDE the layer system, which is CR 613.11, for
-- Pawl.Types.ActiveActivationProhibition's reason: an effect stopping CR 502.3's
-- untap modifies the rules rather than the object, so it is not an ability the
-- permanent has and an ability-removing effect leaves it standing.
--
-- A bare ObjectId, and CR 400.7 answered by the id itself, for
-- Pawl.Types.ActiveActivationProhibition's reasons.
--
-- `expiry` decides when a Pawl.Engine.Expiry sweep drops it (CR 611.2b's "for
-- as long as" is Expiry's conditional sweep). `timestamp` is stored for
-- ActiveBlockProhibition's reason, and nothing observes it.
--
-- Runtime-only: card data writes Pawl.Types.ForbidUntap, never one of these.
-- It has a codec (Pawl.Codec.ActiveUntapProhibition) because a game in progress
-- has to be writable to JSON (#126).
data ActiveUntapProhibition = MkActiveUntapProhibition
  { source :: ObjectId.ObjectId,
    timestamp :: Timestamp.Timestamp,
    expiry :: Expiry.Expiry,
    -- | The permanent that doesn't untap.
    object :: ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
