module Pawl.Types.ActiveBlockRequirement where

import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Timestamp as Timestamp

-- | CR 509.1c / 613.11: a stored, resolution-generated BLOCKING REQUIREMENT,
-- held in GameState.blockRequirements. The block-axis analogue of
-- ActivePlayerEffect: the printed carrier (Pawl.Types.BlockRequirement) is
-- re-derived live from the battlefield, while these are stored because the
-- object that made them may be long gone. CR 702.39a's provoke is the producer.
--
-- Two bare ObjectIds where the printed carrier holds an Affected, and the
-- difference is not a shortcut. CR 611.2c carves rules-modifying effects out of
-- the frozen-set rule -- which is exactly why the printed carrier stays dynamic
-- -- but rule 702.39a names its creature by TARGETING it (CR 115.1), and a
-- target is one object chosen once. There is no set here for the carve-out to
-- widen.
--
-- `expiry` decides when a Pawl.Engine.Expiry sweep drops it (CR 514.2, 611.2a,
-- 611.2b); provoke's "this combat" arms Expiry.AtEndOf CombatPhase.
--
-- `timestamp` is stored for ActivePlayerEffect's reason: CR 613.11 orders by
-- timestamp (CR 613.7), and nothing observes it yet because two requirements
-- cannot conflict -- CR 509.1c counts them and never resolves them against each
-- other.
--
-- No `controller`, where ActivePlayerEffect stores one: a requirement names no
-- player, so CR 109.5's "you" is never asked of it.
--
-- Runtime-only, like Expiry and ActivePlayerEffect: no codec, which keeps a
-- stored value out of a card file and a printed value out of the store.
data ActiveBlockRequirement = MkActiveBlockRequirement
  { source :: ObjectId.ObjectId,
    timestamp :: Timestamp.Timestamp,
    expiry :: Expiry.Expiry,
    -- | The creature that must block -- CR 509.1c's subject axis, which the
    -- printed carrier collapses to "all creatures" (#341).
    blocker :: ObjectId.ObjectId,
    -- | The attacking creature it must block.
    attacker :: ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
