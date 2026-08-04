module Pawl.Types.ActivePlayerEffect where

import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Timestamp as Timestamp

-- | CR 611.1 / 613.11: a stored, resolution-generated player or rules-modifying
-- continuous effect, held in GameState.playerEffects. The player-axis analogue of
-- ActiveReplacement and ContinuousEffect: the printed carrier
-- (Pawl.Types.PlayerStaticAbility) is re-derived live from the battlefield, while
-- these are stored because the object that made them may be long gone.
--
-- `controller` is STORED, where ContinuousEffect stores none, and has to be: a
-- stored Modification re-reads its source's PROJECTED controller (CR 613.1b),
-- which works only because the source is a permanent. Silence is an INSTANT, so
-- by the time its effect is live the source is in a graveyard with no controller
-- to project and "your opponents" would be unanswerable.
--
-- `scope`, by contrast, stays DYNAMIC. CR 611.2c freezes a stored effect's object
-- set but carves out this axis: a rules-modifying effect can affect objects that
-- were not affected when it began. There is no analogue of Affected.TheseObjects
-- here, and PlayerScope is the same type on both carriers.
--
-- `expiry` decides when a Pawl.Engine.Expiry sweep drops it (CR 514.2, 611.2a,
-- 611.2b). Only AtCleanup has a producer; no card arms While or AtTurnOf on this
-- carrier (#97).
--
-- `timestamp` is stored even though nothing observes it yet: CR 613.10 and 613.11
-- order by timestamp (CR 613.7), but no two of the current constructors conflict.
-- Stamping at creation is free; retrofitting an order onto stored effects is
-- not (#93).
--
-- Runtime-only, like Expiry and ActiveReplacement: no codec, which keeps a stored
-- value out of a card file and a printed value out of the store.
data ActivePlayerEffect = MkActivePlayerEffect
  { source :: ObjectId.ObjectId,
    controller :: PlayerId.PlayerId,
    timestamp :: Timestamp.Timestamp,
    expiry :: Expiry.Expiry,
    scope :: PlayerScope.PlayerScope,
    effect :: PlayerEffect.PlayerEffect
  }
  deriving (Eq, Ord, Show)
