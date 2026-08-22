module Pawl.Types.ActiveUnregeneratable where

import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Timestamp as Timestamp

-- | CR 701.19c / 611.1: a stored, resolution-generated REGENERATION
-- PROHIBITION, held in GameState.unregeneratables. Hurr Jackal's "target
-- creature can't be regenerated this turn" is the producer.
--
-- Read at Pawl.Engine.Event.resolveDestruction, the single site that mints a
-- WouldBeDestroyed: a standing row over the object turns that event's
-- Pawl.Types.Regenerability into CantBeRegenerated before any replacement is
-- offered. So Pawl.Types.Regenerability stays a property of the DESTRUCTION and
-- this is what converts a lasting prohibition into one, which is CR 701.19c's
-- own reading -- the shield is still created, it is simply never applied.
--
-- A bare ObjectId where the printed carrier (Pawl.Types.CantBeRegenerated) holds
-- an ObjectRef, for Pawl.Types.ActiveBlockRequirement's reason: the ref is read
-- ONCE, as the ability resolves, and the objects it named are what the
-- prohibition covers thereafter.
--
-- `expiry` decides when a Pawl.Engine.Expiry sweep drops it (CR 514.2, 611.2a,
-- 611.2b); "this turn" arms Expiry.AtCleanup.
--
-- `timestamp` is stored for ActiveBlockRequirement's reason: CR 613.7 timestamps
-- every continuous effect, and nothing observes this one because two
-- prohibitions cannot conflict -- CR 701.19c has no degrees.
--
-- No `controller`, where ActivePlayerEffect stores one: a prohibition names no
-- player, so CR 109.5's "you" is never asked of it.
--
-- Runtime-only, like Expiry and ActiveBlockRequirement: no codec, which keeps a
-- stored value out of a card file and a printed value out of the store.
data ActiveUnregeneratable = MkActiveUnregeneratable
  { source :: ObjectId.ObjectId,
    timestamp :: Timestamp.Timestamp,
    expiry :: Expiry.Expiry,
    -- | The permanent that can't be regenerated.
    object :: ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
