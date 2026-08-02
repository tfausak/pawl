module Pawl.Types.LastKnown where

import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Types.Source as Source

-- | CR 608.2h: what an object WAS, filed under the id it had while it existed --
-- "if it's no longer in that zone ... the effect uses the object's last known
-- information". Written by the one zone-change funnel
-- (Pawl.Engine.Event.changeZoneAttaching) as the object ceases, from the same
-- pre-move state the GameEvent.Moved snapshot is taken against.
--
-- A record of THREE things rather than the characteristics alone, because two of
-- the questions CR 608.2h is asked have no home in ProjectedCharacteristics.
--
-- CR 613.1b control is not a characteristic: CR 109.3 lists an object's
-- characteristics and then says outright that "characteristics don't include ...
-- an object's owner or controller" -- while "who controlled it" is exactly what
-- CR 603.3a asks of a triggered ability whose source is already gone.
--
-- Neither is the object's SOURCE -- what kind of object it was, and the card or
-- token text behind it. The projection is a fold over characteristics and CR
-- 603.7's delayed-ability declarations are not among them (they are read
-- straight from the card; see Pawl.Types.Card.delayedAbilities), so an
-- ArmDelayedTrigger whose source has just exiled itself -- Meandering
-- Towershell's, which does exactly that one opcode earlier -- has nowhere else
-- to find the ability it names. Pawl.Engine.Game.cardOfWithLastKnown is the
-- reader.
--
-- All three fields STRICT (!), for GameEvent.Moved's reason: entries are keyed by an
-- id that no longer exists and are never pruned, so an unforced field would be a
-- thunk closing over the whole pre-move GameState, retained for the rest of the
-- game instead of the one small value this record is meant to carry.
data LastKnown = MkLastKnown
  { characteristics :: !ProjectedCharacteristics.ProjectedCharacteristics,
    -- | CR 110.2 / 613.1b: the PROJECTED controller as the object left, which is
    -- not its owner -- "a permanent's controller is, by default, the player
    -- under whose control it entered the battlefield", and layer 2 can move it
    -- since. A permanent stolen by Control Magic was controlled by the thief
    -- right up to the moment it died, and CR 603.3a hands that player its
    -- trigger.
    controller :: !PlayerId.PlayerId,
    -- | CR 608.2h: what KIND of object it was and the card behind it -- the same
    -- Object.source the live object carried, copied as it ceased. Strict for the
    -- reason the other two are.
    source :: !Source.Source
  }
  deriving (Eq, Show)
