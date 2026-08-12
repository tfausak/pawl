module Pawl.Types.ActivePlayerEffect where

import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerId as PlayerId
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
-- `scope`'s Scoped arm, by contrast, stays DYNAMIC. CR 611.2c freezes a stored
-- effect's object set but carves out this axis: a rules-modifying effect can
-- affect objects that were not affected when it began. There is no analogue of
-- Affected.TheseObjects here, and PlayerScope is the same type on both carriers.
--
-- The Named arm is the half the printed carrier cannot have: the seat the ability
-- TARGETED (CR 601.2c), baked by Resolve as the effect begins because the
-- bindings that answered the slot are gone once the resolution is over. That is
-- the same move Expiry.While makes with Condition.bakeBound, and it is not a
-- freeze of the CR 611.2c kind -- see Pawl.Types.AffectedPlayers.
--
-- `expiry` decides when a Pawl.Engine.Expiry sweep drops it (CR 514.2, 611.2a,
-- 611.2b). AtCleanup has a producer (Silence) and so does Never (Sea Gate
-- Restoration's "for the rest of the game"); no card arms While or AtTurnOf on
-- this carrier (#97).
--
-- `timestamp` is CR 613.7b's stamp, taken as the effect began, and it is READ:
-- Pawl.Engine.PlayerEffect.applying merges these rows into the printed carrier's
-- by it, so CR 613.10's and CR 613.11's timestamp order holds across the two
-- carriers rather than within each (Pawl.PlayerEffectSpec's Sea Gate Restoration
-- against The Ten Rings is the proof).
--
-- Runtime-only, like ActiveReplacement: no codec, which keeps a stored value out
-- of a card file and a printed value out of the store. NOT like Expiry, which
-- does have one (Pawl.Codec.Expiry) because a Duration's condition serialises
-- through it -- the claim this comment used to make (#1059).
data ActivePlayerEffect = MkActivePlayerEffect
  { source :: ObjectId.ObjectId,
    controller :: PlayerId.PlayerId,
    timestamp :: Timestamp.Timestamp,
    expiry :: Expiry.Expiry,
    scope :: AffectedPlayers.AffectedPlayers PlayerId.PlayerId,
    effect :: PlayerEffect.PlayerEffect
  }
  deriving (Eq, Ord, Show)
