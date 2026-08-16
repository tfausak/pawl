module Pawl.Types.ContinuousEffect where

import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Timestamp as Timestamp

-- | A stored continuous effect (CR 611.2), held in GameState.continuousEffects.
-- `timestamp` orders it within its layer (CR 613.7); `expiry` decides when a
-- sweep drops it (Pawl.Engine.Expiry; CR 514.2, 611.2a, 611.2b); `affected` is
-- its fixed set (CR 611.2c). Static-ability effects are NOT stored here -- they
-- are re-derived from Face.staticAbilities each projection.
--
-- Made by a spell or ability RESOLVING, all but one of them. The exception is
-- the one clause that turns a static ability's effect into a stored one:
-- StaticAbility.lingers, Titania's Song's "if this enchantment leaves the
-- battlefield, this effect continues until end of turn", which
-- Pawl.Engine.Event hands over as the permanent goes. Such an effect keeps the
-- timestamp CR 613.7a gave it rather than taking a fresh one, since the card
-- says it is the same effect continuing.
--
-- Parametric in `card` for Pawl.Types.StaticAbility's reason, and instantiated at
-- the SAME width: the lingers clause hands a static ability's own modification
-- over here, so this position has to hold a granted ability. What a RESOLUTION
-- stores arrives narrower (Pawl.Types.ModifyTarget) and is widened on the way in.
data ContinuousEffect card = MkContinuousEffect
  { source :: ObjectId.ObjectId,
    timestamp :: Timestamp.Timestamp,
    expiry :: Expiry.Expiry,
    modification :: Modification.Modification (ActivatedAbility.ActivatedAbility card),
    affected :: Affected.Affected
  }
  deriving (Eq, Ord, Show)
