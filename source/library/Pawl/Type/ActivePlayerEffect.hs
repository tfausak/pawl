module Pawl.Type.ActivePlayerEffect where

import Pawl.Type.Expiry (Expiry)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerEffect (PlayerEffect)
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.PlayerScope (PlayerScope)
import Pawl.Type.Timestamp (Timestamp)

-- CR 611.1 / 613.11: a stored, resolution-generated player or rules-modifying
-- continuous effect, held in GameState.playerEffects. The player-axis analogue of
-- ActiveReplacement and ContinuousEffect: the printed carrier
-- (Pawl.Type.PlayerStaticAbility) is re-derived live from the battlefield, while
-- these are stored because the object that made them may be long gone.
--
-- `controller` is STORED, where ContinuousEffect stores none. It has to be. A
-- stored Modification re-reads its source's PROJECTED controller (CR 613.1b),
-- which works because the source is a permanent; Silence is an INSTANT, so by the
-- time its effect is live the source is in a graveyard with no controller to
-- project and "your opponents" would be unanswerable. The controller is baked in
-- at creation -- the same treatment Expiry.While already gives CR 109.5's "you".
--
-- `scope`, by contrast, stays DYNAMIC. CR 611.2c's first sentence freezes a
-- stored effect's object set, but its third sentence carves out exactly this
-- axis: such an effect "modifies the rules of the game, so it can affect objects
-- that weren't affected when that continuous effect began". There is no
-- stored-set analogue of Affected.TheseObjects here, and PlayerScope is the same
-- type on both carriers.
--
-- `expiry` decides when a Pawl.Expiry sweep drops it (CR 514.2, 611.2a, 611.2b).
--
-- `timestamp` is stored even though nothing observes it yet: CR 613.10 and 613.11
-- both order by timestamp (CR 613.7), and no two of P7's constructors conflict,
-- so the order is unobservable in this pool. Stamping at creation is free;
-- retrofitting an order onto effects already stored is not (#N).
--
-- Runtime-only, like Expiry and ActiveReplacement: it never appears in card JSON
-- and has no codec, which is what keeps a stored value out of a card file and a
-- printed value out of the store.
data ActivePlayerEffect = MkActivePlayerEffect
  { source :: ObjectId,
    controller :: PlayerId,
    timestamp :: Timestamp,
    expiry :: Expiry,
    scope :: PlayerScope,
    effect :: PlayerEffect
  }
  deriving (Eq, Ord, Show)
