module Pawl.Types.GraveyardScope where

import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.SlotName as SlotName

-- | WHOSE graveyards Pawl.Types.Pool.CardsInGraveyard draws from. CR 400.1 gives
-- each player their own graveyard, so a pool naming that zone has to say whose,
-- and there are two ways a card says it.
--
-- Its own type rather than Pawl.Types.PlayerScope, which the pool used to carry
-- directly: that type is shared with two Pawl.Types.PlayerEffect carriers whose
-- evaluator has no slots to resolve, so an InSlot arm there would be an arm those
-- carriers could not answer. And not Pawl.Types.PlayerRef, whose Relative arm
-- cannot say CR 806.1's "each opponent" as one value. Neither of those types
-- subsumes this one; this is the pair of readings a TARGET POOL needs.
data GraveyardScope
  = -- | CR 109.5, resolved against the perspective the pool is read for -- Raise
    -- Dead's "your graveyard", Withered Wretch's "a graveyard".
    Scoped PlayerScope.PlayerScope
  | -- | Dwell on the Past's "their graveyard": the graveyards of the players
    -- another target slot names. CR 601.2c announces every target AT ONCE, so at
    -- the moment this pool is read the named slot holds not one player but the
    -- players it could still be answered with -- and at CR 608.2b's re-check,
    -- the one it was. Both are the same lookup against whatever bindings the
    -- caller has; see Pawl.Engine.Target.graveyardRecipients.
    --
    -- The PRINTED slot name, which a REPEATED mode's later occurrences rename
    -- (Pawl.Engine.Modal.instanceSlot) and this payload does not follow (#1297).
    InSlot SlotName.SlotName
  deriving (Eq, Ord, Show)
