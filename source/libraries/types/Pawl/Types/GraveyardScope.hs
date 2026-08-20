module Pawl.Types.GraveyardScope where

import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.SlotName as SlotName

-- | WHOSE graveyards Pawl.Types.Pool.CardsInGraveyard draws from, and which ones
-- Pawl.Types.ObjectRef.EachCardInGraveyard sweeps. CR 400.1 gives each player
-- their own graveyard, so anything naming that zone has to say whose, and there
-- are two ways a card says it.
--
-- Its own type rather than Pawl.Types.PlayerScope, which the pool used to carry
-- directly: that type is shared with two Pawl.Types.PlayerEffect carriers whose
-- evaluator has no slots to resolve, so an InSlot arm there would be an arm those
-- carriers could not answer. And not Pawl.Types.PlayerRef, whose Relative arm
-- cannot say CR 806.1's "each opponent" as one value. Neither of those types
-- subsumes this one; this is the pair of readings both a TARGET POOL and a
-- resolution-time SWEEP need, and both resolve it through
-- Pawl.Engine.Target.graveyardScopePlayers.
data GraveyardScope
  = -- | CR 109.5, resolved against the reading caller's perspective -- Raise
    -- Dead's "your graveyard", Withered Wretch's "a graveyard".
    Scoped PlayerScope.PlayerScope
  | -- | Dwell on the Past's "their graveyard" and Angel of Finality's "target
    -- player's graveyard": the graveyards of the players another target slot
    -- names. CR 601.2c announces every target AT ONCE, so at the moment a target
    -- pool is read the named slot holds not one player but the players it could
    -- still be answered with -- and at CR 608.2b's re-check, the one it was. A
    -- SWEEP has only the second moment, since it is read as the effect executes
    -- (CR 608.2c). All of them are the same lookup against whatever bindings the
    -- caller has; see Pawl.Engine.Target.graveyardScopePlayers.
    --
    -- The PRINTED slot name, which a REPEATED mode's later occurrences rename
    -- (Pawl.Engine.Modal.instanceSlot) and this payload does not follow (#1297).
    InSlot SlotName.SlotName
  deriving (Eq, Ord, Show)
