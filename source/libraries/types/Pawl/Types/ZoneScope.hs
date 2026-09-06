module Pawl.Types.ZoneScope where

import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.SlotName as SlotName

-- | CR 400.1's WHOSE, for the zones that rule gives each player their own of:
-- which graveyards Pawl.Types.Pool.CardsInGraveyard draws from and
-- Pawl.Types.ObjectRef.EachCardInGraveyard sweeps, which hands
-- Pawl.Types.ObjectRef.EachCardInHand sweeps, and which graveyards
-- Pawl.Types.ObjectRef.ChosenCardInGraveyard offers candidates out of. Neither
-- arm names a zone, which is what lets one type answer for all of them.
--
-- Its own type rather than Pawl.Types.PlayerScope, which the pool used to carry
-- directly: that type is shared with two Pawl.Types.PlayerEffect carriers whose
-- evaluator has no slots to resolve, so an InSlot arm there would be an arm those
-- carriers could not answer. And not Pawl.Types.PlayerRef, whose Relative arm
-- cannot say CR 806.1's "each opponent" as one value. Neither of those types
-- subsumes this one; these are the readings a TARGET POOL, a
-- resolution-time SWEEP and a resolution-time CHOICE all need, and all three
-- resolve it through Pawl.Engine.Target.zoneScopePlayers.
data ZoneScope
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
    -- caller has; see Pawl.Engine.Target.zoneScopePlayers.
    --
    -- The PRINTED slot name, which a REPEATED mode's later occurrences rename
    -- alongside the slot keys (Pawl.Engine.Modal.instanceTargetSlots), so the
    -- payload names that occurrence's own slot (CR 700.2d).
    InSlot SlotName.SlotName
  | -- | CR 108.4 / CR 608.2h: the controller of the OBJECT a slot names -- Hour
    -- of Glory's "its controller reveals their hand", where the slot holds the
    -- creature the spell targeted rather than a player. Read through last known
    -- information, since the clause naming the player generally moved the object
    -- first and CR 108.4 leaves a card that is neither permanent nor spell with
    -- no controller at all; Pawl.Types.PlayerRef.ControllerOfBound is the same
    -- read one type over.
    ControllerOfBound SlotName.SlotName
  deriving (Eq, Ord, Show)
