module Pawl.Types.SpeedDecrease where

import Numeric.Natural (Natural)
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity

-- | CR 702.179: "reduce that opponent's speed by 1. This effect can't reduce
-- their speed below 1" -- Spikeshell Harrier, the payload of
-- Pawl.Types.Effect.DecreaseSpeed.
--
-- A record of its own rather than a Pawl.Types.PlayerQuantity, on that type's
-- own stated rule: the floor is a field none of its seven sharers has, and
-- bolting a nullable one on would make a field's absence the tag again.
-- Pawl.Types.Mill is the precedent for spinning out instead.
data SpeedDecrease = MkSpeedDecrease
  { -- | Whose speed drops. PlayerRef.ControllerOfBound is what the one producer
    -- writes -- "that opponent" is the player whose permanent the ability
    -- targeted, and CR 608.2h is what still answers once the same ability has
    -- bounced it.
    player :: PlayerRef.PlayerRef,
    -- | By how much. A Quantity like every other printed amount, though the one
    -- producer prints a literal 1.
    quantity :: Quantity.Quantity,
    -- | The lowest speed this effect may leave -- Spikeshell Harrier's "can't
    -- reduce their speed below 1". The CARD's own clause and not a rule: nothing
    -- in rule 702.179 bounds speed from below, so a card that printed no such
    -- sentence writes 0 here and the type's own Natural is the only floor left.
    --
    -- A Natural rather than a Quantity, which is what every other printed amount
    -- is: a floor is a bound the card states outright, and no printing computes
    -- one. Widen it if one ever does.
    --
    -- Shadows the Prelude's `floor`, deliberately and for the reason
    -- Pawl.Types.Count's `filter` does: the wire format's key is the field name,
    -- and the printed word is the right one.
    floor :: Natural
  }
  deriving (Eq, Ord, Show)
