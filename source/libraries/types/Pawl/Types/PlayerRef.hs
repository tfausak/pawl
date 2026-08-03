module Pawl.Types.PlayerRef where

import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

-- | CR 400.1: "each player has their own library, hand, and graveyard. The
-- other zones are shared by all players." This says WHOSE zone a scope folds
-- over.
--
-- And whose MANA POOL: CR 106.4 attaches a pool to a player exactly as CR 400.1
-- attaches a library, so Pawl.Types.ManaCount names its player with this same
-- type and Pawl.Engine.Count.playersFor resolves both.
--
-- Deliberately NOT Pawl.Types.PlayerScope, which is You | Opponents | EachPlayer
-- and looks like the same type. PlayerScope is resolved against a perspective and
-- nothing else (Pawl.Engine.PlayerEffect.inScope); this can also name a binding slot,
-- which PlayerEffect has no way to answer. Adding InSlot there would give
-- Pawl.Types.PlayerEffect a constructor its evaluator cannot resolve.
--
-- The split cuts the other way too, and Pawl.Types.Pool.CardsInGraveyard is where:
-- a TARGET pool folds over graveyards while CR 601.2c is still choosing the
-- targets, so InSlot would read a slot nothing has bound yet. That pool takes a
-- PlayerScope for exactly the reason a PlayerEffect does -- there is no slot to
-- name -- which is why neither type subsumes the other.
data PlayerRef
  = -- | Every player's copy of the zone. For a SHARED zone (CR 400.1: battlefield,
    -- stack, exile, command) this is the only meaningful value; the pairing is
    -- checked by the card lint, not by this type (#161).
    EachPlayer
  | -- | CR 109.5 / 102.2, resolved against the evaluation context's perspective.
    Relative PlayerRelation.PlayerRelation
  | -- | The player bound in a slot -- Sudden Impact's "that player's hand", where
    -- the slot was filled by targeting (CR 601.2c).
    InSlot SlotName.SlotName
  deriving (Eq, Ord, Show)
