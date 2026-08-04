module Pawl.Types.PlayerRef where

import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

-- | CR 400.1: whose zone a scope folds over, per-player zones being a player's
-- own. And whose MANA POOL, since CR 106.4 attaches a pool to a player exactly as
-- CR 400.1 attaches a library, so Pawl.Types.ManaCount uses this type too and
-- Pawl.Engine.Count.playersFor resolves both.
--
-- Deliberately NOT Pawl.Types.PlayerScope, which looks like the same type.
-- PlayerScope is resolved against a perspective and nothing else; this can also
-- name a binding slot, which PlayerEffect's evaluator has no way to resolve. The
-- split cuts the other way at Pawl.Types.Pool.CardsInGraveyard: a target pool
-- folds over graveyards while CR 601.2c is still choosing targets, so InSlot
-- would read a slot nothing has bound yet. Neither type subsumes the other.
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
