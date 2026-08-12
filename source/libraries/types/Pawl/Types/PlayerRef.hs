module Pawl.Types.PlayerRef where

import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

-- | CR 400.1: whose zone a scope folds over, per-player zones being a player's
-- own. And whose MANA POOL, since CR 106.4 attaches a pool to a player exactly as
-- CR 400.1 attaches a library, so Pawl.Types.ManaCount uses this type too and
-- Pawl.Engine.Count.playersFor resolves both.
--
-- Deliberately NOT Pawl.Types.PlayerScope, which looks like the same type.
-- PlayerScope is resolved against a perspective and nothing else; this can also
-- name a binding slot, which PlayerEffect's evaluator has no way to resolve. A
-- target pool needs BOTH readings and neither type whole, which is what
-- Pawl.Types.GraveyardScope is: this type's Relative arm cannot say CR 806.1's
-- "each opponent" as one value, and PlayerScope has no slot.
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
  | -- | InSlot's BAKED half, and runtime-only: one particular player, named
    -- outright. Pawl.Engine.Condition.bakeBound substitutes it for an InSlot as a
    -- CR 611.2b duration begins, so a "for as long as" condition naming the
    -- player a trigger's event bound (Garland, Royal Kidnapper's "for as long as
    -- they're the monarch") still answers once the resolution that stored it is
    -- over and its bindings are unreachable. Filter.ControlledByPlayer is the same
    -- move one type over, and Modification.SetController's baked controller is the
    -- older precedent.
    --
    -- NO CARD MAY WRITE IT -- only a resolution knows a PlayerId -- which the
    -- codec cannot enforce (it is total both ways, since an Expiry serialises
    -- through Pawl.Codec.Condition) and Pawl.CardSpec's pool sweep does.
    Specific PlayerId.PlayerId
  deriving (Eq, Ord, Show)
