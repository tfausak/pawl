module Pawl.Types.AimedPlayers where

import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.SlotName as SlotName

-- | Pawl.Types.AimedAt's players: whose side of the board a resolution-generated
-- attack restriction bars (CR 508.1c).
data AimedPlayers
  = -- | CR 109.5, read against the stored controller at each declaration
    -- (Chronomantic Escape's "you").
    Scoped PlayerScope.PlayerScope
  | -- | Every player a slot names (Chaos Dragon's "those players"), baked into
    -- BoundPlayer rows as the restriction is stored.
    EachInSlot SlotName.SlotName
  | -- | EachInSlot's baked half, and runtime-only: one particular player.
    BoundPlayer PlayerId.PlayerId
  deriving (Eq, Ord, Show)
