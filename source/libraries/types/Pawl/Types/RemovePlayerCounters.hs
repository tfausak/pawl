module Pawl.Types.RemovePlayerCounters where

import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | CR 122: "these players lose this many of this kind" -- Pawl.Types.Effect's
-- RemovePlayerCounters arm. Spun out of Pawl.Types.PlayerCounters, which
-- GainPlayerCounters keeps, when the removal alone needed a tally.
data RemovePlayerCounters = MkRemovePlayerCounters
  { player :: PlayerRef.PlayerRef,
    kind :: PlayerCounterKind.PlayerCounterKind,
    quantity :: Quantity.Quantity,
    -- | Where to bind how many counters the player lost, for a later effect's
    -- "that much" (Leeches) -- Pawl.Types.RemoveCounters' tally, on a player.
    tally :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
