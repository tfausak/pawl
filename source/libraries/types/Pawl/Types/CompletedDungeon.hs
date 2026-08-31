module Pawl.Types.CompletedDungeon where

import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | The payload of Pawl.Types.Quantity's CompletedDungeon arm: has this player
-- completed a dungeon with this name (CR 309.7)?
--
-- Pawl.Types.PlayerCounterTally's shape, and NOT parametric for its reason: this
-- arm is a LEAF, holding no Quantity, so nothing here can close a cycle with
-- Quantity.
data CompletedDungeon = MkCompletedDungeon
  { player :: PlayerRef.PlayerRef,
    dungeon :: CardName.CardName
  }
  deriving (Eq, Ord, Show)
