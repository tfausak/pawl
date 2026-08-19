module Pawl.Types.PlayerSacrifices where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's PlayerSacrifices arm (#1305): the players
-- the slot names each sacrifice this many permanents matching the filter.
data PlayerSacrifices = MkPlayerSacrifices
  { slot :: SlotName.SlotName,
    filter :: Filter.Filter Keyword.Keyword,
    quantity :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
