module Pawl.Types.PlayerSacrifices where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity

-- | The payload of Pawl.Types.Effect's PlayerSacrifices arm (#1305): each player
-- the reference names sacrifices this many permanents matching the filter, in CR
-- 101.4's APNAP order -- every seat's pick is made before any of them is
-- sacrificed, which is that rule's own worked example.
data PlayerSacrifices = MkPlayerSacrifices
  { players :: PlayerRef.PlayerRef,
    filter :: Filter.Filter Keyword.Keyword,
    quantity :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
