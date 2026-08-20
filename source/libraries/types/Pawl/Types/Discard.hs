module Pawl.Types.Discard where

import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | CR 701.9a: the ONE player the slot names discards this many cards.
--
-- Not implemented: a slot naming several players discards nothing, so "each
-- player discards a card" has no spelling (#1965). Its sibling
-- Pawl.Types.PlayerSacrifices does fold over every seat the slot holds.
data Discard = MkDiscard
  { slot :: SlotName.SlotName,
    quantity :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
