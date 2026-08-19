module Pawl.Types.Discard where

import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | CR 701.8: the players the slot names each discard this many cards.
data Discard = MkDiscard
  { slot :: SlotName.SlotName,
    quantity :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
