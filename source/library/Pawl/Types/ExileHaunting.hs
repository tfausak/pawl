module Pawl.Types.ExileHaunting where

import qualified Pawl.Types.SlotName as SlotName

-- | CR 702.55's haunt: exile the card in one slot, haunting the object in the other.

-- BOTH fields are a SlotName and they are NOT interchangeable, so they are named
-- rather than positional: a card file that swapped them would otherwise exile the
-- wrong object and haunt the wrong one, with nothing to notice.
data ExileHaunting = MkExileHaunting
  { card :: SlotName.SlotName,
    host :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
