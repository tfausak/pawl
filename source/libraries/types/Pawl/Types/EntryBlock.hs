module Pawl.Types.EntryBlock where

import qualified Pawl.Types.SlotName as SlotName

-- | What a creature put onto the battlefield blocking is blocking (CR 509.4).
data EntryBlock
  = -- | CR 509.4: its controller chooses as it enters.
    Chosen
  | -- | CR 509.4's parenthetical: the attacking creature the slot names.
    Specified SlotName.SlotName
  deriving (Eq, Ord, Show)
