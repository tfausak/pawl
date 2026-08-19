module Pawl.Types.Hybrid where

import qualified Pawl.Types.ManaType as ManaType

-- | CR 107.4e's hybrid symbol: the two ways one symbol may be paid.

-- BOTH fields are a ManaType, and the order is what a card prints -- @{W/U}@ and
-- @{U/W}@ are the same symbol to the rules (CR 202.2f) but not the same text --
-- so they are named rather than positional.
data Hybrid = MkHybrid
  { left :: ManaType.ManaType,
    right :: ManaType.ManaType
  }
  deriving (Eq, Ord, Show)
