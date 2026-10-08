module Pawl.Types.CounteredEnd where

import qualified Pawl.Types.LibraryPosition as LibraryPosition

-- | Which end of its owner's library a countered card goes to, where a
-- Pawl.Types.CounterDestination sends it there (CR 401.2).
data CounteredEnd
  = -- | The end the card states -- Memory Lapse's "on top".
    Stated LibraryPosition.LibraryPosition
  | -- | The countering spell's controller chooses -- Hinder's "your choice of
    -- the top or bottom".
    CounteringPlayerChooses
  deriving (Eq, Ord, Show)

defaultValue :: CounteredEnd
defaultValue = Stated LibraryPosition.defaultValue
