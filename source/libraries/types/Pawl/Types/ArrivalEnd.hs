module Pawl.Types.ArrivalEnd where

import qualified Pawl.Types.LibraryPosition as LibraryPosition

-- | Where in its owner's pile a card put there arrives.
data ArrivalEnd
  = -- | CR 401.2: a stated end of a library.
    IntoLibrary LibraryPosition.LibraryPosition
  | -- | CR 404.1: the top of a graveyard.
    OntoGraveyard
  deriving (Eq, Ord, Show)
