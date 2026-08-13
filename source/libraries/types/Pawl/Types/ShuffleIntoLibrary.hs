module Pawl.Types.ShuffleIntoLibrary where

import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | CR 701.24: shuffle the objects an ObjectRef names into a library, and shuffle
-- that library whether or not the objects arrived.
data ShuffleIntoLibrary = MkShuffleIntoLibrary
  { -- | NAMES the library, which CR 701.24c needs: an owner read off the objects
    -- disappears with them. Absent when the card's own words derive it instead,
    -- as Riftsweeper's "its owner" does, so the key is elided in that case.
    library :: Maybe PlayerRef.PlayerRef,
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
