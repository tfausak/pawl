module Pawl.Types.LibraryArrival where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 401.4: one move a replacement redirected to an end of a library during an
-- Event.simultaneously bracket, held for its owner to arrange as the bracket
-- ends (Pawl.Engine.Event.arrangeLibraryArrivals).
data LibraryArrival = MkLibraryArrival
  { -- | The library's owner (CR 400.3), who arranges it.
    owner :: PlayerId.PlayerId,
    -- | The end it arrived at (CR 401.2).
    position :: LibraryPosition.LibraryPosition,
    -- | The cards the move placed: one, or a melded permanent's components
    -- (CR 712.21a).
    cards :: Seq.Seq ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
