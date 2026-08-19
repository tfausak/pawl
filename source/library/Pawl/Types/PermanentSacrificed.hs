module Pawl.Types.PermanentSacrificed where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 701.21a: a permanent was sacrificed, and by whom.
data PermanentSacrificed = MkPermanentSacrificed
  { player :: PlayerId.PlayerId,
    permanent :: ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
