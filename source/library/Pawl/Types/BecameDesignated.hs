module Pawl.Types.BecameDesignated where

import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 701's designations: which one a permanent gained, and which permanent.
data BecameDesignated = MkBecameDesignated
  { designation :: Designation.Designation,
    object :: ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
