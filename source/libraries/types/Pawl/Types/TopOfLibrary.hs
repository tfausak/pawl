module Pawl.Types.TopOfLibrary where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | CR 401.1: the top cards of a library, named by whose library and how many.
data TopOfLibrary = MkTopOfLibrary
  { player :: PlayerRef.PlayerRef,
    count :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
