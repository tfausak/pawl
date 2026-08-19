module Pawl.Types.PermanentBecomesDesignated where

import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | CR 603.2 over a CR 701's designation: which designation is gained, and which
-- permanents gaining it fire the ability.
data PermanentBecomesDesignated = MkPermanentBecomesDesignated
  { designation :: Designation.Designation,
    filter :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
