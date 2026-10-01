module Pawl.Types.KeywordsAre where

import Numeric.Natural (Natural)
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Reference as Reference

-- | CR 702.1: how many instances of one keyword an object has.
data KeywordsAre = MkKeywordsAre
  { object :: Reference.Reference,
    keyword :: Keyword.Keyword,
    count :: Natural
  }
  deriving (Eq, Ord, Show)
