module Pawl.Types.NameInsertion where

import qualified Data.Text as Text
import qualified Numeric.Natural as Natural

-- | CR 123.6b-c: a name sticker's effect, its word after this many words.
data NameInsertion = MkNameInsertion
  { word :: Text.Text,
    after :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
