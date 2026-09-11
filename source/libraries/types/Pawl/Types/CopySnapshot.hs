module Pawl.Types.CopySnapshot where

import qualified Pawl.Types.ProjectedCharacteristics as ProjectedCharacteristics

-- | CR 707.2-3: the normal copiable values and the optional alternative values
-- selected when the recipient permanent's own flipped status is set.
data CopySnapshot = MkCopySnapshot
  { normal :: ProjectedCharacteristics.ProjectedCharacteristics,
    flipped :: Maybe ProjectedCharacteristics.ProjectedCharacteristics
  }
  deriving (Eq, Ord, Show)
