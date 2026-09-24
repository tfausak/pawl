module Pawl.Types.DuplicateCard where

import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.ProjectedCharacteristics as ProjectedCharacteristics

-- | CR 108.2 / 707.2: a conjured duplicate as a merge component -- the card it
-- is, plus the copiable values it was conjured with (Pawl.Types.Object's
-- `duplicate`), which CR 730.3's split hands back to that card alone.
data DuplicateCard = MkDuplicateCard
  { printing :: PrintingId.PrintingId,
    values :: ProjectedCharacteristics.ProjectedCharacteristics
  }
  deriving (Eq, Ord, Show)
