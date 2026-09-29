module Pawl.Types.ChooseNumber where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's ChooseNumber arm: where the number is
-- bound, and the most it may be -- Trade Secrets' "draw up to four cards"
-- beside Rites of Initiation's unbounded "any number" (CR 107.1c).
data ChooseNumber = MkChooseNumber
  { slot :: SlotName.SlotName,
    upTo :: Maybe Natural.Natural
  }
  deriving (Eq, Ord, Show)
