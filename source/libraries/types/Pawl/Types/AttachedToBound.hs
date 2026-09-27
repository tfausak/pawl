module Pawl.Types.AttachedToBound where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.SlotName as SlotName

-- | CR 303.4b / 608.2h: the permanents attached to the object `slot` holds --
-- or, once it has left, those attached as it left -- that `filter` admits (Rhuk,
-- Hexgold Nabber's "all Equipment attached to that creature", Fumble's "that
-- were attached to it").
data AttachedToBound = MkAttachedToBound
  { slot :: SlotName.SlotName,
    filter :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
