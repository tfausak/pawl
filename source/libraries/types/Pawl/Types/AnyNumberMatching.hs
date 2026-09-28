module Pawl.Types.AnyNumberMatching where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Quantity as Quantity

-- | CR 608.2d's plural battlefield choice: which permanents may be picked, and
-- at most how many -- Teferi, Hero of Dominaria's "up to two lands". No ceiling
-- is Tovolar's "any number".
data AnyNumberMatching = MkAnyNumberMatching
  { filter :: Filter.Filter Keyword.Keyword,
    atMost :: Maybe Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
