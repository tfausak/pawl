module Pawl.Types.AnyNumberMatching where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Quantity as Quantity

-- | CR 608.2d's plural choice: which objects may be picked, and at most how
-- many -- Teferi, Hero of Dominaria's "up to two lands". No ceiling is Tovolar's
-- "any number". The zone is the carrier's: Pawl.Types.ObjectRef's arm picks
-- permanents, Pawl.Types.AnyNumberDiscard picks cards in a hand.
data AnyNumberMatching = MkAnyNumberMatching
  { filter :: Filter.Filter Keyword.Keyword,
    atMost :: Maybe Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
