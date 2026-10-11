module Pawl.Types.AnyNumberMatching where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Quantity as Quantity

-- | CR 608.2d's plural choice of cards in a hand: which may be picked, and at
-- most how many. No ceiling is "any number". Pawl.Types.AnyNumberDiscard's;
-- the battlefield's is Pawl.Types.ChosenPermanents.
data AnyNumberMatching = MkAnyNumberMatching
  { filter :: Filter.Filter Keyword.Keyword,
    atMost :: Maybe Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
