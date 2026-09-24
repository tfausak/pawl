module Pawl.Types.FromReference where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Quantity as Quantity

-- | The cards of the Oracle card reference (CR 108.1) a conjure picks over --
-- Fear of Change\'s "a random creature card with mana value X". Card data the
-- card file does not write out: the pool is every card the reference holds, and
-- the filter narrows it.
data FromReference = MkFromReference
  { -- | Which of the reference\'s cards are candidates, matched against the
    -- PRINTED card since none of them is in the game (CR 400.11).
    filter :: Filter.Filter Keyword.Keyword,
    -- | The bound 'Pawl.Types.Filter.ManaValueEqualToAmount' reads -- Fear of
    -- Change\'s "where X is 2 plus the exiled creature\'s mana value". Nothing
    -- where the filter names no amount.
    amount :: Maybe Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
