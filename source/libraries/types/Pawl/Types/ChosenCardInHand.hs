module Pawl.Types.ChosenCardInHand where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | CR 402.3 with a choice: whose hand is looked in -- which is also who picks --
-- and what may be picked.

-- One field where Pawl.Types.ChosenCardInGraveyard needs two, because CR 402.3
-- collapses the chooser and the zone's owner onto one seat; the fields are named
-- rather than positional for that type's reason.
data ChosenCardInHand = MkChosenCardInHand
  { player :: PlayerRef.PlayerRef,
    filter :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
