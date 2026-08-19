module Pawl.Types.ChosenCardInGraveyard where

import qualified Pawl.Types.Chooser as Chooser
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerScope as PlayerScope

-- | CR 404.1 with a choice: who picks, whose graveyards are looked at, and what
-- may be picked.

-- The Chooser and the PlayerScope are different questions and are easy to
-- confuse -- "target opponent chooses a card in YOUR graveyard" names two
-- different seats -- so they are named rather than positional.
data ChosenCardInGraveyard = MkChosenCardInGraveyard
  { chooser :: Chooser.Chooser,
    players :: PlayerScope.PlayerScope,
    filter :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
