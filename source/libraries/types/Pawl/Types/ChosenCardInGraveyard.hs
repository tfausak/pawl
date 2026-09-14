module Pawl.Types.ChosenCardInGraveyard where

import qualified Pawl.Types.Chooser as Chooser
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.ZoneScope as ZoneScope

-- | CR 404.1 with a choice: who picks, whose graveyards are looked at, what
-- may be picked, and HOW MANY.

-- The Chooser and the ZoneScope are different questions and are easy to
-- confuse -- "target opponent chooses a card in YOUR graveyard" names two
-- different seats -- so they are named rather than positional.
--
-- The COUNT is a Quantity rather than a Natural, Pawl.Types.ChosenCardFromAmong's
-- reason, and is per CHOOSER: Fall of the Thran's "each player returns two land
-- cards from their graveyard" is two each. It names DISTINCT cards, so each card
-- picked is dropped from the candidates before the next ask (CR 608.2d). CR 609.3
-- covers the shortfall -- a graveyard holding fewer matches than the count gives
-- what it has.
data ChosenCardInGraveyard = MkChosenCardInGraveyard
  { chooser :: Chooser.Chooser,
    players :: ZoneScope.ZoneScope,
    filter :: Filter.Filter Keyword.Keyword,
    count :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
