module Pawl.Types.RandomCardInGraveyard where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.ZoneScope as ZoneScope

-- | CR 404.1 with no choice in it: whose graveyards randomness reaches, what it
-- may name there, and how many cards it names.

-- Pawl.Types.ChosenCardInGraveyard's record with the CHOOSER struck out, which is
-- the whole difference between them: randomness stands where that type's chooser
-- stands, so there is no seat to name and CR 608.2d has nobody to ask.
--
-- The ZoneScope survives that deletion where Pawl.Types.RandomCardInHand's single
-- PlayerRef does not, because CR 400.2 makes the graveyard a public zone: nothing
-- collapses the pile's owner onto the seat carrying the instruction, so a scope
-- naming somebody else's graveyard is as sayable as Ghoulraiser's "your".
--
-- The FILTER narrows the candidates handed to Prompt.RandomObject rather than
-- adding a roll -- Ghoulraiser's "a Zombie card at random" is one pick among the
-- Zombies, not a pick that may miss.
--
-- The COUNT is a Quantity for Pawl.Types.ChosenCardFromAmong's reason, is per
-- graveyard, and counts DISTINCT cards: Make a Wish's "two cards at random" names
-- two cards rather than making two picks that may coincide, so
-- Pawl.Engine.Resolve.Effect.randomCardsInGraveyard drops each card it names from
-- the candidates before asking again. CR 609.3 covers the shortfall -- a graveyard
-- holding fewer matches than the count gives what it has, and an empty one
-- nothing.
--
-- The fields are named rather than positional for ChosenCardInGraveyard's reason.
data RandomCardInGraveyard = MkRandomCardInGraveyard
  { players :: ZoneScope.ZoneScope,
    filter :: Filter.Filter Keyword.Keyword,
    count :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
