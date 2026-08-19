module Pawl.Types.ExileCardsFromGraveyard where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter

-- | The payload of Pawl.Types.CostComponent's ExileCardsFromGraveyard arm
-- (#1305): CR 406.2 as a cost, exiling this many matching cards from the paying
-- player's graveyard.
--
-- PARAMETRIC in the keyword for the Filter it carries, exactly as
-- Pawl.Types.CostComponent is.
--
-- count is Pawl.Types.Sacrifice's reading and not
-- Pawl.Types.TapForTotalPower's: the cards are chosen and counted exactly. The
-- Filter is matched against the card's CR 613 projection, which is the
-- CostComponent arm's note rather than this record's.
data ExileCardsFromGraveyard keyword = MkExileCardsFromGraveyard
  { count :: Natural.Natural,
    whichCards :: Filter.Filter keyword
  }
  deriving (Eq, Ord, Show)
