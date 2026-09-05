module Pawl.Types.RandomCardInHand where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity

-- | CR 701.20a over CR 402.3 with no choice in it: whose hand randomness reaches,
-- what it may name there, and how many cards it names.

-- ONE PlayerRef, Pawl.Types.ChosenCardInHand's reason -- CR 402.3 collapses the
-- zone's owner and the seat carrying the instruction out onto one player -- with
-- randomness standing where that type's chooser stands (CR 701.9b).
--
-- The FILTER narrows the candidates handed to Prompt.RandomObject rather than
-- adding a roll, so it changes what is asked and not who answers. Nothing in CR
-- 701.20a or CR 402.3 forbids narrowing a pick out of a hidden zone, which is the
-- argument ChosenCardInHand already makes over the same zone.
--
-- The COUNT is a Quantity for Pawl.Types.ChosenCardFromAmong's reason, and it is
-- a count of DISTINCT cards: the printed "two cards at random" (Fall) names two
-- cards rather than making two picks that may coincide, so
-- Pawl.Engine.Resolve.Effect's reveal arm drops each card it names from the
-- candidates before asking again. CR 609.3 covers the shortfall -- a hand holding
-- fewer matches than the count gives what it has, and an empty one nothing.
--
-- The fields are named rather than positional for ChosenCardInHand's reason.
data RandomCardInHand = MkRandomCardInHand
  { player :: PlayerRef.PlayerRef,
    filter :: Filter.Filter Keyword.Keyword,
    count :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
