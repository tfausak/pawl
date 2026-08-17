module Pawl.Types.DiscardCards where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter

-- | The payload of Pawl.Types.CostComponent's DiscardCards arm (#1620): CR
-- 601.2f's discard as a cost, discarding this many matching cards from the
-- paying player's hand.
--
-- PARAMETRIC in the keyword for the Filter it carries, exactly as
-- Pawl.Types.CostComponent is.
--
-- count is Pawl.Types.Sacrifice's reading and not
-- Pawl.Types.TapForTotalPower's: the cards are chosen and counted exactly.
--
-- The Filter admits everything for a cost that names no quality -- Cathartic
-- Reunion's "discard two cards" is @And []@ -- rather than the field being
-- optional. CR 701.9a's discard names a card either way, and one unconditional
-- reading keeps Pawl.Engine.Cost.discardCandidates from having two.
--
-- The Filter is matched against the card's CR 613 projection, which is the
-- CostComponent arm's note rather than this record's.
data DiscardCards keyword = MkDiscardCards
  { count :: Natural.Natural,
    whichCards :: Filter.Filter keyword
  }
  deriving (Eq, Ord, Show)
