module Pawl.Types.Times where

import qualified Numeric.Natural as Natural

-- | The payload of Pawl.Types.Quantity's Times arm: a printed literal times a
-- quantity, which is how "you gain 3 life for each creature attacking you"
-- reads. CR 107.1 is why no rounding word rides here as it does on
-- Pawl.Types.Halved: a whole number of a whole number is a whole number.
--
-- PARAMETRIC in the quantity for Pawl.Types.Halved's reason: the inner value is
-- a whole Quantity and Quantity names this record.
--
-- The factor is a Natural and never an Integer, because the minus sign a card
-- prints is Quantity's Negate and CR 107.1b is that arm's rule; a zero factor is
-- legal, since rule 702.23a's rampage 0 is one.
data Times quantity = MkTimes
  { factor :: Natural.Natural,
    quantity :: quantity
  }
  deriving (Eq, Ord, Show)
