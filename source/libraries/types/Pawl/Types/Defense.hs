module Pawl.Types.Defense where

import qualified Numeric.Natural as Natural

-- | CR 310.4: defense is a characteristic only battles have. CR 109.3 lists it
-- among an object's characteristics and CR 707.2 makes it a copiable value, which
-- is why it is projected rather than read off the printed card.
--
-- This type carries CR 310.4a's PRINTED number -- the one in the card's lower
-- right corner, which is what a battle card outside the battlefield has. CR 310.4c
-- gives a battle on the battlefield the number of defense counters on it instead,
-- which is a count in Object.counters and never a value of this type.
--
-- The same shape as Pawl.Types.Loyalty, and for the same reasons: Natural rather
-- than Quantity, because Power and Toughness wrap Quantity for CR 208.2's star and
-- the CR 208.2a ability behind it, and no rule gives defense one. Unlike loyalty
-- there is not even a printed-X printing to defer -- every battle printed so far
-- prints a literal.
newtype Defense = MkDefense
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
