module Pawl.Types.Loyalty where

import Numeric.Natural (Natural)

-- CR 306.5: "Loyalty is a characteristic only planeswalkers have." CR 109.3
-- lists it among an object's characteristics, and CR 707.2 makes it a copiable
-- value, which is why it is projected (Pawl.Types.ProjectedCharacteristics)
-- rather than read off the printed card.
--
-- CR 306.5a: "The loyalty of a planeswalker card not on the battlefield is equal
-- to the number printed in its lower right corner." That printed number is what
-- this type carries. CR 306.5c gives a planeswalker ON the battlefield a
-- different loyalty -- "the number of loyalty counters on it" -- which is a count
-- in Object.counters and never a value of this type.
--
-- Natural, not Quantity, and not a signed number. Power and Toughness wrap
-- Quantity because CR 208.2a's printed star is a characteristic-defining
-- ability; no rule makes loyalty characteristic-defining, and a printed loyalty
-- is a number in a box. A printed X loyalty (Nissa, Steward of Elements) is a
-- separate capability -- it needs the value of X chosen as the spell was cast --
-- and is unrepresentable here (#495).
newtype Loyalty = MkLoyalty
  { unwrap :: Natural
  }
  deriving (Eq, Ord, Show)
