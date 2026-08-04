module Pawl.Types.Loyalty where

import qualified Numeric.Natural as Natural

-- | CR 306.5: loyalty is a characteristic only planeswalkers have. CR 109.3 lists
-- it among an object's characteristics and CR 707.2 makes it a copiable value,
-- which is why it is projected rather than read off the printed card.
--
-- This type carries CR 306.5a's PRINTED number. CR 306.5c gives a planeswalker on
-- the battlefield the number of loyalty counters on it instead, which is a count
-- in Object.counters and never a value of this type.
--
-- Natural, not Quantity: Power and Toughness wrap Quantity for CR 208.2a's
-- characteristic-defining star, and no rule gives loyalty one. A printed X
-- loyalty (Nissa, Steward of Elements) needs the value of X chosen as the spell
-- was cast, and is unrepresentable here (#495).
newtype Loyalty = MkLoyalty
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
