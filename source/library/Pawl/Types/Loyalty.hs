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
-- Not a Quantity, which is what Power and Toughness wrap for CR 208.2's star and
-- the CR 208.2a characteristic-defining ability behind it. No rule gives loyalty
-- one: rule 306.5a says "the number printed in its lower right corner", and the
-- only non-numeral a planeswalker prints there is CR 107.3's X.
data Loyalty
  = -- | CR 306.5a: the printed numeral, which is every planeswalker but one.
    Literal Natural.Natural
  | -- | CR 107.3's X printed as the loyalty (Nissa, Steward of Elements).
    --
    -- CR 107.3m is what makes this readable at all: the value of X for a
    -- permanent's enters-the-battlefield replacement effect -- CR 306.5b's
    -- intrinsic loyalty ability is one -- is the value chosen for the spell that
    -- became that object, "although the value of X for that permanent is 0". So
    -- this arm is a SNAPSHOT of the announcement (CR 601.2b), read once as the
    -- permanent enters off Object.announcedX, and never a live read of anything.
    --
    -- Zero wherever no such announcement stands behind the object: a token copy
    -- of Nissa, a Nissa put onto the battlefield by an effect rather than cast,
    -- and CR 107.3g's card in any zone but the stack. Such a planeswalker enters
    -- with no loyalty counters and CR 704.5i puts it into its owner's graveyard.
    Variable
  deriving (Eq, Ord, Show)
