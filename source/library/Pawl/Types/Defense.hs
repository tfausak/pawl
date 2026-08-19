module Pawl.Types.Defense where

import qualified Numeric.Natural as Natural

-- | CR 310.4: "defense is a characteristic that battles have". CR 109.3 lists it
-- among an object's characteristics, which is why it is projected rather than read
-- off the printed card.
--
-- NOT because CR 707.2 lists it. That rule's parenthetical of copiable values ends
-- "power, toughness, and/or loyalty" and never names defense -- the one place the
-- symmetry with Pawl.Types.Loyalty breaks, and an omission rather than a rule:
-- Gatherer's ruling for Invasion of Dominaria says outright that "if another
-- permanent enters the battlefield as a copy of a battle, it also enters with that
-- number of defense counters". Projecting this field is what produces that answer,
-- since CR 310.4b's intrinsic ability reads the projection.
--
-- This type carries CR 210.1's PRINTED number -- the one in the card's lower right
-- corner, which CR 310.4a makes the defense of a battle card that is not on the
-- battlefield. CR 310.4c gives a battle on the battlefield the number of defense
-- counters on it instead, which is a count in Object.counters and never a value of
-- this type.
--
-- Not a Quantity, for Pawl.Types.Loyalty's reason: Power and Toughness wrap one
-- for CR 208.2's star and the CR 208.2a ability behind it, and no rule gives
-- defense one. A NEWTYPE where loyalty is a sum, which is the one place the two
-- diverge: loyalty has a printing whose lower right corner holds CR 107.3's X
-- (Nissa, Steward of Elements), and every battle printed so far prints a literal.
newtype Defense = MkDefense
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
