module Pawl.Types.Vanguard where

-- | CR 313.6 \/ 313.7: the two modifiers a vanguard card prints in its lower
-- corners, which CR 902.4 and CR 902.5 turn into its owner's starting totals.
--
-- ONE type carrying both and not two optional fields on the face, because CR
-- 313.6 and CR 313.7 each say "each vanguard card has" -- a vanguard with one
-- modifier and not the other is a card the rules do not describe. Pawl.CardSpec's
-- "a card is a vanguard iff it has printed modifiers" lint is the corpus half of
-- the same biconditional.
--
-- Signed, and Integer rather than Natural: rule 313.6's number is "a number
-- preceded by a plus sign, a number preceded by a minus sign, or a zero", and
-- every reader adds it to a base. CR 107.1b's floor is applied where the sum is
-- read (Pawl.Engine.Vanguard), since the modifier itself is legitimately
-- negative.
data Vanguard = MkVanguard
  { -- | CR 313.6: applied to the starting hand size and the maximum hand size of
    -- this card's owner, both normally seven (CR 902.5, CR 902.5b).
    handModifier :: Integer,
    -- | CR 313.7: applied to the starting life total of this card's owner,
    -- normally twenty (CR 902.4).
    lifeModifier :: Integer
  }
  deriving (Eq, Ord, Show)
