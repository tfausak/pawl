module Pawl.Types.HandWindowCap where

-- | CR 103.5b / CR 103.6: whether a pre-game hand window re-offers a card whose
-- action has already been taken. The two windows Pawl.Engine.Mulligan.handWindow
-- serves answer differently, and nothing about the effects themselves says which
-- -- the rule the window implements does, so the window passes it in.
data HandWindowCap
  = -- | CR 103.5b: no cap. That rule lets a player act "at a time they would
    -- declare" as often as they like, and Serum Powder's second copy is drawn by
    -- the first one's action -- so a card back in hand is a fresh offer.
    Repeatable
  | -- | CR 103.6b: "Each card may be revealed this way only once." Enforced per
    -- CARD rather than per action, which is how that sentence words it: two
    -- reveal actions on one card would still be one reveal.
    --
    -- Applied to every CR 103.6 action and not just to a reveal, because the
    -- closed half classifies effects and never identifies them. That is exact
    -- over rule 103.6's own enumeration: CR 103.6a's other kind puts the card
    -- onto the battlefield, so it is out of the hand and unofferable anyway, and
    -- the cap can only bite a card printing BOTH kinds -- which no card does
    -- (#803 is the same absence, for two actions of any kind).
    OncePerCard
  deriving (Bounded, Enum, Eq, Ord, Show)
