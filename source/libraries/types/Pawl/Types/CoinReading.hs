module Pawl.Types.CoinReading where

-- | Which of CR 705.2's two kinds of coin flip a Pawl.Types.FlipCoin is, and so
-- what its slot counts.
--
-- The rule's two kinds are exclusive -- an effect that cares only about the face
-- has no winner -- which is why this is one field beside one slot rather than a
-- slot apiece: a card cannot ask for both.
data CoinReading
  = -- | CR 705.2's second sentence onward: the flipper calls, and the slot counts the flips they won (Winter Sky).
    Wins
  | -- | CR 705.2's first sentence: no call is made and no player wins, and the slot counts the coins that came up heads (Odds).
    Heads
  deriving (Bounded, Enum, Eq, Ord, Show)
