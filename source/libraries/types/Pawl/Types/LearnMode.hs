module Pawl.Types.LearnMode where

-- | CR 701.48a: which of learning's two branches a learning player takes.
--
-- A named sum rather than a Bool, Pawl.Types.ForageMode's posture, so a
-- transcript reads as the decision it records.
--
-- Rule 701.48a prints the two as sequential "you may"s -- "you may discard a
-- card. If you do, draw a card. If you didn't discard a card, you may reveal a
-- Lesson card you own from outside the game and put it into your hand" -- and
-- the prompt offers them together, which is why declining is the ABSENCE of a
-- mode rather than a third constructor. The three outcomes rule 701.48a admits
-- are exactly discard-then-draw, take a Lesson, and neither; a player who
-- declines the discard is the only one the second sentence offers anything, so
-- one choice over the three is the same information the two sentences give.
data LearnMode
  = -- | Rule 701.48a's first two sentences: discard a card, then draw a card.
    DiscardAndDraw
  | -- | Rule 701.48a's third sentence: reveal a Lesson card this player owns
    -- from outside the game and put it into their hand.
    TakeLesson
  deriving (Bounded, Enum, Eq, Ord, Show)
