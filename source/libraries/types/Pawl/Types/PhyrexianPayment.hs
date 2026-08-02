module Pawl.Types.PhyrexianPayment where

-- | CR 107.4f: "A Phyrexian mana symbol represents a cost that can be paid either
-- with one mana of its color or by paying 2 life." This is WHICH of those two,
-- as announced under CR 118.13a -- "the choice of how to pay for that symbol is
-- made as its controller proposes that spell or ability."
--
-- A named sum rather than a Bool (no boolean blindness), the posture
-- MulliganDecision (Keep/Mulligan), Concession (Continues/Concedes) and
-- OptionalDecision (Declines/Exercises) take: every player-facing choice in this
-- engine is written out, so a transcript reads as the decision it records rather
-- than as an unlabelled boolean.
--
-- Named for what the player DOES, and deliberately not for the resource's amount:
-- "one mana of its color" is the symbol's own colour and "2 life" is fixed by CR
-- 107.4f, so neither way needs a payload and the announcement carries none. CR
-- 107.4f's hybrid Phyrexian symbols ("{G/U/P}") would break that -- their mana
-- way names two colours -- and they have no ManaSymbol constructor either (#364).
data PhyrexianPayment
  = -- | CR 107.4f: "with one mana of its color".
    PaysMana
  | -- | CR 107.4f: "or by paying 2 life".
    PaysLife
  deriving (Eq, Ord, Show)
