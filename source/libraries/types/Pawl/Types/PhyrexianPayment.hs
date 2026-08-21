module Pawl.Types.PhyrexianPayment where

-- | CR 107.4f: a Phyrexian mana symbol is paid either with one mana of its colour
-- or by paying 2 life. This is WHICH of those two, announced under CR 118.13a as
-- its controller proposes the spell or ability, and under CR 118.13b immediately
-- before a cost paid during a resolution is paid.
--
-- A named sum rather than a Bool, the posture every player-facing choice in this
-- engine takes, so a transcript reads as the decision it records.
--
-- Neither way needs a payload: the colour is the symbol's own and the 2 life is
-- fixed by CR 107.4f. Hybrid Phyrexian symbols would break that, naming two
-- colours, and have no ManaSymbol constructor either (#364).
data PhyrexianPayment
  = -- | CR 107.4f: one mana of the symbol's colour.
    PaysMana
  | -- | CR 107.4f: 2 life.
    PaysLife
  deriving (Eq, Ord, Show)
