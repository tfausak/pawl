module Pawl.Types.Scaling where

import qualified Numeric.Natural as Natural

-- | CR 614.1: how a counting replacement rewrites the count. Corpsejack Menace
-- and Doubling Season are Multiply 2 ("twice that many"); Hardened Scales is
-- AddMore 1 ("that many plus one"). The difference between those cards is a
-- NUMBER, which is the whole point of this type existing instead of two effect
-- constructors.
data Scaling
  = Multiply Natural.Natural
  | AddMore Natural.Natural
  | -- | CR 107.1a: Vorinclex, Monstrous Raider's "half that many . . . rounded
    -- down". Payload-free where its siblings carry a number, because a divisor
    -- field admits `Divide 0` -- a calculation no rule describes -- and no
    -- printing divides a count by anything but two. Rounding is DOWN because
    -- rule 107.1a has the card say which way and this card says down; a "rounded
    -- up" sibling waits for a printing that asks for one.
    --
    -- The only Scaling that can answer ZERO, which is a replacement removing the
    -- event rather than resizing it. Its callers already guard on that: see
    -- Pawl.Engine.Event.putCounters and putPlayerCounters.
    Halve
  deriving (Eq, Ord, Show)
