module Pawl.Types.Rounding where

-- | CR 107.1a: which way a calculation that could yield a fraction is rounded.
--
-- A PAYLOAD rather than a rule the engine applies on its own, because CR 107.1a
-- says so: "if a spell or ability could generate a fractional number, the spell
-- or ability will tell you whether to round up or down". Both directions are
-- printed, and Aspect of Wolf prints BOTH IN ONE SENTENCE -- "+X/+Y, where X is
-- half the number of Forests you control, rounded down, and Y is half the
-- number of Forests you control, rounded up" -- so there is no direction to fix.
--
-- Toward the greater integer and toward the lesser, NOT away from and toward
-- zero: CR 107.1 makes every game value an integer and the printed words are the
-- ordinary ones, so half of -3 rounded up is -1. A negative can reach the
-- rounding at all only because CR 107.1b lets a game value be less than zero;
-- no card in the pool halves one.
data Rounding
  = Up
  | Down
  deriving (Eq, Ord, Show)
