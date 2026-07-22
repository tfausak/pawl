module Pawl.Type.Scaling where

import Numeric.Natural (Natural)

-- CR 614.1: how a counting replacement rewrites the count. Corpsejack Menace and
-- Doubling Season are Multiply 2 ("twice that many"); Hardened Scales is AddMore
-- 1 ("that many plus one"). The difference between those cards is a NUMBER, which
-- is the whole point of this type existing instead of two effect constructors.
data Scaling
  = Multiply Natural
  | AddMore Natural
  deriving (Eq, Ord, Show)
