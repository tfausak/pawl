-- Total conversions out of Integer, the counterpart to Pawl.Extra.Natural.
-- Integer is where the rules' own arithmetic lives (Quantity.evaluate can
-- legitimately produce a negative), so these are the conversions that meet a
-- quantity at the boundary of a count. The alternative is the one that bites:
-- fromInteger on a negative value bound for a Natural CRASHES, and on one bound
-- for an Int it wraps.
module Pawl.Extra.Integer where

import qualified Data.Bits as Bits
import qualified Data.Maybe as Maybe
import Numeric.Natural (Natural)

-- Nothing for a negative Integer.
toNatural :: Integer -> Maybe Natural
toNatural = Bits.toIntegralSized

-- Zero for a negative Integer. That floor is CR 107.1b, not a house policy:
-- "If a calculation that would determine the result of an effect yields a
-- negative number, zero is used instead." The rule's exception -- doubling,
-- tripling, or setting a life total or a creature's power/toughness -- does not
-- route through here, because every caller is a count (damage, draws, mills,
-- discards, sacrifices, tokens, counters) rather than one of those effects.
toNaturalSaturating :: Integer -> Natural
toNaturalSaturating = Maybe.fromMaybe 0 . toNatural

-- Nothing when the value does not fit in an Int, in either direction.
toInt :: Integer -> Maybe Int
toInt = Bits.toIntegralSized

-- The nearer bound when the value does not fit. Correct for every count handed
-- to take, drop, replicate and their kin: those read a negative count as none,
-- and cannot tell maxBound apart from more.
toIntSaturating :: Integer -> Int
toIntSaturating x = case toInt x of
  Just i -> i
  Nothing -> if x < 0 then minBound else maxBound
