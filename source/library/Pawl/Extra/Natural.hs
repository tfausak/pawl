-- Total conversions out of Natural, plus the one conversion into it that every
-- module needs (length). Naming the failure -- in the type with Maybe, or in
-- the name with Saturating -- is the whole point: fromIntegral silently wraps a
-- Natural above maxBound into a NEGATIVE Int. .hlint.yaml bans the unchecked
-- conversions outright, so these are the only ones there are.
-- Pawl.Extra.Int and Pawl.Extra.Integer are the counterparts, each named for
-- the type it converts out of.
module Pawl.Extra.Natural where

import qualified Data.Bits as Bits
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import Numeric.Natural (Natural)
import qualified Pawl.Extra.Int as Int

-- Nothing when the value is too big for an Int.
toInt :: Natural -> Maybe Int
toInt = Bits.toIntegralSized

-- maxBound when the value is too big for an Int. That is the right answer for
-- every count and index handed to take, drop, replicate, Seq.lookup and their
-- kin: no list that fits in memory can tell maxBound apart from more.
toIntSaturating :: Natural -> Int
toIntSaturating = Maybe.fromMaybe maxBound . toInt

-- How many elements a container holds, as the Natural it always is -- so a
-- count can be compared against a length without either side converting. Total:
-- a length is never negative, and never overflows an Int in the first place.
length :: (Foldable.Foldable t) => t a -> Natural
length = Int.toNaturalSaturating . Foldable.length
