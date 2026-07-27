-- Total conversions out of Int, the counterpart to Pawl.Extra.Natural.
module Pawl.Extra.Int where

import qualified Data.Bits as Bits
import qualified Data.Maybe as Maybe
import Numeric.Natural (Natural)

-- Nothing for a negative Int.
toNatural :: Int -> Maybe Natural
toNatural = Bits.toIntegralSized

-- Zero for a negative Int.
toNaturalSaturating :: Int -> Natural
toNaturalSaturating = Maybe.fromMaybe 0 . toNatural
