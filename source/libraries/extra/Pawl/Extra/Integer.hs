module Pawl.Extra.Integer where

import qualified Data.Bits as Bits
import qualified Data.Maybe as Maybe
import qualified Numeric.Natural as Natural

-- | Converts an 'Integer' into an 'Int'. If the input is out of bounds, this
-- will return 'Nothing'.
toInt :: Integer -> Maybe Int
toInt = Bits.toIntegralSized

-- | Converts an 'Integer' into an 'Int'. If the input is out of bounds, this
-- will return the nearest value (either 'minBound' or 'maxBound').
toIntSaturating :: Integer -> Int
toIntSaturating x = case toInt x of
  Just i -> i
  Nothing -> if x < 0 then minBound else maxBound

-- | Converts an 'Integer' into a 'Natural.Natural'. If the input is negative,
-- this will return 'Nothing'.
toNatural :: Integer -> Maybe Natural.Natural
toNatural = Bits.toIntegralSized

-- | Converts an 'Integer' into a 'Natural.Natural'. If the input is negative,
-- this will return '0'.
toNaturalSaturating :: Integer -> Natural.Natural
toNaturalSaturating = Maybe.fromMaybe 0 . toNatural
