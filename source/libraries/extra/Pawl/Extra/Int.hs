module Pawl.Extra.Int where

import qualified Data.Bits as Bits
import qualified Data.Maybe as Maybe
import qualified Numeric.Natural as Natural

-- | Converts an 'Int' into a 'Natural.Natural'. If the input is negative, this
-- will return 'Nothing'.
toNatural :: Int -> Maybe Natural.Natural
toNatural = Bits.toIntegralSized

-- | Converts an 'Int' into a 'Natural.Natural'. If the input is negative, this
-- will return '0'.
toNaturalSaturating :: Int -> Natural.Natural
toNaturalSaturating = Maybe.fromMaybe 0 . toNatural
