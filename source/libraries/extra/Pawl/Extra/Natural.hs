module Pawl.Extra.Natural where

import qualified Data.Bits as Bits
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Numeric.Natural as Natural
import qualified Pawl.Extra.Int as Int

-- | Returns the length of a finite structure.
--
-- Note that this is not the same as 'Data.List.genericLength', which is always
-- O(n)! Typically this is either O(1) or O(log n). For some unlikely
-- degenerate instances, it could be O(n).
length :: (Foldable.Foldable t) => t a -> Natural.Natural
length = Int.toNaturalSaturating . Foldable.length

-- | Converts an 'Natural.Natural' into an 'Int'. If the input is too large,
-- this will  return 'Nothing'.
toInt :: Natural.Natural -> Maybe Int
toInt = Bits.toIntegralSized

-- | Converts an 'Natural.Natural' into a Int. If the input is too large, this
-- will return 'maxBound'.
toIntSaturating :: Natural.Natural -> Int
toIntSaturating = Maybe.fromMaybe maxBound . toInt
