module Pawl.Extra.Ord where

-- | Returns true if the value is between the lower and upper bounds
-- (inclusive).
between :: (Ord a) => a -> a -> a -> Bool
between lo hi x = lo <= x && x <= hi
