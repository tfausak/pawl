module Pawl.Extra.Semigroup where

-- | Surround a value with a prefix and suffix.
around :: (Semigroup a) => a -> a -> a -> a
around l r x = l <> x <> r
