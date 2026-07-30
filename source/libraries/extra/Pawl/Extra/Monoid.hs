module Pawl.Extra.Monoid where

-- | Puts the separator between each element of the list.
sepBy :: (Monoid a) => a -> [a] -> a
sepBy s xs = case xs of
  [] -> mempty
  h : t -> h <> foldMap (s <>) t
