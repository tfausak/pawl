module Pawl.Type.Exclusion where

-- CR 601.2c: whether a target slot admits its own source as a legal target.
-- "another" excludes it; the default includes it. A property of the slot, not of
-- the object predicate, so it lives here rather than as a Filter atom.
data Exclusion
  = IncludesSource
  | ExcludesSource
  deriving (Eq, Ord, Show)
