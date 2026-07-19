module Pawl.Type.Supertype where

-- Grows: Snow, World, …
data Supertype
  = Basic
  | Legendary
  deriving (Eq, Ord, Show)
