module Pawl.Type.BeginningStep where

data BeginningStep
  = Untap
  | Upkeep
  | DrawStep
  deriving (Eq, Ord, Show)
