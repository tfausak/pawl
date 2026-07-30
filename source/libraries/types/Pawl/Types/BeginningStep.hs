module Pawl.Types.BeginningStep where

data BeginningStep
  = Untap
  | Upkeep
  | DrawStep
  deriving (Eq, Ord, Show)
