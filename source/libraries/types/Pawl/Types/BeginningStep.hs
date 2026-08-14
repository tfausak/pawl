module Pawl.Types.BeginningStep where

data BeginningStep
  = Untap
  | Upkeep
  | DrawStep
  deriving (Bounded, Enum, Eq, Ord, Show)
