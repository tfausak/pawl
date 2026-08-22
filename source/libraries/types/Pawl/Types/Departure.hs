module Pawl.Types.Departure where

data Departure
  = Lost
  | Conceded
  | Drew
  deriving (Bounded, Enum, Eq, Ord, Show)
