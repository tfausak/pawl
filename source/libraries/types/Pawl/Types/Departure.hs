module Pawl.Types.Departure where

data Departure
  = Lost
  | Conceded
  | Drew
  deriving (Eq, Ord, Show)
