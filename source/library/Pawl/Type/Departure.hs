module Pawl.Type.Departure where

data Departure
  = Lost
  | Conceded
  | Drew
  deriving (Eq, Ord, Show)
