module Pawl.Type.Status where

import Pawl.Type.Departure (Departure)

data Status
  = Playing
  | Departed Departure
  deriving (Eq, Ord, Show)
