module Pawl.Types.Status where

import Pawl.Types.Departure (Departure)

data Status
  = Playing
  | Departed Departure
  deriving (Eq, Ord, Show)
