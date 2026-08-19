module Pawl.Types.Status where

import qualified Pawl.Types.Departure as Departure

data Status
  = Playing
  | Departed Departure.Departure
  deriving (Eq, Ord, Show)
