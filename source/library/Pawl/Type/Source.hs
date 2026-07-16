module Pawl.Type.Source where

import Pawl.Type.Printing (Printing)

-- Grows: OfToken, OfCopy, OfEmblem, OfAbility, …
data Source
  = OfCard Printing
  deriving (Eq, Ord, Show)
