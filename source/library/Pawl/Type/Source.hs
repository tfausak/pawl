module Pawl.Type.Source where

import Pawl.Type.Printing (Printing)

-- Becomes a `data` once there is more than one way to be an object's source
-- (OfToken, OfCopy, OfEmblem, OfAbility, …).
newtype Source
  = OfCard Printing
  deriving (Eq, Ord, Show)
