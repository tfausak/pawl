module Pawl.Type.Action where

import Pawl.Type.ObjectId (ObjectId)

-- Grows: Cast, Activate, …
data Action
  = Pass
  | Play ObjectId
  deriving (Eq, Ord, Show)
