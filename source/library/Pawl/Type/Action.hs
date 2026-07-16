module Pawl.Type.Action where

import Pawl.Type.ObjectId (ObjectId)

-- Grows: Activate, special actions beyond Play, …
data Action
  = Pass
  | Play ObjectId
  | Cast ObjectId
  deriving (Eq, Ord, Show)
