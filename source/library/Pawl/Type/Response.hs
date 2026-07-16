module Pawl.Type.Response where

import Pawl.Type.Action (Action)
import Pawl.Type.ObjectId (ObjectId)

data Response
  = ChoseAction Action
  | Shuffled [ObjectId]
  | ChoseDiscard [ObjectId]
  deriving (Eq, Show)
