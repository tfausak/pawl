module Pawl.Type.Response where

import Data.Map.Strict (Map)
import Numeric.Natural (Natural)
import Pawl.Type.Action (Action)
import Pawl.Type.ObjectId (ObjectId)

data Response
  = ChoseAction Action
  | Shuffled [ObjectId]
  | ChoseDiscard [ObjectId]
  | DeclaredAttackers [ObjectId]
  | DeclaredBlockers (Map ObjectId ObjectId)
  | AssignedCombatDamage (Map ObjectId Natural)
  deriving (Eq, Show)
