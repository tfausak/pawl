module Pawl.Type.Response where

import Data.Map.Strict (Map)
import Numeric.Natural (Natural)
import Pawl.Type.Action (Action)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Recipient (Recipient)
import Pawl.Type.SlotName (SlotName)

data Response
  = ChoseAction Action
  | Shuffled [ObjectId]
  | ChoseDiscard [ObjectId]
  | DeclaredAttackers [ObjectId]
  | DeclaredBlockers (Map ObjectId ObjectId)
  | AssignedCombatDamage (Map Recipient Natural)
  | ChoseTargets (Map SlotName Recipient)
  deriving (Eq, Show)
