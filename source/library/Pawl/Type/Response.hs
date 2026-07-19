module Pawl.Type.Response where

import Data.Map.Strict (Map)
import Numeric.Natural (Natural)
import Pawl.Type.Action (Action)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Recipient (Recipient)
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.Subtype (Subtype)

data Response
  = ChoseAction Action
  | Shuffled [ObjectId]
  | ChoseDiscard [ObjectId]
  | DeclaredAttackers [ObjectId]
  | DeclaredBlockers (Map ObjectId ObjectId)
  | AssignedCombatDamage (Map Recipient Natural)
  | ChoseTargets (Map SlotName Recipient)
  | -- CR 612 / the D4 binding: the (from, to) basic land types a text-changer's
    -- caster chose, serialized so a DecisionLog replays the hack deterministically.
    ChoseBasicLandTypes (Subtype, Subtype)
  | -- CR 701.23: the library card a search found (Nothing = failed to find),
    -- serialized so a DecisionLog replays a tutor deterministically.
    Searched (Maybe ObjectId)
  deriving (Eq, Show)
