module Pawl.Type.Action where

import Pawl.Type.ActivatedAbility (ActivatedAbility)
import Pawl.Type.Card (Card)
import Pawl.Type.ObjectId (ObjectId)

-- Grows: special actions beyond Play, …
data Action
  = Pass
  | Play ObjectId
  | Cast ObjectId
  | -- CR 602: activate the source permanent's ability. Carries the ability VALUE
    -- (validated by membership in Projection.abilitiesOf), never an index.
    Activate ObjectId (ActivatedAbility Card)
  deriving (Eq, Ord, Show)
