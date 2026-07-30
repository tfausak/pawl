module Pawl.Types.Action where

import Pawl.Types.ActivatedAbility (ActivatedAbility)
import Pawl.Types.Card (Card)
import Pawl.Types.ObjectId (ObjectId)

-- Grows: special actions beyond Play, …
data Action
  = Pass
  | Play ObjectId
  | Cast ObjectId
  | -- CR 602: activate the source permanent's ability. Carries the ability VALUE
    -- (validated by membership in Projection.abilitiesOf), never an index.
    Activate ObjectId (ActivatedAbility Card)
  deriving (Eq, Ord, Show)
