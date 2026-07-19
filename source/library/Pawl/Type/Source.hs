module Pawl.Type.Source where

import Pawl.Type.ActivatedAbility (ActivatedAbility)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Printing (Printing)
import Pawl.Type.TriggeredAbility (TriggeredAbility)

-- Becomes a `data` once there is more than one way to be an object's source
-- (OfToken, OfCopy, OfEmblem, …).
data Source
  = OfCard Printing
  | -- CR 602: an activated ability on the stack -- the source permanent's id plus
    -- the ability. The ability travels with the object so it resolves even if the
    -- source leaves (CR 608.2g; LKI is a future refinement).
    OfAbility ObjectId ActivatedAbility
  | -- CR 603.3: a triggered ability on the stack -- the source permanent's id
    -- plus the ability. Travels with the object so it resolves even if the source
    -- leaves (CR 603.3d).
    OfTrigger ObjectId TriggeredAbility
  deriving (Eq, Ord, Show)
