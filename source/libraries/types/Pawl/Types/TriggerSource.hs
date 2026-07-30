module Pawl.Types.TriggerSource where

import Pawl.Types.ObjectId (ObjectId)

-- What an ability that has TRIGGERED but is not yet on the stack hangs on.
--
-- CR 113.7: "The source of a triggered ability ... that has triggered and is
-- waiting to be put on the stack, is the object whose ability triggered" --
-- `OfObject`, which is every trigger in the game but two.
--
-- CR 725.2 carves out the exception: the monarch's two inherent abilities "have
-- no source and are controlled by the player who was the monarch at the time the
-- abilities triggered. This is an exception to rule 113.8." Nothing bears them,
-- so there is no id to name -- `Sourceless`.
--
-- The split exists so that ONE list can carry both kinds through CR 603.3b's
-- ordering: a player who controls a sourceless trigger and an ordinary one from
-- the same batch chooses the order between them, which two separate batches
-- cannot express. Pawl.Types.Source is the on-the-stack counterpart, recording
-- the same distinction (OfTrigger / OfInherentTrigger) once the ability has been
-- placed.
data TriggerSource
  = OfObject ObjectId
  | Sourceless
  deriving (Eq, Ord, Show)
