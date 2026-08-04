module Pawl.Types.TriggerSource where

import qualified Pawl.Types.ObjectId as ObjectId

-- | What an ability that has TRIGGERED but is not yet on the stack hangs on.
--
-- CR 113.7 makes the source the object whose ability triggered -- `OfObject`,
-- every trigger in the game but two. CR 725.2 carves out the exception: the
-- monarch's two inherent abilities have no source, so there is no id to name.
--
-- The split exists so that ONE list can carry both kinds through CR 603.3b's
-- ordering: a player controlling a sourceless trigger and an ordinary one from
-- the same batch chooses the order between them, which two separate batches
-- cannot express. Pawl.Types.Source is the on-the-stack counterpart.
--
-- This is HALF of what that ordering choice shows a player, not all of it:
-- Pawl.Types.TriggerEntry pairs it with the ability, because one source can have
-- two distinct abilities in one batch and CR 725.2's pair shares this type's one
-- Sourceless value (#61).
data TriggerSource
  = OfObject ObjectId.ObjectId
  | Sourceless
  deriving (Eq, Ord, Show)
