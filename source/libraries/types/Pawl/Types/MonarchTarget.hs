module Pawl.Types.MonarchTarget where

import qualified Pawl.Types.SlotName as SlotName

-- | Which player an Effect.BecomeMonarch names. CR 725.1 says only that "an
-- effect instructs a player to become the monarch" and leaves the naming to the
-- card, so this enumerates the three ways the pool and the rulebook do it.
--
-- Its own sum rather than a Pawl.Types.PlayerRef, which the InSlot arm now
-- otherwise duplicates: CR 725.2's ControllerOfSource has no PlayerRef spelling
-- (that type's Relative arm resolves against a perspective, never against a
-- bound object), and PlayerRef.EachPlayer is meaningless for a designation CR
-- 725.3 gives to exactly one player at a time. Reusing it would widen this
-- opcode to two values it must reject at resolution.
data MonarchTarget
  = -- | "you become the monarch" (Palace Jailer's ETB): the resolving controller.
    TheController
  | -- | CR 725.2: "its controller becomes the monarch" (the steal): the controller
    -- of the object bound as the ability's source (the damaging creature).
    ControllerOfSource
  | -- | "Target player becomes the monarch" (Denethor, Stone Seer): the player
    -- bound in a target slot, announced under CR 601.2c and re-checked under CR
    -- 608.2b like any other target. The first arm that reads a slot, so it is
    -- also what makes Effect.BecomeMonarch a slot-reading opcode for
    -- Pawl.Engine.Resolve.slotsOf and the dataflow lint.
    InSlot SlotName.SlotName
  deriving (Eq, Ord, Show)
