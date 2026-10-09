module Pawl.Types.MonarchTarget where

import qualified Pawl.Types.SlotName as SlotName

-- | Which player an Effect.BecomeMonarch names. CR 725.1 says only that "an
-- effect instructs a player to become the monarch" and leaves the naming to the
-- card, so this enumerates the three ways the pool and the rulebook do it.
--
-- Its own sum rather than a Pawl.Types.PlayerRef, which would widen this
-- opcode to references naming several players -- meaningless for a designation
-- CR 725.3 gives to exactly one player at a time. Each arm is resolved as the
-- PlayerRef it means (Pawl.Engine.Resolve.Effect's oneSeat), so the seat is
-- read the way every other reference reads it.
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
    -- Pawl.Engine.Resolve.Slots.slotsOf and the dataflow lint.
    InSlot SlotName.SlotName
  deriving (Eq, Ord, Show)
