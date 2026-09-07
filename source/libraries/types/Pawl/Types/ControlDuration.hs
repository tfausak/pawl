module Pawl.Types.ControlDuration where

-- | How long one player's control of another lasts, which is the axis CR 723
-- splits on: rule 723.1's effects run for a whole turn, and rule 723.2's two
-- named cards for a limited duration inside one.
--
-- The field is on Pawl.Types.PlayerControl rather than on the opcode that
-- installs it, because the expiry is asked of the STATE -- Pawl.Engine.Engine's
-- turn handoff and Pawl.Engine.Stack's end of resolution each drop the arm that
-- is theirs, and neither can see the effect that wrote the row.
data ControlDuration
  = -- | CR 723.1: the whole of the controlled player's turn (Mindslaver).
    UntilTurnEnds
  | -- | CR 723.2: until the object that installed it finishes resolving (Word of
    -- Command).
    UntilResolutionEnds
  deriving (Bounded, Enum, Eq, Ord, Show)
