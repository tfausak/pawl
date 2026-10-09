module Pawl.Types.ManaSegment where

import Pawl.Types.GameState (GameState)

-- | A stretch of a CR 605.3a mana window that one activation wrote, between
-- two states. An activation whose own cost opened a nested window owns the
-- stretches either side of the nested activations, which own theirs.
--
-- Deliberately no codec: a payment in flight is never serialised.
data ManaSegment = MkManaSegment
  { -- | The activation that wrote it, by index into
    -- Pawl.Types.ManaWindow.activated.
    activation :: Int,
    -- | The state it began on.
    opened :: GameState,
    -- | The state it ended on.
    closed :: GameState
  }
