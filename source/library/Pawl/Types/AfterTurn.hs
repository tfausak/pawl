module Pawl.Types.AfterTurn where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 611.2a's "until the end of your next turn" expiry: the player whose turn
-- it names, and the turn the duration began on.

-- Both are SAMPLED at arming time (Pawl.Engine.Expiry.arm) and never rewritten
-- afterwards. `turn` is GameState.turnNumber as the effect was stored, and the
-- effect ends at the end of the first turn of `player` numbered ABOVE it --
-- which is what makes the reading correct when the duration begins during that
-- player's OWN turn, the one case that separates "until the end of your next
-- turn" from "until your next turn". Without the number, the current turn is
-- indistinguishable from the next one and the effect ends a whole turn early.
data AfterTurn = MkAfterTurn
  { player :: PlayerId.PlayerId,
    turn :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
