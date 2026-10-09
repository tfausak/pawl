module Pawl.Types.CardIdentity where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 108.3: one card through every new object it becomes (CR 400.7), a
-- restart (CR 727.2) and a subgame (CR 729.2, 729.5).
data CardIdentity = MkCardIdentity
  { -- | The number of the id the card was first minted under; no rule reads it.
    serial :: Natural.Natural,
    -- | CR 108.3: who started the game with it, or brought it in.
    startingOwner :: PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)
