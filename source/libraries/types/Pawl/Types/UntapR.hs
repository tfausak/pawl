module Pawl.Types.UntapR where

import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.UntapRewrite as UntapRewrite

-- | The payload of Pawl.Types.ReplacementEffect's UntapR arm: when a permanent's
-- untap is intercepted, and what happens instead.
data UntapR = MkUntapR
  { -- | CR 502.3: "during its controller's untap step" (Bewitching Leechcraft),
    -- as the step the game is in. Whose step needs no field: CR 502.3 untaps
    -- only the active player's permanents, and CR 502.4 gives no player
    -- priority, so nothing else untaps during it. Nothing is every untap (CR
    -- 122.1d).
    during :: Maybe Phase.Phase,
    rewrite :: UntapRewrite.UntapRewrite
  }
  deriving (Eq, Ord, Show)
