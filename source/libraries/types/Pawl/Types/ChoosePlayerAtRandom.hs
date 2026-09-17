module Pawl.Types.ChoosePlayerAtRandom where

import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.SlotName as SlotName

-- | CR 608.2d: the payload of Pawl.Types.Effect's ChoosePlayerAtRandom arm --
-- which players randomness picks among, and where the pick is bound.
--
-- Pawl.Types.ChoosePlayer's twin with the decision replaced by randomness (CR
-- 701.9b), and the SCOPE is the same field for the same reason: it separates
-- Ruhan of the Fomori's "choose an opponent at random" (PlayerScope.Opponents)
-- from Strax, Sontaran Nurse's "choose a player at random"
-- (PlayerScope.EachPlayer, which includes the chooser). It is resolved through
-- Pawl.Engine.PlayerEffect.playersInScope, the one fold every player-set read
-- shares, against CR 109.5's "you" -- the resolving controller.
data ChoosePlayerAtRandom = MkChoosePlayerAtRandom
  { scope :: PlayerScope.PlayerScope,
    slot :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
