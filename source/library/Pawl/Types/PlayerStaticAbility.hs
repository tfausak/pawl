module Pawl.Types.PlayerStaticAbility where

import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerScope as PlayerScope

-- | A card's printed player/rules-modifying static ability (CR 604.1/604.2: a
-- static ability creates a continuous effect active while its permanent is on
-- the battlefield). The player-axis sibling of Pawl.Types.StaticAbility, whose
-- Affected/Modification pair this mirrors with a PlayerScope/PlayerEffect pair.
--
-- Gathered LIVE from every battlefield permanent by Pawl.Engine.PlayerEffect.applying on
-- every read, never captured -- so Rule of Law leaving the battlefield lifts its
-- restriction with nothing to unwind. Rule of Law, Thalia, Sapphire Medallion and
-- Reliquary Tower each declare one; Null Chamber declares two, because CR 305.1
-- makes playing a land a special action and its one printed sentence therefore
-- prohibits on two different axes.
data PlayerStaticAbility = MkPlayerStaticAbility
  { scope :: PlayerScope.PlayerScope,
    effect :: PlayerEffect.PlayerEffect
  }
  deriving (Eq, Ord, Show)
