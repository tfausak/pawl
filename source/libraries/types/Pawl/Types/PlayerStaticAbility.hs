module Pawl.Types.PlayerStaticAbility where

import Pawl.Types.PlayerEffect (PlayerEffect)
import Pawl.Types.PlayerScope (PlayerScope)

-- A card's printed player/rules-modifying static ability (CR 604.1/604.2: a
-- static ability creates a continuous effect active while its permanent is on
-- the battlefield). The player-axis sibling of Pawl.Types.StaticAbility, whose
-- Affected/Modification pair this mirrors with a PlayerScope/PlayerEffect pair.
--
-- Gathered LIVE from every battlefield permanent by Pawl.Engine.PlayerEffect.applying on
-- every read, never captured -- so Rule of Law leaving the battlefield lifts its
-- restriction with nothing to unwind. Rule of Law, Thalia, Sapphire Medallion and
-- Reliquary Tower each declare exactly one.
data PlayerStaticAbility = MkPlayerStaticAbility
  { scope :: PlayerScope,
    effect :: PlayerEffect
  }
  deriving (Eq, Ord, Show)
