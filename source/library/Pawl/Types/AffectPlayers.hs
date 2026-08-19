module Pawl.Types.AffectPlayers where

import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's AffectPlayers arm (#1305): install this
-- player effect over the players named, for this duration.
data AffectPlayers = MkAffectPlayers
  { duration :: Duration.Duration,
    players :: AffectedPlayers.AffectedPlayers SlotName.SlotName,
    effect :: PlayerEffect.PlayerEffect
  }
  deriving (Eq, Ord, Show)
