module Pawl.Codec.Teams where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.TeamId as TeamId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Teams as Teams

-- | Keyed by a PlayerId, a Natural newtype, so it takes 'Common.naturalMap' --
-- Pawl.Codec.Player's `commanderDamage`, same key.
codec :: Codec.Codec Teams.Teams
codec = Common.wrapper (Common.naturalMap PlayerId.codec TeamId.codec) Teams.MkTeams Teams.unwrap
