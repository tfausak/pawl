module Pawl.Codec.TeamId where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.TeamId as TeamId

-- | A Natural rather than a bare Integer, Pawl.Codec.PlayerId's reason: a
-- negative wire value cannot go through a partial fromInteger.
codec :: Codec.Codec TeamId.TeamId
codec = Common.wrapper Common.natural TeamId.MkTeamId TeamId.unwrap
