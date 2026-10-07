module Pawl.Types.LandPlayed where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Zone as Zone

-- | CR 305.1: a player played a land, the special action of CR 116.2a.
data LandPlayed = MkLandPlayed
  { -- | The player who played it.
    player :: PlayerId.PlayerId,
    -- | The land as it arrived (CR 400.7), or the card played where a
    -- replacement kept it from arriving.
    land :: ObjectId.ObjectId,
    -- | The zone it was played from.
    from :: Zone.Zone
  }
  deriving (Eq, Ord, Show)
