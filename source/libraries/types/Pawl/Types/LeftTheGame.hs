module Pawl.Types.LeftTheGame where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Zone as Zone

-- | An object that left the game rather than moving to a zone: the id it had,
-- and the zone it was in when it went (CR 729.4a, CR 800.4a).
data LeftTheGame = MkLeftTheGame
  { -- | The id it had while it existed, the key its CR 608.2h last known
    -- information is filed under.
    object :: ObjectId.ObjectId,
    -- | The zone it left (CR 400.7).
    from :: Zone.Zone
  }
  deriving (Eq, Ord, Show)
