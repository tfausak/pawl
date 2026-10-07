module Pawl.Types.PlaysLand where

import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Zone as Zone

-- | CR 305.1: whose land play fires the ability, and the zone it is played
-- from -- Urianger Augurelt's "whenever you play a land from exile".
data PlaysLand = MkPlaysLand
  { player :: PlayerRelation.PlayerRelation,
    from :: Zone.Zone
  }
  deriving (Eq, Ord, Show)
