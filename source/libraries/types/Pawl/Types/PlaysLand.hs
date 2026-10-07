module Pawl.Types.PlaysLand where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Zone as Zone

-- | CR 305.1: whose land play fires the ability, the zone it is played from,
-- and which land -- Urianger Augurelt's "whenever you play a land from exile",
-- Fires of Mount Doom's "when you play a card this way".
data PlaysLand = MkPlaysLand
  { player :: PlayerRelation.PlayerRelation,
    from :: Zone.Zone,
    -- | Read against the land as it arrived (CR 400.7i).
    filter :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
