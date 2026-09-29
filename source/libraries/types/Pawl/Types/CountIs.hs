module Pawl.Types.CountIs where

import Numeric.Natural (Natural)
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Label as Label
import qualified Pawl.Types.Zone as Zone

-- | How many objects with one card name are in one zone: the ones a player
-- controls on the battlefield (CR 110.2), the ones they own anywhere else.
data CountIs = MkCountIs
  { player :: Label.Label,
    zone :: Zone.Zone,
    card :: CardName.CardName,
    count :: Natural
  }
  deriving (Eq, Ord, Show)
