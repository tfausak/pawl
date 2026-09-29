module Pawl.Types.DefendersAre where

import qualified Pawl.Types.Label as Label

-- | CR 506.2: the defending players, in the order CR 802.4 has them declare.
newtype DefendersAre = MkDefendersAre
  { players :: [Label.Label]
  }
  deriving (Eq, Ord, Show)
