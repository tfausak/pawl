module Pawl.Types.LifeIs where

import qualified Pawl.Types.Label as Label

-- | CR 119.1: a player's life total.
data LifeIs = MkLifeIs
  { player :: Label.Label,
    life :: Integer
  }
  deriving (Eq, Ord, Show)
