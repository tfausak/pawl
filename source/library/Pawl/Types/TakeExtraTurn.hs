module Pawl.Types.TakeExtraTurn where

import qualified Data.Set as Set
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | CR 500.7: the players the PlayerRef names each take an extra turn, skipping the phases named.
data TakeExtraTurn = MkTakeExtraTurn
  { player :: PlayerRef.PlayerRef,
    skips :: Set.Set PhaseSelector.PhaseSelector
  }
  deriving (Eq, Ord, Show)
