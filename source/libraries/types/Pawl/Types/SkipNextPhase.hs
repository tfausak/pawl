module Pawl.Types.SkipNextPhase where

import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | CR 500.8: the players the PlayerRef names each skip their next matching phase.
data SkipNextPhase = MkSkipNextPhase
  { player :: PlayerRef.PlayerRef,
    selector :: PhaseSelector.PhaseSelector
  }
  deriving (Eq, Ord, Show)
