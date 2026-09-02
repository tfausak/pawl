module Pawl.Types.TakeExtraTurn where

import qualified Data.Set as Set
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity

-- | CR 500.7: the players the PlayerRef names each take `count` extra turns, every one skipping the phases named.
--
-- `count` is a Quantity because Ral Zarek's "take an extra turn after this one
-- for each coin that comes up heads" reads the tally an earlier effect of the
-- same resolution bound. One is the value the codec elides, which is every other
-- extra-turn card in data\/cards\/.
--
-- Construct with BRACE syntax everywhere: positional construction absorbs a new
-- field in argument order with nothing red (#2009, #2021).
data TakeExtraTurn = MkTakeExtraTurn
  { player :: PlayerRef.PlayerRef,
    skips :: Set.Set PhaseSelector.PhaseSelector,
    count :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)

-- | What "take an extra turn" writes, and the value the codec elides.
defaultCount :: Quantity.Quantity
defaultCount = Quantity.Literal 1
