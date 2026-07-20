module Pawl.Type.Mode where

import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Pawl.Type.Effect (Effect)
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.TargetSpec (TargetSpec)

-- CR 700.2: one mode of a modal spell or ability -- its own effects and its own
-- target namespace. `effects` is a Seq (ordered; CR 608.2c resolves in written
-- order; duplicates allowed). `targetSpecs` is per-mode: CR 601.2c/700.2c fill only
-- the CHOSEN mode's slots. Parametric in `card` like Effect (a concrete Effect Card
-- would cycle with Card, which embeds the payload; Card ties the knot at Mode Card).
-- A non-modal payload is a single Mode.
data Mode card = MkMode
  { effects :: Seq (Effect card),
    targetSpecs :: Map SlotName TargetSpec
  }
  deriving (Eq, Ord, Show)
