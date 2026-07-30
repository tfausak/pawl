module Pawl.Types.Mode where

import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Pawl.Types.Effect (Effect)
import Pawl.Types.Optionality (Optionality)
import Pawl.Types.SlotName (SlotName)
import Pawl.Types.TargetSpec (TargetSpec)

-- CR 700.2: one mode of a modal spell or ability -- its own effects and its own
-- target namespace. `effects` is a Seq (ordered; CR 608.2c resolves in written
-- order; duplicates allowed). `targetSpecs` is per-mode: CR 601.2c/700.2c fill only
-- the CHOSEN mode's slots. Parametric in `card` like Effect (a concrete Effect Card
-- would cycle with Card, which embeds the payload; Card ties the knot at Mode Card).
-- A non-modal payload is a single Mode.
--
-- `optionality` is CR 603.5's printed "may", covering the whole effect list --
-- see Pawl.Types.Optionality for why the flag rides the mode rather than wrapping
-- effects, and Pawl.Resolve.resolveModes for where the choice is asked. A
-- non-modal card's single mode is its whole instruction list, which is exactly
-- what "you may [everything this ability says]" means.
data Mode card = MkMode
  { effects :: Seq (Effect card),
    targetSpecs :: Map SlotName TargetSpec,
    optionality :: Optionality
  }
  deriving (Eq, Ord, Show)
