module Pawl.Types.Mode where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TargetSpec as TargetSpec

-- | CR 700.2: one mode of a modal spell or ability -- its own clauses and its own
-- target namespace. `clauses` is a Seq (ordered; CR 608.2c resolves in written
-- order; duplicates allowed), and CR 608.2e is what makes the clause the unit
-- inside a mode: "multiple steps or actions, denoted by separate sentences or
-- clauses". `targetSpecs` is per-mode: CR 601.2c/700.2c fill only
-- the CHOSEN mode's slots. Parametric in `card` like Effect (a concrete Effect Card
-- would cycle with Card, which embeds the payload; Card ties the knot at Mode Card).
-- A non-modal payload is a single Mode.
--
-- The MODE is the unit fixed as the spell is CAST and the CLAUSE the unit
-- decided as the effect is APPLIED, which is the split this type and
-- Pawl.Types.Clause exist to keep: CR 601.2b/700.2b choose the modes and CR
-- 601.2c the targets, never revisited, while CR 608.2d announces the rest "while
-- applying the effect". Both resolution-time riders -- CR 603.5's printed "may"
-- and CR 118.12a's "unless [a player] pays" -- therefore live on the clause.
-- They rode the mode until a card needed a "may" narrower than one (#335).
data Mode card = MkMode
  { clauses :: Seq.Seq (Clause.Clause card),
    targetSpecs :: Map.Map SlotName.SlotName TargetSpec.TargetSpec
  }
  deriving (Eq, Ord, Show)

-- Every effect this mode holds, in printed order (CR 608.2c), with the clause
-- boundaries dropped. For the readers that legitimately want the flat list: the
-- static analyses (the slot lints, CR 612's text rewriting) ask what the mode
-- SAYS, and a clause boundary is a resolution-time question they never pose.
allEffects :: Mode card -> Seq.Seq (Effect.Effect card)
allEffects = foldMap Clause.effects . clauses
