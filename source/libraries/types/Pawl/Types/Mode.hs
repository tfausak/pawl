module Pawl.Types.Mode where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.UnlessPaid as UnlessPaid

-- | CR 700.2: one mode of a modal spell or ability -- its own effects and its own
-- target namespace. `effects` is a Seq (ordered; CR 608.2c resolves in written
-- order; duplicates allowed). `targetSpecs` is per-mode: CR 601.2c/700.2c fill only
-- the CHOSEN mode's slots. Parametric in `card` like Effect (a concrete Effect Card
-- would cycle with Card, which embeds the payload; Card ties the knot at Mode Card).
-- A non-modal payload is a single Mode.
--
-- `optionality` is CR 603.5's printed "may", covering the whole effect list --
-- see Pawl.Types.Optionality for why the flag rides the mode rather than wrapping
-- effects, and Pawl.Engine.Resolve.resolveModes for where the choice is asked. A
-- non-modal card's single mode is its whole instruction list, which is exactly
-- what "you may [everything this ability says]" means.
--
-- `unlessPaid` is CR 118.12a's "unless [a player] pays", riding the mode for
-- that same reason and covering the same span: it is the rewriting of that rule
-- read onto this effect list, so the effects are the "if they don't" branch.
-- Nothing for every card that states no such cost. The two gates are
-- independent, and Pawl.Engine.Resolve asks them in printed order -- the "may"
-- first, since a declined mode has no instruction left for an "unless" to
-- qualify.
data Mode card = MkMode
  { effects :: Seq.Seq (Effect.Effect card),
    targetSpecs :: Map.Map SlotName.SlotName TargetSpec.TargetSpec,
    optionality :: Optionality.Optionality,
    unlessPaid :: Maybe UnlessPaid.UnlessPaid
  }
  deriving (Eq, Ord, Show)
