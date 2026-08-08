module Pawl.Types.Clause where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.Effect as Effect

-- | CR 608.2e's own unit: one of the "multiple steps or actions, denoted by
-- separate sentences or clauses" a spell or ability may have. `effects` is a Seq
-- because CR 608.2c resolves in written order and duplicates are allowed.
--
-- The clause is what a resolution-time rider covers, and the MODE is what a
-- cast-time choice covers. That split is the whole point of this type: CR
-- 601.2b/700.2b fix the modes and CR 601.2c the targets as the spell is cast and
-- never revisit them, while CR 608.2d announces the remaining choices "while
-- applying the effect". Pawl.Types.Mode carried both jobs until a card needed
-- them apart (#335).
--
-- No `targetSpecs` here, and that asymmetry is the design rather than an
-- omission: CR 601.2c fills a target slot as the spell is cast, which is the
-- mode's business, so a clause has no namespace of its own.
--
-- Parametric in `card` for Pawl.Types.Effect's reason -- Card embeds the
-- payload, so a concrete `Effect Card` here would make the two modules mutually
-- importing.
newtype Clause card = MkClause
  { effects :: Seq.Seq (Effect.Effect card)
  }
  deriving (Eq, Ord, Show)
