module Pawl.Types.SacrificeAnyNumber where

import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | CR 614.1c's as-enters sacrifice: which permanents may be sacrificed, and
-- which counter kind the entering permanent takes one of per sacrifice.
data SacrificeAnyNumber = MkSacrificeAnyNumber
  { filter :: Filter.Filter Keyword.Keyword,
    -- | Nothing for a rewrite that places no counters -- the sacrifice is the
    -- whole of what the card asks.
    kind :: Maybe (CounterKind.CounterKind Keyword.Keyword)
  }
  deriving (Eq, Ord, Show)
