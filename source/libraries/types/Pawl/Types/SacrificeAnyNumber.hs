module Pawl.Types.SacrificeAnyNumber where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | CR 614.1c's as-enters sacrifice: which permanents may be sacrificed, and how
-- many counters of which kind the entering permanent takes per sacrifice.
data SacrificeAnyNumber = MkSacrificeAnyNumber
  { filter :: Filter.Filter Keyword.Keyword,
    -- | Nothing for a rewrite that places no counters -- the sacrifice is the
    -- whole of what the card asks.
    kind :: Maybe (CounterKind.CounterKind Keyword.Keyword),
    -- | CR 702.82a's devour N: how many counters of `kind` each sacrificed
    -- permanent buys. 1 where the card prints "that many" (Shimatsu the
    -- Bloodcloaked), and inert where `kind` is Nothing, nothing being placed
    -- (Wood Elemental).
    --
    -- A Natural: rule 702.82a's N is the keyword's own number, read before the
    -- choice is made. Pawl.Engine.Event multiplies by the count, so 0 places
    -- nothing.
    --
    -- Not implemented: an N that names the sacrifice's own count -- Thromok the
    -- Insatiable's "devour X, where X is the number of creatures devoured this
    -- way" (#3600).
    each :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
