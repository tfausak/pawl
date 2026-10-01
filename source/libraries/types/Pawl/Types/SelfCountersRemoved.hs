module Pawl.Types.SelfCountersRemoved where

import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Zone as Zone

-- | A counter removal from the ability's own object: which counter kind, and
-- the zone the ability functions from.
data SelfCountersRemoved = MkSelfCountersRemoved
  { kind :: CounterKind.CounterKind Keyword.Keyword,
    -- | CR 113.6b: the zone the ability states it functions from (Benalish
    -- Commander's "while it's exiled"); the battlefield is CR 113.6's default.
    zone :: Zone.Zone
  }
  deriving (Eq, Ord, Show)
