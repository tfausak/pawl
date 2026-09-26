module Pawl.Types.Backup where

import qualified Data.Set as Set
import qualified Numeric.Natural as Natural

-- | The payload of Pawl.Types.Keyword's Backup arm: CR 702.165a's N, and the
-- keywords the card prints ABOVE this backup line, which rule 702.165a's
-- "printed below this one" keeps out of the grant (Saiba Cryptomancer's
-- flash). It rides the keyword, so a copy keeps it (CR 702.165b).
--
-- PARAMETRIC in the keyword for Pawl.Types.Cycling's reason: Keyword names
-- this. Only @Backup Keyword.Keyword@ is ever written.
data Backup keyword = MkBackup
  { count :: Natural.Natural,
    printedAbove :: Set.Set keyword
  }
  deriving (Eq, Ord, Show)
