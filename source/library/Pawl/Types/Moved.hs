module Pawl.Types.Moved where

import qualified Pawl.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Types.ZoneChange as ZoneChange

-- | CR 400.7's zone change, plus the moving object's characteristics as of the
-- move. The characteristics are stamped because CR 608.2h's last-known
-- information is gone by the time a trigger reads them.
data Moved = MkMoved
  { change :: ZoneChange.ZoneChange,
    characteristics :: ProjectedCharacteristics.ProjectedCharacteristics
  }
  deriving (Eq, Ord, Show)
