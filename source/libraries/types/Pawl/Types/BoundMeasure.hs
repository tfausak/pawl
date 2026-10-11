module Pawl.Types.BoundMeasure where

import qualified Pawl.Types.Measure as Measure
import qualified Pawl.Types.SlotName as SlotName

-- | CR 608.2c: a measure of the one object an earlier part of the same
-- resolution bound at a slot -- Profaner of the Dead's "the exploited
-- creature's toughness".
data BoundMeasure = MkBoundMeasure
  { slot :: SlotName.SlotName,
    measure :: Measure.Measure
  }
  deriving (Eq, Ord, Show)
