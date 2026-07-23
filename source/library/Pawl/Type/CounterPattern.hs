module Pawl.Type.CounterPattern where

import Pawl.Type.ControllerRelation (ControllerRelation)
import Pawl.Type.CounterKind (CounterKind)
import Pawl.Type.Filter (Filter)

-- CR 122.6 / 614.1: which counter placements a scaling replacement intercepts.
-- Hardened Scales is (Just PlusOnePlusOne, Yours, "HasCardType Creature");
-- Doubling Season's counter clause is (Nothing, Yours, "And []" -- the trivial
-- filter matching every permanent). `whichKind = Nothing` means ANY kind, never
-- "no kind" -- the two cards differ by data, and neither is a constructor.
data CounterPattern = MkCounterPattern
  { whichKind :: Maybe CounterKind,
    whose :: ControllerRelation,
    onWhat :: Filter
  }
  deriving (Eq, Ord, Show)
