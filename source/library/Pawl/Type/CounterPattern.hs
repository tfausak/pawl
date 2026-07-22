module Pawl.Type.CounterPattern where

import Pawl.Type.ControllerRelation (ControllerRelation)
import Pawl.Type.CounterKind (CounterKind)
import Pawl.Type.PermanentCriterion (PermanentCriterion)

-- CR 122.6 / 614.1: which counter placements a scaling replacement intercepts.
-- Hardened Scales is (Just PlusOnePlusOne, Yours, CreaturePermanent); Doubling
-- Season's counter clause is (Nothing, Yours, AnyPermanent). `whichKind =
-- Nothing` means ANY kind, never "no kind" -- the two cards differ by data, and
-- neither is a constructor.
data CounterPattern = MkCounterPattern
  { whichKind :: Maybe CounterKind,
    whose :: ControllerRelation,
    onWhat :: PermanentCriterion
  }
  deriving (Eq, Ord, Show)
