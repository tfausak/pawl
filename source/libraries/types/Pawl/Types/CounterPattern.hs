module Pawl.Types.CounterPattern where

import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Filter as Filter

-- | CR 122.6 / 614.1: which counter placements a scaling replacement intercepts.
-- Hardened Scales is (Just PlusOnePlusOne, Yours, "HasCardType Creature");
-- Doubling Season's counter clause is (Nothing, Yours, "And []" -- the trivial
-- filter matching every permanent). `whichKind = Nothing` means ANY kind, never
-- "no kind" -- the two cards differ by data, and neither is a constructor.
data CounterPattern = MkCounterPattern
  { whichKind :: Maybe CounterKind.CounterKind,
    whose :: ControllerRelation.ControllerRelation,
    onWhat :: Filter.Filter
  }
  deriving (Eq, Ord, Show)
