module Pawl.Types.CounterPattern where

import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | CR 122.6 / 614.1: which counter placements a scaling replacement intercepts.
-- Hardened Scales is (Just PlusOnePlusOne, Yours, "HasCardType Creature");
-- Doubling Season's counter clause is (Nothing, Yours, "And []" -- the trivial
-- filter matching every permanent). `whichKind = Nothing` means ANY kind, never
-- "no kind" -- the two cards differ by data, and neither is a constructor.
data CounterPattern = MkCounterPattern
  { whichKind :: Maybe (CounterKind.CounterKind Keyword.Keyword),
    whose :: ControllerRelation.ControllerRelation,
    onWhat :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
