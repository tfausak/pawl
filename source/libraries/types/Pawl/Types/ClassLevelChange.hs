module Pawl.Types.ClassLevelChange where

import qualified Pawl.Types.ClassLevel as ClassLevel
import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 716.2a: a permanent's class level BECAME something, with the level before
-- and the level after.

-- The pair rather than the new level alone, for
-- Pawl.Types.CounterChange's reason: "when this Class becomes level N" asks
-- whether the level was below N and reached N -- a THRESHOLD CROSSING that the
-- resulting level alone cannot answer, since a Class already at level 3 whose
-- level is set to 3 again became nothing.
--
-- Both halves are a ClassLevel rather than a Maybe: CR 716.2d reads a permanent
-- with no level as level 1, so `before` is the DEFAULTED level and "has no level"
-- has no separate meaning to a trigger. Object.classLevel keeps the Maybe,
-- because "has never been levelled" is still a distinct state of the permanent.
--
-- BOTH levels are a ClassLevel, which is why they are named: a swap would invert
-- every crossing this exists to answer.
data ClassLevelChange = MkClassLevelChange
  { object :: ObjectId.ObjectId,
    before :: ClassLevel.ClassLevel,
    after :: ClassLevel.ClassLevel
  }
  deriving (Eq, Ord, Show)
