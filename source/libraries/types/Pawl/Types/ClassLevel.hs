module Pawl.Types.ClassLevel where

import qualified Numeric.Natural as Natural

-- | CR 716.2b: the level designation a permanent has. "A level is a designation
-- that any permanent can have. A Class retains its level even if it stops being a
-- Class. Levels are not a copiable characteristic."
--
-- A designation and NOT a counter, which is the whole of CR 716.4's separation
-- from rule 711's levelers: a level counter is
-- Pawl.Types.CounterKind's Level arm living in Object.counters, and CR 716.4 says
-- the two do not interact, so they may not share a field.
--
-- A newtype over Natural, not a bare one, for Pawl.Types.RoomIndex's reason: a
-- level is a position on a card's printed ladder and must not be confusable with
-- a count of anything.
--
-- CR 716.2d -- "if a rule or effect refers to a permanent's level and that
-- permanent doesn't have a level, it is treated as though its level is 1" -- is
-- `defaulted` below rather than an initial value, so Object.classLevel stays a
-- Maybe and "has no level" keeps its own representation.
newtype ClassLevel = MkClassLevel
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)

-- | CR 716.2d's reader: the level of a permanent that may not have one.
defaulted :: Maybe ClassLevel -> Natural.Natural
defaulted = maybe 1 unwrap
