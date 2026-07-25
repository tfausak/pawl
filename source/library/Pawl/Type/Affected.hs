module Pawl.Type.Affected where

import Data.Set (Set)
import Pawl.Type.Filter (Filter)
import Pawl.Type.ObjectId (ObjectId)

-- What a continuous effect applies to. CR 611.2c: a resolution effect's set is
-- LOCKED when it begins (TheseObjects -- a bounced-and-returned creature is a new
-- id the effect no longer names). A static ability's set is DYNAMIC (Matching --
-- any object currently matching the Filter), which is why static effects are
-- re-derived each projection, never captured once.
data Affected
  = -- CR 611.2c: a fixed id set, NOT a predicate.
    TheseObjects (Set ObjectId)
  | -- Dynamic: any object matching the Filter, re-derived each projection against
    -- the PARTIAL projection accumulated so far, so it reads each axis as of
    -- whichever layers have already applied (CR 613: layers apply in order) -- a
    -- layer-4 type change is visible to a later layer. CR 305.2's "each other"
    -- (Opalescence does not animate itself) is Filter.Not Filter.IsSource inside
    -- the Filter, not a separate field -- the predicate language already names
    -- the source that way.
    Matching Filter
  deriving (Eq, Ord, Show)
