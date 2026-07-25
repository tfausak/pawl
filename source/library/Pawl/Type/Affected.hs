module Pawl.Type.Affected where

import Data.Set (Set)
import Pawl.Type.Filter (Filter)
import Pawl.Type.ObjectId (ObjectId)

-- What a continuous effect applies to. CR 611.2c: a resolution effect's set is
-- LOCKED when it begins (TheseObjects -- a bounced-and-returned creature is a new
-- id the effect no longer names). A static ability's set is DYNAMIC, of which
-- there are two kinds: Matching (any object currently matching a Filter) and
-- Attached (the one object the source is attached to, if any) -- both are
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
  | -- CR 303.4m: the object this ability's SOURCE is attached to -- "enchanted
    -- creature". A THIRD kind of affected set: TheseObjects is fixed at
    -- resolution (CR 611.2c) and Matching is a predicate re-derived per
    -- candidate, while this is re-derived from the SOURCE's own state.
    --
    -- The set is {o} when the source is attached to o, and EMPTY when it is
    -- unattached -- an Aura in the graveyard, or one the CR 704.5m sweep has not
    -- reached yet. Payload-free: CR 303.4m defines it for any permanent, "even
    -- if the permanent with the ability isn't an Aura", so there is nothing to
    -- parameterize.
    Attached
  deriving (Eq, Ord, Show)
