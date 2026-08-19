module Pawl.Types.DurationRef where

import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | "These objects, for this long" -- the payload of Pawl.Types.Effect's
-- GainControl arm, and today its only one (#1305).
--
-- The name says the shape rather than a concept, because there never was a
-- shared concept: it was shared FOR EXPEDIENCY, by arms that coincided in what
-- they must be told and nothing more. Both the others have since taken the
-- instruction below -- GrantPlayFromExile when CR 118.14's rider arrived, then
-- PreventAllDamage when CR 615.5's did.
--
-- WHEN ONE SHARER NEEDS A FIELD THE OTHERS DO NOT, SPIN OUT A SEPARATE TYPE FOR
-- IT. Never bolt an optional field on here for one arm's sake -- a record
-- carrying a Maybe for exactly one of its users has become an untagged union,
-- since the field's absence would be how a reader tells the arms apart. That is
-- what GrantPlayFromExile did when CR 118.14's rider arrived, and what
-- PreventAllDamage did when CR 615.5's did: each shared this payload until it
-- needed a third field, and each is now its own type.
data DurationRef = MkDurationRef
  { duration :: Duration.Duration,
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
