module Pawl.Types.DurationRef where

import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | "These objects, for this long" -- the payload shared by Pawl.Types.Effect's
-- PreventAllDamage and GainControl arms (#1305).
--
-- SHARED FOR EXPEDIENCY, and not because the two mean the same thing. The name
-- says the shape rather than a concept, precisely because there is no shared
-- concept: preventing damage and gaining control coincide in what they must be
-- told, and nothing more.
--
-- WHEN ONE SHARER NEEDS A FIELD THE OTHERS DO NOT, SPIN OUT A SEPARATE TYPE FOR
-- IT. Never bolt an optional field on here for one arm's sake -- a record
-- carrying a Maybe for exactly one of its users has become an untagged union,
-- since the field's absence would be how a reader tells the arms apart. That is
-- what GrantPlayFromExile did when CR 118.14's rider arrived: it shared this
-- payload until it needed a third field, and Pawl.Types.GrantPlayFromExile is
-- now its own type.
data DurationRef = MkDurationRef
  { duration :: Duration.Duration,
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
