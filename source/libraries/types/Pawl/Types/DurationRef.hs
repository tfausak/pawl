module Pawl.Types.DurationRef where

import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | "These objects, for this long" -- the payload shared by Pawl.Types.Effect's
-- PreventAllDamage, GainControl and GrantPlayFromExile arms (#1305).
--
-- SHARED FOR EXPEDIENCY, and not because those three mean the same thing. The
-- name says the shape rather than a concept, precisely because there is no shared
-- concept: preventing damage, gaining control and granting permission coincide in
-- what they must be told, and nothing more.
--
-- WHEN ONE SHARER NEEDS A FIELD THE OTHERS DO NOT, SPIN OUT A SEPARATE TYPE FOR
-- IT. Never bolt an optional field on here for one arm's sake -- a record
-- carrying a Maybe for exactly one of its users has become an untagged union,
-- since the field's absence would be how a reader tells the arms apart.
data DurationRef = MkDurationRef
  { duration :: Duration.Duration,
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
