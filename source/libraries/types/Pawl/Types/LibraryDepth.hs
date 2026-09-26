module Pawl.Types.LibraryDepth where

import qualified Numeric.Natural as Natural

-- | How far down a library a conjured card lands, counted from the top (CR
-- 401.2's ordered pile). Conjure is digital-only and in no rule of the CR, so
-- the authority is the printed sentence.
--
-- A depth rather than a 'Pawl.Types.LibraryPosition.LibraryPosition' end: that
-- type is two-valued by design, and "seventh from the top" is neither end. The
-- top itself is @FromTop 1@.
--
-- A depth past the bottom lands on the bottom: Seq.insertAt's clamp, and
-- Approach of the Second Sun's printed ruling for the same "seventh from the
-- top" wording.
data LibraryDepth
  = -- | Calim, Djinn Emperor\'s "into your library seventh from the top": the
    -- card is this many cards down, 1 being the top.
    FromTop Natural.Natural
  | -- | Mine Security\'s "into the top eight cards of your library at random":
    -- randomness names one of the top this-many places (Prompt.RandomDepth).
    AtRandomInTop Natural.Natural
  deriving (Eq, Ord, Show)
