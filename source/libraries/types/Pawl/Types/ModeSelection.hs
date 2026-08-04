module Pawl.Types.ModeSelection where

import qualified Numeric.Natural as Natural

-- | CR 700.2: the instruction preceding the bulleted list ("Choose one --"). Only
-- ChooseExactly exists so far, with n = 1 for a charm AND for every non-modal
-- card (one mode, forced). "Choose one or more" (escalate), CR 700.2i's pawprint
-- and CR 700.2d's repeated mode are future constructors. A newtype for now;
-- becomes `data` when a second constructor lands. Do NOT add an hlint ignore.
--
-- CR 702.42a's entwine is NOT one of those, and the distinction is the point:
-- this type is what the card PRINTS, while entwine is a decision made as one
-- particular cast is announced. Pawl.Engine.Cast substitutes Modal.modeCount for
-- that cast alone; nothing rewrites the card.
newtype ModeSelection = ChooseExactly
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
