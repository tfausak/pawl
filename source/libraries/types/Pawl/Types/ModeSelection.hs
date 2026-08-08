module Pawl.Types.ModeSelection where

import qualified Numeric.Natural as Natural

-- | CR 700.2: the instruction preceding the bulleted list ("Choose one --"),
-- carrying both halves of CR 700.2d. That rule states a default and an exception:
-- "If a player is allowed to choose more than one mode for a modal spell or
-- ability, that player normally can't choose the same mode more than once.
-- However, some modal spells include the instruction 'You may choose the same
-- mode more than once.'" The two constructors are those two sentences --
-- 'ChooseExactly' is the default, 'ChooseExactlyWithRepeats' the printed
-- exception (Mystic Confluence). Both carry n = how many modes are chosen, with
-- n = 1 for a charm AND for every non-modal card (one mode, forced).
--
-- "Choose one or more" (escalate) and CR 700.2i's pawprint are future
-- constructors.
--
-- Two constructors rather than a count plus a flag, because that is what keeps
-- the card data untouched: a repeat-free selection still encodes as it always
-- did, so adding the exception rewrote no card file.
--
-- CR 702.42a's entwine is NOT one of those, and the distinction is the point:
-- this type is what the card PRINTS, while entwine is a decision made as one
-- particular cast is announced. Pawl.Engine.Cast substitutes Modal.modeCount for
-- that cast alone; nothing rewrites the card.
data ModeSelection
  = ChooseExactly Natural.Natural
  | ChooseExactlyWithRepeats Natural.Natural
  deriving (Eq, Ord, Show)
