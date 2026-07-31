module Pawl.Types.ModeSelection where

import Numeric.Natural (Natural)

-- CR 700.2: the instruction preceding the bulleted list ("Choose one --"). A sum,
-- not a bare Natural, so it grows without primitive blindness. Only ChooseExactly
-- exists at M4g: n = 1 for a charm AND for every non-modal card (one mode, forced).
-- "Choose two"/commands are ChooseExactly 2; "choose one or more" (escalate),
-- pawprint "worth of modes" (CR 700.2i), and "same mode more than once" (CR 700.2d)
-- are future constructors. A newtype for now; becomes `data` when a second
-- constructor (e.g. ChooseAtLeast, escalate/pawprint per CR 700.2d/700.2i) lands.
-- Do NOT add an hlint ignore.
--
-- CR 702.42a's entwine is NOT one of those future constructors, and the
-- distinction is the point: this type is what the card PRINTS -- Dream's Grip
-- still says "Choose one --", ChooseExactly 1 -- while entwine is a decision
-- made as one particular cast is announced. Pawl.Engine.Cast substitutes
-- Modal.modeCount for this count for that cast alone; nothing rewrites the card.
newtype ModeSelection
  = ChooseExactly Natural
  deriving (Eq, Ord, Show)
