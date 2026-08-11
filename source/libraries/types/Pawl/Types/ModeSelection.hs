module Pawl.Types.ModeSelection where

import qualified Numeric.Natural as Natural

-- | CR 700.2: the instruction preceding the bulleted list ("Choose one --"),
-- carrying both halves of CR 700.2d. That rule states a default and an exception:
-- "If a player is allowed to choose more than one mode for a modal spell or
-- ability, that player normally can't choose the same mode more than once.
-- However, some modal spells include the instruction 'You may choose the same
-- mode more than once.'" The first two constructors are those two sentences --
-- 'ChooseExactly' is the default, 'ChooseExactlyWithRepeats' the printed
-- exception (Mystic Confluence). Both carry n = how many modes are chosen, with
-- n = 1 for a charm AND for every non-modal card (one mode, forced).
--
-- 'ChooseBetween' is the third printed shape: an instruction naming a RANGE
-- rather than a number, `least` then `most`, both inclusive. "Choose one or
-- both --" over two modes (Vandalize) is 1 to 2, and CR 702.120a's escalate
-- ("Choose one or more --") is 1 to the printed mode count. Repeat-free: no
-- printing pairs a range with CR 700.2d's exception, which is why there is no
-- fourth constructor and why Pawl.Engine.Modal may read `most` where it means
-- "the count" on the repeating arm.
--
-- Not implemented: escalate's OTHER half, CR 702.120a's cost per mode beyond the
-- first, so no escalate card can be written even though this states its
-- instruction (#1258).
--
-- CR 700.2i's pawprint is a future constructor: it bounds a WEIGHTED total
-- rather than a number of modes, so no range over mode counts states it.
--
-- Constructors rather than a count plus a flag plus a bound, because that is what
-- keeps the card data untouched: a repeat-free exact selection still encodes as it
-- always did, so neither the exception nor the range rewrote a card file.
--
-- `least <= most` is an invariant nothing here maintains: a selection is card
-- DATA, so Pawl.Codec.ModeSelection rejects a range that breaks it. Not a safety
-- property -- an impossible range makes the spell uncastable rather than making
-- anything crash.
--
-- CR 702.42a's entwine is NOT one of these, and the distinction is the point:
-- this type is what the card PRINTS, while entwine is a decision made as one
-- particular cast is announced. Pawl.Engine.Cast substitutes Modal.modeCount for
-- that cast alone; nothing rewrites the card.
data ModeSelection
  = ChooseExactly Natural.Natural
  | ChooseExactlyWithRepeats Natural.Natural
  | ChooseBetween Natural.Natural Natural.Natural
  deriving (Eq, Ord, Show)
