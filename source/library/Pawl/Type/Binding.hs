module Pawl.Type.Binding where

import Data.Set (Set)
import Numeric.Natural (Natural)
import Pawl.Type.ModeIndex (ModeIndex)
import Pawl.Type.Recipient (Recipient)
import Pawl.Type.Subtype (Subtype)

-- CR 601.2: the cast-time choices bound to one named slot of a spell or ability
-- on the stack. A record, not a sum, because one slot may carry several kinds of
-- choice at once -- Magical Hack's slot is both TARGETED (a Recipient) and
-- WORD-SWAPPED (a Subtype pair). A field per binding kind; a kind absent for this
-- slot is Nothing. Grows a field per future binding (a mode, a for-each count).
data Binding = MkBinding
  { -- CR 601.2c: the chosen target for this slot; re-validated at CR 608.2b.
    target :: Maybe Recipient,
    -- CR 612: the (from, to) basic land types chosen for a text-changing slot.
    subtypes :: Maybe (Subtype, Subtype),
    -- CR 601.2b: the value chosen for a variable in the cost (X). Read by
    -- Quantity.evaluate. Nothing for a slot with no amount.
    amount :: Maybe Natural,
    -- CR 700.2 / 601.2b: the modes chosen for a modal spell, by index. A Set: no
    -- duplicate modes (CR 700.2d "same mode more than once" is future), and Set's
    -- ordering IS printed order (CR 608.2c), so resolution reads them pre-sorted.
    -- Stored only under the reserved Binding.chosenModes slot. Nothing elsewhere.
    modes :: Maybe (Set ModeIndex)
  }
  deriving (Eq, Ord, Show)

-- The empty binding: no choice of any kind. The unit for merging.
empty :: Binding
empty = MkBinding {target = Nothing, subtypes = Nothing, amount = Nothing, modes = Nothing}
