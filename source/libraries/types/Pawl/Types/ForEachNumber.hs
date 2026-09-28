module Pawl.Types.ForEachNumber where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | CR 608.2f's loop over NUMBERS rather than objects: the body run once for
-- each number from 1 up to the quantity, with that number bound under the
-- slot -- Ornate Imitations' "for each number between 1 and X, conjure a
-- duplicate of a random creature card with that mana value".
--
-- Parametric in the effect for Pawl.Types.ForEach's reason.
data ForEachNumber effect = MkForEachNumber
  { -- | The last number, read once before the first iteration; 0 or less runs
    -- nothing.
    upTo :: Quantity.Quantity,
    -- | The name this iteration's number is bound under, for the body to read
    -- as a Quantity.InSlot.
    slot :: SlotName.SlotName,
    -- | The instructions run once per number, in written order (CR 608.2c).
    body :: Seq.Seq effect
  }
  deriving (Eq, Ord, Show)
