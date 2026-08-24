module Pawl.Types.RollDie where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's RollDie arm (CR 706.1): which die to
-- roll, what the instruction itself adds to the natural result (CR 706.2), and
-- the slot the result is bound at for a later effect of the same resolution to
-- read as Pawl.Types.Quantity's InSlot (CR 706.4).
--
-- `sides` is CR 706.1a's N -- a dN has N equally likely outcomes numbered 1 to
-- N -- so it is the whole description of the die, and the range the ANSWER is
-- filtered back against. CR 706.2 adds the modifier afterwards, and no rule
-- bounds the sum, so a d20 answered 20 with a modifier of 5 is a result of 25.
--
-- `modifier` is CR 706.2's first sentence: "the instruction may include
-- modifiers to the roll which add to or subtract from the natural result"
-- (Diviner's Portent, "roll a d20 and add the number of cards in your hand").
-- Nothing where the instruction prints none, which is every other roll in the
-- pool.
--
-- `slot` binds the RESULT, not the natural result. Not implemented: a binding
-- for the natural result (#2083). CR 706.3a's striations and CR 706.4's text
-- both read the result, and the only reader of the natural result anywhere in
-- rule 706 is CR 706.2b's competing modifiers, which pawl does not build, so a
-- second slot would be a capability no card exercises.
--
-- CR 706.3's results table is NOT a field here and never will be: a striation
-- is a Pawl.Types.Clause of the same mode whose `condition` compares this slot
-- against the striation's range, which is what CR 706.3b's "all part of one
-- ability" already says (Djinni Windseer, Pawl.DiceSpec).
--
-- Construct with BRACE syntax everywhere. A count added later (#2085) is a new
-- field, and positional construction absorbs a new field in argument order with
-- nothing red (#2009, #2021).
data RollDie = MkRollDie
  { sides :: Natural.Natural,
    modifier :: Maybe Quantity.Quantity,
    slot :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
