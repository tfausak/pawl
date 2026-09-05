module Pawl.Types.RollDie where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's RollDie arm (CR 706.1): which die to
-- roll and how many of them, what the instruction itself adds to the natural
-- result (CR 706.2), and the slots the results are bound at for a later effect
-- of the same resolution to read as Pawl.Types.Quantity's InSlot (CR 706.4).
--
-- `sides` is CR 706.1a's N -- a dN has N equally likely outcomes numbered 1 to
-- N -- so it is the whole description of the die, and the range the ANSWER is
-- filtered back against. CR 706.2 adds the modifier afterwards, and no rule
-- bounds the sum, so a d20 answered 20 with a modifier of 5 is a result of 25.
--
-- `count` is CR 706.1's other half, how many of those dice one instruction
-- throws. A Quantity rather than a numeral for FlipCoin's reason: Neverwinter
-- Hydra's "roll X dice" is the announced X where Valiant Endeavor's "roll two
-- d6" is a literal. One is the value the codec elides, which is every roll in
-- data\/cards\/ but the Endeavor's.
--
-- `modifier` is CR 706.2's first sentence -- what the roll's OWN instruction
-- adds to or subtracts from the natural result (Diviner's Portent, "roll a d20
-- and add the number of cards in your hand"). Nothing where the instruction
-- prints none, which is every other roll in data/cards/. Applied to EACH die of
-- the instruction: CR 706.2 words the modifier against "the roll", and no
-- printing pairs a modifier with a count above one.
--
-- `slot` binds the result the roller USES. With one die that is the only result
-- there is; with more the roller chooses among them (CR 706.4, "roll two d6 and
-- choose one result" -- the whole Endeavor cycle), which is what
-- Pawl.Types.Prompt's ChooseDieResult asks. Not implemented: a binding for the
-- natural result (#2083). CR 706.3a's striations and CR 706.4's text both read
-- the result, and the only reader of the natural result anywhere in rule 706 is
-- CR 706.2b's competing modifiers, which pawl does not build, so a second slot
-- would be a capability no card exercises.
--
-- `other` binds "the other result" -- the one result the roller did not choose,
-- for a card that reads both from one instruction (Valiant Endeavor's "create a
-- number of ... tokens equal to the other result"). Meaningful only where the
-- instruction rolled exactly TWO dice, which is the only count any printing
-- words that way; Pawl.CardSpec's lint holds data\/cards\/ to it, and at any
-- other count the slot is left unbound. Bound from the rolls actually made
-- rather than left to the card to re-derive, FlipCoin's `misses` and for its
-- reason. Nothing for every roll that reads one result.
--
-- Not implemented: the readings that take the results as a SET rather than one
-- at a time -- CR 706.5's doubles and a total (#3243) -- and CR 706.6's ignored
-- roll (#2083).
--
-- CR 706.3's results table is NOT a field here and never will be: a striation
-- is a Pawl.Types.Clause of the same mode whose `condition` compares this slot
-- against the striation's range, which is what CR 706.3b's "all part of one
-- ability" already says (Djinni Windseer, Pawl.DiceSpec).
--
-- Construct with BRACE syntax everywhere: positional construction absorbs a new
-- field in argument order with nothing red (#2009, #2021).
data RollDie = MkRollDie
  { sides :: Natural.Natural,
    count :: Quantity.Quantity,
    modifier :: Maybe Quantity.Quantity,
    slot :: SlotName.SlotName,
    other :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)

-- | What an instruction rolling ONE die writes, and the value the codec elides.
defaultCount :: Quantity.Quantity
defaultCount = Quantity.Literal 1
