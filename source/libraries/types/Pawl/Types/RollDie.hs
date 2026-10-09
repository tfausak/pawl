module Pawl.Types.RollDie where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.DiceReading as DiceReading
import qualified Pawl.Types.PlayerScope as PlayerScope
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
-- d6" is a literal. One is the value the codec elides.
--
-- `modifier` is CR 706.2's first sentence -- what the roll's OWN instruction
-- adds to or subtracts from the natural result (Diviner's Portent, "roll a d20
-- and add the number of cards in your hand"). Nothing where the instruction
-- prints none, which is every other roll in data/cards/. Applied to EACH die of
-- the instruction: CR 706.2 words the modifier against "the roll", and no
-- printing pairs a modifier with a count above one.
--
-- `slot` binds the result the roller USES. With one die that is the only result
-- there is; with more, `reading` below says which number it is -- under a
-- choice (CR 706.4, "roll two d6 and choose one result" -- the whole Endeavor
-- cycle) the one Pawl.Types.Prompt's ChooseDieResult asks for. No slot binds
-- the natural result: CR 706.3a's striations and CR 706.4's text both read the result, and the only
-- reader of the natural result in rule 706 is CR 706.2b's reroll step, which
-- Pawl.Engine.Dice.rerollOffers reads internally (Clam-I-Am's "if you roll a
-- 3"), so a second slot would be a capability no card exercises.
--
-- `other` binds "the other result" -- the one result the roller did not choose,
-- for a card that reads both from one instruction (Valiant Endeavor's "create a
-- number of ... tokens equal to the other result"), or the second die's result
-- where `reading` takes each on its own. Meaningful only where the
-- instruction rolled exactly TWO dice, which is the only count any printing
-- words that way; Pawl.CardSpec's lint holds data\/cards\/ to it, and at any
-- other count the slot is left unbound. Bound from the rolls actually made
-- rather than left to the card to re-derive, FlipCoin's `misses` and for its
-- reason. Nothing for every roll that reads one result.
--
-- CR 706.6's ignored roll is not a field here YET: the count of ignored rolls
-- rides Pawl.Types.DiceRoll, where a replacement over the roll can change it,
-- and the only printings of the word in data\/cards\/ are such replacements
-- (Pawl.Types.DieRollRewrite, Pixie Guide). An instruction that prints the
-- ignore itself -- Berserker's Frenzy, "roll two d20 and ignore the lower roll"
-- -- would seed this record's count instead of leaving it at zero. The field
-- appears when that card does.
--
-- `reading` is how `slot` reads the results where the instruction threw more
-- than one: the roller's choice of one (the Endeavor cycle), their total
-- (Neverwinter Hydra's "the total of those results"), which asks nothing and
-- binds zero where no die was thrown, or each of two results on its own
-- (Celebr-8000), which asks nothing either and binds the second die's result
-- at `other`. `other` is meaningful beside the choice and the pair, never the
-- total.
--
-- CR 706.3's results table is NOT a field here and never will be: a striation
-- is a Pawl.Types.Clause of the same mode whose `condition` compares this slot
-- against the striation's range, which is what CR 706.3b's "all part of one
-- ability" already says (Djinni Windseer, Pawl.DiceSpec).
--
-- `roller` is CR 706.1's player the instruction is aimed at: You for every
-- printing but Chaos Dragon's "each player rolls a d20", which is EachPlayer.
-- Each roller throws in APNAP order (CR 101.4) and is the player CR 706.2b's
-- pick and CR 706.4's choice belong to. Where several roll, `slot` binds the
-- highest result any of them used, and `other` is held by Pawl.CardSpec's lint
-- to the one-roller instruction.
--
-- `highest` binds EVERY player whose used result was the highest of the
-- instruction's rollers, ties included, as players for a later clause to name
-- (Chaos Dragon's "those players"). Nothing where no clause reads it.
--
-- `store` is CR 706.8a's "store those results on it": the slot naming the
-- permanent every result the instruction kept is stored on (Centaur of
-- Attention). A storing instruction USES no one result, so CR 706.4's reading
-- is never asked and `slot` binds nothing. Nothing for every other roll.
--
-- Construct with BRACE syntax everywhere: positional construction absorbs a new
-- field in argument order with nothing red (#2009, #2021).
data RollDie = MkRollDie
  { sides :: Natural.Natural,
    count :: Quantity.Quantity,
    modifier :: Maybe Quantity.Quantity,
    reading :: DiceReading.DiceReading,
    slot :: SlotName.SlotName,
    other :: Maybe SlotName.SlotName,
    roller :: PlayerScope.PlayerScope,
    highest :: Maybe SlotName.SlotName,
    store :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)

-- | What an instruction rolling ONE die writes, and the value the codec elides.
defaultCount :: Quantity.Quantity
defaultCount = Quantity.Literal 1

-- | What an instruction that reads no total writes, and the value the codec
-- elides.
defaultReading :: DiceReading.DiceReading
defaultReading = DiceReading.ChooseOne

-- | What an instruction rolled by its controller writes (CR 109.5), and the
-- value the codec elides.
defaultRoller :: PlayerScope.PlayerScope
defaultRoller = PlayerScope.You
