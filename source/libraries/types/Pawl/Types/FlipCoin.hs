module Pawl.Types.FlipCoin where

import qualified Pawl.Types.CoinReading as CoinReading
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's FlipCoin arm (CR 705.1): how many coins
-- one instruction flips, which of CR 705.2's two kinds of flip they are, and the
-- slot the tally is bound at for a later effect of the same resolution to read
-- as Pawl.Types.Quantity's InSlot.
--
-- `count` is a Quantity because the printed instructions are not all numerals:
-- Ral Zarek's "flip five coins" is a literal, Flock of Rabid Sheep's "flip X
-- coins" is the announced X, and Mutalith Vortex Beast's "flip a coin for each
-- opponent you have" is a count. Nothing in rule 705 bounds it. One is the value
-- the codec elides, which is every flip in data/cards/ but the Flock's.
--
-- `reading` picks which of CR 705.2's two kinds this is, and so what `slot`
-- counts: how many of the flips the flipper WON, or how many coins came up
-- HEADS. One field beside one slot rather than a slot apiece, because the rule's
-- two kinds are exclusive -- see Pawl.Types.CoinReading.
--
-- `slot` binds a COUNT and not a yes-or-no, which for the one-coin instruction
-- Winter Sky prints is the 1 or 0 this record used to bind. Quantity's
-- vocabulary is numeric -- IsMonarch, IsStartingPlayer and IsActivePlayer each
-- say so at their own sites -- so a card asking "did I win" arrives there as a
-- comparison against a 0 or a 1, and no new Quantity arm is owed.
--
-- CR 705.2's first-sentence flip also has an ENTRY-REWRITE shape, which does not
-- come here at all: Pawl.Types.EntryRewrite's ChoiceByCoinFlip asks
-- Prompt.FlipCoin as a permanent enters and consumes the face on the spot, where
-- this opcode binds a tally for a later effect to read.
--
-- Construct with BRACE syntax everywhere: positional construction absorbs a new
-- field in argument order with nothing red (#2009, #2021).
data FlipCoin = MkFlipCoin
  { count :: Quantity.Quantity,
    reading :: CoinReading.CoinReading,
    slot :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)

-- | What an instruction flipping ONE coin writes, and the value the codec
-- elides.
defaultCount :: Quantity.Quantity
defaultCount = Quantity.Literal 1

-- | What CR 705.2's win\/lose flip writes, and the value the codec elides.
defaultReading :: CoinReading.CoinReading
defaultReading = CoinReading.Wins
