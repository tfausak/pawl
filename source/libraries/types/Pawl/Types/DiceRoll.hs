module Pawl.Types.DiceRoll where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.PlayerId as PlayerId

-- | The payload of Pawl.Types.ProposedEvent's WouldRollDice arm (CR 706.1 /
-- 614.1a): who rolls, how many dice the instruction throws, and how many of the
-- lowest rolls are then thrown away (CR 706.6).
--
-- A RECORD rather than WouldFlipCoin's bare seat and count, because this event
-- carries TWO numbers of the same type and a caller that transposed them would
-- compile: `dice` is what Pawl.Types.Prompt's RollDie is asked for, and `ignored`
-- is what CR 706.6 discards afterward.
--
-- `roller` is CR 109.5's "you" on the resolving object -- rule 706.1's
-- instruction is aimed at a player and Pawl.Types.RollDie names none of its own.
--
-- `dice` is the number the INSTRUCTION names (CR 706.1), never the number it ends
-- up reading: a row that adds a die leaves CR 616.2's next iteration a
-- differently-numbered instruction, WouldMillCards' currency and for its reason.
--
-- `ignored` is CR 706.6's count of lowest rolls, zero on every instruction that
-- prints no ignore -- which is every roll in data\/cards\/ today, since the only
-- printings of the word are the replacement effects Pawl.Types.DieRollRewrite
-- carries. NOT the dice that were not rolled: rule 706.6 makes an ignored roll one
-- that HAPPENED and is then treated as never having happened, so the die is
-- thrown and its face asked for either way.
--
-- No die SIZE here, although rule 706.1 makes it part of the instruction: nothing
-- that replaces this event may look at it (see Pawl.Types.DieRollR), and an event
-- field no rewrite and no pattern reads is a field this type does not need. It
-- appears when a card needs it.
data DiceRoll = MkDiceRoll
  { roller :: PlayerId.PlayerId,
    dice :: Natural.Natural,
    ignored :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
