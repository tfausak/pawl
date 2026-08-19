module Pawl.Types.Fight where

import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's Fight arm.
--
-- TWO SlotNames and no ObjectRef, because CR 701.14a's subject is a pair and
-- never a set: "a spell or ability may instruct a creature to fight ANOTHER
-- creature or it may instruct TWO creatures to fight each other". Both halves of
-- that sentence are two slots; the first is Prey Upon's, and the second is
-- Arena's "those creatures fight each other". A slot need not be a TARGET slot:
-- Nightfall Predator's "this creature fights target creature" names CR 113.7's
-- reserved `self` in one and a target in the other, which is also how the two
-- can name ONE permanent (CR 701.14c).
--
-- Not an ordered pair in any rules sense -- CR 701.14a's damage is simultaneous
-- and symmetric -- so `first` and `second` name the card's own two clauses rather
-- than a sequence.
data Fight = MkFight
  { first :: SlotName.SlotName,
    second :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
