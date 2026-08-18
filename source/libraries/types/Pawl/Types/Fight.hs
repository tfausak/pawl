module Pawl.Types.Fight where

import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's Fight arm.
--
-- TWO SlotNames and no ObjectRef, because CR 701.14a's subject is a pair and
-- never a set: "a spell or ability may instruct a creature to fight ANOTHER
-- creature or it may instruct TWO creatures to fight each other". Both halves of
-- that sentence are two slots; the first is Prey Upon's, and the second is
-- Arena's "those creatures fight each other".
--
-- Not an ordered pair in any rules sense -- CR 701.14a's damage is simultaneous
-- and symmetric -- so `first` and `second` name the card's own two clauses rather
-- than a sequence.
data Fight = MkFight
  { first :: SlotName.SlotName,
    second :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
