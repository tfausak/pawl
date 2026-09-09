module Pawl.Types.TappedForMana where

import qualified Data.Set as Set
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 106.12a's event: which permanent was tapped for mana, and which types of
-- mana that activation produced. The payload of Pawl.Types.GameEvent's arm of
-- the same name.
--
-- The TYPES are here because CR 106.12a's second half asks for them -- "or is
-- tapped for mana of a specified type ... triggers whenever such a mana ability
-- resolves and produces ... the specified type of mana" -- and nothing on the
-- board can answer for them afterwards: the mana is a Pawl.Types.ManaUnit in
-- some pool, carrying no reference to the source that made it, and it may
-- already have been spent.
--
-- A SET and not the yield's own list: the rule asks which types were produced,
-- so Sol Ring's "{T}: Add {C}{C}" is answered by the one type it made, and the
-- amount is nothing CR 106.12a's trigger reads. CR 106.12b's replacement is the
-- rule that reads an amount ("tapped for mana of a specific type and/or
-- amount"), and it is a different sentence about a different kind of effect.
data TappedForMana = MkTappedForMana
  { permanent :: ObjectId.ObjectId,
    mana :: Set.Set ManaType.ManaType
  }
  deriving (Eq, Ord, Show)
