module Pawl.Types.HalfUnlocked where

import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 709.5e: a door of a Room was unlocked -- which permanent, which half, and
-- whether that unlock completed the card.

-- The Bool is CR 709.5i's "fully unlocks", carried rather than re-derived when a
-- trigger is matched: by then a Room that left the battlefield, or whose other
-- door opened in the same settle, would answer about the present rather than
-- about the moment the designation was given.
data HalfUnlocked = MkHalfUnlocked
  { object :: ObjectId.ObjectId,
    name :: CardName.CardName,
    fully :: Bool
  }
  deriving (Eq, Ord, Show)
