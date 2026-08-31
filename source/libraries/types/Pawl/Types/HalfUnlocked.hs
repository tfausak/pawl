module Pawl.Types.HalfUnlocked where

import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 709.5e: a door of a Room was unlocked -- which permanent, who unlocked
-- it, which half, and whether that unlock completed the card.

-- The Bool is CR 709.5i's "fully unlocks", carried rather than re-derived when a
-- trigger is matched: by then a Room that left the battlefield, or whose other
-- door opened in the same settle, would answer about the present rather than
-- about the moment the designation was given.
--
-- It marks the WRITE and not the half, which is the whole of CR 709.5i's second
-- branch: a write that gives both designations at once records one event per
-- designation (CR 709.5h asks each door its own question) and sets this on
-- exactly one of them, so the "has neither designation and gains both" ability
-- triggers once rather than twice. Which of the write's events carries it is
-- unobservable -- Pawl.Engine.Event's RoomFullyUnlocked arm reads the object and
-- the flag and never the half.
--
-- The PLAYER is CR 709.5h/709.5i's "a player unlocks", which a card names as
-- "you" through CR 109.5. Every route that gives a designation supplies one:
-- rule 709.5e's special action has its payer, rule 709.5f's keyword action has
-- the resolving object's controller, and rule 709.5d's entry designation has the
-- controller of the permanent that entered, which is the only player that rule
-- connects to the designation.
data HalfUnlocked = MkHalfUnlocked
  { object :: ObjectId.ObjectId,
    actor :: PlayerId.PlayerId,
    name :: CardName.CardName,
    fully :: Bool
  }
  deriving (Eq, Ord, Show)
