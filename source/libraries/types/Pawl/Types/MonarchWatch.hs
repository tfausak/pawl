module Pawl.Types.MonarchWatch where

import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 725 (Palace Jailer): one object exiled "until an opponent becomes the
-- monarch", and what the game must remember to know when that has happened.
--
-- The card's ruling makes this an EVENT, not a state. "If you're not the monarch
-- as Palace Jailer's second ability resolves, the creature will be exiled until
-- there's a new monarch and that player is one of your opponents. The creature
-- won't immediately return just because an opponent is the monarch." Its
-- companion ruling says the same from the other side: "The game will continue to
-- watch for the next time an opponent becomes the monarch."
--
-- So "an opponent holds the crown" is not the question -- that can already be
-- true when the exile resolves, and the creature stays exiled anyway. The
-- question is whether the crown has CHANGED HANDS to an opponent since.
data MonarchWatch = MkMonarchWatch
  { -- | The effect's controller. Whose opponents are being watched for -- fixed
    -- when the exile resolves, and it outlives both its source permanent and the
    -- controller's own departure from the game.
    controller :: PlayerId.PlayerId,
    -- | The monarch as of the last time this watch was examined. `Nothing` means no
    -- player was the monarch then, which is the game's opening state (CR 725.1)
    -- and a perfectly ordinary thing to be watching from: the first player ever
    -- crowned is a change like any other.
    --
    -- Comparing against this is what turns a state check into an event one, and
    -- it is why the field is updated rather than merely read: a crown that passes
    -- to the controller themselves discharges nothing, but it does move the
    -- baseline, so that the SAME opponent retaking it later still counts as a new
    -- monarch being crowned.
    lastMonarch :: Maybe PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)
