module Pawl.Types.MonarchWatch where

import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 725 (Palace Jailer): one object exiled "until an opponent becomes the
-- monarch", and what the game must remember to know when that has happened.
--
-- The card's rulings make this an EVENT, not a state: the creature does not
-- return just because an opponent already is the monarch, so the question is
-- whether the crown has CHANGED HANDS to an opponent since the exile resolved.
data MonarchWatch = MkMonarchWatch
  { -- | The effect's controller. Whose opponents are being watched for -- fixed
    -- when the exile resolves, and it outlives both its source permanent and the
    -- controller's own departure from the game.
    controller :: PlayerId.PlayerId,
    -- | The monarch as of the last time this watch was examined. `Nothing` is the
    -- game's opening state (CR 725.1) and an ordinary thing to watch from.
    --
    -- Comparing against this turns a state check into an event one, and is why
    -- the field is updated rather than merely read: a crown passing to the
    -- controller themselves discharges nothing but does move the baseline, so the
    -- SAME opponent retaking it later still counts as a new monarch.
    lastMonarch :: Maybe PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)
