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
    -- | Has an opponent of `controller` become the monarch since the exile
    -- resolved? Written by Pawl.Engine.Monarch.crown at the crowning itself and
    -- read by Pawl.Engine.Monarch.returnExiledForMonarch at the next settle, so
    -- no number of crownings between two settles can hide one of them from the
    -- watch.
    --
    -- A recorded EVENT rather than a remembered holder: comparing the current
    -- monarch against the one seen at the last look cannot tell an unmoved crown
    -- from one that moved away and back (#208).
    due :: Bool
  }
  deriving (Eq, Ord, Show)
