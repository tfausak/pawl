module Pawl.Types.ManaWindow where

import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | A CR 605.3a mana window that has closed, as CR 733.1 needs it to reverse
-- the action it was opened in: whose it was, what they activated, and the
-- states it opened and closed on. Pawl.Engine.Cost.reverseIllegal reads it.
--
-- Deliberately no codec: a payment in flight is never serialised.
data ManaWindow = MkManaWindow
  { -- | The player the window was offered to.
    payer :: PlayerId.PlayerId,
    -- | The sources whose mana abilities the payer activated in it, oldest
    -- first.
    activated :: [ObjectId.ObjectId],
    -- | The state the window opened on.
    opened :: GameState,
    -- | The state it closed on, before a symbol of the cost was paid.
    closed :: GameState
  }
