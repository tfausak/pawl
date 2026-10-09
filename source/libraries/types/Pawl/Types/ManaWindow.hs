module Pawl.Types.ManaWindow where

import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.ManaActivation as ManaActivation
import qualified Pawl.Types.ManaSegment as ManaSegment
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.PlayerId as PlayerId

-- | A CR 605.3a mana window that has closed, as CR 733.1 needs it to reverse
-- the action it was opened in: whose it was, what they activated, and the
-- states it opened and closed on. Pawl.Engine.Cost.reverseIllegal reads it.
--
-- Deliberately no codec: a payment in flight is never serialised.
data ManaWindow = MkManaWindow
  { -- | The player the window was offered to.
    payer :: PlayerId.PlayerId,
    -- | The mana abilities the payer activated in it, nested ones included,
    -- in the order they finished.
    activated :: [ManaActivation.ManaActivation],
    -- | What each of them wrote, oldest first.
    segments :: [ManaSegment.ManaSegment],
    -- | The mana the window's own payment took from the pool, empty unless it
    -- paid.
    spent :: [ManaUnit.ManaUnit],
    -- | The state the window opened on.
    opened :: GameState,
    -- | The state it closed on, before a symbol of the cost was paid.
    closed :: GameState
  }
