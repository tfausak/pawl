module Pawl.Types.ManaActivation where

import qualified Data.List.NonEmpty as NonEmpty
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.ObjectId as ObjectId

-- | One mana ability a payer activated in a CR 605.3a mana window, as CR 733.1
-- offers it back: its sources and the states either side of it.
--
-- Deliberately no codec: a payment in flight is never serialised.
data ManaActivation = MkManaActivation
  { -- | The source whose ability this was -- or, for an activation that failed
    -- and was reversed, the sources its own nested window kept.
    sources :: NonEmpty.NonEmpty ObjectId.ObjectId,
    -- | The state the activation began on.
    opened :: GameState,
    -- | The state it ended on.
    closed :: GameState
  }
