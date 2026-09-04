module Pawl.Types.OutsideCard where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PrintingId as PrintingId

-- | CR 400.11c \/ 729.4: one card a wish may reach, from either of the two
-- places outside the game it can now come from.
--
-- Why a new type rather than reusing PrintingId: the two are NOT
-- indistinguishable. Which zone the card leaves decides which main-game
-- abilities trigger (CR 729.4a), so a player holding a Grizzly Bears in the
-- pool and another on the main-game battlefield is making a real choice, and
-- the engine may not make it for them.
--
-- 'Ord' is derived because the prompt's NonEmpty is built in interning order,
-- and both fields are already ordered ids -- the wire's shape, not a claim
-- about a stable "which is greater" ranking between the two constructors.
data OutsideCard
  = -- | CR 103.2a's sideboard pool, named by its printing the way
    -- Pawl.Engine.Event.bringInto's existing prompt already does.
    InPool PrintingId.PrintingId
  | -- | CR 729.4's main game, seen from inside a subgame as
    -- GameState.outsideObjects -- named by the id the card has out there, since
    -- that is the only handle the outer frame needs to apply the departure when
    -- the subgame ends.
    InAnotherGame ObjectId.ObjectId
  deriving (Eq, Ord, Show)
