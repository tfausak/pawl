module Pawl.Types.ExileLooker where

import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 406.3: one grant of permission to look at a card exiled face down. A set
-- of these is 'Pawl.Types.Object.exileLookers', and
-- 'Pawl.Engine.Exile.mayLookAt' is what asks each of them about a player. Ord
-- because it is a Set member.
data ExileLooker
  = -- | CR 406.3: the seat the instruction named, stored (Extract Power).
    ThePlayer PlayerId.PlayerId
  | -- | CR 702.75a: whoever controls the permanent that exiled the card, asked
    -- afresh each time (Windbrisk Heights). CR 607.2's link is where that
    -- permanent is written down, so this arm carries no object of its own:
    -- 'Pawl.Types.GameState.exiledWith' already answers "which permanent exiled
    -- this card" for the exile the same instruction made.
    --
    -- Not implemented: the rule's grant is sticky, CR 406.3 letting a player who
    -- was once allowed to look continue to -- so a player who controlled the
    -- permanent and then lost it keeps the look, where this arm answers the
    -- CURRENT controller alone (#3452).
    TheExiler
  deriving (Eq, Ord, Show)
