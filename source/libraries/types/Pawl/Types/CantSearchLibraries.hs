module Pawl.Types.CantSearchLibraries where

import qualified Pawl.Types.PlayerScope as PlayerScope

-- | The payload of Pawl.Types.PlayerEffect's CantSearchLibraries arm (CR
-- 701.23): the two axes a printed prohibition narrows, each a set of players.
--
-- Both scopes are anchored to the PROHIBITED player rather than to CR 109.5's
-- "you", which is the anchoring Pawl.Types.PlayerEffect's CantBeTargetedBy
-- already takes: Ashiok, Dream Render says "their controller" and "their
-- library" of the opponent its scope has put in the prohibition, so You here is
-- that opponent and not Ashiok's controller.
data CantSearchLibraries = MkCantSearchLibraries
  { -- | CR 701.23: whose library may not be searched -- Ashiok, Dream Render's
    -- "their library" (You), Leonin Arbiter's unqualified "libraries"
    -- (EachPlayer).
    library :: PlayerScope.PlayerScope,
    -- | CR 405.4: who must control the spell or ability causing the search --
    -- Ashiok's "spells and abilities your opponents control" (You), read as the
    -- cause's CONTROLLER and not as the cause itself. EachPlayer states no
    -- cause.
    cause :: PlayerScope.PlayerScope
  }
  deriving (Eq, Ord, Show)
