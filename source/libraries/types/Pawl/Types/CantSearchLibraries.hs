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
  { -- | Whose library may not be searched -- Ashiok, Dream Render's "their
    -- library", which is the prohibited player's own (You). EachPlayer is Leonin
    -- Arbiter's unqualified "libraries", and leaves the prohibited player unable
    -- to search any library at all.
    library :: PlayerScope.PlayerScope,
    -- | Who controls the spell or ability whose resolution causes the search --
    -- Ashiok's "spells and abilities your opponents control", read from the
    -- prohibited opponent as You. EachPlayer states no cause, which is what
    -- Leonin Arbiter says.
    --
    -- The CONTROLLER of the cause (CR 113.7's source, taken to its controller)
    -- rather than the cause itself, so a card narrowing by any other quality of
    -- the source cannot be written here. "Spells and abilities" needs no
    -- qualifier of its own on top of that: Pawl.Types.Effect's Search is
    -- executed by Pawl.Engine.Resolve alone, so every search pawl can perform is
    -- caused by a resolving spell or ability and the class Ashiok names is every
    -- cause there is.
    cause :: PlayerScope.PlayerScope
  }
  deriving (Eq, Ord, Show)
