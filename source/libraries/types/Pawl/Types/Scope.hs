module Pawl.Types.Scope where

import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Zone as Zone

-- | What a Pawl.Types.Count folds over: a zone's current residents, the event
-- log, or the players themselves. Three domains rather than one because the
-- second reads CR 608.2h last-known information from a stored snapshot, not a
-- live object, and the third folds over candidates CR 109.1 says are not
-- objects at all.
--
-- A MANA POOL is deliberately not a fourth arm: the pool is none of CR 400.1's
-- zones (CR 106.4 attaches it to a player instead), and a Count's Filter and
-- Aggregation have nothing to say about a mana unit. Pawl.Types.ManaCount is the
-- parallel axis, and its haddock carries the argument in full.
data Scope
  = InZone Zone.Zone PlayerRef.PlayerRef
  | -- | CR 608.2i: effects that look back in time.
    InHistory EventShape.EventShape
  | -- | CR 102.1: the PLAYERS the reference names -- Tyranid Invasion's "the
    -- number of opponents you have". The one arm whose candidates are not
    -- objects (CR 109.1), so each is seen through Pawl.Engine.Filter.playerView
    -- rather than through a projection.
    --
    -- The PlayerRef says WHICH players, exactly as it says whose zone in InZone
    -- above, and Pawl.Engine.Count.playersFor answers both -- which is what
    -- makes CR 800.4a's departed seat uncountable here for free: that function
    -- already folds through Game.stillPlaying, on the stated grounds that the
    -- first scope folding over players rather than over their objects would
    -- observe the difference. This is that scope.
    --
    -- The Filter still runs, and every player candidate answers exactly one
    -- atom -- Filter.IsPlayer -- so nothing in the pool needs it and every
    -- producer pairs this with an empty conjunction. Saying "opponents" HERE
    -- rather than through that atom is the convention, since the reference is
    -- the only spelling the sibling arm has. Anything narrower than a relation
    -- ("each player who controls more lands than you") is expressible by
    -- neither and is a Filter question (#283).
    OverPlayers PlayerRef.PlayerRef
  deriving (Eq, Ord, Show)
