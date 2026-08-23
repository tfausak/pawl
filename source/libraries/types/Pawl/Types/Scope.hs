module Pawl.Types.Scope where

import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.PlayerRef as PlayerRef

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
  = InZone InZone.InZone
  | -- | CR 608.2i: effects that look back in time.
    InHistory EventShape.EventShape
  | -- | CR 102.1: the PLAYERS the reference names -- Tyranid Invasion's "the
    -- number of opponents you have". The one arm whose candidates are not
    -- objects (CR 109.1), so each is seen through Pawl.Engine.Count.playerView
    -- rather than through a projection.
    --
    -- The PlayerRef says WHICH players, exactly as it says whose zone in InZone
    -- above, and Pawl.Engine.Count.playersFor answers both -- which is what
    -- makes CR 800.4a's departed seat uncountable here for free: that function
    -- already folds through Game.stillPlaying, on the stated grounds that the
    -- first scope folding over players rather than over their objects would
    -- observe the difference. This is that scope.
    --
    -- The Filter runs over the players, and the atoms that answer for one split
    -- in two. Filter.IsPlayer is answered off the VIEW, relating the candidate to
    -- the perspective, as is Filter.DealtDamageThisTurn, which Count.playerView
    -- fills from the board (CR 120.1).
    -- Filter.ControlsMoreThanYou (Oreskos Explorer's "players who control more
    -- lands than you"), Filter.IsControllerOfBound (Spikeshell Harrier's "each
    -- other player") and Filter.CardsInGraveyardAtLeast (The Master of
    -- Lake-town's "each graveyard with seven or more cards in it") are instead
    -- BAKED against the board at Pawl.Engine.Count.bakePerspective, each asking
    -- something no view of a player could carry.
    --
    -- Saying "opponents" through the REFERENCE rather than through the first atom
    -- is the convention, since the reference is the only spelling the sibling arm
    -- has; a question about a candidate's own board or zone has no spelling but
    -- one of the baked three.
    OverPlayers PlayerRef.PlayerRef
  deriving (Eq, Ord, Show)
