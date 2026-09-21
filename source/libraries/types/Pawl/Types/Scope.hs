module Pawl.Types.Scope where

import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.SlotName as SlotName

-- | What a Pawl.Types.Count folds over: a zone's current residents, the event
-- log, the players themselves, the objects one of the surrounding
-- announcement's slots names, or one POSITION in a zone. Five domains rather
-- than one because the second reads CR 608.2h last-known information from a
-- stored snapshot, not a live object, the third folds over candidates CR 109.1
-- says are not objects at all, the fourth takes its candidates from the
-- resolution's bindings rather than from the board, and the fifth narrows the
-- first's candidates before the Filter is asked rather than after.
--
-- A MANA POOL is deliberately not an arm of its own: the pool is none of CR 400.1's
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
  | -- | CR 400.7j: the objects the named slot of the surrounding announcement is
    -- bound to, wherever they wound up -- Psychic Miasma's "if a land card is
    -- discarded this way", read off Pawl.Types.CountedDiscard's `discarded`.
    --
    -- The one scope whose candidates come from the resolution's bindings instead
    -- of from the board, which is what lets it follow a CR 614 redirect: asking
    -- the same question as a Count over a ZONE filtered by Filter.IsBound gets
    -- the redirected card wrong, because the card is no longer in the zone the
    -- fold reads.
    --
    -- PUBLIC destinations, or a REVEALED arrival in a hidden one, and
    -- Pawl.Engine.Count.findableAfterMove is where that is enforced. CR 400.7j
    -- grants the find "to a public zone" (CR 400.2's list, which
    -- Pawl.Engine.Game.isHiddenZone encodes), and CR 701.9c leaves a card
    -- discarded into a hidden zone with every characteristic undefined only when
    -- it got there without being revealed.
    --
    -- The slot is read through Pawl.Engine.Filter's slot map exactly as
    -- Filter.IsBound is, and may be either a CR 115.10a group binding whose ids
    -- are the ARRIVALS a move wrote (Psychic Miasma) or a TARGET slot bound
    -- before this resolution moved it, whose id is the DEPARTURE (Hour of
    -- Glory's "if that creature was a God"). findableAfterMove asks CR 400.7j of
    -- where the object landed either way; what the fold then reads for a
    -- departure is CR 608.2h's last known information.
    OverBound SlotName.SlotName
  | -- | CR 404.1 / Guiding Spirit: the top card of each graveyard the PlayerRef
    -- names -- the NEWEST arrival, which is the LAST member of the pile.
    -- Pawl.Types.ObjectRef.TopOfGraveyard reads the same position to ACT on the
    -- card; this arm is how a Condition TESTS it without acting.
    --
    -- Not InZone narrowed by a Filter, and that is the whole reason it is an arm
    -- of its own: a Filter in that position means "every member that matches",
    -- so "if the top card is a creature card" would become "if any creature card
    -- is in the graveyard". The POSITION has to be the scope and the Filter the
    -- test, in that order. Pawl.Engine.Cost.topExileCandidate reads the other
    -- composition -- "the top MATCHING card" -- and is not this.
    --
    -- One card per named graveyard, none at all from an empty one, so
    -- Aggregation.Members over it is 0 or the number of named graveyards whose
    -- top matches.
    TopOfGraveyard PlayerRef.PlayerRef
  deriving (Eq, Ord, Show)
