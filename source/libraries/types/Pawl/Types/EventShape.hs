module Pawl.Types.EventShape where

import qualified Pawl.Types.MovedBetween as MovedBetween
import qualified Pawl.Types.Zone as Zone

-- | Which recorded events a history count folds over. GameState.events is cleared
-- at the turn change (Pawl.Engine.Engine), an engine choice made under CR 608.2i
-- because every history-reading card in the pool asks "this turn" -- so the
-- log's extent IS the window and none is carried here.
--
-- Only MovedBetween, CardArrivedIn and SpellCast exist: every other GameEvent
-- constructor is recorded in the log with no EventShape arm, so a count cannot
-- fold over any of them (#162).
--
-- That is not the same as the log being unreadable for those events, and the
-- readers that exist do not come through here. A fold is the wrong instrument
-- whenever the question is about a PLAYER or about ONE named object rather than
-- about a population of objects, since a member of this fold is a Filter view of
-- an object and the unit counted is an EVENT. Those questions are
-- Pawl.Types.Quantity arms instead -- CardsDiscardedThisTurn,
-- PlayersDealtDamageThisTurn, EnteredThisTurn, EnteredFrom and WasCastFrom among
-- them -- each of which reads GameState.events directly through a
-- Pawl.Engine.Game accessor.
data EventShape
  = -- | CR 700.4: "dies" is MovedBetween Battlefield Graveyard.
    MovedBetween MovedBetween.MovedBetween
  | -- | CR 712.21e's second half: how many CARDS were put into this zone, from
    -- anywhere.
    --
    -- MovedBetween above is the same rule's first half and counts OBJECTS, which
    -- is why the two cannot be one arm: a melded permanent's death is one object
    -- that moved and two cards that were put into a graveyard, so folding the
    -- arrivals under that shape would make Khabal Ghoul's "each creature that
    -- died this turn" see three.
    --
    -- ONE zone and it is the DESTINATION, where MovedBetween names both ends.
    -- CR 712.21e's "changed zones" puts no condition on where a card came from,
    -- and the pool's producers ask the same way -- "put into graveyards from
    -- anywhere", and Ashiok, Wicked Manipulator's Nightmare token's "if a card was
    -- put into exile this turn" (Pawl.PlaneswalkerSpec's AshiokLoyalty group is
    -- what proves the origin is not read: it exiles out of a HAND).
    -- CR 903.9c is what makes the destination the load-bearing half:
    -- a melded commander's two cards can land in two different zones, and only
    -- the one that arrived in the named zone is counted.
    --
    -- Not implemented: an origin RESTRICTION -- Dimir Strandcatcher's "from
    -- anywhere other than the battlefield" -- which would need an origin set
    -- beside this zone rather than a second shape (#3151).
    CardArrivedIn Zone.Zone
  | -- | CR 601.2i: a spell became cast. What "for each spell you've cast this
    -- turn" folds over (Aetherflux Reservoir).
    --
    -- NO PAYLOAD, unlike MovedBetween above, and that asymmetry is the point: a
    -- zone change is only a shape once its two zones are named, and no Filter atom
    -- can name a zone. Everything a cast count narrows by IS a Filter atom --
    -- "you've cast" is Filter.ControlledBy against the caster CR 601.2a made the
    -- spell's controller, "instant and sorcery spell" a disjunction of
    -- Filter.HasCardType -- so putting any of it here would be a second way to
    -- say what the Count's own filter already says.
    SpellCast
  deriving (Eq, Ord, Show)
