module Pawl.Types.EventShape where

import qualified Pawl.Types.Zone as Zone

-- | Which recorded events a history count folds over. GameState.events is cleared
-- at the turn change (Pawl.Engine.Engine), an engine choice made under CR 608.2i
-- because every history-reading card in the pool asks "this turn" -- so the
-- log's extent IS the window and none is carried here.
--
-- Only MovedBetween and SpellCast exist: every other GameEvent constructor is
-- recorded in the log with no EventShape arm, so a count cannot fold over any of
-- them (#162).
data EventShape
  = -- | CR 700.4: "dies" is MovedBetween Battlefield Graveyard.
    MovedBetween Zone.Zone Zone.Zone
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
