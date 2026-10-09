-- CR 607.2d / 109.5: the Filter.Context a source's OWN ability is matched in,
-- with every answer the board gives about that source filled in one place --
-- its host (CR 303.4b), its owner (CR 108.3), what it put onto the battlefield
-- (CR 400.7) and every choice it made as it entered (CR 614.1c) or while
-- resolving (CR 608.2c) -- so no position can fill one and forget another.
--
-- Not the source's power, toughness, mana value, colours or names: those are
-- projected characteristics, and this module sits below Pawl.Engine.Projection,
-- whose layer fold (CR 613) builds its own contexts here. Each stays with the
-- caller that reads it (see Filter.Context.sourcePower).
module Pawl.Engine.SourceContext where

import qualified Data.Map.Strict as Map
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.SourceChoices as SourceChoices

-- | Filter.contextFor framed by `source`, with every source-derived field
-- filled (framedBy).
sourceContext :: GameState.GameState -> Maybe PlayerId.PlayerId -> ObjectId.ObjectId -> Filter.Context
sourceContext gs perspective source =
  framedBy source gs (Filter.contextFor (Game.teams gs) perspective (Just source))

-- | `context` with every source-derived field of `source` filled, its choices
-- read live through CR 608.2h's last known information (choicesOf). Leaves
-- `Filter.source` alone, so a caller whose IsSource names nothing keeps that.
framedBy :: ObjectId.ObjectId -> GameState.GameState -> Filter.Context -> Filter.Context
framedBy source gs = framedWith (choicesOf source gs) source gs

-- | framedBy with the choices supplied: what a stored effect baked as it began
-- (CR 608.2h), in place of the source's current ones. Every other field is
-- still read off the board. All are thunks: a filter that names none of their
-- atoms forces none of them.
framedWith :: SourceChoices.SourceChoices -> ObjectId.ObjectId -> GameState.GameState -> Filter.Context -> Filter.Context
framedWith choices source gs context =
  (withChoices choices context)
    { Filter.sourceAttachedTo = Game.hostOf source gs,
      -- CR 108.3 fixes an owner when the game starts, so a live read; a source
      -- the state no longer holds answers Nothing.
      Filter.sourceOwner = fmap Object.owner (Game.lookupObject source gs),
      Filter.sourceEntrants = Map.keysSet (Map.filter (== source) (GameState.enteredWith gs))
    }

-- | The choices `source` has made so far, through CR 608.2h's last known
-- information; what a stored effect bakes as it begins.
choicesOf :: ObjectId.ObjectId -> GameState.GameState -> SourceChoices.SourceChoices
choicesOf source gs =
  SourceChoices.MkSourceChoices
    { SourceChoices.names = Game.chosenNamesWithLastKnown source gs,
      SourceChoices.colors = Game.chosenColorsWithLastKnown source gs,
      SourceChoices.subtype = Game.chosenSubtypeWithLastKnown source gs
    }

-- | `context` with these choices as the source's: CR 105.2's colours, CR
-- 205.3's subtype and CR 201.4's names.
withChoices :: SourceChoices.SourceChoices -> Filter.Context -> Filter.Context
withChoices choices context =
  context
    { Filter.sourceChosenColors = SourceChoices.colors choices,
      Filter.sourceChosenSubtype = SourceChoices.subtype choices,
      Filter.sourceChosenNames = SourceChoices.names choices
    }
