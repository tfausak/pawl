-- CR 607.2d: the Filter.Context a source's OWN ability is matched in, with every
-- choice that source made as it entered (CR 614.1c) or while resolving (CR
-- 608.2c) filled in one place, so no position can fill one choice and forget
-- another.
module Pawl.Engine.SourceContext where

import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | Filter.contextFor framed by `source`, with its choices (withChoicesOf).
sourceContext :: GameState.GameState -> Maybe PlayerId.PlayerId -> ObjectId.ObjectId -> Filter.Context
sourceContext gs perspective source =
  withChoicesOf source gs (Filter.contextFor (Game.teams gs) perspective (Just source))

-- | `context` with the choices of `source`, the object whose own ability is
-- asking: CR 105.2's colour, CR 205.3's subtype and CR 201.4's names. Read
-- through CR 608.2h's last known information, the reading CR 113.7a gives an
-- ability whose source has left.
withChoicesOf :: ObjectId.ObjectId -> GameState.GameState -> Filter.Context -> Filter.Context
withChoicesOf source gs context =
  context
    { Filter.sourceChosenColors = Game.chosenColorsWithLastKnown source gs,
      Filter.sourceChosenSubtype = Game.chosenSubtypeWithLastKnown source gs,
      Filter.sourceChosenNames = Game.chosenNamesWithLastKnown source gs
    }
