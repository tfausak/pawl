-- One GameState's shared reads, taken once and handed to every question an
-- enumeration asks of it: the whole-board projection (#200, #716) and the
-- control-grant walk. Answers nothing a fresh read would not, the board being
-- a snapshot of one state (Pawl.Engine.Projection.projectGiven).
module Pawl.Engine.Snapshot where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import Pawl.Types.GameState (GameState)
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.ProjectedCharacteristics (ProjectedCharacteristics)

-- LAZY FIELDS: each is a thunk until a question forces it, so a caller that
-- asks no projection question pays for no projection.
data Snapshot = MkSnapshot
  { -- Every object's characteristics, or empty to project each object on
    -- demand (Projection.projectGiven's fallback).
    projected :: Map ObjectId ProjectedCharacteristics,
    grants :: [Projection.ControlGrant]
  }

-- The whole board projected at once: for a caller about to ask about most of
-- the battlefield.
whole :: GameState -> Snapshot
whole gs = MkSnapshot {projected = Projection.projectAll gs, grants = Projection.controlGrants gs}

-- Each object projected only when asked about: for a caller asking about one
-- object, where projecting the rest would be waste.
onDemand :: GameState -> Snapshot
onDemand gs = MkSnapshot {projected = Map.empty, grants = Projection.controlGrants gs}
