module Pawl.Types.Staged where

import qualified Data.Map.Strict as Map
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Label as Label
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | A scenario's board placed into a game, and what each of its labels names.
data Staged = MkStaged
  { state :: GameState.GameState,
    seats :: Map.Map Label.Label PlayerId.PlayerId,
    objects :: Map.Map Label.Label ObjectId.ObjectId
  }
  deriving (Eq, Show)
