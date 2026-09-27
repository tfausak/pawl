module Pawl.Types.Result where

import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.TeamId as TeamId

data Result
  = Won PlayerId.PlayerId
  | -- | CR 104.2c: a team won, and each player on it wins -- including one
    -- who had already lost.
    TeamWon TeamId.TeamId
  | Drawn
  deriving (Eq, Ord, Show)
