module Pawl.Types.DecisionLog where

import qualified Pawl.Types.Response as Response

data DecisionLog = MkDecisionLog
  { seed :: Int,
    responses :: [Response.Response]
  }
  deriving (Eq, Ord, Show)
