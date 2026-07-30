module Pawl.Types.DecisionLog where

import Pawl.Types.Response (Response)

data DecisionLog = MkDecisionLog
  { seed :: Int,
    responses :: [Response]
  }
  deriving (Eq, Show)
