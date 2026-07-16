module Pawl.Type.DecisionLog where

import Pawl.Type.Response (Response)

data DecisionLog = MkDecisionLog
  { seed :: Int,
    responses :: [Response]
  }
  deriving (Eq, Show)
