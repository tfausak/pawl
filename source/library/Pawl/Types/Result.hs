module Pawl.Types.Result where

import qualified Pawl.Types.PlayerId as PlayerId

data Result
  = Won PlayerId.PlayerId
  | Drawn
  deriving (Eq, Ord, Show)
