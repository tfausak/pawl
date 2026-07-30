module Pawl.Types.Result where

import Pawl.Types.PlayerId (PlayerId)

data Result
  = Won PlayerId
  | Drawn
  deriving (Eq, Ord, Show)
