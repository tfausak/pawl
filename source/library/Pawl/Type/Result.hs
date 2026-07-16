module Pawl.Type.Result where

import Pawl.Type.PlayerId (PlayerId)

data Result
  = Won PlayerId
  | Drawn
  deriving (Eq, Ord, Show)
