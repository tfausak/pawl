module Pawl.Type.Decider where

import Pawl.Type.PlayerId (PlayerId)

newtype Decider = MkDecider PlayerId
  deriving (Eq, Ord, Show)
