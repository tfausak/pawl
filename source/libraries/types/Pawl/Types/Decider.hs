module Pawl.Types.Decider where

import Pawl.Types.PlayerId (PlayerId)

newtype Decider = MkDecider PlayerId
  deriving (Eq, Ord, Show)
