module Pawl.Types.Decider where

import qualified Pawl.Types.PlayerId as PlayerId

newtype Decider = MkDecider
  { unwrap :: PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)
