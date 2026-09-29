module Pawl.Types.Entry where

import qualified Pawl.Types.Check as Check
import qualified Pawl.Types.Move as Move

-- | What one timeline entry does at its moment.
data Entry
  = -- | Answers the prompt the moment asks.
    Do Move.Move
  | -- | Asserts, when its player is next offered priority at the moment.
    Expect Check.Check
  deriving (Eq, Ord, Show)
