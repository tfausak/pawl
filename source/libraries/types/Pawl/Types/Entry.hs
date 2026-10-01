module Pawl.Types.Entry where

import qualified Pawl.Types.Check as Check
import qualified Pawl.Types.Move as Move

-- | What one timeline entry does at its moment.
data Entry
  = -- | Answers the prompt the moment asks.
    Do Move.Move
  | -- | Answers the prompt with a move the engine must reverse and ask again (CR 733.1).
    Refuse Move.Move
  | -- | Asserts, when its player is next offered priority at the moment.
    Expect Check.Check
  deriving (Eq, Ord, Show)
