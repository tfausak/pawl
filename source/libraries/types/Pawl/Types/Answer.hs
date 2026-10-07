module Pawl.Types.Answer where

import qualified Data.Text as Text
import qualified Pawl.Types.Reply as Reply

-- | Any prompt answered by name, for a prompt no dedicated move covers.
data Answer = MkAnswer
  { prompt :: Text.Text,
    with :: Reply.Reply,
    -- | The answer is one the prompt does NOT offer, on purpose: a probe of the
    -- offer, or of the engine's fallback for an answer outside it. The runner
    -- fails an unflagged answer outside the offer, and a flagged one inside it.
    unoffered :: Bool
  }
  deriving (Eq, Ord, Show)
