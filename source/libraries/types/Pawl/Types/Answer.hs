module Pawl.Types.Answer where

import qualified Data.Text as Text
import qualified Pawl.Types.Reply as Reply

-- | Any prompt answered by name, for a prompt no dedicated move covers.
data Answer = MkAnswer
  { prompt :: Text.Text,
    with :: Reply.Reply
  }
  deriving (Eq, Ord, Show)
