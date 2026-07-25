module Pawl.Type.CorruptCard where

import qualified Control.Exception as Exception
import Data.Text (Text)

-- A card's file exists but does not read back as a card: not valid UTF-8, not
-- valid JSON, or JSON that does not decode to a Card. One type for all three
-- because they are one thing to a caller -- "that file is broken", as opposed to
-- UnknownCard's "there is no such card" -- and the specific cause is prose no
-- program should branch on.
data CorruptCard = MkCorruptCard
  { path :: FilePath,
    reason :: Text
  }
  deriving (Eq, Show)

instance Exception.Exception CorruptCard
