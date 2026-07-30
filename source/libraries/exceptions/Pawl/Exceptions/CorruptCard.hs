module Pawl.Exceptions.CorruptCard where

import qualified Control.Exception as Exception
import qualified Data.Text as Text

-- A card's file exists but does not read back as a card. Either it is not
-- valid UTF-8, not valid JSON, or JSON that does not decode to a card.
data CorruptCard = MkCorruptCard
  { path :: FilePath,
    reason :: Text.Text
  }
  deriving (Eq, Show)

instance Exception.Exception CorruptCard
