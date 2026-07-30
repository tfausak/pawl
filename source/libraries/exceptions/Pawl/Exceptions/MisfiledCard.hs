module Pawl.Exceptions.MisfiledCard where

import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Pawl.Slug as Slug

-- | A card's file parsed, but the card's own name does not slugify back to the
-- slug it is filed under.
data MisfiledCard = MkMisfiledCard
  { path :: FilePath,
    name :: Text.Text,
    expected :: Slug.Slug,
    actual :: Slug.Slug
  }
  deriving (Eq, Show)

instance Exception.Exception MisfiledCard
