module Pawl.Type.MisfiledCard where

import qualified Control.Exception as Exception
import Data.Text (Text)
import Pawl.Type.Slug (Slug)

-- A card's file parsed, but the card's own name does not slugify back to the
-- slug it is filed under -- so a lookup would quietly serve a different card
-- than it was asked for. `slugifiesTo` is Maybe because a card whose name has no
-- slug at all is misfiled under every name.
data MisfiledCard = MkMisfiledCard
  { path :: FilePath,
    filedUnder :: Slug,
    name :: Text,
    slugifiesTo :: Maybe Slug
  }
  deriving (Eq, Show)

instance Exception.Exception MisfiledCard
