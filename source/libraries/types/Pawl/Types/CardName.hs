module Pawl.Types.CardName where

import Data.Text (Text)

-- A card's printed name ("Goblin Piker"), as a registry is asked for it.
--
-- Not a Pawl.Slug: a name is what a card calls itself and what a caller types,
-- and slugifying it is a registry's business rather than a caller's. A
-- file-backed registry slugifies to find a path; a map-backed one need not
-- slugify at all.
newtype CardName = MkCardName {unwrap :: Text}
  deriving (Eq, Ord, Show)
