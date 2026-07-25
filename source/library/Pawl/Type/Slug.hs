module Pawl.Type.Slug where

import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text

-- A card's file-name key (Pawl.Slug.slugify, "Goblin Piker" -> "goblin-piker"):
-- non-empty, every character in [a-z0-9_-], no leading or trailing '-', no
-- doubled '-' or '_'. The house style forbids export lists (CLAUDE.md's
-- UnsafeX / textToX / xToText section), so UnsafeSlug -- the one exception to
-- the no-punning-constructor rule this codebase makes -- is a convention
-- rather than an enforced boundary: build a Slug through textToSlug or
-- Pawl.Slug.slugify, not the bare constructor.
newtype Slug = UnsafeSlug Text
  deriving (Eq, Ord, Show)

slugToText :: Slug -> Text
slugToText (UnsafeSlug t) = t

-- Accepts exactly the texts that are already slugs -- the fixed points of
-- Pawl.Slug.slugify -- and rejects everything else, including a raw card name.
-- Validates only; it never normalizes ("Goblin Piker" is Nothing here, not
-- "goblin-piker").
textToSlug :: Text -> Maybe Slug
textToSlug t =
  let isSlugChar c = Char.isAsciiLower c || Char.isDigit c || c == '_' || c == '-'
      dash = Text.pack "-"
      doubleDash = Text.pack "--"
      doubleUnderscore = Text.pack "__"
   in if not (Text.null t)
        && Text.all isSlugChar t
        && not (Text.isPrefixOf dash t)
        && not (Text.isSuffixOf dash t)
        && not (Text.isInfixOf doubleDash t)
        && not (Text.isInfixOf doubleUnderscore t)
        then Just (UnsafeSlug t)
        else Nothing
