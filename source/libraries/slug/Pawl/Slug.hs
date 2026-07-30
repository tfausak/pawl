module Pawl.Slug where

import qualified Data.Char as Char
import qualified Data.Text as Text

-- | A kebab-case version of some text that's easy to read and write.
newtype Slug = UnsafeSlug
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

-- | Converts text into a slug.
fromText :: Text.Text -> Slug
fromText =
  UnsafeSlug
    . Text.concat
    . fmap (Text.take 1)
    . Text.groupBy (\x y -> x == '_' && y == '_')
    . Text.intercalate (Text.singleton '-')
    . Text.words
    . Text.map (\c -> if isValidChar c then c else ' ')
    . Text.filter (/= '\'')
    . Text.concatMap transliterate
    . Text.toCaseFold

-- | Returns true if the character is valid to use in a slug.
isValidChar :: Char -> Bool
isValidChar c =
  Char.isAsciiLower c
    || Char.isDigit c
    || c == '_'

-- | Converts some Unicode characters into their ASCII equivalents. This is not
-- exhaustive!
transliterate :: Char -> Text.Text
transliterate c = case c of
  '\xe0' -> Text.singleton 'a'
  '\xe1' -> Text.singleton 'a'
  '\xe2' -> Text.singleton 'a'
  '\xe3' -> Text.singleton 'a'
  '\xe4' -> Text.singleton 'a'
  '\xe5' -> Text.singleton 'a'
  '\xe6' -> Text.pack "ae"
  '\xe7' -> Text.singleton 'c'
  '\xe8' -> Text.singleton 'e'
  '\xe9' -> Text.singleton 'e'
  '\xea' -> Text.singleton 'e'
  '\xeb' -> Text.singleton 'e'
  '\xec' -> Text.singleton 'i'
  '\xed' -> Text.singleton 'i'
  '\xee' -> Text.singleton 'i'
  '\xef' -> Text.singleton 'i'
  '\xf0' -> Text.singleton 'd'
  '\xf1' -> Text.singleton 'n'
  '\xf2' -> Text.singleton 'o'
  '\xf3' -> Text.singleton 'o'
  '\xf4' -> Text.singleton 'o'
  '\xf5' -> Text.singleton 'o'
  '\xf6' -> Text.singleton 'o'
  '\xf8' -> Text.singleton 'o'
  '\xf9' -> Text.singleton 'u'
  '\xfa' -> Text.singleton 'u'
  '\xfb' -> Text.singleton 'u'
  '\xfc' -> Text.singleton 'u'
  '\xfd' -> Text.singleton 'y'
  '\xfe' -> Text.pack "th"
  '\xff' -> Text.singleton 'y'
  '\x14d' -> Text.singleton 'o'
  '\x153' -> Text.pack "oe"
  '\x16b' -> Text.singleton 'u'
  _ -> Text.singleton c
