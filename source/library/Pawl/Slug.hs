module Pawl.Slug where

import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Type.Slug as Slug

-- The file name for a card: case-folded, transliterated to ASCII, apostrophes
-- dropped, every remaining run of non-alphanumeric-non-underscore characters
-- (spaces, punctuation, "//") collapsed to a single "-", every run of
-- underscores collapsed to a single "_", and the (hyphen) edges trimmed.
-- "Urborg, Tomb of Yawgmoth" -> "urborg-tomb-of-yawgmoth", "Serpent's Gift" ->
-- "serpents-gift", "Khabál Ghoul" -> "khabal-ghoul".
--
-- "_" is kept rather than treated as punctuation so a name built entirely from
-- underscores -- Unhinged's literal "_____" and Unknown Event's "______" --
-- slugifies to a non-empty blank rather than Nothing. Runs collapse to one
-- underscore so a card's slug never depends on counting how many underscores
-- its name has ("Knight in _____ Armor" -> "knight-in-_-armor"). A
-- consequence: "_____" and "______" collapse to the same "_" and collide --
-- already true before this change (both slugified to the empty text, which
-- was itself already a collision), so this is not a regression, and it is one
-- instance of the pool-scale collisions #166 already tracks.
--
-- Case-folding rather than lower-casing so "ß" folds to "ss" with no table
-- entry. The keep-or-separate step is what makes the output ASCII
-- unconditionally: a letter `transliterate` does not carry becomes a separator
-- rather than leaking a non-ASCII byte into a file name.
--
-- Idempotent -- the output is already [a-z0-9_-] with no runs of "-" or "_"
-- and no edge hyphens -- which is why Pawl.Registry needs only one lookup
-- function: a card name and its slug are the same lookup. Only hyphens are
-- trimmed from the edges: trimming underscores too would turn "_____" back
-- into Nothing.
--
-- A name with nothing to keep (no letters, digits, or underscores) slugifies
-- to Nothing rather than the empty text: Pawl.Type.Slug.fromText rejects the
-- empty text, and this function is exactly that validation applied to its own
-- output.
--
-- Unique over the committed corpus (Pawl.CardSpec checks it), but not over the
-- full ~34k-name pool: joke-set, playtest and blank-name cards collide (#166).
slugify :: Text -> Maybe Slug.Slug
slugify t =
  let folded = Text.toCaseFold t
      unquoted = Text.filter (/= '\'') folded
      ascii = Text.concatMap transliterate unquoted
      isSlugChar c = Char.isAsciiLower c || Char.isDigit c || c == '_'
      keep c = if isSlugChar c then c else ' '
      collapseUnderscores = Text.concat . fmap collapseRun . Text.group
      collapseRun run = case Text.uncons run of
        Just ('_', _) -> Text.singleton '_'
        _ -> run
   in Slug.fromText (Text.intercalate (Text.pack "-") (Text.words (collapseUnderscores (Text.map keep ascii))))

-- ASCII stand-ins for the accented letters that occur in card names. Applied
-- after case folding, so only the lower-case forms need entries. Everything
-- unlisted falls through to slugify's separator rule.
transliterate :: Char -> Text
transliterate c = case c of
  'à' -> Text.pack "a"
  'á' -> Text.pack "a"
  'â' -> Text.pack "a"
  'ã' -> Text.pack "a"
  'ä' -> Text.pack "a"
  'å' -> Text.pack "a"
  'æ' -> Text.pack "ae"
  'ç' -> Text.pack "c"
  'è' -> Text.pack "e"
  'é' -> Text.pack "e"
  'ê' -> Text.pack "e"
  'ë' -> Text.pack "e"
  'ì' -> Text.pack "i"
  'í' -> Text.pack "i"
  'î' -> Text.pack "i"
  'ï' -> Text.pack "i"
  'ð' -> Text.pack "d"
  'ñ' -> Text.pack "n"
  'ò' -> Text.pack "o"
  'ó' -> Text.pack "o"
  'ô' -> Text.pack "o"
  'õ' -> Text.pack "o"
  'ö' -> Text.pack "o"
  'ø' -> Text.pack "o"
  'ō' -> Text.pack "o"
  'œ' -> Text.pack "oe"
  'ù' -> Text.pack "u"
  'ú' -> Text.pack "u"
  'û' -> Text.pack "u"
  'ü' -> Text.pack "u"
  'ū' -> Text.pack "u"
  'ý' -> Text.pack "y"
  'ÿ' -> Text.pack "y"
  'þ' -> Text.pack "th"
  _ -> Text.singleton c
