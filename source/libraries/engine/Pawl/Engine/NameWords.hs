-- | CR 123.6a: the words of a name as name stickers count them, and CR
-- 123.6d-e's letter and vowel reads. Pure text, no game state.
module Pawl.Engine.NameWords where

import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.CardName as CardName

-- | CR 123.6a: a blank line, a run of underscores, is not a word.
isBlank :: Text.Text -> Bool
isBlank token = not (Text.null token) && Text.all (== '_') token

-- | CR 123.6a: each maximal run of non-space characters, blanks left out;
-- "_____-o-saurus" is one hyphenated word.
wordsOf :: CardName.CardName -> [Text.Text]
wordsOf = filter (not . isBlank) . Text.words . CardName.unwrap

-- | CR 123.6a: how many words the name has.
wordCount :: CardName.CardName -> Natural
wordCount = Natural.length . wordsOf

-- | CR 123.6b-c: @word@ after the first @k@ words, or at the end of a name
-- with fewer. A blank stays, and the word goes before a blank that follows
-- word k, pawl's reading (#4900).
insertAfter :: Natural -> Text.Text -> CardName.CardName -> CardName.CardName
insertAfter k word name =
  let go :: Natural -> [Text.Text] -> [Text.Text]
      go n tokens = case (n, tokens) of
        (0, _) -> word : tokens
        (_, []) -> [word]
        (_, token : rest) -> token : go (if isBlank token then n else n - 1) rest
   in CardName.MkCardName (Text.unwords (go k (Text.words (CardName.unwrap name))))

-- | CR 123.6d: how many of the text's characters are one of these letters,
-- either case.
letterCount :: Text.Text -> Text.Text -> Natural
letterCount letters text =
  let wanted = Set.fromList (Text.unpack (Text.toLower letters))
   in Natural.length (filter (\c -> Set.member c wanted) (Text.unpack (Text.toLower text)))

-- | CR 123.6e: how many different vowels, A, E, I, O, U and Y, either case.
uniqueVowels :: Text.Text -> Natural
uniqueVowels text = Natural.length (Set.filter (\c -> elem c "aeiouy") (Set.fromList (Text.unpack (Text.toLower text))))
