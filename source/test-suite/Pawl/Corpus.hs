-- The bundled card corpus, read as data rather than through a registry.
--
-- Every consumer of this module is a lint over data/cards: that each committed
-- file re-parses, that each file's name agrees with the card's own name, that
-- every mode declares the slots it reads. Those are claims about what pawl
-- SHIPS, not questions anyone asks a registry -- and treating them as registry
-- operations is what forced Pawl.Registry to enumerate directories at all.
--
-- So this lives in the test suite. Nothing outside it lints the corpus, and a
-- library module no consumer calls is dead weight.
module Pawl.Corpus where

import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Json as Json
import qualified Pawl.Slug as Slug
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardError as CardError
import qualified Pawl.Types.CardName as CardName
import qualified System.Directory as Directory

-- Every card file in `root`, by slug, ascending.
--
-- A file name is slugified, not validated, so a stem that is not already a slug
-- yields a slug naming a DIFFERENT path. Pawl.CardSpec pins every committed file
-- name to its own slug so that never arises in the corpus. Non-.json entries are
-- ignored outright -- a README is not a broken card.
slugsIn :: FilePath -> IO [Slug.Slug]
slugsIn root =
  let json = ".json"
      stem name = take (length name - length json) name
      toSlug = Slug.fromText . Text.pack . stem
   in do
        entries <- Directory.listDirectory root
        pure $ fmap toSlug (List.sort (filter (List.isSuffixOf json) entries))

-- Read and parse every card file in `root`, keeping each slug beside its result.
--
-- Per-card Either rather than an exception, so a sweep can name every bad file
-- in one run instead of dying on the first. That is the whole reason a lint
-- wants a different shape from a lookup: a registry answering one question
-- should fail; a linter answering 180 should report.
loadAll :: FilePath -> IO [(Slug.Slug, Either CardError.CardError Card.Card)]
loadAll root = do
  found <- slugsIn root
  mapM (\slug -> fmap ((,) slug) (loadOne root slug)) found

-- Read as bytes and decoded as UTF-8 explicitly, not via Data.Text.IO.readFile:
-- that decodes using the locale encoding, which is ASCII under LC_ALL=C (a
-- minimal CI container, env -i, cron), so a card whose text has a non-ASCII
-- character (khabal-ghoul.json's "a") would fail with an unhelpful "invalid byte
-- sequence" instead of naming the offending file.
loadOne :: FilePath -> Slug.Slug -> IO (Either CardError.CardError Card.Card)
loadOne root slug =
  let path = root <> "/" <> Text.unpack (Slug.unwrap slug) <> ".json"
      name = CardName.MkCardName (Slug.unwrap slug)
      invalid reason = Left (CardError.Invalid name (path <> ": " <> reason))
   in do
        bytes <- ByteString.readFile path
        pure $ case Encoding.decodeUtf8' bytes of
          Left err -> invalid ("not valid UTF-8: " <> show err)
          Right contents -> case Json.parse contents >>= Card.fromJson of
            Left err -> invalid (Text.unpack err)
            Right card ->
              let actual = Slug.fromText (Card.name card)
               in if actual == slug
                    then Right card
                    else
                      invalid
                        ( "is named "
                            <> Text.unpack (Card.name card)
                            <> ", which files under "
                            <> Text.unpack (Slug.unwrap actual)
                        )
