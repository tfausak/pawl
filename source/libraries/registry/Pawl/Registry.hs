-- Answering one question: given a card's name, what card is that?
--
-- The registry is that function and nothing else. It is parameterized over the
-- caller's monad, so a caller who already has the cards -- a fixture map, a
-- pool compiled in, a generated one -- can supply their own without pretending
-- to do IO. Failure is a returned value rather than an exception for the same
-- reason: a pure registry cannot throw. How a registry answers is not part of
-- the type.
--
-- Enumerating the pool is deliberately NOT here: every caller that wanted it
-- was linting the corpus pawl ships, which is a claim about the data rather
-- than a question for a registry, and that lives in the test suite. What the
-- test suite does borrow is cardPath and parseCard -- facts about the on-disk
-- format rather than about looking a card up. Only how the bytes are obtained
-- differs; see loadFile.
module Pawl.Registry where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Paths_pawl as Paths
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Exceptions.MissingRoot as MissingRoot
import qualified Pawl.Slug as Slug
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardError as CardError
import qualified Pawl.Types.CardName as CardName
import qualified System.Directory as Directory
import qualified System.IO.Error as IOError

newtype Registry m = MkRegistry
  { fetchCard :: CardName.CardName -> m (Either CardError.CardError Card.Card)
  }

-- A card by name ("Goblin Piker") or by slug ("goblin-piker") -- a file-backed
-- registry slugifies, and slugify is idempotent, so both are the same lookup.
-- Takes a String because pawl does not enable OverloadedStrings: the newtype is
-- what the interface speaks, this is what callers write.
named :: Registry m -> String -> m (Either CardError.CardError Card.Card)
named registry = fetchCard registry . CardName.MkCardName . Text.pack

-- The card corpus's default root: data/cards declared as cabal data-files,
-- resolved through the cabal-generated Paths_pawl so an installed pawl finds
-- its cards. A default, not a hidden global -- 'fileRegistry' still takes an
-- explicit FilePath.
defaultRoot :: IO FilePath
defaultRoot = Paths.getDataFileName "cards"

-- A registry over a directory of one-card-per-file JSON, each named by the slug
-- of the card's own name, memoized per name.
--
-- The root is checked here rather than at the first lookup because a mistyped
-- --cards-dir otherwise surfaces as N identical failures, one per card, instead
-- of one clear failure at startup (#167).
fileRegistry :: FilePath -> IO (Registry IO)
fileRegistry root = do
  exists <- Directory.doesDirectoryExist root
  if not exists
    then Exception.throwIO MissingRoot.MkMissingRoot {MissingRoot.path = root}
    else do
      -- An MVar rather than an IORef because tasty runs cases concurrently:
      -- holding it across the read-and-parse is what makes "each file is parsed
      -- at most once" exact rather than merely likely. Not a TVar either, since
      -- the lock has to span a readFile, which no transaction can (#265).
      cache <- MVar.newMVar Map.empty
      pure (MkRegistry (memoized root cache))

-- A failed load is not remembered, so a fixed file is picked up by the next
-- lookup (modifyMVar restores the cache when the read throws).
memoized ::
  FilePath ->
  MVar.MVar (Map.Map Slug.Slug (Either CardError.CardError Card.Card)) ->
  CardName.CardName ->
  IO (Either CardError.CardError Card.Card)
memoized root cache name =
  let slug = Slug.fromText (CardName.unwrap name)
   in MVar.modifyMVar cache $ \entries ->
        case Map.lookup slug entries of
          Just hit -> pure (entries, hit)
          Nothing -> do
            result <- loadFile root name slug
            pure (either (const entries) (const (Map.insert slug result entries)) result, result)

-- Where one card's file lives: one file per card, named by the slug of the
-- card's own name.
cardPath :: FilePath -> Slug.Slug -> FilePath
cardPath root slug = root <> "/" <> Text.unpack (Slug.unwrap slug) <> ".json"

-- What a card file's bytes mean. Pure, and separate from reading them, because
-- that is exactly what this module and the test suite's Pawl.Corpus agree
-- about; they disagree about how to obtain the bytes and about what a failure
-- to obtain them means. `path` is carried only to name the file in the error.
--
-- Decoded as UTF-8 explicitly rather than via Data.Text.IO.readFile, which
-- decodes using the locale encoding -- ASCII under LC_ALL=C -- so a card with a
-- non-ASCII character would fail with "invalid byte sequence" instead of naming
-- the offending file.
--
-- The name check: a file's own `name` field must slugify back to the name it is
-- filed under, or a lookup would quietly serve a different card than it was
-- asked for. Both callers inherit it.
parseCard :: CardName.CardName -> Slug.Slug -> FilePath -> ByteString.ByteString -> Either CardError.CardError Card.Card
parseCard name slug path bytes = do
  contents <- either (\err -> invalid ("not valid UTF-8: " <> show err)) Right (Encoding.decodeUtf8' bytes)
  card <- either (invalid . Text.unpack) Right (Common.parse contents >>= Card.fromJson)
  let actual = Slug.fromText . CardName.unwrap $ Card.name card
  if actual == slug
    then Right card
    else invalid ("is named " <> show (Card.name card) <> ", which files under " <> show actual)
  where
    invalid reason = Left (CardError.Invalid name (path <> ": " <> reason))

-- Read one file, and say what a failure to read it means. The registry asks for
-- one named card, so a file that is not there is an answer (CardError.Missing)
-- rather than a crash.
loadFile :: FilePath -> CardName.CardName -> Slug.Slug -> IO (Either CardError.CardError Card.Card)
loadFile root name slug = do
  result <- IOError.tryIOError (ByteString.readFile path)
  case result of
    -- Only a missing file is a Missing card. A permission error or a bad mount
    -- is neither that nor an unusable card, so it passes through as the IOError
    -- it already is.
    Left err
      | IOError.isDoesNotExistError err -> pure (Left (CardError.Missing name))
      | otherwise -> Exception.throwIO err
    Right bytes -> pure (parseCard name slug path bytes)
  where
    path = cardPath root slug
