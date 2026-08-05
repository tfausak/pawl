-- Answering one question: given a card's name, what card is that?
--
-- The registry is that function and nothing else. It is parameterized over the
-- caller's monad, so a caller who already has the cards -- a fixture map, a
-- pool compiled in, a generated one -- can supply their own without pretending
-- to do IO. Failure is a returned value rather than an exception for the same
-- reason: a pure registry cannot throw. How a registry answers is not part of
-- the type.
--
-- Enumerating the pool is deliberately NOT in the INTERFACE: every caller that
-- wanted it was linting the corpus pawl ships, which is a claim about the data
-- rather than a question for a registry, and that lives in the test suite. What
-- the test suite does borrow is cardPath and parseCard -- facts about the
-- on-disk format rather than about looking a card up. Only how the bytes are
-- obtained differs; see loadFile.
--
-- The file-backed registry does list its own root, in byFaceName, to answer one
-- lookup that the one-file-per-name convention cannot (CR 709.4a). That is not
-- the same thing: the listing is not reachable through the Registry record, so
-- it is one lookup's implementation rather than a question a caller can ask.
module Pawl.Registry where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
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
import qualified Pawl.Types.Face as Face
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
            direct <- loadFile root name slug
            result <- case direct of
              Left (CardError.Missing _) -> byFaceName root name slug direct
              _ -> pure direct
            pure (either (const entries) (const (Map.insert slug result entries)) result, result)

-- CR 709.4a: "Each split card has two names." Neither of them is the joined
-- string parseCard files the card under, so a lookup by one half's own name
-- misses the direct path and lands here: list the root, keep the filenames
-- whose slug contains the requested one as a whole hyphen-separated run, and
-- read those in order until one's faces include a face of the requested name.
-- `missing` is what a fruitless scan answers with, so a name no file carries is
-- still the CardError.Missing loadFile already produced -- the fallback can add
-- an answer and never change one.
--
-- Confirmation is by FACE NAME and never by filename: the filename only narrows
-- what is worth reading, so a beeswax-wane.json could not answer for "Wax".
-- Faces are compared by slug for the same reason `named` accepts either form --
-- slugify is idempotent, so a name and its slug are one lookup.
--
-- Not a member of the Registry record, which is what bounds the interface: a
-- caller holding a Registry can ask for a named card and nothing else, so this
-- stays one lookup's implementation. Not a claim about module exports -- pawl
-- writes no export lists, so this function is exported like every other.
--
-- A stopgap either way, since it re-lists the root on every miss and leaves the
-- joined filename standing in for a name the rules do not give the card (#649).
byFaceName ::
  FilePath ->
  CardName.CardName ->
  Slug.Slug ->
  Either CardError.CardError Card.Card ->
  IO (Either CardError.CardError Card.Card)
byFaceName root name slug missing = do
  -- Sorted so the "first" a scan accepts is a fact about the pool rather than
  -- about the order a directory happens to enumerate in.
  files <- fmap List.sort (Directory.listDirectory root)
  scan (filter (containsRun slug) (Maybe.mapMaybe fileSlug files))
  where
    carries card = any ((== slug) . Slug.fromText . CardName.unwrap . Face.name) (Card.faces card)
    scan candidates = case candidates of
      [] -> pure missing
      candidate : rest -> do
        loaded <- loadFile root name candidate
        case loaded of
          Right card | carries card -> pure loaded
          _ -> scan rest

-- The slug a card file is filed under, from its name. Nothing for anything that
-- is not a .json file, which is not a card file at all.
fileSlug :: FilePath -> Maybe Slug.Slug
fileSlug file = fmap Slug.fromText (Text.stripSuffix (Text.pack ".json") (Text.pack file))

-- Whether `part`'s hyphen-separated words appear consecutively in `whole`'s.
-- Whole words rather than a substring, so "wax" runs inside "wax-wane" but not
-- inside "beeswax-wane".
--
-- A narrowing and NOT a guarantee. All it decides is which files are worth
-- reading, so all it buys is that a miss does not parse the whole pool; what
-- makes the answer right is byFaceName's face-name confirmation, which refuses
-- a beeswax-wane.json for "Wax" whether or not this filtered it out first.
-- Loosening this to a plain substring would cost reads and change no answer,
-- which is why the case that pins the behaviour ("a filename that merely
-- contains the name asked for does not answer for it") is written against the
-- confirmation and not against this.
containsRun :: Slug.Slug -> Slug.Slug -> Bool
containsRun part whole = List.isInfixOf (wordsOf part) (wordsOf whole)
  where
    wordsOf = Text.splitOn (Text.singleton '-') . Slug.unwrap

-- Where one card's file lives: one file per card, named by the slug of the
-- card's own name.
cardPath :: FilePath -> Slug.Slug -> FilePath
cardPath root slug = root <> "/" <> Text.unpack (Slug.unwrap slug) <> ".json"

-- What a card file's bytes mean. Pure, and separate from reading them, because
-- that is exactly what a lookup and the corpus-wide lints agree about; they
-- disagree about how to obtain the bytes and about what a failure to obtain
-- them means. The caller pairs the reason with the path.
--
-- Decoded as UTF-8 explicitly rather than via Data.Text.IO.readFile, which
-- decodes using the locale encoding -- ASCII under LC_ALL=C -- so a card with a
-- non-ASCII character would fail with "invalid byte sequence" instead of naming
-- the offending file. The caller pairs the reason with the path.
--
-- Says NOTHING about where the file was found: that is `filedAs` below, and
-- keeping the two apart is what lets a root be read without a filename deciding
-- what any of it answers for.
parseCard :: ByteString.ByteString -> Either Text.Text Card.Card
parseCard bytes = do
  contents <- either (\err -> Left (Text.pack ("not valid UTF-8: " <> show err))) Right (Encoding.decodeUtf8' bytes)
  Common.parse contents >>= Card.fromJson

-- The slug a card's file is named for: its face names joined, slugified.
--
-- A FILING convention and not a name the card has -- CR 709.4a gives a split
-- card two names and no combined one (#649) -- so this decides where a file
-- LIVES and never what a lookup may ask for. Also not Pawl.Engine.Card.combined's
-- name, which is a different question and only coincidentally the same string:
-- CR 715.4 gives an adventurer card its normal face's name alone, so the
-- combined view would name embereth-shieldbreaker while the file is
-- embereth-shieldbreaker-battle-display.
filedAs :: Card.Card -> Slug.Slug
filedAs = Slug.fromText . CardName.unwrap . CardName.join . fmap Face.name . Card.faces

-- Every card file in a root, ascending by path, each paired with what its bytes
-- mean. One IO pass over the whole directory: this is what a registry is built
-- from and what the corpus-wide lints sweep, so neither has to restate how a
-- pool is enumerated.
--
-- Per-file Either rather than an exception, so one pass can name every bad file
-- instead of dying on the first.
--
-- Sorted so that what a caller reports first is a fact about the pool rather
-- than about the order a directory happens to enumerate in. Non-.json entries
-- are ignored outright -- a README is not a broken card.
loadRoot :: FilePath -> IO [(FilePath, Either Text.Text Card.Card)]
loadRoot root = do
  entries <- fmap List.sort (Directory.listDirectory root)
  let paths = fmap (\entry -> root <> "/" <> entry) (filter (List.isSuffixOf ".json") entries)
  mapM (\path -> fmap (\bytes -> (path, parseCard bytes)) (ByteString.readFile path)) paths

-- What a card file's bytes mean, confirmed against the slug it was filed under.
-- A lookup builds a path from a slug, so a file whose card is named otherwise
-- would quietly serve a different card than the one asked for.
parseFiledCard :: Slug.Slug -> ByteString.ByteString -> Either Text.Text Card.Card
parseFiledCard slug bytes = do
  card <- parseCard bytes
  if filedAs card == slug
    then Right card
    else Left (Text.pack ("is named " <> show (CardName.join (fmap Face.name (Card.faces card))) <> ", which files under " <> show (filedAs card)))

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
    Right bytes -> pure (either (Left . CardError.Invalid name . (<>) (path <> ": ") . Text.unpack) Right (parseFiledCard slug bytes))
  where
    path = cardPath root slug
