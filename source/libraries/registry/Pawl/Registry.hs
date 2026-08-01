-- Answering one question: given a card's name, what card is that?
--
-- The registry is that function and nothing else. It is parameterized over the
-- caller's monad, so a caller who already has the cards -- a fixture map, a pool
-- compiled in, a generated one -- can supply their own without pretending to do
-- IO. Failure is a returned value rather than an exception for the same reason:
-- a pure registry cannot throw.
--
-- How a registry answers is not part of the type. fileRegistry below reads one
-- JSON file per name and memoizes; an eager one, a bounded one, a network-backed
-- one are all further values of the same type rather than changes to it. That is
-- also where the 30k-card cache question lands when it arrives.
--
-- Enumerating the pool is deliberately NOT here. Every caller that wanted it was
-- linting the corpus pawl ships, which is a claim about the data rather than a
-- question for a registry; that lives in the test suite now.
module Pawl.Registry where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Paths_pawl as Paths
import Pawl.Codec.Card (jsonToCard)
import qualified Pawl.Codec.Json as Json
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
--
-- Takes a String because pawl does not enable OverloadedStrings, so the newtype
-- at every call site would read
-- `CardName.MkCardName (Text.pack "Goblin Piker")` -- a worse document than the
-- String it replaced. The newtype is what the interface speaks; this is what
-- callers write.
named :: Registry m -> String -> m (Either CardError.CardError Card.Card)
named registry = fetchCard registry . CardName.MkCardName . Text.pack

-- The card corpus's default root: data/cards declared as cabal data-files,
-- resolved through the cabal-generated Paths_pawl so an installed pawl (not
-- just an in-place checkout) finds its cards. A default, not a hidden global:
-- 'fileRegistry' still takes an explicit FilePath, so this is something callers
-- opt into, and a future CLI's --cards-dir can pass something else entirely.
defaultRoot :: IO FilePath
defaultRoot = Paths.getDataFileName "cards"

-- A registry over a directory of one-card-per-file JSON, each named by the slug
-- of the card's own name. Reads a file the first time a name is asked for and
-- remembers the result.
--
-- IO to construct, not merely to use: allocating the memo is an effect, and so
-- is checking the root. The root is checked here rather than at the first lookup
-- because a mistyped --cards-dir otherwise surfaces as N identical failures, one
-- per card, instead of one clear failure at startup (#167).
fileRegistry :: FilePath -> IO (Registry IO)
fileRegistry root = do
  exists <- Directory.doesDirectoryExist root
  if not exists
    then Exception.throwIO MissingRoot.MkMissingRoot {MissingRoot.path = root}
    else do
      -- An MVar rather than an IORef because the test suite is built -threaded
      -- and tasty runs cases concurrently: holding it across the read-and-parse
      -- is what makes "each file is parsed at most once" exact rather than
      -- merely likely. Not a TVar either, for that same reason: the lock has to
      -- span a readFile, which no transaction can, so the STM equivalent is a
      -- TMVar -- this, plus a hand-written copy of base's modifyMVar (#265).
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

-- Read and parse one file.
--
-- Read as bytes and decoded as UTF-8 explicitly, not via Data.Text.IO.readFile:
-- that decodes using the locale encoding, which is ASCII under LC_ALL=C (a
-- minimal CI container, env -i, cron), so a card whose text has a non-ASCII
-- character (khabal-ghoul.json's "a") would fail with an unhelpful "invalid byte
-- sequence" instead of naming the offending file.
--
-- The name check is the one thing a per-card load can assert that no sweep has
-- to: a file's own `name` field must slugify back to the name it is filed under,
-- or a lookup would quietly serve a different card than it was asked for.
loadFile :: FilePath -> CardName.CardName -> Slug.Slug -> IO (Either CardError.CardError Card.Card)
loadFile root name slug =
  let path = root <> "/" <> Text.unpack (Slug.unwrap slug) <> ".json"
      invalid reason = Left (CardError.Invalid name (path <> ": " <> reason))
   in do
        result <- IOError.tryIOError (ByteString.readFile path)
        case result of
          -- Only a missing file is a Missing card. A permission error or a bad
          -- mount is neither that nor an unusable card, so it passes through as
          -- the IOError it already is.
          Left err ->
            if IOError.isDoesNotExistError err
              then pure (Left (CardError.Missing name))
              else Exception.throwIO err
          Right bytes -> pure $ case Encoding.decodeUtf8' bytes of
            Left err -> invalid ("not valid UTF-8: " <> show err)
            Right contents -> case Json.parse contents >>= jsonToCard of
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
