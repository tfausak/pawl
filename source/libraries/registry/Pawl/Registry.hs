-- The card pool: a root directory of one-card-per-file JSON, plus the cards
-- read from it so far. Not a record of every card -- nothing is read until it is
-- asked for, so a root holding the whole ~34k-card pool costs one MVar.
--
-- Loading cards from a directory of JSON files, one card per file, each named
-- by the slug of the card's own name. This is the library's only module that
-- performs IO: it is the shell around the pure codec, and the only place in the
-- library that touches a file system.
--
-- Every way this can fail has its own type (Pawl.Exceptions.{MissingRoot,
-- UnknownCard, CorruptCard, MisfiledCard}), thrown as an exception. A caller
-- that wants to say "unknown card X, did you mean...?" catches UnknownCard; one
-- that wants "that file is broken" catches CorruptCard. Before that they were
-- all IOErrors distinguishable only by matching their message prose.
module Pawl.Registry where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Paths_pawl as Paths
import qualified Pawl.Codec.All as Codec
import qualified Pawl.Codec.Json as Json
import qualified Pawl.Exceptions.CorruptCard as CorruptCard
import qualified Pawl.Exceptions.MisfiledCard as MisfiledCard
import qualified Pawl.Exceptions.MissingRoot as MissingRoot
import qualified Pawl.Exceptions.UnknownCard as UnknownCard
import qualified Pawl.Slug as Slug
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Printing as Printing
import qualified System.Directory as Directory
import qualified System.IO.Error as IOError

data Registry = MkRegistry
  { root :: FilePath,
    -- Keyed by slug (Pawl.Slug.slugify of the card's name). An MVar rather
    -- than an IORef because the test suite is built -threaded and tasty runs
    -- cases concurrently: holding it across the read-and-parse is what makes
    -- "each file is parsed at most once" exact rather than merely likely.
    -- Not a TVar either, for that same reason: the lock has to span a
    -- readFile, which no transaction can, so the STM equivalent is a TMVar --
    -- this, plus a hand-written copy of base's modifyMVar (#265).
    cache :: MVar.MVar (Map.Map Slug.Slug Card.Card)
  }
  -- No Show: MVar has no Show instance. Eq is MVar identity, so two registries
  -- over one root are equal only if they share a cache.
  deriving (Eq)

-- Checks the root here rather than at the first lookup: a mistyped --cards-dir
-- otherwise surfaces as N identical "no such file or directory" failures, one
-- per card, instead of one clear failure at startup.
new :: FilePath -> IO Registry
new rt = do
  exists <- Directory.doesDirectoryExist rt
  if not exists
    then Exception.throwIO MissingRoot.MkMissingRoot {MissingRoot.path = rt}
    else do
      ch <- MVar.newMVar Map.empty
      pure
        MkRegistry
          { root = rt,
            cache = ch
          }

-- The card corpus's default root: data/cards declared as cabal data-files,
-- resolved through the cabal-generated Paths_pawl so an installed pawl (not
-- just an in-place checkout) finds its cards. A default, not a hidden global:
-- 'new' still takes an explicit FilePath, so this is something callers opt
-- into, and a future CLI's --cards-dir can pass something else entirely.
defaultRoot :: IO FilePath
defaultRoot = Paths.getDataFileName "data/cards"

-- Every card in the pool, by slug, ascending. The listing IS the pool: a
-- deckbuilder, a linter, a scenario loader and "load every card" all need it,
-- and a hand-kept list is exactly what forgets the file nobody loads.
--
-- A file name is slugified, not validated, so a stem that is not already a slug
-- yields a slug naming a DIFFERENT path -- and loading it fails as UnknownCard
-- rather than reporting the mismatch here. Pawl.CardSpec pins every committed
-- file name to its own slug so that never arises in the corpus. Non-.json
-- entries are ignored outright -- a README is not a broken card.
slugs :: Registry -> IO [Slug.Slug]
slugs registry =
  let json = ".json"
      stem name = take (length name - length json) name
      toSlug = Slug.fromText . Text.pack . stem
   in do
        entries <- Directory.listDirectory (root registry)
        pure $ fmap toSlug (List.sort (filter (List.isSuffixOf json) entries))

-- The whole pool, loaded. Goes through the same cache as `card`, so a caller
-- that sweeps everything and then looks one card up does not read it twice.
cards :: Registry -> IO [Card.Card]
cards registry = slugs registry >>= mapM (cached registry)

printings :: Registry -> IO [Printing.Printing]
printings registry = fmap (fmap Printing.MkPrinting) (cards registry)

-- A card by name ("Goblin Piker") or by slug ("goblin-piker") -- slugify is
-- idempotent, so both are the same lookup.
card :: Registry -> String -> IO Card.Card
card registry = cached registry . Slug.fromText . Text.pack

printing :: Registry -> String -> IO Printing.Printing
printing registry name = fmap Printing.MkPrinting (card registry name)

-- Parsed at most once per registry; a failed load is not cached, so a fixed file
-- is picked up by the next lookup (modifyMVar restores the cache when `load`
-- throws).
cached :: Registry -> Slug.Slug -> IO Card.Card
cached registry slug = MVar.modifyMVar (cache registry) $ \entries ->
  case Map.lookup slug entries of
    Just c -> pure (entries, c)
    Nothing -> do
      c <- load registry slug
      pure (Map.insert slug c entries, c)

pathIn :: Registry -> FilePath -> FilePath
pathIn registry name = root registry <> "/" <> name

-- Read and parse one file.
--
-- Read as bytes and decoded as UTF-8 explicitly, not via Data.Text.IO.readFile:
-- that decodes using the locale encoding, which is ASCII under LC_ALL=C (a
-- minimal CI container, env -i, cron), so a card whose text has a non-ASCII
-- character (khabal-ghoul.json's "á") would throw an unhelpful "invalid byte
-- sequence" instead of naming the offending file.
--
-- The name check is the one thing a per-card load can assert that no sweep has
-- to: a file's own `name` field must slugify back to the name it is filed
-- under, or a lookup would quietly serve a different card than it was asked for.
load :: Registry -> Slug.Slug -> IO Card.Card
load registry slug =
  let path = pathIn registry (Text.unpack (Slug.unwrap slug) <> ".json")
      corrupt reason =
        Exception.throwIO
          CorruptCard.MkCorruptCard
            { CorruptCard.path = path,
              CorruptCard.reason = reason
            }
   in do
        result <- IOError.tryIOError (ByteString.readFile path)
        case result of
          -- Only a missing file is an UnknownCard. A permission error or a bad
          -- mount is neither that nor a corrupt card, so it passes through as
          -- the IOError it already is.
          Left err ->
            if IOError.isDoesNotExistError err
              then Exception.throwIO UnknownCard.MkUnknownCard {UnknownCard.slug = slug, UnknownCard.path = path}
              else Exception.throwIO err
          Right bytes -> case Encoding.decodeUtf8' bytes of
            Left err -> corrupt (Text.pack ("not valid UTF-8: " <> show err))
            Right contents ->
              case Json.parse contents >>= Codec.jsonToCard of
                Left err -> corrupt err
                Right c ->
                  let actual = Slug.fromText (Card.name c)
                   in if actual == slug
                        then pure c
                        else
                          Exception.throwIO
                            MisfiledCard.MkMisfiledCard
                              { MisfiledCard.path = path,
                                MisfiledCard.name = Card.name c,
                                MisfiledCard.expected = slug,
                                MisfiledCard.actual = actual
                              }
