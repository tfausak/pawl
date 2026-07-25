-- Loading cards from a directory of JSON files, one card per file, each named
-- by the slug of the card's own name. This is the library's only module that
-- performs IO: it is the shell around the pure codec, and the only place in the
-- library that touches a file system.
--
-- Every way this can fail has its own type (Pawl.Type.{MissingRoot,
-- UnslugifiableName, UnknownCard, CorruptCard, MisfiledCard,
-- UnslugifiableFile}), thrown as an exception. A caller that wants to say
-- "unknown card X, did you mean...?" catches UnknownCard; one that wants "that
-- file is broken" catches CorruptCard. Before that they were all IOErrors
-- distinguishable only by matching their message prose.
module Pawl.Registry where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Paths_pawl as Paths
import qualified Pawl.Codec as Codec
import qualified Pawl.Json as Json
import qualified Pawl.Slug as Slug
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.CorruptCard as CorruptCard
import qualified Pawl.Type.MisfiledCard as MisfiledCard
import qualified Pawl.Type.MissingRoot as MissingRoot
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Registry as Registry
import qualified Pawl.Type.Slug as Slug.Type
import qualified Pawl.Type.UnknownCard as UnknownCard
import qualified Pawl.Type.UnslugifiableFile as UnslugifiableFile
import qualified Pawl.Type.UnslugifiableName as UnslugifiableName
import qualified System.Directory as Directory
import qualified System.IO.Error as IOError

-- Checks the root here rather than at the first lookup: a mistyped --cards-dir
-- otherwise surfaces as N identical "no such file or directory" failures, one
-- per card, instead of one clear failure at startup.
new :: FilePath -> IO Registry.Registry
new root = do
  exists <- Directory.doesDirectoryExist root
  if not exists
    then Exception.throwIO (MissingRoot.MkMissingRoot root)
    else do
      cache <- MVar.newMVar Map.empty
      pure
        Registry.MkRegistry
          { Registry.root = root,
            Registry.cache = cache
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
-- A .json file whose stem is not already a slug is an error, not a skip: a
-- lookup builds its path FROM a slug, so nothing could ever open that file by
-- name, and quietly omitting it would report a pool larger than the loadable
-- one. Non-.json entries are ignored outright -- a README is not a broken card.
slugs :: Registry.Registry -> IO [Slug.Type.Slug]
slugs registry =
  let json = ".json"
      stem name = take (length name - length json) name
      toSlug name = case Slug.Type.textToSlug (Text.pack (stem name)) of
        Nothing -> Exception.throwIO (UnslugifiableFile.MkUnslugifiableFile (pathIn registry name))
        Just slug -> pure slug
   in do
        entries <- Directory.listDirectory (Registry.root registry)
        mapM toSlug (List.sort (filter (List.isSuffixOf json) entries))

-- The whole pool, loaded. Goes through the same cache as `card`, so a caller
-- that sweeps everything and then looks one card up does not read it twice.
cards :: Registry.Registry -> IO [Card.Card]
cards registry = slugs registry >>= mapM (cached registry)

printings :: Registry.Registry -> IO [Printing.Printing]
printings registry = fmap (fmap Printing.MkPrinting) (cards registry)

-- A card by name ("Goblin Piker") or by slug ("goblin-piker") -- slugify is
-- idempotent, so both are the same lookup.
card :: Registry.Registry -> String -> IO Card.Card
card registry name =
  case Slug.slugify (Text.pack name) of
    Nothing -> Exception.throwIO (UnslugifiableName.MkUnslugifiableName (Text.pack name))
    Just slug -> cached registry slug

printing :: Registry.Registry -> String -> IO Printing.Printing
printing registry name = fmap Printing.MkPrinting (card registry name)

-- Parsed at most once per registry; a failed load is not cached, so a fixed file
-- is picked up by the next lookup (modifyMVar restores the cache when `load`
-- throws).
cached :: Registry.Registry -> Slug.Type.Slug -> IO Card.Card
cached registry slug = MVar.modifyMVar (Registry.cache registry) $ \entries ->
  case Map.lookup slug entries of
    Just c -> pure (entries, c)
    Nothing -> do
      c <- load registry slug
      pure (Map.insert slug c entries, c)

pathIn :: Registry.Registry -> FilePath -> FilePath
pathIn registry name = Registry.root registry <> "/" <> name

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
load :: Registry.Registry -> Slug.Type.Slug -> IO Card.Card
load registry slug =
  let path = pathIn registry (Text.unpack (Slug.Type.slugToText slug) <> ".json")
      corrupt reason = Exception.throwIO (CorruptCard.MkCorruptCard path reason)
   in do
        result <- IOError.tryIOError (ByteString.readFile path)
        case result of
          -- Only a missing file is an UnknownCard. A permission error or a bad
          -- mount is neither that nor a corrupt card, so it passes through as
          -- the IOError it already is.
          Left err ->
            if IOError.isDoesNotExistError err
              then Exception.throwIO (UnknownCard.MkUnknownCard slug path)
              else Exception.throwIO err
          Right bytes -> case Encoding.decodeUtf8' bytes of
            Left err -> corrupt (Text.pack ("not valid UTF-8: " <> show err))
            Right contents ->
              case Json.parse contents >>= Codec.jsonToCard of
                Left err -> corrupt err
                Right c ->
                  let actual = Slug.slugify (Card.name c)
                   in if actual == Just slug
                        then pure c
                        else Exception.throwIO (MisfiledCard.MkMisfiledCard path slug (Card.name c) actual)
