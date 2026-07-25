-- Loading cards from a directory of JSON files, one card per file, each named
-- by the slug of the card's own name. This is the library's only module that
-- performs IO: it is the shell around the pure codec, and the only place in the
-- library that touches a file system.
module Pawl.Registry where

import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Paths_pawl as Paths
import qualified Pawl.Codec as Codec
import qualified Pawl.Json as Json
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Registry as Registry

new :: FilePath -> IO Registry.Registry
new root = do
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

-- A card by name ("Goblin Piker") or by slug ("goblin-piker") -- slugify is
-- idempotent, so both are the same lookup. Parsed at most once per registry; a
-- failed load is not cached, so a fixed file is picked up by the next lookup.
card :: Registry.Registry -> String -> IO Card.Card
card registry name =
  let slug = Codec.slugify (Text.pack name)
   in if Text.null slug
        then ioError (userError ("registry: " <> show name <> " has no slug"))
        else MVar.modifyMVar (Registry.cache registry) $ \cached ->
          case Map.lookup slug cached of
            Just c -> pure (cached, c)
            Nothing -> do
              c <- load registry slug
              pure (Map.insert slug c cached, c)

printing :: Registry.Registry -> String -> IO Printing.Printing
printing registry name = fmap Printing.MkPrinting (card registry name)

-- Read and parse one file. A missing file surfaces as readFile's own IO error,
-- which already names the path. Everything else is a userError naming it.
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
load :: Registry.Registry -> Text.Text -> IO Card.Card
load registry slug =
  let path = Registry.root registry <> "/" <> Text.unpack slug <> ".json"
   in do
        bytes <- ByteString.readFile path
        case Encoding.decodeUtf8' bytes of
          Left err -> ioError (userError (path <> ": not valid UTF-8: " <> show err))
          Right contents ->
            case Json.parse contents >>= Codec.jsonToCard of
              Left err -> ioError (userError (path <> ": " <> Text.unpack err))
              Right c ->
                let actual = Codec.slugify (Card.name c)
                 in if actual == slug
                      then pure c
                      else
                        ioError
                          ( userError
                              ( path
                                  <> ": filed under "
                                  <> Text.unpack slug
                                  <> " but the card is named "
                                  <> Text.unpack (Card.name c)
                                  <> ", which slugifies to "
                                  <> Text.unpack actual
                              )
                          )
