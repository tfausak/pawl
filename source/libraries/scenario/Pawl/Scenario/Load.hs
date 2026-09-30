-- Reads scenarios from disk: data/scenarios, and any file a caller names.
module Pawl.Scenario.Load where

import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Paths_pawl as Paths
import qualified Pawl.Codec.Scenario as Scenario
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Scenario as Scenario.Type
import qualified System.Directory as Directory

-- | data/scenarios as cabal data-files, as Pawl.Registry.defaultRoot finds
-- data/cards.
defaultRoot :: IO FilePath
defaultRoot = Paths.getDataFileName "scenarios"

-- | Decoded as UTF-8 explicitly, for Pawl.Registry.parseCard's reason.
parse :: ByteString.ByteString -> Either Text.Text Scenario.Type.Scenario
parse bytes = do
  contents <- either (\err -> Left (Text.pack ("not valid UTF-8: " <> show err))) Right (Encoding.decodeUtf8' bytes)
  Common.parse contents >>= Codec.decode Scenario.codec

-- | Every .json file beneath a root, ascending by path, each with what it
-- decodes to, so one pass names every file that will not. Pawl.Registry.loadRoot's
-- shape and its caveat: a file that cannot be READ still throws.
loadRoot :: FilePath -> IO [(FilePath, Either Text.Text Scenario.Type.Scenario)]
loadRoot root = do
  paths <- jsonFiles root
  mapM loadFile (List.sort paths)

jsonFiles :: FilePath -> IO [FilePath]
jsonFiles dir = do
  entries <- Directory.listDirectory dir
  fmap concat . Monad.forM entries $ \entry -> do
    let path = dir <> "/" <> entry
    nested <- Directory.doesDirectoryExist path
    if nested
      then jsonFiles path
      else pure [path | List.isSuffixOf ".json" entry]

loadFile :: FilePath -> IO (FilePath, Either Text.Text Scenario.Type.Scenario)
loadFile path = fmap (\bytes -> (path, parse bytes)) (ByteString.readFile path)
