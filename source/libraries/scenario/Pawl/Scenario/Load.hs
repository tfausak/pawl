-- Reads scenarios from disk: data/scenarios, and any file a caller names.
module Pawl.Scenario.Load where

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

-- | Every .json file in a root, ascending by path, each with what it decodes
-- to, so one pass names every file that will not. Pawl.Registry.loadRoot's
-- shape and its caveat: a file that cannot be READ still throws.
loadRoot :: FilePath -> IO [(FilePath, Either Text.Text Scenario.Type.Scenario)]
loadRoot root = do
  entries <- fmap List.sort (Directory.listDirectory root)
  let paths = fmap (\entry -> root <> "/" <> entry) (filter (List.isSuffixOf ".json") entries)
  mapM loadFile paths

loadFile :: FilePath -> IO (FilePath, Either Text.Text Scenario.Type.Scenario)
loadFile path = fmap (\bytes -> (path, parse bytes)) (ByteString.readFile path)
