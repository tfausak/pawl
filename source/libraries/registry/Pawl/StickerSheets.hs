-- | CR 123.2: the sticker sheets pawl ships, one per file under
-- data/sticker-sheets, read the way Pawl.Registry reads cards. Sheets are not
-- cards, so they are not in the card registry.
module Pawl.StickerSheets where

import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Paths_pawl as Paths
import qualified Pawl.Codec.StickerSheet as StickerSheet
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.StickerSheet as StickerSheet.Type
import qualified System.Directory as Directory

-- | The sheet corpus's default root, Pawl.Registry.defaultRoot's posture.
defaultRoot :: IO FilePath
defaultRoot = Paths.getDataFileName "sticker-sheets"

-- | What a sheet file's bytes mean, Pawl.Registry.parseCard's posture.
parse :: ByteString.ByteString -> Either Text.Text StickerSheet.Type.StickerSheet
parse bytes = do
  contents <- either (\err -> Left (Text.pack ("not valid UTF-8: " <> show err))) Right (Encoding.decodeUtf8' bytes)
  Common.parse contents >>= Codec.decode StickerSheet.codec

-- | Every sheet file in a root, ascending by path, Pawl.Registry.loadRoot's
-- posture.
loadRoot :: FilePath -> IO [(FilePath, Either Text.Text StickerSheet.Type.StickerSheet)]
loadRoot root = do
  entries <- fmap List.sort (Directory.listDirectory root)
  let paths = fmap (\entry -> root <> "/" <> entry) (filter (List.isSuffixOf ".json") entries)
  mapM (\path -> fmap (\bytes -> (path, parse bytes)) (ByteString.readFile path)) paths
