-- | Rewrites every committed card through the codec, which is what makes the
-- corpus's canonical form checkable: `script/format-json.sh` can only normalize
-- whitespace and key order, while the set of keys a card file carries is the
-- encoder's to decide.
module Main where

import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Printing as Printing
import qualified Pawl.Registry as Registry
import qualified System.Directory as Directory

main :: IO ()
main = do
  root <- Registry.defaultRoot
  files <- Directory.listDirectory root
  mapM_ (rewrite root) (List.sort (filter (List.isSuffixOf ".json") files))

-- | Read as bytes and decoded explicitly, for the reason Pawl.Registry.parseCard
-- is: Data.Text.IO.readFile decodes using the locale encoding, which is ASCII
-- under LC_ALL=C, so this would otherwise die on khabal-ghoul.json's "á".
rewrite :: FilePath -> FilePath -> IO ()
rewrite root file = do
  let path = root <> "/" <> file
  bytes <- ByteString.readFile path
  case Encoding.decodeUtf8' bytes of
    Left err -> fail (path <> ": not valid UTF-8: " <> show err)
    Right contents -> case Common.parse contents >>= Printing.fromJson of
      Left err -> fail (path <> ": " <> Text.unpack err)
      Right printing ->
        ByteString.writeFile path
          . Encoding.encodeUtf8
          $ Common.render (Printing.toJson printing) <> Text.pack "\n"
