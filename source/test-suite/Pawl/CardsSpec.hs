-- Covers data/cards/*.json and Pawl.Slug.slugify.
module Pawl.CardsSpec where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Printing as Printing
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Printing as Printing

spec :: Spec.Spec IO n -> n ()
spec s = Spec.describe s "Pawl.Cards" $ do
  Spec.it s "each committed file re-parses to its compiled card (P3)" $ do
    root <- Registry.defaultRoot
    ps <- S.allPrintings s
    mapM_ (checkFile s root) ps

checkFile :: Spec.Spec IO n -> FilePath -> Printing.Printing -> IO ()
checkFile s root p = do
  let slug = Registry.filedAs (Printing.card p)
  let path = Registry.cardPath root slug
  -- Read as bytes and decoded as UTF-8 explicitly, for the reason
  -- Pawl.Registry.parseCard is: Data.Text.IO.readFile decodes using the locale
  -- encoding, which is ASCII under LC_ALL=C, so this would otherwise die on
  -- khabal-ghoul.json's "á". Not parseCard itself -- this case wants the decoded
  -- JSON value to compare against, not a Card.
  bytes <- ByteString.readFile path
  case Encoding.decodeUtf8' bytes of
    Left err -> Spec.assertFailure s (path <> ": not valid UTF-8: " <> show err)
    Right contents ->
      case Common.parse contents of
        -- Unreachable: S.allPrintings would have failed in IO first.
        Left err -> Spec.assertFailure s (path <> ": " <> Text.unpack err)
        Right value ->
          -- The loader reads everything the file says and invents nothing:
          -- re-encoding the loaded printing reproduces the file's meaning. Compared
          -- up to key order and whitespace, because JSON objects are unordered and
          -- formatting is not part of the contract. The corpus is committed
          -- pretty-printed (`jq -S .`) while Common.render emits compact output, so
          -- this can never quietly regress into a byte comparison: every file would
          -- fail at once.
          Spec.assertEqWith s path (Common.sortKeys (Printing.toJson p)) (Common.sortKeys value)
