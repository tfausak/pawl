-- Covers data/cards/*.json and Pawl.Codec.slugify.
module Pawl.CardsSpec where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Pawl.Codec as Codec
import qualified Pawl.Json as Json
import qualified Pawl.Registry as Registry
import qualified Pawl.Support as S
import qualified Pawl.Type.Card as CardT
import qualified Pawl.Type.EntryRewrite as EntryRewrite
import qualified Pawl.Type.Power as Power
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

slugOf :: Printing.Printing -> Text.Text
slugOf = Codec.slugify . CardT.name . Printing.card

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Pawl.CardsSpec"
    [ HU.testCase "each committed file re-parses to its compiled card (P3)" $ do
        ps <- S.allPrintings registry
        mapM_ (checkFile registry) ps,
      HU.testCase "clone.json loads as a 0/0 Shapeshifter with an EntryR AsCopy" $ do
        c <- Registry.card registry "Clone"
        HU.assertEqual "entry replacement" [ReplacementEffect.EntryR EntryRewrite.AsCopy] (CardT.replacementEffects c)
        HU.assertEqual "name" (Text.pack "Clone") (CardT.name c)
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Literal 0))) (CardT.power c)
    ]

checkFile :: Registry.Type.Registry -> Printing.Printing -> HU.Assertion
checkFile registry p = do
  let path = Registry.Type.root registry <> "/" <> Text.unpack (slugOf p) <> ".json"
  -- Read as bytes and decoded as UTF-8 explicitly, matching Pawl.Registry.load:
  -- Data.Text.IO.readFile decodes using the locale encoding, which is ASCII
  -- under LC_ALL=C, so this would otherwise die on khabal-ghoul.json's "á".
  bytes <- ByteString.readFile path
  case Encoding.decodeUtf8' bytes of
    Left err -> HU.assertFailure (path <> ": not valid UTF-8: " <> show err)
    Right contents ->
      case Json.parse contents of
        -- Unreachable: S.allPrintings would have failed in IO first.
        Left err -> HU.assertFailure (path <> ": " <> Text.unpack err)
        Right value ->
          -- The loader reads everything the file says and invents nothing:
          -- re-encoding the loaded printing reproduces the file's meaning. Compared
          -- up to key order and whitespace, because JSON objects are unordered and
          -- formatting is not part of the contract. The corpus is committed
          -- pretty-printed (`jq -S .`) while Json.render emits compact output, so
          -- this can never quietly regress into a byte comparison: every file would
          -- fail at once.
          HU.assertEqual path (Json.sortKeys value) (Json.sortKeys (Codec.printingToJson p))
