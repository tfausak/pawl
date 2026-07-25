-- Covers data/cards/*.json and Pawl.Slug.slugify.
module Pawl.CardsSpec where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Pawl.Binding as Binding
import qualified Pawl.Codec as Codec
import qualified Pawl.Json as Json
import qualified Pawl.Registry as Registry
import qualified Pawl.Slug as Slug
import qualified Pawl.Support as S
import qualified Pawl.Type.Card as CardT
import qualified Pawl.Type.ControllerRelation as ControllerRelation
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.EntryRewrite as EntryRewrite
import qualified Pawl.Type.Power as Power
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.Slug as Slug.Type
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChangePattern as ZoneChangePattern
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

slugOf :: Printing.Printing -> Maybe Slug.Type.Slug
slugOf = Slug.slugify . CardT.name . Printing.card

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
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Literal 0))) (CardT.power c),
      HU.testCase "serum-powder.json loads as a {3} artifact with a CR 103.5b mulligan action" $ do
        c <- Registry.card registry "Serum Powder"
        HU.assertEqual "name" (Text.pack "Serum Powder") (CardT.name c)
        HU.assertEqual "the CR 103.5b action" [Effect.ExileHandThenDraw] (CardT.mulliganAction c)
        HU.assertEqual "one activated ability, the {T}: Add {C} mana ability" 1 (length (CardT.activatedAbilities c)),
      HU.testCase "leyline-of-the-void.json loads with a CR 103.6a action and an Opponents redirect" $ do
        c <- Registry.card registry "Leyline of the Void"
        HU.assertEqual "name" (Text.pack "Leyline of the Void") (CardT.name c)
        HU.assertEqual
          "the CR 103.6a action puts itself onto the battlefield"
          [Effect.MoveToZone Binding.triggerSource Zone.Battlefield]
          (CardT.openingHandAction c)
        HU.assertEqual
          "and the redirect is scoped to an opponent's graveyard"
          [ ReplacementEffect.ZoneChangeR
              (ZoneChangePattern.MkZoneChangePattern Zone.Graveyard ControllerRelation.Opponents)
              Zone.Exile
          ]
          (CardT.replacementEffects c)
    ]

checkFile :: Registry.Type.Registry -> Printing.Printing -> HU.Assertion
checkFile registry p =
  case slugOf p of
    -- Unreachable: every committed card's name slugifies, since Registry.card
    -- already had to slugify it (via Pawl.Slug.slugify) to fetch this printing.
    Nothing -> HU.assertFailure (Text.unpack (CardT.name (Printing.card p)) <> ": does not slugify")
    Just slug -> do
      let path = Registry.Type.root registry <> "/" <> Text.unpack (Slug.Type.slugToText slug) <> ".json"
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
