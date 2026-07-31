-- Covers Pawl.Registry and Pawl.Registry. Every test builds its own corpus
-- in a temporary directory: the committed data/cards is read-only here, and the
-- failure modes (a missing file, a malformed file, a file whose name disagrees
-- with its file name) have no representative in it by construction.
module Pawl.RegistrySpec where

import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Pawl.Exceptions.CorruptCard as CorruptCard
import qualified Pawl.Exceptions.MisfiledCard as MisfiledCard
import qualified Pawl.Exceptions.MissingRoot as MissingRoot
import qualified Pawl.Exceptions.UnknownCard as UnknownCard
import qualified Pawl.Registry as Registry
import qualified Pawl.Slug as Slug
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Printing as Printing
import qualified System.Directory as Directory
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- A registry over a throwaway directory holding `files` (name, contents). The
-- label keeps concurrently running cases in separate directories, since tasty
-- runs them in parallel.
withCorpus :: String -> [(FilePath, Text.Text)] -> (Registry.Registry -> IO a) -> IO a
withCorpus label files action = do
  tmp <- Directory.getTemporaryDirectory
  let dir = tmp <> "/pawl-registry-spec-" <> label
  Exception.bracket_
    ( do
        Directory.createDirectoryIfMissing True dir
        mapM_ (\(name, contents) -> TextIO.writeFile (dir <> "/" <> name) contents) files
    )
    (Directory.removeDirectoryRecursive dir)
    (Registry.new dir >>= action)

-- The committed Goblin Piker file, used as a known-good card in a throwaway
-- corpus. Read rather than inlined so this spec never becomes a second source
-- of truth for a card's contents.
pikerJson :: IO Text.Text
pikerJson = do
  root <- Registry.defaultRoot
  TextIO.readFile (root <> "/goblin-piker.json")

-- Like withCorpus, but writes a single file's raw bytes rather than Text:
-- withCorpus cannot express a file containing invalid UTF-8, since Text.Text
-- cannot hold one. This exercises Registry.load's decodeUtf8' failure branch,
-- otherwise unreached by any case here.
withInvalidUtf8Corpus :: String -> (Registry.Registry -> IO a) -> IO a
withInvalidUtf8Corpus label action = do
  tmp <- Directory.getTemporaryDirectory
  let dir = tmp <> "/pawl-registry-spec-" <> label
      -- Valid JSON except for a lone 0xFF byte inside the name string, which
      -- is not valid UTF-8 on its own (it is never a legal leading byte).
      badBytes =
        ByteString.concat
          [ ByteString.Char8.pack "{\"name\": \"Goblin Piker",
            ByteString.singleton 0xFF,
            ByteString.Char8.pack "\"}"
          ]
  Exception.bracket_
    ( do
        Directory.createDirectoryIfMissing True dir
        ByteString.writeFile (dir <> "/goblin-piker.json") badBytes
    )
    (Directory.removeDirectoryRecursive dir)
    (Registry.new dir >>= action)

-- An IO action that must fail with a specific exception VALUE. The `expected`
-- argument fixes Exception.try's type, so no ScopedTypeVariables is needed --
-- which is the point of the typed failures: a caller says which failure it means
-- rather than matching prose (#167).
expectException :: (Exception.Exception e, Eq e) => String -> e -> IO a -> HU.Assertion
expectException label expected action = do
  result <- Exception.try action
  case result of
    Left err -> HU.assertEqual label expected err
    Right _ -> HU.assertFailure (label <> ": expected " <> show expected <> ", got a value")

-- Like expectException where the payload is not worth pinning exactly (a parser
-- message). The continuation's own field accessors fix the exception type.
expectExceptionWith :: (Exception.Exception e) => String -> (e -> HU.Assertion) -> IO a -> HU.Assertion
expectExceptionWith label check action = do
  result <- Exception.try action
  case result of
    Left err -> check err
    Right _ -> HU.assertFailure (label <> ": expected an exception, got a value")

nameOf :: Printing.Printing -> Text.Text
nameOf = Card.name . Printing.card

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Pawl.RegistrySpec"
    [ HU.testCase "a card loads by its real name" $ do
        piker <- pikerJson
        withCorpus "by-name" [("goblin-piker.json", piker)] $ \registry -> do
          p <- Registry.printing registry "Goblin Piker"
          HU.assertEqual "name" (Text.pack "Goblin Piker") (nameOf p),
      HU.testCase "the same card loads by its slug -- slugify is idempotent" $ do
        piker <- pikerJson
        withCorpus "by-slug" [("goblin-piker.json", piker)] $ \registry -> do
          byName <- Registry.card registry "Goblin Piker"
          bySlug <- Registry.card registry "goblin-piker"
          HU.assertEqual "same card" byName bySlug,
      HU.testCase "a card is parsed at most once: the file may vanish after the first load" $ do
        piker <- pikerJson
        withCorpus "cached" [("goblin-piker.json", piker)] $ \registry -> do
          first <- Registry.card registry "Goblin Piker"
          Directory.removeFile (Registry.root registry <> "/goblin-piker.json")
          second <- Registry.card registry "Goblin Piker"
          HU.assertEqual "served from the cache" first second,
      -- The three failure kinds the loader can raise were distinguishable only
      -- by their message prose, so a caller wanting "unknown card X, did you
      -- mean...?" versus "that file is broken" had to string-match a `show`
      -- (#167). Each is now its own type, and these cases catch it AT that type
      -- -- which is the assertion: catching UnknownCard cannot succeed on a
      -- CorruptCard however similar the two messages are.
      HU.testCase "an unknown card raises UnknownCard, naming the slug and the path it looked for"
        . withCorpus "missing" []
        $ \registry ->
          expectExceptionWith
            "missing file"
            ( \err -> do
                HU.assertEqual "names the slug" (Text.pack "goblin-piker") (Slug.unwrap (UnknownCard.slug err))
                HU.assertEqual "names the path" (Registry.root registry <> "/goblin-piker.json") (UnknownCard.path err)
            )
            (Registry.card registry "Goblin Piker"),
      HU.testCase "a malformed file raises CorruptCard, not UnknownCard"
        . withCorpus "malformed" [("goblin-piker.json", Text.pack "{oh no")]
        $ \registry ->
          expectExceptionWith
            "malformed json"
            ( \err -> do
                HU.assertEqual "names the path" (Registry.root registry <> "/goblin-piker.json") (CorruptCard.path err)
                HU.assertBool "says why" (not (Text.null (CorruptCard.reason err)))
            )
            (Registry.card registry "Goblin Piker"),
      HU.testCase "a file whose card is named something else raises MisfiledCard, naming both slugs" $ do
        piker <- pikerJson
        withCorpus "misfiled" [("bird-maiden.json", piker)] $ \registry ->
          expectExceptionWith
            "misfiled card"
            ( \err -> do
                HU.assertEqual "names the path" (Registry.root registry <> "/bird-maiden.json") (MisfiledCard.path err)
                HU.assertEqual "names the card" (Text.pack "Goblin Piker") (MisfiledCard.name err)
                HU.assertEqual "names the file's slug" (Text.pack "bird-maiden") (Slug.unwrap (MisfiledCard.expected err))
                HU.assertEqual "names the card's slug" (Text.pack "goblin-piker") (Slug.unwrap (MisfiledCard.actual err))
            )
            (Registry.card registry "Bird Maiden"),
      HU.testCase "a file with invalid UTF-8 bytes raises CorruptCard naming the path and the decode failure"
        . withInvalidUtf8Corpus "invalid-utf8"
        $ \registry ->
          expectExceptionWith
            "invalid utf-8"
            ( \err -> do
                HU.assertEqual "names the path" (Registry.root registry <> "/goblin-piker.json") (CorruptCard.path err)
                -- Specifically the decodeUtf8' failure, not merely any
                -- CorruptCard: an incomplete JSON payload (this file's
                -- contents) would fail for an unrelated reason (missing
                -- fields) once decoded, so this pins the decode branch rather
                -- than any downstream one.
                HU.assertBool ("names the decode failure: " <> show err) (List.isInfixOf "not valid UTF-8" (Text.unpack (CorruptCard.reason err)))
            )
            (Registry.card registry "Goblin Piker"),
      HU.testCase "a failed load is not cached: fixing the file fixes the lookup" $ do
        piker <- pikerJson
        withCorpus "retry" [("goblin-piker.json", Text.pack "{oh no")] $ \registry -> do
          expectExceptionWith
            "malformed json"
            ( \err -> do
                HU.assertEqual "names the path" (Registry.root registry <> "/goblin-piker.json") (CorruptCard.path err)
                HU.assertBool "says why" (not (Text.null (CorruptCard.reason err)))
            )
            (Registry.card registry "Goblin Piker")
          TextIO.writeFile (Registry.root registry <> "/goblin-piker.json") piker
          c <- Registry.card registry "Goblin Piker"
          HU.assertEqual "name" (Text.pack "Goblin Piker") (Card.name c),
      -- (b) A mistyped --cards-dir should fail once, at startup, rather than
      -- once per card looked up (#167).
      HU.testCase "a root that does not exist is rejected by new, not by the first lookup" $ do
        tmp <- Directory.getTemporaryDirectory
        let missing = tmp <> "/pawl-registry-spec-no-such-root"
        Directory.removePathForcibly missing
        expectException "missing root" MissingRoot.MkMissingRoot {MissingRoot.path = missing} (Registry.new missing),
      -- (a) A CLI, a scenario loader, a deckbuilder and a linter all want "every
      -- card in this pool", which only the test suite could express before.
      HU.testCase "slugs enumerates the pool in ascending order, ignoring non-.json entries" $ do
        piker <- pikerJson
        withCorpus
          "enumerate"
          [ ("goblin-piker.json", piker),
            ("bird-maiden.json", piker),
            ("README.md", Text.pack "not a card")
          ]
          $ \registry -> do
            found <- Registry.slugs registry
            HU.assertEqual "sorted, .json only" [Text.pack "bird-maiden", Text.pack "goblin-piker"] (fmap Slug.unwrap found),
      HU.testCase "cards loads every card the pool enumerates" $ do
        piker <- pikerJson
        withCorpus "load-all" [("goblin-piker.json", piker)] $ \registry -> do
          loaded <- Registry.cards registry
          HU.assertEqual "one card, by name" [Text.pack "Goblin Piker"] (fmap Card.name loaded)
    ]
