-- Covers Pawl.Registry and Pawl.Type.Registry. Every test builds its own corpus
-- in a temporary directory: the committed data/cards is read-only here, and the
-- failure modes (a missing file, a malformed file, a file whose name disagrees
-- with its file name) have no representative in it by construction.
module Pawl.RegistrySpec where

import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Pawl.Registry as Registry
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Registry as Registry.Type
import qualified System.Directory as Directory
import qualified System.IO.Error as IOError
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- A registry over a throwaway directory holding `files` (name, contents). The
-- label keeps concurrently running cases in separate directories, since tasty
-- runs them in parallel.
withCorpus :: String -> [(FilePath, Text.Text)] -> (Registry.Type.Registry -> IO a) -> IO a
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
pikerJson = TextIO.readFile "data/cards/goblin-piker.json"

-- An IO action that must fail. tryIOError fixes the exception type without
-- ScopedTypeVariables, which this project does not enable.
expectIOError :: String -> IO a -> HU.Assertion
expectIOError label action = do
  result <- IOError.tryIOError action
  case result of
    Left _ -> pure ()
    Right _ -> HU.assertFailure (label <> ": expected an IO error, got a card")

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
          Directory.removeFile (Registry.Type.root registry <> "/goblin-piker.json")
          second <- Registry.card registry "Goblin Piker"
          HU.assertEqual "served from the cache" first second,
      HU.testCase "an unknown card fails loudly"
        . withCorpus "missing" []
        $ \registry ->
          expectIOError "missing file" (Registry.card registry "Goblin Piker"),
      HU.testCase "a malformed file fails loudly"
        . withCorpus "malformed" [("goblin-piker.json", Text.pack "{oh no")]
        $ \registry ->
          expectIOError "malformed json" (Registry.card registry "Goblin Piker"),
      HU.testCase "a file whose card is named something else fails loudly, naming both slugs" $ do
        piker <- pikerJson
        withCorpus "misfiled" [("bird-maiden.json", piker)] $ \registry -> do
          result <- IOError.tryIOError (Registry.card registry "Bird Maiden")
          case result of
            Right _ -> HU.assertFailure "expected an IO error, got a card"
            Left err -> do
              HU.assertBool ("names the file's slug: " <> show err) (List.isInfixOf "bird-maiden" (show err))
              HU.assertBool ("names the card's slug: " <> show err) (List.isInfixOf "goblin-piker" (show err)),
      HU.testCase "a name with no alphanumerics fails loudly instead of reading .json"
        . withCorpus "empty-slug" []
        $ \registry ->
          expectIOError "empty slug" (Registry.card registry "!!!"),
      HU.testCase "a failed load is not cached: fixing the file fixes the lookup" $ do
        piker <- pikerJson
        withCorpus "retry" [("goblin-piker.json", Text.pack "{oh no")] $ \registry -> do
          expectIOError "malformed json" (Registry.card registry "Goblin Piker")
          TextIO.writeFile (Registry.Type.root registry <> "/goblin-piker.json") piker
          c <- Registry.card registry "Goblin Piker"
          HU.assertEqual "name" (Text.pack "Goblin Piker") (Card.name c)
    ]
