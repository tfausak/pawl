-- Covers Pawl.Registry. Every test builds its own corpus in a temporary
-- directory: the committed data/cards is read-only here, and the failure modes
-- (a missing file, a malformed file, a file whose name disagrees with its file
-- name) have no representative in it by construction.
module Pawl.RegistrySpec where

import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Pawl.Exceptions.MissingRoot as MissingRoot
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardError as CardError
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Printing as Printing
import qualified System.Directory as Directory

-- A registry over a throwaway corpus, handing the case both the root (for the
-- paths it asserts on) and the registry itself.
withCorpus :: String -> [(FilePath, Text.Text)] -> (FilePath -> Registry.Registry IO -> IO a) -> IO a
withCorpus label files action =
  S.withCorpusDir label files (\dir -> Registry.fileRegistry dir >>= action dir)

-- Like withCorpus, but writes a single file's raw bytes rather than Text:
-- withCorpus cannot express a file containing invalid UTF-8, since Text.Text
-- cannot hold one. This exercises Registry.loadFile's decodeUtf8' failure branch,
-- otherwise unreached by any case here.
withInvalidUtf8Corpus :: String -> (FilePath -> Registry.Registry IO -> IO a) -> IO a
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
    (Registry.fileRegistry dir >>= action dir)

-- An IO action that must fail with a specific exception VALUE. The `expected`
-- argument fixes Exception.try's type, so no ScopedTypeVariables is needed.
-- Only the root check still throws; a card that will not load is a returned
-- CardError, asserted on directly by the cases below.
expectException :: (Exception.Exception e, Eq e) => Spec.Spec IO n -> String -> e -> IO a -> IO ()
expectException s label expected action = do
  result <- Exception.try action
  case result of
    Left err -> Spec.assertEqWith s label err expected
    Right _ -> Spec.assertFailure s (label <> ": expected " <> show expected <> ", got a value")

-- The message an Invalid carries, for cases that assert on its content rather
-- than on the constructor alone.
reasonOf :: CardError.CardError -> String
reasonOf err = case err of
  CardError.Missing _ -> ""
  CardError.Invalid _ reason -> reason

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Pawl.Registry" $ do
  Spec.it s "a card loads by its real name" $ do
    piker <- S.pikerJson
    withCorpus "by-name" [("goblin-piker.json", piker)] $ \_ registry -> do
      p <- S.printingOf s registry "Goblin Piker"
      Spec.assertEq s (Card.name $ Printing.card p) . CardName.MkCardName $ Text.pack "Goblin Piker"

  Spec.it s "the same card loads by its slug -- slugify is idempotent" $ do
    piker <- S.pikerJson
    withCorpus "by-slug" [("goblin-piker.json", piker)] $ \_ registry -> do
      byName <- Registry.named registry "Goblin Piker"
      bySlug <- Registry.named registry "goblin-piker"
      Spec.assertEq s byName bySlug

  Spec.it s "a card is parsed at most once: the file may vanish after the first load" $ do
    piker <- S.pikerJson
    withCorpus "cached" [("goblin-piker.json", piker)] $ \root registry -> do
      first <- Registry.named registry "Goblin Piker"
      Directory.removeFile (root <> "/goblin-piker.json")
      second <- Registry.named registry "Goblin Piker"
      Spec.assertEqWith s "served from the memo" first second

  -- CardError says nothing about files, because it belongs to the interface and
  -- a map-backed registry has no path to report. What #167 bought survives as
  -- the constructor split: a caller wanting "unknown card X, did you mean...?"
  -- matches Missing, one wanting "that card is broken" matches Invalid, and
  -- neither has to read a message to tell them apart. The cases below assert
  -- the constructor first and the message second.
  Spec.it s "an unknown card is Missing, naming the card"
    . withCorpus "missing" []
    $ \_ registry -> do
      result <- Registry.named registry "Goblin Piker"
      Spec.assertEqWith s "missing, by name" result . Left . CardError.Missing . CardName.MkCardName $ Text.pack "Goblin Piker"

  Spec.it s "a malformed file is Invalid, not Missing"
    . withCorpus "malformed" [("goblin-piker.json", Text.pack "{oh no")]
    $ \root registry -> do
      result <- Registry.named registry "Goblin Piker"
      case result of
        Right _ -> Spec.assertFailure s "expected a malformed file to fail"
        Left err -> do
          Spec.assertBool s (List.isInfixOf (root <> "/goblin-piker.json") (reasonOf err)) ("names the path: " <> show err)
          Spec.assertBool s (not (null (reasonOf err))) "says why"

  Spec.it s "a file whose card is named something else is Invalid, naming both names" $ do
    piker <- S.pikerJson
    withCorpus "misfiled" [("bird-maiden.json", piker)] $ \root registry -> do
      result <- Registry.named registry "Bird Maiden"
      case result of
        Right _ -> Spec.assertFailure s "expected a misfiled card to fail"
        Left err -> do
          Spec.assertBool s (List.isInfixOf (root <> "/bird-maiden.json") (reasonOf err)) ("names the path: " <> show err)
          Spec.assertBool s (List.isInfixOf "Goblin Piker" (reasonOf err)) ("names the card: " <> show err)
          Spec.assertBool s (List.isInfixOf "goblin-piker" (reasonOf err)) ("names where it belongs: " <> show err)

  Spec.it s "a file with invalid UTF-8 bytes is Invalid, naming the path and the decode failure"
    . withInvalidUtf8Corpus "invalid-utf8"
    $ \root registry -> do
      result <- Registry.named registry "Goblin Piker"
      case result of
        Right _ -> Spec.assertFailure s "expected invalid UTF-8 to fail"
        Left err -> do
          Spec.assertBool s (List.isInfixOf (root <> "/goblin-piker.json") (reasonOf err)) ("names the path: " <> show err)
          -- Specifically the decodeUtf8' failure, not merely any Invalid: an
          -- incomplete JSON payload (this file's contents) would fail for an
          -- unrelated reason (missing fields) once decoded, so this pins the
          -- decode branch rather than any downstream one.
          Spec.assertBool s (List.isInfixOf "not valid UTF-8" (reasonOf err)) ("names the decode failure: " <> show err)

  Spec.it s "a failed load is not memoized: fixing the file fixes the lookup" $ do
    piker <- S.pikerJson
    withCorpus "retry" [("goblin-piker.json", Text.pack "{oh no")] $ \root registry -> do
      broken <- Registry.named registry "Goblin Piker"
      Spec.assertBool s (either (const True) (const False) broken) "the malformed file fails first"
      TextIO.writeFile (root <> "/goblin-piker.json") piker
      c <- S.cardOf s registry "Goblin Piker"
      Spec.assertEq s (Card.name c) . CardName.MkCardName $ Text.pack "Goblin Piker"

  -- (b) A mistyped --cards-dir should fail once, at startup, rather than
  -- once per card looked up (#167). This is the one failure that is still an
  -- exception: it is a failure of CONSTRUCTING a file registry, not of
  -- fetching a card, so no CardError can express it.
  Spec.it s "a root that does not exist is rejected when the registry is built, not at the first lookup" $ do
    tmp <- Directory.getTemporaryDirectory
    let missing = tmp <> "/pawl-registry-spec-no-such-root"
    Directory.removePathForcibly missing
    expectException s "missing root" MissingRoot.MkMissingRoot {MissingRoot.path = missing} (Registry.fileRegistry missing)
