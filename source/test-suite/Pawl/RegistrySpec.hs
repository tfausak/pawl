-- Covers Pawl.Registry. Every test builds its own corpus in a temporary
-- directory: the committed data/cards is read-only here, and the failure modes
-- (a missing file, a malformed file, a file whose name disagrees with its file
-- name) have no representative in it by construction.
module Pawl.RegistrySpec where

import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Pawl.Exceptions.MissingRoot as MissingRoot
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardError as CardError
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Printing as Printing
import qualified System.Directory as Directory

-- A registry over a throwaway corpus, handing the case both the root (for the
-- paths it asserts on) and the registry itself.
withCorpus :: String -> [(FilePath, Text.Text)] -> (FilePath -> Registry.Registry IO -> IO a) -> IO a
withCorpus label files action =
  S.withCorpusDir label files (\dir -> Registry.fileRegistry dir >>= action dir)

-- Like withCorpus, but writes a single file's raw bytes rather than Text:
-- withCorpus cannot express a file containing invalid UTF-8, since Text.Text
-- cannot hold one. This exercises Registry.parseCard's decodeUtf8' failure
-- branch, otherwise unreached by any case here.
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
      Spec.assertEq s (S.nameOf $ Printing.card p) . CardName.MkCardName $ Text.pack "Goblin Piker"

  Spec.it s "the same card loads by its slug -- slugify is idempotent" $ do
    piker <- S.pikerJson
    withCorpus "by-slug" [("goblin-piker.json", piker)] $ \_ registry -> do
      byName <- Registry.named registry "Goblin Piker"
      bySlug <- Registry.named registry "goblin-piker"
      Spec.assertEq s byName bySlug

  -- CR 709.4a: "Each split card has two names." Neither of them is the joined
  -- string the file is named for, so a lookup by either half's own name has to
  -- reach the file all the same (#649).
  Spec.it s "CR 709.4a a split card is found by either of its names" $ do
    waxWane <- S.waxWaneJson
    withCorpus "split-by-either-name" [("wax-wane.json", waxWane)] $ \_ registry -> do
      byLeft <- Registry.named registry "Wax"
      byRight <- Registry.named registry "Wane"
      Spec.assertEqWith s "the same one card" byLeft byRight
      -- CR 709.2: "each split card is only one card", so what came back is the
      -- whole two-faced card rather than the half that was asked for. Without
      -- this, a fallback that returned some OTHER card entirely would pass the
      -- equality above.
      Spec.assertEqWith
        s
        "both faces"
        (fmap (fmap Face.name . NonEmpty.toList . Card.faces) byLeft)
        (Right (fmap (CardName.MkCardName . Text.pack) ["Wax", "Wane"]))
      -- The joined name still resolves by the direct path, since "Wax // Wane"
      -- slugifies to the filename: the fallback is an addition, not a
      -- replacement.
      byJoined <- Registry.named registry "Wax // Wane"
      Spec.assertEqWith s "and by the joined name" byJoined byLeft

  -- The rejecting direction of that scan. "goblin-piker.json" is a candidate
  -- for "Goblin" by filename -- the slug is a whole hyphen-separated run of it
  -- -- and is read and parsed, and then refused, because no FACE of it is named
  -- "Goblin". Without this the scan would answer by filename, which is how a
  -- lookup starts serving a card it was not asked for.
  Spec.it s "a filename that merely contains the name asked for does not answer for it" $ do
    piker <- S.pikerJson
    withCorpus "not-a-face-name" [("goblin-piker.json", piker)] $ \_ registry -> do
      result <- Registry.named registry "Goblin"
      Spec.assertEqWith s "still missing" result . Left . CardError.Missing . CardName.MkCardName $ Text.pack "Goblin"

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
      Spec.assertEq s (S.nameOf c) . CardName.MkCardName $ Text.pack "Goblin Piker"

  -- (b) A mistyped --cards-dir should fail once, at startup, rather than
  -- once per card looked up (#167). This is the one failure that is still an
  -- exception: it is a failure of CONSTRUCTING a file registry, not of
  -- fetching a card, so no CardError can express it.
  Spec.it s "a root that does not exist is rejected when the registry is built, not at the first lookup" $ do
    tmp <- Directory.getTemporaryDirectory
    let missing = tmp <> "/pawl-registry-spec-no-such-root"
    Directory.removePathForcibly missing
    expectException s "missing root" MissingRoot.MkMissingRoot {MissingRoot.path = missing} (Registry.fileRegistry missing)
