-- Covers Pawl.Registry. Every test builds its own corpus in a temporary
-- directory: the committed data/cards is read-only here, and the failure modes
-- (a missing file, a malformed file, two cards claiming one name) have no
-- representative in it by construction.
module Pawl.RegistrySpec where

import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Pawl.Exceptions.InvalidCorpus as InvalidCorpus
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
withInvalidUtf8Corpus :: String -> (FilePath -> IO (Registry.Registry IO) -> IO a) -> IO a
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
    (action dir (Registry.fileRegistry dir))

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

-- The problems an InvalidCorpus carries, for the cases that assert on their
-- content. Pattern-matching the field fixes Exception.try's type, so no
-- ScopedTypeVariables is needed -- the `expectException` precedent.
problemsOf :: Spec.Spec IO n -> String -> IO a -> IO [String]
problemsOf s label action = do
  result <- Exception.try action
  case result of
    Left err -> pure (InvalidCorpus.problems err)
    Right _ -> Spec.assertFailure s (label <> ": expected an InvalidCorpus")

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
      -- CR 709.4a gives a split card two names and no combined one, so the
      -- joined string is not a name a lookup may ask for -- it survived until
      -- now only because it happened to be the file name (#649).
      byJoined <- Registry.named registry "Wax // Wane"
      Spec.assertEqWith s "and not by the two joined" byJoined . Left . CardError.Missing . CardName.MkCardName $ Text.pack "Wax // Wane"

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

  -- A file that will not parse has no face names, so there is no key it could
  -- be filed under and no lookup that could report it. Construction is the only
  -- place it can surface -- and it names every offender at once, not the first
  -- (#167).
  Spec.it s "a corpus with a malformed file is rejected when the registry is built" $ do
    piker <- S.pikerJson
    S.withCorpusDir "malformed" [("goblin-piker.json", piker), ("bird-maiden.json", Text.pack "{oh no")] $ \root -> do
      problems <- problemsOf s "malformed" (Registry.fileRegistry root)
      Spec.assertEqWith s "one problem, for the one bad file" (length problems) 1
      Spec.assertBool s (any (List.isInfixOf (root <> "/bird-maiden.json")) problems) ("names the path: " <> show problems)
      Spec.assertBool s (not (any (List.isInfixOf "goblin-piker") problems)) ("and not the good file: " <> show problems)

  -- CR 709.4a: a name belongs to a card, so one name answering for two cards is
  -- a broken pool rather than a lookup with a preference. Impossible while
  -- filenames were the key, since those are unique; keying by face name makes it
  -- representable, and a silent last-one-wins would be a lookup quietly serving
  -- a card it was not asked for.
  Spec.it s "a corpus where two cards claim one name is rejected when the registry is built" $ do
    waxWane <- S.waxWaneJson
    S.withCorpusDir "ambiguous" [("wax-wane.json", waxWane), ("wane-wax.json", waxWane)] $ \root -> do
      problems <- problemsOf s "ambiguous" (Registry.fileRegistry root)
      Spec.assertBool s (any (List.isInfixOf "wax") problems) ("names the ambiguous name: " <> show problems)
      Spec.assertBool s (any (List.isInfixOf (root <> "/wax-wane.json")) problems) ("names the first claimant: " <> show problems)
      Spec.assertBool s (any (List.isInfixOf (root <> "/wane-wax.json")) problems) ("names the second claimant: " <> show problems)

  -- The filename is a filing convention with no standing in the rules (#649),
  -- so it decides where a file LIVES and never what a lookup may ask for.
  Spec.it s "a card is found by its own name, and never by its file name" $ do
    piker <- S.pikerJson
    withCorpus "misfiled" [("bird-maiden.json", piker)] $ \_ registry -> do
      byOwnName <- Registry.named registry "Goblin Piker"
      Spec.assertBool s (either (const False) (const True) byOwnName) "found by the name the card has"
      byFileName <- Registry.named registry "Bird Maiden"
      Spec.assertEqWith s "and not by the name its file has" byFileName . Left . CardError.Missing . CardName.MkCardName $ Text.pack "Bird Maiden"

  Spec.it s "a corpus with an undecodable file is rejected, naming the decode failure"
    . withInvalidUtf8Corpus "invalid-utf8"
    $ \root build -> do
      problems <- problemsOf s "invalid-utf8" build
      Spec.assertBool s (any (List.isInfixOf (root <> "/goblin-piker.json")) problems) ("names the path: " <> show problems)
      -- Specifically the decodeUtf8' failure, not merely any problem: an
      -- incomplete JSON payload (this file's contents) would fail for an
      -- unrelated reason once decoded, so this pins the decode branch.
      Spec.assertBool s (any (List.isInfixOf "not valid UTF-8") problems) ("names the decode failure: " <> show problems)

  -- (b) A mistyped --cards-dir should fail once, at startup, rather than
  -- once per card looked up (#167). This is the one failure that is still an
  -- exception: it is a failure of CONSTRUCTING a file registry, not of
  -- fetching a card, so no CardError can express it.
  Spec.it s "a root that does not exist is rejected when the registry is built, not at the first lookup" $ do
    tmp <- Directory.getTemporaryDirectory
    let missing = tmp <> "/pawl-registry-spec-no-such-root"
    Directory.removePathForcibly missing
    expectException s "missing root" MissingRoot.MkMissingRoot {MissingRoot.path = missing} (Registry.fileRegistry missing)
