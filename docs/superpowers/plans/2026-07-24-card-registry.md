# Card Registry Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the hand-maintained `Pawl.Cards` record (449 lines, one field
per card) with `Pawl.Registry` — a lazily loading, memoizing card pool keyed on
the slugified card name — so adding a card means adding one JSON file.

**Architecture:** `Codec.slugify` becomes a case-folding, ASCII-transliterating,
idempotent function. `Pawl.Type.Registry` holds a root directory and an
`MVar (Map Text Card)`; `Pawl.Registry` is the library's only IO-performing
module, an IO shell around the pure codec that reads `<root>/<slug>.json` on a
cache miss and verifies the parsed card's own name slugifies back to that slug.
The test suite threads a `Registry` exactly where it threads `Cards` today, and
migrates module by module — `Pawl.Cards` and the registry coexist until the last
task, so every commit compiles and the whole suite passes.

**Tech Stack:** GHC 9.14.1 from the Nix flake, Haskell 2010 (no extensions),
`tasty` + `tasty-hunit` + `tasty-quickcheck`, hand-rolled JSON via `Pawl.Json`.

**Spec:** `docs/superpowers/specs/2026-07-24-card-registry-design.md`. Read the
section a task cites before starting it.

## Global Constraints

- **Haskell 2010, no language extensions.** `NamedFieldPuns` is permitted where
  it improves record-heavy clarity. No `LambdaCase`, no `OverloadedStrings`.
- **No explicit export lists.** `module Pawl.Foo where`.
- **One type per module** under `Pawl.Type.<TypeName>` — type and instances only.
  A `Pawl.Type.*` module may import a sibling `Pawl.Type.*` but never a parent.
- **No partial functions**, written or used. No `head`, `error`, `undefined`, or
  non-exhaustive matches. Failures in IO are `ioError`, never `error`.
- **Constructors take a `Mk` prefix** and never pun the type name: `MkRegistry`.
- **Qualified imports aliased to the last component** (`Data.Map.Strict` → `Map`).
  Documented exception, used by this plan: where `Pawl.Registry` is imported as
  `Registry`, the type module takes a dotted alias —
  `import qualified Pawl.Type.Registry as Registry.Type`. `Pawl.Support` is
  imported as `S`.
- **Derive at least `Eq` and `Show`.** `Registry` is the documented exception: it
  derives `Eq` only, because `MVar` has no `Show` instance. Say so at the type.
- **`Text` not `String`** — except `Registry`'s lookup argument and `root`, which
  are `String`/`FilePath` because their destiny is a path (spec §2).
- **The build must be warning-clean.** `flags: +pedantic` in `cabal.project.local`
  makes `-Weverything` (minus the cabal allow-list) an error. Always build `all`:
  `cabal build all --enable-tests --enable-benchmarks`.
- **Never run two builds concurrently** — `jobs: $ncpus` already saturates the
  machine.
- **`hooky fix` then `hooky run` before every commit.** Both act on *staged*
  files only, so `git add` the paths first; `hooky fix` reformats, so re-add
  afterwards. Stage explicit paths, not `git add -A` — other sessions may share
  this checkout.
- **New modules are discovered by `cabal-gild`**, not hand-edited into
  `pawl.cabal`. `hooky fix` runs it. A *deleted* module needs a direct
  `cabal-gild --io pawl.cabal`.
- **One small complete commit per task**, directly on `main`. Never push.
- Commit messages end with:
  `Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>`

**TDD applies to Tasks 1, 2 and 11** — new behavior, so write the failing test,
run it, watch it fail, then implement. **Tasks 3–10 and 12 are refactors**: the
existing suite is the oracle, so the cycle is "change, build, run the affected
tests, commit". Never weaken an assertion or delete a test to make a check pass.
If the plan looks wrong, stop and say so.

**Running one module's tests:** `cabal test --test-options='-p SUBSTRING'` —
tasty's `-p` substring-matches the full test path. Shell is fish; single quotes
behave.

---

## File Structure

**Created (library):** `source/library/Pawl/Type/Registry.hs` (the type),
`source/library/Pawl/Registry.hs` (loading and caching).

**Created (test suite):** `source/test-suite/Pawl/RegistrySpec.hs`.

**Modified (library):** `source/library/Pawl/Codec.hs` (`slugify` only).

**Modified (test suite):** `Main.hs`, `Pawl/Support.hs`, `Pawl/Cards.hs`, and all
28 `Pawl/*Spec.hs` modules that name a card.

**Modified (data):** `data/cards/khabál-ghoul.json` →
`data/cards/khabal-ghoul.json`, `data/cards/serpent-s-gift.json` →
`data/cards/serpents-gift.json`.

**Modified (benchmark):** `source/benchmark/Main.hs` (drops its private
`loadPrinting`).

**Deleted (end state):** the `Cards` record, `Cards.loadPrinting`,
`Cards.allPrintings`. `Pawl.Cards` survives holding only the four decks.

---

### Task 1: The slug rule

Spec §1. `Codec.slugify` currently lowercases and keeps every alphanumeric
character, including non-ASCII letters (`Khabál Ghoul` → `khabál-ghoul`) and
turns an apostrophe into a separator (`Serpent's Gift` → `serpent-s-gift`). It
becomes case-folding, ASCII-only and apostrophe-dropping. Because
`Pawl.CardSpec` asserts that the `data/cards` listing agrees with
`slugify . name`, the two file renames must land in this same commit.

**Files:**
- Modify: `source/library/Pawl/Codec.hs:1746-1755` (the `-- Slug` section)
- Modify: `source/test-suite/Pawl/CodecSpec.hs` (add a `Tasty.testGroup "slugify"`)
- Modify: `source/test-suite/Pawl/Cards.hs:131,177` (the two changed slugs)
- Rename: `data/cards/khabál-ghoul.json`, `data/cards/serpent-s-gift.json`

**Interfaces:**
- Produces: `Codec.slugify :: Text -> Text` — unchanged signature, new rule.
  Task 2 depends on it being idempotent and ASCII-only.

- [ ] **Step 1: Open the tracking issue for pool-scale slug collisions**

The comment written in Step 4 cites it, so it must exist first.

```bash
gh issue create --label chore --title "Slugs are not unique over the full card pool" --body "Status: not implemented. \`Codec.slugify\` keys the card registry (#145) and is unique over the committed corpus, but not over the full ~34,660-name pool: \`Glimpse the Unthinkable\` collides with \`Glimpse, the Unthinkable\` (Mystery Booster 2 #594, promoTypes [\"playtest\"], legal in no format), five Unknown Event joke cards collide with their real counterparts (\`Lava, Axe\` vs \`Lava Axe\` and siblings), and two blank-name cards (\`_____\`, \`______\`) slugify to the empty string. No two tournament-legal cards collide, so nothing is broken today: a file name holds one card, and the registry rejects an empty slug.

Whoever imports the full pool needs a disambiguation rule (set code plus collector number is the obvious one). Until then the corpus-wide uniqueness check in Pawl.CardSpec is the guard.

Expiry: card-driven (fires when the full pool is imported)."
```

Note the issue number it prints; call it `#N` below.

- [ ] **Step 2: Write the failing tests**

Add to `source/test-suite/Pawl/CodecSpec.hs`, inside the top-level list in
`tests` (the module already imports `Data.Text as Text`, `Pawl.Codec as Codec`,
`Test.Tasty as Tasty`, `Test.Tasty.HUnit as HU` and `Test.Tasty.QuickCheck as
QC` — add any that are missing):

```haskell
      Tasty.testGroup
        "slugify"
        [ HU.testCase "a plain name" $
            HU.assertEqual "goblin-piker" (Text.pack "goblin-piker") (Codec.slugify (Text.pack "Goblin Piker")),
          HU.testCase "an apostrophe is dropped, not separated" $
            HU.assertEqual "serpents-gift" (Text.pack "serpents-gift") (Codec.slugify (Text.pack "Serpent's Gift")),
          HU.testCase "an accented letter folds to ASCII" $
            HU.assertEqual "khabal-ghoul" (Text.pack "khabal-ghoul") (Codec.slugify (Text.pack "Khabál Ghoul")),
          HU.testCase "a comma is one separator, not two" $
            HU.assertEqual "inner-calm" (Text.pack "inner-calm-outer-strength") (Codec.slugify (Text.pack "Inner Calm, Outer Strength")),
          HU.testCase "a split card's slashes collapse" $
            HU.assertEqual "fire-ice" (Text.pack "fire-ice") (Codec.slugify (Text.pack "Fire // Ice")),
          HU.testCase "trailing punctuation is trimmed" $
            HU.assertEqual "no trailing hyphen" (Text.pack "sword-of-dungeons-dragons") (Codec.slugify (Text.pack "Sword of Dungeons & Dragons®")),
          HU.testCase "the eszett folds to ss without a table entry" $
            HU.assertEqual "strasse" (Text.pack "strasse") (Codec.slugify (Text.pack "Straße")),
          HU.testCase "digits survive; the comma between them separates" $
            HU.assertEqual "borrowing-100-000-arrows" (Text.pack "borrowing-100-000-arrows") (Codec.slugify (Text.pack "Borrowing 100,000 Arrows")),
          HU.testCase "a name with no alphanumerics slugifies to nothing" $
            HU.assertEqual "empty" Text.empty (Codec.slugify (Text.pack "_____")),
          QC.testProperty "idempotent: a slug slugifies to itself" $
            \s -> let t = Codec.slugify (Text.pack s) in Codec.slugify t QC.=== t,
          QC.testProperty "the output is ASCII [a-z0-9-] throughout" $
            \s ->
              let ok c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-'
               in QC.property (Text.all ok (Codec.slugify (Text.pack s))),
          QC.testProperty "no leading, trailing, or doubled hyphen" $
            \s ->
              let t = Codec.slugify (Text.pack s)
               in QC.property
                    ( not (Text.isPrefixOf (Text.pack "-") t)
                        && not (Text.isSuffixOf (Text.pack "-") t)
                        && not (Text.isInfixOf (Text.pack "--") t)
                    )
        ],
```

- [ ] **Step 3: Run the tests to verify they fail**

Run: `cabal test --test-options='-p slugify'`
Expected: FAIL — `serpents-gift` (got `serpent-s-gift`), `khabal-ghoul` (got
`khabál-ghoul`), `strasse` (got `strae`), and the ASCII property.

- [ ] **Step 4: Rewrite `slugify`**

Replace `source/library/Pawl/Codec.hs:1746-1755` (the whole `-- Slug` section)
with:

```haskell
-- Slug -----------------------------------------------------------------------

-- The file name for a card: case-folded, transliterated to ASCII, apostrophes
-- dropped, every remaining run of non-alphanumerics (spaces, punctuation, "//")
-- collapsed to a single "-", and the edges trimmed. "Urborg, Tomb of Yawgmoth"
-- -> "urborg-tomb-of-yawgmoth", "Serpent's Gift" -> "serpents-gift",
-- "Khabál Ghoul" -> "khabal-ghoul".
--
-- Case-folding rather than lower-casing so "ß" folds to "ss" with no table
-- entry. The keep-or-separate step is what makes the output ASCII
-- unconditionally: a letter `transliterate` does not carry becomes a separator
-- rather than leaking a non-ASCII byte into a file name.
--
-- Idempotent -- the output is already [a-z0-9-] with no runs and no edge
-- hyphens -- which is why Pawl.Registry needs only one lookup function: a card
-- name and its slug are the same lookup.
--
-- Unique over the committed corpus (Pawl.CardSpec checks it), but not over the
-- full ~34k-name pool: joke-set, playtest and blank-name cards collide (#N).
slugify :: Text -> Text
slugify t =
  let folded = Text.toCaseFold t
      unquoted = Text.filter (/= '\'') folded
      ascii = Text.concatMap transliterate unquoted
      isSlugChar c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
      keep c = if isSlugChar c then c else ' '
   in Text.intercalate (Text.pack "-") (Text.words (Text.map keep ascii))

-- ASCII stand-ins for the accented letters that occur in card names. Applied
-- after case folding, so only the lower-case forms need entries. Everything
-- unlisted falls through to slugify's separator rule.
transliterate :: Char -> Text
transliterate c = case c of
  'à' -> Text.pack "a"
  'á' -> Text.pack "a"
  'â' -> Text.pack "a"
  'ã' -> Text.pack "a"
  'ä' -> Text.pack "a"
  'å' -> Text.pack "a"
  'æ' -> Text.pack "ae"
  'ç' -> Text.pack "c"
  'è' -> Text.pack "e"
  'é' -> Text.pack "e"
  'ê' -> Text.pack "e"
  'ë' -> Text.pack "e"
  'ì' -> Text.pack "i"
  'í' -> Text.pack "i"
  'î' -> Text.pack "i"
  'ï' -> Text.pack "i"
  'ð' -> Text.pack "d"
  'ñ' -> Text.pack "n"
  'ò' -> Text.pack "o"
  'ó' -> Text.pack "o"
  'ô' -> Text.pack "o"
  'õ' -> Text.pack "o"
  'ö' -> Text.pack "o"
  'ø' -> Text.pack "o"
  'ō' -> Text.pack "o"
  'œ' -> Text.pack "oe"
  'ù' -> Text.pack "u"
  'ú' -> Text.pack "u"
  'û' -> Text.pack "u"
  'ü' -> Text.pack "u"
  'ū' -> Text.pack "u"
  'ý' -> Text.pack "y"
  'ÿ' -> Text.pack "y"
  'þ' -> Text.pack "th"
  _ -> Text.singleton c
```

Replace `#N` with the issue number from Step 1. `Data.Char` was imported into
`Pawl.Codec` only for `Char.isAlphaNum` in the old `slugify`; if nothing else
uses it, delete the import or the build fails on `-Wunused-imports`.

- [ ] **Step 5: Rename the two card files and their loader lines**

```bash
git mv "data/cards/khabál-ghoul.json" data/cards/khabal-ghoul.json
git mv data/cards/serpent-s-gift.json data/cards/serpents-gift.json
```

In `source/test-suite/Pawl/Cards.hs`, change the two `loadPrinting` strings:
`"serpent-s-gift"` → `"serpents-gift"` (line 131) and `"khabál-ghoul"` →
`"khabal-ghoul"` (line 177).

- [ ] **Step 6: Build and run the whole suite**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS, including `CardSpec`'s "the data/cards directory and
Cards.allPrintings agree, by slug" — that test is what proves the renames and
the new rule agree.

- [ ] **Step 7: Commit**

```bash
git add source/library/Pawl/Codec.hs source/test-suite/Pawl/CodecSpec.hs source/test-suite/Pawl/Cards.hs data/cards
hooky fix
git add source/library/Pawl/Codec.hs source/test-suite/Pawl/CodecSpec.hs source/test-suite/Pawl/Cards.hs data/cards
hooky run
git commit -m "refactor: slugify to ASCII, case-folded, apostrophe-free (#145)"
```

---

### Task 2: The registry

Spec §2 and §3. Two new library modules and their spec. Nothing else changes —
no caller uses them yet.

**Files:**
- Create: `source/library/Pawl/Type/Registry.hs`
- Create: `source/library/Pawl/Registry.hs`
- Create: `source/test-suite/Pawl/RegistrySpec.hs`
- Modify: `source/test-suite/Main.hs` (wire `RegistrySpec.tests` in)

**Interfaces:**
- Consumes: `Codec.slugify :: Text -> Text` (Task 1), `Codec.jsonToCard :: Value
  -> Either Text Card.Card`, `Json.parse :: Text -> Either Text Value`.
- Produces, relied on by every later task:
  - `Registry.Type.MkRegistry`, fields `Registry.Type.root :: Registry ->
    FilePath` and `Registry.Type.cache`
  - `Registry.new :: FilePath -> IO Registry.Type.Registry`
  - `Registry.card :: Registry.Type.Registry -> String -> IO Card.Card`
  - `Registry.printing :: Registry.Type.Registry -> String -> IO
    Printing.Printing`

- [ ] **Step 1: Write the failing spec**

Create `source/test-suite/Pawl/RegistrySpec.hs`:

```haskell
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
      HU.testCase "an unknown card fails loudly" $
        withCorpus "missing" [] $ \registry ->
          expectIOError "missing file" (Registry.card registry "Goblin Piker"),
      HU.testCase "a malformed file fails loudly" $
        withCorpus "malformed" [("goblin-piker.json", Text.pack "{oh no")] $ \registry ->
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
      HU.testCase "a name with no alphanumerics fails loudly instead of reading .json" $
        withCorpus "empty-slug" [] $ \registry ->
          expectIOError "empty slug" (Registry.card registry "_____"),
      HU.testCase "a failed load is not cached: fixing the file fixes the lookup" $ do
        piker <- pikerJson
        withCorpus "retry" [("goblin-piker.json", Text.pack "{oh no")] $ \registry -> do
          expectIOError "malformed json" (Registry.card registry "Goblin Piker")
          TextIO.writeFile (Registry.Type.root registry <> "/goblin-piker.json") piker
          c <- Registry.card registry "Goblin Piker"
          HU.assertEqual "name" (Text.pack "Goblin Piker") (Card.name c)
    ]
```

Wire it into `source/test-suite/Main.hs`: add
`import qualified Pawl.RegistrySpec as RegistrySpec` and `RegistrySpec.tests,`
to the `testTree` list (it takes no argument).

- [ ] **Step 2: Run the spec to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL to compile — `Could not find module 'Pawl.Registry'`.

- [ ] **Step 3: Write the type**

Create `source/library/Pawl/Type/Registry.hs`:

```haskell
-- The card pool: a root directory of one-card-per-file JSON, plus the cards
-- read from it so far. Not a record of every card -- nothing is read until it is
-- asked for, so a root holding the whole ~34k-card pool costs one MVar.
module Pawl.Type.Registry where

import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Type.Card as Card

data Registry = MkRegistry
  { root :: FilePath,
    -- Keyed by slug (Pawl.Codec.slugify of the card's name). An MVar rather
    -- than an IORef because the test suite is built -threaded and tasty runs
    -- cases concurrently: holding it across the read-and-parse is what makes
    -- "each file is parsed at most once" exact rather than merely likely.
    cache :: MVar.MVar (Map.Map Text.Text Card.Card)
  }
  -- No Show: MVar has no Show instance. Eq is MVar identity, so two registries
  -- over one root are equal only if they share a cache.
  deriving (Eq)
```

- [ ] **Step 4: Write the loader**

Create `source/library/Pawl/Registry.hs`:

```haskell
-- Loading cards from a directory of JSON files, one card per file, each named
-- by the slug of the card's own name. This is the library's only module that
-- performs IO: it is the shell around the pure codec, and the only place in the
-- library that touches a file system.
module Pawl.Registry where

import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Pawl.Codec as Codec
import qualified Pawl.Json as Json
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Registry as Registry

new :: FilePath -> IO Registry.Registry
new root = do
  cache <- MVar.newMVar Map.empty
  pure
    Registry.MkRegistry
      { Registry.root = root,
        Registry.cache = cache
      }

-- A card by name ("Goblin Piker") or by slug ("goblin-piker") -- slugify is
-- idempotent, so both are the same lookup. Parsed at most once per registry; a
-- failed load is not cached, so a fixed file is picked up by the next lookup.
card :: Registry.Registry -> String -> IO Card.Card
card registry name =
  let slug = Codec.slugify (Text.pack name)
   in if Text.null slug
        then ioError (userError ("registry: " <> show name <> " has no slug"))
        else MVar.modifyMVar (Registry.cache registry) $ \cached ->
          case Map.lookup slug cached of
            Just c -> pure (cached, c)
            Nothing -> do
              c <- load registry slug
              pure (Map.insert slug c cached, c)

printing :: Registry.Registry -> String -> IO Printing.Printing
printing registry name = fmap Printing.MkPrinting (card registry name)

-- Read and parse one file. A missing file surfaces as readFile's own IO error,
-- which already names the path. Everything else is a userError naming it.
--
-- The name check is the one thing a per-card load can assert that no sweep has
-- to: a file's own `name` field must slugify back to the name it is filed
-- under, or a lookup would quietly serve a different card than it was asked for.
load :: Registry.Registry -> Text.Text -> IO Card.Card
load registry slug =
  let path = Registry.root registry <> "/" <> Text.unpack slug <> ".json"
   in do
        contents <- TextIO.readFile path
        case Json.parse contents >>= Codec.jsonToCard of
          Left err -> ioError (userError (path <> ": " <> Text.unpack err))
          Right c ->
            let actual = Codec.slugify (Card.name c)
             in if actual == slug
                  then pure c
                  else
                    ioError
                      ( userError
                          ( path
                              <> ": filed under "
                              <> Text.unpack slug
                              <> " but the card is named "
                              <> Text.unpack (Card.name c)
                              <> ", which slugifies to "
                              <> Text.unpack actual
                          )
                      )
```

- [ ] **Step 5: Run the spec to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p RegistrySpec'`
Expected: PASS, 8 cases.

- [ ] **Step 6: Run the whole suite**

Run: `cabal test`
Expected: PASS — nothing else changed yet.

- [ ] **Step 7: Commit**

```bash
git add source/library/Pawl/Registry.hs source/library/Pawl/Type/Registry.hs source/test-suite/Pawl/RegistrySpec.hs source/test-suite/Main.hs pawl.cabal
hooky fix
git add source/library/Pawl/Registry.hs source/library/Pawl/Type/Registry.hs source/test-suite/Pawl/RegistrySpec.hs source/test-suite/Main.hs pawl.cabal
hooky run
git commit -m "feat: add a lazily loading, caching card registry (#145)"
```

---

### Task 3: Support fixtures take printings, not the record

Spec §4. `Pawl.Support`'s fixtures currently take the whole `Cards` record to
reach one or two fields. Making them take `Printing`s decouples them from *how*
a caller got the card, which is what lets Tasks 4–10 migrate one spec module at
a time. Callers in this task keep passing `Cards.xPrinting cards` — the record
is still alive.

The rule: **a fixture that needs one card takes a `Printing`; a fixture that
needs several takes several.** No fixture takes the registry (except the corpus
sweep below), so `Pawl.Support` stays pure.

**Files:**
- Modify: `source/test-suite/Pawl/Support.hs` (lines 432-433, 482-488, 609-610,
  647-648, 678-703, 706-709, 744-745, 956-957, 964-966, 980-987, 990-991,
  1049-1053)
- Modify: every spec module that calls one of the changed fixtures (find them
  with the grep in Step 1)

**Interfaces:**
- Consumes: `Registry.new`, `Registry.printing`, `Registry.Type.root` (Task 2).
- Produces, used by Tasks 4–11:
  - `S.addPiker` is **deleted** — callers use `S.addCreature piker pid gs`
  - `S.mountainsInPlay` is **deleted** — callers use `S.landsInPlay mountain n`
  - `S.pikerCard` is **deleted** — callers use `Printing.card piker`
  - `S.withHumility :: Printing.Printing -> GameState -> GameState`
  - `S.combatBoard :: Printing.Printing -> Int -> Int -> (GameState, [ObjectId], [ObjectId])`
  - `S.anthemEmblemCard :: Printing.Printing -> Card.Type.Card`
  - `S.oneMountainState :: Printing.Printing -> Phase.Phase -> GameState`
  - `S.boardWithCreatureArtifactLand :: Printing.Printing -> Printing.Printing -> Printing.Printing -> GameState` (creature, artifact, land)
  - `S.pikerInHand :: Printing.Printing -> Printing.Printing -> Int -> Phase.Phase -> (GameState, ObjectId)` (land, piker)
  - `S.boltInHand :: Printing.Printing -> Printing.Printing -> Int -> Phase.Phase -> (GameState, ObjectId)` (land, bolt)
  - `S.boltAtBobsPiker :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState, GameState, ObjectId)` (piker, land, bolt)
  - `S.m2aKeywords :: [(String, Keyword.Keyword)]` replaces `S.m2aPrintings`
  - `S.corpusSlugs :: Registry.Type.Registry -> IO [String]`
  - `S.allPrintings :: Registry.Type.Registry -> IO [Printing.Printing]`

- [ ] **Step 1: List the call sites**

```bash
rg -n 'S\.(addPiker|mountainsInPlay|pikerCard|withHumility|combatBoard|anthemEmblemCard|oneMountainState|boardWithCreatureArtifactLand|pikerInHand|boltInHand|boltAtBobsPiker|m2aPrintings)\b' source/test-suite
```

Every hit is edited in Step 3. Keep the list — the build will confirm it.

- [ ] **Step 2: Rewrite the fixtures**

In `source/test-suite/Pawl/Support.hs`:

Delete `addPiker` (lines 432-433), `mountainsInPlay` (646-648) and `pikerCard`
(956-957) outright — each is a one-liner over a fixture that already takes a
`Printing` (`addCreature`, `landsInPlay`, `Printing.card`).

Rewrite the rest, keeping each existing comment:

```haskell
boardWithCreatureArtifactLand :: Printing.Printing -> Printing.Printing -> Printing.Printing -> GameState.GameState
boardWithCreatureArtifactLand creature artifact land =
  let gs0 = Setup.emptyGame bothPlayers
      (_, gs1) = addCreature creature alice gs0
      (_, gs2) = addCreature artifact alice gs1
      (_, gs3) = addCreature land alice gs2
   in gs3

withHumility :: Printing.Printing -> GameState.GameState -> GameState.GameState
withHumility humility gs = snd (addCreature humility bob gs)

pikerInHand :: Printing.Printing -> Printing.Printing -> Int -> Phase.Phase -> (GameState.GameState, ObjectId.ObjectId)
pikerInHand land piker n ph =
  let base = landsInPlay land n
      (oid, gs1) = Game.freshObjectId base
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = alice,
            Object.source = Source.OfCard piker,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
      gs3 =
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.hand = Map.insert alice (Seq.singleton oid) (GameState.hand gs2),
            GameState.phase = ph,
            GameState.activePlayer = alice,
            GameState.priority = Just alice
          }
   in (gs3, oid)

boltInHand :: Printing.Printing -> Printing.Printing -> Int -> Phase.Phase -> (GameState.GameState, ObjectId.ObjectId)
boltInHand land bolt n ph =
  let (gs, oid) = handOne bolt (landsInPlay land n)
   in (gs {GameState.phase = ph}, oid)

combatBoard :: Printing.Printing -> Int -> Int -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
combatBoard piker a b = combatBoardOf (replicate a piker) (replicate b piker)

-- Body unchanged below the first line: only the parameter moved, from the whole
-- record to the one printing it reached for. Keep the LABELED SYNTHETIC comment
-- and the (#125) citation above it.
anthemEmblemCard :: Printing.Printing -> Card.Type.Card
anthemEmblemCard piker =
  (Printing.card piker)
    { Card.Type.staticAbilities =
        [ StaticAbility.MkStaticAbility
            { StaticAbility.affected =
                Affected.Matching
                  Exclusion.IncludesSource
                  (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.ControlledBy PlayerRelation.You]),
              StaticAbility.modification =
                Modification.ModifyPowerToughness (Quantity.Literal 1) (Quantity.Literal 1)
            }
        ]
    }

-- The cards M2a adds, paired with the single keyword each must carry. Named
-- rather than loaded here so Pawl.Support stays pure: the caller loads them.
m2aKeywords :: [(String, Keyword.Keyword)]
m2aKeywords =
  [ ("Bird Maiden", Keyword.Flying),
    ("Nimble Birdsticker", Keyword.Reach),
    ("Ogre Sentry", Keyword.Defender),
    ("Windseeker Centaur", Keyword.Vigilance),
    ("Goblin Chariot", Keyword.Haste)
  ]

-- Body unchanged apart from the source field, which becomes
-- `Object.source = Source.OfCard mountain` (was `Source.OfCard
-- (Cards.mountainPrinting cards)`). The 40-odd GameState fields stay exactly as
-- they are.
oneMountainState :: Printing.Printing -> Phase.Phase -> GameState.GameState
oneMountainState mountain ph = ...

boltAtBobsPiker :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, GameState.GameState, ObjectId.ObjectId)
boltAtBobsPiker piker land bolt =
  let (_, withPiker) = addCreature piker bob (landsInPlay land 1)
      (gs, oid) = handOne bolt withPiker
   in (gs, snd (Engine.runGamePure identityAnswer gs (Cast.castSpell alice oid)), oid)
```

Add the corpus sweep at the end of the module (this is the only Support function
that takes a registry; it exists so a sweep covers *every* file, not only the
ones some test happens to name):

```haskell
-- Every card file in a registry's root, by slug. The corpus-wide checks need
-- the directory listing rather than a hand-kept list: a file nobody loads is
-- exactly the file a hand-kept list forgets.
corpusSlugs :: Registry.Type.Registry -> IO [String]
corpusSlugs registry = do
  entries <- Directory.listDirectory (Registry.Type.root registry)
  let stem name = take (length name - 5) name
  pure (List.sort (fmap stem (filter (List.isSuffixOf ".json") entries)))

allPrintings :: Registry.Type.Registry -> IO [Printing.Printing]
allPrintings registry = do
  slugs <- corpusSlugs registry
  mapM (Registry.printing registry) slugs
```

Add the imports `Pawl.Registry as Registry`, `Pawl.Type.Registry as
Registry.Type` and `System.Directory as Directory` to `Pawl.Support`.

- [ ] **Step 3: Update every call site**

At each hit from Step 1, pass the printing(s) the fixture used to fetch itself,
still from the record. Mechanically:

| Before | After |
| --- | --- |
| `S.addPiker cards pid gs` | `S.addCreature (Cards.pikerPrinting cards) pid gs` |
| `S.mountainsInPlay cards n` | `S.landsInPlay (Cards.mountainPrinting cards) n` |
| `S.pikerCard cards` | `Printing.card (Cards.pikerPrinting cards)` |
| `S.withHumility cards gs` | `S.withHumility (Cards.humilityPrinting cards) gs` |
| `S.combatBoard cards a b` | `S.combatBoard (Cards.pikerPrinting cards) a b` |
| `S.anthemEmblemCard cards` | `S.anthemEmblemCard (Cards.pikerPrinting cards)` |
| `S.oneMountainState cards ph` | `S.oneMountainState (Cards.mountainPrinting cards) ph` |
| `S.boardWithCreatureArtifactLand cards` | `S.boardWithCreatureArtifactLand (Cards.pikerPrinting cards) (Cards.mindslaverPrinting cards) (Cards.mountainPrinting cards)` |
| `S.pikerInHand cards n ph` | `S.pikerInHand (Cards.mountainPrinting cards) (Cards.pikerPrinting cards) n ph` |
| `S.boltInHand cards n ph` | `S.boltInHand (Cards.mountainPrinting cards) (Cards.lightningBoltPrinting cards) n ph` |
| `S.boltAtBobsPiker cards` | `S.boltAtBobsPiker (Cards.pikerPrinting cards) (Cards.mountainPrinting cards) (Cards.lightningBoltPrinting cards)` |

`S.m2aPrintings cards` has one caller, `Pawl.CardSpec`, which has no registry
until Task 10. So in *this* task its test keeps using the record, through an
inline list at the call site:

```haskell
        mapM_
          (\(p, keyword) -> ...assertions...)
          [ (Cards.birdMaidenPrinting cards, Keyword.Flying),
            (Cards.nimbleBirdstickerPrinting cards, Keyword.Reach),
            (Cards.ogreSentryPrinting cards, Keyword.Defender),
            (Cards.windseekerCentaurPrinting cards, Keyword.Vigilance),
            (Cards.goblinChariotPrinting cards, Keyword.Haste)
          ]
```

and Task 10 replaces that list with `S.m2aKeywords` plus `Registry.printing`.

- [ ] **Step 4: Build and run the whole suite**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS, with the same test count as before. Behavior is unchanged — only
who fetches the printing moved.

- [ ] **Step 5: Commit**

```bash
git add source/test-suite
hooky fix
git add source/test-suite
hooky run
git commit -m "refactor(test): Support fixtures take printings, not the Cards record (#145)"
```

---

### Tasks 4-9: Migrate the spec modules

Six commits, each covering a batch of spec modules. The recipe is identical in
all six; only the module list changes. **Read this recipe once and apply it per
batch.** These six share one set of steps rather than repeating them, so each
ticks its own `- [ ] **Task N**` box; the usual
`grep -c -- '- \[ \] **Step'` progress check does not see them, so check
`grep -c -- '- \[ \] **Task'` as well. Both must reach `0`.

For each module in the batch:

1. Change the signature and binder:
   `tests :: Cards.Cards -> Tasty.TestTree` / `tests cards =` becomes
   `tests :: Registry.Type.Registry -> Tasty.TestTree` / `tests registry =`.
2. Swap the import `Pawl.Cards as Cards` for `Pawl.Registry as Registry` and
   `Pawl.Type.Registry as Registry.Type`.
3. In each test case, load the cards it names at the top of the body and use the
   bound printing. `HU.testCase "..." $ let ... in ...` becomes
   `HU.testCase "..." $ do { ...loads...; let ...; ...assertions... }`.
4. In `source/test-suite/Main.hs`, change that module's line in `testTree` from
   `FooSpec.tests cards` to `FooSpec.tests registry`.
5. Build, run that module's tests, move to the next module.

The card name for each old field is in the **Appendix** at the end of this plan.

Worked example — `Pawl.CountSpec` before:

```haskell
tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Count"
    [ HU.testCase "Objects counts the matching members of a zone" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (a1, gs1) = S.addCreature (Cards.swampPrinting cards) S.alice gs0
            (a2, gs2) = S.addCreature (Cards.swampPrinting cards) S.alice gs1
            (b1, gs) = S.addCreature (Cards.swampPrinting cards) S.bob gs2
         in HU.assertEqual "two" 2 (Count.evaluate ...),
```

and after:

```haskell
tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Count"
    [ HU.testCase "Objects counts the matching members of a zone" $ do
        swamp <- Registry.printing registry "Swamp"
        let gs0 = Setup.emptyGame S.bothPlayers
            (a1, gs1) = S.addCreature swamp S.alice gs0
            (a2, gs2) = S.addCreature swamp S.alice gs1
            (b1, gs) = S.addCreature swamp S.bob gs2
        HU.assertEqual "two" 2 (Count.evaluate ...),
```

Note the three shape changes: `$ let … in assertion` → `$ do`, the load lines
first, and the trailing assertion loses its `in`. A case that needs the same
card several times binds it once.

**Before the first batch (Task 4 only), make `Main.hs` carry both.** Change:

```haskell
main :: IO ()
main = do
  registry <- Registry.new "data/cards"
  cards <- Cards.loadCards
  Tasty.defaultMain (testTree registry cards)

testTree :: Registry.Type.Registry -> Cards.Cards -> Tasty.TestTree
testTree registry cards =
```

and pass `registry` to migrated modules, `cards` to the rest. Both stay in use
until Task 12, so no unused-binding warning fires.

**Verification for every batch:**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS, with the same test count as the previous commit. A migration
that changes the count has dropped a case — find it before committing.

**Commit for every batch** (`<batch>` = the modules, e.g. "small specs"):

```bash
git add source/test-suite
hooky fix
git add source/test-suite
hooky run
git commit -m "refactor(test): load cards from the registry in <batch> (#145)"
```

---

- [ ] **Task 4: The small specs**

`TurnSpec`, `ConditionSpec`, `CoreSpec`, `ReplaySpec`, `CopySpec`, `CountSpec`,
`ActivateSpec`. Includes the `Main.hs` two-argument change described above.

- [ ] **Task 5: The codec and effect specs**

`ManaSpec`, `ExpirySpec`, `ModalSpec`, `EventSpec`, `CodecSpec`, `ColorSpec`,
`CastSpec`. `CodecSpec` uses `Cards.allPrintings cards` twice — replace with
`ps <- S.allPrintings registry` inside the test body.

- [ ] **Task 6: Combat and damage**

`MulliganSpec`, `DamageSpec`, `CombatSpec`.

- [ ] **Task 7: Cost, game and player effects**

`CostSpec`, `GameSpec`, `PlayerEffectSpec`.

- [ ] **Task 8: Projection, power/toughness and triggers**

`PowerToughnessSpec`, `ProjectionSpec`, `TriggerSpec`.

- [ ] **Task 9: Replacement and resolve**

`ReplacementSpec`, `ResolveSpec` (the largest module, 145 card references).

---

### Task 10: The corpus-wide specs

`Pawl.CardSpec` and `Pawl.CardsSpec` are the two modules that sweep the whole
corpus rather than naming cards. They migrate to the directory listing, which is
the point of the exercise: a card file nobody registers is currently invisible,
and after this task it cannot be.

**Files:**
- Modify: `source/test-suite/Pawl/CardSpec.hs` (16 `allPrintings` uses, the
  directory-agreement test at 371-387, the M2a list from Task 3)
- Modify: `source/test-suite/Pawl/CardsSpec.hs`
- Modify: `source/test-suite/Main.hs` (two lines)

**Interfaces:**
- Consumes: `S.allPrintings`, `S.corpusSlugs`, `S.m2aKeywords` (Task 3),
  `Registry.printing`, `Registry.card` (Task 2).

- [ ] **Step 1: Migrate `Pawl.CardsSpec`**

Replace its body with the registry-driven form. `Codec.slugify` still keys
everything, so the round-trip test is unchanged in substance:

```haskell
tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Pawl.CardsSpec"
    [ HU.testCase "slugs are unique" $ do
        ps <- S.allPrintings registry
        let slugs = fmap slugOf ps
        HU.assertEqual "unique" (List.sort slugs) (List.sort (List.nub slugs)),
      HU.testCase "each committed file re-parses to its compiled card (P3)" $ do
        ps <- S.allPrintings registry
        mapM_ checkFile ps,
      HU.testCase "clone.json loads as a 0/0 Shapeshifter with an EntryR AsCopy" $ do
        c <- Registry.card registry "Clone"
        HU.assertEqual "entry replacement" [ReplacementEffect.EntryR EntryRewrite.AsCopy] (CardT.replacementEffects c)
        HU.assertEqual "name" (Text.pack "Clone") (CardT.name c)
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Literal 0))) (CardT.power c)
    ]
```

`checkFile` is unchanged except for its comment: the "unreachable, loadCards
would have failed first" note becomes "unreachable: `S.allPrintings` would have
failed in IO first".

- [ ] **Step 2: Migrate `Pawl.CardSpec`'s directory test**

Replace the test at lines 371-387 ("the data/cards directory and
Cards.allPrintings agree, by slug") with the sweep that the registry makes
possible. The name-versus-file-name check now lives in `Registry.load`, so
loading every file *is* the assertion:

```haskell
      HU.testCase "every file in data/cards loads, and its card is named by its file name" $ do
        -- The registry checks name-against-file-name on each load (Pawl.Registry.load),
        -- so sweeping the listing is the whole assertion: a stray file, a file whose
        -- card was renamed, and a file that no test happens to name all fail here.
        -- A hand-kept list is exactly what forgets the file nobody loads.
        slugs <- S.corpusSlugs registry
        HU.assertBool "the corpus is not empty" (not (null slugs))
        mapM_ (Registry.card registry) slugs,
```

- [ ] **Step 3: Migrate the rest of `Pawl.CardSpec`**

Apply the Tasks 4-9 recipe to the remaining cases: 16 `Cards.allPrintings cards`
uses become `ps <- S.allPrintings registry`, and each named field becomes a
`Registry.printing` load (Appendix). Restore the M2a test to the
`S.m2aKeywords`-driven form deferred in Task 3:

```haskell
      HU.testCase "every M2a printing carries exactly its keyword" $
        mapM_
          ( \(name, keyword) -> do
              p <- Registry.printing registry name
              ...the existing per-printing assertions, with `p`...
          )
          S.m2aKeywords,
```

- [ ] **Step 4: Build and run**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS. The test count changes by design here — the directory-agreement
pair of assertions becomes one sweep.

- [ ] **Step 5: Prove the sweep catches a misfiled card**

```bash
cp data/cards/goblin-piker.json data/cards/nonesuch.json
cabal test --test-options='-p "every file in data/cards loads"'
```

Expected: FAIL, naming `nonesuch` and `goblin-piker`. Then:

```bash
rm data/cards/nonesuch.json
```

- [ ] **Step 6: Commit**

```bash
git add source/test-suite
hooky fix
git add source/test-suite
hooky run
git commit -m "refactor(test): sweep the card corpus by directory listing (#145)"
```

---

### Task 11: The decks

The four decks are the last thing reading the record. They become registry-fed
and IO, along with the `Pawl.Support` matchup fixtures and their two callers.

**Files:**
- Modify: `source/test-suite/Pawl/Cards.hs:381-449` (the four decks)
- Modify: `source/test-suite/Pawl/Support.hs:103-119` (`redRed`, `greenBlack`,
  `blueBlack`, `matchups`, `landsOnly`)
- Modify: `source/test-suite/Pawl/SetupSpec.hs`, `source/test-suite/Pawl/PropertySpec.hs`
- Modify: `source/test-suite/Main.hs`

**Interfaces:**
- Produces: `Cards.redDeck`, `greenDeck`, `blueDeck`, `blackDeck ::
  Registry.Type.Registry -> IO Deck.Deck`; `S.redRed`, `S.greenBlack`,
  `S.blueBlack`, `S.landsOnly :: Registry.Type.Registry -> IO (NonEmpty.NonEmpty
  (PlayerId.PlayerId, Deck.Deck))`; `S.matchups :: Registry.Type.Registry -> IO
  [NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck)]`.

- [ ] **Step 1: Rewrite the decks**

In `source/test-suite/Pawl/Cards.hs`, keeping every existing comment (the
swap-in rationales are load-bearing — they explain the 60-card and 120-object
conservation counts):

```haskell
redDeck :: Registry.Type.Registry -> IO Deck.Deck
redDeck registry = do
  mountain <- Registry.printing registry "Mountain"
  piker <- Registry.printing registry "Goblin Piker"
  birdMaiden <- Registry.printing registry "Bird Maiden"
  bolt <- Registry.printing registry "Lightning Bolt"
  blaze <- Registry.printing registry "Blaze"
  dragonFodder <- Registry.printing registry "Dragon Fodder"
  chaosCharm <- Registry.printing registry "Chaos Charm"
  pure $
    Deck.MkDeck $
      Map.fromList
        [ (mountain, 36),
          (piker, 4),
          (birdMaiden, 4),
          (bolt, 4),
          -- ...existing comments, unchanged...
          (blaze, 4),
          (dragonFodder, 4),
          (chaosCharm, 4)
        ]
```

and the same shape for `greenDeck` (Forest 36, War Mammoth 8, Fog 4, Giant
Growth 4, Serpent's Gift 4, Battlegrowth 4), `blueDeck` (Island 40, Unsummon 8,
Divination 8, Tome Scour 4) and `blackDeck` (Swamp 36, Typhoid Rats 8, Drudge
Skeletons 4, Murder 4, Mind Rot 4, Instill Infection 4).

- [ ] **Step 2: Rewrite the matchup fixtures**

In `source/test-suite/Pawl/Support.hs`:

```haskell
redRed :: Registry.Type.Registry -> IO (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
redRed registry = do
  deck <- Cards.redDeck registry
  pure (Setup.mirror deck bothPlayers)

greenBlack :: Registry.Type.Registry -> IO (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
greenBlack registry = do
  green <- Cards.greenDeck registry
  black <- Cards.blackDeck registry
  pure ((alice, green) NonEmpty.:| [(bob, black)])

blueBlack :: Registry.Type.Registry -> IO (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
blueBlack registry = do
  blue <- Cards.blueDeck registry
  black <- Cards.blackDeck registry
  pure ((alice, blue) NonEmpty.:| [(bob, black)])

matchups :: Registry.Type.Registry -> IO [NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck)]
matchups registry = do
  rr <- redRed registry
  gb <- greenBlack registry
  bb <- blueBlack registry
  pure [rr, gb, bb]

-- A 60-basic-land mirror: no spell can be cast and no creature can attack, so the
-- only loss condition reachable is CR 704.5b deck-out. Used by the durable
-- lands-only-decks property.
landsOnly :: Registry.Type.Registry -> IO (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
landsOnly registry = do
  mountain <- Registry.printing registry "Mountain"
  pure (Setup.mirror (Deck.MkDeck (Map.singleton mountain 60)) bothPlayers)
```

- [ ] **Step 3: Migrate `Pawl.PropertySpec`**

The properties reach IO through `QC.ioProperty`, which `Test.Tasty.QuickCheck`
re-exports:

```haskell
propertyTests :: Registry.Type.Registry -> Tasty.TestTree
propertyTests registry =
  Tasty.testGroup
    "..."
    [ QC.testProperty "..." $
        \s -> QC.ioProperty $ do
          ms <- S.matchups registry
          pure (QC.conjoin (fmap (\m -> universalInvariants (S.runRandomGame m s)) ms)),
      QC.testProperty "..." $
        \s -> QC.ioProperty $ do
          decks <- S.landsOnly registry
          let final = S.runRandomGame decks s
          pure (...the existing assertion on `final`...)
    ]
```

Keep `iterations` and every `QC.counterexample` label exactly as they are — the
labels are how a failure names the invariant that broke.

- [ ] **Step 4: Migrate `Pawl.SetupSpec`** with the Tasks 4-9 recipe; its
      `Cards.redDeck cards` uses become `deck <- Cards.redDeck registry`.

- [ ] **Step 5: Build and run**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS. `PropertySpec` plays whole games, so it is the slowest — expect
roughly the same wall time as before; the registry caches, so the decks are
parsed once, not once per seed.

- [ ] **Step 6: Commit**

```bash
git add source/test-suite
hooky fix
git add source/test-suite
hooky run
git commit -m "refactor(test): build the decks from the registry (#145)"
```

---

### Task 12: Delete the record

Nothing reads `Cards.Cards` any more. This task removes it, the loader, and the
benchmark's private copy of the same loading.

**Files:**
- Modify: `source/test-suite/Pawl/Cards.hs` (delete the record, `loadPrinting`,
  `loadCards`, `allPrintings`; keep the four decks)
- Modify: `source/test-suite/Main.hs` (one registry, one-argument `testTree`)
- Modify: `source/benchmark/Main.hs:182-183` (its own `loadPrinting`)

- [ ] **Step 1: Check nothing still names the record**

```bash
rg -n 'Cards\.(Cards|MkCards|loadCards|loadPrinting|allPrintings|\w+Printing)' source
```

Expected: hits only in `source/test-suite/Pawl/Cards.hs` itself. Anything else
means a module was missed in Tasks 4-11 — migrate it before continuing.

- [ ] **Step 2: Delete the record and the loaders**

In `source/test-suite/Pawl/Cards.hs`, delete `data Cards`, `loadPrinting`,
`loadCards` and `allPrintings`, leaving only the four decks. Replace the module
header comment with:

```haskell
-- The decks the random-game properties and setup tests play with. Cards
-- themselves come from Pawl.Registry, one file per card, loaded on demand; this
-- module is only the four hand-tuned 60-card lists, whose comments explain the
-- swap-ins that keep each at 60 cards (and so the CR 400.7 conservation count
-- at 120).
```

- [ ] **Step 3: Simplify `Main.hs`**

```haskell
main :: IO ()
main = do
  registry <- Registry.new "data/cards"
  Tasty.defaultMain (testTree registry)

testTree :: Registry.Type.Registry -> Tasty.TestTree
testTree registry =
  Tasty.testGroup
    "pawl"
    [ CoreSpec.tests registry,
      ...every module taking `registry`, RegistrySpec.tests and the
      no-argument specs (BindingSpec, DecideSpec, DepartureSpec, JsonSpec,
      FilterSpec) unchanged...
    ]
```

`Main.hs` no longer names `Pawl.Cards` at all — the decks are reached through
`SetupSpec` and `Pawl.Support`, not through `Main` — so drop its
`import qualified Pawl.Cards as Cards`, or the build fails on
`-Wunused-imports`.

- [ ] **Step 4: Switch the benchmark to the registry**

In `source/benchmark/Main.hs`, delete its private `loadPrinting` and load
through the registry instead:

```haskell
  registry <- Registry.new "data/cards"
  piker <- Registry.printing registry "Goblin Piker"
```

Match the existing benchmark's structure: whatever it loaded by slug, it now
loads by name from one registry created at the top of `main`.

- [ ] **Step 5: Build, test and benchmark**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test && cabal bench`
Expected: all PASS. The benchmark's numbers should be unchanged — it loads the
same cards, once.

- [ ] **Step 6: Clean build to catch hidden warnings**

Incremental builds hide warnings from unchanged modules, and this task deleted
code in three components.

Run: `cabal clean && cabal build all --enable-tests --enable-benchmarks`
Expected: no warnings (the `pedantic` flag turns any into an error).

- [ ] **Step 7: Commit and close the issue**

```bash
git add source/test-suite source/benchmark pawl.cabal
hooky fix
git add source/test-suite source/benchmark pawl.cabal
hooky run
git commit -m "refactor: retire the Cards record for the registry (#145)"
gh issue close 145 --comment "Done. Cards load from Pawl.Registry by name, parsed on demand and at most once per registry; adding a card is adding one file. The slug rule is Codec.slugify (case-folded, ASCII, apostrophe-free) and Pawl.CardSpec sweeps the directory so a file nobody names still gets loaded and checked."
```

---

## Appendix: field to card name

The `Pawl.Cards` field each spec module uses, and the name to load instead.
`Registry.printing registry "<name>"` for a `Printing`, `Registry.card registry
"<name>"` for a `Card`.

| Field | Name | Field | Name |
| --- | --- | --- | --- |
| `actOfTreasonPrinting` | Act of Treason | `masterThiefPrinting` | Master Thief |
| `aetherChannelerPrinting` | Aether Channeler | `mindRotPrinting` | Mind Rot |
| `angelicEdictPrinting` | Angelic Edict | `mindslaverPrinting` | Mindslaver |
| `aphoticWispsPrinting` | Aphotic Wisps | `mountainPrinting` | Mountain |
| `badMoonPrinting` | Bad Moon | `murderPrinting` | Murder |
| `barbarianOutcastPrinting` | Barbarian Outcast | `nightmarePrinting` | Nightmare |
| `battlegrowthPrinting` | Battlegrowth | `nimbleBirdstickerPrinting` | Nimble Birdsticker |
| `birdMaidenPrinting` | Bird Maiden | `ogreSentryPrinting` | Ogre Sentry |
| `blazePrinting` | Blaze | `opalescencePrinting` | Opalescence |
| `bloodMoonPrinting` | Blood Moon | `palaceJailerPrinting` | Palace Jailer |
| `cancelPrinting` | Cancel | `panglacialWurmPrinting` | Panglacial Wurm |
| `chaosCharmPrinting` | Chaos Charm | `pikerPrinting` | Goblin Piker |
| `clonePrinting` | Clone | `plainsPrinting` | Plains |
| `corpsejackMenacePrinting` | Corpsejack Menace | `primalPlasmaPrinting` | Primal Plasma |
| `crimsonWispsPrinting` | Crimson Wisps | `prodigalSorcererPrinting` | Prodigal Sorcerer |
| `darksteelMyrPrinting` | Darksteel Myr | `reliquaryTowerPrinting` | Reliquary Tower |
| `devoidDronePrinting` | Synthetic Devoid Drone | `reprisalPrinting` | Reprisal |
| `divinationPrinting` | Divination | `restInPeacePrinting` | Rest in Peace |
| `doomBladePrinting` | Doom Blade | `ridgetopRaptorPrinting` | Ridgetop Raptor |
| `doublingSeasonPrinting` | Doubling Season | `ruleOfLawPrinting` | Rule of Law |
| `dragonFodderPrinting` | Dragon Fodder | `sabretoothTigerPrinting` | Sabretooth Tiger |
| `drudgeSkeletonsPrinting` | Drudge Skeletons | `sapphireMedallionPrinting` | Sapphire Medallion |
| `evolvingWildsPrinting` | Evolving Wilds | `sarcomancyPrinting` | Sarcomancy |
| `fireblastPrinting` | Fireblast | `serpentsGiftPrinting` | Serpent's Gift |
| `fogPrinting` | Fog | `silencePrinting` | Silence |
| `forestPrinting` | Forest | `suddenImpactPrinting` | Sudden Impact |
| `giantGrowthPrinting` | Giant Growth | `swampPrinting` | Swamp |
| `glistenerElfPrinting` | Glistener Elf | `syntheticModalActivatedPrinting` | Synthetic Modal Activator |
| `goblinChariotPrinting` | Goblin Chariot | `syntheticModalTriggerPrinting` | Synthetic Modal Trigger |
| `greedPrinting` | Greed | `syntheticRestartPrinting` | Synthetic Restart |
| `hagOfInnerWeaknessPrinting` | Hag of Inner Weakness | `syntheticSubgamePrinting` | Synthetic Subgame |
| `hardenedScalesPrinting` | Hardened Scales | `tarmogoyfPrinting` | Tarmogoyf |
| `humilityPrinting` | Humility | `terrorPrinting` | Terror |
| `innerCalmPrinting` | Inner Calm, Outer Strength | `thaliaPrinting` | Thalia, Guardian of Thraben |
| `instillInfectionPrinting` | Instill Infection | `tidalWavePrinting` | Tidal Wave |
| `islandPrinting` | Island | `tomeScourPrinting` | Tome Scour |
| `khabalGhoulPrinting` | Khabál Ghoul | `twistedImagePrinting` | Twisted Image |
| `landformPrinting` | Landform | `typhoidRatsPrinting` | Typhoid Rats |
| `lightningBoltPrinting` | Lightning Bolt | `unsummonPrinting` | Unsummon |
| `llanowarElvesPrinting` | Llanowar Elves | `urborgPrinting` | Urborg, Tomb of Yawgmoth |
| `longtuskCubPrinting` | Longtusk Cub | `villageRitesPrinting` | Village Rites |
| `magicalHackPrinting` | Magical Hack | `wallOfStonePrinting` | Wall of Stone |
| `warMammothPrinting` | War Mammoth | `windseekerCentaurPrinting` | Windseeker Centaur |

`Khabál Ghoul` keeps its accent in the card's `name` field — only the file name
loses it. Passing either spelling works, since `slugify` folds the accent.
