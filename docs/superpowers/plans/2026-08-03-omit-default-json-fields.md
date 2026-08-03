# Omit Default JSON Fields Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Issue:** #629

**Goal:** Make an omitted JSON field mean its default, so the codec stops writing keys that say nothing.

**Architecture:** Three combinators in `Pawl.Codec.Common` replace the hand-rolled `if null then [] else [...]` blocks and the `decode*Default` family. Each record codec declares, per field, whether it is required or carries a default; the encoder omits a defaulted field and the decoder supplies the same value from the same binding. The committed corpus is regenerated from the encoder after each codec change, so the suite is green at every task boundary.

**Tech Stack:** GHC 9.14.1, `tasty`, `Pawl.Spec`, `parsec`, `jq`.

**Spec:** `docs/superpowers/specs/2026-08-03-omit-default-json-fields-design.md`. Rules R1–R7 are cited by number throughout; read that file first.

## Global Constraints

- Toolchain comes from the Nix flake. Run every tool through `direnv exec .` — a bare `cabal`/`hooky`/`jq` may be missing or broken.
- **One build at a time.** `jobs: $ncpus` already saturates the machine; never run two builds or test runs concurrently.
- `cabal build all` must be warning-free — build `all`, not just the library, since the suites break separately.
- `direnv exec . git add <explicit paths>` then `hooky fix`, then `git add` again, then `hooky run`. Hooky acts on **staged** files only. Never `git add -A`: other sessions share this checkout.
- Stay on branch `2026-08-03-test-cleanup`. Never commit to `main`.
- No unchecked numeric conversions: `fromIntegral`, `fromInteger`, `realToFrac`, `toEnum` are banned by `.hlint.yaml`. Convert through `Pawl.Extra.Int`/`Integer`/`Natural`.
- Constructors take a `Mk` prefix. Imports are qualified, aliased to the module's last component.
- Every rules claim is checked against `docs/rules.txt` by rule number, never from memory.
- The rules core must not case on an effect's identity. This plan touches only `Pawl.Codec.*`, `source/executable/Main.hs`, and `data/cards/`; it never touches `Pawl.Engine` or `Pawl.Types`.

---

### Task 1: A corpus formatter that is provably faithful

The corpus's canonical form stops being "what `jq --sort-keys` prints" and becomes "what the encoder writes, then `jq --sort-keys`". `script/format-json.sh` alone can no longer produce it, so the tool that can has to exist before anything is migrated — and it has to be proven faithful while the encoder still writes the current form, when "faithful" means "changes nothing".

**Files:**
- Modify: `source/executable/Main.hs` (currently `main = pure ()`)

**Interfaces:**
- Consumes: `Registry.defaultRoot :: IO FilePath`, `Registry.cardPath :: FilePath -> Slug.Slug -> FilePath`, `Registry.fileRegistry :: FilePath -> IO (Registry IO)`, `Common.render :: Value.Value -> Text.Text`, `Printing.toJson :: Printing.Printing -> Value.Value`
- Produces: a `pawl` executable that rewrites every file in `data/cards/` through the codec. Tasks 3–6 each re-run it.

- [ ] **Step 1: Write the formatter**

`source/executable/Main.hs`:

Each file is rewritten in place through parse-then-encode. Going through the file
rather than through `Registry.named` keeps the program independent of the
name-to-slug mapping: it never has to decide what a file should be *called*, only
what it should *say*.

```haskell
-- | Rewrites every committed card through the codec, which is what makes the
-- corpus's canonical form checkable: `script/format-json.sh` can only normalize
-- whitespace and key order, while the set of keys a card file carries is the
-- encoder's to decide.
module Main where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.ByteString as ByteString
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
-- under LC_ALL=C, so this would otherwise die on khabal-ghoul.json's "a".
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
```

The `pawl` executable stanza in `pawl.cabal` inherits the `executable` import; if
`bytestring`, `directory`, `text`, `codec` or `registry` is missing from its
`build-depends`, add it and run `direnv exec . cabal-gild pawl.cabal` directly —
`hooky fix` only acts on staged files.

- [ ] **Step 2: Build**

Run: `direnv exec . cabal build all`
Expected: warning-free.

- [ ] **Step 3: Prove it faithful — run it and see nothing change**

```bash
direnv exec . cabal run pawl
direnv exec . script/format-json.sh fix data/cards/*.json
git status --short data/cards
```

Expected: **empty output.** The encoder writes exactly what is committed today, so a faithful formatter is a no-op. If any file changes, stop — the formatter is wrong, or the corpus was not canonical, and either way that must be understood before it is used to migrate 226 files.

- [ ] **Step 4: Run the suite**

Run: `direnv exec . cabal test`
Expected: PASS, 2598 tests.

- [ ] **Step 5: Commit**

```bash
direnv exec . git add source/executable/Main.hs pawl.cabal
direnv exec . hooky fix
direnv exec . git add source/executable/Main.hs pawl.cabal
direnv exec . hooky run
direnv exec . git commit -m "Add a corpus formatter that rewrites cards through the codec"
```

---

### Task 2: The three combinators

**Files:**
- Modify: `source/libraries/codec/Pawl/Codec/Common.hs`
- Test: `source/libraries/codec/Pawl/Codec/CommonSpec.hs`

**Interfaces:**
- Produces, used by every later task:
  - `Common.requiredPair :: String -> (a -> Value.Value) -> a -> [Pair.Pair Value.Value]`
  - `Common.optionalPair :: (Eq a) => String -> a -> (a -> Value.Value) -> a -> [Pair.Pair Value.Value]`
  - `Common.defaultedField :: String -> a -> (Value.Value -> Either Text.Text a) -> [Pair.Pair Value.Value] -> Either Text.Text a`

- [ ] **Step 1: Write the failing tests**

Append to `CommonSpec.hs`'s `spec`, following the existing `Spec.describe`/`Spec.it` shape:

```haskell
  Spec.describe s "optionalPair" $ do
    Spec.it s "omits a field equal to its default" $
      Spec.assertEq s (Common.optionalPair "k" (0 :: Integer) Common.integer 0) []
    Spec.it s "writes a field differing from its default" $
      Spec.assertEq s (Common.optionalPair "k" (0 :: Integer) Common.integer 1) [Common.pair "k" (Common.integer 1)]
    -- The default is not required to be the type's zero: R2's enum defaults are
    -- ordinary values, and a field equal to one of those is the omitted case.
    Spec.it s "omits a non-zero default" $
      Spec.assertEq s (Common.optionalPair "k" (7 :: Integer) Common.integer 7) []

  Spec.describe s "requiredPair"
    . Spec.it s "always writes the field"
    $ Spec.assertEq s (Common.requiredPair "k" Common.integer (0 :: Integer)) [Common.pair "k" (Common.integer 0)]

  Spec.describe s "defaultedField" $ do
    Spec.it s "supplies the default for an absent key" $
      Spec.assertEq s (Common.defaultedField "k" (0 :: Integer) Common.asInteger []) (Right 0)
    Spec.it s "decodes a present key" $
      Spec.assertEq s (Common.defaultedField "k" (0 :: Integer) Common.asInteger [Common.pair "k" (Common.integer 1)]) (Right 1)
    -- R7: a present null goes to the decoder rather than short-circuiting to the
    -- default, which is what lets decodeMaybe keep accepting an explicit null.
    Spec.it s "hands a present null to the decoder" $
      Spec.assertEq
        s
        (Common.defaultedField "k" (Just (1 :: Integer)) (Common.decodeMaybe Common.asInteger) [Common.pair "k" Common.null])
        (Right Nothing)
    -- The round trip the two halves have to agree on, stated once here so the
    -- per-codec cases in Task 11 are checking a property this pins down.
    Spec.it s "round-trips the default through an omitted field" $
      Spec.assertEq
        s
        (Common.defaultedField "k" (7 :: Integer) Common.asInteger (Common.optionalPair "k" 7 Common.integer 7))
        (Right 7)
```

- [ ] **Step 2: Run to verify it fails**

Run: `direnv exec . cabal build all`
Expected: FAIL — `Variable not in scope: Common.optionalPair` and the other two.

- [ ] **Step 3: Implement**

In `Common.hs`, replace the `-- Defaults ---` section header comment and add the three functions under a `-- Fields ---` heading, beside `field`/`lookupPair`:

```haskell
-- | A field that is always written, whatever its value. The singleton list is
-- so that 'Common.object . concat' can take required and defaulted fields in one
-- list, with which is which readable down the left edge.
requiredPair :: String -> (a -> Value.Value) -> a -> [Pair.Pair Value.Value]
requiredPair k f x = [pair k (f x)]

-- | A field written only when it differs from the default that an absent key
-- means. The default passed here and the one 'defaultedField' supplies must be
-- the same binding: that is the whole guarantee that a codec's two halves agree.
optionalPair :: (Eq a) => String -> a -> (a -> Value.Value) -> a -> [Pair.Pair Value.Value]
optionalPair k d f x = if x == d then [] else [pair k (f x)]

-- | Reads a field that may be absent, supplying the default 'optionalPair'
-- omits. A key that is present but null goes to the decoder rather than
-- short-circuiting, so composing with 'decodeMaybe' accepts an absent key, an
-- explicit null, and a value alike (R7).
defaultedField ::
  String ->
  a ->
  (Value.Value -> Either Text.Text a) ->
  [Pair.Pair Value.Value] ->
  Either Text.Text a
defaultedField k d f ps = case lookupPair k ps of
  Nothing -> Right d
  Just v -> f v
```

Leave `nullableField` and the `decode*Default` family in place for now — they still have callers, and Task 10 removes them once those are gone.

- [ ] **Step 4: Run to verify it passes**

Run: `direnv exec . cabal build all && direnv exec . cabal test`
Expected: PASS, 2598 + 9 = 2607 tests.

- [ ] **Step 5: Make a failing `assertToJson` say what the encoder wrote**

This is what makes Tasks 3–6 tractable: the literal to paste back is in the failure. Replace `assertToJson`'s body:

```haskell
-- | Compares 'sortKeys'-normalized values, because JSON objects are unordered
-- and key order is not a property the codec has. The failure renders both sides
-- as JSON rather than as 'Value.Value', so a mismatch can be read — and pasted
-- back into the literal — without translating a Show instance by hand.
assertToJson ::
  (Stack.HasCallStack, Monad m) =>
  Spec.Spec m n ->
  (a -> Value.Value) ->
  a ->
  String ->
  m ()
assertToJson s f x j = do
  v <- assertJson s j
  Spec.assertBool
    s
    (sortKeys (f x) == sortKeys v)
    ("encoded " <> Text.unpack (render (f x)) <> " but the literal says " <> j)
```

- [ ] **Step 6: Verify the new message by breaking one literal on purpose**

Edit any one literal in `source/libraries/codec/Pawl/Codec/ColorSpec.hs` to a wrong-but-valid value, run `direnv exec . cabal test`, and confirm the failure prints `encoded {"type":"White"} but the literal says ...`. Then revert the edit.

- [ ] **Step 7: Commit**

```bash
direnv exec . git add source/libraries/codec/Pawl/Codec/Common.hs source/libraries/codec/Pawl/Codec/CommonSpec.hs
direnv exec . hooky fix
direnv exec . git add source/libraries/codec/Pawl/Codec/Common.hs source/libraries/codec/Pawl/Codec/CommonSpec.hs
direnv exec . hooky run
direnv exec . git commit -m "Add requiredPair, optionalPair, and defaultedField"
```

---

### Task 3: TypeLine

First real conversion, and the one with a behavior change beyond omission: R6's non-empty `types`.

**Files:**
- Modify: `source/libraries/codec/Pawl/Codec/TypeLine.hs`
- Test: `source/libraries/codec/Pawl/Codec/TypeLineSpec.hs`
- Modify: `data/cards/*.json` (regenerated)

**Interfaces:**
- Consumes: `Common.requiredPair`, `Common.optionalPair`, `Common.defaultedField` from Task 2; the `pawl` executable from Task 1.
- Produces: `TypeLine.toJson`/`fromJson` with `supertypes` and `subtypes` omissible and `types` required non-empty.

- [ ] **Step 1: Write the failing tests**

Add to `TypeLineSpec.hs`:

```haskell
  -- R6: the typeLine requirement only guards a truncated file if it reaches the
  -- content. An empty types set is the shape a half-written card file takes.
  Spec.it s "rejects an empty types set" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"types":[]} """) >>= TypeLine.fromJson))
      "expected a decode failure"
  Spec.it s "rejects an absent types key" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {} """) >>= TypeLine.fromJson))
      "expected a decode failure"
  Spec.it s "omits empty supertypes and subtypes" $
    Common.assertJsonCodec
      s
      TypeLine.toJson
      TypeLine.fromJson
      (TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Sorcery) Set.empty)
      """ {"types":[{"type":"Sorcery"}]} """
```

Add `import qualified Data.Either as Either`, `import qualified Data.Set as Set`, `import qualified Pawl.Types.CardType as CardType` and `import qualified Data.Text as Text` if absent, and `{-# LANGUAGE MultilineStrings #-}` if absent.

- [ ] **Step 2: Run to verify it fails**

Run: `direnv exec . cabal test 2>&1 | grep -A 3 'Pawl.Codec.TypeLine'`
Expected: the three new cases FAIL.

- [ ] **Step 3: Implement**

```haskell
toJson :: TypeLine.TypeLine -> Value.Value
toJson tl =
  Common.object . concat $
    [ Common.optionalPair "supertypes" Set.empty (Common.encodeSet Supertype.toJson) (TypeLine.supertypes tl),
      Common.requiredPair "types" (Common.encodeSet CardType.toJson) (TypeLine.types tl),
      Common.optionalPair "subtypes" Set.empty (Common.encodeSet Subtype.toJson) (TypeLine.subtypes tl)
    ]

fromJson :: Value.Value -> Either Text.Text TypeLine.TypeLine
fromJson value = do
  ps <- Common.asObject value
  sup <- Common.defaultedField "supertypes" Set.empty (Common.decodeSet Supertype.fromJson) ps
  tys <- Common.field "types" ps >>= Common.decodeSet CardType.fromJson
  -- R6: a card has at least one card type, so an empty set is a malformed file
  -- rather than a card with no types.
  Monad.when (Set.null tys) . Left $ Text.pack "typeLine has no types"
  sub <- Common.defaultedField "subtypes" Set.empty (Common.decodeSet Subtype.fromJson) ps
  pure (TypeLine.MkTypeLine sup tys sub)
```

Add `import qualified Control.Monad as Monad`. Note the trivial defaults (`Set.empty`) are written inline rather than bound: a named binding earns its place only where the default is a *choice* (R2's enums, `ChooseExactly 1`), because two spellings of `Set.empty` cannot disagree.

- [ ] **Step 4: Run to verify the new cases pass and see what else broke**

Run: `direnv exec . cabal test 2>&1 | tail -40`
Expected: the three new cases PASS. Existing `TypeLineSpec` literals and the corpus tests P1/P2/P3 now FAIL, because the encoder omits keys the literals and files still carry. That is expected and the next two steps fix it.

- [ ] **Step 5: Fix this module's literals from the failure output**

Each failure prints `encoded <json> but the literal says <json>`. Replace each affected literal with the `encoded` side, keeping the `""" … """` wrapping and the surrounding spaces.

- [ ] **Step 6: Regenerate the corpus**

```bash
direnv exec . cabal run pawl
direnv exec . script/format-json.sh fix data/cards/*.json
direnv exec . cabal test
```

Expected: PASS. `git diff --stat data/cards` should show `supertypes: []` and `subtypes: []` gone.

- [ ] **Step 7: Commit**

```bash
direnv exec . git add source/libraries/codec/Pawl/Codec/TypeLine.hs source/libraries/codec/Pawl/Codec/TypeLineSpec.hs data/cards
direnv exec . hooky fix
direnv exec . git add source/libraries/codec/Pawl/Codec/TypeLine.hs source/libraries/codec/Pawl/Codec/TypeLineSpec.hs data/cards
direnv exec . hooky run
direnv exec . git commit -m "Omit empty supertypes and subtypes, require a card type"
```

---

### Task 4: Mode and Modal

**Files:**
- Modify: `source/libraries/codec/Pawl/Codec/Mode.hs`, `source/libraries/codec/Pawl/Codec/Modal.hs`
- Modify: `source/libraries/codec/Pawl/Codec/Optionality.hs` (delete `fromJsonDefault`)
- Test: `source/libraries/codec/Pawl/Codec/ModeSpec.hs`, `source/libraries/codec/Pawl/Codec/ModalSpec.hs`
- Modify: `data/cards/*.json`

**Interfaces:**
- Produces: `Modal.defaultSelection :: ModeSelection.ModeSelection`, referenced by `Modal.toJson` and `Modal.fromJson`. Task 5 uses it to build `Card`'s default `spell`.

- [ ] **Step 1: Write the failing tests**

In `ModeSpec.hs`:

```haskell
  Spec.it s "omits every default field" $
    Common.assertJsonCodec
      s
      (Mode.toJson Card.toJson)
      (Mode.fromJson Card.fromJson)
      (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory)
      """ {} """
```

In `ModalSpec.hs`:

```haskell
  Spec.it s "omits a ChooseExactly 1 selection" $
    Common.assertJsonCodec
      s
      (Modal.toJson Card.toJson)
      (Modal.fromJson Card.fromJson)
      (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory)) Modal.defaultSelection)
      """ {"modes":[{}]} """
```

Match the existing imports and the `card` type each spec instantiates at — read the top of each file first; if a spec already parameterizes over a stub codec rather than `Card`, use that instead.

- [ ] **Step 2: Run to verify it fails**

Run: `direnv exec . cabal build all`
Expected: FAIL — `Modal.defaultSelection` not in scope.

- [ ] **Step 3: Implement Modal**

```haskell
-- | CR 700.2 via Pawl.Types.Modal's header: "A non-modal payload is one Mode
-- with ChooseExactly 1", so this is what a card that says nothing about modes
-- means.
defaultSelection :: ModeSelection.ModeSelection
defaultSelection = ModeSelection.ChooseExactly 1

toJson :: (card -> Value.Value) -> Modal.Modal card -> Value.Value
toJson codec m =
  Common.object . concat $
    [ Common.requiredPair "modes" (Common.encodeSeq (Mode.toJson codec)) (Modal.modes m),
      Common.optionalPair "selection" defaultSelection ModeSelection.toJson (Modal.selection m)
    ]

fromJson :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (Modal.Modal card)
fromJson decode value = do
  ps <- Common.asObject value
  ms <- Common.field "modes" ps >>= Common.decodeSeq (Mode.fromJson decode)
  if Seq.null ms
    then Left (Text.pack "modal has no modes")
    else do
      sel <- Common.defaultedField "selection" defaultSelection ModeSelection.fromJson ps
      pure (Modal.MkModal ms sel)
```

`ModeSelection.ChooseExactly` takes a `Natural`; check its arity in `Pawl.Types.ModeSelection` and match it.

- [ ] **Step 4: Implement Mode**

```haskell
toJson :: (card -> Value.Value) -> Mode.Mode card -> Value.Value
toJson codec m =
  Common.object . concat $
    [ Common.optionalPair "effects" Seq.empty (Common.encodeSeq (Effect.toJson codec)) (Mode.effects m),
      Common.optionalPair "targetSpecs" Map.empty TargetSpec.toJsonMap (Mode.targetSpecs m),
      -- R2: Mandatory is the absence of a rider (CR 603.5's "may" is the marked case).
      Common.optionalPair "optionality" Optionality.Mandatory Optionality.toJson (Mode.optionality m)
    ]

fromJson :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (Mode.Mode card)
fromJson decode value = do
  ps <- Common.asObject value
  es <- Common.defaultedField "effects" Seq.empty (Common.decodeSeq (Effect.fromJson decode)) ps
  ts <- Common.defaultedField "targetSpecs" Map.empty TargetSpec.fromJsonMap ps
  o <- Common.defaultedField "optionality" Optionality.Mandatory Optionality.fromJson ps
  pure (Mode.MkMode es ts o)
```

Check `TargetSpec.toJsonMap`'s argument type and use that as the `optionalPair` default (it is a `Map`, so `Map.empty`). Add `import qualified Data.Map.Strict as Map` and `import qualified Data.Sequence as Seq` where absent.

- [ ] **Step 5: Delete the superseded per-type helper**

Remove `fromJsonDefault` from `source/libraries/codec/Pawl/Codec/Optionality.hs` and any case in `OptionalitySpec.hs` that covers it. `defaultedField` takes the default as an argument, which is all `fromJsonDefault` ever did.

Run: `direnv exec . grep -rn 'Optionality.fromJsonDefault' source/`
Expected: no matches.

- [ ] **Step 6: Run, fix this module's literals, regenerate the corpus**

```bash
direnv exec . cabal test 2>&1 | tail -60
```

Fix each `ModeSpec`/`ModalSpec` literal from the `encoded …` side of its failure, then:

```bash
direnv exec . cabal run pawl
direnv exec . script/format-json.sh fix data/cards/*.json
direnv exec . cabal test
```

Expected: PASS. The corpus loses 325 `selection` stanzas and every `"effects": []` / `"targetSpecs": []`.

- [ ] **Step 7: Commit**

```bash
direnv exec . git add source/libraries/codec/Pawl/Codec/Mode.hs source/libraries/codec/Pawl/Codec/Modal.hs source/libraries/codec/Pawl/Codec/Optionality.hs source/libraries/codec/Pawl/Codec/ModeSpec.hs source/libraries/codec/Pawl/Codec/ModalSpec.hs source/libraries/codec/Pawl/Codec/OptionalitySpec.hs data/cards
direnv exec . hooky fix
direnv exec . git add source/libraries/codec/Pawl/Codec/Mode.hs source/libraries/codec/Pawl/Codec/Modal.hs source/libraries/codec/Pawl/Codec/Optionality.hs source/libraries/codec/Pawl/Codec/ModeSpec.hs source/libraries/codec/Pawl/Codec/ModalSpec.hs source/libraries/codec/Pawl/Codec/OptionalitySpec.hs data/cards
direnv exec . hooky run
direnv exec . git commit -m "Default Mode's fields and Modal's selection"
```

---

### Task 5: Card

The largest single change: 12 unconditional keys and 16 hand-rolled blocks collapse into one uniform list, and `spell` becomes omissible.

**Files:**
- Modify: `source/libraries/codec/Pawl/Codec/Card.hs`
- Modify: `source/libraries/codec/Pawl/Codec/Counterability.hs` (delete `fromJsonDefault`)
- Test: `source/libraries/codec/Pawl/Codec/CardSpec.hs`
- Modify: `data/cards/*.json`

**Interfaces:**
- Consumes: `Modal.defaultSelection` from Task 4.
- Produces: `Card.defaultSpell :: Modal.Modal Card.Card`.

- [ ] **Step 1: Write the failing test**

In `CardSpec.hs`:

```haskell
  -- R6: name and typeLine are the only required keys, and a card that says
  -- nothing else is a vanilla card rather than a malformed file.
  Spec.it s "a minimal card carries only name and typeLine" $
    Common.assertJsonCodec
      s
      Card.toJson
      Card.fromJson
      minimalCard
      """ {"name":"Mountain","typeLine":{"types":[{"type":"Land"}]}} """
```

with, beside the existing `baseCard`:

```haskell
-- Every field at the default an omitted key means, which is what the minimal
-- JSON above has to decode to.
minimalCard :: Card.Card
minimalCard =
  Card.MkCard
    { Card.name = CardName.MkCardName (Text.pack "Mountain"),
      Card.manaCost = Nothing,
      Card.typeLine = TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Land) Set.empty,
      Card.power = Nothing,
      Card.toughness = Nothing,
      Card.loyalty = Nothing,
      Card.keywords = Set.empty,
      Card.colorIndicator = Set.empty,
      Card.characteristicPT = Nothing,
      Card.staticAbilities = [],
      Card.spell = Card.defaultSpell,
      Card.activatedAbilities = [],
      Card.replacementEffects = [],
      Card.triggeredAbilities = [],
      Card.delayedAbilities = Map.empty,
      Card.castingPermissions = [],
      Card.castingRestrictions = [],
      Card.enchant = Nothing,
      Card.counterability = Counterability.Counterable,
      Card.additionalCosts = [],
      Card.alternativeCosts = [],
      Card.playerAbilities = [],
      Card.blockRequirements = [],
      Card.attackRequirements = [],
      Card.combatRestrictions = [],
      Card.attackCosts = [],
      Card.mulliganAction = [],
      Card.openingHandAction = []
    }
```

- [ ] **Step 2: Run to verify it fails**

Run: `direnv exec . cabal build all`
Expected: FAIL — `Card.defaultSpell` not in scope.

- [ ] **Step 3: Implement**

Add above `toJson`:

```haskell
-- | What a card that says nothing about its spell means: one mode with no
-- effects and no targets, chosen. Every land and vanilla creature in the pool
-- has exactly this, which is why it is the default rather than a required key.
defaultSpell :: Modal.Modal Card.Card
defaultSpell =
  Modal.MkModal
    (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory))
    Modal.defaultSelection
```

Then replace `toJson`'s body entirely — no two-tier split, no `<>` blocks:

```haskell
toJson :: Card.Card -> Value.Value
toJson c =
  Common.object . concat $
    [ Common.requiredPair "name" CardName.toJson (Card.name c),
      Common.requiredPair "typeLine" TypeLine.toJson (Card.typeLine c),
      Common.optionalPair "manaCost" Nothing (Common.encodeMaybe ManaCost.toJson) (Card.manaCost c),
      Common.optionalPair "power" Nothing (Common.encodeMaybe Power.toJson) (Card.power c),
      Common.optionalPair "toughness" Nothing (Common.encodeMaybe Toughness.toJson) (Card.toughness c),
      Common.optionalPair "loyalty" Nothing (Common.encodeMaybe Loyalty.toJson) (Card.loyalty c),
      Common.optionalPair "characteristicPT" Nothing (Common.encodeMaybe Quantity.toJson) (Card.characteristicPT c),
      Common.optionalPair "enchant" Nothing (Common.encodeMaybe TargetSpec.toJson) (Card.enchant c),
      Common.optionalPair "keywords" Set.empty (Common.encodeSet Keyword.toJson) (Card.keywords c),
      Common.optionalPair "colorIndicator" Set.empty (Common.encodeSet Color.toJson) (Card.colorIndicator c),
      Common.optionalPair "spell" defaultSpell (Modal.toJson toJson) (Card.spell c),
      Common.optionalPair "staticAbilities" [] (Common.encodeList StaticAbility.toJson) (Card.staticAbilities c),
      Common.optionalPair "activatedAbilities" [] (Common.encodeList (ActivatedAbility.toJson toJson)) (Card.activatedAbilities c),
      Common.optionalPair "replacementEffects" [] (Common.encodeList ReplacementEffect.toJson) (Card.replacementEffects c),
      Common.optionalPair "triggeredAbilities" [] (Common.encodeList (TriggeredAbility.toJson toJson)) (Card.triggeredAbilities c),
      Common.optionalPair "delayedAbilities" Map.empty (TriggeredAbility.toJsonDelayed toJson) (Card.delayedAbilities c),
      Common.optionalPair "castingPermissions" [] (Common.encodeList CastingPermission.toJson) (Card.castingPermissions c),
      Common.optionalPair "castingRestrictions" [] (Common.encodeList CastingRestriction.toJson) (Card.castingRestrictions c),
      Common.optionalPair "playerAbilities" [] (Common.encodeList PlayerStaticAbility.toJson) (Card.playerAbilities c),
      Common.optionalPair "blockRequirements" [] (Common.encodeList BlockRequirement.toJson) (Card.blockRequirements c),
      Common.optionalPair "attackRequirements" [] (Common.encodeList AttackRequirement.toJson) (Card.attackRequirements c),
      Common.optionalPair "combatRestrictions" [] (Common.encodeList CombatRestriction.toJson) (Card.combatRestrictions c),
      Common.optionalPair "attackCosts" [] (Common.encodeList AttackCost.toJson) (Card.attackCosts c),
      Common.optionalPair "additionalCosts" [] (Common.encodeList (CostComponent.toJson Keyword.toJson)) (Card.additionalCosts c),
      Common.optionalPair "alternativeCosts" [] (Common.encodeList (Cost.toJson Keyword.toJson)) (Card.alternativeCosts c),
      Common.optionalPair "mulliganAction" [] (Common.encodeList (Effect.toJson toJson)) (Card.mulliganAction c),
      Common.optionalPair "openingHandAction" [] (Common.encodeList (Effect.toJson toJson)) (Card.openingHandAction c),
      -- R2: Counterable is the absence of a restriction (CR 701.5).
      Common.optionalPair "counterability" Counterability.Counterable Counterability.toJson (Card.counterability c)
    ]
```

Delete the three block comments the old shape carried — "CR 306.5: omitted for every card that is not a planeswalker…", "Omitted when Counterable, the posture every other defaulted key here takes…", and "Omitted when empty, unlike the required `castingPermissions` key it mirrors…". All three explain a distinction that no longer exists; leaving them is exactly the stale-prose failure the branch's earlier review caught.

Then `fromJson`: replace every `Common.field "k" ps >>= dec` for an omissible field, and every `Common.decode*Default … (Common.nullableField "k" ps)`, with `Common.defaultedField "k" <default> <dec> ps`, using the same defaults as above. `name` and `typeLine` keep `Common.field`. `counterability` becomes `Common.defaultedField "counterability" Counterability.Counterable Counterability.fromJson ps`.

- [ ] **Step 4: Delete the superseded per-type helper**

Remove `fromJsonDefault` from `Counterability.hs` and its coverage in `CounterabilitySpec.hs`.

Run: `direnv exec . grep -rn 'Counterability.fromJsonDefault' source/`
Expected: no matches.

- [ ] **Step 5: Run, fix literals, regenerate the corpus**

```bash
direnv exec . cabal test 2>&1 | tail -80
```

`CardSpec`'s `baseCardJson` and `populatedCardJson` are named bindings, not inline literals — edit them from the failure output the same way. The 16 `init baseCardJson <> ",…"` sites depend on `baseCardJson`'s last character being `}`, which still holds.

```bash
direnv exec . cabal run pawl
direnv exec . script/format-json.sh fix data/cards/*.json
direnv exec . cabal test
```

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
direnv exec . git add source/libraries/codec/Pawl/Codec/Card.hs source/libraries/codec/Pawl/Codec/Counterability.hs source/libraries/codec/Pawl/Codec/CardSpec.hs source/libraries/codec/Pawl/Codec/CounterabilitySpec.hs data/cards
direnv exec . hooky fix
direnv exec . git add source/libraries/codec/Pawl/Codec/Card.hs source/libraries/codec/Pawl/Codec/Counterability.hs source/libraries/codec/Pawl/Codec/CardSpec.hs source/libraries/codec/Pawl/Codec/CounterabilitySpec.hs data/cards
direnv exec . hooky run
direnv exec . git commit -m "Give every Card field but name and typeLine a default"
```

---

### Task 6: The ten codecs with nothing to default

Ten record codecs have no omissible field at all: their every field is an identity
(R3) or one whose constructors are all equally meaningful (R4). Converting them
changes the *shape* of the code and nothing on the wire, which makes this the one
task with an exact success criterion — **the corpus must come out byte-identical.**

**Files:**
- Modify, under `source/libraries/codec/Pawl/Codec/`: `AttackCost.hs`, `AttackRequirement.hs`, `BlockRequirement.hs`, `Condition.hs`, `Count.hs`, `Countering.hs`, `ManaCount.hs`, `PlayerStaticAbility.hs`, `StaticAbility.hs`, `ZoneChange.hs`
- Test: their `*Spec.hs` siblings (expected to need no edits — see Step 3)

**Interfaces:**
- Consumes: `Common.requiredPair :: String -> (a -> Value.Value) -> a -> [Pair.Pair Value.Value]` from Task 2.
- Produces: nothing new.

**Why each has nothing to default** — settled, do not re-derive:

| Module | Why every field is required |
|---|---|
| `AttackCost` | `subject` and `perAttacker` both name the cost; neither has an unmarked value (R4) |
| `AttackRequirement` | `subject` (R4) |
| `BlockRequirement` | `attacker` (R4) |
| `Condition` | `measured`, `comparison`, `threshold` — no `Comparison` constructor is privileged (R4) |
| `Count` | `scope`, `filter`, `aggregation` (R4) |
| `Countering` | `spell`, `source`, `controller` are `ObjectId`/`PlayerId` (R3) |
| `ManaCount` | `player` is a `PlayerRef` (R3); `filter` is R4 |
| `PlayerStaticAbility` | `scope`, `effect` (R4) |
| `StaticAbility` | `affected`, `modifications` — `modifications` is `NonEmpty`, which cannot be empty (R4) |
| `ZoneChange` | `departed`, `object` are `ObjectId` (R3); `from`, `to` are `Zone` (R4) |

- [ ] **Step 1: Convert all ten**

The transformation is uniform: `Common.object [ ... ]` becomes
`Common.object . concat $ [ ... ]`, and each `Common.pair k f x` inside becomes
`Common.requiredPair k f x`. `fromJson` is untouched in all ten. Worked example —
`ZoneChange` before:

```haskell
toJson zc =
  Common.object
    [ Common.pair "departed" (ObjectId.toJson (ZoneChange.departed zc)),
      Common.pair "object" (ObjectId.toJson (ZoneChange.object zc)),
      Common.pair "from" (Zone.toJson (ZoneChange.from zc)),
      Common.pair "to" (Zone.toJson (ZoneChange.to zc))
    ]
```

after:

```haskell
toJson zc =
  Common.object . concat $
    [ Common.requiredPair "departed" ObjectId.toJson (ZoneChange.departed zc),
      Common.requiredPair "object" ObjectId.toJson (ZoneChange.object zc),
      Common.requiredPair "from" Zone.toJson (ZoneChange.from zc),
      Common.requiredPair "to" Zone.toJson (ZoneChange.to zc)
    ]
```

Read each module's real body first — some take a `codec` argument, and some write
their pair list with the encoder applied by `.` and `$` rather than by
application. `requiredPair` takes the encoder and the value as separate
arguments, so `Common.pair "k" . Enc.toJson $ Rec.k r` becomes
`Common.requiredPair "k" Enc.toJson (Rec.k r)`.

- [ ] **Step 2: Build and test**

Run: `direnv exec . cabal build all && direnv exec . cabal test`
Expected: warning-free; PASS with no spec literal edited. **If any spec literal
fails, stop** — a failure here means a field changed on the wire, which for these
ten modules is a mistake, not a migration.

- [ ] **Step 3: Prove the wire did not move**

```bash
direnv exec . cabal run pawl
direnv exec . script/format-json.sh fix data/cards/*.json
git status --short data/cards
```

Expected: **empty output.** These ten modules add and remove no key, so the
corpus cannot change. Any diff is a defect in Step 1.

- [ ] **Step 4: Commit**

```bash
direnv exec . git add source/libraries/codec/Pawl/Codec
direnv exec . hooky fix
direnv exec . git add source/libraries/codec/Pawl/Codec
direnv exec . hooky run
direnv exec . git commit -m "Use requiredPair in the codecs with nothing to default"
```

---

### Task 7: The seven codecs whose defaults are absence

**Files:**
- Modify, under `source/libraries/codec/Pawl/Codec/`: `Binding.hs`, `CombatRestriction.hs`, `Cost.hs`, `DelayedTrigger.hs`, `EntryOption.hs`, `PhasePattern.hs`, `TriggeredAbility.hs`
- Test: their `*Spec.hs` siblings
- Modify: `data/cards/*.json`

**Interfaces:**
- Consumes: `Common.requiredPair`, `Common.optionalPair :: (Eq a) => String -> a -> (a -> Value.Value) -> a -> [Pair.Pair Value.Value]`, `Common.defaultedField :: String -> a -> (Value.Value -> Either Text.Text a) -> [Pair.Pair Value.Value] -> Either Text.Text a`, all from Task 2.
- Produces: nothing new.

Every default here is `Nothing` or an empty container — R1 only, no judgment
calls. The classification is settled; every field not listed stays
`requiredPair` + `Common.field`:

| Module | Omissible field | Default |
|---|---|---|
| `Binding` | `target`, `amount`, `modes`, `copy` | `Nothing` |
| `CombatRestriction` | `unless` | `Nothing` |
| `Cost` | `mana`, `components` | `Nothing`, `[]` |
| `DelayedTrigger` | `expiry`, `bindings` | `Nothing`, empty |
| `EntryOption` | `keywords` | `Set.empty` |
| `PhasePattern` | `whosePhase` | `Nothing` |
| `TriggeredAbility` | `intervening` | `Nothing` |

`EntryOption.power` and `EntryOption.toughness` stay **required** under R5: a 0/0
token is legal, so an absent power must not read as `0`.

- [ ] **Step 1: Convert `PhasePattern` first and see it green**

Before:

```haskell
toJson p =
  Common.object
    [ Common.pair "whichPhase" (PhaseSelector.toJson (PhasePattern.whichPhase p)),
      Common.pair "whosePhase" (Common.encodeMaybe PlayerId.toJson (PhasePattern.whosePhase p))
    ]

fromJson value = do
  ps <- Common.asObject value
  which <- Common.field "whichPhase" ps >>= PhaseSelector.fromJson
  whose <- Common.field "whosePhase" ps >>= Common.decodeMaybe PlayerId.fromJson
  pure (PhasePattern.MkPhasePattern which whose)
```

After:

```haskell
toJson p =
  Common.object . concat $
    [ Common.requiredPair "whichPhase" PhaseSelector.toJson (PhasePattern.whichPhase p),
      Common.optionalPair "whosePhase" Nothing (Common.encodeMaybe PlayerId.toJson) (PhasePattern.whosePhase p)
    ]

fromJson value = do
  ps <- Common.asObject value
  which <- Common.field "whichPhase" ps >>= PhaseSelector.fromJson
  whose <- Common.defaultedField "whosePhase" Nothing (Common.decodeMaybe PlayerId.fromJson) ps
  pure (PhasePattern.MkPhasePattern which whose)
```

The mapping, which is the same for every module in this task: `Common.pair k f x`
becomes `Common.requiredPair k f x`; a hand-rolled `if <default> then [] else
[Common.pair k …]` block becomes `Common.optionalPair k <default> f x`;
`Common.field k ps >>= dec` for an omissible field becomes
`Common.defaultedField k <default> dec ps`; and
`Common.decode*Default dec (Common.nullableField k ps)` becomes the same thing.

Write `Nothing`, `[]`, `Set.empty`, `Map.empty` and `Seq.empty` inline rather than
binding a name for them — two spellings of an empty container cannot disagree, so
a binding buys nothing here.

Run: `direnv exec . cabal test 2>&1 | tail -30`. A failing `assertToJson` prints
`encoded <json> but the literal says <json>`; replace that literal with the
`encoded` side, keeping the `""" … """` wrapping and its surrounding spaces.

- [ ] **Step 2: Convert the other six, one at a time**

For each: convert both halves, build, run the suite, fix that module's literals
from the failure output, re-run until green. Do not batch — a module whose spec
you fixed from another module's failure output is a module you did not check.

- [ ] **Step 3: Regenerate the corpus**

```bash
direnv exec . cabal run pawl
direnv exec . script/format-json.sh fix data/cards/*.json
direnv exec . cabal test
```

Expected: PASS.

- [ ] **Step 4: Commit**

```bash
direnv exec . git add source/libraries/codec/Pawl/Codec data/cards
direnv exec . hooky fix
direnv exec . git add source/libraries/codec/Pawl/Codec data/cards
direnv exec . hooky run
direnv exec . git commit -m "Default the absent-means-nothing fields"
```

---

### Task 8: The seven codecs with a chosen default

These carry the defaults that are a *judgment* rather than an absence — R2's
"one constructor means no restriction", plus `DamageEvent`'s boolean flags under
R1. Each R2 default gets a named binding in its codec module, referenced by both
halves; that binding is what stops the two halves drifting.

**Files:**
- Modify, under `source/libraries/codec/Pawl/Codec/`: `ActivatedAbility.hs`, `CounterPattern.hs`, `DamageEvent.hs`, `DamagePattern.hs`, `EntryRiders.hs`, `TokenPattern.hs`, `ZoneChangePattern.hs`
- Test: their `*Spec.hs` siblings
- Modify: `data/cards/*.json`

**Interfaces:**
- Consumes: `Common.requiredPair`, `Common.optionalPair :: (Eq a) => String -> a -> (a -> Value.Value) -> a -> [Pair.Pair Value.Value]`, `Common.defaultedField :: String -> a -> (Value.Value -> Either Text.Text a) -> [Pair.Pair Value.Value] -> Either Text.Text a`, all from Task 2.
- Produces: nothing later tasks import.

The classification is settled; every field not listed stays `requiredPair` +
`Common.field`:

| Module | Omissible field | Default | Rule |
|---|---|---|---|
| `ActivatedAbility` | `timing` | `ActivationTiming.AnyTime` | R2 |
| `CounterPattern` | `whichKind` | `Nothing` | R1 |
| `CounterPattern` | `whose` | `ControllerRelation.Anyones` | R2 |
| `DamageEvent` | `dealtByDeathtouch`, `dealtByInfect`, `dealtByToxic`, `dealtByLifelink` | `False` | R1 |
| `DamagePattern` | `whichKind`, `whichRecipient` | `Nothing` | R1 |
| `DamagePattern` | `whichSource` | `SourceRelation.AnySource` | R2 |
| `EntryRiders` | `tapped` | `TapState.Untapped` | R2 |
| `EntryRiders` | `attacking` | `False` | R1 |
| `TokenPattern` | `whose` | `ControllerRelation.Anyones` | R2 |
| `ZoneChangePattern` | `whichObject` | `ZoneChangeSubject.AnyObject` | R2 |
| `ZoneChangePattern` | `whoseObject` | `ControllerRelation.Anyones` | R2 |

`ZoneChangePattern.whenDestination` stays required (R4) — no `Zone` is the
unmarked one. `DamageEvent`'s `source`, `target`, `amount` and `kind` stay
required (R3, R4).

- [ ] **Step 1: Convert `TokenPattern` first and see it green**

The shape, with the named binding that is this task's distinguishing feature:

```haskell
-- | CR 109.5 reads a controller relation against the effect's source; "anyone's"
-- is the unrestricted reading, so it is what a pattern that says nothing means.
defaultWhose :: ControllerRelation.ControllerRelation
defaultWhose = ControllerRelation.Anyones

toJson tp =
  Common.object . concat $
    [ Common.optionalPair "whose" defaultWhose ControllerRelation.toJson (TokenPattern.whose tp)
    ]

fromJson value = do
  ps <- Common.asObject value
  whose <- Common.defaultedField "whose" defaultWhose ControllerRelation.fromJson ps
  pure (TokenPattern.MkTokenPattern whose)
```

Read the module's real body first — `TokenPattern` may carry fields beyond
`whose`, and those stay `requiredPair` + `Common.field`.

The mapping for every module in this task: `Common.pair k f x` becomes
`Common.requiredPair k f x`; a hand-rolled `if <default> then [] else
[Common.pair k …]` block becomes `Common.optionalPair k <default> f x`;
`Common.field k ps >>= dec` for an omissible field becomes
`Common.defaultedField k <default> dec ps`; and
`Common.decode*Default dec (Common.nullableField k ps)` becomes the same thing.

Bind a `default<Field>` for every R2 default in the table above, with a comment
saying why that constructor is the unmarked one. Write `Nothing` and `False`
inline — two spellings of those cannot disagree.

Run: `direnv exec . cabal test 2>&1 | tail -30`. A failing `assertToJson` prints
`encoded <json> but the literal says <json>`; replace that literal with the
`encoded` side, keeping the `""" … """` wrapping and its surrounding spaces.

- [ ] **Step 2: Convert the other six, one at a time**

For each: convert both halves, build, run the suite, fix that module's literals
from the failure output, re-run until green. Do not batch.

- [ ] **Step 3: Regenerate the corpus**

```bash
direnv exec . cabal run pawl
direnv exec . script/format-json.sh fix data/cards/*.json
direnv exec . cabal test
```

Expected: PASS.

- [ ] **Step 4: Commit**

```bash
direnv exec . git add source/libraries/codec/Pawl/Codec data/cards
direnv exec . hooky fix
direnv exec . git add source/libraries/codec/Pawl/Codec data/cards
direnv exec . hooky run
direnv exec . git commit -m "Default the unmarked-constructor fields"
```

---

### Task 9: ProjectedCharacteristics

On its own because it is the widest record in the codec — twelve fields, of which
ten are omissible — and because it is the one whose JSON appears inside other
modules' spec literals (`ProjectedCharacteristicsSpec.testCharacteristicsJson` is
concatenated into `BindingSpec`, `GameEventSpec` and `DelayedTriggerSpec`).

**Files:**
- Modify: `source/libraries/codec/Pawl/Codec/ProjectedCharacteristics.hs`
- Test: `source/libraries/codec/Pawl/Codec/ProjectedCharacteristicsSpec.hs`, and the three specs that concatenate its JSON: `BindingSpec.hs`, `GameEventSpec.hs`, `DelayedTriggerSpec.hs`
- Modify: `data/cards/*.json`

**Interfaces:**
- Consumes: `Common.requiredPair`, `Common.optionalPair :: (Eq a) => String -> a -> (a -> Value.Value) -> a -> [Pair.Pair Value.Value]`, `Common.defaultedField :: String -> a -> (Value.Value -> Either Text.Text a) -> [Pair.Pair Value.Value] -> Either Text.Text a`, all from Task 2.

| Field | Required or default |
|---|---|
| `name` | **required** (R3 — a `CardName` has no default) |
| `cardTypes` | **required** (R6's reasoning: a projection with no card type is a malformed value, not a typeless permanent) |
| `supertypes`, `subtypes`, `colors` | `Set.empty` |
| `keywords` | `Map.empty` (a multiset) |
| `power`, `toughness`, `loyalty`, `characteristicPT` | `Nothing` |
| `activatedAbilities`, `replacementEffects` | `[]` |

- [ ] **Step 1: Convert both halves**

The mapping: `Common.pair k f x` becomes `Common.requiredPair k f x`;
`Common.field k ps >>= dec` for an omissible field becomes
`Common.defaultedField k <default> dec ps`; and
`Common.decode*Default dec (Common.nullableField k ps)` becomes the same thing.
Write every default in the table inline — they are all `Nothing` or empty, so a
named binding buys nothing.

Unlike Task 3, add no `cardTypes` non-empty check: R6's non-empty rule is written
for `TypeLine` and Task 3 implemented it there. Keeping `cardTypes` a required
key is the whole of this module's guard.

- [ ] **Step 2: Fix its own spec, then the three that concatenate its JSON**

Run: `direnv exec . cabal test 2>&1 | tail -40`.

`ProjectedCharacteristicsSpec`'s own literals come from the `encoded …` side of
each failure. Then `testCharacteristicsJson` — the binding those three other
specs concatenate — has to shrink to match, and the surrounding literals in
`BindingSpec`, `GameEventSpec` and `DelayedTriggerSpec` may shrink too if this
task changed a field they exercise. Fix `testCharacteristicsJson` first and
re-run; the remaining failures name what else moved.

- [ ] **Step 3: Regenerate the corpus**

```bash
direnv exec . cabal run pawl
direnv exec . script/format-json.sh fix data/cards/*.json
direnv exec . cabal test
```

Expected: PASS. `ProjectedCharacteristics` is not reachable from a card file, so
the corpus should not move — but run it, because a shared codec it calls might.

- [ ] **Step 4: Verify no hand-rolled omission blocks survive anywhere**

Run: `direnv exec . grep -rn 'then \[\] else \[' source/libraries/codec/`
Expected: no matches.

Run: `direnv exec . grep -rn 'Common.object$' source/libraries/codec/ | grep -v Spec`
Expected: no matches — every record encoder now goes through `Common.object . concat`.

- [ ] **Step 5: Commit**

```bash
direnv exec . git add source/libraries/codec/Pawl/Codec data/cards
direnv exec . hooky fix
direnv exec . git add source/libraries/codec/Pawl/Codec data/cards
direnv exec . hooky run
direnv exec . git commit -m "Default ProjectedCharacteristics' fields"
```
---

### Task 10: Remove the superseded helpers

**Files:**
- Modify: `source/libraries/codec/Pawl/Codec/Common.hs`
- Test: `source/libraries/codec/Pawl/Codec/CommonSpec.hs`

- [ ] **Step 1: Confirm nothing calls them**

```bash
direnv exec . grep -rn 'nullableField\|decodeListDefault\|decodeSetDefault\|decodeMapDefault\|decodeBooleanDefault' source/
```

Expected: matches only in `Common.hs` and any `CommonSpec.hs` case covering them. If a codec module still calls one, it was missed in Tasks 3-9 — go convert it before continuing.

- [ ] **Step 2: Delete**

Remove `nullableField`, `decodeListDefault`, `decodeSetDefault`, `decodeMapDefault`, `decodeBooleanDefault` and the `-- Defaults ---` section header from `Common.hs`, plus their `CommonSpec` cases. Keep `decodeMaybe` — R7 depends on it.

- [ ] **Step 3: Update Common's module haddock**

Its header describes "the element-generic `encode*`/`decode*` combinators every per-type module is written in terms of". Add the field combinators to that sentence and state the invariant they carry: an omitted key means the default `defaultedField` supplies, which is the same value `optionalPair` omits.

- [ ] **Step 4: Build and test**

Run: `direnv exec . cabal build all && direnv exec . cabal test`
Expected: warning-free, PASS.

- [ ] **Step 5: Commit**

```bash
direnv exec . git add source/libraries/codec/Pawl/Codec/Common.hs source/libraries/codec/Pawl/Codec/CommonSpec.hs
direnv exec . hooky fix
direnv exec . git add source/libraries/codec/Pawl/Codec/Common.hs source/libraries/codec/Pawl/Codec/CommonSpec.hs
direnv exec . hooky run
direnv exec . git commit -m "Remove the decode*Default family"
```

---

### Task 11: An all-defaults case per record codec

This is the mechanism that closes the loop between a codec's two halves. Tasks 3–5 already added it for `TypeLine`, `Mode`, `Modal` and `Card`; this task covers the other 25.

**Files:**
- Test: the `*Spec.hs` siblings of every module converted in Tasks 6-9

- [ ] **Step 1: Add the case to each spec**

For every record codec, one case asserting that the value with every field at its default round-trips against its minimal JSON. For a module with no omissible fields (`AttackRequirement`, `Condition`, `Count`, `Countering`, `ManaCount`, `PlayerStaticAbility`, `StaticAbility`, `ZoneChange`, `AttackCost`, `BlockRequirement`) the minimal JSON is the full object and the case is still worth having, because it pins that *nothing* was made omissible by accident. Shape:

```haskell
  Spec.it s "an all-default value omits every optional key" $
    Common.assertJsonCodec
      s
      PhasePattern.toJson
      PhasePattern.fromJson
      (PhasePattern.MkPhasePattern PhaseSelector.CombatPhase Nothing)
      """ {"whichPhase":{"type":"CombatPhase"}} """
```

Write the literal by hand from the rules, then run the suite: a mismatch means either the literal or a default is wrong, and the failure names both sides.

- [ ] **Step 2: Run**

Run: `direnv exec . cabal test`
Expected: PASS.

- [ ] **Step 3: Commit**

```bash
direnv exec . git add source/libraries/codec/Pawl/Codec
direnv exec . hooky fix
direnv exec . git add source/libraries/codec/Pawl/Codec
direnv exec . hooky run
direnv exec . git commit -m "Assert every record codec round-trips its all-default value"
```

---

### Task 12: The verbose form still decodes

R7 makes the pre-migration shape a supported input, and after Tasks 3–6 nothing exercises it any more.

**Files:**
- Test: `source/libraries/codec/Pawl/Codec/CardSpec.hs`, `source/libraries/codec/Pawl/Codec/CommonSpec.hs`

- [ ] **Step 1: Write the cases**

In `CardSpec.hs`, using `Common.assertFromJson` (decode direction only — the encoder no longer writes this shape, so `assertJsonCodec` would fail by design):

```haskell
  -- R7: omission is permitted on input, never required. This is goblin-piker.json
  -- as it stood before the migration; every such file must still load.
  Spec.it s "a pre-migration card file still decodes" $
    Common.assertFromJson
      s
      Card.fromJson
      """ {"name":"Mountain","typeLine":{"supertypes":[{"type":"Basic"}],"types":[{"type":"Land"}],"subtypes":[{"type":"Mountain"}]},"manaCost":null,"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}}} """
      mountainCard
```

with, beside `minimalCard` from Task 5:

```haskell
-- The same card the verbose literal above spells out: minimalCard's fields, with
-- the type line a real basic land carries.
mountainCard :: Card.Card
mountainCard =
  minimalCard
    { Card.typeLine =
        TypeLine.MkTypeLine
          (Set.singleton Supertype.Basic)
          (Set.singleton CardType.Land)
          (Set.singleton Subtype.Mountain)
    }
```

In `CommonSpec.hs`, one case per shape of default:

```haskell
  Spec.describe s "defaultedField accepts the verbose form" $ do
    Spec.it s "an explicit null reads as the default for a Maybe" $
      Spec.assertEq
        s
        (Common.defaultedField "k" Nothing (Common.decodeMaybe Common.asInteger) [Common.pair "k" Common.null])
        (Right (Nothing :: Maybe Integer))
    Spec.it s "an explicit empty array reads as the default for a list" $
      Spec.assertEq
        s
        (Common.defaultedField "k" [] (Common.decodeList Common.asInteger) [Common.pair "k" (Common.array [])])
        (Right ([] :: [Integer]))
```

- [ ] **Step 2: Run**

Run: `direnv exec . cabal test`
Expected: PASS.

- [ ] **Step 3: Commit**

```bash
direnv exec . git add source/libraries/codec/Pawl/Codec/CardSpec.hs source/libraries/codec/Pawl/Codec/CommonSpec.hs
direnv exec . hooky fix
direnv exec . git add source/libraries/codec/Pawl/Codec/CardSpec.hs source/libraries/codec/Pawl/Codec/CommonSpec.hs
direnv exec . hooky run
direnv exec . git commit -m "Cover the pre-migration JSON shape on input"
```

---

### Task 13: Prove the corpus lost nothing, then open the PR

P3 compares each file against the encoder, so after regeneration it passes by construction. The check that actually matters is that the cards *mean* the same thing before and after, and it needs the old corpus on disk.

**Files:**
- None committed. The check below is scaffolding — run it, report it in the PR body, delete it.

- [ ] **Step 1: Materialize the pre-migration corpus**

```bash
BASE=$(direnv exec . git merge-base HEAD main)
rm -rf /tmp/pawl-old-cards && mkdir -p /tmp/pawl-old-cards
direnv exec . git archive "$BASE" data/cards | tar -x -C /tmp/pawl-old-cards --strip-components=2
ls /tmp/pawl-old-cards | wc -l
```

Expected: 226.

- [ ] **Step 2: Write the temporary comparison**

Add to `source/test-suite/Pawl/CardsSpec.hs`, wired into its `spec`:

```haskell
  Spec.it s "TEMPORARY: every card means what it meant before the migration" $ do
    old <- Registry.fileRegistry "/tmp/pawl-old-cards"
    ps <- S.allPrintings s
    mapM_
      ( \p -> do
          let n = Text.unpack (CardName.unwrap (CardT.name (Printing.card p)))
          before <- Registry.named old n
          Spec.assertEqWith s n before (Right (Printing.card p))
      )
      ps
```

The old files decode through the *new* decoder, which R7 guarantees still accepts them, so any difference is a default that does not mean what the old explicit value meant.

- [ ] **Step 3: Run it**

Run: `direnv exec . cabal test 2>&1 | grep -A 5 TEMPORARY`
Expected: PASS for all 226. A failure names the card and shows both `Card` values — that is a wrong default in Tasks 3-9, not a corpus problem.

- [ ] **Step 4: Delete the temporary case**

Remove it and rebuild. It depends on `/tmp`, so it must not be committed.

Run: `direnv exec . grep -rn TEMPORARY source/`
Expected: no matches.

- [ ] **Step 5: Final verification**

```bash
direnv exec . cabal build all
direnv exec . cabal test
direnv exec . git add -- source data docs pawl.cabal
direnv exec . hooky run
direnv exec . git diff --stat main -- data/cards | tail -1
```

Expected: warning-free build; suite PASS; hooky clean; the corpus diff showing roughly 4,500 lines removed.

- [ ] **Step 6: Self-review the branch before opening the PR**

Per CONTRIBUTING: re-check every CR citation added by this branch against `docs/rules.txt` by number (CR 700.2 in `Modal`, CR 603.5 in `Mode`, CR 701.5 in `Card`), and re-read every comment the change touched for prose the rewrite made wrong. `Card.toJson`'s three deleted block comments are the known instance; look for others in the modules from Tasks 6-9. Fix findings on the branch.

- [ ] **Step 7: Open the PR**

Open as a draft, then mark ready once the self-review's findings are pushed and the suite is green. Do not wait for CI. The body carries: what changed and why, with `Closes #N` as plain text (backticks break the link); the CR citations, each checked; the design calls made and rejected — per-field defaults over structurally-empty-only, regenerate-from-encoder over hand-stripping, and what the latter costs; how it was verified, including the Task 13 before/after result, the suite count before → after, and the corpus line delta; an explicit "no" on whether the diff makes the rules core case on an effect's identity; and what was deferred.

---

## Deferred

The 24 `Common.assertJsonCodec` sites that build JSON by `<>` concatenation are untouched. They are not literals, and the `init baseCardJson <> ",…"` family depends on the last character being `}` — shrinking them is a separate cleanup with its own hazard.

The tracking issue is **#629**; Task 13's PR body closes it.
