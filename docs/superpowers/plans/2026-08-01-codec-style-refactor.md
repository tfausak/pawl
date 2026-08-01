# Type and Codec Style Refactor Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Apply the style established in `2dfeb6c` to all 145 types and 98 codecs, replacing `Pawl.Codec.Json` with `Pawl.Codec.Common` and giving every codec a spec that pins its wire format, without changing a single byte of what the codec emits.

**Architecture:** `Pawl.Codec.Common` grows on demand as the base API; codec modules are converted in dependency-layer batches so each commit compiles; each converted module gains a sibling `Pawl.Codec.XSpec` asserting one literal-JSON case per constructor; `Pawl.Codec.Json` and `Pawl/CodecSpec.hs` are deleted once emptied.

**Tech Stack:** GHC 9.14.1 via the Nix flake, `cabal`, `tasty`, `parsec`, `cabal-gild`, `hooky`.

**Spec:** `docs/superpowers/specs/2026-08-01-codec-style-refactor-design.md`

## Global Constraints

- **One PR, small commits.** Every commit builds warning-free and leaves the suite green. Do not open the PR until the last task is done.
- **The wire format does not change.** `data/cards/` is never edited. `Pawl.CardsSpec` round-trips every card file and must stay green at every commit — it is the safety net.
- **One build at a time.** `cabal build all`, never just the library. Never poke inside `dist-newstyle`.
- **`hooky` acts on staged files.** `git add`, then `hooky fix`, then `git add` again, then `hooky run`.
- **`cabal-gild pawl.cabal` must be run directly** whenever a module is added or deleted; `hooky fix` only covers staged files and will not regenerate `exposed-modules` for a file it has not seen.
- **Scope is `source/libraries/types/` and `source/libraries/codec/` only.** The engine's 37 modules are out. Other files change only where they must to compile.
- **Never trust recalled Magic rules.** Any CR citation a comment carries must be re-checked against `docs/rules.txt` when that comment is touched.
- **Never edit this plan, weaken an assertion, or delete a test to make a check pass.** If the plan looks wrong, stop and say so.
- Build must be warning-free under `-Weverything` (see `pawl.cabal`'s `common warnings`).

---

## The Recipe

Tasks 3–13 each apply this recipe to a named list of modules. It is stated once,
in full, here. It is not a cross-reference to another task — everything needed is
below.

### Rules

- **R1** — delete the `-- | The @X ⇆ Json@ codec (#481).` module header. #481 is closed. Add a module haddock only if the module has something non-obvious to say.
- **R2** — every import is `qualified <Module> as <LastComponent>`. No `import Data.Text (Text)`, no `import Pawl.Json.Value (Value)`, no `import Pawl.Json.Array (Array (MkArray))`, no `import Pawl.Codec.Foo (fooToJson, jsonToFoo)`.
- **R3** — a newtype in `Pawl.Types` is a record with an `unwrap` field. Codec modules read through `X.unwrap` rather than pattern-matching `MkX`.
- **R4** — `xToJson` → `toJson`, `jsonToX` → `fromJson`. **Do not reorder declarations.**
- **R5** — no `Text.pack` for a JSON key or tag; `Common.pair`, `Common.tagged`, `Common.nullary`, `Common.field`, `Common.decodeNullary` all take `String`.
- **R6** — a bare `--` block *immediately above a declaration* becomes `-- |`. A floating note between or inside declarations stays bare.
- **R7** — add `Pawl.Codec.XSpec` beside the module, one `assertJsonCodec` case per constructor.

**One name collides.** `Pawl.Types.TargetSpec` is a real type, so
`Pawl.Codec.TargetSpec` is a real codec and its spec is
`Pawl.Codec.TargetSpecSpec`. The doubled suffix is correct and deliberate — the
convention is "append `Spec` to the module name", and a type whose name ends in
`Spec` does not get an exception. Do not shorten it to `Pawl.Codec.TargetSpec`,
which is the module under test.

### Secondary codec names

| Today | Becomes |
|---|---|
| `jsonToCounterabilityDefault` | `Counterability.fromJsonDefault` |
| `jsonToOptionalityDefault` | `Optionality.fromJsonDefault` |
| `jsonToQuantityPair` | `Quantity.fromJsonPair` |
| `jsonToSubtypePair` | `Subtype.fromJsonPair` |
| `bindingsToJson` / `jsonToBindings` | `Binding.toJsonMap` / `Binding.fromJsonMap` |
| `delayedAbilitiesToJson` / `jsonToDelayedAbilities` | `TriggeredAbility.toJsonDelayed` / `TriggeredAbility.fromJsonDelayed` |
| `defaultEntryRiders` | `EntryRiders.defaultValue` |
| `optionalFilter` | `Filter.optional` |

### `Pawl.Codec.Json` → `Pawl.Codec.Common` name map

| `Json` | `Common` |
|---|---|
| `jNull` | `null` |
| `jBool` | `boolean` |
| `jInt n` | `integer n` |
| `jText` | `text` |
| `jArray` | `array` |
| `jObject [(Text.pack "k", v)]` | `object [pair "k" v]` |
| `tagged (Text.pack "T")` | `tagged "T"` |
| `nullary (Text.pack "T")` | `nullary "T"` |
| `tag` | `asTagged` (returns `String`, not `Text`) |
| `decodeNullary (Text.pack "T") [(Text.pack "A", …)]` | `decodeNullary "T" [("A", …)]` |
| `asObject` | `asObject` (returns `[Pair.Pair Value]`) |
| `asArray`, `asText`, `asInteger` | unchanged |
| `field (Text.pack "k") ps` | `field "k" ps` |
| `optField` | `optionalField` |
| `getOpt` | `nullableField` |
| `withValue` | `withValue` |
| `listTo` / `listFrom` | `encodeList` / `decodeList` |
| `nonEmptyTo` / `nonEmptyFrom` | `encodeNonEmpty` / `decodeNonEmpty` |
| `seqTo` / `seqFrom` | `encodeSeq` / `decodeSeq` |
| `setTo` / `setFrom` | `encodeSet` / `decodeSet` |
| `multisetTo` / `multisetFrom` | `encodeMultiset` / `decodeMultiset` |
| `maybeTo` / `maybeFrom` | `encodeMaybe` / `decodeMaybe` |
| `natTo` / `natFrom` | `encodeNatural` / `decodeNatural` |
| `listFromDefault` | `decodeListDefault` |
| `setFromDefault` | `decodeSetDefault` |
| `mapFromDefault` | `decodeMapDefault` |
| `jsonToBoolDefault` | `decodeBooleanDefault` |
| `sortKeys`, `render`, `parse` | unchanged |

`Pawl.Json.Value (Value (Array))` pattern matches become `Value.Array`;
`Pawl.Json.Array (Array (MkArray))` becomes `Array.MkArray`. So
`Just (Array (MkArray [x, y]))` becomes `Just (Value.Array (Array.MkArray [x, y]))`.

### Worked shape A — nullary enum (`Color`)

Before:

```haskell
-- | The @Color ⇆ Json@ codec (#481).
module Pawl.Codec.Color where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Color as Color

colorToJson :: Color.Color -> Value
colorToJson c = Json.nullary . Text.pack $ case c of
  Color.White -> "White"
  ...

jsonToColor :: Value -> Either Text Color.Color
jsonToColor =
  Json.decodeNullary
    (Text.pack "Color")
    [ (Text.pack "White", Color.White),
      ...
    ]
```

After:

```haskell
module Pawl.Codec.Color where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Color as Color

toJson :: Color.Color -> Value.Value
toJson c = Common.nullary $ case c of
  Color.White -> "White"
  Color.Blue -> "Blue"
  Color.Black -> "Black"
  Color.Red -> "Red"
  Color.Green -> "Green"

fromJson :: Value.Value -> Either Text.Text Color.Color
fromJson =
  Common.decodeNullary
    "Color"
    [ ("White", Color.White),
      ("Blue", Color.Blue),
      ("Black", Color.Black),
      ("Red", Color.Red),
      ("Green", Color.Green)
    ]
```

Spec:

```haskell
module Pawl.Codec.ColorSpec where

import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Color" $ do
  Spec.it s "White" $
    Common.assertJsonCodec s Color.toJson Color.fromJson Color.White "{\"type\":\"White\"}"
  Spec.it s "Blue" $
    Common.assertJsonCodec s Color.toJson Color.fromJson Color.Blue "{\"type\":\"Blue\"}"
  Spec.it s "Black" $
    Common.assertJsonCodec s Color.toJson Color.fromJson Color.Black "{\"type\":\"Black\"}"
  Spec.it s "Red" $
    Common.assertJsonCodec s Color.toJson Color.fromJson Color.Red "{\"type\":\"Red\"}"
  Spec.it s "Green" $
    Common.assertJsonCodec s Color.toJson Color.fromJson Color.Green "{\"type\":\"Green\"}"
```

Note the two `Color` imports resolve to one alias — `Pawl.Codec.Color` and
`Pawl.Types.Color` both alias to `Color`, which is legal and is exactly what
`Pawl.Codec.AbilityNameSpec` does in `2dfeb6c`.

### Worked shape B — newtype delegating (`Power`)

Before:

```haskell
powerToJson :: Power.Power -> Value
powerToJson (Power.MkPower q) = quantityToJson q

jsonToPower :: Value -> Either Text Power.Power
jsonToPower value = Power.MkPower <$> jsonToQuantity value
```

After — note R3 requires adding `unwrap` to `Pawl.Types.Power` first:

```haskell
-- source/libraries/types/Pawl/Types/Power.hs
newtype Power = MkPower
  { unwrap :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
```

```haskell
-- source/libraries/codec/Pawl/Codec/Power.hs
module Pawl.Codec.Power where

import qualified Data.Text as Text
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Power as Power

toJson :: Power.Power -> Value.Value
toJson = Quantity.toJson . Power.unwrap

fromJson :: Value.Value -> Either Text.Text Power.Power
fromJson = fmap Power.MkPower . Quantity.fromJson
```

### Worked shape C — tagged with payload (`Quantity`)

```haskell
toJson :: Quantity.Quantity -> Value.Value
toJson q = case q of
  Quantity.Literal n -> Common.tagged "Literal" . Just $ Common.integer n
  Quantity.ManaValue -> Common.nullary "ManaValue"
  Quantity.InSlot s -> Common.tagged "InSlot" . Just $ SlotName.toJson s
  Quantity.Plus a b -> Common.tagged "Plus" . Just . Common.array $ [toJson a, toJson b]
  Quantity.Count c -> Count.toJson toJson c
  ...

fromJson :: Value.Value -> Either Text.Text Quantity.Quantity
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("Literal", Just v) -> Quantity.Literal <$> Common.asInteger v
    ("ManaValue", _) -> Right Quantity.ManaValue
    ("InSlot", Just v) -> Quantity.InSlot <$> SlotName.fromJson v
    ("Plus", Just (Value.Array (Array.MkArray [x, y]))) -> Quantity.Plus <$> fromJson x <*> fromJson y
    ("Count", _) -> Quantity.Count <$> Count.fromJson fromJson value
    _ -> Left . Text.pack $ "unknown Quantity: " <> t
```

`asTagged` returns `String`, so the `Text.unpack t` that 31 modules open with is
deleted, and the error message concatenates `String` before a single `Text.pack`.

### Worked shape D — object with defaulted fields (`EntryRiders`)

```haskell
toJson :: EntryRiders.EntryRiders -> Value.Value
toJson e =
  Common.object
    [ Common.pair "tapped" . TapState.toJson $ EntryRiders.tapped e,
      Common.pair "attacking" . Common.boolean $ EntryRiders.attacking e
    ]

fromJson :: Value.Value -> Either Text.Text EntryRiders.EntryRiders
fromJson value = do
  ps <- Common.asObject value
  t <- Common.decodeMaybe TapState.fromJson (Common.nullableField "tapped" ps)
  a <- Common.decodeBooleanDefault False (Common.nullableField "attacking" ps)
  pure
    EntryRiders.MkEntryRiders
      { EntryRiders.tapped = Maybe.fromMaybe TapState.Untapped t,
        EntryRiders.attacking = a
      }
```

The local `orDefault` helper in the current `jsonToEntryRiders` is exactly
`decodeMaybe` plus a default; deleting it in favour of `Common.decodeMaybe` is
part of the conversion. **Verify by test, not by eye** — `Pawl.CardsSpec` and the
new `EntryRidersSpec` both cover it.

### Per-module procedure

1. Add `unwrap` to the type if it is a newtype lacking one (R3), and convert its decl-attached comments to haddock (R6). Re-check any CR citation in a touched comment against `docs/rules.txt`.
2. Rewrite the codec module per R1–R5. Add to `Common` any helper the module needs that is not there yet, under its name from the map above.
3. Write `Pawl.Codec.XSpec` — one `assertJsonCodec` case per constructor of the type. **Derive every JSON literal by reading the encoder you just wrote, never by recalling a tag name.**
4. Delete the cases this module supersedes from `source/test-suite/Pawl/CodecSpec.hs`. Bespoke shape assertions there (the elision tests built on `payloadHead` / `payloadLength`) move into the new `XSpec` — they are not dropped.
5. Wire `Pawl.Codec.XSpec` into `source/test-suite/Main.hs` (`import qualified` at the top, `Pawl.Codec.XSpec.spec s` in `spec`, both alphabetical).
6. `cabal-gild pawl.cabal`.
7. `cabal build all` — warning-free. `cabal test` — green.
8. `git add -A`, `hooky fix`, `git add -A`, `hooky run`, commit.

---

## Task 1: Capture the baseline

**Files:**
- Create: none
- Modify: none

**Interfaces:**
- Consumes: nothing
- Produces: a recorded suite count and a green baseline, quoted in the PR body by Task 16

- [ ] **Step 1: Confirm the working tree is clean and on the branch**

```bash
git status --short && git branch --show-current
```

Expected: no output from `git status`, branch `2026-08-01-refactor-codecs`.

- [ ] **Step 2: Build**

```bash
cabal build all 2>&1 | tail -20
```

Expected: no warnings, no errors.

- [ ] **Step 3: Record the suite count**

```bash
cabal test 2>&1 | tail -20
```

Expected: all pass. **Write the total test count down** — it is reported in the PR body as "before". Do not proceed if the suite is red.

---

## Task 2: `Pawl.Codec.Common` foundation

Builds out `Common` with everything the Recipe's name map promises, fixes
`assertJson` to be `eof`-terminated, adds `sortKeys` normalization to
`assertToJson`, and adds `assertJsonCodec`. `Pawl.Codec.Json` stays for now —
both modules coexist until Task 14.

**Files:**
- Modify: `source/libraries/codec/Pawl/Codec/Common.hs`
- Create: `source/libraries/codec/Pawl/Codec/CommonSpec.hs`
- Modify: `source/test-suite/Main.hs`
- Modify: `pawl.cabal`

**Interfaces:**
- Consumes: `Pawl.Spec`, `Pawl.Json.*`, `Pawl.Decimal`, `Pawl.Extra.Integer`, `Pawl.Extra.Builder`
- Produces: every name in the Recipe's name map, plus `assertJson`, `assertFromJson`, `assertToJson`, `assertJsonCodec`. Tasks 3–15 consume these.

- [ ] **Step 1: Write the failing spec**

Create `source/libraries/codec/Pawl/Codec/CommonSpec.hs`. The `parse`, `render`,
`tagged` and `sortKeys` cases are ported from `source/test-suite/Pawl/JsonSpec.hs`;
the `eof` case is new and is the reason this task exists.

```haskell
module Pawl.Codec.CommonSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Common" $ do
  Spec.describe s "parse" $ do
    Spec.it s "rejects trailing input" $
      Spec.assertEq s (fmap Common.render . Common.parse $ Text.pack "\"a\" x") (Left . Text.pack $ "")
    Spec.it s "round trips through render" $
      Spec.assertEq s (Common.parse (Common.render (Common.array [Common.integer 1]))) (Right (Common.array [Common.integer 1]))

  Spec.describe s "tagged" $ do
    Spec.it s "omits an absent value" $
      Spec.assertEq s (Common.render (Common.tagged "ManaValue" Nothing)) (Text.pack "{\"type\":\"ManaValue\"}")
    Spec.it s "includes a present value" $
      Spec.assertEq s (Common.render (Common.tagged "Literal" (Just (Common.integer 5)))) (Text.pack "{\"type\":\"Literal\",\"value\":5}")

  Spec.describe s "asTagged" $
    Spec.it s "returns a String tag" $
      Spec.assertEq s (Common.asTagged (Common.nullary "X")) (Right ("X", Nothing))

  Spec.describe s "sortKeys" $
    Spec.it s "orders object keys" $
      Spec.assertEq
        s
        (Common.sortKeys (Common.object [Common.pair "b" (Common.integer 1), Common.pair "a" (Common.integer 2)]))
        (Common.object [Common.pair "a" (Common.integer 2), Common.pair "b" (Common.integer 1)])

  Spec.describe s "assertToJson" $
    Spec.it s "ignores object key order" $
      Common.assertToJson s id (Common.object [Common.pair "b" (Common.integer 1), Common.pair "a" (Common.integer 2)]) "{\"a\":2,\"b\":1}"
```

The `"rejects trailing input"` case above is written to fail on purpose in
Step 2 — the expected `Left` message is a placeholder that Step 4 replaces with
the parser's real text. Run Step 2 first and copy the actual message.

- [ ] **Step 2: Run and observe the failures**

```bash
cabal build all 2>&1 | tail -30
```

Expected: FAIL to compile — `Common.render`, `Common.parse`, `Common.integer`,
`Common.asTagged`, `Common.sortKeys`, `Common.assertJsonCodec` are not in scope.

- [ ] **Step 3: Write `Pawl.Codec.Common` in full**

Replace `source/libraries/codec/Pawl/Codec/Common.hs` with the following. The
module haddock is ported from `Pawl.Codec.Json`, minus its stale `(#481)`.

```haskell
-- | Construction, normalization, and extraction helpers over the @json@
-- sublibrary's 'Value.Value', plus the tagged-object convention the codec builds
-- on, the element-generic combinators every per-type codec module is written in
-- terms of, and the assertions its specs are written in terms of. Encoding and
-- decoding themselves live in 'Pawl.Json.Value'; this module adapts them to the
-- codec's @Either Text@ error channel.
--
-- Nothing here names a @Pawl.Types@ type, which is what keeps it below all 98
-- per-type modules rather than in a cycle with them.
--
-- 'object' and 'asObject' trade in 'Pair.Pair' lists, which is the shape the
-- codec wants: it writes fields in a readable order rather than an alphabetical
-- one, and reads them back by name. That order is incidental -- JSON objects are
-- unordered, nothing checks the bytes of a card file, and 'sortKeys' exists to
-- compare two values regardless of it.
module Pawl.Codec.Common where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified GHC.Stack as Stack
import qualified Numeric.Natural as Natural
import qualified Pawl.Decimal as Decimal
import qualified Pawl.Extra.Builder as Builder
import qualified Pawl.Extra.Integer as Integer
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Boolean as Boolean
import qualified Pawl.Json.Null as Null
import qualified Pawl.Json.Number as Number
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.String as String
import qualified Pawl.Json.Value as Value
import qualified Pawl.Spec as Spec
import qualified Text.Parsec as Parsec

-- Construction ---------------------------------------------------------------

null :: Value.Value
null = Value.Null $ Null.MkNull ()

boolean :: Bool -> Value.Value
boolean = Value.Boolean . Boolean.MkBoolean

number :: Integer -> Integer -> Value.Value
number m = Value.Number . Number.MkNumber . Decimal.mkDecimal m

-- | The whole-number case of 'number', which is every number the codec writes.
integer :: Integer -> Value.Value
integer m = number m 0

string :: String -> Value.Value
string = text . Text.pack

text :: Text.Text -> Value.Value
text = Value.String . String.MkString

array :: [Value.Value] -> Value.Value
array = Value.Array . Array.MkArray

pair :: String -> a -> Pair.Pair a
pair = Pair.MkPair . String.MkString . Text.pack

object :: [Pair.Pair Value.Value] -> Value.Value
object = Value.Object . Object.MkObject

-- Tagged objects -------------------------------------------------------------

tagged :: String -> Maybe Value.Value -> Value.Value
tagged t mv =
  object $ pair "type" (string t) : case mv of
    Nothing -> []
    Just v -> [pair "value" v]

nullary :: String -> Value.Value
nullary t = tagged t Nothing

-- Normalization --------------------------------------------------------------

-- | Recursively sorts every object's keys, so that two values differing only in
-- key order compare equal. JSON objects are unordered, so this is the right
-- notion of equality for comparing a parsed file against a re-encoded one.
--
-- Arrays are deliberately left alone: JSON arrays /are/ ordered, and the codec
-- relies on that -- a name-keyed map is rendered as a sorted array of entries
-- precisely so the order is deterministic.
--
-- Duplicate keys are not merged. 'List.sortOn' is stable and the extraction
-- helpers take the first match, so the two agree.
sortKeys :: Value.Value -> Value.Value
sortKeys value = case value of
  Value.Array a -> Value.Array . Array.MkArray . fmap sortKeys $ Array.unwrap a
  Value.Object o ->
    Value.Object
      . Object.MkObject
      . List.sortOn (String.unwrap . Pair.name)
      . fmap (\p -> Pair.MkPair (Pair.name p) (sortKeys (Pair.value p)))
      $ Object.unwrap o
  _ -> value

-- Rendering and parsing ------------------------------------------------------

render :: Value.Value -> Text.Text
render = Text.pack . Builder.toString . Value.encode

-- | 'Value.decode' already consumes the blanks around a document, so this only
-- has to pin the end of input and adapt the error to the codec's channel.
parse :: Text.Text -> Either Text.Text Value.Value
parse input = case Parsec.parse (Value.decode <* Parsec.eof) "" input of
  Left e -> Left . Text.pack $ show e
  Right value -> Right value

-- Extraction -----------------------------------------------------------------

asObject :: Value.Value -> Either Text.Text [Pair.Pair Value.Value]
asObject v = case v of
  Value.Object o -> Right $ Object.unwrap o
  _ -> Left . Text.pack $ "expected object but got " <> show v

asArray :: Value.Value -> Either Text.Text [Value.Value]
asArray v = case v of
  Value.Array a -> Right $ Array.unwrap a
  _ -> Left . Text.pack $ "expected array but got " <> show v

asText :: Value.Value -> Either Text.Text Text.Text
asText v = case v of
  Value.String s -> Right $ String.unwrap s
  _ -> Left . Text.pack $ "expected string but got " <> show v

asBoolean :: Value.Value -> Either Text.Text Bool
asBoolean v = case v of
  Value.Boolean b -> Right $ Boolean.unwrap b
  _ -> Left . Text.pack $ "expected boolean but got " <> show v

asInteger :: Value.Value -> Either Text.Text Integer
asInteger v = case v of
  Value.Number n ->
    let d = Number.unwrap n
        e = Decimal.exponent d
     in if e >= 0
          then Right $ Decimal.mantissa d * (10 ^ e)
          else Left . Text.pack $ "expected integer but got fraction " <> show v
  _ -> Left . Text.pack $ "expected number but got " <> show v

asTagged :: Value.Value -> Either Text.Text (String, Maybe Value.Value)
asTagged v = do
  ps <- asObject v
  t <- field "type" ps >>= asText
  pure (Text.unpack t, optionalField "value" ps)

-- Fields ---------------------------------------------------------------------

lookupPair :: String -> [Pair.Pair a] -> Maybe a
lookupPair k = fmap Pair.value . List.find ((== Text.pack k) . String.unwrap . Pair.name)

field :: String -> [Pair.Pair Value.Value] -> Either Text.Text Value.Value
field k ps = case lookupPair k ps of
  Just v -> Right v
  Nothing -> Left . Text.pack $ "missing field: " <> k

optionalField :: String -> [Pair.Pair Value.Value] -> Maybe Value.Value
optionalField = lookupPair

-- | An absent field reads as JSON null, which is what the @decode*Default@
-- family treats as "say nothing and take the default".
nullableField :: String -> [Pair.Pair Value.Value] -> Value.Value
nullableField k = Maybe.fromMaybe null . lookupPair k

withValue :: Maybe Value.Value -> (Value.Value -> Either Text.Text a) -> Either Text.Text a
withValue mv f = case mv of
  Just v -> f v
  Nothing -> Left $ Text.pack "missing tagged value"

-- Combinators ----------------------------------------------------------------
--
-- Generic over the element codec, which is taken as an argument.

decodeNullary :: String -> [(String, a)] -> Value.Value -> Either Text.Text a
decodeNullary tyName table value = do
  (t, _) <- asTagged value
  case lookup t table of
    Just x -> Right x
    Nothing -> Left . Text.pack $ "unknown " <> tyName <> ": " <> t

encodeList :: (a -> Value.Value) -> [a] -> Value.Value
encodeList f = array . fmap f

decodeList :: (Value.Value -> Either Text.Text a) -> Value.Value -> Either Text.Text [a]
decodeList f value = asArray value >>= traverse f

-- CR 613.6's card-data invariant: a static ability has at least one part. An
-- empty array is a decode failure, not an ability that does nothing.
encodeNonEmpty :: (a -> Value.Value) -> NonEmpty.NonEmpty a -> Value.Value
encodeNonEmpty f = encodeList f . NonEmpty.toList

decodeNonEmpty :: (Value.Value -> Either Text.Text a) -> Value.Value -> Either Text.Text (NonEmpty.NonEmpty a)
decodeNonEmpty f value = do
  xs <- decodeList f value
  case NonEmpty.nonEmpty xs of
    Nothing -> Left $ Text.pack "expected a non-empty array"
    Just ne -> pure ne

encodeSeq :: (a -> Value.Value) -> Seq.Seq a -> Value.Value
encodeSeq f = encodeList f . Foldable.toList

decodeSeq :: (Value.Value -> Either Text.Text a) -> Value.Value -> Either Text.Text (Seq.Seq a)
decodeSeq f value = Seq.fromList <$> decodeList f value

encodeSet :: (a -> Value.Value) -> Set.Set a -> Value.Value
encodeSet f = encodeList f . Set.toAscList

decodeSet :: (Ord a) => (Value.Value -> Either Text.Text a) -> Value.Value -> Either Text.Text (Set.Set a)
decodeSet f value = Set.fromList <$> decodeList f value

-- A count-per-key multiset, on the wire as a plain array WITH REPEATS rather
-- than as key/count pairs: it is what the thing being encoded is a list of, and
-- the encoding stays legible beside encodeSet's. Ascending by key, so it is
-- canonical. decodeMultiset recounts, so a hand-written file may repeat a key in
-- any order and a zero count is simply unsayable.
encodeMultiset :: (a -> Value.Value) -> Map.Map a Natural.Natural -> Value.Value
encodeMultiset f = encodeList f . concatMap (\(k, n) -> List.genericReplicate n k) . Map.toAscList

decodeMultiset :: (Ord a) => (Value.Value -> Either Text.Text a) -> Value.Value -> Either Text.Text (Map.Map a Natural.Natural)
decodeMultiset f value = Map.fromListWith (+) . fmap (\k -> (k, 1)) <$> decodeList f value

encodeMaybe :: (a -> Value.Value) -> Maybe a -> Value.Value
encodeMaybe = Maybe.maybe null

decodeMaybe :: (Value.Value -> Either Text.Text a) -> Value.Value -> Either Text.Text (Maybe a)
decodeMaybe f value = case value of
  Value.Null _ -> Right Nothing
  _ -> Just <$> f value

encodeNatural :: Natural.Natural -> Value.Value
encodeNatural = integer . toInteger

decodeNatural :: Value.Value -> Either Text.Text Natural.Natural
decodeNatural value = do
  n <- asInteger value
  case Integer.toNatural n of
    Just x -> Right x
    Nothing -> Left . Text.pack $ "expected natural but got " <> show n

-- Defaults -------------------------------------------------------------------
--
-- An omitted field decodes to the empty or default value, which lets an
-- all-default field stay OUT of the committed JSON so existing card files remain
-- byte-identical.

decodeListDefault :: (Value.Value -> Either Text.Text a) -> Value.Value -> Either Text.Text [a]
decodeListDefault f value = case value of
  Value.Null _ -> Right []
  _ -> decodeList f value

decodeSetDefault :: (Ord a) => (Value.Value -> Either Text.Text a) -> Value.Value -> Either Text.Text (Set.Set a)
decodeSetDefault f value = case value of
  Value.Null _ -> Right Set.empty
  _ -> decodeSet f value

decodeMapDefault :: (Value.Value -> Either Text.Text (Map.Map k v)) -> Value.Value -> Either Text.Text (Map.Map k v)
decodeMapDefault f value = case value of
  Value.Null _ -> Right Map.empty
  _ -> f value

decodeBooleanDefault :: Bool -> Value.Value -> Either Text.Text Bool
decodeBooleanDefault d value = case value of
  Value.Null _ -> Right d
  _ -> asBoolean value

-- Assertions -----------------------------------------------------------------

-- | Asserts both directions of a codec against one JSON literal, which is the
-- shape almost every case in a @Pawl.Codec.XSpec@ takes.
assertJsonCodec :: (Stack.HasCallStack, Monad m, Eq a, Show a) => Spec.Spec m n -> (a -> Value.Value) -> (Value.Value -> Either Text.Text a) -> a -> String -> m ()
assertJsonCodec s enc dec x j = do
  assertToJson s enc x j
  assertFromJson s dec j x

assertFromJson :: (Stack.HasCallStack, Monad m, Eq a, Eq b, Show a, Show b) => Spec.Spec m n -> (Value.Value -> Either a b) -> String -> b -> m ()
assertFromJson s f j x = do
  v <- assertJson s j
  Spec.assertEq s (f v) (Right x)

-- | Compares 'sortKeys'-normalized values, because JSON objects are unordered
-- and key order is not a property the codec has.
assertToJson :: (Stack.HasCallStack, Monad m) => Spec.Spec m n -> (a -> Value.Value) -> a -> String -> m ()
assertToJson s f x j = do
  v <- assertJson s j
  Spec.assertEq s (sortKeys (f x)) (sortKeys v)

-- | Goes through 'parse' rather than parsing itself, so a literal with trailing
-- garbage is a test failure instead of silently parsing as its prefix.
assertJson :: (Stack.HasCallStack, Monad m) => Spec.Spec m n -> String -> m Value.Value
assertJson s j = case parse (Text.pack j) of
  Left e -> Spec.assertFailure s $ "invalid JSON: " <> show j <> ": " <> show e
  Right v -> pure v
```

- [ ] **Step 4: Fix the placeholder in the spec and wire it up**

Build, read the real parse-error text for `"\"a\" x"`, and put it in the
`"rejects trailing input"` case. Then add to `source/test-suite/Main.hs`:
`import qualified Pawl.Codec.CommonSpec` (alphabetical, after
`Pawl.Codec.AbilityNameSpec`) and `Pawl.Codec.CommonSpec.spec s` in `spec`
(same position). Then:

```bash
cabal-gild pawl.cabal
```

- [ ] **Step 5: Verify**

```bash
cabal build all 2>&1 | tail -20 && cabal test 2>&1 | tail -20
```

Expected: warning-free build, green suite, count up by 7 from Task 1's baseline.

- [ ] **Step 6: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Build out Pawl.Codec.Common and its spec"
```

---

## Tasks 3–13: Convert the codec modules

Each task applies the Recipe's **Per-module procedure** to its list, in one
commit per task (or one per module if a task's list is long — either is fine, so
long as each commit builds and tests green). The lists are the dependency layers
of the codec import graph, leaves first, so every module's dependencies are
already converted when its turn comes.

**Files, for every task in this range:**
- Modify: `source/libraries/types/Pawl/Types/<Name>.hs` for each listed name
- Modify: `source/libraries/codec/Pawl/Codec/<Name>.hs` for each listed name
- Create: `source/libraries/codec/Pawl/Codec/<Name>Spec.hs` for each listed name
- Modify: `source/test-suite/Main.hs`, `source/test-suite/Pawl/CodecSpec.hs`, `pawl.cabal`

**Interfaces, for every task in this range:**
- Consumes: every name in `Pawl.Codec.Common` from Task 2; `X.toJson` / `X.fromJson` from earlier tasks in this range
- Produces: `X.toJson :: X.X -> Value.Value` and `X.fromJson :: Value.Value -> Either Text.Text X.X` for each listed name, plus any secondary name from the Recipe's table; and `Pawl.Codec.XSpec.spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()`

### Task 3: Layer 0, first half (20 modules)

- [ ] `AbilityName` (already converted in `2dfeb6c` — only re-point it at any `Common` helper Task 2 renamed, and confirm its spec still passes), `Aggregation`, `BeginningStep`, `CardType`, `CastingPermission`, `Color`, `CombatStep`, `Comparison`, `ControllerRelation`, `Counterability`, `DamageKind`, `DamageRewrite`, `DestructionRewrite`, `DiscardCause`, `EndingStep`, `ExtraPhase`, `Loyalty`, `ModeIndex`, `ModeSelection`, `MonarchTarget`
- [ ] Run `cabal build all` (warning-free) and `cabal test` (green)
- [ ] `git add -A && hooky fix && git add -A && hooky run && git commit`

`Counterability` carries `fromJsonDefault`; `ModeIndex` and `ModeSelection` are
newtypes needing `unwrap` (R3).

### Task 4: Layer 0, second half (20 modules)

- [ ] `ObjectId`, `Onset`, `Optionality`, `PlayerCounterKind`, `PlayerId`, `PlayerRelation`, `PlayerScope`, `Pool`, `Regenerability`, `Scaling`, `SearchDestination`, `SlotName`, `Subtype`, `Supertype`, `TapState`, `TriggerFrequency`, `TurnScope`, `Uses`, `Zone`, `ZoneChangeSubject`
- [ ] Run `cabal build all` and `cabal test`
- [ ] Commit

`Optionality` carries `fromJsonDefault`; `Subtype` carries `fromJsonPair` and has
87 constructors — its spec is the largest in this task. `ObjectId`, `PlayerId`
and `SlotName` are newtypes needing `unwrap` (R3).

### Task 5: Layer 1 (13 modules)

- [ ] `Countering`, `DamagePattern`, `EntryRiders`, `EventShape`, `Filter`, `ManaType`, `Phase`, `PlayerRef`, `Recipient`, `TokenPattern`, `TypeLine`, `ZoneChange`, `ZoneChangePattern`
- [ ] Run `cabal build all` and `cabal test`
- [ ] Commit

`EntryRiders` carries `defaultValue` and is Worked shape D. `Filter` carries
`optional`. `DamagePattern` and `TokenPattern` are newtypes needing `unwrap`.

### Task 6: Layer 2 (11 modules)

- [ ] `ActivationTiming`, `Affected`, `CastingRestriction`, `CostComponent`, `DamageEvent`, `ManaProduction`, `ManaSymbol`, `ObjectRef`, `PhaseSelector`, `Scope`, `TargetSpec`
- [ ] Run `cabal build all` and `cabal test`
- [ ] Commit

`TargetSpec`'s spec module is `Pawl.Codec.TargetSpecSpec` — see the note under
R7 in the Recipe.

### Task 7: Layers 3 and 4 (8 modules)

- [ ] `AttackRequirement`, `BlockRequirement`, `Count`, `ManaCost`, `PhasePattern`, `Cost`, `PlayerEffect`, `Quantity`
- [ ] Run `cabal build all` and `cabal test`
- [ ] Commit

`Quantity` is Worked shape C and carries `fromJsonPair`; its comment about
`Count`'s shared `"Count"` tag is decl-attached and becomes haddock (R6).
`AttackRequirement`, `BlockRequirement` and `ManaCost` are newtypes needing
`unwrap`.

### Task 8: Layers 5 and 6 (11 modules)

- [ ] `Condition`, `Keyword`, `PlayerStaticAbility`, `Power`, `Toughness`, `CounterKind`, `Duration`, `EntryOption`, `Expiry`, `Modification`, `TriggerCondition`
- [ ] Run `cabal build all` and `cabal test`
- [ ] Commit

`Power` and `Toughness` are Worked shape B and need `unwrap`. `Keyword` has 27
constructors, `TriggerCondition` 26.

### Task 9: Layers 7 and 8 (4 modules)

- [ ] `CounterPattern`, `EntryRewrite`, `StaticAbility`, `ReplacementEffect`
- [ ] Run `cabal build all` and `cabal test`
- [ ] Commit

### Task 10: `Effect` (layer 9)

- [ ] `Effect` — 77 constructors, 224 lines, the largest codec in the repo
- [ ] Move `CodecSpec`'s `ArmDelayedTrigger`, `Create` and `MoveToZone` elision assertions (the ones built on `payloadHead` and `payloadLength`) into `Pawl.Codec.EffectSpec`, rewritten against JSON literals. **These assert an ELISION — that a default rider is not written — and must not be lost.**
- [ ] Run `cabal build all` and `cabal test`
- [ ] Commit

Its three `-- ` blocks explaining the length-discriminated `ArmDelayedTrigger`,
`Create` and `MoveToZone` shapes are floating notes inside `case` bodies and stay
bare (R6).

### Task 11: Layers 10 to 12 (4 modules)

- [ ] `Mode`, `Modal`, `ActivatedAbility`, `TriggeredAbility`
- [ ] Run `cabal build all` and `cabal test`
- [ ] Commit

`TriggeredAbility` carries `toJsonDelayed` / `fromJsonDelayed`.

### Task 12: `Card` (layer 13)

- [ ] `Card` — 188 lines
- [ ] Run `cabal build all` and `cabal test`
- [ ] Commit

### Task 13: Layers 14 to 16 (5 modules)

- [ ] `Printing`, `ProjectedCharacteristics`, `Binding`, `GameEvent`, `DelayedTrigger`
- [ ] Run `cabal build all` and `cabal test`
- [ ] Commit

`Binding` carries `toJsonMap` / `fromJsonMap` and needs `unwrap` on `Printing`.

---

## Task 14: Delete `Pawl.Codec.Json` and the old specs

**Files:**
- Delete: `source/libraries/codec/Pawl/Codec/Json.hs`
- Delete: `source/test-suite/Pawl/CodecSpec.hs`
- Delete: `source/test-suite/Pawl/JsonSpec.hs`
- Modify: `source/libraries/registry/Pawl/Registry.hs`, `source/test-suite/Pawl/Corpus.hs`, `source/test-suite/Pawl/CardsSpec.hs`, `source/test-suite/Main.hs`, `pawl.cabal`

**Interfaces:**
- Consumes: `Common.parse`, `Common.sortKeys` from Task 2
- Produces: a codec library with exactly one base module

- [ ] **Step 1: Confirm `CodecSpec` is empty of codec cases**

```bash
grep -c 'Spec.it s' source/test-suite/Pawl/CodecSpec.hs
```

Expected: `0`. If not, the remaining cases belong to a module Tasks 3–13
converted — move them into that module's `XSpec` before proceeding. **Do not
delete a case to make this step pass.**

- [ ] **Step 2: Re-point the last three consumers**

`source/libraries/registry/Pawl/Registry.hs:125` — `Json.parse` becomes
`Common.parse`, and `jsonToCard` becomes `Card.fromJson`.
`source/test-suite/Pawl/Corpus.hs:65` — the same two.
`source/test-suite/Pawl/CardsSpec.hs:1189,1200` — `Json.parse` becomes
`Common.parse`, `Json.sortKeys` becomes `Common.sortKeys`, `printingToJson`
becomes `Printing.toJson`.

- [ ] **Step 3: Delete and regenerate**

```bash
git rm source/libraries/codec/Pawl/Codec/Json.hs source/test-suite/Pawl/CodecSpec.hs source/test-suite/Pawl/JsonSpec.hs
cabal-gild pawl.cabal
```

Remove `Pawl.CodecSpec` and `Pawl.JsonSpec` from `source/test-suite/Main.hs`
(both the import and the call) and from the test suite's `other-modules`.

- [ ] **Step 4: Verify**

```bash
cabal build all 2>&1 | tail -20 && cabal test 2>&1 | tail -20
```

Expected: warning-free, green. `Pawl.CardsSpec`'s corpus round-trip over
`data/cards/` passing here is the proof the wire format never moved.

- [ ] **Step 5: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Delete Pawl.Codec.Json and the monolithic codec spec"
```

---

## Task 15: Sweep the types with no codec

The 47 types the codec never touches still need R2, R3 and R6.

**Files:**
- Modify: `source/libraries/types/Pawl/Types/<Name>.hs` for each name below

**Interfaces:**
- Consumes: nothing
- Produces: `X.unwrap` on the newtypes among them

- [ ] **Step 1: Convert**

`Action`, `ActivePlayerEffect`, `ActiveReplacement`, `AttackTarget`,
`CandidateId`, `CardError`, `CardName`, `Combat`, `Concession`,
`ContinuousEffect`, `Decider`, `DecisionLog`, `Deck`, `Departure`, `Desync`,
`EntwineDecision`, `ExtraTurn`, `Game`, `GameState`, `HandActionPerformer`,
`LastKnown`, `Layer`, `Mana`, `ManaUnit`, `MonarchWatch`, `MulliganDecision`,
`MulliganOffer`, `Object`, `OptionalDecision`, `Payment`, `PendingTrigger`,
`PhyrexianPayment`, `Player`, `ProductionTag`, `Program`, `Prompt`,
`ProposedEvent`, `ReplacementBucket`, `ReplacementCandidate`, `Response`,
`RestartSignal`, `Result`, `Sickness`, `Source`, `Status`, `Timestamp`,
`TriggerSource`

`AttackTarget`, `Decider`, `Deck`, `Mana` and `Timestamp` are the newtypes among
them needing `unwrap` (R3).

- [ ] **Step 2: Verify**

```bash
cabal build all 2>&1 | tail -20 && cabal test 2>&1 | tail -20
```

Expected: warning-free, green. If the engine fails to compile, an `unwrap`
addition was not additive — revert that one and report it, do not edit the
engine.

- [ ] **Step 3: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Apply the type style to the types with no codec"
```

---

## Task 16: Update `CLAUDE.md`, self-review, and open the PR

**Files:**
- Modify: `CLAUDE.md`

**Interfaces:**
- Consumes: Task 1's baseline suite count
- Produces: the PR

- [ ] **Step 1: Update `CLAUDE.md`**

Two changes in "Where the tests go": the spec signature for codec specs is
`(Monad m, Monad n) => Spec.Spec m n -> n ()`, not `(Applicative m, Monad n)`;
and codec specs live beside the codec (`Pawl.Codec.Color` →
`Pawl.Codec.ColorSpec`) rather than in `source/test-suite/`. In "Code
conventions", state the `toJson`/`fromJson` naming and that `Pawl.Codec.Common`
is the codec's base module.

- [ ] **Step 2: Self-review the branch**

Per `CLAUDE.md`: re-check every CR citation the diff touched against
`docs/rules.txt`, and re-read every comment the rewrite touched for prose the
rename made wrong. Fix findings on the branch. The comment sweep (R6) is where
this pays — a misattached `-- |` reads as correct.

```bash
git diff main --stat | tail -5
git diff main -- '*.hs' | grep -E '^\+.*CR [0-9]' | sort -u
```

- [ ] **Step 3: Final verification**

```bash
cabal build all 2>&1 | tail -20 && cabal test 2>&1 | tail -20
```

Record the final suite count. Expected: up by roughly 600 (the new
constructor cases) minus 164 (the retired `CodecSpec` cases) from Task 1's
baseline.

- [ ] **Step 4: Open the PR as a draft, then mark it ready**

The body must carry: what changed and why; the design calls made (`encode*` /
`decode*` over `to*` / `from*` for combinators, `String` keys, one spec per
codec) and the alternatives rejected; how it was verified (warning-free build,
`hooky run` clean, suite count before → after, `Pawl.CardsSpec`'s corpus
round-trip as the proof the wire format is unchanged); and an explicit **"No"**
to whether the diff makes the rules core case on an effect's identity — this
diff adds no engine code at all.

There is no `Closes #N`: no issue tracks this refactor. Reference
`docs/superpowers/specs/2026-08-01-codec-style-refactor-design.md` instead.

Flip the draft to ready once the self-review's findings are pushed and the suite
is green. Do not wait for CI. Report and stop.
