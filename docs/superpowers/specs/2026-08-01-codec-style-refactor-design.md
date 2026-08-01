# One style for every type and codec

## The problem

`2dfeb6c` refactors `AbilityName` — the type, the codec, and a new spec — into a
style the rest of the repo does not follow. It is one type out of 246. The work
is to apply that style to the other 245 without changing a single byte of what
the codec puts on the wire.

The commit is small enough to read in one screen, which hides how much of it is
policy rather than edit. Seven rules come out of it, and one of them —
`Pawl.Codec.Json` becoming `Pawl.Codec.Common` — is not a rename at all but a
redesign of the codec's base API.

## The rules

Each is stated so a reviewer can check a module against it.

**R1 — no ceremonial module haddock.** `2dfeb6c` deletes
`-- | The @AbilityName ⇆ Json@ codec (#481).`, which says only what the module
name says. **98 codec modules carry that header**, and #481 is closed, so these
are 98 stale citations to a finished issue. A module keeps a haddock only when it
has something non-obvious to say; `Pawl.Codec.Json`'s current header is an
example that earns its place and moves to `Common`.

**R2 — every import is qualified**, aliased to the module's last component, with
`Pawl.Support as S` remaining the one documented exception (it is out of scope
here). The repo-wide count:

| Import | Sites |
|---|---|
| `Data.Text (Text)` | 98 (codec) + 4 (types) |
| `Pawl.Json.Value (Value)` and `Value (Array)`/`(Null)` | 95 |
| `Pawl.Json.Array (Array (MkArray))` | 21 |
| sibling `Pawl.Codec.*` value imports | ~150 |
| `Pawl.Types.*` type imports (types library) | ~230 |
| `Numeric.Natural (Natural)`, `Data.Set (Set)`, `Data.Map.Strict (Map)` | 51 |

**R3 — a newtype is a record with an `unwrap` field.** 21 newtypes in the types
library, **18 lacking it**. Adding the field is backward-compatible for both
construction and `MkX x` pattern matching, so it forces **no** changes in the
engine, the registry, or the test suite. Codec modules switch to `X.unwrap`
deliberately, as `AbilityName` did.

**R4 — `toJson` / `fromJson`.** The `x` in `xToJson` is redundant once the import
is qualified. 101 `xToJson` and 105 `jsonToX` become `X.toJson` and
`X.fromJson`. **Declaration order does not change** — a module that reads
encoder-then-decoder today keeps that order, so the diff stays a rename.
(`Pawl.Codec.AbilityName` already reads decoder-first; it stays as it is, since
the rule is "don't reorder", not "encoder must come first".)

Eight modules hold a second codec pair. Those suffix the distinguishing word
onto the base name, keeping every codec in the module sorted together:

| Today | Becomes |
|---|---|
| `jsonToCounterabilityDefault` | `Counterability.fromJsonDefault` |
| `jsonToOptionalityDefault` | `Optionality.fromJsonDefault` |
| `jsonToQuantityPair` | `Quantity.fromJsonPair` |
| `jsonToSubtypePair` | `Subtype.fromJsonPair` |
| `bindingsToJson` / `jsonToBindings` | `Binding.toJsonMap` / `fromJsonMap` |
| `delayedAbilitiesToJson` / `jsonToDelayedAbilities` | `TriggeredAbility.toJsonDelayed` / `fromJsonDelayed` |
| `defaultEntryRiders` | `EntryRiders.defaultValue` |
| `optionalFilter` | `Filter.optional` |

**R5 — no `Text.pack` for a JSON key or tag.** `Common.pair`, `Common.tagged`,
`Common.nullary` and `Common.field` take `String`. This is where most of the
noise in the codec dies: `Json.jObject [(Text.pack "name", v)]` becomes
`Common.object [Common.pair "name" v]`.

**R6 — decl-attached comments become haddock.** A bare `--` block *immediately
above a declaration* becomes `-- |`. A floating note between or inside
declarations stays bare — `-- |` on an unattached comment either fails to parse
or silently documents the wrong thing. **120 of 145** types modules and **27**
codec modules have bare `--` blocks, so this needs per-comment judgment and is
not a `sed`.

**R7 — a spec beside every codec.** `Pawl.Codec.XSpec`, exposed from
`library codec`, wired into `Main.hs`'s `spec`. Their signature is
`(Monad m, Monad n) => Spec.Spec m n -> n ()`; `CLAUDE.md` currently documents
`(Applicative m, Monad n)`, which `assertFromJson`'s bind rules out.

## `Pawl.Codec.Json` → `Pawl.Codec.Common`

`2dfeb6c` migrated 9 of ~40 helpers, and three of the nine changed *shape*, not
just name. That is the redesign hiding inside the rename.

### Two families for primitives

Bare nouns construct, `as*` extracts. The commit establishes both; they extend
without argument.

| `Pawl.Codec.Json` | `Pawl.Codec.Common` | Note |
|---|---|---|
| `jNull` | `null` | done |
| `jBool` | `boolean` | done |
| `jInt :: Integer -> Value` | `number :: Integer -> Integer -> Value` | done; mantissa + exponent |
| — | `integer :: Integer -> Value` | new: `number n 0`, which is what every current `jInt` site wants |
| `jText` | `text` | done |
| — | `string :: String -> Value` | done |
| `jArray` | `array` | done |
| `jObject :: [(Text, Value)] -> Value` | `object :: [Pair.Pair Value] -> Value` | done |
| — | `pair :: String -> a -> Pair.Pair a` | done |
| `asText` | `asText` | done; gained the offending value in its error |
| `asObject :: … -> [(Text, Value)]` | `asObject :: … -> [Pair.Pair Value]` | mirrors `object` |
| `asArray` | `asArray` | |
| `asInteger` | `asInteger` | |
| — | `asBoolean` | new; `jsonToBoolDefault` currently inlines it |

Every `as*` gains the `"expected X but got " <> show v` treatment `asText`
already has. Today they all say only `"expected object"`.

### The tagged-object convention

| Today | Becomes |
|---|---|
| `tagged :: Text -> Maybe Value -> Value` | `tagged :: String -> Maybe Value -> Value` |
| `nullary :: Text -> Value` | `nullary :: String -> Value` |
| `tag :: Value -> Either Text (Text, Maybe Value)` | `asTagged :: Value -> Either Text (String, Maybe Value)` |
| `decodeNullary :: Text -> [(Text, a)] -> …` | `decodeNullary :: String -> [(String, a)] -> …` |
| `field :: Text -> [(Text, Value)] -> Either Text Value` | `field :: String -> [Pair.Pair Value] -> Either Text Value` |
| `optField` | `optionalField :: String -> [Pair.Pair Value] -> Maybe Value` |
| `getOpt` | `nullableField :: String -> [Pair.Pair Value] -> Value` |
| `withValue` | `withValue` (unchanged; 23 sites) |

`asTagged` returning `String` matters: **31 codec modules already open with
`case Text.unpack t of`**, so this deletes a conversion rather than adding one.

### Combinators: `encode*` / `decode*`

The helpers taking an element codec cannot use `to*`/`from*`. In
`Pawl.Codec.AbilityName` the implied self is `AbilityName`, so `toJson`
*encodes*; in `Common` the implied self is JSON, so `toList` would *decode*. Same
two words, opposite direction. `encode`/`decode` is unambiguous without a
convention memo, and `decodeNullary` already reads that way.

| Today | Becomes |
|---|---|
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

`sortKeys`, `render` and `parse` keep their names.

### Growth is on demand

`Common` does not get a big-bang port. A helper moves when the first codec module
being refactored needs it, under its new name. `Pawl.Codec.Json` shrinks and is
deleted when its last consumer is gone — `Pawl.Registry` (`Json.parse`) and
`Pawl.Corpus` are the two outside the codec library.

New helpers are welcome when a call site wants one; `integer` and `asBoolean`
above are the first two.

### Test helpers

`assertJson`, `assertFromJson` and `assertToJson` stay in `Common`. Two changes:

- **`assertToJson` normalizes with `sortKeys`.** It already parses its literal
  into a `Value` and compares `Value`s, so this is a one-line insertion. Without
  it, every object-valued codec's spec would pin JSON key *order*, which is not
  a property the codec has.
- **Add `assertJsonCodec s toJson fromJson x "…"`**, asserting both directions
  from one literal. With ~600 constructors to cover, one line per constructor
  instead of two is the difference between a large job and an unreasonable one.

`Common` itself gets a `Pawl.Codec.CommonSpec`, which is where the existing
`Pawl/JsonSpec.hs` cases for `parse`, `render`, `tagged` and `sortKeys` land.

## Tests

Every codec module gets `Pawl.Codec.XSpec` with `describe "toJson"` /
`describe "fromJson"` groups. **The bar is every constructor**: one
`assertJsonCodec` line per constructor of the type, ~600 in total across the
codec'd types (`Subtype` 87, `Effect` 77, `Keyword` 27, `Filter` 27,
`TriggerCondition` 26, `GameEvent` 20, and a long tail).

This is a real strengthening, not a move. `CodecSpec`'s `roundTrip` asserts
`decode (encode x) == Right x`, which never pins the wire format — an encoder
that renamed every tag would still pass. A literal JSON string pins it. It is
also the majority of the work in this refactor, well past the ~100 module
rewrites, and it is why the bar is worth stating explicitly.

`Pawl/CodecSpec.hs` (1392 lines, 164 cases) is emptied batch by batch and
deleted; its bespoke shape assertions — the elision tests using `payloadHead`
and `payloadLength` — move into the relevant `XSpec` rather than being dropped.
`Pawl/JsonSpec.hs` moves to `Pawl.Codec.CommonSpec`. `Pawl/CardsSpec.hs` and
`Pawl/Corpus.hs` keep their corpus-level round-trips over `data/cards/` and only
switch `Json` to `Common`.

## Scope

**In:** `source/libraries/types/` (145 modules), `source/libraries/codec/`
(101 modules), the ~100 new `XSpec` modules, `pawl.cabal`,
`source/test-suite/Main.hs`, and whatever `Pawl.Registry`, `Pawl.Corpus`,
`Pawl.CardsSpec` and `Pawl.CodecSpec` must change to compile.

**Out:** the engine's 37 modules, and R2/R6 in every sublibrary other than
`types` and `codec`. Those are a separate pass. The engine is the most
rules-dense code in the repo and the least suited to sharing a diff with a
mechanical rename.

**Not a goal:** changing the wire format. `data/cards/` is not touched.

## Delivery

One PR, with small commits inside it. Each commit builds and leaves the suite
green.

1. `Common` gains `sortKeys`, `assertJsonCodec`, the `assertToJson`
   normalization, and the helpers the first batch needs.
2. …N. Codec modules in dependency-layer batches, leaves first (`Color`,
   `Onset`, `Power`, …), `Card` and `Effect` last. Each commit carries the type
   module, the codec module, the new `XSpec`, the `Main.hs` wiring and the
   `pawl.cabal` regeneration together, and deletes the cases it supersedes from
   `CodecSpec.hs`.
3. Delete `Pawl.Codec.Json`, `Pawl/CodecSpec.hs`, `Pawl/JsonSpec.hs`.
4. Sweep the 47 types with no codec module (`Game`, `GameState`, `Object`,
   `Player`, …) for R2, R3 and R6.
5. Update `CLAUDE.md`: the spec signature is `(Monad m, Monad n)` for codec
   specs, and the codec naming conventions are new.

Batching by dependency layer, not alphabetically, is what keeps each commit
compiling: renaming `Color.toJson` breaks the six modules that import it, and
those six should be in the same commit.

## Verification

- `cabal build all` warning-free under `-Weverything`, `hooky fix` then
  `hooky run` clean, per commit.
- **`Pawl.CardsSpec` is the safety net.** It round-trips every file in
  `data/cards/` and compares `sortKeys`-normalized values, so any accidental
  change to the wire format fails there regardless of which `XSpec` was written.
  It must stay green at every commit.
- Suite count captured before the first commit and reported at the end; it
  should rise by roughly the ~600 new cases minus the 164 retired.
- No new case where the rules core cases on an effect's identity — this diff
  adds no engine code at all, so the answer is no by construction.

## Risks

**The comment sweep (R6) is the one place a mechanical pass can silently lose
information.** A misattached `-- |` documents the wrong declaration and reads as
correct. Every converted comment needs a human-checked attachment, and comments
carrying CR citations need those citations re-checked against `docs/rules.txt`
per `CLAUDE.md` — the rename is a good moment to catch a stale one, and a bad
moment to propagate one.

**~600 hand-written JSON literals is where a typo hides.** A wrong tag string in
a literal produces a spec that agrees with a wrong encoder. `Pawl.CardsSpec`
catches this only for shapes that appear in `data/cards/`. Literals should be
derived by reading the encoder, not by recalling the tag name.

**`Common.parse` and `Common.assertJson` use different parsers** — `parse` goes
through `Text.Parsec` on `Text` with `eof`, `assertJson` through
`Pawl.Extra.Parsec.parseString` on `String` without one. They should be unified
onto one, or the difference stated, before ~600 specs are written against
`assertJson`.
