# Card File Formatting Independence Design

**Status:** approved (brainstorm), pending spec review
**Date:** 2026-07-22
**Topic:** replace the `data/cards/*.json` byte-stability assertion with a
key-order-insensitive `Value`-level one

## Motivation

`Pawl.CardsSpec.checkFile` currently makes two assertions per card file:

```haskell
HU.assertEqual path (Right p) (Json.parse contents >>= Codec.jsonToPrinting)
HU.assertEqual (path <> " bytes") (Json.render (Codec.printingToJson p) <> "\n") contents
```

The second demands **byte stability**: the committed file must equal the codec's
render exactly. That is stricter than the property anyone actually wants, and it
constrains the corpus in two ways that have no rules or engine justification:

1. **JSON objects are unordered.** `Pawl.Json.field` and `optField` are `lookup`
   on an assoc list, so *parsing* is already fully order-insensitive. Only the
   byte assertion cares about key order. Reordering a field by hand — a
   semantically null edit — fails the suite.
2. **Formatting is frozen.** `Json.render` emits minified, single-line output
   with no trailing newline. Pretty-printing a card file, or letting a tool
   normalize one, is unrepresentable.

Since M3.5 moved the source of truth into the files, the *first* assertion has
also gone nearly vacuous: `p` was loaded from that same file by
`Pawl.Cards.loadPrinting`, so it only checks that `slugify (name p)` agrees with
the filename it came from.

So one `assertEqual` is conflating three claims:

- **(a) the file loads.** Already guaranteed — a bad file fails `loadCards` in IO
  before any test runs.
- **(b) the loader consumes every byte of meaning in the file.** Nothing is
  silently ignored, nothing invented. Real, valuable, and caught *only* here: the
  codec is strict about `type` tags (`"unknown Effect: …"`) but not about object
  keys, so a file with `"toughnes": 0` parses happily and loads a wrong card.
- **(c) the file is in the codec's exact serialization.** Pure formatting.

This design keeps (b), which subsumes (a), and deletes (c).

## Goals

- Assert the round-trip at the `Value` level, up to key order and whitespace.
- Preserve the "no unread keys" guarantee exactly — no regression in what a
  malformed or stale card file is caught doing.
- Leave the corpus free to be formatted, reordered, or pretty-printed by any
  tool, with a committed regression guard proving it.

## Non-goals

- **No canonical on-disk form, and nothing that checks bytes.** `jq -S .` is
  available in the dev shell (`flake.nix:39`) for anyone who wants to tidy a file
  by hand; it is a convenience, not a contract. A mixed-format corpus is the
  property working, not drift.
- **No pretty-printing in `Json.render`.** The renderer stays minified. This is
  deliberate: adding indentation support would create exactly the canonical form
  the design is removing.
- **No strict-unknown-key decoding.** Rejecting unrecognized keys in
  `jsonToPrinting` is the other way to secure claim (b), but it makes forwards
  compatibility hard: a third-party file carrying keys pawl does not yet know
  would stop loading. The `Value`-level assertion lands the strictness at the
  *corpus* level instead — pawl's own committed files must be fully read, while
  any file with extra keys still loads.
- No engine changes. No changes to `Pawl.Codec`.

## Design

### The property

> Re-encoding the loaded printing reproduces the file's meaning.

Formally `toJson (fromJson v) ≡ v`, compared up to key order. This is strictly
stronger than the two assertions it replaces: it implies (a), and unlike today's
line 43 it would catch a file holding a *different* card under the wrong slug.

It preserves (b) in full. A file with a stray or misspelled key keeps that key in
`v` but not in `toJson p`, so the comparison still fails.

### `Pawl.Json.sortKeys`

```haskell
sortKeys :: Value -> Value
sortKeys value = case value of
  Json.Array xs -> Json.Array (map sortKeys xs)
  Json.Object ps -> Json.Object (List.sortOn fst (map (\(k, v) -> (k, sortKeys v)) ps))
  _ -> value
```

Arrays are left alone: JSON arrays genuinely are ordered, and the codec relies on
that (`Codec.delayedAbilitiesToJson` renders a name-keyed map as a sorted array
of entries precisely so the order is deterministic).

Named `sortKeys`, not `canonical`: there is no canonical form any more, so the
name should describe the operation and imply nothing about the corpus.

Duplicate keys are not deduped. `List.sortOn` is stable and `lookup` takes the
first, so behavior is consistent; two files differing only in duplicate-key order
would compare unequal, which is defensible and not worth special-casing.

### `CardsSpec.checkFile`

Collapses to a single assertion:

```haskell
checkFile p = do
  let path = "data/cards/" <> Text.unpack (slugOf p) <> ".json"
  contents <- TextIO.readFile path
  case Json.parse contents of
    Left err -> HU.assertFailure (path <> ": " <> Text.unpack err)
    Right value ->
      HU.assertEqual path (Json.sortKeys value) (Json.sortKeys (Codec.printingToJson p))
```

The `Left` branch is unreachable in practice (`loadCards` would have failed
first) but is required — no partial functions.

`Value`s are compared, not their renders. Comparing `render (sortKeys a)` against
`render (sortKeys b)` is equivalent and would give a more readable failure
message, but it reads like the byte check being deleted; the uglier `Show` on
mismatch is the honest choice.

### The regression guard

The whole corpus is committed in `jq -S .` form — sorted keys, indented,
trailing newline — while `Json.render` stays compact. The two are maximally far
apart, so the check cannot quietly regress into a byte comparison: every file
would fail at once, not just the one that happened to be reformatted.

It survives hooky unchanged: `jq` emits a trailing newline and no trailing
whitespace, satisfying `end_of_file_fixer` and `trailing_whitespace`.

The property also holds in the other direction without anyone maintaining it.
Regenerating a card writes compact output into a pretty corpus, and that file
passes too.

### Regenerating a card file

Unchanged, and still minified:

```haskell
TextIO.writeFile ("data/cards/" <> slug <> ".json")
  (Json.render (Codec.printingToJson p) <> Text.pack "\n")
```

To pretty-print one by hand (`sponge` is not in the dev shell):

```
jq -S . data/cards/foo.json > tmp && mv tmp data/cards/foo.json
```

Neither form is preferred; both pass.

## Task sequence

Two commits.

1. **`Json.sortKeys` + `JsonSpec` coverage.** Tests first, watched failing:
   nested objects sort, array order is preserved, `sortKeys` is idempotent, an
   already-sorted value is unchanged. `JsonSpec`'s existing byte assertions on
   `render` stay — those test the *renderer*, which is a different thing from
   card-data byte stability.
2. **Swap the assertion.** First run `jq -S .` over `clone.json` and run the
   suite to watch the *old* byte assertion fail on a semantically identical
   file — the bug, made visible. Then replace `checkFile`'s two assertions with
   the one above and watch it pass with `clone.json` left pretty.

The new assertion passes immediately on the current corpus, so it cannot be
watched to fail on its own; reformatting `clone.json` first is what makes the
TDD step meaningful, and it is the same act that leaves the regression guard
behind.

## Documentation touched

- `Pawl.Type.Json`'s header comment claims `Object` is an assoc list because
  "the codec controls key order, so a canonical render falls out of emitting
  fields in a fixed order." That rationale is now an implementation detail.
- **Not** `Codec.hs:1276` ("the render is deterministic and the file
  byte-stable"). Its point — deterministic array order for the name-keyed map —
  remains load-bearing, since arrays stay order-sensitive under `sortKeys`. Only
  the phrase "byte-stable" goes imprecise.
- **Not** `docs/progress.md:255`, which records that M3.5 established "P3 files
  re-parse and re-render byte-stable." That is an accurate historical record of
  what that milestone established; the completion log is not rewritten.

No issue is filed. This is a design decision reached and implemented, not an
elision with an expiry trigger, so the `(#N)` convention does not apply.
