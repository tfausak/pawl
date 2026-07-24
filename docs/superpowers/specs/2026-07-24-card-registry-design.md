# Card Registry Design

**Status:** approved (brainstorm), pending spec review
**Date:** 2026-07-24
**Topic:** replace the hand-maintained `Pawl.Cards` record with a lazily loading,
caching registry keyed on a card's slugified name (#145)

## Motivation

`Pawl.Cards` is 449 lines of hand-maintained indirection over files that are
already the source of truth. Adding a card edits three places — a record field, a
`loadPrinting` line, and a record-construction line — and every fixture that
wants one card threads the whole `Cards` record to reach one field (1,622
references across 30 spec modules). The `data/cards/*.json` files are already
loaded through `Pawl.Codec`; only the path to them is written by hand.

Two further consumers duplicate the same loading: `source/benchmark/Main.hs` has
its own private `loadPrinting`, and a future interactive CLI will need the same
thing again. The card pool is also going to grow — the real corpus is ~34,660
names — so loading everything up front to fill a record is the wrong shape.

## Goals

- Cards load **on demand**: nothing is read until it is asked for, so a
  30k-card root costs one `MVar` at startup.
- Each card file is parsed **at most once** per registry, however many tests use
  it.
- Adding a card to the pool means **adding one JSON file** and nothing else.
- A lookup is written with the card's **real name** (`"Serpent's Gift"`), which
  is also what the deferred scenario format (#146) will have in hand.
- The loading path is usable **outside the test suite** — the benchmark today, an
  interactive CLI later.

## Non-goals

- **No global mutable state and no `unsafePerformIO`.** The registry is a value
  the caller creates and passes. The top-level-`IORef` idiom is itself
  `unsafePerformIO`, so it is excluded on the same grounds. Threading is
  acceptable: it already happens, and this replaces `Cards` with `Registry`
  one-for-one.
- **No dependency injection of the file system.** A registry reads real files
  from a real directory; tests that need a controlled corpus build one in a
  temporary directory.
- **No eviction, no size bound, no invalidation.** A registry is
  write-once-per-key and lives as long as its owner.
- **No change to the card file format** and no change to `Pawl.Codec`'s parsing
  beyond the slug rule below.

## Design

### 1. The slug rule

`Codec.slugify :: Text -> Text` stays where it is — `Pawl.Codec` is already the
data-file boundary, and `Pawl.Registry` importing it is a legal sibling import.
Its rule becomes:

1. **Case-fold** with `Text.toCaseFold` (not `toLower`; folding maps `ß` to `ss`,
   so the eszett needs no table entry).
2. **Transliterate** through an explicit table: every Latin-1 Supplement letter
   plus the macron forms that occur in real names, mapped to ASCII — `á â à ä ã
   å → a`, `é è ê ë → e`, `í ì î ï → i`, `ó ò ô ö õ ø ō → o`, `ú ù û ü ū → u`,
   `ñ → n`, `ç → c`, `ý ÿ → y`, `æ → ae`, `œ → oe`, `ð → d`, `þ → th`.
3. **Delete `'`** so possessives read naturally.
4. Replace every maximal run of characters outside `[a-z0-9]` with a single `-`.
5. Trim leading and trailing `-`.

Step 4 is what makes the output ASCII *unconditionally*: an unmapped non-ASCII
character becomes a separator, so a gap in the table degrades slug quality but
can never leak a non-ASCII byte into a filename. It also absorbs the three
non-letter oddities in the real corpus (`®`, `꞉` U+A789, `—`).

```
Goblin Piker                  -> goblin-piker
Serpent's Gift                -> serpents-gift
Khabál Ghoul                  -> khabal-ghoul
Inner Calm, Outer Strength    -> inner-calm-outer-strength
Urborg, Tomb of Yawgmoth      -> urborg-tomb-of-yawgmoth
Fire // Ice                   -> fire-ice
Sword of Dungeons & Dragons®  -> sword-of-dungeons-dragons
_____                         -> (empty)
```

The rule is **idempotent** — its output is already `[a-z0-9-]` with no runs and
no edge hyphens — which is why the registry needs only one lookup function
rather than a by-name and a by-slug pair: `"Goblin Piker"` and `"goblin-piker"`
are the same call.

**Evidence.** Applied to all 34,660 distinct names in the vendored MTGJSON dump,
the rule produces `[a-z0-9-]` throughout and 7 collisions: `_____`/`______`
(Unhinged and Unknown Event blank-name cards), five joke cards from Unknown
Event (`Lava, Axe` vs `Lava Axe` and four siblings), and `Glimpse, the
Unthinkable` — Mystery Booster 2 #594, `promoTypes: ["playtest"]`, legal in no
format — against `Glimpse the Unthinkable`. **No two tournament-legal cards
collide.** Uniqueness is nevertheless checked (§3) rather than assumed: it is
insurance against a typo in a new file, not a live conflict.

Two committed files are renamed to match the new rule:
`khabál-ghoul.json → khabal-ghoul.json` and
`serpent-s-gift.json → serpents-gift.json`.

### 2. The registry

In the **library**, since the benchmark and a future CLI want it as much as the
tests do. Per the one-type-per-module rule:

```haskell
-- Pawl.Type.Registry
data Registry = MkRegistry
  { root :: FilePath,
    cache :: MVar.MVar (Map.Map Text.Text Card.Card)  -- keyed by slug
  }
  deriving (Eq)

-- Pawl.Registry
new      :: FilePath -> IO Registry
card     :: Registry -> String -> IO Card.Card
printing :: Registry -> String -> IO Printing.Printing
```

`card` slugifies its argument, takes the `MVar`, and returns the cached card if
there is one. On a miss it reads `<root>/<slug>.json`, parses it with
`Codec.jsonToCard`, checks that the parsed card's own `name` slugifies back to
the slug it was filed under, inserts, and returns. `printing` is
`fmap Printing.MkPrinting . card` — the cache holds **cards**, because that is
what the files contain; `Printing` is `newtype Printing = MkPrinting { card ::
Card }` and wrapping is the caller's business.

Three deliberate choices:

- **`MVar`, not `IORef`.** The suite is built `-threaded` and tasty runs test
  cases concurrently, so an `IORef` cache lets two threads miss and both parse
  the same file. The result is still correct (parsing is pure and
  deterministic), but it breaks the at-most-once property this design promises.
  Holding the `MVar` across the read-and-parse makes once-only exact. The cost —
  loads serialize — is irrelevant for files of this size.
- **The argument is `String`, not `Text`.** This is the one place where the house
  `Text` rule loses on its own terms: the argument's destiny is a `FilePath`, and
  the alternative is `Text.pack` at ~1,600 call sites with `OverloadedStrings`
  banned. The cache key is `Text` (the slug).
- **No directory listing in the library**, so it needs no new dependency.
  Enumerating a corpus is a test-suite concern, and the test suite already
  depends on `directory`.

`Pawl.Registry` is the library's **first module that performs IO** — everything
else, `Pawl.Codec` included, is pure. That is the point: it is the IO shell
around a pure codec, and it is the only place in the library that touches a file
system. It needs no new dependency: `MVar` comes from `base`,
`Data.Text.IO.readFile` from `text`, and `Map` from `containers`, all of which
the library already depends on.

`Registry` derives `Eq` but not `Show`: `MVar` has an `Eq` instance and no `Show`
instance. This is a documented exception to the derive-`Eq`-and-`Show` rule,
noted at the type.

### 3. Failure modes

All four are `ioError`s naming the offending path or slug — loud, in IO, never
`error` in pure code:

| Condition | Message |
| --- | --- |
| the argument slugifies to the empty string | names the argument, so `""` never reads `<root>/.json` |
| no such file | names the full path |
| the file does not parse | names the path and the codec's `Left` |
| the parsed `name` slugifies to a different slug | names both slugs and the path |

The last is the check the issue asks for — a file's `name` field disagreeing with
its filename — and it now happens per load, lazily, rather than needing a sweep.
Failures are **not** cached: a second lookup retries and fails the same way.

### 4. Test-suite migration

`Main` creates one registry and threads it exactly where `cards` goes today, so
the 30 `tests :: Cards.Cards -> TestTree` signatures become
`tests :: Registry.Registry -> TestTree` — same shape, minus the record:

```haskell
main :: IO ()
main = do
  registry <- Registry.new "data/cards"
  Tasty.defaultMain (testTree registry)
```

Use sites become `piker <- Registry.printing registry "Goblin Piker"`. Fixtures
follow the issue's guidance: a fixture that needs one card takes a `Printing`
directly; a fixture that needs several takes the `Registry` and becomes `IO`.
The QuickCheck properties in `PropertySpec` reach the registry through
`QC.ioProperty`.

`Pawl.Cards` survives only as the four deck definitions, now
`redDeck :: Registry -> IO Deck.Deck` and siblings. `Cards.allPrintings`,
`Cards.loadPrinting`, and the `Cards` record itself are deleted, as is the
benchmark's private `loadPrinting`.

Two existing corpus-wide tests change shape, both driven by a directory listing
in the test suite rather than by `allPrintings`:

- `CardSpec`'s "the directory and `Cards.allPrintings` agree, by slug" becomes
  "every file in `data/cards` loads, and its `name` slugifies to its filename".
  The registry's per-load check makes the second half automatic; the test's job
  is to make it run over *every* file rather than only the ones some test
  happens to use.
- `CardsSpec`'s slug-uniqueness and re-parse round-trip tests load the corpus by
  listing the directory and asking the registry for each slug.

## Testing

New `Pawl.RegistrySpec`, covering `Pawl.Registry` and `Pawl.Type.Registry`:

- **Slug table** (unit): the worked examples above, plus leading/trailing
  punctuation, interior `//`, digits, and the empty result.
- **Idempotence** (property): `slugify (slugify t) == slugify t`.
- **ASCII output** (property): every character of `slugify t` is in `[a-z0-9-]`.
- **Name and slug agree** (unit): looking a card up by its real name and by its
  slug returns the same card.
- **Loaded at most once** (unit): in a temporary directory, load a card, delete
  the file, load it again — the second lookup succeeds from the cache. This is
  the only way to observe the cache without instrumenting it.
- **Each failure mode** (unit): missing file, malformed JSON, a file whose `name`
  disagrees with its filename, and an argument that slugifies to empty — each
  built in a temporary directory and asserted to raise.

The whole-corpus checks stay in `CardSpec`/`CardsSpec` as described above.

## Phasing

Each step is one commit on `main`; the plan will expand them.

1. Rewrite `Codec.slugify` (case-fold, transliterate, drop apostrophes) with its
   unit and property tests; rename the two card files.
2. Add `Pawl.Type.Registry` and `Pawl.Registry` with `Pawl.RegistrySpec`. No
   caller changes yet.
3. Migrate `Pawl.Support` and `Main` to the registry; reduce `Pawl.Cards` to its
   decks.
4. Migrate the spec modules in batches (largest first: `ResolveSpec`,
   `CardSpec`, `GameSpec`, …), reshaping the two corpus-wide tests as they come.
5. Delete the `Cards` record and `loadPrinting`; switch the benchmark to
   `Registry`.

## Risks

- **Size.** ~1,600 mechanical call-site edits across 30 files. Mitigated by
  phasing: the suite builds and passes at every step, since `Pawl.Cards` and the
  registry coexist until step 5.
- **Slug collisions in a future 30k-card pool.** One real pair exists
  (`Glimpse the Unthinkable` vs the MB2 playtest card) and more would arrive with
  joke sets. Out of scope here — a filename holds one card, and the pool this
  registry serves is the committed corpus. Whoever imports the full pool will
  need a disambiguation rule; the uniqueness check will tell them so loudly.
- **`MVar` held across IO.** A load that throws must not leave the `MVar` empty.
  The implementation uses `MVar.modifyMVar`, which restores on exception.

## References

- Issue #145 — this work.
- Issue #146 — the deferred scenario format that consumes name-keyed lookup.
- `docs/superpowers/specs/2026-07-22-card-file-formatting-independence-design.md`
  — the round-trip assertion whose shape §4 preserves.
