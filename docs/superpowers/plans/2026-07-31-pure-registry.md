# Pure Registry Implementation Plan

Spec: `docs/superpowers/specs/2026-07-31-pure-registry-design.md`.

Turns `Pawl.Registry` from a concrete `IO` record into
`newtype Registry m = MkRegistry { fetchCard :: CardName -> m (Either CardError Card) }`,
moves the corpus sweeps into the test suite, and relaxes 30 spec modules off `IO`.

## Global constraints

- `cabal build all` warning-free after every task. `-Werror` is on
  (`cabal.project.local` sets `+pedantic`), so `-Wredundant-constraints` and
  `-Wname-shadowing` are build failures, not warnings.
- `cabal test` green after every task. The count is **2042** at the start and
  never changes — this refactor adds and removes no test cases.
- One build at a time. Subagents dispatched for sweeps must not build or test.
- No behaviour change to which cards resolve: `"Goblin Piker"` and
  `"goblin-piker"` remain the same lookup.

## Task order

Each task leaves the tree green, so each is a commit — except 3 and 4, which are
one commit because the suite cannot build between them.

### Task 1: `Pawl.Types.CardName` and `Pawl.Types.CardError`

Add both modules. `CardError` mentions only `CardName` and `String`, so `types`
needs no new dependency.

```haskell
newtype CardName = MkCardName {unwrap :: Text.Text}   -- Eq, Ord, Show
data CardError = Missing CardName | Invalid CardName String   -- Eq, Show
```

Nothing uses them yet. Run `cabal-gild pawl.cabal` directly — `hooky fix` skips
it on module add.

**Verify:** build warning-free; suite 2042.

### Task 2: `Pawl.Corpus` in the test suite, and the 18 lint sites repointed

New test-suite module `source/test-suite/Pawl/Corpus.hs`, holding the bodies of
today's `Registry.slugs` and `Registry.load`:

```haskell
slugsIn :: FilePath -> IO [Slug.Slug]
loadAll :: FilePath -> IO [(Slug.Slug, Either CardError.CardError Card.Card)]
```

`loadAll` reports a per-card `Either` rather than throwing, so the "every
committed file re-parses" case can name every bad file at once instead of dying
on the first.

Repoint:

- `S.corpusSlugs` → `Corpus.slugsIn`
- `S.allPrintings` → `Corpus.loadAll`, turning any `Left` into an
  `assertFailure`
- `RegistrySpec`'s `Registry.slugs` / `Registry.cards` cases → `Corpus`

`Corpus` takes a `FilePath` so it stays testable, and `S.allPrintings` resolves
`Registry.defaultRoot` itself rather than taking a registry — the lint is about
*the bundled corpus* by definition, so it should not need to be handed one. Its
signature becomes `Spec.Spec IO n -> IO [Printing.Printing]`; the 18 call sites
change from `S.allPrintings registry` to `S.allPrintings s`, and the modules that
used a registry for nothing else stop taking one.

`Pawl.Registry` still has its old API this task; only its enumeration callers move.

**Verify:** build warning-free; suite 2042; no `Registry.slugs`,
`Registry.cards` or `Registry.printings` anywhere.

### Task 3: the `Registry` newtype (with Task 4, one commit)

Rewrite `Pawl.Registry`:

```haskell
newtype Registry m = MkRegistry
  {fetchCard :: CardName.CardName -> m (Either CardError.CardError Card.Card)}

fileRegistry :: FilePath -> IO (Registry IO)
named        :: Registry m -> String -> m (Either CardError.CardError Card.Card)
defaultRoot  :: IO FilePath   -- unchanged
```

`fileRegistry` keeps today's `MVar` memo verbatim, including the #265
`modifyMVar` reasoning, now private. It keeps the root check so a mistyped
`--cards-dir` still fails once at startup (#167), throwing `MissingRoot`.

The `load` body moves in as a private function returning
`Either CardError Card`; its three failure paths become
`Missing name`, `Invalid name "<path>: not valid UTF-8: …"`,
`Invalid name "<path>: <parse error>"` and
`Invalid name "<path>: is named X, which files under Y"` — every detail today's
exceptions carry, in the message.

Delete `card`, `printing`, `printings`, `cards`, `slugs`, `new`, `cached`,
`load`, `root`, and the record fields. Delete the `Directory.listDirectory`
import — the library no longer enumerates.

`Main.hs` and `source/benchmark/Main.hs` switch to `fileRegistry` + `named`.

### Task 4: `S.printingOf` and the 2697-site sweep

Add to `Pawl.Support`:

```haskell
printingOf :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> m Printing.Printing
cardOf     :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> m Card.Card
```

Each calls `Registry.named` and turns a `Left` into
`Spec.assertFailure s (show err)`. Taking `String` keeps all 2697 literals
unchanged.

Sweep, per module:

- `Registry.printing registry "X"` → `S.printingOf s registry "X"`
- `Registry.card registry "X"` → `S.cardOf s registry "X"`

Mechanical and dispatchable to subagents, one module each, same protocol as the
pawl:spec conversion: edit one file, do not build, do not touch `Main.hs`, report
before/after counts so a dropped site is visible.

**The deck loaders take a fetch function, not a registry.** `redRed`,
`greenBlack` and `threeWayMirror` are called from spec modules (which become
polymorphic in `m`); `matchups`, `landsOnly` and `threePlayerLandsOnly` are
called only from `PropertySpec`, which is tasty-based and has no `s`. Since
`matchups` calls `redRed`, neither can simply be `IO`-only. So:

```haskell
redRed :: (Monad m) => (String -> m Printing.Printing) -> m (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
orThrow :: Registry.Registry IO -> String -> IO Printing.Printing
```

Spec modules pass `S.printingOf s registry`; `PropertySpec` passes
`S.orThrow registry`, which turns a `Left` into an exception — correct at
`m ~ IO`, and the reason `PropertySpec` needs no spec record.

**Verify:** build warning-free; suite 2042;
`grep -c "Registry\.printing\|Registry\.card " source/test-suite` is 0.

### Task 5: relax the 30 spec signatures

Each registry-backed module goes from

```haskell
spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
```

to

```haskell
spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
```

`Main.hs`'s aggregator follows. `RegistrySpec` stays at `IO` — it tests the file
strategy. Some groups will trip `-Wredundant-constraints`; drive that off GHC's
diagnostics rather than guessing which.

**Verify:** build warning-free; suite 2042; `grep "Spec.Spec IO n"` in the suite
matches only `RegistrySpec`.

### Task 6: retire the three exception types

Delete `Pawl.Exceptions.{UnknownCard,CorruptCard,MisfiledCard}`; keep
`MissingRoot`. Run `cabal-gild pawl.cabal` directly. `RegistrySpec`'s
`expectException` / `expectExceptionWith` helpers collapse into ordinary
assertions on the returned `CardError`, which is the point of the change.

**Verify:** build warning-free; suite 2042; `grep -r UnknownCard source/` finds
nothing.

### Task 7: hoist deck loading out of the property iterations

`PropertySpec` loads its decks *inside* `QC.ioProperty`, so `S.matchups` runs
once per QuickCheck iteration — 48 loads where 3 would do.

Measured, this is not a performance problem: the Properties group is 20.8s of the
24s suite, but that is 96 played-out games (4 matchups + 2 lands-only mirrors ×
16 seeds) at ~217ms each. The redundant loads are cached lookups and 60-element
list building — noise beside the games. This task is worth doing for what it does
to the *shape*, not the clock.

`Main.hs` loads the three deck sets once, in the `IO` it already has, and passes
them in:

```haskell
tests :: [NonEmpty …] -> NonEmpty … -> NonEmpty … -> Tasty.TestTree
```

Every `QC.ioProperty $ do { ms <- S.matchups registry; pure … }` then collapses
to a plain `QC.property`, because nothing in the body is effectful any more. The
properties become pure functions of the seed, which is what they always were.

Also correct the group's header comment: it says one game costs "~66 ms" and that
"the other 948 tests together take ~1 s". Both are stale — ~217ms and 2042 tests.

**Verify:** build warning-free; suite 2042; no `QC.ioProperty` remains in
`PropertySpec`; Properties group wall time unchanged within noise.

### Task 8: comment sweep and docs

The pawl:spec conversion taught that a rename of this size silently invalidates
prose. Sweep for:

- comments naming `Registry.printing`, `Registry.card`, `Registry.slugs`,
  `Registry.new`, `Registry.load`, `allPrintings`, `corpusSlugs`
- comments describing the registry as lazy or caching — still true, but now of
  `fileRegistry` specifically rather than of the type
- `Pawl.Registry`'s header, which claims to be "the library's only module that
  performs IO" and describes a record of root plus cache
- `CLAUDE.md`'s sublibrary table row for `registry`, and its
  "Where the tests go" section if `Pawl.Corpus` warrants a mention

**Verify:** `grep -rn "Registry\.\(printing\|card\|slugs\|new\|load\)" source/ docs/`
returns only live references.

## Verification summary

| | build | suite | extra |
|---|---|---|---|
| 1 | clean | 2042 | — |
| 2 | clean | 2042 | no `Registry.slugs/cards/printings` anywhere |
| 3+4 | clean | 2042 | no `Registry.printing/card` in the suite |
| 5 | clean | 2042 | `Spec.Spec IO n` only in `RegistrySpec` |
| 6 | clean | 2042 | three exception modules gone |
| 7 | clean | 2042 | no `QC.ioProperty` in `PropertySpec` |
| 8 | clean | 2042 | no stale prose |
