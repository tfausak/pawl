# A registry that is one function, and a corpus lint that is not a registry

## The problem

`Pawl.Registry` is a concrete record over `IO`:

```haskell
data Registry = MkRegistry { root :: FilePath, cache :: MVar (Map Slug Card) }
card :: Registry -> String -> IO Card.Card
```

Three separable things are fused into it:

1. **the question** — given a card name, what card is that?
2. **an answer strategy** — read one JSON file per lookup, lazily;
3. **a memo** — an `MVar` holding what has been read so far.

Only (1) is what a caller asks for. (2) and (3) are one implementation of it,
and their being welded to the type has two costs.

**It monomorphizes every consumer on `IO`.** `Registry.printing` returns
`IO Printing`, so a spec that touches a card is pinned to `Spec IO n` — 30 of
the 37 modules in the test suite. Whether *this* repo would ever instantiate the
assertion monad at anything else is beside the point: the type currently says
"reading a card may do anything IO can do", when what it needs to say is
"reading a card may fail". A caller who has the cards already — a fixture map, a
pool compiled in, a fuzzer's generated pool — cannot express that.

**It forbids other answer strategies.** Eager loading, a bounded cache, a
network-backed pool, a pure map: each is a different way to answer the same
question, and none of them can be a `Registry` today.

Caching is not load-bearing at this scale. Measured, on 180 cards and 2694
dynamic lookups: the suite runs in 24.10s with the cache and 26.16s with
`cached` reduced to `load`, and exactly one test fails — the one asserting the
memo. So the memo buys ~8% and costs the abstraction. At 30k cards the answer
will differ, which is an argument for making the strategy swappable, not for
fixing one in the type.

## Design

### A. `Registry` is one function

```haskell
newtype Registry m = MkRegistry
  { fetchCard :: CardName.CardName -> m (Either CardError.CardError Card.Card) }
```

A newtype record rather than a bare `type Registry m = …`: it matches the `MkFoo`
convention, and a field name reads better at call sites than an application.

`m` is the caller's monad. `Registry IO` reads files; `Registry Identity` reads a
`Map`; `Registry (Either e)` is legal and useless, which is fine — the type does
not have to forbid it.

**Failure is a value, not an exception.** `Either CardError Card` in the return
type is what makes a pure registry possible at all: `Registry Identity` cannot
throw. It also puts "this may fail" where a reader sees it.

### B. One strategy, because the type is the generality

```haskell
fileRegistry :: FilePath -> IO (Registry IO)
```

One value, not three. The point of `Registry m` is that a caller can write their
own — a `Map`-backed one, an eager one, a network one — not that this library
ships them. Writing `mapRegistry` here would be shipping a caller's code on
speculation.

Caching is not a separate layer either: it is how `fileRegistry` happens to
answer, private to it. The existing `MVar`, its hand-written `modifyMVar` and the
#265 concurrency reasoning move inside and stop being part of the interface. A
future bounded/LRU variant is a second value of the same type, not a change to it.

`IO (Registry IO)` rather than `Registry IO` because allocating the memo — and
checking the root exists, which #167 wants done once at startup rather than once
per missing card — are themselves effects.

### C. `CardError` says nothing about files

```haskell
data CardError
  = Missing CardName.CardName
  | Invalid CardName.CardName String
```

The error type belongs to the *interface*, so it must be expressible by every
strategy. An earlier draft had `Unknown Slug FilePath` / `Corrupt FilePath Text` /
`Misfiled {path, expected, actual}`, which is the file-backed implementation
leaking through the abstraction: a `Map`-backed registry has no path to report
and no slug to disagree about. `Missing` and `Invalid` are answerable by anything.

The detail is not lost, it moves into `Invalid`'s `String`: the file strategy
still says which path failed and why, including the misfiled-name mismatch. What
#167 actually bought — not having to string-match to tell "no such card" from
"that card is broken" — survives as the constructor split. `Corrupt` and
`Misfiled` were both the second case; they differed only in the message, which is
where they now differ.

`Invalid` rather than `Corrupt`: corruption suggests damaged bytes, which is one
file-specific cause among several. A card that parses but fails validation is
equally unusable and equally not corrupt.

`MissingRoot` is not a `CardError`. It is a failure of *constructing* a file
registry, not of fetching a card, so it stays an exception thrown by
`fileRegistry`.

### D. The corpus lint moves into the test suite

`Registry.slugs`, `Registry.cards` and `Registry.printings` exist for 18 test
sites, every one of them a sweep asserting a property of *the bundled data*:
every committed file re-parses, every mode's slots are declared, every card's
file name matches its own name. That is a lint over `data/cards`, not a question
anyone asks a registry — and it is the only reason `Registry` would need a second
field.

It moves out of the library entirely, to `source/test-suite/Pawl/Corpus.hs`:

```haskell
slugsIn :: FilePath -> IO [Slug.Slug]
loadAll :: FilePath -> IO [(Slug.Slug, Either CardError.CardError Card.Card)]
```

Nothing outside the suite lints the shipped corpus, and a library module no
consumer calls is dead weight. The `registry` sublibrary consequently stops
enumerating directories at all — it reads one named file.

### E. The test suite absorbs the `Either`

2697 call sites take a card. None of them should grow a `case`. `Pawl.Support`
absorbs it, and `Spec.assertFailure` is the natural absorber:

```haskell
printingOf :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> CardName -> m Printing.Printing
```

A site goes from `Registry.printing registry "Bog Wraith"` to
`S.printingOf s registry "Bog Wraith"` — one qualifier, one argument. This is
strictly better than today: a missing card becomes a named test failure instead
of an exception escaping the assertion.

Once no spec mentions `IO` directly, the 30 registry-backed modules relax from
`Spec.Spec IO n -> Registry.Registry -> n ()` to
`(Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()`.

`Pawl.RegistrySpec` is the exception and stays at `IO` by rights: it tests the
file-backed strategy, including failure modes that only a filesystem has.

### E2. Nothing needs to demonstrate the purity

`design.md` §4 says a capability is not done until something exercises it. That
rule is about the open half — an effect the engine can express but no card
reaches is a branch nobody has checked. It does not apply here, and an earlier
draft misapplied it by proposing a `mapRegistry` plus a pure `Pawl.Spec` backend
to "prove" the abstraction.

There is nothing to prove. `spec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()`
cannot secretly reach `IO`: parametricity is the guarantee, and it is checked by
the compiler at the definition, not by a caller at some instantiation. A test
that instantiated `m` at something pure would be testing GHC.

That is the sense in which the guarantee is free. It costs one type parameter and
buys a property no test has to maintain.

### F. `CardName`, plus a `String` helper that keeps it quiet

`card :: Registry -> String -> IO Card` takes a bare `String`, slugified
internally, so `"Goblin Piker"` and `"goblin-piker"` are one lookup.
`Pawl.Types.CardName` (`newtype CardName = MkCardName Text.Text`) makes the
argument say what it is.

Without `OverloadedStrings` the newtype would cost
`CardName.MkCardName (Text.pack "Goblin Piker")` at every one of 2697 sites,
which is a worse document than the `String` it replaced. So the newtype is what
the *interface* speaks, and a `String` helper is what callers use:

```haskell
-- Pawl.Registry
named :: Registry m -> String -> m (Either CardError.CardError Card.Card)
```

Behaviour is unchanged: both spellings still resolve, because `fetchCard` still
slugifies.

## What this buys

- Any caller can supply their own registry — a fixture map, a compiled-in pool, a
  network source — and the ~1400 registry-backed test cases become typed as
  unable to read the disk or the clock. Parametricity gives that for free; no
  test maintains it.
- The answer strategy becomes a value, so the 30k-card cache question (LRU or
  otherwise) is a new value rather than a change to the interface.
- Three exception types become a two-constructor match that no strategy has to
  lie about.
- The `MVar` leaves the public surface.

## What this does not buy

Nothing in this repo will instantiate `m` at anything but `IO`. The tasty backend
is the only backend and `RegistrySpec` genuinely wants a filesystem. The gain is
the guarantee and the room, not a capability anyone exercises today.

## Deliberately not done

- **A configurable/LRU cache.** `fileRegistry` keeps today's unbounded memo. At
  30k cards that wants a bound, but choosing an eviction policy without the 30k
  pool to measure against would be guessing.
- **A `mapRegistry`, an eager one, a network one.** They fit the type; shipping
  them would be writing a caller's code speculatively. The type is the offer.
- **A test that runs a spec purely.** Parametricity already establishes it — see
  §E2.
- **Making `RegistrySpec` polymorphic.** It tests the file strategy; `IO` is correct.
- **Removing `MissingRoot`.** Still thrown when constructing a file registry.

## Risks

- **The sweep is large** — 2697 call sites across 30 modules. Mechanical, but the
  same class of change as the pawl:spec conversion, where a rename silently
  invalidated seven comments. The plan budgets a comment sweep.
- **`fileRegistry` being pure to construct means a bad root is not diagnosed at
  construction.** Today `Registry.new` checks the directory so a mistyped
  `--cards-dir` fails once rather than per card (#167). `cachedFileRegistry`
  keeps that check; `fileRegistry` cannot. Callers wanting the early check use
  the cached one or validate the root themselves.

## Does the rules core case on an effect's identity?

No. Nothing here touches the engine; the change is confined to the registry
sublibrary, the test suite, and the benchmark's deck loaders.
