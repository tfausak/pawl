# Contributing to pawl

## Development environment

pawl pins its entire toolchain with a [Nix](https://nixos.org) flake, so everyone
builds with the same GHC, Cabal, formatter, and linters. Don't install these
globally — use the flake.

- With [direnv](https://direnv.net): `direnv allow`. The `.envrc` (`use flake`)
  loads the shell automatically whenever you enter the directory.
- Without direnv: `nix develop`.

Either way you get GHC 9.14.1, `cabal`, `ormolu`, `hlint`, `cabal-gild`,
`hooky`, `gh`, and the rest.

## Building

```sh
cabal build   # compile the library
cabal repl    # load it into GHCi
```

The build must be **warning-clean**: pawl compiles with `-Weverything -Werror`
minus a short, explicit allow-list in `pawl.cabal`. A warning is a failure, not
a suggestion.

```sh
cabal test    # the tasty suite
cabal bench   # the tasty-bench benchmarks
```

Build `all` when you touch anything the suites use — they break separately from
the library:

```sh
cabal build all --enable-tests --enable-benchmarks
```

## Checks: formatting and linting

`hooky` runs every check configured in `.hooky.kdl` — `cabal check`, `cabal-gild`
(cabal-file formatting), `hlint`, `ormolu` (Haskell formatting),
`script/format-json.sh` (the card corpus), and a few file-hygiene rules
(trailing whitespace, final newline, merge markers).

```sh
hooky install   # install hooky as your git pre-commit hook (do this once)
hooky fix       # run all checks, autofixing what can be fixed
hooky run       # run all checks without fixing — what the commit hook enforces
```

Run the underlying tools directly if you prefer (`ormolu --mode inplace`,
`hlint`, `cabal-gild --mode format`, `script/format-json.sh fix data/cards/*.json`).
Apply HLint's suggestions rather than suppressing them, unless you can defend the
exception in review.

Card JSON is canonically `jq --sort-keys` output: two-space indent, object keys
sorted, one trailing newline. Don't hand-tune the layout — the check will undo
it.

## Adding a module

Modules live under `source/library/` and are namespaced under `Pawl`. The
`exposed-modules` field is generated from a `-- cabal-gild: discover` directive
in `pawl.cabal` — **add the file, then let `cabal-gild` update the field**
(`hooky fix` does this). Don't hand-edit `exposed-modules`.

## Code style

Layout is not up for debate — Ormolu decides it. Beyond layout, pawl follows a
deliberate set of conventions. Prefer **clarity over cleverness**: code that is
easy to read, debug, and modify. The load-bearing rules:

- **Stay in Haskell 2010.** Avoid language extensions; a staggering pile of them
  turns the language into many sub-languages. Reach for one only when there's no
  reasonable alternative, and be ready to justify it.
- **Prefer explicit over point-free.** `case` over clever combinators, `do`
  notation over bare `>>=`, one equation with a `case` over multiple
  pattern-matching clauses.
- **No partial functions.** No `head`, `undefined`, `error`, or non-exhaustive
  matches — writing them or using them. Model failure with `Maybe`/`Either`.
- **`newtype` liberally, with smart constructors.** Wrap primitives
  (`newtype Name = Name Text`). Guard invariants behind an `UnsafeX` constructor
  plus a validating `textToX`. Unwrap with a descriptive `xToText`, never
  `unwrapX`. (Records are the exception — export their constructors and fields.)
- **Qualified imports**, aliased to the last module component
  (`import qualified Data.List as List`); operators imported unqualified. All
  imports in one group, no first-party/third-party split. A module `A.B.C` must
  not import `A.B` or `A`.
- **Prefer `Text` over `String`**, arbitrary-precision numbers
  (`Integer`/`Natural`/`Rational`) over fixed-width unless a wire or database
  boundary demands otherwise, and custom sum types over `Bool` (avoid boolean
  blindness).
- **Derive at least `Eq` and `Show`** on the types you define.
- **Short exported names.** Uniqueness only has to hold within a module; lean on
  module qualification at the call site instead of prefixing every identifier.

## Workflow

`main` is protected: it takes changes only through a pull request, and only the
repository owner merges them.

1. **File or pick an issue.** It is the spec for the work.
2. **Branch from current `main`**, named `<issue>-<slug>` — say,
   `29-combat-damage-departed-blockers`.
3. **Work, with tests.** Commit as often as you like; merges are squashed, so
   a branch's internal history is working state, not a record.
4. **Open a pull request.** Give the reviewer what they need: what changed and
   why, a link to the issue, the rules citations behind it, how you verified
   it, and anything you deliberately left out.
5. **Wait for CI.** `Test` is the only check that blocks a merge. The others —
   `Ormolu`, `HLint`, `Gild`, `Cabal` — are deliberately non-blocking: `hooky`
   already runs them locally, and a contributor's PR won't be bounced over
   formatting. It will be merged and tidied.

**One pull request per logical chunk of work.** A large issue may span several,
each independently mergeable and each leaving `main` green; only the last one
closes the issue.

`docs/workflow.md` has the longer version, including how work is picked.

## Commits and versioning

- The version in `pawl.cabal` is date-based: `0.YEAR.MONTH.DAY`.
- Keep `CHANGELOG.md` current.
- pawl is licensed under 0BSD.
