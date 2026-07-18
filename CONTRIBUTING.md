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

There is no test suite yet. When there is, `cabal test` will run it.

## Checks: formatting and linting

`hooky` runs every check configured in `.hooky.kdl` — `cabal check`, `cabal-gild`
(cabal-file formatting), `hlint`, `ormolu` (Haskell formatting), and a few
file-hygiene rules (trailing whitespace, final newline, merge markers).

```sh
hooky install   # install hooky as your git pre-commit hook (do this once)
hooky fix       # run all checks, autofixing what can be fixed
hooky run       # run all checks without fixing — what the commit hook enforces
```

Run the underlying tools directly if you prefer (`ormolu --mode inplace`,
`hlint`, `cabal-gild --mode format`). Apply HLint's suggestions rather than
suppressing them, unless you can defend the exception in review.

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
  reasonable alternative, and be ready to justify it. `NamedFieldPuns` is the one
  exception, permitted where it makes record-heavy construction/pattern matching
  clearer — it does not relax the non-punning rule for *constructor* names.
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

## Commits and versioning

- The version in `pawl.cabal` is date-based: `0.YEAR.MONTH.DAY`.
- Keep `CHANGELOG.md` current.
- pawl is licensed under 0BSD.
