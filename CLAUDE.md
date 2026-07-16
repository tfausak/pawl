# CLAUDE.md

Guidance for Claude Code (and other agents) working in this repository.

## What pawl is

A pure-Haskell rules engine for *Magic: The Gathering*, structured as a virtual
machine:

- a **closed half** — the comprehensive rules (turn structure, priority, the
  stack, zones, state-based actions, the layer system, combat). Finite. It can
  genuinely be finished.
- an **open half** — a first-order, non-recursive, statically-analyzable effect
  DSL, loaded as runtime **data** (never as Haskell modules). Grows forever;
  that's fine.

**The invariant that keeps them apart:** the closed half depends on a
*classification* of effects (which layer, is-it-a-mana-ability, does-it-target),
**never** on the *identity* of an effect. The rules core must never
`case effect of DealDamage{} -> …`. Fusing the two halves is the single failure
mode that sinks this project — audit for it.

Design notes live in `_scratch/` (local, not tracked in git):

- `design.md` — architecture decisions and the M0–M7 implementation path.
- `prior-art-lessons.md` — what to copy/avoid from Argentum (MIT), Forge
  (GPLv3, reference only), and XMage.
- `rules.txt` — the comprehensive rules.

Follow the design doc's sequencing: retire architectural risk before adding
cards. M0 is a complete game with **zero** cards; the first real ABI test
(Magical Hack, Humility/Opalescence, Mindslaver) comes long before card #13.

## Current work and tracking

- **M0 is complete** (a full game of 60 Mountains vs. 60 Mountains, replaying
  deterministically). Its spec and plan are kept as reference:
  `docs/superpowers/specs/2026-07-15-m0-core-types-design.md` and
  `docs/superpowers/plans/2026-07-16-m0-engine.md`.
- **Current work is M1a** — casting a creature (mana, the stack, resolution).
  The design doc's M1 bundled two independent subsystems and is now split into
  **M1a** (casting) and **M1b** (combat); see `_scratch/design.md`. The
  **committed spec** is `docs/superpowers/specs/2026-07-16-m1a-casting-design.md`.
  Work through its plan task by task (TDD, one small complete commit per task on
  `main`).
- TODOs are tracked **ephemerally** with **git-bug** (installed in the Nix user
  profile, not the flake; data lives in `refs/bugs/*`). List with `git-bug bug`;
  create with `git-bug bug new -t "…" -m "…"`; close with
  `git-bug bug status close <id>`. This is a temporary pre-flight tool, to be
  replaced by GitHub Issues once the project is off the ground.

## Environment and commands

The toolchain comes from the Nix flake — GHC 9.14.1, already on `PATH` in the
dev shell (`nix develop` or direnv).

- `cabal build` — compile. Must be **warning-clean** (`-Weverything -Werror`,
  minus the allow-list in `pawl.cabal`). A warning is a failure.
- `cabal test` — the `tasty` suite, kept as a single file
  `source/test-suite/Main.hs` for now (`tasty-hunit` + `tasty-quickcheck`).
- `cabal bench` — the `tasty-bench` benchmark, single file
  `source/benchmark/Main.hs`.
- `cabal repl` — GHCi.
- `hooky fix` then `hooky run` — format and lint (ormolu, hlint, cabal-gild,
  cabal check, file hygiene).

## Before you consider a change done

1. `cabal build` is warning-free.
2. `hooky fix` applied, `hooky run` passes.
3. HLint suggestions applied, or the exception justified.

## Code conventions

These are the project's rules and several differ from common Haskell practice —
follow them without being asked. Full rationale in the style section of
`CONTRIBUTING.md`.

- **Haskell 2010, no language extensions** unless there's genuinely no
  alternative. No `LambdaCase`, `OverloadedStrings`, etc. by default.
- **No explicit export lists** (`module Pawl.Foo where`). The cabal file already
  silences `-Wmissing-export-lists`.
- **One type per module** under `Pawl.Type.<TypeName>` (type + instances only);
  cross-type logic lives in other `Pawl.*` modules. A module never imports its
  parents; a sibling `Pawl.Type.*` import is fine. Only `GADTs` and `RankNTypes`
  are permitted (the suspension core); nothing else.
- **Qualified imports**, aliased to the last component (`Data.List` → `List`);
  import operators unqualified. One import group, no first/third-party split.
  `A.B.C` must not import `A.B` or `A`.
- **No partial functions**, written or used. `Maybe`/`Either`, not `head`,
  `undefined`, `error`, or non-exhaustive matches.
- **`newtype` liberally + smart constructors, non-punning.** Constructors take a
  `Mk` prefix: `newtype Name = MkName Text`, `data Foo = MkFoo {…}` — never pun
  the type and constructor names. Invariant-checking types instead use an
  `UnsafeX` constructor with a validating `textToX`; unwrap with a descriptive
  `xToText`, never `unwrapX`. Build records with `do` + record syntax, not
  `<$>`/`<*>`.
- **Prefer explicit:** `case` over point-free; a single equation with a `case`
  over multiple pattern clauses; `do` notation over bare `>>=` (but never
  `do` for pure code); `let` over `where`; `$` over parentheses and `.` over
  chained `$`.
- **`Text` not `String`.** Arbitrary-precision numbers (`Integer`, `Natural`,
  `Rational`) unless a wire/DB boundary forces fixed width.
- **No boolean blindness** — a custom sum type beats a bare `Bool`.
- **Derive at least `Eq` and `Show`.**
- **Short names**, disambiguated by module (`Pawl.Mana.Mana`, imported as
  `Mana`), not by long prefixes. camelCase; no primes (a trailing `_` or number
  is the fallback). Prefer functions to operators; no backtick-infixed functions
  (except Hspec's `shouldBe`). No list comprehensions.

## Adding a module

Put it under `source/library/` namespaced beneath `Pawl` — one type per module as
`Pawl.Type.<Name>`, logic in other `Pawl.*` modules. The `exposed-modules` field
is generated by a `-- cabal-gild: discover` directive — add the file and run
`cabal-gild` (via `hooky fix`); don't hand-edit the field. Tests go in the single
`source/test-suite/Main.hs` (don't split into per-module test files yet).
