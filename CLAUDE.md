# CLAUDE.md

Guidance for Claude Code (and other agents) working in this repository.

## What Pawl is

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

Design notes live in `docs/` (tracked in git):

- `design.md` — architecture decisions and the M0–M7 implementation path.
- `prior-art-lessons.md` — what to copy/avoid from Argentum (MIT), Forge
  (GPLv3, reference only), XMage, the two Haskell engines (mtg-pure and
  MedeaMelana's Magic, both BSD-3 — §10), and the wider field (Magarena,
  ygopro, Arena/MTGO/Duels).
- `rules.txt` — the comprehensive rules.

Follow the design doc's sequencing: retire architectural risk before adding
cards. M0 is a complete game with **zero** cards; the first real ABI test
(Magical Hack, Humility/Opalescence, Mindslaver) comes long before card #13.

## Current work and tracking

- **Status: the closed half is built.** M0–M5.6 have landed, together with the
  M3.5 cards-as-data and M5.5 count/compare interstitials, M4.5's closed-half
  gap census, the mulligan-adjacent closures, and the card-driven **Auras**
  unit. `docs/progress.md` is the frozen record of what each established.
  **Work is now issue-driven, not milestone-driven** — `gh issue list` says
  what is next. M6 (the transpiler) and M7 (interpreters) are ordinary issues,
  #9 and #10.
- **The development workflow is `docs/workflow.md`** — pick an issue, branch,
  TDD, `/code-review`, open a PR, stop. **One PR per logical chunk of work.
  Never commit to `main`, never merge a PR, never push to `main`** — `main`'s
  ruleset requires a pull request, and only the repository owner merges.
- **Keywords are closed half, and casing on one is not a violation.** Rule 702 is
  the rulebook; `case keyword of Flying -> …` is the same kind of act as casing on
  `Phase`. The invariant forbids casing on an *effect's identity* — a keyword is
  not an effect. See §1 of the M2a spec before "fixing" this.
- **Outstanding work is tracked in GitHub Issues** (`tfausak/pawl`). List with
  `gh issue list`; create with `gh issue create`; close with `gh issue close`.
  **GitHub milestones are retired** — all closed as of 2026-07-24. `M6` and
  `M7` are ordinary issues (#9, #10), picked up when they're picked up, and
  a landed milestone's card-driven residue is de-milestoned rather than left
  hanging off a finished phase. The useful
  labels are `elision`, `gap`, `rules-correctness`, `bug`, and the two expiry
  triggers `expires:milestone` and `expires:card-driven` — roughly half of
  pawl's deferrals fire when a *card* demands them and have no scheduled date,
  which is why that axis is a label and not a milestone. git-bug is retired; the
  hash → issue mapping for citations in landed specs and plans is §10 of
  `docs/superpowers/specs/2026-07-21-github-issue-migration-design.md`.
- **File the issue, cite it inline.** When you elide something, open an issue
  carrying the status, rationale and expiry trigger, and leave a comment at the
  code site stating only what is *not* implemented, plus `(#N)`. Never write the
  expiry into the comment: an in-code expiry naming a milestone is a promise
  nothing checks, and it drifted at a 23% rate before the tracker existed. The
  comment dies in the same commit that closes the issue. **That rule is about a
  deferral marker** — a comment stating what is *not yet* implemented. A
  **closure citation** — a comment stating what a test now covers and pointing
  at the proving test, e.g. `(#62)` beside `Pawl.Engine.settleAll`'s comment
  about a creature settling under indefinite control — is a different genre,
  the same as `docs/progress.md` citing a closed issue, and legitimately
  survives the issue closing.

## Environment and commands

The toolchain comes from the Nix flake — GHC 9.14.1, already on `PATH` in the
dev shell (`nix develop` or direnv).

- `cabal build` — compile. Must be **warning-clean** (`-Weverything` minus the
  allow-list in `pawl.cabal`). The `pedantic` cabal flag adds `-Werror` and is
  enabled in `cabal.project.local` (`flags: +pedantic`), so the build fails on
  any warning; if the flag is somehow off, treat any warning as a failure
  yourself. Incremental builds **hide** warnings from unchanged modules: when
  you need a definitive check, `cabal clean` first — never poke at paths inside
  `dist-newstyle`. Suites break separately from the library, so always build
  `all` (`cabal build all --enable-tests --enable-benchmarks`).
- `cabal test` — the `tasty` suite (`tasty-hunit` + `tasty-quickcheck`), split by
  subsystem under `source/test-suite/Pawl/`. Each `Pawl.<Area>Spec` near-mirrors a
  library module (`Pawl.Foo` ↔ `Pawl.FooSpec`), exposes `tests :: TestTree`, and
  heads with a comment listing the modules it covers; `Main.hs` only aggregates
  them. Shared fixtures live in `Pawl.Support`, imported `qualified ... as S` (the
  one documented exception to alias-to-last-component); a group-local helper stays
  with its group.
- `cabal bench` — the `tasty-bench` benchmark, single file
  `source/benchmark/Main.hs`.
- `cabal repl` — GHCi.
- `hooky fix` then `hooky run` — format and lint (ormolu, hlint, cabal-gild,
  cabal check, file hygiene). Acts on **staged** files only: `git add -A` first,
  or it reports "hooks skipped" and checks nothing. `hooky fix` reformats, so
  `git add -A` again before `hooky run`.

## Before you consider a change done

1. `cabal build` is warning-free.
2. `hooky fix` applied, `hooky run` passes.
3. HLint suggestions applied, or the exception justified.
4. Every rules claim was checked against `docs/rules.txt`. **Never trust
   recalled Magic rules** — they go stale. Two M1b spec bugs came from exactly
   this: damage assignment order was *removed from the game* (the glossary lists
   it "Obsolete"), and CR 733 is about human error at a table, not engine
   validation. Cite the rule number in the code comment so the next reader can
   check your work.

## Working a unit

Work happens on a branch and lands as a pull request. `docs/workflow.md` is the
full loop; the load-bearing rules:

- **One PR per logical chunk of work**, usually one issue. A large issue may
  span several PRs — each independently mergeable, each leaving `main` green;
  only the last says `Closes #N`, the others `Part of #N`.
- **Branch from current `main`**, named `<issue>-<slug>`, e.g.
  `29-combat-damage-departed-blockers`.
- **TDD is not optional:** write each failing test and actually run it to watch
  it fail before implementing.
- **Commit granularity inside a branch does not matter** — squash merge
  collapses it. Commit as often as is convenient.
- **Run `/code-review` on the branch before opening the PR** — the invariant
  audit and the rules-correctness pass. Fix findings on the branch.
- **The PR body carries the case for merging.** Only the repository owner
  merges, so the work terminates at opening a PR that is likely to be merged,
  and the body is what makes that case. Say: what changed and why, with
  `Closes #N`; the CR citations behind it, each checked against `rules.txt`;
  the design calls made and the alternatives rejected; how it was verified
  (build warning-clean, `hooky run` clean, suite count before → after, and the
  proving test); whether the diff makes the rules core case on an effect's
  *identity* — an explicit "no" is cheap, and fusing the halves is the single
  named failure mode; and what was deferred, with the issue filed and `(#N)`
  cited at the code site.
- **Run `hooky` before pushing; don't wait for CI afterward.** Only `Test`
  blocks a merge, and that looseness is deliberate (`workflow.md` says why); a
  red `Ormolu` means `hooky` was skipped, which is a bug in the work. Reading
  the results is the reviewer's job — report the PR and stop.
- A spec or plan is **optional, not ceremony** — write one when the unit
  warrants it and commit it in the same PR. If you are following a plan: tasks
  strictly in order, and **never** edit the plan, weaken an assertion, or
  delete a test to make a check pass. If the plan looks wrong, **stop and say
  so** — it has been wrong before.
- The two invariants outrank everything: the engine never cases on a card's
  identity (only classifications), and never makes a player's choice. Eliding a
  prompt is legitimate only for indistinguishable options, and every elision
  carries an issue. Where the rules leave nothing to ask, don't prompt.

## Code conventions

These are the project's rules and several differ from common Haskell practice —
follow them without being asked. Full rationale in the style section of
`CONTRIBUTING.md`.

- **No API stability obligations.** The project has no consumers. Rename
  functions and modules, split or merge them, change signatures freely —
  never add deprecation shims, compat re-exports, or keep an old name "just
  in case."
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
`cabal-gild` (via `hooky fix`); don't hand-edit the field.

Tests go in the subsystem spec under `source/test-suite/Pawl/` that near-mirrors
the library module under test (`Pawl.Foo` → `Pawl.FooSpec`); a new subsystem gets
a new `Pawl.<Area>Spec` exposing `tests :: TestTree`, wired into `Main.hs`'s
`testTree`. Shared fixtures go in `Pawl.Support` (aliased `S`); group-local
helpers stay with their group. New `Pawl.*Spec` files must be added to the
test-suite `other-modules` list.
