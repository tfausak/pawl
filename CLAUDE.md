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

- **Status: M0–M4h are complete** — the closed-half milestones M0–M3g, the
  M3.5 cards-as-data interstitial, and the whole of M4 (M4a–M4g plus the
  M4h fast-follow). **M4.5 (phased) is in progress: P1–P3b closed Cluster 1**
  (every sublayer of the layer system has a producer), **P4 opened Cluster 2**
  (the event-history substrate) **and discharged the `P4 → {P6, P7}` edge**,
  **and P5 closed Cluster 2's second phase — the monadic replacement path and
  CR 616 — leaving P6 and P7 unblocked. P6 (conditional & event durations) is
  next**; P8/P9 still float. The umbrella spec is
  `docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`.
- The **milestone completion log** — one distilled entry per milestone with
  its gate card, the decision it proved, and the opcodes/types it added —
  lives in `docs/progress.md`. It records what each milestone *established*,
  not what is left to do; outstanding work is in GitHub Issues. The
  forward path (M0–M7 and the letter/phase splits) is `docs/design.md` §3;
  each milestone's authoritative detail is its spec and plan under
  `docs/superpowers/{specs,plans}/`. **Maintain the status bullet above by
  replacing it, never appending** — milestone history goes in
  `progress.md`, not here.
- The **milestone workflow** — the session-per-phase loop, model tiering,
  and context discipline — is `docs/workflow.md`. Follow it for all
  milestone work.
- **Keywords are closed half, and casing on one is not a violation.** Rule 702 is
  the rulebook; `case keyword of Flying -> …` is the same kind of act as casing on
  `Phase`. The invariant forbids casing on an *effect's identity* — a keyword is
  not an effect. See §1 of the M2a spec before "fixing" this.
- **Outstanding work is tracked in GitHub Issues** (`tfausak/pawl`). List with
  `gh issue list`; create with `gh issue create`; close with `gh issue close`.
  Milestones are the unlanded phases (`M4.5 P6`…`P11`, `M5`–`M7`); the useful
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
  comment dies in the same commit that closes the issue.

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

## Executing a plan

Plans live in `docs/superpowers/plans/`. Work tasks **strictly in order**; each is
one small complete commit on `main`.

- **TDD is not optional:** write each failing test and actually run it to watch it
  fail before implementing. Tick each `- [ ]` as you finish that step.
- **Progress check:** `grep -c -- '- \[ \] \*\*Step' <plan>` must reach `0`. Use
  *that* grep, not `grep -c -- '- \[ \]'` — the plan template's line 3 quotes the
  checkbox syntax in prose, so the naive grep can never reach 0, and asking it to
  is unsatisfiable without editing the plan.
- **Never** edit the plan, weaken an assertion, or delete a test to make a check
  pass. If the plan looks wrong, **stop and say so** — it has been wrong before.
  A test failing against correct code is a plan bug: fix the plan's test, not the
  engine.
- A milestone's **exit criterion** may legitimately retire a property (M1b kills
  M0's "no life changes"). That is the milestone landing, not a regression — the
  plan says so explicitly where it applies.
- The two invariants outrank the plan: the engine never cases on a card's
  identity (only classifications), and never makes a player's choice. Eliding a
  prompt is legitimate only for indistinguishable options, and every elision
  carries a documented expiry naming the milestone that kills it. Where the rules
  leave nothing to ask, don't prompt.

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
  `NamedFieldPuns` is permitted where it improves clarity on record-heavy code;
  it does not relax the non-punning rule for *constructor* names below.
- **No explicit export lists** (`module Pawl.Foo where`). The cabal file already
  silences `-Wmissing-export-lists`.
- **One type per module** under `Pawl.Type.<TypeName>` (type + instances only);
  cross-type logic lives in other `Pawl.*` modules. A module never imports its
  parents; a sibling `Pawl.Type.*` import is fine. Only `GADTs` and `RankNTypes`
  are permitted (the suspension core), plus `NamedFieldPuns` per the extensions
  note above; nothing else.
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
