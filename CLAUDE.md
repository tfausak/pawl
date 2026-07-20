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

- **M0–M3g, M3.5, M4a, M4b, M4c, and M4d are complete** — a zero-card game, then casting, combat, the
  keyword seam, first/double strike, deathtouch/trample, and the M3 effect-DSL gate
  cards through the payoff pair (Mindslaver's CR 723 control + Panglacial Wurm's
  cast-during-search re-entrancy), then **M3.5** (cards as data files: a hand-rolled
  JSON codec with the honesty round-trip, card data relocated out of the engine
  library into the test suite's `Pawl.Cards`, and `data/cards/*.json` as the source
  of truth — both the benchmark and the test suite load the cards by `IO`; the
  interim TH shim's named expiry was cashed before M4, so no `TemplateHaskell` or
  `Lift` remains in the library), then **M4a** (the numeric tower's `X` — Blaze —
  on a unified `Object.bindings :: Map SlotName Binding` environment that replaced
  the parallel `targets`/`chosenSubtypes` maps; X is chosen at cast via `ChooseX`,
  stored under the reserved `Binding.variableX` slot, and re-read at resolution by
  `Quantity.evaluate`), then **M4b** (the targeted zone-change verbs —
  `Destroy`/`MoveToZone`/`Draw`/`Mill`/`Discard` plus the `Indestructible`
  keyword — each executed only by `Resolve.applyEffect` through M3f's
  `Event.changeZone` funnel, gated by Murder vs. Darksteel Myr proving
  destroy ≠ move-to-graveyard), then **M4c** (tokens — the first card-less object:
  `Source.OfToken Card` read through the single `Game.cardOf` chokepoint,
  `Effect.Create Quantity card` minting via a new `Event.createToken`, and the
  CR 704.5d cease-to-exist SBA, gated by Dragon Fodder; `Effect`/`ActivatedAbility`/
  `TriggeredAbility` were made parametric over the card type — knot tied in `Card` —
  to embed a token's characteristics without a module cycle), then **M4d** (the two
  replacement-shield shapes: damage **prevention** — a cancel hooked into the head
  of the damage funnel via `DamageKind`/`GameState.preventions`/`Event.applyPreventions`,
  gated by Fog — and **regeneration** — a one-shot `GameState.regenerationShields`
  count installed by `Effect.RegenerateSelf` and consumed by the unified
  `Event.destroy` funnel that every destruction now flows through, with the
  creature-death SBA split into `zeroToughness` (704.5f) and `destroyedBySba`
  (704.5g/h), gated by Drudge Skeletons; `Event.destroy` edits combat through the
  type module `Pawl.Type.Combat` to avoid the `Pawl.Combat`→`Sba`→`Event` cycle;
  CR 701.19c "can't be regenerated" stays deferred to Wrath of God). **M4e
  (counter target spell, per the design.md §3 M4 table) is next.** The **milestone completion log** — one distilled entry per milestone with
  its gate card, the decision it proved, the opcodes/types it added, and every
  elision and its named expiry — lives in `docs/progress.md`. The forward path
  (M0–M7 and the M3a–M3g split) is in `docs/design.md` §3; each milestone's
  authoritative detail is its spec and plan under `docs/superpowers/{specs,plans}/`.
- **Keywords are closed half, and casing on one is not a violation.** Rule 702 is
  the rulebook; `case keyword of Flying -> …` is the same kind of act as casing on
  `Phase`. The invariant forbids casing on an *effect's identity* — a keyword is
  not an effect. See §1 of the M2a spec before "fixing" this.
- TODOs are tracked **ephemerally** with **git-bug** (installed in the Nix user
  profile, not the flake; data lives in `refs/bugs/*`). List with `git-bug bug`;
  create with `git-bug bug new -t "…" -m "…"`; close with
  `git-bug bug status close <id>`. This is a temporary pre-flight tool, to be
  replaced by GitHub Issues once the project is off the ground.

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
