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

Keywords are the exception that proves the rule. Rule 702 is part of the
rulebook, so `case keyword of Flying -> …` is the same kind of act as casing on
`Phase` and is not a violation; the invariant forbids casing on an *effect's*
identity.

**The second invariant: the engine never makes a player's choice.** Eliding a
prompt is legitimate only for indistinguishable options, and every elision
carries an issue. Where the rules leave nothing to ask, don't prompt.

Design notes live in `docs/`:

| Question | Where |
|---|---|
| What's next | `gh issue list` |
| Architecture rationale | `design.md`, the relevant § only |
| Rules ground truth | `rules.txt`, grepped by rule number — never memory |
| Prior-art evidence | `prior-art-lessons.md`, cited § only |
| What the milestone era established | `progress.md` (frozen), newest first |
| A landed unit's authoritative detail | its spec, then its plan, under `docs/superpowers/` |
Read these by section, on demand. Reading a whole doc "for context"
front-loads tens of thousands of tokens before the first question.

## Workflow

`CONTRIBUTING.md` has the loop — issue, branch, TDD, draft PR — and it applies
to agents as written. What it doesn't say:

- **Self-review the branch before opening the PR**, and fix the findings on the
  branch. At minimum: re-check every CR citation against `docs/rules.txt`, and
  re-read every comment the change touched for prose the rewrite made wrong —
  those two reliably catch real defects here. Scale the effort to the diff. A
  mechanical refactor does not need a fleet of subagents; a subtle rules change
  does. `/code-review` is one way to run it, but its recipe is fixed and written
  for a PR that does not exist yet at this point.
- **The PR body carries the case for merging**, since the owner reads every PR
  that lands and your work terminates at opening one likely to be merged. Give:
  what changed and why, with `Closes #N`; the CR citations behind it, each
  checked against `rules.txt`; the design calls made and the alternatives
  rejected; how it was verified (build warning-clean, `hooky run` clean, suite
  count before → after, and the proving test); whether the diff makes the rules
  core case on an effect's *identity* — an explicit "no" is cheap; and what was
  deferred.
- **Mark the PR ready for review, then report it and stop.** Open it as a draft,
  but flip it once the self-review's findings are pushed and the suite is green —
  that is what "ready for review" means, and a finished PR left as a draft is
  waiting on nobody. Don't wait for CI. Don't start the next unit either: one
  unit at a time per checkout, since two branches would contend for `HEAD`.
  **The drain loop is the exception.** A session running the drain goal sets the
  PR to auto-merge, waits for it to land, and then picks up the next issue —
  that loop is the one thing that merges its own work, and it does so only for
  PRs it opened. It runs in `.claude/worktrees/drain`, so it never contends with
  an interactive session for `HEAD`.
- **Most of what's left is card-driven** — it fires when a card demands it, so
  the backlog is a menu rather than a queue. Working one means **finding the
  real card and adding it to `data/cards/`**; that is expected, not a side
  quest, and "no producer in the pool" is a description of the work rather than
  a reason to skip. What is forbidden is building a capability no card
  exercises. Per `design.md` §4, an effect is not done until a card exercises it
  in a gameplay-level test: the card is the proof, not the deliverable. The one
  good reason to stop is a *rules* reason — the card turns out not to exercise
  the thing after all. The useful issue labels are `elision`, `gap`,
  `rules-correctness`, `bug`, and the expiry triggers `expires:milestone` and
  `expires:card-driven`. Priority labels (`priority-high` and friends) are the
  owner's; prefer high-priority issues when picking, and never set one yourself.
- **File the issue, cite it inline.** When you elide something, open an issue
  carrying the status, rationale and expiry trigger, then leave a comment at the
  code site stating only what is *not* implemented, plus `(#N)`. Never write the
  expiry into the comment — an in-code expiry is a promise nothing checks, and
  it drifted at a 23% rate before the tracker existed. That comment dies in the
  commit that closes the issue. A comment citing the test that *proves* a
  behavior is a different genre and outlives the issue.
- A spec or plan is **optional, not ceremony** — write one when the unit
  warrants it and commit it in the same PR. If you are following a plan, work
  its tasks strictly in order, and **never** edit the plan, weaken an assertion,
  or delete a test to make a check pass. If the plan looks wrong, **stop and say
  so** — it has been wrong before.

## Environment and commands

The toolchain comes from the Nix flake — GHC 9.14.1, already on `PATH` in the
dev shell (`nix develop` or direnv).

- `cabal build all` — compile. The suites break separately from the library, so
  build `all`, not just the library. Never poke at paths inside `dist-newstyle`.
- `cabal test` — the `tasty` suite. `cabal bench` and `cabal repl` as usual.
- **One build at a time.** `jobs: $ncpus` already saturates the machine, so a
  second concurrent build buys nothing and can lose: two of them racing on the
  same `dist-newstyle` have left it broken mid-write. If you dispatch subagents,
  tell them not to build or test — you are already doing it for them. This is
  about corruption, not politeness, so it is per-checkout: the drain worktree has
  its own `dist-newstyle` and may build while this one does. Its
  `cabal.project.local` sets `jobs: 4` rather than `$ncpus` so the two together
  leave the machine usable.
- `hooky fix` then `hooky run` — format and lint. Acts on **staged** files only:
  `git add` first, or it reports "hooks skipped" and checks nothing. `hooky fix`
  reformats, so stage again before `hooky run`.

## Before you consider a change done

1. `cabal build all` is warning-free.
2. `hooky fix` applied, `hooky run` passes.
3. Every rules claim was checked against `docs/rules.txt`. **Never trust
   recalled Magic rules** — they go stale. Two spec bugs came from exactly this:
   damage assignment order was *removed from the game* (the glossary lists it
   "Obsolete"), and CR 733 is about human error at a table, not engine
   validation. Cite the rule number in the code comment so the next reader can
   check your work.

## Code conventions

`docs/style-guide.md` is the style guide; it is not repeated here. The
project-specific rules it doesn't cover:

- **No API stability obligations.** The project has no consumers. Rename
  functions and modules, split or merge them, change signatures freely — never
  add deprecation shims or compat re-exports.
- **One type per module** under `Pawl.Types.<TypeName>`, holding the type and its
  instances only; cross-type logic lives in other `Pawl.*` modules.
- **Language extensions come from the allowlist in `.hlint.yaml`**, which is the
  list — this file does not restate it. Using one the allowlist doesn't name
  means adding it there in the same change, with the reason it earned a place.
- **Constructors take a `Mk` prefix and never pun the type name** —
  `newtype Name = MkName Text`, `data Foo = MkFoo {…}`. A type whose smart
  constructor maintains an invariant uses `UnsafeX` instead, to mark that the
  bare constructor sidesteps it. Unwrap through an `unwrap` field.
- **Short names, disambiguated by module** — `Pawl.Types.Mana.Mana`, imported as
  `Mana`, rather than long prefixes.
- **No unchecked numeric conversions.** `fromIntegral`, `fromInteger`,
  `realToFrac` and `toEnum` are banned outright by `.hlint.yaml`, because each
  silently produces a wrong answer instead of failing. Convert through
  `Pawl.Extra.Int`, `Pawl.Extra.Integer` or `Pawl.Extra.Natural` — one module
  per source type, each function named for its target and for what it does at
  the boundary (`Maybe` in the type, or `Saturating` in the name). Add the
  missing conversion there rather than reaching around the ban.
- **A per-type codec module exposes `toJson`/`fromJson`.** `Pawl.Codec.Common`
  is the codec's base module — the tagged-object convention, the
  element-generic `encode*`/`decode*` combinators, and the `assertJson*` spec
  helpers every per-type module and spec is written in terms of.
  `Pawl.Codec.Json` no longer exists.

## Adding a module

Pick the sublibrary by what the module *is*, not by who calls it. Each is
`source/libraries/<name>/`, declared as a `library <name>` stanza in
`pawl.cabal` whose comment states its scope:

| Sublibrary | Holds |
|---|---|
| `spec` | the abstract testing interface, and nothing else |
| `extra` | quality-of-life helpers for packages pawl doesn't own |
| `decimal` | the arbitrary-precision decimal number |
| `slug` | slugifying text |
| `json` | RFC-compliant JSON types and codecs |
| `exceptions` | the catalog of exceptions pawl throws |
| `types` | type definitions, without behavior |
| `codec` | JSON encoders and decoders for pawl's types |
| `registry` | answering "what card is this name?", plus a file-backed answer |
| `engine` | the rules engine itself |

Every library module belongs to one of those; only the executable, benchmark and
test suite sit outside, under `source/executable/`, `source/benchmark/` and
`source/test-suite/`. A sublibrary may only depend on ones above it in that
table; adding an edge that inverts it means the module is in the wrong place.

The `exposed-modules` field of every stanza is generated by a
`-- cabal-gild: discover` directive — add the file and run `cabal-gild` (via
`hooky fix`); don't hand-edit the field. Adding or deleting a module needs
`cabal-gild pawl.cabal` run directly, since `hooky fix` acts only on staged
files.

### Where the tests go

Two conventions, split by whether the module is in the engine.

**Outside the engine**, the spec sits next to the module it covers
(`Pawl.Json.Value` → `Pawl.Json.ValueSpec`; `Pawl.Codec.Color` →
`Pawl.Codec.ColorSpec`), is an ordinary exposed module of that sublibrary, and
exposes `spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()` built from
`Pawl.Spec` — codec specs need `(Monad m, Monad n)`, since `Common.assertJson`
binds. Wire it into the `spec` function in `Main.hs`. Shipping the specs as
exposed modules is deliberate: `Pawl.Spec` is base-only, so they cost
essentially nothing and stay runnable by any consumer.

**In the engine** (`source/libraries/engine/`), tests go in the subsystem spec
under `source/test-suite/Pawl/` that near-mirrors the module under test
(`Pawl.Engine.Foo` → `Pawl.FooSpec` — the specs keep the shorter name, since the
test suite covers more than the engine). Each exposes `tests :: TestTree` and
heads with a comment listing the modules it covers; `Main.hs` only aggregates
them. A new subsystem gets a new `Pawl.<Area>Spec` wired into `Main.hs`'s
`testTree` and added to the test-suite `other-modules` list. Shared fixtures
live in `Pawl.Support`, imported `qualified ... as S` — the one documented
exception to alias-to-last-component; a group-local helper stays with its group.
