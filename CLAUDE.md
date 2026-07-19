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

- **M0 is complete** (a full game of 60 Mountains vs. 60 Mountains, replaying
  deterministically). Its spec and plan are kept as reference:
  `docs/superpowers/specs/2026-07-15-m0-core-types-design.md` and
  `docs/superpowers/plans/2026-07-16-m0-engine.md`.
- **M1a is complete** (casting a Goblin Piker: mana, the stack, resolution).
  Spec and plan kept as reference:
  `docs/superpowers/specs/2026-07-16-m1a-casting-design.md` and
  `docs/superpowers/plans/2026-07-16-m1a-casting.md`.
- **M1b is complete** (Pikers attack, block, deal damage simultaneously per CR
  510.2, and die). The design doc's M1 bundled two independent subsystems and was
  split into **M1a** (casting) and **M1b** (combat); see `docs/design.md`.
  Spec and plan kept as reference:
  `docs/superpowers/specs/2026-07-16-m1b-combat-design.md` and
  `docs/superpowers/plans/2026-07-16-m1b-combat.md`.
- **M2a is complete** (the keyword seam plus flying, reach, defender, vigilance
  and haste — blocking/attacking legality through the `keywordsOf` projection).
  Spec and plan kept as reference:
  `docs/superpowers/specs/2026-07-16-m2a-keywords-design.md` and
  `docs/superpowers/plans/2026-07-16-m2a-keywords.md`.
- **M2b is complete** (first strike + double strike and the CR 506.1 conditional
  turn structure: the turn is now data — `GameState.remaining` is the schedule
  `Engine.advance` pops — so the CR 508.8 skip is a drop and the CR 510.4 second
  combat damage step is a splice; `git-bug 5f50eec` is closed). Spec and plan kept
  as reference:
  `docs/superpowers/specs/2026-07-17-m2b-first-strike-design.md` and
  `docs/superpowers/plans/2026-07-17-m2b-first-strike.md`.
- **M2c is complete** (deathtouch + trample. Deathtouch is the first damage-event
  reader: `Damage.applyCombatDamage` is now a change-and-emit funnel recording
  `GameState.damageEvents`, and the CR 704.5h SBA (`Sba.woundedByDeathtouch`)
  destroys a wounded creature the SBA check then drains. Trample restructures
  assignment: `AssignCombatDamage` carries a keyword-agnostic `Map Recipient
  Natural` of lethal thresholds, `Damage.legalAssignment` is the CR 702.19b
  defender-gating implication, and CR 702.2c collapses a deathtouch source's
  threshold to 1 in one line of `Damage.blockerThreshold`. Zero opcodes). Spec and
  plan kept as reference:
  `docs/superpowers/specs/2026-07-17-m2c-deathtouch-trample-design.md` and
  `docs/superpowers/plans/2026-07-17-m2c-deathtouch-trample.md`.
- **M2d is complete** (M2c's black/green creatures are castable: `Swamp`/`Forest`
  basic lands, a `Deck` multiset (`Map Printing Natural`), and setup taking an
  explicit `NonEmpty (PlayerId, Deck)` matchup. The property suite runs over two
  matchups — red-red (unchanged) and green-black (alice green, bob black) — giving
  the 704.5h deathtouch SBA, trample assignment, and their CR 702.2c interaction
  random-game coverage; a deterministic test casts each card through the stack.
  No new rules, zero opcodes. `git-bug 14138aa` is closed). Spec and plan kept as
  reference: `docs/superpowers/specs/2026-07-17-m2d-castable-decks-design.md` and
  `docs/superpowers/plans/2026-07-17-m2d-castable-decks.md`.
- **M3a is complete** (the first opcode — Lightning Bolt as data. A first-order,
  non-recursive `Effect` AST (`DealDamage SlotName Quantity`) referenced by named
  slots (`SlotName`), with `Pawl.Resolve` the *sole* module that may `case` on an
  `Effect` — executor plus `slotsOf`, the read half of the D4 dataflow lint that
  equates every printing's slot reads to its declared `targetSpecs`. `Pawl.Target`
  owns targeting legality (CR 115.4 `AnyTarget`), shared by casting and the CR
  608.2b re-validation. Casting prompts `ChooseTargets`, reject-not-repair, and
  stamps `Object.targets` on the new stack incarnation (reset by `changeZone`, CR
  400.7); `Cast` honors CR 117.1a instant speed. `Resolve.resolveSpell` runs the
  executor through the generalized `Damage.applyDamage` funnel, buries to the
  graveyard (CR 608.2n), and fizzles when every target is illegal (CR 608.2b). The
  priority loop checks state-based actions after each resolution (CR 117.5) and
  bails on a result. `Engine.runMatch`/`runMatchPure` derive the player list from
  the matchup — `git-bug 15de615` is closed — and four Lightning Bolts in
  `redDeck` give instant speed random-game coverage. Spec and plan kept as
  reference: `docs/superpowers/specs/2026-07-17-m3a-effects-design.md` and
  `docs/superpowers/plans/2026-07-17-m3a-effects.md`.
- **M3b is complete** (continuous effects: `Pawl.Projection` runs the CR 613
  single-effect layer fold over a type family — `Timestamp`, `Layer`,
  `Duration`, `Affected`, `Modification`, `ContinuousEffect`, `StaticAbility`,
  `ProjectedCharacteristics` — and `Effect.ModifyTarget` is ONE opcode driving
  both Giant Growth (layer 7c, +3/+3) and Serpent's Gift (layer 6, a deathtouch
  grant); no per-card opcode was added. Humility contributes two static
  abilities (layer 6 strips keywords, layer 7b sets base power/toughness to
  1/1), and CR 514.2 wears until-end-of-turn effects off at cleanup. The
  `DamageEvent.dealtByDeathtouch` deal-time bit (CR 702.2e) retires M2c's
  synthetic deathtrampler fixture in favor of real cards; `CreatureTarget` gives
  Serpent's Gift and Giant Growth a targeting spec; the green deck carries both
  for random-game coverage. Spec and plan kept as reference:
  `docs/superpowers/specs/2026-07-18-m3b-continuous-effects-design.md` and
  `docs/superpowers/plans/2026-07-18-m3b-continuous-effects.md`.
- **M3c is complete** (the CR 613.8 *existence* dependency — the go/no-go for the
  whole continuous-effects approach, a genuine YES. `Pawl.Projection` folds a full
  projected **type line** (`cardTypes`, `subtypes`, `rulesTextActive` on
  `ProjectedCharacteristics`) layer by layer, evaluating each effect's affected-set
  against the *partial* projection, so a layer-4 type change is visible to a
  layer-6 grant. Three layer-4 `Modification`s (`SetLandSubtype`, `AddLandSubtype`,
  `AddCardType`), `Quantity.ManaValue` (CR 202.3), and `Supertype.Legendary` land
  with no new opcode. **The dependency is resolved by source-liveness, not trial
  application** (a plan refinement): `Projection.staticAbilitiesLive` gathers a
  permanent's static abilities only if no *live* `SetLandSubtype` applies to it,
  reading base characteristics so nothing recurses into the projection — Blood Moon
  strips Urborg order-**independently**, verified in both timestamp orders (the
  falsifier: a naive timestamp fold gives a Forest Swamp). Opalescence animates
  every *other* non-Aura enchantment; `setPT` now *establishes* P/T on a set (CR
  613.4b), so Humility becomes a 4/4 creature under it, resolved in both 7b orders.
  Mana taps off projected subtypes (CR 305.6/305.7); `Sba`/`Target`/`Combat` read
  projected creature-ness. Blood Moon, Urborg, and Opalescence are deterministic
  fixtures (no random-game entry, per the white/red-fixture posture). The
  topological CR 613.8b *applies-to* reorder is deferred behind a documenting test
  (`git-bug f90e0c4`). Performance: `Projection.projectAll` projects the whole
  board from one `gather` per state-based-action sweep. Spec and plan kept as
  reference: `docs/superpowers/specs/2026-07-18-m3c-dependency-design.md` and
  `docs/superpowers/plans/2026-07-18-m3c-dependency.md`.
- **M3d is complete** (layer 3 — the rewritable effect AST, gated by Magical Hack;
  the M3 go/no-go for text-changing, a genuine GO. One new layer-3 `Modification`,
  `ChangeSubtypeWord from to` (CR 612, `Layer.Text`, sorts before layer 4 by the
  derived `Ord`), is read at **three** points, all through the projection so every
  consumer cashes out with no special case: (1) **the type line** — `Projection`'s
  own fold rewrites projected `subtypes` before layer 4, so a hacked basic Mountain
  taps `{U}` (CR 305.6); (2) **a source's static abilities** — `gather` rewrites the
  land-type words inside a live permanent's ability `Modification`s via
  `Projection.textChangesAffecting`/`rewriteModification` **before** they fold onto
  others, so hacking Blood Moon `Mountain→Island` makes nonbasic lands Islands
  **order-independently** (the part XMage cannot do — the go/no-go); (3) **a
  resolving spell's one-shot effects** — `Resolve.effectsOf`/`rewriteEffect` rewrite
  the AST at resolution (delegating the inner `Modification` to
  `Projection.rewriteModification`, so neither module touches the other's
  constructors), so a fixture `Landform` (`{U}` "target land becomes a Swamp",
  labeled synthetic crutch, spec §8) hacked `Swamp→Mountain` on the stack resolves
  as Mountain, and a Blood Moon *spell* hacked on the stack loses the change on
  resolution (CR 400.7 new object). The value choice binds at cast: `ChangeText
  SlotName` opcode + `ChooseBasicLandTypes` prompt + `Object.chosenSubtypes` store
  (reset by `changeZone`; serialized as `Response.ChoseBasicLandTypes` for replay),
  keeping `Resolve` pure — an elision justified by indistinguishability, expiry in
  §8. `Duration.Indefinite` (cleanup never drops it), `ToObject` recipient,
  `SpellOrPermanentTarget`/`LandTarget` specs, and `Subtype.Island`/`Plains` land
  with no rules-core casing on an effect's identity. Magical Hack and the fixture
  are blue deterministic fixtures (no random-game entry). Spec and plan kept as
  reference: `docs/superpowers/specs/2026-07-18-m3d-text-changing-design.md` and
  `docs/superpowers/plans/2026-07-18-m3d-text-changing.md`.
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
