# Test Suite Restructure Design

**Status:** approved (brainstorm), pending spec review
**Date:** 2026-07-17
**Topic:** split the single-file test suite by subsystem; make the property
tests durable

## Motivation

`source/test-suite/Main.hs` has grown to ~2753 lines and ~50 `TestTree` groups,
with shared fixtures, per-group helpers, answerers, and property tests all
interleaved. CLAUDE.md's "keep it a single file for now / don't split into
per-module test files yet" note has outlived its usefulness: the file is now too
large to hold in context at once, and adding a milestone's tests means appending
to an already-unwieldy file.

Two problems, addressed together:

1. **Organization.** No boundaries. A subsystem's coverage, its fixtures, and its
   helpers are scattered through one file with no locality.
2. **Property-test churn.** Three of the ten `QC.testProperty` assertions are
   *existence* checks ("across 100 seeds, at least one game does X") that serve as
   milestone exit criteria. Each milestone bolts on another `any … [1..100]` plus
   a helper, and one of them (`instants happen`) already had to be *added* this
   milestone and another (`casting actually happens`) *adapted*. These are "did
   the random generator exercise the feature yet," not invariants, and they don't
   belong in an ever-growing property list.

## Goals

- Split the suite into per-subsystem files, tracking the library's own module
  structure, with a shared `Support` module for cross-cutting fixtures.
- Keep the *universal* properties (true for all seeds, forever) as properties;
  convert the *existence* properties to targeted deterministic tests that live
  with their subsystem.
- Add one genuinely durable structural property: a lands-only mirror match always
  ends by a player decking.
- Preserve every existing assertion's coverage. No behavior changes to the
  engine. No unrelated refactoring.

## Non-goals

- No changes to `source/library` or `source/benchmark` (beyond nothing).
- No new engine coverage beyond the lands-only-decks property and the three
  existence→deterministic conversions.
- No renaming or rewording of existing test cases except where a group physically
  moves files. Assertions are preserved verbatim.

## Target module layout

All modules are top-level (test-suite-local), under `source/test-suite/`:

```
Main.hs         -- imports each *Spec, builds testTree, runs defaultMain
Support.hs      -- cross-cutting shared fixtures, answerers, assertions
CoreSpec.hs     -- the VM core: Program suspension, Quantity evaluation
CardSpec.hs     -- card data, type-line rules, printings, the dataflow lint
SetupSpec.hs    -- setup, decks, opening hands
TurnSpec.hs     -- turn structure, phase schedule, CR 508.8 skips
GameSpec.hs     -- zones/changeZone, legal actions, object facts, engine steps
ManaSpec.hs     -- mana payment, castability
CastingSpec.hs  -- cast timing, the stack, discard, summoning sickness
CombatSpec.hs   -- attack/block legality, combat damage, keywords
DamageSpec.hs   -- the damage funnel, deathtouch, trample, state-based actions
ResolveSpec.hs  -- targeting, resolution, the CR 608.2b fizzle
ReplaySpec.hs   -- record/replay round-trips
PropertySpec.hs -- universal QuickCheck invariants only
```

### Intended group assignment

The default mapping below is the plan's starting point. When the plan moves a
group it confirms the fit by reading the group; a group may land in a sibling
file if that reads better, but the file list above is fixed.

| File | Groups |
| --- | --- |
| CoreSpec | programTests, quantityTests |
| CardSpec | cardTests, ruleTests, lintTests, m2aCardTests, m2bCardTests, m2cCardTests, basicLandTests |
| SetupSpec | setupTests, greenBlackSetupTests, deckTests |
| TurnSpec | turnTests, turnDataTests, skipTests |
| GameSpec | gameTests, actionTests, objectFactTests, engineTests |
| ManaSpec | manaTests, castabilityTests |
| CastingSpec | castTests, castEngineTests, stackTests, discardTests, sicknessTests |
| CombatSpec | combatLegalityTests, declareTests, combatDamageTests, keywordTests, firstStrikeTests, m2bExitTests, defenderTests, vigilanceTests, hasteTests, evasionTests |
| DamageSpec | damageTests, damageEventTests, deathtouchTests, assignmentLegalityTests, trampleTests, trampleDeathtouchTests, sbaTests, creatureSbaTests, m2cPropertyTests |
| ResolveSpec | targetTests, resolveTests, fizzleTests |
| ReplaySpec | replayTests, combatReplayTests |
| PropertySpec | propertyTests (universal invariants only) + the new lands-only-decks property |

Note: `m2cPropertyTests` is misnamed — it holds deterministic HUnit fixture
tests (a deathtouch coverage case and a documentation-only assertion), not
QuickCheck properties, so it moves to `DamageSpec`, not `PropertySpec`.

## Support module

`Support` is the test suite's prelude: it holds *only* items used by two or more
spec files. Group-local helpers stay in the one spec that uses them.

Shared (into `Support`):

- Players and matchups: `alice`, `bob`, `bothPlayers`, `redRed`, `greenBlack`,
  `matchups`.
- Answerers used across areas: `identityAnswer`, `castAnswer`, `aggressiveAnswer`,
  `playLandAnswer`, `randomAnswer`, plus `runRandomGame`, `shuffleWith`, and the
  `pick`/insertion helpers those need.
- Board builders shared across areas: `addCreature`, `addPiker`, `landsInPlay`,
  `mountainsInPlay`, `handOne`, `combatBoardOf`, `combatBoard`, `pikerInHand`,
  `boltInHand`.
- Shared assertions/queries: `lifeOf`, `creaturesInPlay`, `countByName`,
  `markDamage`, `tappedCount`, `handSize`, `pikerCard`, `isCreatureRecipient`.

Group-local (stay with their spec):

- `tramplingAnswer` (DamageSpec), `discardLastAnswer` (CastingSpec),
  `recordingAnswer`/`askedPlayers` (GameSpec or CastingSpec, per its group),
  `boltAnswer`/`twoBoltState` (ResolveSpec), `boltAtBobsPiker`/`pikerOf`
  (ResolveSpec), `fightWith`/`resolvedCreature` (their sole user's spec), and any
  other helper the migration finds is referenced from exactly one spec.

The plan determines each helper's home by grep: a helper referenced from ≥2 spec
files goes to `Support`; a helper referenced from exactly one goes to that spec.

## Property module and the existence→deterministic conversions

`PropertySpec` keeps only the universal QuickCheck invariants (true for every
seed):

- conservation: 120 objects at end;
- every game terminates with a result;
- at least 120 ids were minted;
- no mana floats at the end;
- life never increases above the starting total;
- **new:** a lands-only mirror match (both decks 60 basic lands) always ends by a
  player decking themselves (`Result.Won`/`Drawn` reached with each library
  empty and no other loss condition having fired) — a durable structural property
  that stays true no matter what cards later exist.

The three existence properties are converted to deterministic tests, each moving
to its subsystem's spec:

| Was (existence property) | Becomes (deterministic test) | Lands in |
| --- | --- | --- |
| `combat happens: some seed changes a life total` | a scripted or fixed-seed game in which combat provably changes a life total | CombatSpec |
| `green-black: some seed sends a creature to the graveyard` | a scripted or fixed-seed green-black game in which a creature provably dies | DamageSpec |
| `instants happen: some seed casts a Bolt` | a scripted or fixed-seed red-red game in which a Bolt provably reaches a graveyard | ResolveSpec |

Each conversion prefers a hand-built/scripted scenario; a single fixed seed
(`runRandomGame redRed 7`, etc.) is acceptable where scripting the full game is
disproportionate, provided the chosen seed is asserted to exhibit the behavior
deterministically. The helpers `someLifeChanged`, `creatureDied`, `boltCast_`
are inlined or reduced to the single-case form; no `any … [1..100]` survives for
these three. Future milestones add a targeted deterministic test for their new
feature, not another existence property.

## Conventions

- Each `*Spec.hs` exposes a single `tests :: TestTree` — a `testGroup` named for
  the subsystem, holding that file's subgroups. `Main` imports each spec
  qualified (alias to last component, e.g. `import qualified CombatSpec`) and
  references `CombatSpec.tests` in `testTree`.
- `Support` is imported qualified under the short alias `S`
  (`import qualified Support as S`), so call sites read `S.alice`, `S.lifeOf`,
  `S.combatBoardOf`. This is a deliberate, documented exception to the
  alias-to-last-component rule, chosen because these names appear on nearly every
  test line; the short alias keeps the qualified-import discipline without the
  noise of `Support.` everywhere.
- Test modules follow the same Haskell 2010 / no-explicit-export-list /
  qualified-import / no-partial-function style as the library.
- The test-suite cabal stanza gains `other-modules:` with a
  `-- cabal-gild: discover source/test-suite` directive so new `*Spec` files are
  auto-registered; `Main.hs` stays `main-is`. The plan's first task verifies
  cabal-gild's discover excludes the `main-is` module; if it does not, the stanza
  hand-lists `other-modules` instead (a fixed ~13-entry list).

## Migration strategy

Incremental, with the full suite green after every commit — never a big-bang
move. Roughly:

1. Add `Support.hs` with the cross-cutting fixtures/answerers/assertions; make
   `Main.hs` import it (`as S`) and drop the now-duplicated definitions. Suite
   green.
2. For each subsystem file, in turn: create `<Area>Spec.hs`, move its groups and
   group-local helpers out of `Main.hs`, expose `tests`, wire `<Area>Spec.tests`
   into `Main`'s `testTree`. Suite green after each file. (~12 small commits.)
3. Convert the three existence properties to deterministic tests in their new
   homes; add the lands-only-decks universal property to `PropertySpec`. Suite
   green.
4. `Main.hs` ends as imports + `testTree` + `defaultMain` only.
5. Update CLAUDE.md: replace the "single file / don't split" guidance with the
   new layout and the rule "shared fixtures → `Support` (aliased `S`),
   group-local helpers stay with their group, a new subsystem gets a new
   `*Spec.hs` exposing `tests`."

## Verification

- After each migration commit: warning-clean
  `cabal build all --enable-tests --enable-benchmarks` and a green `cabal test`.
- Test-count guard: the total case count is preserved across pure moves. The net
  change over the whole restructure is well-defined — the three existence
  properties collapse from 3 QC properties to 3 deterministic cases (count of
  test *cases* as reported by tasty may shift by a small, enumerated amount), and
  the lands-only-decks property adds one. The plan states the expected final
  count and asserts it, so an accidentally-dropped group is caught.
- Final: `cabal clean` then a warning-free build, and a green suite.
