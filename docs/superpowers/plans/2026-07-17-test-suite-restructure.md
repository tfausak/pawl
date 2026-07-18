# Test Suite Restructure Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Split the single-file `source/test-suite/Main.hs` into per-subsystem
spec modules that near-mirror the library, with a shared `Pawl.Support` prelude,
and make the property tests durable (universal invariants only; the three
existence/exit-criterion properties become deterministic tests).

**Architecture:** `Main.hs` becomes a thin aggregator. Each `Pawl.<Area>Spec`
module lives under `source/test-suite/Pawl/`, exposes a single `tests ::
Tasty.TestTree`, and heads with a comment listing the library modules it covers.
`Pawl.Support` holds cross-cutting fixtures/answerers/assertions and is imported
`qualified ... as S`. The migration is incremental — the full suite is green after
every commit — never a big-bang move.

**Tech Stack:** GHC 9.14.1 (Nix dev shell), cabal, tasty (tasty-hunit +
tasty-quickcheck), cabal-gild, hooky.

## Global Constraints

- Haskell 2010; no new language extensions.
- Warning-clean build: `cabal build all --enable-tests --enable-benchmarks` (the
  `pedantic` flag makes warnings errors).
- No engine behavior changes. This is a test-only refactor (`source/library` and
  `source/benchmark` are untouched). Every existing assertion is preserved
  verbatim when its group moves.
- The full `cabal test` suite must be green after **every** task. Baseline is
  **261** tasty tests. Pure-move tasks preserve the count exactly; the final count
  after Task 14 is **262** (the three existence properties convert 3→3, and the
  lands-only-decks property adds 1).
- Qualified imports aliased to the last component. The sole documented exception:
  `import qualified Pawl.Support as S`.
- Each `Pawl.<Area>Spec` exposes `tests :: Tasty.TestTree` and opens with a
  header comment naming the library modules it covers.
- New `Pawl.*` test modules are registered in the test-suite stanza's
  `other-modules` (see Task 1 for discover-vs-hand-list). Run `hooky fix` (which
  runs cabal-gild); never hand-edit a discovered field.
- Before each commit: `git add <explicit paths>`, `hooky fix`, re-`git add`,
  `hooky run`, then commit. If cabal-gild rewrites `pawl.cabal`, `git add
  pawl.cabal` too. Commit messages are plain sentences ending with:
  `Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>`
- Test players are `alice`/`bob` (now in `Pawl.Support`); tests are named by CR
  number where one applies; cards by real name.

---

### Task 1: `Pawl.Support` and the multi-module machinery

**Files:**
- Create: `source/test-suite/Pawl/Support.hs`
- Modify: `pawl.cabal` (test-suite stanza gains `other-modules`)
- Modify: `source/test-suite/Main.hs` (remove the moved definitions; import them
  from `Pawl.Support`)

**Interfaces:**
- Produces: module `Pawl.Support` exporting (no export list) the cross-cutting
  fixtures/answerers/assertions listed below. Later tasks import it
  `qualified ... as S`.

**What moves into `Pawl.Support`** — cut these top-level definitions verbatim from
`Main.hs` and paste them into `Pawl.Support`, together with the transitive closure
of any private helper they reference (the compiler will name any that are still
missing):

- Players / matchups: `alice`, `bob`, `bothPlayers`, `redRed`, `greenBlack`,
  `matchups`.
- Answerers and the random driver: `identityAnswer`, `castAnswer`,
  `aggressiveAnswer`, `playLandAnswer`, `randomAnswer`, `runRandomGame`,
  `shuffleWith`, `pick`, `isCreatureRecipient`.
- Board builders: `addCreature`, `addPiker`, `landsInPlay`, `mountainsInPlay`,
  `handOne`, `combatBoardOf`, `combatBoard`, `pikerInHand`, `boltInHand`.
- Combat drivers (used by TurnSpec's `skipTests`, CombatSpec, and DamageSpec):
  `fightWith`, `runCombat`, `inCombatPhase`.
- Assertions / queries: `lifeOf`, `creaturesInPlay`, `countByName`, `markDamage`,
  `tappedCount`, `handSize`, `pikerCard`.

Group-local helpers (`tramplingAnswer`, `discardLastAnswer`, `recordingAnswer`,
`boltAnswer`, `twoBoltState`, `boltAtBobsPiker`, `pikerOf`, `fightWith`,
`resolvedCreature`, and the various `*Answer`/scenario helpers used by exactly one
group) STAY in `Main.hs` for now; they travel with their group in later tasks.

- [x] **Step 1: Create `Pawl.Support` with the module header and the moved definitions**

Create `source/test-suite/Pawl/Support.hs` beginning with:

```haskell
-- Cross-cutting test fixtures, answerers, and assertions shared by two or more
-- spec modules -- the test suite's prelude. Imported "qualified ... as S"
-- everywhere (the one documented exception to alias-to-last-component; these
-- names appear on nearly every test line). Group-local helpers live with their
-- group, not here.
module Pawl.Support where
```

Then paste the moved definitions (from the "What moves" list above) below it, and
add the imports each needs (the same `qualified` library imports `Main.hs`
already uses — copy the relevant import lines). Build errors from `Pawl.Support`
referencing a name still in `Main.hs` mean that name's definition must also move
here; follow the closure until `Pawl.Support` compiles.

- [x] **Step 2: Register the module in `pawl.cabal` and wire `Main.hs`**

In the `test-suite pawl-test-suite` stanza, directly under `main-is: Main.hs`, add:

```
  other-modules:
    -- cabal-gild: discover source/test-suite
```

Run `cabal-gild pawl.cabal pawl.cabal` (or `hooky fix`) and inspect the generated
`other-modules`. It MUST list `Pawl.Support` and MUST NOT list `Main`. If `Main`
appears (cabal will later error "module ‘Main’ is defined in multiple files" or
"Main listed in other-modules and main-is"), replace the discover directive with
a hand-maintained list and add `Pawl.Support` to it:

```
  other-modules: Pawl.Support
```

Record which form you used in the commit message; later tasks either rely on
discover (run `hooky fix`) or append their module to the hand list.

In `Main.hs`, delete the now-moved definitions and add — near the other imports —
an explicit unqualified import of exactly the moved names (transitional
scaffolding; it shrinks and disappears as groups leave `Main.hs`):

```haskell
import Pawl.Support
  ( addCreature, addPiker, aggressiveAnswer, alice, bob, boltInHand, bothPlayers,
    castAnswer, combatBoard, combatBoardOf, countByName, creaturesInPlay,
    fightWith, greenBlack, handOne, handSize, identityAnswer, inCombatPhase,
    isCreatureRecipient, landsInPlay, lifeOf, markDamage, matchups,
    mountainsInPlay, pick, pikerCard, pikerInHand, playLandAnswer, randomAnswer,
    runCombat, runRandomGame, shuffleWith, tappedCount )
```

(Adjust the list to exactly the names you moved — the build will flag any
mismatch.) `Main.hs`'s test bodies are otherwise untouched.

- [x] **Step 3: Build and run the full suite**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -3`
Expected: clean (warning-free).
Run: `cabal test 2>&1 | grep -E "tests? passed|out of"`
Expected: `All 261 tests passed`.

- [x] **Step 4: Commit**

```bash
git add source/test-suite/Pawl/Support.hs source/test-suite/Main.hs pawl.cabal
hooky fix && git add source/test-suite/Pawl/Support.hs source/test-suite/Main.hs pawl.cabal && hooky run
git commit -m "Extract Pawl.Support and add the multi-module test machinery

Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>"
```

---

### Task 2: `Pawl.CoreSpec`

**Files:**
- Create: `source/test-suite/Pawl/CoreSpec.hs`
- Modify: `source/test-suite/Main.hs` (remove `programTests`, `quantityTests`;
  import `Pawl.CoreSpec`; wire `CoreSpec.tests` into `testTree`)
- Modify: `pawl.cabal` only if `other-modules` is hand-listed.

**Interfaces:**
- Consumes: `Pawl.Support` (as `S`) for any shared helper these groups use.
- Produces: `CoreSpec.tests :: Tasty.TestTree` grouping `programTests` and
  `quantityTests`.

- [x] **Step 1: Create the spec module**

Create `source/test-suite/Pawl/CoreSpec.hs`:

```haskell
-- Covers the VM core: Pawl.Type.Program (the suspension interpreter) and
-- Pawl.Quantity (numeric evaluation).
module Pawl.CoreSpec where
```

Move `programTests` and `quantityTests` (and any group-local helper used only by
them) verbatim from `Main.hs`. Add the `qualified` imports they need (copy the
relevant lines from `Main.hs`; import `Pawl.Support as S` only if a shared helper
is used, replacing the bare name with `S.<name>`). Add the aggregator:

```haskell
tests :: Tasty.TestTree
tests = Tasty.testGroup "Core" [programTests, quantityTests]
```

- [x] **Step 2: Wire into `Main.hs`**

In `Main.hs`: delete `programTests` and `quantityTests`; add
`import qualified Pawl.CoreSpec`; in `testTree`'s list replace the two entries
with `CoreSpec.tests`. If any name in the transitional `Pawl.Support` explicit
import is now unused in `Main.hs`, remove it from that list.

- [x] **Step 3: Register and build**

If `other-modules` is hand-listed, add `Pawl.CoreSpec`. Then:
Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -3` → clean.
Run: `cabal test 2>&1 | grep -E "tests? passed|out of"` → `All 261 tests passed`.

- [x] **Step 4: Commit**

```bash
git add source/test-suite/Pawl/CoreSpec.hs source/test-suite/Main.hs pawl.cabal
hooky fix && git add source/test-suite/Pawl/CoreSpec.hs source/test-suite/Main.hs pawl.cabal && hooky run
git commit -m "Move the Program and Quantity tests into Pawl.CoreSpec

Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>"
```

---

### Task 3: `Pawl.CardSpec`

**Files:**
- Create: `source/test-suite/Pawl/CardSpec.hs`
- Modify: `source/test-suite/Main.hs`, `pawl.cabal` (if hand-listed)

**Interfaces:**
- Produces: `CardSpec.tests` grouping `cardTests`, `lintTests`, `m2aCardTests`,
  `m2bCardTests`, `m2cCardTests`, `basicLandTests`.

- [x] **Step 1: Create the spec module**

Create `source/test-suite/Pawl/CardSpec.hs`:

```haskell
-- Covers Pawl.Card: card data, type-line rules, every printing, and the D4
-- dataflow lint.
module Pawl.CardSpec where
```

Move `cardTests`, `lintTests`, `m2aCardTests`, `m2bCardTests`, `m2cCardTests`,
`basicLandTests` and their group-local helpers verbatim. Add imports (`S.` for
shared helpers). Add:

```haskell
tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Card"
    [cardTests, lintTests, m2aCardTests, m2bCardTests, m2cCardTests, basicLandTests]
```

- [x] **Step 2: Wire into `Main.hs`**

Delete the six groups from `Main.hs`; add `import qualified Pawl.CardSpec`;
replace their `testTree` entries with `CardSpec.tests`. Prune any now-unused
`Pawl.Support` import names.

- [x] **Step 3: Register and build**

Add `Pawl.CardSpec` to `other-modules` if hand-listed. Then build clean and
`cabal test` → `All 261 tests passed`.

- [x] **Step 4: Commit**

```bash
git add source/test-suite/Pawl/CardSpec.hs source/test-suite/Main.hs pawl.cabal
hooky fix && git add source/test-suite/Pawl/CardSpec.hs source/test-suite/Main.hs pawl.cabal && hooky run
git commit -m "Move the card-data and lint tests into Pawl.CardSpec

Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>"
```

---

### Task 4: `Pawl.SetupSpec`

**Files:**
- Create: `source/test-suite/Pawl/SetupSpec.hs`
- Modify: `source/test-suite/Main.hs`, `pawl.cabal` (if hand-listed)

**Interfaces:**
- Produces: `SetupSpec.tests` grouping `setupTests`, `greenBlackSetupTests`,
  `deckTests`.

- [x] **Step 1: Create the spec module**

Create `source/test-suite/Pawl/SetupSpec.hs`:

```haskell
-- Covers Pawl.Setup and Pawl.Type.Deck: setup, deck composition, opening hands.
module Pawl.SetupSpec where
```

Move `setupTests`, `greenBlackSetupTests`, `deckTests` and their group-local
helpers (`setupState`, `greenBlackSetup`, etc.) verbatim. Add imports. Add:

```haskell
tests :: Tasty.TestTree
tests = Tasty.testGroup "Setup" [setupTests, greenBlackSetupTests, deckTests]
```

- [x] **Step 2: Wire into `Main.hs`** — delete the three groups, `import qualified
  Pawl.SetupSpec`, replace their `testTree` entries with `SetupSpec.tests`, prune
  unused `Pawl.Support` imports.

- [x] **Step 3: Register and build** — add to `other-modules` if hand-listed;
  build clean; `cabal test` → `All 261 tests passed`.

- [x] **Step 4: Commit**

```bash
git add source/test-suite/Pawl/SetupSpec.hs source/test-suite/Main.hs pawl.cabal
hooky fix && git add source/test-suite/Pawl/SetupSpec.hs source/test-suite/Main.hs pawl.cabal && hooky run
git commit -m "Move the setup and deck tests into Pawl.SetupSpec

Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>"
```

---

### Task 5: `Pawl.TurnSpec`

**Files:**
- Create: `source/test-suite/Pawl/TurnSpec.hs`
- Modify: `source/test-suite/Main.hs`, `pawl.cabal` (if hand-listed)

**Interfaces:**
- Produces: `TurnSpec.tests` grouping `turnTests`, `turnDataTests`, `skipTests`.

- [x] **Step 1: Create the spec module**

Create `source/test-suite/Pawl/TurnSpec.hs`:

```haskell
-- Covers Pawl.Turn: turn structure, the phase schedule, and the CR 508.8 skips.
module Pawl.TurnSpec where
```

Move `turnTests`, `turnDataTests`, `skipTests` and their group-local skip
fixtures verbatim. Add imports. Add:

```haskell
tests :: Tasty.TestTree
tests = Tasty.testGroup "Turn" [turnTests, turnDataTests, skipTests]
```

Note: `skipTests` uses the shared `S.combatBoardOf`, `S.runCombat`, and
`S.inCombatPhase` (all in `Pawl.Support` from Task 1) — reference them qualified.

- [x] **Step 2: Wire into `Main.hs`** — delete the three groups, `import qualified
  Pawl.TurnSpec`, replace `testTree` entries with `TurnSpec.tests`, prune unused
  imports.

- [x] **Step 3: Register and build** — add to `other-modules` if hand-listed;
  build clean; `cabal test` → `All 261 tests passed`.

- [x] **Step 4: Commit**

```bash
git add source/test-suite/Pawl/TurnSpec.hs source/test-suite/Main.hs pawl.cabal
hooky fix && git add source/test-suite/Pawl/TurnSpec.hs source/test-suite/Main.hs pawl.cabal && hooky run
git commit -m "Move the turn-structure tests into Pawl.TurnSpec

Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>"
```

---

### Task 6: `Pawl.GameSpec`

**Files:**
- Create: `source/test-suite/Pawl/GameSpec.hs`
- Modify: `source/test-suite/Main.hs`, `pawl.cabal` (if hand-listed)

**Interfaces:**
- Produces: `GameSpec.tests` grouping `gameTests`, `actionTests`,
  `objectFactTests`, `engineTests`, `ruleTests`.

- [ ] **Step 1: Create the spec module**

Create `source/test-suite/Pawl/GameSpec.hs`:

```haskell
-- Covers Pawl.Game, Pawl.Engine, and Pawl.Action: zones and changeZone, legal
-- actions, object facts, engine steps, and engine-rule integration (priority
-- rounds, the CR 103.7a draw skip, CR 514.2 discard, CR 704.5b deck-out).
module Pawl.GameSpec where
```

Move `gameTests`, `actionTests`, `objectFactTests`, `engineTests`, `ruleTests`
and their group-local helpers (`oneMountainState`, `recordingAnswer`/`askedPlayers`
if used here, `sbaBase` only if used here, etc.) verbatim. Add imports. Add:

```haskell
tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Game"
    [gameTests, actionTests, objectFactTests, engineTests, ruleTests]
```

- [ ] **Step 2: Wire into `Main.hs`** — delete the five groups, `import qualified
  Pawl.GameSpec`, replace `testTree` entries with `GameSpec.tests`, prune unused
  imports.

- [ ] **Step 3: Register and build** — add to `other-modules` if hand-listed;
  build clean; `cabal test` → `All 261 tests passed`.

- [ ] **Step 4: Commit**

```bash
git add source/test-suite/Pawl/GameSpec.hs source/test-suite/Main.hs pawl.cabal
hooky fix && git add source/test-suite/Pawl/GameSpec.hs source/test-suite/Main.hs pawl.cabal && hooky run
git commit -m "Move the game, action, and engine-rule tests into Pawl.GameSpec

Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>"
```

---

### Task 7: `Pawl.ManaSpec`

**Files:**
- Create: `source/test-suite/Pawl/ManaSpec.hs`
- Modify: `source/test-suite/Main.hs`, `pawl.cabal` (if hand-listed)

**Interfaces:**
- Produces: `ManaSpec.tests` grouping `manaTests`, `castabilityTests`.

- [ ] **Step 1: Create the spec module**

```haskell
-- Covers Pawl.Mana: mana payment and castability.
module Pawl.ManaSpec where
```

Move `manaTests`, `castabilityTests` and their group-local helpers verbatim; add
imports; add:

```haskell
tests :: Tasty.TestTree
tests = Tasty.testGroup "Mana" [manaTests, castabilityTests]
```

- [ ] **Step 2: Wire into `Main.hs`** — delete the two groups, `import qualified
  Pawl.ManaSpec`, replace entries with `ManaSpec.tests`, prune unused imports.

- [ ] **Step 3: Register and build** — add to `other-modules` if hand-listed;
  build clean; `cabal test` → `All 261 tests passed`.

- [ ] **Step 4: Commit**

```bash
git add source/test-suite/Pawl/ManaSpec.hs source/test-suite/Main.hs pawl.cabal
hooky fix && git add source/test-suite/Pawl/ManaSpec.hs source/test-suite/Main.hs pawl.cabal && hooky run
git commit -m "Move the mana and castability tests into Pawl.ManaSpec

Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>"
```

---

### Task 8: `Pawl.CastSpec`

**Files:**
- Create: `source/test-suite/Pawl/CastSpec.hs`
- Modify: `source/test-suite/Main.hs`, `pawl.cabal` (if hand-listed)

**Interfaces:**
- Produces: `CastSpec.tests` grouping `castTests`, `castEngineTests`,
  `stackTests`, `discardTests`, `sicknessTests`.

- [ ] **Step 1: Create the spec module**

```haskell
-- Covers Pawl.Cast and Pawl.Stack: cast timing, the stack, discard, and
-- summoning sickness.
module Pawl.CastSpec where
```

Move `castTests`, `castEngineTests`, `stackTests`, `discardTests`, `sicknessTests`
and their group-local helpers (`castGameState`, `discardLastAnswer`, `lastN`,
`resolvedCreature`, and any sickness/stack fixtures used only here) verbatim; add
imports; add:

```haskell
tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Cast"
    [castTests, castEngineTests, stackTests, discardTests, sicknessTests]
```

- [ ] **Step 2: Wire into `Main.hs`** — delete the five groups, `import qualified
  Pawl.CastSpec`, replace entries with `CastSpec.tests`, prune unused imports.

- [ ] **Step 3: Register and build** — add to `other-modules` if hand-listed;
  build clean; `cabal test` → `All 261 tests passed`.

- [ ] **Step 4: Commit**

```bash
git add source/test-suite/Pawl/CastSpec.hs source/test-suite/Main.hs pawl.cabal
hooky fix && git add source/test-suite/Pawl/CastSpec.hs source/test-suite/Main.hs pawl.cabal && hooky run
git commit -m "Move the casting, stack, and discard tests into Pawl.CastSpec

Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>"
```

---

### Task 9: `Pawl.CombatSpec`

**Files:**
- Create: `source/test-suite/Pawl/CombatSpec.hs`
- Modify: `source/test-suite/Main.hs`, `pawl.cabal` (if hand-listed)

**Interfaces:**
- Produces: `CombatSpec.tests` grouping `combatLegalityTests`, `declareTests`,
  `combatDamageTests`, `keywordTests`, `firstStrikeTests`, `m2bExitTests`,
  `defenderTests`, `vigilanceTests`, `hasteTests`, `evasionTests`.

- [ ] **Step 1: Create the spec module**

```haskell
-- Covers Pawl.Combat: attack/block legality, combat damage, and the combat
-- keywords (flying, reach, defender, vigilance, haste, first/double strike).
module Pawl.CombatSpec where
```

Move the ten groups above and their group-local helpers (`declaredAttackers` and
the keyword fixtures used only here) verbatim; add imports; add:

```haskell
tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Combat"
    [ combatLegalityTests, declareTests, combatDamageTests, keywordTests,
      firstStrikeTests, m2bExitTests, defenderTests, vigilanceTests, hasteTests,
      evasionTests ]
```

The combat drivers `S.fightWith`/`S.runCombat`/`S.inCombatPhase` are in
`Pawl.Support` (Task 1) — reference them qualified. `declaredAttackers` is used
only by the combat groups, so it stays local to this spec.

- [ ] **Step 2: Wire into `Main.hs`** — delete the ten groups, `import qualified
  Pawl.CombatSpec`, replace entries with `CombatSpec.tests`, prune unused imports.

- [ ] **Step 3: Register and build** — add to `other-modules` if hand-listed;
  build clean; `cabal test` → `All 261 tests passed`.

- [ ] **Step 4: Commit**

```bash
git add source/test-suite/Pawl/CombatSpec.hs source/test-suite/Main.hs pawl.cabal
hooky fix && git add source/test-suite/Pawl/CombatSpec.hs source/test-suite/Main.hs pawl.cabal && hooky run
git commit -m "Move the combat and keyword tests into Pawl.CombatSpec

Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>"
```

---

### Task 10: `Pawl.DamageSpec`

**Files:**
- Create: `source/test-suite/Pawl/DamageSpec.hs`
- Modify: `source/test-suite/Main.hs`, `pawl.cabal` (if hand-listed)

**Interfaces:**
- Produces: `DamageSpec.tests` grouping `damageTests`, `damageEventTests`,
  `deathtouchTests`, `assignmentLegalityTests`, `trampleTests`,
  `trampleDeathtouchTests`, `sbaTests`, `creatureSbaTests`, `m2cPropertyTests`.

- [ ] **Step 1: Create the spec module**

```haskell
-- Covers Pawl.Damage and Pawl.Sba: the damage funnel, deathtouch, trample, and
-- state-based actions. (m2cPropertyTests is deterministic fixture coverage, not
-- QuickCheck properties.)
module Pawl.DamageSpec where
```

Move the nine groups above and their group-local helpers (`tramplingAnswer`,
`sbaBase` if used here, deathtouch/trample fixtures, `syntheticDeathtramplerPrinting`,
etc.) verbatim; add imports; add:

```haskell
tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Damage"
    [ damageTests, damageEventTests, deathtouchTests, assignmentLegalityTests,
      trampleTests, trampleDeathtouchTests, sbaTests, creatureSbaTests,
      m2cPropertyTests ]
```

- [ ] **Step 2: Wire into `Main.hs`** — delete the nine groups, `import qualified
  Pawl.DamageSpec`, replace entries with `DamageSpec.tests`, prune unused imports.

- [ ] **Step 3: Register and build** — add to `other-modules` if hand-listed;
  build clean; `cabal test` → `All 261 tests passed`.

- [ ] **Step 4: Commit**

```bash
git add source/test-suite/Pawl/DamageSpec.hs source/test-suite/Main.hs pawl.cabal
hooky fix && git add source/test-suite/Pawl/DamageSpec.hs source/test-suite/Main.hs pawl.cabal && hooky run
git commit -m "Move the damage and state-based-action tests into Pawl.DamageSpec

Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>"
```

---

### Task 11: `Pawl.ResolveSpec`

**Files:**
- Create: `source/test-suite/Pawl/ResolveSpec.hs`
- Modify: `source/test-suite/Main.hs`, `pawl.cabal` (if hand-listed)

**Interfaces:**
- Produces: `ResolveSpec.tests` grouping `targetTests`, `resolveTests`,
  `fizzleTests`.

- [ ] **Step 1: Create the spec module**

```haskell
-- Covers Pawl.Resolve and Pawl.Target: targeting legality, spell resolution, and
-- the CR 608.2b fizzle.
module Pawl.ResolveSpec where
```

Move `targetTests`, `resolveTests`, `fizzleTests` and their group-local helpers
(`boltAnswer`, `twoBoltState`, `boltAtBobsPiker`, `pikerOf`) verbatim; add
imports; add:

```haskell
tests :: Tasty.TestTree
tests = Tasty.testGroup "Resolve" [targetTests, resolveTests, fizzleTests]
```

- [ ] **Step 2: Wire into `Main.hs`** — delete the three groups, `import qualified
  Pawl.ResolveSpec`, replace entries with `ResolveSpec.tests`, prune unused
  imports.

- [ ] **Step 3: Register and build** — add to `other-modules` if hand-listed;
  build clean; `cabal test` → `All 261 tests passed`.

- [ ] **Step 4: Commit**

```bash
git add source/test-suite/Pawl/ResolveSpec.hs source/test-suite/Main.hs pawl.cabal
hooky fix && git add source/test-suite/Pawl/ResolveSpec.hs source/test-suite/Main.hs pawl.cabal && hooky run
git commit -m "Move the targeting and resolution tests into Pawl.ResolveSpec

Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>"
```

---

### Task 12: `Pawl.ReplaySpec`

**Files:**
- Create: `source/test-suite/Pawl/ReplaySpec.hs`
- Modify: `source/test-suite/Main.hs`, `pawl.cabal` (if hand-listed)

**Interfaces:**
- Produces: `ReplaySpec.tests` grouping `replayTests`, `combatReplayTests`.

- [ ] **Step 1: Create the spec module**

```haskell
-- Covers Pawl.Replay: record/replay transcript round-trips.
module Pawl.ReplaySpec where
```

Move `replayTests`, `combatReplayTests` and their group-local helpers verbatim;
add imports; add:

```haskell
tests :: Tasty.TestTree
tests = Tasty.testGroup "Replay" [replayTests, combatReplayTests]
```

- [ ] **Step 2: Wire into `Main.hs`** — delete the two groups, `import qualified
  Pawl.ReplaySpec`, replace entries with `ReplaySpec.tests`, prune unused imports.

- [ ] **Step 3: Register and build** — add to `other-modules` if hand-listed;
  build clean; `cabal test` → `All 261 tests passed`.

- [ ] **Step 4: Commit**

```bash
git add source/test-suite/Pawl/ReplaySpec.hs source/test-suite/Main.hs pawl.cabal
hooky fix && git add source/test-suite/Pawl/ReplaySpec.hs source/test-suite/Main.hs pawl.cabal && hooky run
git commit -m "Move the record/replay tests into Pawl.ReplaySpec

Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>"
```

---

### Task 13: `Pawl.PropertySpec` (move the property group as-is)

**Files:**
- Create: `source/test-suite/Pawl/PropertySpec.hs`
- Modify: `source/test-suite/Main.hs`, `pawl.cabal` (if hand-listed)

**Interfaces:**
- Produces: `PropertySpec.tests` (initially the whole `propertyTests` group,
  unchanged — universal invariants AND the three existence properties; Task 14
  splits them).

- [ ] **Step 1: Create the spec module**

```haskell
-- Covers cross-cutting universal QuickCheck invariants (true for every seed).
-- The three existence/exit-criterion properties are converted to deterministic
-- tests in their subsystem specs by the next task.
module Pawl.PropertySpec where
```

Move `propertyTests` and its helpers (`someLifeChanged`, `creatureDied`,
`boltCast_`, `nextIdOf`, and any other property-only helper) verbatim; add
imports; add:

```haskell
tests :: Tasty.TestTree
tests = Tasty.testGroup "Properties" [propertyTests]
```

- [ ] **Step 2: Wire into `Main.hs`** — delete `propertyTests` and its helpers,
  `import qualified Pawl.PropertySpec`, replace the `testTree` entry with
  `PropertySpec.tests`, prune unused imports.

- [ ] **Step 3: Register and build** — add to `other-modules` if hand-listed;
  build clean; `cabal test` → `All 261 tests passed`.

- [ ] **Step 4: Commit**

```bash
git add source/test-suite/Pawl/PropertySpec.hs source/test-suite/Main.hs pawl.cabal
hooky fix && git add source/test-suite/Pawl/PropertySpec.hs source/test-suite/Main.hs pawl.cabal && hooky run
git commit -m "Move the property suite into Pawl.PropertySpec

Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>"
```

---

### Task 14: Finalize the properties — convert existence checks, add the durable one

**Files:**
- Modify: `source/test-suite/Pawl/CombatSpec.hs` (new deterministic test)
- Modify: `source/test-suite/Pawl/DamageSpec.hs` (new deterministic test)
- Modify: `source/test-suite/Pawl/ResolveSpec.hs` (new deterministic test)
- Modify: `source/test-suite/Pawl/PropertySpec.hs` (drop the three existence
  properties and their helpers; add the lands-only-decks property)
- Modify: `source/test-suite/Pawl/Support.hs` (add the `landsOnly` matchup fixture)

**Interfaces:**
- Consumes: from `Pawl.Support` — `S.combatBoard`, `S.combatBoardOf`,
  `S.runCombat`, `S.fightWith`, `S.aggressiveAnswer`, `S.creaturesInPlay`,
  `S.lifeOf`, `S.runRandomGame`, `S.landsOnly`; the ResolveSpec-local
  `boltAtBobsPiker`; and `Stack.resolveTop`, `Sba.checkStateBasedActions`,
  `Game.zoneMembers`.
- Produces: three named deterministic successor tests; the `landsOnly` matchup;
  the `landsOnlyDecks` property. Final suite count **262**.

- [ ] **Step 1: Add the deterministic combat successor to `Pawl.CombatSpec`**

In `combatDamageTests` (or a small new group `exitCriterionTests` added to
`CombatSpec.tests`), add:

```haskell
      -- The deterministic successor to the retired "combat happens" property: an
      -- unblocked 2/1 attacker reduces the defender's life by its power.
      HU.testCase "combat deals damage to the defending player" $
        let (gs, _, _) = S.combatBoardOf [Card.pikerPrinting] []
            after = S.runCombat S.aggressiveAnswer gs
         in HU.assertEqual "defender took two" (Just 18) (S.lifeOf S.bob after)
```

- [ ] **Step 2: Add the deterministic death successor to `Pawl.DamageSpec`**

Add to an appropriate group (e.g. `creatureSbaTests`):

```haskell
      -- The deterministic successor to the retired green-black "some seed sends a
      -- creature to the graveyard" property: two 2/1 Pikers trade in combat and
      -- both die to the CR 704.5g state-based action.
      HU.testCase "a creature dies in a played-out combat" $
        let (gs, _, _) = S.combatBoard 1 1
            after = Sba.checkStateBasedActions (S.fightWith S.aggressiveAnswer gs)
         in do
              HU.assertEqual "attacker died" 0 (S.creaturesInPlay S.alice after)
              HU.assertEqual "blocker died" 0 (S.creaturesInPlay S.bob after)
```

- [ ] **Step 3: Add the deterministic instant successor to `Pawl.ResolveSpec`**

Add to `resolveTests`:

```haskell
      -- The deterministic successor to the retired "instants happen" property: a
      -- Bolt cast in a game and resolved ends in its owner's graveyard.
      HU.testCase "a cast Bolt reaches its owner's graveyard" $
        let (_, cast, _) = boltAtBobsPiker
            after = Stack.resolveTop cast
         in HU.assertEqual "one card in the graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after))
```

- [ ] **Step 4: Add the `landsOnly` matchup to `Pawl.Support`**

In `Pawl.Support`, next to `redRed`/`greenBlack`:

```haskell
-- A 60-basic-land mirror: no spell can be cast and no creature can attack, so the
-- only loss condition reachable is CR 704.5b deck-out. Used by the durable
-- lands-only-decks property.
landsOnly :: NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck)
landsOnly = Setup.mirror (Deck.MkDeck (Map.singleton Card.mountainPrinting 60)) bothPlayers
```

(Add `Data.Map.Strict`, `Pawl.Type.Deck`, `Pawl.Card`, `Pawl.Setup`,
`Pawl.Type.PlayerId`, `Data.List.NonEmpty` imports if not already present.)

- [ ] **Step 5: Remove the three existence properties, add the durable one**

In `Pawl.PropertySpec`, delete the three `QC.testProperty` entries
`combat happens: some seed changes a life total`,
`green-black: some seed sends a creature to the graveyard`, and
`instants happen: some seed casts a Bolt`, and delete the now-unused helpers
`someLifeChanged`, `creatureDied`, `boltCast_`. In their place add:

```haskell
      -- Durable structural property: with a deck that can only ever deck out (60
      -- basic lands, no spells, no attackers), every seed's game ends AND ends by
      -- a player drawing from an empty library (CR 704.5b) -- never by any other
      -- loss condition. Stays true no matter what cards later exist.
      QC.testProperty "a lands-only mirror always ends by deck-out" $ \s ->
        let final = S.runRandomGame S.landsOnly s
         in QC.property
              ( Maybe.isJust (GameState.result final)
                  && not (Set.null (GameState.drewFromEmpty final))
              )
```

(Ensure `Data.Maybe`, `Data.Set`, and `Pawl.Type.GameState` are imported in
`PropertySpec`.)

- [ ] **Step 6: Build and run the full suite**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -3` → clean.
Run: `cabal test 2>&1 | grep -E "tests? passed|out of"`
Expected: `All 262 tests passed` (261 − 3 existence properties + 3 deterministic
tests + 1 lands-only property).

- [ ] **Step 7: Commit**

```bash
git add source/test-suite/Pawl/CombatSpec.hs source/test-suite/Pawl/DamageSpec.hs source/test-suite/Pawl/ResolveSpec.hs source/test-suite/Pawl/PropertySpec.hs source/test-suite/Pawl/Support.hs
hooky fix && git add source/test-suite/Pawl/CombatSpec.hs source/test-suite/Pawl/DamageSpec.hs source/test-suite/Pawl/ResolveSpec.hs source/test-suite/Pawl/PropertySpec.hs source/test-suite/Pawl/Support.hs && hooky run
git commit -m "Convert the exit-criterion properties to deterministic tests; add the lands-only-decks invariant

Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>"
```

---

### Task 15: Final cleanup and CLAUDE.md

**Files:**
- Modify: `source/test-suite/Main.hs` (verify it is a pure aggregator)
- Modify: `CLAUDE.md` (replace the single-file guidance)

**Interfaces:**
- Consumes: every `Pawl.<Area>Spec.tests`.
- Produces: a `Main.hs` of only imports + `testTree` + `defaultMain`; updated
  project guidance.

- [ ] **Step 1: Verify `Main.hs` is a pure aggregator**

Confirm `Main.hs` now contains only: the module header, `import qualified
Pawl.<Area>Spec` lines (one per spec), a `Test.Tasty`/`Test.Tasty.Ingredients`
import as needed, `testTree`, and `main`. Delete any leftover transitional
`import Pawl.Support (...)` line (nothing in `Main.hs` should reference a Support
helper anymore). If any helper or group definition still remains in `Main.hs`,
it was missed by an earlier task — move it to the spec that uses it and note which.

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -3` → clean.
Run: `cabal test 2>&1 | grep -E "tests? passed|out of"` → `All 262 tests passed`.

- [ ] **Step 2: Update CLAUDE.md**

Replace the two single-file directives. The first is in the "Environment and
commands" section (`cabal test` — the `tasty` suite, "kept as a single file
`source/test-suite/Main.hs`"). Change it to:

```
- `cabal test` — the `tasty` suite (`tasty-hunit` + `tasty-quickcheck`), split by
  subsystem under `source/test-suite/Pawl/`. Each `Pawl.<Area>Spec` near-mirrors a
  library module (`Pawl.Foo` ↔ `Pawl.FooSpec`), exposes `tests :: TestTree`, and
  heads with a comment listing the modules it covers; `Main.hs` only aggregates
  them. Shared fixtures live in `Pawl.Support`, imported `qualified ... as S` (the
  one documented exception to alias-to-last-component); a group-local helper stays
  with its group.
```

The second is in the "Adding a module" section (tests "go in the single
`source/test-suite/Main.hs` (don't split into per-module test files yet)").
Change it to:

```
Tests go in the subsystem spec under `source/test-suite/Pawl/` that near-mirrors
the library module under test (`Pawl.Foo` → `Pawl.FooSpec`); a new subsystem gets
a new `Pawl.<Area>Spec` exposing `tests :: TestTree`, wired into `Main.hs`'s
`testTree`. Shared fixtures go in `Pawl.Support` (aliased `S`); group-local
helpers stay with their group. New `Pawl.*Spec` files are picked up by cabal-gild
— run `hooky fix`.
```

(If Task 1 used a hand-listed `other-modules`, change the last sentence to "add
the module to the test-suite `other-modules` list".)

- [ ] **Step 3: Final clean-build verification**

Run: `cabal clean && cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -c "warning:"`
Expected: `0`.
Run: `cabal test 2>&1 | grep -E "tests? passed|out of"` → `All 262 tests passed`.

- [ ] **Step 4: Commit**

```bash
git add source/test-suite/Main.hs CLAUDE.md
hooky fix && git add source/test-suite/Main.hs CLAUDE.md && hooky run
git commit -m "Finish the test-suite split: Main is a pure aggregator; update CLAUDE.md

Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>"
```
