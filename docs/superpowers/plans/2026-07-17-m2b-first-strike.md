# M2b First Strike Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Combat damage stops being one simultaneous event — a first striker kills before it can be hit, a double striker hits twice, and a turn on which nobody attacks skips its two dead steps — completing milestone **M2b** from `docs/superpowers/specs/2026-07-17-m2b-first-strike-design.md`.

**Architecture:** The turn becomes **data**. `GameState` keeps `phase` (the current step) and gains `remaining :: Seq Phase` (the steps still scheduled this turn); `Turn.next` is deleted and `Engine.advance` pops the schedule. Conditional turn structure is then a pair of edits to that schedule: CR 508.8 is a **drop** (remove declare blockers + combat damage when nobody attacks), CR 510.4 is a **splice** (add a second combat damage step when a striker is present). Two keywords — `FirstStrike` (702.7) and `DoubleStrike` (702.4) — are read through M2a's `keywordsOf` projection, and a `Combat.struckFirst` snapshot routes each combat damage step to the right wave. **Zero opcodes. No prompt changes.**

**Tech Stack:** GHC 9.14.1, Cabal. Library depends only on GHC boot libraries (`base`, `containers`, `text`, `transformers`) — no new dependencies. Tests use `tasty`/`tasty-hunit`/`tasty-quickcheck`; benchmark uses `tasty-bench`.

## Starting point (already exists — do not recreate)

M2a is complete and its tests pass. The engine, the `Program`/`Prompt` seam, `changeZone`, mana, casting, the stack, combat declaration, simultaneous combat damage, SBAs, the keyword seam (`Pawl.Type.Keyword`, `Card.keywords`, `Game.keywordsOf`/`hasKeyword`), replay, the single-file test suite (`source/test-suite/Main.hs`) and benchmark all exist. This plan **modifies** them.

Read the spec before starting. The invariants it turns on:

1. **Casing on a `Keyword` is NOT a violation of the closed/open invariant.** Rule 702 is the rulebook; a keyword is a numbered rule, not an effect. See M2a spec §1 and the note atop `Pawl.Type.Keyword` before "fixing" a `case keyword of` into a classification.
2. **The closed half asks `Game.keywordsOf`, never `Card.keywords`.** Layer 6 grants abilities at M3; the wave and splice decisions read the projection **live, at the step boundary**, never precomputed at combat start.
3. **The two combat damage steps are two steps.** The second is a spliced step, so it gets its own CR 510.3 priority window and its own SBA check for free from the step machinery. Do not deal both waves inside one handler.
4. **Every rules claim is checked against `docs/rules.txt`.** The load-bearing rules are CR 506.1, 508.8, 500.8, 500.9, 500.11, 510.1–510.5, 702.4, 702.7. Cite the number in the code.

## Global Constraints

Every task's requirements implicitly include all of these:

- **Warning-clean:** library, test suite, and benchmark compile under `-Weverything` minus the allow-list in `pawl.cabal`. A warning is a failure. `-Wunused-matches` is active: prefix genuinely unused binders with `_`. `-Wmonomorphism-restriction` is active: a `let` without a signature that needs one is a failure — annotate it. Check with a **clean** build (`rm -rf dist-newstyle/build/aarch64-osx/ghc-9.14.1/pawl-0.2026.7.16/{b,t,build}`) — incremental builds hide warnings.
- **Boot libraries only** in the library (`base`, `containers`, `text`, `transformers`). No new dependencies.
- **Haskell 2010.** The only permitted extensions are `GADTs` and `RankNTypes`, per-file.
- **Non-punning constructors:** `Mk` prefix on newtypes and single-constructor records. Multi-constructor ADTs are written plainly.
- **One type per module** under `Pawl.Type.<TypeName>` — **type and instances only**. Cross-type logic lives in other `Pawl.*` modules. A module never imports its parents.
- **Derive at least `Eq` and `Show`.** `Combat` derives `Ord`, so everything inside it must too (`Maybe (Set ObjectId)` does).
- **No partial functions.** `Maybe`/`Either`, never `head`/`error`/non-exhaustive matches. Prefer `let` over `where`, `case` over point-free, `$` over parens and `.` over chained `$`.
- **No list comprehensions, no backtick-infixed functions.** `.hlint.yaml` already ignores the hints that contradict the house style; if HLint suggests one anyway, restructure rather than suppress.
- **Imports:** qualified, aliased to the last component; one import group; operators unqualified.
- **Tests:** all in `source/test-suite/Main.hs`, appended to the `testTree` list.
- **Commits:** directly to `main`, one small complete commit per task.
- **Per-task gate:** `cabal build`, then `hooky fix` and `hooky run` must pass before committing. `hooky` acts on **staged** files — `git add -A` first, or it reports "hooks skipped".

**Verification commands:** build `cabal build`; all targets `cabal build all --enable-tests --enable-benchmarks`; test `cabal test`; one group `cabal test --test-options='-p "/Pattern/"'`; bench `cabal bench`; lint `hooky fix` then `hooky run`.

**Do not pipe `cabal test` into `head`** — closing the pipe early wedges the process and looks like a hang. Redirect to a file and grep it. (`git-bug b9164e2` notes the suite can also deadlock intermittently under tasty's default parallelism; if a run hangs with no output, re-run — it is not this milestone's change.)

## File structure

**Modified (library):**

| File | Change |
|---|---|
| `Pawl/Type/GameState.hs` | add `remaining :: Seq Phase` |
| `Pawl/Type/Combat.hs` | add `struckFirst :: Maybe (Set ObjectId)` |
| `Pawl/Type/Keyword.hs` | add `FirstStrike`, `DoubleStrike` (CR order) |
| `Pawl/Type/Subtype.hs` | add `Cat`, `Dinosaur`, `Beast` |
| `Pawl/Turn.hs` | delete `next`; add `laterPhases`, `dropSkippedCombatSteps`, `spliceSecondDamage` |
| `Pawl/Engine.hs` | `advance` pops the schedule; `handoffTurn` refills it; skip + splice wiring |
| `Pawl/Combat.hs` | `skipEmptyCombat`; `emptyCombat` sets `struckFirst` |
| `Pawl/Damage.hs` | `dealCombatDamage :: Game Bool`, wave predicates, `gatherCombatDamage` takes a filter |
| `Pawl/Setup.hs` | `emptyGame` sets `remaining` |
| `Pawl/Card.hs` | `sabretoothTigerPrinting`, `ridgetopRaptorPrinting` |

**Modified (suites):** `source/test-suite/Main.hs`.

**Untouched, deliberately:** `Pawl/Type/Prompt.hs`, `Pawl/Type/Response.hs`, `Pawl/Replay.hs`, and all eight interpreters. M2b is additive across the decision seam, like M2a. If a task appears to need a prompt change, stop — that is a corner the spec's §3 exists to avoid.

## Task ordering rationale

- **Task 1 makes the turn data with no behavior change.** It is the foundation every other task consumes, and it is a pure refactor: all existing tests stay green, and the turn tests that named the deleted `Turn.next` are rewritten to the new model.
- **Task 2 is the first schedule edit and the simplest — a drop.** It fixes `git-bug 5f50eec` (CR 508.8) and proves the mechanism before the harder splice.
- **Task 3 lands the two keywords and the two printings** before anything consumes them, the M2a Task-1 pattern.
- **Task 4 is the hard one — the splice, the `struckFirst` snapshot, the two waves.** Double strike is the falsifier; it needs both the schedule (Task 1) and the keywords (Task 3).
- **Task 5 closes the milestone** — properties, warning-clean, replay.

---

### Task 1: The turn becomes data

`GameState.remaining`, `Turn.laterPhases`, `advance` pops the schedule, `handoffTurn` refills it, `Turn.next` deleted. **No behavior change** — every phase runs in the same order as before.

**Files:**
- Modify: `source/library/Pawl/Type/GameState.hs`, `source/library/Pawl/Turn.hs`, `source/library/Pawl/Engine.hs`, `source/library/Pawl/Setup.hs`, `source/test-suite/Main.hs`

**Interfaces:**
- Consumes: `Turn.allPhases`, `Turn.firstPhase`, `Engine.runStep`, `Engine.runGamePure` (all exist).
- Produces:
  - `GameState` gains `remaining :: Seq Phase`
  - `Turn.laterPhases :: Seq Phase` (the schedule of a fresh turn after its first step)
  - `Engine.advance :: Game ()` (was `Phase -> Game ()`)
  - `Turn.next` is **deleted**

- [x] **Step 1: Write the failing test**

In `source/test-suite/Main.hs`, **replace** the `turnTests` group and **delete** the `turnSequence` helper (the two `where`-bound lines under it); keep `dedupe`:

```haskell
turnTests :: Tasty.TestTree
turnTests =
  Tasty.testGroup
    "Turn"
    [ HU.testCase "firstPhase is the untap step" $
        HU.assertEqual "firstPhase" (Phase.Beginning BeginningStep.Untap) Turn.firstPhase,
      HU.testCase "a turn has twelve steps" $
        HU.assertEqual "twelve" 12 (length Turn.allPhases),
      HU.testCase "firstPhase and laterPhases reconstruct the turn template" $
        HU.assertEqual "reconstruct" (Seq.fromList (drop 1 Turn.allPhases)) Turn.laterPhases,
      HU.testCase "untap and cleanup grant no priority" $
        HU.assertBool "no priority" $
          not (Turn.grantsPriority (Phase.Beginning BeginningStep.Untap))
            && not (Turn.grantsPriority (Phase.Ending EndingStep.Cleanup)),
      QC.testProperty "a turn never revisits a phase" $
        QC.property (length Turn.allPhases == length (dedupe Turn.allPhases))
    ]
```

Add this new group, and add `turnDataTests` to the `testTree` list:

```haskell
turnDataTests :: Tasty.TestTree
turnDataTests =
  Tasty.testGroup
    "TurnData"
    [ HU.testCase "advance pops the schedule head into the current phase" $
        let gs0 = Setup.emptyGame bothPlayers
            gs =
              gs0
                { GameState.phase = Phase.PrecombatMain,
                  GameState.remaining = Seq.fromList [Phase.Combat CombatStep.BeginningOfCombat, Phase.PostcombatMain]
                }
            after = snd (Engine.runGamePure aggressiveAnswer gs Engine.advance)
         in do
              HU.assertEqual "phase" (Phase.Combat CombatStep.BeginningOfCombat) (GameState.phase after)
              HU.assertEqual "remaining" (Seq.fromList [Phase.PostcombatMain]) (GameState.remaining after),
      HU.testCase "advance on an empty schedule hands off the turn" $
        let gs0 = Setup.emptyGame bothPlayers
            gs =
              gs0
                { GameState.phase = Phase.Ending EndingStep.Cleanup,
                  GameState.remaining = Seq.empty,
                  GameState.activePlayer = alice,
                  GameState.turnNumber = 1
                }
            after = snd (Engine.runGamePure aggressiveAnswer gs Engine.advance)
         in do
              HU.assertEqual "new active player" bob (GameState.activePlayer after)
              HU.assertEqual "phase reset" Turn.firstPhase (GameState.phase after)
              HU.assertEqual "schedule refilled" Turn.laterPhases (GameState.remaining after)
              HU.assertEqual "turn incremented" 2 (GameState.turnNumber after),
      HU.testCase "a fresh game starts at untap with the rest of the turn scheduled" $
        let gs = Setup.emptyGame bothPlayers
         in do
              HU.assertEqual "phase" Turn.firstPhase (GameState.phase gs)
              HU.assertEqual "remaining" Turn.laterPhases (GameState.remaining gs)
    ]
```

- [x] **Step 2: Run to verify it fails**

Run: `cabal test 2>&1 > /tmp/t.txt; grep -m3 -E "error|not in scope" /tmp/t.txt`
Expected: FAIL — `GameState.remaining` and `Turn.laterPhases` not in scope.

- [x] **Step 3: Add the field to `GameState`**

In `source/library/Pawl/Type/GameState.hs`, add the field immediately after `phase :: Phase,`:

```haskell
    phase :: Phase,
    -- CR 500. The steps still scheduled this turn, in order; `phase` is the one
    -- in progress. The turn is DATA: CR 508.8 drops steps from this, CR 510.4 and
    -- 500.8/500.9 splice steps and phases into it. `Turn.allPhases` is the
    -- template a new turn refills from (see Engine.handoffTurn).
    remaining :: Seq Phase,
```

`Seq` and `Phase` are already imported by this module.

- [x] **Step 4: Rework `Pawl.Turn`**

`source/library/Pawl/Turn.hs` — add the two imports, delete `next`, add `laterPhases`:

```haskell
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
```

Delete the whole `next` definition (the `next :: Phase -> Maybe Phase` block). Add, after `firstPhase`:

```haskell
-- The steps of a fresh turn AFTER its first (the untap step). A new turn's
-- schedule refills to this; `firstPhase` is its current step. Demoted from the
-- old `next` walk: nothing computes a successor any more -- Engine.advance pops
-- this sequence instead.
laterPhases :: Seq Phase
laterPhases = Seq.fromList (drop 1 allPhases)
```

Leave `allPhases`, `firstPhase`, `grantsPriority` and `isMainPhase` as they are.

- [x] **Step 5: Rework `advance` and `handoffTurn`**

In `source/library/Pawl/Engine.hs`, add the import (keep the block sorted):

```haskell
import qualified Data.Sequence as Seq
```

Replace `handoffTurn` and `advance`:

```haskell
handoffTurn :: Game ()
handoffTurn = State.modify' $ \gs ->
  gs
    { GameState.activePlayer = nextInOrder (GameState.turnOrder gs) (GameState.activePlayer gs),
      GameState.turnNumber = GameState.turnNumber gs + 1,
      GameState.phase = Turn.firstPhase,
      GameState.remaining = Turn.laterPhases
    }

-- Consume the schedule: the next step becomes current. An empty schedule means
-- the turn is over, so hand off. Replaces the old `Turn.next` walk -- the turn is
-- data now, and this is the only thing that reads its order.
advance :: Game ()
advance = do
  gs <- State.get
  case Seq.viewl (GameState.remaining gs) of
    p Seq.:< rest -> State.put gs {GameState.phase = p, GameState.remaining = rest}
    Seq.EmptyL -> handoffTurn
```

In `runStep`, change the final `advance phase` to `advance` (it no longer takes an argument). The line reads:

```haskell
    Monad.unless stillFinished advance
```

- [x] **Step 6: Set `remaining` in `Setup.emptyGame`**

In `source/library/Pawl/Setup.hs`, add the field immediately after `GameState.phase = Turn.firstPhase,`:

```haskell
          GameState.phase = Turn.firstPhase,
          GameState.remaining = Turn.laterPhases,
```

- [x] **Step 7: Set `remaining` on the combat fixture**

In `source/test-suite/Main.hs`, in `combatBoardOf`, add `remaining` to the record update that already sets `activePlayer` and `phase`:

```haskell
   in ( gs2
          { GameState.activePlayer = alice,
            GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
            -- The steps after declare attackers, so a runStep-driven test (Tasks
            -- 2 and 4) can advance through combat. Direct-call tests ignore it.
            GameState.remaining =
              Seq.fromList
                [ Phase.Combat CombatStep.DeclareBlockers,
                  Phase.Combat CombatStep.CombatDamage,
                  Phase.Combat CombatStep.EndOfCombat,
                  Phase.PostcombatMain,
                  Phase.Ending EndingStep.EndStep,
                  Phase.Ending EndingStep.Cleanup
                ]
          },
        ours,
        yours
      )
```

- [x] **Step 8: Build and test**

Run: `cabal build all --enable-tests --enable-benchmarks`, then `cabal test --test-options='-p "/TurnData/"'` and `cabal test --test-options='-p "/Turn/"'`.
Expected: PASS (3 cases, then 5 cases).

Then `cabal test`. Expected: **all pass** — this is a pure refactor. Phases run in the same order, so the property games, the replay, and every combat test are untouched. If any existing test other than `turnTests` needs editing, the refactor is wrong — stop and reread Step 5.

- [x] **Step 9: Lint and commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git commit -m "Make the turn data: a schedule the engine pops"
```

---

### Task 2: CR 508.8 — skip the unattacked combat steps

`git-bug 5f50eec`. When nobody attacks, drop the declare blockers and combat damage steps from the schedule (CR 500.11: proceed as though they didn't exist).

**Files:**
- Modify: `source/library/Pawl/Turn.hs`, `source/library/Pawl/Combat.hs`, `source/library/Pawl/Engine.hs`, `source/test-suite/Main.hs`

**Interfaces:**
- Consumes: Task 1's `GameState.remaining`; `Combat.emptyCombat` shape (`attackers`).
- Produces:
  - `Turn.dropSkippedCombatSteps :: Seq Phase -> Seq Phase`
  - `Combat.skipEmptyCombat :: GameState -> GameState`

- [x] **Step 1: Write the failing test**

Add this group, and add `skipTests` to `testTree`:

```haskell
skipTests :: Tasty.TestTree
skipTests =
  Tasty.testGroup
    "Skip"
    [ HU.testCase "CR 508.8 dropSkippedCombatSteps removes declare blockers and combat damage" $
        let full =
              Seq.fromList
                [ Phase.Combat CombatStep.DeclareBlockers,
                  Phase.Combat CombatStep.CombatDamage,
                  Phase.Combat CombatStep.EndOfCombat,
                  Phase.PostcombatMain
                ]
            expected = Seq.fromList [Phase.Combat CombatStep.EndOfCombat, Phase.PostcombatMain]
         in HU.assertEqual "dropped" expected (Turn.dropSkippedCombatSteps full),
      HU.testCase "CR 508.8 no attacker declared skips to end of combat" $
        -- Nobody has a creature, so no attackers are declared: the declare
        -- blockers and combat damage steps must not run at all.
        let (gs, _, _) = combatBoardOf [] []
            after = snd (Engine.runGamePure aggressiveAnswer gs Engine.runStep)
         in HU.assertEqual "jumped past the two dead steps" (Phase.Combat CombatStep.EndOfCombat) (GameState.phase after),
      HU.testCase "CR 508.8 an attacker keeps the declare blockers step" $
        -- The control: with an attacker, the step after declare attackers is
        -- declare blockers, exactly as before. So the skip is not "always skip".
        let (gs, _, _) = combatBoardOf [Card.pikerPrinting] []
            after = snd (Engine.runGamePure aggressiveAnswer gs Engine.runStep)
         in HU.assertEqual "declare blockers still next" (Phase.Combat CombatStep.DeclareBlockers) (GameState.phase after),
      HU.testCase "CR 508.8 an attacker-less combat changes no life total" $
        -- End to end: run the whole combat region. No attackers means no damage,
        -- and the turn still leaves combat cleanly.
        let (gs, _, _) = combatBoardOf [] []
            after = runCombat aggressiveAnswer gs
         in do
              HU.assertEqual "bob untouched" (Just 20) (lifeOf bob after)
              HU.assertEqual "alice untouched" (Just 20) (lifeOf alice after)
              HU.assertBool "left combat" (not (inCombatPhase (GameState.phase after)))
    ]

-- Run whole steps through the engine while the current phase is in the combat
-- phase, stopping once combat is left or the game ends. Bounded so a bug cannot
-- loop forever.
runCombat :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runCombat answer gs0 =
  let go n g =
        if n <= (0 :: Int) || Maybe.isJust (GameState.result g) || not (inCombatPhase (GameState.phase g))
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 24 gs0

inCombatPhase :: Phase.Phase -> Bool
inCombatPhase p = case p of
  Phase.Combat _ -> True
  _ -> False
```

- [x] **Step 2: Run to verify it fails**

Run: `cabal test --test-options='-p "/Skip/"' 2>&1 > /tmp/t.txt; grep -m3 -E "error|not in scope" /tmp/t.txt`
Expected: FAIL — `Turn.dropSkippedCombatSteps` not in scope.

- [x] **Step 3: Add the drop to `Pawl.Turn`**

In `source/library/Pawl/Turn.hs`, append:

```haskell
-- CR 508.8 / 500.11: drop the declare blockers and combat damage steps from a
-- schedule, so the turn proceeds "as though they didn't exist". There is one
-- combat phase per turn at M2b, so removing every occurrence is unambiguous.
--
-- EXPIRES at M4: with a second combat phase (CR 500.8) this must drop only the
-- current phase's steps, positionally, not every combat step in the schedule.
dropSkippedCombatSteps :: Seq Phase -> Seq Phase
dropSkippedCombatSteps =
  let skipped p =
        p == Phase.Combat CombatStep.DeclareBlockers
          || p == Phase.Combat CombatStep.CombatDamage
   in Seq.filter (\p -> not (skipped p))
```

- [x] **Step 4: Add `skipEmptyCombat` to `Pawl.Combat`**

In `source/library/Pawl/Combat.hs`, add the import (keep the block sorted):

```haskell
import qualified Pawl.Turn as Turn
```

Append:

```haskell
-- CR 508.8: if no creatures were declared as attackers, skip the declare
-- blockers and combat damage steps. Called right after declareAttackers, when
-- the attacker set is final. "Put onto the battlefield attacking" (508.8) has no
-- source at M2b; EXPIRES at M4+ with the effects that create attacking creatures.
skipEmptyCombat :: GameState -> GameState
skipEmptyCombat gs =
  if Map.null (Combat.attackers (GameState.combat gs))
    then gs {GameState.remaining = Turn.dropSkippedCombatSteps (GameState.remaining gs)}
    else gs
```

- [x] **Step 5: Wire it into the declare attackers step**

In `source/library/Pawl/Engine.hs`, in `runTurnBasedActions`, replace the declare attackers case:

```haskell
    Phase.Combat CombatStep.DeclareAttackers -> do
      Combat.declareAttackers active
      -- CR 508.8: with the attacker set now final, drop the two combat steps that
      -- have nothing to do if nobody attacked.
      State.modify' Combat.skipEmptyCombat
```

- [x] **Step 6: Run to verify it passes**

Run: `cabal test --test-options='-p "/Skip/"' 2>&1 > /tmp/t.txt; grep -E "^All|FAIL" /tmp/t.txt`
Expected: PASS (4 cases).

Then `cabal test`. Expected: all pass — nothing else declares an attacker-less combat and then inspects the schedule.

- [x] **Step 7: Lint, commit, close the bug**

```bash
git add -A
hooky fix
git add -A
hooky run
git commit -m "Skip the declare blockers and combat damage steps when nobody attacks (CR 508.8)"
git-bug bug status close 5f50eec
```

---

### Task 3: First strike and double strike — the keywords and the two printings

`FirstStrike` and `DoubleStrike` in CR-number order; Sabretooth Tiger and Ridgetop Raptor; three new subtypes. Nothing consumes them yet.

**Files:**
- Modify: `source/library/Pawl/Type/Keyword.hs`, `source/library/Pawl/Type/Subtype.hs`, `source/library/Pawl/Card.hs`, `source/test-suite/Main.hs`

**Interfaces:**
- Consumes: Task 1 (none directly); `Game.keywordsOf`/`hasKeyword`, `addCreature` (M2a).
- Produces: `Pawl.Card.sabretoothTigerPrinting`, `Pawl.Card.ridgetopRaptorPrinting :: Printing`

Both cards are **verified against Scryfall** (`api.scryfall.com/cards/named?exact=…`, checked 2026-07-17, zero Gatherer rulings). Do not "correct" them from memory:

| Card | Cost | P/T | Type line | Rules text |
|---|---|---|---|---|
| Sabretooth Tiger | `{2}{R}` | 2/1 | Creature — Cat | First strike |
| Ridgetop Raptor | `{3}{R}` | 2/1 | Creature — Dinosaur Beast | Double strike |

- [x] **Step 1: Write the failing test**

Add this group, and add `m2bCardTests` to `testTree`:

```haskell
m2bCardTests :: Tasty.TestTree
m2bCardTests =
  let card = Printing.card
      red = ManaSymbol.OfType (ManaType.Colored Color.Red)
      gs0 = Setup.emptyGame bothPlayers
   in Tasty.testGroup
        "M2bCards"
        [ HU.testCase "Sabretooth Tiger is a {2}{R} 2/1 Cat with first strike" $ do
            HU.assertEqual "name" (Text.pack "Sabretooth Tiger") (Card.Type.name (card Card.sabretoothTigerPrinting))
            HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, red])) (Card.Type.manaCost (card Card.sabretoothTigerPrinting))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (card Card.sabretoothTigerPrinting))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness (card Card.sabretoothTigerPrinting))
            HU.assertEqual "subtypes" (Set.singleton Subtype.Cat) (TypeLine.subtypes (Card.Type.typeLine (card Card.sabretoothTigerPrinting)))
            HU.assertEqual "keyword" (Set.singleton Keyword.FirstStrike) (Card.Type.keywords (card Card.sabretoothTigerPrinting)),
          HU.testCase "Ridgetop Raptor is a {3}{R} 2/1 Dinosaur Beast with double strike" $ do
            HU.assertEqual "name" (Text.pack "Ridgetop Raptor") (Card.Type.name (card Card.ridgetopRaptorPrinting))
            HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3, red])) (Card.Type.manaCost (card Card.ridgetopRaptorPrinting))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (card Card.ridgetopRaptorPrinting))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness (card Card.ridgetopRaptorPrinting))
            HU.assertEqual "subtypes" (Set.fromList [Subtype.Dinosaur, Subtype.Beast]) (TypeLine.subtypes (Card.Type.typeLine (card Card.ridgetopRaptorPrinting)))
            HU.assertEqual "keyword" (Set.singleton Keyword.DoubleStrike) (Card.Type.keywords (card Card.ridgetopRaptorPrinting)),
          HU.testCase "the tiger has first strike through the projection" $
            let (oid, gs) = addCreature Card.sabretoothTigerPrinting alice gs0
             in do
                  HU.assertBool "first strike" (Game.hasKeyword Keyword.FirstStrike oid gs)
                  HU.assertBool "not double strike" (not (Game.hasKeyword Keyword.DoubleStrike oid gs)),
          HU.testCase "the raptor has double strike through the projection" $
            let (oid, gs) = addCreature Card.ridgetopRaptorPrinting alice gs0
             in do
                  HU.assertBool "double strike" (Game.hasKeyword Keyword.DoubleStrike oid gs)
                  HU.assertBool "not first strike" (not (Game.hasKeyword Keyword.FirstStrike oid gs)),
          HU.testCase "both are 2/1s, the same body as a Piker" $
            let bodyOf p = (Card.Type.power (card p), Card.Type.toughness (card p))
             in do
                  HU.assertEqual "tiger body" (bodyOf Card.pikerPrinting) (bodyOf Card.sabretoothTigerPrinting)
                  HU.assertEqual "raptor body" (bodyOf Card.pikerPrinting) (bodyOf Card.ridgetopRaptorPrinting)
        ]
```

- [x] **Step 2: Run to verify it fails**

Run: `cabal test 2>&1 > /tmp/t.txt; grep -m3 -E "error|not in scope" /tmp/t.txt`
Expected: FAIL — `Card.sabretoothTigerPrinting` not in scope.

- [x] **Step 3: Add the keywords**

`source/library/Pawl/Type/Keyword.hs` — insert `DoubleStrike` and `FirstStrike` in CR-number order and update the header comment's count line:

```haskell
data Keyword
  = Defender -- 702.3
  | DoubleStrike -- 702.4
  | FirstStrike -- 702.7
  | Flying -- 702.9
  | Haste -- 702.10
  | Reach -- 702.17
  | Vigilance -- 702.20
  deriving (Eq, Ord, Show)
```

In the block comment above the type, change the "Five, because five have consumers -- M2b inserts FirstStrike (702.7) and DoubleStrike (702.4); …" sentence to read: "Seven, because seven have consumers -- M2b added FirstStrike (702.7) and DoubleStrike (702.4); M2c inserts Deathtouch (702.2) and Trample (702.19)."

- [x] **Step 4: Add the subtypes**

`source/library/Pawl/Type/Subtype.hs` — append `Cat`, `Dinosaur`, `Beast`:

```haskell
data Subtype
  = Mountain
  | Goblin
  | Warrior
  | Human
  | Bird
  | Ogre
  | Centaur
  | Cat
  | Dinosaur
  | Beast
  deriving (Eq, Ord, Show)
```

- [x] **Step 5: Add the printings**

Append to `source/library/Pawl/Card.hs`:

```haskell
-- The M2b keyword cards. Each is mono-red, a 2/1 (the same body as a Goblin
-- Piker, so the only thing the engine can see is the keyword), and genuinely
-- vanilla-plus-one-keyword. Verified against Scryfall
-- (api.scryfall.com/cards/named?exact=...), zero Gatherer rulings.

-- Sabretooth Tiger: {2}{R}, Creature - Cat, 2/1, First strike.
sabretoothTigerPrinting :: Printing.Printing
sabretoothTigerPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Sabretooth Tiger",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 2,
                      ManaSymbol.OfType (ManaType.Colored Color.Red)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.singleton Subtype.Cat
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 2)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 1)),
            Card.keywords = Set.singleton Keyword.FirstStrike
          }
    }

-- Ridgetop Raptor: {3}{R}, Creature - Dinosaur Beast, 2/1, Double strike.
ridgetopRaptorPrinting :: Printing.Printing
ridgetopRaptorPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Ridgetop Raptor",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 3,
                      ManaSymbol.OfType (ManaType.Colored Color.Red)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.fromList [Subtype.Dinosaur, Subtype.Beast]
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 2)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 1)),
            Card.keywords = Set.singleton Keyword.DoubleStrike
          }
    }
```

- [x] **Step 6: Run to verify it passes**

Run: `cabal test --test-options='-p "/M2bCards/"' 2>&1 > /tmp/t.txt; grep -E "^All|FAIL" /tmp/t.txt`
Expected: PASS (5 cases).

Then `cabal test`. Expected: all pass — nothing consumes the keywords yet.

- [x] **Step 7: Lint and commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git commit -m "Add first strike and double strike, with Sabretooth Tiger and Ridgetop Raptor"
```

---

### Task 4: The two combat damage steps

CR 510.4. `Combat.struckFirst` snapshot, the wave predicates, the splice, the "still on the battlefield" guard. Double strike is the falsifier.

**Files:**
- Modify: `source/library/Pawl/Type/Combat.hs`, `source/library/Pawl/Combat.hs`, `source/library/Pawl/Damage.hs`, `source/library/Pawl/Engine.hs`, `source/test-suite/Main.hs`

**Interfaces:**
- Consumes: Task 1's `GameState.remaining`; Task 3's keywords and printings; `Game.hasKeyword`, `Game.lookupObject`, `runCombat`.
- Produces:
  - `Combat` gains `struckFirst :: Maybe (Set ObjectId)`
  - `Damage.dealCombatDamage :: Game Bool` (was `Game ()`)
  - `Turn.spliceSecondDamage :: Seq Phase -> Seq Phase`

- [x] **Step 1: Write the failing test**

Add this group and the `runToFirstStrikeDone` helper, and add `firstStrikeTests` to `testTree`:

```haskell
-- Run whole steps until the first-strike combat damage step has been dealt
-- (struckFirst is set) or combat ends, so a test can observe the board BETWEEN
-- the two combat damage steps.
runToFirstStrikeDone :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runToFirstStrikeDone answer gs0 =
  let go n g =
        if n <= (0 :: Int)
          || Maybe.isJust (Combat.Type.struckFirst (GameState.combat g))
          || not (inCombatPhase (GameState.phase g))
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 24 gs0

firstStrikeTests :: Tasty.TestTree
firstStrikeTests =
  Tasty.testGroup
    "FirstStrike"
    [ HU.testCase "CR 702.7b a first striker kills a vanilla blocker and lives" $
        -- The tiger (2/1 first strike) kills the Piker (2/1) in the first-strike
        -- step; the SBA between steps buries it before it can deal, so the tiger
        -- survives at zero damage.
        let (gs, _, _) = combatBoardOf [Card.sabretoothTigerPrinting] [Card.pikerPrinting]
            after = runCombat aggressiveAnswer gs
         in do
              HU.assertEqual "the blocker is dead" 0 (creaturesInPlay bob after)
              HU.assertEqual "the first striker lives" 1 (creaturesInPlay alice after),
      HU.testCase "CR 510.2 the control: two vanilla 2/1s trade" $
        -- With a Piker in the tiger's place there is one combat damage step and
        -- both die. So first strike is the sole cause above.
        let (gs, _, _) = combatBoardOf [Card.pikerPrinting] [Card.pikerPrinting]
            after = runCombat aggressiveAnswer gs
         in do
              HU.assertEqual "alice's is dead" 0 (creaturesInPlay alice after)
              HU.assertEqual "bob's is dead" 0 (creaturesInPlay bob after),
      HU.testCase "CR 702.4b a double striker deals twice to an unblocked player" $
        -- The raptor (2/1 double strike) deals 2 in each step: bob loses 4.
        let (gs, _, _) = combatBoardOf [Card.ridgetopRaptorPrinting] []
            after = runCombat aggressiveAnswer gs
         in HU.assertEqual "bob took 4" (Just 16) (lifeOf bob after),
      HU.testCase "CR 702.7b the control: a first striker deals once to a player" $
        let (gs, _, _) = combatBoardOf [Card.sabretoothTigerPrinting] []
            after = runCombat aggressiveAnswer gs
         in HU.assertEqual "bob took 2" (Just 18) (lifeOf bob after),
      HU.testCase "CR 510.1b the control: a vanilla creature deals once to a player" $
        let (gs, _, _) = combatBoardOf [Card.pikerPrinting] []
            after = runCombat aggressiveAnswer gs
         in HU.assertEqual "bob took 2" (Just 18) (lifeOf bob after),
      HU.testCase "CR 510.4 double strike kills a 3/3 across two steps; first strike does not" $
        -- The raptor deals 2 + 2 = 4 to the Ogre (3/3), killing it. A first
        -- striker deals 2 once, and the Ogre lives.
        let raptorVs = combatBoardOf [Card.ridgetopRaptorPrinting] [Card.ogreSentryPrinting]
            tigerVs = combatBoardOf [Card.sabretoothTigerPrinting] [Card.ogreSentryPrinting]
            afterRaptor = runCombat aggressiveAnswer (frst raptorVs)
            afterTiger = runCombat aggressiveAnswer (frst tigerVs)
         in do
              HU.assertEqual "double strike kills the Ogre" 0 (creaturesInPlay bob afterRaptor)
              HU.assertEqual "first strike leaves the Ogre" 1 (creaturesInPlay bob afterTiger),
      HU.testCase "CR 510.4 a striker killed in the first step does not deal in the second" $
        -- Raptor (double strike) and tiger (first strike) each block-kill the
        -- other in the first step. Neither is "remaining" for the second step, so
        -- no second-wave damage; both are simply dead.
        let (gs, _, _) = combatBoardOf [Card.ridgetopRaptorPrinting] [Card.sabretoothTigerPrinting]
            after = runCombat aggressiveAnswer gs
         in do
              HU.assertEqual "attacker dead" 0 (creaturesInPlay alice after)
              HU.assertEqual "blocker dead" 0 (creaturesInPlay bob after),
      HU.testCase "CR 510.4 the mixed board: first strike once, vanilla once, double strike twice" $
        -- Tiger (first strike), raptor (double strike) and Piker (vanilla) all
        -- attack unblocked. First-strike step: tiger 2 + raptor 2 = 4. Second
        -- step: raptor 2 + Piker 2 = 4. bob: 20 - 8 = 12. The naive "strikers in
        -- step one, everyone else in step two" drops the raptor's second hit and
        -- lands bob at 14.
        let (gs, _, _) = combatBoardOf [Card.sabretoothTigerPrinting, Card.ridgetopRaptorPrinting, Card.pikerPrinting] []
            mid = runToFirstStrikeDone aggressiveAnswer gs
            after = runCombat aggressiveAnswer gs
         in do
              HU.assertEqual "after the first-strike step, bob took 4" (Just 16) (lifeOf bob mid)
              HU.assertEqual "after both steps, bob took 8" (Just 12) (lifeOf bob after)
    ]

-- The state out of a combatBoardOf triple.
frst :: (a, b, c) -> a
frst (a, _, _) = a
```

- [x] **Step 2: Run to verify it fails**

Run: `cabal test --test-options='-p "/FirstStrike/"' 2>&1 > /tmp/t.txt; grep -m3 -E "error|not in scope|FAIL" /tmp/t.txt`
Expected: FAIL — `Combat.Type.struckFirst` not in scope.

- [x] **Step 3: Add `struckFirst` to the `Combat` type**

In `source/library/Pawl/Type/Combat.hs`, add the field after `blockers`:

```haskell
    blockers :: Map ObjectId (Set ObjectId),
    -- CR 510.4: which attackers and blockers had first strike or double strike as
    -- the FIRST combat damage step began. Nothing while that step has not
    -- happened; Just once it has, so the second combat damage step knows who is
    -- excluded ("had neither...") and the step router knows a second step already
    -- ran. Reset at CR 511 (end of combat).
    --
    -- EXPIRES at M3: this is captured live off the projection because nothing
    -- changes keywords mid-combat; at M3 (layer 6) "had it then" and "has it now"
    -- come apart, and the CR 510.3 window between the two steps can change it.
    struckFirst :: Maybe (Set ObjectId)
  }
  deriving (Eq, Ord, Show)
```

`Maybe` and `Set` are already available to this module (`Set` is imported; `Maybe` is in the Prelude).

- [x] **Step 4: Reset `struckFirst` in `emptyCombat`**

In `source/library/Pawl/Combat.hs`, add the field to `emptyCombat`:

```haskell
emptyCombat :: Combat
emptyCombat =
  Combat.MkCombat
    { Combat.attackers = Map.empty,
      Combat.blockers = Map.empty,
      Combat.struckFirst = Nothing
    }
```

- [x] **Step 5: Add the splice to `Pawl.Turn`**

In `source/library/Pawl/Turn.hs`, append:

```haskell
-- CR 510.4 / 500.9: a second combat damage step, spliced directly after the
-- current one -- i.e. at the head of the remaining schedule, so it runs next.
-- CR 500.9's "most recently created step occurs first" is exactly cons-at-head.
spliceSecondDamage :: Seq Phase -> Seq Phase
spliceSecondDamage remaining = Phase.Combat CombatStep.CombatDamage Seq.<| remaining
```

- [x] **Step 6: Rewrite the combat damage step in `Pawl.Damage`**

In `source/library/Pawl/Damage.hs`, add the import (keep the block sorted):

```haskell
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Zone as Zone
```

Give `gatherCombatDamage` a per-participant filter (this is the only change to its body — the two `filter`s), and leave `attackerAssignment`, `blockerAssignment` and `applyCombatDamage` exactly as they are:

```haskell
-- CR 510.2: gather all combat damage from the participants that `assigns` admits,
-- before applying any of it (simultaneity). The filter is how a wave restricts
-- who deals: attackers by their id, blockers by pruning each attacker's set.
gatherCombatDamage :: (ObjectId -> Bool) -> Game ([(ObjectId, Natural)], [(PlayerId, Natural)])
gatherCombatDamage assigns = do
  gs <- State.get
  let combat = GameState.combat gs
      attackers = filter (assigns . fst) (Map.toList (Combat.Type.attackers combat))
      blockers = map (\(a, bs) -> (a, Set.filter assigns bs)) (Map.toList (Combat.Type.blockers combat))
  parts <- Monad.mapM (attackerAssignment gs) attackers
  let fromBlockers = concatMap (blockerAssignment gs) blockers
  pure (concatMap fst parts ++ fromBlockers, concatMap snd parts)
```

Replace `dealCombatDamage` with the wave logic:

```haskell
-- Deal one combat damage step, returning True iff this was the FIRST of two --
-- i.e. a second combat damage step must be spliced (CR 510.4).
--
-- Which creatures assign is read LIVE off the projection at this boundary (spec
-- §3), never precomputed. `struckFirst` both routes the wave and records CR
-- 510.4's "had first strike or double strike as the first step began" snapshot.
-- Only creatures still on the battlefield assign ("the REMAINING attackers and
-- blockers") -- a striker killed in the first step is gone for the second.
dealCombatDamage :: Game Bool
dealCombatDamage = do
  gs <- State.get
  let combat = GameState.combat gs
      participants =
        Set.union
          (Map.keysSet (Combat.Type.attackers combat))
          (Set.unions (Map.elems (Combat.Type.blockers combat)))
      striking oid = Game.hasKeyword Keyword.FirstStrike oid gs || Game.hasKeyword Keyword.DoubleStrike oid gs
      strikers = Set.filter striking participants
      onBattlefield oid = case Game.lookupObject oid gs of
        Just obj -> Object.zone obj == Zone.Battlefield
        Nothing -> False
  case Combat.Type.struckFirst combat of
    Nothing
      -- CR 510.4 does not apply: no striker, so one step and everyone deals.
      | Set.null strikers -> do
          dealWave onBattlefield
          pure False
      -- CR 510.4: a striker is present. This is the first of two steps; only
      -- first strikers and double strikers deal, and a second step follows.
      | otherwise -> do
          State.modify' (\g -> g {GameState.combat = (GameState.combat g) {Combat.Type.struckFirst = Just strikers}})
          dealWave (\oid -> onBattlefield oid && Set.member oid strikers)
          pure True
    -- CR 510.4 second step: those that had neither first strike nor double strike
    -- as the first step began (not in the snapshot), plus those that currently
    -- have double strike -- and are still on the battlefield.
    Just snapshot -> do
      dealWave (\oid -> onBattlefield oid && (Set.notMember oid snapshot || Game.hasKeyword Keyword.DoubleStrike oid gs))
      pure False

-- Gather this wave's damage under `assigns` and apply it.
dealWave :: (ObjectId -> Bool) -> Game ()
dealWave assigns = do
  assignment <- gatherCombatDamage assigns
  State.modify' (applyCombatDamage assignment)
```

- [x] **Step 7: Splice the second step in the engine**

In `source/library/Pawl/Engine.hs`, in `runTurnBasedActions`, replace the combat damage case:

```haskell
    Phase.Combat CombatStep.CombatDamage -> do
      -- CR 510.4: deal this step's damage; if it was the first-strike step,
      -- splice a second combat damage step in after it. The between-steps
      -- priority (CR 510.3) and SBA check come free from the step machinery.
      needSecond <- Damage.dealCombatDamage
      Monad.when needSecond $
        State.modify' (\gs -> gs {GameState.remaining = Turn.spliceSecondDamage (GameState.remaining gs)})
```

- [x] **Step 8: Run to verify it passes**

Run: `cabal test --test-options='-p "/FirstStrike/"' 2>&1 > /tmp/t.txt; grep -E "^All|FAIL" /tmp/t.txt`
Expected: PASS (8 cases).

If "the mixed board" fails at `Just 14` rather than `Just 12`, the second wave is dropping double strikers — check that the `Just snapshot` branch's predicate is `Set.notMember oid snapshot || hasKeyword DoubleStrike`, not `Set.notMember oid snapshot` alone. Fix the engine, not the test.

Then `cabal test`. Expected: all pass — M1b/M2a combat tests use Pikers and keyword cards with no strikers, so `dealCombatDamage` takes the single-wave `Nothing`/`null strikers` branch and behaves exactly as before. The `Game ()` → `Game Bool` change is absorbed by `fightWith`, which discards the block's result.

- [x] **Step 9: Lint and commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git commit -m "Split combat damage into two steps for first strike and double strike (CR 510.4)"
```

---

### Task 5: Properties, exit criterion, and close M2b

No new library code. Confirm the milestone: every property survives, the suite is warning-clean, replay is untouched.

**Files:**
- Modify: `source/test-suite/Main.hs`

**Interfaces:**
- Consumes: everything above.
- Produces: no new library interfaces.

- [x] **Step 1: Run the properties**

Run: `cabal test --test-options='-p "/Properties/"' 2>&1 > /tmp/t.txt; grep -E "^All|FAIL" /tmp/t.txt`
Expected: PASS (6 properties). **M2b retires no property.** The deck is unchanged, so conservation still asserts 120; the turn-structure change is additive.

If "conservation: 120 objects at end" fails, something touched the deck — M2b must not. If "every game terminates" hangs, suspect an unbounded splice: the splice must fire only in the `Nothing`/striker branch of `dealCombatDamage`, and the `Just snapshot` branch must never return `True`.

- [x] **Step 2: Add the exit-criterion assertion**

Add this group, and add `m2bExitTests` to `testTree`. It states the milestone's headline in one deterministic case: a first striker that would trade as a vanilla instead lives, and a turn with no attacker changes nothing.

```haskell
m2bExitTests :: Tasty.TestTree
m2bExitTests =
  Tasty.testGroup
    "M2bExit"
    [ HU.testCase "the milestone: first strike breaks the trade, double strike doubles the hit, no attacker no damage" $
        let trade = runCombat aggressiveAnswer (frst (combatBoardOf [Card.sabretoothTigerPrinting] [Card.pikerPrinting]))
            doubled = runCombat aggressiveAnswer (frst (combatBoardOf [Card.ridgetopRaptorPrinting] []))
            quiet = runCombat aggressiveAnswer (frst (combatBoardOf [] []))
         in do
              HU.assertEqual "first striker lives" 1 (creaturesInPlay alice trade)
              HU.assertEqual "its would-be killer is dead" 0 (creaturesInPlay bob trade)
              HU.assertEqual "double striker deals 4" (Just 16) (lifeOf bob doubled)
              HU.assertEqual "an attacker-less turn deals nothing" (Just 20) (lifeOf bob quiet)
    ]
```

- [x] **Step 3: Run the exit test and the full suite**

Run: `cabal test --test-options='-p "/M2bExit/"' 2>&1 > /tmp/t.txt; grep -E "^All|FAIL" /tmp/t.txt`
Expected: PASS (1 case).

Then `cabal test 2>&1 > /tmp/t.txt; grep -E "^All|FAIL" /tmp/t.txt`. Expected: all pass.

- [x] **Step 4: Confirm replay is untouched**

The `Prompt` type did not change in M2b, so the transcript and replay are unaffected — confirm it:

```bash
cabal test --test-options='-p "/Replay/"'
cabal test --test-options='-p "/CombatReplay/"'
```

Expected: PASS. If either fails, something added or changed a prompt — M2b must not.

- [x] **Step 5: Full warning-clean build and benchmark**

```bash
rm -rf dist-newstyle/build/aarch64-osx/ghc-9.14.1/pawl-0.2026.7.16/{b,t,build}
cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -c "warning:"
```

Expected: `0`.

Then `cabal bench`. Expected: PASS — the benchmark reports `goldfish 2p`, `casting 2p` and `fighting 2p`; nothing in M2b changes its shape.

- [x] **Step 6: Commit and close the milestone**

```bash
git add -A
hooky fix
git add -A
hooky run
git commit -m "Record M2b complete: first strike, double strike, and conditional turn structure"
git-bug bug new -t "M2b — first strike and conditional turn structure" -m "Complete."
```

Then update `CLAUDE.md`'s "Current work and tracking": mark M2b complete (spec and plan kept as reference, like M0/M1a/M1b/M2a), and set current work to **M2c** (deathtouch + trample) with the brief at `docs/design.md` §M2c and the M2c pointers left in this milestone's expiries. Commit that as a small separate change.

---

## Self-review notes (soft spots flagged, not hidden)

1. **Task 1 is a pure refactor and must stay one.** The whole claim is that popping `remaining` reproduces the old `Turn.next` walk exactly. The only existing test that legitimately changes is `turnTests`, because it named the deleted `next`. If any *other* test needs editing to pass Task 1, the refactor changed behavior — stop and reread `advance`/`handoffTurn`.
2. **The battlefield guard is faithful but not independently falsifiable at M2b, and that is deliberate.** CR 510.4 says only "remaining" creatures deal in the second step, so `dealCombatDamage` filters by `onBattlefield`. But with M2b's card pool a striker can only die in the first step while *blocked* or *blocking*, so its phantom second hit would land on an already-dead creature — unobservable. The "a striker killed in the first step does not deal" test exercises the guard's code path; a genuinely falsifying test needs a tough first-striker, which arrives with P/T effects at M3. This is the same shape as haste's CR 702.10c in M2a: implemented for faithfulness, its expiry named.
3. **`dealCombatDamage` changed from `Game ()` to `Game Bool`, and `fightWith` swallows the Bool.** `fightWith` ends its `do` block with `Damage.dealCombatDamage`; `snd (runGamePure …)` discards the block's result, so every M1b/M2a damage test compiles and behaves identically. If one of them fails to *compile*, it is calling `dealCombatDamage` in a context that needs `()` — none should.
4. **The mixed-board total (bob == 12) does not by itself say *which* creature struck twice** — (tiger twice, raptor once) also sums to 8. The single-creature controls pin it: raptor-alone lands bob at 16 (twice) and tiger-alone at 18 (once). Read the `FirstStrike` group as a set; no one case is sufficient, which is why the controls are not padding.
5. **The spec's §6 "no priority in a skipped step, across seeds" property is realized deterministically, not as a QuickCheck property, and this is a real adaptation.** The `Prompt` carries a `PlayerId` but not the phase it was asked in, and the interpreter (`forall r. Prompt r -> r`) cannot see `GameState`, so "a prompt timestamped to a skipped step" is not observable from a seeded game without new machinery. The `Skip` group tests the fix at its source instead — the schedule literally no longer contains the two steps (CR 500.11's "as though it didn't exist"), so they cannot grant priority. That is stronger than counting prompts, and it needs no new type. Flagged so the reviewer does not read a missing QuickCheck property as a gap.
6. **`struckFirst` stores a set where a Bool would do at M2b, on purpose.** Nothing changes keywords mid-combat now, so the snapshot always equals "current strikers" and a bare flag would route the waves correctly. The set is stored because it *is* CR 510.4's "had first strike or double strike as the first step began," it routes safely even for a 0-power first striker (which the flag-by-emptiness alternative would mis-handle), and it is what M3 needs when the projection starts moving. Do not "simplify" it to a `Bool`.
7. **`skipEmptyCombat` lives in `Pawl.Combat`, the splice wiring in `Pawl.Engine`.** Both edit `GameState.remaining`. The skip is triggered by `declareAttackers` (Combat's) and reads `Combat.attackers`, so it sits with combat and imports `Turn` for the pure sequence op; the splice is triggered by `dealCombatDamage`'s return and is applied where the step is dispatched. `Turn` owns the pure schedule algebra (`dropSkippedCombatSteps`, `spliceSecondDamage`); the two consumers apply it. `Turn` imports only `Type.*`, so `Combat`→`Turn` and `Engine`→`Turn` are acyclic.
8. **Every `Card.MkCard` now carries `keywords`, and Task 3 adds two more sites.** The two M2b printings set it explicitly; `-Werror` (via the missing-field warning) confirms nothing was left unset. No existing `Card.MkCard` site changes.
9. **`runCombat` and `runToFirstStrikeDone` are bounded at 24 steps.** A combat phase has at most five steps here, so 24 is slack, not a real limit — but if a test ever hits the bound, it means the schedule is not draining (a splice that never stops, or `advance` not popping), and the bounded loop turns that into a failed assertion instead of a hang.
