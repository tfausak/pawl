# M5a — Controlling Another Player (CR 723) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close out CR 723 (Controlling Another Player) by proving the already-built `Decider`/`activeControl` substrate at *gameplay level* with Mindslaver, adding the three small-correctness falsifiers (723.1a overwrite, 723.5a resource isolation, 723.6 concede guard), and filing the concede deferral — **without adding any new library types, opcodes, or prompts.**

**Architecture:** M5a is a *close-out, not a new axis*. The control machinery already exists and is wired (`Pawl.Type.Decider`, `Pawl.Decide.deciderFor`, `GameState.pendingControl`/`activeControl`, `Effect.ControlPlayerNextTurn`, the `pendingControl → activeControl` promotion in `Engine.handoffTurn`). Every task here is a **test** against that correct substrate, plus one documentation comment and one GitHub issue. Because the machinery is already correct, these gate/falsifier tests are expected to **pass on first run**; each test task therefore includes an explicit *falsification check* (a named one-line mutation that must turn the test red, then is reverted) so the test's teeth are proven, honoring the TDD discipline for characterization tests.

**Tech Stack:** Haskell 2010 (GHC 9.14.1 from the Nix dev shell), `tasty` + `tasty-hunit`. Tests live in `source/test-suite/Pawl/`. Cards are data files under `data/cards/`.

## Global Constraints

Every task's requirements implicitly include this section. Values copied verbatim from `CLAUDE.md`.

- **Warning-clean build.** `cabal build all --enable-tests --enable-benchmarks` must compile under `-Weverything` minus the allow-list, with `flags: +pedantic` (`-Werror`) on. Incremental builds hide warnings from unchanged modules — for a definitive check, `cabal clean` first.
- **No new language extensions.** Haskell 2010. `GameSpec.hs` and `ResolveSpec.hs` already carry `{-# LANGUAGE GADTs #-}` (needed for pattern-matching the `Prompt` GADT). Do not add others.
- **Qualified imports, aliased to the last component** (`Pawl.Decide` → `Decide`); operators unqualified. One import group.
- **No partial functions.** No `head`, `undefined`, `error`, or non-exhaustive matches. Answerers over the `Prompt` GADT that delegate to another answerer for the unhandled constructors use a `_ -> otherAnswer p` catch-all — this is total.
- **Cite the CR number in every rules-bearing comment.** Every CR claim was checked against `docs/rules.txt` (lines 6157–6188 for CR 723; 104.3a at line 340; 405.6g at line 2062). Never trust recalled Magic rules.
- **The two invariants outrank this plan.** (1) The rules core reads a *classification*, never an effect's identity — 723 stays a `Decider` swap at `Decide.deciderFor`; the engine never cases on "is this Mindslaver." (Test answerers *may* inspect actions/effects — the invariant governs the engine core, not test code.) (2) The engine makes no choices — every controlled choice is a prompt routed to the controller's `Decider`; nothing is elided.
- **One small complete commit per task, on `main`.** Never push. Commit message trailer: `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>`.
- **`hooky` before done.** `git add -A` → `hooky fix` → `git add -A` → `hooky run` (acts on staged files only).

---

## Substrate reference (read once before starting)

These already exist and are correct. Tasks assert against them; do not modify them (except the one comment in Task 5).

- `Pawl.Type.Decider` — `newtype Decider = MkDecider PlayerId` (`Eq, Ord, Show`).
- `Pawl.Decide.deciderFor :: PlayerId -> GameState -> Decider` (`source/library/Pawl/Decide.hs:14-17`):
  ```haskell
  deciderFor pid gs = case GameState.activeControl gs of
    Just decider | pid == GameState.activePlayer gs -> decider
    _ -> Decider.MkDecider pid
  ```
- `GameState.pendingControl :: Map PlayerId Decider` and `GameState.activeControl :: Maybe Decider` (`source/library/Pawl/Type/GameState.hs:94,99`).
- `Effect.ControlPlayerNextTurn SlotName` resolved in `Pawl.Resolve.applyEffect` (`source/library/Pawl/Resolve.hs:511-519`): `Map.insert target (MkDecider controller)` into `pendingControl`. The `controller` is `Object.owner obj` of the resolving ability (`Resolve.hs:368`).
- `Engine.handoffTurn` (`source/library/Pawl/Engine.hs:441-470`) promotes `activeControl = Map.lookup newActive (pendingControl gs)` and `pendingControl = Map.delete newActive (...)`.
- Priority action site: `Engine.priorityLoop` (`source/library/Pawl/Engine.hs:404-407`) issues `Prompt.ChooseAction (Decide.deciderFor p gs) p (Action.legalActions p gs)`.
- `Cost.pay pid oid` and `Mana.payCost pid` debit `pid` exclusively — cost payment is already 723.5a-correct by construction; the only `Decider` in the payment path (`Cost.hs:258`, `ChooseSacrifices`) governs *who chooses*, not whose resources.

## Existing test assets reused (already in `GameSpec.hs`)

- `slaveAnswer :: Prompt.Prompt r -> r` (`GameSpec.hs:402-432`) — a control-aware answerer: when `ChooseAction`'s decider is alice and the player is bob, it casts; it targets bob; minimal for everything else.
- `isCastAction :: A.Action -> Bool` (`GameSpec.hs:434-437`).
- `handBobBolt :: Cards.Cards -> GameState -> (ObjectId, GameState)` (`GameSpec.hs:369-385`) — one Lightning Bolt in bob's hand.
- `namedIs :: Text -> Maybe Object.Object -> Bool` (`GameSpec.hs:387-396`).
- Support fixtures (`Pawl.Support`, aliased `S`): `addCreature`, `combatBoardOf`, `runCombat`, `lifeOf`, `tappedCount`, `handSize`, `bothPlayers`, `alice`, `bob`. `addCreature` places a permanent `Settled`, `Untapped`, on the battlefield under the given player.

## File structure

| File | Change | Responsibility |
|---|---|---|
| `source/test-suite/Pawl/GameSpec.hs` | Modify | Tasks 1, 2, 4: gameplay-level control tests in `ruleTests`; new answerers `gateAnswer`, `controlCombatAnswer`, helper `isActivateAction`; add `import qualified Pawl.Decide as Decide`. |
| `source/test-suite/Pawl/ResolveSpec.hs` | Modify | Task 3: 723.1a overwrite falsifier next to the existing installation test; helper `installControlBy`. |
| `source/library/Pawl/Engine.hs` | Modify (comment only) | Task 5: inline concede guard note at the `ChooseAction` site. |
| GitHub issue (`tfausak/pawl`) | Create | Task 5: concede-subsystem deferral. |
| `docs/progress.md`, `CLAUDE.md`, umbrella spec | Modify | Task 6: completion entry, status bullet replacement, phase tick. |

No library `Pawl.Type.*` modules are added or changed. M5a introduces **zero** new opcodes, prompts, or types.

---

### Task 1: Gameplay-level gate — Mindslaver hands alice bob's whole turn, then control lapses

The headline gate (CR 723.1 / 723.3). Alice activates a **real** Mindslaver through the driver loop targeting bob; the engine installs pending control, promotes it on bob's turn, alice makes bob's action/mode/target choices for bob's turn, and control lapses at the next turn boundary. This is strictly more end-to-end than either existing test: `ResolveSpec` drives the effect via `Stack.resolveTop`+`handoffTurn` (no activation, no controlled turn through the loop); the existing `GameSpec` 723.3/723.5 test sets `activeControl` by hand and runs one `priorityLoop`.

**Files:**
- Modify: `source/test-suite/Pawl/GameSpec.hs` — add import (top, in the import group), two new top-level helpers, one test case in `ruleTests` (the list ending at `GameSpec.hs:360`).

**Interfaces:**
- Consumes: `Engine.runGamePure`, `Engine.priorityLoop`, `Engine.handoffTurn`, `Setup.emptyGame`, `S.addCreature`, `Cards.mindslaverPrinting`, `Cards.mountainPrinting`, `handBobBolt`, `isCastAction`, `slaveAnswer`, `Decide.deciderFor`, `GameState.{pendingControl,activeControl,activePlayer}`, `Decider.MkDecider`.
- Produces (used by Tasks 2 & 4): `isActivateAction :: A.Action -> Bool`; `gateAnswer :: Prompt.Prompt r -> r`.

- [x] **Step 1: Add the `Decide` import**

In the import group of `source/test-suite/Pawl/GameSpec.hs` (alphabetically near the other `Pawl.*` imports, e.g. just after `import qualified Pawl.Cost as Cost` on line 18), add:

```haskell
import qualified Pawl.Decide as Decide
```

- [x] **Step 2: Add the activation helper and the gate answerer**

Append these two top-level definitions to `source/test-suite/Pawl/GameSpec.hs` (put them next to `slaveAnswer`, after line 437):

```haskell
-- Is this a legal-action Activate? On the gate board (a Mindslaver plus basic
-- lands, whose mana abilities are intrinsic and never surface as activated
-- abilities) the ONLY Activate action is Mindslaver's, so "the first Activate"
-- is unambiguously Mindslaver's control ability.
isActivateAction :: A.Action -> Bool
isActivateAction a = case a of
  A.Activate _ _ -> True
  _ -> False

-- CR 723 gate strategy. Alice, deciding for herself, fires Mindslaver (the only
-- activation on the board) at bob; once she is bob's decider (CR 723.5, the
-- prompt's Decider is alice while player is bob) she casts bob's Bolt; otherwise
-- pass. Non-ChooseAction prompts (targets, modes, shuffle, ...) delegate to
-- slaveAnswer, which targets bob. A naive engine ignoring control would send
-- bob's ChooseAction with Decider = bob; the else-branch would pass, bob would
-- keep 20 life, and the gate would fail -- the falsifier.
gateAnswer :: Prompt.Prompt r -> r
gateAnswer p = case p of
  Prompt.ChooseAction (Decider.MkDecider d) player actions ->
    case filter isActivateAction actions of
      activation : _ -> activation
      [] ->
        if player == S.bob && d == S.alice
          then case filter isCastAction actions of
            h : _ -> h
            [] -> A.Pass
          else A.Pass
  _ -> slaveAnswer p
```

- [x] **Step 3: Add the gate test case to `ruleTests`**

Insert this test into the `ruleTests` list in `source/test-suite/Pawl/GameSpec.hs`. Add a comma after the existing final element (the `"CR 723.3/723.5 ..."` case that currently ends the list at line 359, before the closing `]` on line 360), then add:

```haskell
      HU.testCase "CR 723.1/723.3 gameplay: Mindslaver hands alice bob's whole turn, then control lapses" $
        -- Alice activates a REAL Mindslaver through the driver loop at bob, the
        -- engine promotes control on bob's turn, alice casts bob's Bolt at bob
        -- (bob's own resource), and control ends at the following turn boundary.
        let g0 = Setup.emptyGame S.bothPlayers
            (_msId, g1) = S.addCreature (Cards.mindslaverPrinting cards) S.alice g0
            -- {4} for Mindslaver's activation: four untapped Mountains for alice.
            (_a1, g2) = S.addCreature (Cards.mountainPrinting cards) S.alice g1
            (_a2, g3) = S.addCreature (Cards.mountainPrinting cards) S.alice g2
            (_a3, g4) = S.addCreature (Cards.mountainPrinting cards) S.alice g3
            (_a4, g5) = S.addCreature (Cards.mountainPrinting cards) S.alice g4
            -- bob's own resources for his controlled turn: a Mountain and a Bolt.
            (_bMtn, g6) = S.addCreature (Cards.mountainPrinting cards) S.bob g5
            (_bBolt, g7) = handBobBolt cards g6
            gStart =
              g7
                { GameState.activePlayer = S.alice,
                  GameState.phase = Phase.PrecombatMain,
                  GameState.priority = Just S.alice
                }
            -- Alice's turn: activate Mindslaver at bob; the ability resolves and
            -- installs pending control for bob (CR 723.1).
            afterActivation = snd (Engine.runGamePure gateAnswer gStart Engine.priorityLoop)
            -- Handoff to bob's turn promotes pendingControl -> activeControl.
            bobsTurn = snd (Engine.runGamePure gateAnswer afterActivation Engine.handoffTurn)
            -- Bob's controlled main phase: alice decides, casting bob's Bolt at bob.
            bobMain = bobsTurn {GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.bob}
            bobPlayed = snd (Engine.runGamePure gateAnswer bobMain Engine.priorityLoop)
            -- Next handoff (bob -> alice) clears control (CR 723.1: ends at the
            -- beginning of the next turn).
            afterBob = snd (Engine.runGamePure gateAnswer bobPlayed Engine.handoffTurn)
            boltInBobGrave =
              length
                ( filter
                    (namedIs (Text.pack "Lightning Bolt"))
                    (map (\i -> Game.lookupObject i bobPlayed) (Game.zoneMembers Zone.Graveyard S.bob bobPlayed))
                )
         in do
              HU.assertEqual "CR 723.1: control pending for bob after activation" (Just (Decider.MkDecider S.alice)) (Map.lookup S.bob (GameState.pendingControl afterActivation))
              HU.assertEqual "CR 723.1: promoted to active control on bob's turn" (Just (Decider.MkDecider S.alice)) (GameState.activeControl bobsTurn)
              HU.assertEqual "CR 723.3: bob is still the active player while controlled" S.bob (GameState.activePlayer bobsTurn)
              HU.assertEqual "CR 723.5: bob's decisions route to alice" (Decider.MkDecider S.alice) (Decide.deciderFor S.bob bobsTurn)
              HU.assertEqual "alice's whole-turn choice moved bob's life" (Just 17) (S.lifeOf S.bob bobPlayed)
              HU.assertEqual "bob's Bolt went to bob's graveyard" 1 boltInBobGrave
              HU.assertEqual "bob's Mountain (his resource) is tapped" 1 (S.tappedCount S.bob bobPlayed)
              HU.assertEqual "CR 723.1: control lapses at the next turn" (Decider.MkDecider S.bob) (Decide.deciderFor S.bob afterBob)
              HU.assertEqual "active control cleared after bob's turn" Nothing (GameState.activeControl afterBob)
```

- [x] **Step 4: Build and run the gate test — expect PASS**

```
cabal build all --enable-tests --enable-benchmarks
cabal test --test-options='-p "$0~/Mindslaver hands alice bob/"'
```
Expected: **PASS.** The substrate is already correct, so this characterization gate passes on first run. If it **fails**, the substrate has a real bug — STOP, do not weaken the test, investigate the control path (`deciderFor`, `handoffTurn`, the `ControlPlayerNextTurn` arm). A gate failing against supposedly-correct code is a genuine finding, not a test to relax.

- [x] **Step 5: Falsification check — prove the gate has teeth**

Temporarily make control a no-op to confirm the gate actually exercises it. In `source/library/Pawl/Decide.hs`, change the body of `deciderFor` to ignore `activeControl`:

```haskell
deciderFor pid _gs = Decider.MkDecider pid
```

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "$0~/Mindslaver hands alice bob/"'`
Expected: **FAIL** — with control neutered, alice never gets bob's `ChooseAction`, bob keeps 20 life, and `deciderFor S.bob bobsTurn` is bob. This proves the gate depends on the control substrate.

Then **revert** `Decide.hs` exactly (restore the two-line `case GameState.activeControl gs of ...` body) and re-run: expected **PASS**. Confirm `git diff source/library/Pawl/Decide.hs` is empty.

- [x] **Step 6: Commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "test(m5a): gameplay gate — Mindslaver controls bob's whole turn (CR 723.1/723.3)"
```

---

### Task 2: Combat choices route to the controller (CR 723.5)

CR 723.5's second example: "The controller of another player decides which of that player's creatures attack." The gate (Task 1) covers action/mode/target routing but not combat. This test drives bob's combat through the engine under control, with alice declaring bob's attacker.

**Files:**
- Modify: `source/test-suite/Pawl/GameSpec.hs` — one new answerer `controlCombatAnswer`, one test case in `ruleTests`.

**Interfaces:**
- Consumes: `S.combatBoardOf`, `S.runCombat`, `S.lifeOf`, `slaveAnswer`, `Decider.MkDecider`, `GameState.{activePlayer,activeControl}`, `Prompt.{DeclareAttackers,AssignCombatDamage}`.
- Produces: `controlCombatAnswer :: Prompt.Prompt r -> r`.

- [x] **Step 1: Add the combat answerer**

Append to `source/test-suite/Pawl/GameSpec.hs` (near the other answerers):

```haskell
-- CR 723.5 combat: alice, controlling bob, declares bob's attackers. Attackers
-- are declared only when the prompt's Decider is alice for player bob; a naive
-- engine that sent the prompt with Decider = bob would fall to `[]` and no one
-- would attack. Damage from the lone unblocked attacker goes to its sole
-- recipient (the defending player, alice). Everything else delegates to
-- slaveAnswer (blocks: none; priority: pass).
controlCombatAnswer :: Prompt.Prompt r -> r
controlCombatAnswer p = case p of
  Prompt.DeclareAttackers (Decider.MkDecider d) player attackers ->
    if d == S.alice && player == S.bob then attackers else []
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case Map.keys thresholds of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  _ -> slaveAnswer p
```

- [x] **Step 2: Add the combat test case to `ruleTests`**

Add (comma-separated) into `ruleTests`:

```haskell
      HU.testCase "CR 723.5 combat: alice declares bob's attackers, so alice takes the hit" $
        -- bob's turn, controlled by alice, with one 2/1 Piker. combatBoardOf sets
        -- alice active with `mine` and bob with `theirs`; here alice attacks with
        -- nothing and bob has the Piker, and we flip the active player to bob.
        let (board, _mine, _bobsPikers) = S.combatBoardOf [] [Cards.pikerPrinting cards]
            g0 =
              board
                { GameState.activePlayer = S.bob,
                  GameState.activeControl = Just (Decider.MkDecider S.alice)
                }
            after = S.runCombat controlCombatAnswer g0
         in HU.assertEqual "alice took 2 from bob's Piker, declared by alice-as-bob" (Just 18) (S.lifeOf S.alice after)
```

- [x] **Step 3: Build and run — expect PASS**

```
cabal build all --enable-tests --enable-benchmarks
cabal test --test-options='-p "$0~/alice declares bob.s attackers/"'
```
Expected: **PASS.** If it fails, investigate combat's use of `deciderFor` in `Pawl.Combat` (`Combat.hs:191,219`) — do not weaken the test.

- [x] **Step 4: Falsification check**

In `source/library/Pawl/Decide.hs`, neuter `deciderFor` as in Task 1 Step 5 (`deciderFor pid _gs = Decider.MkDecider pid`). Run the test: expected **FAIL** (attackers prompt now carries Decider = bob → `[]` → no attack → alice stays 20). Revert `Decide.hs`, re-run: expected **PASS**, `git diff` empty.

- [x] **Step 5: Commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "test(m5a): combat choices route to the controller (CR 723.5)"
```

---

### Task 3: Multiple player-controlling effects overwrite, last created wins (CR 723.1a)

CR 723.1a: "Multiple player-controlling effects that affect the same player overwrite each other. The last one to be created is the one that works." A continuous effect from resolution is *created* when the spell/ability resolves (CR 613.7d), so last-created = last-resolved = last `Map.insert` — and `pendingControl :: Map PlayerId Decider` overwrites on insert. This test resolves two `ControlPlayerNextTurn` effects at the same target (bob) with different controllers and asserts the second wins. It lives beside the existing installation test in `ResolveSpec`, which already imports the ability-construction machinery.

**Files:**
- Modify: `source/test-suite/Pawl/ResolveSpec.hs` — a helper `installControlBy` and a test case near the existing `"CR 723.1: Mindslaver's ability installs pending control ..."` case (around line 527–565).

**Interfaces:**
- Consumes (all already imported in `ResolveSpec.hs`): `Setup.emptyGame`, `S.addCreature`, `Cards.mindslaverPrinting`, `Game.freshObjectId`, `Game.freshTimestamp`, `Object.MkObject`, `Source.OfAbility`, `ActivatedAbility.MkActivatedAbility`, `Cost.Type.MkCost`, `ManaCost.MkManaCost`, `ManaSymbol.Generic`, `CostComponent.{TapThis,SacrificeThis}`, `Modal.MkModal`, `Mode.MkMode`, `Effect.ControlPlayerNextTurn`, `TargetSpec.MkTargetSpec`, `Pool.Players`, `Exclusion.IncludesSource`, `ModeSelection.ChooseExactly`, `ModeIndex.MkModeIndex`, `Binding.fromChoices`, `Recipient.ToPlayer`, `SlotName.MkSlotName`, `Zone.Stack`, `Engine.runGamePure`, `S.identityAnswer`, `Stack.resolveTop`, `Decider.MkDecider`, `GameState.pendingControl`.
- Produces: `installControlBy :: PlayerId -> PlayerId -> GameState -> GameState` (build+resolve one `ControlPlayerNextTurn` ability owned by the controller, targeting the given player).

> **Before writing:** confirm `ResolveSpec.hs` already imports the constructors above by reading its import list and the existing installation test (lines ~527–565). It does — that test builds exactly this ability. If any import is missing, add it aliased to the last component.

- [x] **Step 1: Add the `installControlBy` helper**

Append to `source/test-suite/Pawl/ResolveSpec.hs` (top level, near the existing 723 test's helpers). The `Object.owner` is the resolving ability's controller (`Resolve.hs:368` uses `effectController = Object.owner obj`), so setting `owner = controller` installs `MkDecider controller`:

```haskell
-- Build a Mindslaver-shaped ControlPlayerNextTurn ability owned by `controller`,
-- targeting `target`, put it on the stack, and resolve it. Returns the resulting
-- state. Object.owner is the resolving ability's controller (Resolve.hs), so this
-- installs pendingControl[target] = MkDecider controller.
installControlBy :: PlayerId.PlayerId -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
installControlBy controller target gs0 =
  let (srcId, gs1) = S.addCreature (Cards.mindslaverPrinting cards0) controller gs0
      slot = SlotName.MkSlotName (Text.pack "target")
      ability =
        ActivatedAbility.MkActivatedAbility
          { ActivatedAbility.cost =
              Cost.Type.MkCost
                { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 4]),
                  Cost.Type.components = [CostComponent.TapThis, CostComponent.SacrificeThis]
                },
            ActivatedAbility.modal =
              Modal.MkModal
                (Seq.singleton (Mode.MkMode (Seq.fromList [Effect.ControlPlayerNextTurn slot]) (Map.singleton slot (TargetSpec.MkTargetSpec Pool.Players Nothing Exclusion.IncludesSource))))
                (ModeSelection.ChooseExactly 1)
          }
      (abilId, gs2) = Game.freshObjectId gs1
      (ts, gs3) = Game.freshTimestamp gs2
      abilObj =
        Object.MkObject
          { Object.owner = controller,
            Object.source = Source.OfAbility srcId ability,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToPlayer target)) Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
      gs4 = gs3 {GameState.objects = Map.insert abilId abilObj (GameState.objects gs3), GameState.stack = abilId : GameState.stack gs3}
   in snd (Engine.runGamePure S.identityAnswer gs4 Stack.resolveTop)
```

> **Note:** `cards0` is whatever the surrounding test binds the loaded `Cards.Cards` to. In `ResolveSpec`, the tests are parameterized by a `cards` argument threaded from `tests :: Cards.Cards -> Tasty.TestTree`. Define `installControlBy` **inside** the relevant `let`/`where` of the test group so `cards` is in scope, or add a `Cards.Cards` parameter: `installControlBy :: Cards.Cards -> PlayerId -> PlayerId -> GameState -> GameState`. Prefer the explicit parameter (`installControlBy cards controller target gs0`) and pass `cards` at each call — it keeps the helper top-level and matches the file's other `cards`-taking helpers. Adjust the signature and the `Cards.mindslaverPrinting cards` reference accordingly. Verify `Object`, `TapState`, `Sickness`, `Set`, `Seq`, `Nothing` are imported (they are, in the existing installation test).

- [x] **Step 2: Add the overwrite test case**

Add (comma-separated) into the same `testGroup` list that holds the existing 723.1 installation test in `ResolveSpec.hs`:

```haskell
      HU.testCase "CR 723.1a: a second player-controlling effect overwrites the first (last created wins)" $
        let base = Setup.emptyGame S.bothPlayers
            -- First: alice controls bob.
            afterAlice = installControlBy cards S.alice S.bob base
            -- Then: bob controls bob (CR 723.9 self-control), created LATER.
            afterBob = installControlBy cards S.bob S.bob afterAlice
         in do
              HU.assertEqual "the first effect installed alice as bob's decider" (Just (Decider.MkDecider S.alice)) (Map.lookup S.bob (GameState.pendingControl afterAlice))
              HU.assertEqual "CR 723.1a: the later effect overwrites — bob's own control wins" (Just (Decider.MkDecider S.bob)) (Map.lookup S.bob (GameState.pendingControl afterBob)),
```

> If `installControlBy` was kept parameterless per the `let`-scoped variant, drop the `cards` argument at both call sites.

- [x] **Step 3: Build and run — expect PASS**

```
cabal build all --enable-tests --enable-benchmarks
cabal test --test-options='-p "$0~/a second player-controlling effect overwrites/"'
```
Expected: **PASS** — `Map.insert` overwrites, and the second resolution is the last-created.

- [x] **Step 4: Falsification check**

In `source/library/Pawl/Resolve.hs`, temporarily change the `ControlPlayerNextTurn` arm (line ~517) from `Map.insert` to insert-only-if-absent, so a second effect would NOT overwrite:

```haskell
gs {GameState.pendingControl = Map.insertWith (\_new old -> old) target (Decider.MkDecider controller) (GameState.pendingControl gs)}
```

Run the test: expected **FAIL** (the first effect's alice would wrongly survive). This proves the test pins the overwrite semantics. **Revert** to the original `Map.insert ...` line, re-run: expected **PASS**, `git diff source/library/Pawl/Resolve.hs` empty.

- [x] **Step 5: Commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "test(m5a): player-controlling effects overwrite, last created wins (CR 723.1a)"
```

---

### Task 4: The controller spends only the controlled player's resources (CR 723.5a)

CR 723.5a: "The controller of another player can use only that player's resources (cards, mana, and so on) to pay costs for that player." Cost payment is already correct by construction (`Cost.pay pid`/`Mana.payCost pid` debit `pid`). The existing 723.3/723.5 test shows bob's resources *move*; it does not assert alice's stay *put*. This test adds the negative half: with alice and bob each holding an untapped Mountain, alice-as-bob casts bob's Bolt, and **alice's Mountain remains untapped** and **alice's hand is unchanged**.

**Files:**
- Modify: `source/test-suite/Pawl/GameSpec.hs` — one test case in `ruleTests`. Reuses `slaveAnswer`, `handBobBolt`, `S.addCreature`, `S.tappedCount`, `S.handSize`, `S.lifeOf`.

**Interfaces:**
- Consumes: `Setup.emptyGame`, `S.addCreature`, `Cards.mountainPrinting`, `handBobBolt`, `slaveAnswer`, `Engine.{runGamePure,priorityLoop}`, `GameState.{activePlayer,phase,priority,activeControl}`, `Decider.MkDecider`.
- Produces: nothing new.

- [x] **Step 1: Add the resource-isolation test to `ruleTests`**

Add (comma-separated) into `ruleTests`:

```haskell
      HU.testCase "CR 723.5a: the controller spends only the controlled player's resources" $
        -- bob (controlled by alice) and alice each have an untapped Mountain; bob
        -- has a Bolt. Alice-as-bob casts bob's Bolt, paid from BOB's Mountain.
        -- alice's Mountain and hand must be untouched.
        let g0 = Setup.emptyGame S.bothPlayers
            (_bMtn, g1) = S.addCreature (Cards.mountainPrinting cards) S.bob g0
            (_aMtn, g2) = S.addCreature (Cards.mountainPrinting cards) S.alice g1
            (_bBolt, g3) = handBobBolt cards g2
            g4 =
              g3
                { GameState.activePlayer = S.bob,
                  GameState.phase = Phase.PrecombatMain,
                  GameState.priority = Just S.bob,
                  GameState.activeControl = Just (Decider.MkDecider S.alice)
                }
            after = snd (Engine.runGamePure slaveAnswer g4 Engine.priorityLoop)
         in do
              HU.assertEqual "bob took 3 from his own Bolt" (Just 17) (S.lifeOf S.bob after)
              HU.assertEqual "bob's Mountain (his resource) is tapped" 1 (S.tappedCount S.bob after)
              HU.assertEqual "CR 723.5a: alice's Mountain is untouched" 0 (S.tappedCount S.alice after)
              HU.assertEqual "CR 723.5a: alice's hand is untouched" 0 (S.handSize S.alice after)
```

- [x] **Step 2: Build and run — expect PASS**

```
cabal build all --enable-tests --enable-benchmarks
cabal test --test-options='-p "$0~/spends only the controlled player.s resources/"'
```
Expected: **PASS** — `Mana.payCost S.bob` only ever taps bob's Mountain.

- [x] **Step 3: Falsification check**

Confirm the negative assertion has teeth: temporarily change the test's cast target-payer would require an engine change, so instead verify by a data mutation — temporarily give alice's Mountain to bob is not it; simplest teeth check is to flip the assertion's expected value to `1` for alice's Mountain and confirm the test then **fails** (proving `tappedCount S.alice` is genuinely read and is `0`). Concretely: change `HU.assertEqual "CR 723.5a: alice's Mountain is untouched" 0` to `... 1`, run, see **FAIL**, then restore `0`, run, see **PASS**. (No library file is touched in this check; revert leaves `git diff` empty for `source/library`.)

- [x] **Step 4: Commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "test(m5a): controller spends only the controlled player's resources (CR 723.5a)"
```

---

### Task 5: Concede guard — file the deferral issue and cite it at the priority site (CR 723.6)

CR 723.6: "The controller of another player can't make that player concede." Concede is **unimplemented** — `Departure.Conceded` exists but is never constructed; there is no `Concede` action, prompt, or handler. Concede-at-any-time is a new player-action axis outside M5a's close-out scope (decided: *defer concede, ship guard*). This task files the deferral and leaves the durable guard: an inline note at the priority `ChooseAction` site stating what is not implemented and the CR 723.6 constraint that concede must read the true player, never the `Decider`. Per `CLAUDE.md`: file the issue, cite it inline; the comment states only what is *not* implemented, plus `(#N)`.

**Files:**
- Create: GitHub issue in `tfausak/pawl`.
- Modify: `source/library/Pawl/Engine.hs` — one comment above line 405 (`let decider = Decide.deciderFor p gs`).

**Interfaces:** none (comment + issue only).

- [x] **Step 1: File the deferral issue**

Run (adjust wording only if a near-duplicate already exists — check `gh issue list --search "concede"` first):

```bash
gh issue create \
  --repo tfausak/pawl \
  --title "Concede special action (CR 104.3a / 405.6g): a player may concede at any time; must bypass the Decider (CR 723.6)" \
  --label gap --label rules-correctness \
  --body "Status: deferred; not implemented in M5a.

Concede is entirely unbuilt. \`Departure.Conceded\` exists (Pawl/Type/Departure.hs) but is never constructed; there is no \`Concede\` action (Pawl/Type/Action.hs has only Pass/Play/Cast/Activate), no concede prompt, and no handler. The only departure the engine produces is \`Departure.Lost\` via SBAs (Sba.hs).

Rationale for deferral: M5a is a close-out of the existing player-control substrate ('proof and edges, not machinery'). Concede-at-any-time (CR 104.3a: 'A player can concede the game at any time'; CR 405.6g) is a genuine new player-action axis — a priority-independent special action the prompt-driven engine does not yet model. Out of M5a's scope.

CR 723.6 constraint (why this is filed at the control close-out): the controller of a player CANNOT make the controlled player concede — a controlled player may still concede themselves. The footgun: adding \`Concede\` as an ordinary \`Action\` would route it through \`Prompt.ChooseAction decider p actions\` in Engine.priorityLoop, letting the controller answer for the controlled player — the exact leak CR 723.6 forbids. When concede is built it MUST read the true player, never \`Decide.deciderFor\`; it needs a channel keyed to the true player, outside the decider-mediated ChooseAction path.

Expiry trigger: rules-completeness. No scheduled milestone; not card-driven (no single card forces it — the two limited-duration control cards, Word of Command / Opposition Agent, are tracked separately).

Code-site guard: an inline note at Engine.priorityLoop's ChooseAction site cites this issue."
```

Record the issue number `N` printed by the command; you need it for Step 2.

- [x] **Step 2: Add the inline guard comment in `Engine.hs`**

In `source/library/Pawl/Engine.hs`, immediately above line 405 (`let decider = Decide.deciderFor p gs` inside `priorityLoop`), add (replace `#N` with the issue number from Step 1):

```haskell
                -- CR 723.6: concede is not implemented — there is no Concede
                -- action or concede channel. When it is added it must read the
                -- true player `p`, never `decider`: a controller cannot concede
                -- on the controlled player's behalf (CR 104.3a). (#N)
                let decider = Decide.deciderFor p gs
```

(Match the existing indentation of the `let` — it sits inside the `Just p -> do` block.)

- [x] **Step 3: Build — expect warning-clean**

```
cabal build all --enable-tests --enable-benchmarks
```
Expected: compiles clean (a comment cannot change behavior; this confirms indentation/placement did not break the layout).

- [x] **Step 4: Commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "docs(m5a): defer concede (CR 723.6), guard the priority site (#N)"
```

---

### Task 6: Close-out — verify, record, and tick M5a

Land the phase per `docs/workflow.md`: full warning-clean build, full test suite, invariant/rules audit, then the three docs updates.

**Files:**
- Modify: `docs/progress.md` (append the M5a entry), `CLAUDE.md` (replace the status bullet), `docs/superpowers/specs/2026-07-23-m5-player-control-restart-subgames-design.md` (tick M5a in the §3 table).

- [x] **Step 1: Definitive warning-clean build**

```
cabal clean
cabal build all --enable-tests --enable-benchmarks
```
Expected: **no warnings, no errors** (clean build defeats incremental warning-hiding).

- [x] **Step 2: Full test suite green**

```
cabal test
```
Expected: **all pass**, including the four new cases (Tasks 1–4). Do not proceed if anything is red.

- [x] **Step 3: Invariant + rules audit**

Confirm by inspection (record findings in the commit body if anything is notable):
- The engine core still never cases on Mindslaver's identity — 723 remains a `Decider` swap at `Decide.deciderFor`; the only new identity-casing is in **test** answerers (`isActivateAction` matches the `Activate` shape, not Mindslaver), which the invariant permits.
- No prompt was elided: every controlled choice flowed through `ChooseAction`/`ChooseTargets`/`DeclareAttackers` to the controller's `Decider`.
- Re-verify the CR numbers cited in the new comments/tests against `docs/rules.txt` (723.1 line 6159, 723.1a 6161, 723.3 6167, 723.5 6172, 723.5a 6176, 723.6 6182; 104.3a line 340). Delegate this to a cheap model if using subagents (Haiku, per `docs/workflow.md`).

- [x] **Step 4: Append the M5a completion entry to `docs/progress.md`**

Add after the M4.5 P11 entry (matching the surrounding bullet style):

```markdown
- **M5a is complete** (Controlling Another Player — CR 723 — the first M5 phase;
  a *close-out* of the `Decider` bet placed on day one and wired through M4a, not
  a new axis). **Gate: Mindslaver** at gameplay level — alice activates a real
  Mindslaver through the driver loop targeting bob, the engine installs pending
  control (CR 723.1), promotes it on bob's turn (`Engine.handoffTurn`), alice
  makes bob's action/mode/target choices (and, in a sibling test, bob's combat
  attackers, CR 723.5) for bob's turn, and control lapses at the following turn
  boundary. The decisions it proves: **723 is an indirection, already built** —
  the substrate (`Pawl.Type.Decider`, `Decide.deciderFor`, `pendingControl`/
  `activeControl`, `Effect.ControlPlayerNextTurn`, the `handoffTurn` promotion)
  needed no change; M5a adds proof and edges, not machinery. The three edges:
  **723.1a** — player-controlling effects overwrite, last created wins, because a
  resolution-created continuous effect's creation time is its resolution time, so
  last-created = last-`Map.insert` on `pendingControl` (asserted with two effects
  at the same target, distinct controllers); **723.5a** — cost payment debits only
  the controlled player's resources (already true by construction: `Cost.pay pid`/
  `Mana.payCost pid`; the negative half — the controller's own Mountain and hand
  untouched — is now asserted); **723.6** — the controller cannot make the
  controlled player concede: concede is unbuilt, deferred as a new player-action
  axis (#N), with a durable guard comment at `Engine.priorityLoop`'s `ChooseAction`
  site stating concede must read the true player, never the `Decider`. **Added:**
  nothing in the library — zero new types, opcodes, or prompts; four gameplay/
  resolution tests (`GameSpec`, `ResolveSpec`) and one guard comment. **Deferred:**
  concede (#N); 723.2 limited-duration control (Word of Command, Opposition Agent;
  card-driven).
```

Replace `#N` with the Task 5 issue number.

- [x] **Step 5: Replace the `CLAUDE.md` status bullet**

In `CLAUDE.md`, the "Current work and tracking" first bullet currently ends "**M5 is the next milestone to plan.**" Update the status to reflect M5a landed and M5b next. Replace the final sentence(s) of that bullet — keep the M0–M4.5 history summary, but change the tail from the M4.5-complete framing to note M5 is underway with M5a done. Concretely, append/replace so it reads (adjust to fit the existing sentence flow — **replace, do not append a second status bullet**):

> ... **M5 is now underway: M5a (Controlling Another Player, CR 723) has
> landed** — a gameplay-level Mindslaver gate over the already-built `Decider`
> substrate, plus the 723.1a/723.5a edges and the 723.6 concede deferral
> (concede is unbuilt, #N). **M5b (Restarting the Game, CR 727) is the next
> phase to plan** (introduces `startGameFromCards`). The umbrella spec is
> `docs/superpowers/specs/2026-07-23-m5-player-control-restart-subgames-design.md`.

Keep the existing pointers to `docs/progress.md` and the umbrella spec intact. Replace `#N`.

- [x] **Step 6: Tick M5a in the umbrella spec**

In `docs/superpowers/specs/2026-07-23-m5-player-control-restart-subgames-design.md`, mark the **M5a** row of the §3 phase table (line ~101) as landed — prefix the phase cell with a check, e.g. change `| **M5a** |` to `| **M5a ✅** |`, so the table shows the phase is done (per §6: "record its completion entry in docs/progress.md and tick the phase here").

- [x] **Step 7: Commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "docs(m5a): completion entry, CLAUDE.md status, umbrella tick"
```

- [x] **Step 8: Confirm the plan is fully executed**

```
grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-23-m5a-controlling-another-player.md
```
Expected: **`0`** (every step checked off). Confirm `git log --oneline -6` shows the six M5a commits on `main`.

---

## Self-review notes (for the executor)

- **Spec coverage.** Umbrella §3 M5a row + notes map to tasks: gameplay gate (723.1/723.3) → Task 1; combat clause of 723.5 → Task 2; 723.1a overwrite → Task 3; 723.5a resources → Task 4; 723.6 concede + guard → Task 5; close-out/exit-criterion → Task 6. Deferred items (723.2, full Karn is M5b, subgame prompt tagging) are out of M5a by the umbrella's own §4.
- **No new machinery.** Per the umbrella's "M5a is a close-out, not a new axis" and the *defer-concede* decision, no library type/opcode/prompt is added. If any task seems to require one, STOP — that is a scope error, not an implementation detail.
- **These are characterization/gate tests over correct code.** They pass on first run; a red result is a real substrate finding, never a cue to weaken the assertion (CLAUDE.md: "Never edit the plan, weaken an assertion, or delete a test to make a check pass"). The falsification checks (Steps 5/4/4 of Tasks 1/2/3) prove teeth via a *reverted* mutation — always confirm `git diff` on library files is empty afterward.
- **Type consistency.** Helper names used across tasks are stable: `gateAnswer`, `isActivateAction` (Task 1, reused conceptually by 2), `controlCombatAnswer` (Task 2), `installControlBy` (Task 3). Answerers all have type `Prompt.Prompt r -> r` and delegate unhandled constructors via `_ -> slaveAnswer p`.
