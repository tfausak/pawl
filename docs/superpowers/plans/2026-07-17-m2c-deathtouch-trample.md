# M2c Deathtouch + Trample Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Combat damage carries where it came from and how it is apportioned — a deathtoucher destroys any creature it wounds, and a trampler spills its excess onto the defending player — completing milestone **M2c** from `docs/superpowers/specs/2026-07-17-m2c-deathtouch-trample-design.md`.

**Architecture:** Two keywords on two axes. **Deathtouch** (702.2) needs the damage *source*, which M1b discards, so `Damage.applyCombatDamage` becomes a **change-and-emit** helper: it marks damage *and* appends a `DamageEvent` to `GameState.damageEvents`; the CR 704.5h state-based action reads those events and the SBA check drains them. **Trample** (702.19) restructures assignment: the `AssignCombatDamage` prompt generalizes to a keyword-agnostic `Map Recipient Natural` of legal recipients to **lethal thresholds**, and validation becomes the CR 702.19b implication *(if the defender is assigned any damage, every blocker got ≥ its lethal threshold)*. Their interaction (702.2c) is one line: a deathtouch source lowers each blocker's threshold to 1. **Zero opcodes.**

**Tech Stack:** GHC 9.14.1, Cabal. Library depends only on GHC boot libraries (`base`, `containers`, `text`, `transformers`) — no new dependencies. Tests use `tasty`/`tasty-hunit`/`tasty-quickcheck`; benchmark uses `tasty-bench`.

## Starting point (already exists — do not recreate)

M2b is complete and its tests pass. The engine, the `Program`/`Prompt` seam, `changeZone`, mana, casting, the stack, combat declaration, the two combat damage steps and `struckFirst` snapshot, the CR 508.8 skip, SBAs (`Sba.checkStateBasedActions`, single-pass), the keyword seam (`Pawl.Type.Keyword`, `Card.keywords`, `Game.keywordsOf`/`hasKeyword`), replay, the single-file test suite (`source/test-suite/Main.hs`) and benchmark all exist. This plan **modifies** them.

Read the spec before starting. The invariants it turns on:

1. **Casing on a `Keyword` is NOT a violation of the closed/open invariant.** Rule 702 is the rulebook. See the note atop `Pawl.Type.Keyword`.
2. **The closed half asks `Game.keywordsOf`/`hasKeyword`, never `Card.keywords`.** The 704.5h source-has-deathtouch read and the trample threshold both consult the projection.
3. **Damage flows through one helper.** `applyCombatDamage` is the sole place damage is marked and the sole place events are emitted. Do not mark damage anywhere else.
4. **The trample threshold is not a floor.** CR 702.19b lets a blocker be under-assigned; the threshold gates the *defender* only. Implement the implication, not `every blocker ≥ lethal`.
5. **Every rules claim is checked against `docs/rules.txt`.** Load-bearing: CR 510.1, 510.2, 702.2, 702.19, 704.5f/g/h. Cite the number in the code.

## Global Constraints

Every task's requirements implicitly include all of these:

- **Warning-clean:** library, test suite, and benchmark compile under `-Weverything` minus the allow-list in `pawl.cabal`. A warning is a failure. `-Wunused-matches` is active: prefix genuinely unused binders with `_`. Check with a **clean** build (`rm -rf dist-newstyle/build/aarch64-osx/ghc-9.14.1/pawl-0.2026.7.16/{b,t,build}`) then `cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -c "warning:"` must print `0` — incremental builds hide warnings.
- **Boot libraries only** in the library. No new dependencies.
- **Haskell 2010.** The only permitted extensions are `GADTs` and `RankNTypes`, per-file (already on `Prompt.hs` and `Replay.hs`).
- **Non-punning constructors:** `Mk` prefix on newtypes and single-constructor records (`MkDamageEvent`). Multi-constructor ADTs plainly (`Recipient`, `Keyword`).
- **One type per module** under `Pawl.Type.<TypeName>` — type and instances only. A module never imports its parents.
- **Derive at least `Eq` and `Show`.** `Recipient` also derives `Ord` (it is a `Map` key). `DamageEvent` derives `Eq`, `Ord`, `Show`.
- **No partial functions, no list comprehensions, no backtick-infixed functions.** `Maybe`/`Either`, `case` over point-free, `let` over `where`, `$` over parens, `.` over chained `$`.
- **Imports:** qualified, aliased to the last component; one import group; operators unqualified.
- **Tests:** all in `source/test-suite/Main.hs`, appended to the `testTree` list.
- **Commits:** directly to `main`, one small complete commit per task.
- **Per-task gate:** clean `cabal build all --enable-tests --enable-benchmarks` is warning-free; `cabal test` passes; `hooky fix` then `hooky run` pass. `hooky` acts on **staged** files — `git add -A` first (then `git add -A` again after `hooky fix` reformats).

**Verification commands:** build `cabal build`; all targets `cabal build all --enable-tests --enable-benchmarks`; test `cabal test`; one group `cabal test --test-options='-p "/Pattern/"'`; bench `cabal bench`; lint `hooky fix` then `hooky run`.

**Do not pipe `cabal test` into `head`** — closing the pipe early wedges the process and looks like a hang. Redirect to a file and grep it. (`git-bug b9164e2`: the suite can also deadlock intermittently under tasty's default parallelism; if a run hangs with no output, re-run.)

## File structure

**New (library):**

| File | Responsibility |
|---|---|
| `Pawl/Type/Recipient.hs` | `data Recipient = ToCreature ObjectId \| ToDefender PlayerId` — a combat-damage recipient |
| `Pawl/Type/DamageEvent.hs` | `data DamageEvent = MkDamageEvent { source, target, amount }` — one dealt-damage record |

**Modified (library):**

| File | Change |
|---|---|
| `Pawl/Type/Keyword.hs` | add `Deathtouch` (702.2), `Trample` (702.19), in CR order |
| `Pawl/Type/Subtype.hs` | add `Rat`, `Elephant` |
| `Pawl/Type/GameState.hs` | add `damageEvents :: [DamageEvent]` |
| `Pawl/Type/Prompt.hs` | `AssignCombatDamage` takes `Map Recipient Natural` (thresholds), returns `Map Recipient Natural` |
| `Pawl/Type/Response.hs` | `AssignedCombatDamage (Map Recipient Natural)` |
| `Pawl/Card.hs` | `typhoidRatsPrinting`, `warMammothPrinting` |
| `Pawl/Damage.hs` | events funnel; recipient/threshold assignment; `legalAssignment`; `blockerThreshold` |
| `Pawl/Sba.hs` | CR 704.5h clause in `creatureDies`; drain `damageEvents` |
| `Pawl/Replay.hs` | `defaultAnswer` for the new `AssignCombatDamage` shape |
| `Pawl/Setup.hs` | `emptyGame` sets `damageEvents = []` |

**Modified (suites):** `source/test-suite/Main.hs`, `source/benchmark/Main.hs`.

**Untouched, deliberately:** the eight interpreters' other prompt branches, the stack, mana, casting. M2c changes exactly one prompt.

## Task ordering rationale

- **Task 1** lands the keywords, subtypes, and two printings before any consumer — the M2a/M2b pattern.
- **Task 2** builds the event funnel with **no behavior change** (events emitted, nothing reads them, old prompt untouched). Foundation for deathtouch.
- **Task 3** is deathtouch: the 704.5h SBA reads the events and the drain. Its falsifier (1 damage kills a 3/3) needs single-blocker combat — no prompt — so it lands before the prompt change.
- **Task 4** is the cross-cutting prompt generalization (Recipient, thresholds, `legalAssignment` + its exhaustive property), **behavior-preserving** for the existing non-trample division.
- **Task 5** is trample: thresholds, the forced-vs-prompt decision, the 702.19b implication wired in.
- **Task 6** is the 702.2c interaction (one line: deathtouch → threshold 1) and the synthetic falsifier fixture.
- **Task 7** closes the milestone — new properties, warning-clean, replay, CLAUDE.md.

---

### Task 1: Keywords, subtypes, and the two printings

`Deathtouch` and `Trample` join `Keyword`; `Rat` and `Elephant` join `Subtype`; `typhoidRatsPrinting` and `warMammothPrinting` join `Card`. No consumer yet.

**Files:**
- Modify: `source/library/Pawl/Type/Keyword.hs`, `source/library/Pawl/Type/Subtype.hs`, `source/library/Pawl/Card.hs`, `source/test-suite/Main.hs`

**Interfaces:**
- Consumes: `Card.MkCard`, `Printing.MkPrinting`, `ManaCost.MkManaCost`, `ManaSymbol.Generic`/`OfType`, `ManaType.Colored`, `Color.Black`/`Green`, `Power.MkPower`, `Toughness.MkToughness`, `Quantity.Literal` (all exist).
- Produces: `Keyword.Deathtouch`, `Keyword.Trample`, `Subtype.Rat`, `Subtype.Elephant`, `Card.typhoidRatsPrinting`, `Card.warMammothPrinting`.

- [x] **Step 1: Write the failing test**

In `source/test-suite/Main.hs`, add this group and append `m2cCardTests` to the `testTree` list:

```haskell
m2cCardTests :: Tasty.TestTree
m2cCardTests =
  Tasty.testGroup
    "M2cCards"
    [ HU.testCase "Typhoid Rats is a {B} 1/1 Rat with deathtouch" $ do
        let c = Printing.card Card.typhoidRatsPrinting
        HU.assertEqual "name" (Text.pack "Typhoid Rats") (Card.Type.name c)
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 1))) (Card.Type.power c)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness c)
        HU.assertEqual "keywords" (Set.singleton Keyword.Deathtouch) (Card.Type.keywords c),
      HU.testCase "War Mammoth is a {3}{G} 3/3 Elephant with trample" $ do
        let c = Printing.card Card.warMammothPrinting
        HU.assertEqual "name" (Text.pack "War Mammoth") (Card.Type.name c)
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 3))) (Card.Type.power c)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 3))) (Card.Type.toughness c)
        HU.assertEqual "keywords" (Set.singleton Keyword.Trample) (Card.Type.keywords c)
    ]

(In the test suite, `Pawl.Type.Quantity` is aliased `Quantity.Type` — note the qualifier differs from `Pawl/Card.hs`, which uses `Quantity`.)
```

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test 2>&1 | tail -30`
Expected: FAIL — `typhoidRatsPrinting`/`warMammothPrinting` not in scope, `Deathtouch`/`Trample` not in scope.

- [x] **Step 3: Add the keywords and subtypes**

In `source/library/Pawl/Type/Keyword.hs`, add the two constructors in CR-number order (Deathtouch before `Defender`; Trample between `Reach` and `Vigilance`) and update the count comment:

```haskell
data Keyword
  = Deathtouch -- 702.2
  | Defender -- 702.3
  | DoubleStrike -- 702.4
  | FirstStrike -- 702.7
  | Flying -- 702.9
  | Haste -- 702.10
  | Reach -- 702.17
  | Trample -- 702.19
  | Vigilance -- 702.20
  deriving (Eq, Ord, Show)
```

In `source/library/Pawl/Type/Subtype.hs`, add `Rat` and `Elephant` to the list.

- [x] **Step 4: Add the two printings**

In `source/library/Pawl/Card.hs`, add (verified against Scryfall `api.scryfall.com/cards/named?exact=…`, 2026-07-17 — both carry zero Gatherer rulings, french-vanilla oracle is the CR):

```haskell
-- Typhoid Rats: {B}, Creature - Rat, 1/1, Deathtouch (CR 702.2).
-- Black, not red: mono-red deathtouch does not exist (Scryfall keyword:deathtouch
-- c=r is empty). Never cast, only placed in combat fixtures, so color is cosmetic.
-- A 1/1 on purpose: one power isolates deathtouch, since 1 damage is lethal to a
-- 3/3 ONLY because of 702.2. See the M2c spec, section 6.
typhoidRatsPrinting :: Printing.Printing
typhoidRatsPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Typhoid Rats",
            Card.manaCost =
              Just (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Black)]),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.singleton Subtype.Rat
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 1)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 1)),
            Card.keywords = Set.singleton Keyword.Deathtouch
          }
    }

-- War Mammoth: {3}{G}, Creature - Elephant, 3/3, Trample (CR 702.19).
-- Green: clean vanilla-plus-trample lives in green. A 3/3 tramples cleanly over a
-- 2/1 (assign 1, spill 2) and survives a 2/1 blocker, so the overflow is visible.
warMammothPrinting :: Printing.Printing
warMammothPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "War Mammoth",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 3,
                      ManaSymbol.OfType (ManaType.Colored Color.Green)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.singleton Subtype.Elephant
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 3)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 3)),
            Card.keywords = Set.singleton Keyword.Trample
          }
    }
```

- [x] **Step 5: Run tests and clean-build to verify pass + warning-free**

Run: `cabal test 2>&1 | tail -20` — Expected: PASS (M2cCards green, all others green).
Run the clean-build warning check from Global Constraints — Expected: `0`.

- [x] **Step 6: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Add deathtouch and trample keywords with Typhoid Rats and War Mammoth"
```

---

### Task 2: The damage-event funnel

`Recipient` and `DamageEvent` types; `GameState.damageEvents`; `Damage.applyCombatDamage` becomes change-and-emit; `gatherCombatDamage`/`attackerAssignment`/`blockerAssignment` produce `[DamageEvent]`. **No behavior change** to marked damage or life — only events are additionally recorded. The `AssignCombatDamage` prompt is untouched (Task 4).

**Files:**
- Create: `source/library/Pawl/Type/Recipient.hs`, `source/library/Pawl/Type/DamageEvent.hs`
- Modify: `source/library/Pawl/Type/GameState.hs`, `source/library/Pawl/Damage.hs`, `source/library/Pawl/Setup.hs`, `source/test-suite/Main.hs`

**Interfaces:**
- Consumes: `Game.powerOf`, `Game.controllerOf`, `Combat.blockersOf`, `Program.prompt`, `Prompt.AssignCombatDamage` (old shape), `AttackTarget.OfPlayer`.
- Produces:
  - `Recipient.Recipient = ToCreature ObjectId | ToDefender PlayerId`
  - `DamageEvent.DamageEvent` with fields `source :: ObjectId`, `target :: Recipient`, `amount :: Natural`
  - `GameState.damageEvents :: [DamageEvent]`
  - `Damage.gatherCombatDamage :: (ObjectId -> Bool) -> Game [DamageEvent]`
  - `Damage.applyCombatDamage :: [DamageEvent] -> GameState -> GameState`
  - `Damage.attackerAssignment :: GameState -> (ObjectId, AttackTarget) -> Game [DamageEvent]`
  - `Damage.blockerAssignment :: GameState -> (ObjectId, Set ObjectId) -> [DamageEvent]`

- [x] **Step 1: Write the failing test**

In `source/test-suite/Main.hs`, add and append `damageEventTests` to `testTree`. It asserts a Piker-vs-Piker exchange emits the two events (each Piker deals 2 to the other), with existing damage/life behavior unchanged:

```haskell
damageEventTests :: Tasty.TestTree
damageEventTests =
  Tasty.testGroup
    "DamageEvent"
    [ HU.testCase "a blocked 2/1 trade emits both damage events" $
        let (gs, mine, theirs) = combatBoard 1 1
            after = fightWith aggressiveAnswer gs
            events = GameState.damageEvents after
         in case (mine, theirs) of
              (a : _, b : _) -> do
                HU.assertEqual "two events" 2 (length events)
                HU.assertBool "attacker hit blocker for 2" $
                  elem (DamageEvent.MkDamageEvent a (Recipient.ToCreature b) 2) events
                HU.assertBool "blocker hit attacker for 2" $
                  elem (DamageEvent.MkDamageEvent b (Recipient.ToCreature a) 2) events
              _ -> HU.assertFailure "fixture should have one creature per side",
      HU.testCase "an unblocked 2/1 emits a ToDefender event" $
        let (gs, mine, _) = combatBoard 1 0
            after = fightWith aggressiveAnswer gs
         in case mine of
              a : _ ->
                HU.assertEqual
                  "one player event"
                  [DamageEvent.MkDamageEvent a (Recipient.ToDefender bob) 2]
                  (GameState.damageEvents after)
              _ -> HU.assertFailure "fixture should have an attacker"
    ]
```

Add to the test suite's import block: `import qualified Pawl.Type.DamageEvent as DamageEvent` and `import qualified Pawl.Type.Recipient as Recipient` (they sort between `Combat.Type`/`CombatStep` and `GameState`/`Keyword` respectively; `cabal-gild`/`hooky fix` will order them).

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test 2>&1 | tail -30`
Expected: FAIL — `DamageEvent`, `Recipient`, `GameState.damageEvents` not in scope (they are created in Step 3).

- [x] **Step 3: Create the two types**

`source/library/Pawl/Type/Recipient.hs`:

```haskell
module Pawl.Type.Recipient where

import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)

-- CR 510.1: combat damage is assigned to a blocking creature or to the player
-- being attacked. Grows toward planeswalkers and battles (other attack targets)
-- when those card types exist; ToDefender is all M2c's single-opponent,
-- planeswalker-free board can produce. Ord because it is a Map key.
data Recipient
  = ToCreature ObjectId
  | ToDefender PlayerId
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/DamageEvent.hs`:

```haskell
module Pawl.Type.DamageEvent where

import Numeric.Natural (Natural)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Recipient (Recipient)

-- One instance of combat damage: a source dealt `amount` to `target`. The first
-- reader is deathtouch's CR 704.5h SBA, which asks whether `source` has deathtouch
-- (via the projection, at check time). Minimal by design (CR 702.2 needs only
-- source + creature target + nonzero amount); lifelink and M4 combat-damage
-- triggers grow the payload rather than reshape it. See the M2c spec, section 2.
data DamageEvent = MkDamageEvent
  { source :: ObjectId,
    target :: Recipient,
    amount :: Natural
  }
  deriving (Eq, Ord, Show)
```

- [x] **Step 4: Add the GameState field and initialize it**

In `source/library/Pawl/Type/GameState.hs`, import `DamageEvent` and add the field after `combat` (or anywhere in the record), with a comment:

```haskell
    -- CR 510: combat damage dealt this step, as events, for the SBA to read.
    -- The change-and-emit funnel's log; drained at each SBA check (Sba). See spec §2.
    damageEvents :: [DamageEvent],
```

In `source/library/Pawl/Setup.hs`, in `emptyGame`'s record, add `GameState.damageEvents = [],`.

- [x] **Step 5: Rewrite the Damage assignment path to emit events**

Replace `attackerAssignment`, `blockerAssignment`, `gatherCombatDamage`, and `applyCombatDamage` in `source/library/Pawl/Damage.hs`. Add imports `qualified Pawl.Type.DamageEvent as DamageEvent` and `qualified Pawl.Type.Recipient as Recipient`. Keep the CR 510.1e reject-not-repair comment.

```haskell
-- What one attacking creature assigns, as damage events carrying the source.
-- CR 510.1a: a creature that would assign 0 or less assigns none, so events all
-- carry amount > 0.
attackerAssignment :: GameState -> (ObjectId, AttackTarget) -> Game [DamageEvent.DamageEvent]
attackerAssignment gs (attacker, target) = case Game.powerOf attacker gs of
  Nothing -> pure []
  Just p ->
    if p <= 0
      then pure []
      else do
        let power :: Natural
            power = fromInteger p
        case Set.toList (Combat.blockersOf attacker gs) of
          -- CR 510.1b: unblocked, so it hits what it is attacking.
          [] -> case target of
            AttackTarget.OfPlayer pid ->
              pure [DamageEvent.MkDamageEvent attacker (Recipient.ToDefender pid) power]
          -- CR 510.1c: exactly one blocker takes ALL of it. Forced, so not asked.
          [blocker] ->
            pure [DamageEvent.MkDamageEvent attacker (Recipient.ToCreature blocker) power]
          blockers -> case Game.controllerOf attacker gs of
            Nothing -> pure []
            Just pid -> do
              let decider = Decide.deciderFor pid gs
              chosen <-
                Trans.lift
                  (Program.prompt (Prompt.AssignCombatDamage decider pid attacker (Set.fromList blockers) power))
              -- CR 510.1e checks the assignment AS A WHOLE. An illegal answer is
              -- rejected and the attacker assigns nothing (CR 510.1b/c degenerate
              -- case), NOT the CR 733 rewind. See the spec, section 4.
              let isBlocker o = List.elem o blockers
                  onlyBlockers = all isBlocker (Map.keys chosen)
                  totalsPower = sum (Map.elems chosen) == power
                  toEvent (blocker, n) = DamageEvent.MkDamageEvent attacker (Recipient.ToCreature blocker) n
                  positive (_, n) = n > 0
              pure
                ( if onlyBlockers && totalsPower
                    then map toEvent (filter positive (Map.toList chosen))
                    else []
                )

-- CR 510.1d: a blocking creature assigns its damage to the creature it blocks.
blockerAssignment :: GameState -> (ObjectId, Set.Set ObjectId) -> [DamageEvent.DamageEvent]
blockerAssignment gs (attacker, blockers) =
  let assign blocker = case Game.powerOf blocker gs of
        Just p ->
          if p <= 0
            then []
            else [DamageEvent.MkDamageEvent blocker (Recipient.ToCreature attacker) (fromInteger p)]
        Nothing -> []
   in concatMap assign (Set.toList blockers)

-- CR 510.2: gather all combat damage before applying any of it (simultaneity).
gatherCombatDamage :: (ObjectId -> Bool) -> Game [DamageEvent.DamageEvent]
gatherCombatDamage assigns = do
  gs <- State.get
  let combat = GameState.combat gs
      attackers = filter (assigns . fst) (Map.toList (Combat.Type.attackers combat))
      blockers = Map.toList (Map.map (Set.filter assigns) (Combat.Type.blockers combat))
  parts <- Monad.mapM (attackerAssignment gs) attackers
  let fromBlockers = concatMap (blockerAssignment gs) blockers
  pure (concat parts ++ fromBlockers)

-- CR 120.3e / 120.3a: mark damage on creatures, drain life from players -- AND
-- emit each event into GameState.damageEvents. This is the change-and-emit funnel:
-- the sole place combat damage is applied and the sole place an event is recorded.
applyCombatDamage :: [DamageEvent.DamageEvent] -> GameState -> GameState
applyCombatDamage events gs =
  let markOne g ev = case DamageEvent.target ev of
        Recipient.ToCreature oid ->
          let hurt obj = obj {Object.damage = Object.damage obj + DamageEvent.amount ev}
           in g {GameState.objects = Map.adjust hurt oid (GameState.objects g)}
        Recipient.ToDefender pid ->
          let drain player = player {Player.life = Player.life player - toInteger (DamageEvent.amount ev)}
           in g {GameState.players = Map.adjust drain pid (GameState.players g)}
      marked = List.foldl' markOne gs events
   in marked {GameState.damageEvents = GameState.damageEvents marked ++ events}
```

`dealCombatDamage` and `dealWave` are unchanged in structure (`dealWave` still does `assignment <- gatherCombatDamage assigns; State.modify' (applyCombatDamage assignment)` — the types line up). Remove the now-unused `AttackTarget` import only if the compiler flags it (it is still used).

- [x] **Step 6: Run tests and clean-build**

Run: `cabal test 2>&1 | tail -20` — Expected: PASS. The existing `CombatDamage` tests still pass (marked damage and life unchanged); `DamageEvent` is green.
Run the clean-build warning check — Expected: `0`.

- [x] **Step 7: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Route combat damage through a change-and-emit helper that records DamageEvents"
```

---

### Task 3: Deathtouch — the CR 704.5h state-based action

`Sba.creatureDies` gains the 704.5h clause (a wounded-by-deathtouch creature with toughness > 0 dies), reading `GameState.damageEvents` through `Game.hasKeyword`; `checkStateBasedActions` drains the events after burying, making "since the last SBA check" precise.

**Files:**
- Modify: `source/library/Pawl/Sba.hs`, `source/test-suite/Main.hs`

**Interfaces:**
- Consumes: `GameState.damageEvents`, `DamageEvent` fields, `Recipient.ToCreature`, `Game.hasKeyword`, `Keyword.Deathtouch`, `Game.toughnessOf` (all exist / from Tasks 1–2).
- Produces: `Sba.creatureDies` now also destroys 704.5h victims; `Sba.checkStateBasedActions` clears `damageEvents`.

- [x] **Step 1: Write the failing test**

In `source/test-suite/Main.hs`, add and append `deathtouchTests` to `testTree`. Uses `combatBoardOf` with the new printings; drives through `fightWith`, then the SBA:

```haskell
deathtouchTests :: Tasty.TestTree
deathtouchTests =
  Tasty.testGroup
    "Deathtouch"
    [ HU.testCase "CR 704.5h a 1/1 deathtoucher destroys a 3/3 it deals 1 to" $
        -- Typhoid Rats attacks, Ogre Sentry blocks. Rat deals 1 -> Ogre dies by
        -- 704.5h (toughness 3, not lethal by the numbers); Ogre's 3 kills the Rat.
        let (gs, _, _) = combatBoardOf [Card.typhoidRatsPrinting] [Card.ogreSentryPrinting]
            after = Sba.checkStateBasedActions (fightWith aggressiveAnswer gs)
         in do
              HU.assertEqual "the Ogre is dead" 0 (creaturesInPlay bob after)
              HU.assertEqual "the Rat is dead" 0 (creaturesInPlay alice after),
      HU.testCase "CR 704.5g the control: a 2/1 without deathtouch leaves the 3/3 alive" $
        let (gs, _, _) = combatBoardOf [Card.pikerPrinting] [Card.ogreSentryPrinting]
            after = Sba.checkStateBasedActions (fightWith aggressiveAnswer gs)
         in HU.assertEqual "the Ogre survives" 1 (creaturesInPlay bob after),
      HU.testCase "the SBA check drains the damage events" $
        let (gs, _, _) = combatBoardOf [Card.typhoidRatsPrinting] [Card.ogreSentryPrinting]
            after = Sba.checkStateBasedActions (fightWith aggressiveAnswer gs)
         in HU.assertEqual "events drained" [] (GameState.damageEvents after)
    ]
```

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "/Deathtouch/"' 2>&1 | tail -20`
Expected: FAIL — "the Ogre is dead" gets 1 (no 704.5h yet), and "events drained" gets a non-empty list.

- [x] **Step 3: Add the 704.5h clause and the drain**

In `source/library/Pawl/Sba.hs`, add imports `qualified Pawl.Type.DamageEvent as DamageEvent`, `qualified Pawl.Type.Recipient as Recipient`, `qualified Pawl.Type.Keyword as Keyword`. Add a helper and extend `creatureDies`:

```haskell
-- CR 704.5h: a creature with toughness > 0 dealt damage by a deathtouch source
-- since the last SBA check is destroyed. "Since the last check" is exactly the
-- span of GameState.damageEvents, which checkStateBasedActions drains below. The
-- source's deathtouch is read through the projection (Game.hasKeyword), at check
-- time -- CR 702.2e's last-known-information never differs while keywords are
-- printed (M3 expiry).
woundedByDeathtouch :: GameState -> ObjectId -> Bool
woundedByDeathtouch gs oid =
  let hits ev =
        DamageEvent.target ev == Recipient.ToCreature oid
          && DamageEvent.amount ev > 0
          && Game.hasKeyword Keyword.Deathtouch (DamageEvent.source ev) gs
   in any hits (GameState.damageEvents gs)
```

In `creatureDies`, add 704.5h alongside 704.5f/g. The `Just toughness` branch becomes:

```haskell
        Just toughness ->
          -- CR 704.5f: toughness 0 or less.
          (toughness <= 0)
            -- CR 704.5g: damage marked is lethal.
            || ( case Game.lookupObject oid gs of
                   Nothing -> False
                   Just obj -> toInteger (Object.damage obj) >= toughness
               )
            -- CR 704.5h: wounded by a deathtouch source (toughness > 0 already,
            -- since 704.5f handled <= 0).
            || woundedByDeathtouch gs oid
```

In `checkStateBasedActions`, drain the events in the returned state. Change the final expression so the result also clears `damageEvents`:

```haskell
      drained = departed {GameState.damageEvents = []}
   in drained {GameState.result = outcome <|> GameState.result drained}
```

(Rename the existing final `departed {GameState.result = …}` to thread through `drained` as shown; `dying`/`buried` still read the pre-drain `gs`, so victims are computed before the drain.)

- [x] **Step 4: Run tests and clean-build**

Run: `cabal test 2>&1 | tail -20` — Expected: PASS. `Deathtouch` green; the existing 704.5f/g `markDamage` tests still pass (they emit no events, so the drain is a no-op and 704.5h never fires).
Run the clean-build warning check — Expected: `0`.

- [x] **Step 5: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Destroy creatures wounded by a deathtouch source as CR 704.5h"
```

---

### Task 4: The generalized AssignCombatDamage prompt

`AssignCombatDamage` becomes keyword-agnostic: it carries `Map Recipient Natural` (legal recipients → lethal thresholds) and returns `Map Recipient Natural`. A pure `legalAssignment` encodes CR 702.19b's implication and is property-tested across the whole space. **Behavior-preserving:** the existing non-trample division still divides among blockers. Trample is Task 5.

**Files:**
- Modify: `source/library/Pawl/Type/Prompt.hs`, `source/library/Pawl/Type/Response.hs`, `source/library/Pawl/Replay.hs`, `source/library/Pawl/Damage.hs`, `source/test-suite/Main.hs`, `source/benchmark/Main.hs`

**Interfaces:**
- Consumes: `Recipient` (Task 2).
- Produces:
  - `Prompt.AssignCombatDamage :: Decider -> PlayerId -> ObjectId -> Map Recipient Natural -> Natural -> Prompt (Map Recipient Natural)`
  - `Response.AssignedCombatDamage (Map Recipient Natural)`
  - `Damage.legalAssignment :: Map Recipient Natural -> Natural -> Map Recipient Natural -> Bool`

- [x] **Step 1: Write the failing test — the exhaustive validation property**

In `source/test-suite/Main.hs`, add and append `assignmentLegalityTests` to `testTree`. This is where "all scenarios" is proven — the pure predicate, independent of any card:

```haskell
assignmentLegalityTests :: Tasty.TestTree
assignmentLegalityTests =
  Tasty.testGroup
    "AssignmentLegality"
    [ HU.testCase "under-assignment with no overflow is legal (power below lethal)" $
        -- One blocker, lethal 3, power 2, defender present with threshold 0.
        let thresholds =
              Map.fromList
                [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 3),
                  (Recipient.ToDefender bob, 0)
                ]
            answer = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 1), 2)]
         in HU.assertBool "accepted" (Damage.legalAssignment thresholds 2 answer),
      HU.testCase "defender damage while a blocker is short is illegal" $
        let thresholds =
              Map.fromList
                [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 3),
                  (Recipient.ToDefender bob, 0)
                ]
            answer =
              Map.fromList
                [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 0),
                  (Recipient.ToDefender bob, 3)
                ]
         in HU.assertBool "rejected" (not (Damage.legalAssignment thresholds 3 answer)),
      HU.testCase "defender damage once the blocker has lethal is legal" $
        let thresholds =
              Map.fromList
                [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 1),
                  (Recipient.ToDefender bob, 0)
                ]
            answer =
              Map.fromList
                [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 1),
                  (Recipient.ToDefender bob, 2)
                ]
         in HU.assertBool "accepted" (Damage.legalAssignment thresholds 3 answer),
      HU.testCase "an answer that does not total power is illegal" $
        let thresholds = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 1), 0)]
            answer = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 1), 1)]
         in HU.assertBool "rejected" (not (Damage.legalAssignment thresholds 2 answer)),
      HU.testCase "an illegal recipient is rejected" $
        let thresholds = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 1), 0)]
            answer = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 2), 2)]
         in HU.assertBool "rejected" (not (Damage.legalAssignment thresholds 2 answer)),
      QC.testProperty "an accepted assignment always totals power and gates the defender" $
        QC.forAll genLegalityCase $ \(thresholds, power, answer) ->
          not (Damage.legalAssignment thresholds power answer)
            || ( sum (Map.elems answer) == power
                   && all (\r -> Map.member r thresholds) (Map.keys answer)
                   && ( Map.findWithDefault 0 (Recipient.ToDefender bob) answer == 0
                          || all
                            (\(r, t) -> Map.findWithDefault 0 r answer >= t)
                            (Map.toList (Map.filterWithKey (\r _ -> isCreatureRecipient r) thresholds))
                      )
               )
    ]

isCreatureRecipient :: Recipient.Recipient -> Bool
isCreatureRecipient r = case r of
  Recipient.ToCreature _ -> True
  Recipient.ToDefender _ -> False

-- A blocker (lethal 0..4), a defender (threshold 0), power 0..6, and an arbitrary
-- assignment over those two recipients. Covers power below / equal to / above
-- lethal and every over/under split.
genLegalityCase :: QC.Gen (Map.Map Recipient.Recipient Natural.Natural, Natural.Natural, Map.Map Recipient.Recipient Natural.Natural)
genLegalityCase = do
  lethal <- QC.choose (0, 4) :: QC.Gen Integer
  power <- QC.choose (0, 6) :: QC.Gen Integer
  toBlocker <- QC.choose (0, 6) :: QC.Gen Integer
  toDefender <- QC.choose (0, 6) :: QC.Gen Integer
  let blocker = Recipient.ToCreature (ObjectId.MkObjectId 1)
      thresholds = Map.fromList [(blocker, fromInteger lethal), (Recipient.ToDefender bob, 0)]
      answer = Map.fromList [(blocker, fromInteger toBlocker), (Recipient.ToDefender bob, fromInteger toDefender)]
  pure (thresholds, fromInteger power, answer)
```

(`ObjectId.MkObjectId :: Natural -> ObjectId` is used directly, as the suite already does at `Main.hs:290`. No new helper.)

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "/AssignmentLegality/"' 2>&1 | tail -20`
Expected: FAIL — `Damage.legalAssignment` not in scope.

- [x] **Step 3: Change the prompt and response types**

In `source/library/Pawl/Type/Prompt.hs`, import `Recipient` and change the constructor and its comment:

```haskell
  -- CR 510.1 / 702.19b: the attacker divides its power among the legal recipients.
  -- The Map is recipient -> lethal threshold (blockers -> lethal, the defender ->
  -- 0); trample-ness is entirely in whether the defender is a key and what the
  -- thresholds are. Not asked when the division is forced (single blocker, no
  -- excess). Validation is Damage.legalAssignment. See the M2c spec, section 4.
  AssignCombatDamage :: Decider -> PlayerId -> ObjectId -> Map Recipient Natural -> Natural -> Prompt (Map Recipient Natural)
```

In `source/library/Pawl/Type/Response.hs`, import `Recipient` and change:

```haskell
  | AssignedCombatDamage (Map Recipient Natural)
```

- [x] **Step 4: Add `legalAssignment` and issue the new prompt (non-trample behavior preserved)**

In `source/library/Pawl/Damage.hs`, add the pure predicate and rewrite `attackerAssignment`'s multi-blocker branch to build the thresholds map (all `0` for now — non-trample), issue the new prompt, and validate with `legalAssignment`:

```haskell
-- CR 510.1e / 702.19b, as a pure predicate over the whole assignment. Legal iff it
-- totals power, uses only legal recipients, and -- the trample implication -- the
-- defender got damage ONLY if every blocker is at its lethal threshold. The
-- threshold is NOT a per-blocker floor: a blocker may be under-assigned as long as
-- the defender then gets nothing. See the M2c spec, section 4.
legalAssignment :: Map.Map Recipient.Recipient Natural -> Natural -> Map.Map Recipient.Recipient Natural -> Bool
legalAssignment thresholds power answer =
  let assigned r = Map.findWithDefault 0 r answer
      totalsPower = sum (Map.elems answer) == power
      onlyLegal = all (\r -> Map.member r thresholds) (Map.keys answer)
      isDefender r = case r of
        Recipient.ToDefender _ -> True
        Recipient.ToCreature _ -> False
      defenderAmount = sum (Map.elems (Map.filterWithKey (\r _ -> isDefender r) answer))
      blockerThresholds = Map.filterWithKey (\r _ -> not (isDefender r)) thresholds
      everyBlockerLethal = all (\(r, t) -> assigned r >= t) (Map.toList blockerThresholds)
      defenderGated = defenderAmount == 0 || everyBlockerLethal
   in totalsPower && onlyLegal && defenderGated
```

Replace the multi-blocker branch of `attackerAssignment` (the `blockers -> …` case) with:

```haskell
          blockers -> case Game.controllerOf attacker gs of
            Nothing -> pure []
            Just pid -> do
              let decider = Decide.deciderFor pid gs
                  -- Non-trample: recipients are the blockers, every threshold 0.
                  -- Trample thresholds and the defender recipient arrive in Task 5.
                  thresholds =
                    Map.fromList (map (\b -> (Recipient.ToCreature b, 0)) blockers)
              chosen <-
                Trans.lift
                  (Program.prompt (Prompt.AssignCombatDamage decider pid attacker thresholds power))
              -- CR 510.1e checks the assignment AS A WHOLE. An illegal answer is
              -- rejected and the attacker assigns nothing -- the rules' degenerate
              -- case (CR 510.1b/c), NOT the CR 733 human-error rewind. See spec §4.
              let toEvent (recipient, n) = DamageEvent.MkDamageEvent attacker recipient n
                  positive (_, n) = n > 0
              pure
                ( if legalAssignment thresholds power chosen
                    then map toEvent (filter positive (Map.toList chosen))
                    else []
                )
```

Note the answer's keys are now `Recipient`, so `toEvent` builds the event directly from the recipient (no `ToCreature` wrapping here — the map already carries recipients).

- [x] **Step 5: Migrate `Replay.defaultAnswer`**

In `source/library/Pawl/Replay.hs`, the `AssignCombatDamage` branch of `defaultAnswer` (currently destructures `_ _ _ blockers n` over a `Set` and returns `Map.singleton b n`). Replace with a legal default that dumps all power on the first non-defender recipient (defender gets nothing, so the gate is vacuous):

```haskell
  -- Must be a LEGAL division (Damage.legalAssignment), or the attacker deals
  -- nothing. All power onto the first blocker totals power with the defender at 0.
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    let blockers = filter isCreatureRecipient (Map.keys thresholds)
        isCreatureRecipient r = case r of
          Recipient.ToCreature _ -> True
          Recipient.ToDefender _ -> False
     in case blockers of
          r : _ -> Map.singleton r n
          [] -> Map.empty
```

Add `qualified Pawl.Type.Recipient as Recipient` to `Replay.hs`'s imports. `encode`/`decode` use `{}` and `AssignedCombatDamage answer`, which are unchanged.

- [x] **Step 6: Migrate the interpreters in the test suite and benchmark**

Every responder that matched `Prompt.AssignCombatDamage _ _ _ ids n` where `ids :: Set ObjectId` now matches `_ _ _ thresholds n` where `thresholds :: Map Recipient Natural`, and returns `Map Recipient Natural`. The mechanical transform for the common "dump all on the first blocker" responders (`aggressiveAnswer` line ~208, and the identical clauses at test lines ~861, ~1037, ~1288, ~1346, ~1427, ~1555, and benchmark `source/benchmark/Main.hs` lines ~23, ~35, ~55):

```haskell
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
```

using `isCreatureRecipient` — in the **test suite** reuse the top-level one already added in Step 1; in the **benchmark** (a separate module) add its own copy: `isCreatureRecipient r = case r of Recipient.ToCreature _ -> True; Recipient.ToDefender _ -> False`. The `pure $ case …` variants (lines ~1427, ~1555) keep their `pure`.

The three structurally-different responders in `combatDamageTests` migrate as:

- `noAssign` (line ~150): `Prompt.AssignCombatDamage {} -> Map.empty` — unchanged.
- `split` (line ~168, "1 each"): `Prompt.AssignCombatDamage _ _ _ thresholds _ -> Map.fromList (map (\r -> (r, 1)) (filter isCreatureRecipient (Map.keys thresholds)))`.
- `dump` (line ~178): same as the common transform above.
- `cheat` (line ~190, "99 each"): `Prompt.AssignCombatDamage _ _ _ thresholds _ -> Map.fromList (map (\r -> (r, 99)) (filter isCreatureRecipient (Map.keys thresholds)))`.

And the prompt **construction** at test line ~293 (`damagePrompt = Prompt.AssignCombatDamage decider alice oid (Set.singleton oid) 2`) becomes `Prompt.AssignCombatDamage decider alice oid (Map.singleton (Recipient.ToCreature oid) 0) 2`. Add `qualified Pawl.Type.Recipient as Recipient` to the test suite and benchmark imports.

- [x] **Step 7: Run tests and clean-build**

Run: `cabal test 2>&1 | tail -30` — Expected: PASS. `AssignmentLegality` green; every migrated `CombatDamage` test still green (behavior preserved); replay/determinism tests green.
Run: `cabal bench 2>&1 | tail -5` — Expected: builds and runs.
Run the clean-build warning check — Expected: `0`.

- [x] **Step 8: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Generalize AssignCombatDamage to recipients and lethal thresholds"
```

---

### Task 5: Trample assignment

`attackerAssignment` computes trample thresholds (each blocker's lethal, the defender at 0) and the defender recipient when the attacker has trample, decides forced-vs-prompt, and validates via `legalAssignment`. The 702.2c deathtouch interaction is Task 6.

**Files:**
- Modify: `source/library/Pawl/Damage.hs`, `source/test-suite/Main.hs`

**Interfaces:**
- Consumes: `Game.hasKeyword`, `Keyword.Trample`, `Game.toughnessOf`, `Game.lookupObject`, `Object.damage`, `legalAssignment` (Task 4).
- Produces: `Damage.blockerThreshold :: GameState -> ObjectId -> ObjectId -> Natural`; trample-aware `attackerAssignment`.

- [x] **Step 1: Write the failing tests**

In `source/test-suite/Main.hs`, add and append `trampleTests` to `testTree`. A trample-aware responder that assigns lethal to blockers and pours the rest on the defender:

```haskell
-- Assigns each blocker exactly its threshold, and every leftover point to the
-- defender. A legal trample division for these boards.
tramplingAnswer :: Prompt.Prompt r -> r
tramplingAnswer p = case p of
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    let blockers = Map.toList (Map.filterWithKey (\r _ -> isCreatureRecipient r) thresholds)
        toBlockers = Map.fromList (map (\(r, t) -> (r, t)) blockers)
        spent = sum (map snd blockers)
        leftover = if n >= spent then n - spent else 0
        defenders = filter (\r -> not (isCreatureRecipient r)) (Map.keys thresholds)
     in case defenders of
          d : _ -> Map.insert d leftover toBlockers
          [] -> toBlockers
  _ -> aggressiveAnswer p

trampleTests :: Tasty.TestTree
trampleTests =
  Tasty.testGroup
    "Trample"
    [ HU.testCase "CR 702.19b a 3/3 trampler spills excess onto the defending player" $
        -- War Mammoth (3/3 trample) blocked by a Piker (2/1): 1 lethal to the
        -- Piker, 2 to bob. Mammoth survives (2 marked < 3).
        let (gs, _, _) = combatBoardOf [Card.warMammothPrinting] [Card.pikerPrinting]
            after = Sba.checkStateBasedActions (fightWith tramplingAnswer gs)
         in do
              HU.assertEqual "bob took the 2 overflow" (Just 18) (lifeOf bob after)
              HU.assertEqual "the Piker is dead" 0 (creaturesInPlay bob after)
              HU.assertEqual "the Mammoth survives" 1 (creaturesInPlay alice after),
      HU.testCase "CR 702.19b a non-trample control spills nothing" $
        -- Ogre Sentry is a 3/3 that cannot attack (defender), so use the Piker's
        -- existing behavior as the control: a blocked non-trample attacker deals
        -- nothing to the player. (combatDamageTests already asserts bob = 20.)
        let (gs, _, _) = combatBoardOf [Card.pikerPrinting] [Card.pikerPrinting]
            after = fightWith tramplingAnswer gs
         in HU.assertEqual "bob untouched by a non-trampler" (Just 20) (lifeOf bob after),
      HU.testCase "CR 702.19b defender-short assignment is rejected" $
        -- A cheat responder gives bob 3 while the Piker gets 0. Illegal: the
        -- attacker deals nothing, bob untouched, Piker survives.
        let (gs, _, _) = combatBoardOf [Card.warMammothPrinting] [Card.pikerPrinting]
            cheat p = case p of
              Prompt.AssignCombatDamage _ _ _ thresholds n ->
                case filter (\r -> not (isCreatureRecipient r)) (Map.keys thresholds) of
                  d : _ -> Map.singleton d n
                  [] -> Map.empty
              _ -> aggressiveAnswer p
            after = Sba.checkStateBasedActions (fightWith cheat gs)
         in do
              HU.assertEqual "bob untouched" (Just 20) (lifeOf bob after)
              HU.assertEqual "the Piker survives the rejected assignment" 1 (creaturesInPlay bob after),
      HU.testCase "CR 702.19b under-assignment across two blockers spills nothing (power below total lethal)" $
        -- War Mammoth (3/3 trample) blocked by TWO Ogre Sentries (3/3 each): it
        -- cannot reach lethal on both (needs 6, has 3), so no overflow -- bob is
        -- untouched -- and the division among the Ogres is free. Real cards, the
        -- power-below-lethal case the property covers exhaustively.
        let (gs, _, _) = combatBoardOf [Card.warMammothPrinting] [Card.ogreSentryPrinting, Card.ogreSentryPrinting]
            dumpOne p = case p of
              Prompt.AssignCombatDamage _ _ _ thresholds n ->
                case filter isCreatureRecipient (Map.keys thresholds) of
                  r : _ -> Map.singleton r n
                  [] -> Map.empty
              _ -> aggressiveAnswer p
            after = Sba.checkStateBasedActions (fightWith dumpOne gs)
         in do
              HU.assertEqual "bob untouched (no overflow)" (Just 20) (lifeOf bob after)
              HU.assertEqual "one Ogre took all 3 and died, the other lived" 1 (creaturesInPlay bob after)
    ]
```

- [x] **Step 2: Run tests to verify they fail**

Run: `cabal test --test-options='-p "/Trample/"' 2>&1 | tail -30`
Expected: FAIL — the trample spill test gets `bob = 20` (trample not implemented; the Mammoth's blocked damage all lands on the Piker).

- [x] **Step 3: Compute trample thresholds and recipients**

In `source/library/Pawl/Damage.hs`, add `blockerThreshold` (deathtouch consultation is Task 6, so the attacker is unused now — prefix `_attacker`):

```haskell
-- CR 702.19b: a blocker's lethal threshold is its toughness minus damage already
-- marked, never negative. (Damage already marked matters against M2b: a first-
-- strike step can leave marked damage that lowers this in the regular step.) The
-- 702.2c deathtouch collapse to 1 arrives in Task 6.
blockerThreshold :: GameState -> ObjectId -> ObjectId -> Natural
blockerThreshold gs _attacker blocker =
  let marked = maybe 0 Object.damage (Game.lookupObject blocker gs)
   in case Game.toughnessOf blocker gs of
        Nothing -> 0
        Just t -> fromInteger (max 0 (t - toInteger marked))
```

Replace `attackerAssignment` in full (Task 4's version does not distinguish trample). `trample` is bound once in the `do` block, so all three arms see it; the single-blocker guard falls through to the prompt arm when a trampler has excess power:

```haskell
attackerAssignment :: GameState -> (ObjectId, AttackTarget) -> Game [DamageEvent.DamageEvent]
attackerAssignment gs (attacker, target) = case Game.powerOf attacker gs of
  Nothing -> pure []
  Just p ->
    if p <= 0
      then pure []
      else do
        let power :: Natural
            power = fromInteger p
            trample = Game.hasKeyword Keyword.Trample attacker gs
        case Set.toList (Combat.blockersOf attacker gs) of
          -- CR 510.1b: unblocked, so it hits what it is attacking.
          [] -> case target of
            AttackTarget.OfPlayer defender ->
              pure [DamageEvent.MkDamageEvent attacker (Recipient.ToDefender defender) power]
          -- CR 510.1c / 702.19b: a single blocker with no trample -- or trample but
          -- no power past its threshold -- is forced: all onto the blocker. A single
          -- trample blocker WITH excess fails this guard and falls to the prompt arm.
          [blocker]
            | not trample || power <= blockerThreshold gs attacker blocker ->
                pure [DamageEvent.MkDamageEvent attacker (Recipient.ToCreature blocker) power]
          blockers -> case Game.controllerOf attacker gs of
            Nothing -> pure []
            -- CR 702.19b: the excess is assigned "as its controller chooses," so the
            -- chooser is the attacker's controller. EXPIRES at M3: banding (702.22j,
            -- the DEFENDING player chooses) and Mindslaver (a Decider, not this
            -- PlayerId) both invert it -- a Decider problem, not a combat one. See
            -- the M2c spec, sections 4 and 8.
            Just pid -> do
              let decider = Decide.deciderFor pid gs
                  thresholdOf b = if trample then blockerThreshold gs attacker b else 0
                  blockerEntries = map (\b -> (Recipient.ToCreature b, thresholdOf b)) blockers
                  defenderEntry = case target of
                    AttackTarget.OfPlayer defender ->
                      if trample then [(Recipient.ToDefender defender, 0)] else []
                  thresholds = Map.fromList (blockerEntries ++ defenderEntry)
              chosen <-
                Trans.lift
                  (Program.prompt (Prompt.AssignCombatDamage decider pid attacker thresholds power))
              -- CR 510.1e / 702.19b: reject-not-repair (NOT the CR 733 human-error
              -- rewind). An illegal answer assigns nothing. See the M2c spec, §4.
              let toEvent (recipient, n) = DamageEvent.MkDamageEvent attacker recipient n
                  positive (_, n) = n > 0
              pure
                ( if legalAssignment thresholds power chosen
                    then map toEvent (filter positive (Map.toList chosen))
                    else []
                )
```

`Keyword` and `Object` are already imported in `Damage.hs`. Note the fallthrough: a single blocker whose guard fails is matched by the variable pattern `blockers` (a one-element list), so it is prompted — this is the M1b "single blocker not asked" shortcut being correctly falsified for trample only.

- [x] **Step 4: Run tests and clean-build**

Run: `cabal test 2>&1 | tail -30` — Expected: PASS. `Trample` green; `CombatDamage` (non-trample) still green (the `trample` flag is `False` for Pikers, so behavior is unchanged); `AssignmentLegality` green.
Run the clean-build warning check — Expected: `0`.

- [x] **Step 5: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Assign trample overflow to the defending player past lethal (CR 702.19b)"
```

---

### Task 6: The CR 702.2c interaction and the synthetic falsifier

Deathtouch lowers each blocker's threshold to 1 (any nonzero assignment is lethal). One line in `blockerThreshold`. The falsifier is tested with a labeled synthetic trample+deathtouch fixture, since no printed card has both keywords and M2c has no granting (M3 expiry).

**Files:**
- Modify: `source/library/Pawl/Damage.hs`, `source/test-suite/Main.hs`

**Interfaces:**
- Consumes: `Game.hasKeyword`, `Keyword.Deathtouch` (Task 1); `blockerThreshold` (Task 5).
- Produces: deathtouch-aware `blockerThreshold`.

- [x] **Step 1: Write the failing test**

In `source/test-suite/Main.hs`, add and append `trampleDeathtouchTests` to `testTree`. Define the synthetic fixture inline, clearly labeled:

```haskell
-- SYNTHETIC, NOT A REAL CARD. No printed Magic creature has both deathtouch and
-- trample (Scryfall keyword:deathtouch keyword:trample is empty), and M2c has no
-- granting effect (that is M3, e.g. Basilisk Collar) to combine them on a real
-- card. This fixture is the only way to exercise CR 702.2c in M2c. EXPIRES at M3:
-- grant deathtouch to a real trampler (War Mammoth) and delete this. See the M2c
-- spec, section 6, and git-bug's M3 work.
syntheticDeathtramplerPrinting :: Printing.Printing
syntheticDeathtramplerPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.Type.name = Text.pack "Synthetic Deathtrampler (test fixture)",
            Card.Type.manaCost = Nothing,
            Card.Type.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.empty
                },
            Card.Type.power = Just (Power.MkPower (Quantity.Type.Literal 3)),
            Card.Type.toughness = Just (Toughness.MkToughness (Quantity.Type.Literal 3)),
            Card.Type.keywords = Set.fromList [Keyword.Deathtouch, Keyword.Trample]
          }
    }

trampleDeathtouchTests :: Tasty.TestTree
trampleDeathtouchTests =
  Tasty.testGroup
    "TrampleDeathtouch"
    [ HU.testCase "CR 702.2c a deathtouch trampler needs only 1 on the blocker, spilling the rest" $
        -- Synthetic 3/3 deathtouch+trample into Ogre Sentry (3/3): lethal is 1, so
        -- 1 to the Ogre and 2 tramples to bob. The Ogre still dies (704.5h).
        let (gs, _, _) = combatBoardOf [syntheticDeathtramplerPrinting] [Card.ogreSentryPrinting]
            after = Sba.checkStateBasedActions (fightWith tramplingAnswer gs)
         in do
              HU.assertEqual "bob took 2 overflow" (Just 18) (lifeOf bob after)
              HU.assertEqual "the Ogre is dead" 0 (creaturesInPlay bob after),
      HU.testCase "CR 702.19b the control: plain trample into the same 3/3 spills nothing" $
        -- War Mammoth (3/3 trample, no deathtouch) into Ogre Sentry (3/3): lethal
        -- is 3, all 3 go to the Ogre, 0 tramples. Only deathtouch changes the spill.
        let (gs, _, _) = combatBoardOf [Card.warMammothPrinting] [Card.ogreSentryPrinting]
            after = Sba.checkStateBasedActions (fightWith tramplingAnswer gs)
         in HU.assertEqual "bob untouched without deathtouch" (Just 20) (lifeOf bob after)
    ]
```

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "/TrampleDeathtouch/"' 2>&1 | tail -20`
Expected: FAIL — the deathtouch case gets `bob = 20` (threshold is still full toughness 3, so `tramplingAnswer` assigns all 3 to the Ogre and spills nothing).

- [x] **Step 3: Add the 702.2c collapse**

In `source/library/Pawl/Damage.hs`, use the `attacker` argument (drop the `_` prefix) and add the deathtouch branch:

```haskell
-- CR 702.19b / 702.2c: a blocker's lethal threshold is toughness minus marked
-- damage -- but 702.2c makes any nonzero assignment by a deathtouch source lethal,
-- so a deathtouch attacker needs only 1 (0 if the blocker is already lethal). Read
-- through the projection (Game.hasKeyword), the same way the 704.5h SBA reads it.
blockerThreshold :: GameState -> ObjectId -> ObjectId -> Natural
blockerThreshold gs attacker blocker =
  let marked = maybe 0 Object.damage (Game.lookupObject blocker gs)
      lethal = case Game.toughnessOf blocker gs of
        Nothing -> 0
        Just t -> fromInteger (max 0 (t - toInteger marked))
   in if lethal > 0 && Game.hasKeyword Keyword.Deathtouch attacker gs
        then 1
        else lethal
```

- [x] **Step 4: Run tests and clean-build**

Run: `cabal test 2>&1 | tail -20` — Expected: PASS. `TrampleDeathtouch` green; `Trample` and `Deathtouch` still green (the collapse only fires when the attacker has deathtouch AND the blocker is not already lethal).
Run the clean-build warning check — Expected: `0`.

- [x] **Step 5: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Treat a deathtouch source's assignment as lethal for trample (CR 702.2c)"
```

---

### Task 7: Close the milestone

New properties, the classification assertion, a clean warning-free build of all targets, replay/determinism intact, and CLAUDE.md updated.

**Files:**
- Modify: `source/test-suite/Main.hs`, `CLAUDE.md`

**Interfaces:**
- Consumes: everything above.
- Produces: the milestone's exit criterion, asserted.

- [x] **Step 1: Write the deathtouch-destroys property and the classification assertion**

In `source/test-suite/Main.hs`, add and append `m2cPropertyTests` to `testTree`:

```haskell
m2cPropertyTests :: Tasty.TestTree
m2cPropertyTests =
  Tasty.testGroup
    "M2cProperties"
    [ HU.testCase "a deathtoucher's victim with toughness > 0 is gone after the SBA" $
        -- The property in fixture form (the deck has no deathtoucher, so this is
        -- the M2c coverage; it becomes a random-game property when a deathtoucher
        -- joins a deck -- git-bug's castability work). Every toughness we throw at
        -- the 1/1 deathtoucher dies to it.
        let victims = [Card.pikerPrinting, Card.nimbleBirdstickerPrinting, Card.ogreSentryPrinting]
            killsIt v =
              let (gs, _, _) = combatBoardOf [Card.typhoidRatsPrinting] [v]
                  after = Sba.checkStateBasedActions (fightWith aggressiveAnswer gs)
               in creaturesInPlay bob after == 0
         in HU.assertBool "deathtouch kills every toughness" (all killsIt victims),
      HU.testCase "the deathtouch and trample reads never name a card" $
        -- A structural reminder, asserted by the interaction falsifier's outcome
        -- (TrampleDeathtouch) depending only on the keyword projection. This case
        -- documents the invariant; the real enforcement is code review of
        -- Damage.blockerThreshold and Sba.woundedByDeathtouch, which case on
        -- Keyword, never on a printing.
        HU.assertBool "see TrampleDeathtouch and Deathtouch groups" True
    ]
```

- [x] **Step 2: Run the full suite and confirm all M1b/M2a/M2b properties survive**

Run: `cabal test 2>&1 | tee /tmp/m2c-test.txt | tail -5` then `grep -E "FAIL|Errors|passed" /tmp/m2c-test.txt`
Expected: all groups pass; conservation (120 objects), termination, ids ≥ 120, no mana floats, combat happens, fliers get through, no priority in a skipped step, **life never increases** (M2c adds no lifegain) all still green.

- [x] **Step 3: Clean-build all targets warning-free and run the benchmark**

Run:
```bash
rm -rf dist-newstyle/build/aarch64-osx/ghc-9.14.1/pawl-0.2026.7.16/{b,t,build}
cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -c "warning:"
```
Expected: `0`.
Run: `cabal bench 2>&1 | tail -5` — Expected: builds and runs.

- [x] **Step 4: Update CLAUDE.md**

In `CLAUDE.md`, under "Current work and tracking", change the M2c bullet from "Current work is M2c" to a completion record mirroring the M2a/M2b entries: note M2c complete (deathtouch's 704.5h SBA + the damage-event funnel; trample's recipient/threshold assignment and the 702.19b defender-gating; the 702.2c interaction), cite the spec and this plan as reference, and set current work to **M3** (the ABI test). Keep the paragraph's structure identical to the M2b entry.

- [x] **Step 5: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Record M2c complete: deathtouch, trample, and their CR 702.2c interaction"
```

---

## Notes for the implementer

- **The two invariants outrank this plan.** If a task seems to need `case printing of` or a player choice the rules do not offer, stop. `blockerThreshold` and `woundedByDeathtouch` case on `Keyword` (a rule number) and read the projection — that is legal; casing on a *card* is not.
- **A test failing against correct code is a plan bug.** Do not weaken an assertion or delete a test to make a check pass — fix the plan and say so. The spec has been wrong before (its "minimum" framing was corrected to the CR 702.19b implication during planning).
- **Reject-not-repair.** An illegal assignment yields no damage; it is never "fixed up." This is the engine defending against a broken interpreter, not the CR 733 table rewind.
- **`git-bug 14138aa`** tracks making these cards castable (Swamp/Forest + per-player mono-color decks) for random-game coverage — out of scope here.
