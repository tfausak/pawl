# M3b Continuous Effects Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Generalize the characteristic projection into a single-effect layer system (CR 613.7 within-layer timestamp order, no 613.8 dependency) with until-end-of-turn durations, landing Giant Growth (layer 7c), Serpent's Gift (layer-6 deathtouch grant), and Humility solo (layers 6 + 7b), per `docs/superpowers/specs/2026-07-18-m3b-continuous-effects-design.md`.

**Architecture:** A new `Pawl.Projection` module owns `project`, the gather → sort-by-`(layer, timestamp)` → fold engine, and is the single module allowed to `case` on a `Modification` (`Resolve : Effect :: Projection : Modification`). `keywordsOf`/`powerOf`/`toughnessOf`/`hasKeyword` move there from `Pawl.Game` and become wrappers over `project`; combat, SBAs, and targeting reach continuous effects for free through those seams. Continuous effects come from two sources: stored resolution effects (`GameState.continuousEffects`, with a `Duration`, dropped at cleanup) and the static abilities of battlefield permanents (`Card.staticAbilities`, re-derived live). `DamageEvent` grows a deal-time deathtouch bit so CR 702.2e's last-known-information is structural.

**Tech Stack:** GHC 9.14.1 (Nix dev shell), cabal, tasty (split suite under `source/test-suite/Pawl/*Spec.hs`, shared fixtures in `Pawl.Support` aliased `S`), hooky.

## Global Constraints

- **Haskell 2010.** `NamedFieldPuns` is newly permitted (this milestone's amendment) where it improves clarity, but the codebase's established style is *qualified* record construction/update (`obj {Object.damage = 0}`), and this plan follows that style throughout — no module here needs a puns pragma. The permission is recorded in Task 12; do not scatter pragmas. No other new extensions (only the existing `GADTs`/`RankNTypes` where already present).
- **One type per module** under `Pawl.Type.<Name>` (type + instances only); logic in other `Pawl.*` modules. New `Pawl.*` modules are discovered by cabal-gild's `discover` directive — run `hooky fix`, never hand-edit `exposed-modules` or the test-suite `other-modules`.
- **Qualified imports aliased to the last component**; `Pawl.Type.Card` takes alias `Card.Type` when it shares a module with `Pawl.Card` (alias `Card`). Import operators unqualified. `A.B.C` must not import `A.B` or `A`.
- **No partial functions** (`Set.lookupMin`, `Maybe.maybeToList`, not `head`/`elemAt`). **No list comprehensions** — use `map`/`filter`/`concatMap`. `Mk` constructor prefixes; non-punning constructor names. `Text`, not `String`. Arbitrary-precision numbers.
- **No boolean blindness**; derive at least `Eq` and `Show` (add `Ord` only where a value is sorted or keys a `Map`/`Set`).
- **Every rules claim cites its CR number** in a comment, checked against `docs/rules.txt` (never memory).
- **Warning-clean build:** `cabal build all --enable-tests --enable-benchmarks` (the `pedantic` flag makes warnings fatal). When you move or delete a definition, remove now-unused imports in the same edit. A definitive check needs `cabal clean` first (incremental builds hide warnings in unchanged modules).
- **The two invariants outrank this plan:** the rules core (`Combat`, `Damage`, `Sba`, `Engine`, `Cast`, `Target`) never cases on a `Modification` or an `Effect` — only `Pawl.Projection` cases on `Modification`, only `Pawl.Resolve` on `Effect`. The engine never makes a player's choice. If a step looks wrong, **stop and say so** — do not weaken a test to make a check pass.
- **TDD:** write each failing test, **run it and watch it fail**, then implement. Narrow with `cabal test --test-options='-p "<pattern>"'` while iterating; the full `cabal test` must pass before every commit.
- **Test players** are `S.alice`/`S.bob`; tests are named by CR number; cards by real name (Scryfall-verified: Giant Growth `{G}` "Target creature gets +3/+3 until end of turn."; Serpent's Gift `{2}{G}` "Target creature gains deathtouch until end of turn."; Humility `{2}{W}{W}` "All creatures lose all abilities and have base power and toughness 1/1.").
- **Before each commit:** `git add <explicit paths>` (the checkout is shared with other sessions — never `git add -A`), `hooky fix`, re-`git add` (fix reformats), `hooky run`, then commit. Commit messages are plain sentences (match `git log`), ending with:
  ```
  Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_01Sni8JpsTfduJC15gLtLc3e
  ```
  (If a different model executes a task, use its own name on the `Co-Authored-By:` line.)

---

### Task 1: Timestamps — the `Timestamp` type, `Object`/`GameState` fields, and stamping

**Files:**
- Create: `source/library/Pawl/Type/Timestamp.hs`
- Modify: `source/library/Pawl/Type/Object.hs` (`timestamp` field), `source/library/Pawl/Type/GameState.hs` (`nextTimestamp` field), `source/library/Pawl/Game.hs` (`freshTimestamp`, `changeZone` stamping), `source/library/Pawl/Setup.hs` (`createCard`, `emptyGame`)
- Modify (every `MkObject`/`MkGameState`): `source/test-suite/Pawl/Support.hs`, `source/test-suite/Pawl/GameSpec.hs`, `source/test-suite/Pawl/DamageSpec.hs`, `source/test-suite/Pawl/ResolveSpec.hs`
- Test: `source/test-suite/Pawl/GameSpec.hs`

**Interfaces:**
- Produces: `Timestamp.MkTimestamp Natural`; `Object.timestamp :: Timestamp`; `GameState.nextTimestamp :: Timestamp`; `Game.freshTimestamp :: GameState -> (Timestamp, GameState)`. `Game.changeZone` now stamps the new incarnation with a fresh timestamp. Later tasks read `Object.timestamp` (static-ability ordering) and call `freshTimestamp` (stored-effect creation).

- [x] **Step 1: Write the failing test**

In `source/test-suite/Pawl/GameSpec.hs`, add to the `changeZone` group (find the group testing `Game.changeZone`; if none, add these to the module's top-level `tests` list):

```haskell
      HU.testCase "CR 613.7b changeZone stamps the new incarnation with a fresh timestamp" $
        let (oid, gs) = S.addPiker S.bob (S.mountainsInPlay 1)
            before = GameState.nextTimestamp gs
            after = Game.changeZone oid Zone.Graveyard gs
            movedId = case Game.zoneMembers Zone.Graveyard S.bob after of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            stamp = fmap Object.timestamp (Game.lookupObject movedId after)
         in do
              HU.assertEqual "the incarnation carries the pre-move next timestamp" (Just before) stamp
              HU.assertBool "the counter advanced" (GameState.nextTimestamp after > before),
      HU.testCase "emptyGame starts the timestamp counter at zero" $
        HU.assertEqual "zero" (Timestamp.MkTimestamp 0) (GameState.nextTimestamp (Setup.emptyGame S.bothPlayers))
```

Add imports to `GameSpec.hs` if missing: `import qualified Pawl.Type.Timestamp as Timestamp`, and ensure `Setup`, `Object`, `Zone`, `ObjectId`, `GameState`, `Game` are imported (they are, in the existing spec).

- [x] **Step 2: Run it and watch it fail**

Run: `cabal test 2>&1 | tail -20`
Expected: compile error — `Pawl.Type.Timestamp`, `GameState.nextTimestamp`, `Object.timestamp` not in scope.

- [x] **Step 3: Implement**

`source/library/Pawl/Type/Timestamp.hs`:

```haskell
module Pawl.Type.Timestamp where

import Numeric.Natural (Natural)

-- CR 613.7: within a layer, continuous effects apply in timestamp order. A
-- monotonic counter (GameState.nextTimestamp) stamps every object when it is
-- created -- CR 613.7b: a permanent's static-ability timestamp is when the
-- object entered -- and every stored continuous effect when it begins. One
-- comparable sequence is what lets a static ability (Humility) and a stored
-- effect (Serpent's Gift) order against each other in layer 6.
--
-- A dedicated newtype, not the object id reused: id is identity, this is
-- entry-order. Both are monotone today, but conflating them is a pun this rule
-- rejects.
newtype Timestamp = MkTimestamp Natural
  deriving (Eq, Ord, Show)
```

`Pawl.Type.Object` gains, after `targets` (import `Pawl.Type.Timestamp (Timestamp)`):

```haskell
    -- CR 613.7b: when this object was created (its "entered" time). A static
    -- ability's timestamp is this; stamped fresh on every zone change (CR 400.7
    -- makes each a new object). Read by the projection when ordering layer 6/7.
    timestamp :: Timestamp
```

`Pawl.Type.GameState` gains, after `nextObjectId` (import `Pawl.Type.Timestamp (Timestamp)`):

```haskell
    -- CR 613.7: the monotonic source of timestamps for objects (at creation) and
    -- stored continuous effects (at CR 611 creation). See Timestamp.
    nextTimestamp :: Timestamp,
```

In `Pawl.Game` (import `qualified Pawl.Type.Timestamp as Timestamp`), next to `freshObjectId`:

```haskell
freshTimestamp :: GameState -> (Timestamp.Timestamp, GameState)
freshTimestamp gs =
  let Timestamp.MkTimestamp n = GameState.nextTimestamp gs
   in (Timestamp.MkTimestamp n, gs {GameState.nextTimestamp = Timestamp.MkTimestamp (n + 1)})
```

`Game.changeZone` stamps the new incarnation. Replace its `let` block so a fresh timestamp is drawn and set on `newObj`:

```haskell
    let pid = Object.owner obj
        (newId, gs1) = freshObjectId gs
        (ts, gs1b) = freshTimestamp gs1
        newObj = obj {Object.zone = dest, Object.tapped = TapState.Untapped, Object.damage = 0, Object.sickness = Sickness.Sick, Object.targets = Map.empty, Object.timestamp = ts}
        gs2 = removeFromZones pid oid gs1b
        gs3 = gs2 {GameState.objects = Map.insert newId newObj (Map.delete oid (GameState.objects gs2))}
     in insertIntoZone dest pid newId gs3
```

In `Pawl.Setup`: `emptyGame`'s `MkGameState` gains `GameState.nextTimestamp = Timestamp.MkTimestamp 0` (add `import qualified Pawl.Type.Timestamp as Timestamp`). `createCard` stamps the object it builds — thread a fresh timestamp:

```haskell
createCard pid printing = do
  gs <- State.get
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Library,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
            Object.targets = Map.empty,
            Object.timestamp = ts
          }
      gs3 =
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.library = Map.insertWith (flip (Seq.><)) pid (Seq.singleton oid) (GameState.library gs2)
          }
  State.put gs3
  pure oid
```

Now every remaining `Object.MkObject` and `GameState.MkGameState` gains its timestamp field, or the build fails with a missing-field error (that failure *is* the checklist). In `Pawl.Support`: `addCreature` and `landsInPlay` build objects while threading `gs1` from `freshObjectId` — draw a fresh timestamp too and set `Object.timestamp = ts`; `handOne` and `pikerInHand` likewise; `oneMountainState` sets `Object.timestamp = Timestamp.MkTimestamp 0` and its `MkGameState` sets `GameState.nextTimestamp = Timestamp.MkTimestamp 1` (add `import qualified Pawl.Type.Timestamp as Timestamp`). Give battlefield-fixture creatures *fresh, increasing* timestamps (via `freshTimestamp`) so multi-creature fixtures reflect entry order — Task 8's layer tests depend on it. For example, `addCreature`:

```haskell
addCreature printing pid gs =
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Battlefield,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.targets = Map.empty,
            Object.timestamp = ts
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.battlefield = Set.insert oid (GameState.battlefield gs2)
          }
      )
```

Then `grep -rn "MkObject\|MkGameState" source/test-suite/Pawl/GameSpec.hs source/test-suite/Pawl/DamageSpec.hs source/test-suite/Pawl/ResolveSpec.hs` and add `Object.timestamp = Timestamp.MkTimestamp 0` (and `GameState.nextTimestamp = Timestamp.MkTimestamp 0` on any full `MkGameState`) to each, importing `Timestamp` where needed.

- [x] **Step 4: Build and run the full suite**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -3` → clean (this proves every `MkObject`/`MkGameState` was updated).
Run: `cabal test 2>&1 | tail -5` → PASS (all existing + the two new).

- [x] **Step 5: Commit**

```bash
git add source/library/Pawl/Type/Timestamp.hs source/library/Pawl/Type/Object.hs source/library/Pawl/Type/GameState.hs source/library/Pawl/Game.hs source/library/Pawl/Setup.hs source/test-suite/Pawl/Support.hs source/test-suite/Pawl/GameSpec.hs source/test-suite/Pawl/DamageSpec.hs source/test-suite/Pawl/ResolveSpec.hs pawl.cabal
hooky fix && git add -u && git add pawl.cabal && hooky run
git commit -m "Add object and effect timestamps for CR 613.7 layer ordering

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01Sni8JpsTfduJC15gLtLc3e"
```

---

### Task 2: The continuous-effect type family and the two container fields

**Files:**
- Create: `source/library/Pawl/Type/Layer.hs`, `source/library/Pawl/Type/Duration.hs`, `source/library/Pawl/Type/Affected.hs`, `source/library/Pawl/Type/Modification.hs`, `source/library/Pawl/Type/ContinuousEffect.hs`, `source/library/Pawl/Type/StaticAbility.hs`, `source/library/Pawl/Type/ProjectedCharacteristics.hs`
- Modify: `source/library/Pawl/Type/GameState.hs` (`continuousEffects` field), `source/library/Pawl/Type/Card.hs` (`staticAbilities` field), `source/library/Pawl/Card.hs` (empty `staticAbilities` on all 14 printings), `source/library/Pawl/Setup.hs` (`emptyGame`), `source/test-suite/Pawl/Support.hs` (`oneMountainState`; `pikerCard` and other `MkCard` fixtures), plus the test `MkGameState`/`MkCard` sites found by grep
- Test: `source/test-suite/Pawl/GameSpec.hs`

**Interfaces:**
- Produces: `Layer.{Copy,Control,Text,Type,Color,Ability,CharacteristicPT,SetPT,ModifyPT,SwitchPT}`; `Duration.UntilEndOfTurn`; `Affected.{TheseObjects (Set ObjectId), AllCreatures}`; `Modification.{GainKeyword Keyword, LoseAllAbilities, SetBasePowerToughness Quantity Quantity, ModifyPowerToughness Quantity Quantity}`; `ContinuousEffect.MkContinuousEffect {source, timestamp, duration, modification, affected}`; `StaticAbility.MkStaticAbility {affected, modification}`; `ProjectedCharacteristics.MkProjectedCharacteristics {keywords, power, toughness}`; `GameState.continuousEffects :: [ContinuousEffect]`; `Card.staticAbilities :: [StaticAbility]`.

- [x] **Step 1: Write the failing test**

In `GameSpec.hs`:

```haskell
      HU.testCase "a fresh game has no continuous effects" $
        HU.assertEqual "empty" [] (GameState.continuousEffects (Setup.emptyGame S.bothPlayers)),
      HU.testCase "a vanilla printing declares no static abilities" $
        HU.assertEqual "empty" [] (Card.Type.staticAbilities S.pikerCard)
```

(Import `qualified Pawl.Type.Card as Card.Type` if not present; `S.pikerCard` exists.)

- [x] **Step 2: Run and watch it fail**

Run: `cabal test 2>&1 | tail -20` → compile error, `GameState.continuousEffects` / `Card.Type.staticAbilities` not in scope.

- [x] **Step 3: Implement the types**

`source/library/Pawl/Type/Layer.hs`:

```haskell
module Pawl.Type.Layer where

-- CR 613.1: the layers a continuous effect can apply in, ordered by rule number
-- so the DERIVED Ord IS the application order -- the sole thing the projection
-- sorts on. Complete for diffability against CR 613 (the Keyword posture); only
-- Ability (6), SetPT (7b), and ModifyPT (7c) have producers at M3b. No
-- Enum/Bounded -- nothing enumerates layers or asks for bounds.
data Layer
  = Copy -- 613.1a, layer 1
  | Control -- 613.1b, layer 2
  | Text -- 613.1c, layer 3
  | Type -- 613.1d, layer 4
  | Color -- 613.1e, layer 5
  | Ability -- 613.1f, layer 6
  | CharacteristicPT -- 613.1g / 613.3, layer 7a (characteristic-defining)
  | SetPT -- layer 7b
  | ModifyPT -- layer 7c
  | SwitchPT -- layer 7d
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/Duration.hs`:

```haskell
module Pawl.Type.Duration where

-- How long a stored continuous effect lasts (CR 611.2). Only UntilEndOfTurn
-- (CR 514.2 drops it during cleanup) exists at M3b; static-ability effects carry
-- no Duration -- they last while their source and ability do, which is "while
-- re-derived from the battlefield". Grows WhileSourceOnBattlefield,
-- UntilYourNextTurn, etc. as cards need them.
data Duration
  = UntilEndOfTurn
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/Affected.hs`:

```haskell
module Pawl.Type.Affected where

import Data.Set (Set)
import Pawl.Type.ObjectId (ObjectId)

-- What a continuous effect applies to. CR 611.2c: a resolution effect's set is
-- LOCKED when it begins (TheseObjects -- a bounced-and-returned creature is a new
-- id the effect no longer names). A static ability's set is dynamic (AllCreatures
-- -- any creature currently on the battlefield), which is why static effects are
-- re-derived each projection, never captured once.
data Affected
  = TheseObjects (Set ObjectId)
  | AllCreatures
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/Modification.hs`:

```haskell
module Pawl.Type.Modification where

import Pawl.Type.Keyword (Keyword)
import Pawl.Type.Quantity (Quantity)

-- The open-half continuous-effect vocabulary -- its own leaf family (design.md's
-- M3g note: "continuous-effect specifications, classified by layer"), distinct
-- from Effect. The ONLY module that may case on a constructor is Pawl.Projection
-- (Projection.layer classifies it; Projection.applyModification applies it) --
-- the same standing Pawl.Resolve has over Effect. GainKeyword carries a Keyword,
-- a closed-half CITATION (casing on it is not an invariant violation -- see the
-- M2a spec). P/T constructors carry signed Quantity (+3/+3 or a future -1/-1).
data Modification
  = GainKeyword Keyword -- layer 6 (Serpent's Gift)
  | LoseAllAbilities -- layer 6 (Humility)
  | SetBasePowerToughness Quantity Quantity -- layer 7b (Humility 1/1)
  | ModifyPowerToughness Quantity Quantity -- layer 7c (Giant Growth +3/+3)
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/ContinuousEffect.hs`:

```haskell
module Pawl.Type.ContinuousEffect where

import Pawl.Type.Affected (Affected)
import Pawl.Type.Duration (Duration)
import Pawl.Type.Modification (Modification)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Timestamp (Timestamp)

-- A stored, resolution-generated continuous effect (CR 611.2), held in
-- GameState.continuousEffects. `timestamp` orders it within its layer (CR 613.7);
-- `duration` decides when cleanup drops it (CR 514.2); `affected` is its fixed
-- set (CR 611.2c). Static-ability effects are NOT stored here -- they are
-- re-derived from Card.staticAbilities each projection.
data ContinuousEffect = MkContinuousEffect
  { source :: ObjectId,
    timestamp :: Timestamp,
    duration :: Duration,
    modification :: Modification,
    affected :: Affected
  }
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/StaticAbility.hs`:

```haskell
module Pawl.Type.StaticAbility where

import Pawl.Type.Affected (Affected)
import Pawl.Type.Modification (Modification)

-- A card's printed static continuous ability (CR 604.3). Gathered live from every
-- battlefield permanent by the projection, with the permanent's own timestamp
-- (CR 613.7b). Humility declares two: (AllCreatures, LoseAllAbilities) and
-- (AllCreatures, SetBasePowerToughness 1 1).
data StaticAbility = MkStaticAbility
  { affected :: Affected,
    modification :: Modification
  }
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/ProjectedCharacteristics.hs`:

```haskell
module Pawl.Type.ProjectedCharacteristics where

import Data.Set (Set)
import Pawl.Type.Keyword (Keyword)

-- The characteristics of an object after the layer fold (design.md §2.5). Maybe
-- P/T because a land has none. No Ord: never sorted, never a key.
data ProjectedCharacteristics = MkProjectedCharacteristics
  { keywords :: Set Keyword,
    power :: Maybe Integer,
    toughness :: Maybe Integer
  }
  deriving (Eq, Show)
```

`Pawl.Type.GameState` gains, after `combat`/`damageEvents` (import `Pawl.Type.ContinuousEffect (ContinuousEffect)`):

```haskell
    -- CR 611.2: stored continuous effects from resolutions (Giant Growth,
    -- Serpent's Gift), each with a duration cleanup consults. Static-ability
    -- effects are NOT here -- the projection re-derives those live.
    continuousEffects :: [ContinuousEffect],
```

`Pawl.Type.Card` gains, after `keywords` (import `Pawl.Type.StaticAbility (StaticAbility)`):

```haskell
    -- CR 604.3: this card's static continuous abilities (Humility). Empty for
    -- everything but the few printings that generate a continuous effect just by
    -- being on the battlefield. The projection gathers these live.
    staticAbilities :: [StaticAbility],
```

`Setup.emptyGame`'s `MkGameState` gains `GameState.continuousEffects = []`. Every `Card.MkCard` in `Pawl.Card` (all 14 printings) gains `Card.staticAbilities = []` — `grep -n "MkCard" source/library/Pawl/Card.hs` and visit each. `Support.oneMountainState`'s `MkGameState` gains `GameState.continuousEffects = []`; any `MkCard` fixture in the test suite (`grep -rn "MkCard" source/test-suite/`) gains `Card.Type.staticAbilities = []`.

- [x] **Step 4: Build and run the full suite**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -3` → clean.
Run: `cabal test 2>&1 | tail -5` → PASS.

- [x] **Step 5: Commit**

```bash
git add source/library/Pawl/Type/Layer.hs source/library/Pawl/Type/Duration.hs source/library/Pawl/Type/Affected.hs source/library/Pawl/Type/Modification.hs source/library/Pawl/Type/ContinuousEffect.hs source/library/Pawl/Type/StaticAbility.hs source/library/Pawl/Type/ProjectedCharacteristics.hs source/library/Pawl/Type/GameState.hs source/library/Pawl/Type/Card.hs source/library/Pawl/Card.hs source/library/Pawl/Setup.hs source/test-suite/Pawl/Support.hs source/test-suite/Pawl/GameSpec.hs pawl.cabal
hooky fix && git add -u && git add pawl.cabal && hooky run
git commit -m "Add the continuous-effect type family and its two container fields

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01Sni8JpsTfduJC15gLtLc3e"
```

---

### Task 3: Create `Pawl.Projection`; move the characteristic seams out of `Game`

**Files:**
- Create: `source/library/Pawl/Projection.hs`
- Modify: `source/library/Pawl/Game.hs` (delete the four functions + now-unused imports), `source/library/Pawl/Combat.hs`, `source/library/Pawl/Damage.hs`, `source/library/Pawl/Sba.hs` (migrate call sites)

**Interfaces:**
- Produces: `Projection.keywordsOf :: ObjectId -> GameState -> Set Keyword`; `Projection.powerOf :: ObjectId -> GameState -> Maybe Integer`; `Projection.toughnessOf :: ObjectId -> GameState -> Maybe Integer`; `Projection.hasKeyword :: Keyword -> ObjectId -> GameState -> Bool`. Same signatures as the old `Game.*`; Task 4 rewrites the bodies. This task is a **behavior-preserving move** — the existing suite is its test.

- [x] **Step 1: Create the module (verbatim bodies)**

`source/library/Pawl/Projection.hs` — the four functions, moved unchanged from `Game.hs` (still reading the base printing; Task 4 makes them layer-aware):

```haskell
module Pawl.Projection where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Game as Game
import qualified Pawl.Quantity as Quantity
import qualified Pawl.Type.Card as Card.Type
import Pawl.Type.GameState (GameState)
import Pawl.Type.Keyword (Keyword)
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Power as Power
import qualified Pawl.Type.Toughness as Toughness

-- The projected characteristics of an object. Task 4 makes these a layer fold;
-- for now they are the base printing, moved out of Pawl.Game so the projection
-- has one home (and so Game does not depend on the projection -- Projection
-- depends on Game, not the reverse).
powerOf :: ObjectId -> GameState -> Maybe Integer
powerOf oid gs = case fmap Card.Type.power (Game.cardOf oid gs) of
  Just (Just (Power.MkPower quantity)) -> Quantity.evaluate gs oid quantity
  _ -> Nothing

toughnessOf :: ObjectId -> GameState -> Maybe Integer
toughnessOf oid gs = case fmap Card.Type.toughness (Game.cardOf oid gs) of
  Just (Just (Toughness.MkToughness quantity)) -> Quantity.evaluate gs oid quantity
  _ -> Nothing

keywordsOf :: ObjectId -> GameState -> Set Keyword
keywordsOf oid gs = maybe Set.empty Card.Type.keywords (Game.cardOf oid gs)

hasKeyword :: Keyword -> ObjectId -> GameState -> Bool
hasKeyword keyword oid gs = Set.member keyword (keywordsOf oid gs)
```

(The `Card.Type` alias is `Pawl.Type.Card`; `Game.cardOf` is the raw state read, which stays in `Game`.)

- [x] **Step 2: Delete the four functions from `Game` and migrate callers**

In `Pawl.Game`, delete `powerOf`, `toughnessOf`, `keywordsOf`, `hasKeyword` and remove imports they alone used (`Power`, `Toughness`, `Quantity`, and `Keyword` if now unused — check with the build). Keep `cardOf`, `controllerOf`, `lookupObject`, `zoneMembers`, `changeZone`, `freshObjectId`, `freshTimestamp`, etc.

Then migrate every `Game.<fn>` → `Projection.<fn>` for the four moved functions in `Combat.hs`, `Damage.hs`, `Sba.hs` (each adds `import qualified Pawl.Projection as Projection`):

```bash
grep -rn "Game\.\(powerOf\|toughnessOf\|keywordsOf\|hasKeyword\)" source/library/
```

Visit each hit — `Combat.hs` (lines ~78, 82, 113–115, 184: `hasKeyword`), `Damage.hs` (`toughnessOf` in `blockerThreshold`, `hasKeyword` in `blockerThreshold`/`attackerAssignment`/`dealCombatDamage`, `powerOf` in `attackerAssignment`/`blockerAssignment`), `Sba.hs` (`toughnessOf` in `creatureDies`, `hasKeyword` in `woundedByDeathtouch`) — and change the qualifier to `Projection.`. Update the comments in `Sba.woundedByDeathtouch` and `Damage.blockerThreshold` that say "Game.hasKeyword" to "Projection.hasKeyword".

- [x] **Step 3: Build and run the full suite (this is the whole test)**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -3` → clean (a missed call site is a scope error).
Run: `cabal test 2>&1 | tail -5` → PASS, **unchanged test count** — a pure move changes no behavior. If any test changes outcome, you changed behavior; revert and redo the move faithfully.

- [x] **Step 4: Commit**

```bash
git add source/library/Pawl/Projection.hs source/library/Pawl/Game.hs source/library/Pawl/Combat.hs source/library/Pawl/Damage.hs source/library/Pawl/Sba.hs pawl.cabal
hooky fix && git add -u && git add pawl.cabal && hooky run
git commit -m "Move the characteristic seams into Pawl.Projection

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01Sni8JpsTfduJC15gLtLc3e"
```

---

### Task 4: `Projection.project` — the gather → sort → fold layer engine

**Files:**
- Modify: `source/library/Pawl/Projection.hs` (add `layer`, `applyModification`, `affects`, `baseCharacteristics`, `gather`, `project`; rewrite the four wrappers)
- Create: `source/test-suite/Pawl/ProjectionSpec.hs`
- Modify: `source/test-suite/Main.hs` (aggregate `ProjectionSpec.tests`)

**Interfaces:**
- Consumes: `Card.staticAbilities`, `ContinuousEffect.*`, `StaticAbility.*`, `Affected.*`, `Modification.*`, `Layer`, `Object.timestamp`, `GameState.continuousEffects`, `Game.cardOf`/`lookupObject`, `Card.isCreature`, `Quantity.evaluate`.
- Produces: `Projection.project :: ObjectId -> GameState -> ProjectedCharacteristics`; `Projection.layer :: Modification -> Layer`. The four wrappers now derive from `project`.

- [x] **Step 1: Write the failing tests**

`source/test-suite/Pawl/ProjectionSpec.hs`:

```haskell
-- Covers Pawl.Projection: the single-effect layer fold -- CR 613 layer order and
-- CR 613.7 within-layer timestamp order, with no CR 613.8 dependency (M3c). Uses
-- directly-constructed continuous effects so the engine is proven before any card
-- wiring; real-card behavior lands in later tasks and DamageSpec.
module Pawl.ProjectionSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Card as Card
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Support as S
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Layer as Layer
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.Timestamp as Timestamp
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Append a stored continuous effect affecting exactly `oid`, at timestamp `ts`.
withEffect :: ObjectId.ObjectId -> Timestamp.Timestamp -> Modification.Modification -> GameState.GameState -> GameState.GameState
withEffect oid ts m gs =
  let eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = ObjectId.MkObjectId 998,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.duration = Duration.UntilEndOfTurn,
            ContinuousEffect.modification = m,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
          }
   in gs {GameState.continuousEffects = eff : GameState.continuousEffects gs}

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Projection"
    [ HU.testCase "layer classification matches CR 613.1" $ do
        HU.assertEqual "grant is layer 6" Layer.Ability (Projection.layer (Modification.GainKeyword Keyword.Deathtouch))
        HU.assertEqual "lose-all is layer 6" Layer.Ability (Projection.layer Modification.LoseAllAbilities)
        HU.assertEqual "set base is 7b" Layer.SetPT (Projection.layer (Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1)))
        HU.assertEqual "modify is 7c" Layer.ModifyPT (Projection.layer (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3))),
      HU.testCase "no effects: the projection is the base printing (Piker is 2/1)" $
        let (oid, gs) = S.addPiker S.bob (S.mountainsInPlay 1)
         in do
              HU.assertEqual "power" (Just 2) (Projection.powerOf oid gs)
              HU.assertEqual "toughness" (Just 1) (Projection.toughnessOf oid gs)
              HU.assertBool "no keywords" (Set.null (Projection.keywordsOf oid gs)),
      HU.testCase "CR 613.3 layer 7c +3/+3 raises a Piker to 5/4" $
        let (oid, gs0) = S.addPiker S.bob (S.mountainsInPlay 1)
            gs = withEffect oid (Timestamp.MkTimestamp 100) (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)) gs0
         in do
              HU.assertEqual "power" (Just 5) (Projection.powerOf oid gs)
              HU.assertEqual "toughness" (Just 4) (Projection.toughnessOf oid gs),
      HU.testCase "CR 613 layer 6 GainKeyword adds deathtouch" $
        let (oid, gs0) = S.addPiker S.bob (S.mountainsInPlay 1)
            gs = withEffect oid (Timestamp.MkTimestamp 100) (Modification.GainKeyword Keyword.Deathtouch) gs0
         in HU.assertBool "has deathtouch" (Projection.hasKeyword Keyword.Deathtouch oid gs),
      HU.testCase "CR 613 layer 7b SetBasePowerToughness makes a Piker 1/1" $
        let (oid, gs0) = S.addPiker S.bob (S.mountainsInPlay 1)
            gs = withEffect oid (Timestamp.MkTimestamp 100) (Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1)) gs0
         in do
              HU.assertEqual "power" (Just 1) (Projection.powerOf oid gs)
              HU.assertEqual "toughness" (Just 1) (Projection.toughnessOf oid gs),
      HU.testCase "CR 613 sublayer order: 7b then 7c, a set-1/1 Piker with +3/+3 is 4/4" $
        let (oid, gs0) = S.addPiker S.bob (S.mountainsInPlay 1)
            -- Deliberately give 7c the EARLIER timestamp to prove layer beats
            -- timestamp: 7b still applies first.
            gs1 = withEffect oid (Timestamp.MkTimestamp 50) (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)) gs0
            gs = withEffect oid (Timestamp.MkTimestamp 100) (Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1)) gs1
         in do
              HU.assertEqual "power 1 then +3" (Just 4) (Projection.powerOf oid gs)
              HU.assertEqual "toughness 1 then +3" (Just 4) (Projection.toughnessOf oid gs),
      HU.testCase "CR 613.7 within layer 6, timestamp order: later grant survives an earlier lose-all" $
        let (oid, gs0) = S.addPiker S.bob (S.mountainsInPlay 1)
            gs1 = withEffect oid (Timestamp.MkTimestamp 10) Modification.LoseAllAbilities gs0
            gs = withEffect oid (Timestamp.MkTimestamp 20) (Modification.GainKeyword Keyword.Deathtouch) gs1
         in HU.assertBool "grant wins" (Projection.hasKeyword Keyword.Deathtouch oid gs),
      HU.testCase "CR 613.7 within layer 6, timestamp order: earlier grant is erased by a later lose-all" $
        let (oid, gs0) = S.addPiker S.bob (S.mountainsInPlay 1)
            gs1 = withEffect oid (Timestamp.MkTimestamp 10) (Modification.GainKeyword Keyword.Deathtouch) gs0
            gs = withEffect oid (Timestamp.MkTimestamp 20) Modification.LoseAllAbilities gs1
         in HU.assertBool "lose-all wins" (not (Projection.hasKeyword Keyword.Deathtouch oid gs)),
      HU.testCase "a P/T modification never gives P/T to a land" $
        let gs0 = S.mountainsInPlay 1
            landId = case Game.zoneMembers Pawl.Type.Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = withEffect landId (Timestamp.MkTimestamp 100) (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)) gs0
         in HU.assertEqual "still no power" Nothing (Projection.powerOf landId gs)
    ]
```

For the land test, add `import qualified Pawl.Type.Zone as Zone` and use `Zone.Battlefield` (rename `Pawl.Type.Zone.Battlefield` above to `Zone.Battlefield`). Add `ProjectionSpec.tests` to `testTree` in `source/test-suite/Main.hs` (and `import qualified Pawl.ProjectionSpec as ProjectionSpec`).

- [x] **Step 2: Run and watch them fail**

Run: `cabal test 2>&1 | tail -30`
Expected: compile error — `Projection.layer`/`project` not defined; the +3/+3 and layer tests fail (the Task 3 bodies ignore continuous effects).

- [x] **Step 3: Implement the fold**

Rewrite `Pawl.Projection` (add imports: `qualified Data.List as List`, `qualified Data.Maybe as Maybe`, `qualified Pawl.Card as Card`, `qualified Pawl.Type.Affected as Affected`, `qualified Pawl.Type.ContinuousEffect as ContinuousEffect`, `qualified Pawl.Type.GameState as GameState`, `qualified Pawl.Type.Layer as Layer`, `Pawl.Type.Layer (Layer)`, `qualified Pawl.Type.Modification as Modification`, `Pawl.Type.Modification (Modification)`, `qualified Pawl.Type.Object as Object`, `qualified Pawl.Type.ProjectedCharacteristics as PC`, `Pawl.Type.ProjectedCharacteristics (ProjectedCharacteristics)`, `qualified Pawl.Type.StaticAbility as StaticAbility`, `Pawl.Type.Timestamp (Timestamp)`, `qualified Data.Set as Set`):

```haskell
-- CR 613.1: the layer a modification applies in. THE ABI classification the
-- rules core would ask -- never the modification's identity. One of two case-on-
-- Modification functions this module is the sole home of.
layer :: Modification -> Layer
layer m = case m of
  Modification.GainKeyword _ -> Layer.Ability
  Modification.LoseAllAbilities -> Layer.Ability
  Modification.SetBasePowerToughness _ _ -> Layer.SetPT
  Modification.ModifyPowerToughness _ _ -> Layer.ModifyPT

-- Apply one modification to characteristics-in-progress. THE ONE applier
-- (Resolve : Effect :: Projection : Modification). P/T quantities are evaluated
-- here against the state; CR 611.2b's freeze-at-creation is a no-op while every
-- Quantity is a Literal (identical value either way). When X lands, Resolve must
-- freeze the value into the stored effect and this reads the frozen Literal.
applyModification :: GameState -> ObjectId -> Modification -> ProjectedCharacteristics -> ProjectedCharacteristics
applyModification gs oid m pc = case m of
  Modification.GainKeyword k ->
    pc {PC.keywords = Set.insert k (PC.keywords pc)}
  Modification.LoseAllAbilities ->
    pc {PC.keywords = Set.empty}
  Modification.SetBasePowerToughness p t ->
    pc
      { PC.power = setPT (PC.power pc) (Quantity.evaluate gs oid p),
        PC.toughness = setPT (PC.toughness pc) (Quantity.evaluate gs oid t)
      }
  Modification.ModifyPowerToughness p t ->
    pc
      { PC.power = addPT (PC.power pc) (Quantity.evaluate gs oid p),
        PC.toughness = addPT (PC.toughness pc) (Quantity.evaluate gs oid t)
      }

-- Layer 7b sets P/T only on an object that HAS P/T; a land stays without.
setPT :: Maybe Integer -> Maybe Integer -> Maybe Integer
setPT base new = case (base, new) of
  (Just _, Just n) -> Just n
  (Just b, Nothing) -> Just b
  (Nothing, _) -> Nothing

-- Layer 7c adds; an unevaluable delta leaves the value, a land stays without.
addPT :: Maybe Integer -> Maybe Integer -> Maybe Integer
addPT base delta = case (base, delta) of
  (Just b, Just d) -> Just (b + d)
  (Just b, Nothing) -> Just b
  (Nothing, _) -> Nothing

-- CR 611.2c: does this effect's set include the object? A fixed set is a
-- membership test; AllCreatures is re-evaluated live (creatures on the
-- battlefield).
affects :: ObjectId -> Affected.Affected -> GameState -> Bool
affects oid a gs = case a of
  Affected.TheseObjects s -> Set.member oid s
  Affected.AllCreatures ->
    Set.member oid (GameState.battlefield gs)
      && fmap Card.isCreature (Game.cardOf oid gs) == Just True

-- Printed characteristics before any effect (CR 613.2/613.4 starting point).
baseCharacteristics :: ObjectId -> GameState -> ProjectedCharacteristics
baseCharacteristics oid gs = case Game.cardOf oid gs of
  Nothing -> PC.MkProjectedCharacteristics {PC.keywords = Set.empty, PC.power = Nothing, PC.toughness = Nothing}
  Just card ->
    PC.MkProjectedCharacteristics
      { PC.keywords = Card.Type.keywords card,
        PC.power = case Card.Type.power card of
          Nothing -> Nothing
          Just (Power.MkPower q) -> Quantity.evaluate gs oid q,
        PC.toughness = case Card.Type.toughness card of
          Nothing -> Nothing
          Just (Toughness.MkToughness q) -> Quantity.evaluate gs oid q
      }

-- Every continuous effect touching this object, from BOTH sources, tagged with
-- its layer and timestamp: stored resolution effects, plus the static abilities
-- of every battlefield permanent (CR 613.7b: the permanent's own timestamp).
gather :: ObjectId -> GameState -> [(Layer, Timestamp, Modification)]
gather oid gs =
  let fromStored eff =
        if affects oid (ContinuousEffect.affected eff) gs
          then [(layer (ContinuousEffect.modification eff), ContinuousEffect.timestamp eff, ContinuousEffect.modification eff)]
          else []
      stored = concatMap fromStored (GameState.continuousEffects gs)
      fromStatic permId = case Game.lookupObject permId gs of
        Nothing -> []
        Just permObj -> case Game.cardOf permId gs of
          Nothing -> []
          Just card ->
            let one sa =
                  if affects oid (StaticAbility.affected sa) gs
                    then [(layer (StaticAbility.modification sa), Object.timestamp permObj, StaticAbility.modification sa)]
                    else []
             in concatMap one (Card.Type.staticAbilities card)
      static_ = concatMap fromStatic (Set.toList (GameState.battlefield gs))
   in stored ++ static_

-- CR 613: apply every continuous effect to the base characteristics in layer
-- order, ties broken by CR 613.7 timestamp. A linear fold -- no CR 613.8
-- dependency (that is M3c's trial application). design.md §2.5.
project :: ObjectId -> GameState -> ProjectedCharacteristics
project oid gs =
  let sorted = List.sortOn (\(l, ts, _) -> (l, ts)) (gather oid gs)
      step pc (_, _, m) = applyModification gs oid m pc
   in List.foldl' step (baseCharacteristics oid gs) sorted

powerOf :: ObjectId -> GameState -> Maybe Integer
powerOf oid gs = PC.power (project oid gs)

toughnessOf :: ObjectId -> GameState -> Maybe Integer
toughnessOf oid gs = PC.toughness (project oid gs)

keywordsOf :: ObjectId -> GameState -> Set Keyword
keywordsOf oid gs = PC.keywords (project oid gs)

hasKeyword :: Keyword -> ObjectId -> GameState -> Bool
hasKeyword keyword oid gs = Set.member keyword (keywordsOf oid gs)
```

Delete the Task-3 verbatim bodies of the four wrappers (they are replaced above). Keep the `Power`/`Toughness` imports (now used by `baseCharacteristics`).

- [x] **Step 4: Run the full suite**

Run: `cabal test 2>&1 | tail -5` → PASS (existing suite unchanged: with no continuous effects, `project` equals the base printing; the new `ProjectionSpec` group passes).

- [x] **Step 5: Commit**

```bash
git add source/library/Pawl/Projection.hs source/test-suite/Pawl/ProjectionSpec.hs source/test-suite/Main.hs pawl.cabal
hooky fix && git add -u && git add pawl.cabal && hooky run
git commit -m "Make the projection a CR 613 single-effect layer fold

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01Sni8JpsTfduJC15gLtLc3e"
```

---

### Task 5: `TargetSpec.CreatureTarget` and creature-only legality

**Files:**
- Modify: `source/library/Pawl/Type/TargetSpec.hs` (`CreatureTarget`), `source/library/Pawl/Target.hs` (the new case), `source/test-suite/Pawl/ResolveSpec.hs` (tests in the `Target` group)

**Interfaces:**
- Produces: `TargetSpec.CreatureTarget`; `Target.legalRecipients`/`stillLegal` handle it (creatures on the battlefield, no players).

- [x] **Step 1: Write the failing tests**

In `ResolveSpec.hs`'s `Target` group:

```haskell
      HU.testCase "CR 115.4 CreatureTarget offers creatures but no players" $
        let (oid, gs) = S.addPiker S.bob (Setup.emptyGame S.bothPlayers)
         in HU.assertEqual
              "just the creature"
              (Set.singleton (Recipient.ToCreature oid))
              (Target.legalRecipients TargetSpec.CreatureTarget gs),
      HU.testCase "CR 601.2c CreatureTarget has an empty legal set with no creatures" $
        HU.assertBool
          "nothing to target"
          (Set.null (Target.legalRecipients TargetSpec.CreatureTarget (Setup.emptyGame S.bothPlayers))),
      HU.testCase "CR 608.2b a creature that left is no longer a legal CreatureTarget" $
        let (oid, gs) = S.addPiker S.bob (Setup.emptyGame S.bothPlayers)
            gone = Game.changeZone oid Zone.Graveyard gs
         in do
              HU.assertBool "legal while fielded" (Target.stillLegal (Recipient.ToCreature oid) TargetSpec.CreatureTarget gs)
              HU.assertBool "illegal once moved" (not (Target.stillLegal (Recipient.ToCreature oid) TargetSpec.CreatureTarget gone))
```

- [x] **Step 2: Run and watch them fail**

Run: `cabal test 2>&1 | tail -20` → `TargetSpec.CreatureTarget` not in scope.

- [x] **Step 3: Implement**

`Pawl.Type.TargetSpec` gains a constructor (comment updated):

```haskell
data TargetSpec
  = AnyTarget
  | -- CR 115.4: "target creature" -- a creature on the battlefield, no players.
    -- The first spec whose legal set can be EMPTY, which falsifies M3a's
    -- CR 601.2c targeting gate (Giant Growth with no creature is uncastable).
    CreatureTarget
  deriving (Eq, Ord, Show)
```

`Pawl.Target.legalRecipients` gains the case (its existing `AnyTarget` case already builds `creatures`; factor the creature list out so both cases share it):

```haskell
legalRecipients :: TargetSpec -> GameState -> Set Recipient
legalRecipients spec gs =
  let isCreatureId oid = fmap Card.isCreature (Game.cardOf oid gs) == Just True
      creatures =
        map Recipient.ToCreature $
          filter isCreatureId $
            concatMap (\pid -> Game.zoneMembers Zone.Battlefield pid gs) (Sba.stillPlaying gs)
      players = map Recipient.ToPlayer (Sba.stillPlaying gs)
   in case spec of
        TargetSpec.AnyTarget -> Set.fromList (creatures ++ players)
        TargetSpec.CreatureTarget -> Set.fromList creatures
```

(`stillLegal` is already `Set.member recipient (legalRecipients spec gs)` — it handles the new spec with no change.)

- [x] **Step 4: Run the full suite**

Run: `cabal test 2>&1 | tail -5` → PASS.

- [x] **Step 5: Commit**

```bash
git add source/library/Pawl/Type/TargetSpec.hs source/library/Pawl/Target.hs source/test-suite/Pawl/ResolveSpec.hs
hooky fix && git add -u && hooky run
git commit -m "Add the CreatureTarget spec and its legality (CR 115.4)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01Sni8JpsTfduJC15gLtLc3e"
```

---

### Task 6: Giant Growth — `Effect.ModifyTarget`, the `Resolve` create-branch, the card

**Files:**
- Modify: `source/library/Pawl/Type/Effect.hs` (`ModifyTarget`), `source/library/Pawl/Resolve.hs` (`slotsOf` + `applyEffect` branch), `source/library/Pawl/Card.hs` (`giantGrowthPrinting`, `allPrintings`), `source/test-suite/Pawl/Support.hs` (a `forestsInPlay` helper if convenient), `source/test-suite/Pawl/ProjectionSpec.hs` (cast tests), `source/test-suite/Pawl/CardSpec.hs` (lint count)

**Interfaces:**
- Consumes: `Game.freshTimestamp`, `Target.stillLegal`, `Affected.TheseObjects`, `ContinuousEffect.MkContinuousEffect`, `Object.targets`.
- Produces: `Effect.ModifyTarget Duration Modification SlotName`; `Card.giantGrowthPrinting :: Printing`. `Resolve.resolveSpell` now appends a stored `ContinuousEffect` for a `ModifyTarget` effect.

- [x] **Step 1: Write the failing tests**

In `ProjectionSpec.hs` (add imports `qualified Pawl.Cast as Cast`, `qualified Pawl.Engine as Engine`, `qualified Pawl.Stack as Stack`, `qualified Pawl.Type.Phase as Phase`):

```haskell
-- alice has a Forest for mana, a Piker on the battlefield, and Giant Growth in
-- hand, in her main phase. Cast Giant Growth (identityAnswer targets the only
-- creature), then resolve it.
giantGrowthOnPiker :: (ObjectId.ObjectId, GameState.GameState)
giantGrowthOnPiker =
  let base = S.landsInPlay Card.forestPrinting 1
      (pikerId, withPiker) = S.addPiker S.alice base
      (gs, ggId) = S.handOne Card.giantGrowthPrinting withPiker
      cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice ggId))
      resolved = Stack.resolveTop cast
   in (pikerId, resolved)
```

```haskell
      HU.testCase "CR 611 Giant Growth stores a +3/+3 effect; the Piker is 5/4" $
        let (pikerId, gs) = giantGrowthOnPiker
         in do
              HU.assertEqual "one stored effect" 1 (length (GameState.continuousEffects gs))
              HU.assertEqual "power" (Just 5) (Projection.powerOf pikerId gs)
              HU.assertEqual "toughness" (Just 4) (Projection.toughnessOf pikerId gs),
      HU.testCase "CR 601.2c Giant Growth is uncastable with no creature to target" $
        let (gs, ggId) = S.handOne Card.giantGrowthPrinting (S.landsInPlay Card.forestPrinting 1)
         in HU.assertBool "no legal target, not castable" (not (Cast.castable S.alice ggId gs))
```

- [x] **Step 2: Run and watch them fail**

Run: `cabal test 2>&1 | tail -30` → `Card.giantGrowthPrinting` / `Effect.ModifyTarget` not in scope.

- [x] **Step 3: Implement**

`Pawl.Type.Effect` gains the constructor (import `Pawl.Type.Duration (Duration)`, `Pawl.Type.Modification (Modification)`):

```haskell
data Effect
  = DealDamage SlotName Quantity
  | -- CR 611: create a continuous effect on the slot's target for a duration.
    -- Giant Growth and Serpent's Gift are this one opcode, differing only in the
    -- Modification (layer 7c vs 6). Resolve stores it; it never cases on the
    -- Modification.
    ModifyTarget Duration Modification SlotName
  deriving (Eq, Ord, Show)
```

`Resolve.slotsOf` gains the case:

```haskell
slotsOf effect = case effect of
  Effect.DealDamage slot _ -> Set.singleton slot
  Effect.ModifyTarget _ _ slot -> Set.singleton slot
```

`Resolve.applyEffect` gains the `ModifyTarget` branch (imports: `qualified Pawl.Type.Affected as Affected`, `qualified Pawl.Type.ContinuousEffect as ContinuousEffect`; `Game.freshTimestamp` is available):

```haskell
  Effect.ModifyTarget duration modification slot ->
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just (Recipient.ToCreature target), True) ->
        -- CR 611.2c: the affected set is locked to this one object now. The
        -- Modification's quantities are stored as-is (Literals); CR 611.2b's
        -- freeze is a no-op until X exists, at which point evaluate-and-freeze
        -- here. See the M3b spec, section 3.
        let (ts, gs1) = Game.freshTimestamp gs
            eff =
              ContinuousEffect.MkContinuousEffect
                { ContinuousEffect.source = source,
                  ContinuousEffect.timestamp = ts,
                  ContinuousEffect.duration = duration,
                  ContinuousEffect.modification = modification,
                  ContinuousEffect.affected = Affected.TheseObjects (Set.singleton target)
                }
         in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}
      -- A creature-only modification cannot land on a player (CreatureTarget) or
      -- an illegal slot (CR 608.2b): no-op.
      _ -> gs
```

(Ensure `Data.Set as Set`, `Recipient`, `GameState` are imported in `Resolve` — `Set` and `GameState` are, from M3a; add `Pawl.Type.Recipient as Recipient` if absent.)

In `Pawl.Card` (imports `Duration`, `Modification` as needed):

```haskell
-- Giant Growth: {G}, Instant, "Target creature gets +3/+3 until end of turn."
-- Scryfall-verified. Layer 7c, until end of turn -- the first duration effect and
-- the first creature-only target.
giantGrowthPrinting :: Printing.Printing
giantGrowthPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Giant Growth",
            Card.manaCost = Just (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Green)]),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Instant,
                  TypeLine.subtypes = Set.empty
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty,
            Card.staticAbilities = [],
            Card.effects = [Effect.ModifyTarget Duration.UntilEndOfTurn (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)) (SlotName.MkSlotName (Text.pack "target"))],
            Card.targetSpecs = Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.CreatureTarget
          }
    }
```

Add `giantGrowthPrinting` to `allPrintings`. Before finalizing the comment, check Giant Growth's Gatherer rulings per design.md §4: `curl -s -H "User-Agent: pawl/1.0" "https://api.scryfall.com/cards/named?exact=Giant+Growth" | python3 -c "import json,sys; print(json.load(sys.stdin)['rulings_uri'])"` and fetch that URI; if any ruling is Q&A-shaped *and* expressible in this pool, STOP and tell the user. In `CardSpec.hs`, bump the `allPrintings` count assertion (14 → 15) if such a test exists.

- [x] **Step 4: Run the full suite**

Run: `cabal test 2>&1 | tail -5` → PASS. The slot lint (over `allPrintings`) now covers Giant Growth: its one effect reads slot `"target"`, its `targetSpecs` declares `"target"` — equal, so the lint is green.

- [x] **Step 5: Commit**

```bash
git add source/library/Pawl/Type/Effect.hs source/library/Pawl/Resolve.hs source/library/Pawl/Card.hs source/test-suite/Pawl/ProjectionSpec.hs source/test-suite/Pawl/CardSpec.hs pawl.cabal
hooky fix && git add -u && git add pawl.cabal && hooky run
git commit -m "Add Giant Growth: the ModifyTarget opcode and stored duration effect

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01Sni8JpsTfduJC15gLtLc3e"
```

---

### Task 7: Cleanup wear-off — CR 514.2 drops `UntilEndOfTurn` effects

**Files:**
- Modify: `source/library/Pawl/Projection.hs` (a `dropEndOfTurnEffects` helper, or put it in `Pawl.Game` if it fits better — it touches only `GameState.continuousEffects`), `source/library/Pawl/Engine.hs` (cleanup step), `source/test-suite/Pawl/ProjectionSpec.hs` (test)

**Interfaces:**
- Produces: a pure `dropEndOfTurnEffects :: GameState -> GameState` dropping every `UntilEndOfTurn` continuous effect. `Engine`'s cleanup step calls it.

- [x] **Step 1: Write the failing test**

In `ProjectionSpec.hs` (imports `qualified Pawl.Type.BeginningStep as BeginningStep`, `qualified Pawl.Type.EndingStep as EndingStep` as needed):

```haskell
      HU.testCase "CR 514.2 an until-end-of-turn effect wears off at cleanup" $
        let (pikerId, cast) = giantGrowthOnPiker
            -- Run the cleanup step's turn-based actions; the +3/+3 must be gone.
            afterCleanup = snd (Engine.runGamePure S.identityAnswer cast (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)))
         in do
              HU.assertEqual "effect dropped" [] (GameState.continuousEffects afterCleanup)
              HU.assertEqual "Piker back to base power" (Just 2) (Projection.powerOf pikerId afterCleanup)
              HU.assertEqual "Piker back to base toughness" (Just 1) (Projection.toughnessOf pikerId afterCleanup)
```

(`giantGrowthOnPiker` from Task 6 leaves the Piker on the battlefield with the stored effect; note the Piker id survives cleanup because a battlefield permanent is not re-created — only the *effect* is dropped.)

- [x] **Step 2: Run and watch it fail**

Run: `cabal test 2>&1 | tail -20` → the effect is still present after cleanup (power 5).

- [x] **Step 3: Implement**

In `Pawl.Projection` (imports `qualified Pawl.Type.ContinuousEffect as ContinuousEffect`, `qualified Pawl.Type.Duration as Duration` — already present):

```haskell
-- CR 514.2: during cleanup, "all 'until end of turn' and 'this turn' effects
-- end". Delete-and-recompute (design.md §2.5): dropping the stored effect makes
-- the next projection revert -- nothing is explicitly undone.
dropEndOfTurnEffects :: GameState -> GameState
dropEndOfTurnEffects gs =
  let keep eff = ContinuousEffect.duration eff /= Duration.UntilEndOfTurn
   in gs {GameState.continuousEffects = filter keep (GameState.continuousEffects gs)}
```

In `Pawl.Engine`, the cleanup arm (currently `discardToHandSize active` then `State.modify' Damage.removeAllDamage`) gains a sibling (import `qualified Pawl.Projection as Projection` if absent):

```haskell
    Phase.Ending EndingStep.Cleanup -> do
      discardToHandSize active
      -- CR 514.2: damage wears off AND until-end-of-turn effects end,
      -- simultaneously.
      State.modify' Damage.removeAllDamage
      State.modify' Projection.dropEndOfTurnEffects
```

- [x] **Step 4: Run the full suite**

Run: `cabal test 2>&1 | tail -5` → PASS.

- [x] **Step 5: Commit**

```bash
git add source/library/Pawl/Projection.hs source/library/Pawl/Engine.hs source/test-suite/Pawl/ProjectionSpec.hs
hooky fix && git add -u && hooky run
git commit -m "Drop until-end-of-turn effects at cleanup (CR 514.2)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01Sni8JpsTfduJC15gLtLc3e"
```

---

### Task 8: Humility — the static-ability card

**Files:**
- Modify: `source/library/Pawl/Card.hs` (`humilityPrinting`, `allPrintings`), `source/library/Pawl/Type/CardType.hs` (`Enchantment` if absent — check first), `source/test-suite/Pawl/Support.hs` (a `withHumility` fixture helper), `source/test-suite/Pawl/ProjectionSpec.hs` (tests), `source/test-suite/Pawl/CardSpec.hs` (lint count)

**Interfaces:**
- Consumes: `Card.staticAbilities`, `StaticAbility.MkStaticAbility`, `Affected.AllCreatures`, `Modification.{LoseAllAbilities, SetBasePowerToughness}`.
- Produces: `Card.humilityPrinting :: Printing`; `S.withHumility :: GameState -> GameState` (places a Humility on the battlefield under bob's control).

- [x] **Step 1: Write the failing tests**

`S.withHumility` in `Support.hs` (reuses `addCreature`, which places any printing on the battlefield):

```haskell
-- Humility on the battlefield under bob's control (it is not a creature, so
-- AllCreatures does not touch it). Returns the updated state.
withHumility :: GameState.GameState -> GameState.GameState
withHumility gs = snd (addCreature Card.humilityPrinting bob gs)
```

In `ProjectionSpec.hs`:

```haskell
      HU.testCase "CR 613 Humility makes every creature 1/1 with no abilities" $
        let (flyerId, gs0) = S.addCreature Card.birdMaidenPrinting S.bob (S.mountainsInPlay 1)
            gs = S.withHumility gs0
         in do
              HU.assertEqual "power 1" (Just 1) (Projection.powerOf flyerId gs)
              HU.assertEqual "toughness 1" (Just 1) (Projection.toughnessOf flyerId gs)
              HU.assertBool "no flying" (not (Projection.hasKeyword Keyword.Flying flyerId gs)),
      HU.testCase "CR 704.5g Humility's toughness drop makes an already-damaged creature die" $
        let (mammothId, gs0) = S.addCreature Card.warMammothPrinting S.bob (S.mountainsInPlay 1)
            damaged = S.markDamage mammothId 2 gs0
            underHumility = S.withHumility damaged
            afterSba = Sba.checkStateBasedActions underHumility
         in do
              HU.assertEqual "survives at 3/3 with 2 marked" (Just 3) (Projection.toughnessOf mammothId damaged)
              HU.assertEqual "no creature survives once toughness is 1" 0 (S.creaturesInPlay S.bob afterSba),
      HU.testCase "CR 613 layer order: Giant Growth on a Humility'd Piker is 4/4" $
        let base = S.landsInPlay Card.forestPrinting 1
            (pikerId, withPiker) = S.addPiker S.alice base
            withHum = S.withHumility withPiker
            (gs, ggId) = S.handOne Card.giantGrowthPrinting withHum
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice ggId))
            resolved = Stack.resolveTop cast
         in do
              -- Layer 7b (set 1/1) before 7c (+3/+3): 1 then +3 = 4.
              HU.assertEqual "power" (Just 4) (Projection.powerOf pikerId resolved)
              HU.assertEqual "toughness" (Just 4) (Projection.toughnessOf pikerId resolved)
```

(Add `import qualified Pawl.Sba as Sba` to `ProjectionSpec.hs`.)

- [x] **Step 2: Run and watch them fail**

Run: `cabal test 2>&1 | tail -30` → `Card.humilityPrinting` not in scope.

- [x] **Step 3: Implement**

Check `Pawl.Type.CardType` for an `Enchantment` constructor; if absent, add it (comment ordered like the others) and give `Card.isPermanentType` the `CardType.Enchantment -> True` case (CR 110.1 lists enchantment among permanent types). Then in `Pawl.Card`:

```haskell
-- Humility: {2}{W}{W}, Enchantment, "All creatures lose all abilities and have
-- base power and toughness 1/1." Scryfall-verified. Two static abilities: layer 6
-- (lose all abilities) and layer 7b (set base 1/1), each over AllCreatures. No
-- targets, no effects -- the projection gathers it live while it is on the
-- battlefield. White, so it is a deterministic fixture only (no white matchup).
humilityPrinting :: Printing.Printing
humilityPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Humility",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.OfType (ManaType.Generic 2),
                      ManaSymbol.OfType (ManaType.Colored Color.White),
                      ManaSymbol.OfType (ManaType.Colored Color.White)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Enchantment,
                  TypeLine.subtypes = Set.empty
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty,
            Card.staticAbilities =
              [ StaticAbility.MkStaticAbility Affected.AllCreatures Modification.LoseAllAbilities,
                StaticAbility.MkStaticAbility Affected.AllCreatures (Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1))
              ],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }
```

(Check `ManaType.Generic`/`ManaSymbol.OfType` spelling against how existing multi-symbol costs are written in `Pawl.Card` — the Piker's `{1}{R}` is the template; match it exactly. Import `StaticAbility`, `Affected`, `Modification`.) Add `humilityPrinting` to `allPrintings`; bump the `CardSpec` count (15 → 16). Pull Humility's Gatherer rulings per design.md §4 before finalizing the comment (it has only 3 today; check whether any is Q&A-shaped and expressible — layer interactions mostly are not, but verify).

- [x] **Step 4: Run the full suite**

Run: `cabal test 2>&1 | tail -5` → PASS.

- [x] **Step 5: Commit**

```bash
git add source/library/Pawl/Card.hs source/library/Pawl/Type/CardType.hs source/test-suite/Pawl/Support.hs source/test-suite/Pawl/ProjectionSpec.hs source/test-suite/Pawl/CardSpec.hs pawl.cabal
hooky fix && git add -u && git add pawl.cabal && hooky run
git commit -m "Add Humility: static abilities in layers 6 and 7b

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01Sni8JpsTfduJC15gLtLc3e"
```

---

### Task 9: Serpent's Gift — the layer-6 grant and the timestamp interaction

**Files:**
- Modify: `source/library/Pawl/Card.hs` (`serpentsGiftPrinting`, `allPrintings`), `source/test-suite/Pawl/ProjectionSpec.hs` (tests), `source/test-suite/Pawl/CardSpec.hs` (lint count)

**Interfaces:**
- Consumes: `Effect.ModifyTarget` with `Modification.GainKeyword` (Task 6's opcode, no new engine code).
- Produces: `Card.serpentsGiftPrinting :: Printing`.

- [x] **Step 1: Write the failing tests**

In `ProjectionSpec.hs`:

```haskell
      HU.testCase "CR 611 Serpent's Gift grants deathtouch to its target" $
        let base = S.landsInPlay Card.forestPrinting 2
            (mammothId, withMammoth) = S.addCreature Card.warMammothPrinting S.alice base
            (gs, sgId) = S.handOne Card.serpentsGiftPrinting withMammoth
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice sgId))
            resolved = Stack.resolveTop cast
         in do
              HU.assertBool "keeps trample" (Projection.hasKeyword Keyword.Trample mammothId resolved)
              HU.assertBool "gains deathtouch" (Projection.hasKeyword Keyword.Deathtouch mammothId resolved),
      HU.testCase "CR 613.7 layer 6: a grant older than Humility is erased; newer survives" $
        -- War Mammoth and Humility on the battlefield; a directly-built
        -- Serpent's-Gift effect (GainKeyword Deathtouch, the same value the card
        -- creates) whose timestamp straddles Humility's object timestamp, to
        -- witness BOTH orders of CR 613.7 in layer 6. h-1 and h+1 make the
        -- relative order exact, not a guess.
        let (mammothId, gs0) = S.addCreature Card.warMammothPrinting S.bob (S.mountainsInPlay 1)
            withHum = S.withHumility gs0
            Timestamp.MkTimestamp h = humilityTimestamp withHum
            olderGrant = withEffect mammothId (Timestamp.MkTimestamp (h - 1)) (Modification.GainKeyword Keyword.Deathtouch) withHum
            newerGrant = withEffect mammothId (Timestamp.MkTimestamp (h + 1)) (Modification.GainKeyword Keyword.Deathtouch) withHum
         in do
              HU.assertBool "grant before Humility: erased" (not (Projection.hasKeyword Keyword.Deathtouch mammothId olderGrant))
              HU.assertBool "grant after Humility: survives" (Projection.hasKeyword Keyword.Deathtouch mammothId newerGrant)
```

with the helper (in `ProjectionSpec.hs`) that reads Humility's object timestamp (`h` is a `Natural`; `withHumility` places Humility after the Mammoth and the land, so `h >= 2` and `h - 1` is safe):

```haskell
-- The object timestamp of the (single) Humility on the battlefield.
humilityTimestamp :: GameState.GameState -> Timestamp.Timestamp
humilityTimestamp gs =
  let isHum oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard p -> Printing.card p == Printing.card Card.humilityPrinting
      hums = filter isHum (Set.toList (GameState.battlefield gs))
      stampOf oid = fmap Object.timestamp (Game.lookupObject oid gs)
   in case Maybe.mapMaybe stampOf hums of
        t : _ -> t
        [] -> Timestamp.MkTimestamp 0
```

(Add imports `qualified Pawl.Type.Source as Source`, `qualified Pawl.Type.Printing as Printing`, `qualified Data.Maybe as Maybe` to `ProjectionSpec.hs`. `withEffect` is Task 4's helper.)

- [x] **Step 2: Run and watch them fail**

Run: `cabal test 2>&1 | tail -30` → `Card.serpentsGiftPrinting` not in scope.

- [x] **Step 3: Implement**

In `Pawl.Card` (mirror Giant Growth, but `{2}{G}` and `GainKeyword Deathtouch`):

```haskell
-- Serpent's Gift: {2}{G}, Instant, "Target creature gains deathtouch until end of
-- turn." Scryfall-verified. Layer 6, until end of turn -- the same ModifyTarget
-- opcode as Giant Growth with a GainKeyword modification. Granting deathtouch to
-- War Mammoth rebuilds M2c's synthetic deathtouch+trample fixture from real cards.
serpentsGiftPrinting :: Printing.Printing
serpentsGiftPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Serpent's Gift",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.OfType (ManaType.Generic 2),
                      ManaSymbol.OfType (ManaType.Colored Color.Green)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Instant,
                  TypeLine.subtypes = Set.empty
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty,
            Card.staticAbilities = [],
            Card.effects = [Effect.ModifyTarget Duration.UntilEndOfTurn (Modification.GainKeyword Keyword.Deathtouch) (SlotName.MkSlotName (Text.pack "target"))],
            Card.targetSpecs = Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.CreatureTarget
          }
    }
```

(Match the `{2}{G}` template to how the code writes generic + colored symbols; `Keyword` is imported in `Pawl.Card` already.) Add `serpentsGiftPrinting` to `allPrintings`; bump the `CardSpec` count (16 → 17). Pull Serpent's Gift's rulings per design.md §4.

- [x] **Step 4: Run the full suite**

Run: `cabal test 2>&1 | tail -5` → PASS.

- [x] **Step 5: Commit**

```bash
git add source/library/Pawl/Card.hs source/test-suite/Pawl/ProjectionSpec.hs source/test-suite/Pawl/CardSpec.hs pawl.cabal
hooky fix && git add -u && git add pawl.cabal && hooky run
git commit -m "Add Serpent's Gift: a layer-6 deathtouch grant, and the CR 613.7 order

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01Sni8JpsTfduJC15gLtLc3e"
```

---

### Task 10: The deal-time deathtouch bit, and the real-card 702.2c interaction

**Files:**
- Modify: `source/library/Pawl/Type/DamageEvent.hs` (`dealtByDeathtouch` field), `source/library/Pawl/Damage.hs` (set the bit at construction), `source/library/Pawl/Resolve.hs` (set the bit — `False` — in its `MkDamageEvent`), `source/library/Pawl/Sba.hs` (`woundedByDeathtouch` reads the bit), `source/test-suite/Pawl/DamageSpec.hs` (retire the synthetic fixture; add the real-card interaction), plus test `MkDamageEvent` sites found by grep

**Interfaces:**
- Produces: `DamageEvent.dealtByDeathtouch :: Bool`, set from the projected deathtouch of the source at deal time; `Sba.woundedByDeathtouch` reads it.

- [x] **Step 1: Write the failing tests**

In `DamageSpec.hs`, replace the `syntheticDeathtramplerPrinting`-based `trampleDeathtouchTests` with real cards. The synthetic 3/3 deathtoucher+trampler becomes War Mammoth (3/3 trample) granted deathtouch by a directly-appended Serpent's-Gift effect (no mana plumbing in a combat fixture). Add a local helper to `DamageSpec.hs`:

```haskell
-- Grant deathtouch to `oid` the way Serpent's Gift does: a stored continuous
-- effect over just that object. Timestamp is arbitrary (no competing layer-6
-- effect in these fixtures).
grantDeathtouch :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
grantDeathtouch oid gs =
  let eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = ObjectId.MkObjectId 997,
            ContinuousEffect.timestamp = Timestamp.MkTimestamp 500,
            ContinuousEffect.duration = Duration.UntilEndOfTurn,
            ContinuousEffect.modification = Modification.GainKeyword Keyword.Deathtouch,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
          }
   in gs {GameState.continuousEffects = eff : GameState.continuousEffects gs}
```

The synthetic 3/3 deathtoucher+trampler becomes War Mammoth (3/3 trample) with `grantDeathtouch` — the existing `tramplingAnswer` (the group's assignment answerer for the trample split) and the existing control test carry over. **Use `tramplingAnswer`/`aggressiveAnswer`, never `identityAnswer`, for a combat test — `identityAnswer` declares no attackers, so no damage is dealt.**

```haskell
trampleDeathtouchTests :: Tasty.TestTree
trampleDeathtouchTests =
  Tasty.testGroup
    "TrampleDeathtouch"
    [ HU.testCase "CR 702.2c a deathtouch-granted trampler needs only 1 on the blocker, spilling the rest" $
        -- War Mammoth (3/3 trample) GRANTED deathtouch into Ogre Sentry (3/3):
        -- lethal collapses to 1, so 1 to the Ogre and 2 tramples to bob; the Ogre
        -- still dies (704.5h, via the deal-time bit). Real cards replace M2c's
        -- synthetic deathtrampler.
        let (gs0, mammoths, _) = S.combatBoardOf [Card.warMammothPrinting] [Card.ogreSentryPrinting]
            mammothId = case mammoths of
              m : _ -> m
              [] -> ObjectId.MkObjectId 999
            gs = grantDeathtouch mammothId gs0
            after = Sba.checkStateBasedActions (S.fightWith tramplingAnswer gs)
         in do
              HU.assertEqual "bob took the 2 overflow" (Just 18) (S.lifeOf S.bob after)
              HU.assertEqual "the Ogre is dead" 0 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 702.19b the control: plain trample into the same 3/3 spills nothing" $
        -- War Mammoth (3/3 trample, NO deathtouch) into Ogre Sentry (3/3): lethal
        -- is 3, all 3 go to the Ogre, 0 tramples. Only deathtouch changes the spill.
        let (gs, _, _) = S.combatBoardOf [Card.warMammothPrinting] [Card.ogreSentryPrinting]
            after = Sba.checkStateBasedActions (S.fightWith tramplingAnswer gs)
         in HU.assertEqual "bob untouched without deathtouch" (Just 20) (S.lifeOf S.bob after)
    ]
```

Delete `syntheticDeathtramplerPrinting` and its now-dead imports. Add to `DamageSpec.hs` the imports the helpers use: `qualified Pawl.Type.ContinuousEffect as ContinuousEffect`, `qualified Pawl.Type.Duration as Duration`, `qualified Pawl.Type.Modification as Modification`, `qualified Pawl.Type.Affected as Affected`, `qualified Pawl.Type.Timestamp as Timestamp` (keep `Keyword`, `Set`, `ObjectId`, `GameState`, `DamageEvent`).

Add two bit assertions to the `Deathtouch` group. These pin the deal-time bit **directly** on the event, so Humility's P/T side effects cannot confound them (under Humility everything is 1/1, so a survival check would be about 704.5g lethal damage, not the deathtouch bit):

```haskell
      HU.testCase "CR 702.2e the deal-time bit is true for a real deathtoucher, false for a plain source" $
        -- Typhoid Rats (deathtouch) and Ogre Sentry trade combat damage under
        -- aggressiveAnswer (which DOES declare attackers). fightWith runs no SBAs,
        -- so the wave is still in damageEvents.
        let (gs, rats, ogres) = S.combatBoardOf [Card.typhoidRatsPrinting] [Card.ogreSentryPrinting]
            fought = S.fightWith S.aggressiveAnswer gs
            ratId = case rats of r : _ -> r; [] -> ObjectId.MkObjectId 999
            ogreId = case ogres of o : _ -> o; [] -> ObjectId.MkObjectId 999
            bitFor src = any (\ev -> DamageEvent.source ev == src && DamageEvent.dealtByDeathtouch ev) (GameState.damageEvents fought)
         in do
              HU.assertBool "Rat's damage is flagged deathtouch" (bitFor ratId)
              HU.assertBool "Ogre's damage is not" (not (bitFor ogreId)),
      HU.testCase "CR 702.2e Humility removes deathtouch, so the deal-time bit is false" $
        -- Under Humility the Rat loses deathtouch (layer 6); its combat-damage
        -- event's bit is false -- asserted directly on the event, not via a kill.
        let (gs0, rats, _) = S.combatBoardOf [Card.typhoidRatsPrinting] [Card.ogreSentryPrinting]
            gs = S.withHumility gs0
            fought = S.fightWith S.aggressiveAnswer gs
            ratId = case rats of r : _ -> r; [] -> ObjectId.MkObjectId 999
            ratBit = any (\ev -> DamageEvent.source ev == ratId && DamageEvent.dealtByDeathtouch ev) (GameState.damageEvents fought)
         in HU.assertBool "no deathtouch at deal time under Humility" (not ratBit)
```

- [x] **Step 2: Run and watch them fail**

Run: `cabal test 2>&1 | tail -30` → `DamageEvent.dealtByDeathtouch` not in scope.

- [x] **Step 3: Implement**

`Pawl.Type.DamageEvent` gains the field (comment updated):

```haskell
data DamageEvent = MkDamageEvent
  { source :: ObjectId,
    target :: Recipient,
    amount :: Natural,
    -- CR 702.2e: whether the source had deathtouch WHEN THIS DAMAGE WAS DEALT.
    -- Captured from the projection at deal time (Projection.hasKeyword), not
    -- re-derived at SBA-check time -- last-known information. Read by the CR
    -- 704.5h SBA. See the M3b spec, section 4.
    dealtByDeathtouch :: Bool
  }
  deriving (Eq, Ord, Show)
```

In `Pawl.Damage`, every `MkDamageEvent` gains `Projection.hasKeyword Keyword.Deathtouch <source> gs` as the bit (the `<source>` is the attacker/blocker whose id is the first field). In `attackerAssignment`, `blockerAssignment`, and the two prompt-arm `toEvent` constructions, add the field. For example, `blockerAssignment`:

```haskell
blockerAssignment gs (attacker, blockers) =
  let assign blocker = case Projection.powerOf blocker gs of
        Just p ->
          if p <= 0
            then []
            else [DamageEvent.MkDamageEvent blocker (Recipient.ToCreature attacker) (fromInteger p) (Projection.hasKeyword Keyword.Deathtouch blocker gs)]
        Nothing -> []
   in concatMap assign (Set.toList blockers)
```

and in `attackerAssignment` the unblocked and single-blocker events add `(Projection.hasKeyword Keyword.Deathtouch attacker gs)`, and the prompt-arm `toEvent (recipient, n) = DamageEvent.MkDamageEvent attacker recipient n (Projection.hasKeyword Keyword.Deathtouch attacker gs)`.

In `Pawl.Resolve`, the `DealDamage` `MkDamageEvent` gains the bit for a spell source (import `qualified Pawl.Projection as Projection`, `qualified Pawl.Type.Keyword as Keyword`):

```haskell
            else Damage.applyDamage [DamageEvent.MkDamageEvent source recipient (fromInteger n) (Projection.hasKeyword Keyword.Deathtouch source gs)] gs
```

(For a Bolt this is `False` — a spell has no deathtouch keyword. Correct, and it keeps M3a's tests green.)

In `Pawl.Sba.woundedByDeathtouch`, read the bit instead of re-querying (drop the `Projection.hasKeyword`/`Keyword` use there; the comment changes to name the deal-time capture):

```haskell
-- CR 704.5h: a creature with toughness > 0 dealt damage by a deathtouch source
-- since the last SBA check is destroyed. "Deathtouch source" is read from the
-- event's deal-time bit (CR 702.2e last-known information), NOT re-derived now --
-- so a source that lost deathtouch (Humility) or left after dealing damage is
-- still judged by what it was. See the M3b spec, section 4.
woundedByDeathtouch :: GameState -> ObjectId -> Bool
woundedByDeathtouch gs oid =
  let hits ev =
        DamageEvent.target ev == Recipient.ToCreature oid
          && DamageEvent.amount ev > 0
          && DamageEvent.dealtByDeathtouch ev
   in any hits (GameState.damageEvents gs)
```

(If `Sba` no longer uses `Projection`/`Keyword` after this, remove those imports to stay warning-clean — but `creatureDies` still uses `Projection.toughnessOf`, so `Projection` stays; `Keyword` may become unused.) Finally, `grep -rn "MkDamageEvent" source/test-suite/` and add the `dealtByDeathtouch` field (usually `False`) to any test that builds a `DamageEvent` literal.

- [x] **Step 4: Run the full suite**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -3` → clean.
Run: `cabal test 2>&1 | tail -5` → PASS. The M2c deathtouch/trample behavior is preserved (a printed deathtoucher's bit is `True` through the projection), now proven with real cards, and the synthetic fixture is gone.

- [x] **Step 5: Commit**

```bash
git add source/library/Pawl/Type/DamageEvent.hs source/library/Pawl/Damage.hs source/library/Pawl/Resolve.hs source/library/Pawl/Sba.hs source/test-suite/Pawl/DamageSpec.hs
hooky fix && git add -u && hooky run
git commit -m "Capture deathtouch at deal time on the damage event (CR 702.2e)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01Sni8JpsTfduJC15gLtLc3e"
```

---

### Task 11: The green deck, the properties, and the deck-composition counts

**Files:**
- Modify: `source/library/Pawl/Setup.hs` (`greenDeck`), `source/test-suite/Pawl/SetupSpec.hs` (composition counts), `source/test-suite/Pawl/PropertySpec.hs` (engagement guards)

**Interfaces:**
- Consumes: everything prior. Produces the M3b random-game coverage.

- [x] **Step 1: Write the failing tests**

In `SetupSpec.hs`, update `greenDeck`'s composition assertion to 36 Forest / 16 War Mammoth / 4 Giant Growth / 4 Serpent's Gift (still 36 land + 24 spells = 60), and any post-setup count that referenced 24 Mammoths. In `PropertySpec.hs`, add engagement guards mirroring M3a's "instants happen":

```haskell
      QC.testProperty "continuous effects happen: some green-black seed casts Giant Growth" $
        QC.once (QC.property (any (castsNamed (Text.pack "Giant Growth")) [1 .. 100 :: Int])),
      QC.testProperty "grants happen: some green-black seed casts Serpent's Gift" $
        QC.once (QC.property (any (castsNamed (Text.pack "Serpent's Gift")) [1 .. 100 :: Int]))
```

with a helper (in `PropertySpec.hs`, generalizing M3a's `boltCast_`):

```haskell
-- Did this seed's green-black game put a card of this name into a graveyard? A
-- cast instant always ends there (resolved or fizzled); nothing else moves a
-- Giant Growth or Serpent's Gift out of a library.
castsNamed :: Text.Text -> Int -> Bool
castsNamed name s =
  let gs = S.runRandomGame S.greenBlack s
      named oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard printing -> Card.Type.name (Printing.card printing) == name
      inGrave pid = any named (Game.zoneMembers Zone.Graveyard pid gs)
   in any inGrave [S.alice, S.bob]
```

(Import `Text`, `Card.Type`, `Printing`, `Source`, `Zone`, `Object`, `Game` in `PropertySpec.hs` if not present.)

- [x] **Step 2: Run and watch them fail**

Run: `cabal test 2>&1 | tail -30` → composition asserts the new counts; `greenDeck` still says 24 Mammoths.

- [x] **Step 3: Implement**

`Setup.greenDeck` becomes (comment notes the split keeps 36 land + 24 spells, and that Giant Growth/Serpent's Gift give continuous effects random-game coverage; War Mammoth stays plentiful so combat still happens and Serpent's Gift has a trampler to grant):

```haskell
greenDeck :: Deck.Deck
greenDeck =
  Deck.MkDeck $
    Map.fromList
      [ (Card.forestPrinting, 36),
        (Card.warMammothPrinting, 16),
        (Card.giantGrowthPrinting, 4),
        (Card.serpentsGiftPrinting, 4)
      ]
```

- [x] **Step 4: Run the full suite twice**

Run: `cabal test 2>&1 | tail -5` → PASS. The property suite now fuzzes continuous effects in every green-black seed; a failure in *any* existing property here (conservation at 120, termination, life-never-increases, green-black engagement) is a real M3b bug — investigate, don't reseed. Run once more to confirm determinism.

- [x] **Step 5: Commit**

```bash
git add source/library/Pawl/Setup.hs source/test-suite/Pawl/SetupSpec.hs source/test-suite/Pawl/PropertySpec.hs
hooky fix && git add -u && hooky run
git commit -m "Put Giant Growth and Serpent's Gift in the green deck for random coverage

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01Sni8JpsTfduJC15gLtLc3e"
```

---

### Task 12: Milestone close-out

**Files:**
- Modify: `CLAUDE.md` (M3b complete bullet; current-work → M3c), `CONTRIBUTING.md` (NamedFieldPuns amendment), `docs/superpowers/plans/2026-07-18-m3b-continuous-effects.md` (this file — all steps ticked)

- [x] **Step 1: Full verification**

```bash
cabal clean && cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -c "warning:"
```
Expected: `0` (the clean build surfaces warnings incremental builds hide; `pedantic` makes them fatal anyway).

```bash
cabal test 2>&1 | tail -5
grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-18-m3b-continuous-effects.md
```
Expected: tests PASS; the grep prints `0` (every step ticked).

Run `cabal bench 2>&1 | tail -8` and eyeball: the per-query projection fold adds cost to every `powerOf`/`toughnessOf`/`keywordsOf` read (combat is read-heavy). Note the numbers in the commit message; a collapse (10x) is a bug — stop and say so. (This is the memoized-projected-state expiry becoming visible; it is watched, not asserted.)

- [x] **Step 2: Record the convention amendment**

In `CONTRIBUTING.md` (the style section) and `CLAUDE.md` (§Code conventions, the "Haskell 2010, no language extensions" bullet), record that `NamedFieldPuns` is permitted where it improves clarity, and that it does not relax the non-punning rule for *constructor* names. Keep it one sentence each, matching the surrounding prose.

- [x] **Step 3: Record M3b complete**

In `CLAUDE.md`, add an "M3b is complete" bullet after M3a's (pattern-match the existing bullets: what landed — the `Pawl.Projection` layer fold and its `Modification`/`Layer`/`Duration`/`Affected`/`ContinuousEffect`/`StaticAbility`/`ProjectedCharacteristics`/`Timestamp` types; `ModifyTarget` as one opcode for Giant Growth and Serpent's Gift; Humility's static abilities; CR 514.2 wear-off; the deal-time deathtouch bit retiring M2c's synthetic fixture; `CreatureTarget`; the green deck; and the spec/plan paths), and update the "Current work" bullet to **M3c** (CR 613.8 dependency via trial application — the go/no-go — Humility+Opalescence and Blood Moon+Urborg, both orders; the test set must exist *before* the resolver, per the risk register).

- [x] **Step 4: Commit**

```bash
git add CLAUDE.md CONTRIBUTING.md docs/superpowers/plans/2026-07-18-m3b-continuous-effects.md
hooky fix && git add -u && hooky run
git commit -m "Record M3b complete: the projection generalized to a layer system

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01Sni8JpsTfduJC15gLtLc3e"
```
