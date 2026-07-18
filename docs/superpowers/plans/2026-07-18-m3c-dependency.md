# M3c Dependency (CR 613.8) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land layer-4 type-changing and CR 613.8 *existence* dependency (via source-liveness) so Blood Moon + Urborg and Humility + Opalescence resolve correctly, both timestamp orders — the M3 go/no-go.

**Architecture:** `Pawl.Projection` generalizes from keywords/P·T (M3b) to the projected **type line** (card types + subtypes), folded layer by layer with affected-sets evaluated against the partial projection. CR 613.8 existence dependency (Blood Moon strips Urborg's rules text, CR 305.7) is resolved by `staticAbilitiesLive`: a permanent's static abilities are gathered only if no live `SetLandSubtype` applies to it — an order-independent fixpoint, not a topological reorder. The projected type line then feeds mana (CR 305.6), `Sba`, `Target`, and `Combat`.

**Tech Stack:** Haskell 2010 (GHC 9.14.1), `tasty` (`tasty-hunit` + `tasty-quickcheck`), Cabal. Boot libraries only.

## Global Constraints

Copied from the spec and `CLAUDE.md`; every task implicitly includes these:

- **Haskell 2010, no language extensions** beyond `GADTs`, `RankNTypes`, `NamedFieldPuns`. No `LambdaCase`, no `OverloadedStrings`, **no list comprehensions**.
- **No explicit export lists** (`module Pawl.Foo where`).
- **One type per module** under `Pawl.Type.<TypeName>` (type + instances only); logic in other `Pawl.*` modules. A module never imports its parents.
- **Qualified imports, aliased to the last component** (`Data.List` → `List`); operators unqualified; one import group.
- **No partial functions** — `Maybe`/`Either`, never `head`/`error`/non-exhaustive matches.
- **`newtype` + `Mk`-prefixed, non-punning constructors**; build records with `do`/record syntax.
- **Prefer explicit:** `case` over point-free; `let` over `where`; `$` over parens, `.` over chained `$`; `Text` not `String`; arbitrary-precision numbers.
- **No boolean blindness**; **derive at least `Eq` and `Show`**.
- **Warning-clean** under `-Weverything` minus the allow-list; the `pedantic` flag makes any warning a build failure. Build `all`: `cabal build all --enable-tests --enable-benchmarks`. When in doubt, `cabal clean` first — incremental builds hide warnings.
- **The two invariants:** the rules core never `case`s on an effect's *identity*; `Pawl.Projection` is the **sole** `case`-on-`Modification` home (as `Pawl.Resolve` is for `Effect`). The engine makes no player choices.
- **Every rules claim cited** against `docs/rules.txt` in a code comment. Never trust recalled Magic rules.
- **After each task:** `cabal build all --enable-tests --enable-benchmarks` warning-free, `cabal test`, `git add -A` (stage explicit paths under a shared worktree) then `hooky fix` && `hooky run`, HLint clean. Commit directly to `main`, one small complete commit per task.

**Commit message footer** (every commit):

```
Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01NTJULXEgqPcgT5gbizas3U
```

**Cards (Scryfall-verified 2026-07-18):**
- **Blood Moon** — `{2}{R}` Enchantment — "Nonbasic lands are Mountains."
- **Urborg, Tomb of Yawgmoth** — Legendary Land (no cost) — "Each land is a Swamp in addition to its other land types."
- **Opalescence** — `{2}{W}{W}` Enchantment — "Each other non-Aura enchantment is a creature in addition to its other types and has base power and base toughness each equal to its mana value."

---

## Phase 1 — Axis A: the projected type line (Tasks 1–5)

### Task 1: `ProjectedCharacteristics` gains the projected type line

**Files:**
- Modify: `source/library/Pawl/Type/ProjectedCharacteristics.hs`
- Modify: `source/library/Pawl/Projection.hs` (`baseCharacteristics`; add `subtypesOf`/`cardTypesOf`/`isCreatureOf`)
- Test: `source/test-suite/Pawl/ProjectionSpec.hs`

**Interfaces:**
- Produces: `ProjectedCharacteristics` with new fields `cardTypes :: Set CardType`, `subtypes :: Set Subtype`, `rulesTextActive :: Bool`; `Projection.subtypesOf :: ObjectId -> GameState -> Set Subtype`, `Projection.cardTypesOf :: ObjectId -> GameState -> Set CardType`, `Projection.isCreatureOf :: ObjectId -> GameState -> Bool`.

- [x] **Step 1: Write the failing test**

Add to the `tests` list in `source/test-suite/Pawl/ProjectionSpec.hs` (new imports `qualified Pawl.Type.CardType as CardType` and `qualified Pawl.Type.Subtype as Subtype`):

```haskell
      HU.testCase "projected type line: a Piker is a Creature - Goblin Warrior" $
        let (oid, gs) = S.addPiker S.bob (S.mountainsInPlay 1)
         in do
              HU.assertBool "is a creature" (Projection.isCreatureOf oid gs)
              HU.assertEqual "card types" (Set.singleton CardType.Creature) (Projection.cardTypesOf oid gs)
              HU.assertEqual "subtypes" (Set.fromList [Subtype.Goblin, Subtype.Warrior]) (Projection.subtypesOf oid gs),
      HU.testCase "projected type line: a Mountain is a Land - Mountain, not a creature" $
        let gs = S.mountainsInPlay 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
         in do
              HU.assertBool "not a creature" (not (Projection.isCreatureOf landId gs))
              HU.assertEqual "subtypes" (Set.singleton Subtype.Mountain) (Projection.subtypesOf landId gs),
```

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "projected type line"'`
Expected: FAIL to compile — `cardTypesOf`/`subtypesOf`/`isCreatureOf` not in scope.

- [x] **Step 3: Extend `ProjectedCharacteristics`**

Replace the record in `source/library/Pawl/Type/ProjectedCharacteristics.hs` (add imports `qualified Pawl.Type.CardType`… actually import the types unqualified via their modules):

```haskell
module Pawl.Type.ProjectedCharacteristics where

import Data.Set (Set)
import Pawl.Type.CardType (CardType)
import Pawl.Type.Keyword (Keyword)
import Pawl.Type.Subtype (Subtype)

-- The characteristics of an object after the layer fold (design.md §2.5). Maybe
-- P/T because a land has none. cardTypes/subtypes are the projected type line
-- (CR 613 layer 4). rulesTextActive is CR 305.7: False once an effect SETS this
-- object's land subtype to a basic type, stripping its rules-text abilities.
-- No Ord: never sorted, never a key.
data ProjectedCharacteristics = MkProjectedCharacteristics
  { keywords :: Set Keyword,
    power :: Maybe Integer,
    toughness :: Maybe Integer,
    cardTypes :: Set CardType,
    subtypes :: Set Subtype,
    rulesTextActive :: Bool
  }
  deriving (Eq, Show)
```

- [x] **Step 4: Seed the new fields in `baseCharacteristics` and add the reads**

In `source/library/Pawl/Projection.hs`, add imports `qualified Pawl.Type.CardType as CardType`, `qualified Pawl.Type.Subtype as Subtype`, `qualified Pawl.Type.TypeLine as TypeLine`. Replace `baseCharacteristics`:

```haskell
-- Printed characteristics before any effect (CR 613.2/613.4 starting point).
baseCharacteristics :: ObjectId -> GameState -> ProjectedCharacteristics
baseCharacteristics oid gs = case Game.cardOf oid gs of
  Nothing ->
    PC.MkProjectedCharacteristics
      { PC.keywords = Set.empty,
        PC.power = Nothing,
        PC.toughness = Nothing,
        PC.cardTypes = Set.empty,
        PC.subtypes = Set.empty,
        PC.rulesTextActive = True
      }
  Just card ->
    PC.MkProjectedCharacteristics
      { PC.keywords = Card.Type.keywords card,
        PC.power = case Card.Type.power card of
          Nothing -> Nothing
          Just (Power.MkPower q) -> Quantity.evaluate gs oid q,
        PC.toughness = case Card.Type.toughness card of
          Nothing -> Nothing
          Just (Toughness.MkToughness q) -> Quantity.evaluate gs oid q,
        PC.cardTypes = TypeLine.types (Card.Type.typeLine card),
        PC.subtypes = TypeLine.subtypes (Card.Type.typeLine card),
        PC.rulesTextActive = True
      }
```

Add the three reads next to `keywordsOf`:

```haskell
subtypesOf :: ObjectId -> GameState -> Set Subtype.Subtype
subtypesOf oid gs = PC.subtypes (project oid gs)

cardTypesOf :: ObjectId -> GameState -> Set CardType.CardType
cardTypesOf oid gs = PC.cardTypes (project oid gs)

-- CR 305.2 / 613.1d: creature-ness is the projected card-type question, the same
-- projection posture as keywordsOf. An Opalescence'd enchantment is a creature.
isCreatureOf :: ObjectId -> GameState -> Bool
isCreatureOf oid gs = Set.member CardType.Creature (cardTypesOf oid gs)
```

(`Set` is the bare type from `import Data.Set (Set)`, already in `Projection.hs`; `Set.member` uses the `qualified Data.Set as Set` import, also present.)

- [x] **Step 5: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — new tests green, all M3b projection tests still green (the new fields pass through the existing fold untouched).

- [x] **Step 6: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Project the type line: cardTypes, subtypes, rulesTextActive (CR 613.1d)"
```

---

### Task 2: `Modification` layer-4 constructors + `Supertype.Legendary`

**Files:**
- Modify: `source/library/Pawl/Type/Supertype.hs`
- Modify: `source/library/Pawl/Type/Modification.hs`
- Modify: `source/library/Pawl/Projection.hs` (`layer`, `applyModification`)
- Test: `source/test-suite/Pawl/ProjectionSpec.hs`

**Interfaces:**
- Consumes: `ProjectedCharacteristics` fields from Task 1.
- Produces: `Modification.SetLandSubtype :: Subtype -> Modification`, `Modification.AddLandSubtype :: Subtype -> Modification`, `Modification.AddCardType :: CardType -> Modification`; `Supertype.Legendary`. All three modifications classify to `Layer.Type`.

- [x] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/ProjectionSpec.hs`:

```haskell
      HU.testCase "CR 613.1d layer 4: the three type-changing modifications are Type" $ do
        HU.assertEqual "set land subtype" Layer.Type (Projection.layer (Modification.SetLandSubtype Subtype.Mountain))
        HU.assertEqual "add land subtype" Layer.Type (Projection.layer (Modification.AddLandSubtype Subtype.Swamp))
        HU.assertEqual "add card type" Layer.Type (Projection.layer (Modification.AddCardType CardType.Creature)),
      HU.testCase "CR 613.1d AddLandSubtype gives a Forest the Swamp subtype" $
        let gs0 = S.landsInPlay Card.forestPrinting 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = withEffect landId (Timestamp.MkTimestamp 100) (Modification.AddLandSubtype Subtype.Swamp) gs0
         in HU.assertEqual "Forest and Swamp" (Set.fromList [Subtype.Forest, Subtype.Swamp]) (Projection.subtypesOf landId gs),
      HU.testCase "CR 305.7 SetLandSubtype sets a Forest to only Mountain" $
        let gs0 = S.landsInPlay Card.forestPrinting 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = withEffect landId (Timestamp.MkTimestamp 100) (Modification.SetLandSubtype Subtype.Mountain) gs0
         in HU.assertEqual "only Mountain" (Set.singleton Subtype.Mountain) (Projection.subtypesOf landId gs),
      HU.testCase "CR 613.1d AddCardType makes a land a creature" $
        let gs0 = S.landsInPlay Card.forestPrinting 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = withEffect landId (Timestamp.MkTimestamp 100) (Modification.AddCardType CardType.Creature) gs0
         in HU.assertBool "now a creature" (Projection.isCreatureOf landId gs),
```

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "613.1d"'`
Expected: FAIL to compile — the new `Modification` constructors are not in scope.

- [x] **Step 3: Add `Supertype.Legendary`**

In `source/library/Pawl/Type/Supertype.hs`:

```haskell
data Supertype
  = Basic
  | Legendary
  deriving (Eq, Ord, Show)
```

- [x] **Step 4: Add the `Modification` constructors**

In `source/library/Pawl/Type/Modification.hs`, add the import `import Pawl.Type.CardType (CardType)` and `import Pawl.Type.Subtype (Subtype)`, and extend the type (keep the existing header comment, extend it):

```haskell
data Modification
  = GainKeyword Keyword -- layer 6 (Serpent's Gift)
  | LoseAllAbilities -- layer 6 (Humility)
  | SetBasePowerToughness Quantity Quantity -- layer 7b (Humility 1/1; Opalescence mana value)
  | ModifyPowerToughness Quantity Quantity -- layer 7c (Giant Growth +3/+3)
  | SetLandSubtype Subtype -- layer 4, CR 305.7 set (Blood Moon -> Mountain)
  | AddLandSubtype Subtype -- layer 4, CR 305.7 add (Urborg -> Swamp)
  | AddCardType CardType -- layer 4 (Opalescence -> Creature)
  deriving (Eq, Ord, Show)
```

- [x] **Step 5: Classify and apply them in `Projection`**

Add `qualified Data.Set as Set` is already present. In `source/library/Pawl/Projection.hs`, extend `layer`:

```haskell
layer :: Modification -> Layer
layer m = case m of
  Modification.GainKeyword _ -> Layer.Ability
  Modification.LoseAllAbilities -> Layer.Ability
  Modification.SetBasePowerToughness _ _ -> Layer.SetPT
  Modification.ModifyPowerToughness _ _ -> Layer.ModifyPT
  Modification.SetLandSubtype _ -> Layer.Type
  Modification.AddLandSubtype _ -> Layer.Type
  Modification.AddCardType _ -> Layer.Type
```

Extend `applyModification` with the three arms (append inside the `case m of`):

```haskell
  Modification.AddLandSubtype s ->
    pc {PC.subtypes = Set.insert s (PC.subtypes pc)}
  Modification.AddCardType t ->
    pc {PC.cardTypes = Set.insert t (PC.cardTypes pc)}
  -- CR 305.7: setting a land's subtype to a basic type removes its old land
  -- types AND strips its rules-text abilities (here: keywords and, via
  -- rulesTextActive, its static abilities -- see gather). It gains the new mana
  -- ability from the subtype (CR 305.6, read at the mana call site).
  Modification.SetLandSubtype s ->
    pc
      { PC.subtypes = Set.singleton s,
        PC.keywords = Set.empty,
        PC.rulesTextActive = False
      }
```

- [x] **Step 6: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS. (`withEffect` uses `TheseObjects`, which the existing gather-sort-fold applies in layer order; layer 4 sorts before 6/7, so these land correctly with no restructure yet.)

- [x] **Step 7: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Add layer-4 type-changing modifications and Legendary (CR 305.7)"
```

---

### Task 3: `Quantity.ManaValue`

**Files:**
- Modify: `source/library/Pawl/Type/Quantity.hs`
- Modify: `source/library/Pawl/Quantity.hs`
- Test: `source/test-suite/Pawl/ProjectionSpec.hs` (or `Pawl.CoreSpec`; keep it near the Quantity evaluator — use `ProjectionSpec`)

**Interfaces:**
- Produces: `Quantity.ManaValue :: Quantity`; `Quantity.evaluate gs oid ManaValue` returns the affected object's mana value (CR 202.3) as `Just`, `Just 0` for a card with no mana cost, `Nothing` if the object has no card.

- [x] **Step 1: Write the failing test**

Test `ManaValue` through the projection path (this both exercises `evaluate` and avoids the `Pawl.Quantity` / `Pawl.Type.Quantity` last-component alias collision — `ProjectionSpec` already imports the *type* module as `Quantity`). Add to `source/test-suite/Pawl/ProjectionSpec.hs`:

```haskell
      HU.testCase "CR 202.3 SetBasePowerToughness ManaValue sets a Piker to its mana value ({1}{R} = 2)" $
        let (oid, gs0) = S.addPiker S.bob (S.mountainsInPlay 1)
            gs = withEffect oid (Timestamp.MkTimestamp 100) (Modification.SetBasePowerToughness Quantity.ManaValue Quantity.ManaValue) gs0
         in do
              HU.assertEqual "power = mana value" (Just 2) (Projection.powerOf oid gs)
              HU.assertEqual "toughness = mana value" (Just 2) (Projection.toughnessOf oid gs),
```

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "mana value"'`
Expected: FAIL to compile — `Quantity.ManaValue` not in scope.

- [x] **Step 3: Add the constructor**

In `source/library/Pawl/Type/Quantity.hs`, promote the `newtype` to `data` (its header comment already anticipates this — "it becomes a `data` the moment the second one lands"):

```haskell
data Quantity
  = Literal Integer
  | -- CR 202.3: an object's mana value, computed from its mana cost. A
    -- computed quantity (Opalescence's "base P/T equal to its mana value"),
    -- evaluated against the affected object.
    ManaValue
  deriving (Eq, Ord, Show)
```

- [x] **Step 4: Evaluate it**

In `source/library/Pawl/Quantity.hs`, add imports `qualified Pawl.Game as Game`, `qualified Pawl.Type.Card as Card`, `qualified Pawl.Type.ManaCost as ManaCost`, `qualified Pawl.Type.ManaSymbol as ManaSymbol`. Replace `evaluate`:

```haskell
-- Nothing when the value cannot be determined.
evaluate :: GameState -> ObjectId -> Quantity -> Maybe Integer
evaluate gs oid quantity = case quantity of
  Quantity.Literal n -> Just n
  Quantity.ManaValue -> fmap manaValueOf (Game.cardOf oid gs)

-- CR 202.3: the mana value is the total amount of mana in the cost -- each
-- generic symbol contributes its number, each colored/typed symbol contributes
-- one. A land has no mana cost (CR 202.1), so its mana value is 0.
manaValueOf :: Card.Card -> Integer
manaValueOf card = case Card.manaCost card of
  Nothing -> 0
  Just (ManaCost.MkManaCost symbols) -> sum (map symbolValue symbols)

symbolValue :: ManaSymbol.ManaSymbol -> Integer
symbolValue symbol = case symbol of
  ManaSymbol.Generic n -> toInteger n
  ManaSymbol.OfType _ -> 1
```

**Cycle check:** `Pawl.Quantity` now imports `Pawl.Game`. `Pawl.Game` imports only `Pawl.Type.*` (verified), never `Pawl.Quantity`, so no cycle. If `cabal build` reports a non-exhaustive `case` on `Quantity` in any *other* module, add a `ManaValue` arm there — but `Pawl.Quantity.evaluate` is the expected sole matcher.

- [x] **Step 5: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS.

- [x] **Step 6: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Add the ManaValue quantity and evaluate it (CR 202.3)"
```

---

### Task 4: `Affected` dynamic sets + layer-by-layer `project`

**Files:**
- Modify: `source/library/Pawl/Type/Affected.hs`
- Modify: `source/library/Pawl/Projection.hs` (`affects`, `gather`, `project`; add `isBasic`)
- Test: `source/test-suite/Pawl/ProjectionSpec.hs`

**Interfaces:**
- Consumes: `ProjectedCharacteristics` (Task 1), the layer-4 modifications (Task 2).
- Produces: `Affected.AllLands`, `Affected.AllNonbasicLands`, `Affected.OtherNonAuraEnchantments`; a restructured `project` that folds layer by layer, evaluating each effect's affected-set against the partial projection through the previous layers. `affects :: ObjectId -> ObjectId -> Affected -> ProjectedCharacteristics -> GameState -> Bool` (source, then object). `gather :: GameState -> [Gathered]`.

> **Note (self-exclusion):** Opalescence's "each **other**" needs the effect's *source* to exclude itself. A `Card` cannot carry its future object id, so the exclusion is applied at fold time using the source id gathered alongside each effect — `Affected.OtherNonAuraEnchantments` carries **no** id (a plan refinement of the spec's `AllOtherNonAuraEnchantments ObjectId`).

- [x] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/ProjectionSpec.hs` a helper and a test proving affected-sets read the *partial* (a layer-4 creature-add is visible to a layer-6 `AllCreatures` grant):

```haskell
-- Append a stored continuous effect over a dynamic set, at timestamp `ts`.
withDynamicEffect :: Affected.Affected -> Timestamp.Timestamp -> Modification.Modification -> GameState.GameState -> GameState.GameState
withDynamicEffect aff ts m gs =
  let eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = ObjectId.MkObjectId 997,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.duration = Duration.UntilEndOfTurn,
            ContinuousEffect.modification = m,
            ContinuousEffect.affected = aff
          }
   in gs {GameState.continuousEffects = eff : GameState.continuousEffects gs}
```

```haskell
      HU.testCase "CR 613 affected-set reads the partial: a layer-4 creature-add is seen by a layer-6 AllCreatures grant" $
        let gs0 = S.landsInPlay Card.forestPrinting 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            -- Layer 4 makes the land a creature; layer 6 grants flying to all
            -- creatures. The grant reaches the land ONLY because the affected set
            -- is evaluated after layer 4.
            gs1 = withEffect landId (Timestamp.MkTimestamp 100) (Modification.AddCardType CardType.Creature) gs0
            gs = withDynamicEffect Affected.AllCreatures (Timestamp.MkTimestamp 200) (Modification.GainKeyword Keyword.Flying) gs1
         in HU.assertBool "land gained flying because it became a creature" (Projection.hasKeyword Keyword.Flying landId gs),
```

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "reads the partial"'`
Expected: FAIL — the M3b `project` gathers with a `GameState`-only `affects`, so the land is not yet a creature when `AllCreatures` is evaluated, and it does not gain flying.

- [x] **Step 3: Add the `Affected` constructors**

In `source/library/Pawl/Type/Affected.hs`:

```haskell
data Affected
  = TheseObjects (Set ObjectId)
  | AllCreatures
  | AllLands -- Urborg
  | AllNonbasicLands -- Blood Moon
  | OtherNonAuraEnchantments -- Opalescence ("each other"); self excluded by the effect's source at fold time
  deriving (Eq, Ord, Show)
```

- [x] **Step 4: Restructure `Projection` — `affects`, `gather`, `project`**

In `source/library/Pawl/Projection.hs` add imports `qualified Pawl.Type.Supertype as Supertype`, `qualified Pawl.Type.StaticAbility as StaticAbility` (if not already present), and `qualified Pawl.Type.Object as Object` (present). Define the fold accumulator, the new `affects`, `isBasic`, `gather`, and `project`:

```haskell
-- A continuous effect ready to fold: its source (for OtherNonAuraEnchantments
-- self-exclusion), the set it affects, its layer, its timestamp, and the
-- modification. Projection-internal; not a domain type.
data Gathered = MkGathered
  { gSource :: ObjectId,
    gAffected :: Affected.Affected,
    gLayer :: Layer,
    gTimestamp :: Timestamp,
    gModification :: Modification
  }

-- CR 611.2c / 613: does the effect from `source` apply to `oid`, given the
-- partial projection built by the layers below this one? Fixed sets are a
-- membership test; dynamic sets read the PARTIAL type line, so a layer-4 type
-- change is visible to a later layer. Supertype (nonbasic) is read from the
-- printed type line -- no effect changes a supertype at M3c.
affects :: ObjectId -> ObjectId -> Affected.Affected -> ProjectedCharacteristics -> GameState -> Bool
affects source oid a partial gs =
  let onBattlefield = Set.member oid (GameState.battlefield gs)
      hasType t = Set.member t (PC.cardTypes partial)
   in case a of
        Affected.TheseObjects s -> Set.member oid s
        Affected.AllCreatures -> onBattlefield && hasType CardType.Creature
        Affected.AllLands -> onBattlefield && hasType CardType.Land
        Affected.AllNonbasicLands -> onBattlefield && hasType CardType.Land && not (isBasic oid gs)
        Affected.OtherNonAuraEnchantments -> onBattlefield && oid /= source && hasType CardType.Enchantment

-- CR 205.4a: a basic land is one with the Basic supertype. Read from the printed
-- type line (supertypes are not projected at M3c).
isBasic :: ObjectId -> GameState -> Bool
isBasic oid gs = case Game.cardOf oid gs of
  Nothing -> False
  Just card -> Set.member Supertype.Basic (TypeLine.supertypes (Card.Type.typeLine card))

-- Every continuous effect in the game: stored resolution effects, plus the
-- static abilities of every battlefield permanent (CR 613.7a: with the
-- permanent's own timestamp). NOT filtered by object here -- project filters
-- per layer against the partial. (Task 6 adds the CR 305.7 source-liveness gate.)
gather :: GameState -> [Gathered]
gather gs =
  let fromStored eff =
        MkGathered
          { gSource = ContinuousEffect.source eff,
            gAffected = ContinuousEffect.affected eff,
            gLayer = layer (ContinuousEffect.modification eff),
            gTimestamp = ContinuousEffect.timestamp eff,
            gModification = ContinuousEffect.modification eff
          }
      stored = map fromStored (GameState.continuousEffects gs)
      fromStatic permId permObj sa =
        MkGathered
          { gSource = permId,
            gAffected = StaticAbility.affected sa,
            gLayer = layer (StaticAbility.modification sa),
            gTimestamp = Object.timestamp permObj,
            gModification = StaticAbility.modification sa
          }
      fromPermanent permId = case Game.lookupObject permId gs of
        Nothing -> []
        Just permObj -> case Game.cardOf permId gs of
          Nothing -> []
          Just card -> map (fromStatic permId permObj) (Card.Type.staticAbilities card)
      static_ = concatMap fromPermanent (Set.toList (GameState.battlefield gs))
   in stored ++ static_

-- CR 613: apply continuous effects layer by layer (only the layers with effects,
-- ascending). Within a layer, CR 613.7 timestamp order. An effect's affected set
-- is evaluated against the partial projection through the previous layers.
-- CR 613.8 EXISTENCE dependency is handled by source-liveness in Task 6, not a
-- within-layer reorder; the topological CR 613.8b applies-to reorder is deferred
-- (spec §6, git-bug). design.md §2.5.
project :: ObjectId -> GameState -> ProjectedCharacteristics
project oid gs =
  let cands = gather gs
      layers = Set.toAscList (Set.fromList (map gLayer cands))
      applyLayer partial lyr =
        let here = filter (\c -> gLayer c == lyr && affects (gSource c) oid (gAffected c) partial gs) cands
            ordered = List.sortOn gTimestamp here
            step pc c = applyModification gs oid (gModification c) pc
         in List.foldl' step partial ordered
   in List.foldl' applyLayer (baseCharacteristics oid gs) layers
```

Delete the old M3b `gather` and `project` (the ones with the `(Layer, Timestamp, Modification)` tuple and `List.sortOn (\(l, ts, _) -> (l, ts))`). Remove the now-unused `Layer`/`Timestamp` tuple imports only if they become unused (they are still used by `Gathered`). Keep `affects`'s old two-argument form deleted — the new five-argument form replaces it.

- [x] **Step 5: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — the new "reads the partial" test is green, and every M3b projection test still passes (for a plain creature the partial at layer 6/7 equals its printed type, so `AllCreatures` resolves identically).

- [x] **Step 6: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Fold the projection layer by layer over the partial type line (CR 613)"
```

---

### Task 5: The three card printings

**Files:**
- Modify: `source/library/Pawl/Card.hs` (`bloodMoonPrinting`, `urborgPrinting`, `opalescencePrinting`, `allPrintings`)
- Test: `source/test-suite/Pawl/CardSpec.hs`

**Interfaces:**
- Produces: `Card.bloodMoonPrinting`, `Card.urborgPrinting`, `Card.opalescencePrinting`, each in `Card.allPrintings`.

- [x] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/CardSpec.hs` (match its existing style; it likely asserts printings are registered and shaped — mirror an existing case):

```haskell
      HU.testCase "M3c printings are registered in allPrintings" $ do
        HU.assertBool "Blood Moon" (Card.bloodMoonPrinting `elem` Card.allPrintings)
        HU.assertBool "Urborg" (Card.urborgPrinting `elem` Card.allPrintings)
        HU.assertBool "Opalescence" (Card.opalescencePrinting `elem` Card.allPrintings),
      HU.testCase "Blood Moon is a {2}{R} enchantment with one SetLandSubtype static ability" $
        let card = Printing.card Card.bloodMoonPrinting
         in do
              HU.assertEqual "one static ability" 1 (length (Card.Type.staticAbilities card))
              HU.assertBool "not a permanent target" (Map.null (Card.Type.targetSpecs card)),
```

(Add imports `qualified Pawl.Type.Card as Card.Type`, `qualified Pawl.Type.Printing as Printing`, `qualified Data.Map.Strict as Map` if not present.)

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "M3c printings"'`
Expected: FAIL to compile — the three printings are not in scope.

- [x] **Step 3: Add the printings**

In `source/library/Pawl/Card.hs` (imports `Affected`, `Modification`, `StaticAbility`, `CardType`, `Subtype`, `Supertype`, `TypeLine`, `ManaCost`, `ManaSymbol`, `ManaType`, `Color` are already present). Add before `allPrintings`:

```haskell
-- Blood Moon: {2}{R}, Enchantment, "Nonbasic lands are Mountains."
-- Scryfall-verified. One layer-4 static ability that SETS every nonbasic land's
-- subtype to Mountain (CR 305.7), stripping their rules-text abilities. Red, so
-- it is a deterministic fixture only (no white/blue matchup churn here).
bloodMoonPrinting :: Printing.Printing
bloodMoonPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Blood Moon",
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
                  TypeLine.types = Set.singleton CardType.Enchantment,
                  TypeLine.subtypes = Set.empty
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty,
            Card.staticAbilities =
              [StaticAbility.MkStaticAbility Affected.AllNonbasicLands (Modification.SetLandSubtype Subtype.Mountain)],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }

-- Urborg, Tomb of Yawgmoth: Legendary Land, "Each land is a Swamp in addition to
-- its other land types." Scryfall-verified. One layer-4 static ability that ADDS
-- the Swamp subtype to every land (CR 305.7 add). Nonbasic (Legendary, no Basic),
-- so Blood Moon strips it -- the existence dependency. No mana cost (CR 202.1).
urborgPrinting :: Printing.Printing
urborgPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Urborg, Tomb of Yawgmoth",
            Card.manaCost = Nothing,
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.singleton Supertype.Legendary,
                  TypeLine.types = Set.singleton CardType.Land,
                  TypeLine.subtypes = Set.empty
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty,
            Card.staticAbilities =
              [StaticAbility.MkStaticAbility Affected.AllLands (Modification.AddLandSubtype Subtype.Swamp)],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }

-- Opalescence: {2}{W}{W}, Enchantment, "Each other non-Aura enchantment is a
-- creature in addition to its other types and has base power and base toughness
-- each equal to its mana value." Scryfall-verified. Two static abilities over the
-- same set (each OTHER non-Aura enchantment): layer 4 adds the Creature type,
-- layer 7b sets base P/T to the mana value. White, so it is a deterministic
-- fixture only (no white matchup).
opalescencePrinting :: Printing.Printing
opalescencePrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Opalescence",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 2,
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
              [ StaticAbility.MkStaticAbility Affected.OtherNonAuraEnchantments (Modification.AddCardType CardType.Creature),
                StaticAbility.MkStaticAbility Affected.OtherNonAuraEnchantments (Modification.SetBasePowerToughness Quantity.ManaValue Quantity.ManaValue)
              ],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }
```

Append the three to `allPrintings`:

```haskell
    humilityPrinting,
    serpentsGiftPrinting,
    bloodMoonPrinting,
    urborgPrinting,
    opalescencePrinting
  ]
```

- [x] **Step 4: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS. (The D4 slot lint over `allPrintings` is satisfied: all three have empty `effects`/`targetSpecs`, so `slotsOf == targetSpecs keys == ∅`.)

- [x] **Step 5: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Add Blood Moon, Urborg, and Opalescence printings (Scryfall-verified)"
```

---

## Phase 2 + 3 — the existence dependency (Tasks 6–8)

### Task 6: Source-liveness — CR 613.8 existence (the go/no-go)

**Files:**
- Modify: `source/library/Pawl/Projection.hs` (`staticAbilitiesLive`, `setLandSubtypeEffects`, `affectsBase`; `gather` consults liveness)
- Test: `source/test-suite/Pawl/ProjectionSpec.hs`

**Interfaces:**
- Consumes: `gather`/`project`/`affects` (Task 4), the cards (Task 5).
- Produces: `staticAbilitiesLive :: ObjectId -> GameState -> Bool`; `gather` drops a permanent's static abilities when it is not `staticAbilitiesLive`. Blood Moon + Urborg then resolves order-independently.

- [ ] **Step 1: Write the failing tests (the Phase-2 existence test set)**

Add helpers and the dependency tests to `source/test-suite/Pawl/ProjectionSpec.hs` (new imports `qualified Pawl.Mana as Mana` for the mana observable in Task 7; here assert via `subtypesOf`). A fixture placing Blood Moon, Urborg, and a Forest in a controllable order:

```haskell
-- Blood Moon, Urborg, and a Forest on the battlefield. `urborgFirst` controls
-- the timestamp order (fresh timestamps ascend with placement), to prove the
-- outcome is order-INDEPENDENT (CR 613.8 dependency overrides CR 613.7).
bloodMoonUrborg :: Bool -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
bloodMoonUrborg urborgFirst =
  let base = Setup.emptyGame S.bothPlayers
      (forestId, g1) = S.addCreature Card.forestPrinting S.alice base
      place g = if urborgFirst
        then let (u, g') = S.addCreature Card.urborgPrinting S.alice g
                 (_, g'') = S.addCreature Card.bloodMoonPrinting S.alice g'
              in (u, g'')
        else let (_, g') = S.addCreature Card.bloodMoonPrinting S.alice g
                 (u, g'') = S.addCreature Card.urborgPrinting S.alice g'
              in (u, g'')
      (urborgId, gs) = place g1
   in (forestId, urborgId, gs)
```

```haskell
      HU.testCase "CR 305.7/613.8 Blood Moon strips Urborg: Urborg is only a Mountain (Blood Moon older)" $
        let (_, urborgId, gs) = bloodMoonUrborg False
         in HU.assertEqual "Urborg subtypes" (Set.singleton Subtype.Mountain) (Projection.subtypesOf urborgId gs),
      HU.testCase "CR 305.7/613.8 Blood Moon strips Urborg: Urborg is only a Mountain (Urborg older)" $
        let (_, urborgId, gs) = bloodMoonUrborg True
         in HU.assertEqual "Urborg subtypes, order-independent" (Set.singleton Subtype.Mountain) (Projection.subtypesOf urborgId gs),
      HU.testCase "CR 613.8 Urborg's stripped ability adds no Swamp to a Forest (Blood Moon older)" $
        let (forestId, _, gs) = bloodMoonUrborg False
         in HU.assertEqual "Forest stays a Forest" (Set.singleton Subtype.Forest) (Projection.subtypesOf forestId gs),
      HU.testCase "CR 613.8 Urborg's stripped ability adds no Swamp to a Forest (Urborg older)" $
        let (forestId, _, gs) = bloodMoonUrborg True
         in HU.assertEqual "Forest stays a Forest, order-independent" (Set.singleton Subtype.Forest) (Projection.subtypesOf forestId gs),
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cabal test --test-options='-p "613.8"'`
Expected: FAIL — without source-liveness, `gather` still includes Urborg's `AddLandSubtype Swamp`, so the Forest projects `{Forest, Swamp}` and Urborg projects `{Mountain, Swamp}` (or timestamp-dependent), not the correct `{Forest}` / `{Mountain}`.

- [ ] **Step 3: Implement source-liveness**

In `source/library/Pawl/Projection.hs`, add `affectsBase`, `setLandSubtypeEffects`, and `staticAbilitiesLive`, and gate `gather`'s static branch:

```haskell
-- affects evaluated against an object's BASE characteristics (used by
-- source-liveness, which must not recurse into the projection it feeds).
affectsBase :: ObjectId -> ObjectId -> Affected.Affected -> GameState -> Bool
affectsBase source oid a gs = affects source oid a (baseCharacteristics oid gs) gs

-- Every SetLandSubtype effect in the game, each with its source and affected set
-- (from stored effects and battlefield permanents' static abilities). This is a
-- legitimate case-on-Modification -- Projection is its sole home.
setLandSubtypeEffects :: GameState -> [(ObjectId, Affected.Affected)]
setLandSubtypeEffects gs =
  let isSet m = case m of
        Modification.SetLandSubtype _ -> True
        _ -> False
      fromStored eff =
        if isSet (ContinuousEffect.modification eff)
          then [(ContinuousEffect.source eff, ContinuousEffect.affected eff)]
          else []
      fromPerm permId = case Game.cardOf permId gs of
        Nothing -> []
        Just card ->
          map (\sa -> (permId, StaticAbility.affected sa)) $
            filter (\sa -> isSet (StaticAbility.modification sa)) (Card.Type.staticAbilities card)
   in concatMap fromStored (GameState.continuousEffects gs)
        ++ concatMap fromPerm (Set.toList (GameState.battlefield gs))

-- CR 305.7: a land whose subtype is SET to a basic type loses its rules-text
-- abilities. So an object's static abilities are live unless a live SetLandSubtype
-- applies to it. "Live" recurses on the stripper's own source; "applies to" reads
-- BASE characteristics (nonbasic is a printed supertype; card-type Land is
-- unchanged by any M3c effect), so nothing recurses into the projection and the
-- result is order-INDEPENDENT. A cycle trips the visited set (both treated as
-- live -- the CR 613.8b loop-escape analog; expiry in the spec).
staticAbilitiesLive :: ObjectId -> GameState -> Bool
staticAbilitiesLive = staticAbilitiesLiveVisited Set.empty

staticAbilitiesLiveVisited :: Set ObjectId -> ObjectId -> GameState -> Bool
staticAbilitiesLiveVisited visited oid gs =
  if Set.member oid visited
    then True
    else
      let visited' = Set.insert oid visited
          strips (src, aff) =
            staticAbilitiesLiveVisited visited' src gs
              && affectsBase src oid aff gs
       in not (any strips (setLandSubtypeEffects gs))
```

Gate the static branch of `gather` (replace `fromPermanent`):

```haskell
      fromPermanent permId = case Game.lookupObject permId gs of
        Nothing -> []
        Just permObj -> case Game.cardOf permId gs of
          Nothing -> []
          Just card ->
            if staticAbilitiesLive permId gs
              then map (fromStatic permId permObj) (Card.Type.staticAbilities card)
              else []
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — all four CR 613.8 tests green, both orders, and every prior test still green. **This is the go/no-go: a genuine YES on the existence dependency Argentum could not represent.**

- [ ] **Step 5: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Resolve CR 613.8 existence via source-liveness (Blood Moon strips Urborg)"
```

---

### Task 7: Mana reads the projected subtypes (the end-to-end observable)

**Files:**
- Modify: `source/library/Pawl/Mana.hs` (`manaTypesOf`)
- Test: `source/test-suite/Pawl/ManaSpec.hs`

**Interfaces:**
- Consumes: `Projection.subtypesOf` (Task 1), source-liveness (Task 6).
- Produces: `Mana.manaTypesOf` reads the *projected* subtypes, so a Blood Moon'd land taps `{R}`, an Urborg'd land taps `{B}`.

- [ ] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/ManaSpec.hs` (mirror its style; import `qualified Pawl.Card as Card`, `qualified Pawl.Mana as Mana`, `qualified Pawl.Setup as Setup`, `qualified Pawl.Support as S`, `qualified Pawl.Type.Color as Color`, `qualified Pawl.Type.ManaType as ManaType` as needed):

```haskell
      HU.testCase "CR 305.6/305.7 an Urborg'd Mountain taps for black too" $
        let base = Setup.emptyGame S.bothPlayers
            (mountainId, g1) = S.addCreature Card.mountainPrinting S.alice base
            (_, gs) = S.addCreature Card.urborgPrinting S.alice g1
            -- Urborg adds Swamp to all lands, so the Mountain taps for black too.
         in do
              HU.assertBool "black available" (ManaType.Colored Color.Black `elem` Mana.manaTypesOf mountainId gs)
              HU.assertBool "red still available" (ManaType.Colored Color.Red `elem` Mana.manaTypesOf mountainId gs),
      HU.testCase "CR 305.6/305.7 a Blood Moon'd Urborg taps for red only" $
        let base = Setup.emptyGame S.bothPlayers
            (urborgId, g1) = S.addCreature Card.urborgPrinting S.alice base
            (_, gs) = S.addCreature Card.bloodMoonPrinting S.alice g1
         in do
              HU.assertBool "red available" (ManaType.Colored Color.Red `elem` Mana.manaTypesOf urborgId gs)
              HU.assertBool "black not available (stripped)" (ManaType.Colored Color.Black `notElem` Mana.manaTypesOf urborgId gs),
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "taps for"'`
Expected: FAIL — `manaTypesOf` reads the *printed* subtypes, so Urborg (printed no subtypes) taps for nothing and the Blood Moon'd Urborg is unchanged.

- [ ] **Step 3: Read projected subtypes in `manaTypesOf`**

In `source/library/Pawl/Mana.hs`, add `import qualified Pawl.Projection as Projection`, and replace `manaTypesOf` (the `Source`/`Printing`/`TypeLine` imports may become unused — remove any that do to stay warning-clean):

```haskell
-- Every mana type an object could produce, derived from its PROJECTED subtypes
-- (CR 305.6, through the layer system: Blood Moon and Urborg change what a land
-- taps for). CR 305.7's "gains the appropriate mana ability" needs no explicit
-- grant -- the projected subtype IS the ability.
manaTypesOf :: ObjectId -> GameState -> [ManaType]
manaTypesOf oid gs = Maybe.mapMaybe subtypeMana (Set.toList (Projection.subtypesOf oid gs))
```

**Cycle check:** `Pawl.Mana` now imports `Pawl.Projection`; `Pawl.Projection` does not import `Pawl.Mana` (verified), so no cycle.

- [ ] **Step 4: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS. If the build warns about unused imports in `Mana.hs` (`Source`, `Printing`, `TypeLine`, `Card`), delete them.

- [ ] **Step 5: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Tap lands off their projected subtypes (CR 305.6/305.7)"
```

---

### Task 8: `Sba` / `Target` / `Combat` read projected creature-ness

**Files:**
- Modify: `source/library/Pawl/Sba.hs` (line ~63)
- Modify: `source/library/Pawl/Target.hs` (line ~25)
- Modify: `source/library/Pawl/Combat.hs` (`isCreatureObject`, line ~64)
- Test: `source/test-suite/Pawl/ProjectionSpec.hs` (Opalescence creature-ness end-to-end)

**Interfaces:**
- Consumes: `Projection.isCreatureOf` (Task 1), the cards (Task 5).
- Produces: `Sba`, `Target`, `Combat` treat an Opalescence'd enchantment as a creature.

- [ ] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/ProjectionSpec.hs` (import `qualified Pawl.Target as Target`, `qualified Pawl.Type.Recipient as Recipient` if needed). Opalescence + Humility: Humility becomes a creature and is a legal `CreatureTarget`; and, marked lethal, an SBA destroys it:

```haskell
      HU.testCase "CR 305.2 Opalescence makes Humility a creature: legal creature target and SBA-killable" $
        let base = Setup.emptyGame S.bothPlayers
            (humilityId, g1) = S.addCreature Card.humilityPrinting S.alice base
            -- Opalescence AFTER Humility, so Opalescence's 7b (mana value 4) wins
            -- the timestamp race: Humility is a 4/4 creature.
            (_, g2) = S.addCreature Card.opalescencePrinting S.alice g1
         in do
              HU.assertBool "Humility is a creature" (Projection.isCreatureOf humilityId g2)
              HU.assertEqual "base P/T = its mana value" (Just 4) (Projection.toughnessOf humilityId g2)
              let damaged = S.markDamage humilityId 4 g2
                  afterSba = Sba.checkStateBasedActions damaged
              HU.assertBool "lethal damage destroys the animated enchantment" (not (Set.member humilityId (GameState.battlefield afterSba))),
```

(If `Sba.checkStateBasedActions` has a different name, use the one `ProjectionSpec` already imports — the M3b Humility test uses `Sba.checkStateBasedActions`.)

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "Opalescence makes Humility"'`
Expected: FAIL — `Sba` reads the *printed* type via `Card.isCreature`, so Humility is not seen as a creature and the SBA never destroys it.

- [ ] **Step 3: Rewire the three consumers**

`source/library/Pawl/Sba.hs` — replace the printed read (line ~63). It already imports `Projection`:

```haskell
  let isCreature = Projection.isCreatureOf oid gs
```

`source/library/Pawl/Target.hs` — add `import qualified Pawl.Projection as Projection`, replace line ~25:

```haskell
  let isCreatureId oid = Projection.isCreatureOf oid gs
```

`source/library/Pawl/Combat.hs` — add `import qualified Pawl.Projection as Projection`, replace `isCreatureObject` (line ~64):

```haskell
isCreatureObject :: ObjectId -> GameState -> Bool
isCreatureObject oid gs = Projection.isCreatureOf oid gs
```

Remove any now-unused `Card`/`Card.isCreature` imports from `Target.hs`/`Combat.hs`/`Sba.hs` to stay warning-clean. **Cycle check:** `Projection` imports none of `Sba`/`Target`/`Combat`, so these edges are safe.

- [ ] **Step 4: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — Opalescence'd Humility is a targetable, SBA-killable creature; all combat/targeting/SBA tests for ordinary creatures still green (printed and projected agree when no type-changer is present).

- [ ] **Step 5: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Read creature-ness through the projection in Sba, Target, Combat"
```

---

## Phase 4 — the cross-layer correctness gate and tracked incompleteness (Tasks 9–10)

### Task 9: Humility + Opalescence, both 7b timestamp orders

**Files:**
- Test: `source/test-suite/Pawl/ProjectionSpec.hs`

**Interfaces:**
- Consumes: everything above. No production code — this is the famous cross-layer correctness gate (CR 613.7 within layer 7b), which the layer-by-layer fold already handles.

- [ ] **Step 1: Write the test**

Add to `source/test-suite/Pawl/ProjectionSpec.hs`:

```haskell
      HU.testCase "CR 613 Humility + Opalescence: a real creature is 1/1 with no abilities" $
        let base = Setup.emptyGame S.bothPlayers
            (pikerId, g1) = S.addCreature Card.pikerPrinting S.alice base
            (_, g2) = S.addCreature Card.humilityPrinting S.alice g1
            (_, gs) = S.addCreature Card.opalescencePrinting S.alice g2
         in do
              HU.assertEqual "power 1" (Just 1) (Projection.powerOf pikerId gs)
              HU.assertEqual "toughness 1" (Just 1) (Projection.toughnessOf pikerId gs)
              HU.assertBool "no abilities" (Set.null (Projection.keywordsOf pikerId gs)),
      HU.testCase "CR 613.7 Humility + Opalescence: Humility is 4/4 when Opalescence is newer" $
        let base = Setup.emptyGame S.bothPlayers
            (humilityId, g1) = S.addCreature Card.humilityPrinting S.alice base
            (_, gs) = S.addCreature Card.opalescencePrinting S.alice g1
         in HU.assertEqual "Opalescence's mana-value 7b wins" (Just 4) (Projection.powerOf humilityId gs),
      HU.testCase "CR 613.7 Humility + Opalescence: Humility is 1/1 when Humility is newer" $
        let base = Setup.emptyGame S.bothPlayers
            (_, g1) = S.addCreature Card.opalescencePrinting S.alice base
            (humilityId, gs) = S.addCreature Card.humilityPrinting S.alice g1
         in HU.assertEqual "Humility's 1/1 7b wins" (Just 1) (Projection.powerOf humilityId gs),
      HU.testCase "CR 305.2 Opalescence is not itself a creature (\"each other\")" $
        let base = Setup.emptyGame S.bothPlayers
            (opalId, g1) = S.addCreature Card.opalescencePrinting S.alice base
            (_, gs) = S.addCreature Card.humilityPrinting S.alice g1
         in HU.assertBool "Opalescence stays a non-creature enchantment" (not (Projection.isCreatureOf opalId gs)),
```

- [ ] **Step 2: Run tests to verify they pass**

Run: `cabal test --test-options='-p "Humility + Opalescence"'` then the full `cabal test`.
Expected: PASS — no production change needed; the fold already applies layer 4 (creature-add) before layer 6 (lose-all) before layer 7b (two P/T sets, timestamp-ordered). If any fails, the fold or timestamp order is wrong — **stop and report**, do not weaken the assertion.

- [ ] **Step 3: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Gate Humility + Opalescence in both 7b timestamp orders (CR 613.7)"
```

---

### Task 10: Track the deferred topological CR 613.8b resolver

**Files:**
- Modify: `source/library/Pawl/Projection.hs` (an expiry comment at the within-layer sort)
- Test: `source/test-suite/Pawl/ProjectionSpec.hs` (a documenting test asserting the current timestamp contract)
- git-bug entry

**Interfaces:**
- No production behavior change. This records the one CR 613.8 case M3c does not implement — the same-layer *applies-to* topological reorder — as a live, executable tracker rather than a doc footnote (risk register D2).

- [ ] **Step 1: Open the git-bug**

```bash
git-bug bug new -t "M3c: topological CR 613.8b (applies-to) dependency deferred" -m "Within-layer ordering is CR 613.7 timestamp only. The CR 613.8b topological reorder -- apply B tentatively, observe it changes which objects A applies to (both surviving), reorder -- is not implemented; no M3c card falsifies it (existence dependencies are handled by source-liveness). Post-M3c: find a real same-layer applies-to card pair, then implement the reorder and flip ProjectionSpec's documenting test to assert the dependency outcome."
```

Capture the printed bug id for the comment and test below.

- [ ] **Step 2: Add the expiry comment**

In `source/library/Pawl/Projection.hs`, at the `ordered = List.sortOn gTimestamp here` line inside `project`, add:

```haskell
        -- CR 613.7 timestamp order within a layer. EXPIRES: CR 613.8b dependency
        -- (a same-layer effect that changes which objects another applies to)
        -- would override this. Deferred -- no M3c card falsifies it; existence
        -- dependencies are handled by staticAbilitiesLive. git-bug <ID>.
        ordered = List.sortOn gTimestamp here
```

- [ ] **Step 3: Add the documenting test**

Add to `source/test-suite/Pawl/ProjectionSpec.hs` a hand-built same-layer applies-to case, asserting the **current timestamp behavior** with a comment naming the expiry (so a future implementer consciously flips it):

```haskell
      HU.testCase "CR 613.7 within layer 4, timestamp order (EXPIRES at CR 613.8b, git-bug <ID>)" $
        -- A Piker made a Land by B (layer 4, TheseObjects), and A = AddLandSubtype
        -- Swamp over AllLands (layer 4). With A OLDER than B, timestamp order applies
        -- A before B, so A does not yet see the Piker as a land and adds no Swamp.
        -- The CR 613.8b-correct answer is that A depends on B (B changes what A
        -- applies to), so B applies first and the Piker WOULD gain Swamp. When the
        -- topological resolver lands, flip this assertion to assert the Swamp.
        let (pikerId, gs0) = S.addPiker S.bob (S.mountainsInPlay 1)
            gsA = withDynamicEffect Affected.AllLands (Timestamp.MkTimestamp 10) (Modification.AddLandSubtype Subtype.Swamp) gs0
            gs = withEffect pikerId (Timestamp.MkTimestamp 20) (Modification.AddCardType CardType.Land) gsA
         in HU.assertBool "timestamp-only: no Swamp yet (known-incomplete, tracked)" (not (Set.member Subtype.Swamp (Projection.subtypesOf pikerId gs))),
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS (the test asserts the current, deliberately-incomplete contract).

- [ ] **Step 5: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Track the deferred CR 613.8b applies-to resolver with a documenting test"
```

---

## Final verification

- [ ] **Step 1: Clean build, all suites**

Run: `cabal clean && cabal build all --enable-tests --enable-benchmarks`
Expected: warning-free (a clean build surfaces warnings incremental builds hide).

- [ ] **Step 2: Full test suite (including the property suite over both matchups)**

Run: `cabal test`
Expected: PASS — every M2d/M3a/M3b property still holds; replay determinism covers the projected type line and source-liveness. (No card enters a random game — the three M3c cards are deterministic fixtures, per the spec's white/red-fixture posture — so `PropertySpec` needs no change.)

- [ ] **Step 3: Lint and format**

Run: `git add -A && hooky fix && git add -A && hooky run`
Expected: all hooks pass; apply any HLint suggestions or justify the exception.

- [ ] **Step 4: Progress check**

Run: `grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-18-m3c-dependency.md`
Expected: `0` — every step ticked.
