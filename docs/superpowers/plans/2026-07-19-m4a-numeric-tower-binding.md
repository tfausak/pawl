# M4a — Numeric Tower's X + General Binding Environment — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land the numeric tower's `X` (Blaze, `{X}{R}` "deal X damage to any target") on a unified cast-time binding environment that replaces the two parallel `Object` choice-maps.

**Architecture:** Two ordered phases. **Phase 1** is a behavior-preserving refactor: unify `Object.targets` and `Object.chosenSubtypes` into one `Object.bindings :: Map SlotName Binding` (a per-slot product record), with the existing M3a–M3g suite as the regression net. **Phase 2** builds `X` on that environment: a new `Quantity.X` and `ManaSymbol.Variable`, a `ChooseX` cast-time prompt whose value substitutes into the cost and is stamped into `bindings`, re-read at resolution by `Quantity.evaluate`.

**Tech Stack:** Haskell 2010 (GHC 9.14.1, Nix flake dev shell), `tasty` (`tasty-hunit` + `tasty-quickcheck`), hand-rolled JSON codec (no aeson). Cards are `data/cards/*.json` loaded by the test suite.

## Global Constraints

Copied from the spec (`docs/superpowers/specs/2026-07-19-m4a-numeric-tower-binding-design.md`) and `CLAUDE.md`; every task's requirements implicitly include these:

- **Haskell 2010, no language extensions** beyond those already enabled per-module (`GADTs`, `RankNTypes`, `NamedFieldPuns`). No `LambdaCase`/`OverloadedStrings`.
- **Warning-clean** under `-Weverything` minus the cabal allow-list; the `+pedantic` flag makes any warning a build failure. Incremental builds hide warnings — `cabal clean` for a definitive check.
- **Build all suites:** `cabal build all --enable-tests --enable-benchmarks`.
- **No partial functions** — no `head`/`undefined`/`error`/non-exhaustive matches. `Maybe`/`Either`.
- **`newtype`/`data` + smart constructors, non-punning** — `MkBinding`, never `Binding` as the constructor. New sum constructors take no `Mk` prefix (`X`, `Variable`, `Sorcery`).
- **One type per module** under `Pawl.Type.<Name>` (type + instances only); logic in other `Pawl.*` modules. Qualified imports aliased to the last component; operators unqualified; prefer functions to operators.
- **Arbitrary-precision numbers** — `Natural` for X, `Integer` for `Quantity.evaluate`'s result. Never fixed-width.
- **The two invariants outrank this plan:** the engine never cases on a card's *identity* (only classifications), and never makes a player's choice (elide only indistinguishable options, with a named expiry). `Pawl.Resolve` is the sole `case effect of` / `case quantity of` home.
- **TDD is not optional** — write each failing test, run it, watch it fail, then implement. For the Phase 1 refactor the regression net is the *existing* suite: it must be green before and after.
- **Every rules claim checked against `docs/rules.txt`** with the CR number in the code comment. Card text is Scryfall-verified (Blaze: `{X}{R}`, Sorcery, "Blaze deals X damage to any target." — fetched 2026-07-19).
- **After each change:** `cabal build all` warning-free; `git add -A` then `hooky fix`, `git add -A` again, then `hooky run` passes; HLint applied. Each task is one small complete commit on `main`.

## File Structure

**New files:**
- `source/library/Pawl/Type/Binding.hs` — the `Binding` product record (target/subtypes/amount per slot).
- `source/library/Pawl/Binding.hs` — logic over `Map SlotName Binding`: projections (`targetsOf`, `subtypesOf`, `amountOf`), the reserved X slot (`variableX`), and the write-site constructor (`fromChoices`).
- `data/cards/blaze.json` — Blaze as card data.

**Modified — library:**
- `Pawl/Type/Object.hs` — drop `targets`/`chosenSubtypes`, add `bindings`.
- `Pawl/Type/Quantity.hs` — add `X`. `Pawl/Type/ManaSymbol.hs` — add `Variable`. `Pawl/Type/CardType.hs` — add `Sorcery`.
- `Pawl/Type/Prompt.hs` — add `ChooseX`. `Pawl/Type/Response.hs` — add `ChoseX`.
- `Pawl/Quantity.hs` — `evaluate` X arm; `manaValueOf` Variable=0.
- `Pawl/Mana.hs` — `substituteX`; Variable arms in the symbol helpers.
- `Pawl/Cast.hs` — `ChooseX` in `castSpell`; `substituteX 0` castability floor.
- `Pawl/Resolve.hs`, `Pawl/Activate.hs`, `Pawl/Event.hs`, `Pawl/Setup.hs`, `Pawl/Engine.hs` — migrate readers/constructors to `bindings`.
- `Pawl/Replay.hs` — `ChooseX`/`ChoseX` record + replay arms.
- `Pawl/Codec.hs` — `Variable`, `X`, `Sorcery` Codec arms.

**Modified — test suite:**
- `Pawl/Support.hs` — `bindings` in fixture constructors; `ChooseX` arm in every answerer.
- `Pawl/GameSpec.hs` — rewrite the CR 400.7 reset tests to assert on `bindings`.
- `Pawl/CastSpec.hs`, `Pawl/ResolveSpec.hs` — migrate constructors/readers.
- `Pawl/CardSpec.hs` — generalized D4 lint; printing count 30→31.
- `Pawl/BindingSpec.hs` (new) — unit tests for the `Pawl.Binding` logic.
- `Pawl/CastSpec.hs` / a Blaze gameplay group — X scenarios.
- `Pawl/Cards.hs` — register `blazePrinting`; add to `allPrintings`, `redDeck`.

---

## Phase 1 — The binding environment (behavior-preserving)

### Task 1: The `Binding` type and its logic module

**Files:**
- Create: `source/library/Pawl/Type/Binding.hs`
- Create: `source/library/Pawl/Binding.hs`
- Test: `source/test-suite/Pawl/BindingSpec.hs` (new)
- Modify: `source/test-suite/pawl.cabal` `other-modules` (add `Pawl.BindingSpec`); `source/test-suite/Pawl/Main.hs` (wire `BindingSpec.tests`)

**Interfaces:**
- Produces:
  - `Binding.MkBinding { target :: Maybe Recipient, subtypes :: Maybe (Subtype, Subtype), amount :: Maybe Natural }`
  - `Binding.empty :: Binding`
  - `Binding.variableX :: SlotName`
  - `Binding.targetsOf :: Map SlotName Binding -> Map SlotName Recipient`
  - `Binding.subtypesOf :: Map SlotName Binding -> Map SlotName (Subtype, Subtype)`
  - `Binding.amountOf :: SlotName -> Map SlotName Binding -> Maybe Natural`
  - `Binding.fromChoices :: Map SlotName Recipient -> Map SlotName (Subtype, Subtype) -> Maybe Natural -> Map SlotName Binding`

- [x] **Step 1: Write the failing test**

Create `source/test-suite/Pawl/BindingSpec.hs`:

```haskell
-- Covers: Pawl.Type.Binding, Pawl.Binding
module Pawl.BindingSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Binding as Binding
import qualified Pawl.Type.Binding as Binding.Type
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Subtype as Subtype
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU
import qualified Pawl.Support as S

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Pawl.Binding"
    [ HU.testCase "fromChoices merges a shared slot's target and subtypes" $
        let slot = SlotName.MkSlotName (Text.pack "target")
            r = Recipient.ToPlayer S.alice
            pair = (Subtype.Mountain, Subtype.Island)
            m = Binding.fromChoices (Map.singleton slot r) (Map.singleton slot pair) Nothing
         in do
              HU.assertEqual "target projected" (Map.singleton slot r) (Binding.targetsOf m)
              HU.assertEqual "subtypes projected" (Map.singleton slot pair) (Binding.subtypesOf m),
      HU.testCase "fromChoices stores X under the reserved slot" $
        let m = Binding.fromChoices Map.empty Map.empty (Just 3)
         in HU.assertEqual "amount readable" (Just 3) (Binding.amountOf Binding.variableX m),
      HU.testCase "amountOf is Nothing for an absent slot" $
        HU.assertEqual "no amount" Nothing (Binding.amountOf Binding.variableX Map.empty)
    ]
```

- [x] **Step 2: Run test to verify it fails**

Run: `cabal build all --enable-tests 2>&1 | tail -20`
Expected: FAIL — `Could not find module 'Pawl.Type.Binding'` / `Pawl.Binding`.

- [x] **Step 3: Write the `Binding` type**

Create `source/library/Pawl/Type/Binding.hs`:

```haskell
{-# LANGUAGE NamedFieldPuns #-}

module Pawl.Type.Binding where

import Numeric.Natural (Natural)
import Pawl.Type.Recipient (Recipient)
import Pawl.Type.Subtype (Subtype)

-- CR 601.2: the cast-time choices bound to one named slot of a spell or ability
-- on the stack. A record, not a sum, because one slot may carry several kinds of
-- choice at once -- Magical Hack's slot is both TARGETED (a Recipient) and
-- WORD-SWAPPED (a Subtype pair). A field per binding kind; a kind absent for this
-- slot is Nothing. Grows a field per future binding (a mode, a for-each count).
data Binding = MkBinding
  { -- CR 601.2c: the chosen target for this slot; re-validated at CR 608.2b.
    target :: Maybe Recipient,
    -- CR 612: the (from, to) basic land types chosen for a text-changing slot.
    subtypes :: Maybe (Subtype, Subtype),
    -- CR 601.2b: the value chosen for a variable in the cost (X). Read by
    -- Quantity.evaluate. Nothing for a slot with no amount.
    amount :: Maybe Natural
  }
  deriving (Eq, Ord, Show)

-- The empty binding: no choice of any kind. The unit for merging.
empty :: Binding
empty = MkBinding {target = Nothing, subtypes = Nothing, amount = Nothing}
```

- [x] **Step 4: Write the `Pawl.Binding` logic module**

Create `source/library/Pawl/Binding.hs`:

```haskell
module Pawl.Binding where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import Pawl.Type.Binding (Binding)
import qualified Pawl.Type.Binding as Binding
import Pawl.Type.Recipient (Recipient)
import Pawl.Type.SlotName (SlotName)
import qualified Pawl.Type.SlotName as SlotName
import Pawl.Type.Subtype (Subtype)

-- CR 601.2b: the reserved slot under which a spell's single chosen X is stored.
-- No card's targetSpecs may name it (the D4 lint enforces this): X is not a
-- target, so it needs a key the target namespace cannot collide with.
variableX :: SlotName
variableX = SlotName.MkSlotName (Text.pack "X")

-- Project the chosen targets (CR 601.2c) out of a binding environment, dropping
-- slots with no target. Restores the pre-M4a Object.targets view for readers.
targetsOf :: Map SlotName Binding -> Map SlotName Recipient
targetsOf = Map.mapMaybe Binding.target

-- Project the chosen (from, to) land-type pairs (CR 612), dropping slots without.
subtypesOf :: Map SlotName Binding -> Map SlotName (Subtype, Subtype)
subtypesOf = Map.mapMaybe Binding.subtypes

-- The amount (X) bound at a slot, if any.
amountOf :: SlotName -> Map SlotName Binding -> Maybe Natural
amountOf slot m = Binding.amount =<< Map.lookup slot m

-- Build the binding environment stamped on a stack object at cast: the chosen
-- targets, the chosen land-type pairs, and (Just x) the chosen X under variableX.
-- A slot present in several inputs keeps every choice (Magical Hack's slot).
fromChoices ::
  Map SlotName Recipient ->
  Map SlotName (Subtype, Subtype) ->
  Maybe Natural ->
  Map SlotName Binding
fromChoices targets subtypes mAmount =
  let fromTargets = Map.map (\r -> Binding.empty {Binding.target = Just r}) targets
      fromSubtypes = Map.map (\p -> Binding.empty {Binding.subtypes = Just p}) subtypes
      merged = Map.unionWith mergeBinding fromTargets fromSubtypes
   in case mAmount of
        Nothing -> merged
        Just n ->
          Map.insertWith mergeBinding variableX (Binding.empty {Binding.amount = Just n}) merged

-- Combine two bindings for the same slot, preferring the left's present choice in
-- each field. Inputs are disjoint per field by construction, so this is a total,
-- order-independent merge.
mergeBinding :: Binding -> Binding -> Binding
mergeBinding a b =
  Binding.MkBinding
    { Binding.target = firstJust (Binding.target a) (Binding.target b),
      Binding.subtypes = firstJust (Binding.subtypes a) (Binding.subtypes b),
      Binding.amount = firstJust (Binding.amount a) (Binding.amount b)
    }

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust a b = maybe b Just a
```

(Note the `{-# LANGUAGE NamedFieldPuns #-}` pragma is on `Pawl.Type.Binding` for the record; `Pawl.Binding` uses qualified field updates and needs no pragma.)

- [x] **Step 5: Wire `BindingSpec` into the suite**

In `source/test-suite/pawl.cabal`, add `Pawl.BindingSpec` to `other-modules` (keep alphabetical). In `source/test-suite/Pawl/Main.hs`, import `qualified Pawl.BindingSpec as BindingSpec` and add `BindingSpec.tests` to the aggregated `testTree`.

- [x] **Step 6: Run test to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -20`
Expected: PASS — `Pawl.Binding` group green; whole suite still green.

- [x] **Step 7: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4a: the Binding record and its logic module (no wiring yet)"
```

---

### Task 2: Migrate `Object` to the binding environment

**Files:**
- Modify: `source/library/Pawl/Type/Object.hs` (drop `targets`, `chosenSubtypes`; add `bindings`)
- Modify: `source/library/Pawl/Cast.hs:170`, `source/library/Pawl/Activate.hs:88-89,109`, `source/library/Pawl/Setup.hs:100-101`, `source/library/Pawl/Engine.hs:213-214`, `source/library/Pawl/Event.hs:46`, `source/library/Pawl/Resolve.hs:147,156,170,178`
- Modify: `source/test-suite/Pawl/Support.hs` (constructors at 257,284,314,341,369,527), `source/test-suite/Pawl/CastSpec.hs:264,360-361`, `source/test-suite/Pawl/GameSpec.hs:99-100,113-133,353-354`, `source/test-suite/Pawl/ResolveSpec.hs:233`

**Interfaces:**
- Consumes: `Binding.fromChoices`, `Binding.targetsOf`, `Binding.subtypesOf`, `Binding.empty` (Task 1).
- Produces: `Object.bindings :: Map SlotName Binding` (replacing `Object.targets`, `Object.chosenSubtypes`).

- [x] **Step 1: Rewrite the CR 400.7 reset tests first (they are the behavior spec)**

In `source/test-suite/Pawl/GameSpec.hs`, replace the two reset tests (`~113-133`) so they assert on `bindings`:

```haskell
        HU.testCase "CR 400.7 changeZone resets bindings" $
          let slot = SlotName.MkSlotName (Text.pack "target")
              seeded =
                Map.adjust
                  (\o -> o {Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToPlayer S.alice)) Map.empty Nothing})
                  someOid
                  (GameState.objects gs0)
              -- ... move the object between zones exactly as the old test did ...
           in HU.assertEqual "reset to empty" (Just Map.empty) (fmap Object.bindings movedObj),
```

Match the exact fixture plumbing of the existing test (the `someOid`, the move). Add `import qualified Pawl.Binding as Binding` and `import qualified Pawl.Type.Binding` as needed.

- [x] **Step 2: Run to verify the reset test fails to compile**

Run: `cabal build all --enable-tests 2>&1 | tail -20`
Expected: FAIL — `Object.bindings` not in scope / `targets` still the field.

- [x] **Step 3: Change the `Object` type**

In `source/library/Pawl/Type/Object.hs`, delete the `targets` and `chosenSubtypes` fields and their comments; add:

```haskell
    -- CR 601.2: the choices bound while casting, by slot name. Empty for
    -- everything but a spell or ability on the stack. Per-incarnation state:
    -- reset by changeZone, so CR 400.7 forgets them when the object moves.
    -- Replaces the M3a `targets` and M3d `chosenSubtypes` fields, unified as the
    -- risk-register's D4 named binding slots when X arrived (the second customer).
    bindings :: Map SlotName Binding,
```

Add `import Pawl.Type.Binding (Binding)`; drop the now-unused `Recipient`/`Subtype` imports if the compiler flags them (they may still be used elsewhere — let `-Weverything` guide).

- [x] **Step 4: Migrate the library constructors and readers**

- `Setup.hs:100-101`, `Engine.hs:213-214`, `Activate.hs:88-89`: replace the two `Map.empty` field lines with `Object.bindings = Map.empty,`.
- `Event.hs:46`: replace `Object.targets = Map.empty, Object.chosenSubtypes = Map.empty` with `Object.bindings = Map.empty`.
- `Cast.hs:170`: replace `(\o -> o {Object.targets = chosen, Object.chosenSubtypes = bound})` with `(\o -> o {Object.bindings = Binding.fromChoices chosen bound Nothing})`. Add `import qualified Pawl.Binding as Binding`.
- `Activate.hs:109`: replace `(\o -> o {Object.targets = chosen})` with `(\o -> o {Object.bindings = Binding.fromChoices chosen Map.empty Nothing})`. Add the `Binding` import.
- `Resolve.hs:147,170`: `chosen = Object.targets obj` becomes `chosen = Binding.targetsOf (Object.bindings obj)`.
- `Resolve.hs:156,178`: `Object.chosenSubtypes obj` becomes `Binding.subtypesOf (Object.bindings obj)`. Add `import qualified Pawl.Binding as Binding` to `Resolve.hs`. (`applyEffect`'s signature is unchanged — it still takes the projected `Map SlotName (Subtype, Subtype)` and `Map SlotName Recipient`.)

- [x] **Step 5: Migrate the test constructors and readers**

- `Support.hs` (six sites), `CastSpec.hs:360-361`, `GameSpec.hs:99-100,353-354`: replace each `Object.targets = Map.empty, Object.chosenSubtypes = Map.empty` pair with `Object.bindings = Map.empty`.
- `CastSpec.hs:264`: `Object.targets obj` becomes `Binding.targetsOf (Object.bindings obj)`.
- `ResolveSpec.hs:233`: `Object.targets = Map.singleton slot (Recipient.ToObject targetLand)` becomes `Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToObject targetLand)) Map.empty Nothing`.
- Add `import qualified Pawl.Binding as Binding` to each touched test module.

- [x] **Step 6: Build and run the full suite**

Run: `cabal clean && cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -25`
Expected: PASS — the entire M3a–M3g suite green, including the rewritten reset test. `cabal clean` first so no unchanged-module warnings are hidden.

- [x] **Step 7: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4a: migrate Object.targets/chosenSubtypes to one bindings environment"
```

---

## Phase 2 — X

### Task 3: `ManaSymbol.Variable`

**Files:**
- Modify: `source/library/Pawl/Type/ManaSymbol.hs`, `source/library/Pawl/Quantity.hs:26-29`, `source/library/Pawl/Mana.hs:130-131,135-136`, `source/library/Pawl/Codec.hs:336-347`
- Test: `source/test-suite/Pawl/CodecSpec.hs` (or wherever ManaSymbol round-trip lives — the honesty round-trip already iterates `allPrintings`; add a direct unit)

**Interfaces:**
- Produces: `ManaSymbol.Variable` (the `{X}` symbol); Codec arms; `Variable`-as-0 in the mana-value / generic-amount helpers.

- [x] **Step 1: Write the failing test**

In the codec spec, add a round-trip unit for the new symbol:

```haskell
      HU.testCase "ManaSymbol.Variable round-trips" $
        HU.assertEqual
          "round-trip"
          (Right ManaSymbol.Variable)
          (Codec.jsonToManaSymbol (Codec.manaSymbolToJson ManaSymbol.Variable)),
```

- [x] **Step 2: Run to verify it fails**

Run: `cabal build all --enable-tests 2>&1 | tail -20`
Expected: FAIL — `Variable` not a constructor of `ManaSymbol`.

- [x] **Step 3: Add the constructor**

In `source/library/Pawl/Type/ManaSymbol.hs`:

```haskell
data ManaSymbol
  = Generic Natural
  | OfType ManaType
  | -- CR 107.3 / 601.2b: the {X} symbol in a cost. Contributes the chosen value
    -- of X once chosen (0 before, for the CR 601.2b castability floor and for a
    -- mana value off the stack, CR 202.3b).
    Variable
  deriving (Eq, Ord, Show)
```

- [x] **Step 4: Handle `Variable` in every `ManaSymbol` case (the compiler lists them)**

- `Quantity.hs:26-29` `symbolValue`: add `ManaSymbol.Variable -> 0` (CR 202.3b — off the stack a variable's contribution is 0).
- `Mana.hs:130-131` (typed extractor): add `ManaSymbol.Variable -> Nothing`.
- `Mana.hs:135-136` (generic amount): add `ManaSymbol.Variable -> 0` (unreachable in payment — `substituteX` removes it first, Task 4 — but the match must be total).
- `Codec.hs:338-339` `manaSymbolToJson`: add `ManaSymbol.Variable -> nullary (Text.pack "Variable")`.
- `Codec.hs:345-346` `jsonToManaSymbol`: add `("Variable", _) -> Right ManaSymbol.Variable`.

- [x] **Step 5: Run to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -15`
Expected: PASS — round-trip green; suite green.

- [x] **Step 6: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4a: ManaSymbol.Variable ({X}) with Codec and zero contribution off the stack"
```

---

### Task 4: `Mana.substituteX` and the castability floor

**Files:**
- Modify: `source/library/Pawl/Mana.hs` (add `substituteX`), `source/library/Pawl/Cast.hs:77,100` (floor)
- Test: `source/test-suite/Pawl/ManaSpec.hs`

**Interfaces:**
- Produces: `Mana.substituteX :: Natural -> ManaCost -> ManaCost`.
- Consumes: `ManaSymbol.Variable` (Task 3).

- [x] **Step 1: Write the failing test**

In `source/test-suite/Pawl/ManaSpec.hs`:

```haskell
      HU.testCase "substituteX replaces each Variable with Generic X, keeping order" $
        let red = ManaSymbol.OfType (ManaType.Colored Color.Red)
            cost = ManaCost.MkManaCost [ManaSymbol.Variable, red]
         in HU.assertEqual
              "X=3 -> {3}{R}"
              (ManaCost.MkManaCost [ManaSymbol.Generic 3, red])
              (Mana.substituteX 3 cost),
      HU.testCase "substituteX 0 leaves a Variable-free cost payable" $
        let red = ManaSymbol.OfType (ManaType.Colored Color.Red)
         in HU.assertEqual
              "floor is {0}{R}"
              (ManaCost.MkManaCost [ManaSymbol.Generic 0, red])
              (Mana.substituteX 0 (ManaCost.MkManaCost [ManaSymbol.Variable, red])),
```

- [x] **Step 2: Run to verify it fails**

Run: `cabal build all --enable-tests 2>&1 | tail -20`
Expected: FAIL — `substituteX` not in scope.

- [x] **Step 3: Implement `substituteX`**

In `source/library/Pawl/Mana.hs`:

```haskell
-- CR 601.2f: the total cost with X resolved -- each Variable symbol becomes
-- Generic n, every other symbol unchanged, order preserved (ManaCost is a list,
-- never fixed arity). Applied before any payment, so a Variable never reaches
-- spend/canPay.
substituteX :: Natural -> ManaCost.ManaCost -> ManaCost.ManaCost
substituteX x (ManaCost.MkManaCost symbols) =
  ManaCost.MkManaCost (map sub symbols)
  where
    sub symbol = case symbol of
      ManaSymbol.Variable -> ManaSymbol.Generic x
      other -> other
```

(Per the style guide `let` is preferred over `where`, but a single trivial local helper as `where` matches surrounding `Mana.hs`; follow the file's local convention.)

- [x] **Step 4: Apply the castability floor**

In `source/library/Pawl/Cast.hs`, the affordability checks must treat a `{X}` cost as affordable when payable at X=0 (a caster may always choose X=0):
- Line ~77 in `castable`: `&& Mana.canPay pid (Mana.substituteX 0 cost) gs`.
- Line ~100 in `castableWhileSearching`: `Just cost -> Mana.canPay pid (Mana.substituteX 0 cost) gs`.

(`substituteX 0` is the identity on any Variable-free cost, so every existing card is unaffected.)

- [x] **Step 5: Run to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -15`
Expected: PASS.

- [x] **Step 6: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4a: Mana.substituteX and the X=0 castability floor"
```

---

### Task 5: `Quantity.X` and its evaluation

**Files:**
- Modify: `source/library/Pawl/Type/Quantity.hs`, `source/library/Pawl/Quantity.hs:13-16`, `source/library/Pawl/Codec.hs:355-366`
- Test: `source/test-suite/Pawl/QuantitySpec.hs` (or wherever `evaluate` is tested)

**Interfaces:**
- Produces: `Quantity.X`; `Quantity.evaluate` resolves `X` from the source object's `bindings`.
- Consumes: `Binding.amountOf`, `Binding.variableX` (Task 1); `Game.lookupObject`.

- [x] **Step 1: Write the failing test**

In `source/test-suite/Pawl/QuantitySpec.hs`, seed an object carrying an amount and assert `evaluate` reads it:

```haskell
      HU.testCase "evaluate reads X from the object's binding environment" $
        let oid = -- an ObjectId present in gs
            gs = -- a GameState whose object `oid` has
                 -- Object.bindings = Binding.fromChoices Map.empty Map.empty (Just 5)
         in HU.assertEqual "X = 5" (Just 5) (Quantity.evaluate gs oid Quantity.Type.X),
      HU.testCase "evaluate X is Nothing when no amount was bound" $
         HU.assertEqual "unbound X" Nothing (Quantity.evaluate gsNoBinding oid Quantity.Type.X),
```

Build `gs`/`oid` with the existing Support fixture that installs a stack object (reuse the ResolveSpec pattern), setting `Object.bindings = Binding.fromChoices Map.empty Map.empty (Just 5)`.

- [x] **Step 2: Run to verify it fails**

Run: `cabal build all --enable-tests 2>&1 | tail -20`
Expected: FAIL — `X` not a constructor of `Quantity`.

- [x] **Step 3: Add the constructor and the evaluation arm**

In `source/library/Pawl/Type/Quantity.hs`:

```haskell
data Quantity
  = Literal Integer
  | ManaValue
  | -- CR 601.2b: X -- a value the caster chose while casting, read from the
    -- object's binding environment (Pawl.Binding.variableX). One-shot only: a
    -- continuous effect must FREEZE this to a Literal when stored (Projection.hs
    -- note), which no M4a card exercises.
    X
  deriving (Eq, Ord, Show)
```

In `source/library/Pawl/Quantity.hs`, add to `evaluate`:

```haskell
  Quantity.X -> case Game.lookupObject oid gs of
    Nothing -> Nothing
    Just obj -> fmap toInteger (Binding.amountOf Binding.variableX (Object.bindings obj))
```

Add `import qualified Pawl.Binding as Binding` and `import qualified Pawl.Type.Object as Object`.

- [x] **Step 4: Add the Codec arms**

In `source/library/Pawl/Codec.hs`:
- `quantityToJson` (~357): add `Quantity.X -> nullary (Text.pack "X")`.
- `jsonToQuantity` (~364): add `("X", _) -> Right Quantity.X`.

- [x] **Step 5: Run to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -15`
Expected: PASS.

- [x] **Step 6: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4a: Quantity.X, evaluated from the object binding environment, with Codec"
```

---

### Task 6: The `ChooseX` prompt, response, and replay wiring

**Files:**
- Modify: `source/library/Pawl/Type/Prompt.hs`, `source/library/Pawl/Type/Response.hs`, `source/library/Pawl/Replay.hs:30-33,63-72`
- Modify: `source/test-suite/Pawl/Support.hs` (every answerer — the compiler lists them)
- Test: `source/test-suite/Pawl/ReplaySpec.hs` (or wherever the record/replay round-trip lives)

**Interfaces:**
- Produces: `Prompt.ChooseX :: Decider -> PlayerId -> ObjectId -> Prompt Natural`; `Response.ChoseX Natural`; the `Replay` record/replay arms.

- [x] **Step 1: Write the failing test**

Add a replay round-trip unit mirroring the existing `Searched`/`ChoseBasicLandTypes` coverage:

```haskell
      HU.testCase "ChooseX records and replays a Natural" $
        let p = Prompt.ChooseX (Decider.MkDecider S.alice) S.alice someOid
         in HU.assertEqual "round-trip" (Just 4) (Replay.replayAnswer p (Response.ChoseX 4)),
```

(Match the actual `Replay` helper names — `Replay.hs:30` records via a `toResponse`-style function and `63` replays via a `fromResponse`-style function; use whichever the file exposes.)

- [x] **Step 2: Run to verify it fails**

Run: `cabal build all --enable-tests 2>&1 | tail -20`
Expected: FAIL — `ChooseX`/`ChoseX` not in scope.

- [x] **Step 3: Add the prompt and response**

In `source/library/Pawl/Type/Prompt.hs`:

```haskell
  -- CR 601.2b: choose the value of X while casting (the ObjectId is the spell).
  -- Any Natural; payment (reject-not-repair) rejects an unaffordable choice, so
  -- the engine computes no maximum. Prompted before targets (CR 601.2b precedes
  -- 601.2c), and only when the cost contains a Variable symbol -- a spell with no
  -- {X} is not asked (where the rules leave nothing to choose, don't prompt).
  ChooseX :: Decider -> PlayerId -> ObjectId -> Prompt Natural
```

In `source/library/Pawl/Type/Response.hs`, add to the sum:

```haskell
  | -- CR 601.2b: the value of X a caster chose, serialized so a DecisionLog
    -- replays a variable-cost spell deterministically.
    ChoseX Natural
```

Add `import Numeric.Natural (Natural)` to `Response.hs` if not already present (it is).

- [x] **Step 4: Wire `Replay` (record and replay)**

In `source/library/Pawl/Replay.hs`:
- Record side (~30-33): `Prompt.ChooseX {} -> Response.ChoseX answer`.
- Replay side (~63-72): `Response.ChoseX n -> Just n`.

- [x] **Step 5: Add a `ChooseX` arm to every interpreter the compiler flags**

The `Prompt` GADT is matched exhaustively in each Support.hs answerer. Add a `Prompt.ChooseX` arm to each:
- Deterministic/identity answerers (`Support.hs` ~93,103,133,152): `Prompt.ChooseX {} -> 0` (X=0 is always payable and observationally neutral where these answerers cast nothing that has {X}).
- The `StdGen`-driven random answerer (`Support.hs` ~207): `Prompt.ChooseX {} -> pure smallX` where `smallX` is a bounded draw (e.g. `0..3`) from the generator — Blaze under random play should sometimes choose nonzero X to exercise `substituteX`. Match the answerer's existing `StdGen` threading style.

- [x] **Step 6: Run to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -15`
Expected: PASS.

- [x] **Step 7: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4a: ChooseX prompt, ChoseX response, and replay wiring"
```

---

### Task 7: Prompt and bind X in `castSpell`

**Files:**
- Modify: `source/library/Pawl/Cast.hs:133-174` (`castSpell`)
- Test: `source/test-suite/Pawl/CastSpec.hs`

**Interfaces:**
- Consumes: `ChooseX` (Task 6), `substituteX` (Task 4), `Binding.fromChoices` (Task 1), `ManaSymbol.Variable`.
- Produces: a cast that, for a `{X}`-cost spell, prompts X first (CR 601.2b), pays the substituted cost, and stamps the amount into the stack object's `bindings`.

- [x] **Step 1: Write the failing test**

In `source/test-suite/Pawl/CastSpec.hs`, cast a Variable-cost spell (use Blaze if Task 8 lands first, or a synthetic `{X}{R}` fixture) with an answerer returning X=3; assert the stack object carries the amount and the substituted cost was paid:

```haskell
      HU.testCase "casting a {X}{R} spell at X=3 stamps amount 3 and pays {3}{R}" $
        let -- caster has >= 4 red-producing mana; answerer returns ChooseX -> 3
            after = castThrough answerX3 gs0 blazeOid
            top = -- head of GameState.stack after
         in do
              HU.assertEqual "amount bound" (Just 3) (Binding.amountOf Binding.variableX (Object.bindings top))
              HU.assertEqual "four mana spent" 4 (manaSpent gs0 after),
```

- [x] **Step 2: Run to verify it fails**

Run: `cabal build all --enable-tests 2>&1 | tail -20`
Expected: FAIL — `castSpell` does not prompt `ChooseX`; amount is `Nothing`.

- [x] **Step 3: Add the X step to `castSpell`**

In `source/library/Pawl/Cast.hs`, before the target prompt (CR 601.2b precedes 601.2c), choose X when the cost carries a `Variable`, thread it into both the paid cost and the stamped bindings:

```haskell
        let decider = Decide.deciderFor pid gs
            hasVariable = case cost of
              ManaCost.MkManaCost syms -> elem ManaSymbol.Variable syms
        mAmount <-
          if hasVariable
            then fmap Just (Trans.lift (Program.prompt (Prompt.ChooseX decider pid oid)))
            else pure Nothing
        let paidCost = maybe cost (\x -> Mana.substituteX x cost) mAmount
        -- ... existing target prompt (chosen) and text-slot prompt (bound) ...
        case Mana.payCost pid paidCost gs of
          Nothing -> pure ()
          Just paid -> do
            let moved = Event.changeZone oid Zone.Stack paid
            -- stamp all three choices in one environment:
            -- Object.bindings = Binding.fromChoices chosen bound mAmount
```

Replace the existing single-map stamp (from Task 2's `Binding.fromChoices chosen bound Nothing`) with `Binding.fromChoices chosen bound mAmount`. Add `import qualified Pawl.Type.ManaSymbol as ManaSymbol` and `import qualified Pawl.Type.ManaCost as ManaCost` if not already present.

- [x] **Step 4: Run to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -15`
Expected: PASS.

- [x] **Step 5: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4a: castSpell prompts and binds X (CR 601.2b), paying the substituted cost"
```

---

### Task 8: Blaze as card data (with `CardType.Sorcery`)

**Files:**
- Modify: `source/library/Pawl/Type/CardType.hs`, `source/library/Pawl/Codec.hs:111-123`
- Create: `data/cards/blaze.json`
- Modify: `source/test-suite/Pawl/Cards.hs` (register `blazePrinting`; `allPrintings`; `redDeck`)
- Modify: `source/test-suite/Pawl/CardSpec.hs:168` (printing count 30 → 31)

**Interfaces:**
- Produces: `CardType.Sorcery`; `Cards.blazePrinting :: Cards -> Printing`; Blaze in `allPrintings` and `redDeck`.

- [x] **Step 1: Write the failing test**

In `source/test-suite/Pawl/CardSpec.hs` (or `CardsSpec`), assert Blaze loads with the right shape:

```haskell
      HU.testCase "Blaze is a {X}{R} Sorcery dealing X to any target" $
        let card = Printing.card (Cards.blazePrinting cards)
         in do
              HU.assertEqual "name" (Text.pack "Blaze") (Card.Type.name card)
              HU.assertBool "sorcery, not instant" (not (Card.isInstant card))
              HU.assertEqual "one AnyTarget slot" (Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.AnyTarget) (Card.Type.targetSpecs card)
              HU.assertEqual "effect deals X" [Effect.DealDamage (SlotName.MkSlotName (Text.pack "target")) Quantity.Type.X] (Card.Type.effects card),
```

- [x] **Step 2: Run to verify it fails**

Run: `cabal build all --enable-tests 2>&1 | tail -20`
Expected: FAIL — `Cards.blazePrinting` not in scope; `CardType.Sorcery` not a constructor.

- [x] **Step 3: Add `CardType.Sorcery` and its Codec**

In `source/library/Pawl/Type/CardType.hs`, add after `Instant`:

```haskell
  | -- CR 307: a sorcery, cast only at sorcery speed (not a permanent). Blaze is
    -- the first sorcery printing (M4a).
    Sorcery
```

In `source/library/Pawl/Codec.hs`:
- `cardTypeToJson` (~115): add `CardType.Sorcery -> "Sorcery"`.
- `jsonToCardType` assoc list (~123): add `(Text.pack "Sorcery", CardType.Sorcery)`.

- [x] **Step 4: Write `data/cards/blaze.json`**

Create `data/cards/blaze.json` (one line, matching the pool's render format):

```json
{"name":"Blaze","manaCost":[{"type":"Variable"},{"type":"OfType","value":{"type":"Colored","value":{"type":"Red"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Sorcery"}],"subtypes":[]},"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"effects":[{"type":"DealDamage","value":["target",{"type":"X"}]}],"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"targetSpecs":[{"slot":"target","spec":{"type":"AnyTarget"}}]}
```

- [x] **Step 5: Register Blaze in `Cards.hs`**

In `source/test-suite/Pawl/Cards.hs`: add `blazePrinting :: Printing.Printing` to the record; `blazePrinting_ <- loadPrinting "blaze"` in the loader; `blazePrinting = blazePrinting_` in the record build; add `blazePrinting cards,` to `allPrintings`; and add `(blazePrinting cards, 4)` to `redDeck`. Update `CardSpec.hs:168` count assertion from `30` to `31`.

- [x] **Step 6: Run to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -20`
Expected: PASS — Blaze loads; the honesty round-trip (`jsonToCard . cardToJson`) holds for it; the count test reads 31.

- [x] **Step 7: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4a: Blaze as card data, plus CardType.Sorcery"
```

---

### Task 9: Generalize the D4 lint for X

**Files:**
- Modify: `source/library/Pawl/Resolve.hs` (add an X-reader classifier) and `source/test-suite/Pawl/CardSpec.hs:155-177` (the lint group)

**Interfaces:**
- Produces: `Resolve.readsX :: [Effect] -> Bool` (an effect list references `Quantity.X`); a lint conjunct "reads X ⟺ cost has a Variable"; the reserved X slot exempt from the target-slot equality.

- [x] **Step 1: Write the failing test**

In `source/test-suite/Pawl/CardSpec.hs`, extend `lintTests`:

```haskell
      HU.testCase "every printing that reads X declares {X}, and vice versa" $
        let readsX c = Resolve.readsX (Card.Type.effects c)
            hasVariable c = case Card.Type.manaCost c of
              Nothing -> False
              Just (ManaCost.MkManaCost syms) -> elem ManaSymbol.Variable syms
            offenders =
              filter
                (\p -> readsX (Printing.card p) /= hasVariable (Printing.card p))
                (Cards.allPrintings cards)
         in HU.assertEqual "X read iff {X} declared" [] (map (Card.Type.name . Printing.card) offenders),
      HU.testCase "the reserved X slot is never a declared target slot" $
        let offenders =
              filter
                (\p -> Map.member Binding.variableX (Card.Type.targetSpecs (Printing.card p)))
                (Cards.allPrintings cards)
         in HU.assertEqual "no card names the X slot" [] (map (Card.Type.name . Printing.card) offenders),
```

Also confirm the existing "slot reads equal declared slots" test still passes for Blaze (its `"target"` slot is declared and read; `Quantity.X` contributes no slot to `slotsOf`).

- [x] **Step 2: Run to verify it fails**

Run: `cabal build all --enable-tests 2>&1 | tail -20`
Expected: FAIL — `Resolve.readsX` not in scope.

- [x] **Step 3: Add the X-reader classifier**

In `source/library/Pawl/Resolve.hs`, alongside `slotsOf`:

```haskell
-- D4 (the value half): does any of these effects read X? A card that reads X
-- must declare {X} in its cost (the lint), the same reads-equal-declares contract
-- slotsOf draws for target slots. Casing on Effect/Quantity is this module's
-- charter.
readsX :: [Effect] -> Bool
readsX = any effectReadsX
  where
    effectReadsX effect = case effect of
      Effect.DealDamage _ quantity -> quantity == Quantity.X
      Effect.ModifyTarget _ _ _ -> False
      Effect.ChangeText _ -> False
      Effect.AddMana _ -> False
      Effect.Search _ -> False
      Effect.ExileAllGraveyards -> False
      Effect.ControlPlayerNextTurn _ -> False
```

(A future opcode carrying a `Quantity` extends this match — the compiler will not force it since `Quantity` is compared by `==`, so add a note: when an opcode gains a `Quantity` field, add its arm here.)

- [x] **Step 4: Run to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -15`
Expected: PASS.

- [x] **Step 5: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4a: generalize the D4 lint -- a card reads X iff it declares {X}"
```

---

### Task 10: Blaze gameplay scenarios and random-game coverage

**Files:**
- Modify: `source/test-suite/Pawl/CastSpec.hs` (or a new `M4aCards` group in `CardSpec.hs`) — the X scenarios
- Modify: the property suite entry points if a new `ChooseX` interpreter arm needs asserting (Support.hs answerers already updated in Task 6)

**Interfaces:**
- Consumes: everything above. Produces: gameplay-level assertions that Blaze deals the chosen X, X=0 is legal, and an unaffordable X is a no-op; plus random-game coverage via `redDeck`.

- [ ] **Step 1: Write the failing tests**

Add a Blaze gameplay group (deterministic fixtures, following the M3a Lightning Bolt scenario style):

```haskell
      HU.testCase "Blaze at X=3 deals 3 to the opponent (CR 601.2b/f/h, 608.2)" $
        let after = resolveBlaze answerX3 gs0 -- cast Blaze at Bob, X=3, pay {3}{R}, resolve
         in HU.assertEqual "Bob at 17" 17 (lifeOf S.bob after),
      HU.testCase "Blaze at X=0 is castable and deals nothing (the X=0 floor)" $
        let after = resolveBlaze answerX0 gs0
         in do
              HU.assertBool "was cast (paid {R})" (blazeLeftHand after)
              HU.assertEqual "Bob unharmed" 20 (lifeOf S.bob after),
      HU.testCase "Blaze at an unaffordable X is a no-op (reject-not-repair)" $
        let after = castBlaze answerX3 gsOnlyOneRed -- only {R} available, choose X=3
         in do
              HU.assertBool "still in hand" (blazeInHand after)
              HU.assertEqual "no life change" 20 (lifeOf S.bob after),
```

Build the fixtures from the M3a red mana base (Mountains) plus Blaze in hand; `answerX3`/`answerX0` are answerers whose `Prompt.ChooseX` arm returns 3 / 0 and whose target arm picks Bob.

- [ ] **Step 2: Run to verify they fail**

Run: `cabal build all --enable-tests 2>&1 | tail -20`
Expected: FAIL — the fixtures/helpers are undefined (write them), then the assertions drive the behavior.

- [ ] **Step 3: Implement the fixtures/helpers**

Add the `resolveBlaze` / `castBlaze` helpers (cast through the stack via `Engine.runGamePure` with the given answerer, then resolve the top), reusing existing CastSpec plumbing. No engine change should be needed — if a test fails on engine behavior, STOP: a green-code test failure is a plan bug, per `CLAUDE.md`.

- [ ] **Step 4: Run to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -20`
Expected: PASS — all three scenarios green.

- [ ] **Step 5: Confirm random-game coverage**

Run the property suite (it runs `runMatch` over the matchups, now with Blaze in `redDeck` and the `ChooseX` random answerer from Task 6). Confirm the red-matchup properties (conservation, termination, ids, no floating mana at end of step, life-never-increases, replay determinism) still hold with Blaze in the deck.

Run: `cabal test 2>&1 | tail -20`
Expected: PASS — properties green; replay determinism now covers `ChoseX`.

- [ ] **Step 6: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4a: Blaze gameplay scenarios (X=3, X=0, unaffordable) and random-game coverage"
```

---

## Self-Review

**1. Spec coverage** (against `2026-07-19-m4a-numeric-tower-binding-design.md`):

- §0 phased spine (binding env then X) → Phase 1 (Tasks 1–2), Phase 2 (Tasks 3–10). ✅
- §1 `Binding` product record → Task 1. `Object.bindings` → Task 2. `Quantity.X` → Task 5. `ManaSymbol.Variable` → Task 3. `Prompt.ChooseX`/`Response.ChoseX` → Task 6. Blaze card → Task 8. Reserved X slot (`Binding.variableX`) → Task 1, exemption lint → Task 9. ✅
- §2 migration (write site, read sites, changeZone reset) → Task 2 (all sites enumerated with line numbers). ✅
- §3 choose-at-cast in CR order, `substituteX`, castability floor, evaluate arm, freeze note → Tasks 4 (substitute+floor), 5 (evaluate), 7 (castSpell order + stamp). Freeze is a non-goal (named expiry), correctly untouched. ✅
- §4 generalized D4 lint (reads X ⟺ {X}; reserved slot exempt) → Task 9. ✅
- §5 invariants (Resolve sole `case`; X prompted; conventions) → preserved across tasks; `readsX` lives in `Resolve`. ✅
- §6 testing (Phase 1 = existing suite green; Phase 2 deterministic + random) → Tasks 2, 10. ✅
- §7 expiries → documented in the spec; git-bug `c7a0077` already tracks the `Quantity.Bound SlotName` generalization. No task needed (expiries are deferrals). ✅

**2. Placeholder scan:** No "TBD"/"handle edge cases"/"similar to". Test fixtures that reuse existing plumbing (`resolveBlaze`, `castThrough`) name the pattern to copy and the file it lives in. ✅

**3. Type consistency:** `Binding.fromChoices` (3 args: targets, subtypes, `Maybe Natural`) is used identically in Tasks 1, 2, 7. `Binding.variableX`, `Binding.amountOf`, `Binding.targetsOf`, `Binding.subtypesOf` consistent across Tasks 1, 2, 5, 7, 9. `Mana.substituteX :: Natural -> ManaCost -> ManaCost` consistent across Tasks 4, 7. `Prompt.ChooseX :: Decider -> PlayerId -> ObjectId -> Prompt Natural` / `Response.ChoseX Natural` consistent across Tasks 6, 7, 10. `Resolve.readsX :: [Effect] -> Bool` consistent across Tasks 9. `Quantity.evaluate :: GameState -> ObjectId -> Quantity -> Maybe Integer` (unchanged signature, new arm) — Task 5. ✅

One known soft spot flagged for the implementer: the exact `Replay` helper names (`Replay.replayAnswer` in Task 6's test is illustrative) — use whatever `Replay.hs:30-33/63-72` actually expose (`toResponse`/`fromResponse`-style). The wiring sites are pinned by line number regardless.
