# M3e Activated Abilities (Abilities on the Stack) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make an activated ability a first-class object on the stack — Prodigal Sorcerer's `{T}` ability resolves and deals damage then ceases, Evolving Wilds' `{T}, Sacrifice` ability searches a library, and Llanowar Elves' `{T}: Add {G}` is classified a mana ability that never touches the stack — proving the CR 605 mana-ability ABI predicate.

**Architecture:** A new `ActivatedAbility` (cost + reused `Effect`s + target slots) is put on the stack as a new `Source.OfAbility` incarnation by an `Action.Activate` carrying the ability *value* (validated by membership in the projected abilities). It resolves through the existing `Resolve` executor with the source *permanent* as the effect source, then ceases to exist (CR 608.2n) rather than being buried. One ABI predicate, `isManaAbility` (structure: produces mana and targets nothing), is read at exactly two sites — `Mana.manaTypesOf` includes a mana ability as a source; `Action.legalActions` excludes it from the stack. `Search` forces resolution to become `Game`-monadic. `abilitiesOf` is a projection (the `keywordsOf` move) so Humility strips activated abilities.

**Tech Stack:** Haskell 2010 (GHC 9.14.1), `tasty` (`tasty-hunit` + `tasty-quickcheck`), Cabal. Boot libraries only.

## Global Constraints

Copied from the spec and `CLAUDE.md`; every task implicitly includes these:

- **Haskell 2010, no language extensions** beyond `GADTs`, `RankNTypes`, `NamedFieldPuns`. No `LambdaCase`, no `OverloadedStrings`, **no list comprehensions**.
- **No explicit export lists** (`module Pawl.Foo where`).
- **One type per module** under `Pawl.Type.<TypeName>` (type + instances only); logic in other `Pawl.*` modules. A module never imports its parents. Add a new `Pawl.Type.*` file and run `cabal-gild` (via `hooky fix`) — the `exposed-modules` field is `discover`-generated. A new `Pawl.*Spec` goes in the test-suite `other-modules` list.
- **Qualified imports, aliased to the last component** (`Data.List` → `List`); operators unqualified; one import group.
- **No partial functions** — `Maybe`/`Either`, never `head`/`error`/non-exhaustive matches.
- **`newtype`/record + `Mk`-prefixed, non-punning constructors**; build records with `do`/record syntax. Sum-type data constructors (like `TapSelf`, `AddMana`) take no `Mk` prefix.
- **Prefer explicit:** `case` over point-free; `let` over `where`; `$` over parens, `.` over chained `$`; `Text` not `String`; arbitrary-precision numbers.
- **No boolean blindness**; **derive at least `Eq` and `Show`** (and `Ord` on anything a `Card`/`Action`/`Source` transitively contains — those derive `Ord`).
- **Warning-clean** under `-Weverything` minus the allow-list; the `pedantic` flag makes any warning a build failure (including `-Wincomplete-patterns` on a match missing a new constructor, and `-Wmissing-fields` on a record missing a new field). Build `all`: `cabal build all --enable-tests --enable-benchmarks`. When in doubt, `cabal clean` first — incremental builds hide warnings.
- **The two invariants:** the rules core never `case`s on an effect's *identity*; `Pawl.Resolve` is the **sole** `case`-on-`Effect` home (`slotsOf`, `manaProduced`, `rewriteEffect`, `matchesCriterion`, `applyEffect`), `Pawl.Projection` the **sole** `case`-on-`Modification` home. Constructing a value is not casing on it. `Stack.resolveTop` dispatches on the `Source`/type-line *classification*, never a card's identity. The engine makes no player choice except where the rules leave one, eliding only indistinguishable options (with a documented expiry).
- **Every rules claim cited** against `docs/rules.txt` in a code comment. Never trust recalled Magic rules. (Numbers verified for this plan: activation CR 602.2; mana-ability definition CR 605.1a; mana ability doesn't use the stack CR 605.3b; ability ceases to exist CR 608.2n; summoning sickness CR 302.6; Sacrifice CR 701.21; Search CR 701.23, fail-to-find 701.23b; basic land CR 205.4c.)
- **After each task:** `cabal build all --enable-tests --enable-benchmarks` warning-free, `cabal test`, `git add -A` (stage explicit paths under a shared worktree) then `hooky fix` && `git add -A` && `hooky run`, HLint clean. Commit directly to `main`, one small complete commit per task.

**Commit message footer** (every commit):

```
Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
```

**Cards (Scryfall-verified 2026-07-18):**
- **Prodigal Sorcerer** — `{2}{U}` Creature — Human Wizard Sorcerer — 1/1 — "{T}: This creature deals 1 damage to any target."
- **Llanowar Elves** — `{G}` Creature — Elf Druid — 1/1 — "{T}: Add {G}."
- **Evolving Wilds** — Land (no mana cost) — "{T}, Sacrifice this land: Search your library for a basic land card, put it onto the battlefield tapped, then shuffle."

(Creature subtypes Wizard/Sorcerer/Elf/Druid are not load-bearing in M3e — no tribal — and are omitted from the printings' `subtypes` sets. Creature-ness comes from the `Creature` card type. A comment in each printing records the omission.)

---

## Phase 1 — activation onto the stack + the CR 605 classification (Tasks 1–6)

### Task 1: Ability types, the `AddMana` opcode, and the CR 605 classifiers

**Files:**
- Create: `source/library/Pawl/Type/AdditionalCost.hs`
- Create: `source/library/Pawl/Type/AbilityCost.hs`
- Create: `source/library/Pawl/Type/ActivatedAbility.hs`
- Modify: `source/library/Pawl/Type/Effect.hs` (add `AddMana`)
- Modify: `source/library/Pawl/Resolve.hs` (`manaProduced`; `slotsOf`, `rewriteEffect`, `applyEffect` arms for `AddMana`)
- Modify: `source/library/Pawl/Mana.hs` (`isManaAbility`)
- Test: `source/test-suite/Pawl/ManaSpec.hs`, `source/test-suite/Pawl/ResolveSpec.hs`

**Interfaces:**
- Produces: `AdditionalCost.AdditionalCost` = `TapSelf | SacrificeSelf`; `AbilityCost.MkAbilityCost { additional :: [AdditionalCost] }`; `ActivatedAbility.MkActivatedAbility { cost :: AbilityCost, effects :: [Effect], targetSpecs :: Map SlotName TargetSpec }`; `Effect.AddMana :: ManaType -> Effect`; `Resolve.manaProduced :: Effect -> Maybe ManaType`; `Mana.isManaAbility :: ActivatedAbility -> Bool`.

- [ ] **Step 1: Write the failing tests**

Add to the `Mana` test group in `source/test-suite/Pawl/ManaSpec.hs` (add imports `qualified Pawl.Type.ActivatedAbility as ActivatedAbility`, `qualified Pawl.Type.AbilityCost as AbilityCost`, `qualified Pawl.Type.Effect as Effect`, `qualified Pawl.Type.ManaType as ManaType`, `qualified Pawl.Type.Color as Color`, `qualified Pawl.Type.TargetSpec as TargetSpec`, `qualified Pawl.Type.SlotName as SlotName`, `qualified Data.Map.Strict as Map`, `qualified Data.Text as Text` if absent):

```haskell
      HU.testCase "CR 605.1a a {T}: Add {G} ability is a mana ability" $
        let ab =
              ActivatedAbility.MkActivatedAbility
                { ActivatedAbility.cost = AbilityCost.MkAbilityCost {AbilityCost.additional = []},
                  ActivatedAbility.effects = [Effect.AddMana (ManaType.Colored Color.Green)],
                  ActivatedAbility.targetSpecs = Map.empty
                }
         in HU.assertBool "mana ability" (Mana.isManaAbility ab),
      HU.testCase "CR 605.1a an ability that targets is NOT a mana ability" $
        let ab =
              ActivatedAbility.MkActivatedAbility
                { ActivatedAbility.cost = AbilityCost.MkAbilityCost {AbilityCost.additional = []},
                  ActivatedAbility.effects = [Effect.AddMana (ManaType.Colored Color.Green)],
                  ActivatedAbility.targetSpecs = Map.singleton (SlotName.MkSlotName (Text.pack "x")) TargetSpec.AnyTarget
                }
         in HU.assertBool "targets -> not mana" (not (Mana.isManaAbility ab)),
      HU.testCase "CR 605.1a a damage ability is NOT a mana ability" $
        let ab =
              ActivatedAbility.MkActivatedAbility
                { ActivatedAbility.cost = AbilityCost.MkAbilityCost {AbilityCost.additional = []},
                  ActivatedAbility.effects = [Effect.DealDamage (SlotName.MkSlotName (Text.pack "x")) (Quantity.Literal 1)],
                  ActivatedAbility.targetSpecs = Map.singleton (SlotName.MkSlotName (Text.pack "x")) TargetSpec.AnyTarget
                }
         in HU.assertBool "no mana produced -> not mana" (not (Mana.isManaAbility ab)),
```

(Add `qualified Pawl.Type.Quantity as Quantity` if absent.)

Add to the `Resolve` test group in `source/test-suite/Pawl/ResolveSpec.hs`:

```haskell
      HU.testCase "CR 605 manaProduced reads AddMana, nothing else" $ do
        HU.assertEqual "add mana" (Just (ManaType.Colored Color.Green)) (Resolve.manaProduced (Effect.AddMana (ManaType.Colored Color.Green)))
        HU.assertEqual "damage produces no mana" Nothing (Resolve.manaProduced (Effect.DealDamage (SlotName.MkSlotName (Text.pack "x")) (Quantity.Literal 1))),
```

(Add `qualified Pawl.Type.ManaType as ManaType`, `qualified Pawl.Type.Color as Color` to `ResolveSpec` if absent.)

- [ ] **Step 2: Run tests to verify they fail**

Run: `cabal test --test-options='-p "is a mana ability"'`
Expected: FAIL to compile — `ActivatedAbility`, `AbilityCost`, `Effect.AddMana`, `Mana.isManaAbility`, `Resolve.manaProduced` not in scope.

- [ ] **Step 3: Create the three ability types**

`source/library/Pawl/Type/AdditionalCost.hs`:

```haskell
module Pawl.Type.AdditionalCost where

-- A non-mana cost of an activated ability that names the source permanent.
-- CR 602.1a: the {T} symbol taps it; CR 701.21: Sacrifice moves it to its
-- owner's graveyard. Grows: pay life, discard, remove a counter, tap another
-- object, ....
data AdditionalCost
  = TapSelf
  | SacrificeSelf
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/AbilityCost.hs`:

```haskell
module Pawl.Type.AbilityCost where

import Pawl.Type.AdditionalCost (AdditionalCost)

-- The cost of an activated ability (CR 602.1). A `mana :: Maybe ManaCost` field
-- is the named future addition (no M3e gate has a mana symbol in its ability
-- cost); for now only the non-mana costs.
data AbilityCost = MkAbilityCost
  { additional :: [AdditionalCost]
  }
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/ActivatedAbility.hs`:

```haskell
module Pawl.Type.ActivatedAbility where

import Data.Map.Strict (Map)
import Pawl.Type.AbilityCost (AbilityCost)
import Pawl.Type.Effect (Effect)
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.TargetSpec (TargetSpec)

-- CR 602.1: "[cost]: [effect]". Reuses the Effect vocabulary and the slot/target
-- machinery of a spell. An ability is VALUE-typed: two abilities with the same
-- cost and effect are indistinguishable, so Action.Activate carries the value and
-- validates by membership (Projection.abilitiesOf), never an index.
data ActivatedAbility = MkActivatedAbility
  { cost :: AbilityCost,
    effects :: [Effect],
    targetSpecs :: Map SlotName TargetSpec
  }
  deriving (Eq, Ord, Show)
```

- [ ] **Step 4: Add the `AddMana` opcode**

In `source/library/Pawl/Type/Effect.hs`, add the import `import Pawl.Type.ManaType (ManaType)` and the constructor (extend the header comment):

```haskell
  | -- CR 605: add one unit of this mana type. Executed by Mana.tapForMana at
    -- payment (CR 605.3b: a mana ability never uses the stack); Resolve.applyEffect
    -- never runs it. Read by Resolve.manaProduced (the "produces mana?" ABI bit).
    AddMana ManaType
```

- [ ] **Step 5: Add the classifiers and the `AddMana` arms in `Resolve`**

In `source/library/Pawl/Resolve.hs`, add `import Pawl.Type.ManaType (ManaType)`. Add the `slotsOf` arm:

```haskell
  Effect.AddMana _ -> Set.empty
```

Add the `rewriteEffect` arm (an `AddMana` carries no basic-land-type word):

```haskell
  Effect.AddMana _ -> effect
```

Add `manaProduced` (a new `case`-on-`Effect`, Resolve's charter — alongside `slotsOf`):

```haskell
-- CR 605: does this effect add mana, and which type? The "produces mana?" ABI
-- classification (design.md risk register). Read by Mana.isManaAbility to keep
-- mana abilities off the stack. Casing on Effect is Resolve's charter.
manaProduced :: Effect -> Maybe ManaType
manaProduced effect = case effect of
  Effect.AddMana mt -> Just mt
  Effect.DealDamage _ _ -> Nothing
  Effect.ModifyTarget _ _ _ -> Nothing
  Effect.ChangeText _ -> Nothing
```

Add the `applyEffect` arm (a documented no-op — Task 3 makes `applyEffect` monadic, and this arm becomes `pure ()` then; write the pure form now):

```haskell
  -- CR 605.3b: a mana ability never resolves on the stack. AddMana is applied by
  -- Mana.tapForMana at payment, never here. Reaching this arm means a mana ability
  -- was wrongly put on the stack -- an isManaAbility classification bug.
  Effect.AddMana _ -> gs
```

- [ ] **Step 6: Add `isManaAbility` in `Mana`**

In `source/library/Pawl/Mana.hs`, add imports `import qualified Pawl.Resolve as Resolve`, `import qualified Pawl.Type.ActivatedAbility as ActivatedAbility`. Add:

```haskell
-- CR 605.1a: an activated ability is a mana ability if it could add mana AND
-- doesn't target (the loyalty clause is vacuous -- no planeswalkers). The ABI
-- predicate read at two sites: manaTypesOf includes a mana ability as a source
-- (Task 6); Action.legalActions excludes it from the stack (Task 5).
isManaAbility :: ActivatedAbility.ActivatedAbility -> Bool
isManaAbility ab =
  not (null (Maybe.mapMaybe Resolve.manaProduced (ActivatedAbility.effects ab)))
    && Map.null (ActivatedAbility.targetSpecs ab)
```

- [ ] **Step 7: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS.

- [ ] **Step 8: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Add activated-ability types + AddMana + the CR 605 mana-ability classifiers"
```

---

### Task 2: `Card.activatedAbilities` + Prodigal Sorcerer & Llanowar Elves

**Files:**
- Modify: `source/library/Pawl/Type/Card.hs` (add `activatedAbilities`)
- Modify: `source/library/Pawl/Card.hs` (seed `[]` at every printing; add the two printings; `allPrintings`)
- Modify: test/benchmark `Card.MkCard` sites that build a card by hand (the build enumerates them via `-Wmissing-fields`)
- Test: `source/test-suite/Pawl/CardSpec.hs`

**Interfaces:**
- Consumes: `ActivatedAbility` (Task 1).
- Produces: `Card.activatedAbilities :: [ActivatedAbility]` (empty for all but the two new printings); `Card.prodigalSorcererPrinting`, `Card.llanowarElvesPrinting` in `allPrintings`. Prodigal Sorcerer's one ability: cost `{T}`, effect `DealDamage "target" 1`, spec `AnyTarget`. Llanowar Elves' one ability: cost `{T}`, effect `AddMana Green`, no slots.

- [ ] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/CardSpec.hs`. Assert the printings carry the abilities the classifier expects (add imports `qualified Pawl.Card as Card`, `qualified Pawl.Type.Printing as Printing`, `qualified Pawl.Type.Card as Card.Type`, `qualified Pawl.Mana as Mana` if absent):

```haskell
      HU.testCase "Prodigal Sorcerer has one non-mana activated ability" $
        case Card.Type.activatedAbilities (Printing.card Card.prodigalSorcererPrinting) of
          [ab] -> HU.assertBool "not a mana ability" (not (Mana.isManaAbility ab))
          _ -> HU.assertFailure "expected exactly one ability",
      HU.testCase "Llanowar Elves has one mana activated ability" $
        case Card.Type.activatedAbilities (Printing.card Card.llanowarElvesPrinting) of
          [ab] -> HU.assertBool "mana ability" (Mana.isManaAbility ab)
          _ -> HU.assertFailure "expected exactly one ability",
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "activated ability"'`
Expected: FAIL to compile — `Card.Type.activatedAbilities`, `Card.prodigalSorcererPrinting`, `Card.llanowarElvesPrinting` not in scope.

- [ ] **Step 3: Add the field to `Card`**

In `source/library/Pawl/Type/Card.hs`, add `import Pawl.Type.ActivatedAbility (ActivatedAbility)` and the field (after `effects`, before `targetSpecs`):

```haskell
    -- CR 602: this card's printed activated abilities. Empty for all but the few
    -- printings that grant one. The closed half reads these through
    -- Pawl.Projection.abilitiesOf (Task 9), never directly: layer 6 (Humility)
    -- removes abilities.
    activatedAbilities :: [ActivatedAbility],
```

- [ ] **Step 4: Seed `activatedAbilities = []` at every existing `MkCard`**

Every `Card.MkCard { ... }` now needs `Card.activatedAbilities = []` (or `Card.Type.activatedAbilities = []` in tests). The build enumerates any missed one via `-Wmissing-fields` (a pedantic error). Add the line (next to `effects`) at each site. Known sites: every printing in `source/library/Pawl/Card.hs`; the hand-built `Card.Type.MkCard` in `source/test-suite/Pawl/ResolveSpec.hs` (the `textChangeSlots` test from M3d). Build after this step to let the compiler list any others.

- [ ] **Step 5: Add the two printings and register them**

In `source/library/Pawl/Card.hs` (imports for `ActivatedAbility`, `AbilityCost`, `AdditionalCost`, `Effect`, `TargetSpec`, `SlotName`, `Quantity` are needed — add any absent, mirroring existing qualified style):

```haskell
-- Prodigal Sorcerer: {2}{U}, 1/1, "{T}: This creature deals 1 damage to any
-- target." Scryfall-verified 2026-07-18. Reuses DealDamage; the ability goes on
-- the stack (it targets and adds no mana). Subtypes Wizard/Sorcerer omitted (no
-- tribal in M3e).
prodigalSorcererPrinting :: Printing.Printing
prodigalSorcererPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Prodigal Sorcerer",
            Card.manaCost =
              Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, ManaSymbol.OfType (ManaType.Colored Color.Blue)]),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.singleton Subtype.Human
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 1)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 1)),
            Card.keywords = Set.empty,
            Card.staticAbilities = [],
            Card.effects = [],
            Card.activatedAbilities =
              [ ActivatedAbility.MkActivatedAbility
                  { ActivatedAbility.cost = AbilityCost.MkAbilityCost {AbilityCost.additional = [AdditionalCost.TapSelf]},
                    ActivatedAbility.effects = [Effect.DealDamage (SlotName.MkSlotName (Text.pack "target")) (Quantity.Literal 1)],
                    ActivatedAbility.targetSpecs = Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.AnyTarget
                  }
              ],
            Card.targetSpecs = Map.empty
          }
    }

-- Llanowar Elves: {G}, 1/1, "{T}: Add {G}." Scryfall-verified 2026-07-18. A
-- printed activated MANA ability (CR 605.1a): it does not use the stack. Subtypes
-- Elf/Druid omitted (no tribal in M3e).
llanowarElvesPrinting :: Printing.Printing
llanowarElvesPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Llanowar Elves",
            Card.manaCost = Just (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Green)]),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.empty
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 1)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 1)),
            Card.keywords = Set.empty,
            Card.staticAbilities = [],
            Card.effects = [],
            Card.activatedAbilities =
              [ ActivatedAbility.MkActivatedAbility
                  { ActivatedAbility.cost = AbilityCost.MkAbilityCost {AbilityCost.additional = [AdditionalCost.TapSelf]},
                    ActivatedAbility.effects = [Effect.AddMana (ManaType.Colored Color.Green)],
                    ActivatedAbility.targetSpecs = Map.empty
                  }
              ],
            Card.targetSpecs = Map.empty
          }
    }
```

Register both in `allPrintings` (after `landformPrinting`):

```haskell
    landformPrinting,
    prodigalSorcererPrinting,
    llanowarElvesPrinting
  ]
```

- [ ] **Step 6: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS. If the build errors on a missing field, add `Card.activatedAbilities = []` at that `MkCard` site.

- [ ] **Step 7: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Add the activatedAbilities field + Prodigal Sorcerer and Llanowar Elves"
```

---

### Task 3: Make resolution `Game`-monadic (no behavior change)

**Files:**
- Modify: `source/library/Pawl/Resolve.hs` (`applyEffect`, `resolveSpell` → `Game`)
- Modify: `source/library/Pawl/Stack.hs` (`resolveTop` → `Game`)
- Modify: `source/library/Pawl/Engine.hs` (`priorityLoop` resolution site)
- Modify: `source/test-suite/Pawl/ResolveSpec.hs`, `source/test-suite/Pawl/StackSpec.hs`, `source/test-suite/Pawl/Support.hs` (call sites that ran `resolveSpell`/`resolveTop` purely)
- Test: existing suite is the test — this is a behavior-preserving refactor.

**Interfaces:**
- Produces: `Resolve.resolveSpell :: ObjectId -> Game ()`; `Resolve.applyEffect :: ObjectId -> Map SlotName (Subtype, Subtype) -> Map SlotName Bool -> Map SlotName Recipient -> Effect -> Game ()`; `Stack.resolveTop :: Game ()`. Search (Task 7) prompts at resolution, so resolution must be monadic; this task lands the shape with no behavior change (spec §5, §9 — retires M3d's pure-`Resolve` posture).

- [ ] **Step 1: Note the regression baseline**

Run: `cabal test` and record the passing count. This task must end at the same count (plus nothing) — it changes types, not behavior.

- [ ] **Step 2: Convert `applyEffect` to `Game ()`**

In `source/library/Pawl/Resolve.hs`, add `import Pawl.Type.Game (Game)` and `import qualified Control.Monad.Trans.State.Strict as State`. Convert `applyEffect` to `Game ()`: each arm's existing `GameState`-producing body is wrapped in `State.modify' $ \gs -> ...` (the body already binds `gs` and returns a `GameState`, so wrapping is mechanical), and the `gs` function parameter is dropped. `AddMana` becomes `pure ()`. The full converted function (`recipientObject` is unchanged from M3d):

```haskell
applyEffect :: ObjectId -> Map.Map SlotName (Subtype, Subtype) -> Map.Map SlotName Bool -> Map.Map SlotName Recipient -> Effect -> Game ()
applyEffect source bound legality chosen effect = case effect of
  Effect.DealDamage slot quantity ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case Quantity.evaluate gs source quantity of
          Nothing -> gs
          Just n ->
            if n <= 0
              then gs
              else Damage.applyDamage [DamageEvent.MkDamageEvent source recipient (fromInteger n) (Projection.hasKeyword Keyword.Deathtouch source gs)] gs
        _ -> gs
  Effect.ModifyTarget duration modification slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs
          Just target ->
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
        _ -> gs
  Effect.ChangeText slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality, Map.lookup slot bound) of
        (Just recipient, True, Just (from, to)) ->
          case recipientObject recipient of
            Nothing -> gs
            Just target ->
              let (ts, gs1) = Game.freshTimestamp gs
                  eff =
                    ContinuousEffect.MkContinuousEffect
                      { ContinuousEffect.source = source,
                        ContinuousEffect.timestamp = ts,
                        ContinuousEffect.duration = Duration.Indefinite,
                        ContinuousEffect.modification = Modification.ChangeSubtypeWord from to,
                        ContinuousEffect.affected = Affected.TheseObjects (Set.singleton target)
                      }
               in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}
        _ -> gs
  Effect.AddMana _ -> pure ()
```

(The `controller` argument that `Search` needs is added in Task 7, where it is first used — adding it here would be an unused parameter and fail the warning-clean build.)

- [ ] **Step 3: Convert `resolveSpell` to `Game ()`**

Replace `resolveSpell`'s body so it reads state, computes fizzle, and either buries or folds `applyEffect` monadically:

```haskell
resolveSpell :: ObjectId -> Game ()
resolveSpell oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just obj -> case Game.cardOf oid gs of
      Nothing -> pure ()
      Just card ->
        let specs = Card.targetSpecs card
            chosen = Object.targets obj
            legalSlot slot recipient = case Map.lookup slot specs of
              Nothing -> False
              Just spec -> Target.stillLegal recipient spec gs
            legality = Map.mapWithKey legalSlot chosen
            fizzles = not (Map.null specs) && not (or (Map.elems legality))
         in if fizzles
              then State.modify' (Game.changeZone oid Zone.Graveyard)
              else do
                Monad.mapM_ (applyEffect oid (Object.chosenSubtypes obj) legality chosen) (effectsOf oid gs)
                State.modify' (Game.changeZone oid Zone.Graveyard)
```

(Add `import qualified Control.Monad as Monad` if absent. `effectsOf oid gs` is evaluated against the pre-resolution `gs`, matching M3d — the text-change set does not change mid-resolution.)

- [ ] **Step 4: Convert `Stack.resolveTop` to `Game ()`**

In `source/library/Pawl/Stack.hs`, make `resolveTop` monadic (add `import Pawl.Type.Game (Game)`, `import qualified Control.Monad.Trans.State.Strict as State`):

```haskell
resolveTop :: Game ()
resolveTop = do
  gs <- State.get
  case GameState.stack gs of
    [] -> pure ()
    oid : rest -> case Game.lookupObject oid gs of
      Nothing -> State.put gs {GameState.stack = rest}
      Just obj -> case Object.source obj of
        Source.OfCard printing ->
          if Card.isPermanent (Printing.card printing)
            then State.modify' (Game.changeZone oid Zone.Battlefield)
            else Resolve.resolveSpell oid
```

- [ ] **Step 5: Update the priority-loop resolution site**

In `source/library/Pawl/Engine.hs`, `priorityLoop`'s full-round-of-passes branch currently does `let resolved = Sba.checkStateBasedActions (Stack.resolveTop gs)`. Replace with a monadic sequence:

```haskell
                  else do
                    Stack.resolveTop
                    checkSba
                    resolved <- State.get
                    case GameState.result resolved of
                      Just _ ->
                        State.put resolved {GameState.priority = Nothing, GameState.passes = 0}
                      Nothing -> do
                        State.put
                          resolved
                            { GameState.passes = 0,
                              GameState.priority = Just (GameState.activePlayer resolved)
                            }
                        loop
```

(`checkSba` already exists as `State.modify' Sba.checkStateBasedActions`. The `gs` bound at the top of `loop` is now stale after `resolveTop`; re-`get` as shown.)

- [ ] **Step 6: Fix pure call sites in tests**

Any test that wrote `Resolve.resolveSpell oid gs` or `Stack.resolveTop gs` now runs it through the interpreter. Replace with `snd (Engine.runGamePure S.identityAnswer gs (Resolve.resolveSpell oid))` (or `Stack.resolveTop`). Known sites: `source/test-suite/Pawl/ResolveSpec.hs` (Bolt resolution, Magical Hack resolution from M3d), `source/test-suite/Pawl/StackSpec.hs`, and any `Support` helper (e.g. a `resolveTop`-based fixture). The build's type errors list them exactly. Use `S.identityAnswer` unless the test needs a specific answer.

- [ ] **Step 7: Run the suite to verify no behavior changed**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — the same tests as Step 1, green. If a count differs, a call site was mis-threaded; fix it — do not change any assertion.

- [ ] **Step 8: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Make spell/stack resolution Game-monadic (no behavior change; readies Search)"
```

---

### Task 4: `Source.OfAbility` + `resolveAbility` — an ability resolves and ceases

**Files:**
- Modify: `source/library/Pawl/Type/Source.hs` (add `OfAbility`)
- Modify: `source/library/Pawl/Game.hs` (`cardOf` gains an `OfAbility` arm)
- Modify: `source/library/Pawl/Resolve.hs` (`resolveAbility`)
- Modify: `source/library/Pawl/Stack.hs` (`resolveTop` gains the `OfAbility` arm)
- Modify: any other exhaustive `case Object.source` (build enumerates them: `Action.hs`, `Support.hs`)
- Test: `source/test-suite/Pawl/ResolveSpec.hs`

**Interfaces:**
- Consumes: `ActivatedAbility` (Task 1), monadic resolution (Task 3).
- Produces: `Source.OfAbility :: ObjectId -> ActivatedAbility -> Source`; `Resolve.resolveAbility :: ObjectId -> ObjectId -> ActivatedAbility -> Game ()` (ability-object id, source-permanent id, the ability) — folds `applyEffect` with the source permanent as the effect source, then the ability ceases (removed from stack and objects, CR 608.2n); `Stack.resolveTop` resolves an `OfAbility` object.

- [ ] **Step 1: Write the failing test**

Add to the `Resolve` group in `source/test-suite/Pawl/ResolveSpec.hs`. Hand-build an ability object on the stack whose source is a Prodigal Sorcerer, targeting bob, and resolve the top of the stack; assert 1 damage dealt and the ability object gone (add imports `qualified Pawl.Type.Source as Source`, `qualified Pawl.Type.ActivatedAbility as ActivatedAbility`, `qualified Pawl.Type.AbilityCost as AbilityCost`, `qualified Pawl.Type.Recipient as Recipient`, `qualified Pawl.Stack as Stack`, `qualified Pawl.Engine as Engine` if absent):

```haskell
      HU.testCase "CR 608.2n a resolving ability deals its damage and ceases" $
        let (srcId, g0) = S.addCreature Card.prodigalSorcererPrinting S.alice (Setup.emptyGame S.bothPlayers)
            ability = case Card.Type.activatedAbilities (Printing.card Card.prodigalSorcererPrinting) of
              ab : _ -> ab
              [] -> ActivatedAbility.MkActivatedAbility (AbilityCost.MkAbilityCost []) [] Map.empty
            (abilId, g1) = Game.freshObjectId g0
            (ts, g2) = Game.freshTimestamp g1
            slot = SlotName.MkSlotName (Text.pack "target")
            abilObj =
              Object.MkObject
                { Object.owner = S.alice,
                  Object.source = Source.OfAbility srcId ability,
                  Object.zone = Zone.Stack,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled,
                  Object.targets = Map.singleton slot (Recipient.ToPlayer S.bob),
                  Object.chosenSubtypes = Map.empty,
                  Object.timestamp = ts
                }
            g3 =
              g2
                { GameState.objects = Map.insert abilId abilObj (GameState.objects g2),
                  GameState.stack = abilId : GameState.stack g2
                }
            resolved = snd (Engine.runGamePure S.identityAnswer g3 Stack.resolveTop)
         in do
              HU.assertEqual "bob took 1" (Just 19) (S.lifeOf S.bob resolved)
              HU.assertEqual "ability object gone" Nothing (Game.lookupObject abilId resolved)
              HU.assertEqual "stack empty" [] (GameState.stack resolved),
```

(`Object.MkObject`, `TapState`, `Sickness`, `Zone` imports are already in `ResolveSpec` from M3d; add any the compiler flags. The starting life is 20 — confirm against `Setup`; adjust `Just 19` if the default differs.)

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "deals its damage and ceases"'`
Expected: FAIL to compile — `Source.OfAbility`, `Resolve.resolveAbility` not in scope.

- [ ] **Step 3: Add the `OfAbility` source**

In `source/library/Pawl/Type/Source.hs`, add imports `import Pawl.Type.ActivatedAbility (ActivatedAbility)`, `import Pawl.Type.ObjectId (ObjectId)`, and the constructor (it is now a `data`, not a `newtype`):

```haskell
data Source
  = OfCard Printing
  | -- CR 602: an activated ability on the stack -- the source permanent's id plus
    -- the ability. The ability travels with the object so it resolves even if the
    -- source leaves (CR 608.2g; LKI is a future refinement).
    OfAbility ObjectId ActivatedAbility
  deriving (Eq, Ord, Show)
```

- [ ] **Step 4: Handle `OfAbility` in `Game.cardOf` and other `Source` matches**

In `source/library/Pawl/Game.hs`, `cardOf`'s `case Object.source obj of` gains:

```haskell
    Source.OfAbility _ _ -> Nothing
```

The build enumerates every other exhaustive `case Object.source`. Add an `OfAbility` arm at each (all are "an ability is not a card", so the card-reading branch's default applies):
- `source/library/Pawl/Action.hs`, `playableLands`' `isLandObject` — `Source.OfAbility _ _ -> False`.
- `source/test-suite/Pawl/Support.hs`, `creaturesInPlay` and `countByName` — `Source.OfAbility _ _ -> False`.

- [ ] **Step 5: Add `resolveAbility` and the `resolveTop` arm**

In `source/library/Pawl/Resolve.hs`, add `import qualified Pawl.Type.ActivatedAbility as ActivatedAbility`. Add:

```haskell
-- CR 608: resolve an activated ability. The effect SOURCE is the source permanent
-- (srcId), not the ability object -- so DealDamage comes from Prodigal Sorcerer
-- (CR 608.2g). Reuses applyEffect with the same per-slot legality and CR 608.2b
-- fizzle as a spell. CR 608.2n: the ability then ceases to exist -- removed from
-- the stack and objects, NOT buried (an ability is not a card).
resolveAbility :: ObjectId -> ObjectId -> ActivatedAbility.ActivatedAbility -> Game ()
resolveAbility abilId srcId ability = do
  gs <- State.get
  case Game.lookupObject abilId gs of
    Nothing -> pure ()
    Just obj ->
      let specs = ActivatedAbility.targetSpecs ability
          chosen = Object.targets obj
          legalSlot slot recipient = case Map.lookup slot specs of
            Nothing -> False
            Just spec -> Target.stillLegal recipient spec gs
          legality = Map.mapWithKey legalSlot chosen
          fizzles = not (Map.null specs) && not (or (Map.elems legality))
       in do
            Monad.unless fizzles $
              Monad.mapM_ (applyEffect srcId (Object.chosenSubtypes obj) legality chosen) (ActivatedAbility.effects ability)
            State.modify' (cease abilId)

-- CR 608.2n: an ability leaves the stack and ceases to exist (no graveyard).
cease :: ObjectId -> GameState -> GameState
cease abilId gs =
  gs
    { GameState.stack = filter (/= abilId) (GameState.stack gs),
      GameState.objects = Map.delete abilId (GameState.objects gs)
    }
```

In `source/library/Pawl/Stack.hs`, add the `OfAbility` arm to `resolveTop`'s `case Object.source obj of`:

```haskell
        Source.OfAbility srcId ability -> Resolve.resolveAbility oid srcId ability
```

- [ ] **Step 6: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — bob is at 19, the ability object is gone, the stack is empty.

- [ ] **Step 7: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Resolve an ability on the stack: OfAbility + resolveAbility ceases (CR 608.2n)"
```

---

### Task 5: `Action.Activate` + `Pawl.Activate` — Prodigal Sorcerer end-to-end + sickness

**Files:**
- Modify: `source/library/Pawl/Type/Action.hs` (add `Activate`)
- Create: `source/library/Pawl/Activate.hs`
- Modify: `source/library/Pawl/Action.hs` (`legalActions` offers activations)
- Modify: `source/library/Pawl/Engine.hs` (`priorityLoop` `Activate` arm)
- Test: `source/test-suite/Pawl/ActivateSpec.hs` (new — wire into `Main.hs` `testTree` and the test-suite `other-modules`)

**Interfaces:**
- Consumes: `resolveAbility`/`OfAbility` (Task 4), `isManaAbility` (Task 1), the printings (Task 2).
- Produces: `Action.Activate :: ObjectId -> ActivatedAbility -> Action`; `Activate.activatable :: PlayerId -> ObjectId -> ActivatedAbility -> GameState -> Bool` (controlled, non-mana, cost payable, `{T}` not sick-on-a-creature, has a legal target); `Activate.activateAbility :: PlayerId -> ObjectId -> ActivatedAbility -> Game ()` (mint the ability on the stack, prompt+stamp targets, pay costs, keep priority); `Action.legalActions` offers `Activate` for each controlled permanent's non-mana abilities.

- [ ] **Step 1: Write the failing tests**

Create `source/test-suite/Pawl/ActivateSpec.hs`:

```haskell
module Pawl.ActivateSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Action as Action
import qualified Pawl.Activate as Activate
import qualified Pawl.Card as Card
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.AbilityCost as AbilityCost
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.Action as A
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.TapState as TapState
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- The single ability of a printing (all M3e gates have exactly one). Total: the
-- empty-ability fallback is unreachable in these fixtures, and honors the
-- no-partial-functions rule (no `error`).
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility
theAbility p = case Card.Type.activatedAbilities (Printing.card p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (AbilityCost.MkAbilityCost []) [] Map.empty

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Pawl.Activate"
    [ HU.testCase "CR 602 activating Prodigal Sorcerer's {T} puts an ability on the stack and taps it" $
        let (srcId, g0) = S.addCreature Card.prodigalSorcererPrinting S.alice (Setup.emptyGame S.bothPlayers)
            g1 = g0 {GameState.priority = Just S.alice}
            after = snd (Engine.runGamePure S.identityAnswer g1 (Activate.activateAbility S.alice srcId (theAbility Card.prodigalSorcererPrinting)))
         in do
              HU.assertEqual "one thing on the stack" 1 (length (GameState.stack after))
              HU.assertEqual "source tapped" (Just TapState.Tapped) (fmap Object.tapped (Game.lookupObject srcId after)),
      HU.testCase "CR 602.5/302.6 a summoning-sick creature's {T} ability is not offered" $
        let (srcId, g0) = S.addCreature Card.prodigalSorcererPrinting S.alice (Setup.emptyGame S.bothPlayers)
            sick = g0 {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) srcId (GameState.objects g0), GameState.priority = Just S.alice}
         in HU.assertBool "no Activate offered" (not (any isActivate (Action.legalActions S.alice sick))),
      HU.testCase "CR 602 a settled Prodigal Sorcerer's ability IS offered" $
        let (_, g0) = S.addCreature Card.prodigalSorcererPrinting S.alice (Setup.emptyGame S.bothPlayers)
            g1 = g0 {GameState.priority = Just S.alice}
         in HU.assertBool "Activate offered" (any isActivate (Action.legalActions S.alice g1)),
      HU.testCase "CR 602 activating then resolving deals 1 damage and the ability ceases" $
        let (srcId, g0) = S.addCreature Card.prodigalSorcererPrinting S.alice (Setup.emptyGame S.bothPlayers)
            g1 = g0 {GameState.priority = Just S.alice}
            -- identityAnswer's ChooseTargets picks the lowest recipient; with no
            -- creatures but two players, it targets a player. Resolve the stack.
            activated = snd (Engine.runGamePure S.identityAnswer g1 (Activate.activateAbility S.alice srcId (theAbility Card.prodigalSorcererPrinting)))
            resolved = snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
         in HU.assertEqual "stack empty after resolution" [] (GameState.stack resolved)
    ]

isActivate :: A.Action -> Bool
isActivate a = case a of
  A.Activate _ _ -> True
  _ -> False
```

(Wire `Pawl.ActivateSpec.tests` into `Main.hs`'s `testTree` and add `Pawl.ActivateSpec` to the test-suite `other-modules` list.)

- [ ] **Step 2: Run tests to verify they fail**

Run: `cabal test --test-options='-p "puts an ability on the stack"'`
Expected: FAIL to compile — `Action.Activate`, `Pawl.Activate` not in scope.

- [ ] **Step 3: Add the `Activate` action**

In `source/library/Pawl/Type/Action.hs`, add `import Pawl.Type.ActivatedAbility (ActivatedAbility)` and the constructor (update the header comment):

```haskell
  | -- CR 602: activate the source permanent's ability. Carries the ability VALUE
    -- (validated by membership in Projection.abilitiesOf), never an index.
    Activate ObjectId ActivatedAbility
```

- [ ] **Step 4: Create `Pawl.Activate`**

`source/library/Pawl/Activate.hs`:

```haskell
module Pawl.Activate where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Decide as Decide
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.Projection as Projection
import qualified Pawl.Target as Target
import qualified Pawl.Type.AbilityCost as AbilityCost
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.AdditionalCost as AdditionalCost
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CardType as CardType
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Timestamp as Timestamp
import qualified Pawl.Type.Zone as Zone

-- CR 302.6: a creature's {T}-cost ability can't be activated while summoning
-- sick. Reads projected creature-ness -- a land (Evolving Wilds) is never sick-
-- gated. Only {T} (TapSelf) is affected.
tapSicknessOk :: ObjectId -> ActivatedAbility.ActivatedAbility -> GameState -> Bool
tapSicknessOk srcId ability gs =
  let needsTap = elem AdditionalCost.TapSelf (AbilityCost.additional (ActivatedAbility.cost ability))
      isCreature = Set.member CardType.Creature (Projection.cardTypesOf srcId gs)
      settled = case Game.lookupObject srcId gs of
        Just obj -> Object.sickness obj == Sickness.Settled
        Nothing -> False
   in not (needsTap && isCreature && not settled)

-- Can this additional cost be paid right now?
canPayAdditional :: ObjectId -> GameState -> AdditionalCost.AdditionalCost -> Bool
canPayAdditional srcId gs c = case c of
  AdditionalCost.TapSelf -> case Game.lookupObject srcId gs of
    Just obj -> Object.tapped obj == TapState.Untapped
    Nothing -> False
  AdditionalCost.SacrificeSelf -> Set.member srcId (GameState.battlefield gs)

-- The abilities to consider activating. Task 5: the card's PRINTED abilities.
-- Task 9 switches the body to `Projection.abilitiesOf srcId gs` so Humility
-- (layer 6) strips them -- the single switch point, the keywordsOf pattern.
abilitiesFor :: ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility]
abilitiesFor srcId gs = case Game.cardOf srcId gs of
  Nothing -> []
  Just card -> Card.Type.activatedAbilities card

-- CR 602.2/602.5: the ability is a member of the source's abilities (abilitiesFor),
-- it is not a mana ability (mana abilities are handled at payment, not the
-- stack), every additional cost is payable, the {T} sickness gate holds, and
-- every target slot has a legal recipient (CR 602.2b).
activatable :: PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility -> GameState -> Bool
activatable pid srcId ability gs =
  Game.controllerOf srcId gs == Just pid
    && elem ability (abilitiesFor srcId gs)
    && not (Mana.isManaAbility ability)
    && all (canPayAdditional srcId gs) (AbilityCost.additional (ActivatedAbility.cost ability))
    && tapSicknessOk srcId ability gs
    && not (any Set.null (Map.elems (Target.legalSets (ActivatedAbility.targetSpecs ability) gs)))

-- CR 602.2: put the ability on the stack (a fresh OfAbility object), choose and
-- stamp targets (602.2b), pay the additional costs, keep priority (117.3c).
-- Reject-not-repair on an illegal target answer; enumeration guarantees costs are
-- payable, so payment cannot fail after the prompt.
activateAbility :: PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility -> Game ()
activateAbility pid srcId ability = do
  gs <- State.get
  let (abilId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.source = Source.OfAbility srcId ability,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.targets = Map.empty,
            Object.chosenSubtypes = Map.empty,
            Object.timestamp = ts
          }
      onStack =
        gs2
          { GameState.objects = Map.insert abilId obj (GameState.objects gs2),
            GameState.stack = abilId : GameState.stack gs2
          }
      decider = Decide.deciderFor pid gs
      sets = Target.legalSets (ActivatedAbility.targetSpecs ability) gs
  State.put onStack
  chosen <-
    if Map.null sets
      then pure Map.empty
      else Trans.lift (Program.prompt (Prompt.ChooseTargets decider pid abilId sets))
  let keysAgree = Map.keysSet chosen == Map.keysSet sets
      eachLegal = and (Map.intersectionWith Set.member chosen sets)
  if not (keysAgree && eachLegal)
    then State.put gs -- reject: the whole activation is a no-op
    else do
      State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.targets = chosen}) abilId (GameState.objects g)})
      State.modify' (\g -> List.foldl' (payAdditional srcId) g (AbilityCost.additional (ActivatedAbility.cost ability)))

-- Pay one additional cost against the source permanent.
payAdditional :: ObjectId -> GameState -> AdditionalCost.AdditionalCost -> GameState
payAdditional srcId gs c = case c of
  AdditionalCost.TapSelf ->
    gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) srcId (GameState.objects gs)}
  AdditionalCost.SacrificeSelf -> Game.changeZone srcId Zone.Graveyard gs
```

(The module needs `import qualified Data.List as List`. `Timestamp` may be unused — drop that import if the compiler warns.)

- [ ] **Step 5: Offer activations in `legalActions`**

In `source/library/Pawl/Action.hs`, add `import qualified Pawl.Activate as Activate`. Extend `legalActions` to append an `Activate` per controlled permanent's activatable ability (reading `Activate.abilitiesFor`, which Task 9 flips to the projection):

```haskell
legalActions :: PlayerId -> GameState -> [Action]
legalActions pid gs =
  let canPlayLand =
        Turn.isMainPhase (GameState.phase gs)
          && GameState.activePlayer gs == pid
          && not (Set.member pid (GameState.landPlayed gs))
      lands = if canPlayLand then map Action.Play (playableLands pid gs) else []
      spells = map Action.Cast (Cast.castableSpells pid gs)
      activations =
        let forPermanent oid =
              map (Action.Activate oid) (filter (\ab -> Activate.activatable pid oid ab gs) (Activate.abilitiesFor oid gs))
         in concatMap forPermanent (Game.zoneMembers Zone.Battlefield pid gs)
   in Action.Pass : lands ++ spells ++ activations
```

- [ ] **Step 6: Add the `Activate` arm to `priorityLoop`**

In `source/library/Pawl/Engine.hs`, add `import qualified Pawl.Activate as Activate`. Add the arm beside `Cast` (CR 117.3c: activating keeps priority):

```haskell
              Action.Type.Activate oid ability -> do
                Activate.activateAbility p oid ability
                State.modify' $ \g -> g {GameState.passes = 0, GameState.priority = Just p}
                loop
```

- [ ] **Step 7: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — activation puts an ability on the stack and taps the source; a sick creature's ability is not offered, a settled one's is; resolving deals 1 damage and the ability ceases.

- [ ] **Step 8: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Activate an ability onto the stack: Action.Activate + Pawl.Activate (CR 602)"
```

---

### Task 6: The mana-ability true branch — Llanowar Elves taps, no stack

**Files:**
- Modify: `source/library/Pawl/Mana.hs` (`manaTypesOf` reads printed mana abilities; `manaSources` excludes sick creatures)
- Test: `source/test-suite/Pawl/ManaSpec.hs`, `source/test-suite/Pawl/ActivateSpec.hs`

**Interfaces:**
- Consumes: `isManaAbility`/`manaProduced` (Task 1), the printings (Task 2), the sickness field.
- Produces: `Mana.manaTypesOf` returns a permanent's intrinsic subtype mana PLUS the mana of each printed ability that `isManaAbility`; `Mana.manaSources` excludes a summoning-sick creature whose mana ability needs `{T}`. `Action.Activate` already excludes mana abilities (Task 5, via `isManaAbility` in `activatable`), so no separate change is needed here — assert it.

- [ ] **Step 1: Write the failing tests**

Add to `source/test-suite/Pawl/ManaSpec.hs`:

```haskell
      HU.testCase "CR 605 a settled Llanowar Elves is a green mana source" $
        let (elfId, gs) = S.addCreature Card.llanowarElvesPrinting S.alice (Setup.emptyGame S.bothPlayers)
         in do
              HU.assertBool "taps green" (elem (ManaType.Colored Color.Green) (Mana.manaTypesOf elfId gs))
              HU.assertBool "is a mana source" (elem elfId (Mana.manaSources S.alice gs)),
      HU.testCase "CR 302.6 a summoning-sick Llanowar Elves is NOT a mana source" $
        let (elfId, g0) = S.addCreature Card.llanowarElvesPrinting S.alice (Setup.emptyGame S.bothPlayers)
            sick = g0 {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) elfId (GameState.objects g0)}
         in HU.assertBool "sick elf excluded" (notElem elfId (Mana.manaSources S.alice sick)),
```

(Add `qualified Pawl.Setup as Setup`, `qualified Pawl.Type.GameState as GameState`, `qualified Pawl.Type.Object as Object`, `qualified Pawl.Type.Sickness as Sickness` to `ManaSpec` if absent.)

Add to `source/test-suite/Pawl/ActivateSpec.hs` — Llanowar Elves' ability must NOT be offered as a stack action:

```haskell
      HU.testCase "CR 605.3b a mana ability is not offered as a stack activation" $
        let (_, g0) = S.addCreature Card.llanowarElvesPrinting S.alice (Setup.emptyGame S.bothPlayers)
            g1 = g0 {GameState.priority = Just S.alice}
         in HU.assertBool "no Activate for the mana ability" (not (any isActivate (Action.legalActions S.alice g1))),
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cabal test --test-options='-p "green mana source"'`
Expected: FAIL — `manaTypesOf` reads only the subtype (a creature has none), so the Elf is not yet a source; the assertion fails.

- [ ] **Step 3: Read printed mana abilities in `manaTypesOf`**

In `source/library/Pawl/Mana.hs`, extend `manaTypesOf` to add the mana of printed abilities classified as mana abilities. Add `import qualified Pawl.Type.Card as Card.Type`:

```haskell
-- Every mana type an object could produce: its intrinsic subtype mana (CR 305.6)
-- PLUS every printed activated ability that is a mana ability (CR 605.1a),
-- resolved inline at payment and never on the stack. Read from the card's
-- abilities directly here; Task 9 switches this to the projection (abilitiesOf)
-- so Humility strips a creature's mana ability.
manaTypesOf :: ObjectId -> GameState -> [ManaType]
manaTypesOf oid gs =
  let fromSubtypes = Maybe.mapMaybe subtypeMana (Set.toList (Projection.subtypesOf oid gs))
      fromAbilities = case Game.cardOf oid gs of
        Nothing -> []
        Just card ->
          Maybe.mapMaybe Resolve.manaProduced $
            concatMap ActivatedAbility.effects (filter isManaAbility (Card.Type.activatedAbilities card))
   in fromSubtypes ++ fromAbilities
```

- [ ] **Step 4: Exclude summoning-sick creatures in `manaSources`**

In `source/library/Pawl/Mana.hs`, extend `manaSources`' `isSource` so a summoning-sick creature is not a source (CR 302.6 — its mana ability needs `{T}`). Add `import qualified Pawl.Type.CardType as CardType`, `import qualified Pawl.Type.Sickness as Sickness`:

```haskell
manaSources :: PlayerId -> GameState -> [ObjectId]
manaSources pid gs =
  let notSickCreature oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj ->
          -- CR 302.6: a sick creature can't use a {T} mana ability. A land is
          -- never sick-gated. (M3e mana abilities all cost {T}.)
          not (Set.member CardType.Creature (Projection.cardTypesOf oid gs) && Object.sickness obj == Sickness.Sick)
      isSource oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> Object.tapped obj == TapState.Untapped && not (null (manaTypesOf oid gs)) && notSickCreature oid
   in filter isSource (Game.zoneMembers Zone.Battlefield pid gs)
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — a settled Elf taps green and is a source; a sick Elf is not; the mana ability is never offered as a stack activation. **The falsifier:** an engine that put every activated ability on the stack would offer (or deadlock on) the Elf's ability; the classification keeps it inline.

- [ ] **Step 6: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Read printed mana abilities as sources; exclude sick creatures (CR 605/302.6)"
```

---

## Phase 2 — sacrifice cost + the Search opcode (Tasks 7–8)

### Task 7: `Search` — the opcode, criterion, prompt, and executor

**Files:**
- Create: `source/library/Pawl/Type/CardCriterion.hs`
- Modify: `source/library/Pawl/Type/Effect.hs` (add `Search`)
- Modify: `source/library/Pawl/Type/Prompt.hs` (add `SearchLibrary`)
- Modify: `source/library/Pawl/Resolve.hs` (`slotsOf`/`rewriteEffect`/`manaProduced` arms; `matchesCriterion`; the `Search` executor in `applyEffect`)
- Modify: `source/test-suite/Pawl/Support.hs` + `source/benchmark/Main.hs` (answerers gain a `SearchLibrary` arm)
- Test: `source/test-suite/Pawl/ResolveSpec.hs`

**Interfaces:**
- Consumes: monadic `applyEffect` (Task 3).
- Produces: `CardCriterion.CardCriterion` = `BasicLandCard`; `Effect.Search :: CardCriterion -> Effect`; `Prompt.SearchLibrary :: Decider -> PlayerId -> [ObjectId] -> Prompt (Maybe ObjectId)`; `Resolve.matchesCriterion :: CardCriterion -> Card -> Bool`; `applyEffect`'s `Search` arm prompts the controller to find a matching library card, puts it onto the battlefield tapped, then shuffles (CR 701.23; fail-to-find allowed, 701.23b).

- [ ] **Step 1: Write the failing test**

Add to the `Resolve` group in `source/test-suite/Pawl/ResolveSpec.hs`. Build a state where alice has a Mountain in her library and resolve a bare `Search BasicLandCard` effect (via a hand-built ability object or directly through `applyEffect`); assert the Mountain is on the battlefield tapped and gone from the library. Use an answerer that finds the first match:

```haskell
      HU.testCase "CR 701.23 Search fetches a basic land to the battlefield tapped" $
        -- The fetched card gets a NEW object id (CR 400.7 changeZone), so assert by
        -- count/tapped-count, never by the library incarnation's id.
        let base = Setup.emptyGame S.bothPlayers
            (_, g1) = S.addLibraryCard Card.mountainPrinting S.alice base
            ability =
              ActivatedAbility.MkActivatedAbility
                (AbilityCost.MkAbilityCost [])
                [Effect.Search CardCriterion.BasicLandCard]
                Map.empty
            (abilId, g2) = Game.freshObjectId g1
            (ts, g3) = Game.freshTimestamp g2
            abilObj =
              Object.MkObject S.alice (Source.OfAbility (ObjectId.MkObjectId 0) ability) Zone.Stack TapState.Untapped 0 Sickness.Settled Map.empty Map.empty ts
            g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
            resolved = snd (Engine.runGamePure findFirst g4 Stack.resolveTop)
         in do
              HU.assertEqual "one permanent on the battlefield" 1 (length (Game.zoneMembers Zone.Battlefield S.alice resolved))
              HU.assertEqual "it is tapped" 1 (S.tappedCount S.alice resolved)
              HU.assertEqual "library empty" [] (Game.zoneMembers Zone.Library S.alice resolved),
      HU.testCase "CR 701.23b Search may fail to find" $
        let base = Setup.emptyGame S.bothPlayers
            (_, g1) = S.addLibraryCard Card.mountainPrinting S.alice base
            ability = ActivatedAbility.MkActivatedAbility (AbilityCost.MkAbilityCost []) [Effect.Search CardCriterion.BasicLandCard] Map.empty
            (abilId, g2) = Game.freshObjectId g1
            (ts, g3) = Game.freshTimestamp g2
            abilObj = Object.MkObject S.alice (Source.OfAbility (ObjectId.MkObjectId 0) ability) Zone.Stack TapState.Untapped 0 Sickness.Settled Map.empty Map.empty ts
            g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = [abilId]}
            resolved = snd (Engine.runGamePure findNothing g4 Stack.resolveTop)
         in HU.assertEqual "nothing entered the battlefield" Set.empty (GameState.battlefield resolved),
```

The `Object.MkObject` positional form matches the field order in `Pawl.Type.Object` (owner, source, zone, tapped, damage, sickness, targets, chosenSubtypes, timestamp). The `Source.OfAbility (ObjectId.MkObjectId 0) ability` source-permanent id is a throwaway: `Search` searches the *controller's* library (the ability object's owner, `S.alice`), never the source permanent — so a dead/placeholder source id is fine (this is exactly why Task 7 threads the controller rather than reading it off the source). Add the two local answerers:

```haskell
findFirst :: Prompt.Prompt r -> r
findFirst p = case p of
  Prompt.SearchLibrary _ _ matches -> case matches of
    m : _ -> Just m
    [] -> Nothing
  _ -> S.identityAnswer p

findNothing :: Prompt.Prompt r -> r
findNothing p = case p of
  Prompt.SearchLibrary {} -> Nothing
  _ -> S.identityAnswer p
```

Add a `Support` helper (Step referenced): `addLibraryCard :: Printing -> PlayerId -> GameState -> (ObjectId, GameState)` puts one card into a player's library. Add it in `source/test-suite/Pawl/Support.hs`:

```haskell
-- One card of a printing in pid's library.
addLibraryCard :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
addLibraryCard printing pid gs =
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
            Object.chosenSubtypes = Map.empty,
            Object.timestamp = ts
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.library = Map.insertWith (Seq.><) pid (Seq.singleton oid) (GameState.library gs2)
          }
      )
```

(Add imports to `ResolveSpec`: `qualified Pawl.Type.CardCriterion as CardCriterion`.)

- [ ] **Step 2: Run tests to verify they fail**

Run: `cabal test --test-options='-p "fetches a basic land"'`
Expected: FAIL to compile — `CardCriterion`, `Effect.Search`, `Prompt.SearchLibrary`, `S.addLibraryCard` not in scope.

- [ ] **Step 3: Add `CardCriterion` and the `Search` opcode**

`source/library/Pawl/Type/CardCriterion.hs`:

```haskell
module Pawl.Type.CardCriterion where

-- A first-order, analyzable predicate over a card, as data (CR 701.23a). Its one
-- inhabitant now is CR 205.4c's basic land. Grows: by color, by card type, by
-- name, .... Evaluated by Resolve.matchesCriterion, never a card's identity.
data CardCriterion
  = BasicLandCard
  deriving (Eq, Ord, Show)
```

In `source/library/Pawl/Type/Effect.hs`, add `import Pawl.Type.CardCriterion (CardCriterion)` and the constructor:

```haskell
  | -- CR 701.23: search the controller's library for a card matching the
    -- criterion, put it onto the battlefield tapped, then shuffle (Evolving
    -- Wilds' exact shape; destination/tapped are baked in for now).
    Search CardCriterion
```

- [ ] **Step 4: Add the `SearchLibrary` prompt**

In `source/library/Pawl/Type/Prompt.hs`, add the constructor:

```haskell
  -- CR 701.23 / 701.23b: the [ObjectId] is the library cards MATCHING the
  -- criterion (the engine pre-filters to legal choices); Nothing is "fail to
  -- find," always permitted for a search of one's own library for a quality.
  SearchLibrary :: Decider -> PlayerId -> [ObjectId] -> Prompt (Maybe ObjectId)
```

- [ ] **Step 5: Add the `Resolve` arms and the `Search` executor**

In `source/library/Pawl/Resolve.hs`, add the `slotsOf` arm (`Search` names no slot):

```haskell
  Effect.Search _ -> Set.empty
```

the `rewriteEffect` arm (no land-type word — `BasicLandCard` is a criterion, not a rewritable subtype word here):

```haskell
  Effect.Search _ -> effect
```

the `manaProduced` arm:

```haskell
  Effect.Search _ -> Nothing
```

`matchesCriterion` (a `case`-on-`CardCriterion`, reading card characteristics — never identity):

```haskell
-- CR 701.23a / 205.4c: does this card match the search criterion? BasicLandCard =
-- a Land with the Basic supertype.
matchesCriterion :: CardCriterion.CardCriterion -> Card.Card -> Bool
matchesCriterion crit card = case crit of
  CardCriterion.BasicLandCard ->
    Set.member CardType.Land (TypeLine.types (Card.typeLine card))
      && Set.member Supertype.Basic (TypeLine.supertypes (Card.typeLine card))
```

(Add imports `qualified Pawl.Type.CardCriterion as CardCriterion`, `qualified Pawl.Type.CardType as CardType`, `qualified Pawl.Type.Supertype as Supertype`, `qualified Pawl.Type.TypeLine as TypeLine`.)

**Thread the controller into `applyEffect`.** `Search` searches the *controller's* library — the controller of the resolving spell/ability, which is NOT the effect's `source` (for an ability, `source` is the source permanent, which may already be sacrificed as a cost — Evolving Wilds). Add a `controller :: PlayerId` parameter to `applyEffect`, immediately after `source`:

```haskell
applyEffect :: ObjectId -> PlayerId -> Map.Map SlotName (Subtype, Subtype) -> Map.Map SlotName Bool -> Map.Map SlotName Recipient -> Effect -> Game ()
applyEffect source controller bound legality chosen effect = case effect of
```

`controller` is used only by the `Search` arm (below), so it is not an unused parameter. Update the two call sites to pass the resolving object's owner:
- In `resolveSpell` (Task 3): `Monad.mapM_ (applyEffect oid (Object.owner obj) (Object.chosenSubtypes obj) legality chosen) (effectsOf oid gs)`.
- In `resolveAbility` (Task 4): `Monad.mapM_ (applyEffect srcId (Object.owner obj) (Object.chosenSubtypes obj) legality chosen) (ActivatedAbility.effects ability)` — `obj` is the ability object, whose `owner` is the activating player (M3e: owner == controller).

Add the `Search` arm to `applyEffect` (monadic — it prompts; it uses the `controller` param, CR 701.23 searches *your* library):

```haskell
  Effect.Search crit ->
    let matches1 g oid = case Game.cardOf oid g of
          Nothing -> False
          Just card -> matchesCriterion crit card
     in do
          gs <- State.get
          let matches = filter (matches1 gs) (Game.zoneMembers Zone.Library controller gs)
              decider = Decide.deciderFor controller gs
          found <- Trans.lift (Program.prompt (Prompt.SearchLibrary decider controller matches))
          case found of
            Nothing -> pure ()
            Just cardId -> State.modify' (putTapped cardId)
          -- CR 701.23: shuffle the (possibly reduced) library afterward.
          lib <- State.gets (Game.zoneMembers Zone.Library controller)
          shuffled <- Trans.lift (Program.prompt (Prompt.Shuffle lib))
          State.modify' (reorderLibrary controller shuffled)

-- Put a library card onto the battlefield tapped (CR 701.23's Evolving Wilds
-- shape). changeZone mints a new object; tap it by id after the move.
putTapped :: ObjectId -> GameState -> GameState
putTapped cardId gs =
  let moved = Game.changeZone cardId Zone.Battlefield gs
   in case newestBattlefieldOf cardId gs moved of
        Nothing -> moved
        Just newId ->
          moved {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) newId (GameState.objects moved)}
```

The `changeZone`-mints-a-new-id problem: `putTapped` must tap the *new* incarnation. The simplest robust approach is to look up the owner and take the battlefield object that was not present before. Implement `newestBattlefieldOf` as "the battlefield id in `moved` that is absent from `gs`" (there is exactly one after a single move):

```haskell
newestBattlefieldOf :: ObjectId -> GameState -> GameState -> Maybe ObjectId
newestBattlefieldOf _ before after =
  case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield before)) of
    newId : _ -> Just newId
    [] -> Nothing
```

`reorderLibrary` writes the shuffled order back:

```haskell
reorderLibrary :: PlayerId -> [ObjectId] -> GameState -> GameState
reorderLibrary pid order gs =
  gs {GameState.library = Map.insert pid (Seq.fromList order) (GameState.library gs)}
```

(Add imports `import Pawl.Type.PlayerId (PlayerId)`, `import qualified Data.Sequence as Seq`, `import qualified Pawl.Decide as Decide`, `import qualified Pawl.Type.Program as Program`, `import qualified Pawl.Type.Prompt as Prompt`, `import qualified Control.Monad.Trans.Class as Trans`, `import qualified Pawl.Type.TapState as TapState` to `Resolve`. `Zone`/`Object`/`GameState` are already imported.)

- [ ] **Step 6: Add the `SearchLibrary` arm to every answerer**

`-Wincomplete-patterns` (pedantic) flags every `Prompt r -> r` matcher. In `source/test-suite/Pawl/Support.hs`, add to `identityAnswer`, `castAnswer`, `aggressiveAnswer`, `playLandAnswer` a canonical fail-to-find (finding nothing is always legal and never wedges a random game):

```haskell
  Prompt.SearchLibrary {} -> Nothing
```

For `randomAnswer` (monadic):

```haskell
  Prompt.SearchLibrary {} -> pure Nothing
```

In `source/benchmark/Main.hs`, add `Prompt.SearchLibrary {} -> Nothing` to each flagged answerer.

- [ ] **Step 7: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — the Mountain is fetched to the battlefield tapped and gone from the library; fail-to-find leaves the battlefield empty.

- [ ] **Step 8: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Add the Search opcode: tutor a basic land to the battlefield tapped (CR 701.23)"
```

---

### Task 8: `SacrificeSelf` + Evolving Wilds end-to-end

**Files:**
- Modify: `source/library/Pawl/Card.hs` (add `evolvingWildsPrinting`; `allPrintings`)
- Test: `source/test-suite/Pawl/ActivateSpec.hs`

**Interfaces:**
- Consumes: `SacrificeSelf` payment (Task 5's `payAdditional`/`canPayAdditional`), the `Search` opcode (Task 7), activation (Task 5).
- Produces: `Card.evolvingWildsPrinting` (in `allPrintings`), a Land with one ability: cost `[TapSelf, SacrificeSelf]`, effect `[Search BasicLandCard]`, no slots. `SacrificeSelf` was already implemented in Task 5; this task exercises it end-to-end.

- [ ] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/ActivateSpec.hs` (add imports `qualified Pawl.Mana as Mana`, `qualified Data.Set as Set`, `qualified Pawl.Type.Zone as Zone`, `qualified Pawl.Type.Prompt as Prompt` if absent), plus a local `findFirst` answerer at module top level:

```haskell
findFirst :: Prompt.Prompt r -> r
findFirst p = case p of
  Prompt.SearchLibrary _ _ matches -> case matches of
    m : _ -> Just m
    [] -> Nothing
  _ -> S.identityAnswer p
```

Then the tests:

```haskell
      HU.testCase "CR 701.21/701.23 Evolving Wilds sacrifices itself and fetches a basic land tapped" $
        -- The fetched land gets a NEW id (CR 400.7); assert by count/tapped-count.
        let base = Setup.emptyGame S.bothPlayers
            (wildsId, g1) = S.addCreature Card.evolvingWildsPrinting S.alice base
            (_, g2) = S.addLibraryCard Card.forestPrinting S.alice g1
            g3 = g2 {GameState.priority = Just S.alice}
            ability = theAbility Card.evolvingWildsPrinting
            activated = snd (Engine.runGamePure findFirst g3 (Activate.activateAbility S.alice wildsId ability))
            resolved = snd (Engine.runGamePure findFirst activated Stack.resolveTop)
         in do
              HU.assertBool "Evolving Wilds' ability is NOT a mana ability" (not (Mana.isManaAbility ability))
              HU.assertBool "Evolving Wilds sacrificed (gone from battlefield)" (not (Set.member wildsId (GameState.battlefield resolved)))
              HU.assertEqual "one permanent on the battlefield (the fetched land)" 1 (length (Game.zoneMembers Zone.Battlefield S.alice resolved))
              HU.assertEqual "the fetched land is tapped" 1 (S.tappedCount S.alice resolved),
      HU.testCase "CR 302.6 a freshly-added land can tap+sac immediately (no summoning sickness)" $
        let base = Setup.emptyGame S.bothPlayers
            (wildsId, g1) = S.addCreature Card.evolvingWildsPrinting S.alice base
            -- Force it Sick: a land ignores sickness, so the ability is still offered.
            g2 = g1 {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) wildsId (GameState.objects g1), GameState.priority = Just S.alice}
         in HU.assertBool "land ability offered despite sickness" (any isActivate (Action.legalActions S.alice g2)),
```

(`S.addCreature` puts a permanent on the battlefield settled regardless of its type — reuse it for the land; a comment notes it stands in for a played land.)

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "Evolving Wilds sacrifices"'`
Expected: FAIL to compile — `Card.evolvingWildsPrinting` not in scope.

- [ ] **Step 3: Add the Evolving Wilds printing**

In `source/library/Pawl/Card.hs`:

```haskell
-- Evolving Wilds: Land, "{T}, Sacrifice this land: Search your library for a
-- basic land card, put it onto the battlefield tapped, then shuffle."
-- Scryfall-verified 2026-07-18. Its ability is NOT a mana ability (it fetches a
-- land but adds no mana) so it uses the stack -- the CR 605 false branch on a
-- mana-adjacent card.
evolvingWildsPrinting :: Printing.Printing
evolvingWildsPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Evolving Wilds",
            Card.manaCost = Nothing,
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Land,
                  TypeLine.subtypes = Set.empty
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty,
            Card.staticAbilities = [],
            Card.effects = [],
            Card.activatedAbilities =
              [ ActivatedAbility.MkActivatedAbility
                  { ActivatedAbility.cost = AbilityCost.MkAbilityCost {AbilityCost.additional = [AdditionalCost.TapSelf, AdditionalCost.SacrificeSelf]},
                    ActivatedAbility.effects = [Effect.Search CardCriterion.BasicLandCard],
                    ActivatedAbility.targetSpecs = Map.empty
                  }
              ],
            Card.targetSpecs = Map.empty
          }
    }
```

Add `import qualified Pawl.Type.CardCriterion as CardCriterion` to `Card.hs`, and register in `allPrintings`:

```haskell
    llanowarElvesPrinting,
    evolvingWildsPrinting
  ]
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — Evolving Wilds sacrifices itself, its ability (not a mana ability) resolves off the stack, and a Forest enters tapped; a sick land's ability is still offered.

- [ ] **Step 5: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Evolving Wilds: sacrifice cost + Search, end-to-end (CR 701.21/701.23)"
```

---

## Phase 3 — `abilitiesOf` as a projection (Task 9)

### Task 9: `abilitiesOf` — Humility strips activated abilities

**Files:**
- Modify: `source/library/Pawl/Type/ProjectedCharacteristics.hs` (add `activatedAbilities`)
- Modify: `source/library/Pawl/Projection.hs` (`baseCharacteristics` seeds it; `applyModification` `LoseAllAbilities` strips it; `abilitiesOf`)
- Modify: `source/library/Pawl/Mana.hs` (`manaTypesOf` reads `abilitiesOf` instead of the printed list)
- Test: `source/test-suite/Pawl/ProjectionSpec.hs`, `source/test-suite/Pawl/ActivateSpec.hs`

**Interfaces:**
- Consumes: the printings, `LoseAllAbilities` (M3b), `activatable`/`manaTypesOf` (Tasks 5–6).
- Produces: `ProjectedCharacteristics.activatedAbilities :: [ActivatedAbility]`; `Projection.abilitiesOf :: ObjectId -> GameState -> [ActivatedAbility]` (printed abilities minus layer-6 `LoseAllAbilities`). This task flips both consumers to the projection: `Activate.abilitiesFor` (used by `activatable`/`legalActions`) and `Mana.manaTypesOf`, so Humility strips a creature's activated *and* mana abilities.

- [ ] **Step 1: Write the failing tests**

Add to `source/test-suite/Pawl/ProjectionSpec.hs` (Humility is `S.withHumility`; Prodigal Sorcerer is a creature, so `AllCreatures LoseAllAbilities` hits it):

```haskell
      HU.testCase "CR 613 layer 6: Humility strips a creature's activated abilities" $
        let (sorcId, g0) = S.addCreature Card.prodigalSorcererPrinting S.alice (Setup.emptyGame S.bothPlayers)
            gs = S.withHumility g0
         in HU.assertEqual "no abilities under Humility" [] (Projection.abilitiesOf sorcId gs),
      HU.testCase "without Humility the ability is present" $
        let (sorcId, gs) = S.addCreature Card.prodigalSorcererPrinting S.alice (Setup.emptyGame S.bothPlayers)
         in HU.assertEqual "one ability" 1 (length (Projection.abilitiesOf sorcId gs)),
```

Add to `source/test-suite/Pawl/ActivateSpec.hs` — a Humility'd Prodigal Sorcerer cannot be activated:

```haskell
      HU.testCase "CR 613/602 a Humility'd Prodigal Sorcerer's ability is not offered" $
        let (_, g0) = S.addCreature Card.prodigalSorcererPrinting S.alice (Setup.emptyGame S.bothPlayers)
            gs = (S.withHumility g0) {GameState.priority = Just S.alice}
         in HU.assertBool "no Activate under Humility" (not (any isActivate (Action.legalActions S.alice gs))),
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cabal test --test-options='-p "Humility strips"'`
Expected: FAIL — `Projection.abilitiesOf` not in scope.

- [ ] **Step 3: Add `activatedAbilities` to `ProjectedCharacteristics`**

In `source/library/Pawl/Type/ProjectedCharacteristics.hs`, add `import Pawl.Type.ActivatedAbility (ActivatedAbility)` and the field:

```haskell
    -- CR 602 / 613 layer 6: the object's activated abilities after the layer
    -- system. Seeded from the card; emptied by LoseAllAbilities (Humility).
    activatedAbilities :: [ActivatedAbility],
```

Both `MkProjectedCharacteristics` sites in `Projection.baseCharacteristics` need the field: the `Nothing` (unknown card) case gets `PC.activatedAbilities = []`; the `Just card` case gets `PC.activatedAbilities = Card.Type.activatedAbilities card`.

- [ ] **Step 4: Strip abilities in `LoseAllAbilities`, add `abilitiesOf`**

In `source/library/Pawl/Projection.hs`, extend the `LoseAllAbilities` arm of `applyModification` to also clear abilities (CR 613 layer 6 removes ALL abilities — keywords, static, and activated):

```haskell
  Modification.LoseAllAbilities ->
    pc {PC.keywords = Set.empty, PC.activatedAbilities = []}
```

Add the projection reader (the `keywordsOf` mirror):

```haskell
-- CR 602 / 613.1f: an object's activated abilities after the layer system, the
-- same projection posture as keywordsOf. A Humility'd creature has none.
abilitiesOf :: ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility]
abilitiesOf oid gs = PC.activatedAbilities (project oid gs)
```

(Add `import qualified Pawl.Type.ActivatedAbility as ActivatedAbility` to `Projection`.)

Then flip `Activate.abilitiesFor` (Task 5) to read the projection — the single switch that makes `activatable` and `legalActions` respect Humility:

```haskell
abilitiesFor :: ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility]
abilitiesFor srcId gs = Projection.abilitiesOf srcId gs
```

(`Pawl.Projection` is already imported in `Activate`; the `Game.cardOf`/`Card.Type` reads in the old body may leave `Card.Type` unused — drop that import if the build warns.)

- [ ] **Step 5: Switch `manaTypesOf` to the projection**

In `source/library/Pawl/Mana.hs`, change `manaTypesOf`'s `fromAbilities` to read `Projection.abilitiesOf` instead of the printed `Card.Type.activatedAbilities`, so a Humility'd creature loses its mana ability too:

```haskell
      fromAbilities =
        Maybe.mapMaybe Resolve.manaProduced $
          concatMap ActivatedAbility.effects (filter isManaAbility (Projection.abilitiesOf oid gs))
```

(The `case Game.cardOf oid gs` wrapper is no longer needed for `fromAbilities`; `abilitiesOf` returns `[]` for an unknown object. Drop the now-unused `Card.Type` import if the build warns.)

- [ ] **Step 6: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — a Humility'd Prodigal Sorcerer projects no abilities and offers no `Activate`; without Humility it has one. **The falsifier:** reading `Card.activatedAbilities` directly would still offer the ability under Humility.

- [ ] **Step 7: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "abilitiesOf as a projection: Humility strips activated abilities (CR 613 layer 6)"
```

---

## Final verification

- [ ] **All `- [ ] **Step` checkboxes above are ticked.** Confirm: `grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-18-m3e-activated-abilities.md` reaches `0`.
- [ ] **Clean build, warning-free:** `cabal clean && cabal build all --enable-tests --enable-benchmarks` (incremental builds hide warnings from unchanged modules).
- [ ] **Full suite green:** `cabal test`.
- [ ] **Lint clean:** `git add -A && hooky fix && git add -A && hooky run`.
- [ ] **Exit criteria (spec § "Goal and scope") all demonstrated:** activation on the stack + cease (Task 4/5), the mana-ability true branch with no stack (Task 6), summoning sickness (Tasks 5, 6, 8), sacrifice + Search (Tasks 7, 8), the `abilitiesOf` projection strip (Task 9).
- [ ] **Update `CLAUDE.md`** with the M3e completion note (mirror the M3d entry's shape), and **close the milestone** if tracked. Do NOT close git-bug `65ce714` (the mana-source-choice expiry) — it remains open by design (spec §9).
- [ ] **Commit the doc update:**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Record M3e complete: activated abilities on the stack (CR 602/605)"
```
