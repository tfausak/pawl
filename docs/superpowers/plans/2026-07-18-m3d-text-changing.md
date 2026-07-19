# M3d Text-Changing (Layer 3 / the Rewritable AST) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land layer-3 text-changing so Magical Hack rewrites a basic-land-type word at three read-points — the projected type line (a hacked Mountain taps a new color), a source's static abilities (hack Blood Moon), and a spell's one-shot effects on the stack — the M3 go/no-go for the rewritable AST.

**Architecture:** One new modification, `ChangeSubtypeWord from to` (the first `Layer.Text` producer), is read (1) in an object's own projection fold, editing its projected `subtypes` before layer 4; (2) at `gather`-time, rewriting the land-type words inside a source permanent's static-ability `Modification`s before they fold onto others; (3) at resolve-time, rewriting the words inside a resolving spell's `Effect`s. The rewrite splits across the two sanctioned modules by which type it destructures: `Projection.rewriteModification` cases on `Modification`, `Resolve.rewriteEffect` cases on `Effect` and delegates the inner modification to Projection. The two chosen land types are a player choice bound at cast (Cast is already monadic; `Resolve` stays pure), stored on the stack `Object`.

**Tech Stack:** Haskell 2010 (GHC 9.14.1), `tasty` (`tasty-hunit` + `tasty-quickcheck`), Cabal. Boot libraries only.

## Global Constraints

Copied from the spec and `CLAUDE.md`; every task implicitly includes these:

- **Haskell 2010, no language extensions** beyond `GADTs`, `RankNTypes`, `NamedFieldPuns`. No `LambdaCase`, no `OverloadedStrings`, **no list comprehensions**.
- **No explicit export lists** (`module Pawl.Foo where`).
- **One type per module** under `Pawl.Type.<TypeName>` (type + instances only); logic in other `Pawl.*` modules. A module never imports its parents.
- **Qualified imports, aliased to the last component** (`Data.List` → `List`); operators unqualified; one import group.
- **No partial functions** — `Maybe`/`Either`, never `head`/`error`/non-exhaustive matches.
- **`newtype` + `Mk`-prefixed, non-punning constructors**; build records with `do`/record syntax. Sum-type data constructors (like `ChangeSubtypeWord`) take no `Mk` prefix.
- **Prefer explicit:** `case` over point-free; `let` over `where`; `$` over parens, `.` over chained `$`; `Text` not `String`; arbitrary-precision numbers.
- **No boolean blindness**; **derive at least `Eq` and `Show`**.
- **Warning-clean** under `-Weverything` minus the allow-list; the `pedantic` flag makes any warning a build failure (including `-Wincomplete-patterns` on a GADT match missing a new constructor). Build `all`: `cabal build all --enable-tests --enable-benchmarks`. When in doubt, `cabal clean` first — incremental builds hide warnings.
- **The two invariants:** the rules core never `case`s on an effect's *identity*; `Pawl.Projection` is the **sole** `case`-on-`Modification` home, `Pawl.Resolve` the **sole** `case`-on-`Effect` home. Constructing a value is not casing on it. The engine makes no player choices except where the rules leave one, and elides only indistinguishable options (with a documented expiry).
- **Every rules claim cited** against `docs/rules.txt` in a code comment. Never trust recalled Magic rules.
- **After each task:** `cabal build all --enable-tests --enable-benchmarks` warning-free, `cabal test`, `git add -A` (stage explicit paths under a shared worktree) then `hooky fix` && `git add -A` && `hooky run`, HLint clean. Commit directly to `main`, one small complete commit per task.

**Commit message footer** (every commit):

```
Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_012ghxv3K6WVbS8LApj4YUao
```

**Cards (Scryfall-verified 2026-07-18):**
- **Magical Hack** — `{U}` Instant — "Change the text of target spell or permanent by replacing all instances of one basic land type with another. (For example, you may change \"swampwalk\" to \"plainswalk.\" This effect lasts indefinitely.)"
- **Blood Moon** — `{2}{R}` Enchantment (already in the repo) — "Nonbasic lands are Mountains."
- **Fixture (labeled synthetic crutch, §8 of the spec):** "Landform" — a `{U}` instant "Target land becomes a Swamp until end of turn" — `ModifyTarget UntilEndOfTurn (SetLandSubtype Swamp)`. Not a real card; expires when a real non-Aura land-type spell (M4 `Attach`/`Destroy`) replaces it.

---

## Phase 1 — Layer-3 machinery + type-line rewrite (Task 1)

### Task 1: `ChangeSubtypeWord` — the layer-3 type-line rewrite (read-point 1)

**Files:**
- Modify: `source/library/Pawl/Type/Subtype.hs` (add `Island`, `Plains`)
- Modify: `source/library/Pawl/Mana.hs` (`subtypeMana` gains `Island`/`Plains`)
- Modify: `source/library/Pawl/Type/Modification.hs` (add `ChangeSubtypeWord`)
- Modify: `source/library/Pawl/Projection.hs` (`layer`, `applyModification`)
- Test: `source/test-suite/Pawl/ProjectionSpec.hs`, `source/test-suite/Pawl/ManaSpec.hs`

**Interfaces:**
- Produces: `Subtype.Island`, `Subtype.Plains`; `Modification.ChangeSubtypeWord :: Subtype -> Subtype -> Modification`, classified `Layer.Text`; `applyModification` rewrites an object's projected `subtypes` (replace `from` with `to` if present). `subtypeMana Island = Just (Colored Blue)`, `subtypeMana Plains = Just (Colored White)`.

- [x] **Step 1: Write the failing tests**

Add to `source/test-suite/Pawl/ProjectionSpec.hs` (the `Timestamp`, `Subtype`, `Modification`, `Layer`, `withEffect` imports/helpers are all present):

```haskell
      HU.testCase "CR 613.1c layer 3: ChangeSubtypeWord is Text" $
        HU.assertEqual "text layer" Layer.Text (Projection.layer (Modification.ChangeSubtypeWord Subtype.Mountain Subtype.Island)),
      HU.testCase "CR 612.1 ChangeSubtypeWord rewrites a Forest's subtype to Island" $
        let gs0 = S.landsInPlay Card.forestPrinting 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = withEffect landId (Timestamp.MkTimestamp 100) (Modification.ChangeSubtypeWord Subtype.Forest Subtype.Island) gs0
         in HU.assertEqual "only Island" (Set.singleton Subtype.Island) (Projection.subtypesOf landId gs),
      HU.testCase "CR 612.2 ChangeSubtypeWord for an absent type is a no-op" $
        let gs0 = S.landsInPlay Card.forestPrinting 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            gs = withEffect landId (Timestamp.MkTimestamp 100) (Modification.ChangeSubtypeWord Subtype.Mountain Subtype.Island) gs0
         in HU.assertEqual "still Forest" (Set.singleton Subtype.Forest) (Projection.subtypesOf landId gs),
```

Add to `source/test-suite/Pawl/ManaSpec.hs` (mirror its style; it already imports `Card`, `Mana`, `Support as S`, `Color`, `ManaType`; add `qualified Pawl.Type.Subtype as Subtype`, `qualified Pawl.Projection as Projection`, `qualified Pawl.Type.Timestamp as Timestamp`, `qualified Pawl.Type.Modification as Modification`, `qualified Pawl.Game as Game`, `qualified Pawl.Type.Zone as Zone`, `qualified Pawl.Type.ObjectId as ObjectId` if not present, and the `withEffect` helper — copy it from `ProjectionSpec` if `ManaSpec` lacks it, or assert `Projection.subtypesOf` is enough and test `subtypeMana` directly):

```haskell
      HU.testCase "CR 305.6 Island taps blue, Plains taps white" $ do
        HU.assertEqual "island" (Just (ManaType.Colored Color.Blue)) (Mana.subtypeMana Subtype.Island)
        HU.assertEqual "plains" (Just (ManaType.Colored Color.White)) (Mana.subtypeMana Subtype.Plains),
```

- [x] **Step 2: Run tests to verify they fail**

Run: `cabal test --test-options='-p "ChangeSubtypeWord"'` and `cabal test --test-options='-p "Island taps blue"'`
Expected: FAIL to compile — `Modification.ChangeSubtypeWord`, `Subtype.Island`, `Subtype.Plains` not in scope.

- [x] **Step 3: Add the two basic land subtypes**

In `source/library/Pawl/Type/Subtype.hs`, add `Island` and `Plains` (keep the existing comment):

```haskell
data Subtype
  = Mountain
  | Swamp
  | Forest
  | Island
  | Plains
  | Goblin
  | Warrior
  | Human
  | Bird
  | Ogre
  | Centaur
  | Cat
  | Dinosaur
  | Beast
  | Rat
  | Elephant
  deriving (Eq, Ord, Show)
```

- [x] **Step 4: Give the two subtypes their mana (CR 305.6)**

In `source/library/Pawl/Mana.hs`, add the two arms to `subtypeMana` (before the creature-type `Nothing` arms):

```haskell
  Subtype.Island -> Just (ManaType.Colored Color.Blue)
  Subtype.Plains -> Just (ManaType.Colored Color.White)
```

- [x] **Step 5: Add the `ChangeSubtypeWord` modification**

In `source/library/Pawl/Type/Modification.hs`, extend the type (the `Subtype` import is already present) and its header comment:

```haskell
  | SetLandSubtype Subtype -- layer 4, CR 305.7 set (Blood Moon -> Mountain)
  | AddLandSubtype Subtype -- layer 4, CR 305.7 add (Urborg -> Swamp)
  | AddCardType CardType -- layer 4 (Opalescence -> Creature)
  | ChangeSubtypeWord Subtype Subtype -- layer 3, CR 612 (Magical Hack: from -> to)
```

- [x] **Step 6: Classify and apply it in `Projection`**

In `source/library/Pawl/Projection.hs`, add the `layer` arm:

```haskell
  Modification.ChangeSubtypeWord _ _ -> Layer.Text
```

Add the `applyModification` arm (inside the `case m of`):

```haskell
  -- CR 612.1/612.2: a text-changing effect replaces one basic land type word with
  -- another where the word is used AS a land type -- here, in the projected type
  -- line. Layer 3, so it folds before layer 4 (Type): a hacked basic Mountain is
  -- an Island by the time mana (CR 305.6) reads its subtypes. Absent `from` is a
  -- no-op.
  Modification.ChangeSubtypeWord from to ->
    if Set.member from (PC.subtypes pc)
      then pc {PC.subtypes = Set.insert to (Set.delete from (PC.subtypes pc))}
      else pc
```

- [x] **Step 7: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS. (`Layer.Text` sorts before `Layer.Type`, so the derived `Ord` already folds text-changing first; no restructure needed.)

- [x] **Step 8: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Rewrite the projected type line at layer 3 (CR 612 ChangeSubtypeWord)"
```

---

## Phase 2 — Magical Hack, castable (Tasks 2–6)

### Task 2: Target a spell or permanent — `ToObject`, the two specs

**Files:**
- Modify: `source/library/Pawl/Type/Recipient.hs` (add `ToObject`)
- Modify: `source/library/Pawl/Type/TargetSpec.hs` (add `SpellOrPermanentTarget`, `LandTarget`)
- Modify: `source/library/Pawl/Target.hs` (`legalRecipients`)
- Modify: `source/test-suite/Pawl/Support.hs` (`isCreatureRecipient` gains a `ToObject` arm)
- Test: `source/test-suite/Pawl/ResolveSpec.hs` (the `Target` group)

**Interfaces:**
- Produces: `Recipient.ToObject :: ObjectId -> Recipient`; `TargetSpec.SpellOrPermanentTarget`, `TargetSpec.LandTarget`; `Target.legalRecipients` maps `SpellOrPermanentTarget` to `ToObject` over `GameState.stack` ++ battlefield, and `LandTarget` to `ToObject` over battlefield lands (projected `Land`).

- [x] **Step 1: Write the failing test**

Add to the `Target` group in `source/test-suite/Pawl/ResolveSpec.hs`:

```haskell
      HU.testCase "CR 115 SpellOrPermanentTarget offers battlefield permanents and stack spells" $
        let (permId, gs) = S.addPiker S.bob (Setup.emptyGame S.bothPlayers)
         in HU.assertBool
              "the permanent is a legal object target"
              (Set.member (Recipient.ToObject permId) (Target.legalRecipients TargetSpec.SpellOrPermanentTarget gs)),
      HU.testCase "LandTarget offers a land as an object target, not a creature or player" $
        let gs = S.mountainsInPlay 1
            landId = case Game.zoneMembers Zone.Battlefield S.alice gs of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
         in do
              HU.assertBool "the land is legal" (Set.member (Recipient.ToObject landId) (Target.legalRecipients TargetSpec.LandTarget gs))
              HU.assertBool "no players" (not (Set.member (Recipient.ToPlayer S.alice) (Target.legalRecipients TargetSpec.LandTarget gs))),
```

(Add `qualified Pawl.Type.ObjectId as ObjectId` to `ResolveSpec` imports if absent.)

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "SpellOrPermanentTarget"'`
Expected: FAIL to compile — `TargetSpec.SpellOrPermanentTarget` and `Recipient.ToObject` not in scope.

- [x] **Step 3: Add the `ToObject` recipient**

In `source/library/Pawl/Type/Recipient.hs`, add the constructor and extend the comment:

```haskell
data Recipient
  = ToCreature ObjectId
  | ToPlayer PlayerId
  | -- A spell on the stack or a permanent, named generically (Magical Hack's
    -- "target spell or permanent", the fixture's "target land"). Text-changing
    -- does not care about creature-ness, so it does not reuse ToCreature.
    ToObject ObjectId
  deriving (Eq, Ord, Show)
```

- [x] **Step 4: Add the two target specs**

In `source/library/Pawl/Type/TargetSpec.hs`:

```haskell
  | -- CR 115: "target spell or permanent" -- any object on the stack, or any
    -- permanent on the battlefield. The first target that reaches the stack.
    SpellOrPermanentTarget
  | -- A land permanent on the battlefield (projected card-type Land). Used by the
    -- M3d fixture "target land becomes ...".
    LandTarget
```

- [x] **Step 5: Enumerate them in `Target.legalRecipients`**

In `source/library/Pawl/Target.hs`, add the two arms to the `case spec of` (the `stack`/battlefield reads use `GameState.stack` and `Game.zoneMembers`/the battlefield set; `Projection.cardTypesOf` gives projected land-ness). Add `import qualified Pawl.Type.CardType as CardType` and `import qualified Pawl.Type.GameState as GameState` if absent:

```haskell
        TargetSpec.SpellOrPermanentTarget ->
          let onStack = map Recipient.ToObject (GameState.stack gs)
              permanents = map Recipient.ToObject (Set.toList (GameState.battlefield gs))
           in Set.fromList (onStack ++ permanents)
        TargetSpec.LandTarget ->
          let isLand oid = Set.member CardType.Land (Projection.cardTypesOf oid gs)
              lands = filter isLand (Set.toList (GameState.battlefield gs))
           in Set.fromList (map Recipient.ToObject lands)
```

- [x] **Step 6: Add the `ToObject` arm to every exhaustive `Recipient` match**

Adding a `Recipient` constructor makes every exhaustive match incomplete (`-Wincomplete-patterns`, a pedantic error). A `ToObject` never receives combat or spell damage in M3d (damage targets are `AnyTarget` → `ToCreature`/`ToPlayer`), so each arm is a safe no-op / `False`. Three known sites:

`source/library/Pawl/Damage.hs`, `legalAssignment`'s `isDefender` (~line 51) — a `ToObject` is not the defending player:

```haskell
      isDefender r = case r of
        Recipient.ToPlayer _ -> True
        Recipient.ToCreature _ -> False
        Recipient.ToObject _ -> False
```

`source/library/Pawl/Damage.hs`, `applyDamage`'s `markOne` (~line 156) — a bare object is never a damage recipient in M3d:

```haskell
        Recipient.ToObject _ -> g
```

`source/test-suite/Pawl/Support.hs`, `isCreatureRecipient`:

```haskell
isCreatureRecipient :: Recipient.Recipient -> Bool
isCreatureRecipient r = case r of
  Recipient.ToCreature _ -> True
  Recipient.ToPlayer _ -> False
  Recipient.ToObject _ -> False
```

**Note:** the build enumerates any other incomplete `Recipient` match; add `Recipient.ToObject _ -> ...` wherever flagged (the `ModifyTarget` arm in `Resolve` has a `_ -> gs` fallback and is unaffected until Task 8).

- [x] **Step 7: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS.

- [x] **Step 8: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Target a spell or permanent: ToObject and the two target specs (CR 115)"
```

---

### Task 3: `Object.chosenSubtypes` — the value-binding store

**Files:**
- Modify: `source/library/Pawl/Type/Object.hs` (add `chosenSubtypes`)
- Modify: `source/library/Pawl/Game.hs` (`changeZone` resets it)
- Modify: `source/library/Pawl/Setup.hs` (its `Object.MkObject`)
- Modify: `source/test-suite/Pawl/Support.hs` (five `Object.MkObject` sites)
- Modify: `source/test-suite/Pawl/GameSpec.hs`, `source/test-suite/Pawl/ResolveSpec.hs` (their `Object.MkObject`)
- Test: `source/test-suite/Pawl/GameSpec.hs`

**Interfaces:**
- Produces: `Object.chosenSubtypes :: Map SlotName (Subtype, Subtype)` — the basic-land-type pairs chosen while casting, by slot name; reset by `changeZone` (CR 400.7 per-incarnation state, like `targets`).

- [x] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/GameSpec.hs` (it already exercises `changeZone` resetting per-incarnation state; add `qualified Data.Map.Strict as Map`, `qualified Pawl.Type.SlotName as SlotName`, `qualified Pawl.Type.Subtype as Subtype`, `qualified Data.Text as Text` if absent):

```haskell
      HU.testCase "CR 400.7 changeZone resets chosenSubtypes" $
        let base = S.oneMountainState Phase.PrecombatMain
            slot = SlotName.MkSlotName (Text.pack "target")
            stamped =
              base
                { GameState.objects =
                    Map.adjust
                      (\o -> o {Object.chosenSubtypes = Map.singleton slot (Subtype.Mountain, Subtype.Island)})
                      (ObjectId.MkObjectId 0)
                      (GameState.objects base)
                }
            moved = Game.changeZone (ObjectId.MkObjectId 0) Zone.Battlefield stamped
            newObj = Game.lookupObject (ObjectId.MkObjectId 1) moved
         in HU.assertEqual "reset to empty" (Just Map.empty) (fmap Object.chosenSubtypes newObj),
```

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "resets chosenSubtypes"'`
Expected: FAIL to compile — `Object.chosenSubtypes` not in scope.

- [x] **Step 3: Add the field**

In `source/library/Pawl/Type/Object.hs`, add the import `import Pawl.Type.Subtype (Subtype)` and the field (after `targets`, before `timestamp`):

```haskell
    -- CR 612 / the D4 binding: the basic-land-type pairs chosen while casting a
    -- text-changing spell, by slot name. Empty for everything but a text-changer
    -- on the stack. Per-incarnation state: reset by changeZone, so CR 400.7
    -- forgets them when the spell moves -- the negative Magical-Hack-on-a-spell
    -- test (Task 8) rides on exactly this reset.
    chosenSubtypes :: Map SlotName (Subtype, Subtype),
```

- [x] **Step 4: Reset it in `changeZone`**

In `source/library/Pawl/Game.hs`, `changeZone`'s `newObj` record update — add `Object.chosenSubtypes = Map.empty`:

```haskell
        newObj = obj {Object.zone = dest, Object.tapped = TapState.Untapped, Object.damage = 0, Object.sickness = Sickness.Sick, Object.targets = Map.empty, Object.chosenSubtypes = Map.empty, Object.timestamp = ts}
```

- [x] **Step 5: Seed it empty at every other constructor**

Add `Object.chosenSubtypes = Map.empty` to each `Object.MkObject` record. Sites (the build enumerates any missed one via `-Wmissing-fields`, which pedantic makes an error):
- `source/library/Pawl/Setup.hs` (~line 130)
- `source/test-suite/Pawl/Support.hs` — `addCreature` (~231), `landsInPlay` (~263), `handOne` (~289), `pikerInHand` (~316), `oneMountainState` (~458)
- `source/test-suite/Pawl/GameSpec.hs` (~88)
- `source/test-suite/Pawl/ResolveSpec.hs` — `twoBoltState` (~160)

Each gets the line, e.g. in `addCreature`:

```haskell
            Object.targets = Map.empty,
            Object.chosenSubtypes = Map.empty,
            Object.timestamp = ts
```

- [x] **Step 6: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS. If the build errors on a missing field, add `Object.chosenSubtypes = Map.empty` at that site.

- [x] **Step 7: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Store cast-time basic-land-type bindings on the Object (CR 400.7-reset)"
```

---

### Task 4: `ChooseBasicLandTypes` — the value-choice prompt

**Files:**
- Modify: `source/library/Pawl/Type/Prompt.hs` (add the constructor)
- Modify: `source/test-suite/Pawl/Support.hs` (five answerers)
- Modify: `source/benchmark/Main.hs` (three answerers)
- Test: none of its own — it is exercised end-to-end in Task 6. (This task is the plumbing its consumer needs; a bare prompt has no behavior to assert alone.)

**Interfaces:**
- Produces: `Prompt.ChooseBasicLandTypes :: Decider -> PlayerId -> ObjectId -> SlotName -> Prompt (Subtype, Subtype)` — choose the `from` and `to` basic land types for a text-changer's slot. Always answerable (the five basics), so it never gates castability.

- [x] **Step 1: Add the prompt constructor**

In `source/library/Pawl/Type/Prompt.hs`, add the import `import Pawl.Type.Subtype (Subtype)` and the constructor:

```haskell
  -- CR 612 / the D4 binding: choose the two basic land types for a text-changing
  -- spell's slot (Magical Hack: "one basic land type" -> "another"). Bound at cast
  -- alongside ChooseTargets; the legal set is always the five basics, so unlike a
  -- target it never gates castability. Cast-vs-resolution timing is elided as
  -- indistinguishable (spec §3), expiry named there.
  ChooseBasicLandTypes :: Decider -> PlayerId -> ObjectId -> SlotName -> Prompt (Subtype, Subtype)
```

- [x] **Step 2: Run the build to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `-Wincomplete-patterns` (pedantic error) on every `Prompt r -> r` matcher lacking the new constructor.

- [x] **Step 3: Add the arm to the pure answerers in `Support`**

In `source/test-suite/Pawl/Support.hs`, add to each of `identityAnswer`, `castAnswer`, `aggressiveAnswer`, `playLandAnswer` a canonical (identity) pair — a hack that changes nothing, safe for any game that never means to cast a text-changer (add `qualified Pawl.Type.Subtype as Subtype` to the imports):

```haskell
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
```

For `randomAnswer` (it returns `State.State Random.StdGen r`), the arm must be monadic:

```haskell
  Prompt.ChooseBasicLandTypes {} -> pure (Subtype.Mountain, Subtype.Mountain)
```

- [x] **Step 4: Add the arm to the benchmark answerers**

In `source/benchmark/Main.hs`, add `import qualified Pawl.Type.Subtype as Subtype` and the same pure arm to each `Prompt r -> r` answerer the build flags (`alwaysPass`, `castAnswer`, `fightAnswer`):

```haskell
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
```

- [x] **Step 5: Run the build and tests**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS. Any remaining `-Wincomplete-patterns` names a test-local answerer without a `_ ->` fallback — add the pure arm there too.

- [x] **Step 6: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Add the ChooseBasicLandTypes value-choice prompt (CR 612 binding)"
```

---

### Task 5: `ChangeText` — the opcode and its executor

**Files:**
- Modify: `source/library/Pawl/Type/Duration.hs` (add `Indefinite`)
- Modify: `source/library/Pawl/Type/Effect.hs` (add `ChangeText`)
- Modify: `source/library/Pawl/Resolve.hs` (`slotsOf`, `textChangeSlots`, `applyEffect`, `resolveSpell` threads the binding)
- Test: `source/test-suite/Pawl/ResolveSpec.hs` (the `Resolve` group)

**Interfaces:**
- Consumes: `Recipient.ToObject` (Task 2), `Object.chosenSubtypes` (Task 3).
- Produces: `Effect.ChangeText :: SlotName -> Effect`; `Duration.Indefinite`; `Resolve.textChangeSlots :: Card.Card -> [SlotName]` (the slots a text-changer binds land types for); `applyEffect`'s `ChangeText` arm stores an `Indefinite` `ChangeSubtypeWord from to` continuous effect on the target object, reading `(from, to)` from the object's `chosenSubtypes`.

- [x] **Step 1: Write the failing test**

Test the opcode's classification directly — self-contained, so Task 5 is green on its own (the executor's *observable* effect is driven end-to-end in Task 6, where `magicalHackPrinting` exists). Add to the `Resolve` group in `source/test-suite/Pawl/ResolveSpec.hs` (add `qualified Pawl.Resolve as Resolve`, `qualified Pawl.Type.Effect as Effect`, `qualified Pawl.Type.Card as Card.Type`, `qualified Pawl.Type.Duration as Duration` to imports if absent):

```haskell
      HU.testCase "CR 612 slotsOf and textChangeSlots find a ChangeText slot" $
        let slot = SlotName.MkSlotName (Text.pack "target")
            card =
              Card.Type.MkCard
                { Card.Type.name = Text.pack "T",
                  Card.Type.manaCost = Nothing,
                  Card.Type.typeLine = Card.Type.typeLine (Printing.card Card.lightningBoltPrinting),
                  Card.Type.power = Nothing,
                  Card.Type.toughness = Nothing,
                  Card.Type.keywords = Set.empty,
                  Card.Type.staticAbilities = [],
                  Card.Type.effects = [Effect.ChangeText slot],
                  Card.Type.targetSpecs = Map.empty
                }
         in do
              HU.assertEqual "slotsOf" (Set.singleton slot) (Resolve.slotsOf (Effect.ChangeText slot))
              HU.assertEqual "textChangeSlots" [slot] (Resolve.textChangeSlots card),
```

(`Duration.Indefinite` is exercised by the executor in Task 6; here we only need the opcode and its classifications to exist and be found. Add `qualified Pawl.Type.Printing as Printing` if absent.)

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "textChangeSlots find"'`
Expected: FAIL to compile — `Effect.ChangeText`, `Duration.Indefinite`, `Resolve.textChangeSlots` not in scope.

- [x] **Step 3: Add `Indefinite`**

In `source/library/Pawl/Type/Duration.hs`:

```haskell
data Duration
  = UntilEndOfTurn -- CR 514.2 (M3b)
  | Indefinite -- CR 611: "lasts indefinitely" (Magical Hack); cleanup never drops it
  deriving (Eq, Ord, Show)
```

- [x] **Step 4: Add the `ChangeText` opcode**

In `source/library/Pawl/Type/Effect.hs`, extend the type and the header comment:

```haskell
data Effect
  = DealDamage SlotName Quantity
  | ModifyTarget Duration Modification SlotName
  | -- CR 612: rewrite basic-land-type words in the target spell or permanent. The
    -- SlotName is the target slot; the two basic land types are read from the
    -- caster's binding (Object.chosenSubtypes) and baked into a stored
    -- ChangeSubtypeWord continuous effect. Resolve stores it; Projection applies it.
    ChangeText SlotName
  deriving (Eq, Ord, Show)
```

- [x] **Step 5: Extend `Resolve` — `slotsOf`, `textChangeSlots`, the `ChangeText` arm, and thread the binding**

In `source/library/Pawl/Resolve.hs`, add imports `import qualified Data.Maybe as Maybe`, `import qualified Pawl.Type.Duration as Duration`, `import qualified Pawl.Type.Modification as Modification`, `import Pawl.Type.Subtype (Subtype)`. Add the `slotsOf` arm:

```haskell
  Effect.ChangeText slot -> Set.singleton slot
```

Add `textChangeSlots` (cases on `Effect` — Resolve's charter — so `Cast` can ask this without casing on `Effect`):

```haskell
-- The target slots of ChangeText effects: the slots whose land-type pair Cast
-- must bind at cast (CR 612). Casing on Effect is Resolve's charter; Cast asks
-- this classification rather than casing on Effect itself.
textChangeSlots :: Card.Card -> [SlotName]
textChangeSlots card =
  let slotOf effect = case effect of
        Effect.ChangeText slot -> Just slot
        _ -> Nothing
   in Maybe.mapMaybe slotOf (Card.effects card)
```

Thread the binding through `resolveSpell` and `applyEffect`. Replace the non-fizzle branch of `resolveSpell` so it passes `Object.chosenSubtypes obj`:

```haskell
                  else bury (List.foldl' (applyEffect oid (Object.chosenSubtypes obj) legality chosen) gs (Card.effects card))
```

Change `applyEffect`'s signature and add the `ChangeText` arm (constructing — not casing on — a `Modification`; `recipientObject` pulls the id from either object-naming recipient):

```haskell
applyEffect :: ObjectId -> Map.Map SlotName (Subtype, Subtype) -> Map.Map SlotName Bool -> Map.Map SlotName Recipient -> GameState -> Effect -> GameState
applyEffect source bound legality chosen gs effect = case effect of
  ...
  Effect.ChangeText slot ->
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality, Map.lookup slot bound) of
      (Just recipient, True, Just (from, to)) ->
        case recipientObject recipient of
          Nothing -> gs
          Just target ->
            -- CR 611 / 612: an indefinite continuous effect over the one target
            -- (CR 611.2c fixed set). The (from, to) is the caster's binding, baked
            -- in here; Projection rewrites both the target's type line and, at
            -- gather, any static-ability words (Task 7). Resolve CONSTRUCTS the
            -- Modification but never cases on one.
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

-- The object a recipient names, if any (CR 612 targets a spell or permanent, not
-- a player).
recipientObject :: Recipient -> Maybe ObjectId
recipientObject r = case r of
  Recipient.ToObject oid -> Just oid
  Recipient.ToCreature oid -> Just oid
  Recipient.ToPlayer _ -> Nothing
```

Update the `DealDamage`/`ModifyTarget` arms' access to the renamed parameters if needed — the extra `bound` parameter is inserted before `legality`; both existing arms ignore `bound`, so only the signature and the `applyEffect ... ` call in `resolveSpell` change.

- [x] **Step 6: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — `slotsOf`/`textChangeSlots` find the `ChangeText` slot. The executor's observable behavior is driven end-to-end in Task 6.

- [x] **Step 7: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Add the ChangeText opcode: store an Indefinite ChangeSubtypeWord (CR 611/612)"
```

---

### Task 6: Magical Hack, cast end-to-end (the stated falsifier)

**Files:**
- Modify: `source/library/Pawl/Cast.hs` (`castSpell` prompts and stores the binding)
- Modify: `source/library/Pawl/Card.hs` (`magicalHackPrinting`, `allPrintings`)
- Test: `source/test-suite/Pawl/CastSpec.hs`

**Interfaces:**
- Consumes: `ChooseBasicLandTypes` (Task 4), `textChangeSlots`/`ChangeText` (Task 5), `SpellOrPermanentTarget` (Task 2).
- Produces: `Card.magicalHackPrinting` (in `allPrintings`); `Cast.castSpell` prompts `ChooseBasicLandTypes` for each `textChangeSlot` and stamps the pairs onto the new stack `Object.chosenSubtypes`.

- [x] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/CastSpec.hs` a dedicated answerer and an end-to-end test. Magical Hack costs `{U}`, so the caster needs blue mana — give alice an Island in play (Task 1 makes an Island tap blue). Add `qualified Pawl.Projection as Projection`, `qualified Pawl.Stack as Stack`, `qualified Pawl.Type.Subtype as Subtype`, `qualified Pawl.Type.Recipient as Recipient` if absent:

```haskell
-- Casts, targeting a permanent (lookupMin picks the lowest ToObject id) and
-- hacking Mountain -> Island.
hackAnswer :: Prompt.Prompt r -> r
hackAnswer p = case p of
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Island)
  _ -> S.identityAnswer p

magicalHackTests :: Tasty.TestTree
magicalHackTests =
  Tasty.testGroup
    "MagicalHack"
    [ HU.testCase "CR 612/305.6 a hacked basic Mountain taps for its new color" $
        -- alice: one Mountain to hack and one Island (blue for the {U}), plus a
        -- Magical Hack in hand. The Mountain is added FIRST so it has the lowest
        -- object id and identityAnswer's ChooseTargets (Set.lookupMin over the
        -- ToObject recipients) picks it, not the Island. Hack Mountain -> Island.
        let (mountainId, g0) = S.addCreature Card.mountainPrinting S.alice (Setup.emptyGame S.bothPlayers)
            (islandId, g1) = S.addCreature Card.islandPrinting S.alice g0
            (gs, hackId) = handInPlay Card.magicalHackPrinting g1
            cast = snd (Engine.runGamePure hackAnswer gs (Cast.castSpell S.alice hackId))
            resolved = Stack.resolveTop cast
         in do
              HU.assertBool "island untouched, still blue" (elem (ManaType.Colored Color.Blue) (Mana.manaTypesOf islandId resolved))
              HU.assertEqual "hacked Mountain projects Island" (Set.singleton Subtype.Island) (Projection.subtypesOf mountainId resolved)
              HU.assertBool "hacked Mountain taps blue" (elem (ManaType.Colored Color.Blue) (Mana.manaTypesOf mountainId resolved))
              HU.assertBool "hacked Mountain no longer taps red" (notElem (ManaType.Colored Color.Red) (Mana.manaTypesOf mountainId resolved)),
      HU.testCase "CR 601.2c Magical Hack with no legal target is uncastable" $
        let (gs, hackId) = handInPlay Card.magicalHackPrinting (Setup.emptyGame S.bothPlayers)
         in -- Empty battlefield and stack: SpellOrPermanentTarget has no legal
            -- recipient (and there is no mana either), so it is uncastable.
            HU.assertBool "no target -> uncastable" (not (Cast.castable S.alice hackId gs)),
    ]
```

Add a small helper near the other CastSpec fixtures (a permanent already in play plus a spell in hand, in a main phase with priority — reuse `S.handOne` style; put the spell in alice's hand over an existing board):

```haskell
-- Put one card of a printing into alice's hand over an existing board, in a main
-- phase with priority.
handInPlay :: Printing.Printing -> GameState.GameState -> (GameState.GameState, ObjectId.ObjectId)
handInPlay printing board =
  let (oid, g1) = Game.freshObjectId board
      (ts, g2) = Game.freshTimestamp g1
      obj =
        Object.MkObject
          { Object.owner = S.alice,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.targets = Map.empty,
            Object.chosenSubtypes = Map.empty,
            Object.timestamp = ts
          }
   in ( g2
          { GameState.objects = Map.insert oid obj (GameState.objects g2),
            GameState.hand = Map.insertWith (Seq.><) S.alice (Seq.singleton oid) (GameState.hand g2),
            GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        oid
      )
```

(Add `qualified Data.Sequence as Seq`, `qualified Pawl.Mana as Mana`, `qualified Pawl.Type.Color as Color`, `qualified Pawl.Type.ManaType as ManaType`, `qualified Pawl.Type.Timestamp as Timestamp` to `CastSpec` if absent, and wire `magicalHackTests` into `CastSpec.tests`.)

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "hacked basic Mountain"'`
Expected: FAIL to compile — `Card.magicalHackPrinting`, `Card.islandPrinting` not in scope.

- [x] **Step 3: Add the Island printing and Magical Hack**

In `source/library/Pawl/Card.hs`, add an `islandPrinting` (mirroring `mountainPrinting`, subtype `Island`) and `magicalHackPrinting`, then register both in `allPrintings`:

```haskell
-- The Island's blue mana ability is granted from its subtype by CR 305.6.
islandPrinting :: Printing.Printing
islandPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Island",
            Card.manaCost = Nothing,
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.singleton Supertype.Basic,
                  TypeLine.types = Set.singleton CardType.Land,
                  TypeLine.subtypes = Set.singleton Subtype.Island
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty,
            Card.staticAbilities = [],
            Card.effects = [],
            Card.targetSpecs = Map.empty
          }
    }

-- Magical Hack: {U}, Instant, "Change the text of target spell or permanent by
-- replacing all instances of one basic land type with another. (... This effect
-- lasts indefinitely.)" Scryfall-verified 2026-07-18. One ChangeText effect over a
-- SpellOrPermanentTarget slot; the two basic land types are the caster's binding
-- (Object.chosenSubtypes). The layer-3 canary (design.md §5): its effect AST word
-- is rewritten at resolution and gather, never special-cased.
magicalHackPrinting :: Printing.Printing
magicalHackPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Magical Hack",
            Card.manaCost =
              Just (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Blue)]),
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
            Card.effects = [Effect.ChangeText (SlotName.MkSlotName (Text.pack "target"))],
            Card.targetSpecs = Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.SpellOrPermanentTarget
          }
    }
```

Append both to `allPrintings` (after `opalescencePrinting`):

```haskell
    opalescencePrinting,
    islandPrinting,
    magicalHackPrinting
  ]
```

- [x] **Step 4: Prompt and store the binding in `Cast`**

In `source/library/Pawl/Cast.hs`, add `import qualified Pawl.Resolve as Resolve` and `import qualified Control.Monad as Monad` (if absent). In `castSpell`, after `chosen` is obtained and validated, prompt the land-type bindings and stamp both maps on the new stack object. Replace the tail of `castSpell` (from the `let decider`/`sets` block through the `State.put`) with:

```haskell
        let decider = Decide.deciderFor pid gs
            sets = Target.legalSets (Card.Type.targetSpecs card) gs
        chosen <-
          if Map.null sets
            then pure Map.empty
            else Trans.lift (Program.prompt (Prompt.ChooseTargets decider pid oid sets))
        let keysAgree = Map.keysSet chosen == Map.keysSet sets
            eachLegal = and (Map.intersectionWith Set.member chosen sets)
        if not (keysAgree && eachLegal)
          then pure ()
          else do
            -- CR 612 binding: choose the basic land types for each text-change
            -- slot. Always answerable (the five basics), so no castability gate.
            let textSlots = Resolve.textChangeSlots card
                ask slot = do
                  pair <- Trans.lift (Program.prompt (Prompt.ChooseBasicLandTypes decider pid oid slot))
                  pure (slot, pair)
            bound <- fmap Map.fromList (traverse ask textSlots)
            case Mana.payCost pid cost gs of
              Nothing -> pure ()
              Just paid -> do
                let moved = Game.changeZone oid Zone.Stack paid
                case GameState.stack moved of
                  [] -> State.put moved
                  top : _ ->
                    State.put
                      moved
                        { GameState.objects =
                            Map.adjust
                              (\o -> o {Object.targets = chosen, Object.chosenSubtypes = bound})
                              top
                              (GameState.objects moved)
                        }
```

(If `traverse` needs `import qualified Data.Traversable`, it is in `Prelude`; no import required. `Monad` may be unneeded — drop it if unused to stay warning-clean.)

- [x] **Step 5: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — the hacked Mountain taps blue and not red; Magical Hack with no target is uncastable. This is the first end-to-end exercise of the `ChangeText` executor (Task 5).

- [x] **Step 6: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Cast Magical Hack: bind land types and hack a Mountain's color (CR 612)"
```

---

## Phase 3 — the go/no-go: the ability-AST rewrite (Task 7)

### Task 7: `gather` rewrites a source's static-ability words (read-point 2)

**Files:**
- Modify: `source/library/Pawl/Projection.hs` (`textChangesAffecting`, `rewriteModification`, `gather`)
- Test: `source/test-suite/Pawl/ProjectionSpec.hs`

**Interfaces:**
- Consumes: `ChangeSubtypeWord` (Task 1), Blood Moon (already in the repo), the projection fold.
- Produces: `Projection.textChangesAffecting :: ObjectId -> GameState -> [(Subtype, Subtype)]`; `Projection.rewriteModification :: [(Subtype, Subtype)] -> Modification -> Modification`; `gather` rewrites each battlefield permanent's static-ability modifications by the text-changes affecting it, before folding them onto other objects.

- [ ] **Step 1: Write the failing tests (the go/no-go)**

Add to `source/test-suite/Pawl/ProjectionSpec.hs`. Blood Moon and a nonbasic land (Urborg is the repo's nonbasic land) on the battlefield, and a `ChangeSubtypeWord Mountain Island` stored effect targeting the real Blood Moon id (via `withEffect`, which locks to `TheseObjects`). "Order" here is the hack's stored-effect timestamp vs. Blood Moon's own object timestamp — the read-point-2 rewrite is applied at `gather` regardless, so both must give Island:

```haskell
      HU.testCase "CR 612 hacking Blood Moon Mountain->Island: nonbasic lands become Islands (hack newer)" $
        let base = Setup.emptyGame S.bothPlayers
            (nonbasicId, g1) = S.addCreature Card.urborgPrinting S.alice base
            (bloodMoonId, g2) = S.addCreature Card.bloodMoonPrinting S.alice g1
            gs = withEffect bloodMoonId (Timestamp.MkTimestamp 500) (Modification.ChangeSubtypeWord Subtype.Mountain Subtype.Island) g2
         in HU.assertEqual "nonbasic land is now Island" (Set.singleton Subtype.Island) (Projection.subtypesOf nonbasicId gs),
      HU.testCase "CR 612 hacking Blood Moon is order-independent (hack older)" $
        let base = Setup.emptyGame S.bothPlayers
            (nonbasicId, g1) = S.addCreature Card.urborgPrinting S.alice base
            (bloodMoonId, g2) = S.addCreature Card.bloodMoonPrinting S.alice g1
            -- Timestamp 1 is older than Blood Moon's own object timestamp; the
            -- outcome must not change.
            gs = withEffect bloodMoonId (Timestamp.MkTimestamp 1) (Modification.ChangeSubtypeWord Subtype.Mountain Subtype.Island) g2
         in HU.assertEqual "nonbasic land is Island, order-independent" (Set.singleton Subtype.Island) (Projection.subtypesOf nonbasicId gs),
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cabal test --test-options='-p "hacking Blood Moon"'`
Expected: FAIL — without the gather rewrite, Blood Moon's printed `SetLandSubtype Mountain` still applies, so the nonbasic land projects `{Mountain}`, not `{Island}`.

- [ ] **Step 3: Add `textChangesAffecting` and `rewriteModification`**

In `source/library/Pawl/Projection.hs`, add `import qualified Data.Maybe as Maybe` (if absent). Add both functions (both are legitimate `case`-on-`Modification`, Projection's charter):

```haskell
-- Every basic-land-type pair a ChangeSubtypeWord continuous effect imposes on
-- `oid` (CR 612). Stored resolution effects only (a text-change is stored by
-- Resolve's ChangeText, never a static ability at M3d); read against BASE
-- characteristics since ChangeSubtypeWord always uses a TheseObjects fixed set,
-- so no projection recursion is needed and nothing loops.
textChangesAffecting :: ObjectId -> GameState -> [(Subtype.Subtype, Subtype.Subtype)]
textChangesAffecting oid gs =
  let pairOf eff = case ContinuousEffect.modification eff of
        Modification.ChangeSubtypeWord from to ->
          if affects (ContinuousEffect.source eff) oid (ContinuousEffect.affected eff) (baseCharacteristics oid gs) gs
            then Just (from, to)
            else Nothing
        _ -> Nothing
   in Maybe.mapMaybe pairOf (GameState.continuousEffects gs)

-- Apply text-changes to a modification's basic-land-type words (CR 612.1/612.2):
-- SetLandSubtype/AddLandSubtype carry a land-type word; every other modification
-- has none to rewrite here. Projection's charter (it cases on Modification); it is
-- delegated to by Resolve.rewriteEffect for the inner modification of ModifyTarget.
rewriteModification :: [(Subtype.Subtype, Subtype.Subtype)] -> Modification -> Modification
rewriteModification pairs m =
  let swap from to s = if s == from then to else s
      apply1 acc (from, to) = case acc of
        Modification.SetLandSubtype s -> Modification.SetLandSubtype (swap from to s)
        Modification.AddLandSubtype s -> Modification.AddLandSubtype (swap from to s)
        _ -> acc
   in List.foldl' apply1 m pairs
```

- [ ] **Step 4: Rewrite static-ability words in `gather`**

In `source/library/Pawl/Projection.hs`, in `gather`, change `fromPermanent` so a live permanent's static-ability modifications are rewritten by the text-changes affecting it before being gathered (read-point 2). Replace the `then` branch's `map (fromStatic permId permObj) ...`:

```haskell
      fromPermanent permId = case Game.lookupObject permId gs of
        Nothing -> []
        Just permObj -> case Game.cardOf permId gs of
          Nothing -> []
          Just card ->
            if null setEffs || liveGiven setEffs Set.empty permId gs
              then
                let changes = textChangesAffecting permId gs
                    gatherOne sa =
                      let m = rewriteModification changes (StaticAbility.modification sa)
                       in MkGathered
                            { gSource = permId,
                              gAffected = StaticAbility.affected sa,
                              gLayer = layer m,
                              gTimestamp = Object.timestamp permObj,
                              gModification = m
                            }
                 in map gatherOne (Card.Type.staticAbilities card)
              else []
```

(The old `fromStatic` helper is now inlined as `gatherOne`; delete `fromStatic` if it becomes unused, or keep it and map `rewriteModification` over the modification first — either way stay warning-clean.)

- [ ] **Step 5: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — both orders give the nonbasic land `{Island}`. **This is the go/no-go: the effect AST is rewritten inside an ability, the part XMage cannot do.** If it cannot be made to pass, Phases 1–2 still stand — **stop and report**, do not weaken the assertion.

- [ ] **Step 6: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Rewrite a source's static-ability land words at gather (CR 612, hack Blood Moon)"
```

---

## Phase 4 — the spell-on-the-stack rewrite (Task 8)

### Task 8: The resolver reads projected effects (read-point 3) + the fixture

**Files:**
- Modify: `source/library/Pawl/Resolve.hs` (`rewriteEffect`, `effectsOf`, `resolveSpell` reads `effectsOf`, `ModifyTarget` accepts `ToObject`)
- Modify: `source/library/Pawl/Card.hs` (the `landformPrinting` fixture, `allPrintings`)
- Test: `source/test-suite/Pawl/ResolveSpec.hs`

**Interfaces:**
- Consumes: `textChangesAffecting`/`rewriteModification` (Task 7), `ChangeText`/`ToObject` (Tasks 5, 2).
- Produces: `Resolve.effectsOf :: ObjectId -> GameState -> [Effect]` (a resolving spell's text-changed effects); `Resolve.rewriteEffect :: [(Subtype, Subtype)] -> Effect -> Effect`; `resolveSpell` runs `effectsOf` instead of `Card.effects`; `ModifyTarget` applies to a `ToObject` land target; `Card.landformPrinting` (the labeled fixture).

- [ ] **Step 1: Write the failing tests**

Add to the `Resolve` group in `source/test-suite/Pawl/ResolveSpec.hs`. The positive case: the fixture "Landform" (target land becomes a Swamp) on the stack, a `ChangeSubtypeWord Swamp Mountain` already stored on it, resolves making the target land a Mountain (read-point 3). The negative CR 400.7 case: hacking Blood Moon *on the stack* is lost when it resolves into a new permanent.

```haskell
      HU.testCase "CR 612 resolve reads projected effects: a hacked 'becomes Swamp' resolves as Mountain" $
        -- The target is a Forest, so the assertion {Mountain} proves the rewrite:
        -- un-rewritten the effect is SetLandSubtype Swamp -> {Swamp}; rewritten
        -- (Swamp -> Mountain) it is SetLandSubtype Mountain -> {Mountain}.
        let base = S.landsInPlay Card.forestPrinting 1
            targetLand = case Game.zoneMembers Zone.Battlefield S.alice base of
              i : _ -> i
              [] -> ObjectId.MkObjectId 999
            slot = SlotName.MkSlotName (Text.pack "target")
            (landformId, g1) = Game.freshObjectId base
            landformObj =
              Object.MkObject
                { Object.owner = S.alice,
                  Object.source = Source.OfCard Card.landformPrinting,
                  Object.zone = Zone.Stack,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled,
                  Object.targets = Map.singleton slot (Recipient.ToObject targetLand),
                  Object.chosenSubtypes = Map.empty,
                  Object.timestamp = Timestamp.MkTimestamp 0
                }
            g2 =
              g1
                { GameState.objects = Map.insert landformId landformObj (GameState.objects g1),
                  GameState.stack = landformId : GameState.stack g1
                }
            -- A resolved Magical Hack already changed Swamp -> Mountain on the
            -- Landform spell (stored on the Landform's id).
            hacked = withEffect landformId (Timestamp.MkTimestamp 1) (Modification.ChangeSubtypeWord Subtype.Swamp Subtype.Mountain) g2
            after = Resolve.resolveSpell landformId hacked
         in do
              -- Landform's own subtype does not matter; its EFFECT was rewritten to
              -- SetLandSubtype Mountain, so the target land ends up a Mountain.
              HU.assertEqual "target land became Mountain, not Swamp" (Set.singleton Subtype.Mountain) (Projection.subtypesOf targetLand after),
      HU.testCase "CR 400.7 hacking Blood Moon on the stack is lost when it resolves" $
        let base = Setup.emptyGame S.bothPlayers
            (nonbasicId, g1) = S.addCreature Card.urborgPrinting S.alice base
            (bloodMoonSpellId, g2) = Game.freshObjectId g1
            bmObj =
              Object.MkObject
                { Object.owner = S.alice,
                  Object.source = Source.OfCard Card.bloodMoonPrinting,
                  Object.zone = Zone.Stack,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled,
                  Object.targets = Map.empty,
                  Object.chosenSubtypes = Map.empty,
                  Object.timestamp = Timestamp.MkTimestamp 0
                }
            g3 =
              g2
                { GameState.objects = Map.insert bloodMoonSpellId bmObj (GameState.objects g2),
                  GameState.stack = bloodMoonSpellId : GameState.stack g2
                }
            hacked = withEffect bloodMoonSpellId (Timestamp.MkTimestamp 1) (Modification.ChangeSubtypeWord Subtype.Mountain Subtype.Island) g3
            after = Stack.resolveTop hacked
            -- Blood Moon entered as a NEW object; the hack (locked to the spell id)
            -- no longer names it, so nonbasic lands are Mountains, not Islands.
         in HU.assertEqual "hack lost: nonbasic land is Mountain" (Set.singleton Subtype.Mountain) (Projection.subtypesOf nonbasicId after),
```

(`withEffect` is a `ProjectionSpec` helper; copy it into `ResolveSpec` or add a local equivalent. `Stack.resolveTop` is already imported in `ResolveSpec`.)

- [ ] **Step 2: Run tests to verify they fail**

Run: `cabal test --test-options='-p "resolve reads projected effects"'`
Expected: FAIL — `resolveSpell` reads `Card.effects` (the printed `SetLandSubtype Swamp`), so the target becomes a Swamp; and `Card.landformPrinting` is not in scope.

- [ ] **Step 3: Add the fixture printing**

In `source/library/Pawl/Card.hs`, add the labeled fixture and register it:

```haskell
-- LABELED SYNTHETIC FIXTURE (not a real card; spec §8). "Landform": {U}, Instant,
-- "Target land becomes a Swamp until end of turn." The M3d positive
-- spell-on-the-stack demonstrator: its ONE-SHOT effect carries a basic-land-type
-- word (SetLandSubtype Swamp), so hacking it on the stack changes what it does.
-- EXPIRES when a real non-Aura land-type spell (M4 Attach: Spreading Seas; or M4
-- Destroy: Boil/Flashfires) replaces it.
landformPrinting :: Printing.Printing
landformPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Landform",
            Card.manaCost =
              Just (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Blue)]),
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
            Card.effects = [Effect.ModifyTarget Duration.UntilEndOfTurn (Modification.SetLandSubtype Subtype.Swamp) (SlotName.MkSlotName (Text.pack "target"))],
            Card.targetSpecs = Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.LandTarget
          }
    }
```

Append `landformPrinting` to `allPrintings`.

- [ ] **Step 4: Make `ModifyTarget` accept a `ToObject` target**

In `source/library/Pawl/Resolve.hs`, the `ModifyTarget` arm matches only `ToCreature`; generalize it to any object-naming recipient via `recipientObject` (Task 5). Replace its `case`:

```haskell
  Effect.ModifyTarget duration modification slot ->
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> case recipientObject recipient of
        Nothing -> gs
        Just target ->
          -- CR 611.2c: the affected set is locked to this one object now.
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
```

- [ ] **Step 5: Read projected effects in `resolveSpell`**

In `source/library/Pawl/Resolve.hs`, add `rewriteEffect` and `effectsOf`, and make `resolveSpell` fold over `effectsOf oid gs` instead of `Card.effects card`:

```haskell
-- Rewrite basic-land-type words in an effect's AST (CR 612). Cases on Effect
-- (Resolve's charter); delegates the inner modification of ModifyTarget to
-- Projection.rewriteModification, so neither module touches the other's
-- constructors. DealDamage and ChangeText carry no rewritable land-type word.
rewriteEffect :: [(Subtype, Subtype)] -> Effect -> Effect
rewriteEffect pairs effect = case effect of
  Effect.ModifyTarget duration modification slot ->
    Effect.ModifyTarget duration (Projection.rewriteModification pairs modification) slot
  Effect.DealDamage _ _ -> effect
  Effect.ChangeText _ -> effect

-- A resolving spell's PROJECTED effects: its printed effects with every
-- text-change affecting it applied (CR 612). This is read-point 3 of the
-- rewritable AST -- the resolver honors a spell hacked on the stack.
effectsOf :: ObjectId -> GameState -> [Effect]
effectsOf oid gs = case Game.cardOf oid gs of
  Nothing -> []
  Just card -> map (rewriteEffect (Projection.textChangesAffecting oid gs)) (Card.effects card)
```

Change the non-fizzle branch of `resolveSpell` to use `effectsOf oid gs`:

```haskell
                  else bury (List.foldl' (applyEffect oid (Object.chosenSubtypes obj) legality chosen) gs (effectsOf oid gs))
```

(Add `import Pawl.Type.Subtype (Subtype)` if not already added in Task 5.)

- [ ] **Step 6: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — the hacked Landform resolves as Mountain (positive, read-point 3); the Blood-Moon-on-the-stack hack is lost on resolution (negative, CR 400.7).

- [ ] **Step 7: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Resolve reads projected effects: hack a spell on the stack (CR 612/400.7)"
```

---

## Final verification

- [ ] **Step 1: Clean build, all suites**

Run: `cabal clean && cabal build all --enable-tests --enable-benchmarks`
Expected: warning-free (a clean build surfaces warnings incremental builds hide).

- [ ] **Step 2: Full test suite (including the property suite over both matchups)**

Run: `cabal test`
Expected: PASS — every M2d/M3a/M3b/M3c property still holds; replay determinism now covers the projected type line, the projected effects, and the cast-time land-type binding. No new card enters a random game (Magical Hack and the fixture are blue, deterministic fixtures — the M3c posture), so `PropertySpec` needs no change.

- [ ] **Step 3: Lint and format**

Run: `git add -A && hooky fix && git add -A && hooky run`
Expected: all hooks pass; apply any HLint suggestions or justify the exception.

- [ ] **Step 4: Progress check**

Run: `grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-18-m3d-text-changing.md`
Expected: `0` — every step ticked.
