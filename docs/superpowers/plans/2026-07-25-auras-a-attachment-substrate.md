# Auras Phase (a): The Attachment Substrate — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make Auras real — cast Unholy Strength on a creature, have it enter the battlefield attached, modify the enchanted creature, and fall off to the graveyard by CR 704.5m when that creature dies.

**Architecture:** Attachment is base object state (`Object.attachedTo`), seeded as the Aura *enters* rather than set afterward. An Aura's target comes from a new `Card.enchant :: Maybe TargetSpec` merged into `Card.allTargetSpecs`/`Card.modesTargetSpecs` under the well-known slot `"enchant"`, so cast-time targeting and CR 608.2b re-validation both fall out — but `Target.fillableModes` and the D4 lint read `Mode.targetSpecs` *directly* and must each be taught separately. A new payload-free `Affected.Attached` lets a static ability name "the enchanted permanent".

**Tech Stack:** Haskell 2010 (GHC 9.14.1), `tasty`/`tasty-hunit`, the `StateT GameState (Program Prompt)` suspension monad, the hand-written JSON codec in `Pawl.Codec`, the on-demand card registry in `Pawl.Registry`.

## Global Constraints

- **Haskell 2010, no language extensions.** Only `GADTs`, `RankNTypes` and `NamedFieldPuns` are permitted, and this phase needs none of them.
- **Warning-clean under `+pedantic` (`-Werror`).** A new `Subtype`, `Affected` or `Card` field breaks every exhaustive match at once — that is the safety net, not a nuisance. Run `cabal build all --enable-tests --enable-benchmarks`; `cabal clean` first for a definitive check, because incremental builds hide warnings from unchanged modules. **Never run two builds concurrently** — `jobs: $ncpus` already saturates the machine.
- **No library module is added or deleted in this phase**, so `hooky fix` handles `pawl.cabal`; no direct `cabal-gild` run is needed. One *test-suite* module is added (Task 7), which **does** require adding it by hand to the test-suite `other-modules` list.
- **No partial functions**, no boolean blindness, `Mk` constructor prefix, derive at least `Eq` and `Show`, `Text` not `String`, no list comprehensions, `let` over `where`, `case` over point-free, `do` + record syntax to build records.
- **One type per module** under `Pawl.Type.<Name>`; qualified imports aliased to the last component; no explicit export lists; a module never imports its parents.
- **The two invariants outrank this plan:** the engine never cases on a card's *identity* (only classifications), and never makes a player's choice. `Card.isAura` is a **subtype** read off the type line — the same closed-half classification as the `Card.isPermanent` beside it (CR 205.3h), and not a violation. See spec §3.4.
- **TDD non-negotiable:** write each failing test, run it, watch it fail, then implement. Tick each `- [ ]` as you finish it.
- **Every rules claim cites the CR** and was checked against `docs/rules.txt`. Never write an expiry into a code comment — file an issue and cite `(#N)`.
- **Commit style:** commit directly to `main`, one small complete commit per task, with the two `CLAUDE.md` trailers.
- **After each task:** `git add <explicit paths>`, `hooky fix`, `git add <explicit paths>`, `hooky run`. Stage explicit paths rather than `-A`: concurrent sessions share this checkout.

**Spec:** `docs/superpowers/specs/2026-07-25-auras-design.md`. Phase (a) is spec §3.1–§3.7.

---

## File Structure

**Create:**
- `data/cards/unholy-strength.json` — the gate card. The registry loads cards on demand by name, so this file is the *entire* card-addition cost; `S.allPrintings` sweeps the directory and gives it whole-pool codec coverage automatically.
- `source/test-suite/Pawl/AuraSpec.hs` — the phase's gameplay-level suite (`tests :: TestTree`), wired into `Main.hs`.

**Modify (library):**
- `source/library/Pawl/Type/Subtype.hs` — `+Aura`.
- `source/library/Pawl/Type/Object.hs` — `+attachedTo`.
- `source/library/Pawl/Type/Card.hs` — `+enchant`.
- `source/library/Pawl/Type/Affected.hs` — `+Attached`.
- `source/library/Pawl/Mana.hs` — the exhaustive `subtypeMana` arm.
- `source/library/Pawl/Card.hs` — `isAura`, `enchantSlot`, `enchantSpecs`, and the two merging functions.
- `source/library/Pawl/Target.hs` — `fillableModes` takes an extra-slots map.
- `source/library/Pawl/Activate.hs`, `source/library/Pawl/Engine.hs` — three callers pass `Map.empty`.
- `source/library/Pawl/Event.hs` — `changeZoneAttaching`, and `attachedTo` in `mkObj`.
- `source/library/Pawl/Projection.hs` — the `Affected.Attached` arm.
- `source/library/Pawl/Resolve.hs` — the fizzle test extracted to `targetsAllIllegal`.
- `source/library/Pawl/Stack.hs` — the Aura resolution branch.
- `source/library/Pawl/Sba.hs` — CR 704.5m, and the stale one-pass comment.
- `source/library/Pawl/Codec.hs` — subtype pair, `Affected.Attached` pair, the `enchant` field.

**Modify (data):**
- `data/cards/opalescence.json` — the non-Aura qualifier (#114).

**Modify (test-suite):**
- `source/test-suite/Pawl/Support.hs` — `attach`.
- `source/test-suite/Pawl/CardSpec.hs` — two new lints.
- `source/test-suite/Pawl/ProjectionSpec.hs` — the #114 assertion and the wrong CR 305.2 citation.
- `source/test-suite/Pawl/Main.hs` — wire `AuraSpec`.
- `pawl.cabal` — `AuraSpec` in the test-suite `other-modules`.

---

## Task 1: `Subtype.Aura`

The subtype alone. One commit, because the constructor breaks three exhaustive matches at once.

**Files:**
- Modify: `source/library/Pawl/Type/Subtype.hs`, `source/library/Pawl/Mana.hs`, `source/library/Pawl/Codec.hs`
- Test: `source/test-suite/Pawl/ManaSpec.hs`

**Interfaces:**
- Produces: `Subtype.Aura`; `Codec.subtypeToJson Subtype.Aura == nullary "Aura"`.

**Append the constructor at the END of the declaration.** `Subtype`'s `Ord` is declaration order, and card JSON stores subtypes in `Ord`-canonical order — a whole-pool test re-parses each `data/cards/*.json` and compares against the codec's canonical emission. Inserting in the middle would reorder existing cards' lists and fail that test.

- [ ] **Step 0: File the phase's deferral issues, so later tasks cite real numbers**

Do this **first**, not at the end: Tasks 2, 3 and 7 each leave a code comment citing one of these, and a comment carrying a placeholder is exactly the drift the tracker exists to prevent. One `gh issue create` per spec §9 entry, each carrying status, rationale and expiry trigger, labelled `gap` or `elision` plus `expires:card-driven`:

1. CR 701.3 `Attach` / CR 303.4j — no opcode moves an Aura already on the battlefield.
2. CR 303.4f/303.4g/303.4i — an Aura entering by any means other than resolving as an Aura spell, including the controller's choice of what to enchant and the stays-in-its-zone rule when no legal object exists.
3. CR 702.5c — multiple `enchant` instances; `Card.enchant` is a `Maybe`, so a second one is unrepresentable. **Cited by Task 2.**
4. CR 702.5d "enchant player" — **a modelling limit, not a missing producer.** The body must say so plainly: `Object.attachedTo :: Maybe ObjectId` cannot name a player, and CR 303.4c's "the player it was attached to has left the game" clause has nowhere to be checked. **Cited by Tasks 3 and 7.**
5. CR 303.4d's chooser — "if a spell or ability would cause an Aura to become attached to more than one object or player, the Aura's controller chooses". No effect attaches, so there is nothing to choose between.
6. CR 303.4k — face-down permanents do not exist.
7. CR 704.5n/704.5p — Equipment, Fortification, attached creatures and battles becoming unattached.

Record the seven numbers before continuing; every `(#N)` below refers to one of them.

- [ ] **Step 1: Write the failing test**

In `source/test-suite/Pawl/ManaSpec.hs`, inside the existing `subtypeMana` group:

```haskell
HU.testCase "CR 205.3h: Aura is an enchantment type, so it has no CR 305.6 intrinsic mana" $
  HU.assertEqual "no mana" Nothing (Mana.subtypeMana Subtype.Aura),
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL to compile — `Data constructor not in scope: Subtype.Aura`.

- [ ] **Step 3: Add the constructor**

At the end of `data Subtype` in `source/library/Pawl/Type/Subtype.hs`, after `Horse`:

```haskell
  | -- CR 205.3h: an ENCHANTMENT type. Appended rather than grouped with the other
    -- enchantment types, of which this pool has none, so every existing card's
    -- Ord-canonical subtype list is unchanged (card JSON stores subtypes in
    -- declaration order, and a whole-pool test compares against it).
    Aura
```

- [ ] **Step 4: Add the three exhaustive arms**

`source/library/Pawl/Mana.hs`, at the end of `subtypeMana`'s case:

```haskell
  -- CR 205.3h: Aura is an enchantment type. CR 305.6's intrinsic mana ability is
  -- a property of BASIC LAND types only, so this is Nothing for the same reason
  -- every creature type above is.
  Subtype.Aura -> Nothing
```

`source/library/Pawl/Codec.hs`, at the end of `subtypeToJson`'s case:

```haskell
  Subtype.Aura -> "Aura"
```

and in `jsonToSubtype`'s `decodeNullary` pair list:

```haskell
    (Text.pack "Aura", Subtype.Aura),
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS, warning-free.

- [ ] **Step 6: Commit**

```bash
git add source/library/Pawl/Type/Subtype.hs source/library/Pawl/Mana.hs source/library/Pawl/Codec.hs source/test-suite/Pawl/ManaSpec.hs
hooky fix
git add source/library/Pawl/Type/Subtype.hs source/library/Pawl/Mana.hs source/library/Pawl/Codec.hs source/test-suite/Pawl/ManaSpec.hs
hooky run
git commit -m "feat(subtype): add Aura, the first enchantment type (CR 205.3h)"
```

---

## Task 2: `Card.enchant` and the `Pawl.Card` classifications

The carrier and its readers. No behaviour yet — a card can *declare* an enchant spec and nothing consults it.

**Files:**
- Modify: `source/library/Pawl/Type/Card.hs`, `source/library/Pawl/Card.hs`, `source/library/Pawl/Codec.hs`
- Test: `source/test-suite/Pawl/CardSpec.hs`

**Interfaces:**
- Produces: `Card.Type.enchant :: Card -> Maybe TargetSpec`; `Pawl.Card.isAura :: Card -> Bool`; `Pawl.Card.enchantSlot :: SlotName`; `Pawl.Card.enchantSpecs :: Card -> Map SlotName TargetSpec`. Consumed by Tasks 4 and 6.

- [ ] **Step 1: Write the failing test**

In `source/test-suite/Pawl/CardSpec.hs`, in the lint group:

```haskell
HU.testCase "a card with no enchant ability declares no enchant slot" $ do
  piker <- Registry.printing registry "Goblin Piker"
  let card = Printing.card piker
  HU.assertEqual "no enchant spec" Nothing (Card.Type.enchant card)
  HU.assertBool "not an Aura" (not (Card.isAura card))
  HU.assertEqual "no enchant slot" Map.empty (Card.enchantSpecs card),
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL to compile — `enchant`, `isAura` and `enchantSpecs` are not in scope.

- [ ] **Step 3: Add the field**

In `source/library/Pawl/Type/Card.hs`, add to the import list `import Pawl.Type.TargetSpec (TargetSpec)`, and add the field after `castingPermissions`:

```haskell
    -- CR 702.5a: this card's `enchant` ability -- "Enchant [object or player]"
    -- -- which "restricts what an Aura spell can target and what an Aura can
    -- enchant". Nothing for every card that is not an Aura; the CardSpec lint
    -- family holds the biconditional (Aura iff enchant) in both directions.
    --
    -- A TargetSpec, not a Filter, because CR 702.5d's enchant-player Auras need
    -- the Pool axis and TargetSpec already is {pool, filter}.
    --
    -- SINGULAR: CR 702.5c's "multiple instances of enchant, all of them apply"
    -- is unrepresentable, and no card in this pool prints two (#N).
    enchant :: Maybe TargetSpec,
```

`#N` is **issue 3** from Task 1 Step 0 (CR 702.5c). Write the real number; never commit a literal `#N`, and never write the expiry into the comment.

- [ ] **Step 4: Add the codec arms**

In `source/library/Pawl/Codec.hs`, in `cardToJson`, alongside the `characteristicPT` block (the exact precedent — a `Maybe` field omitted when absent):

```haskell
        <> ( case CardT.enchant c of
               Nothing -> []
               Just spec -> [(Text.pack "enchant", targetSpecToJson spec)]
           )
```

In `jsonToCard`, beside the other `getOpt` reads:

```haskell
  enchant <- maybeFrom jsonToTargetSpec (getOpt (Text.pack "enchant") ps)
```

and in the `MkCard` record: `CardT.enchant = enchant,`.

**No existing `data/cards/*.json` file changes** — absent decodes to `Nothing`.

- [ ] **Step 5: Add the classifications**

In `source/library/Pawl/Card.hs` (add `import qualified Pawl.Type.SlotName as SlotName`, `import qualified Data.Text as Text`, `import qualified Pawl.Type.Subtype as Subtype`):

```haskell
-- CR 205.3h / 303.4: is this card an Aura? A SUBTYPE read off the printed type
-- line, exactly the kind of closed-half classification isPermanent is -- NOT a
-- case on the card's identity. Pawl.Stack dispatches on it (CR 303.4's "an Aura
-- enters the battlefield attached"), which is the one place the difference
-- between an Aura and any other enchantment is a rules difference.
isAura :: Card.Card -> Bool
isAura c = Set.member Subtype.Aura (TypeLine.subtypes (Card.typeLine c))

-- CR 303.4a: the slot an Aura spell's required target is bound under. A genuine
-- target, so it lives in the ordinary target namespace and is NOT one of
-- Pawl.Binding's reserved names -- those exist precisely because they are not
-- targets. The CardSpec lint holds that no mode declares this name, which is
-- what makes the merge below collision-free.
enchantSlot :: SlotName
enchantSlot = SlotName.MkSlotName (Text.pack "enchant")

-- CR 303.4a / 702.5a: the enchant ability's target spec as a one-entry slot map,
-- empty for every non-Aura. Merged into the two functions below, and passed to
-- Target.fillableModes by Pawl.Cast so castability accounts for it.
enchantSpecs :: Card.Card -> Map SlotName TargetSpec
enchantSpecs card = case Card.enchant card of
  Nothing -> Map.empty
  Just spec -> Map.singleton enchantSlot spec
```

- [ ] **Step 6: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS, warning-free. Every `MkCard` record literal in the test suite fails to compile until it gains `Card.Type.enchant = Nothing`; fix each one the compiler names.

- [ ] **Step 7: Commit**

```bash
git add source/library/Pawl/Type/Card.hs source/library/Pawl/Card.hs source/library/Pawl/Codec.hs source/test-suite/
hooky fix
git add source/library/Pawl/Type/Card.hs source/library/Pawl/Card.hs source/library/Pawl/Codec.hs source/test-suite/
hooky run
git commit -m "feat(card): carry CR 702.5a's enchant ability on Card"
```

---

## Task 3: `Object.attachedTo`, seeded at entry

Base state plus the entry seam. CR 303.4 says an Aura *enters* attached, so the seed rides `changeZone` rather than being written after it returns (spec §3.4, step 3).

**This task has no Aura in it.** The field, its reset, and the seed are properties of *objects*, not of Auras — CR 400.7 resets attachment for the same reason it resets damage and counters. The gate card arrives in Task 4, which is what makes this task's fixture two ordinary permanents rather than a real Aura.

**Files:**
- Modify: `source/library/Pawl/Type/Object.hs`, `source/library/Pawl/Event.hs`, `source/library/Pawl/Setup.hs`, `source/library/Pawl/Monarch.hs`, `source/library/Pawl/Activate.hs`, `source/library/Pawl/Engine.hs`, `source/library/Pawl/Resolve.hs`
- Modify: `source/test-suite/Pawl/Support.hs`
- Test: `source/test-suite/Pawl/EventSpec.hs`

**Interfaces:**
- Produces: `Object.attachedTo :: Maybe ObjectId`; `Event.changeZoneAttaching :: ObjectId -> Zone -> Maybe ObjectId -> Game (Maybe ObjectId)`; `S.attach :: ObjectId -> ObjectId -> GameState -> GameState`. Consumed by Tasks 4, 7 and 8.

- [ ] **Step 1: Write the failing test**

In `source/test-suite/Pawl/EventSpec.hs`. Two Goblin Pikers, deliberately — this asserts that CR 400.7 clears the FIELD, which is true of every object regardless of type, and it must not wait on the Aura printing Task 4 adds:

```haskell
HU.testCase "CR 400.7: a zone change forgets attachment" $ do
  plains <- Registry.printing registry "Plains"
  piker <- Registry.printing registry "Goblin Piker"
  let base = S.landsInPlay plains 1
      (host, withHost) = S.addCreature piker S.bob base
      (rider, withRider) = S.addCreature piker S.alice withHost
      attached = S.attach rider host withRider
      bounced = S.runPure S.identityAnswer attached (Event.changeZone rider Zone.Hand)
      moved = filter (\o -> Object.zone o == Zone.Hand) (Map.elems (GameState.objects bounced))
  HU.assertEqual "attached before the move" (Just (Just host)) (fmap Object.attachedTo (Game.lookupObject rider attached))
  HU.assertEqual "one card in hand" 1 (length moved)
  HU.assertEqual "the new incarnation is unattached" [Nothing] (fmap Object.attachedTo moved),
```

Note the `Just (Just host)` — `Game.lookupObject` returns `Maybe Object` and `attachedTo` is itself a `Maybe`, so `fmap` produces a nested `Maybe`. Match whatever `Game.lookupObject`'s actual signature gives you.

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL to compile — `Object.attachedTo` and `S.attach` are not in scope.

- [ ] **Step 3: Add the field**

In `source/library/Pawl/Type/Object.hs`, add `import Pawl.Type.ObjectId (ObjectId)` and the field after `counters`:

```haskell
    -- CR 303.4b: the object this permanent is attached to -- what CR 303.4b calls
    -- "enchanted". Nothing for every permanent that is not an attached Aura.
    --
    -- BASE state, not projected: attachment is a fact about the object, and no CR
    -- 613 layer reads or writes it. Per-incarnation, like damage and counters:
    -- changeZone resets it, because CR 400.7 makes the moved object a new one with
    -- no memory of what it enchanted.
    --
    -- One direction only. "What is attached to me" is derived by scanning the
    -- battlefield, the posture Projection.controls already takes toward control,
    -- so there is no reverse index to keep consistent across zone changes.
    --
    -- Maybe ObjectId, not a Recipient: CR 702.5d's enchant-player Auras cannot be
    -- expressed and need this widened (#N).
    attachedTo :: Maybe ObjectId,
```

`#N` is **issue 4** from Task 1 Step 0 (CR 702.5d). Write the real number; never commit a literal `#N`, and never write the expiry into the comment.

Add `Object.attachedTo = Nothing` to the six `Object.MkObject` literals the compiler names (`Setup.hs:102`, `Monarch.hs:137`, `Activate.hs:79`, `Engine.hs:419`, `Resolve.hs:792`, `Event.hs:320`).

- [ ] **Step 4: Seed it through the zone change**

In `source/library/Pawl/Event.hs`, rename the body and add the seeded entry point:

```haskell
changeZoneReturning :: ObjectId -> Zone -> Game (Maybe ObjectId)
changeZoneReturning oid requestedDest = changeZoneAttaching oid requestedDest Nothing

-- changeZoneReturning with an attachment seed. CR 303.4: "An Aura ENTERS the
-- battlefield attached to an object or player" -- attachment is a property of
-- entering, not a step after it. The entry replacement loop (CR 614.1c) and the
-- Moved event both run before this function returns, so an Aura attached
-- afterward would be unattached during both. No card in this pool can observe
-- the difference today; the seed buys the ordering rather than a passing test.
changeZoneAttaching :: ObjectId -> Zone -> Maybe ObjectId -> Game (Maybe ObjectId)
changeZoneAttaching oid requestedDest seed = do
```

— the existing body follows unchanged except for `mkObj`, which gains the field:

```haskell
              mkObj ts = obj {Object.zone = dest, Object.tapped = TapState.Untapped, Object.damage = 0, Object.sickness = Sickness.Sick, Object.bindings = Map.empty, Object.counters = Map.empty, Object.attachedTo = seed, Object.timestamp = ts}
```

`changeZone` is unchanged (`Monad.void (changeZoneReturning oid requestedDest)`), so **every existing call site is untouched**.

- [ ] **Step 5: Add the test fixture**

In `source/test-suite/Pawl/Support.hs`:

```haskell
-- CR 303.4b: attach `rider` to `host` directly, without casting. A STATE fixture
-- (the shape addCreature and withEffect already have), not a synthetic card --
-- every printing a caller passes is real. Type-agnostic on purpose: CR 400.7's
-- reset is a property of the field, so the CR 400.7 test does not need an Aura.
attach :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
attach rider host gs =
  let set obj = obj {Object.attachedTo = Just host}
   in gs {GameState.objects = Map.adjust set rider (GameState.objects gs)}
```

- [ ] **Step 6: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS, warning-free.

- [ ] **Step 7: Commit**

```bash
git add source/library/Pawl/ source/test-suite/Pawl/
hooky fix
git add source/library/Pawl/ source/test-suite/Pawl/
hooky run
git commit -m "feat(object): attachment is base state, seeded as the object enters (CR 303.4)"
```

---

## Task 4: Unholy Strength and `Affected.Attached`

The gate card, the affected-set constructor its static ability needs, and the two lints that keep the pool honest. **One task, because the card and the constructor are mutually dependent** — the card's JSON references `Attached`, and `Attached`'s only honest test is a real Aura modifying a real creature. Splitting them means one commit with a knowingly-red test; merging them keeps the tree green and the commit is still one idea: *an Aura's static ability names the permanent it is attached to.*

**Files:**
- Create: `data/cards/unholy-strength.json`
- Modify: `source/library/Pawl/Type/Affected.hs`, `source/library/Pawl/Projection.hs`, `source/library/Pawl/Codec.hs`
- Test: `source/test-suite/Pawl/CardSpec.hs`, `source/test-suite/Pawl/PowerToughnessSpec.hs`

**Interfaces:**
- Consumes: `Subtype.Aura` (Task 1); `Card.Type.enchant`, `Card.isAura`, `Card.enchantSlot` (Task 2); `Object.attachedTo`, `S.attach` (Task 3).
- Produces: `Affected.Attached`; the printing `"Unholy Strength"`, loadable via `Registry.printing registry "Unholy Strength"`. Consumed by Tasks 5, 6, 7, 8.

Scryfall-verified 2026-07-25: `Unholy Strength` — `{B}` — `Enchantment — Aura` — "Enchant creature / Enchanted creature gets +2/+1."

Adding the card costs exactly one file. The registry loads cards on demand by name, and `S.allPrintings` sweeps `data/cards/`, so the new file gets whole-pool codec round-trip coverage automatically — there is no list to append to.

- [ ] **Step 1: Write the failing tests**

In `source/test-suite/Pawl/CardSpec.hs`:

```haskell
HU.testCase "Unholy Strength is a {B} Aura enchanting a creature for +2/+1" $ do
  p <- Registry.printing registry "Unholy Strength"
  let card = Printing.card p
      black = ManaSymbol.OfType (ManaType.Colored Color.Black)
  HU.assertEqual "cost" (Just (ManaCost.MkManaCost [black])) (Card.Type.manaCost card)
  HU.assertEqual "types" (Set.singleton CardType.Enchantment) (TypeLine.types (Card.Type.typeLine card))
  HU.assertEqual "subtypes" (Set.singleton Subtype.Aura) (TypeLine.subtypes (Card.Type.typeLine card))
  HU.assertBool "is an Aura" (Card.isAura card)
  -- CR 702.5a: "Enchant creature" -- the whole creature pool, unnarrowed.
  HU.assertEqual "enchant creature" (Just (TargetSpec.MkTargetSpec Pool.Creatures Nothing)) (Card.Type.enchant card)
  -- CR 303.4m: "enchanted creature gets +2/+1" -- layer 7c on whatever it is
  -- attached to.
  HU.assertEqual
    "one +2/+1 static ability on the enchanted permanent"
    [StaticAbility.MkStaticAbility Affected.Attached (Modification.ModifyPowerToughness (Quantity.Type.Literal 2) (Quantity.Type.Literal 1))]
    (Card.Type.staticAbilities card)
  -- CR 303.4: an Aura spell has no spell effects; it enters attached.
  HU.assertEqual "no spell effects" [] (Card.allEffects card),
```

Plus the two lints, in the lint group:

```haskell
-- CR 303.4 / 702.5a: the biconditional. An Aura without enchant has no legal
-- target and could never be cast; a non-Aura with enchant declares a restriction
-- nothing reads. The D4 lint cannot see either, because it walks
-- Mode.targetSpecs and the enchant slot is not there (#184's shape).
HU.testCase "a card is an Aura iff it declares an enchant ability" $ do
  ps <- S.allPrintings registry
  let offends c = Card.isAura c /= Maybe.isJust (Card.Type.enchant c)
      offenders = filter (offends . Printing.card) ps
  HU.assertEqual "Aura iff enchant" [] (fmap (Card.Type.name . Printing.card) offenders),
-- Pawl.Card.allTargetSpecs binds the enchant spec under this name (Task 6), so a
-- mode declaring it would be silently shadowed.
HU.testCase "no mode declares a slot named enchant" $ do
  ps <- S.allPrintings registry
  let offends c = any (Map.member Card.enchantSlot . Mode.targetSpecs) (Modal.modes (Card.Type.spell c))
      offenders = filter (offends . Printing.card) ps
  HU.assertEqual "the enchant slot is never hand-declared" [] (fmap (Card.Type.name . Printing.card) offenders),
```

And in `source/test-suite/Pawl/PowerToughnessSpec.hs`, the behavioural test — this is the one that proves `Attached` works:

```haskell
HU.testCase "CR 303.4m: an attached Unholy Strength gives the enchanted creature +2/+1" $ do
  piker <- Registry.printing registry "Goblin Piker"
  unholyStrength <- Registry.printing registry "Unholy Strength"
  let base = Setup.emptyGame S.bothPlayers
      -- Goblin Piker is a 2/1.
      (creature, withCreature) = S.addCreature piker S.bob base
      (aura, withAura) = S.addCreature unholyStrength S.alice withCreature
      attached = S.attach aura creature withAura
  HU.assertEqual "unattached, the ability names nothing" (Just (2, 1)) (S.powerToughnessOf creature withAura)
  HU.assertEqual "attached, +2/+1" (Just (4, 2)) (S.powerToughnessOf creature attached),
```

If `S.powerToughnessOf` does not exist, use whatever accessor the neighbouring cases in `PowerToughnessSpec.hs` already use to read projected P/T, and match its exact return type.

While you are in `PowerToughnessSpec.hs`, note the comment at line 120 recording that the Aura family "is blocked on Attach". Half of that is now false — the Darksteel Mutation family needs layer-4 card-type *replacement*, not `Attach`. Correct the comment to say what is actually still missing; do not delete the whole note.

- [ ] **Step 2: Run tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL to compile — `Affected.Attached` is not in scope.

- [ ] **Step 3: Add the constructor**

In `source/library/Pawl/Type/Affected.hs`:

```haskell
  | -- CR 303.4m: the object this ability's SOURCE is attached to -- "enchanted
    -- creature". A THIRD kind of affected set: TheseObjects is fixed at
    -- resolution (CR 611.2c) and Matching is a predicate re-derived per
    -- candidate, while this is re-derived from the SOURCE's own state.
    --
    -- The set is {o} when the source is attached to o, and EMPTY when it is
    -- unattached -- an Aura in the graveyard, or one the CR 704.5m sweep has not
    -- reached yet. Payload-free: CR 303.4m defines it for any permanent, "even
    -- if the permanent with the ability isn't an Aura", so there is nothing to
    -- parameterize.
    Attached
```

Extend the module's own header comment to describe three kinds rather than two.

- [ ] **Step 4: Add the projection arm**

In `source/library/Pawl/Projection.hs`, in `affects`:

```haskell
  -- CR 303.4m: read the SOURCE's attachment, not the candidate's. An unattached
  -- source names nothing, so the set is empty and the effect applies to no one.
  Affected.Attached -> case Game.lookupObject source gs of
    Nothing -> False
    Just src -> Object.attachedTo src == Just oid
```

Then fix every other exhaustive `Affected` match the compiler names. The one at `Projection.hs:865` (inside `controllerOf`) keeps its existing `_ -> False` default — phase (b) is what teaches control to read static abilities, and adding `Attached` there now would be dead code.

- [ ] **Step 5: Add the codec arms**

In `source/library/Pawl/Codec.hs`:

```haskell
  Affected.Attached -> Json.tagged (Text.pack "Attached") Nothing
```

and in `jsonToAffected`:

```haskell
    "Attached" -> pure Affected.Attached
```

Match the exact shape the neighbouring nullary tags use — check how another payload-free tagged constructor decodes in this module before writing the arm.

- [ ] **Step 6: Write the card**

`data/cards/unholy-strength.json`. Field order is alphabetical, matching every other file in the directory:

```json
{
  "activatedAbilities": [],
  "castingPermissions": [],
  "enchant": {
    "pool": {
      "type": "Creatures"
    },
    "restriction": null
  },
  "keywords": [],
  "manaCost": [
    {
      "type": "OfType",
      "value": {
        "type": "Colored",
        "value": {
          "type": "Black"
        }
      }
    }
  ],
  "name": "Unholy Strength",
  "power": null,
  "replacementEffects": [],
  "spell": {
    "modes": [
      {
        "effects": [],
        "targetSpecs": []
      }
    ],
    "selection": {
      "type": "ChooseExactly",
      "value": 1
    }
  },
  "staticAbilities": [
    {
      "affected": {
        "type": "Attached"
      },
      "modification": {
        "type": "ModifyPowerToughness",
        "value": [
          {
            "type": "Literal",
            "value": 2
          },
          {
            "type": "Literal",
            "value": 1
          }
        ]
      }
    }
  ],
  "toughness": null,
  "triggeredAbilities": [],
  "typeLine": {
    "subtypes": [
      {
        "type": "Aura"
      }
    ],
    "supertypes": [],
    "types": [
      {
        "type": "Enchantment"
      }
    ]
  }
}
```

Check `targetSpecToJson` and the `Affected` encoder in `Pawl.Codec` for the exact key names, and correct the `enchant` and `affected` objects above to match what the encoders actually emit — the whole-pool canonical-emission test will tell you immediately if they do not.

- [ ] **Step 7: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS, warning-free.

- [ ] **Step 8: Commit**

```bash
git add data/cards/unholy-strength.json source/library/Pawl/ source/test-suite/Pawl/
hooky fix
git add data/cards/unholy-strength.json source/library/Pawl/ source/test-suite/Pawl/
hooky run
git commit -m "feat(cards): add Unholy Strength, and Affected.Attached for the enchanted permanent (CR 303.4m)"
```

---

## Task 5: Opalescence's non-Aura qualifier (#114)

An Aura printing now exists, so the qualifier is testable for the first time.

**Files:**
- Modify: `data/cards/opalescence.json`
- Modify: `source/test-suite/Pawl/ProjectionSpec.hs`

**Interfaces:**
- Consumes: `Subtype.Aura` (Task 1), the Unholy Strength printing (Task 4).

Opalescence reads "Each other non-Aura enchantment is a creature in addition to its other types and has base power and toughness each equal to its mana value." Both of its static abilities carry the same affected set; **both** get the new conjunct.

- [ ] **Step 1: Write the failing test**

In `source/test-suite/Pawl/ProjectionSpec.hs`, in the Opalescence group:

```haskell
-- Opalescence's own text says "each other NON-AURA enchantment". Card text, not
-- a rule -- CR 305.2 is the one-land-per-turn rule and does not bear on this.
HU.testCase "Opalescence does not animate an Aura" $ do
  opalescence <- Registry.printing registry "Opalescence"
  unholyStrength <- Registry.printing registry "Unholy Strength"
  restInPeace <- Registry.printing registry "Rest in Peace"
  let base = Setup.emptyGame S.bothPlayers
      (_, withOpal) = S.addCreature opalescence S.alice base
      (auraId, withAura) = S.addCreature unholyStrength S.alice withOpal
      (ripId, gs) = S.addCreature restInPeace S.alice withAura
  HU.assertBool "the Aura stays a non-creature" (not (Projection.isCreatureOf auraId gs))
  HU.assertBool "a non-Aura enchantment IS animated" (Projection.isCreatureOf ripId gs),
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test`
Expected: FAIL — "the Aura stays a non-creature" is False; Opalescence animates it.

- [ ] **Step 3: Add the qualifier**

In `data/cards/opalescence.json`, in **both** static abilities' `affected.value.value` array, append a third conjunct:

```json
     {
      "type": "Not",
      "value": {
       "type": "HasSubtype",
       "value": {
        "type": "Aura"
       }
      }
     }
```

so each `And` reads `[HasCardType Enchantment, Not IsSource, Not (HasSubtype Aura)]`.

- [ ] **Step 4: Run tests to verify they pass**

Run: `cabal test`
Expected: PASS.

- [ ] **Step 5: Retire the citations**

Delete the `(#114)` comment at `source/test-suite/Pawl/ProjectionSpec.hs:455` and the sentence it guards. While the file is open, fix the neighbouring comment that attributes "each OTHER enchantment" to *CR 305.2* — 305.2 is the one-land-per-turn rule; Opalescence's "each other" is card text with no rule number.

- [ ] **Step 6: Commit and close the issue**

```bash
git add data/cards/opalescence.json source/test-suite/Pawl/ProjectionSpec.hs
hooky fix
git add data/cards/opalescence.json source/test-suite/Pawl/ProjectionSpec.hs
hooky run
git commit -m "fix(cards): enforce Opalescence's non-Aura qualifier (#114)"
gh issue close 114 --comment "Subtype.Aura now exists, so the qualifier is a Not (HasSubtype Aura) conjunct in both of Opalescence's affected sets, tested against Unholy Strength."
```

---

## Task 6: The enchant slot reaches targeting and castability

The merge, plus `Target.fillableModes`'s extra-slots parameter. Without the second half, an Aura with no legal creature is castable and then countered on resolution — which CR 601.2c says can never happen (spec §3.3, item 1).

**Files:**
- Modify: `source/library/Pawl/Card.hs`, `source/library/Pawl/Target.hs`, `source/library/Pawl/Cast.hs`, `source/library/Pawl/Activate.hs`, `source/library/Pawl/Engine.hs`
- Test: `source/test-suite/Pawl/CastSpec.hs`

**Interfaces:**
- Consumes: `Card.enchantSpecs`, `Card.enchantSlot` (Task 2); the Unholy Strength printing (Task 4).
- Produces: `Target.fillableModes :: ObjectId -> Map SlotName TargetSpec -> Modal.Modal Card -> GameState -> Set ModeIndex` (note the **new second parameter**).

- [ ] **Step 1: Write the failing test**

In `source/test-suite/Pawl/CastSpec.hs`:

```haskell
HU.testCase "CR 303.4a: an Aura spell targets, so it prompts for the creature it enchants" $ do
  swamp <- Registry.printing registry "Swamp"
  piker <- Registry.printing registry "Goblin Piker"
  unholyStrength <- Registry.printing registry "Unholy Strength"
  let base = S.landsInPlay swamp 1
      (creature, withCreature) = S.addCreature piker S.bob base
      (gs, spellId) = S.handOne unholyStrength withCreature
      specs = Card.modesTargetSpecs (Set.singleton (ModeIndex.MkModeIndex 0)) (Printing.card unholyStrength)
  HU.assertEqual "one slot, the enchant slot" (Map.keysSet specs) (Set.singleton Card.enchantSlot)
  HU.assertEqual
    "its legal set is the one creature"
    (Map.singleton Card.enchantSlot (Set.singleton (Recipient.ToCreature creature)))
    (Target.legalSets spellId specs gs),
-- CR 601.2c: a spell whose required target has no legal choice cannot be cast at
-- all. Reading only Mode.targetSpecs would call this castable and let it be
-- countered on resolution instead.
HU.testCase "CR 601.2c: an Aura with no creature on the battlefield is not castable" $ do
  swamp <- Registry.printing registry "Swamp"
  unholyStrength <- Registry.printing registry "Unholy Strength"
  let base = S.landsInPlay swamp 1
      (gs, spellId) = S.handOne unholyStrength base
  HU.assertBool "not castable with an empty board" (not (Cast.canCast S.alice spellId gs)),
```

Use whatever the castability predicate at `Cast.hs:70` is actually named; if it is not exported as `canCast`, assert against the expression that line uses.

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test`
Expected: the first case FAILS (`specs` is empty — the merge does not exist); the second FAILS (the Aura is reported castable).

- [ ] **Step 3: Merge the enchant slot into the two spec functions**

In `source/library/Pawl/Card.hs`:

```haskell
-- CR 303.4a: an Aura spell's target is defined by its enchant ability, not by a
-- mode -- an Aura's spell payload is a single empty mode. Merging here is what
-- puts the enchant slot in front of Cast's prompt (Cast.hs) and Resolve's CR
-- 608.2b re-validation (Resolve.hs) without either learning what an Aura is.
--
-- Union is left-biased, and the CardSpec lint holds that no mode declares this
-- slot name, so the bias is never exercised.
allTargetSpecs :: Card.Card -> Map SlotName TargetSpec
allTargetSpecs card = Map.union (enchantSpecs card) (Modal.allTargetSpecs (Card.spell card))

modesTargetSpecs :: Set.Set ModeIndex.ModeIndex -> Card.Card -> Map SlotName TargetSpec
modesTargetSpecs chosen card = Map.union (enchantSpecs card) (Modal.modesTargetSpecs chosen (Card.spell card))
```

Leave `modeTargetSpecs` (singular, by index) alone — it answers "what does mode *i* declare", which the enchant slot is not part of.

- [ ] **Step 4: Teach `fillableModes` the extra slots**

In `source/library/Pawl/Target.hs`:

```haskell
-- CR 700.2a: the mode indices all of whose target slots have a legal recipient
-- (a mode with no slots is trivially fillable). `extra` is the slots EVERY mode
-- carries in addition to its own -- CR 303.4a's enchant slot, which is declared
-- by the card rather than by a mode, and which castability must see or an Aura
-- with no legal creature would be castable and then countered on resolution (CR
-- 601.2c says it could never have been cast). An ability has no enchant spec and
-- passes Map.empty, which makes that a fact of the call rather than a special
-- case here.
fillableModes :: ObjectId -> Map SlotName TargetSpec -> Modal.Modal Card -> GameState -> Set ModeIndex
fillableModes source extra modal gs =
  let ms = Foldable.toList (Modal.modes modal)
      fillable i m =
        let sets = legalSets source (Map.union extra (Mode.targetSpecs m)) gs
         in if any Set.null (Map.elems sets)
              then Nothing
              else Just (ModeIndex.MkModeIndex (fromIntegral i))
   in Set.fromList (Maybe.mapMaybe (uncurry fillable) (zip [0 :: Int ..] ms))
```

- [ ] **Step 5: Update the five callers**

`Cast.hs:70` and `Cast.hs:162` pass the card's enchant slots:

```haskell
Target.fillableModes oid (Card.enchantSpecs card) (Card.Type.spell card) gs
```

`Activate.hs:65`, `Activate.hs:101` and `Engine.hs:416` pass `Map.empty`:

```haskell
Target.fillableModes srcId Map.empty (ActivatedAbility.modal ability) gs
```

Fix the two test-suite callers the compiler names (`ReplaySpec.hs:230`, `ModalSpec.hs:140` and `:408`) the same way — abilities, so `Map.empty`.

- [ ] **Step 6: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS, warning-free.

- [ ] **Step 7: Commit**

```bash
git add source/library/Pawl/ source/test-suite/Pawl/
hooky fix
git add source/library/Pawl/ source/test-suite/Pawl/
hooky run
git commit -m "feat(target): an Aura spell's enchant slot reaches targeting and castability (CR 303.4a)"
```

---

## Task 7: `Stack`'s Aura branch — fizzle, then enter attached

The end-to-end path, and the phase's headline test.

**Files:**
- Modify: `source/library/Pawl/Resolve.hs`, `source/library/Pawl/Stack.hs`
- Create: `source/test-suite/Pawl/AuraSpec.hs`
- Modify: `source/test-suite/Pawl/Main.hs`, `pawl.cabal`

**Interfaces:**
- Consumes: `Card.isAura`, `Card.enchantSlot` (Task 2); `Event.changeZoneAttaching` (Task 3); the merged specs (Task 6).
- Produces: `Resolve.targetsAllIllegal :: ObjectId -> GameState -> Bool`.

`AuraSpec` heads with a comment listing the modules it covers, exposes `tests :: TestTree`, and is added to `Main.hs`'s `testTree` **and** to the test-suite `other-modules` list in `pawl.cabal` by hand.

- [ ] **Step 1: Write the failing test**

Create `source/test-suite/Pawl/AuraSpec.hs` with the standard header comment and:

```haskell
HU.testCase "CR 303.4: a resolving Aura spell enters the battlefield attached to its target" $ do
  swamp <- Registry.printing registry "Swamp"
  piker <- Registry.printing registry "Goblin Piker"
  unholyStrength <- Registry.printing registry "Unholy Strength"
  let base = S.landsInPlay swamp 1
      (creature, withCreature) = S.addCreature piker S.bob base
      (gs, spellId) = S.handOne unholyStrength withCreature
      cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
      after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
      auras = filter (\o -> Object.zone o == Zone.Battlefield && Maybe.isJust (Object.attachedTo o)) (Map.elems (GameState.objects after))
  HU.assertEqual "one attached permanent on the battlefield" 1 (length auras)
  HU.assertEqual "attached to the creature" [Just creature] (fmap Object.attachedTo auras)
  HU.assertEqual "the creature is a 4/2" (Just (4, 2)) (S.powerToughnessOf creature after),
-- CR 608.2b: an Aura spell is the first PERMANENT spell in this pool that can be
-- countered on resolution. Before this task, Stack sent every permanent spell to
-- the battlefield with no target check at all.
HU.testCase "CR 608.2b: an Aura spell whose target left is countered on resolution" $ do
  swamp <- Registry.printing registry "Swamp"
  piker <- Registry.printing registry "Goblin Piker"
  unholyStrength <- Registry.printing registry "Unholy Strength"
  let base = S.landsInPlay swamp 1
      (creature, withCreature) = S.addCreature piker S.bob base
      (gs, spellId) = S.handOne unholyStrength withCreature
      cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
      -- The target leaves in response, so no legal target remains at resolution.
      bounced = S.runPure S.identityAnswer cast (Event.changeZone creature Zone.Hand)
      after = snd (Engine.runGamePure S.identityAnswer bounced Stack.resolveTop)
  HU.assertEqual "nothing attached on the battlefield" [] (filter (\o -> Object.zone o == Zone.Battlefield && Maybe.isJust (Object.attachedTo o)) (Map.elems (GameState.objects after)))
  HU.assertEqual "the Aura is in its owner's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test`
Expected: the first case FAILS (the Aura enters unattached — `attachedTo` is `Nothing`); the second FAILS (the Aura enters the battlefield instead of the graveyard).

- [ ] **Step 3: Extract the fizzle test**

In `source/library/Pawl/Resolve.hs`, lift the `specs`/`chosen`/`legalSlot`/`fizzles` computation out of `resolveSpellWith` (currently `Resolve.hs:335-348`) into a top-level function, and have `resolveSpellWith` call it. One implementation, so the Aura path and the ordinary spell path cannot drift:

```haskell
-- CR 608.2b: are ALL of this spell's targets illegal? "For every instance of the
-- word 'target'" -- so a spell with no target spec never fizzles, and one with
-- several survives if any one is still legal. Reserved slots (a trigger's source,
-- a token this resolution minted) are not targets and cannot make a spell fizzle;
-- they are vacuously legal.
--
-- Shared by the ordinary spell path (resolveSpellWith) and the Aura path
-- (Pawl.Stack), which is the whole point of it being a function: an Aura spell is
-- the first PERMANENT spell that can be countered on resolution, and a second
-- copy of this logic would drift.
targetsAllIllegal :: ObjectId -> GameState -> Bool
targetsAllIllegal oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj -> case Game.cardOf oid gs of
    Nothing -> False
    Just card ->
      let specs = Card.modesTargetSpecs (Binding.modesOf (Object.bindings obj)) card
          chosen = Binding.targetsOf (Object.bindings obj)
          legalSlot slot recipient = case Map.lookup slot specs of
            Nothing -> True
            Just spec -> Target.stillLegal oid recipient spec gs
          legality = Map.mapWithKey legalSlot chosen
          targeted = Map.restrictKeys legality (Map.keysSet specs)
       in not (Map.null specs) && not (or (Map.elems targeted))
```

Keep `resolveSpellWith`'s existing per-effect re-read of `legalSlot` exactly as it is — that is a different question (which *individual* slot is legal for this effect) and must not be folded into the all-or-nothing test.

- [ ] **Step 4: Add the Stack branch**

In `source/library/Pawl/Stack.hs`, replace the `Source.OfCard` arm:

```haskell
        Source.OfCard printing ->
          let card = Printing.card printing
           in if not (Card.isPermanent card)
                then Resolve.resolveSpellWith runSubgame oid
                else
                  if not (Card.isAura card)
                    then Event.changeZone oid Zone.Battlefield
                    else -- CR 303.4a made this spell target, so CR 608.2b applies to
                    -- it -- the first PERMANENT spell in this pool for which that
                    -- is true. THE INVARIANT: is-it-an-Aura is a SUBTYPE read off
                    -- the type line (CR 205.3h), the same closed-half
                    -- classification as is-it-a-permanent above it. Not a case on
                    -- the card's identity.
                      if Resolve.targetsAllIllegal oid gs
                        then Event.changeZone oid Zone.Graveyard
                        else
                          -- CR 303.4: an Aura ENTERS attached, so the target is
                          -- seeded into the new incarnation rather than written
                          -- after the move (see Event.changeZoneAttaching).
                          Monad.void (Event.changeZoneAttaching oid Zone.Battlefield (enchantedBy oid gs))
```

with the recipient reader beside it:

```haskell
-- The ObjectId an Aura spell's enchant slot names (CR 303.4a). Nothing when the
-- slot is unbound, which CR 303.4a makes unreachable for a cast Aura -- the slot
-- is a required target. A ToPlayer recipient is likewise unreachable while
-- Card.enchant is restricted to object pools; it is REJECTED rather than guessed
-- at, because CR 702.5d's enchant-player Auras need Object.attachedTo widened
-- before they can be attached at all (#N -- issue 4 from Task 1 Step 0).
enchantedBy :: ObjectId -> GameState.GameState -> Maybe ObjectId
enchantedBy oid gs = case Game.lookupObject oid gs of
  Nothing -> Nothing
  Just obj -> case Map.lookup Card.enchantSlot (Binding.targetsOf (Object.bindings obj)) of
    Just (Recipient.ToCreature target) -> Just target
    Just (Recipient.ToObject target) -> Just target
    _ -> Nothing
```

- [ ] **Step 5: Wire the new spec module**

Add `Pawl.AuraSpec` to `source/test-suite/Pawl/Main.hs`'s `testTree` and to the test-suite `other-modules` list in `pawl.cabal`.

- [ ] **Step 6: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS, warning-free.

- [ ] **Step 7: Commit**

```bash
git add source/library/Pawl/ source/test-suite/Pawl/ pawl.cabal
hooky fix
git add source/library/Pawl/ source/test-suite/Pawl/ pawl.cabal
hooky run
git commit -m "feat(stack): an Aura spell fizzles or enters attached (CR 303.4, CR 608.2b)"
```

---

## Task 8: CR 704.5m — the Aura falls off

The state-based action, and the stale comment it falsifies.

**Files:**
- Modify: `source/library/Pawl/Sba.hs`
- Test: `source/test-suite/Pawl/AuraSpec.hs`

**Interfaces:**
- Consumes: `Object.attachedTo` (Task 3), `Target.stillLegal`, `Card.Type.enchant` (Task 2).

**The two-pass behaviour is the assertion, not an accident.** SBAs are simultaneous: `performStateBasedActions` judges every object against the *pre-pass* projection (`Sba.hs:117-120`). When the enchanted creature dies in pass N, the Aura's illegality was judged against the state in which that creature was still on the battlefield — so the Aura survives pass N and falls off in pass N+1. CR 704.3 repeats the check until no state-based action is performed, which is what makes that correct rather than a bug.

- [ ] **Step 1: Write the failing test**

In `source/test-suite/Pawl/AuraSpec.hs`:

```haskell
-- CR 704.5m, and CR 704.3's repeat. SBAs are simultaneous, so the pass that
-- buries the creature judged the Aura against a state in which that creature was
-- still there; the Aura falls off on the NEXT pass. Asserting both passes is the
-- point -- an implementation that dropped the Aura in pass one would be reading
-- post-pass state, which is what CR 704.3's "simultaneously" forbids.
HU.testCase "CR 704.5m: an Aura whose creature died falls off on the next SBA pass" $ do
  swamp <- Registry.printing registry "Swamp"
  piker <- Registry.printing registry "Goblin Piker"
  unholyStrength <- Registry.printing registry "Unholy Strength"
  let base = S.landsInPlay swamp 1
      (creature, withCreature) = S.addCreature piker S.bob base
      (aura, withAura) = S.addCreature unholyStrength S.alice withCreature
      attached = S.attach aura creature withAura
      -- Goblin Piker is 2/1; Unholy Strength makes it 4/2, so 2 damage is not
      -- lethal and 3 is (CR 704.5g reads TOTAL marked damage against projected
      -- toughness).
      damaged = S.markDamage creature 3 attached
      pass1 = S.settleSba damaged
      pass2 = S.settleSba pass1
  HU.assertEqual "the creature is gone after pass one" Nothing (Game.lookupObject creature pass1)
  HU.assertBool "the Aura is still on the battlefield after pass one" (Set.member aura (GameState.battlefield pass1))
  HU.assertBool "the Aura is gone from the battlefield after pass two" (not (Set.member aura (GameState.battlefield pass2)))
  HU.assertEqual "and is in its OWNER's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice pass2)),
-- CR 704.5m's other two clauses.
HU.testCase "CR 704.5m: an unattached Aura on the battlefield goes to the graveyard" $ do
  unholyStrength <- Registry.printing registry "Unholy Strength"
  let base = Setup.emptyGame S.bothPlayers
      (aura, gs) = S.addCreature unholyStrength S.alice base
      after = S.settleSba gs
  HU.assertBool "never attached, so it falls off immediately" (not (Set.member aura (GameState.battlefield after))),
```

If `S.markDamage` does not exist, use whatever the neighbouring destruction tests use to mark damage and match its signature.

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test`
Expected: FAIL — the Aura stays on the battlefield after both passes.

- [ ] **Step 3: Add the state-based action**

In `source/library/Pawl/Sba.hs`, add the predicate:

```haskell
-- CR 704.5m: "If an Aura is attached to an illegal object or player, or is not
-- attached to an object or player, that Aura is put into its owner's graveyard."
-- Three clauses: unattached, attached to an id that is no longer a permanent, and
-- attached to one its own enchant ability no longer admits (CR 303.4c's "as
-- defined by its enchant ability and other applicable effects").
--
-- The third reuses Target.stillLegal so cast-time legality, CR 608.2b's
-- re-validation and this check are one implementation.
--
-- CR 303.4d's first clause -- an Aura can't enchant itself -- is the `oid == self`
-- arm. Unreachable in this pool (a Creatures enchant spec cannot name the Aura
-- spell on the stack), written anyway because it costs one comparison.
--
-- A put-into-graveyard, NOT a destruction: CR 704.5m says "put into its owner's
-- graveyard", so this goes through Event.changeZone and consults neither
-- indestructible (CR 702.12b) nor a regeneration shield (CR 701.19a).
fallsOff :: GameState -> ObjectId -> Bool
fallsOff gs oid = case Game.cardOf oid gs of
  Nothing -> False
  Just card -> case Card.Type.enchant card of
    Nothing -> False
    Just spec -> case Game.lookupObject oid gs of
      Nothing -> False
      Just obj -> case Object.attachedTo obj of
        Nothing -> True
        Just target ->
          target == oid
            || not (Target.stillLegal oid (Recipient.ToCreature target) spec gs)
```

The `Recipient` tag must match what the `Pool` produces — `Pool.Creatures` tags `ToCreature` (`Target.hs:73-80`), so a `Creatures` enchant spec is checked with `ToCreature`. Derive the tag from the spec's pool rather than hard-coding it if a second pool ever appears; for this pool, assert the choice with a comment.

Then wire it into `performStateBasedActions`, computed against the same pre-pass `gs` as the other classifications:

```haskell
      unattachedAuras = filter (fallsOff gs) onBattlefield
```

perform it beside CR 704.5f's bury:

```haskell
  -- CR 704.5m: the Aura follows its creature. A plain put-into-graveyard.
  Monad.mapM_ (\oid -> Event.changeZone oid Zone.Graveyard) unattachedAuras
```

and add it to `acted`:

```haskell
      acted = not (null toGraveyard) || not (null toDestroy) || not (null leaving) || not (null vanishing) || not (null annihilations) || not (null unattachedAuras)
```

- [ ] **Step 4: Correct the stale comment**

`Sba.hs:98-100` currently claims "One pass is enough in M1b: a creature dying cannot cause another SBA, because nothing gains or loses life when a creature dies. Revisit when it can." That is now false. Replace it:

```haskell
-- CR 704.3: repeat until no state-based action is performed. ONE pass here, with
-- the repeat living in Engine's CR 117.5 settle loop, which re-runs while
-- performStateBasedActions reports True.
--
-- A creature dying CAN now cause another state-based action: CR 704.5m puts an
-- Aura attached to it into its owner's graveyard, and because this pass judged
-- that Aura against the state in which its creature was still alive (SBAs are
-- simultaneous), the Aura falls off on the following pass. Any caller that
-- checks state-based actions without looping is therefore wrong; use the settle
-- loop.
```

Verify by inspection that every path that can bury a creature reaches `Engine`'s settle loop rather than calling `checkStateBasedActions` once. If one does not, that is a bug this task fixes — not a test to weaken.

- [ ] **Step 5: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS, warning-free.

- [ ] **Step 6: Commit**

```bash
git add source/library/Pawl/Sba.hs source/test-suite/Pawl/AuraSpec.hs
hooky fix
git add source/library/Pawl/Sba.hs source/test-suite/Pawl/AuraSpec.hs
hooky run
git commit -m "feat(sba): an Aura attached to nothing or to an illegal object falls off (CR 704.5m)"
```

---

## Task 9: Record the phase

The bookkeeping that closes it out. The deferral issues were filed in Task 1 Step 0 and cited as the code went in.

**Files:**
- Modify: `docs/progress.md`, `CLAUDE.md`

- [ ] **Step 1: Verify no placeholder citation survived**

Run: `grep -rn '(#N' source/ docs/superpowers/plans/2026-07-25-auras-a-attachment-substrate.md`
Expected: hits **only** inside the plan file, never inside `source/`. Any hit under `source/` is a comment that shipped without a real issue number — fix it before continuing.

- [ ] **Step 2: Record the phase**

Add one `docs/progress.md` entry in the established format — what this phase *established*, its gate card, the decisions it proved, and the types it added (`Object.attachedTo`, `Card.enchant`, `Subtype.Aura`, `Affected.Attached`, `Event.changeZoneAttaching`, `Resolve.targetsAllIllegal`, `Target.fillableModes`'s extra-slots parameter). Note that it added **no new opcode**: an Aura's whole behaviour is a static ability plus an entry rule.

**Replace** `CLAUDE.md`'s status bullet — never append. Milestone history goes in `progress.md`.

- [ ] **Step 3: Verify the phase is complete**

Run: `cabal clean && cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS, warning-free from a clean build — the only definitive check, since incremental builds hide warnings from unchanged modules. This is the one place a `cabal clean` is worth its cost.

Confirm #114 is closed and the seven deferral issues from Task 1 Step 0 are open.

- [ ] **Step 4: Commit**

```bash
git add docs/progress.md CLAUDE.md
hooky fix
git add docs/progress.md CLAUDE.md
hooky run
git commit -m "docs(auras): record the attachment substrate"
```

- [ ] **Step 5: Confirm every step is ticked**

Run: `grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-25-auras-a-attachment-substrate.md`
Expected: `0`. Use *that* grep, not `grep -c -- '- \[ \]'` — this plan's header quotes the checkbox syntax in prose, so the naive grep can never reach zero.

---

## What phase (a) deliberately leaves undone

Phase (b) (`docs/superpowers/plans/` — written after this one lands) takes:

- `Modification.SetControllerToSource` and `Projection.controllerOf` gathering static abilities, hoisted against the O(permanents³) blowup `Projection.hs:542-546` records.
- The visited-set recursion escape, reusing #37's shape with `Object.owner` as the escape value.
- `Pawl.Departure`'s CR 800.4a interaction and the two comments it falsifies (`Departure.hs:222`, `Departure.hs:261`).
- Control Magic, closing #33 and #62.
