# M4f Counters Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Model +1/+1 and −1/−1 counters as persistent per-permanent state that feeds power/toughness through the layer system (CR 613.4c, layer 7c) and runs the CR 704.5q annihilation state-based action, gated by Battlegrowth and Instill Infection.

**Architecture:** Counters live in a new `Object.counters :: Map CounterKind Natural` field (per-incarnation, reset by `changeZone`, untouched by cleanup — the `damage`/`sickness`/`bindings` posture). A new `Effect.PutCounters` opcode edits that field in place at resolution. The projection injects each counter-bearing object's net counter delta as one synthetic layer-7c `ModifyPowerToughness` folded by the existing path. A new `Sba` arm performs CR 704.5q annihilation.

**Tech Stack:** Haskell 2010 (GHC 9.14.1, no extensions beyond `GADTs`/`RankNTypes`/`NamedFieldPuns`), tasty (`tasty-hunit` + `tasty-quickcheck`), the hand-rolled JSON codec (`Pawl.Codec`/`Pawl.Json`), cards as `data/cards/*.json`.

## Global Constraints

- **Warning-clean build, `-Werror` via the `pedantic` flag.** Every task ends green under `cabal build all --enable-tests --enable-benchmarks`. Incremental builds hide warnings from unchanged modules; when in doubt `cabal clean` first.
- **Two invariants outrank this plan.** The rules core never cases on a card's *identity*, only classifications (`Pawl.Resolve` is the sole `case effect of` home; `Pawl.Projection` the sole `case … Modification` home). The engine never makes a player's choice.
- **Haskell 2010, project style:** one type per `Pawl.Type.<Name>` module; qualified imports aliased to the last component; operators unqualified; `newtype`/`Mk`-prefixed constructors, non-punning; no partial functions; no list comprehensions; `case` over point-free; explicit `Text` not `String`; arbitrary-precision numbers. Derive at least `Eq` and `Show`.
- **Rules discipline:** every rules claim is checked against `docs/rules.txt` and the CR number cited in the code comment. Never trust recalled Magic rules.
- **TDD, one commit per task.** Write the failing test, run it to watch it fail, implement minimally, run it green, then run `hooky`. Never weaken an assertion or edit a test to pass.
- **Formatting/lint before every commit:** `git add -A && hooky fix && git add -A && hooky run` (hooky acts on *staged* files only), then commit. Apply HLint suggestions or justify the exception.
- **Test/deck posture (spec §6):** deterministic fixtures carry the gate scenarios; Battlegrowth swaps 4-for-4 into `greenDeck`, Instill Infection 4-for-4 into `blackDeck`, each staying 60 (card-backed conservation stays 120 — a counter mints no object).

**Spec:** `docs/superpowers/specs/2026-07-20-m4f-counters-design.md`. Cards Scryfall-verified 2026-07-20: **Battlegrowth** `{G}` Instant "Put a +1/+1 counter on target creature."; **Instill Infection** `{3}{B}` Instant "Put a -1/-1 counter on target creature.\nDraw a card."

---

## File Structure

- **Create** `source/library/Pawl/Type/CounterKind.hs` — the `CounterKind` leaf enum.
- **Modify** `source/library/Pawl/Type/Object.hs` — add the `counters` field.
- **Modify** `source/library/Pawl/Event.hs` — reset `counters` in `changeZone`; init in `createToken`.
- **Modify** `source/library/Pawl/Setup.hs`, `source/library/Pawl/Engine.hs`, `source/library/Pawl/Activate.hs` — init `counters = Map.empty` at each `MkObject` site.
- **Modify** `source/library/Pawl/Type/Effect.hs` — the `PutCounters` constructor.
- **Modify** `source/library/Pawl/Resolve.hs` — five classification arms, the `applyEffect` arm, the `putCounters` helper.
- **Modify** `source/library/Pawl/Codec.hs` — `counterKindToJson`/`jsonToCounterKind` and the `PutCounters` effect arms.
- **Modify** `source/library/Pawl/Projection.hs` — `counterGathered` + append in `gather`.
- **Modify** `source/library/Pawl/Sba.hs` — the CR 704.5q annihilation arm.
- **Create** `data/cards/battlegrowth.json`, `data/cards/instill-infection.json`.
- **Modify** `source/test-suite/Pawl/Support.hs` — `addCounter` helper + `MkObject` field at 8 sites.
- **Modify** `source/test-suite/Pawl/Cards.hs` — load, record fields, `allPrintings`, deck swaps.
- **Modify** `source/test-suite/Pawl/{EventSpec,ProjectionSpec,ResolveSpec,CodecSpec}.hs` — tests; plus `MkObject` field at their construction sites.
- **Modify** `docs/design.md`, `docs/progress.md`, `CLAUDE.md` — the 7d→7c correction, the milestone-completion entry, the status note.

---

## Task 1: `CounterKind` type + `Object.counters` storage

**Files:**
- Create: `source/library/Pawl/Type/CounterKind.hs`
- Modify: `source/library/Pawl/Type/Object.hs` (add field after `bindings`)
- Modify: `source/library/Pawl/Event.hs:98` (changeZone reset), `source/library/Pawl/Event.hs:168` (createToken)
- Modify: `source/library/Pawl/Setup.hs:95`, `source/library/Pawl/Engine.hs:204`, `source/library/Pawl/Activate.hs:83`
- Modify: `source/test-suite/Pawl/Support.hs` (8 `MkObject` sites + new `addCounter` helper)
- Modify (compiler-flagged `MkObject` sites): `source/test-suite/Pawl/GameSpec.hs:110,365`, `source/test-suite/Pawl/CastSpec.hs:395`, `source/test-suite/Pawl/ResolveSpec.hs:249,277,306,366,398,478` (record) and `ResolveSpec.hs:510,659` (positional)
- Test: `source/test-suite/Pawl/EventSpec.hs`

**Interfaces:**
- Produces: `CounterKind.CounterKind` = `PlusOnePlusOne | MinusOneMinusOne` (`Eq`, `Ord`, `Show`); `Object.counters :: Map CounterKind.CounterKind Natural`; `S.addCounter :: CounterKind.CounterKind -> Natural -> ObjectId -> GameState -> GameState`.

- [x] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/EventSpec.hs`. Add imports `import qualified Pawl.Type.CounterKind as CounterKind` and (if absent) `import qualified Data.Map.Strict as Map`. Add this test case into the existing `tests` tree's top-level list (mirror a neighboring `HU.testCase`):

```haskell
      HU.testCase "CR 122.2 counters cease to exist when an object changes zones" $
        let base = S.landsInPlay (Cards.swampPrinting cards) 0
            (oid, withCreature) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            withCounter = S.addCounter CounterKind.PlusOnePlusOne 2 oid withCreature
            -- Bounce to hand: changeZone mints a new incarnation (CR 400.7).
            bounced = Event.changeZone oid Zone.Hand withCounter
            -- Total (no `head`): map over the hand zone; expect exactly one card, empty.
            handCounters = map (\h -> maybe (Map.fromList [(CounterKind.PlusOnePlusOne, 99)]) Object.counters (Game.lookupObject h bounced)) (Game.zoneMembers Zone.Hand S.bob bounced)
         in do
              HU.assertEqual "counter present before the move" (Map.fromList [(CounterKind.PlusOnePlusOne, 2)]) (maybe Map.empty Object.counters (Game.lookupObject oid withCounter))
              HU.assertEqual "the one new incarnation in hand has no counters" [Map.empty] handCounters,
```

(If `EventSpec` lacks `Object`/`Zone`/`Game`/`Cards`/`S` imports, add the qualified imports it needs — check the module header; these mirror `ResolveSpec`.)

- [x] **Step 2: Run test to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Not in scope: type constructor CounterKind`, `addCounter`, and record field `counters`.

- [x] **Step 3a: Create the `CounterKind` module**

`source/library/Pawl/Type/CounterKind.hs`:

```haskell
module Pawl.Type.CounterKind where

-- CR 122: a counter is a marker that modifies characteristics or interacts with a
-- rule (CR 122.1). Its KIND is a closed-half classification -- the same posture as
-- Keyword (a citation, not an effect identity): the rules core reads counts by
-- kind (the P/T contribution in CR 613.4c; the CR 704.5q annihilation SBA) and
-- never cases on a card. Only the two P/T-modifying kinds exist at M4f (CR 122.1a);
-- keyword/charge/loyalty/poison/shield/stun counters (CR 122.1b-i) are future.
-- Ord is load-bearing: CounterKind is a Map key on Object.counters.
data CounterKind
  = PlusOnePlusOne -- CR 122.1a: +1/+1
  | MinusOneMinusOne -- CR 122.1a: -1/-1
  deriving (Eq, Ord, Show)
```

Run `cabal-gild` via `hooky fix` later so `exposed-modules` picks it up (the `-- cabal-gild: discover` directive).

- [x] **Step 3b: Add the `Object.counters` field**

In `source/library/Pawl/Type/Object.hs`, add the import `import Pawl.Type.CounterKind (CounterKind)` (alphabetical among the `Pawl.Type.*` imports) and the field immediately after `bindings`:

```haskell
    -- CR 122.1: counters placed on this permanent, counted per kind. Persistent
    -- permanent state -- unlike `damage`, cleanup does NOT clear it (a counter is
    -- not an "until end of turn" effect). Per-incarnation: reset by changeZone,
    -- because CR 122.2 says counters "simply cease to exist" when an object changes
    -- zones (the CR 400.7 mechanism that also resets damage/sickness/bindings). A
    -- +1/+1 or -1/-1 count feeds P/T via the projection (CR 122.1a / 613.4c); both
    -- kinds present trigger the CR 704.5q annihilation SBA.
    counters :: Map CounterKind Natural,
```

- [x] **Step 3c: Fix every `MkObject` construction site**

Record-syntax sites — add `Object.counters = Map.empty,` after the `bindings` line:
`Setup.hs:95`, `Engine.hs:204`, `Activate.hs:83`, `Event.hs:168` (createToken), `Support.hs:265,293,316,345,371,398,570,638`, `GameSpec.hs:110,365`, `CastSpec.hs:395`, `ResolveSpec.hs:249,277,306,366,398,478`. (Each already imports `Data.Map.Strict as Map`; if a test module does not, add it.)

In `Event.hs:98` (the `changeZone` reset, record-*update* syntax) add `Object.counters = Map.empty` to the field list:

```haskell
        mkObj ts = obj {Object.zone = dest, Object.tapped = TapState.Untapped, Object.damage = 0, Object.sickness = Sickness.Sick, Object.bindings = Map.empty, Object.counters = Map.empty, Object.timestamp = ts}
```

Positional sites `ResolveSpec.hs:510` and `ResolveSpec.hs:659` — insert `Map.empty` between the `bindings` arg (the existing `Map.empty`) and the `Timestamp` arg:

```haskell
      obj = Object.MkObject pid (Source.OfCard printing) Zone.Hand TapState.Untapped 0 Sickness.Settled Map.empty Map.empty (Timestamp.MkTimestamp 0)
```

The build's `-Wmissing-fields` under `-Werror` flags any record site you miss — recompile until clean.

- [x] **Step 3d: Add the `addCounter` test helper**

In `source/test-suite/Pawl/Support.hs` add `import qualified Pawl.Type.CounterKind as CounterKind` and a helper (place near `markDamage`/`addRegenShield`):

```haskell
-- Put `n` counters of a kind directly onto an object's per-incarnation state,
-- bypassing the PutCounters opcode -- so a projection or SBA test can set up
-- counters without resolving a spell.
addCounter :: CounterKind.CounterKind -> Natural.Natural -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
addCounter kind n oid gs =
  let bump obj = obj {Object.counters = Map.insertWith (+) kind n (Object.counters obj)}
   in gs {GameState.objects = Map.adjust bump oid (GameState.objects gs)}
```

- [x] **Step 4: Run test to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "/CR 122.2 counters cease/"'`
Expected: PASS.

- [x] **Step 5: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4f: CounterKind type + Object.counters per-incarnation storage (CR 122.2)"
```

---

## Task 2: `Effect.PutCounters` opcode + Resolve wiring + Codec

**Files:**
- Modify: `source/library/Pawl/Type/Effect.hs`
- Modify: `source/library/Pawl/Resolve.hs` (`slotsOf`, `readsX`, `manaProduced`, `searchesLibrary`, `rewriteEffect`, `applyEffect`, new `putCounters`)
- Modify: `source/library/Pawl/Codec.hs`
- Test: `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Consumes: `CounterKind.CounterKind` (Task 1).
- Produces: `Effect.PutCounters CounterKind Quantity SlotName`; `Codec.counterKindToJson`/`Codec.jsonToCounterKind`; `Resolve.putCounters :: ObjectId -> CounterKind -> Natural -> GameState -> GameState`.

- [x] **Step 1: Write the failing test**

In `source/test-suite/Pawl/CodecSpec.hs`, add (imports `import qualified Pawl.Type.CounterKind as CounterKind`, and `Effect`/`Quantity`/`SlotName` if not present):

```haskell
      HU.testCase "PutCounters effect round-trips through the codec" $
        let effect = Effect.PutCounters CounterKind.PlusOnePlusOne (Quantity.Literal 1) (SlotName.MkSlotName (Text.pack "creature"))
         in HU.assertEqual "round-trip" (Right effect) (Codec.jsonToEffect (Codec.effectToJson effect)),
      HU.testCase "both CounterKinds round-trip" $ do
        HU.assertEqual "plus" (Right CounterKind.PlusOnePlusOne) (Codec.jsonToCounterKind (Codec.counterKindToJson CounterKind.PlusOnePlusOne))
        HU.assertEqual "minus" (Right CounterKind.MinusOneMinusOne) (Codec.jsonToCounterKind (Codec.counterKindToJson CounterKind.MinusOneMinusOne)),
```

(Match the exact `SlotName` constructor to the codebase — confirm with `grep 'MkSlotName\|textToSlotName' source/library/Pawl/Type/SlotName.hs`; use whatever the existing tests use.)

- [x] **Step 2: Run test to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `PutCounters`, `counterKindToJson`, `jsonToCounterKind` not in scope.

- [x] **Step 3a: Add the opcode**

In `source/library/Pawl/Type/Effect.hs`, add the import `import Pawl.Type.CounterKind (CounterKind)` and the constructor (place beside the other targeted verbs):

```haskell
  | -- CR 122.6: put this many counters of this kind on the slot's target permanent.
    -- Battlegrowth = PutCounters PlusOnePlusOne (Literal 1) slot; Instill Infection
    -- = PutCounters MinusOneMinusOne (Literal 1) slot. A counter is persistent
    -- object state, NOT a zone change -- Resolve.applyEffect edits Object.counters
    -- in place (Map.insertWith (+)), never through Event.changeZone. Quantity is how
    -- many (reused from M4a; a future X-counter card rides ChooseX). The counter's
    -- P/T effect is applied by the projection (CR 122.1a / 613.4c), not here.
    PutCounters CounterKind Quantity SlotName
```

- [x] **Step 3b: Add the five classification arms in `Resolve.hs`**

Add `import qualified Pawl.Type.CounterKind as CounterKind` (if a qualified alias is needed anywhere; the arms below don't name it, so it may not be). Add each arm:

- `slotsOf`: `Effect.PutCounters _ _ slot -> Set.singleton slot`
- `readsX` (`effectReadsX`): `Effect.PutCounters _ quantity _ -> quantity == Quantity.Type.X`
- `manaProduced`: `Effect.PutCounters {} -> Nothing`
- `searchesLibrary`: `Effect.PutCounters {} -> False`
- `rewriteEffect` (the identity arm): `Effect.PutCounters {} -> effect`

- [x] **Step 3c: Add the `putCounters` helper and the `applyEffect` arm in `Resolve.hs`**

Helper (place near other small state helpers; needs `Object`, `Game`, `Map`, `CounterKind`, `Natural`):

```haskell
-- CR 122.6: add `n` counters of a kind to a permanent's per-incarnation state.
putCounters :: ObjectId -> CounterKind.CounterKind -> Natural -> GameState -> GameState
putCounters oid kind n gs =
  let bump obj = obj {Object.counters = Map.insertWith (+) kind n (Object.counters obj)}
   in gs {GameState.objects = Map.adjust bump oid (GameState.objects gs)}
```

`applyEffect` arm (follow the `DealDamage` idiom exactly — first arg is `source`, evaluate the `Quantity` against `source`, `Nothing`/`n <= 0` no-op):

```haskell
  Effect.PutCounters kind quantity slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs -- a player recipient takes no counters
          Just target -> case Quantity.evaluate gs source quantity of
            Nothing -> gs -- unevaluable quantity: no-op (the powerOf posture)
            Just n -> if n <= 0 then gs else putCounters target kind (fromInteger n) gs
        _ -> gs -- illegal slot at resolution (CR 608.2b): no-op
```

- [x] **Step 3d: Add the Codec arms in `Codec.hs`**

Add `import qualified Pawl.Type.CounterKind as CounterKind`. Add the enum codec (model on `cardTypeToJson`/`jsonToCardType`):

```haskell
counterKindToJson :: CounterKind.CounterKind -> Value
counterKindToJson k = nullary . Text.pack $ case k of
  CounterKind.PlusOnePlusOne -> "PlusOnePlusOne"
  CounterKind.MinusOneMinusOne -> "MinusOneMinusOne"

jsonToCounterKind :: Value -> Either Text CounterKind.CounterKind
jsonToCounterKind =
  decodeNullary
    (Text.pack "CounterKind")
    [ (Text.pack "PlusOnePlusOne", CounterKind.PlusOnePlusOne),
      (Text.pack "MinusOneMinusOne", CounterKind.MinusOneMinusOne)
    ]
```

(`decodeNullary` and `nullary` are the exact helpers `cardTypeToJson`/`jsonToCardType` use — `sed -n '112,132p' source/library/Pawl/Codec.hs` to confirm; copy that shape verbatim.)

Add the `effectToJson` arm (Array of three, like `Discard`):

```haskell
  Effect.PutCounters k q s -> Json.tagged (Text.pack "PutCounters") (Just (Array [counterKindToJson k, quantityToJson q, slotNameToJson s]))
```

Add the `jsonToEffect` arm:

```haskell
    "PutCounters" -> case mv of
      Just (Array [k, q, s]) -> Effect.PutCounters <$> jsonToCounterKind k <*> jsonToQuantity q <*> jsonToSlotName s
      _ -> Left (Text.pack "PutCounters expects [counterKind, quantity, slot]")
```

- [x] **Step 4: Run test to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "/PutCounters/ || /CounterKind/"'`
Expected: PASS.

- [x] **Step 5: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4f: Effect.PutCounters opcode + Resolve wiring + codec (CR 122.6)"
```

---

## Task 3: Projection layer-7c injection

**Files:**
- Modify: `source/library/Pawl/Projection.hs` (`counterGathered`, append in `gather`)
- Test: `source/test-suite/Pawl/ProjectionSpec.hs`

**Interfaces:**
- Consumes: `Object.counters` (Task 1), `S.addCounter` (Task 1).
- Produces: counters visible in `Projection.powerOf`/`toughnessOf` at layer 7c.

- [x] **Step 1: Write the failing tests**

In `source/test-suite/Pawl/ProjectionSpec.hs` (add `import qualified Pawl.Type.CounterKind as CounterKind` if absent), add to the `tests` tree:

```haskell
      HU.testCase "CR 122.1a a +1/+1 counter adds +1/+1 (layer 7c)" $
        let base = S.landsInPlay (Cards.forestPrinting cards) 0
            (oid, gs0) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            gs = S.addCounter CounterKind.PlusOnePlusOne 1 oid gs0
         in do
              HU.assertEqual "power 2 + 1" (Just 3) (Projection.powerOf oid gs)
              HU.assertEqual "toughness 1 + 1" (Just 2) (Projection.toughnessOf oid gs),
      HU.testCase "CR 122.1a a -1/-1 counter subtracts 1/1" $
        let base = S.landsInPlay (Cards.forestPrinting cards) 0
            (oid, gs0) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            gs = S.addCounter CounterKind.MinusOneMinusOne 1 oid gs0
         in do
              HU.assertEqual "power 2 - 1" (Just 1) (Projection.powerOf oid gs)
              HU.assertEqual "toughness 1 - 1" (Just 0) (Projection.toughnessOf oid gs),
      HU.testCase "CR 613.4c a +1/+1 counter and Giant Growth stack in layer 7c" $
        let base = S.landsInPlay (Cards.forestPrinting cards) 0
            (oid, gs0) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            gs1 = S.addCounter CounterKind.PlusOnePlusOne 1 oid gs0
            gs = S.withEffect oid (Timestamp.MkTimestamp 9) (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)) gs1
         in do
              HU.assertEqual "power 2 + 1 + 3" (Just 6) (Projection.powerOf oid gs)
              HU.assertEqual "toughness 1 + 1 + 3" (Just 5) (Projection.toughnessOf oid gs),
```

(Confirm the `S.withEffect` helper name/signature used by the existing Giant-Growth projection tests — `grep 'withEffect' source/test-suite/Pawl/*.hs`; reuse it verbatim. Confirm `Cards.forestPrinting`/`pikerPrinting` are the right fixtures for a green creature.)

- [x] **Step 2: Run test to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "/counter/"'`
Expected: FAIL — counters are stored but do not affect projected P/T yet (power/toughness unchanged from base).

- [x] **Step 3: Inject counters into `gather`**

In `source/library/Pawl/Projection.hs` add `import qualified Pawl.Type.CounterKind as CounterKind` and the producer:

```haskell
-- CR 122.1a / 613.4c: a +1/+1 counter adds +1/+1 and a -1/-1 counter adds -1/-1,
-- in layer 7c. Emit each battlefield object's counters as ONE synthetic 7c
-- ModifyPowerToughness with net delta d = (#PlusOnePlusOne - #MinusOneMinusOne) on
-- each axis, folded by the same path as Giant Growth. Constructed HERE (Projection
-- is the sole home that may name a Modification constructor). Layer 7c is purely
-- additive, so pre-combining the counters and the object's own timestamp are both
-- unobservable (spec section 4). d == 0 emits nothing.
counterGathered :: GameState -> [Gathered]
counterGathered gs = Maybe.mapMaybe fromObject (Set.toList (GameState.battlefield gs))
  where
    fromObject oid = case Game.lookupObject oid gs of
      Nothing -> Nothing
      Just obj ->
        let cs = Object.counters obj
            plus = toInteger (Map.findWithDefault 0 CounterKind.PlusOnePlusOne cs)
            minus = toInteger (Map.findWithDefault 0 CounterKind.MinusOneMinusOne cs)
            d = plus - minus
         in if d == 0
              then Nothing
              else
                Just
                  MkGathered
                    { gSource = oid,
                      gAffected = Affected.TheseObjects (Set.singleton oid),
                      gLayer = Layer.ModifyPT,
                      gTimestamp = Object.timestamp obj,
                      gModification = Modification.ModifyPowerToughness (Quantity.Literal d) (Quantity.Literal d)
                    }
```

Append it in `gather` — change the final line from `stored ++ static_` to:

```haskell
   in stored ++ static_ ++ counterGathered gs
```

(`Object` is imported qualified; confirm `Object.counters`/`Object.timestamp` resolve. `Maybe` is imported as `Maybe`.)

- [x] **Step 4: Run test to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "/counter/ || /7c/"'`
Expected: PASS (all three).

- [x] **Step 5: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4f: project counters as layer-7c P/T (CR 122.1a / 613.4c)"
```

---

## Task 4: CR 704.5q annihilation state-based action

**Files:**
- Modify: `source/library/Pawl/Sba.hs` (`performStateBasedActions`)
- Test: `source/test-suite/Pawl/EventSpec.hs`

**Interfaces:**
- Consumes: `Object.counters` (Task 1), `S.addCounter` (Task 1), `Sba.checkStateBasedActions`.
- Produces: a permanent with both counter kinds has `min` of each removed on an SBA check.

- [x] **Step 1: Write the failing tests**

In `source/test-suite/Pawl/EventSpec.hs` add:

```haskell
      HU.testCase "CR 704.5q both counter kinds annihilate to zero (symmetric)" $
        let base = S.landsInPlay (Cards.swampPrinting cards) 0
            (oid, gs0) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            gs1 = S.addCounter CounterKind.PlusOnePlusOne 1 oid gs0
            gs2 = S.addCounter CounterKind.MinusOneMinusOne 1 oid gs1
            after = Sba.checkStateBasedActions gs2
         in HU.assertEqual "no counters remain" Map.empty (maybe (Map.fromList [(CounterKind.PlusOnePlusOne, 99)]) Object.counters (Game.lookupObject oid after)),
      HU.testCase "CR 704.5q annihilation removes N = min (asymmetric)" $
        let base = S.landsInPlay (Cards.swampPrinting cards) 0
            (oid, gs0) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            gs1 = S.addCounter CounterKind.PlusOnePlusOne 2 oid gs0
            gs2 = S.addCounter CounterKind.MinusOneMinusOne 1 oid gs1
            after = Sba.checkStateBasedActions gs2
         in do
              HU.assertEqual "one +1/+1 remains" (Just 1) (fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid after))
              HU.assertEqual "no -1/-1 remains" (Just 0) (fmap (Map.findWithDefault 0 CounterKind.MinusOneMinusOne . Object.counters) (Game.lookupObject oid after))
              -- Net P/T unchanged by annihilation: base 2/1 + net +1/+1 = 3/2.
              HU.assertEqual "power still 3" (Just 3) (Projection.powerOf oid after)
              HU.assertEqual "toughness still 2" (Just 2) (Projection.toughnessOf oid after),
```

(Add `import qualified Pawl.Projection as Projection` to `EventSpec` if absent.)

- [x] **Step 2: Run test to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "/704.5q/"'`
Expected: FAIL — counters are not annihilated (symmetric case still shows `{PlusOnePlusOne:1, MinusOneMinusOne:1}`).

- [x] **Step 3: Add the annihilation arm to `performStateBasedActions`**

In `source/library/Pawl/Sba.hs` add `import qualified Data.Maybe as Maybe` (if absent) and `import qualified Pawl.Type.CounterKind as CounterKind`. Inside the `let` block of `performStateBasedActions`, add (reading the *incoming* `gs` for CR 704.4 simultaneity):

```haskell
      -- CR 704.5q / 122.3: a permanent with both a +1/+1 and a -1/-1 counter has N
      -- of each removed (N = min). A counter-count edit, not a bury or departure --
      -- it feeds the `acted` flag (CR 704.4 repeats) but never re-fires once
      -- balanced. Net P/T is preserved, so it can neither cause nor prevent a
      -- death; ordering vs the bury/destroy step is immaterial.
      annihilateOne oid = case Game.lookupObject oid gs of
        Nothing -> Nothing
        Just obj ->
          let cs = Object.counters obj
              plus = Map.findWithDefault 0 CounterKind.PlusOnePlusOne cs
              minus = Map.findWithDefault 0 CounterKind.MinusOneMinusOne cs
              n = min plus minus
           in if n > 0 then Just (oid, n) else Nothing
      annihilations = Maybe.mapMaybe annihilateOne onBattlefield
      removeN n c = let c' = c - n in if c' == 0 then Nothing else Just c'
      balance g (oid, n) =
        let strip obj = obj {Object.counters = Map.update (removeN n) CounterKind.MinusOneMinusOne (Map.update (removeN n) CounterKind.PlusOnePlusOne (Object.counters obj))}
         in g {GameState.objects = Map.adjust strip oid (GameState.objects g)}
```

Then apply it to the final state and OR its flag. Change the closing lines from:

```haskell
      acted = not (null toGraveyard) || not (null toDestroy) || not (null leaving) || not (null vanishing)
   in (acted, drained {GameState.result = outcome <|> GameState.result drained})
```

to:

```haskell
      balanced = List.foldl' balance drained annihilations
      acted = not (null toGraveyard) || not (null toDestroy) || not (null leaving) || not (null vanishing) || not (null annihilations)
   in (acted, balanced {GameState.result = outcome <|> GameState.result balanced})
```

(`Map` subtraction is safe: `n = min plus minus`, so both counts are `>= n`; `removeN` deletes the key at zero. `Natural` never goes negative here.)

- [x] **Step 4: Run test to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "/704.5q/"'`
Expected: PASS (both).

- [x] **Step 5: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4f: CR 704.5q / 122.3 counter annihilation state-based action"
```

---

## Task 5: The cards + gameplay gate tests + deck coverage

**Files:**
- Create: `data/cards/battlegrowth.json`, `data/cards/instill-infection.json`
- Modify: `source/test-suite/Pawl/Cards.hs` (record fields, loader lines, `allPrintings`, `greenDeck`, `blackDeck`)
- Modify: `source/test-suite/Pawl/ResolveSpec.hs` (gameplay gate tests; retire the synthetic −0/−1 fixture at lines 616–632)

**Interfaces:**
- Consumes: everything from Tasks 1–4; the M3.5 loader (`Cards.loadPrinting`), `S.handOne`, `Cast.castSpell`, `Stack.resolveTop`, `Sba.checkStateBasedActions`, `S.addCreature`, `S.landsInPlay`.
- Produces: `Cards.battlegrowthPrinting`, `Cards.instillInfectionPrinting`; both cards in `allPrintings` (honesty round-trip) and in the green/black random decks.

- [x] **Step 1: Write the failing gate tests**

In `source/test-suite/Pawl/ResolveSpec.hs`, add a `countersTests :: Cards.Cards -> Tasty.TestTree` group (model its scenario plumbing on `zoneChangeTests`' Murder/Unsummon cases — `S.handOne`, `Cast.castSpell S.alice`, `Stack.resolveTop`, `S.identityAnswer`; target with `S.identityAnswer` if it points a single-creature slot at the only legal creature, else use an answer that aims at the intended creature — inspect how the Murder test lands its target and reuse that path), and wire it into this spec's exported `tests` list.

Cover exactly these cases:

```haskell
countersTests :: Cards.Cards -> Tasty.TestTree
countersTests cards =
  Tasty.testGroup
    "Counters"
    [ HU.testCase "CR 122.6 Battlegrowth puts a +1/+1 counter (gate)" $
        -- alice casts Battlegrowth on bob's Piker (2/1). After resolution the Piker
        -- is 3/2 and carries one +1/+1 counter.
        let base = S.landsInPlay (Cards.forestPrinting cards) 1
            (victim, withFoe) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            (gs, spellId) = S.handOne (Cards.battlegrowthPrinting cards) withFoe
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "power 3" (Just 3) (Projection.powerOf victim after)
              HU.assertEqual "toughness 2" (Just 2) (Projection.toughnessOf victim after),
      HU.testCase "CR 122 counter persists through cleanup (vs Giant Growth wearing off)" $
        -- After a cleanup step, the +1/+1 counter is still on the Piker.
        let base = S.landsInPlay (Cards.forestPrinting cards) 1
            (victim, withFoe) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            (gs, spellId) = S.handOne (Cards.battlegrowthPrinting cards) withFoe
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
            afterCleanup = Projection.dropEndOfTurnEffects resolved
         in do
              HU.assertEqual "still 3/2 after cleanup" (Just 3) (Projection.powerOf victim afterCleanup)
              HU.assertEqual "still 3/2 after cleanup" (Just 2) (Projection.toughnessOf victim afterCleanup),
      HU.testCase "CR 122.6 Instill Infection puts a -1/-1 counter and draws" $
        -- alice casts Instill Infection on bob's Piker; Piker becomes 1/0 and dies
        -- (704.5f); alice draws a card.
        let base = S.landsInPlay (Cards.swampPrinting cards) 4
            (_, withFoe) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            (gs0, spellId) = S.handOne (Cards.instillInfectionPrinting cards) withFoe
            -- put a card in alice's library so the draw has something to find.
            (_, gs) = S.addLibraryCard (Cards.forestPrinting cards) S.alice gs0
            handBefore = S.handSize S.alice gs
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = Sba.checkStateBasedActions (snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop))
         in do
              HU.assertEqual "Piker died to the -1/-1 counter (704.5f)" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "alice drew a card" (handBefore + 1) (S.handSize S.alice after),
      HU.testCase "CR 704.5q both counter kinds on one creature annihilate; net 2/1 survives" $
        -- Both counters on the same creature (placed directly); the SBA removes both.
        let base = S.landsInPlay (Cards.forestPrinting cards) 5
            (victim, withFoe) = S.addCreature (Cards.pikerPrinting cards) S.alice base
            gs1 = S.addCounter CounterKind.PlusOnePlusOne 1 victim withFoe
            gs2 = S.addCounter CounterKind.MinusOneMinusOne 1 victim gs1
            after = Sba.checkStateBasedActions gs2
         in do
              HU.assertEqual "creature survives (net 2/1)" 1 (S.creaturesInPlay S.alice after)
              HU.assertEqual "no counters remain" Map.empty (maybe (Map.fromList [(CounterKind.PlusOnePlusOne, 99)]) Object.counters (Game.lookupObject victim after)),
      HU.testCase "CR 122.2 Unsummon removes a counter-bearing creature's counters" $
        let base = S.landsInPlay (Cards.islandPrinting cards) 1
            (victim, withFoe) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            withCounter = S.addCounter CounterKind.PlusOnePlusOne 1 victim withFoe
            (gs, spellId) = S.handOne (Cards.unsummonPrinting cards) withCounter
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
            -- Total (no `head`): expect exactly one bounced card in hand, empty counters.
            handCounters = map (\h -> maybe (Map.fromList [(CounterKind.PlusOnePlusOne, 99)]) Object.counters (Game.lookupObject h after)) (Game.zoneMembers Zone.Hand S.bob after)
         in HU.assertEqual "the bounced incarnation in hand has no counters" [Map.empty] handCounters
    ]
```

Add imports to `ResolveSpec` as needed: `Pawl.Type.CounterKind as CounterKind`, `Data.Map.Strict as Map` (present), `Pawl.Projection` (present). Verify `S.addLibraryCard`, `S.handSize`, `Game.zoneMembers`, `Zone.Hand` are the right names (all seen in Support/ResolveSpec).

- [x] **Step 2: Run test to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Cards.battlegrowthPrinting`/`instillInfectionPrinting` not in scope (and, once wired, the loader will fail until the JSON files exist).

- [x] **Step 3a: Author the card JSON files**

`data/cards/battlegrowth.json` (model on `tome-scour.json`/`murder.json`; `Instant`, single green pip, one `PutCounters`, one `CreatureTarget` slot):

```json
{"name":"Battlegrowth","manaCost":[{"type":"OfType","value":{"type":"Colored","value":{"type":"Green"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Instant"}],"subtypes":[]},"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"effects":[{"type":"PutCounters","value":[{"type":"PlusOnePlusOne"},{"type":"Literal","value":1},"target"]}],"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"targetSpecs":[{"slot":"target","spec":{"type":"CreatureTarget"}}]}
```

`data/cards/instill-infection.json` (`{3}{B}`, `PutCounters` then `Draw`):

```json
{"name":"Instill Infection","manaCost":[{"type":"Generic","value":3},{"type":"OfType","value":{"type":"Colored","value":{"type":"Black"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Instant"}],"subtypes":[]},"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"effects":[{"type":"PutCounters","value":[{"type":"MinusOneMinusOne"},{"type":"Literal","value":1},"target"]},{"type":"Draw","value":{"type":"Literal","value":1}}],"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"targetSpecs":[{"slot":"target","spec":{"type":"CreatureTarget"}}]}
```

Exact key names/shapes must match the codec — if the loader rejects a file (Step 4), diff against a known-good card (`murder.json` for cost/typeLine/targetSpecs, `tome-scour.json` for a `[slot, Literal]` effect value) and against `counterKindToJson`'s tag strings from Task 2. The `allPrintings` round-trip (Step 4) is the authority: it must render byte-identically.

- [x] **Step 3b: Wire the cards into `Cards.hs`**

Add two record fields to `data Cards`:

```haskell
    battlegrowthPrinting :: Printing.Printing,
    instillInfectionPrinting :: Printing.Printing,
```

Add two loader lines in `loadCards` (before the `pure MkCards`):

```haskell
  battlegrowthPrinting_ <- loadPrinting "battlegrowth"
  instillInfectionPrinting_ <- loadPrinting "instill-infection"
```

Add the two bindings in the `MkCards { … }` record and the two entries in `allPrintings`:

```haskell
        battlegrowthPrinting = battlegrowthPrinting_,
        instillInfectionPrinting = instillInfectionPrinting_,
```
```haskell
    battlegrowthPrinting cards,
    instillInfectionPrinting cards,
```

- [x] **Step 3c: Deck swaps (4-for-4, keep 60)**

In `greenDeck`, change `(warMammothPrinting cards, 12)` to `8` and add:

```haskell
        -- Battlegrowth swaps in for four War Mammoths (deck stays 60; card-backed
        -- conservation stays 120) so random green games exercise +1/+1 counters.
        (battlegrowthPrinting cards, 4),
```

In `blackDeck`, change `(typhoidRatsPrinting cards, 12)` to `8` and add:

```haskell
        -- Instill Infection swaps in for four Typhoid Rats (deck stays 60) so random
        -- black games exercise -1/-1 counters and the CR 704.5q annihilation SBA.
        (instillInfectionPrinting cards, 4),
```

- [x] **Step 3d: Retire the synthetic −0/−1 fixture**

In `source/test-suite/Pawl/ResolveSpec.hs` lines ~616–632, the two "CR 704.5f … does NOT save" tests use `withEffect … (Modification.ModifyPowerToughness (Quantity.Literal 0) (Quantity.Literal (-1)))`. Replace that synthetic continuous effect with a real −1/−1 counter, keeping each test's assertion intact:

- Indestructible test (Myr 0/1): replace `zeroed = withEffect myrId … (Literal 0)(Literal (-1)) gs` with `zeroed = S.addCounter CounterKind.MinusOneMinusOne 1 myrId gs`.
- Regeneration test (Piker 2/1): replace `zeroed = withEffect victim … (Literal 0)(Literal (-1)) gs` with `zeroed = S.addCounter CounterKind.MinusOneMinusOne 1 victim gs`.

Update the two code comments from "A test-local -0/-1 drops …" to "A real -1/-1 counter drops … (CR 122.1a); 704.5f is a put-into-graveyard, …". Leave every assertion unchanged — the outcome (dies despite indestructible / despite the shield) is identical.

- [x] **Step 4: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — the `Counters` group, the retired 704.5f tests, and the `allPrintings` honesty round-trip (`CardsSpec`/`PropertySpec`) including the two new cards. The card-backed conservation property stays 120.

If the round-trip fails on a new card, fix its JSON to render byte-identically (Step 3a).

- [x] **Step 5: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4f: Battlegrowth + Instill Infection gates, deck coverage, retire synthetic -0/-1"
```

---

## Task 6: Docs — rules correction, milestone log, status

**Files:**
- Modify: `docs/design.md` (the M4f row)
- Modify: `docs/progress.md` (milestone-completion entry)
- Modify: `CLAUDE.md` (current-work note)

**Interfaces:** none (documentation only).

- [x] **Step 1: Correct the design.md M4f row**

In `docs/design.md` §3's M4 split table, change the M4f row's "New machinery" cell from "+1/+1 as persistent permanent state, layer 7d (below Giant Growth's 7c)" to "+1/+1 as persistent permanent state, **layer 7c** (CR 613.4c — the same sublayer as Giant Growth; 7d is P/T switching)". Keep the gate/falsifier cell.

- [x] **Step 2: Add the milestone-completion entry to progress.md**

Append one entry to `docs/progress.md` in the established house style (model on the M4e entry): gate cards (Battlegrowth + Instill Infection); the decision proved (counters as persistent per-incarnation typed counts, projected at layer 7c — CR 613.4c corrected from design.md's 7d); the types/opcodes added (`CounterKind`; `Object.counters`; `Effect.PutCounters`; the `gather` 7c injection; the CR 704.5q / 122.3 annihilation SBA); the falsifier (704.5q forces typed counts over a net Integer; persistence vs. Giant Growth); the retirement (synthetic −0/−1 fixture, the M4b/M4d expiry, cashed); and the named deferred expiries verbatim from spec §9. Cite the spec and this plan at the end.

- [x] **Step 3: Update the CLAUDE.md current-work note**

In `CLAUDE.md`'s "Current work and tracking" section, update the milestone list: mark M4f complete (a one-clause summary mirroring the M4e clause) and set **M4g (modal) as next** per the design.md M4 table.

- [x] **Step 4: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4f: milestone completion log entry + design.md 7c correction + status"
```

---

## Self-Review

**Spec coverage:**
- CounterKind classification (spec §1) → Task 1. ✓
- `Object.counters` storage, per-incarnation reset, cleanup-exempt (§2) → Task 1 (field + changeZone reset); cleanup-exempt is proven by Task 5's persistence test (cleanup only drops `continuousEffects`, never `counters`). ✓
- `Effect.PutCounters` + five classifications + `applyEffect` + codec (§3) → Task 2. ✓
- Projection 7c injection + additive-commutativity (§4) → Task 3. ✓
- CR 704.5q annihilation SBA (§5) → Task 4. ✓
- Cards + fixture mana base + deck posture (§6) → Task 5. ✓
- Tests (§7): CounterKind/Object → Task 1; gate → Task 5; persistence → Task 5; lethal −1/−1 retiring synthetic → Task 5 (Step 3d) + the Instill test; annihilation → Task 4 (unit) + Task 5 (gate); zone-change reset → Task 1 (unit) + Task 5 (Unsummon); 7c stacking → Task 3; round-trip → Task 5 (`allPrintings`). ✓
- Layer correction (spec header) → Task 6. ✓
- Named deferred expiries (§9) → recorded in Task 6's progress entry. ✓

**Placeholder scan:** No "TBD"/"handle edge cases"/"similar to". Every code step shows code; every test step shows the assertion. Two intentional *verify-against-codebase* notes (the `SlotName` constructor name, the `withEffect`/untag-helper names) are lookups with an exact `grep`, not placeholders — the surrounding code is complete.

**Type consistency:** `putCounters`/`addCounter` share the `Map.insertWith (+) kind n` shape; `counterKindToJson` tag strings (`"PlusOnePlusOne"`/`"MinusOneMinusOne"`) match the `jsonToCounterKind` cases and the card JSON `{"type":"PlusOnePlusOne"}`; `Effect.PutCounters CounterKind Quantity SlotName` field order is identical in the type, the codec Array `[counterKindToJson, quantityToJson, slotNameToJson]`, and the JSON `value:[kind, quantity, slot]`; `Projection.counterGathered` and `Sba.annihilateOne` both read `Map.findWithDefault 0 CounterKind.PlusOnePlusOne`. Consistent.
