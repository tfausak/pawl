# M4.5 P1 — Permanent Control Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make a permanent's controller a projected layer-2 characteristic (base ownership overridden by `SetController` continuous effects), so a player can gain control of a creature and attack with it, gated by Act of Treason.

**Architecture:** Control is *not* a base `Object` field — it is computed by `Projection.controllerOf` folding layer-2 `SetController` continuous effects (timestamp last-wins) over `Object.owner`, exactly as Giant Growth's P/T rides the projection. A new `Projection.controls` enumerates the battlefield permanents a player controls, and the "you control" call sites (attackers, blockers, mana sources, untap/settle, activations) switch to it from the owner-based `Game.zoneMembers Battlefield`. Two new opcodes — `GainControl` (installs the until-end-of-turn control effect and re-Sicks the creature, CR 302.6) and `Untap` — plus the reused layer-6 haste grant express Act of Treason. `Game.controllerOf` is deleted; everything routes through `Projection.controllerOf`.

**Tech Stack:** Haskell 2010 (GHC 9.14.1, no extensions beyond `GADTs`/`RankNTypes`/`NamedFieldPuns`), tasty (`tasty-hunit` + `tasty-quickcheck`), the hand-rolled JSON codec (`Pawl.Codec`/`Pawl.Json`), cards as `data/cards/*.json`.

## Global Constraints

- **Warning-clean build, `-Werror` via the `pedantic` flag.** Every task ends green under `cabal build all --enable-tests --enable-benchmarks`. Incremental builds hide warnings from unchanged modules; when in doubt `cabal clean` first.
- **Two invariants outrank this plan.** The rules core never cases on a card's *identity*, only classifications: `Pawl.Resolve` is the sole `case effect of` home; `Pawl.Projection` is the sole `case … Modification` home (so `SetController` is applied only in `Projection`, and `Pawl.Codec` may case on it purely as serialization, the standing exception). The engine never makes a player's choice — `GainControl`'s new controller is *derived* (the source's controller, CR 611.2c), never prompted.
- **Haskell 2010, project style:** one type per `Pawl.Type.<Name>` module; qualified imports aliased to the last component; operators unqualified; `newtype`/`Mk`-prefixed constructors, non-punning; **no partial functions**; **no list comprehensions**; `case` over point-free; `let` over `where`; explicit `Text` not `String`; arbitrary-precision numbers. Derive at least `Eq` and `Show`.
- **Rules discipline:** every rules claim is checked against `docs/rules.txt` and the CR number cited in the code comment. Never trust recalled Magic rules. **The spec marks all CR numbers *(verify)*; verify each before it drives code** (control = CR 108.4 / 110.2; control-changing effects = layer 2 / CR 613.1b; summoning sickness = CR 302.6; the effect's set fixed at creation = CR 611.2c; until-end-of-turn wear-off = CR 514.2; untap keyword action = CR 701.20).
- **TDD, one commit per task.** Write the failing test, run it to watch it fail, implement minimally, run it green, then run `hooky`. Never weaken an assertion or delete a test to pass.
- **Formatting/lint before every commit:** `git add -A && hooky fix && git add -A && hooky run` (hooky acts on *staged* files only), then commit. Apply HLint suggestions or justify the exception.
- **Test/deck posture (spec §4):** Act of Treason is a **red deterministic fixture** (no random-game deck entry, the M3d posture), so CR 400.7 conservation counts are undisturbed. The sickness path is a **labeled synthetic scenario** (no card file) with a documented expiry naming the Auras/Control Magic phase.

**Spec:** `docs/superpowers/specs/2026-07-20-p1-permanent-control-design.md`. **Umbrella:** `docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`. Card to Scryfall-verify at Task 5: **Act of Treason** `{2}{R}` Sorcery — "Gain control of target creature until end of turn. Untap that creature. It gains haste until end of turn."

---

## File Structure

- **Modify** `source/library/Pawl/Type/Modification.hs` — add `SetController PlayerId` (layer-2 op); import `PlayerId`.
- **Modify** `source/library/Pawl/Type/Effect.hs` — add `GainControl Duration SlotName` and `Untap SlotName`.
- **Modify** `source/library/Pawl/Projection.hs` — `layer` arm for `SetController`; `isSet` and `rewriteModification` arms; new `controllerOf` and `controls`.
- **Modify** `source/library/Pawl/Game.hs` — delete `controllerOf` (moved to `Projection`).
- **Modify** `source/library/Pawl/Resolve.hs` — `applyEffect` arms for `GainControl` and `Untap`; their five classification arms (`slotsOf`/`readsX`/`manaProduced`/`searchesLibrary`/`rewriteEffect`); switch the two source-controller reads (`resolveSpell`/`resolveAbility`) from `Object.owner` to `Projection.controllerOf`.
- **Modify** `source/library/Pawl/Combat.hs` — `legalAttackers`/`legalBlockers` candidate set → `Projection.controls`.
- **Modify** `source/library/Pawl/Mana.hs` — mana-source enumeration → `Projection.controls`; `tapForMana` routes produced mana to `Projection.controllerOf`, not `Object.owner`.
- **Modify** `source/library/Pawl/Engine.hs` — `untapAll`/`settleAll` → `Projection.controls`.
- **Modify** `source/library/Pawl/Action.hs` — battlefield-permanent enumeration → `Projection.controls`; add the `Pawl.Projection` import.
- **Modify** `source/library/Pawl/Codec.hs` — `modificationToJson`/`jsonToModification` `SetController` arm (+ a `PlayerId` codec pair); `effectToJson`/`jsonToEffect` arms for `GainControl` and `Untap`.
- **Create** `data/cards/act-of-treason.json` — the gate card.
- **Modify** `source/test-suite/Pawl/Cards.hs` — load `act-of-treason`, add the record field, add it to `allPrintings`.
- **Modify** `source/test-suite/Pawl/{ProjectionSpec,ResolveSpec,CombatSpec,ManaSpec,CodecSpec}.hs` — tests (create a `ProjectionSpec`/`ManaSpec` group if absent — check `Main.hs`).
- **Modify** `docs/progress.md`, `docs/design.md` marker if needed, `CLAUDE.md` — completion note; retire the `Sickness` "EXPIRES at M3" comment.

---

## Task 1: The `SetController` layer-2 modification and the projected controller

**Files:**
- Modify: `source/library/Pawl/Type/Modification.hs`
- Modify: `source/library/Pawl/Projection.hs`
- Modify: `source/library/Pawl/Game.hs`
- Modify: `source/library/Pawl/Codec.hs`
- Test: `source/test-suite/Pawl/ProjectionSpec.hs`

**Interfaces:**
- Produces: `Modification.SetController :: PlayerId -> Modification`; `Projection.controllerOf :: ObjectId -> GameState -> Maybe PlayerId`; `Projection.controls :: PlayerId -> GameState -> [ObjectId]`.
- Consumes: `GameState.continuousEffects :: [ContinuousEffect]`; `ContinuousEffect.{modification,affected,timestamp}`; `Affected.TheseObjects`; `Game.{lookupObject,freshTimestamp}`; `GameState.battlefield :: Set ObjectId`.

- [x] **Step 1: Confirm the exhaustive-match footprint of `Modification`**

Run: `rg -n 'Modification\.GainKeyword|case .* of' source/library/Pawl/Projection.hs source/library/Pawl/Codec.hs`
Expected: the functions that pattern-match every `Modification` constructor are `Projection.layer`, `Projection.isSet`, `Projection.rewriteModification`, `Codec.modificationToJson`, `Codec.jsonToModification`. Each needs a `SetController` arm (Steps 5–6, 8). Note their line numbers.

- [x] **Step 2: Write the failing test for `controllerOf` (last-write-wins over `owner`)**

In `source/test-suite/Pawl/ProjectionSpec.hs`, add to the test group (mirror an existing `ProjectionSpec` case for helper style; `S.addPiker`, `Game.freshTimestamp`):

```haskell
HU.testCase "CR 108.4 a SetController effect overrides owner; last timestamp wins" $
  let (oid, base) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
      install pid g =
        let (ts, g1) = Game.freshTimestamp g
            eff = ContinuousEffect.MkContinuousEffect
              { ContinuousEffect.source = oid,
                ContinuousEffect.timestamp = ts,
                ContinuousEffect.duration = Duration.UntilEndOfTurn,
                ContinuousEffect.modification = Modification.SetController pid,
                ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
              }
         in g1 {GameState.continuousEffects = eff : GameState.continuousEffects g1}
      gs = install S.alice (install S.alice base) -- two effects, both -> alice
      owned = base
   in do
        HU.assertEqual "owner controls with no effect" (Just S.bob) (Projection.controllerOf oid owned)
        HU.assertEqual "the effect grants control" (Just S.alice) (Projection.controllerOf oid gs)
        HU.assertEqual "alice controls oid" [oid] (Projection.controls S.alice gs)
        HU.assertEqual "bob controls nothing" [] (Projection.controls S.bob gs)
```

Add imports as needed: `Pawl.Type.ContinuousEffect`, `Pawl.Type.Affected`, `Pawl.Type.Duration`, `Pawl.Type.Modification`, `Data.Set`, `Pawl.Game`, `Pawl.Type.GameState`, `Pawl.Projection`.

- [x] **Step 3: Run the test to verify it fails**

Run: `cabal test --test-options='-p "SetController effect overrides owner"' 2>&1 | tail -20`
Expected: FAIL — `Modification.SetController` and `Projection.controllerOf`/`controls` are not in scope.

- [x] **Step 4: Add the `SetController` constructor**

In `source/library/Pawl/Type/Modification.hs`, add the import `import Pawl.Type.PlayerId (PlayerId)` and a constructor (with a CR-cited comment):

```haskell
  | -- layer 2, CR 613.1b: set this object's controller. The PlayerId is BAKED at
    -- effect creation (CR 611.2c) by Resolve.applyEffect (GainControl) -- it is
    -- the effect's source's controller, never chosen. Applied only by
    -- Projection.controllerOf. Never appears in card JSON (runtime-only).
    SetController PlayerId
```

- [x] **Step 5: Add the `layer`, `isSet`, and `rewriteModification` arms in `Projection`**

In `source/library/Pawl/Projection.hs`:
- `layer` — add `Modification.SetController _ -> Layer.Control`.
- `isSet` — add `Modification.SetController _ -> False` (it is not the land-subtype "set" that gates static-ability liveness; CR 305.7 is unrelated). Verify the existing `isSet` semantics at the line found in Step 1 and match them.
- `rewriteModification` (text-change, M3d) — add `Modification.SetController _ -> m` (a control op has no subtype words for CR 612 to rewrite; identity).

- [x] **Step 6: Add `controllerOf` and `controls`, delete `Game.controllerOf`**

In `source/library/Pawl/Projection.hs` add (note: no list comprehensions; `List.maximumBy` only on the non-empty branch, so it stays total):

```haskell
-- CR 108.4 / 613.1b: the controller of an object is its owner, overridden by
-- layer-2 SetController continuous effects (last timestamp wins, CR 613.7). A
-- lean fold, not the full ProjectedCharacteristics pass -- control feeds combat,
-- mana and priority and is needed before P/T. Projection is the sole applier of
-- SetController (the case-on-Modification invariant). Nothing when the id is
-- unknown. Replaces Game.controllerOf (the M1b owner stand-in, now cashed).
controllerOf :: ObjectId -> GameState -> Maybe PlayerId
controllerOf oid gs = case Game.lookupObject oid gs of
  Nothing -> Nothing
  Just obj ->
    let names a = case a of
          Affected.TheseObjects s -> Set.member oid s
          _ -> False
        setter eff = case ContinuousEffect.modification eff of
          Modification.SetController pid
            | names (ContinuousEffect.affected eff) -> Just (ContinuousEffect.timestamp eff, pid)
          _ -> Nothing
        setters = Maybe.mapMaybe setter (GameState.continuousEffects gs)
     in case setters of
          [] -> Just (Object.owner obj)
          _ -> Just (snd (List.maximumBy (Ord.comparing fst) setters))

-- The battlefield permanents a player controls (CR 108.4). The control-based
-- "your permanents" enumerator; consumers use it wherever they mean "you
-- control", replacing the owner-based Game.zoneMembers Battlefield.
controls :: PlayerId -> GameState -> [ObjectId]
controls pid gs = filter (\oid -> controllerOf oid gs == Just pid) (Set.toList (GameState.battlefield gs))
```

Add imports if absent: `qualified Data.List as List`, `qualified Data.Ord as Ord`, `qualified Data.Maybe as Maybe`, `qualified Pawl.Type.ContinuousEffect as ContinuousEffect`, `qualified Pawl.Type.Affected as Affected`, `qualified Pawl.Type.Object as Object`. Then **delete `controllerOf` from `source/library/Pawl/Game.hs`** (and its now-unused imports). The build will surface every caller; leave those for Task 4 except making Task 1 compile — temporarily, callers can qualify `Projection.controllerOf` where trivial, but the systematic switch is Task 4. (If deleting `Game.controllerOf` breaks too many modules to compile Task 1 in isolation, keep the delete and fix callers here as a mechanical rename `Game.controllerOf` → `Projection.controllerOf`; that rename is behavior-preserving because both return `owner` until an effect exists.)

- [x] **Step 7: Add the `SetController` codec arms**

In `source/library/Pawl/Codec.hs`, add `import qualified Pawl.Type.PlayerId as PlayerId` and a `PlayerId` codec pair (mirroring `quantityToJson`'s `Literal` arm — `Json.jInt` builds the number, `Json.asInteger` reads it):

```haskell
playerIdToJson :: PlayerId.PlayerId -> Value
playerIdToJson (PlayerId.MkPlayerId n) = Json.jInt n

jsonToPlayerId :: Value -> Either Text PlayerId.PlayerId
jsonToPlayerId value = (PlayerId.MkPlayerId . fromInteger) <$> Json.asInteger value
```

Then in `modificationToJson`: `Modification.SetController p -> Json.tagged (Text.pack "SetController") (Just (playerIdToJson p))`; in `jsonToModification`: `"SetController" -> withValue mv (fmap Modification.SetController . jsonToPlayerId)`. (These keep the codec total; `SetController` never appears in real card JSON, so the `allPrintings` round-trip never exercises them, but exhaustiveness requires the arms.)

- [x] **Step 8: Build and run the test to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -5 && cabal test --test-options='-p "SetController effect overrides owner"' 2>&1 | tail -12`
Expected: build warning-clean; test PASS.

- [x] **Step 9: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(m4.5-p1): SetController layer-2 modification + Projection.controllerOf/controls"
```

---

## Task 2: The `Untap` opcode

**Files:**
- Modify: `source/library/Pawl/Type/Effect.hs`
- Modify: `source/library/Pawl/Resolve.hs`
- Modify: `source/library/Pawl/Codec.hs`
- Test: `source/test-suite/Pawl/ResolveSpec.hs`

**Interfaces:**
- Produces: `Effect.Untap :: SlotName -> Effect card`.
- Consumes: `Resolve.applyEffect`; `Recipient.ToCreature`/`recipientObject`; `TapState.Untapped`.

- [x] **Step 1: Write the failing test (applyEffect untaps a tapped target)**

In `ResolveSpec.hs` (mirror the `applyEffect`-driving style; a tapped creature, then apply `Untap`):

```haskell
HU.testCase "CR 701.20 Untap untaps the slot's target" $
  let (oid, base0) = S.addCreature (Cards.pikerPrinting cards) S.bob (Setup.emptyGame S.bothPlayers)
      base = S.tapObject oid base0 -- helper: set Object.tapped = Tapped (add to Support if absent)
      slot = SlotName.MkSlotName (Text.pack "target")
      run = Resolve.applyEffect oid S.alice Map.empty
              (Map.singleton slot True)
              (Map.singleton slot (Recipient.ToCreature oid))
              (Effect.Untap slot)
      after = snd (Engine.runGamePure S.identityAnswer base run)
   in HU.assertEqual "target is untapped" (Just TapState.Untapped) (fmap Object.tapped (Game.lookupObject oid after))
```

If `S.tapObject` does not exist, add it to `Support.hs`: `tapObject oid gs = gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)}`.

- [x] **Step 2: Run to verify it fails**

Run: `cabal test --test-options='-p "Untap untaps"' 2>&1 | tail -15`
Expected: FAIL — `Effect.Untap` not in scope.

- [x] **Step 3: Add the `Untap` constructor**

In `source/library/Pawl/Type/Effect.hs`:

```haskell
  | -- CR 701.20: untap the slot's target permanent. Single-target (Act of
    -- Treason's "untap that creature"); mass/conditional untap is future.
    Untap SlotName
```

- [x] **Step 4: Add the `applyEffect` arm and the five classifications**

In `source/library/Pawl/Resolve.hs`, add to `applyEffect` (mirror the `Destroy`/`MoveToZone` slot-and-legality pattern):

```haskell
  Effect.Untap slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs
          Just target -> gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Untapped}) target (GameState.objects gs)}
        _ -> gs
```

Add the classification arms alongside the other opcodes: `slotsOf` includes `Untap slot -> [slot]`; `readsX (Untap _) = False`; `manaProduced (Untap _) = []`; `searchesLibrary (Untap _) = False`; `rewriteEffect … (Untap s) = Untap s` (identity — no text to rewrite). Match each function's exact shape at its existing arms.

- [x] **Step 5: Add the codec arms**

In `Codec.hs`: `effectToJson` → `Effect.Untap s -> Json.tagged (Text.pack "Untap") (Just (slotNameToJson s))`; `jsonToEffect` → `"Untap" -> withValue mv (fmap Effect.Untap . jsonToSlotName)`.

- [x] **Step 6: Build and run to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -5 && cabal test --test-options='-p "Untap untaps"' 2>&1 | tail -10`
Expected: build clean; test PASS.

- [x] **Step 7: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(m4.5-p1): Untap opcode"
```

---

## Task 3: The `GainControl` opcode

**Files:**
- Modify: `source/library/Pawl/Type/Effect.hs`
- Modify: `source/library/Pawl/Resolve.hs`
- Modify: `source/library/Pawl/Codec.hs`
- Test: `source/test-suite/Pawl/ResolveSpec.hs`

**Interfaces:**
- Produces: `Effect.GainControl :: Duration -> SlotName -> Effect card`.
- Consumes: `applyEffect`'s `controller` argument (the source's controller — the new controller); `ContinuousEffect.MkContinuousEffect`; `Modification.SetController`; `Game.freshTimestamp`; `Object.sickness`; `Sickness.Sick`.

- [x] **Step 1: Write the failing test (GainControl installs control + re-Sicks)**

In `ResolveSpec.hs`:

```haskell
HU.testCase "GainControl gives the source's controller control until end of turn and re-Sicks (CR 302.6)" $
  let (oid, base) = S.addCreature (Cards.pikerPrinting cards) S.bob (Setup.emptyGame S.bothPlayers)
      slot = SlotName.MkSlotName (Text.pack "target")
      -- Apply as though a spell alice controls (controller = alice) resolved it.
      run = Resolve.applyEffect oid S.alice Map.empty
              (Map.singleton slot True)
              (Map.singleton slot (Recipient.ToCreature oid))
              (Effect.GainControl Duration.UntilEndOfTurn slot)
      after = snd (Engine.runGamePure S.identityAnswer base run)
   in do
        HU.assertEqual "alice now controls it" (Just S.alice) (Projection.controllerOf oid after)
        HU.assertEqual "it is summoning sick for the new controller" (Just Sickness.Sick) (fmap Object.sickness (Game.lookupObject oid after))
        HU.assertEqual "control reverts after cleanup" (Just S.bob) (Projection.controllerOf oid (Projection.dropEndOfTurnEffects after))
```

- [x] **Step 2: Run to verify it fails**

Run: `cabal test --test-options='-p "GainControl gives"' 2>&1 | tail -15`
Expected: FAIL — `Effect.GainControl` not in scope.

- [x] **Step 3: Add the `GainControl` constructor**

In `source/library/Pawl/Type/Effect.hs`:

```haskell
  | -- CR 613.1b / 611.2c: install a layer-2 control effect on the slot's target
    -- for a duration. The new controller is THIS effect's source's controller
    -- (the `controller` passed to applyEffect), baked into a stored
    -- SetController continuous effect -- derived, never chosen. Also re-Sicks the
    -- target (CR 302.6: the new controller has not controlled it continuously).
    -- Act of Treason's control clause. NOT a reuse of ModifyTarget, whose
    -- Modification is static card data and cannot carry a resolution-time
    -- PlayerId. Permanent control (CR 613), distinct from Mindslaver's
    -- player-control (CR 723, ControlPlayerNextTurn).
    GainControl Duration SlotName
```

- [x] **Step 4: Add the `applyEffect` arm and the five classifications**

In `applyEffect` (mirror the `ModifyTarget` arm's ContinuousEffect construction, but bake `controller` and set `Sick`):

```haskell
  Effect.GainControl duration slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs
          Just target ->
            let (ts, gs1) = Game.freshTimestamp gs
                eff = ContinuousEffect.MkContinuousEffect
                  { ContinuousEffect.source = source,
                    ContinuousEffect.timestamp = ts,
                    ContinuousEffect.duration = duration,
                    ContinuousEffect.modification = Modification.SetController controller,
                    ContinuousEffect.affected = Affected.TheseObjects (Set.singleton target)
                  }
                sicken o = o {Object.sickness = Sickness.Sick}
             in gs1
                  { GameState.continuousEffects = eff : GameState.continuousEffects gs1,
                    GameState.objects = Map.adjust sicken target (GameState.objects gs1)
                  }
        _ -> gs
```

Classifications: `slotsOf` → `GainControl _ slot -> [slot]`; `readsX (GainControl _ _) = False`; `manaProduced (GainControl _ _) = []`; `searchesLibrary (GainControl _ _) = False`; `rewriteEffect … (GainControl d s) = GainControl d s` (identity). Ensure `Sickness`/`Modification`/`Affected`/`ContinuousEffect` are imported in `Resolve` (some already are).

- [x] **Step 5: Route the source-controller reads through the projection**

In `source/library/Pawl/Resolve.hs`, change the two sites that pass the effect's controller (found at `resolveSpell`/`resolveAbility`, ~lines 242 and 264) from `Object.owner obj` to the projected controller, so a **controlled** permanent's ability resolves under the thief (CR 613 / 608.2g):

```haskell
-- was: (Object.owner obj)
Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf oid gs)   -- resolveSpell (oid = the spell/permanent)
```

Apply the analogous change at the `resolveAbility` site (its source id). Behavior-preserving for spells (a spell has no controller effect), semantically required for a controlled permanent's ability. `Resolve` already imports `Projection` (it calls `Projection.hasKeyword`); add `qualified Data.Maybe as Maybe` if absent.

- [x] **Step 6: Add the codec arms**

`effectToJson` → `Effect.GainControl d s -> Json.tagged (Text.pack "GainControl") (Just (Array [durationToJson d, slotNameToJson s]))`; `jsonToEffect` → `"GainControl" -> case mv of { Just (Array [d, s]) -> Effect.GainControl <$> jsonToDuration d <*> jsonToSlotName s; _ -> Left (Text.pack "GainControl expects [duration, slot]") }` (match the module's existing two-field decode style, e.g. `ModifyTarget`).

- [x] **Step 7: Build and run to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -5 && cabal test --test-options='-p "GainControl gives"' 2>&1 | tail -12`
Expected: build clean; test PASS.

- [x] **Step 8: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(m4.5-p1): GainControl opcode + projected source controller in Resolve"
```

---

## Task 4: Switch the "you control" call sites off owner

**Files:**
- Modify: `source/library/Pawl/Combat.hs`, `source/library/Pawl/Mana.hs`, `source/library/Pawl/Engine.hs`, `source/library/Pawl/Action.hs`
- Test: `source/test-suite/Pawl/CombatSpec.hs`, `source/test-suite/Pawl/ManaSpec.hs`

**Interfaces:**
- Consumes: `Projection.controls`, `Projection.controllerOf`.
- Produces: control-based `Combat.legalAttackers`/`legalBlockers`, `Mana` sources + routing, `Engine.untapAll`/`settleAll`, `Action` permanent enumeration.

- [x] **Step 1: Write the failing tests (thief attacks/taps a controlled permanent)**

In `CombatSpec.hs`:

```haskell
HU.testCase "CR 508.1a a player can attack with a creature they control but do not own" $
  let (oid, base) = S.addCreature (Cards.pikerPrinting cards) S.bob (Setup.emptyGame S.bothPlayers)
      gs0 = S.giveControl oid S.alice base -- helper: install a SetController effect + set haste/Settled so canAttack is testable
   in do
        HU.assertBool "alice may attack with it" (oid `elem` Combat.legalAttackers S.alice gs0)
        HU.assertBool "bob may not (not the controller, not active)" (not (oid `elem` Combat.legalAttackers S.bob gs0))
```

Add `S.giveControl` to `Support.hs` — install a `SetController` continuous effect (like the ProjectionSpec test) making `pid` control `oid`, and set the object `sickness = Settled` (so the attack test isolates control, not sickness):

```haskell
giveControl :: ObjectId.ObjectId -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
giveControl oid pid gs =
  let (ts, g1) = Game.freshTimestamp gs
      eff = ContinuousEffect.MkContinuousEffect
        { ContinuousEffect.source = oid, ContinuousEffect.timestamp = ts,
          ContinuousEffect.duration = Duration.UntilEndOfTurn,
          ContinuousEffect.modification = Modification.SetController pid,
          ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid) }
      settle o = o {Object.sickness = Sickness.Settled}
   in g1 { GameState.continuousEffects = eff : GameState.continuousEffects g1,
           GameState.objects = Map.adjust settle oid (GameState.objects g1) }
```

In `ManaSpec.hs` (create the group + wire into `Main.hs` if absent), the mana-routing test:

```haskell
HU.testCase "mana from a controlled permanent goes to its controller, not owner" $
  let (oid, base) = S.addCreature (Cards.llanowarElvesPrinting cards) S.bob (Setup.emptyGame S.bothPlayers)
      gs0 = S.giveControl oid S.alice base
      after = Mana.tapForMana oid gs0
   in do
        HU.assertBool "alice received a mana unit" (not (null (manaUnitsOf (Mana.poolOf S.alice after))))
        HU.assertBool "bob received none" (null (manaUnitsOf (Mana.poolOf S.bob after)))
```

(`manaUnitsOf` unwraps `Mana.MkMana units`; use the existing accessor — check `Pawl.Type.Mana`.)

- [x] **Step 2: Run to verify they fail**

Run: `cabal test --test-options='-p "attack with a creature they control"' 2>&1 | tail -15`
Expected: FAIL — `legalAttackers` builds its candidates from `Game.zoneMembers Battlefield` (owner-based), so a bob-owned creature is never offered to alice.

- [x] **Step 3: Switch `Combat`**

`legalAttackers pid gs = filter (\oid -> canAttack pid oid gs) (Projection.controls pid gs)` and the same for `legalBlockers` (using `canBlock`). `Combat` already imports `Projection`.

- [x] **Step 4: Switch `Mana`**

In the mana-source enumeration (`Mana.hs:115`): `filter isSource (Projection.controls pid gs)`. In `tapForMana` (`Mana.hs:~132`): route the produced mana to the controller —

```haskell
       in addMana (Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf oid gs)) [ManaUnit.MkManaUnit {ManaUnit.manaType = produced}] gs1
```

`Mana` already imports `Projection`; add `qualified Data.Maybe as Maybe` if absent.

- [x] **Step 5: Switch `Engine` and `Action`**

`Engine.untapAll`/`settleAll`: replace `Game.zoneMembers Zone.Battlefield pid gs` with `Projection.controls pid gs` (both already have `gs` in scope; `Engine` imports `Projection`). `Action` (`Action.hs:42`, the battlefield-permanent enumeration for activations): replace with `Projection.controls pid gs` and add `import qualified Pawl.Projection as Projection`. **Leave the `Zone.Hand` enumerations (`Action.hs:29`, `Engine.hs:126`) and `Target.hs:40` unchanged** — hand is owner-relative, and `Target` builds a union over players (owner vs control gives the same set).

- [x] **Step 6: Build and run all tests**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -5 && cabal test 2>&1 | tail -20`
Expected: build clean; the two new tests PASS and the **full existing suite stays green** (the switch is behavior-preserving until a control effect exists, which no existing fixture installs).

- [x] **Step 7: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(m4.5-p1): route you-control call sites through Projection.controls"
```

---

## Task 5: Act of Treason — the card and the gate scenario

**Files:**
- Create: `data/cards/act-of-treason.json`
- Modify: `source/test-suite/Pawl/Cards.hs`
- Test: `source/test-suite/Pawl/ResolveSpec.hs` (gate scenario), `source/test-suite/Pawl/CodecSpec.hs` (round-trip, if it enumerates cards explicitly)

**Interfaces:**
- Consumes: the loader `Cards.loadPrinting`, `Cards.allPrintings`; `Cast.castSpell`, `Stack.resolveTop`, `Engine.runGamePure`, `S.handOne`, `S.landsInPlay`, `S.addCreature`, `Combat.legalAttackers`.
- Produces: `Cards.actOfTreasonPrinting`.

- [x] **Step 1: Learn the card JSON schema from templates**

Run: `cat data/cards/lightning-bolt.json data/cards/giant-growth.json data/cards/chaos-charm.json`
Expected: `lightning-bolt` shows a single-target instant (name, mana cost, type line, the `spell` modal wrapper with one mode, `DealDamage` effect, `AnyTarget` spec); `giant-growth` shows the `ModifyTarget`/`GainKeyword` effect shape; `chaos-charm` shows the modal `spell` structure with `effects` and `targetSpecs`. Note the exact keys.

- [x] **Step 2: Scryfall-verify Act of Treason**

Confirm against Scryfall: **Act of Treason**, `{2}{R}`, Sorcery, oracle text "Gain control of target creature until end of turn. Untap that creature. It gains haste until end of turn." Record the verification date in the card task comment.

- [x] **Step 3: Author `data/cards/act-of-treason.json`**

Match the template schema. The `spell` is one non-modal mode (`ChooseExactly 1`) whose `effects`, in order, are:
1. `GainControl` `UntilEndOfTurn` at slot `"target"`
2. `Untap` at slot `"target"`
3. `ModifyTarget` `UntilEndOfTurn` `(GainKeyword Haste)` at slot `"target"`

and whose `targetSpecs` maps `"target"` → `CreatureTarget`. Mana cost `{2}{R}`, type line Sorcery. (Author the JSON by hand from the templates; the round-trip in Step 6 is the safety net for any schema mismatch.)

- [x] **Step 4: Wire it into `Cards.hs`**

Add `actOfTreasonPrinting :: Printing.Printing` to the `MkCards` record; `actOfTreasonPrinting_ <- loadPrinting "act-of-treason"` in `loadCards`; the field assignment; and add `actOfTreasonPrinting cards` to `allPrintings`. **Do not** add it to any deck (red *deterministic fixture*, spec §4).

- [x] **Step 5: Write the gate scenario test**

In `ResolveSpec.hs` (mirror the Lightning-Bolt cast/resolve harness — `S.landsInPlay`, `S.handOne`, `Cast.castSpell`, `Stack.resolveTop`; `identityAnswer` targets bob's only creature via `lookupMin`):

```haskell
HU.testCase "Act of Treason: steal, untap, haste, attack, then revert" $
  let base0 = S.landsInPlay (Cards.mountainPrinting cards) 3            -- alice: {R}{R}{R} for {2}{R}
      (oid, base1) = S.addCreature (Cards.pikerPrinting cards) S.bob base0
      base = S.tapObject oid base1                                       -- start it tapped to prove the untap rider
      (gs1, spellId) = S.handOne (Cards.actOfTreasonPrinting cards) base
      cast = snd (Engine.runGamePure S.identityAnswer gs1 (Cast.castSpell S.alice spellId))
      resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
   in do
        HU.assertEqual "alice controls the Piker" (Just S.alice) (Projection.controllerOf oid resolved)
        HU.assertEqual "the untap rider untapped it" (Just TapState.Untapped) (fmap Object.tapped (Game.lookupObject oid resolved))
        HU.assertBool  "it has haste" (Projection.hasKeyword Keyword.Haste oid resolved)
        HU.assertBool  "alice may attack with it this turn" (oid `elem` Combat.legalAttackers S.alice resolved)
        HU.assertBool  "bob may not attack with it" (not (oid `elem` Combat.legalAttackers S.bob resolved))
        HU.assertEqual "control reverts at cleanup" (Just S.bob) (Projection.controllerOf oid (Projection.dropEndOfTurnEffects resolved))
```

- [x] **Step 6: Build and run (gate + round-trip)**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -5 && cabal test 2>&1 | tail -20`
Expected: build clean; the gate test PASSES and the `allPrintings` honesty round-trip (`jsonToCard . cardToJson ≡ Right`, now covering `act-of-treason.json`) stays green. If the round-trip fails, fix the JSON to match the codec (do **not** weaken the round-trip).

- [x] **Step 7: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(m4.5-p1): Act of Treason card + gate scenario (steal, untap, haste, attack, revert)"
```

---

## Task 6: The summoning-sickness synthetic scenario

**Files:**
- Test: `source/test-suite/Pawl/CombatSpec.hs`

**Interfaces:**
- Consumes: `Resolve.applyEffect` with `GainControl` (no haste rider); `Combat.canAttack`.

- [x] **Step 1: Write the synthetic test** (it passes once Tasks 3–4 land; write it and watch it go green)

This is the **labeled synthetic** for the sickness path (Act of Treason's haste masks it). Documented expiry in the test comment: retires when Control Magic / the Auras phase can test control-change sickness with a real indefinite-control card across two turns (spec §4, §7).

```haskell
-- SYNTHETIC (labeled crutch, spec §4): a "steal until end of turn, no haste"
-- effect. A real card would grant haste (masking CR 302.6) or be an Aura (Attach,
-- out of M4.5 scope). EXPIRES: Auras / Control Magic phase.
HU.testCase "CR 302.6 a creature that just changed control is summoning sick (no haste)" $
  let (oid, base) = S.addCreature (Cards.pikerPrinting cards) S.bob (Setup.emptyGame S.bothPlayers)
      slot = SlotName.MkSlotName (Text.pack "target")
      steal = Resolve.applyEffect oid S.alice Map.empty (Map.singleton slot True)
                (Map.singleton slot (Recipient.ToCreature oid))
                (Effect.GainControl Duration.UntilEndOfTurn slot)
      after = snd (Engine.runGamePure S.identityAnswer base steal)
   in do
        HU.assertEqual "alice controls it" (Just S.alice) (Projection.controllerOf oid after)
        HU.assertBool  "but it is summoning sick, so it cannot attack this turn" (not (Combat.canAttack S.alice oid after))
```

- [x] **Step 2: Run to verify it passes** (Tasks 3–4 already implement the behavior)

Run: `cabal test --test-options='-p "just changed control is summoning sick"' 2>&1 | tail -12`
Expected: PASS — `GainControl` set `Sick`, no haste, so `canAttack` is `False`.

- [x] **Step 3: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "test(m4.5-p1): synthetic control-change summoning-sickness scenario"
```

---

## Task 7: Documentation and tracking

**Files:**
- Modify: `source/library/Pawl/Type/Sickness.hs`, `docs/progress.md`, `CLAUDE.md`

- [x] **Step 1: Retire the `Sickness` "EXPIRES at M3" comment**

In `source/library/Pawl/Type/Sickness.hs`, update the comment: control-change sickness is now handled (P1) — `GainControl` re-Sicks the target (CR 302.6); the untap-step settle (`Engine.settleAll`, now controller-based) clears it. Note the remaining deferral: cross-turn settle under *indefinite* control is the Auras phase.

- [x] **Step 2: Add the P1 completion note to `docs/progress.md`**

Add an entry (open an "M4.5 (phased)" subsection if none exists) recording: gate Act of Treason; control is a projected layer-2 characteristic (`Modification.SetController`, `Projection.controllerOf`/`controls`), not a base field; `Game.controllerOf` deleted; opcodes `GainControl`/`Untap`; the `you-control` call-site switch and the `Mana` owner→controller fix; the sickness synthetic and its expiry; and the deferrals (§7 of the spec: Auras/indefinite control, instant-speed/mid-combat steal, CR 613.8 control dependency → `f90e0c4`, multiplayer CR 800.4, mass untap, control-at-base). Cite the spec and this plan.

- [x] **Step 3: Update `CLAUDE.md` current-work note**

Add a one-line note that M4.5 has begun and P1 (permanent control) is complete, pointing at the umbrella and progress log. Keep it to working guidance.

- [x] **Step 4: Final full build + suite**

Run: `cabal clean && cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -5 && cabal test 2>&1 | tail -20`
Expected: warning-clean; entire suite green.

- [x] **Step 5: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "docs(m4.5-p1): completion note, retire Sickness M3 expiry"
```

- [x] **Step 6: Update the git-bug** (per the user's "update the git bugs when we finish each part")

Re-point or annotate `83f1a55`'s GAP-L2 facet as addressed by P1 (control is now a projected layer-2 characteristic; `Object.controller`-less by design). Leave `f90e0c4` open (CR 613.8 control dependency, deferred).

```bash
git-bug bug comment 83f1a55 -m "GAP-L2 permanent control landed as M4.5 P1 (Act of Treason): control is a projected layer-2 characteristic via Projection.controllerOf + Modification.SetController. CR 613.8 control dependency remains open as f90e0c4."
```

---

## Self-Review notes (for the executor)

- **Spec coverage:** §2.1 projected-not-stored → Task 1; §2.2 control-in-Projection + `Game.controllerOf` deletion → Task 1; §2.3 battlefield membership → Task 4; §2.4 sickness → Tasks 3 (set Sick) + 4 (controller-based settle) + 6 (synthetic); §2.5 opcodes → Tasks 2/3; §2.6 mana fix → Task 4; §4 cards/tests → Tasks 5/6; §5 audit → Task 4 (switch sites) + Task 3 (Resolve controller) with `Target`/hand sites explicitly left owner-based; §7 deferrals + §8 tracking → Task 7.
- **Type consistency:** `Projection.controllerOf :: ObjectId -> GameState -> Maybe PlayerId` and `Projection.controls :: PlayerId -> GameState -> [ObjectId]` are used verbatim in Tasks 3–6. `Effect.GainControl Duration SlotName` and `Effect.Untap SlotName` match their `applyEffect`, codec, and card-JSON uses. `Modification.SetController PlayerId` matches its `layer`/`isSet`/`rewriteModification`/codec arms.
- **Verify-before-code:** every CR number in this plan is `(verify)` per the spec — check `docs/rules.txt` at the task that first cites it (701.20 untap, 108.4/110.2 control, 611.2c baking, 613.1b layer 2, 514.2 wear-off, 302.6 sickness).
