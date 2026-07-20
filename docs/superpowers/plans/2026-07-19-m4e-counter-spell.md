# M4e Counter Target Spell Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make Cancel (`{1}{U}{U}` Instant, "Counter target spell") work — the first effect that removes a spell from the stack — via a distinct `Counter` opcode, a `SpellTarget` spec, and an `Event.counter` funnel, all riding seams that already exist.

**Architecture:** A spell on the stack is an object (`Source.OfCard`); countering it (CR 701.6a) puts it into its owner's graveyard without resolving — mechanically a stack→graveyard `changeZone`, which already removes the id from `GameState.stack` and mints the graveyard incarnation (CR 400.7). Three small pieces layer onto that: a narrower `TargetSpec.SpellTarget` (stack spells only), a distinct `Effect.Counter SlotName` opcode (the M4b `Destroy` precedent — a rule-701 keyword action gets its own opcode), and an `Event.counter` funnel mirroring `Event.destroy`. The CR 608.2b fizzle and Rest in Peace composition come free from existing machinery.

**Tech Stack:** Haskell 2010 (GHC 9.14.1), tasty/tasty-hunit, hand-rolled JSON codec (`Pawl.Codec`), cards as `data/cards/*.json` loaded by the test suite.

## Global Constraints

Copied from the spec (`docs/superpowers/specs/2026-07-19-m4e-counter-spell-design.md`) and `CLAUDE.md`; every task's requirements include these:

- **Warning-clean under `-Weverything` minus the allow-list, with `+pedantic` (`-Werror`).** Every constructor added to `Effect`, `TargetSpec` triggers non-exhaustive-match errors until every exhaustive `case` gains its arm — the compiler is the checklist. Build `all`: `cabal build all --enable-tests --enable-benchmarks`.
- **`Pawl.Resolve` is the sole home of `case effect of`.** Its five classifiers (`slotsOf`, `readsX`, `manaProduced`, `searchesLibrary`, `rewriteEffect`) plus `applyEffect` each gain a `Counter` arm.
- **`Pawl.Event` is the sole home of casing on replacement/trigger classifications** and the home of the change-and-emit funnels; `Event.counter` lives here beside `destroy`/`changeZone`.
- **`Pawl.Target` is the sole home of targeting legality;** `Pawl.Codec` is the sole `Card ⇆ Json` authority.
- **The §1 invariant:** the rules core reads *classifications* (`Game.isSpell`, `SpellTarget` legality), never a card's identity. No `case card of`.
- **No new `Prompt`/`Response` type.** Cancel targets through the existing `ChooseTargets`; countering is unprompted. `DecisionLog` replays deterministically.
- **Cancel is covered by deterministic blue fixtures only — no random-game deck** (the M3d/M3f/M3g posture; do NOT add Cancel to `blueDeck`/any matchup). But Cancel **must** be added to `Cards.allPrintings` (the hygiene registry) so the honesty round-trip covers it.
- **No partial functions** (no `head`/`error`/non-exhaustive matches). `Text` not `String`. Constructors are `MkX`, non-punning. Qualified imports aliased to last component.
- **Every rules claim cites its CR number in a code comment**, checked against `docs/rules.txt` — never recalled.
- **TDD:** write each failing test, run it to watch it fail, implement minimally, run to green, commit. After `hooky`: `git add -A`, `hooky fix`, `git add -A`, `hooky run`.

---

## File Structure

- `source/library/Pawl/Type/TargetSpec.hs` — add `SpellTarget`.
- `source/library/Pawl/Type/Effect.hs` — add `Counter SlotName`.
- `source/library/Pawl/Game.hs` — add `isSpell` (the "is this stack object a spell?" classification).
- `source/library/Pawl/Target.hs` — `legalRecipients` arm for `SpellTarget`.
- `source/library/Pawl/Event.hs` — `counter` funnel.
- `source/library/Pawl/Resolve.hs` — five classifier arms + `applyEffect` `Counter` arm.
- `source/library/Pawl/Codec.hs` — `SpellTarget` arm (both directions) + `Counter` effect arm (both directions).
- `data/cards/cancel.json` — the Cancel printing.
- `source/test-suite/Pawl/Cards.hs` — `cancelPrinting` field, loader line, record line, `allPrintings` entry.
- `source/test-suite/Pawl/Support.hs` — `spellOnStack` shared fixture.
- `source/test-suite/Pawl/GameSpec.hs` — `isSpell` unit tests.
- `source/test-suite/Pawl/ResolveSpec.hs` — `SpellTarget` legality test (targetTests) + `counterTests` group (gate + falsifier).
- `source/test-suite/Pawl/EventSpec.hs` — `Event.counter` funnel + RiP composition tests.
- `source/test-suite/Pawl/CodecSpec.hs` — a targeted decode assertion (the `allPrintings` round-trip covers Cancel automatically).

No new library or test module, so **no `pawl.cabal` change** and no `other-modules` edit.

---

## Task 1: Target the stack — `SpellTarget` + `Game.isSpell`

**Files:**
- Modify: `source/library/Pawl/Type/TargetSpec.hs`
- Modify: `source/library/Pawl/Game.hs`
- Modify: `source/library/Pawl/Target.hs`
- Modify: `source/library/Pawl/Codec.hs:277-296`
- Modify: `source/test-suite/Pawl/Support.hs`
- Test: `source/test-suite/Pawl/GameSpec.hs` (objectFactTests), `source/test-suite/Pawl/ResolveSpec.hs` (targetTests)

**Interfaces:**
- Produces: `TargetSpec.SpellTarget`; `Game.isSpell :: ObjectId -> GameState -> Bool`; `Target.legalRecipients` handles `SpellTarget` returning `Set (Recipient.ToObject oid)` for stack spells; `Support.spellOnStack :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)`.

- [ ] **Step 1: Write the failing test (Game.isSpell in GameSpec)**

Add to the `objectFactTests` list in `source/test-suite/Pawl/GameSpec.hs` (after the token test at line 68, before the Mountain test):

```haskell
      HU.testCase "CR 112.1 isSpell is True for a spell on the stack, False off it" $
        let base = Setup.emptyGame S.bothPlayers
            (spellId, gs1) = S.spellOnStack (Cards.pikerPrinting cards) S.alice base
            (permId, gs2) = S.addPiker cards S.bob gs1
            tokenCard = Printing.card (Cards.pikerPrinting cards)
            (tokId, gs3) = S.addToken tokenCard S.bob gs2
         in do
              HU.assertBool "a card on the stack is a spell" (Game.isSpell spellId gs3)
              HU.assertBool "a battlefield permanent is not a spell" (not (Game.isSpell permId gs3))
              HU.assertBool "a token is not a spell" (not (Game.isSpell tokId gs3)),
```

(This test consumes `S.spellOnStack` from Step 5 — add that fixture first, then both tests.)

- [ ] **Step 2: Run it to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -i "isSpell\|not in scope"`
Expected: FAIL — `Variable not in scope: Game.isSpell`.

- [ ] **Step 3: Add `Game.isSpell`**

In `source/library/Pawl/Game.hs`, after `controllerOf` (near line 90), add:

```haskell
-- CR 112.1: a spell is a card on the stack. This asks the object's zone AND its
-- KIND (its Source) -- a classification like isPermanent (Stack.resolveTop), never
-- the card's identity. Only a card (OfCard) currently on the stack is a spell: a
-- token is never a spell, and a card off the stack (hand, a battlefield permanent,
-- graveyard) is not one either.
isSpell :: ObjectId -> GameState -> Bool
isSpell oid gs = case lookupObject oid gs of
  Nothing -> False
  Just obj ->
    Object.zone obj == Zone.Stack && case Object.source obj of
      Source.OfCard _ -> True
      Source.OfToken _ -> False
      Source.OfAbility _ _ -> False
      Source.OfTrigger _ _ -> False
```

(`Game.hs` already imports `Object`, `Source`, `Zone`, and defines `lookupObject` — no new imports.)

- [ ] **Step 4: Run the test to verify it passes**

Run: `cabal test --test-options='-p "isSpell"' 2>&1 | tail -5`
Expected: PASS (1 test).

- [ ] **Step 5: Write the failing test (SpellTarget legality in ResolveSpec)**

First add the shared fixture to `source/test-suite/Pawl/Support.hs` (near `handOne`), so both this test and later tasks can place a spell on the stack:

```haskell
-- Put a fresh `printing` spell (owned by `pid`) onto the stack: a Stack-zone
-- object added to GameState.stack (mirrors EventSpec's inline placement). Used to
-- set up counter targets without paying to cast the victim.
spellOnStack :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
spellOnStack printing pid gs =
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.bindings = Map.empty,
            Object.timestamp = ts
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.stack = oid : GameState.stack gs2
          }
      )
```

Thread `Game.freshTimestamp` (and use named-record syntax) exactly as every sibling helper (`addCreature`, `handOne`, `landsInPlay`) does — a hardcoded `Timestamp.MkTimestamp 0` would collide with objects the caller already placed and break the CR 613.7d layer-ordering invariant. (`Support.hs` already imports `Game`, `Object`, `Source`, `Zone`, `TapState`, `Sickness`, `Map`, `GameState`, `Printing`, `PlayerId`, `ObjectId` — verify and add any missing.)

Then add to the `targetTests` list in `source/test-suite/Pawl/ResolveSpec.hs` (after the `SpellOrPermanentTarget` test at line 131):

```haskell
      HU.testCase "CR 115 SpellTarget offers a stack spell but not a battlefield permanent" $
        let (permId, base) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
            (spellId, gs) = S.spellOnStack (Cards.lightningBoltPrinting cards) S.alice base
            legal = Target.legalRecipients TargetSpec.SpellTarget gs
         in do
              HU.assertBool "the stack spell is a legal target" (Set.member (Recipient.ToObject spellId) legal)
              HU.assertBool "the battlefield permanent is not a legal target" (not (Set.member (Recipient.ToObject permId) legal)),
```

- [ ] **Step 6: Run it to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -i "SpellTarget\|not in scope"`
Expected: FAIL — `Not in scope: data constructor 'TargetSpec.SpellTarget'`.

- [ ] **Step 7: Add the `SpellTarget` constructor**

In `source/library/Pawl/Type/TargetSpec.hs`, add before the closing `deriving`:

```haskell
  | -- CR 115: "target spell" -- an object on the stack that is a spell (a card on
    -- the stack, CR 112.1). Narrower than SpellOrPermanentTarget: Cancel cannot
    -- target a permanent or an ability. The first spec that reaches ONLY the stack.
    SpellTarget
```

- [ ] **Step 8: Add the `Target.legalRecipients` arm**

In `source/library/Pawl/Target.hs`, add to the `case spec of` (after the `CreatureOrEnchantmentTarget` arm, ~line 51):

```haskell
        TargetSpec.SpellTarget ->
          -- CR 112.1: only spells (Source.OfCard) on the stack; abilities and
          -- permanents are excluded by Game.isSpell.
          Set.fromList (map Recipient.ToObject (filter (\oid -> Game.isSpell oid gs) (GameState.stack gs)))
```

- [ ] **Step 9: Add the `Codec` arms for `SpellTarget`**

In `source/library/Pawl/Codec.hs`, add to `targetSpecToJson` (after line 284):

```haskell
  TargetSpec.SpellTarget -> "SpellTarget"
```

and to the `jsonToTargetSpec` list (after line 294, keeping the trailing comma correct):

```haskell
      (Text.pack "SpellTarget", TargetSpec.SpellTarget),
```

- [ ] **Step 10: Run the tests to verify they pass**

Run: `cabal test --test-options='-p "SpellTarget || isSpell"' 2>&1 | tail -5`
Expected: PASS.

- [ ] **Step 11: Full build + hooks**

Run: `cabal build all --enable-tests --enable-benchmarks && git add -A && hooky fix && git add -A && hooky run`
Expected: warning-clean build, hooks pass.

- [ ] **Step 12: Commit**

```bash
git add -A
git commit -m "M4e: SpellTarget spec + Game.isSpell -- target a spell on the stack

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 2: The `Counter` opcode + `Event.counter` funnel

**Files:**
- Modify: `source/library/Pawl/Type/Effect.hs`
- Modify: `source/library/Pawl/Event.hs`
- Modify: `source/library/Pawl/Resolve.hs` (slotsOf, readsX, manaProduced, searchesLibrary, rewriteEffect, applyEffect)
- Modify: `source/library/Pawl/Codec.hs:461-497`
- Test: `source/test-suite/Pawl/EventSpec.hs`

**Interfaces:**
- Consumes: `Support.spellOnStack` (Task 1).
- Produces: `Effect.Counter :: SlotName -> Effect card`; `Event.counter :: ObjectId -> GameState -> GameState`; `applyEffect` executes `Counter` via `Event.counter`.

- [ ] **Step 1: Write the failing test (Event.counter funnel + RiP compose)**

Add a new group to `source/test-suite/Pawl/EventSpec.hs`. First add the group's tests (place near the zone-change tests, e.g. after the `CR 608.2n ... resolving spell is exiled` test at line ~68):

```haskell
      HU.testCase "CR 701.6a Event.counter puts a countered spell into its owner's graveyard" $
        let (spellId, onStack) = S.spellOnStack (Cards.pikerPrinting cards) S.bob (Setup.emptyGame S.bothPlayers)
            after = Event.counter spellId onStack
         in do
              HU.assertEqual "off the stack" [] (GameState.stack after)
              HU.assertEqual "in bob's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after))
              HU.assertEqual "not on the battlefield" 0 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 614 a countered spell is exiled under Rest in Peace" $
        let (_, g0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (spellId, onStack) = S.spellOnStack (Cards.pikerPrinting cards) S.bob g0
            after = Event.counter spellId onStack
         in do
              HU.assertEqual "not in the graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.bob after))
              HU.assertEqual "exiled instead" 1 (length (Game.zoneMembers Zone.Exile S.bob after)),
```

(EventSpec already imports `Event`, `Game`, `Zone`, `GameState`, `Setup`, `S`, `Cards`. `S.creaturesInPlay` and `S.spellOnStack` are Support helpers.)

- [ ] **Step 2: Run it to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -i "Event.counter\|not in scope"`
Expected: FAIL — `Variable not in scope: Event.counter`.

- [ ] **Step 3: Add `Event.counter`**

In `source/library/Pawl/Event.hs`, after `destroy`/`regenerate` (near line 132), add:

```haskell
-- The single counter funnel (CR 701.6). A countered spell is removed from the
-- stack and put into its owner's graveyard (CR 701.6a) via changeZone -- so Rest
-- in Peace's redirect (graveyard->exile) and CR 400.7's new incarnation still
-- compose, exactly as they do for destroy. Ungated today: "can't be countered"
-- (CR 701.6) and a distinct "was countered" event are deferred (spec section 7),
-- as Event.destroy is ungated for CR 701.19c.
counter :: ObjectId -> GameState -> GameState
counter oid gs = case Game.lookupObject oid gs of
  Nothing -> gs
  Just _ -> changeZone oid Zone.Graveyard gs
```

(`Event.hs` already imports `Game`, `Zone`, and defines `changeZone` — no new imports.)

- [ ] **Step 4: Run the Event tests to verify they pass**

Run: `cabal test --test-options='-p "Event.counter || countered spell"' 2>&1 | tail -5`
Expected: PASS (2 tests).

- [ ] **Step 5: Write the failing test's next half — add the `Counter` opcode (compiler-driven)**

There is no isolated unit test for the opcode plumbing (it is exercised end-to-end by the Task 4 gate); the compiler's exhaustiveness is the check. Add the constructor to `source/library/Pawl/Type/Effect.hs`, before the closing `deriving` (after `RegenerateSelf`, ~line 97):

```haskell
  | -- CR 701.6: counter the slot's target spell -- remove it from the stack and
    -- put it into its owner's graveyard (CR 701.6a) via the Event.counter funnel,
    -- so it does not resolve. Distinct from MoveToZone slot Graveyard the way
    -- Destroy is (M4b): Counter is a keyword action on rule 701's list, and this is
    -- the future home of "can't be countered" and a distinct "was countered" event.
    Counter SlotName
```

- [ ] **Step 6: Run the build to see every exhaustive case that must gain a `Counter` arm**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -iE "Pattern match|non-exhaustive|Counter"`
Expected: FAIL — non-exhaustive-pattern errors in `Resolve.slotsOf`, `Resolve.readsX` (`effectReadsX`), `Resolve.manaProduced`, `Resolve.searchesLibrary`, `Resolve.rewriteEffect`, `Resolve.applyEffect`, and `Codec.effectToJson`/`jsonToEffect`.

- [ ] **Step 7: Add the five `Resolve` classifier arms + `applyEffect` arm**

In `source/library/Pawl/Resolve.hs`:

`slotsOf` (after the `RegenerateSelf` arm):
```haskell
  Effect.Counter slot -> Set.singleton slot
```

`readsX`'s inner `effectReadsX` (after `RegenerateSelf`):
```haskell
      Effect.Counter _ -> False
```

`manaProduced` (after `RegenerateSelf`):
```haskell
  Effect.Counter _ -> Nothing
```

`searchesLibrary` (after `RegenerateSelf`):
```haskell
  Effect.Counter _ -> False
```

`rewriteEffect` (after `RegenerateSelf`):
```haskell
  -- No rewritable land-type word.
  Effect.Counter _ -> effect
```

`applyEffect` (add a new arm, after `RegenerateSelf`):
```haskell
  Effect.Counter slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        -- CR 701.6a: the slot's target is a spell on the stack; counter it through
        -- the single funnel. A player recipient / illegal slot (CR 608.2b): no-op.
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs
          Just target -> Event.counter target gs
        _ -> gs
```

(`Resolve.hs` already imports `Event`, `State`, `Map`, `Set`, and defines `recipientObject` — no new imports.)

- [ ] **Step 8: Add the `Codec` effect arms for `Counter`**

In `source/library/Pawl/Codec.hs`, add to `effectToJson` (after the `Destroy` arm, line 470):
```haskell
  Effect.Counter s -> Json.tagged (Text.pack "Counter") (Just (slotNameToJson s))
```

and to `jsonToEffect`'s tag dispatch (after the `Destroy` arm, line 494):
```haskell
    "Counter" -> withValue mv (fmap Effect.Counter . jsonToSlotName)
```

- [ ] **Step 9: Run the build + Event tests to verify green**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "Event.counter || countered spell"' 2>&1 | tail -5`
Expected: warning-clean build; 2 tests PASS.

- [ ] **Step 10: Hooks + Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4e: Effect.Counter opcode + Event.counter funnel (CR 701.6)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 3: Cancel as data — `cancel.json` + `Cards` wiring

**Files:**
- Create: `data/cards/cancel.json`
- Modify: `source/test-suite/Pawl/Cards.hs` (record field, loader line, record construction, `allPrintings`)
- Test: `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Consumes: `Effect.Counter` (Task 2), `TargetSpec.SpellTarget` (Task 1).
- Produces: `Cards.cancelPrinting :: Cards -> Printing.Printing`; `cancel` present in `Cards.allPrintings`.

- [ ] **Step 1: Write the failing test (Cancel parses to the right opcode)**

Add to `source/test-suite/Pawl/CodecSpec.hs` (in the appropriate `testGroup` list — match the file's existing structure; place beside other per-card assertions):

```haskell
      HU.testCase "M4e Cancel loads as a single Counter effect targeting a spell" $
        let card = Printing.card (Cards.cancelPrinting cards)
         in do
              HU.assertEqual
                "effects"
                [Effect.Counter (SlotName.MkSlotName (Text.pack "spell"))]
                (Card.effects card)
              HU.assertEqual
                "target spec"
                (Map.singleton (SlotName.MkSlotName (Text.pack "spell")) TargetSpec.SpellTarget)
                (Card.targetSpecs card),
```

Ensure `CodecSpec.hs` imports `Pawl.Type.Effect as Effect`, `Pawl.Type.SlotName as SlotName`, `Pawl.Type.TargetSpec as TargetSpec`, `Pawl.Type.Card as Card`, `Data.Map.Strict as Map`, `Data.Text as Text`, `Pawl.Type.Printing as Printing` — add any missing.

- [ ] **Step 2: Run it to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -iE "cancelPrinting|not in scope"`
Expected: FAIL — `cancelPrinting` is not a field of `Cards`.

- [ ] **Step 3: Create `data/cards/cancel.json`**

Write this exact content to `data/cards/cancel.json` (Scryfall: `{1}{U}{U}` Instant, "Counter target spell." — verified 2026-07-19). The `manaCost` encoding matches `murder.json` (generic + two colored); `Blue` matches `magical-hack.json`:

```json
{"name":"Cancel","manaCost":[{"type":"Generic","value":1},{"type":"OfType","value":{"type":"Colored","value":{"type":"Blue"}}},{"type":"OfType","value":{"type":"Colored","value":{"type":"Blue"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Instant"}],"subtypes":[]},"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"effects":[{"type":"Counter","value":"spell"}],"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"targetSpecs":[{"slot":"spell","spec":{"type":"SpellTarget"}}]}
```

- [ ] **Step 4: Wire `cancelPrinting` into `Cards.hs`**

In `source/test-suite/Pawl/Cards.hs`, make four edits:

1. Add the record field (in the `Cards` record, after `drudgeSkeletonsPrinting`):
```haskell
    drudgeSkeletonsPrinting :: Printing.Printing,
    cancelPrinting :: Printing.Printing
```
(move the comma onto the previous line as shown).

2. Add the loader line in the `loadCards` do-block (after `drudgeSkeletonsPrinting_ <- loadPrinting "drudge-skeletons"`):
```haskell
  cancelPrinting_ <- loadPrinting "cancel"
```

3. Add the record-construction line (after `drudgeSkeletonsPrinting = drudgeSkeletonsPrinting_`):
```haskell
        drudgeSkeletonsPrinting = drudgeSkeletonsPrinting_,
        cancelPrinting = cancelPrinting_
```
(again shifting the comma).

4. Add to the `allPrintings` list (after `drudgeSkeletonsPrinting cards`):
```haskell
    drudgeSkeletonsPrinting cards,
    cancelPrinting cards
```

Do **not** add Cancel to `blueDeck` or any matchup (fixture-only, per Global Constraints).

- [ ] **Step 5: Run the test + round-trip to verify pass**

Run: `cabal test --test-options='-p "Cancel || round-trip || allPrintings"' 2>&1 | tail -8`
Expected: PASS — the new assertion passes, and the existing `allPrintings` honesty round-trip (`jsonToCard . cardToJson`) now includes Cancel and stays green.

- [ ] **Step 6: Full build + hooks + Commit**

```bash
cabal build all --enable-tests --enable-benchmarks
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4e: Cancel as data (cancel.json) + Cards wiring

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 4: The gate + falsifier — cast Cancel counters; racing counters fizzle

**Files:**
- Modify: `source/test-suite/Pawl/ResolveSpec.hs` (add a `counterTests` group and wire it into the module's `tests`)

**Interfaces:**
- Consumes: `Cards.cancelPrinting` (Task 3), `Support.spellOnStack` (Task 1), `Cast.castSpell`, `Stack.resolveTop`, `Engine.runGamePure`, `S.landsInPlay`, `S.handOne`, `Cards.islandPrinting`, `Cards.pikerPrinting`.
- Produces: gate + falsifier coverage; no new library code.

- [ ] **Step 1: Write the failing gate test**

Add a new `counterTests` group near `fizzleTests` in `source/test-suite/Pawl/ResolveSpec.hs`, with two local helpers and the gate case. First the helpers (top-level, near `twoBoltState`):

```haskell
-- alice has 3 Islands and Cancel in hand; a `victim` spell (bob's) sits on the
-- stack. Returns (victimId, state after alice casts Cancel at it and it resolves).
cancelVictim :: Cards.Cards -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
cancelVictim cards victim =
  let base = S.landsInPlay (Cards.islandPrinting cards) 3
      (victimId, onStack) = S.spellOnStack victim S.bob base
      (gs, cancelId) = S.handOne (Cards.cancelPrinting cards) onStack
      cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice cancelId))
      resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
   in (victimId, resolved)

-- Append a second card of `printing` to `pid`'s hand (handOne overwrites the hand,
-- so a second in-hand card must be appended, not re-inserted).
handAppend :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
handAppend printing pid gs =
  let (oid, gs1) = Game.freshObjectId gs
      obj = Object.MkObject pid (Source.OfCard printing) Zone.Hand TapState.Untapped 0 Sickness.Settled Map.empty (Timestamp.MkTimestamp 0)
   in ( oid,
        gs1
          { GameState.objects = Map.insert oid obj (GameState.objects gs1),
            GameState.hand = Map.insertWith (Seq.><) pid (Seq.singleton oid) (GameState.hand gs1)
          }
      )
```

Then the group (add the group definition and wire it into the module's aggregating `tests` list):

```haskell
counterTests :: Cards.Cards -> Tasty.TestTree
counterTests cards =
  Tasty.testGroup
    "Counter"
    [ HU.testCase "CR 701.6 Cancel counters a spell into its owner's graveyard" $
        let (_victimId, resolved) = cancelVictim cards (Cards.pikerPrinting cards)
         in do
              HU.assertEqual "victim countered into bob's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob resolved))
              HU.assertEqual "victim never resolved onto the battlefield" 0 (S.creaturesInPlay S.bob resolved)
              HU.assertEqual "Cancel in alice's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice resolved))
              HU.assertEqual "stack empty" 0 (length (GameState.stack resolved)),
```

- [ ] **Step 2: Run it to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -iE "counterTests|not in scope"`
Expected: FAIL until `counterTests` is wired into the module's `tests` aggregator (and — if Task 3 were skipped — `cancelPrinting`). After wiring, the case should compile; run it:

Run: `cabal test --test-options='-p "Cancel counters a spell"' 2>&1 | tail -5`
Expected: PASS.

- [ ] **Step 3: Add the falsifier (racing counters) case**

Add to the `counterTests` list (after the gate case), plus the `racingCounters` helper (top-level, near `cancelVictim`):

```haskell
-- alice has 6 Islands and TWO Cancels; a Piker (bob's) sits on the stack. alice
-- casts Cancel A at the Piker, then Cancel B at the Piker (CR 117.3c keeps
-- priority). Stack [B, A, Piker]; resolveTop LIFO: B counters the Piker, then A --
-- its only target gone -- fizzles (CR 608.2b).
racingCounters :: Cards.Cards -> GameState.GameState
racingCounters cards =
  let base = S.landsInPlay (Cards.islandPrinting cards) 6
      (victimId, onStack) = S.spellOnStack (Cards.pikerPrinting cards) S.bob base
      (gs1, cancelA) = S.handOne (Cards.cancelPrinting cards) onStack
      (cancelB, gs2) = handAppend (Cards.cancelPrinting cards) S.alice gs1
      atVictim :: Prompt.Prompt r -> r
      atVictim p = case p of
        Prompt.ChooseTargets _ _ _ sets -> Map.map (const (Recipient.ToObject victimId)) sets
        _ -> S.identityAnswer p
      castA = snd (Engine.runGamePure atVictim gs2 (Cast.castSpell S.alice cancelA))
      castB = snd (Engine.runGamePure atVictim castA (Cast.castSpell S.alice cancelB))
      r1 = snd (Engine.runGamePure atVictim castB Stack.resolveTop) -- B counters the Piker
      r2 = snd (Engine.runGamePure atVictim r1 Stack.resolveTop) -- A fizzles
   in r2
```

The case:
```haskell
      HU.testCase "CR 608.2b a Cancel whose target already left the stack fizzles" $
        let after = racingCounters cards
         in do
              HU.assertEqual "the Piker moved exactly once, to bob's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after))
              HU.assertEqual "both Cancels in alice's graveyard" 2 (length (Game.zoneMembers Zone.Graveyard S.alice after))
              HU.assertEqual "the Piker never hit the battlefield" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "stack cleared" 0 (length (GameState.stack after))
```

(ResolveSpec already imports `Prompt`, `Recipient`, `Map`, `Seq`, `Source`, `Object`, `Zone`, `TapState`, `Sickness`, `Timestamp`, `Game`, `Cast`, `Stack`, `Engine`, `S`, `Cards`, `GameState`, `ObjectId`, `PlayerId`, `Printing` — verify.)

- [ ] **Step 4: Run the falsifier to verify it passes**

Run: `cabal test --test-options='-p "already left the stack fizzles"' 2>&1 | tail -5`
Expected: PASS.

- [ ] **Step 5: Add the RiP composition case (gate suite completeness)**

Add to the `counterTests` list:
```haskell
      HU.testCase "CR 614 Cancel under Rest in Peace exiles the countered spell" $
        let (_, ripOut) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (S.landsInPlay (Cards.islandPrinting cards) 3)
            (_victimId, onStack) = S.spellOnStack (Cards.pikerPrinting cards) S.bob ripOut
            (gs, cancelId) = S.handOne (Cards.cancelPrinting cards) onStack
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice cancelId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "the countered spell is not in the graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.bob resolved))
              HU.assertEqual "the countered spell is exiled" 1 (length (Game.zoneMembers Zone.Exile S.bob resolved))
    ]
```

Note: `S.landsInPlay ... 3` seeds alice's board, and `S.addCreature (restInPeacePrinting)` puts Rest in Peace on the battlefield so its replacement is live (Rest in Peace's ETB exile of graveyards does not fire here because it is placed directly, not cast — its static replacement is what redirects the counter). Confirm `S.identityAnswer` needs no extra arms (Cancel targets the sole stack spell via `Set.lookupMin`).

- [ ] **Step 6: Run the whole Counter group**

Run: `cabal test --test-options='-p "/Counter/"' 2>&1 | tail -8`
Expected: PASS (4 tests: gate, falsifier, RiP compose — adjust the pattern if it collides with other groups).

- [ ] **Step 7: Run the full suite to confirm no regressions**

Run: `cabal test 2>&1 | tail -15`
Expected: all suites PASS.

- [ ] **Step 8: Full clean build + hooks**

Run: `cabal clean && cabal build all --enable-tests --enable-benchmarks && git add -A && hooky fix && git add -A && hooky run`
Expected: warning-clean (a clean build surfaces warnings incremental builds hide), hooks pass.

- [ ] **Step 9: Commit**

```bash
git add -A
git commit -m "M4e: gate + falsifier -- Cancel counters a spell; racing counters fizzle

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 5: Milestone completion log + status update

**Files:**
- Modify: `docs/progress.md` (append the M4e entry)
- Modify: `CLAUDE.md` (update the "Current work" paragraph: M4e complete, M4f next)

**Interfaces:** none (documentation).

- [ ] **Step 1: Append the M4e entry to `docs/progress.md`**

Add a bullet after the M4d entry, matching the house style (gate card, decision proved, opcodes/types added, named elisions/expiries). Draft:

```markdown
- **M4e is complete** (counter target spell -- the first effect that removes a
  spell from the stack. **Gate: Cancel** (`{1}{U}{U}` Instant, "Counter target
  spell"): the falsifier is a Cancel whose target left the stack before it
  resolves, which must fizzle (CR 608.2b) -- a path M3a's re-validation already
  builds, so the milestone proves the seam rather than rebuilding it. One opcode
  `Effect.Counter SlotName` (a distinct keyword action, the M4b Destroy precedent),
  executed by `Resolve.applyEffect` through a new `Event.counter` funnel (CR
  701.6a: remove from the stack, put into the owner's graveyard via `changeZone` --
  so Rest in Peace's redirect and CR 400.7 compose for free). One target spec
  `TargetSpec.SpellTarget` (CR 115 "target spell" -- stack objects that are spells
  only), read via the new `Game.isSpell` classification (`Object.source` is
  `OfCard`; abilities and permanents excluded) -- a classification, not an identity
  case, like `Card.isPermanent`. Cancel is a blue deterministic fixture (no
  random-game deck, the M3d posture); `cancel.json` joins `allPrintings` for the
  honesty round-trip. `Pawl.Resolve` stays the sole `case effect of` home; `Event`
  the sole funnel home. No new prompt/response. **Named elisions/expiries**:
  `Event.counter` is ungated -- "can't be countered" (CR 701.6), conditional
  counters ("counter unless pay", Mana Leak/Daze), a distinct "was countered" event
  and its trigger, countering **abilities** (Stifle -- needs an `AbilityTarget`),
  alternative counter destinations (counter-and-exile, Remand), and restricted
  counters ("counter target spell with mana value N") are each deferred to the
  first card that needs them. Spec and plan kept as reference:
  `docs/superpowers/specs/2026-07-19-m4e-counter-spell-design.md` and
  `docs/superpowers/plans/2026-07-19-m4e-counter-spell.md`.
```

- [ ] **Step 2: Update `CLAUDE.md`'s "Current work" paragraph**

Change the milestone status line to record M4e complete and name **M4f (counters — +1/+1 as persistent permanent state, layer 7d) as next** (per the design.md §3 M4 table). Add a one-clause summary of M4e mirroring the existing M4a–M4d clauses (gate Cancel, `Effect.Counter`/`Event.counter`/`SpellTarget`/`Game.isSpell`).

- [ ] **Step 3: Verify the progress-check grep is satisfiable**

Run: `grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-19-m4e-counter-spell.md`
Expected: `0` once every step is ticked. (This is the plan's own done-check.)

- [ ] **Step 4: Hooks + Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4e: milestone completion log entry + status update

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Self-Review

**Spec coverage:**

- SpellTarget spec (spec §1) → Task 1. ✓
- `Game.isSpell` classification (spec §1) → Task 1. ✓
- `Effect.Counter` opcode + 5 classifiers + applyEffect (spec §2) → Task 2. ✓
- `Event.counter` funnel (spec §3) → Task 2. ✓
- Codec arms for SpellTarget (Task 1) and Counter (Task 2) → covered. ✓
- `cancel.json` + Cards wiring + allPrintings hygiene (spec §4) → Task 3. ✓
- Gate: Cancel counters a spell (spec §5) → Task 4 Step 1. ✓
- Falsifier: 608.2b fizzle / racing counters (spec §5) → Task 4 Step 3. ✓
- SpellTarget excludes permanents (spec §5) → Task 1 Step 5. ✓
- RiP composition (spec §5) → Task 2 (funnel-level) + Task 4 Step 5 (end-to-end). ✓
- Round-trip covers Cancel (spec §5) → Task 3 (allPrintings honesty property). ✓
- Deferred expiries recorded (spec §7) → Task 5 progress entry. ✓
- No new prompt/response; deterministic blue fixtures only (Global Constraints) → honored (no `blueDeck` edit; existing `identityAnswer`/`atVictim` answers). ✓

**Placeholder scan:** No "TBD"/"handle edge cases"/"similar to Task N" — every code step shows full code. The Cabal note explicitly states no `.cabal` change is needed. ✓

**Type consistency:** `Effect.Counter :: SlotName -> Effect card` (Task 2) matches its use in `slotsOf`/`applyEffect`/`Codec`/`cancel.json` (`{"type":"Counter","value":"spell"}` decodes to `Counter (MkSlotName "spell")`) and the Task 3 assertion. `Game.isSpell :: ObjectId -> GameState -> Bool` (Task 1) matches its `Target.hs` call. `Event.counter :: ObjectId -> GameState -> GameState` (Task 2) matches `applyEffect`'s call and the EventSpec tests. `Support.spellOnStack` signature is consistent across Tasks 1, 2, 4. The `SpellTarget` string token is identical in `targetSpecToJson`, `jsonToTargetSpec`, and `cancel.json`. ✓

One risk noted for the implementer: the falsifier's robustness depends on `atVictim` pinning both Cancels to `victimId` (not on object-id ordering) — this is why the custom answer is used instead of `S.identityAnswer`'s `Set.lookupMin`.
