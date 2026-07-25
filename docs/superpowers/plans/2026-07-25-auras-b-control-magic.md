# Auras Phase (b): Control From A Static Ability — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Control Magic works — an Aura grants its controller control of the enchanted creature, indefinitely, across turns.

**Architecture:** A payload-free layer-2 `Modification.SetControllerToSource` whose player is *derived* at projection time from the effect's source's controller, rather than baked at resolution the way `SetController` is. `Projection.controllerOf` — today a lean fold over stored continuous effects only — additionally gathers control-granting static abilities from battlefield permanents, merged under CR 613.7's timestamp ordering, with the hoisting and cycle-escape that `staticAbilitiesLive`/`liveGiven` already model.

**Tech Stack:** Haskell 2010 (GHC 9.14.1), `tasty`/`tasty-hunit`, `tasty-bench`, the `StateT GameState (Program Prompt)` suspension monad, the hand-written JSON codec in `Pawl.Codec`, the on-demand card registry in `Pawl.Registry`.

## Global Constraints

- **Haskell 2010, no language extensions.** Only `GADTs`, `RankNTypes` and `NamedFieldPuns` are permitted; this phase needs none.
- **Warning-clean under `+pedantic` (`-Werror`).** A new `Modification` constructor breaks every exhaustive match at once — that is the safety net. Run `cabal build all --enable-tests --enable-benchmarks`; `cabal clean` first for a definitive check. **Never run two builds concurrently** — `jobs: $ncpus` already saturates the machine.
- **No library module is added or deleted in this phase**, so `hooky fix` handles `pawl.cabal`; no direct `cabal-gild` run is needed.
- **No partial functions**, no boolean blindness, `Mk` constructor prefix, derive at least `Eq` and `Show`, `Text` not `String`, no list comprehensions, `let` over `where`, `case` over point-free, `do` + record syntax to build records.
- **One type per module** under `Pawl.Type.<Name>`; qualified imports aliased to the last component; no explicit export lists; a module never imports its parents.
- **`Pawl.Projection` is the ONLY module permitted to case on `Modification`.** `Pawl.Departure` asks its questions through `Projection.givesControlTo` precisely so the case stays there. Keep it that way.
- **The two invariants outrank this plan:** the engine never cases on a card's *identity* (only classifications), and never makes a player's choice.
- **TDD non-negotiable:** write each failing test, run it, watch it fail, then implement. Tick each `- [ ]` as you finish it.
- **Every rules claim cites the CR** and was checked against `docs/rules.txt`. Never write an expiry into a code comment — file an issue and cite `(#N)`.
- **Commit style:** commit directly to `main`, one small complete commit per task, with the two `CLAUDE.md` trailers.
- **After each task:** `git add <explicit paths>`, `hooky fix`, `git add <explicit paths>`, `hooky run`. Stage explicit paths rather than `-A`: concurrent sessions share this checkout.

**Spec:** `docs/superpowers/specs/2026-07-25-auras-design.md`. Phase (b) is spec §3.8–§3.11.

**Phase (a) landed first** (`49c388d`..`2dcfd1f`): `Subtype.Aura`, `Card.enchant`, `Object.attachedTo`, `Affected.Attached`, `Event.changeZoneAttaching`, `Pawl.Stack`'s Aura branch, and CR 704.5m in `Sba.fallsOff`.

---

## The two things that make this phase harder than it looks

Read both before starting. They are not restatements of the spec — the spec understated them, and the plan's task boundaries follow from them.

### 1. `controllerOf` cannot call `gather`, because `gather` calls `controllerOf`

`Projection.affects` reads `controllerOf source gs` to supply CR 109.5's "you" perspective when matching an `Affected.Matching` filter (`Projection.hs:212`), and `gather` is what feeds `affects`. If `controllerOf` were implemented by asking `gather` for control-granting effects, the two would call each other forever.

So `controllerOf` needs its **own lean gather** — one that reads `Card.staticAbilities` directly off battlefield permanents and resolves their affected sets *without* projecting. That is possible because the affected sets it must handle do not need a projection:

- `Affected.Attached` resolves from `Object.attachedTo` of the source. No projection.
- `Affected.TheseObjects` is a set-membership test. No projection.
- `Affected.Matching` **does** need a projection, and therefore cannot be resolved here.

Control Magic uses `Attached`. A control-granting static ability with a `Matching` affected set is a real gap that this phase defers with an issue — it must not be silently treated as "does not apply".

### 2. `Pawl.Departure` contains two written correctness proofs whose premises this phase invalidates

These are not stale comments. `Departure.hs` argues, in prose, that two clauses of CR 800.4a are *empty by construction*, and both arguments name the premise this phase breaks:

- around `Departure.hs:221` — "Projection.controllerOf is an object's OWNER overridden by a layer-2 `SetController` and nothing else"
- around `Departure.hs:261` — "Modification is a flat sum with exactly one construction site for `SetController`"

After this phase there is a second construction site and a third source of control. The conclusions may well still hold, but the **proofs must be re-derived**, not reworded. Task 5 owns that, and it is written as an investigation with a test, not as a comment edit.

---

## File Structure

**Create:**
- `data/cards/control-magic.json` — the gate card.

**Modify (library):**
- `source/library/Pawl/Type/Modification.hs` — `+SetControllerToSource`.
- `source/library/Pawl/Projection.hs` — `layer`, `applyModification` and the other exhaustive `Modification` matches; the new `controlGrants` / `controllerOfGiven`; `controls`; `givesControlTo`.
- `source/library/Pawl/Departure.hs` — the two re-derived proofs, and `givesControlTo`'s use if the re-derivation demands it.
- `source/library/Pawl/Engine.hs` — `settleAll`'s comment (#62).
- `source/library/Pawl/Codec.hs` — the `Modification` tag pair.

**Modify (test-suite):**
- `source/test-suite/Pawl/ProjectionSpec.hs` — control from a static ability.
- `source/test-suite/Pawl/CombatSpec.hs` — the synthetic fixture retires (#33).
- `source/test-suite/Pawl/AuraSpec.hs` — the cross-turn settle (#62) and the Control Magic scenarios.
- `source/test-suite/Pawl/DepartureSpec.hs` — CR 800.4a with an Aura.
- `source/benchmark/Main.hs` — an Aura-bearing scenario, so a projection regression is catchable.

---

## Task 1: `Modification.SetControllerToSource`

The constructor and its classifications. No behaviour yet — nothing produces it.

**Files:**
- Modify: `source/library/Pawl/Type/Modification.hs`, `source/library/Pawl/Projection.hs`, `source/library/Pawl/Codec.hs`
- Test: `source/test-suite/Pawl/ProjectionSpec.hs`, `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Produces: `Modification.SetControllerToSource`; `Projection.layer Modification.SetControllerToSource == Layer.Control`. Consumed by Tasks 2, 5.

- [ ] **Step 1: Write the failing test**

In `source/test-suite/Pawl/ProjectionSpec.hs`:

```haskell
-- CR 613.1b: layer 2 is where control-changing effects apply, whether the new
-- controller was baked at resolution (SetController) or is derived from the
-- effect's source (SetControllerToSource).
HU.testCase "CR 613.1b: SetControllerToSource is a layer-2 modification" $
  HU.assertEqual "layer 2" Layer.Control (Projection.layer Modification.SetControllerToSource),
```

And in `source/test-suite/Pawl/CodecSpec.hs`, add `Modification.SetControllerToSource` to whichever round-trip list already covers `Modification` values. If none exists, add a single round-trip case matching the file's existing style.

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL to compile — `Data constructor not in scope: Modification.SetControllerToSource`.

- [ ] **Step 3: Add the constructor**

In `source/library/Pawl/Type/Modification.hs`, after `SetController`:

```haskell
  | -- layer 2, CR 613.1b: this object's controller becomes the controller of THIS
    -- effect's SOURCE. Payload-free because the player is DERIVED at projection
    -- time, not baked -- the contrast with SetController above, whose PlayerId is
    -- fixed at resolution by CR 611.2c because a resolution effect's answer is
    -- determined once.
    --
    -- A static ability's modification is CARD DATA and cannot name a PlayerId, so
    -- this is the only shape in which a printed card can grant control. Control
    -- Magic's "You control enchanted creature."
    --
    -- CR 303.4e: an Aura's controller and the enchanted object's controller are
    -- separate. Deriving from the SOURCE's controller is what keeps them so --
    -- gaining control of the creature does not gain control of the Aura, and
    -- gaining control of the Aura DOES move the creature.
    SetControllerToSource
```

- [ ] **Step 4: Fix every exhaustive match the compiler names**

`Projection.layer` gets `Modification.SetControllerToSource -> Layer.Control`.

`Projection.setLandSubtypeEffects`'s `isSet` gets an explicit `Modification.SetControllerToSource -> False` arm beside the `SetController` one already there, with the same reasoning (a control op, not the CR 305.7 land-subtype set).

`Projection.givesControlTo` gets:

```haskell
  -- A STORED ContinuousEffect never carries this: it is producible only by a
  -- static ability, which the projection re-derives and never stores. CR 800.4a's
  -- second clause ends stored effects, so there is nothing here to end -- the
  -- static-ability path is handled by clause 1 removing the source object (see
  -- Pawl.Departure).
  Modification.SetControllerToSource -> False
```

Then fix the remaining arms the compiler names in `Projection.hs` (`applyModification` and the two or three other exhaustive matches) — each takes the same treatment its `SetController` neighbour has, because both are layer-2 control ops applied by `controllerOf` rather than by the characteristic pass.

`Codec.modificationToJson` / `jsonToModification` get the nullary tag pair. **Never emitted in card JSON today is false for this constructor** — unlike `SetController`, this one *is* card data and will appear in `data/cards/control-magic.json` in Task 2, so the codec arms are load-bearing, not defensive.

- [ ] **Step 5: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS, warning-free.

- [ ] **Step 6: Commit**

```bash
git add source/library/Pawl/ source/test-suite/Pawl/
hooky fix
git add source/library/Pawl/ source/test-suite/Pawl/
hooky run
git commit -m "feat(modification): add SetControllerToSource, layer 2 derived from the source (CR 613.1b)"
```

---

## Task 2: Control Magic, and `controllerOf` reading static abilities

The phase's centre. The card and the projection change land together, for the same reason phase (a) merged its gate card with `Affected.Attached`: the card's only honest test is the behaviour, and the behaviour's only honest test is the card.

**Files:**
- Create: `data/cards/control-magic.json`
- Modify: `source/library/Pawl/Projection.hs`
- Test: `source/test-suite/Pawl/ProjectionSpec.hs`, `source/test-suite/Pawl/AuraSpec.hs`

**Interfaces:**
- Consumes: `Modification.SetControllerToSource` (Task 1); `Object.attachedTo`, `Affected.Attached`, `Card.enchant` (phase (a)).
- Produces: `Projection.controlGrants :: GameState -> [Projection.ControlGrant]`, `Projection.controllerOfGiven :: [Projection.ControlGrant] -> Set ObjectId -> ObjectId -> GameState -> Maybe PlayerId`. `controllerOf` and `controls` keep their existing signatures. Consumed by Tasks 3, 4, 5, 6.

Scryfall-verified 2026-07-25: `Control Magic` — `{2}{U}{U}` — `Enchantment — Aura` — "Enchant creature / You control enchanted creature."

- [ ] **Step 1: Write the failing tests**

In `source/test-suite/Pawl/AuraSpec.hs`:

```haskell
-- CR 613.1b / 303.4e: Control Magic's static ability moves control of the
-- enchanted creature to the AURA's controller, and leaves the Aura itself alone.
HU.testCase "CR 613.1b: Control Magic gives the Aura's controller the creature" $ do
  piker <- Registry.printing registry "Goblin Piker"
  controlMagic <- Registry.printing registry "Control Magic"
  let base = Setup.emptyGame S.bothPlayers
      (creature, withCreature) = S.addCreature piker S.bob base
      (aura, withAura) = S.addCreature controlMagic S.alice withCreature
      attached = S.attach aura creature withAura
  HU.assertEqual "unattached, bob still controls it" (Just S.bob) (Projection.controllerOf creature withAura)
  HU.assertEqual "attached, alice controls it" (Just S.alice) (Projection.controllerOf creature attached)
  HU.assertEqual "the Aura's own controller is unchanged" (Just S.alice) (Projection.controllerOf aura attached)
  HU.assertBool "and it is in alice's controls" (elem creature (Projection.controls S.alice attached))
  HU.assertBool "no longer in bob's" (not (elem creature (Projection.controls S.bob attached))),
-- CR 704.5m plus layer 2: destroying the Aura reverts control on the next
-- projection, because a static ability's effect exists only while its source is
-- on the battlefield (CR 604.2).
HU.testCase "CR 604.2: removing Control Magic reverts control" $ do
  piker <- Registry.printing registry "Goblin Piker"
  controlMagic <- Registry.printing registry "Control Magic"
  let base = Setup.emptyGame S.bothPlayers
      (creature, withCreature) = S.addCreature piker S.bob base
      (aura, withAura) = S.addCreature controlMagic S.alice withCreature
      attached = S.attach aura creature withAura
      gone = S.runPure S.identityAnswer attached (Event.changeZone aura Zone.Graveyard)
  HU.assertEqual "alice controlled it" (Just S.alice) (Projection.controllerOf creature attached)
  HU.assertEqual "bob controls it again" (Just S.bob) (Projection.controllerOf creature gone),
```

And the full cast path:

```haskell
-- The whole path: cast, target, enter attached, control moves.
HU.testCase "CR 303.4: casting Control Magic takes the creature" $ do
  island <- Registry.printing registry "Island"
  piker <- Registry.printing registry "Goblin Piker"
  controlMagic <- Registry.printing registry "Control Magic"
  let base = S.landsInPlay island 4
      (creature, withCreature) = S.addCreature piker S.bob base
      (gs, spellId) = S.handOne controlMagic withCreature
      cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
      after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
  HU.assertEqual "alice controls bob's creature" (Just S.alice) (Projection.controllerOf creature after),
```

`S.landsInPlay island 4` must give alice enough blue mana for `{2}{U}{U}`; check how `landsInPlay` assigns lands and adjust the count if four Islands is not enough.

- [ ] **Step 2: Run tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: FAIL — the registry cannot find `"Control Magic"`. After adding the card (Step 3) they should still fail, on control not moving.

- [ ] **Step 3: Write the card**

`data/cards/control-magic.json`, alphabetical fields, matching the shape phase (a) established for `unholy-strength.json` — read that file first and mirror it. The `enchant` object carries pool `Creatures` and **omits** the `filter` key (the encoder omits it when `Nothing`). The one static ability is `affected: Attached`, `modification: SetControllerToSource` (a nullary tag — check `modificationToJson`'s emission for the exact shape). Mana cost is `{2}{U}{U}`: one `Generic` 2 followed by two `OfType`/`Colored`/`Blue` symbols.

Run `cabal test` again: the card now loads, and the control tests fail on behaviour. That is the real red state.

- [ ] **Step 4: Add the lean control gather**

In `source/library/Pawl/Projection.hs`, above `controllerOf`:

```haskell
-- One control-granting static ability, flattened: the source that carries it and
-- the timestamp its effect takes (CR 613.7a: a static ability's continuous effect
-- has the timestamp of the object it is on).
data ControlGrant = MkControlGrant
  { cgSource :: ObjectId,
    cgAffected :: Affected.Affected,
    cgTimestamp :: Timestamp
  }
  deriving (Eq, Ord, Show)

-- Every layer-2 control-granting STATIC ability on the battlefield, gathered once.
--
-- NOT `gather`. This must not project, and cannot: Projection.affects reads
-- controllerOf to supply CR 109.5's "you" when matching a Filter, so a
-- controllerOf built on gather would be mutually recursive with it. That
-- restriction is exactly why Affected.Matching is unsupported below.
--
-- Hoisted for the reason liveGiven's list is hoisted: controllerOf feeds combat,
-- priority, mana and Projection.controls, and `controls` calls it once per
-- battlefield object. Recomputing this list inside controllerOf would make
-- `controls` quadratic in the battlefield, inside a loop the state-based-action
-- sweep runs at every priority boundary.
controlGrants :: GameState -> [ControlGrant]
controlGrants gs =
  let setEffs = setLandSubtypeEffects gs
      grantsOf permId = case Game.lookupObject permId gs of
        Nothing -> []
        Just permObj -> case Game.cardOf permId gs of
          Nothing -> []
          Just card ->
            -- CR 305.7: a land whose subtype was SET has lost its rules text, so
            -- it grants nothing. Same gate gather applies to every static ability.
            if not (null setEffs) && not (liveGiven setEffs Set.empty permId gs)
              then []
              else
                let isControl sa = case StaticAbility.modification sa of
                      Modification.SetControllerToSource -> True
                      _ -> False
                    toGrant sa =
                      MkControlGrant
                        { cgSource = permId,
                          cgAffected = StaticAbility.affected sa,
                          cgTimestamp = Object.timestamp permObj
                        }
                 in fmap toGrant (filter isControl (Card.Type.staticAbilities card))
   in concatMap grantsOf (Set.toList (GameState.battlefield gs))
```

- [ ] **Step 5: Rebuild `controllerOf` on it**

Replace `controllerOf` with a hoisted, cycle-escaping pair, keeping the existing signature so no call site changes:

```haskell
-- CR 108.4 / 613.1b: an object's controller is its owner, overridden by layer-2
-- control effects, last timestamp wins (CR 613.7). TWO sources now: stored
-- continuous effects (Effect.GainControl's baked SetController) and control-
-- granting static abilities (Control Magic's derived SetControllerToSource).
-- Both carry a Timestamp, so they merge into one maximum.
--
-- Still a lean fold, not the full ProjectedCharacteristics pass -- control feeds
-- combat, mana and priority and is needed before P/T.
controllerOf :: ObjectId -> GameState -> Maybe PlayerId.PlayerId
controllerOf oid gs = controllerOfGiven (controlGrants gs) Set.empty oid gs

-- controllerOf with the grant list PRECOMPUTED and a visited set.
--
-- The visited set is the CR 613.8b loop-escape analog liveGiven already uses
-- (#37), not an implementation of it: deriving a grant's player asks for its
-- SOURCE's controller, which can re-enter this function. Re-entering an object
-- already under question returns its owner -- so a cycle means no static ability
-- wins and everything stays with its owner. Order-independent, like liveGiven's.
controllerOfGiven :: [ControlGrant] -> Set ObjectId -> ObjectId -> GameState -> Maybe PlayerId.PlayerId
controllerOfGiven grants visited oid gs = case Game.lookupObject oid gs of
  Nothing -> Nothing
  Just obj ->
    if Set.member oid visited
      then Just (Object.owner obj)
      else
        let visited' = Set.insert oid visited
            -- Does an affected set carried by `source` name `oid`? Parameterized
            -- by the source because Affected.Attached is a question about the
            -- SOURCE's state, and the stored and derived paths carry different
            -- sources.
            namesFrom source a = case a of
              Affected.TheseObjects s -> Set.member oid s
              -- CR 303.4m: the source's own attachment. No projection needed,
              -- which is what keeps this fold lean.
              Affected.Attached -> case Game.lookupObject source gs of
                Nothing -> False
                Just src -> Object.attachedTo src == Just oid
              -- Needs a projection to evaluate, and this fold must not project
              -- (see controlGrants). No card produces one (#195).
              Affected.Matching _ -> False
            storedSetter eff = case ContinuousEffect.modification eff of
              Modification.SetController pid
                | namesFrom (ContinuousEffect.source eff) (ContinuousEffect.affected eff) ->
                    Just (ContinuousEffect.timestamp eff, pid)
              _ -> Nothing
            stored = Maybe.mapMaybe storedSetter (GameState.continuousEffects gs)
            fromGrant g =
              if not (namesFrom (cgSource g) (cgAffected g))
                then Nothing
                else case controllerOfGiven grants visited' (cgSource g) gs of
                  Nothing -> Nothing
                  Just who -> Just (cgTimestamp g, who)
            derived = Maybe.mapMaybe fromGrant grants
         in case stored <> derived of
              [] -> Just (Object.owner obj)
              setters -> Just (snd (List.maximumBy (Ord.comparing fst) setters))
```

Two notes on that body. The stored path's `namesFrom` now also has an `Attached` arm it will never exercise — `Pawl.Resolve` builds only `Affected.TheseObjects` for stored effects — which is deliberate: one affected-set test shared by both paths beats two that can drift. And the `Ord.comparing fst` tie-break behaviour is unchanged from the existing `controllerOf`; do not "improve" it here.

- [ ] **Step 6: Hoist in `controls`**

```haskell
-- The battlefield permanents a player controls (CR 108.4). Computes the grant
-- list ONCE and threads it, rather than letting each controllerOf rebuild it --
-- the difference between linear and quadratic in the battlefield, in a function
-- the state-based-action sweep calls at every priority boundary.
controls :: PlayerId.PlayerId -> GameState -> [ObjectId]
controls pid gs =
  let grants = controlGrants gs
   in filter (\oid -> controllerOfGiven grants Set.empty oid gs == Just pid) (Set.toList (GameState.battlefield gs))
```

- [ ] **Step 7: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS, warning-free.

- [ ] **Step 8: Commit**

```bash
git add data/cards/control-magic.json source/library/Pawl/Projection.hs source/test-suite/Pawl/
hooky fix
git add data/cards/control-magic.json source/library/Pawl/Projection.hs source/test-suite/Pawl/
hooky run
git commit -m "feat(projection): control from a static ability, proven by Control Magic (CR 613.1b)"
```

---

## Task 3: Cross-turn indefinite control (#62)

The deferral Control Magic exists to retire. Every control effect before this one was until-end-of-turn, so the thief never reached their own untap step still holding the creature.

**Files:**
- Modify: `source/library/Pawl/Engine.hs` (the `settleAll` comment)
- Test: `source/test-suite/Pawl/AuraSpec.hs`

**Interfaces:**
- Consumes: the Control Magic printing and the new `controllerOf` (Task 2).

- [ ] **Step 1: Write the failing test**

In `source/test-suite/Pawl/AuraSpec.hs`. The point is that the stolen creature is still alice's at *her* untap step, settles there, and can then attack — which no until-end-of-turn effect could ever exercise:

```haskell
-- CR 302.6 across turns (#62): control from an Aura is INDEFINITE, so alice
-- still holds the creature when her own untap step arrives. Engine.settleAll
-- iterates Projection.controls, so it settles for the controller, and the
-- creature can attack. Act of Treason could never test this -- its control ends
-- at cleanup (CR 514.2), long before the thief's untap step.
HU.testCase "CR 302.6 (#62): a creature held under indefinite control settles at the thief's untap step" $ do
  piker <- Registry.printing registry "Goblin Piker"
  controlMagic <- Registry.printing registry "Control Magic"
  let base = Setup.emptyGame S.bothPlayers
      (creature, withCreature) = S.addCreature piker S.bob base
      (aura, withAura) = S.addCreature controlMagic S.alice withCreature
      attached = S.attach aura creature withAura
      sick = S.resick creature attached
      settled = S.runPure S.identityAnswer sick (Engine.settleAll S.alice)
  HU.assertEqual "alice controls it" (Just S.alice) (Projection.controllerOf creature settled)
  HU.assertBool "and it has settled under her control, so it can attack" (Combat.canAttack S.alice creature settled),
```

If `S.resick` does not exist, add it to `Pawl.Support` beside `S.attach` — a state fixture setting `Object.sickness = Sickness.Sick` on one object, mirroring `S.attach`'s shape and comment style. Do not reach for `Resolve.applyEffect` to induce sickness; that is the synthetic path Task 4 is deleting.

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test`
Expected: FAIL on `canAttack` — or PASS. **If it passes on the first run, that is information, not a reason to skip the task.** It would mean `settleAll` already handled indefinite control and #62 was over-stated. Record that in your report, keep the test (it is now the regression guard #62 asked for), and proceed to Step 3.

- [ ] **Step 3: Correct the comment**

`Engine.settleAll`'s comment currently ends: "Settling a permanent held under INDEFINITE control, across the thief's own untap step, is the Auras / Control Magic phase." That phase is now here. Rewrite the paragraph to state what is true: control from an Aura's static ability persists across turns, `settleAll` iterates `Projection.controls` so it settles for the current controller, and the case is covered by a test. Remove the forward reference.

- [ ] **Step 4: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS, warning-free.

- [ ] **Step 5: Commit and close the issue**

```bash
git add source/library/Pawl/Engine.hs source/test-suite/Pawl/
hooky fix
git add source/library/Pawl/Engine.hs source/test-suite/Pawl/
hooky run
git commit -m "test(engine): settle a creature held under indefinite control across turns (#62)"
gh issue close 62 --comment "Control Magic's control is indefinite, so the thief reaches their own untap step still holding the creature. Covered by a test in Pawl.AuraSpec; Engine.settleAll's forward reference to 'the Auras / Control Magic phase' is gone."
```

Also check `source/library/Pawl/Type/Sickness.hs:15` — issue #62 names it as the citation site. If a `(#62)` comment lives there, it dies in this commit.

---

## Task 4: Retire the synthetic steal fixture (#33)

`CombatSpec`'s CR 302.6 test uses a hand-built `Effect.GainControl` because no real card could steal a creature without granting haste. Control Magic can.

**Files:**
- Modify: `source/test-suite/Pawl/CombatSpec.hs`

**Interfaces:**
- Consumes: the Control Magic printing (Task 2).

- [ ] **Step 1: Rewrite the test against the real card**

In `source/test-suite/Pawl/CombatSpec.hs`, the `controlChangeSicknessTests` group holds a case labelled "SYNTHETIC (labeled crutch, spec §4)" that builds `Effect.GainControl` by hand via `Resolve.applyEffect`. Replace the fixture with Control Magic, keeping the assertion the test exists to make — that a creature which just changed control is summoning sick and cannot attack:

```haskell
controlChangeSicknessTests :: Registry.Type.Registry -> Tasty.TestTree
controlChangeSicknessTests registry =
  Tasty.testGroup
    "ControlChangeSickness"
    [ HU.testCase "CR 302.6 a creature that just changed control is summoning sick (no haste)" $ do
        piker <- Registry.printing registry "Goblin Piker"
        controlMagic <- Registry.printing registry "Control Magic"
        let base = Setup.emptyGame S.bothPlayers
            (creature, withCreature) = S.addCreature piker S.bob base
            (aura, withAura) = S.addCreature controlMagic S.alice withCreature
            attached = S.attach aura creature withAura
            sick = S.resick creature attached
        HU.assertEqual "alice controls it" (Just S.alice) (Projection.controllerOf creature sick)
        HU.assertBool "but it is summoning sick, so it cannot attack this turn" (not (Combat.canAttack S.alice creature sick))
    ]
```

Delete the SYNTHETIC comment block and the `(#33)` citation with it. Remove any import that becomes unused — `-Werror` will name them.

**One thing to check rather than assume.** The old test got its sickness from `Effect.GainControl`'s resolution, which re-Sicks its target (CR 302.6). Attaching an Aura by fixture does not go through that path, so the test must establish sickness itself — that is what `S.resick` is for. If you find that attaching Control Magic *should* re-Sick the creature through some live path and does not, that is a real rules gap: **stop and report it** rather than papering over it with the fixture. CR 302.6 is about control held continuously since the turn began, so a creature that changes control mid-turn is genuinely sick.

- [ ] **Step 2: Run tests**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS, warning-free.

- [ ] **Step 3: Commit and close the issue**

```bash
git add source/test-suite/Pawl/CombatSpec.hs
hooky fix
git add source/test-suite/Pawl/CombatSpec.hs
hooky run
git commit -m "test(combat): retire the synthetic steal fixture for Control Magic (#33)"
gh issue close 33 --comment "Control Magic replaces the hand-built Effect.GainControl in CombatSpec's CR 302.6 control-change test. The labelled crutch and its comment are gone."
```

---

## Task 5: CR 800.4a — re-derive two proofs

`Pawl.Departure` argues in prose that two clauses of CR 800.4a are empty by construction. Both arguments name a premise this phase just broke. **This task is an investigation with a test, not a comment edit** — do the reasoning first, and let what you find decide what the code needs.

**Files:**
- Modify: `source/library/Pawl/Departure.hs`, and `source/library/Pawl/Projection.hs` only if the re-derivation demands it
- Test: `source/test-suite/Pawl/DepartureSpec.hs`

**Interfaces:**
- Consumes: `Projection.controllerOf`, `Projection.givesControlTo` (Tasks 1–2).

- [ ] **Step 1: Do the re-derivation, and write it down before touching code**

The two proofs are around `Departure.hs:221` ("`Projection.controllerOf` is an object's OWNER overridden by a layer-2 `SetController` and nothing else") and around `Departure.hs:261` ("`Modification` is a flat sum with exactly one construction site for `SetController`"). Both now have a counterexample in principle: a third source of control, and a second construction site.

Work through CR 800.4a's clauses in order, with a control-granting Aura in the picture, and write the answer into your report before editing anything:

1. Clause 1 removes every object **owned** by the departing player. If they owned the Control Magic, it leaves and the creature reverts — no further work.
2. Clause 2 ends effects giving that player control. `Projection.givesControlTo` sees only stored effects; a static ability is not stored. **Is there a reachable state where the departing player controls an Aura they do not own?** That requires something to have moved control of the Aura itself — which in this pool means a stored `SetController` on the Aura, which clause 2 removes. Verify that chain, and say whether it closes.
3. Clauses 3 and 4 then ask whether any object is still controlled by the departed player. Re-derive `nonCardStackObjectsCease`'s "empty by construction" claim with `controllerOf`'s third case included.

- [ ] **Step 2: Write the failing test**

In `source/test-suite/Pawl/DepartureSpec.hs`, whatever your re-derivation concluded, pin it:

```haskell
-- CR 800.4a with a control-granting Aura. Alice owns and controls Control Magic
-- enchanting bob's creature; alice leaves. Clause 1 removes objects alice OWNS --
-- including the Aura -- so its static ability goes with it and the creature
-- reverts to bob, with no clause-2 work needed.
HU.testCase "CR 800.4a: a departing player's Control Magic releases the creature" $ do
  piker <- Registry.printing registry "Goblin Piker"
  controlMagic <- Registry.printing registry "Control Magic"
  let base = S.threePlayerGame
      (creature, withCreature) = S.addCreature piker S.bob base
      (aura, withAura) = S.addCreature controlMagic S.alice withCreature
      attached = S.attach aura creature withAura
      after = Departure.depart Departure.Type.Lost S.alice attached
  HU.assertEqual "alice controlled it before she left" (Just S.alice) (Projection.controllerOf creature attached)
  HU.assertEqual "the Aura left the game with her" Nothing (Game.lookupObject aura after)
  HU.assertEqual "and bob has his creature back" (Just S.bob) (Projection.controllerOf creature after),
```

Use `S.threePlayerGame` deliberately: `Departure.objectsLeaveWith` runs only when the game continues after the departure (`turnOrder > 2`), so a two-player fixture would leave alice's objects in place and test something else. Confirm that gate in `Departure.hs` and adjust if the condition differs.

Match `Departure.depart`'s actual signature and argument order — read it rather than trusting the sketch.

- [ ] **Step 3: Run test to verify it fails, or does not**

Run: `cabal test`
Expected: PASS is the likely outcome — clause 1 does the work. **A passing test here is the point, not a failure of TDD:** the test is a regression guard for a proof, and the proof predicts it passes. If it FAILS, you have found a real CR 800.4a bug; report it before fixing, because the fix belongs in `Projection.givesControlTo`'s classification (keeping the `Modification` case inside `Pawl.Projection`) and that is a design decision.

- [ ] **Step 4: Rewrite the two proofs**

Replace both prose arguments with ones whose premises are true after this phase. They must name all three sources of control (owner, stored `SetController`, static `SetControllerToSource`) and carry the re-derivation from Step 1. Do not simply delete the arguments — they are what stops a future change silently skipping a clause of CR 800.4a, which is the value they were written for.

- [ ] **Step 5: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS, warning-free.

- [ ] **Step 6: Commit**

```bash
git add source/library/Pawl/ source/test-suite/Pawl/DepartureSpec.hs
hooky fix
git add source/library/Pawl/ source/test-suite/Pawl/DepartureSpec.hs
hooky run
git commit -m "fix(departure): re-derive CR 800.4a's proofs for static-ability control"
```

---

## Task 6: A benchmark that can see this class of regression

Phase (a)'s final review found `Sba.fallsOff` re-deriving the whole board per Aura. Nothing caught it, because **no benchmark deck contains an Aura**. This phase adds a hotter path — `controllerOf` now walks the battlefield — so the gap closes here.

**Files:**
- Modify: `source/benchmark/Main.hs`
- Possibly modify: `source/test-suite/Pawl/Cards.hs` (a deck list, if the benchmark draws from one)

**Interfaces:**
- Consumes: the Control Magic and Unholy Strength printings.

- [ ] **Step 1: Read how the benchmark builds its scenarios**

`source/benchmark/Main.hs` is a single `tasty-bench` file. Find how it constructs the boards it measures — whether from the `Pawl.Cards` deck lists, from `Pawl.Support` fixtures, or inline. Note which scenario exercises the state-based-action sweep and the projection most heavily; that is the one an Aura belongs in.

- [ ] **Step 2: Add an Aura-bearing scenario**

Add a benchmark case with several attached Auras on a populated battlefield — enough permanents that a per-object re-derivation would show. Both real cards qualify: Unholy Strength (a layer-7c static ability through an attachment) and Control Magic (a layer-2 one that makes `controllerOf` do work). Prefer Control Magic, since `controllerOf` is the hot path this phase touches.

Follow the file's existing style for naming and grouping. Do not restructure the benchmark.

- [ ] **Step 3: Run the benchmark and record a baseline**

Run: `cabal bench`
Record the numbers for the new case in your report. This is a **baseline for future comparison**, not a pass/fail gate — there is no prior number to compare against. Say plainly in your report what the new scenario costs relative to its nearest Aura-free neighbour, so a future regression has something to be measured against.

- [ ] **Step 4: Commit**

```bash
git add source/benchmark/Main.hs source/test-suite/Pawl/Cards.hs
hooky fix
git add source/benchmark/Main.hs source/test-suite/Pawl/Cards.hs
hooky run
git commit -m "bench: cover an Aura-bearing board, so projection regressions are visible"
```

---

## Task 7: Record the phase

**Files:**
- Modify: `docs/progress.md`, `CLAUDE.md`

- [ ] **Step 1: Verify no placeholder citation survived**

Run: `grep -rn '(#N' source/`
Expected: no hits. Any hit is a comment that shipped without a real issue number.

- [ ] **Step 2: Record the phase**

Add the phase (b) material to `docs/progress.md`. Phase (a) already has an entry there — decide, and say in your report why, whether phase (b) extends that entry or gets its own. Both are defensible; the file's existing convention for multi-phase work should decide it.

Cover: the gate card (Control Magic), `Modification.SetControllerToSource` and why a payload-free derived constructor was needed (card data cannot name a `PlayerId`), `controllerOf`'s second source of control and the CR 613.7 timestamp merge, the lean-gather-not-`gather` constraint and why (mutual recursion through `affects`), the hoisting, the #37-shaped cycle escape, and the CR 800.4a re-derivation. Note that #33 and #62 are closed.

**Replace** `CLAUDE.md`'s status bullet — never append.

- [ ] **Step 3: Final verification**

Run: `cabal clean && cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: warning-free, all green.

Confirm #33 and #62 are closed, and that the phase's new deferral issues are open.

- [ ] **Step 4: Commit**

```bash
git add docs/progress.md CLAUDE.md
hooky fix
git add docs/progress.md CLAUDE.md
hooky run
git commit -m "docs(auras): record control from a static ability"
```

- [ ] **Step 5: Confirm every step is ticked**

Run: `grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-25-auras-b-control-magic.md`
Expected: `0`. Use *that* grep, not `grep -c -- '- \[ \]'` — the header quotes the checkbox syntax in prose.

---

## Deferrals — already filed

Filed before execution, so every citation in the code is a real number:

1. **#195** — a control-granting static ability with an `Affected.Matching` set is unsupported. `controllerOf`'s lean fold cannot evaluate a `Filter`; that needs a projection, and projecting from `controllerOf` would be mutually recursive with `Projection.affects`. **Cited in `controllerOfGiven`'s `Matching` arm (Task 2).**
2. **#196** — layer 6 (Humility) does not stop a control-granting static ability, for the same non-projecting reason. **Cite it in `controlGrants`'s liveness comment (Task 2).**
3. **CR 613.8b dependency ordering for layer 2** joins the existing **#11**; no new issue.

No comment may carry an expiry — only what is not implemented, plus the number.
