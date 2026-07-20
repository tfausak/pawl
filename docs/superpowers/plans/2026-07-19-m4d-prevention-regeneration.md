# M4d Prevention & Regeneration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add the two remaining replacement-shield shapes — damage **prevention** (the cancel shape, Fog) and **regeneration** (a one-shot destruction shield through a unified `Event.destroy` funnel, Drudge Skeletons).

**Architecture:** Two internal phases. Phase 1 tags each `DamageEvent` with a `DamageKind` (Combat/Noncombat), adds a floating `Prevention` store to `GameState`, and hooks `Event.applyPreventions` into the head of the `Damage.applyDamage` funnel so a matching combat event is *dropped* (never marked, never drained). Phase 2 adds `Event.destroy` — the single chokepoint every destruction flows through (the `Destroy` opcode and the CR 704.5g/h state-based actions) — consulting CR 700.4 indestructibility and a one-shot `regenerationShields` count; `Effect.RegenerateSelf` installs the shield, and `Sba` is split so toughness-≤-0 (CR 704.5f) stays a plain put-into-graveyard.

**Tech Stack:** Haskell 2010 (GHC 9.14.1), `tasty`/`tasty-hunit`/`tasty-quickcheck`, hand-rolled JSON codec. Cards are JSON data under `data/cards/`.

## Global Constraints

- **Haskell 2010, no language extensions** beyond `GADTs`, `RankNTypes`, `NamedFieldPuns` (per module needs). No `LambdaCase`, `OverloadedStrings`, etc.
- **Warning-clean under `-Weverything` minus the allow-list, with `-Werror`** (the `pedantic` flag). Adding a field to the `MkDamageEvent` record makes every positional construction fail to compile, and adding an `Effect`/`Subtype` constructor makes every `case` on it non-exhaustive — a build error — so every site must gain the new value in the same task. Incremental builds hide warnings from unchanged modules; run `cabal clean` before a definitive check.
- **Build everything:** `cabal build all --enable-tests --enable-benchmarks` (suites break separately from the library).
- **No partial functions** (no `head`/`error`/`undefined`/non-exhaustive matches). `Maybe`/`Either`.
- **`newtype`/constructors take a `Mk` prefix; never pun** type and constructor names. `Text` not `String`. Arbitrary-precision numbers (`Natural`/`Integer`), never fixed-width.
- **No boolean blindness** — a custom sum type beats a bare `Bool` (hence `DamageKind`, not `isCombat :: Bool`).
- **Qualified imports aliased to the last component** (`Data.Map.Strict` → `Map`); operators unqualified. Shared test fixtures are `Pawl.Support` imported `qualified ... as S`.
- **One type per module** under `Pawl.Type.<Name>`; logic in other `Pawl.*` modules. `Pawl.Resolve` is the sole `case`-on-`Effect` home; `Pawl.Event` the sole `case`-on-`ReplacementEffect`/`TriggerCondition`/**`Prevention`** home; `Pawl.Codec` may `case` on any of them for serialization.
- **Every rules claim cites a CR number in a code comment**, checked against `docs/rules.txt` — never recalled. CR anchors this milestone uses: 615.1/615.4/615.6 (prevention), 514.2 (cleanup expiry), 700.4 (indestructible), 701.19a/701.19c (regeneration), 704.5f/704.5g/704.5h (creature-death SBAs), 614.7 (a replaced event that never happens).
- **Module-cycle fact (verified):** `Pawl.Combat` imports `Pawl.Sba`, which imports `Pawl.Event`. Therefore **`Pawl.Event` must NOT import `Pawl.Combat`.** `Event.destroy`'s "remove from combat" edits `GameState.combat` directly through the type module `Pawl.Type.Combat` (safe — a type module, no logic imports). `Pawl.Damage` importing `Pawl.Event` is acyclic (Event imports neither Damage nor Combat).
- **TDD:** write each failing test, run it to watch it fail, then implement. One small complete commit per task. Before "done": `cabal build all …` warning-free, `git add -A` then `hooky fix`, `git add -A` then `hooky run` passes, HLint applied.
- **Test-run filter:** run one group with `cabal test --test-options='-p "<pattern>"'`.
- **Scope note:** CR 701.19c "can't be regenerated" is **deferred to Wrath of God** (spec §"Non-goals" and §7). Do NOT add a `Regenerability` argument; `Event.destroy` is ungated. No mass-destroy opcode in this milestone.

---

## File Structure

**Library (create):**
- `source/library/Pawl/Type/DamageKind.hs` — `Combat | Noncombat`.
- `source/library/Pawl/Type/Prevention.hs` — `PreventAllCombatDamage`.
- `source/library/Pawl/Type/ActivePrevention.hs` — `{ prevention, duration }`.

**Library (modify):**
- `source/library/Pawl/Type/DamageEvent.hs` — add `kind :: DamageKind`.
- `source/library/Pawl/Type/GameState.hs` — add `preventions :: [ActivePrevention]` and `regenerationShields :: Map ObjectId Natural`.
- `source/library/Pawl/Type/Effect.hs` — add `Prevent Duration Prevention` and `RegenerateSelf`.
- `source/library/Pawl/Type/Subtype.hs` — add `Skeleton`.
- `source/library/Pawl/Damage.hs` — every `MkDamageEvent` gets `DamageKind.Combat`; `applyDamage` consults preventions at the head.
- `source/library/Pawl/Resolve.hs` — the `DealDamage` `MkDamageEvent` gets `DamageKind.Noncombat`; `Prevent`/`RegenerateSelf` arms in the five classifications + `applyEffect`; the `Destroy` arm calls `Event.destroy`.
- `source/library/Pawl/Event.hs` — `applyPreventions`, `dropEndOfTurnPreventions`, `clearRegenerationShields`, and `destroy` (+ its `regenerate`/`removeFromCombat` helpers).
- `source/library/Pawl/Sba.hs` — split the creature-death SBA: destruction (704.5g/h) → `Event.destroy`; toughness ≤ 0 (704.5f) → `changeZone Graveyard`.
- `source/library/Pawl/Setup.hs` — initialize the two new `GameState` fields.
- `source/library/Pawl/Engine.hs` — cleanup drops end-of-turn preventions and clears regeneration shields.
- `source/library/Pawl/Codec.hs` — `Prevent`/`RegenerateSelf` effect arms, a `Prevention` codec, the `Skeleton` subtype arm.

**Data (create):**
- `data/cards/fog.json`, `data/cards/drudge-skeletons.json`.

**Test suite (modify):**
- `source/test-suite/Pawl/Support.hs` — `addRegenShield` helper; `addPrevention` helper.
- `source/test-suite/Pawl/Cards.hs` — wire `fogPrinting`, `drudgeSkeletonsPrinting`; `allPrintings`; deck swaps.
- `source/test-suite/Pawl/CardSpec.hs` — pool-count assertion (39 → 41).
- `source/test-suite/Pawl/ResolveSpec.hs`, `DamageSpec.hs`, `EventSpec.hs`, `ActivateSpec.hs`, `PropertySpec.hs` — the tests below.

**cabal wiring:** three new `Pawl.Type.*` modules are discovered by the `-- cabal-gild: discover` directive — `hooky fix` regenerates `exposed-modules`; do not hand-edit. No new test-suite `Pawl.*Spec` module (tests land in existing specs), so no `Main.hs`/`other-modules` edit.

---

# Phase 1 — Damage prevention (the cancel shape)

## Task 1: `DamageKind` and the `DamageEvent.kind` field

**Files:**
- Create: `source/library/Pawl/Type/DamageKind.hs`
- Modify: `source/library/Pawl/Type/DamageEvent.hs`
- Modify: `source/library/Pawl/Damage.hs:93,99,120,135`, `source/library/Pawl/Resolve.hs:266`
- Modify (compiler-flagged test constructions): `source/test-suite/Pawl/DamageSpec.hs:107` and any other `MkDamageEvent` in the suite
- Test: `source/test-suite/Pawl/ResolveSpec.hs`

**Interfaces:**
- Produces: `DamageKind.DamageKind = Combat | Noncombat`; `DamageEvent.kind :: DamageEvent -> DamageKind`; the `MkDamageEvent` record now takes a 5th positional field `kind`.
- Consumes: nothing new.

- [x] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/ResolveSpec.hs` (its `tests` group; it already imports `Cards`, `Cast`, `Engine`, `Stack`, `Sba`, `S`, and uses the `landsInPlay`/`handOne` pattern). A resolved Lightning Bolt records a **Noncombat** damage event (read before any SBA drains it):

```haskell
      HU.testCase "CR 608 a resolved spell's damage is Noncombat" $
        let base = S.landsInPlay (Cards.mountainPrinting cards) 1
            (target, gs0) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            (gs1, spellId) = S.handOne (Cards.lightningBoltPrinting cards) gs0
            cast = snd (Engine.runGamePure S.identityAnswer gs1 (Cast.castSpell S.alice spellId))
            -- resolveTop applies the damage but does NOT run SBAs, so the event persists.
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in HU.assertEqual "the Bolt's damage event is Noncombat"
              [DamageKind.Noncombat]
              (map DamageEvent.kind (GameState.damageEvents resolved)),
```

Add to `ResolveSpec.hs` imports (skip any already present): `import qualified Pawl.Type.DamageEvent as DamageEvent`, `import qualified Pawl.Type.DamageKind as DamageKind`, `import qualified Pawl.Type.GameState as GameState`. The Bolt must actually target `target`; if `S.handOne`/`castSpell` needs an explicit target and `S.identityAnswer` does not supply one, model this on the existing Lightning Bolt cast test already in `ResolveSpec` (reuse its exact target-plumbing).

- [x] **Step 2: Run test to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL to compile — `DamageKind` and `DamageEvent.kind` do not exist, and every `MkDamageEvent` construction is now missing its 5th argument.

- [x] **Step 3: Create the `DamageKind` type**

Create `source/library/Pawl/Type/DamageKind.hs`:

```haskell
module Pawl.Type.DamageKind where

-- Whether a damage event is combat damage (CR 510) or damage from a resolving
-- spell or ability (CR 608). Read by Event.applyPreventions (Fog prevents only
-- combat damage, CR 615) and, later, by combat-damage triggers and lifelink. A
-- Bool would blind the reader to which it is (no-boolean-blindness).
data DamageKind = Combat | Noncombat
  deriving (Eq, Ord, Show)
```

- [x] **Step 4: Add the `kind` field to `DamageEvent`**

In `source/library/Pawl/Type/DamageEvent.hs`, add `import Pawl.Type.DamageKind (DamageKind)` and the field (as the last field):

```haskell
    dealtByDeathtouch :: Bool,
    -- CR 510 vs CR 608: combat damage or not. Set at deal time -- Damage tags
    -- Combat, Resolve's DealDamage tags Noncombat. Read by Event.applyPreventions.
    kind :: DamageKind
  }
```

- [x] **Step 5: Tag every construction site**

The four combat sites in `source/library/Pawl/Damage.hs` (lines 93, 99, 120, 135) each append `DamageKind.Combat`; the one resolving-spell site in `source/library/Pawl/Resolve.hs:266` appends `DamageKind.Noncombat`. Add `import qualified Pawl.Type.DamageKind as DamageKind` to both modules. Examples:

`Damage.hs:93`:
```haskell
              pure [DamageEvent.MkDamageEvent attacker (Recipient.ToPlayer defender) power (Projection.hasKeyword Keyword.Deathtouch attacker gs) DamageKind.Combat]
```
`Damage.hs:120` (the `toEvent` helper):
```haskell
              let toEvent (recipient, n) = DamageEvent.MkDamageEvent attacker recipient n (Projection.hasKeyword Keyword.Deathtouch attacker gs) DamageKind.Combat
```
`Resolve.hs:266`:
```haskell
              else Damage.applyDamage [DamageEvent.MkDamageEvent source recipient (fromInteger n) (Projection.hasKeyword Keyword.Deathtouch source gs) DamageKind.Noncombat] gs
```
Apply the analogous edit to `Damage.hs:99` and `Damage.hs:135` (both `DamageKind.Combat`). Then fix every compiler-flagged **test** construction (e.g. `DamageSpec.hs:107`), each with `DamageKind.Combat` (test-built combat damage) unless the test is explicitly about spell damage:
```haskell
            damaged = Damage.applyDamage [DamageEvent.MkDamageEvent srcId (Recipient.ToCreature tokId) 2 False DamageKind.Combat] gs2
```
Add `import qualified Pawl.Type.DamageKind as DamageKind` to each flagged test file.

- [x] **Step 6: Run the test to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "Noncombat"'`
Expected: PASS.

- [x] **Step 7: Full suite green (behavior-preserving)**

Run: `cabal test`
Expected: PASS — the field is recorded but not yet read anywhere, so behavior is unchanged.

- [x] **Step 8: Commit**

```bash
git add -A
git commit -m "M4d: DamageEvent.kind (Combat/Noncombat)

The damage payload grows a DamageKind so prevention can watch only combat
damage (CR 615). Every combat MkDamageEvent tags Combat; Resolve's DealDamage
tags Noncombat. Behavior-preserving -- the field is not yet read.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 2: The prevention store and the cancel seam in the damage funnel

**Files:**
- Create: `source/library/Pawl/Type/Prevention.hs`, `source/library/Pawl/Type/ActivePrevention.hs`
- Modify: `source/library/Pawl/Type/GameState.hs`, `source/library/Pawl/Setup.hs:57-85`
- Modify: `source/library/Pawl/Event.hs`, `source/library/Pawl/Damage.hs:155-166`
- Modify: `source/test-suite/Pawl/Support.hs`
- Test: `source/test-suite/Pawl/DamageSpec.hs`

**Interfaces:**
- Produces: `Prevention.Prevention = PreventAllCombatDamage`; `ActivePrevention.ActivePrevention` (`MkActivePrevention { prevention, duration }`); `GameState.preventions :: [ActivePrevention]`; `Event.applyPreventions :: [ActivePrevention] -> [DamageEvent] -> [DamageEvent]`; `Event.dropEndOfTurnPreventions :: GameState -> GameState`; `S.addPrevention :: ActivePrevention.ActivePrevention -> GameState -> GameState`.
- Consumes: `DamageEvent.kind` (Task 1), `Duration.Duration`, `Damage.applyDamage`.

- [x] **Step 1: Write the failing tests**

Add to `source/test-suite/Pawl/DamageSpec.hs`. A seeded "prevent all combat damage" shield drops a combat event but not a Noncombat one; `dropEndOfTurnPreventions` removes an until-end-of-turn shield:

```haskell
      HU.testCase "CR 615 a prevention drops combat damage but spares Noncombat" $
        let base = Setup.emptyGame S.bothPlayers
            (victim, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice base
            shield = ActivePrevention.MkActivePrevention Prevention.PreventAllCombatDamage Duration.UntilEndOfTurn
            withShield = S.addPrevention shield gs0
            combat = Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False DamageKind.Combat] withShield
            spell = Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False DamageKind.Noncombat] withShield
         in do
              HU.assertEqual "combat damage prevented -- none marked" (Just 0) (S.damageOf victim combat)
              HU.assertEqual "combat damage prevented -- no event recorded" [] (GameState.damageEvents combat)
              HU.assertEqual "noncombat damage still dealt" (Just 2) (S.damageOf victim spell),
      HU.testCase "CR 514.2 an until-end-of-turn prevention wears off at cleanup" $
        let base = Setup.emptyGame S.bothPlayers
            shield = ActivePrevention.MkActivePrevention Prevention.PreventAllCombatDamage Duration.UntilEndOfTurn
            dropped = Event.dropEndOfTurnPreventions (S.addPrevention shield base)
         in HU.assertEqual "no preventions remain" [] (GameState.preventions dropped),
```

Add imports to `DamageSpec.hs` (skip any present): `Setup`, `Event`, `Prevention`, `ActivePrevention`, `Duration`, `GameState`. (`Damage`, `DamageEvent`, `DamageKind`, `Recipient`, `Cards`, `S` are present after Task 1.)

- [x] **Step 2: Run tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL to compile — `Prevention`, `ActivePrevention`, `GameState.preventions`, `Event.applyPreventions`/`dropEndOfTurnPreventions`, `S.addPrevention` do not exist.

- [x] **Step 3: Create the two leaf types**

`source/library/Pawl/Type/Prevention.hs`:
```haskell
module Pawl.Type.Prevention where

-- CR 615.1a: a prevention effect specification, classified by the damage events
-- it watches and cancels. PreventAllCombatDamage watches every Combat-kind event
-- and drops it (Fog). Its own leaf family, distinct from Effect (one-shot),
-- Modification (continuous, layered), and ReplacementEffect (zone-change redirect).
-- Only Pawl.Event may case on it. Grows PreventFromSource / PreventNextN as cards
-- need them (spec section 8).
data Prevention = PreventAllCombatDamage
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/ActivePrevention.hs`:
```haskell
module Pawl.Type.ActivePrevention where

import Pawl.Type.Duration (Duration)
import Pawl.Type.Prevention (Prevention)

-- A floating, resolution-generated prevention effect (CR 615.3), held in
-- GameState.preventions. `duration` decides when cleanup drops it (CR 514.2) --
-- the prevention analog of ContinuousEffect for the event pipeline rather than
-- the projection. No timestamp (Fog needs no ordering; CR 615.7's multi-source
-- choice is deferred) and no source (CR 615.13 "prevented" triggers are deferred).
data ActivePrevention = MkActivePrevention
  { prevention :: Prevention,
    duration :: Duration
  }
  deriving (Eq, Ord, Show)
```

- [x] **Step 4: Add the `GameState.preventions` field and initialize it**

In `source/library/Pawl/Type/GameState.hs`, add `import Pawl.Type.ActivePrevention (ActivePrevention)` and the field (place it beside `continuousEffects`):
```haskell
    -- CR 615.3: floating prevention effects from resolutions (Fog), each with a
    -- duration cleanup consults (CR 514.2). The event-pipeline analog of
    -- continuousEffects. Event.applyPreventions reads it.
    preventions :: [ActivePrevention],
```
In `source/library/Pawl/Setup.hs`, `emptyGame`'s record, add:
```haskell
          GameState.preventions = [],
```

- [x] **Step 5: Add `applyPreventions` and `dropEndOfTurnPreventions` to `Event`**

In `source/library/Pawl/Event.hs`, add imports `import qualified Pawl.Type.ActivePrevention as ActivePrevention`, `import Pawl.Type.DamageEvent (DamageEvent)`, `import qualified Pawl.Type.DamageEvent as DamageEvent`, `import qualified Pawl.Type.DamageKind as DamageKind`, `import Pawl.Type.Prevention (Prevention)`, `import qualified Pawl.Type.Prevention as Prevention`, `import qualified Pawl.Type.Duration as Duration`. Add:

```haskell
-- CR 615.6: apply active prevention shields to a batch of damage events, dropping
-- each event a shield cancels -- a prevented event never happens (not marked, not
-- drained, never emitted). The cancel shape, as applyReplacements is the redirect
-- shape. This module is the sole home of casing on Prevention.
applyPreventions :: [ActivePrevention.ActivePrevention] -> [DamageEvent] -> [DamageEvent]
applyPreventions preventions events = filter (not . prevented) events
  where
    prevented ev = any (\p -> cancels (ActivePrevention.prevention p) ev) preventions

-- Does this prevention cancel this event? The Prevention case lives here.
cancels :: Prevention -> DamageEvent -> Bool
cancels p ev = case p of
  Prevention.PreventAllCombatDamage -> DamageEvent.kind ev == DamageKind.Combat

-- CR 514.2: at cleanup, drop until-end-of-turn preventions (the prevention analog
-- of Projection.dropEndOfTurnEffects). Indefinite preventions, if ever added, stay.
dropEndOfTurnPreventions :: GameState -> GameState
dropEndOfTurnPreventions gs =
  let keep p = ActivePrevention.duration p /= Duration.UntilEndOfTurn
   in gs {GameState.preventions = filter keep (GameState.preventions gs)}
```

- [x] **Step 6: Hook the prevention step into `Damage.applyDamage`**

In `source/library/Pawl/Damage.hs`, add `import qualified Pawl.Event as Event`, and rewrite `applyDamage` to consult preventions at the head — only the surviving events are marked and recorded:

```haskell
applyDamage :: [DamageEvent.DamageEvent] -> GameState -> GameState
applyDamage events gs =
  -- CR 615: prevention is the head of the funnel -- a prevented event never
  -- happens, so it is neither marked/drained nor recorded (no deathtouch bit for
  -- the CR 704.5h SBA to read).
  let kept = Event.applyPreventions (GameState.preventions gs) events
      markOne g ev = case DamageEvent.target ev of
        Recipient.ToCreature oid ->
          let hurt obj = obj {Object.damage = Object.damage obj + DamageEvent.amount ev}
           in g {GameState.objects = Map.adjust hurt oid (GameState.objects g)}
        Recipient.ToPlayer pid ->
          let drain player = player {Player.life = Player.life player - toInteger (DamageEvent.amount ev)}
           in g {GameState.players = Map.adjust drain pid (GameState.players g)}
        Recipient.ToObject _ -> g
      marked = List.foldl' markOne gs kept
   in marked {GameState.damageEvents = GameState.damageEvents marked ++ kept}
```

(Only the `let` binding of `kept` and the two `events` → `kept` substitutions change; the `markOne` body is unchanged. `Pawl.Damage` importing `Pawl.Event` is acyclic per Global Constraints.)

- [x] **Step 7: Add the `S.addPrevention` fixture**

In `source/test-suite/Pawl/Support.hs`, add (with `import qualified Pawl.Type.ActivePrevention as ActivePrevention` if absent):
```haskell
-- Seed a floating prevention shield directly into GameState (bypasses casting a
-- prevention spell; use when a test needs a shield active without resolving Fog).
addPrevention :: ActivePrevention.ActivePrevention -> GameState.GameState -> GameState.GameState
addPrevention shield gs =
  gs {GameState.preventions = shield : GameState.preventions gs}
```

- [x] **Step 8: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "prevention"'`
Expected: PASS (both new cases).

- [x] **Step 9: Full suite green**

Run: `cabal test`
Expected: PASS — with no preventions in `GameState` anywhere else, `applyPreventions [] = id`, so every existing damage test is unchanged.

- [x] **Step 10: Commit**

```bash
git add -A
git commit -m "M4d: the prevention store + the cancel seam (CR 615)

Prevention (PreventAllCombatDamage) and ActivePrevention (spec + duration) land
in a new GameState.preventions store. Event.applyPreventions -- the sole caser on
Prevention -- drops each combat event a shield cancels, hooked into the head of
Damage.applyDamage (the seam that module reserved). dropEndOfTurnPreventions is
the CR 514.2 wear-off. No card yet installs one.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 3: `Effect.Prevent`, cleanup wiring, and Fog (Phase 1 gate)

**Files:**
- Modify: `source/library/Pawl/Type/Effect.hs`
- Modify: `source/library/Pawl/Resolve.hs` (`slotsOf`, `readsX`, `manaProduced`, `searchesLibrary`, `rewriteEffect`, `applyEffect`)
- Modify: `source/library/Pawl/Codec.hs` (`preventionToJson`/`jsonToPrevention`, `effectToJson`/`jsonToEffect`)
- Modify: `source/library/Pawl/Engine.hs:167-172`
- Create: `data/cards/fog.json`
- Modify: `source/test-suite/Pawl/Cards.hs`, `source/test-suite/Pawl/CardSpec.hs:170`
- Test: `source/test-suite/Pawl/ResolveSpec.hs`

**Interfaces:**
- Produces: `Effect.Prevent :: Duration -> Prevention -> Effect card`; `Codec.preventionToJson`/`jsonToPrevention`; `Cards.fogPrinting :: Cards -> Printing`.
- Consumes: `Event.dropEndOfTurnPreventions` (Task 2), `Cast.castSpell`, `Stack.resolveTop`.

- [x] **Step 1: Write the failing gate tests**

Add to `source/test-suite/Pawl/ResolveSpec.hs`. alice casts Fog (a targetless `{G}` instant); after it resolves, a combat damage event is fully prevented while a Noncombat one still lands:

```haskell
      HU.testCase "CR 615 Fog prevents combat damage but not spell damage (the gate)" $
        let base = S.landsInPlay (Cards.forestPrinting cards) 1
            (victim, gs0) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            (gs1, fogId) = S.handOne (Cards.fogPrinting cards) gs0
            cast = snd (Engine.runGamePure S.identityAnswer gs1 (Cast.castSpell S.alice fogId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
            combat = Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False DamageKind.Combat] resolved
            spell = Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False DamageKind.Noncombat] resolved
         in do
              HU.assertEqual "Fog installed one prevention" 1 (length (GameState.preventions resolved))
              HU.assertEqual "combat damage prevented (the cancel shape)" (Just 0) (S.damageOf victim combat)
              -- The falsifier: a tag-blind Fog would also blunt this spell damage.
              HU.assertEqual "spell damage untouched (Noncombat)" (Just 2) (S.damageOf victim spell),
```

- [x] **Step 2: Run to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL to compile — `Cards.fogPrinting` undefined, `Effect.Prevent` undefined, non-exhaustive `Effect` cases.

- [x] **Step 3: Add the `Prevent` opcode**

In `source/library/Pawl/Type/Effect.hs`, add `import Pawl.Type.Prevention (Prevention)` and the constructor:

```haskell
  | -- CR 615.3: install a floating prevention effect for a duration. Fog =
    -- Prevent UntilEndOfTurn PreventAllCombatDamage. Targetless (Fog watches a
    -- class of events, not a chosen object). Resolve stores it into
    -- GameState.preventions; Event.applyPreventions applies it.
    Prevent Duration Prevention
```

- [x] **Step 4: Add the five `Resolve` classification arms and the executor**

In `source/library/Pawl/Resolve.hs` (add `import qualified Pawl.Type.Prevention as Prevention` is NOT needed — Resolve never cases on the Prevention; it just carries it). Add the arms:

`slotsOf`: `Effect.Prevent _ _ -> Set.empty`
`readsX`'s `effectReadsX`: `Effect.Prevent _ _ -> False`
`manaProduced`: `Effect.Prevent _ _ -> Nothing`
`searchesLibrary`: `Effect.Prevent _ _ -> False`
`rewriteEffect`: `Effect.Prevent _ _ -> effect`

The `applyEffect` executor arm (targetless; append the shield). Add `import qualified Pawl.Type.ActivePrevention as ActivePrevention`:
```haskell
  Effect.Prevent duration prevention ->
    -- CR 615.3: install the shield; Event.applyPreventions consults it at each
    -- damage funnel until cleanup drops it (CR 514.2). Targetless and unprompted.
    State.modify' $ \gs ->
      gs {GameState.preventions = ActivePrevention.MkActivePrevention prevention duration : GameState.preventions gs}
```

- [x] **Step 5: Add the codec arms**

In `source/library/Pawl/Codec.hs`, add a `Prevention` codec (place near `durationToJson`) and add `import qualified Pawl.Type.Prevention as Prevention`:
```haskell
preventionToJson :: Prevention.Prevention -> Value
preventionToJson p = nullary . Text.pack $ case p of
  Prevention.PreventAllCombatDamage -> "PreventAllCombatDamage"

jsonToPrevention :: Value -> Either Text Prevention.Prevention
jsonToPrevention =
  decodeNullary
    (Text.pack "Prevention")
    [(Text.pack "PreventAllCombatDamage", Prevention.PreventAllCombatDamage)]
```
`effectToJson`:
```haskell
  Effect.Prevent d p -> Json.tagged (Text.pack "Prevent") (Just (Array [durationToJson d, preventionToJson p]))
```
`jsonToEffect`:
```haskell
    "Prevent" -> case mv of
      Just (Array [d, p]) -> Effect.Prevent <$> jsonToDuration d <*> jsonToPrevention p
      _ -> Left (Text.pack "Prevent expects [Duration, Prevention]")
```

- [x] **Step 6: Wire cleanup to drop end-of-turn preventions**

In `source/library/Pawl/Engine.hs`, the `Phase.Ending EndingStep.Cleanup` arm, add the drop beside the existing wear-offs:
```haskell
      State.modify' Damage.removeAllDamage
      State.modify' Projection.dropEndOfTurnEffects
      State.modify' Event.dropEndOfTurnPreventions
```
(Add `import qualified Pawl.Event as Event` to `Engine.hs` if absent — check the existing imports first.)

- [x] **Step 7: Create the Fog card data file**

Create `data/cards/fog.json`:
```json
{"name":"Fog","manaCost":[{"type":"OfType","value":{"type":"Colored","value":{"type":"Green"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Instant"}],"subtypes":[]},"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"effects":[{"type":"Prevent","value":[{"type":"UntilEndOfTurn"},{"type":"PreventAllCombatDamage"}]}],"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"targetSpecs":[]}
```

- [x] **Step 8: Wire the printing into the pool**

In `source/test-suite/Pawl/Cards.hs`, add `fogPrinting` following the `mindRotPrinting`/`dragonFodderPrinting` pattern: the `MkCards` field, the `loadCards` binding (`fogPrinting_ <- loadPrinting "fog"`), the record entry, and the `allPrintings` list entry. Bump the count in `source/test-suite/Pawl/CardSpec.hs:170`:
```haskell
        HU.assertEqual "count" 40 (length (Cards.allPrintings cards)),
```

- [x] **Step 9: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "Fog"'` then `cabal test --test-options='-p "round-trip"'`
Expected: PASS — the Fog gate, plus the `allPrintings` honesty round-trip now covering Fog's `Prevent`/`Prevention`/`Duration`.

- [x] **Step 10: Full suite green**

Run: `cabal test`
Expected: PASS.

- [x] **Step 11: Commit**

```bash
git add -A
git commit -m "M4d: Effect.Prevent + Fog (the cancel gate)

Effect.Prevent Duration Prevention installs a floating shield; Resolve gains the
executor and its five classifications, Codec serializes it (with a Prevention
codec), and cleanup drops end-of-turn preventions (CR 514.2). Fog ({G}, prevent
all combat damage this turn) is the gate: after it resolves, combat damage is
cancelled while spell damage is untouched (the DamageKind falsifier).

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

# Phase 2 — Regeneration (the replace-with-side-effects shape)

## Task 4: The regeneration-shield store and `Effect.RegenerateSelf`

**Files:**
- Modify: `source/library/Pawl/Type/GameState.hs`, `source/library/Pawl/Setup.hs`
- Modify: `source/library/Pawl/Type/Effect.hs`, `source/library/Pawl/Resolve.hs`, `source/library/Pawl/Codec.hs`
- Modify: `source/library/Pawl/Event.hs` (`clearRegenerationShields`), `source/library/Pawl/Engine.hs`
- Modify: `source/test-suite/Pawl/Support.hs`
- Test: `source/test-suite/Pawl/EventSpec.hs`

**Interfaces:**
- Produces: `GameState.regenerationShields :: Map ObjectId Natural`; `Effect.RegenerateSelf :: Effect card`; `Event.clearRegenerationShields :: GameState -> GameState`; `S.addRegenShield :: ObjectId -> GameState -> GameState`; `Codec` handles `RegenerateSelf`.
- Consumes: `Quantity`-free (no count); the shield map.

- [x] **Step 1: Write the failing tests**

Add to `source/test-suite/Pawl/EventSpec.hs`. The helper installs a shield; cleanup clears all shields:

```haskell
      HU.testCase "CR 701.19a a regeneration shield is stored per object" $
        let base = Setup.emptyGame S.bothPlayers
            (oid, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice base
            shielded = S.addRegenShield oid gs0
         in HU.assertEqual "one shield on the object" (Just 1) (Map.lookup oid (GameState.regenerationShields shielded)),
      HU.testCase "CR 701.19a regeneration shields are cleared at cleanup (this turn)" $
        let base = Setup.emptyGame S.bothPlayers
            (oid, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice base
            cleared = Event.clearRegenerationShields (S.addRegenShield oid gs0)
         in HU.assertEqual "no shields remain" True (Map.null (GameState.regenerationShields cleared)),
```

Add imports to `EventSpec.hs` as needed: `Setup`, `Map`, `GameState`, `Cards`, `S`.

- [x] **Step 2: Run to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL to compile — `GameState.regenerationShields`, `Event.clearRegenerationShields`, `S.addRegenShield` undefined.

- [x] **Step 3: Add the store field and initialize it**

In `source/library/Pawl/Type/GameState.hs`, add `import Data.Map.Strict (Map)` (already imported) and, beside `preventions`:
```haskell
    -- CR 701.19a: one-shot regeneration shields, counted per object (activating
    -- twice stacks two; each destruction consumes one). Keyed by the shielded
    -- object's id -- stable across regeneration (the creature stays on the
    -- battlefield). Cleared at cleanup ("this turn"). Event.destroy reads it.
    regenerationShields :: Map ObjectId Natural,
```
(Ensure `Numeric.Natural (Natural)` and `Pawl.Type.ObjectId (ObjectId)` are imported — both already are.)

In `source/library/Pawl/Setup.hs`, `emptyGame`'s record:
```haskell
          GameState.regenerationShields = Map.empty,
```

- [x] **Step 4: Add `Effect.RegenerateSelf`, its classifications, and the executor**

In `source/library/Pawl/Type/Effect.hs`:
```haskell
  | -- CR 701.19a/c: install a one-shot regeneration shield on THIS effect's source
    -- permanent (CR 608.2g) -- targetless and self-referential (Drudge Skeletons'
    -- "{B}: Regenerate this creature"). NOT the act of regenerating (701.19c): the
    -- shield fires later, at Event.destroy. A general "Regenerate target creature"
    -- is future (Regenerate SlotName). Executed by Resolve.applyEffect.
    RegenerateSelf
```
In `source/library/Pawl/Resolve.hs`, the five classifications:
`slotsOf`: `Effect.RegenerateSelf -> Set.empty`
`effectReadsX`: `Effect.RegenerateSelf -> False`
`manaProduced`: `Effect.RegenerateSelf -> Nothing`
`searchesLibrary`: `Effect.RegenerateSelf -> False`
`rewriteEffect`: `Effect.RegenerateSelf -> effect`
The `applyEffect` executor arm:
```haskell
  Effect.RegenerateSelf ->
    -- CR 701.19a: add one shield to the source permanent. Map.insertWith (+)
    -- stacks a second activation. A shield on a gone/non-battlefield source is
    -- harmless (nothing will destroy it).
    State.modify' $ \gs ->
      gs {GameState.regenerationShields = Map.insertWith (+) source 1 (GameState.regenerationShields gs)}
```

- [x] **Step 5: Add the codec arm**

In `source/library/Pawl/Codec.hs`, `effectToJson`: `Effect.RegenerateSelf -> nullary (Text.pack "RegenerateSelf")`; `jsonToEffect`: `"RegenerateSelf" -> Right Effect.RegenerateSelf`.

- [x] **Step 6: Add `clearRegenerationShields` and wire it to cleanup**

In `source/library/Pawl/Event.hs`:
```haskell
-- CR 701.19a: regeneration shields last "this turn," so cleanup clears every one.
clearRegenerationShields :: GameState -> GameState
clearRegenerationShields gs = gs {GameState.regenerationShields = Map.empty}
```
In `source/library/Pawl/Engine.hs`, the Cleanup arm:
```haskell
      State.modify' Event.dropEndOfTurnPreventions
      State.modify' Event.clearRegenerationShields
```

- [x] **Step 7: Add the `S.addRegenShield` fixture**

In `source/test-suite/Pawl/Support.hs`:
```haskell
-- Seed a regeneration shield directly onto an object (bypasses activating a
-- regenerate ability; use when a test needs a shield up without the activation).
addRegenShield :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
addRegenShield oid gs =
  gs {GameState.regenerationShields = Map.insertWith (+) oid 1 (GameState.regenerationShields gs)}
```

- [x] **Step 8: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "regeneration shield"'`
Expected: PASS (both cases).

- [x] **Step 9: Full suite green**

Run: `cabal test`
Expected: PASS — the shield store is written but not yet read by any destruction (Task 5 adds the reader).

- [x] **Step 10: Commit**

```bash
git add -A
git commit -m "M4d: regeneration-shield store + Effect.RegenerateSelf

GameState.regenerationShields (a per-object count) and Effect.RegenerateSelf
(install one shield on the source, CR 701.19a) land, with the five Resolve
classifications, the codec arm, and cleanup clearing shields ('this turn').
Event.destroy (Task 5) is the reader; nothing consumes a shield yet.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 5: `Event.destroy` — the unified destruction funnel, and the Destroy-opcode rewire

**Files:**
- Modify: `source/library/Pawl/Event.hs`
- Modify: `source/library/Pawl/Resolve.hs:350-363` (the `Destroy` arm)
- Test: `source/test-suite/Pawl/EventSpec.hs`, `source/test-suite/Pawl/ResolveSpec.hs`

**Interfaces:**
- Produces: `Event.destroy :: ObjectId -> GameState -> GameState` (CR 700.4 indestructible → no-op; CR 701.19a shield → consume + regenerate; else → `changeZone Graveyard`).
- Consumes: `Projection.hasKeyword` (Indestructible), `Event.changeZone`, `GameState.regenerationShields`, `GameState.combat`.

- [x] **Step 1: Write the failing tests**

Add to `source/test-suite/Pawl/EventSpec.hs` (funnel-level) and `source/test-suite/Pawl/ResolveSpec.hs` (the Murder gameplay path).

EventSpec:
```haskell
      HU.testCase "CR 701.19a Event.destroy consumes a shield and regenerates instead" $
        let base = Setup.emptyGame S.bothPlayers
            (oid, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice base
            damaged = S.markDamage oid 1 gs0
            shielded = S.addRegenShield oid damaged
            after = Event.destroy oid shielded
         in do
              HU.assertEqual "still on the battlefield (regenerated, not destroyed)" True (Set.member oid (GameState.battlefield after))
              HU.assertEqual "shield consumed" Nothing (Map.lookup oid (GameState.regenerationShields after))
              case Game.lookupObject oid after of
                Just obj -> do
                  HU.assertEqual "tapped (CR 701.19a)" TapState.Tapped (Object.tapped obj)
                  HU.assertEqual "damage removed (CR 701.19a)" 0 (Object.damage obj)
                Nothing -> HU.assertFailure "the creature vanished",
      HU.testCase "CR 701.19a a second destroy with no shield left kills it" $
        let base = Setup.emptyGame S.bothPlayers
            (oid, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice base
            once = Event.destroy oid (S.addRegenShield oid gs0)   -- regenerated
            twice = Event.destroy oid once                        -- no shield -> dies
         in HU.assertEqual "gone from the battlefield" False (Set.member oid (GameState.battlefield twice)),
      HU.testCase "CR 700.4 Event.destroy no-ops on an indestructible permanent" $
        let base = Setup.emptyGame S.bothPlayers
            (oid, gs0) = S.addCreature (Cards.darksteelMyrPrinting cards) S.alice base
            after = Event.destroy oid gs0
         in HU.assertEqual "indestructible survives" True (Set.member oid (GameState.battlefield after)),
```

ResolveSpec (the opcode path — Murder vs a shielded creature). This mirrors the existing `castBlackRemovalAt` helper (which casts a black removal spell at bob's lone creature via `S.identityAnswer`, which answers `ChooseTargets` by picking the single legal target), inlined so a shield can be seeded on the foe before the cast:
```haskell
      HU.testCase "CR 701.19a Murder is replaced by regeneration" $
        let base = S.landsInPlay (Cards.swampPrinting cards) 3
            (victim, withFoe) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            shielded = S.addRegenShield victim withFoe
            (gs, spellId) = S.handOne (Cards.murderPrinting cards) shielded
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = Sba.checkStateBasedActions (snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop))
         in HU.assertEqual "the shielded creature survived Murder" 1 (S.creaturesInPlay S.bob after),
```

Add imports as needed (EventSpec: `Set`, `Map`, `TapState`, `Object`, `Game`, `GameState`, `Setup`, `S`). `S.identityAnswer` (Support) answers `ChooseTargets` with `Map.mapMaybe Set.lookupMin sets` — the single legal creature — so with only `victim` on the board it targets it. (`ResolveSpec` already imports what this test needs.)

- [x] **Step 2: Run to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "701.19a"'`
Expected: FAIL — `Event.destroy` undefined (EventSpec), and Murder still buries the shielded creature (ResolveSpec).

- [x] **Step 3: Add `Event.destroy` and its helpers**

In `source/library/Pawl/Event.hs`, add `import qualified Pawl.Type.Combat as Combat`, `import qualified Pawl.Type.Keyword as Keyword` (skip if present). Add:

```haskell
-- The single destruction funnel (CR 701.7 / 700.4): every destruction -- the
-- Destroy opcode and the CR 704.5g/h state-based actions -- flows through here.
-- CR 700.4: an indestructible permanent can't be destroyed (the event never
-- happens, so a shield is neither applied nor consumed, CR 614.7). CR 701.19a: a
-- regeneration shield replaces the destruction. Otherwise the permanent is put
-- into its owner's graveyard via changeZone (so Rest in Peace's redirect and a
-- token's CR 704.5d cease-to-exist still compose). CR 701.19c "can't be
-- regenerated" is deferred to Wrath (spec section 7): this funnel is ungated.
destroy :: ObjectId -> GameState -> GameState
destroy oid gs = case Game.lookupObject oid gs of
  Nothing -> gs
  Just _ ->
    if Projection.hasKeyword Keyword.Indestructible oid gs
      then gs
      else case Map.lookup oid (GameState.regenerationShields gs) of
        Just n | n > 0 -> regenerate oid gs
        _ -> changeZone oid Zone.Graveyard gs

-- CR 701.19a: consume one shield, remove all marked damage, tap the permanent,
-- and remove it from combat. The permanent stays on the battlefield (same id).
regenerate :: ObjectId -> GameState -> GameState
regenerate oid gs =
  let shields = Map.update (\n -> if n <= 1 then Nothing else Just (n - 1)) oid (GameState.regenerationShields gs)
      healTap obj = obj {Object.damage = 0, Object.tapped = TapState.Tapped}
      gs1 =
        gs
          { GameState.regenerationShields = shields,
            GameState.objects = Map.adjust healTap oid (GameState.objects gs)
          }
   in removeFromCombat oid gs1

-- CR 701.19a: if it is attacking or blocking, remove it from combat. Edits the
-- GameState.combat maps directly (Event must not import Pawl.Combat -- that would
-- cycle through Sba; see the plan's Global Constraints).
removeFromCombat :: ObjectId -> GameState -> GameState
removeFromCombat oid gs =
  let c = GameState.combat gs
      c1 =
        c
          { Combat.attackers = Map.delete oid (Combat.attackers c),
            Combat.blockers = Map.map (Set.delete oid) (Map.delete oid (Combat.blockers c))
          }
   in gs {GameState.combat = c1}
```

- [x] **Step 4: Rewire the `Destroy` opcode to the funnel**

In `source/library/Pawl/Resolve.hs`, replace the `Effect.Destroy` arm's inline indestructible-check + changeZone with a call to `Event.destroy` (which now owns the CR 700.4 check):
```haskell
  Effect.Destroy slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs
          -- CR 701.7: destroy through the single funnel -- indestructible (CR
          -- 700.4) and regeneration (CR 701.19a) are Event.destroy's to decide.
          Just target -> Event.destroy target gs
        -- Illegal slot (CR 608.2b) or a non-object recipient: no-op.
        _ -> gs
```
(`Projection`/`Keyword` may now be unused in that arm but remain used elsewhere in `Resolve` — no import change.)

- [x] **Step 5: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "701.19a"'` then `cabal test --test-options='-p "700.4"'`
Expected: PASS — regenerate/consume/second-death (EventSpec), indestructible no-op (EventSpec), and Murder replaced (ResolveSpec).

- [x] **Step 6: Full suite green (Destroy behavior preserved for the normal case)**

Run: `cabal test`
Expected: PASS — with no shields and no indestructible, `Event.destroy = changeZone _ Graveyard`, identical to the pre-M4d Destroy; the Murder-vs-Darksteel-Myr (M4b) and every death test still pass.

- [x] **Step 7: Commit**

```bash
git add -A
git commit -m "M4d: Event.destroy -- the unified destruction funnel

Every destruction now flows through Event.destroy: CR 700.4 indestructible ->
no-op; CR 701.19a shield -> consume one, heal/tap/remove-from-combat, stay on the
battlefield; else changeZone to the graveyard (so RiP redirect and CR 704.5d still
compose). The Destroy opcode is rewired to it. Murder is replaced by a regen
shield; a second Murder with no shield kills. CR 701.19c 'can't be regenerated'
stays deferred to Wrath.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 6: Split the creature-death SBA (destruction vs. put-into-graveyard)

**Files:**
- Modify: `source/library/Pawl/Sba.hs:58-148`
- Test: `source/test-suite/Pawl/DamageSpec.hs`

**Interfaces:**
- Produces: `Sba.performStateBasedActions` routes CR 704.5g/h through `Event.destroy` and CR 704.5f through `changeZone Graveyard`. No new exported symbol (internal predicate split).
- Consumes: `Event.destroy` (Task 5), the existing `Projection.projectAll`, `woundedByDeathtouch`.

- [x] **Step 1: Write the failing tests**

Two tests, in the two specs that already own each idiom.

**(a) In `source/test-suite/Pawl/DamageSpec.hs`** — regeneration saves a creature from lethal *combat* damage (the SBA destruction path, via `applyDamage` + `checkStateBasedActions`, the DamageSpec idiom):

```haskell
      HU.testCase "CR 704.5g regeneration saves a creature from lethal combat damage" $
        let base = Setup.emptyGame S.bothPlayers
            (victim, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice base  -- 2/1
            shielded = S.addRegenShield victim gs0
            -- 2 combat damage is lethal to a 2/1; the shield replaces the CR 704.5g destruction.
            damaged = Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False DamageKind.Combat] shielded
            settled = Sba.checkStateBasedActions damaged
         in do
              HU.assertEqual "survived (regenerated)" True (Set.member victim (GameState.battlefield settled))
              case Game.lookupObject victim settled of
                Just obj -> do
                  HU.assertEqual "tapped" TapState.Tapped (Object.tapped obj)
                  HU.assertEqual "damage healed" 0 (Object.damage obj)
                Nothing -> HU.assertFailure "victim vanished",
```

**(b) In `source/test-suite/Pawl/ResolveSpec.hs`**, beside the existing "CR 704.5f indestructible does NOT save a creature with toughness <= 0" test — regeneration is a destruction shield, so it likewise does not save a creature at toughness ≤ 0. Reuse that test's exact `withEffect` local helper (it seeds a layer-7c `ModifyPowerToughness` continuous effect) with the same `-1` toughness modifier that drops a 1-toughness creature to 0:

```haskell
      HU.testCase "CR 704.5f regeneration does NOT save a creature with toughness <= 0" $
        let (victim, gs) = S.addCreature (Cards.pikerPrinting cards) S.bob (Setup.emptyGame S.bothPlayers)  -- 2/1
            -- A test-local -0/-1 drops the toughness to 0; 704.5f is a put-into-graveyard,
            -- not a destruction, so a regeneration shield cannot save it.
            zeroed = withEffect victim (Timestamp.MkTimestamp 5) (Modification.ModifyPowerToughness (Quantity.Literal 0) (Quantity.Literal (-1))) gs
            shielded = S.addRegenShield victim zeroed
            after = Sba.checkStateBasedActions shielded
         in HU.assertEqual "died despite the shield (704.5f is not a destruction)" 0 (S.creaturesInPlay S.bob after),
```

Add any missing imports (DamageSpec: `Setup`, `Object`, `TapState`, `Game`, `Set`, `GameState`; ResolveSpec's `withEffect` test already imports `Modification`, `Quantity`, `Timestamp`, `Setup`, `Sba`). No new Support helper is needed — `S.addRegenShield` (Task 4) and the local `withEffect` cover both.

- [x] **Step 2: Run to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "704.5"'`
Expected: FAIL — the shielded creature is buried by the current merged SBA (which routes everything through `changeZone`, never `Event.destroy`).

- [x] **Step 3: Split the SBA classification and routing**

In `source/library/Pawl/Sba.hs`, replace `creatureDies` with two predicates and rewrite the routing in `performStateBasedActions`. Add `import qualified Pawl.Type.Keyword as Keyword` (present) and rely on the existing `Event` import.

Replace `creatureDies` with:
```haskell
-- CR 704.5f: toughness 0 or less -- a put-into-graveyard, NOT a destruction, so
-- ungated by indestructible and NOT saved by regeneration (CR 701.19a).
zeroToughness :: PC.ProjectedCharacteristics -> Bool
zeroToughness pc =
  Set.member CardType.Creature (PC.cardTypes pc)
    && case PC.toughness pc of
      Nothing -> False
      Just t -> t <= 0

-- CR 704.5g/h: a creature destroyed by lethal marked damage or by a deathtouch
-- source. A DESTRUCTION -- indestructible-gated (CR 700.4) and regeneration-
-- interceptable (CR 701.19a via Event.destroy). Excludes 704.5f (that is
-- zeroToughness), so toughness here is > 0.
destroyedBySba :: GameState -> PC.ProjectedCharacteristics -> ObjectId -> Bool
destroyedBySba gs pc oid =
  let isCreature = Set.member CardType.Creature (PC.cardTypes pc)
      indestructible = Set.member Keyword.Indestructible (PC.keywords pc)
   in isCreature && not indestructible && case PC.toughness pc of
        Nothing -> False
        Just toughness ->
          toughness > 0
            && ( ( case Game.lookupObject oid gs of
                     Nothing -> False
                     Just obj -> toInteger (Object.damage obj) >= toughness
                 )
                   || woundedByDeathtouch gs oid
               )
```

In `performStateBasedActions`, replace the single `dying`/`bury`/`buried` block with a split that routes destructions through `Event.destroy` and zero-toughness through `changeZone`:
```haskell
      pcs = Projection.projectAll gs
      classify oid = case Map.lookup oid pcs of
        Nothing -> Nothing
        Just pc
          -- CR 704.5f wins when both apply: toughness <= 0 is a put-into-graveyard.
          | zeroToughness pc -> Just False
          | destroyedBySba gs pc oid -> Just True
          | otherwise -> Nothing
      onBattlefield = Set.toList (GameState.battlefield gs)
      toGraveyard = filter (\oid -> classify oid == Just False) onBattlefield
      toDestroy = filter (\oid -> classify oid == Just True) onBattlefield
      -- CR 704.5f: a plain put-into-graveyard (regeneration cannot save it).
      buried = List.foldl' (\g oid -> Event.changeZone oid Zone.Graveyard g) gs toGraveyard
      -- CR 704.5g/h: destruction through the funnel (regeneration may replace it).
      destroyed = List.foldl' (flip Event.destroy) buried toDestroy
      leaving = filter (losesNow destroyed) (stillPlaying destroyed)
      departed = foldr depart destroyed leaving
```
Then update the rest of the block to read from `departed` (unchanged from here on: `remaining`, the CR 704.5d `isVanishingToken`/`vanishing`/`ceaseToExist`/`vanished`, `outcome`, `drained`). Update the `acted` flag to count both routes (a regenerated creature still counts as a performed SBA, which the CR 704.4 settle loop re-checks and — because the regen healed the damage — terminates):
```haskell
      acted = not (null toGraveyard) || not (null toDestroy) || not (null leaving) || not (null vanishing)
```

(Delete the now-unused `creatureDies`; a `git grep creatureDies` confirms it has no other caller.)

- [x] **Step 4: Run the tests to verify they pass**

Run: `cabal test --test-options='-p "704.5"'`
Expected: PASS — regeneration saves the combat-lethal creature; toughness-≤-0 still dies.

- [x] **Step 5: Full suite green (existing deaths unchanged)**

Run: `cabal test`
Expected: PASS — for creatures with no shield, `Event.destroy = changeZone Graveyard`, so every existing lethal-damage/deathtouch death (M1b/M2c) and the CR 704.5d token cease-to-exist (M4c) behave exactly as before; the settle loop still terminates.

- [x] **Step 6: Commit**

```bash
git add -A
git commit -m "M4d: split the creature-death SBA (destruction vs put-into-graveyard)

Sba.creatureDies splits into zeroToughness (CR 704.5f, a put-into-graveyard) and
destroyedBySba (CR 704.5g/h, a destruction). performStateBasedActions routes 704.5f
through changeZone and 704.5g/h through Event.destroy, so a regeneration shield
saves a creature from lethal combat damage but never from toughness <= 0. Normal
deaths are unchanged (no shield -> Event.destroy == changeZone Graveyard).

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 7: Drudge Skeletons — the regeneration gate, end to end

**Files:**
- Modify: `source/library/Pawl/Type/Subtype.hs`, `source/library/Pawl/Codec.hs:132-173`
- Create: `data/cards/drudge-skeletons.json`
- Modify: `source/test-suite/Pawl/Cards.hs`, `source/test-suite/Pawl/CardSpec.hs:170`
- Test: `source/test-suite/Pawl/ActivateSpec.hs`, `source/test-suite/Pawl/ResolveSpec.hs`

**Interfaces:**
- Produces: `Subtype.Skeleton`; `Cards.drudgeSkeletonsPrinting :: Cards -> Printing`.
- Consumes: `Activate.activateAbility`, `Effect.RegenerateSelf` (Task 4), `Event.destroy` (Task 5).

- [x] **Step 1: Write the failing gate tests**

Add to `source/test-suite/Pawl/ActivateSpec.hs` (activation path — mirror the existing Prodigal Sorcerer / Llanowar Elves activation tests for the exact `Activate.activateAbility` plumbing and how the ability value is obtained from `Projection.abilitiesOf`). alice's Drudge Skeletons, with a Swamp for `{B}`, activates its regenerate ability and gains a shield; then Murder (bob's) is replaced:

```haskell
      HU.testCase "CR 701.19a Drudge Skeletons regenerates: activate, survive Murder, die to the next" $
        let base = S.landsInPlay (Cards.swampPrinting cards) 1
            (skel, gs0) = S.addCreature (Cards.drudgeSkeletonsPrinting cards) S.alice base
            ability = theAbility (Cards.drudgeSkeletonsPrinting cards)   -- the local ActivateSpec helper
            activated = snd (Engine.runGamePure S.identityAnswer gs0 (Activate.activateAbility S.alice skel ability))
            resolved = snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
            -- First Murder: replaced by the shield.
            firstKill = Sba.checkStateBasedActions (Event.destroy skel resolved)
            -- Second Murder: no shield -> dies.
            secondKill = Sba.checkStateBasedActions (Event.destroy skel firstKill)
         in do
              HU.assertEqual "a shield is up after resolving the ability" (Just 1) (Map.lookup skel (GameState.regenerationShields resolved))
              HU.assertEqual "survived the first destruction (regenerated)" True (Set.member skel (GameState.battlefield firstKill))
              HU.assertEqual "died to the second (one-shot shield consumed)" False (Set.member skel (GameState.battlefield secondKill)),
```

(The test uses `Event.destroy` directly as the "destroy source" for concision; a full Murder-cast variant already lives in `ResolveSpec` from Task 5. `theAbility` is the existing `ActivateSpec` local helper — `theAbility p = case Card.activatedAbilities (Printing.card p) of ab : _ -> ab; [] -> …` — the same one the Prodigal Sorcerer / Llanowar Elves activation tests use; copy those tests' exact `activateAbility` shape.)

Add imports to `ActivateSpec.hs` as needed: `Activate`, `Engine`, `Stack`, `Sba`, `Event`, `Map`, `Set`, `GameState`, `Cards`, `S` (skip any already present — `ActivateSpec` already imports most for the Prodigal/Llanowar tests).

- [x] **Step 2: Run to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL to compile — `Cards.drudgeSkeletonsPrinting` undefined; `Subtype.Skeleton` undefined (the card file references it, so `loadPrinting` would also fail).

- [x] **Step 3: Add the `Skeleton` subtype and its codec**

In `source/library/Pawl/Type/Subtype.hs`, add `| Skeleton` to the sum (place after `Myr`). In `source/library/Pawl/Codec.hs`, add the arm to both `subtypeToJson` (`Subtype.Skeleton -> "Skeleton"`) and the `jsonToSubtype` table (`(Text.pack "Skeleton", Subtype.Skeleton)`).

- [x] **Step 4: Create the Drudge Skeletons card data file**

Create `data/cards/drudge-skeletons.json` (cost `{1}{B}`, 1/1 Creature — Skeleton, a mana-only `{B}` regenerate ability with no additional cost, so a summoning-sick Skeleton can still regenerate — CR 302.6 gates only `{T}`):
```json
{"name":"Drudge Skeletons","manaCost":[{"type":"Generic","value":1},{"type":"OfType","value":{"type":"Colored","value":{"type":"Black"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Creature"}],"subtypes":[{"type":"Skeleton"}]},"power":{"type":"Literal","value":1},"toughness":{"type":"Literal","value":1},"keywords":[],"staticAbilities":[],"effects":[],"activatedAbilities":[{"cost":{"mana":[{"type":"OfType","value":{"type":"Colored","value":{"type":"Black"}}}],"additional":[]},"effects":[{"type":"RegenerateSelf"}],"targetSpecs":[]}],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"targetSpecs":[]}
```

- [x] **Step 5: Wire the printing into the pool**

In `source/test-suite/Pawl/Cards.hs`, add `drudgeSkeletonsPrinting` following the `dragonFodderPrinting` pattern (field, `loadCards` binding `drudgeSkeletonsPrinting_ <- loadPrinting "drudge-skeletons"`, record entry, `allPrintings` entry). Bump `source/test-suite/Pawl/CardSpec.hs:170`:
```haskell
        HU.assertEqual "count" 41 (length (Cards.allPrintings cards)),
```

- [x] **Step 6: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "Drudge"'` then `cabal test --test-options='-p "round-trip"'`
Expected: PASS — the end-to-end regenerate gate, plus the honesty round-trip now covering Drudge Skeletons (`RegenerateSelf`, the `{B}` ability cost, the `Skeleton` subtype).

- [x] **Step 7: Full suite green**

Run: `cabal test`
Expected: PASS.

- [x] **Step 8: Commit**

```bash
git add -A
git commit -m "M4d: Drudge Skeletons -- the regeneration gate

Subtype.Skeleton and Drudge Skeletons ({1}{B} 1/1, '{B}: Regenerate this
creature') land the Phase 2 gate end to end: activating the mana-only regenerate
ability (unaffected by summoning sickness -- no {T}) installs a shield through
Effect.RegenerateSelf; the next destruction is replaced (survive, tapped, healed),
and a second with no shield kills. Honesty round-trip covers the new card.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 8: The negative test and random-game deck coverage

**Files:**
- Modify: `source/test-suite/Pawl/Cards.hs` (`greenDeck`, `blackDeck`)
- Test: `source/test-suite/Pawl/ResolveSpec.hs`, `source/test-suite/Pawl/PropertySpec.hs`

**Interfaces:**
- Consumes: `Cards.fogPrinting`, `Cards.drudgeSkeletonsPrinting`, `S.addRegenShield`, the existing conservation property.

- [x] **Step 1: Write the failing negative test**

Add to `source/test-suite/Pawl/ResolveSpec.hs` — regeneration intercepts destruction, not every leave-the-battlefield: a shielded creature bounced by Unsummon (M4b) still returns to hand:

```haskell
      HU.testCase "CR 701.19a regeneration does not save a bounced creature" $
        let base = S.landsInPlay (Cards.islandPrinting cards) 1
            (victim, withFoe) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            shielded = S.addRegenShield victim withFoe
            (gs, spellId) = S.handOne (Cards.unsummonPrinting cards) shielded
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "the creature left the battlefield (bounce is not a destruction)" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "it is in bob's hand" 1 (length (Game.zoneMembers Zone.Hand S.bob after)),
```

(Mirrors the existing "CR 400.7 Unsummon returns a creature to its owner's hand" test in `ResolveSpec`, which uses `S.identityAnswer` — the only creature on the board is the target — with a shield seeded on the foe first.)

- [x] **Step 2: Run to verify it passes immediately**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "bounced"'`
Expected: PASS — Unsummon uses `MoveToZone _ Hand` (not `Destroy`), so it never touches `Event.destroy` and the shield is irrelevant. This test guards that the funnel split did not over-reach.

- [x] **Step 3: Add Fog and Drudge Skeletons to the decks (keeping 60)**

In `source/test-suite/Pawl/Cards.hs`:

`greenDeck` — swap four War Mammoths for four Fogs (forest 36 + warMammoth 12 + fog 4 + giantGrowth 4 + serpentsGift 4 = 60):
```haskell
      [ (forestPrinting cards, 36),
        (warMammothPrinting cards, 12),
        -- Fog swaps in for four War Mammoths to keep the deck at 60 (card-backed
        -- conservation stays 120) and give random green games combat-damage
        -- prevention coverage (CR 615).
        (fogPrinting cards, 4),
        (giantGrowthPrinting cards, 4),
        (serpentsGiftPrinting cards, 4)
      ]
```

`blackDeck` — swap four Typhoid Rats for four Drudge Skeletons (swamp 36 + typhoidRats 12 + drudge 4 + murder 4 + mindRot 4 = 60); Murder is already present as a destroy source:
```haskell
      [ (swampPrinting cards, 36),
        (typhoidRatsPrinting cards, 12),
        -- Drudge Skeletons swaps in for four Typhoid Rats (deck stays 60) so random
        -- black games exercise regeneration against Murder's destroy (CR 701.19a).
        (drudgeSkeletonsPrinting cards, 4),
        (murderPrinting cards, 4),
        (mindRotPrinting cards, 4)
      ]
```

- [x] **Step 4: Run the property suite with the new coverage**

Run: `cabal test --test-options='-p "Properties"'`
Expected: PASS — the card-backed conservation property (M4c) still holds at 120 in every matchup: Fog and Drudge are `OfCard`, and regeneration keeps the same object on the battlefield (no new incarnation, no mint), so nothing about conservation changes; every game still terminates.

- [x] **Step 5: Full suite green**

Run: `cabal test`
Expected: PASS.

- [x] **Step 6: Commit**

```bash
git add -A
git commit -m "M4d: negative test + random-game deck coverage

A shielded creature bounced by Unsummon still leaves (regeneration intercepts only
destruction, not every leave-the-battlefield). Fog joins greenDeck and Drudge
Skeletons joins blackDeck (deck-preserving 4-for-4 swaps, both stay 60) for random
prevention/regeneration coverage; card-backed conservation is unchanged.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Final verification (after all tasks)

- [x] **Definitive warning-clean build**

Run: `cabal clean && cabal build all --enable-tests --enable-benchmarks`
Expected: no warnings (the `pedantic` `-Werror` build succeeds).

- [x] **Whole suite**

Run: `cabal test`
Expected: all green.

- [x] **Format and lint**

Run: `git add -A && hooky fix && git add -A && hooky run`
Expected: passes (ormolu, hlint, cabal-gild — which regenerates `exposed-modules` for the three new `Pawl.Type.*` modules — cabal check, file hygiene). Apply any HLint suggestion or justify the exception.

- [x] **Progress check**

Run: `grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-19-m4d-prevention-regeneration.md`
Expected: `0`.

- [x] **Update the milestone log and status**

Add the M4d completion entry to `docs/progress.md` (one distilled entry: the two gate cards Fog and Drudge Skeletons; the decisions proved — the cancel replacement shape hooked into the damage funnel, and the unified `Event.destroy` funnel with a one-shot regeneration shield; the types/opcodes added — `DamageKind`, `Prevention`, `ActivePrevention`, `GameState.preventions`/`regenerationShields`, `Effect.Prevent`/`RegenerateSelf`, `Event.applyPreventions`/`destroy`, `Subtype.Skeleton`, the `Sba` destruction/put-into-graveyard split; and the named expiries from spec §7, foremost CR 701.19c deferred to Wrath). Update the "current work" note in `CLAUDE.md` (M4d complete; the next M4 letter — M4e counter-target-spell, per the design.md M4 table — is next). Commit:
```bash
git add -A && git commit -m "M4d: milestone completion log entry + status update

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Self-Review notes (for the executor)

- **Spec coverage:** Task 1 → `DamageKind` (spec §1, §2); Task 2 → `Prevention`/`ActivePrevention`/store/`applyPreventions`/funnel hook/expiry (spec §1, §2); Task 3 → `Effect.Prevent` + Fog gate + cleanup wiring (spec §1, §2, §Goal); Task 4 → `regenerationShields` + `RegenerateSelf` (spec §1, §3); Task 5 → `Event.destroy` + Destroy rewire (spec §3); Task 6 → the `Sba` destruction/put-into-graveyard split (spec §3); Task 7 → Drudge Skeletons gate + `Subtype.Skeleton` (spec §1, §Goal); Task 8 → the regeneration negative test + deck coverage (spec §Goal, §5). The first-strike "both waves prevented" claim (spec §Goal) is covered implicitly — Fog persists in `preventions` and `dealCombatDamage` calls `applyDamage` once per wave; if a dedicated fixture is wanted, add one to `DamageSpec` seeding a `PreventAllCombatDamage` shield and asserting both a first-strike and a regular wave are dropped.
- **Deferred (not in any task), by design:** CR 701.19c "can't be regenerated" (→ Wrath); CR 615.7 amount-shields and the multi-source choice; CR 615.10 static prevention; retaining prevented events for CR 615.13/615.5; general "Regenerate target creature"; CR 701.19b static regeneration; a distinct "was destroyed" event. Each is a spec §7 expiry — do not implement.
- **Module-cycle discipline:** `Event.destroy` edits `GameState.combat` through `Pawl.Type.Combat` (never `Pawl.Combat`, which would cycle via `Sba`); `Damage` imports `Event` (acyclic). If the build reports an import cycle, re-read the Global Constraints note — do not "fix" it by importing `Pawl.Combat` into `Event`.
- **Type-name consistency:** `DamageKind` (`Combat`/`Noncombat`); `Prevention.PreventAllCombatDamage`; `ActivePrevention.MkActivePrevention {prevention, duration}`; `GameState.preventions`/`regenerationShields`; `Effect.Prevent`/`RegenerateSelf`; `Event.applyPreventions`/`dropEndOfTurnPreventions`/`clearRegenerationShields`/`destroy`/`regenerate`/`removeFromCombat`; `Sba.zeroToughness`/`destroyedBySba`. These names are used identically across tasks.
- **Synthetic crutch:** only the toughness-≤-0 case (Task 6b) uses a synthetic continuous effect (the existing local `withEffect` + `ModifyPowerToughness 0 (-1)`), and only because no real −N/−N ability exists yet (the M4b-documented pattern, already used by the neighbouring 704.5f Myr test); everything else is a real, Scryfall-verified card. Its expiry is the first real −N/−N ability.
- **If a card-file round-trip fails on field order:** the loader parses by key, so order does not affect parsing; if a render-stability check exists and fails, regenerate the file via `cabal repl` — `Data.Text.IO.writeFile "data/cards/<slug>.json" (Pawl.Json.render (Pawl.Codec.printingToJson <the loaded printing>))` — then commit the rendered bytes.
