# M3f The Event Pipeline (Replacements + Triggers) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `changeZone` a first-class event so Rest in Peace works whole: its replacement redirects every graveyard-bound object to exile (CR 614), and its enters-the-battlefield trigger goes on the stack and exiles all graveyards (CR 603), both riding one zone-change substrate.

**Architecture:** `changeZone` moves out of `Pawl.Game` into a new `Pawl.Event` module (above `Pawl.Projection`, so replacements can be read through the projection without an import cycle). `Pawl.Event` is the sole home of casing on the two new open-half vocabularies — `ReplacementEffect` (a zone-change pattern → rewrite) and `TriggerCondition` (an event pattern). `changeZone` applies replacements to the proposed move, performs it, and emits the resolved `ZoneChange` into `GameState.zoneChanges`. At every CR 117.5 priority boundary the engine runs state-based actions to fixpoint, then puts matched triggered abilities on the stack as a new `Source.OfTrigger` incarnation that resolves through a `resolveEffects` executor shared with M3e's activated abilities, then ceases (CR 608.2n). Replacements and triggered abilities are read through `Projection` accessors (`replacementsOf` / `triggeredAbilitiesOf`) that mirror `abilitiesOf`, so `LoseAllAbilities` strips them uniformly.

**Tech Stack:** Haskell 2010 (GHC 9.14.1), `tasty` (`tasty-hunit` + `tasty-quickcheck`), Cabal. Boot libraries only.

## Global Constraints

Copied from the spec and `CLAUDE.md`; every task implicitly includes these:

- **Haskell 2010, no language extensions** beyond `GADTs`, `RankNTypes`, `NamedFieldPuns`. No `LambdaCase`, no `OverloadedStrings`, **no list comprehensions**.
- **No explicit export lists** (`module Pawl.Foo where`).
- **One type per module** under `Pawl.Type.<TypeName>` (type + instances only); logic in other `Pawl.*` modules. A module never imports its parents. Add a new `Pawl.Type.*` file and run `cabal-gild` (via `hooky fix`) — the `exposed-modules` field is `discover`-generated. A new `Pawl.*Spec` goes in the test-suite `other-modules` list.
- **Qualified imports, aliased to the last component** (`Data.List` → `List`); operators unqualified; one import group. `A.B.C` must not import `A.B` or `A`.
- **No partial functions** — `Maybe`/`Either`, never `head`/`error`/non-exhaustive matches.
- **`newtype`/record + `Mk`-prefixed, non-punning constructors**; build records with `do`/record syntax. Sum-type data constructors (like `SelfEnters`, `OfTrigger`, `ExileAllGraveyards`, `RedirectZoneChange`) take no `Mk` prefix.
- **Prefer explicit:** `case` over point-free; `let` over `where`; `$` over parens, `.` over chained `$`; `Text` not `String`; arbitrary-precision numbers.
- **No boolean blindness**; **derive at least `Eq` and `Show`** (and `Ord` on anything a `Card`/`Action`/`Source` transitively contains — those derive `Ord`; `ProjectedCharacteristics` has **no** `Ord` and is never a key).
- **Warning-clean** under `-Weverything` minus the allow-list; the `pedantic` flag makes any warning a build failure (including `-Wincomplete-patterns` on a match missing a new constructor, and `-Wmissing-fields` on a record missing a new field). Build `all`: `cabal build all --enable-tests --enable-benchmarks`. When in doubt, `cabal clean` first — incremental builds hide warnings.
- **The two invariants:** the rules core never `case`s on an effect's *identity*. `Pawl.Resolve` is the **sole** `case`-on-`Effect` home (`slotsOf`, `manaProduced`, `rewriteEffect`, `matchesCriterion`, `applyEffect`); `Pawl.Projection` the **sole** `case`-on-`Modification` home; **`Pawl.Event` the sole `case`-on-`ReplacementEffect` and `case`-on-`TriggerCondition` home** (new this milestone). Constructing a value is not casing on it. `Stack.resolveTop` dispatches on the `Source` classification (`OfCard`/`OfAbility`/`OfTrigger`), never a card's identity. The engine makes no player choice except where the rules leave one, eliding only indistinguishable options (with a documented expiry).
- **Every rules claim cited** against `docs/rules.txt` in a code comment. Never trust recalled Magic rules. (Numbers verified for this plan: replacement effects use "instead" CR 614.1a; a replacement gets one opportunity CR 614.5; a replaced event never happens, the modified event may trigger CR 614.6; a replaced event triggers nothing CR 603.2g; a triggered ability goes on the stack the next time a player would get priority CR 603.3 / 117.5; enters-the-battlefield triggers CR 603.6a; APNAP CR 603.3b; an ability ceases to exist CR 608.2n; SBA burial for lethal damage CR 704.5g / zero toughness 704.5f; new object on zone change CR 400.7; a token in a non-battlefield zone ceases CR 111.7 — deferred.)
- **After each task:** `cabal build all --enable-tests --enable-benchmarks` warning-free, `cabal test`, `git add -A` (stage explicit paths under a shared worktree) then `hooky fix` && `git add -A` && `hooky run`, HLint clean. Commit directly to `main`, one small complete commit per task.

**Commit message footer** (every commit):

```
Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
```

**Cards (Scryfall-verified 2026-07-18):**
- **Rest in Peace** — `{1}{W}` Enchantment — "When this enchantment enters, exile all graveyards. / If a card or token would be put into a graveyard from anywhere, exile it instead."
- **Plains** — basic land, no mana cost; its `{W}` mana ability is intrinsic from the `Plains` subtype (CR 305.6), zero opcodes. Added as a white mana source so Rest in Peace is cast, not hand-placed. (`Pawl.Mana` already maps `Subtype.Plains` → white.)

**Module dependency note (why Task 1 exists):** `Pawl.Projection` imports `Pawl.Game`. Replacements are consulted inside `changeZone`, which lives in `Pawl.Game` — below `Projection`. Reading the projection there would be a `Game → Projection → Game` cycle. Task 1 relocates `changeZone` into a new `Pawl.Event` module that sits *above* `Projection`, which every current caller then imports. This is a behavior-preserving refactor done first, in isolation.

---

## Phase 0 — relocate the funnel (Task 1)

### Task 1: Move `changeZone` from `Pawl.Game` to a new `Pawl.Event` (no behavior change)

**Files:**
- Create: `source/library/Pawl/Event.hs`
- Modify: `source/library/Pawl/Game.hs` (delete `changeZone`)
- Modify: every caller — `source/library/Pawl/Stack.hs`, `Setup.hs`, `Engine.hs`, `Activate.hs`, `Cast.hs`, `Resolve.hs`, `Sba.hs`; and tests `source/test-suite/Pawl/DamageSpec.hs`, `GameSpec.hs`, `CastSpec.hs`, `ResolveSpec.hs` (change `Game.changeZone` → `Event.changeZone`, add `import qualified Pawl.Event as Event`)
- Test: the existing suite is the test — behavior must not change.

**Interfaces:**
- Produces: `Event.changeZone :: ObjectId -> Zone -> GameState -> GameState` (byte-for-byte the old `Game.changeZone`). `Game.changeZone` no longer exists.

- [x] **Step 1: Note the regression baseline**

Run: `cabal test` and record the passing count. This task must end at the same count — it moves a function, nothing else.

- [x] **Step 2: Create `Pawl.Event` with `changeZone` moved verbatim**

Create `source/library/Pawl/Event.hs`. Copy the exact `changeZone` body from `Game.hs` (lines 73–86 at HEAD), keeping every field reset. It calls `Game.lookupObject`, `Game.freshObjectId`, `Game.freshTimestamp`, `Game.removeFromZones`, `Game.insertIntoZone` (all stay in `Game`):

```haskell
-- The event pipeline (CR 603/614). This module owns the single zone-change
-- funnel and, later, the sole casing on ReplacementEffect and TriggerCondition.
-- changeZone lives here (not in Pawl.Game) so it can read the projection --
-- Projection imports Game, so a Game.changeZone that read the projection would
-- be an import cycle. See the plan's module dependency note.
module Pawl.Event where

import qualified Data.Map.Strict as Map
import qualified Pawl.Game as Game
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.TapState as TapState
import Pawl.Type.Zone (Zone)

-- The single zone-change primitive (CR 400.7): the source object ceases; a NEW
-- object with a fresh id is created in the destination, carrying owner and
-- source forward and resetting per-incarnation state. No-op if the id is unknown.
changeZone :: ObjectId -> Zone -> GameState -> GameState
changeZone oid dest gs = case Game.lookupObject oid gs of
  Nothing -> gs
  Just obj ->
    let pid = Object.owner obj
        (newId, gs1) = Game.freshObjectId gs
        (ts, gs1b) = Game.freshTimestamp gs1
        newObj = obj {Object.zone = dest, Object.tapped = TapState.Untapped, Object.damage = 0, Object.sickness = Sickness.Sick, Object.targets = Map.empty, Object.chosenSubtypes = Map.empty, Object.timestamp = ts}
        gs2 = Game.removeFromZones pid oid gs1b
        gs3 = gs2 {GameState.objects = Map.insert newId newObj (Map.delete oid (GameState.objects gs2))}
     in Game.insertIntoZone dest pid newId gs3
```

- [x] **Step 3: Delete `changeZone` from `Pawl.Game`**

Remove the `changeZone` definition (and its doc comment) from `source/library/Pawl/Game.hs`. Leave `lookupObject`, `freshObjectId`, `freshTimestamp`, `removeFromZones`, `insertIntoZone`, `cardOf`, `controllerOf`, `zoneMembers`, `objectCount` in place. Drop now-unused imports if the build flags them (e.g. `Sickness`, `TapState`, `Zone` may become unused in `Game.hs` — remove any the compiler warns on).

- [x] **Step 4: Retarget every caller**

The build lists each unresolved `Game.changeZone`. At each, add `import qualified Pawl.Event as Event` and change `Game.changeZone` → `Event.changeZone`. Known library sites: `Stack.hs` (permanent resolution), `Setup.hs` (opening draws), `Engine.hs` (`drawFor`, land play, `discardToHandSize`), `Activate.hs` (`payAdditional` sacrifice), `Cast.hs`, `Resolve.hs` (`resolveSpell` bury, `putTapped`), `Sba.hs` (`bury`). Known test sites: `DamageSpec.hs`, `GameSpec.hs`, `CastSpec.hs`, `ResolveSpec.hs`.

- [x] **Step 5: Run the suite to verify no behavior changed**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — the same count as Step 1. A cycle error means a caller that `Pawl.Event` itself needs; there is none in M3f (Event imports only `Game` here).

- [x] **Step 6: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Relocate changeZone from Pawl.Game to Pawl.Event (no behavior change)"
```

---

## Phase 1 — replacement effects at the funnel (Tasks 2–4)

### Task 2: `ZoneChange` + `ReplacementEffect` types + the pure `applyReplacements` classifier

**Files:**
- Create: `source/library/Pawl/Type/ZoneChange.hs`
- Create: `source/library/Pawl/Type/ReplacementEffect.hs`
- Modify: `source/library/Pawl/Event.hs` (`applyReplacements`)
- Test: `source/test-suite/Pawl/EventSpec.hs` (new — wire into `Main.hs` and the test-suite `other-modules`)

**Interfaces:**
- Produces: `ZoneChange.MkZoneChange { object :: ObjectId, from :: Zone, to :: Zone }`; `ReplacementEffect.RedirectZoneChange { whenDestination :: Zone, toDestination :: Zone }`; `Event.applyReplacements :: [ReplacementEffect] -> ZoneChange -> ZoneChange` (each replacement gets one opportunity, CR 614.5; a redirect that fires produces a destination it no longer matches, so the fold terminates).

- [x] **Step 1: Write the failing test**

Create `source/test-suite/Pawl/EventSpec.hs`:

```haskell
module Pawl.EventSpec where

import qualified Pawl.Event as Event
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.ZoneChange as ZoneChange
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Pawl.Event"
    [ HU.testCase "CR 614.1a a graveyard-bound move is redirected to exile" $
        let rip = ReplacementEffect.RedirectZoneChange {ReplacementEffect.whenDestination = Zone.Graveyard, ReplacementEffect.toDestination = Zone.Exile}
            proposed = ZoneChange.MkZoneChange {ZoneChange.object = ObjectId.MkObjectId 5, ZoneChange.from = Zone.Battlefield, ZoneChange.to = Zone.Graveyard}
         in HU.assertEqual "redirected to exile" Zone.Exile (ZoneChange.to (Event.applyReplacements [rip] proposed)),
      HU.testCase "CR 614.5 the redirect does not re-apply (exile is not graveyard)" $
        let rip = ReplacementEffect.RedirectZoneChange {ReplacementEffect.whenDestination = Zone.Graveyard, ReplacementEffect.toDestination = Zone.Exile}
            proposed = ZoneChange.MkZoneChange {ZoneChange.object = ObjectId.MkObjectId 5, ZoneChange.from = Zone.Battlefield, ZoneChange.to = Zone.Graveyard}
         in HU.assertEqual "exile, applied once" Zone.Exile (ZoneChange.to (Event.applyReplacements [rip, rip] proposed)),
      HU.testCase "a non-graveyard move is untouched" $
        let rip = ReplacementEffect.RedirectZoneChange {ReplacementEffect.whenDestination = Zone.Graveyard, ReplacementEffect.toDestination = Zone.Exile}
            proposed = ZoneChange.MkZoneChange {ZoneChange.object = ObjectId.MkObjectId 5, ZoneChange.from = Zone.Stack, ZoneChange.to = Zone.Battlefield}
         in HU.assertEqual "battlefield unchanged" Zone.Battlefield (ZoneChange.to (Event.applyReplacements [rip] proposed))
    ]
```

Wire `Pawl.EventSpec.tests` into `source/test-suite/Main.hs`'s `testTree` (add the import and list entry) and add `Pawl.EventSpec` to the test-suite `other-modules` list in `pawl.cabal`.

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "redirected to exile"'`
Expected: FAIL to compile — `Pawl.Event.applyReplacements`, `ZoneChange`, `ReplacementEffect` not in scope.

- [x] **Step 3: Create the two types**

`source/library/Pawl/Type/ZoneChange.hs`:

```haskell
module Pawl.Type.ZoneChange where

import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Zone (Zone)

-- One zone-change event (CR 400.7): `object` is the RESULTING object's id (the
-- fresh incarnation in the destination), which is what an enters trigger scans.
-- `from` is carried for the future leaves-the-battlefield pass (M3f reads `to`).
data ZoneChange = MkZoneChange
  { object :: ObjectId,
    from :: Zone,
    to :: Zone
  }
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/ReplacementEffect.hs`:

```haskell
module Pawl.Type.ReplacementEffect where

import Pawl.Type.Zone (Zone)

-- CR 614.1a: a replacement effect. Classified by the event pattern it intercepts:
-- a zone change whose destination is `whenDestination` heads for `toDestination`
-- instead. Rest in Peace = RedirectZoneChange Graveyard Exile (any object, from
-- any source zone). Its own leaf family, distinct from Effect (one-shot) and
-- Modification (continuous). Only Pawl.Event may case on it.
data ReplacementEffect = RedirectZoneChange
  { whenDestination :: Zone,
    toDestination :: Zone
  }
  deriving (Eq, Ord, Show)
```

- [x] **Step 4: Add `applyReplacements` to `Pawl.Event`**

In `source/library/Pawl/Event.hs`, add imports `import qualified Data.List as List`, `import Pawl.Type.ReplacementEffect (ReplacementEffect)`, `import qualified Pawl.Type.ReplacementEffect as ReplacementEffect`, `import Pawl.Type.ZoneChange (ZoneChange)`, `import qualified Pawl.Type.ZoneChange as ZoneChange`. Add:

```haskell
-- CR 614: rewrite the proposed zone change by each active replacement. CR 614.5:
-- a replacement gets ONE opportunity -- applied left-to-right, each sees the
-- running event; RedirectZoneChange's output destination no longer matches its
-- own `whenDestination`, so it cannot re-fire. This module is the sole home of
-- casing on ReplacementEffect.
applyReplacements :: [ReplacementEffect] -> ZoneChange -> ZoneChange
applyReplacements res zc = List.foldl' applyOne zc res

applyOne :: ZoneChange -> ReplacementEffect -> ZoneChange
applyOne zc re = case re of
  ReplacementEffect.RedirectZoneChange whenDest toDest ->
    if ZoneChange.to zc == whenDest
      then zc {ZoneChange.to = toDest}
      else zc
```

- [x] **Step 5: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS.

- [x] **Step 6: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Add ZoneChange + ReplacementEffect + the pure applyReplacements classifier (CR 614)"
```

---

### Task 3: Grow the fields + project replacements/triggers + Rest in Peace and Plains

**Files:**
- Modify: `source/library/Pawl/Type/Card.hs` (add `replacementEffects`, `triggeredAbilities`)
- Modify: `source/library/Pawl/Type/ProjectedCharacteristics.hs` (add `replacementEffects`, `triggeredAbilities`)
- Modify: `source/library/Pawl/Type/GameState.hs` (add `zoneChanges`)
- Modify: `source/library/Pawl/Projection.hs` (seed + strip the two PC fields; `replacementsOf`, `replacementsAffecting`, `triggeredAbilitiesOf`)
- Modify: `source/library/Pawl/Setup.hs` and `source/test-suite/Pawl/Support.hs` (seed `zoneChanges = []` at each full `MkGameState`)
- Modify: `source/library/Pawl/Card.hs` (seed `replacementEffects = []`, `triggeredAbilities = []` at every printing; add Rest in Peace + Plains; register in `allPrintings`)
- Modify: any other hand-built `Card.MkCard` (build lists them via `-Wmissing-fields`)
- Test: `source/test-suite/Pawl/ProjectionSpec.hs`

**Interfaces:**
- Consumes: `ReplacementEffect` (Task 2). This task adds **only** the `replacementEffects` field to `Card`/`PC` — the `triggeredAbilities` field waits for Task 5, where `TriggeredAbility` is created (types live in the task that first uses them, per TDD-locality). The cost is that Task 5 re-touches each `MkCard` to seed the second field; both passes are mechanical, driven by `-Wmissing-fields`.
- Produces: `Card.replacementEffects :: [ReplacementEffect]`; `PC.replacementEffects :: [ReplacementEffect]`; `GameState.zoneChanges :: [ZoneChange]`; `Projection.replacementsOf :: ObjectId -> GameState -> [ReplacementEffect]`; `Projection.replacementsAffecting :: GameState -> [ReplacementEffect]`; `Card.restInPeacePrinting`, `Card.plainsPrinting`. Rest in Peace's `replacementEffects = [RedirectZoneChange Graveyard Exile]`.

- [x] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/ProjectionSpec.hs` (add imports `qualified Pawl.Type.ReplacementEffect as ReplacementEffect`, `qualified Pawl.Type.Zone as Zone`, `qualified Pawl.Support as S` if absent — mirror existing style):

```haskell
      HU.testCase "CR 614: Rest in Peace projects its graveyard->exile replacement" $
        let (rip, gs) = S.addCreature Card.restInPeacePrinting S.alice (Setup.emptyGame S.bothPlayers)
         in HU.assertEqual
              "one redirect replacement"
              [ReplacementEffect.RedirectZoneChange Zone.Graveyard Zone.Exile]
              (Projection.replacementsOf rip gs),
      HU.testCase "a vanilla creature projects no replacements" $
        let (piker, gs) = S.addPiker S.alice (Setup.emptyGame S.bothPlayers)
         in HU.assertEqual "none" [] (Projection.replacementsOf piker gs),
```

(`S.addCreature` puts a permanent on the battlefield settled; Rest in Peace is an enchantment, but `addCreature` is a generic "put this printing on the battlefield" helper despite the name — see `Support.hs`.)

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "projects its graveyard"'`
Expected: FAIL to compile — `Projection.replacementsOf`, `Card.restInPeacePrinting`, `Card.Type.replacementEffects` not in scope.

- [x] **Step 3: Add the `replacementEffects` field to `Card`**

In `source/library/Pawl/Type/Card.hs`, add `import Pawl.Type.ReplacementEffect (ReplacementEffect)` and the field (after `activatedAbilities`, before `targetSpecs`):

```haskell
    -- CR 614: this card's replacement effects, active while it is on the
    -- battlefield. Read through Pawl.Projection.replacementsOf (never directly)
    -- so layer 6 LoseAllAbilities strips them uniformly. Empty for all but Rest
    -- in Peace.
    replacementEffects :: [ReplacementEffect],
```

- [x] **Step 4: Add the `replacementEffects` field to `ProjectedCharacteristics`**

In `source/library/Pawl/Type/ProjectedCharacteristics.hs`, add `import Pawl.Type.ReplacementEffect (ReplacementEffect)` and the field (after `activatedAbilities`):

```haskell
    -- CR 614 layer 6: the object's replacement effects after the layer system,
    -- the same projection posture as activatedAbilities. Emptied by LoseAllAbilities.
    replacementEffects :: [ReplacementEffect],
```

- [x] **Step 5: Add the `zoneChanges` field to `GameState`**

In `source/library/Pawl/Type/GameState.hs`, add `import Pawl.Type.ZoneChange (ZoneChange)` and the field (after `damageEvents`):

```haskell
    -- CR 603 / 117.5: zone-change events emitted since triggers were last placed.
    -- changeZone appends the RESOLVED (post-replacement) event; the 117.5 boundary
    -- scans and drains it. The zone-change analog of damageEvents.
    zoneChanges :: [ZoneChange],
```

- [x] **Step 6: Seed the PC field (base) and strip it (LoseAllAbilities)**

In `source/library/Pawl/Projection.hs`, add `import Pawl.Type.ReplacementEffect (ReplacementEffect)`. In `baseCharacteristics`, the `Nothing` branch sets `PC.replacementEffects = []` and the `Just card` branch sets `PC.replacementEffects = Card.Type.replacementEffects card` (mirror the two `activatedAbilities` lines). In `applyModification`'s `LoseAllAbilities` arm, add `PC.replacementEffects = []`:

```haskell
  Modification.LoseAllAbilities ->
    pc {PC.keywords = Set.empty, PC.activatedAbilities = [], PC.replacementEffects = []}
```

- [x] **Step 7: Add the projection accessors**

In `source/library/Pawl/Projection.hs`, add (near `abilitiesOf`):

```haskell
-- CR 614 / 613 layer 6: an object's replacement effects after the layer system,
-- the same projection posture as abilitiesOf. A Humility'd creature has none.
replacementsOf :: ObjectId -> GameState -> [ReplacementEffect]
replacementsOf oid gs = PC.replacementEffects (project oid gs)

-- CR 614.6: every replacement effect active on the battlefield. Short-circuits
-- when no permanent has one in its base card, so an ordinary zone change (a draw,
-- a land entering) does NOT project the whole board -- projection runs only once
-- a replacement source is actually present.
replacementsAffecting :: GameState -> [ReplacementEffect]
replacementsAffecting gs =
  let onBattlefield = Set.toList (GameState.battlefield gs)
      baseHas oid = case Game.cardOf oid gs of
        Nothing -> False
        Just card -> not (null (Card.Type.replacementEffects card))
   in if not (any baseHas onBattlefield)
        then []
        else concatMap (\oid -> replacementsOf oid gs) onBattlefield
```

(`Card.Type` is already imported in `Projection` as `Card.Type`; confirm the alias and reuse it.)

- [x] **Step 8: Seed `zoneChanges = []` at every full `MkGameState`**

`-Wmissing-fields` lists them. Known sites: `source/library/Pawl/Setup.hs` (`emptyGame`), `source/test-suite/Pawl/Support.hs` (`oneMountainState`). Add `GameState.zoneChanges = []` next to `GameState.damageEvents = []` at each.

- [x] **Step 9: Seed `replacementEffects = []` at every `MkCard`, then add Rest in Peace and Plains**

`-Wmissing-fields` lists every printing. Add `Card.replacementEffects = []` (next to `activatedAbilities`) at each existing printing in `source/library/Pawl/Card.hs` and any hand-built `Card.Type.MkCard` in tests (e.g. `ResolveSpec.hs`). Then add the two printings (imports for `ReplacementEffect`, `Zone`, and `Subtype.Plains` as needed):

```haskell
-- Rest in Peace: {1}{W}, Enchantment, "When this enchantment enters, exile all
-- graveyards. / If a card or token would be put into a graveyard from anywhere,
-- exile it instead." Scryfall-verified 2026-07-18. The ETB trigger is added in
-- Task 5 (triggeredAbilities); here only the replacement.
restInPeacePrinting :: Printing.Printing
restInPeacePrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Rest in Peace",
            Card.manaCost =
              Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.White)]),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Enchantment,
                  TypeLine.subtypes = Set.empty
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty,
            Card.staticAbilities = [],
            Card.effects = [],
            Card.activatedAbilities = [],
            Card.replacementEffects = [ReplacementEffect.RedirectZoneChange Zone.Graveyard Zone.Exile],
            Card.targetSpecs = Map.empty
          }
    }

-- Plains: basic land, no mana cost; {W} is intrinsic from the Plains subtype
-- (CR 305.6). Added so Rest in Peace can be cast with white mana.
plainsPrinting :: Printing.Printing
plainsPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Plains",
            Card.manaCost = Nothing,
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.singleton Supertype.Basic,
                  TypeLine.types = Set.singleton CardType.Land,
                  TypeLine.subtypes = Set.singleton Subtype.Plains
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty,
            Card.staticAbilities = [],
            Card.effects = [],
            Card.activatedAbilities = [],
            Card.replacementEffects = [],
            Card.targetSpecs = Map.empty
          }
    }
```

(Match the exact import aliases the file already uses — `Supertype`, `Subtype`, `CardType`, `ManaCost`, `ManaSymbol`, `ManaType`, `Color`, `TypeLine`, `Set`, `Text`, `Map`. The `Island` printing from M3d is the template for a basic land; copy its structure and swap the subtype.) Register both in `allPrintings`:

```haskell
    prodigalSorcererPrinting,
    llanowarElvesPrinting,
    evolvingWildsPrinting,
    restInPeacePrinting,
    plainsPrinting
  ]
```

(Use the actual tail of the current `allPrintings` list; append the two new names.)

- [x] **Step 10: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS. Fix any `-Wmissing-fields` site the build names.

- [x] **Step 11: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Project replacement effects; add zoneChanges; add Rest in Peace + Plains (CR 614)"
```

---

### Task 4: Wire replacements + emission into `changeZone` — Rest in Peace redirects to exile

**Files:**
- Modify: `source/library/Pawl/Event.hs` (`changeZone` applies replacements + emits)
- Test: `source/test-suite/Pawl/EventSpec.hs`

**Interfaces:**
- Consumes: `applyReplacements` (Task 2), `Projection.replacementsAffecting` (Task 3), `GameState.zoneChanges` (Task 3).
- Produces: `Event.changeZone` now (a) redirects a graveyard-bound object to exile whenever a Rest in Peace is on the battlefield, and (b) appends the resolved `ZoneChange` (carrying the post-move id and the resolved destination) to `GameState.zoneChanges`.

- [x] **Step 1: Write the failing tests**

Add to `source/test-suite/Pawl/EventSpec.hs` (add imports `qualified Pawl.Card as Card`, `qualified Pawl.Setup as Setup`, `qualified Pawl.Support as S`, `qualified Pawl.Game as Game`, `qualified Pawl.Type.GameState as GameState`, `qualified Data.Set as Set`, `qualified Pawl.Type.Object as Object`):

```haskell
      HU.testCase "CR 614: with Rest in Peace out, a creature sent to the graveyard is exiled" $
        let (_, g0) = S.addCreature Card.restInPeacePrinting S.alice (Setup.emptyGame S.bothPlayers)
            (piker, g1) = S.addPiker S.bob g0
            after = Event.changeZone piker Zone.Graveyard g1
            inExile = Set.size (GameState.exile after)
            gyCount = length (Game.zoneMembers Zone.Graveyard S.bob after)
         in do
              HU.assertEqual "exiled, not in graveyard" 0 gyCount
              HU.assertEqual "one object in exile" 1 inExile,
      HU.testCase "CR 603.2g: the emitted event records the RESOLVED destination (exile)" $
        let (_, g0) = S.addCreature Card.restInPeacePrinting S.alice (Setup.emptyGame S.bothPlayers)
            (piker, g1) = S.addPiker S.bob g0
            after = Event.changeZone piker Zone.Graveyard g1
         in case GameState.zoneChanges after of
              zc : _ -> HU.assertEqual "event says exile" Zone.Exile (ZoneChange.to zc)
              [] -> HU.assertFailure "expected an emitted zone change",
      HU.testCase "without Rest in Peace, a creature goes to the graveyard" $
        let (piker, g1) = S.addPiker S.bob (Setup.emptyGame S.bothPlayers)
            after = Event.changeZone piker Zone.Graveyard g1
         in HU.assertEqual "in graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after))
```

- [x] **Step 2: Run tests to verify they fail**

Run: `cabal test --test-options='-p "sent to the graveyard is exiled"'`
Expected: FAIL — the redirect is not wired, so the Piker lands in the graveyard.

- [x] **Step 3: Wire replacements + emission into `changeZone`**

In `source/library/Pawl/Event.hs`, add `import qualified Pawl.Projection as Projection` (and `import qualified Pawl.Type.ZoneChange as ZoneChange` / `import Pawl.Type.ZoneChange (ZoneChange)` if Task 2 did not already add them). The `Zone` type is already imported from Task 1; no `Zone` constructor is referenced here. Replace `changeZone`:

```haskell
changeZone :: ObjectId -> Zone -> GameState -> GameState
changeZone oid requestedDest gs = case Game.lookupObject oid gs of
  Nothing -> gs
  Just obj ->
    let pid = Object.owner obj
        fromZone = Object.zone obj
        -- CR 614.4: replacements exist before the event; read them from the
        -- pre-move state. CR 614.6: the modified event is what actually happens.
        proposed = ZoneChange.MkZoneChange oid fromZone requestedDest
        resolved = applyReplacements (Projection.replacementsAffecting gs) proposed
        dest = ZoneChange.to resolved
        (newId, gs1) = Game.freshObjectId gs
        (ts, gs1b) = Game.freshTimestamp gs1
        newObj = obj {Object.zone = dest, Object.tapped = TapState.Untapped, Object.damage = 0, Object.sickness = Sickness.Sick, Object.targets = Map.empty, Object.chosenSubtypes = Map.empty, Object.timestamp = ts}
        gs2 = Game.removeFromZones pid oid gs1b
        gs3 = gs2 {GameState.objects = Map.insert newId newObj (Map.delete oid (GameState.objects gs2))}
        moved = Game.insertIntoZone dest pid newId gs3
        -- CR 603.2g: emit the RESOLVED event (post-replacement), carrying the new
        -- object's id -- what an enters trigger scans.
        emitted = ZoneChange.MkZoneChange newId fromZone dest
     in moved {GameState.zoneChanges = GameState.zoneChanges moved ++ [emitted]}
```

- [x] **Step 4: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — the Piker is exiled with Rest in Peace out, the event records `Exile`, and without RiP it goes to the graveyard. The whole existing suite stays green (no other card has a replacement, so `replacementsAffecting` short-circuits to `[]` and behavior is unchanged).

- [x] **Step 5: Verify the falsifiers the gate names (stack spell + discard)**

These already pass through the same funnel; add two more assertions to confirm the funnel-generality (a resolving spell and a discard are not creature deaths):

```haskell
      HU.testCase "CR 608.2n: a resolving spell is exiled from the stack under Rest in Peace" $
        let (_, g0) = S.addCreature Card.restInPeacePrinting S.alice (Setup.emptyGame S.bothPlayers)
            (bolt, g1) = S.addLibraryCard Card.lightningBoltPrinting S.bob g0
            onStack = g1 {GameState.stack = bolt : GameState.stack g1, GameState.objects = Map.adjust (\o -> o {Object.zone = Zone.Stack}) bolt (GameState.objects g1)}
            after = Event.changeZone bolt Zone.Graveyard onStack
         in HU.assertEqual "spell exiled, graveyard empty" 0 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
```

Run: `cabal test --test-options='-p "resolving spell is exiled"'`
Expected: PASS. (This exercises a stack→graveyard move — a case a battlefield-only "when a creature dies" hook would miss.)

- [x] **Step 6: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "changeZone applies replacements + emits the resolved event: Rest in Peace exiles (CR 614)"
```

---

## Phase 2 — triggered abilities (Tasks 5–7)

### Task 5: Trigger types + `ExileAllGraveyards` + `OfTrigger` + shared `resolveEffects`

**Files:**
- Create: `source/library/Pawl/Type/TriggerCondition.hs`
- Create: `source/library/Pawl/Type/TriggeredAbility.hs`
- Modify: `source/library/Pawl/Type/Effect.hs` (add `ExileAllGraveyards`)
- Modify: `source/library/Pawl/Type/Source.hs` (add `OfTrigger`)
- Modify: `source/library/Pawl/Type/Card.hs` (add `triggeredAbilities`) and `source/library/Pawl/Type/ProjectedCharacteristics.hs` (add `triggeredAbilities`)
- Modify: `source/library/Pawl/Projection.hs` (seed + strip `triggeredAbilities`; `triggeredAbilitiesOf`)
- Modify: `source/library/Pawl/Resolve.hs` (`slotsOf`/`manaProduced`/`rewriteEffect`/`applyEffect` arms for `ExileAllGraveyards`; refactor `resolveAbility` → `resolveEffects`)
- Modify: `source/library/Pawl/Stack.hs` (`resolveTop` `OfTrigger` arm)
- Modify: `source/library/Pawl/Game.hs` (`cardOf` `OfTrigger` arm) and every other exhaustive `case Object.source` (build lists them: `Action.hs`, `Support.hs`)
- Modify: `source/library/Pawl/Card.hs` (seed `triggeredAbilities = []` at each printing; give Rest in Peace its ETB)
- Test: `source/test-suite/Pawl/ResolveSpec.hs`

**Interfaces:**
- Consumes: `resolveAbility`/`OfAbility` (M3e), `Event.changeZone` (Task 1), `applyEffect` (M3e, monadic).
- Produces: `TriggerCondition.SelfEnters`; `TriggeredAbility.MkTriggeredAbility { condition :: TriggerCondition, effects :: [Effect], targetSpecs :: Map SlotName TargetSpec }`; `Effect.ExileAllGraveyards`; `Source.OfTrigger :: ObjectId -> TriggeredAbility -> Source`; `Card.triggeredAbilities :: [TriggeredAbility]`; `PC.triggeredAbilities`; `Projection.triggeredAbilitiesOf :: ObjectId -> GameState -> [TriggeredAbility]`; `Resolve.resolveEffects :: ObjectId -> ObjectId -> [Effect] -> Map SlotName TargetSpec -> Game ()` (stack-object id, source id, effects, specs — re-validate CR 608.2b, fold `applyEffect` with the source, then cease CR 608.2n). Rest in Peace's `triggeredAbilities = [MkTriggeredAbility SelfEnters [ExileAllGraveyards] Map.empty]`.

- [x] **Step 1: Write the failing test**

Add to the `Resolve` group in `source/test-suite/Pawl/ResolveSpec.hs`. Hand-build an `OfTrigger` object (Rest in Peace's ETB) on the stack with a card already in a graveyard, resolve, and assert the graveyard is exiled and the trigger object ceased (add imports as the compiler flags — `Source`, `TriggeredAbility`, `TriggerCondition`, `Effect`, `Stack`, `Engine`, `Object`, `Zone`, `TapState`, `Sickness`, `Timestamp`):

```haskell
      HU.testCase "CR 603/608.2n Rest in Peace's ETB exiles graveyards and ceases" $
        let g0 = Setup.emptyGame S.bothPlayers
            (ripId, g1) = S.addCreature Card.restInPeacePrinting S.alice g0
            (deadId, g2) = S.addLibraryCard Card.pikerPrinting S.bob g1
            -- move the Piker into bob's graveyard
            g3 = Event.changeZone deadId Zone.Graveyard g2
            ability = TriggeredAbility.MkTriggeredAbility TriggerCondition.SelfEnters [Effect.ExileAllGraveyards] Map.empty
            (abilId, g4) = Game.freshObjectId g3
            (ts, g5) = Game.freshTimestamp g4
            abilObj =
              Object.MkObject
                { Object.owner = S.alice,
                  Object.source = Source.OfTrigger ripId ability,
                  Object.zone = Zone.Stack,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled,
                  Object.targets = Map.empty,
                  Object.chosenSubtypes = Map.empty,
                  Object.timestamp = ts
                }
            g6 = g5 {GameState.objects = Map.insert abilId abilObj (GameState.objects g5), GameState.stack = abilId : GameState.stack g5}
            resolved = snd (Engine.runGamePure S.identityAnswer g6 Stack.resolveTop)
         in do
              HU.assertEqual "bob's graveyard is empty" 0 (length (Game.zoneMembers Zone.Graveyard S.bob resolved))
              HU.assertEqual "ability ceased" Nothing (Game.lookupObject abilId resolved)
    ,
```

(The Piker in the graveyard is exiled by the ETB, *not* by the replacement — Rest in Peace's replacement only intercepts moves *to* the graveyard, and this card is already there. The ETB's `changeZone deadId Exile` moves it out.)

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "ETB exiles graveyards and ceases"'`
Expected: FAIL to compile — `TriggeredAbility`, `TriggerCondition`, `Effect.ExileAllGraveyards`, `Source.OfTrigger` not in scope.

- [x] **Step 3: Create the trigger types**

`source/library/Pawl/Type/TriggerCondition.hs`:

```haskell
module Pawl.Type.TriggerCondition where

-- CR 603.6a: the event pattern that fires a triggered ability. SelfEnters =
-- "when this ... enters [the battlefield]" -- fires when the object bearing the
-- ability enters. A general "whenever a [type] enters" is a future condition.
-- Only Pawl.Event may case on it.
data TriggerCondition = SelfEnters
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/TriggeredAbility.hs`:

```haskell
module Pawl.Type.TriggeredAbility where

import Data.Map.Strict (Map)
import Pawl.Type.Effect (Effect)
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.TargetSpec (TargetSpec)
import Pawl.Type.TriggerCondition (TriggerCondition)

-- CR 603.1: "[condition], [effect]". Reuses the Effect vocabulary and the
-- slot/target machinery of a spell. Differs from ActivatedAbility only in
-- carrying a trigger condition instead of a cost; on the stack the two share one
-- executor (Resolve.resolveEffects).
data TriggeredAbility = MkTriggeredAbility
  { condition :: TriggerCondition,
    effects :: [Effect],
    targetSpecs :: Map SlotName TargetSpec
  }
  deriving (Eq, Ord, Show)
```

- [x] **Step 4: Add the `ExileAllGraveyards` opcode**

In `source/library/Pawl/Type/Effect.hs`, add the constructor (extend the header comment):

```haskell
  | -- CR 701.10 / Rest in Peace: exile every card in every graveyard. Targetless
    -- and bulk (Rest in Peace's exact shape); a general exile-from-zone is future.
    ExileAllGraveyards
```

- [x] **Step 5: Add the `OfTrigger` source**

In `source/library/Pawl/Type/Source.hs`, add `import Pawl.Type.TriggeredAbility (TriggeredAbility)` and the constructor:

```haskell
  | -- CR 603.3: a triggered ability on the stack -- the source permanent's id
    -- plus the ability. Travels with the object so it resolves even if the source
    -- leaves (CR 603.3d).
    OfTrigger ObjectId TriggeredAbility
```

- [x] **Step 6: Handle `OfTrigger` in `cardOf` and other `Source` matches**

In `source/library/Pawl/Game.hs`, `cardOf`'s `case Object.source obj of` gains `Source.OfTrigger _ _ -> Nothing`. The build enumerates every other exhaustive `case Object.source` (all "not a card"): `Action.hs` `playableLands` (`Source.OfTrigger _ _ -> False`), `Support.hs` `creaturesInPlay` and `countByName` (`Source.OfTrigger _ _ -> False`).

- [x] **Step 7: Add the `Card`/`PC` `triggeredAbilities` field, seed + strip, and the accessor**

In `source/library/Pawl/Type/Card.hs`, add `import Pawl.Type.TriggeredAbility (TriggeredAbility)` and the field (after `replacementEffects`):

```haskell
    -- CR 603: this card's triggered abilities, read through
    -- Pawl.Projection.triggeredAbilitiesOf. Empty for all but Rest in Peace.
    triggeredAbilities :: [TriggeredAbility],
```

In `source/library/Pawl/Type/ProjectedCharacteristics.hs`, add `import Pawl.Type.TriggeredAbility (TriggeredAbility)` and the field. In `Projection.baseCharacteristics`, seed both branches (`Nothing -> []`, `Just card -> Card.Type.triggeredAbilities card`). In the `LoseAllAbilities` arm, add `PC.triggeredAbilities = []`. Add the accessor:

```haskell
-- CR 603 / 613 layer 6: an object's triggered abilities after the layer system,
-- the same projection posture as abilitiesOf. A Humility'd creature has none.
triggeredAbilitiesOf :: ObjectId -> GameState -> [TriggeredAbility]
triggeredAbilitiesOf oid gs = PC.triggeredAbilities (project oid gs)
```

Seed `Card.triggeredAbilities = []` at every printing and hand-built `MkCard` (build lists them).

- [x] **Step 8: Give Rest in Peace its ETB**

In `source/library/Pawl/Card.hs`, change Rest in Peace's `Card.triggeredAbilities = []` to (add imports `TriggeredAbility`, `TriggerCondition`, `Effect` if absent):

```haskell
            Card.triggeredAbilities =
              [ TriggeredAbility.MkTriggeredAbility
                  { TriggeredAbility.condition = TriggerCondition.SelfEnters,
                    TriggeredAbility.effects = [Effect.ExileAllGraveyards],
                    TriggeredAbility.targetSpecs = Map.empty
                  }
              ],
```

- [x] **Step 9: Add the `ExileAllGraveyards` executor arm + the four classifier arms**

In `source/library/Pawl/Resolve.hs`, add `import qualified Pawl.Event as Event` and `import qualified Data.List as List` (if absent). Add arms:

`slotsOf`: `Effect.ExileAllGraveyards -> Set.empty`
`manaProduced`: `Effect.ExileAllGraveyards -> Nothing`
`rewriteEffect`: `Effect.ExileAllGraveyards -> effect`
`applyEffect` (targetless, monadic — exiles every graveyard through the funnel, so a token would cease and the emit is honest):

```haskell
  -- Rest in Peace's ETB: exile every card in every graveyard (CR 400.7 each move
  -- funnels through changeZone). A graveyard->exile move matches no M3f
  -- replacement or trigger, so no cascade.
  Effect.ExileAllGraveyards -> do
    gs <- State.get
    let gyCards = concatMap (\pid -> Game.zoneMembers Zone.Graveyard pid gs) (Map.keys (GameState.players gs))
    State.modify' (\g -> List.foldl' (\g1 c -> Event.changeZone c Zone.Exile g1) g gyCards)
```

(Add `import qualified Pawl.Type.GameState as GameState` if not already qualified for `players`; `Map.keys`, `Zone`, `Game.zoneMembers` are already in scope in `Resolve`.)

- [x] **Step 10: Refactor `resolveAbility` into a shared `resolveEffects`; add the `OfTrigger` resolution arm**

In `source/library/Pawl/Resolve.hs`, add `import qualified Pawl.Type.TriggeredAbility as TriggeredAbility` and `import Pawl.Type.SlotName (SlotName)` / `import Pawl.Type.TargetSpec (TargetSpec)` (if absent). Replace `resolveAbility` with a thin wrapper over the new shared executor:

```haskell
-- CR 608.2: the executor shared by an activated ability (M3e) and a triggered
-- ability (M3f) on the stack. Re-validate filled slots (CR 608.2b), fold
-- applyEffect over the effects with `srcId` (the source permanent) as the effect
-- source (CR 608.2g), then the ability ceases (CR 608.2n). `stackId` is the
-- ability object's own id.
resolveEffects :: ObjectId -> ObjectId -> [Effect] -> Map.Map SlotName TargetSpec -> Game ()
resolveEffects stackId srcId effects specs = do
  gs <- State.get
  case Game.lookupObject stackId gs of
    Nothing -> pure ()
    Just obj ->
      let chosen = Object.targets obj
          legalSlot slot recipient = case Map.lookup slot specs of
            Nothing -> False
            Just spec -> Target.stillLegal recipient spec gs
          legality = Map.mapWithKey legalSlot chosen
          fizzles = not (Map.null specs) && not (or (Map.elems legality))
       in do
            Monad.unless fizzles $
              Monad.mapM_ (applyEffect srcId (Object.owner obj) (Object.chosenSubtypes obj) legality chosen) effects
            State.modify' (cease stackId)

resolveAbility :: ObjectId -> ObjectId -> ActivatedAbility.ActivatedAbility -> Game ()
resolveAbility abilId srcId ability =
  resolveEffects abilId srcId (ActivatedAbility.effects ability) (ActivatedAbility.targetSpecs ability)
```

(`cease` is unchanged from M3e. If `SlotName`/`TargetSpec` are already imported, do not re-import.)

In `source/library/Pawl/Stack.hs`, add `import qualified Pawl.Type.TriggeredAbility as TriggeredAbility` and the `resolveTop` arm:

```haskell
        Source.OfTrigger srcId ability ->
          Resolve.resolveEffects oid srcId (TriggeredAbility.effects ability) (TriggeredAbility.targetSpecs ability)
```

- [x] **Step 11: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — the ETB exiles bob's graveyard and the trigger object ceases; the M3e activated-ability tests stay green (they now route through `resolveEffects`).

- [x] **Step 12: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Triggered abilities on the stack: OfTrigger + ExileAllGraveyards + shared resolveEffects (CR 603/608.2n)"
```

---

### Task 6: Trigger detection — `Event.matchesTrigger` + `triggersFrom`

**Files:**
- Modify: `source/library/Pawl/Event.hs` (`matchesTrigger`, `triggersFrom`)
- Test: `source/test-suite/Pawl/EventSpec.hs`

**Interfaces:**
- Consumes: `Projection.triggeredAbilitiesOf` (Task 5), `GameState.zoneChanges` (Task 3), `ZoneChange` (Task 2).
- Produces: `Event.matchesTrigger :: TriggerCondition -> ZoneChange -> Bool` (SelfEnters ⇔ the event's destination is the battlefield); `Event.triggersFrom :: [ZoneChange] -> GameState -> [(ObjectId, PlayerId, TriggeredAbility)]` (the battlefield/enters pass: for each event with `to = Battlefield`, the newcomer's projected triggered abilities whose condition matches, each paired with the newcomer's id and its controller CR 603.3a).

- [x] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/EventSpec.hs` (add imports `qualified Pawl.Type.TriggerCondition as TriggerCondition`, `qualified Pawl.Type.PlayerId as PlayerId`):

```haskell
      HU.testCase "CR 603.6a: Rest in Peace entering yields its ETB trigger" $
        let (ripId, gs) = S.addCreature Card.restInPeacePrinting S.alice (Setup.emptyGame S.bothPlayers)
            -- an event describing RiP having entered the battlefield
            entered = ZoneChange.MkZoneChange ripId Zone.Stack Zone.Battlefield
         in case Event.triggersFrom [entered] gs of
              [(srcId, controller, _)] -> do
                HU.assertEqual "source is RiP" ripId srcId
                HU.assertEqual "controller is alice" S.alice controller
              _ -> HU.assertFailure "expected exactly one pending trigger",
      HU.testCase "a graveyard-bound event yields no enters trigger" $
        let (ripId, gs) = S.addCreature Card.restInPeacePrinting S.alice (Setup.emptyGame S.bothPlayers)
            toGrave = ZoneChange.MkZoneChange ripId Zone.Battlefield Zone.Graveyard
         in HU.assertEqual "no triggers" 0 (length (Event.triggersFrom [toGrave] gs)),
      HU.testCase "SelfEnters matches only a battlefield destination" $ do
        HU.assertBool "enters battlefield matches" (Event.matchesTrigger TriggerCondition.SelfEnters (ZoneChange.MkZoneChange (ObjectId.MkObjectId 1) Zone.Stack Zone.Battlefield))
        HU.assertBool "enters graveyard does not" (not (Event.matchesTrigger TriggerCondition.SelfEnters (ZoneChange.MkZoneChange (ObjectId.MkObjectId 1) Zone.Battlefield Zone.Graveyard)))
```

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "Rest in Peace entering yields"'`
Expected: FAIL to compile — `Event.triggersFrom`, `Event.matchesTrigger` not in scope.

- [x] **Step 3: Add `matchesTrigger` and `triggersFrom`**

In `source/library/Pawl/Event.hs`, add imports `import Pawl.Type.PlayerId (PlayerId)`, `import Pawl.Type.TriggerCondition (TriggerCondition)`, `import qualified Pawl.Type.TriggerCondition as TriggerCondition`, `import Pawl.Type.TriggeredAbility (TriggeredAbility)`, `import qualified Pawl.Type.TriggeredAbility as TriggeredAbility`. Add:

```haskell
-- CR 603.6a: does this condition fire on this event? SelfEnters fires when the
-- bearer's object entered the battlefield -- so the event's destination is the
-- battlefield. This module is the sole home of casing on TriggerCondition.
matchesTrigger :: TriggerCondition -> ZoneChange -> Bool
matchesTrigger cond zc = case cond of
  TriggerCondition.SelfEnters -> ZoneChange.to zc == Zone.Battlefield

-- The battlefield/enters pass of the three-pass trigger scan (leaves-the-
-- battlefield and phase-step passes are future). For each event with to =
-- Battlefield, the newcomer (`object`) is checked for triggered abilities whose
-- condition matches; each becomes a pending trigger paired with its source id and
-- controller (CR 603.3a).
triggersFrom :: [ZoneChange] -> GameState -> [(ObjectId, PlayerId, TriggeredAbility)]
triggersFrom changes gs =
  let fromOne zc =
        let srcId = ZoneChange.object zc
         in case Game.controllerOf srcId gs of
              Nothing -> []
              Just ctrl ->
                map
                  (\ab -> (srcId, ctrl, ab))
                  (filter (\ab -> matchesTrigger (TriggeredAbility.condition ab) zc) (Projection.triggeredAbilitiesOf srcId gs))
   in concatMap fromOne changes
```

(`let` not `where`, per the style guide; `Game.controllerOf` already exists and is total. The `Maybe` import is unnecessary here — drop it if you added it.)

- [x] **Step 4: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS.

- [x] **Step 5: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Detect enters triggers: Event.matchesTrigger + triggersFrom (CR 603.6a)"
```

---

### Task 7: The CR 117.5 loop + the whole-card Rest in Peace gate

**Files:**
- Modify: `source/library/Pawl/Engine.hs` (`placePendingTriggers`, `settleForPriority`; `priorityLoop` uses it)
- Test: `source/test-suite/Pawl/EventSpec.hs` (end-to-end) and `source/test-suite/Pawl/ReplaySpec.hs` (determinism, if a targeted case is warranted)

**Interfaces:**
- Consumes: `Event.triggersFrom` (Task 6), `Source.OfTrigger` (Task 5), `Stack.resolveTop` (Task 5), `checkSba` (existing).
- Produces: `Engine.placePendingTriggers :: Game Bool` (drain `zoneChanges`, put matched triggers on the stack in APNAP order choosing targets, return whether any were placed); `Engine.settleForPriority :: Game ()` (CR 117.5: repeat `checkSba` + `placePendingTriggers` until neither changes state); `priorityLoop` runs `settleForPriority` at each boundary in place of the bare `checkSba`.

- [x] **Step 1: Write the failing end-to-end test**

Add to `source/test-suite/Pawl/EventSpec.hs` (add imports `qualified Pawl.Cast as Cast`, `qualified Pawl.Type.Phase as Phase`, `qualified Pawl.Type.GameState as GameState`, and reuse existing ones). Cast Rest in Peace with white mana, run priority to resolution, and assert the whole card:

```haskell
      HU.testCase "CR 603/614 whole card: cast Rest in Peace, ETB exiles graveyards, then deaths are exiled" $
        let base = S.landsInPlay Card.plainsPrinting 2
            (deadId, withDead) = S.addLibraryCard Card.pikerPrinting S.alice base
            g0 = Event.changeZone deadId Zone.Graveyard withDead -- a card already in the graveyard
            (g1, ripId) = S.handOne Card.restInPeacePrinting g0
            afterCast = snd (Engine.runGamePure S.identityAnswer g1 (Cast.castSpell S.alice ripId))
            -- run priority: both players pass, RiP resolves and enters, its ETB is
            -- placed (CR 117.5) and resolves, exiling the graveyard.
            settled = snd (Engine.runGamePure S.identityAnswer afterCast Engine.priorityLoop)
         in do
              HU.assertEqual "alice's graveyard exiled by the ETB" 0 (length (Game.zoneMembers Zone.Graveyard S.alice settled))
              HU.assertEqual "Rest in Peace is on the battlefield" 1 (S.countOnBattlefieldByName (Text.pack "Rest in Peace") S.alice settled)
              HU.assertEqual "stack empty" [] (GameState.stack settled)
```

If `S.countOnBattlefieldByName` does not exist, add it to `Support.hs` next to `countByName` (a battlefield variant), or assert instead that some battlefield object of alice's is a Rest in Peace by name. Add the minimal helper:

```haskell
countOnBattlefieldByName :: Text.Text -> PlayerId.PlayerId -> GameState.GameState -> Int
countOnBattlefieldByName wanted pid gs =
  let named oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard printing -> Card.Type.name (Printing.card printing) == wanted
          Source.OfAbility _ _ -> False
          Source.OfTrigger _ _ -> False
   in length (filter named (Game.zoneMembers Zone.Battlefield pid gs))
```

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "whole card: cast Rest in Peace"'`
Expected: FAIL — `Engine.settleForPriority` is not wired, so the ETB is never placed and the graveyard survives (or the assertion on the helper fails to compile). If it compiles, the graveyard still has the Piker.

- [x] **Step 3: Add `placePendingTriggers` and `settleForPriority`**

In `source/library/Pawl/Engine.hs`, add `import qualified Pawl.Event as Event`, `import qualified Pawl.Type.Source as Source`, `import qualified Pawl.Type.TriggeredAbility as TriggeredAbility`, and `import Pawl.Type.ObjectId (ObjectId)` (Engine imports `PlayerId` but not `ObjectId` — the pending-trigger tuple needs it). Add:

```haskell
-- CR 603.3: put each triggered ability that fired since the last placement on the
-- stack, in APNAP order (CR 603.3b). M3f has at most one trigger controlled by one
-- player, so the ordering is trivial and the own-order/two-part choice (CR 603.3b)
-- is elided until a second simultaneous trigger exists. Draining zoneChanges makes
-- an event fire its triggers once (CR 603.2c). Targets are chosen as the ability is
-- placed (CR 603.3d); no M3f trigger targets. Returns whether any were placed.
placePendingTriggers :: Game Bool
placePendingTriggers = do
  gs <- State.get
  let changes = GameState.zoneChanges gs
      pending = Event.triggersFrom changes gs
  State.modify' (\g -> g {GameState.zoneChanges = []})
  Monad.mapM_ placeOne (apnapOrder gs pending)
  pure (not (null pending))

-- Put one triggered ability on the stack as a fresh OfTrigger object.
placeOne :: (ObjectId, PlayerId, TriggeredAbility.TriggeredAbility) -> Game ()
placeOne (srcId, controller, ability) = do
  gs <- State.get
  let (abilId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = controller,
            Object.source = Source.OfTrigger srcId ability,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.targets = Map.empty,
            Object.chosenSubtypes = Map.empty,
            Object.timestamp = ts
          }
  State.put gs2 {GameState.objects = Map.insert abilId obj (GameState.objects gs2), GameState.stack = abilId : GameState.stack gs2}

-- CR 603.3b: active player's triggers first, then the others. Stable within a
-- controller (M3f never has two from one controller, so order within is moot).
apnapOrder :: GameState -> [(a, PlayerId, b)] -> [(a, PlayerId, b)]
apnapOrder gs pend =
  let active = GameState.activePlayer gs
      mine (_, p, _) = p == active
   in filter mine pend ++ filter (not . mine) pend

-- CR 117.5: each time a player would receive priority, perform state-based
-- actions, then put triggered abilities on the stack, repeating until neither
-- does anything. Then priority is granted (by the caller).
settleForPriority :: Game ()
settleForPriority = do
  before <- State.get
  checkSba
  placed <- placePendingTriggers
  after <- State.get
  Monad.when (placed || before /= after) settleForPriority
```

(`Object` must be imported qualified in `Engine` for `Object.MkObject`/`Object.owner` — add `import qualified Pawl.Type.Object as Object` if absent. `PlayerId` is already imported.)

**Note on the fixpoint guard:** `before /= after` uses `GameState`'s derived `Eq`. `checkSba` drains `damageEvents`; when there are none it is a genuine no-op (`before == after`). `placePendingTriggers` clears `zoneChanges`; a second iteration finds it empty and places nothing, so the loop terminates.

- [x] **Step 4: Use `settleForPriority` in `priorityLoop`**

In `source/library/Pawl/Engine.hs`, `priorityLoop`'s resolution branch currently runs `Stack.resolveTop` then `checkSba`. Replace that `checkSba` with `settleForPriority`, and add a `settleForPriority` at the top of the inner `loop` so triggers are placed before anyone acts (CR 117.5). Concretely, restructure the inner `loop`:

```haskell
  let loop = do
        settleForPriority
        finished <- State.gets (Maybe.isJust . GameState.result)
        if finished
          then State.modify' (\gs -> gs {GameState.priority = Nothing})
          else do
            gs <- State.get
            case GameState.priority gs of
              Nothing -> pure ()
              Just p -> do
                let decider = Decide.deciderFor p gs
                    actions = Action.legalActions p gs
                chosen <- Trans.lift (Program.prompt (Prompt.ChooseAction decider p actions))
                case chosen of
                  Action.Type.Pass -> do
                    let passes = GameState.passes gs + 1
                        playing = length (Sba.stillPlaying gs)
                    if passes >= fromIntegral playing
                      then case GameState.stack gs of
                        [] -> State.put gs {GameState.priority = Nothing, GameState.passes = passes}
                        _ -> do
                          Stack.resolveTop
                          State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just (GameState.activePlayer g)})
                          loop
                      else do
                        State.put gs {GameState.passes = passes, GameState.priority = Just (nextStillPlaying gs p)}
                        loop
                  Action.Type.Play oid -> do
                    State.modify' (\g -> let played = Event.changeZone oid Zone.Battlefield g in played {GameState.landPlayed = Set.insert p (GameState.landPlayed played), GameState.passes = 0, GameState.priority = Just p})
                    loop
                  Action.Type.Cast oid -> do
                    Cast.castSpell p oid
                    State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just p})
                    loop
                  Action.Type.Activate oid ability -> do
                    Activate.activateAbility p oid ability
                    State.modify' (\g -> g {GameState.passes = 0, GameState.priority = Just p})
                    loop
```

Key changes from the M3e body: (1) `settleForPriority` runs at the top of `loop` (was: a bare `checkSba` only after `resolveTop`); (2) the post-`resolveTop` result-bail is now handled by the next `loop`'s top `settleForPriority` + `finished` check, so the explicit `checkSba`/`result`-case after `resolveTop` is removed. Preserve the outer `priorityLoop` preamble (`priority := active`, `passes := 0`) exactly. This is behavior-preserving where no trigger fires: `settleForPriority` with no triggers is `checkSba` to a no-op fixpoint, and `runStep` already runs `checkSba` before `priorityLoop`, so the extra settle at entry is idempotent.

- [x] **Step 5: Run the whole suite**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — the whole-card gate goes green, and every prior test (the priority loop is heavily covered) stays green. If a prior priority test changed count, the restructure diverged from behavior-preserving; diff against the M3e `priorityLoop` and reconcile — do **not** weaken an assertion.

- [x] **Step 6: Confirm replay determinism**

The `DecisionLog` path now includes the RiP cast, the ETB placement (no target prompt), and the redirected zone changes. If `ReplaySpec` has a targeted deterministic case per milestone, add one that casts Rest in Peace and replays; otherwise the property suite's replay invariant (over the random matchups, which exclude RiP) already covers determinism and no change is needed. Run:

Run: `cabal test --test-options='-p "Replay"'`
Expected: PASS.

- [x] **Step 7: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "CR 117.5 loop: place enters triggers on the stack; whole-card Rest in Peace (CR 603)"
```

---

## Self-Review

**Spec coverage:**

- §0/§2 replacement funnel → Tasks 1 (relocate), 2 (types + pure classifier), 3 (project + card), 4 (wire + gate). ✓
- §1 `ZoneChange`/`ReplacementEffect`/`TriggeredAbility`/`TriggerCondition` types → Tasks 2, 5. `Effect.ExileAllGraveyards`, `Source.OfTrigger`, `Card`/`PC`/`GameState` fields → Tasks 3, 5. ✓
- §3 triggers + CR 117.5 loop → Tasks 5 (resolution), 6 (detection), 7 (placement loop). ✓
- §4 shared `resolveEffects` → Task 5 Step 10. ✓
- §5 invariants: `Pawl.Event` sole home of `case ReplacementEffect`/`TriggerCondition` (Tasks 2, 6); `resolveTop` dispatches on `Source` (Task 5). ✓
- §6 setup/testing: Plains + Rest in Peace deterministic fixtures (Task 3); gate assertions (Tasks 4, 5, 7); properties unchanged (RiP out of random decks). ✓
- §7 expiries are documented in the spec; no task implements them (correct). The one **spec refinement** this plan makes: replacements are projected (Task 3) via a relocated `changeZone` (Task 1) — the module-graph resolution the spec's "through the projection" wording assumed but did not detail. Recorded here and in the Task 1 header.

**Placeholder scan:** no TBD/TODO; every code step shows complete code. The one `error` that appeared in a first draft of `triggersFrom` (Task 6 Step 3) is explicitly corrected in the same step to a total `case`.

**Type consistency:** `resolveEffects :: ObjectId -> ObjectId -> [Effect] -> Map SlotName TargetSpec -> Game ()` is defined in Task 5 and consumed by `Stack.resolveTop` (Task 5) with matching argument order (stack-object id, source id, effects, specs). `triggersFrom` returns `[(ObjectId, PlayerId, TriggeredAbility)]` (Task 6), consumed by `placePendingTriggers`/`apnapOrder`/`placeOne` (Task 7) with the same tuple shape. `ZoneChange.object` is the post-move id in Task 4's emission and is read as the newcomer in Task 6's `triggersFrom`. `replacementsAffecting`/`replacementsOf` (Task 3) are consumed by `changeZone` (Task 4). Consistent.

**Expiry note deferred to git-bug:** the headline expiry (monadic replacement path + CR 616 ordering) should be filed as a git-bug when Task 4 lands, per the spec §7 ("tracked in git-bug so it cannot rot"). Add: `git-bug bug new -t "M3f: replacement seam is pure/single — CR 616 ordering + choice-bearing replacements deferred" -m "..."`.
