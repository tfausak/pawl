# M4.5 P4 — Event history, and the triggers that are not events Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the engine's two drain queues with one turn-scoped event log carrying last-known-information snapshots, and grow `TriggerCondition` from "an event on this object" into a classification that also covers step beginnings (CR 603.2b), game *state* (CR 603.8), delayed abilities (CR 603.7) and intervening "if" (CR 603.4 / 608.2a) — proved by four gate cards.

**Architecture:** `GameState.events :: Seq GameEvent` is appended by the existing change-and-emit funnels and **never cleared by a reader**; each reader (the CR 117.5 trigger scan, the CR 704 damage SBA) keeps a `Natural` watermark and consumes by bumping it. The log is cleared only at turn handoff. `Pawl.Event` stays the sole home of `case … TriggerCondition` and becomes the sole home of `case … StateCondition`; it gathers pending triggers from three sources (event-matched over *all* battlefield permanents, state-matched with derived armedness, delayed-store-matched) and `Pawl.Engine` orders them (CR 603.3b, a new prompt) and places them.

**Tech Stack:** Haskell 2010 (GHC 9.14.1 from the Nix flake), `tasty` + `tasty-hunit`, the hand-rolled `Pawl.Json`/`Pawl.Codec` card codec.

**Spec:** `docs/superpowers/specs/2026-07-21-p4-event-history-triggers-design.md`. Read §2.1–2.2 before Task 1, §2.3 before Tasks 2/4, §2.4 before Task 6, §2.6 before Task 7, §8 before Task 8.

## Global Constraints

Every task's requirements implicitly include all of these. They come from `CLAUDE.md` and are not negotiable.

- **Haskell 2010, no language extensions** unless there is no alternative. `NamedFieldPuns` is permitted; `GADTs`/`RankNTypes` only in the suspension core. Modules that already carry a `{-# LANGUAGE … #-}` pragma keep it.
- **No explicit export lists.** `module Pawl.Foo where`.
- **One type per module** under `Pawl.Type.<TypeName>` (type + instances only). A module never imports its parents; `A.B.C` must not import `A.B` or `A`.
- **Qualified imports aliased to the last component** (`Data.Sequence` → `Seq`, `Pawl.Type.GameEvent` → `GameEvent`). One import group, alphabetical. The one documented exception is `Pawl.Support` as `S` in the test suite.
- **No partial functions**, written or used. No `head`, `error`, `undefined`, or non-exhaustive matches.
- **`newtype` liberally, non-punning constructors** (`MkFoo`). Build records with `do` + record syntax, not `<$>`/`<*>`.
- **Prefer explicit:** `case` over point-free; one equation with a `case` over multiple clauses; `let` over `where`; `$` over parens, `.` over chained `$`. No list comprehensions. No backtick-infixed functions.
- **`Text` not `String`.** Arbitrary-precision numbers (`Integer`, `Natural`).
- **No boolean blindness** — a custom sum type beats a bare `Bool`.
- **Derive at least `Eq` and `Show`.** A type that rides a `Set`/`Map` key, a `Binding`, a `Card`, or `ProjectedCharacteristics` also derives `Ord`.
- **No API stability obligations.** Rename, reshape, and delete freely; never add a compat shim or keep an old name.
- **Every rules claim is checked against `docs/rules.txt`** and the rule number is cited in the code comment. Never trust recalled Magic rules.
- **Build must be warning-clean.** `cabal build all --enable-tests --enable-benchmarks` with `flags: +pedantic` (which is `-Werror`). Incremental builds hide warnings from unchanged modules; `cabal clean` first when a definitive check is needed.
- **Before every commit:** `git add <the paths this task names>`, then `hooky fix`, then `git add` again, then `hooky run`. `hooky` acts on **staged** files only; if you skip the `git add`, it reports "hooks skipped" and checks nothing.
- **TDD is not optional.** Write the failing test and actually run it to watch it fail before implementing.
- **Never edit this plan, weaken an assertion, or delete a test to make a check pass.** If the plan looks wrong, stop and say so.
- **One small complete commit per task, on `main`.** Commit messages end with the `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>` trailer.
- **Concurrent sessions share this checkout.** Stage the explicit paths each task names; do not blanket-stage foreign files that appear in `git status`.

## Card verification (already done — do not re-fetch)

All four gate cards were verified live against the Scryfall API while this plan was written. Use these values verbatim; there is no need for network access during execution.

| Card | Cost | Type line | P/T | Oracle text |
|---|---|---|---|---|
| Barbarian Outcast | `{1}{R}` | Creature — Human Barbarian Beast | 2/2 | "When you control no Swamps, sacrifice this creature." |
| Tidal Wave | `{2}{U}` | Instant | — | "Create a 5/5 blue Wall creature token with defender. Sacrifice it at the beginning of the next end step." |
| Khabál Ghoul | `{2}{B}` | Creature — Zombie | 1/1 | "At the beginning of each end step, put a +1/+1 counter on this creature for each creature that died this turn." |
| Sarcomancy | `{B}` | Enchantment | — | "When this enchantment enters, create a 2/2 black Zombie creature token.\nAt the beginning of your upkeep, if there are no Zombies on the battlefield, this enchantment deals 1 damage to you." |

**Rulings** (Scryfall). Three of the four have none. Khabál Ghoul has exactly one, and it confirms the design rather than deferring anything:

> "The effect counts all creatures put into all graveyards from the battlefield during the turn. This includes creature tokens put into a graveyard as well as creatures put into a graveyard before Khabál Ghoul entered the battlefield."

Both halves are load-bearing: **tokens count** (which is why the event payload is a snapshot, not a card reference — a token has no printed card, CR 111.3) and **deaths before the Ghoul arrived count** (which is why the log is turn-scoped and not drained).

**Filename hazard, already checked.** `Codec.slugify` keeps alphanumerics, so Khabál Ghoul's slug is `khabál-ghoul` and its file must literally be `data/cards/khabál-ghoul.json`. `Pawl.CardSpec`'s directory/registry lint compares `Directory.listDirectory` output against that slug, so a filesystem that renormalised the name would fail the lint. This repo's volume is APFS and was verified to preserve NFC (`á` = U+00E1) round-trip through `listDirectory`. Write the file with a plain NFC `á`; if the lint ever reports a mismatch here, that is the cause.

## File Structure

**Library — new modules** (all `Pawl.Type.<Name>`, type + instances only):

| File | Responsibility | Task |
|---|---|---|
| `source/library/Pawl/Type/GameEvent.hs` | One entry of the turn-scoped log: `Moved`, `DamageDealt`, `StepBegan` | 1 |
| `source/library/Pawl/Type/TurnScope.hs` | `EachTurn \| ControllersTurn` — whose turn a step trigger watches | 2 |
| `source/library/Pawl/Type/PendingTrigger.hs` | A trigger that has triggered but is not yet on the stack | 2 |
| `source/library/Pawl/Type/StateCondition.hs` | CR 603.8 / 603.4 state predicates, hand-carved; EXPIRES at P9 | 4 |
| `source/library/Pawl/Type/AbilityName.hs` | The name joining `ArmDelayedTrigger` to `Card.delayedAbilities` | 6 |
| `source/library/Pawl/Type/DelayedTrigger.hs` | A CR 603.7 delayed ability awaiting its event | 6 |

**Library — modified:**

| File | Responsibility after this plan |
|---|---|
| `source/library/Pawl/Type/GameState.hs` | `+events`, `+scannedThrough`, `+damageScannedThrough`, `+delayedTriggers`; `−zoneChanges`, `−damageEvents` |
| `source/library/Pawl/Type/TriggerCondition.hs` | `+StepBegins Phase TurnScope`, `+StateIs StateCondition` |
| `source/library/Pawl/Type/TriggeredAbility.hs` | `+intervening :: Maybe StateCondition` |
| `source/library/Pawl/Type/Card.hs` | `+delayedAbilities :: Map AbilityName (TriggeredAbility Card)` |
| `source/library/Pawl/Type/Effect.hs` | `+Sacrifice SlotName`, `+ArmDelayedTrigger AbilityName`; `Create` grows a `Maybe SlotName` |
| `source/library/Pawl/Type/CountSpec.hs` | `+CreaturesDiedThisTurn` |
| `source/library/Pawl/Type/Subtype.hs` | `+Barbarian`, `+Zombie` |
| `source/library/Pawl/Type/Prompt.hs` | `+OrderTriggers` |
| `source/library/Pawl/Type/Response.hs` | `+OrderedTriggers` |
| `source/library/Pawl/Event.hs` | the log funnel (`recordEvent`, `movedOf`, `damageOf`, `unscanned*`); `sacrifice`; the three-source trigger gather; sole casing home for `TriggerCondition` and `StateCondition` |
| `source/library/Pawl/Engine.hs` | step-begin emission, watermarked scan, the CR 603.3b ordering prompt, the pre-handoff settle, log clearing at handoff |
| `source/library/Pawl/Sba.hs` | watermarked damage read |
| `source/library/Pawl/Damage.hs` | `applyDamage` appends `DamageDealt` |
| `source/library/Pawl/Resolve.hs` | `Sacrifice`, `ArmDelayedTrigger`, `Create`'s slot binding; reserved slots exempt from the CR 608.2b legality read |
| `source/library/Pawl/Binding.hs` | reserved slots `triggerSource` ("self") and `you` |
| `source/library/Pawl/Quantity.hs` | `CreaturesDiedThisTurn` folds the log |
| `source/library/Pawl/Stack.hs` | the CR 608.2a intervening-"if" re-check |
| `source/library/Pawl/Setup.hs` | `emptyGame`'s new fields |
| `source/library/Pawl/Codec.hs` | JSON for every new/changed type |
| `source/library/Pawl/Replay.hs` | `OrderTriggers` encode/decode/default |

**Data — new card files** under `data/cards/`: `barbarian-outcast.json`, `khabál-ghoul.json`, `tidal-wave.json`, `sarcomancy.json`. **No existing card file changes**: every new `Card` and `TriggeredAbility` field is omitted from the render when it is empty/`Nothing`, and `Create`'s slot is omitted when absent — the `copyOnEnter`/`characteristicPT` precedent. `Pawl.CardsSpec`'s byte-stability assertion is the check that this held.

**Test suite:**

| File | Responsibility |
|---|---|
| `source/test-suite/Pawl/TriggerSpec.hs` | **new.** The phase's own spec (the `PowerToughnessSpec`/`ColorSpec` precedent): the log, the scan, all four gate cards, the ordering scenario |
| `source/test-suite/Pawl/Support.hs` | new fixtures/readers for the log; the new prompt arm in five answerers |
| `source/test-suite/Pawl/Cards.hs` | four new printing fields, loads, and `allPrintings` entries |
| `source/test-suite/Pawl/CardSpec.hs` | the new lint family members |
| `source/test-suite/Pawl/CodecSpec.hs` | round-trips for every new type |
| `source/test-suite/Pawl/ReplaySpec.hs` | the `OrderTriggers` transcript round-trip |
| `source/test-suite/Pawl/{Damage,Resolve,Modal,Event}Spec.hs` | call sites that read the two removed fields |
| `source/test-suite/Pawl/{Game,Cast,Copy}Spec.hs`, `source/benchmark/Main.hs` | the new prompt arm in their answerers |
| `source/test-suite/Main.hs` | wires `TriggerSpec.tests` into `testTree` |

`Pawl.TriggerSpec` must be added to the test-suite `other-modules` list in `pawl.cabal` — that field is generated by a `-- cabal-gild: discover` directive, so add the file and let `hooky fix` regenerate it rather than hand-editing.

---

### Task 1: One turn-scoped log replaces the two drain queues

The riskiest change, and deliberately **behaviour-preserving** — the existing suite is the regression test. `GameState.zoneChanges` and `GameState.damageEvents` become slices of one `Seq GameEvent` that no reader clears; each reader advances a watermark instead. The log is cleared at turn handoff, and `Engine.advance` settles once before handing off so no event is discarded unscanned.

**Files:**
- Create: `source/library/Pawl/Type/GameEvent.hs`
- Modify: `source/library/Pawl/Type/GameState.hs`
- Modify: `source/library/Pawl/Event.hs`
- Modify: `source/library/Pawl/Damage.hs:157-172`
- Modify: `source/library/Pawl/Sba.hs:52-58`, `:166-180`
- Modify: `source/library/Pawl/Engine.hs:198-205`, `:381-404`
- Modify: `source/library/Pawl/Setup.hs:68-69`
- Modify: `source/library/Pawl/Codec.hs`
- Test: `source/test-suite/Pawl/TriggerSpec.hs` (new), `source/test-suite/Pawl/Support.hs`, `source/test-suite/Pawl/CodecSpec.hs`, `source/test-suite/Pawl/{Damage,Resolve,Modal,Event}Spec.hs`, `source/test-suite/Main.hs`

**Interfaces:**
- Produces: `Pawl.Type.GameEvent.GameEvent = Moved ZoneChange ProjectedCharacteristics | DamageDealt DamageEvent | StepBegan Phase PlayerId`; `GameState.events :: Seq GameEvent`, `GameState.scannedThrough :: Natural`, `GameState.damageScannedThrough :: Natural`; `Pawl.Event.recordEvent :: GameEvent -> GameState -> GameState`, `Pawl.Event.movedOf :: GameEvent -> Maybe ZoneChange`, `Pawl.Event.damageOf :: GameEvent -> Maybe DamageEvent`, `Pawl.Event.unscannedEvents :: GameState -> [GameEvent]`, `Pawl.Event.unscannedDamage :: GameState -> [DamageEvent]`, `Pawl.Event.placeObject :: PlayerId -> (Timestamp -> Object) -> Zone -> GameState -> (ObjectId, GameState)`; `Pawl.Codec.gameEventToJson` / `jsonToGameEvent`, `phaseToJson` / `jsonToPhase`; `Pawl.Support.damageEventsOf`, `zoneChangesOf`, `withEvent`.
- Consumes: nothing new.

- [x] **Step 1: Write the failing tests**

Create `source/test-suite/Pawl/TriggerSpec.hs`:

```haskell
-- Covers M4.5 P4: the turn-scoped event log (Pawl.Type.GameEvent, GameState's
-- log and watermarks), the widened CR 603.6a trigger scan, state triggers (CR
-- 603.8), delayed triggers (CR 603.7), intervening "if" (CR 603.4 / 608.2a) and
-- the CR 603.3b ordering prompt.
module Pawl.TriggerSpec where

import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Cards as Cards
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Sba as Sba
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.GameEvent as GameEvent
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChange as ZoneChange
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- CR 608.2i: the log records; it is never emptied by a reader.
logTests :: Cards.Cards -> Tasty.TestTree
logTests cards =
  Tasty.testGroup
    "EventLog"
    [ HU.testCase "CR 400.7 a zone change appends a Moved event carrying the destination" $
        let (piker, gs) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
            after = Event.changeZone piker Zone.Graveyard gs
         in case S.zoneChangesOf after of
              zc : _ -> HU.assertEqual "event says graveyard" Zone.Graveyard (ZoneChange.to zc)
              [] -> HU.assertFailure "expected an emitted zone change",
      -- CR 608.2h: the snapshot is the object as it last existed in the zone it
      -- LEFT. Re-deriving from the printed card would be wrong for an animated
      -- land and impossible for a token (CR 111.3).
      HU.testCase "CR 608.2h a Moved event snapshots the object it moved" $
        let (piker, gs) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
            expected = Projection.project piker gs
            after = Event.changeZone piker Zone.Graveyard gs
         in case Foldable.toList (GameState.events after) of
              GameEvent.Moved _ snapshot : _ -> HU.assertEqual "snapshot from the origin zone" expected snapshot
              _ -> HU.assertFailure "expected exactly one Moved event",
      -- CR 704.5h's window is "since the last SBA check": the check CONSUMES by
      -- bumping a watermark, and the record survives.
      HU.testCase "CR 704 the SBA check advances the damage watermark but keeps the record" $
        let (gs, _, _) = S.combatBoardOf [Cards.typhoidRatsPrinting cards] [Cards.ogreSentryPrinting cards]
            fought = S.fightWith S.aggressiveAnswer gs
            after = Sba.checkStateBasedActions fought
         in do
              HU.assertEqual "nothing left unscanned for damage" [] (Event.unscannedDamage after)
              HU.assertBool "the damage events are still recorded" (not (null (S.damageEventsOf after))),
      HU.testCase "CR 117.5 the trigger scan advances its watermark but keeps the record" $
        let (_, gs) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (piker, gs1) = S.addPiker cards S.bob gs
            moved = Event.changeZone piker Zone.Hand gs1
            scanned = snd (Engine.runGamePure S.identityAnswer moved Engine.placePendingTriggers)
         in do
              HU.assertEqual "nothing left unscanned" [] (Event.unscannedEvents scanned)
              HU.assertBool "the zone change is still recorded" (not (null (S.zoneChangesOf scanned))),
      -- The turn is the log's scope (CR 608.2i). Clearing at cleanup would be
      -- wrong: cleanup is still part of THIS turn.
      HU.testCase "the log and both watermarks are cleared at turn handoff" $
        let (piker, gs) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
            moved = Event.changeZone piker Zone.Graveyard gs
            after = snd (Engine.runGamePure S.identityAnswer moved Engine.handoffTurn)
         in do
              HU.assertEqual "log empty" Seq.empty (GameState.events after)
              HU.assertEqual "scan watermark reset" 0 (GameState.scannedThrough after)
              HU.assertEqual "damage watermark reset" 0 (GameState.damageScannedThrough after),
      -- CR 514.3 (partial): an event emitted by the cleanup step must be scanned
      -- BEFORE handoffTurn clears the log, or its trigger is lost outright.
      HU.testCase "advance settles before handing off, so no unscanned event is discarded" $
        let (ripId, gs0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            entered = ZoneChange.MkZoneChange ripId Zone.Stack Zone.Battlefield
            gs1 = S.withEvent (GameEvent.Moved entered (Projection.project ripId gs0)) gs0
            ending = gs1 {GameState.remaining = Seq.empty}
            after = snd (Engine.runGamePure S.identityAnswer ending Engine.advance)
            isTrigger oid = case Game.lookupObject oid after of
              Just obj -> case Object.source obj of
                Source.OfTrigger _ _ -> True
                _ -> False
              Nothing -> False
         in do
              HU.assertBool "the pending trigger reached the stack" (any isTrigger (GameState.stack after))
              HU.assertEqual "the log was cleared afterwards" Seq.empty (GameState.events after)
    ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Pawl.TriggerSpec" [logTests cards]
```

Add to `source/test-suite/Main.hs`: `import qualified Pawl.TriggerSpec as TriggerSpec` (alphabetically, after `Pawl.SetupSpec`) and `TriggerSpec.tests cards,` as the last entry of `testTree`.

Add to `source/test-suite/Pawl/CodecSpec.hs`, inside the top-level `Tasty.testGroup "Pawl.CodecSpec"` list, a new group (put it immediately before the closing `]`):

```haskell
      Tasty.testGroup
        "P4 runtime types"
        [ HU.testCase "Phase round-trips" $
            mapM_
              (roundTrip "phase" Codec.phaseToJson Codec.jsonToPhase)
              [ Phase.Beginning BeginningStep.Upkeep,
                Phase.PrecombatMain,
                Phase.Combat CombatStep.DeclareBlockers,
                Phase.PostcombatMain,
                Phase.Ending EndingStep.EndStep
              ],
          HU.testCase "GameEvent.Moved round-trips with its snapshot" $
            let zc = ZoneChange.MkZoneChange (ObjectId.MkObjectId 3) Zone.Battlefield Zone.Graveyard
                snapshot = Projection.project (ObjectId.MkObjectId 3) (Setup.emptyGame S.bothPlayers)
             in roundTrip "moved" Codec.gameEventToJson Codec.jsonToGameEvent (GameEvent.Moved zc snapshot),
          HU.testCase "GameEvent.DamageDealt round-trips" $
            roundTrip
              "damage"
              Codec.gameEventToJson
              Codec.jsonToGameEvent
              (GameEvent.DamageDealt (DamageEvent.MkDamageEvent (ObjectId.MkObjectId 1) (Recipient.ToPlayer S.bob) 2 True DamageKind.Combat)),
          HU.testCase "GameEvent.StepBegan round-trips" $
            roundTrip "step" Codec.gameEventToJson Codec.jsonToGameEvent (GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice)
        ]
```

with these added imports in `CodecSpec.hs`: `Pawl.Projection` as `Projection`, `Pawl.Setup` as `Setup`, `Pawl.Support` as `S`, `Pawl.Type.BeginningStep` as `BeginningStep`, `Pawl.Type.CombatStep` as `CombatStep`, `Pawl.Type.DamageEvent` as `DamageEvent`, `Pawl.Type.DamageKind` as `DamageKind`, `Pawl.Type.EndingStep` as `EndingStep`, `Pawl.Type.GameEvent` as `GameEvent`, `Pawl.Type.Phase` as `Phase`, `Pawl.Type.ProjectedCharacteristics` as `PC` (if needed), `Pawl.Type.Recipient` as `Recipient`, `Pawl.Type.ZoneChange` as `ZoneChange`.

- [x] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Pawl.TriggerSpec` does not exist; `Pawl.Type.GameEvent` is not a module; `Event.unscannedDamage`, `S.zoneChangesOf`, `S.damageEventsOf`, `S.withEvent`, `Codec.gameEventToJson`, `Codec.phaseToJson`, `GameState.events`, `GameState.scannedThrough`, `GameState.damageScannedThrough` are not in scope.

- [x] **Step 3: Add the `GameEvent` type**

Create `source/library/Pawl/Type/GameEvent.hs`:

```haskell
module Pawl.Type.GameEvent where

import Pawl.Type.DamageEvent (DamageEvent)
import Pawl.Type.Phase (Phase)
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.ProjectedCharacteristics (ProjectedCharacteristics)
import Pawl.Type.ZoneChange (ZoneChange)

-- CR 608.2i: one entry of the turn-scoped record of what happened. "Some effects
-- look back in time and require information about previous game states and
-- actions rather than considering the current game state" -- so entries are
-- APPENDED by the change-and-emit funnels and never removed by a reader. Each
-- reader keeps its own watermark into GameState.events; the log itself is cleared
-- only at turn handoff.
data GameEvent
  = -- CR 400.7: an object moved between zones. The ZoneChange is the RESOLVED
    -- (post-replacement) event, carrying the RESULTING object's id.
    --
    -- The ProjectedCharacteristics is the moved object as it last existed in the
    -- zone it LEFT (CR 608.2h: "if it's no longer in that zone ... the effect uses
    -- the object's last known information"). A snapshot, never a re-derivation
    -- from the printed card: a land animated into a creature DIED as a creature,
    -- and a token has no printed card at all (CR 111.3).
    --
    -- STRICT (a plain Haskell 2010 field annotation, no extension). Lazy, the
    -- snapshot would be a thunk closing over the entire PRE-MOVE GameState,
    -- appended to a log that lives a whole turn -- defeating the Map.delete in
    -- changeZone and retaining a turn of superseded states. Measured: no
    -- regression outside run-to-run noise (see Event.changeZone).
    Moved ZoneChange !ProjectedCharacteristics
  | -- CR 120 / 510: damage was dealt. The record the CR 704.5h deathtouch
    -- state-based action reads, watermarked rather than drained.
    DamageDealt DamageEvent
  | -- CR 603.2b: a phase or step began, on whose turn (the active player). What a
    -- "at the beginning of each end step" trigger and a CR 603.7 delayed ability
    -- both match against.
    StepBegan Phase PlayerId
  deriving (Eq, Ord, Show)
```

- [x] **Step 4: Swap the two drain queues for the log in `GameState`**

In `source/library/Pawl/Type/GameState.hs`, replace the `damageEvents` and `zoneChanges` fields (and their comments) with:

```haskell
    -- CR 608.2i: what happened this turn, in order. Appended by the
    -- change-and-emit funnels (Event.changeZone, Event.createToken,
    -- Damage.applyDamage) and by Engine.runStep's step-begin emission; NEVER
    -- cleared by a reader. Cleared with both watermarks at turn handoff
    -- (Engine.handoffTurn) -- not at cleanup, which is still part of this turn.
    events :: Seq GameEvent,
    -- CR 117.5: how far the trigger scan has consumed. Everything at or after
    -- this index is unscanned. Consumption is an index bump; the record stays.
    scannedThrough :: Natural,
    -- CR 704.5h ("since the last state-based action check"): how far the
    -- state-based-action damage read has consumed.
    damageScannedThrough :: Natural,
```

Replace the `Pawl.Type.DamageEvent` and `Pawl.Type.ZoneChange` imports with `import Pawl.Type.GameEvent (GameEvent)`.

- [x] **Step 5: Make the funnels record instead of enqueue**

In `source/library/Pawl/Event.hs`, add these accessors near the top (after the module comment's imports), and add `import qualified Data.Foldable as Foldable`, `import qualified Data.Sequence as Seq`, `import Pawl.Type.GameEvent (GameEvent)`, `import qualified Pawl.Type.GameEvent as GameEvent`:

```haskell
-- CR 608.2i: append one entry to the turn-scoped log. The single write point;
-- nothing else touches GameState.events.
recordEvent :: GameEvent -> GameState -> GameState
recordEvent event gs = gs {GameState.events = GameState.events gs Seq.|> event}

-- The zone change an event describes, if it is one.
movedOf :: GameEvent -> Maybe ZoneChange
movedOf event = case event of
  GameEvent.Moved zc _ -> Just zc
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.StepBegan _ _ -> Nothing

-- The damage an event describes, if it is any.
damageOf :: GameEvent -> Maybe DamageEvent
damageOf event = case event of
  GameEvent.DamageDealt ev -> Just ev
  GameEvent.Moved _ _ -> Nothing
  GameEvent.StepBegan _ _ -> Nothing

-- CR 117.5: the events the trigger scan has not yet consumed.
unscannedEvents :: GameState -> [GameEvent]
unscannedEvents gs =
  Foldable.toList (Seq.drop (fromIntegral (GameState.scannedThrough gs)) (GameState.events gs))

-- CR 704.5h: the damage the state-based-action check has not yet consumed.
unscannedDamage :: GameState -> [DamageEvent]
unscannedDamage gs =
  Maybe.mapMaybe damageOf (Foldable.toList (Seq.drop (fromIntegral (GameState.damageScannedThrough gs)) (GameState.events gs)))
```

`Pawl.Event` does not currently import `Data.Maybe`; add `import qualified Data.Maybe as Maybe`.

Replace `placeObject` and its two callers. `placeObject` no longer emits — each caller owns its own last-known-information snapshot, because only the caller knows which state to project against:

```haskell
-- Insert a freshly-built object into `dest` under a new id and timestamp, and
-- return that id. The common tail of changeZone (a moved incarnation) and
-- createToken (a token from nothing). `mkObj` receives the fresh timestamp so the
-- object records when it entered (CR 613.7d). The Moved event is emitted by the
-- CALLER: only it knows which state the CR 608.2h snapshot must be taken against.
placeObject :: PlayerId -> (Timestamp.Timestamp -> Object.Object) -> Zone -> GameState -> (ObjectId, GameState)
placeObject pid mkObj dest gs =
  let (newId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj = markCopyOnEnter dest (mkObj ts)
      gs3 = gs2 {GameState.objects = Map.insert newId obj (GameState.objects gs2)}
   in (newId, Game.insertIntoZone dest pid newId gs3)
```

In `changeZone`, replace the final `in placeObject …` with a snapshot-then-place-then-record:

```haskell
changeZone :: ObjectId -> Zone -> GameState -> GameState
changeZone oid requestedDest gs = case Game.lookupObject oid gs of
  Nothing -> gs
  Just obj ->
    let pid = Object.owner obj
        fromZone = Object.zone obj
        -- CR 608.2h: last known information -- the object as it exists in the zone
        -- it is LEAVING, projected against the PRE-MOVE state. Costs one board
        -- projection per zone change; that is the price of an honest history (a
        -- token has no printed card to re-derive from, CR 111.3).
        snapshot = Projection.project oid gs
        -- CR 614.4: replacements exist before the event; read them from the
        -- pre-move state. CR 614.6: the modified event is what actually happens.
        proposed = ZoneChange.MkZoneChange oid fromZone requestedDest
        resolved = applyReplacements (Projection.replacementsAffecting gs) proposed
        dest = ZoneChange.to resolved
        mkObj ts = obj {Object.zone = dest, Object.tapped = TapState.Untapped, Object.damage = 0, Object.sickness = Sickness.Sick, Object.bindings = Map.empty, Object.counters = Map.empty, Object.timestamp = ts}
        gs1 = Game.removeFromZones pid oid gs
        gs2 = gs1 {GameState.objects = Map.delete oid (GameState.objects gs1)}
        (newId, placed) = placeObject pid mkObj dest gs2
     in -- CR 603.2g: record the RESOLVED event, carrying the NEW object's id --
        -- what an enters trigger scans.
        recordEvent (GameEvent.Moved (ZoneChange.MkZoneChange newId fromZone dest) snapshot) placed
```

In `createToken`, replace the final `in placeObject …` with:

```haskell
   in let (newId, placed) = placeObject controller mkObj Zone.Battlefield gs
          -- A token is created from nothing, so there is no prior incarnation to
          -- snapshot: its last known information IS what it is now (CR 111.3 makes
          -- the creating effect's stated values functionally printed values).
          snapshot = Projection.project newId placed
       in recordEvent (GameEvent.Moved (ZoneChange.MkZoneChange newId Zone.Battlefield Zone.Battlefield) snapshot) placed
```

In `source/library/Pawl/Damage.hs`, change `applyDamage`'s last line from the `damageEvents` append to:

```haskell
   in -- CR 608.2i: each kept event is RECORDED, not enqueued. Sba consumes by
      -- bumping GameState.damageScannedThrough; the record survives the check.
      List.foldl' (\g ev -> Event.recordEvent (GameEvent.DamageDealt ev) g) marked kept
```

adding `import qualified Pawl.Type.GameEvent as GameEvent`.

- [x] **Step 6: Watermark the two readers, and settle before handoff**

In `source/library/Pawl/Sba.hs`, change `woundedByDeathtouch`'s last line from `(GameState.damageEvents gs)` to `(Event.unscannedDamage gs)`, and update its comment's final sentence to read "…read from the WATERMARKED slice of the turn log, not a drained queue."

Replace the `drained` binding in `performStateBasedActions` with:

```haskell
      -- CR 704.5h's window is "since the last SBA check", so the watermark is
      -- advanced to the log length AS THIS PASS BEGAN: every 704.5h victim was
      -- computed from that same pre-pass state, and the Moved events this pass
      -- itself appends carry no damage. The record is never removed.
      drained = vanished {GameState.damageScannedThrough = fromIntegral (Seq.length (GameState.events gs))}
```

adding `import qualified Data.Sequence as Seq`.

In `source/library/Pawl/Engine.hs`, rewrite `placePendingTriggers`'s body (leave `placeOne` and `apnapOrder` alone in this task) and update its comment's "Draining zoneChanges" sentence:

```haskell
-- CR 603.3: put each triggered ability that fired since the last placement on the
-- stack, in APNAP order (CR 603.3b). M3f has at most one trigger controlled by one
-- player, so the ordering is trivial and the own-order/two-part choice (CR 603.3b)
-- is elided until a second simultaneous trigger exists. Advancing scannedThrough
-- makes an event fire its triggers once (CR 603.2c) WITHOUT discarding the record.
-- Targets are chosen as the ability is placed (CR 603.3d). Returns whether any
-- were placed.
placePendingTriggers :: Game Bool
placePendingTriggers = do
  gs <- State.get
  let changes = Maybe.mapMaybe Event.movedOf (Event.unscannedEvents gs)
      pending = Event.triggersFrom changes gs
  State.modify' (\g -> g {GameState.scannedThrough = fromIntegral (Seq.length (GameState.events g))})
  Monad.mapM_ placeOne (apnapOrder gs pending)
  pure (not (null pending))
```

Add the log clear to `handoffTurn`'s record update, alongside `turnNumber`:

```haskell
          -- CR 608.2i: the log's scope is ONE turn. Cleared here, with both
          -- watermarks, and never at cleanup -- cleanup is still part of this turn
          -- and CR 514.1's discard is itself an event of it. Engine.advance settles
          -- immediately before calling this, so nothing unscanned is discarded.
          GameState.events = Seq.empty,
          GameState.scannedThrough = 0,
          GameState.damageScannedThrough = 0,
```

Change `advance`'s empty-schedule branch:

```haskell
    -- CR 514.3 (partial) / 117.5: the turn is over. Settle once more so every
    -- event the cleanup step's turn-based actions emitted is scanned BEFORE
    -- handoffTurn clears the log -- an unscanned event discarded at handoff is a
    -- lost trigger. EXPIRES with the full CR 514.3: the extra cleanup step and the
    -- priority round it grants are not implemented, so a trigger placed here
    -- resolves at the next turn's first priority rather than during this cleanup.
    Seq.EmptyL -> do
      settleForPriority
      handoffTurn
```

In `source/library/Pawl/Setup.hs`, replace the two `emptyGame` lines with:

```haskell
          GameState.events = Seq.empty,
          GameState.scannedThrough = 0,
          GameState.damageScannedThrough = 0,
```

- [x] **Step 7: Add the codec arms**

In `source/library/Pawl/Codec.hs`, add (after `zoneToJson`/`jsonToZone`, keeping the leaf-enum section together):

```haskell
beginningStepToJson :: BeginningStep.BeginningStep -> Value
beginningStepToJson s = nullary . Text.pack $ case s of
  BeginningStep.Untap -> "Untap"
  BeginningStep.Upkeep -> "Upkeep"
  BeginningStep.DrawStep -> "DrawStep"

jsonToBeginningStep :: Value -> Either Text BeginningStep.BeginningStep
jsonToBeginningStep =
  decodeNullary
    (Text.pack "BeginningStep")
    [ (Text.pack "Untap", BeginningStep.Untap),
      (Text.pack "Upkeep", BeginningStep.Upkeep),
      (Text.pack "DrawStep", BeginningStep.DrawStep)
    ]

combatStepToJson :: CombatStep.CombatStep -> Value
combatStepToJson s = nullary . Text.pack $ case s of
  CombatStep.BeginningOfCombat -> "BeginningOfCombat"
  CombatStep.DeclareAttackers -> "DeclareAttackers"
  CombatStep.DeclareBlockers -> "DeclareBlockers"
  CombatStep.CombatDamage -> "CombatDamage"
  CombatStep.EndOfCombat -> "EndOfCombat"

jsonToCombatStep :: Value -> Either Text CombatStep.CombatStep
jsonToCombatStep =
  decodeNullary
    (Text.pack "CombatStep")
    [ (Text.pack "BeginningOfCombat", CombatStep.BeginningOfCombat),
      (Text.pack "DeclareAttackers", CombatStep.DeclareAttackers),
      (Text.pack "DeclareBlockers", CombatStep.DeclareBlockers),
      (Text.pack "CombatDamage", CombatStep.CombatDamage),
      (Text.pack "EndOfCombat", CombatStep.EndOfCombat)
    ]

endingStepToJson :: EndingStep.EndingStep -> Value
endingStepToJson s = nullary . Text.pack $ case s of
  EndingStep.EndStep -> "EndStep"
  EndingStep.Cleanup -> "Cleanup"

jsonToEndingStep :: Value -> Either Text EndingStep.EndingStep
jsonToEndingStep =
  decodeNullary
    (Text.pack "EndingStep")
    [ (Text.pack "EndStep", EndingStep.EndStep),
      (Text.pack "Cleanup", EndingStep.Cleanup)
    ]

phaseToJson :: Phase.Phase -> Value
phaseToJson p = case p of
  Phase.Beginning s -> Json.tagged (Text.pack "Beginning") (Just (beginningStepToJson s))
  Phase.PrecombatMain -> nullary (Text.pack "PrecombatMain")
  Phase.Combat s -> Json.tagged (Text.pack "Combat") (Just (combatStepToJson s))
  Phase.PostcombatMain -> nullary (Text.pack "PostcombatMain")
  Phase.Ending s -> Json.tagged (Text.pack "Ending") (Just (endingStepToJson s))

jsonToPhase :: Value -> Either Text Phase.Phase
jsonToPhase value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Beginning", Just v) -> Phase.Beginning <$> jsonToBeginningStep v
    ("PrecombatMain", _) -> Right Phase.PrecombatMain
    ("Combat", Just v) -> Phase.Combat <$> jsonToCombatStep v
    ("PostcombatMain", _) -> Right Phase.PostcombatMain
    ("Ending", Just v) -> Phase.Ending <$> jsonToEndingStep v
    _ -> Left (Text.pack "unknown Phase: " <> t)
```

And, in the records section (after `jsonToAffected`), the runtime-only payload types — `SetController`'s runtime-only `PlayerId` is the precedent for covering a type the card JSON never carries:

```haskell
recipientToJson :: Recipient.Recipient -> Value
recipientToJson r = case r of
  Recipient.ToCreature oid -> Json.tagged (Text.pack "ToCreature") (Just (objectIdToJson oid))
  Recipient.ToPlayer pid -> Json.tagged (Text.pack "ToPlayer") (Just (playerIdToJson pid))
  Recipient.ToObject oid -> Json.tagged (Text.pack "ToObject") (Just (objectIdToJson oid))

jsonToRecipient :: Value -> Either Text Recipient.Recipient
jsonToRecipient value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("ToCreature", Just v) -> Recipient.ToCreature <$> jsonToObjectId v
    ("ToPlayer", Just v) -> Recipient.ToPlayer <$> jsonToPlayerId v
    ("ToObject", Just v) -> Recipient.ToObject <$> jsonToObjectId v
    _ -> Left (Text.pack "unknown Recipient: " <> t)

damageKindToJson :: DamageKind.DamageKind -> Value
damageKindToJson k = nullary . Text.pack $ case k of
  DamageKind.Combat -> "Combat"
  DamageKind.Noncombat -> "Noncombat"

jsonToDamageKind :: Value -> Either Text DamageKind.DamageKind
jsonToDamageKind =
  decodeNullary
    (Text.pack "DamageKind")
    [ (Text.pack "Combat", DamageKind.Combat),
      (Text.pack "Noncombat", DamageKind.Noncombat)
    ]

damageEventToJson :: DamageEvent.DamageEvent -> Value
damageEventToJson ev =
  Object
    [ (Text.pack "source", objectIdToJson (DamageEvent.source ev)),
      (Text.pack "target", recipientToJson (DamageEvent.target ev)),
      (Text.pack "amount", natTo (DamageEvent.amount ev)),
      (Text.pack "dealtByDeathtouch", Json.jBool (DamageEvent.dealtByDeathtouch ev)),
      (Text.pack "kind", damageKindToJson (DamageEvent.kind ev))
    ]

jsonToDamageEvent :: Value -> Either Text DamageEvent.DamageEvent
jsonToDamageEvent value = do
  ps <- Json.asObject value
  s <- Json.field (Text.pack "source") ps >>= jsonToObjectId
  t <- Json.field (Text.pack "target") ps >>= jsonToRecipient
  a <- Json.field (Text.pack "amount") ps >>= natFrom
  d <- Json.field (Text.pack "dealtByDeathtouch") ps >>= jsonToBoolDefault False
  k <- Json.field (Text.pack "kind") ps >>= jsonToDamageKind
  pure
    DamageEvent.MkDamageEvent
      { DamageEvent.source = s,
        DamageEvent.target = t,
        DamageEvent.amount = a,
        DamageEvent.dealtByDeathtouch = d,
        DamageEvent.kind = k
      }

zoneChangeToJson :: ZoneChange.ZoneChange -> Value
zoneChangeToJson zc =
  Object
    [ (Text.pack "object", objectIdToJson (ZoneChange.object zc)),
      (Text.pack "from", zoneToJson (ZoneChange.from zc)),
      (Text.pack "to", zoneToJson (ZoneChange.to zc))
    ]

jsonToZoneChange :: Value -> Either Text ZoneChange.ZoneChange
jsonToZoneChange value = do
  ps <- Json.asObject value
  o <- Json.field (Text.pack "object") ps >>= jsonToObjectId
  f <- Json.field (Text.pack "from") ps >>= jsonToZone
  t <- Json.field (Text.pack "to") ps >>= jsonToZone
  pure (ZoneChange.MkZoneChange o f t)

projectedCharacteristicsToJson :: PC.ProjectedCharacteristics -> Value
projectedCharacteristicsToJson pc =
  Object
    [ (Text.pack "keywords", setTo keywordToJson (PC.keywords pc)),
      (Text.pack "colors", setTo colorToJson (PC.colors pc)),
      (Text.pack "power", maybeTo Json.jInt (PC.power pc)),
      (Text.pack "toughness", maybeTo Json.jInt (PC.toughness pc)),
      (Text.pack "characteristicPT", maybeTo (\(p, t) -> Array [quantityToJson p, quantityToJson t]) (PC.characteristicPT pc)),
      (Text.pack "cardTypes", setTo cardTypeToJson (PC.cardTypes pc)),
      (Text.pack "subtypes", setTo subtypeToJson (PC.subtypes pc)),
      (Text.pack "rulesTextActive", Json.jBool (PC.rulesTextActive pc)),
      (Text.pack "activatedAbilities", listTo activatedAbilityToJson (PC.activatedAbilities pc)),
      (Text.pack "replacementEffects", listTo replacementEffectToJson (PC.replacementEffects pc)),
      (Text.pack "triggeredAbilities", listTo triggeredAbilityToJson (PC.triggeredAbilities pc))
    ]

jsonToProjectedCharacteristics :: Value -> Either Text PC.ProjectedCharacteristics
jsonToProjectedCharacteristics value = do
  ps <- Json.asObject value
  kws <- Json.field (Text.pack "keywords") ps >>= setFrom jsonToKeyword
  cols <- Json.field (Text.pack "colors") ps >>= setFrom jsonToColor
  pow <- maybeFrom Json.asInteger (getOpt (Text.pack "power") ps)
  tou <- maybeFrom Json.asInteger (getOpt (Text.pack "toughness") ps)
  cda <- maybeFrom jsonToQuantityPair (getOpt (Text.pack "characteristicPT") ps)
  cts <- Json.field (Text.pack "cardTypes") ps >>= setFrom jsonToCardType
  subs <- Json.field (Text.pack "subtypes") ps >>= setFrom jsonToSubtype
  live <- Json.field (Text.pack "rulesTextActive") ps >>= jsonToBoolDefault True
  acts <- Json.field (Text.pack "activatedAbilities") ps >>= listFrom jsonToActivatedAbility
  reps <- Json.field (Text.pack "replacementEffects") ps >>= listFrom jsonToReplacementEffect
  trigs <- Json.field (Text.pack "triggeredAbilities") ps >>= listFrom jsonToTriggeredAbility
  pure
    PC.MkProjectedCharacteristics
      { PC.keywords = kws,
        PC.colors = cols,
        PC.power = pow,
        PC.toughness = tou,
        PC.characteristicPT = cda,
        PC.cardTypes = cts,
        PC.subtypes = subs,
        PC.rulesTextActive = live,
        PC.activatedAbilities = acts,
        PC.replacementEffects = reps,
        PC.triggeredAbilities = trigs
      }

jsonToQuantityPair :: Value -> Either Text (Quantity.Quantity, Quantity.Quantity)
jsonToQuantityPair value = case value of
  Array [p, t] -> do
    p_ <- jsonToQuantity p
    t_ <- jsonToQuantity t
    pure (p_, t_)
  _ -> Left (Text.pack "expected a [power, toughness] quantity pair")

gameEventToJson :: GameEvent.GameEvent -> Value
gameEventToJson e = case e of
  GameEvent.Moved zc pc -> Json.tagged (Text.pack "Moved") (Just (Array [zoneChangeToJson zc, projectedCharacteristicsToJson pc]))
  GameEvent.DamageDealt ev -> Json.tagged (Text.pack "DamageDealt") (Just (damageEventToJson ev))
  GameEvent.StepBegan p pid -> Json.tagged (Text.pack "StepBegan") (Just (Array [phaseToJson p, playerIdToJson pid]))

jsonToGameEvent :: Value -> Either Text GameEvent.GameEvent
jsonToGameEvent value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Moved", Just (Array [zc, pc])) -> GameEvent.Moved <$> jsonToZoneChange zc <*> jsonToProjectedCharacteristics pc
    ("DamageDealt", Just v) -> GameEvent.DamageDealt <$> jsonToDamageEvent v
    ("StepBegan", Just (Array [p, pid])) -> GameEvent.StepBegan <$> jsonToPhase p <*> jsonToPlayerId pid
    _ -> Left (Text.pack "unknown GameEvent: " <> t)
```

Add the imports these need to `Codec.hs`: `Pawl.Type.BeginningStep`, `Pawl.Type.CombatStep`, `Pawl.Type.DamageEvent`, `Pawl.Type.DamageKind`, `Pawl.Type.EndingStep`, `Pawl.Type.GameEvent`, `Pawl.Type.Phase`, `Pawl.Type.ProjectedCharacteristics` as `PC`, `Pawl.Type.Recipient`, `Pawl.Type.ZoneChange`.

Note `getOpt` and `jsonToBoolDefault` are declared further down the file today; move them up above this new block (or leave them — Haskell has no ordering requirement; keep them where they are).

- [x] **Step 8: Update the test suite's readers of the two removed fields**

In `source/test-suite/Pawl/Support.hs`, add to the `oneMountainState` record (replacing the `damageEvents`/`zoneChanges` lines):

```haskell
          GameState.events = Seq.empty,
          GameState.scannedThrough = 0,
          GameState.damageScannedThrough = 0,
```

and add these fixtures/readers near `markDamage`:

```haskell
-- The damage events recorded so far this turn, in order. Replaces the
-- GameState.damageEvents list P4 folded into the one turn-scoped log.
damageEventsOf :: GameState.GameState -> [DamageEvent.DamageEvent]
damageEventsOf gs = Maybe.mapMaybe Event.damageOf (Foldable.toList (GameState.events gs))

-- The zone changes recorded so far this turn, in order.
zoneChangesOf :: GameState.GameState -> [ZoneChange.ZoneChange]
zoneChangesOf gs = Maybe.mapMaybe Event.movedOf (Foldable.toList (GameState.events gs))

-- A state carrying exactly one UNSCANNED event -- the hand-built-event fixture
-- shape a scan test needs (EventSpec and ModalSpec both build one).
withEvent :: GameEvent.GameEvent -> GameState.GameState -> GameState.GameState
withEvent event gs =
  gs
    { GameState.events = Seq.singleton event,
      GameState.scannedThrough = 0,
      GameState.damageScannedThrough = 0
    }
```

adding `import qualified Data.Foldable as Foldable`, `import qualified Pawl.Event as Event`, `import qualified Pawl.Type.DamageEvent as DamageEvent`, `import qualified Pawl.Type.GameEvent as GameEvent`, `import qualified Pawl.Type.ZoneChange as ZoneChange`.

Then the call sites:

- `source/test-suite/Pawl/DamageSpec.hs:164` — `(GameState.damageEvents combat)` → `(S.damageEventsOf combat)`.
- `:201` — `events = GameState.damageEvents after` → `events = S.damageEventsOf after`.
- `:218` — `(GameState.damageEvents after)` → `(S.damageEventsOf after)`.
- `:241` — replace the whole test case with the watermark form:

```haskell
      HU.testCase "the SBA check consumes the damage events by watermark, not by draining" $
        let (gs, _, _) = S.combatBoardOf [Cards.typhoidRatsPrinting cards] [Cards.ogreSentryPrinting cards]
            after = Sba.checkStateBasedActions (S.fightWith S.aggressiveAnswer gs)
         in do
              HU.assertEqual "nothing left unscanned" [] (Event.unscannedDamage after)
              HU.assertBool "the record survives (CR 608.2i)" (not (null (S.damageEventsOf after))),
```

adding `import qualified Pawl.Event as Event` to `DamageSpec.hs` if absent.
- `:245` — the comment "so the wave is still in damageEvents" → "so the wave is still unscanned in the turn log".
- `:250` and `:261` — `(GameState.damageEvents fought)` → `(S.damageEventsOf fought)`.
- `source/test-suite/Pawl/ResolveSpec.hs:169`, `:191`, `:203` — `(GameState.damageEvents …)` → `(S.damageEventsOf …)`.
- `:634` — replace the `wounded` binding with:

```haskell
            wounded = S.withEvent (GameEvent.DamageDealt (DamageEvent.MkDamageEvent (ObjectId.MkObjectId 900) (Recipient.ToCreature myrId) 1 True DamageKind.Combat)) gs
```

adding `import qualified Pawl.Type.GameEvent as GameEvent`.
- `source/test-suite/Pawl/ModalSpec.hs:163`, `:272` — `(GameState.damageEvents …)` → `(S.damageEventsOf …)`.
- `:293` — `in (acId, gs1 {GameState.zoneChanges = [entered]})` → `in (acId, S.withEvent (GameEvent.Moved entered (Projection.project acId gs1)) gs1)`.
- `:369` — `gs2 = gs1 {GameState.zoneChanges = [entered]}` → `gs2 = S.withEvent (GameEvent.Moved entered (Projection.project smtId gs1)) gs1`.

`ModalSpec.hs` needs `import qualified Pawl.Projection as Projection` and `import qualified Pawl.Type.GameEvent as GameEvent` if absent.
- `source/test-suite/Pawl/EventSpec.hs:58` — `case GameState.zoneChanges after of` → `case S.zoneChangesOf after of`.
- `:133` — `(map ZoneChange.to (GameState.zoneChanges after))` → `(map ZoneChange.to (S.zoneChangesOf after))`.

- [x] **Step 9: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS, including every pre-existing test — this task changes no behaviour except the extra pre-handoff settle.

- [x] **Step 10: Commit**

```bash
git add source/library/Pawl/Type/GameEvent.hs source/library/Pawl/Type/GameState.hs source/library/Pawl/Event.hs source/library/Pawl/Damage.hs source/library/Pawl/Sba.hs source/library/Pawl/Engine.hs source/library/Pawl/Setup.hs source/library/Pawl/Codec.hs source/test-suite/Pawl/TriggerSpec.hs source/test-suite/Pawl/Support.hs source/test-suite/Pawl/CodecSpec.hs source/test-suite/Pawl/DamageSpec.hs source/test-suite/Pawl/ResolveSpec.hs source/test-suite/Pawl/ModalSpec.hs source/test-suite/Pawl/EventSpec.hs source/test-suite/Main.hs pawl.cabal
hooky fix && git add -u && hooky run
git commit -m "feat(m4.5-p4): one turn-scoped event log replaces the two drain queues"
```

---

### Task 2: Step beginnings, and the CR 603.6a widened scan

Two halves of one change. `TriggerCondition` grows `StepBegins Phase TurnScope`, and `Engine.runStep` emits a `StepBegan` event — but a step trigger belongs to a permanent that has nothing to do with any event, so the scan must widen from M3f's newcomer-only walk to CR 603.6a's "*all* permanents on the battlefield (including the newcomers) are checked". Widening forces `matchesTrigger` to learn who bears the ability, which is what makes `SelfEnters` stay self-scoped.

**Files:**
- Create: `source/library/Pawl/Type/TurnScope.hs`, `source/library/Pawl/Type/PendingTrigger.hs`
- Modify: `source/library/Pawl/Type/TriggerCondition.hs`
- Modify: `source/library/Pawl/Event.hs:228-250`
- Modify: `source/library/Pawl/Engine.hs` (`runStep`, `placePendingTriggers`, `placeOne`, `apnapOrder`)
- Modify: `source/library/Pawl/Codec.hs`
- Test: `source/test-suite/Pawl/TriggerSpec.hs`, `source/test-suite/Pawl/EventSpec.hs`, `source/test-suite/Pawl/CodecSpec.hs`, `source/test-suite/Pawl/Support.hs` (the new `emptyCharacteristics` fixture)

**Interfaces:**
- Consumes: `Pawl.Event.recordEvent`, `unscannedEvents`, `GameEvent.StepBegan` (Task 1).
- Produces: `Pawl.Type.TurnScope.TurnScope = EachTurn | ControllersTurn`; `Pawl.Type.PendingTrigger.PendingTrigger` with fields `source :: ObjectId`, `controller :: PlayerId`, `ability :: TriggeredAbility Card`, `bindings :: Map SlotName Binding`; `TriggerCondition.StepBegins Phase TurnScope`; `Pawl.Event.matchesTrigger :: ObjectId -> PlayerId -> TriggerCondition -> GameEvent -> Bool`; `Pawl.Event.eventTriggers :: [GameEvent] -> GameState -> [PendingTrigger]`; `Pawl.Event.gatherTriggers :: [GameEvent] -> GameState -> [PendingTrigger]`; `Pawl.Engine.placeOne :: PendingTrigger -> Game ()`; `Pawl.Engine.apnapOrder :: GameState -> [PendingTrigger] -> [PendingTrigger]`; `Pawl.Codec.turnScopeToJson` / `jsonToTurnScope`.

- [x] **Step 1: Write the failing tests**

Add to `source/test-suite/Pawl/TriggerSpec.hs` a new group, and add it to the `tests` list:

```haskell
-- CR 603.2b / 603.6a: a step begins, and EVERY permanent is checked.
scanTests :: Cards.Cards -> Tasty.TestTree
scanTests cards =
  Tasty.testGroup
    "Scan"
    [ HU.testCase "CR 603.2b running a step records that it began, on the active player's turn" $
        let gs = (Setup.emptyGame S.bothPlayers) {GameState.phase = Phase.Ending EndingStep.EndStep, GameState.activePlayer = S.alice}
            after = snd (Engine.runGamePure S.identityAnswer gs Engine.runStep)
            began ev = case ev of
              GameEvent.StepBegan p pid -> Just (p, pid)
              _ -> Nothing
         in HU.assertEqual
              "the end step's beginning is recorded"
              [(Phase.Ending EndingStep.EndStep, S.alice)]
              (take 1 (Maybe.mapMaybe began (Foldable.toList (GameState.events after)))),
      HU.testCase "CR 603.2b StepBegins matches its own step and no other" $
        let bearer = ObjectId.MkObjectId 1
            cond = TriggerCondition.StepBegins (Phase.Ending EndingStep.EndStep) TurnScope.EachTurn
         in do
              HU.assertBool "the end step matches" $
                Event.matchesTrigger bearer S.alice cond (GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice)
              HU.assertBool "the upkeep does not" $
                not (Event.matchesTrigger bearer S.alice cond (GameEvent.StepBegan (Phase.Beginning BeginningStep.Upkeep) S.alice)),
      -- CR 603.3a: "your upkeep" is the ABILITY CONTROLLER's, so the scope is
      -- read against the bearer's controller, not the card.
      HU.testCase "CR 603.3a ControllersTurn matches only the bearer's controller's turn" $
        let bearer = ObjectId.MkObjectId 1
            cond = TriggerCondition.StepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn
         in do
              HU.assertBool "alice's upkeep matches for alice" $
                Event.matchesTrigger bearer S.alice cond (GameEvent.StepBegan (Phase.Beginning BeginningStep.Upkeep) S.alice)
              HU.assertBool "bob's upkeep does not" $
                not (Event.matchesTrigger bearer S.alice cond (GameEvent.StepBegan (Phase.Beginning BeginningStep.Upkeep) S.bob)),
      -- The widening falsifier: the scan now visits every battlefield permanent,
      -- so SelfEnters must ask whether the event is about THIS permanent. Rest in
      -- Peace is on the battlefield and a DIFFERENT object entered.
      HU.testCase "CR 603.6a a SelfEnters trigger does not fire on another object's entry" $
        let (_, gs0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (piker, gs1) = S.addPiker cards S.bob gs0
            entered = ZoneChange.MkZoneChange piker Zone.Stack Zone.Battlefield
            gs2 = S.withEvent (GameEvent.Moved entered (Projection.project piker gs1)) gs1
         in HU.assertEqual "no trigger" 0 (length (Event.gatherTriggers (Event.unscannedEvents gs2) gs2)),
      HU.testCase "CR 603.6a a SelfEnters trigger still fires on its own entry" $
        let (ripId, gs0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            entered = ZoneChange.MkZoneChange ripId Zone.Stack Zone.Battlefield
            gs1 = S.withEvent (GameEvent.Moved entered (Projection.project ripId gs0)) gs0
         in case Event.gatherTriggers (Event.unscannedEvents gs1) gs1 of
              [pt] -> do
                HU.assertEqual "source is RiP" ripId (PendingTrigger.source pt)
                HU.assertEqual "controller is alice" S.alice (PendingTrigger.controller pt)
              other -> HU.assertFailure ("expected exactly one pending trigger, got " <> show (length other))
    ]
```

Add these imports to `TriggerSpec.hs`: `Data.Maybe` as `Maybe`, `Pawl.Type.BeginningStep` as `BeginningStep`, `Pawl.Type.EndingStep` as `EndingStep`, `Pawl.Type.PendingTrigger` as `PendingTrigger`, `Pawl.Type.Phase` as `Phase`, `Pawl.Type.TriggerCondition` as `TriggerCondition`, `Pawl.Type.TurnScope` as `TurnScope`.

Add to `CodecSpec.hs`'s "P4 runtime types" group:

```haskell
          HU.testCase "TurnScope round-trips" $
            mapM_ (roundTrip "scope" Codec.turnScopeToJson Codec.jsonToTurnScope) [TurnScope.EachTurn, TurnScope.ControllersTurn],
          HU.testCase "TriggerCondition.StepBegins round-trips" $
            roundTrip
              "cond"
              Codec.triggerConditionToJson
              Codec.jsonToTriggerCondition
              (TriggerCondition.StepBegins (Phase.Ending EndingStep.EndStep) TurnScope.EachTurn),
```

with `Pawl.Type.TriggerCondition` and `Pawl.Type.TurnScope` imported.

Three tests in `source/test-suite/Pawl/EventSpec.hs:85-100` call the two functions whose signatures change. They belong to the scan, which now lives in `TriggerSpec` — **port them, do not drop them.** Move all three out of `EventSpec.hs` and into `TriggerSpec.hs`'s `scanTests` group, rewritten against the new signatures:

- `"CR 603.6a: Rest in Peace entering yields its ETB trigger"` is now an exact duplicate of the new `"a SelfEnters trigger still fires on its own entry"` case above (same fixture, same assertions, stronger form). Drop **only** this one; its coverage is strictly retained.
- `"a graveyard-bound event yields no enters trigger"` becomes:

```haskell
      HU.testCase "a graveyard-bound event yields no enters trigger" $
        let (ripId, gs0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            toGrave = ZoneChange.MkZoneChange ripId Zone.Battlefield Zone.Graveyard
            gs1 = S.withEvent (GameEvent.Moved toGrave (Projection.project ripId gs0)) gs0
         in HU.assertEqual "no triggers" 0 (length (Event.gatherTriggers (Event.unscannedEvents gs1) gs1)),
```

- `"SelfEnters matches only a battlefield destination"` becomes:

```haskell
      HU.testCase "SelfEnters matches only a battlefield destination" $
        let bearer = ObjectId.MkObjectId 1
            movedTo zone = GameEvent.Moved (ZoneChange.MkZoneChange bearer Zone.Stack zone) S.emptyCharacteristics
         in do
              HU.assertBool "enters battlefield matches" $
                Event.matchesTrigger bearer S.alice TriggerCondition.SelfEnters (movedTo Zone.Battlefield)
              HU.assertBool "enters graveyard does not" $
                not (Event.matchesTrigger bearer S.alice TriggerCondition.SelfEnters (movedTo Zone.Graveyard)),
```

That second one needs a snapshot value for an object that does not exist. Add to `source/test-suite/Pawl/Support.hs`:

```haskell
-- The characteristics of nothing: what Projection.baseCharacteristics yields for
-- an id with no card. The filler snapshot for a hand-built GameEvent.Moved whose
-- payload no assertion reads.
emptyCharacteristics :: PC.ProjectedCharacteristics
emptyCharacteristics = Projection.project (ObjectId.MkObjectId 999) (Setup.emptyGame bothPlayers)
```

with `import qualified Pawl.Type.ProjectedCharacteristics as PC`.

Then delete the now-unused `TriggerCondition` / `ZoneChange` imports from `EventSpec.hs` if the compiler reports them unused.

- [x] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Pawl.Type.TurnScope` and `Pawl.Type.PendingTrigger` do not exist; `Event.matchesTrigger` has the wrong arity; `Event.gatherTriggers`, `Codec.turnScopeToJson` are not in scope.

- [x] **Step 3: Add the two new types and the condition arm**

Create `source/library/Pawl/Type/TurnScope.hs`:

```haskell
module Pawl.Type.TurnScope where

-- CR 603.2b / 603.3a: whose turn a step-beginning trigger watches. "At the
-- beginning of EACH end step" is EachTurn; "at the beginning of YOUR upkeep" is
-- ControllersTurn, relative to the ability's CONTROLLER (CR 603.3a), never the
-- card's owner.
--
-- A CR 603.7 delayed ability keyed to "the NEXT end step" is EachTurn: any
-- player's end step qualifies, and its once-ness comes from the delayed store
-- (CR 603.7b), never from the scope.
data TurnScope
  = EachTurn
  | ControllersTurn
  deriving (Eq, Ord, Show)
```

Create `source/library/Pawl/Type/PendingTrigger.hs`:

```haskell
module Pawl.Type.PendingTrigger where

import Data.Map.Strict (Map)
import Pawl.Type.Binding (Binding)
import Pawl.Type.Card (Card)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.TriggeredAbility (TriggeredAbility)

-- CR 603.3: an ability that has TRIGGERED but is not yet on the stack. Gathered
-- by Pawl.Event at the CR 117.5 boundary, ordered and placed by Pawl.Engine.
--
-- `source` is the object the ability belongs to (CR 113.7's source of the ability);
-- `controller` is who controls the ability (CR 603.3a). `bindings` is the
-- environment CAPTURED when a CR 603.7 delayed ability was armed -- how "it" and
-- "that card" (CR 603.7c) are remembered. Empty for an event- or state-matched
-- trigger, whose source binding Engine.placeOne stamps at placement instead.
data PendingTrigger = MkPendingTrigger
  { source :: ObjectId,
    controller :: PlayerId,
    ability :: TriggeredAbility Card,
    bindings :: Map SlotName Binding
  }
  deriving (Eq, Show)
```

In `source/library/Pawl/Type/TriggerCondition.hs`, replace the whole module body:

```haskell
module Pawl.Type.TriggerCondition where

import Pawl.Type.Phase (Phase)
import Pawl.Type.TurnScope (TurnScope)

-- CR 603.2: the pattern that fires a triggered ability. Only Pawl.Event may case
-- on it.
data TriggerCondition
  = -- CR 603.6a: "when this ... enters [the battlefield]" -- fires when the object
    -- BEARING the ability enters. Self-scoped: the scan checks every permanent
    -- (CR 603.6a), so the bearer's identity is part of the match, not an accident
    -- of which object the scan happened to visit. A general "whenever a [type]
    -- enters" is a future condition.
    SelfEnters
  | -- CR 603.2b: "at the beginning of [each|your] <step>". Matched against a
    -- GameEvent.StepBegan; the TurnScope decides whose turn qualifies.
    StepBegins Phase TurnScope
  deriving (Eq, Ord, Show)
```

(`StateIs` lands in Task 4 — do not add it yet.)

- [x] **Step 4: Widen the scan**

In `source/library/Pawl/Event.hs`, replace `matchesTrigger` and `triggersFrom` wholesale:

```haskell
-- CR 603.2: does this condition fire on this event, for the permanent that bears
-- it? `bearer` is the object whose ability this is and `you` is its controller
-- (CR 603.3a) -- both are part of the match, because the scan below visits EVERY
-- permanent, not only the one an event names. This module is the sole home of
-- casing on TriggerCondition.
matchesTrigger :: ObjectId -> PlayerId -> TriggerCondition -> GameEvent -> Bool
matchesTrigger bearer you cond event = case cond of
  -- CR 603.6a: the bearer's own object entered the battlefield.
  TriggerCondition.SelfEnters -> case event of
    GameEvent.Moved zc _ -> ZoneChange.object zc == bearer && ZoneChange.to zc == Zone.Battlefield
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan _ _ -> False
  -- CR 603.2b: this step began, on a turn the scope admits.
  TriggerCondition.StepBegins wanted scope -> case event of
    GameEvent.StepBegan began active ->
      began == wanted && case scope of
        TurnScope.EachTurn -> True
        TurnScope.ControllersTurn -> active == you
    GameEvent.Moved _ _ -> False
    GameEvent.DamageDealt _ -> False

-- CR 603.6a: "all permanents on the battlefield (including the newcomers) are
-- checked". This WIDENS M3f's scan, which only ever inspected the object an
-- enters event named: a step trigger belongs to a permanent that has nothing to
-- do with the event at all.
--
-- The battlefield is the ONLY scanned zone. An ability that functions from a
-- graveyard, hand or exile is a named deferral (the P4 spec, section 8), expiring
-- at the first such card.
--
-- Events outer, permanents inner (ascending by id): a deterministic canonical
-- order, which is what the CR 603.3b ordering prompt indexes into.
eventTriggers :: [GameEvent] -> GameState -> [PendingTrigger]
eventTriggers events gs =
  let permanents = Set.toAscList (GameState.battlefield gs)
      forOne event oid = case Projection.controllerOf oid gs of
        Nothing -> []
        Just ctrl ->
          let fires ab = matchesTrigger oid ctrl (TriggeredAbility.condition ab) event
              pend ab = PendingTrigger.MkPendingTrigger oid ctrl ab Map.empty
           in map pend (filter fires (Projection.triggeredAbilitiesOf oid gs))
   in concatMap (\event -> concatMap (forOne event) permanents) events

-- Everything that has triggered and is not yet on the stack. One function, so
-- Pawl.Engine never needs to know how many sources there are. Grows a state pass
-- (CR 603.8) at Task 4 and a delayed pass (CR 603.7) at Task 6.
gatherTriggers :: [GameEvent] -> GameState -> [PendingTrigger]
gatherTriggers events gs = eventTriggers events gs
```

Add to `Event.hs`'s imports: `Pawl.Type.PendingTrigger (PendingTrigger)` and `qualified … as PendingTrigger`, `qualified Pawl.Type.TurnScope as TurnScope`. `Data.Map.Strict`, `Data.Set`, `Pawl.Projection`, `Pawl.Type.TriggeredAbility` and `Pawl.Type.Zone` are already imported. `Pawl.Type.Card` and `Pawl.Type.TriggeredAbility (TriggeredAbility)` may become unused; remove any import the build reports unused.

- [x] **Step 5: Emit the step, and place a `PendingTrigger`**

In `source/library/Pawl/Engine.hs`:

**Comment-accuracy check this task owes (carried from Task 1's review).** `Pawl.Type.GameState`'s `events` comment says the log is appended "by `Engine.runStep`'s step-begin emission", and `Pawl.Type.GameEvent`'s `StepBegan` comment describes what a step trigger matches — both landed in Task 1 but were false until this task. **After implementing, re-read both comments and confirm they are now accurate.** Report that you did.

`runStep` emits before its turn-based actions, so a beginning-of-step trigger is in the log by the time the step's first CR 117.5 boundary scans it:

```haskell
runStep :: Game ()
runStep = do
  phase <- State.gets GameState.phase
  -- CR 603.2b: the step began. Recorded BEFORE the step's turn-based actions, so
  -- the first priority boundary of this step scans it. The untap step grants no
  -- priority (CR 502.4), so its event waits until upkeep -- which is exactly what
  -- CR 502.4 says happens to an ability that triggers during untap ("held until
  -- the next time a player would receive priority"), and CR 503.1a is where those
  -- held triggers go on the stack.
  State.modify' (\gs -> Event.recordEvent (GameEvent.StepBegan phase (GameState.activePlayer gs)) gs)
  runTurnBasedActions phase
  checkSba
  finished <- State.gets (Maybe.isJust . GameState.result)
  Monad.unless finished $ do
    Monad.when (Turn.grantsPriority phase) priorityLoop
    State.modify' Mana.emptyManaPools
    checkSba
    stillFinished <- State.gets (Maybe.isJust . GameState.result)
    Monad.unless stillFinished advance
```

`placePendingTriggers` now feeds whole events to the gather:

```haskell
placePendingTriggers :: Game Bool
placePendingTriggers = do
  gs <- State.get
  let pending = Event.gatherTriggers (Event.unscannedEvents gs) gs
  State.modify' (\g -> g {GameState.scannedThrough = fromIntegral (Seq.length (GameState.events g))})
  Monad.mapM_ placeOne (apnapOrder gs pending)
  pure (not (null pending))
```

`placeOne` takes the record. Change its signature and its first lines; the rest of the body is unchanged apart from the three renamed bindings:

```haskell
placeOne :: PendingTrigger.PendingTrigger -> Game ()
placeOne pending = do
  gs <- State.get
  let srcId = PendingTrigger.source pending
      controller = PendingTrigger.controller pending
      ability = PendingTrigger.ability pending
      (abilId, gs1) = Game.freshObjectId gs
      …
```

and `apnapOrder`:

```haskell
-- CR 603.3b: active player's triggers first, then the others. Stable within a
-- controller; the within-controller ORDER becomes that player's choice at Task 7.
apnapOrder :: GameState -> [PendingTrigger.PendingTrigger] -> [PendingTrigger.PendingTrigger]
apnapOrder gs pend =
  let active = GameState.activePlayer gs
      mine pt = PendingTrigger.controller pt == active
   in filter mine pend ++ filter (not . mine) pend
```

Add `import qualified Pawl.Type.GameEvent as GameEvent` and `import qualified Pawl.Type.PendingTrigger as PendingTrigger` to `Engine.hs`.

- [x] **Step 6: Add the codec arms**

In `source/library/Pawl/Codec.hs`, add next to the other leaf enums:

```haskell
turnScopeToJson :: TurnScope.TurnScope -> Value
turnScopeToJson s = nullary . Text.pack $ case s of
  TurnScope.EachTurn -> "EachTurn"
  TurnScope.ControllersTurn -> "ControllersTurn"

jsonToTurnScope :: Value -> Either Text TurnScope.TurnScope
jsonToTurnScope =
  decodeNullary
    (Text.pack "TurnScope")
    [ (Text.pack "EachTurn", TurnScope.EachTurn),
      (Text.pack "ControllersTurn", TurnScope.ControllersTurn)
    ]
```

and replace `triggerConditionToJson`/`jsonToTriggerCondition`:

```haskell
triggerConditionToJson :: TriggerCondition.TriggerCondition -> Value
triggerConditionToJson c = case c of
  TriggerCondition.SelfEnters -> nullary (Text.pack "SelfEnters")
  TriggerCondition.StepBegins p s -> Json.tagged (Text.pack "StepBegins") (Just (Array [phaseToJson p, turnScopeToJson s]))

jsonToTriggerCondition :: Value -> Either Text TriggerCondition.TriggerCondition
jsonToTriggerCondition value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("SelfEnters", _) -> Right TriggerCondition.SelfEnters
    ("StepBegins", Just (Array [p, s])) -> TriggerCondition.StepBegins <$> jsonToPhase p <*> jsonToTurnScope s
    _ -> Left (Text.pack "unknown TriggerCondition: " <> t)
```

adding `import qualified Pawl.Type.TurnScope as TurnScope`.

- [x] **Step 7: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS. The existing Rest in Peace whole-card test (`EventSpec.hs:101`) and the Aether Channeler trigger tests (`ModalSpec.hs`) are the regression that the widened scan did not change enters-trigger behaviour.

- [x] **Step 8: Commit**

```bash
git add source/library/Pawl/Type/TurnScope.hs source/library/Pawl/Type/PendingTrigger.hs source/library/Pawl/Type/TriggerCondition.hs source/library/Pawl/Event.hs source/library/Pawl/Engine.hs source/library/Pawl/Codec.hs source/test-suite/Pawl/TriggerSpec.hs source/test-suite/Pawl/EventSpec.hs source/test-suite/Pawl/CodecSpec.hs source/test-suite/Pawl/Support.hs pawl.cabal
hooky fix && git add -u && hooky run
git commit -m "feat(m4.5-p4): step-beginning events and the CR 603.6a widened trigger scan"
```

---

### Task 3: The `Sacrifice` opcode and the reserved trigger-source slot

CR 701.21's keyword action, plus the mechanism that makes "this creature" expressible: the trigger's source object is bound into a reserved slot as the ability is placed, so `Sacrifice` needs no self-referential variant. This also fixes a latent hazard the reserved slot exposes — CR 608.2b's re-validation is about **targets**, and a slot with no target spec was never targeted, so it must not be judged illegal.

**Files:**
- Modify: `source/library/Pawl/Type/Effect.hs`
- Modify: `source/library/Pawl/Binding.hs`
- Modify: `source/library/Pawl/Event.hs`
- Modify: `source/library/Pawl/Resolve.hs` (`slotsOf`, `readsX`, `manaProduced`, `searchesLibrary`, `rewriteEffect`, `resolveSpell`, `resolveEffects`, `applyEffect`)
- Modify: `source/library/Pawl/Engine.hs` (`placeOne`)
- Modify: `source/library/Pawl/Codec.hs`
- Test: `source/test-suite/Pawl/TriggerSpec.hs`, `source/test-suite/Pawl/CardSpec.hs`, `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Consumes: `PendingTrigger` and `Engine.placeOne` (Task 2).
- Produces: `Effect.Sacrifice SlotName`; `Pawl.Binding.triggerSource :: SlotName` (the text `"self"`), `Pawl.Binding.setTriggerSource :: ObjectId -> Map SlotName Binding -> Map SlotName Binding`; `Pawl.Event.sacrifice :: ObjectId -> GameState -> GameState`.

- [x] **Step 1: Write the failing tests**

Add to `source/test-suite/Pawl/TriggerSpec.hs` (and to the `tests` list):

```haskell
-- CR 701.21: sacrificing is its own keyword action -- NOT a destruction.
sacrificeTests :: Cards.Cards -> Tasty.TestTree
sacrificeTests cards =
  Tasty.testGroup
    "Sacrifice"
    [ HU.testCase "CR 701.21a a sacrificed permanent goes to its owner's graveyard" $
        let (piker, gs) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
            after = Event.sacrifice piker gs
         in do
              HU.assertEqual "off the battlefield" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "in bob's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      -- CR 701.21a: "sacrificing a permanent doesn't destroy it", so neither CR
      -- 700.4's indestructible gate nor CR 701.19a's shield applies.
      HU.testCase "CR 701.21a an indestructible permanent can still be sacrificed" $
        let (myr, gs) = S.addCreature (Cards.darksteelMyrPrinting cards) S.bob (Setup.emptyGame S.bothPlayers)
            after = Event.sacrifice myr gs
         in HU.assertEqual "gone from the battlefield" 0 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 701.21a sacrificing neither consults nor consumes a regeneration shield" $
        let (piker, gs0) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
            gs = S.addRegenShield piker gs0
            after = Event.sacrifice piker gs
         in do
              HU.assertEqual "still sacrificed" 0 (S.creaturesInPlay S.bob after)
              HU.assertEqual "the shield is untouched" (Just 1) (Map.lookup piker (GameState.regenerationShields after)),
      HU.testCase "only a battlefield permanent can be sacrificed (CR 701.21a)" $
        let (card, gs) = S.addLibraryCard (Cards.pikerPrinting cards) S.bob (Setup.emptyGame S.bothPlayers)
            after = Event.sacrifice card gs
         in HU.assertEqual "the library card is untouched" gs after,
      -- CR 113.7 / 603.7c: "this creature" is a slot read, filled at placement.
      HU.testCase "CR 113.7 a placed trigger binds its source into the reserved self slot" $
        let (ripId, gs0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            entered = ZoneChange.MkZoneChange ripId Zone.Stack Zone.Battlefield
            gs1 = S.withEvent (GameEvent.Moved entered (Projection.project ripId gs0)) gs0
            placed = snd (Engine.runGamePure S.identityAnswer gs1 Engine.placePendingTriggers)
            bindingsOn oid = maybe Map.empty Object.bindings (Game.lookupObject oid placed)
            selfOf oid = Map.lookup Binding.triggerSource (Binding.targetsOf (bindingsOn oid))
         in HU.assertEqual
              "the trigger names its source"
              [Just (Recipient.ToObject ripId)]
              (map selfOf (GameState.stack placed))
    ]
```

Add imports to `TriggerSpec.hs`: `Data.Map.Strict` as `Map`, `Pawl.Binding` as `Binding`, `Pawl.Type.Recipient` as `Recipient`.

Add to `source/test-suite/Pawl/CardSpec.hs`'s `lintTests` list:

```haskell
      HU.testCase "the reserved trigger-source slot is never a declared target slot" $
        let offenders =
              filter
                (Map.member Binding.triggerSource . Card.allTargetSpecs . Printing.card)
                (Cards.allPrintings cards)
         in HU.assertEqual "no card names the self slot" [] (map (Card.Type.name . Printing.card) offenders),
```

Add to `CodecSpec.hs`'s "effect" group:

```haskell
          HU.testCase "Sacrifice round-trips" $
            roundTrip "e5" Codec.effectToJson Codec.jsonToEffect (Effect.Sacrifice (SlotName.MkSlotName (Text.pack "self"))),
```

- [x] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Event.sacrifice`, `Binding.triggerSource` and `Effect.Sacrifice` are not in scope.

- [x] **Step 3: Add the opcode, the funnel, and the reserved slot**

In `source/library/Pawl/Type/Effect.hs`, add a constructor (after `Destroy`, whose comment it contrasts with):

```haskell
  | -- CR 701.21/701.21a: the slot's target permanent is sacrificed -- its
    -- CONTROLLER moves it to its OWNER's graveyard. NOT a destruction: CR 701.21a
    -- says so explicitly, so this consults neither indestructible (CR 702.12b) nor a
    -- regeneration shield (CR 701.19a), and is therefore not a reuse of Destroy.
    --
    -- One opcode, not a targetless SacrificeSelf plus a slotted variant on
    -- RegenerateSelf's precedent: "this creature" is expressible because
    -- Engine.placeOne binds the trigger's SOURCE into the reserved
    -- Pawl.Binding.triggerSource slot, and "this creature" recurs far too often to
    -- pay for a second opcode.
    Sacrifice SlotName
```

In `source/library/Pawl/Binding.hs`, add next to `copySource`:

```haskell
-- CR 113.7 / 603.7c: the reserved slot under which a triggered ability's SOURCE
-- object is bound as the ability is placed, so "this creature" / "this
-- enchantment" is a slot read rather than a self-referential opcode. No card's
-- targetSpecs may name it (the D4 lint enforces this): a source is not a target.
triggerSource :: SlotName
triggerSource = SlotName.MkSlotName (Text.pack "self")

-- Bind an object under the reserved triggerSource slot. A dedicated
-- single-purpose slot, so this insert never clobbers another binding (setCopy's
-- posture).
setTriggerSource :: ObjectId -> Map SlotName Binding -> Map SlotName Binding
setTriggerSource oid =
  Map.insert triggerSource (Binding.empty {Binding.target = Just (Recipient.ToObject oid)})
```

adding `import Pawl.Type.ObjectId (ObjectId)` and `import qualified Pawl.Type.Recipient as Recipient` (the unqualified `Recipient` type import is already there).

In `source/library/Pawl/Event.hs`, add beside `destroy` and `counter`:

```haskell
-- CR 701.21/701.21a: the single sacrifice funnel. The permanent is put into its
-- OWNER's graveyard through changeZone (so Rest in Peace's redirect and a token's
-- CR 704.5d cease-to-exist still compose), and -- unlike Event.destroy -- with no
-- indestructible gate (CR 702.12b) and no regeneration shield consulted (CR
-- 701.19a): CR 701.21a says sacrificing is not destroying. CR 701.21a also
-- restricts it to permanents on the battlefield, so anything else is a no-op.
sacrifice :: ObjectId -> GameState -> GameState
sacrifice oid gs = case Game.lookupObject oid gs of
  Nothing -> gs
  Just obj ->
    if Object.zone obj == Zone.Battlefield
      then changeZone oid Zone.Graveyard gs
      else gs
```

- [x] **Step 4: Teach `Resolve` the opcode, and exempt reserved slots from CR 608.2b**

In `source/library/Pawl/Resolve.hs`, add the `Sacrifice` arm to all five classification functions, in each case beside `Destroy`:

- `slotsOf`: `Effect.Sacrifice slot -> Set.singleton slot`
- `readsX`: `Effect.Sacrifice _ -> False`
- `manaProduced`: `Effect.Sacrifice _ -> Nothing`
- `searchesLibrary`: `Effect.Sacrifice _ -> False`
- `rewriteEffect`: `Effect.Sacrifice _ -> effect`

Add the executor arm to `applyEffect`, after `Effect.Destroy`:

```haskell
  Effect.Sacrifice slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs -- a player recipient cannot be sacrificed
          -- CR 701.21: through the single funnel, which is NOT Event.destroy --
          -- CR 701.21a: sacrificing is not destroying.
          Just target -> Event.sacrifice target gs
        _ -> gs
```

Then change the legality computation in **both** `resolveSpell` and `resolveEffects`. Today an absent spec makes a slot illegal, which would silently no-op every reserved-slot read. Replace each `legalSlot` and `fizzles` pair with:

```haskell
            legalSlot slot recipient = case Map.lookup slot specs of
              -- CR 608.2b is about TARGETS. A slot with no target spec is a
              -- RESERVED binding -- the trigger's source (Pawl.Binding.triggerSource),
              -- a token this resolution minted -- and was never targeted, so it can
              -- never have become an illegal target.
              Nothing -> True
              Just spec -> Target.stillLegal recipient spec gs
            legality = Map.mapWithKey legalSlot chosen
            -- CR 608.2b's fizzle asks about the TARGETED slots only, so the
            -- reserved slots above cannot rescue a spell whose every target is gone.
            targeted = Map.restrictKeys legality (Map.keysSet specs)
            fizzles = not (Map.null specs) && not (or (Map.elems targeted))
```

(In `resolveSpell` these are `let` bindings inside the existing `let … in if fizzles`; in `resolveEffects` they are inside the existing `let … in do`. Keep each in place, only replacing the three bindings.)

- [x] **Step 5: Stamp the reserved slot at placement**

In `source/library/Pawl/Engine.hs`, change `placeOne`'s final `State.modify'` so the source binding rides alongside the chosen targets:

```haskell
      -- CR 113.7 / 603.7c: the ability's SOURCE is bound under the reserved slot
      -- as it is placed, so "this creature" resolves as an ordinary slot read even
      -- after the source has left the battlefield.
      State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.setTriggerSource srcId (Binding.fromChoices chosen Map.empty Nothing chosenModes)}) abilId (GameState.objects g)})
```

- [x] **Step 6: Add the codec arm**

In `source/library/Pawl/Codec.hs`, add to `effectToJson` (beside `Destroy`):

```haskell
  Effect.Sacrifice s -> Json.tagged (Text.pack "Sacrifice") (Just (slotNameToJson s))
```

and to `jsonToEffect`:

```haskell
    "Sacrifice" -> withValue mv (fmap Effect.Sacrifice . jsonToSlotName)
```

- [x] **Step 7: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS. The Rest in Peace whole-card test in `EventSpec.hs` is the regression for the CR 608.2b change: its ETB has no target specs and must still resolve.

- [x] **Step 8: Commit**

```bash
git add source/library/Pawl/Type/Effect.hs source/library/Pawl/Binding.hs source/library/Pawl/Event.hs source/library/Pawl/Resolve.hs source/library/Pawl/Engine.hs source/library/Pawl/Codec.hs source/test-suite/Pawl/TriggerSpec.hs source/test-suite/Pawl/CardSpec.hs source/test-suite/Pawl/CodecSpec.hs
hooky fix && git add -u && hooky run
git commit -m "feat(m4.5-p4): the Sacrifice opcode and the reserved trigger-source slot"
```

---

### Task 4: State triggers → **Barbarian Outcast**

CR 603.8's trigger that is not an event: a condition over game *state*, checked at every CR 117.5 boundary, whose armedness is **derived** — an instance already on the stack suppresses a new one — so there is no bookkeeping field to leak. The sharpest falsifier in the phase is the flooding one: with no Swamps in play, exactly **one** trigger reaches the stack, not one per priority boundary.

**Files:**
- Create: `source/library/Pawl/Type/StateCondition.hs`, `data/cards/barbarian-outcast.json`
- Modify: `source/library/Pawl/Type/TriggerCondition.hs`, `source/library/Pawl/Type/Subtype.hs`
- Modify: `source/library/Pawl/Event.hs`
- Modify: `source/library/Pawl/Codec.hs`
- Test: `source/test-suite/Pawl/TriggerSpec.hs`, `source/test-suite/Pawl/Cards.hs`, `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Consumes: `Event.gatherTriggers`, `PendingTrigger` (Task 2); `Effect.Sacrifice`, `Binding.triggerSource` (Task 3).
- Produces: `Pawl.Type.StateCondition.StateCondition = YouControlNo Subtype | NoPermanentsOfSubtype Subtype`; `TriggerCondition.StateIs StateCondition`; `Pawl.Event.stateHolds :: PlayerId -> StateCondition -> GameState -> Bool`; `Pawl.Event.stateTriggers :: GameState -> [PendingTrigger]`; `Subtype.Barbarian`; `Pawl.Cards.barbarianOutcastPrinting`; `Pawl.Codec.stateConditionToJson` / `jsonToStateCondition`.

- [x] **Step 1: Write the failing tests**

Add to `source/test-suite/Pawl/TriggerSpec.hs` (and to the `tests` list):

```haskell
-- Barbarian Outcast {1}{R} Creature -- Human Barbarian Beast 2/2:
-- "When you control no Swamps, sacrifice this creature." CR 603.8's own example
-- shape ("a player controlling no permanents of a particular card type"), chosen
-- by the rulebook to illustrate the rule.
stateTriggerTests :: Cards.Cards -> Tasty.TestTree
stateTriggerTests cards =
  let outcastBoard swamps =
        let (oid, gs) = S.addCreature (Cards.barbarianOutcastPrinting cards) S.alice (S.landsInPlay (Cards.swampPrinting cards) swamps)
         in (oid, gs)
      triggerIds gs = filter (isTriggerObject gs) (GameState.stack gs)
      isTriggerObject gs oid = case Game.lookupObject oid gs of
        Just obj -> case Object.source obj of
          Source.OfTrigger _ _ -> True
          _ -> False
        Nothing -> False
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
   in Tasty.testGroup
        "StateTrigger"
        [ -- THE flooding falsifier. CR 603.8's second sentence exists to prevent
          -- exactly this: a state trigger that re-fires at every boundary.
          HU.testCase "CR 603.8 a true state condition puts EXACTLY ONE instance on the stack" $
            let (_, gs) = outcastBoard 0
                settled = settle gs
             in HU.assertEqual "one trigger, not one per boundary" 1 (length (triggerIds settled)),
          HU.testCase "CR 603.8 re-settling while the instance is on the stack adds no second copy" $
            let (_, gs) = outcastBoard 0
                twice = settle (settle gs)
             in HU.assertEqual "still exactly one" 1 (length (triggerIds twice)),
          HU.testCase "CR 603.8 the condition being FALSE means no trigger at all" $
            let (_, gs) = outcastBoard 1
                settled = settle gs
             in HU.assertEqual "no trigger while a Swamp is out" 0 (length (triggerIds settled)),
          HU.testCase "CR 603.8 losing the last Swamp makes the condition true and fires it" $
            let (_, gs) = outcastBoard 1
                quiet = settle gs
                swamp = case Game.zoneMembers Zone.Battlefield S.alice quiet of
                  ids -> case filter (\oid -> Set.member Subtype.Swamp (Projection.subtypesOf oid quiet)) ids of
                    s : _ -> s
                    [] -> ObjectId.MkObjectId 999
                gone = settle (Event.destroy swamp quiet)
             in HU.assertEqual "the Swamp's death arms it" 1 (length (triggerIds gone)),
          -- CR 603.8: "doesn't trigger again until the ability has resolved, has
          -- been countered, or has otherwise left the stack" -- all three are
          -- "no longer on the stack", which is why armedness is derived.
          HU.testCase "CR 603.8 an instance leaving the stack re-arms the trigger" $
            let (_, gs) = outcastBoard 0
                settled = settle gs
                removed = case triggerIds settled of
                  abilId : _ -> Resolve.cease abilId settled
                  [] -> settled
                again = settle removed
             in HU.assertEqual "a fresh instance" 1 (length (triggerIds again)),
          -- The whole card, at gameplay level: the trigger resolves and the
          -- Outcast sacrifices itself (CR 701.21a, through Event.sacrifice).
          HU.testCase "CR 701.21 the resolved trigger sacrifices the Outcast into its owner's graveyard" $
            let (outcast, gs) = outcastBoard 0
                settled = settle gs
                resolved = snd (Engine.runGamePure S.identityAnswer settled Stack.resolveTop)
             in do
                  HU.assertBool "the Outcast is off the battlefield" (not (Set.member outcast (GameState.battlefield resolved)))
                  HU.assertEqual "and in alice's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice resolved))
        ]
```

Add imports to `TriggerSpec.hs`: `Pawl.Resolve` as `Resolve`, `Pawl.Stack` as `Stack`, `Pawl.Type.Subtype` as `Subtype`.

Add to `CodecSpec.hs`'s "P4 runtime types" group:

```haskell
          HU.testCase "StateCondition round-trips" $
            mapM_
              (roundTrip "state" Codec.stateConditionToJson Codec.jsonToStateCondition)
              [StateCondition.YouControlNo Subtype.Swamp, StateCondition.NoPermanentsOfSubtype Subtype.Zombie],
          HU.testCase "TriggerCondition.StateIs round-trips" $
            roundTrip
              "cond"
              Codec.triggerConditionToJson
              Codec.jsonToTriggerCondition
              (TriggerCondition.StateIs (StateCondition.YouControlNo Subtype.Swamp)),
```

with `Pawl.Type.StateCondition` imported. (`Subtype.Zombie` lands in Task 5; for this task use `Subtype.Swamp` in both entries and switch the second to `Zombie` when Task 5 adds it.)

- [x] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Pawl.Type.StateCondition` does not exist; `Cards.barbarianOutcastPrinting`, `Event.stateTriggers`, `Codec.stateConditionToJson` are not in scope.

- [x] **Step 3: Add the state-condition classification**

Create `source/library/Pawl/Type/StateCondition.hs`:

```haskell
module Pawl.Type.StateCondition where

import Pawl.Type.Subtype (Subtype)

-- CR 603.8 / 603.4: a predicate over game STATE rather than over an event. Two
-- customers, one vocabulary: a state TRIGGER's condition (CR 603.8, checked at
-- every CR 117.5 boundary) and an INTERVENING "if" (CR 603.4 when the trigger
-- event occurs, CR 608.2a again on resolution).
--
-- Hand-carved, one variant per card -- the TargetSpec.WallTarget and CountSpec
-- posture, specific before general. Only Pawl.Event may case on it, and it reads
-- the PROJECTION: a subtype is CR 613 layer 4 and control is layer 2, so a card
-- that changed either must change the answer.
--
-- EXPIRES at P9, whose criterion/filter language retires this type wholesale.
data StateCondition
  = -- CR 603.8: Barbarian Outcast, "you control no Swamps". Scoped to the
    -- ABILITY's controller (CR 603.3a), not to the board.
    YouControlNo Subtype
  | -- CR 603.4: Sarcomancy, "if there are no Zombies on the battlefield". ANY
    -- player's permanents -- deliberately distinct from YouControlNo.
    NoPermanentsOfSubtype Subtype
  deriving (Eq, Ord, Show)
```

In `source/library/Pawl/Type/TriggerCondition.hs`, add the arm and the import:

```haskell
  | -- CR 603.8: a STATE trigger -- it fires whenever its condition is true, not
    -- when an event occurs. "It doesn't trigger again until the ability has
    -- resolved, has been countered, or has otherwise left the stack", which is why
    -- Pawl.Event derives armedness from the stack rather than storing it.
    StateIs StateCondition
```

In `source/library/Pawl/Type/Subtype.hs`, append after `Arcane` (appending keeps every existing card file's subtype ordering byte-identical):

```haskell
  | Barbarian -- CR 205.3m (a creature type; Barbarian Outcast's)
```

- [x] **Step 4: Evaluate the condition and gather state triggers**

In `source/library/Pawl/Event.hs`, add the `StateIs` arm to `matchesTrigger`:

```haskell
  -- CR 603.8: a state trigger is not an event trigger. It never matches an entry
  -- in the log; stateTriggers below is its whole story.
  TriggerCondition.StateIs _ -> False
```

and add the evaluator plus the pass:

```haskell
-- CR 603.8 / 603.4: is this state condition currently true, for an ability whose
-- controller is `you`? Reads the PROJECTION -- a subtype is CR 613 layer 4 and
-- control is layer 2, so Blood Moon and Act of Treason both change the answer.
-- This module is the sole home of casing on StateCondition.
stateHolds :: PlayerId -> StateCondition -> GameState -> Bool
stateHolds you cond gs =
  let hasSubtype subtype oid = Set.member subtype (Projection.subtypesOf oid gs)
   in case cond of
        -- CR 109.5: "you" on a triggered ability is the controller of the object
        -- when the ability triggered; CR 110.2 gives every permanent a controller.
        StateCondition.YouControlNo subtype -> not (any (hasSubtype subtype) (Projection.controls you gs))
        -- Any player's -- the whole battlefield.
        StateCondition.NoPermanentsOfSubtype subtype -> not (any (hasSubtype subtype) (Set.toList (GameState.battlefield gs)))

-- CR 603.8: state triggers. Every battlefield permanent whose StateIs condition
-- is currently TRUE and which has no instance already on the stack.
--
-- Armedness is DERIVED, never stored. CR 603.8's second sentence -- "doesn't
-- trigger again until the ability has resolved, has been countered, or has
-- otherwise left the stack" -- names three outcomes that are all "no longer on
-- the stack", so an instance sitting there is the whole suppression rule and
-- there is no bookkeeping field to leak. There is no triggered-but-not-yet-placed
-- window to worry about: Engine.placePendingTriggers puts them on the stack
-- within the same settle step.
--
-- A trigger whose modes are all unfillable would be removed from the stack (CR
-- 603.3c) and re-trigger on the next settle pass while its condition held, which
-- would not terminate. No card in the pool can do that -- Barbarian Outcast's
-- single mode has no target slots and is always fillable -- and the first card
-- that could is the one that must revisit this.
stateTriggers :: GameState -> [PendingTrigger]
stateTriggers gs =
  let alreadyOnStack srcId ab =
        let isInstance sid = case Game.lookupObject sid gs of
              Nothing -> False
              Just obj -> Object.source obj == Source.OfTrigger srcId ab
         in any isInstance (GameState.stack gs)
      forOne oid = case Projection.controllerOf oid gs of
        Nothing -> []
        Just ctrl ->
          let live ab = case TriggeredAbility.condition ab of
                TriggerCondition.StateIs cond -> stateHolds ctrl cond gs && not (alreadyOnStack oid ab)
                TriggerCondition.SelfEnters -> False
                TriggerCondition.StepBegins _ _ -> False
              pend ab = PendingTrigger.MkPendingTrigger oid ctrl ab Map.empty
           in map pend (filter live (Projection.triggeredAbilitiesOf oid gs))
   in concatMap forOne (Set.toAscList (GameState.battlefield gs))
```

and widen `gatherTriggers`:

```haskell
gatherTriggers :: [GameEvent] -> GameState -> [PendingTrigger]
gatherTriggers events gs = eventTriggers events gs ++ stateTriggers gs
```

Add `import Pawl.Type.StateCondition (StateCondition)` and `import qualified Pawl.Type.StateCondition as StateCondition` to `Event.hs`. `Pawl.Type.Source` is already imported.

- [x] **Step 5: Add the codec arms and the card**

In `source/library/Pawl/Codec.hs`:

```haskell
stateConditionToJson :: StateCondition.StateCondition -> Value
stateConditionToJson c = case c of
  StateCondition.YouControlNo s -> Json.tagged (Text.pack "YouControlNo") (Just (subtypeToJson s))
  StateCondition.NoPermanentsOfSubtype s -> Json.tagged (Text.pack "NoPermanentsOfSubtype") (Just (subtypeToJson s))

jsonToStateCondition :: Value -> Either Text StateCondition.StateCondition
jsonToStateCondition value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("YouControlNo", Just v) -> StateCondition.YouControlNo <$> jsonToSubtype v
    ("NoPermanentsOfSubtype", Just v) -> StateCondition.NoPermanentsOfSubtype <$> jsonToSubtype v
    _ -> Left (Text.pack "unknown StateCondition: " <> t)
```

plus the `StateIs` arms in `triggerConditionToJson` / `jsonToTriggerCondition`:

```haskell
  TriggerCondition.StateIs c -> Json.tagged (Text.pack "StateIs") (Just (stateConditionToJson c))
```
```haskell
    ("StateIs", Just v) -> TriggerCondition.StateIs <$> jsonToStateCondition v
```

and the `Barbarian` entries in `subtypeToJson` / `jsonToSubtype`. Add `import qualified Pawl.Type.StateCondition as StateCondition`.

Create `data/cards/barbarian-outcast.json` — one line, exactly this, plus the trailing newline the repo's file-hygiene rule mandates:

```json
{"name":"Barbarian Outcast","manaCost":[{"type":"Generic","value":1},{"type":"OfType","value":{"type":"Colored","value":{"type":"Red"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Creature"}],"subtypes":[{"type":"Human"},{"type":"Beast"},{"type":"Barbarian"}]},"power":{"type":"Literal","value":2},"toughness":{"type":"Literal","value":2},"keywords":[],"staticAbilities":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[{"condition":{"type":"StateIs","value":{"type":"YouControlNo","value":{"type":"Swamp"}}},"modal":{"modes":[{"effects":[{"type":"Sacrifice","value":"self"}],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}}}],"castingPermissions":[]}
```

The subtype array order is `Set.toAscList` order, i.e. the `Pawl.Type.Subtype` declaration order — `Human`, then `Beast`, then the newly appended `Barbarian`. If `Pawl.CardsSpec`'s byte-stability assertion reports a mismatch, the render is authoritative: the fix is to make the file match `Json.render (Codec.printingToJson p)`, never to weaken the assertion.

In `source/test-suite/Pawl/Cards.hs`, add `barbarianOutcastPrinting :: Printing.Printing` to the record, `barbarianOutcastPrinting_ <- loadPrinting "barbarian-outcast"` to `loadCards`, the field to the returned `MkCards`, and `barbarianOutcastPrinting cards,` to `allPrintings`.

- [x] **Step 6: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS, including `Pawl.CardsSpec`'s per-file re-parse and byte-stability assertions and `Pawl.CardSpec`'s directory/registry lint.

- [x] **Step 7: Commit**

```bash
git add source/library/Pawl/Type/StateCondition.hs source/library/Pawl/Type/TriggerCondition.hs source/library/Pawl/Type/Subtype.hs source/library/Pawl/Event.hs source/library/Pawl/Codec.hs data/cards/barbarian-outcast.json source/test-suite/Pawl/TriggerSpec.hs source/test-suite/Pawl/Cards.hs source/test-suite/Pawl/CodecSpec.hs
hooky fix && git add -u && hooky run
git commit -m "feat(m4.5-p4): CR 603.8 state triggers with derived re-arming (Barbarian Outcast)"
```

---

### Task 5: Turn history → **Khabál Ghoul**

The log becomes readable as history. `CountSpec` grows one arm that folds `GameState.events`, counting the `Moved` entries that went battlefield → graveyard and whose **snapshot** says creature (CR 700.4's definition of *dies*, CR 608.2h/608.2i's last known information). Khabál Ghoul introduces **zero** new opcodes — it is `PutCounters PlusOnePlusOne (Count …) "self"`, cashing P3b's numeric tower and Task 3's reserved slot.

**Files:**
- Create: `data/cards/khabál-ghoul.json`
- Modify: `source/library/Pawl/Type/CountSpec.hs`, `source/library/Pawl/Type/Subtype.hs`
- Modify: `source/library/Pawl/Quantity.hs`
- Modify: `source/library/Pawl/Codec.hs`
- Test: `source/test-suite/Pawl/TriggerSpec.hs`, `source/test-suite/Pawl/Cards.hs`, `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Consumes: `GameState.events`, `GameEvent.Moved` (Task 1); `TriggerCondition.StepBegins`, `TurnScope.EachTurn` (Task 2); `Binding.triggerSource` (Task 3).
- Produces: `CountSpec.CreaturesDiedThisTurn`; `Subtype.Zombie`; `Pawl.Cards.khabalGhoulPrinting`.

- [x] **Step 1: Write the failing tests**

Add to `source/test-suite/Pawl/TriggerSpec.hs` (and to the `tests` list):

```haskell
-- Khabál Ghoul {2}{B} Creature -- Zombie 1/1: "At the beginning of each end step,
-- put a +1/+1 counter on this creature for each creature that died this turn."
-- Scryfall's only ruling on the card is the design in one sentence: the count
-- "includes creature tokens ... as well as creatures put into a graveyard before
-- Khabál Ghoul entered the battlefield."
historyTests :: Cards.Cards -> Tasty.TestTree
historyTests cards =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs =
        Event.recordEvent (GameEvent.StepBegan endStep S.alice) (gs {GameState.phase = endStep})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      countersOn oid gs = maybe 0 (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
   in Tasty.testGroup
        "TurnHistory"
        [ -- The drained-queue falsifier: the deaths are SCANNED past before the end
          -- step's trigger ever exists, and must still be counted.
          HU.testCase "CR 608.2i deaths the trigger scan already passed are still counted" $
            let (ghoul, gs0) = S.addCreature (Cards.khabalGhoulPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
                (p1, gs1) = S.addPiker cards S.bob gs0
                (p2, gs2) = S.addPiker cards S.bob gs1
                dead = Event.destroy p2 (Event.destroy p1 gs2)
                scanned = settle dead
                atEnd = resolveAll (settle (beginEndStep scanned))
             in do
                  HU.assertEqual "two +1/+1 counters" 2 (countersOn ghoul atEnd)
                  HU.assertEqual "a 3/3" (Just 3) (Projection.powerOf ghoul atEnd),
          -- The ruling's OTHER half: "...as well as creatures put into a graveyard
          -- before Khabál Ghoul entered the battlefield." Every other case here adds
          -- the Ghoul first, so without this one nothing proves the count is not
          -- scoped to the Ghoul's own lifetime. A regression gate on the signature
          -- rather than a discriminator: `countOf` takes no ObjectId, so scoping the
          -- fold to the counting object is currently unrepresentable.
          HU.testCase "CR 608.2i a creature that died before the Ghoul entered is still counted" $
            let (p1, gs0) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
                dead = Event.destroy p1 gs0
                (ghoul, gs1) = S.addCreature (Cards.khabalGhoulPrinting cards) S.alice dead
                atEnd = resolveAll (settle (beginEndStep gs1))
             in HU.assertEqual "the earlier death counts" 1 (countersOn ghoul atEnd),
          -- CR 111.3 / 608.2h: a token has NO printed card, so an implementation
          -- that re-derived card types from print instead of from the event's
          -- snapshot would read zero here.
          HU.testCase "CR 111.3 a token creature that died counts, though it has no printed card" $
            let (ghoul, gs0) = S.addCreature (Cards.khabalGhoulPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
                (tok, gs1) = S.addToken (Printing.card (Cards.pikerPrinting cards)) S.bob gs0
                dead = Sba.checkStateBasedActions (Event.destroy tok gs1)
                atEnd = resolveAll (settle (beginEndStep dead))
             in HU.assertEqual "the token is counted" 1 (countersOn ghoul atEnd),
          HU.testCase "a creature that left the battlefield for HAND did not die (CR 700.4)" $
            let (ghoul, gs0) = S.addCreature (Cards.khabalGhoulPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
                (p1, gs1) = S.addPiker cards S.bob gs0
                bounced = Event.changeZone p1 Zone.Hand gs1
                atEnd = resolveAll (settle (beginEndStep bounced))
             in HU.assertEqual "a bounce is not a death" 0 (countersOn ghoul atEnd),
          -- CR 608.2i: the history's scope is ONE turn.
          HU.testCase "the count resets at turn handoff, not at the trigger scan" $
            let (ghoul, gs0) = S.addCreature (Cards.khabalGhoulPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
                (p1, gs1) = S.addPiker cards S.bob gs0
                dead = Event.destroy p1 gs1
                nextTurn = snd (Engine.runGamePure S.identityAnswer dead Engine.handoffTurn)
                atEnd = resolveAll (settle (beginEndStep nextTurn))
             in HU.assertEqual "last turn's death does not count" 0 (countersOn ghoul atEnd),
          -- CR 603.2b: the step trigger belongs to a permanent with nothing to do
          -- with the event -- Task 2's widened scan, at gameplay level.
          HU.testCase "CR 603.2b the end step's beginning is what fires it" $
            let (_, gs0) = S.addCreature (Cards.khabalGhoulPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
                quiet = settle gs0
                fired = settle (beginEndStep quiet)
                isTrigger oid = case Game.lookupObject oid fired of
                  Just obj -> case Object.source obj of
                    Source.OfTrigger _ _ -> True
                    _ -> False
                  Nothing -> False
             in do
                  HU.assertEqual "nothing before the step began" [] (GameState.stack quiet)
                  HU.assertEqual "one trigger once it did" 1 (length (filter isTrigger (GameState.stack fired)))
        ]
```

Add imports to `TriggerSpec.hs`: `Pawl.Type.CounterKind` as `CounterKind`, `Pawl.Type.Printing` as `Printing`.

Add to `CodecSpec.hs`'s "P4 runtime types" group:

```haskell
          HU.testCase "CountSpec.CreaturesDiedThisTurn round-trips" $
            roundTrip "count" Codec.countSpecToJson Codec.jsonToCountSpec CountSpec.CreaturesDiedThisTurn,
```

with `Pawl.Type.CountSpec` imported, and switch the second `StateCondition` round-trip entry added in Task 4 to `StateCondition.NoPermanentsOfSubtype Subtype.Zombie`.

- [x] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `CountSpec.CreaturesDiedThisTurn`, `Subtype.Zombie` and `Cards.khabalGhoulPrinting` are not in scope.

- [x] **Step 3: Add the counting arm**

In `source/library/Pawl/Type/CountSpec.hs`, add the arm — and correct the type's header comment, whose "Both inhabitants read only zone membership and PRINTED card types" claim this arm changes:

```haskell
  | -- CR 608.2i / 700.4: Khabál Ghoul. How many creatures DIED this turn -- were
    -- put into a graveyard FROM THE BATTLEFIELD (CR 700.4's definition). Folds
    -- GameState.events, and reads creature-ness from each event's last-known-
    -- information snapshot (CR 608.2h), never from a printed card: a token has no
    -- printed card at all (CR 111.3) and an animated land died as a creature.
    CreaturesDiedThisTurn
```

Amend the header comment's third paragraph to:

```haskell
-- The first two inhabitants read only zone membership and PRINTED card types, and
-- CreaturesDiedThisTurn reads only the event log's own snapshots -- never the LIVE
-- projection. Pawl.Quantity cannot import Pawl.Projection (Projection imports
-- Quantity), and a count evaluated inside the layer fold would recurse into the
-- fold that called it. A count over live projected state ("lands you control") is
-- a named deferral in the P3b spec, section 8.
```

In `source/library/Pawl/Type/Subtype.hs`, append after `Barbarian`:

```haskell
  | Zombie -- CR 205.3m (a creature type; Khabál Ghoul's and Sarcomancy's token's)
```

In `source/library/Pawl/Quantity.hs`, add the `countOf` arm and its helper:

```haskell
  -- CR 700.4: "dies" means put into a graveyard FROM THE BATTLEFIELD.
  CountSpec.CreaturesDiedThisTurn ->
    Just (toInteger (length (filter died (Foldable.toList (GameState.events gs)))))
```

```haskell
-- CR 700.4 / 608.2h: did this event record a creature dying? Creature-ness comes
-- from the event's own snapshot -- the object as it last existed on the
-- battlefield -- so a land animated into a creature counts, and so does a token
-- (which has no printed card to consult, CR 111.3).
died :: GameEvent.GameEvent -> Bool
died event = case event of
  GameEvent.Moved zc snapshot ->
    ZoneChange.from zc == Zone.Battlefield
      && ZoneChange.to zc == Zone.Graveyard
      && Set.member CardType.Creature (PC.cardTypes snapshot)
  GameEvent.DamageDealt _ -> False
  GameEvent.StepBegan _ _ -> False
```

Add to `Quantity.hs`'s imports: `qualified Pawl.Type.GameEvent as GameEvent`, `qualified Pawl.Type.ProjectedCharacteristics as PC`, `qualified Pawl.Type.ZoneChange as ZoneChange`. `Data.Foldable`, `Data.Set`, `Pawl.Type.CardType` and `Pawl.Type.Zone` are already imported. Note `Pawl.Quantity` cases on `GameEvent` here while `Pawl.Event` cases on it too — that is fine: `GameEvent` is a data record, not a case-restricted classification like `TriggerCondition`, whose sole casing home stays `Pawl.Event`.

- [x] **Step 4: Add the codec arm and the card**

In `source/library/Pawl/Codec.hs`, add `CountSpec.CreaturesDiedThisTurn -> "CreaturesDiedThisTurn"` to `countSpecToJson` and the matching pair to `jsonToCountSpec`'s table, plus the `Zombie` entries in `subtypeToJson` / `jsonToSubtype`.

Create `data/cards/khabál-ghoul.json` — note the non-ASCII `á` (NFC, U+00E1) in **both** the file name and the `name` field; `Codec.slugify` keeps alphanumerics, so the slug is `khabál-ghoul`:

```json
{"name":"Khabál Ghoul","manaCost":[{"type":"Generic","value":2},{"type":"OfType","value":{"type":"Colored","value":{"type":"Black"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Creature"}],"subtypes":[{"type":"Zombie"}]},"power":{"type":"Literal","value":1},"toughness":{"type":"Literal","value":1},"keywords":[],"staticAbilities":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[{"condition":{"type":"StepBegins","value":[{"type":"Ending","value":{"type":"EndStep"}},{"type":"EachTurn"}]},"modal":{"modes":[{"effects":[{"type":"PutCounters","value":[{"type":"PlusOnePlusOne"},{"type":"Count","value":{"type":"CreaturesDiedThisTurn"}},"self"]}],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}}}],"castingPermissions":[]}
```

In `source/test-suite/Pawl/Cards.hs`, add `khabalGhoulPrinting` (an ASCII field name for a non-ASCII slug) with `khabalGhoulPrinting_ <- loadPrinting "khabál-ghoul"`, the record field, and the `allPrintings` entry.

- [x] **Step 5: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS, including `Pawl.CardSpec`'s "the data/cards directory and Cards.allPrintings agree, by slug" test — that one is the check that the non-ASCII file name survived the filesystem round trip.

- [x] **Step 6: Commit**

```bash
git add source/library/Pawl/Type/CountSpec.hs source/library/Pawl/Type/Subtype.hs source/library/Pawl/Quantity.hs source/library/Pawl/Codec.hs "data/cards/khabál-ghoul.json" source/test-suite/Pawl/TriggerSpec.hs source/test-suite/Pawl/Cards.hs source/test-suite/Pawl/CodecSpec.hs
hooky fix && git add -u && hooky run
git commit -m "feat(m4.5-p4): CR 608.2i turn history, counted from the log's snapshots (Khabál Ghoul)"
```

---

### Task 6: Delayed triggered abilities → **Tidal Wave**

CR 603.7's trigger that belongs to no object on the battlefield. `Effect` is first-order and non-recursive, so the delayed ability's payload cannot live inside an opcode: it is **card data** (`Card.delayedAbilities`, keyed by name) and the opcode only **arms** it. `Create` grows an optional slot so the token it mints is referable by the ability armed in the same resolution — that is how CR 603.7c's "it" is remembered.

**Files:**
- Create: `source/library/Pawl/Type/AbilityName.hs`, `source/library/Pawl/Type/DelayedTrigger.hs`, `data/cards/tidal-wave.json`
- Modify: `source/library/Pawl/Type/Effect.hs`, `source/library/Pawl/Type/Card.hs`, `source/library/Pawl/Type/GameState.hs`
- Modify: `source/library/Pawl/Card.hs`, `source/library/Pawl/Event.hs`, `source/library/Pawl/Engine.hs`, `source/library/Pawl/Resolve.hs`, `source/library/Pawl/Setup.hs`, `source/library/Pawl/Codec.hs`
- Test: `source/test-suite/Pawl/TriggerSpec.hs`, `source/test-suite/Pawl/Support.hs`, `source/test-suite/Pawl/Cards.hs`, `source/test-suite/Pawl/CardSpec.hs`, `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Consumes: `PendingTrigger` (Task 2), `Effect.Sacrifice` + `Binding.triggerSource` (Task 3), `TriggerCondition.StepBegins` (Task 2).
- Produces: `Pawl.Type.AbilityName.AbilityName = MkAbilityName Text`; `Pawl.Type.DelayedTrigger.DelayedTrigger` with fields `ability`, `source`, `controller`, `bindings`; `Card.delayedAbilities :: Map AbilityName (TriggeredAbility Card)`; `Effect.ArmDelayedTrigger AbilityName`; `Effect.Create Quantity card (Maybe SlotName)`; `GameState.delayedTriggers :: Seq DelayedTrigger`; `Pawl.Event.gatherTriggers :: [GameEvent] -> GameState -> ([PendingTrigger], Seq DelayedTrigger)`; `Pawl.Card.delayedEffects`; `Pawl.Resolve.armedAbilities`, `definedSlots`, `bindsSeveralTokens`, `bindSlot`; `Pawl.Cards.tidalWavePrinting`.

- [x] **Step 1: Write the failing tests**

Add to `source/test-suite/Pawl/TriggerSpec.hs` (and to the `tests` list):

```haskell
-- Tidal Wave {2}{U} Instant: "Create a 5/5 blue Wall creature token with defender.
-- Sacrifice it at the beginning of the next end step." CR 603.7c's object-bound
-- delayed ability -- "it" must survive the resolution that armed it.
delayedTests :: Cards.Cards -> Tasty.TestTree
delayedTests cards =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan endStep S.alice) (gs {GameState.phase = endStep})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      -- alice casts Tidal Wave off three Islands and lets it resolve.
      castWave =
        let (gs, oid) = S.handOne (Cards.tidalWavePrinting cards) (S.landsInPlay (Cards.islandPrinting cards) 3)
         in resolveAll (snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid)))
      walls gs = filter (\oid -> Set.member Subtype.Wall (Projection.subtypesOf oid gs)) (Set.toList (GameState.battlefield gs))
   in Tasty.testGroup
        "DelayedTrigger"
        [ HU.testCase "CR 111.3 the spell mints a 5/5 Wall with defender and arms one delayed ability" $
            let after = castWave
             in case walls after of
                  [wall] -> do
                    HU.assertEqual "5 power" (Just 5) (Projection.powerOf wall after)
                    HU.assertEqual "5 toughness" (Just 5) (Projection.toughnessOf wall after)
                    HU.assertBool "defender" (Projection.hasKeyword Keyword.Defender wall after)
                    HU.assertEqual "one delayed ability waiting" 1 (Seq.length (GameState.delayedTriggers after))
                  other -> HU.assertFailure ("expected exactly one Wall token, got " <> show (length other)),
          -- CR 603.7b: "only once, the next time its trigger event occurs".
          HU.testCase "CR 603.7 the token is sacrificed at the beginning of the next end step" $
            let after = resolveAll (settle (beginEndStep castWave))
             in do
                  HU.assertEqual "no Wall left" [] (walls after)
                  HU.assertEqual "the store is empty" 0 (Seq.length (GameState.delayedTriggers after)),
          HU.testCase "CR 603.7b a second end step does not re-fire it" $
            let once = resolveAll (settle (beginEndStep castWave))
                again = settle (beginEndStep once)
             in HU.assertEqual "nothing on the stack" [] (GameState.stack again),
          -- CR 603.7a: a delayed ability does not trigger on an event that
          -- happened BEFORE it was created. Falls out of the watermark for free.
          HU.testCase "CR 603.7a armed during an end step, it waits for the NEXT one" $
            let (gs0, oid) = S.handOne (Cards.tidalWavePrinting cards) (S.landsInPlay (Cards.islandPrinting cards) 3)
                inEndStep = settle (beginEndStep gs0)
                cast = resolveAll (snd (Engine.runGamePure S.identityAnswer inEndStep (Cast.castSpell S.alice oid)))
                sameStep = settle cast
                nextStep = resolveAll (settle (beginEndStep sameStep))
             in do
                  HU.assertEqual "still alive during the step it was armed in" 1 (length (walls sameStep))
                  HU.assertEqual "sacrificed at the next end step" [] (walls nextStep),
          -- CR 603.7c: the ability still triggers and is still consumed even when
          -- the object it remembers is gone.
          HU.testCase "CR 603.7c with the token already gone the ability does nothing and is consumed" $
            let armed = castWave
                killed = case walls armed of
                  wall : _ -> Sba.checkStateBasedActions (Event.destroy wall armed)
                  [] -> armed
                after = resolveAll (settle (beginEndStep killed))
             in do
                  HU.assertEqual "no Wall" [] (walls after)
                  HU.assertEqual "the store is still emptied" 0 (Seq.length (GameState.delayedTriggers after))
                  HU.assertEqual "nothing stuck on the stack" [] (GameState.stack after)
        ]
```

Add imports to `TriggerSpec.hs`: `Pawl.Cast` as `Cast`, `Pawl.Type.Keyword` as `Keyword`.

Update the two Task 2 scan tests that call `Event.gatherTriggers` — it now returns a pair. Change `length (Event.gatherTriggers …)` to `length (fst (Event.gatherTriggers …))` and `case Event.gatherTriggers … of [pt] ->` to `case fst (Event.gatherTriggers … ) of [pt] ->`.

Add to `source/test-suite/Pawl/CardSpec.hs`'s `lintTests` list:

```haskell
      -- The AbilityName half of the D4 dataflow lint (CR 603.7): an
      -- ArmDelayedTrigger naming an ability the card does not declare is a FAILING
      -- TEST, never a trigger that silently never fires. Equality, not subset: a
      -- declared ability nothing arms is dead card text.
      HU.testCase "every armed delayed ability is declared, and every declared one is armed" $
        let cardOffends card =
              Resolve.armedAbilities (Card.allEffects card) /= Map.keysSet (Card.Type.delayedAbilities card)
            offenders = filter (cardOffends . Printing.card) (Cards.allPrintings cards)
         in HU.assertEqual "no dangling or unused delayed abilities" [] (map (Card.Type.name . Printing.card) offenders),
      -- Every slot a delayed ability READS must be one the arming card DEFINES:
      -- the reserved trigger-source slot, or a token bound by a Create.
      HU.testCase "every slot a delayed ability reads is bound by its card" $
        let cardOffends card =
              let available = Set.insert Binding.triggerSource (Resolve.definedSlots (Card.allEffects card))
                  wanted = Set.unions (map Resolve.slotsOf (Card.delayedEffects card))
               in not (Set.isSubsetOf wanted available)
            offenders = filter (cardOffends . Printing.card) (Cards.allPrintings cards)
         in HU.assertEqual "no dangling delayed-ability slot" [] (map (Card.Type.name . Printing.card) offenders),
      -- CR 603.7c: binding a slot to a MULTI-token Create would silently name one
      -- of them. A named deferral (P4 spec section 8), rejected rather than guessed.
      HU.testCase "no Create binds a slot while making more than one token" $
        let offenders =
              filter
                (Resolve.bindsSeveralTokens . Card.allEffects . Printing.card)
                (Cards.allPrintings cards)
         in HU.assertEqual "no multi-token binding" [] (map (Card.Type.name . Printing.card) offenders),
```

Add to `CodecSpec.hs`'s "P4 runtime types" group:

```haskell
          HU.testCase "AbilityName round-trips" $
            roundTrip "name" Codec.abilityNameToJson Codec.jsonToAbilityName (AbilityName.MkAbilityName (Text.pack "sacrifice it")),
          HU.testCase "ArmDelayedTrigger round-trips" $
            roundTrip "arm" Codec.effectToJson Codec.jsonToEffect (Effect.ArmDelayedTrigger (AbilityName.MkAbilityName (Text.pack "sacrifice it"))),
          HU.testCase "DelayedTrigger round-trips with its captured bindings" $
            let ability =
                  TriggeredAbility.MkTriggeredAbility
                    { TriggeredAbility.condition = TriggerCondition.StepBegins (Phase.Ending EndingStep.EndStep) TurnScope.EachTurn,
                      TriggeredAbility.modal = Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1)
                    }
                entry =
                  DelayedTrigger.MkDelayedTrigger
                    { DelayedTrigger.ability = ability,
                      DelayedTrigger.source = ObjectId.MkObjectId 4,
                      DelayedTrigger.controller = S.alice,
                      DelayedTrigger.bindings = Map.singleton (SlotName.MkSlotName (Text.pack "token")) (Binding.toObject (ObjectId.MkObjectId 9))
                    }
             in roundTrip "delayed" Codec.delayedTriggerToJson Codec.jsonToDelayedTrigger entry,
```

with `Pawl.Type.AbilityName`, `Pawl.Type.DelayedTrigger`, `Pawl.Type.TriggeredAbility` and `Pawl.Binding` imported. Also change the existing Create round-trip, if `CodecSpec` has one, to pass `Nothing` as the third argument. The `TriggeredAbility.intervening` field lands in Task 8; until then omit it from this literal, and add it when Task 8 introduces it.

- [x] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Pawl.Type.AbilityName` and `Pawl.Type.DelayedTrigger` do not exist; `Card.Type.delayedAbilities`, `GameState.delayedTriggers`, `Cards.tidalWavePrinting`, `Resolve.armedAbilities` are not in scope; `Effect.Create` has the wrong arity.

- [x] **Step 3: Add the two new types and the two data fields**

Create `source/library/Pawl/Type/AbilityName.hs`:

```haskell
module Pawl.Type.AbilityName where

import Data.Text (Text)

-- The name of a delayed triggered ability declared on a card (CR 603.7), joining
-- Effect.ArmDelayedTrigger to Card.delayedAbilities. SlotName's exact shape, and
-- for the same reason: named, never positional.
--
-- A card's text therefore lives in two fields joined by a name -- and the join is
-- policed the way SlotName's is. The dataflow lint (test suite) checks that every
-- armed name is declared and every declared name is armed, so a dangling name is
-- a FAILING TEST, never a trigger that silently never fires.
newtype AbilityName = MkAbilityName Text
  deriving (Eq, Ord, Show)
```

Create `source/library/Pawl/Type/DelayedTrigger.hs`:

```haskell
module Pawl.Type.DelayedTrigger where

import Data.Map.Strict (Map)
import Pawl.Type.Binding (Binding)
import Pawl.Type.Card (Card)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.TriggeredAbility (TriggeredAbility)

-- CR 603.7: a delayed triggered ability that has been created and is waiting for
-- its trigger event. A concrete `TriggeredAbility Card`, exactly as
-- Source.OfTrigger already carries one.
--
-- `controller` is the player who controlled the SPELL OR ABILITY that created it,
-- as that spell or ability RESOLVED (CR 603.7d-f) -- baked in at arming, never
-- re-derived. `bindings` is the environment captured at that moment, which is how
-- "it" and "that card" (CR 603.7c) survive the resolution that armed the ability.
--
-- An entry is removed as it fires (CR 603.7b: "only once, the next time its
-- trigger event occurs"). A STATED-DURATION delayed ability ("this turn") would
-- fire repeatedly instead; that is a named deferral (the P4 spec, section 8).
data DelayedTrigger = MkDelayedTrigger
  { ability :: TriggeredAbility Card,
    source :: ObjectId,
    controller :: PlayerId,
    bindings :: Map SlotName Binding
  }
  deriving (Eq, Show)
```

In `source/library/Pawl/Type/Card.hs`, add the field after `triggeredAbilities`:

```haskell
    -- CR 603.7: this card's DELAYED triggered abilities, keyed by name -- the
    -- payloads an Effect.ArmDelayedTrigger in this card's own text arms. Card
    -- DATA, not an opcode payload: Effect is first-order and non-recursive
    -- (design.md section 1), and Effect -> TriggeredAbility -> Modal -> Mode ->
    -- Effect is a genuine module cycle. Empty for all but Tidal Wave.
    --
    -- Read straight from the card, never through the projection: a delayed
    -- ability is not ON the source object -- CR 603.7d gives it no source
    -- permanent to lose, so layer 6 cannot strip it.
    delayedAbilities :: Map AbilityName (TriggeredAbility Card),
```

with `import Data.Map.Strict (Map)` and `import Pawl.Type.AbilityName (AbilityName)`.

In `source/library/Pawl/Type/Effect.hs`, add the arm and grow `Create`:

```haskell
  | -- CR 603.7: create a delayed triggered ability -- the one this card declares
    -- under this name (Card.delayedAbilities). First-order: the payload is card
    -- data joined by a name, so this opcode carries no nested ability and adds no
    -- type parameter. The resolving object's binding environment is captured as
    -- the ability is armed, which is how "it" / "that card" (CR 603.7c) is
    -- remembered after this resolution ends.
    ArmDelayedTrigger AbilityName
```

and change `Create`'s signature to `Create Quantity card (Maybe SlotName)`, appending to its comment:

```haskell
    -- The Maybe SlotName BINDS the minted token into LIVE object state so the
    -- ArmDelayedTrigger in the same resolution -- which re-reads Object.bindings --
    -- can capture it as CR 603.7c's "it". NOT visible to a later effect of the same
    -- resolution: applyEffect's `chosen` map is computed once before the fold, so a
    -- sibling effect still reads the pre-Create snapshot (the D4 declared-equals-read
    -- lint catches a spell mode that tries, so this fails loudly, never silently). A DEFINITION, not a read: it is not a target and never appears in
    -- targetSpecs. Defined only for a single-token create; a Create that binds a
    -- slot while making several tokens is a named deferral (the P4 spec, section
    -- 8) that the Pawl.CardSpec lint family rejects rather than guessing at.
```

with `import Pawl.Type.AbilityName (AbilityName)`.

In `source/library/Pawl/Type/GameState.hs`, add after `damageScannedThrough`:

```haskell
    -- CR 603.7: delayed triggered abilities awaiting their event, in creation
    -- order. Appended by Resolve's ArmDelayedTrigger; an entry is removed as it
    -- fires (CR 603.7b). NOT cleared at turn handoff -- "at the beginning of the
    -- next end step" survives into the next turn if this turn's end step passed
    -- before the ability was armed.
    delayedTriggers :: Seq DelayedTrigger,
```

with `import Pawl.Type.DelayedTrigger (DelayedTrigger)`, and add `GameState.delayedTriggers = Seq.empty,` to `Setup.emptyGame` and to `Support.oneMountainState`.

In `source/library/Pawl/Card.hs`, add:

```haskell
-- CR 603.7: every effect across all of a card's DELAYED abilities' modes. The
-- read half of the delayed-ability dataflow lint, as allEffects is for the spell.
delayedEffects :: Card.Card -> [Effect Card.Card]
delayedEffects card = concatMap (Modal.allEffects . TriggeredAbility.modal) (Map.elems (Card.delayedAbilities card))
```

with `import qualified Data.Map.Strict as Map` and `import qualified Pawl.Type.TriggeredAbility as TriggeredAbility`.

- [x] **Step 4: Arm, bind, and fire**

In `source/library/Pawl/Resolve.hs`, update the five classification functions for `Create`'s new arity and add the two new arms:

- `slotsOf`: `Effect.Create _ _ _ -> Set.empty` (with `-- Create's slot is a DEFINITION, not a read: it is not a target, so the D4 lint must not see it here.`) and `Effect.ArmDelayedTrigger _ -> Set.empty`
- `readsX`: `Effect.Create quantity _ _ -> quantity == Quantity.Type.X` and `Effect.ArmDelayedTrigger _ -> False`
- `manaProduced`: add `Effect.ArmDelayedTrigger _ -> Nothing` (`Effect.Create {}` already matches the new arity)
- `searchesLibrary`: add `Effect.ArmDelayedTrigger _ -> False`
- `rewriteEffect`: change `Effect.Create _ _ -> effect` to `Effect.Create {} -> effect` and add `Effect.ArmDelayedTrigger _ -> effect`

Add the three lint-read classifications beside `textChangeSlots`:

```haskell
-- CR 603.7: the delayed abilities an effect list ARMS, by name. The read half of
-- the AbilityName dataflow lint, exactly as slotsOf is for target slots.
armedAbilities :: [Effect Card.Type.Card] -> Set AbilityName
armedAbilities effects =
  let named effect = case effect of
        Effect.ArmDelayedTrigger name -> Just name
        _ -> Nothing
   in Set.fromList (Maybe.mapMaybe named effects)

-- The slots an effect list DEFINES rather than reads: a Create that names the
-- token it mints (CR 603.7c's "it"). The write half of the same lint.
definedSlots :: [Effect Card.Type.Card] -> Set SlotName
definedSlots effects =
  let bound effect = case effect of
        Effect.Create _ _ mSlot -> mSlot
        _ -> Nothing
   in Set.fromList (Maybe.mapMaybe bound effects)

-- Does any Create bind a slot while minting more than one token? CR 603.7c's "it"
-- names ONE object; binding one of several would be the engine choosing. A named
-- deferral (the P4 spec, section 8), rejected by the lint until a card needs
-- "sacrifice THEM".
bindsSeveralTokens :: [Effect Card.Type.Card] -> Bool
bindsSeveralTokens effects =
  let offends effect = case effect of
        Effect.Create quantity _ (Just _) -> quantity /= Quantity.Type.Literal 1
        _ -> False
   in any offends effects
```

with `import Pawl.Type.AbilityName (AbilityName)`.

Replace `applyEffect`'s `Create` arm and add `ArmDelayedTrigger`:

```haskell
  Effect.Create quantity card mSlot -> do
    gs <- State.get
    case Quantity.evaluate gs source (Just controller) quantity of
      Just n
        | n > 0 -> do
            -- CR 111: create n tokens with these characteristics under the
            -- effect's controller (CR 111.2). Each createToken mints a distinct
            -- object.
            let before = GameState.battlefield gs
            State.modify' (\g -> List.foldl' (\g1 _ -> Event.createToken controller card g1) g [1 .. n])
            case mSlot of
              Nothing -> pure ()
              Just slot -> do
                -- CR 603.7c: bind the minted token so a later effect in this same
                -- resolution -- or the delayed ability it arms -- can name it. The
                -- lint guarantees n == 1 here, so the single new battlefield id is
                -- unambiguous.
                after <- State.gets GameState.battlefield
                case Set.toList (Set.difference after before) of
                  newId : _ -> State.modify' (bindSlot source slot newId)
                  [] -> pure ()
      _ -> pure ()
  Effect.ArmDelayedTrigger name -> do
    gs <- State.get
    case Game.cardOf source gs >>= (Map.lookup name . Card.Type.delayedAbilities) of
      -- The dataflow lint makes a dangling name a failing test, never a silent
      -- no-op; this arm only keeps the executor total.
      Nothing -> pure ()
      Just ability ->
        -- CR 603.7d-f: the controller is the player who controlled the spell or
        -- ability AS IT RESOLVED -- `controller`, baked in now. CR 603.7a: an
        -- entry appended here can only ever match events at or after the current
        -- watermark, so it never fires on an event that already happened.
        let captured = maybe Map.empty Object.bindings (Game.lookupObject source gs)
            entry =
              DelayedTrigger.MkDelayedTrigger
                { DelayedTrigger.ability = ability,
                  DelayedTrigger.source = source,
                  DelayedTrigger.controller = controller,
                  DelayedTrigger.bindings = captured
                }
         in State.put gs {GameState.delayedTriggers = GameState.delayedTriggers gs Seq.|> entry}
```

and add the helper:

```haskell
-- CR 603.7c: bind `target` into `slot` of `holder`'s LIVE binding environment, so
-- the ArmDelayedTrigger of this same resolution can capture it. A sibling effect
-- cannot see it: applyEffect's `chosen` is fixed before the fold begins. `holder` is the effect SOURCE, which is the resolving spell
-- itself for a spell and the source permanent for an ability; the same object
-- ArmDelayedTrigger captures from, so the two always agree.
bindSlot :: ObjectId -> SlotName -> ObjectId -> GameState -> GameState
bindSlot holder slot target gs =
  let put obj = obj {Object.bindings = Map.insert slot (Binding.Type.empty {Binding.Type.target = Just (Recipient.ToObject target)}) (Object.bindings obj)}
   in gs {GameState.objects = Map.adjust put holder (GameState.objects gs)}
```

`Pawl.Resolve` already imports `Pawl.Binding` as `Binding`; import the record type as `qualified Pawl.Type.Binding as Binding.Type` — or, if the alias-to-last-component rule makes that awkward, add the constructor helper to `Pawl.Binding` instead and call it from here. Prefer the latter: add to `source/library/Pawl/Binding.hs`

```haskell
-- A binding that names one object and nothing else -- what a token bound by a
-- Create (CR 603.7c) or a trigger's source slot holds.
toObject :: ObjectId -> Binding
toObject oid = Binding.empty {Binding.target = Just (Recipient.ToObject oid)}
```

and rewrite `setTriggerSource` as `setTriggerSource oid = Map.insert triggerSource (toObject oid)` and `bindSlot`'s `put` as `Map.insert slot (Binding.toObject target)`.

In `source/library/Pawl/Event.hs`, add the delayed pass and widen the gather:

```haskell
-- CR 603.7: delayed abilities whose trigger event is among these events. Each one
-- that fires is REMOVED from the store (CR 603.7b: "only once, the next time its
-- trigger event occurs"); the survivors are returned so the caller can store them
-- back. CR 603.7d-f: the controller travels with the entry, so a delayed ability
-- resolves under the player who controlled the spell that created it even if that
-- spell's source object is long gone.
delayedPending :: [GameEvent] -> GameState -> ([PendingTrigger], Seq.Seq DelayedTrigger)
delayedPending events gs =
  let fires entry =
        let cond = TriggeredAbility.condition (DelayedTrigger.ability entry)
         in any (matchesTrigger (DelayedTrigger.source entry) (DelayedTrigger.controller entry) cond) events
      pend entry =
        PendingTrigger.MkPendingTrigger
          (DelayedTrigger.source entry)
          (DelayedTrigger.controller entry)
          (DelayedTrigger.ability entry)
          (DelayedTrigger.bindings entry)
      store = GameState.delayedTriggers gs
   in (map pend (Foldable.toList (Seq.filter fires store)), Seq.filter (not . fires) store)

-- Everything that has triggered and is not yet on the stack, from all three
-- sources, plus the delayed store as it stands afterwards. One function, so
-- Pawl.Engine never needs to know how many sources there are.
gatherTriggers :: [GameEvent] -> GameState -> ([PendingTrigger], Seq.Seq DelayedTrigger)
gatherTriggers events gs =
  let (fromDelayed, surviving) = delayedPending events gs
   in (eventTriggers events gs ++ stateTriggers gs ++ fromDelayed, surviving)
```

with `import Pawl.Type.DelayedTrigger (DelayedTrigger)` and `import qualified Pawl.Type.DelayedTrigger as DelayedTrigger`.

In `source/library/Pawl/Engine.hs`, thread the store through `placePendingTriggers`, and merge the captured bindings in `placeOne`:

```haskell
placePendingTriggers :: Game Bool
placePendingTriggers = do
  gs <- State.get
  let (pending, surviving) = Event.gatherTriggers (Event.unscannedEvents gs) gs
  State.put
    gs
      { GameState.scannedThrough = fromIntegral (Seq.length (GameState.events gs)),
        GameState.delayedTriggers = surviving
      }
  Monad.mapM_ placeOne (apnapOrder gs pending)
  pure (not (null pending))
```

```haskell
      -- CR 603.7c: a delayed ability's CAPTURED environment (its "it") rides
      -- alongside the targets chosen now; the source slot is stamped over the top.
      -- Map.union is LEFT-biased and the placement-time bindings are on the left, so
      -- THIS ability's own reserved slots win: the captured environment belongs to
      -- whatever armed the ability and carries a spell's own `modes` and X, which
      -- must not override the delayed ability's mode selection. The captured token
      -- slot has no placement-time counterpart, so it still rides along.
      State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.setTriggerSource srcId (Map.union (Binding.fromChoices chosen Map.empty Nothing chosenModes) (PendingTrigger.bindings pending))}) abilId (GameState.objects g)})
```

- [x] **Step 5: Add the codec arms and the card**

In `source/library/Pawl/Codec.hs`:

```haskell
abilityNameToJson :: AbilityName.AbilityName -> Value
abilityNameToJson (AbilityName.MkAbilityName t) = Json.jText t

jsonToAbilityName :: Value -> Either Text AbilityName.AbilityName
jsonToAbilityName value = AbilityName.MkAbilityName <$> Json.asText value

-- The targetSpecsToJson shape: a name-keyed map as a sorted array of entries, so
-- the render is deterministic and the file byte-stable.
delayedAbilitiesToJson :: Map.Map AbilityName.AbilityName (TriggeredAbility.TriggeredAbility CardT.Card) -> Value
delayedAbilitiesToJson m =
  listTo
    (\(k, v) -> Object [(Text.pack "name", abilityNameToJson k), (Text.pack "ability", triggeredAbilityToJson v)])
    (Map.toAscList m)

jsonToDelayedAbilities :: Value -> Either Text (Map.Map AbilityName.AbilityName (TriggeredAbility.TriggeredAbility CardT.Card))
jsonToDelayedAbilities value =
  let decodeEntry v = do
        ps <- Json.asObject v
        k <- Json.field (Text.pack "name") ps >>= jsonToAbilityName
        a <- Json.field (Text.pack "ability") ps >>= jsonToTriggeredAbility
        pure (k, a)
   in Map.fromList <$> listFrom decodeEntry value

-- An omitted delayedAbilities field decodes to empty, so every card file that
-- predates P4 stays byte-identical (the copyOnEnter precedent).
mapFromDefault :: (Ord k) => (Value -> Either Text (Map.Map k v)) -> Value -> Either Text (Map.Map k v)
mapFromDefault f value = case value of
  Null -> Right Map.empty
  _ -> f value

-- Runtime-only, never in card JSON -- covered for the same reason SetController's
-- PlayerId is: the codec must stay total over the transitive closure of what the
-- game state carries.
bindingToJson :: Binding.Binding -> Value
bindingToJson b =
  Object
    [ (Text.pack "target", maybeTo recipientToJson (Binding.target b)),
      (Text.pack "subtypes", maybeTo (\(f, t) -> Array [subtypeToJson f, subtypeToJson t]) (Binding.subtypes b)),
      (Text.pack "amount", maybeTo natTo (Binding.amount b)),
      (Text.pack "modes", maybeTo (setTo modeIndexToJson) (Binding.modes b)),
      (Text.pack "copy", maybeTo projectedCharacteristicsToJson (Binding.copy b))
    ]

jsonToBinding :: Value -> Either Text Binding.Binding
jsonToBinding value = do
  ps <- Json.asObject value
  t <- maybeFrom jsonToRecipient (getOpt (Text.pack "target") ps)
  s <- maybeFrom jsonToSubtypePair (getOpt (Text.pack "subtypes") ps)
  a <- maybeFrom natFrom (getOpt (Text.pack "amount") ps)
  m <- maybeFrom (setFrom jsonToModeIndex) (getOpt (Text.pack "modes") ps)
  c <- maybeFrom jsonToProjectedCharacteristics (getOpt (Text.pack "copy") ps)
  pure
    Binding.MkBinding
      { Binding.target = t,
        Binding.subtypes = s,
        Binding.amount = a,
        Binding.modes = m,
        Binding.copy = c
      }

jsonToSubtypePair :: Value -> Either Text (Subtype.Subtype, Subtype.Subtype)
jsonToSubtypePair value = case value of
  Array [f, t] -> do
    f_ <- jsonToSubtype f
    t_ <- jsonToSubtype t
    pure (f_, t_)
  _ -> Left (Text.pack "expected a [from, to] subtype pair")

bindingsToJson :: Map.Map SlotName.SlotName Binding.Binding -> Value
bindingsToJson m =
  listTo
    (\(k, v) -> Object [(Text.pack "slot", slotNameToJson k), (Text.pack "binding", bindingToJson v)])
    (Map.toAscList m)

jsonToBindings :: Value -> Either Text (Map.Map SlotName.SlotName Binding.Binding)
jsonToBindings value =
  let decodeEntry v = do
        ps <- Json.asObject v
        k <- Json.field (Text.pack "slot") ps >>= jsonToSlotName
        b <- Json.field (Text.pack "binding") ps >>= jsonToBinding
        pure (k, b)
   in Map.fromList <$> listFrom decodeEntry value

delayedTriggerToJson :: DelayedTrigger.DelayedTrigger -> Value
delayedTriggerToJson d =
  Object
    [ (Text.pack "ability", triggeredAbilityToJson (DelayedTrigger.ability d)),
      (Text.pack "source", objectIdToJson (DelayedTrigger.source d)),
      (Text.pack "controller", playerIdToJson (DelayedTrigger.controller d)),
      (Text.pack "bindings", bindingsToJson (DelayedTrigger.bindings d))
    ]

jsonToDelayedTrigger :: Value -> Either Text DelayedTrigger.DelayedTrigger
jsonToDelayedTrigger value = do
  ps <- Json.asObject value
  a <- Json.field (Text.pack "ability") ps >>= jsonToTriggeredAbility
  s <- Json.field (Text.pack "source") ps >>= jsonToObjectId
  c <- Json.field (Text.pack "controller") ps >>= jsonToPlayerId
  b <- Json.field (Text.pack "bindings") ps >>= jsonToBindings
  pure
    DelayedTrigger.MkDelayedTrigger
      { DelayedTrigger.ability = a,
        DelayedTrigger.source = s,
        DelayedTrigger.controller = c,
        DelayedTrigger.bindings = b
      }
```

adding `import qualified Pawl.Type.Binding as Binding` and `import qualified Pawl.Type.DelayedTrigger as DelayedTrigger` to `Codec.hs`.

In `effectToJson`, replace the `Create` arm and add the new one:

```haskell
  Effect.Create q c Nothing -> Json.tagged (Text.pack "Create") (Just (Array [quantityToJson q, cardToJson c]))
  Effect.Create q c (Just s) -> Json.tagged (Text.pack "Create") (Just (Array [quantityToJson q, cardToJson c, slotNameToJson s]))
  Effect.ArmDelayedTrigger n -> Json.tagged (Text.pack "ArmDelayedTrigger") (Just (abilityNameToJson n))
```

In `jsonToEffect`:

```haskell
    "Create" -> case mv of
      Just (Array [q, c]) -> Effect.Create <$> jsonToQuantity q <*> jsonToCard c <*> pure Nothing
      Just (Array [q, c, s]) -> Effect.Create <$> jsonToQuantity q <*> jsonToCard c <*> (Just <$> jsonToSlotName s)
      _ -> Left (Text.pack "Create expects [Quantity, Card] or [Quantity, Card, slot]")
    "ArmDelayedTrigger" -> withValue mv (fmap Effect.ArmDelayedTrigger . jsonToAbilityName)
```

In `cardToJson`, append after the `characteristicPT` block:

```haskell
        ++ ( if Map.null (CardT.delayedAbilities c)
               then []
               else [(Text.pack "delayedAbilities", delayedAbilitiesToJson (CardT.delayedAbilities c))]
           )
```

and in `jsonToCard`, `delayed <- mapFromDefault jsonToDelayedAbilities (getOpt (Text.pack "delayedAbilities") ps)` plus `CardT.delayedAbilities = delayed` in the record. Add `import qualified Pawl.Type.AbilityName as AbilityName`.

Create `data/cards/tidal-wave.json` (one line, plus the trailing newline):

```json
{"name":"Tidal Wave","manaCost":[{"type":"Generic","value":2},{"type":"OfType","value":{"type":"Colored","value":{"type":"Blue"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Instant"}],"subtypes":[]},"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"spell":{"modes":[{"effects":[{"type":"Create","value":[{"type":"Literal","value":1},{"name":"Wall","manaCost":null,"typeLine":{"supertypes":[],"types":[{"type":"Creature"}],"subtypes":[{"type":"Wall"}]},"power":{"type":"Literal","value":5},"toughness":{"type":"Literal","value":5},"keywords":[{"type":"Defender"}],"staticAbilities":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"colorIndicator":[{"type":"Blue"}]},"token"]},{"type":"ArmDelayedTrigger","value":"sacrifice it"}],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"delayedAbilities":[{"name":"sacrifice it","ability":{"condition":{"type":"StepBegins","value":[{"type":"Ending","value":{"type":"EndStep"}},{"type":"EachTurn"}]},"modal":{"modes":[{"effects":[{"type":"Sacrifice","value":"token"}],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}}}}]}
```

In `source/test-suite/Pawl/Cards.hs`, register `tidalWavePrinting` (load `"tidal-wave"`), and add it to `allPrintings`.

- [x] **Step 6: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS. `Pawl.CardsSpec`'s byte-stability assertion over every *existing* card file is the check that adding `delayedAbilities` and `Create`'s slot left them untouched.

- [x] **Step 7: Commit**

```bash
git add source/library/Pawl/Type/AbilityName.hs source/library/Pawl/Type/DelayedTrigger.hs source/library/Pawl/Type/Effect.hs source/library/Pawl/Type/Card.hs source/library/Pawl/Type/GameState.hs source/library/Pawl/Card.hs source/library/Pawl/Binding.hs source/library/Pawl/Event.hs source/library/Pawl/Engine.hs source/library/Pawl/Resolve.hs source/library/Pawl/Setup.hs source/library/Pawl/Codec.hs data/cards/tidal-wave.json source/test-suite/Pawl/TriggerSpec.hs source/test-suite/Pawl/Support.hs source/test-suite/Pawl/Cards.hs source/test-suite/Pawl/CardSpec.hs source/test-suite/Pawl/CodecSpec.hs
hooky fix && git add -u && hooky run
git commit -m "feat(m4.5-p4): CR 603.7 delayed triggered abilities, armed by opcode (Tidal Wave)"
```

---

### Task 7: CR 603.3b ordering — the elision dies here, and the centerpiece scenario

M3f elided the trigger-ordering choice with a documented expiry: *"until a second simultaneous trigger exists."* Tasks 5 and 6 created one, so the expiry falls due and "the engine makes no choices" outranks everything. `Prompt.OrderTriggers` is asked of a player only when they control **two or more** pending triggers — where the rules leave nothing to ask, don't prompt.

The centerpiece test is the phase's whole thesis in one scenario: Tidal Wave's delayed sacrifice and Khabál Ghoul's counter trigger at the beginning of the *same* end step under one controller, the controller must order them, and **the order changes the answer** — because CR 608.2h determines the count when the effect is applied, the thing counted is a token with no printed card, and the death happened at a boundary the trigger scan had already passed.

**Files:**
- Modify: `source/library/Pawl/Type/Prompt.hs`, `source/library/Pawl/Type/Response.hs`
- Modify: `source/library/Pawl/Engine.hs`, `source/library/Pawl/Replay.hs`
- Modify: `source/test-suite/Pawl/Support.hs`, `source/test-suite/Pawl/{Game,Cast,Copy}Spec.hs`, `source/benchmark/Main.hs`
- Test: `source/test-suite/Pawl/TriggerSpec.hs`, `source/test-suite/Pawl/ReplaySpec.hs`

**Interfaces:**
- Consumes: everything from Tasks 1–6.
- Produces: `Prompt.OrderTriggers :: Decider -> PlayerId -> [ObjectId] -> Prompt [Natural]`; `Response.OrderedTriggers [Natural]`; `Pawl.Engine.orderPending :: [PendingTrigger] -> Game [PendingTrigger]`. `Pawl.Engine.apnapOrder` is **replaced** by `Pawl.Engine.apnapPlayers :: GameState -> [PendingTrigger] -> [PlayerId]`.

- [ ] **Step 1: Write the failing tests**

Add to `source/test-suite/Pawl/TriggerSpec.hs` (and to the `tests` list):

```haskell
-- CR 603.3b: "that player puts them on the stack in any order they choose". The
-- centerpiece: two triggers, one controller, and an order that changes the answer.
orderingTests :: Cards.Cards -> Tasty.TestTree
orderingTests cards =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan endStep S.alice) (gs {GameState.phase = endStep})
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      countersOn oid gs = maybe 0 (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      -- alice has Khabál Ghoul out and casts Tidal Wave, so both a delayed
      -- sacrifice and a step trigger are pending at the same end step.
      board =
        let (gs0, waveId) = S.handOne (Cards.tidalWavePrinting cards) (S.landsInPlay (Cards.islandPrinting cards) 3)
            (ghoul, gs1) = S.addCreature (Cards.khabalGhoulPrinting cards) S.alice gs0
            cast = resolveAll (snd (Engine.runGamePure S.identityAnswer gs1 (Cast.castSpell S.alice waveId)))
         in (ghoul, beginEndStep cast)
      -- The source of the OTHER pending trigger: Tidal Wave's delayed ability,
      -- whose source is the resolved spell's id rather than any permanent.
      otherThan ghoul gs =
        let sources = map PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedEvents gs) gs))
         in case filter (/= ghoul) sources of
              oid : _ -> oid
              [] -> ghoul
      -- An answerer that puts a chosen source LAST on the stack, so it resolves
      -- FIRST (CR 603.3b's answer is the order they are PUT on the stack).
      orderLast :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      orderLast wanted p = case p of
        Prompt.OrderTriggers _ _ sources ->
          let indexed = zip [0 :: Int ..] sources
              pick keep = map (fromIntegral . fst) (filter (\entry -> (snd entry == wanted) == keep) indexed)
           in pick False ++ pick True
        _ -> S.identityAnswer p
      -- Counts how many times the ordering prompt was asked, answering canonically.
      countingAnswer :: Prompt.Prompt r -> State.State Int r
      countingAnswer p = case p of
        Prompt.OrderTriggers _ _ sources -> do
          State.modify' (+ 1)
          pure (map fromIntegral (take (length sources) [0 :: Int ..]))
        _ -> pure (S.identityAnswer p)
   in Tasty.testGroup
        "TriggerOrdering"
        [ HU.testCase "CR 603.3b two triggers under one controller ask for an order, exactly once" $
            let (_, gs) = board
                (_, asked) = State.runState (Engine.runGame countingAnswer gs Engine.settleForPriority) 0
             in HU.assertEqual "asked once" 1 asked,
          -- Sacrifice resolves FIRST: the Wall token dies, and CR 608.2h has the
          -- Ghoul count it when its own effect is applied. The token has NO printed
          -- card (CR 111.3) and its death happened at a boundary the scan already
          -- passed -- so a re-derived type line or a drained queue both read zero.
          HU.testCase "CR 608.2h sacrificing first makes the Ghoul count the token" $
            let (ghoul, gs) = board
                after = snd (Engine.runGamePure (orderLast ghoul) gs Engine.priorityLoop)
             in HU.assertEqual "the token was counted" 1 (countersOn ghoul after),
          -- The Ghoul resolves FIRST: the token is still alive, so it is not
          -- counted. Same board, same cards, opposite answer -- which is what makes
          -- the ordering a genuine choice rather than a formality.
          HU.testCase "CR 608.2h counting first means the token is still alive and is not counted" $
            let (ghoul, gs) = board
                after = snd (Engine.runGamePure (orderLast (otherThan ghoul gs)) gs Engine.priorityLoop)
             in HU.assertEqual "nothing counted" 0 (countersOn ghoul after)
        ]
```

Add imports to `TriggerSpec.hs`: `Control.Monad.Trans.State.Strict` as `State`, `Pawl.Type.Prompt` as `Prompt`, and the `{-# LANGUAGE GADTs #-}` and `{-# LANGUAGE RankNTypes #-}` pragmas (an answerer that pattern-matches a `Prompt r` needs `GADTs`; passing one to `runGamePure` needs `RankNTypes`), matching `Pawl.Support`'s header. `orderLast` and `countingAnswer` carry explicit signatures inside the `let`, which needs `ScopedTypeVariables`-free rank-1 types — they are, since `r` is universally quantified by the `Prompt r` GADT match; if GHC objects, lift both to top-level definitions in the module rather than adding an extension.

Add to `source/test-suite/Pawl/ReplaySpec.hs`'s `combatReplayTests` list:

```haskell
          HU.testCase "OrderTriggers records and replays a permutation" $
            let p = Prompt.OrderTriggers decider S.alice [oid, ObjectId.MkObjectId 8]
                answer = [1, 0] :: [Natural.Natural]
             in HU.assertEqual "round-trip" (Just answer) (Replay.decode p (Replay.encode p answer)),
          HU.testCase "defaultAnswer keeps the canonical order" $
            HU.assertEqual
              "identity permutation"
              [0, 1 :: Natural.Natural]
              (Replay.defaultAnswer (Prompt.OrderTriggers decider S.alice [oid, ObjectId.MkObjectId 8])),
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Prompt.OrderTriggers` is not a constructor.

- [ ] **Step 3: Add the prompt and its response**

In `source/library/Pawl/Type/Prompt.hs`:

```haskell
  -- CR 603.3b: "If a player controlled two or more triggered abilities ... that
  -- player puts them on the stack in any order they choose." The [ObjectId] is
  -- that player's pending triggers, each entry its SOURCE object, in the engine's
  -- canonical order; the answer is a permutation of the entry INDICES, giving the
  -- order they are PUT ON THE STACK (so the last named resolves first).
  --
  -- Positional by necessity, unlike a target slot: two triggers from one source
  -- are genuinely indistinguishable, so any permutation among identical entries
  -- is equivalent. Asked ONLY when the player controls two or more -- with one
  -- there is nothing to choose, and where the rules leave nothing to ask, don't
  -- prompt. CR 603.3b's TWO-PART process (first the triggers whose condition is
  -- not another ability triggering, then the rest) is vacuous while no condition
  -- triggers on another ability triggering; this carries the note, not the
  -- machinery.
  OrderTriggers :: Decider -> PlayerId -> [ObjectId] -> Prompt [Natural]
```

In `source/library/Pawl/Type/Response.hs`:

```haskell
  | -- CR 603.3b: the order a player chose for their simultaneous triggers, as a
    -- permutation of the offered indices, serialized so a DecisionLog replays it.
    OrderedTriggers [Natural]
```

In `source/library/Pawl/Replay.hs`, add the three arms:

```haskell
  Prompt.OrderTriggers {} -> Response.OrderedTriggers answer
```
```haskell
  Prompt.OrderTriggers {} -> case response of
    Response.OrderedTriggers order -> Just order
    _ -> Nothing
```
```haskell
  -- CR 603.3b: the canonical order is always a legal answer, and is the least
  -- eventful fallback when a transcript runs short.
  Prompt.OrderTriggers _ _ sources -> map fromIntegral (take (length sources) [0 :: Int ..])
```

Add the same canonical-order arm to every other answerer: `Pawl.Support`'s `identityAnswer`, `castAnswer`, `aggressiveAnswer`, `playLandAnswer` and `randomAnswer` (the last wrapped in `pure`), `Pawl.GameSpec`'s two answerers, `Pawl.CastSpec`'s three, `Pawl.CopySpec`'s one, and `source/benchmark/Main.hs`'s `alwaysPass`, `castAnswer` and `fightAnswer`. In each, the line is:

```haskell
  Prompt.OrderTriggers _ _ sources -> map fromIntegral (take (length sources) [0 :: Int ..])
```

- [ ] **Step 4: Ask the question in `Engine`**

In `source/library/Pawl/Engine.hs`, replace `apnapOrder` with the APNAP player walk plus the per-controller ordering, and call it from `placePendingTriggers`:

```haskell
-- CR 101.4 / 603.3b: the players who control a pending trigger, active player
-- first and then the rest in turn order. Replaces M3f's apnapOrder, which sorted
-- the triggers directly -- with a within-controller ORDER now being a choice, the
-- pass has to group by controller first.
apnapPlayers :: GameState -> [PendingTrigger.PendingTrigger] -> [PlayerId]
apnapPlayers gs pending =
  let order = GameState.turnOrder gs
      active = GameState.activePlayer gs
      rotated = dropWhile (/= active) order ++ takeWhile (/= active) order
      controls pid = any (\pt -> PendingTrigger.controller pt == pid) pending
   in filter controls rotated

-- CR 603.3b: APNAP across controllers, and within one controller's set, that
-- player's chosen order. Asked only when they control two or more.
orderPending :: [PendingTrigger.PendingTrigger] -> Game [PendingTrigger.PendingTrigger]
orderPending pending = do
  gs <- State.get
  groups <- Monad.mapM (orderFor gs pending) (apnapPlayers gs pending)
  pure (concat groups)

orderFor :: GameState -> [PendingTrigger.PendingTrigger] -> PlayerId -> Game [PendingTrigger.PendingTrigger]
orderFor gs pending pid = do
  let mine = filter (\pt -> PendingTrigger.controller pt == pid) pending
  if length mine < 2
    then pure mine
    else do
      let decider = Decide.deciderFor pid gs
      answer <- Trans.lift (Program.prompt (Prompt.OrderTriggers decider pid (map PendingTrigger.source mine)))
      pure (permute mine answer)

-- Reject-not-repair, as payment already does: only a genuine permutation of the
-- offered indices is honoured. Anything else -- a short answer, a duplicate, an
-- out-of-range index -- leaves the canonical order standing rather than dropping
-- or duplicating a trigger.
permute :: [a] -> [Natural] -> [a]
permute xs order =
  let canonical = map fromIntegral (take (length xs) [0 :: Int ..])
      at i = case drop (fromIntegral i) xs of
        h : _ -> Just h
        [] -> Nothing
   in if List.sort order == canonical
        then Maybe.mapMaybe at order
        else xs
```

and in `placePendingTriggers`, replace `Monad.mapM_ placeOne (apnapOrder gs pending)` with:

```haskell
  ordered <- orderPending pending
  Monad.mapM_ placeOne ordered
```

Add `import Numeric.Natural (Natural)` to `Engine.hs`.

Note the stack semantics the prompt's comment states: `placeOne` conses onto the stack, so the trigger placed **last** is on top and resolves **first**. That is CR 603.3b read literally — the answer is the order they are *put on the stack*.

- [ ] **Step 5: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS. The two centerpiece cases must give **different** counter totals; if they agree, either the ordering is not being honoured or the count is not being taken at application time (CR 608.2h) — do not adjust the assertions.

- [ ] **Step 6: Commit**

```bash
git add source/library/Pawl/Type/Prompt.hs source/library/Pawl/Type/Response.hs source/library/Pawl/Engine.hs source/library/Pawl/Replay.hs source/test-suite/Pawl/TriggerSpec.hs source/test-suite/Pawl/ReplaySpec.hs source/test-suite/Pawl/Support.hs source/test-suite/Pawl/GameSpec.hs source/test-suite/Pawl/CastSpec.hs source/test-suite/Pawl/CopySpec.hs source/benchmark/Main.hs
hooky fix && git add -u && hooky run
git commit -m "feat(m4.5-p4): CR 603.3b trigger ordering is a prompt, retiring M3f's elision"
```

---

### Task 8: Intervening "if" → **Sarcomancy**

The last gate, and the cheapest: `StateCondition` already exists, so CR 603.4 and CR 608.2a are two calls to `Event.stateHolds` at two sites. The case that distinguishes an intervening "if" from a plain condition is the third one — a Zombie created **in response** makes the trigger resolve doing nothing — and it is the reason both sites exist.

**Files:**
- Create: `data/cards/sarcomancy.json`
- Modify: `source/library/Pawl/Type/TriggeredAbility.hs`
- Modify: `source/library/Pawl/Binding.hs`, `source/library/Pawl/Event.hs`, `source/library/Pawl/Engine.hs`, `source/library/Pawl/Stack.hs`, `source/library/Pawl/Codec.hs`
- Test: `source/test-suite/Pawl/TriggerSpec.hs`, `source/test-suite/Pawl/Cards.hs`, `source/test-suite/Pawl/CardSpec.hs`, `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Consumes: `StateCondition`, `Event.stateHolds` (Task 4); `Binding.setTriggerSource` (Task 3).
- Produces: `TriggeredAbility.intervening :: Maybe StateCondition`; `Pawl.Binding.you :: SlotName` (the text `"you"`), `Pawl.Binding.setYou :: PlayerId -> Map SlotName Binding -> Map SlotName Binding`; `Pawl.Cards.sarcomancyPrinting`.

- [ ] **Step 1: Write the failing tests**

Add to `source/test-suite/Pawl/TriggerSpec.hs` (and to the `tests` list):

```haskell
-- Sarcomancy {B} Enchantment: "When this enchantment enters, create a 2/2 black
-- Zombie creature token. At the beginning of your upkeep, if there are no Zombies
-- on the battlefield, this enchantment deals 1 damage to you."
interveningTests :: Cards.Cards -> Tasty.TestTree
interveningTests cards =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      beginUpkeep gs = Event.recordEvent (GameEvent.StepBegan upkeep S.alice) (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      zombies gs = filter (\oid -> Set.member Subtype.Zombie (Projection.subtypesOf oid gs)) (Set.toList (GameState.battlefield gs))
      -- Sarcomancy enters and its ETB resolves, so a Zombie token is out.
      withZombie =
        let (sarcId, gs0) = S.addCreature (Cards.sarcomancyPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            entered = ZoneChange.MkZoneChange sarcId Zone.Stack Zone.Battlefield
            gs1 = S.withEvent (GameEvent.Moved entered (Projection.project sarcId gs0)) gs0
         in (sarcId, resolveAll (settle gs1))
   in Tasty.testGroup
        "InterveningIf"
        [ HU.testCase "CR 603.6a the enters trigger makes a 2/2 black Zombie token" $
            let (_, after) = withZombie
             in case zombies after of
                  [tok] -> do
                    HU.assertEqual "2 power" (Just 2) (Projection.powerOf tok after)
                    HU.assertEqual "black" (Set.singleton Color.Black) (Projection.colorsOf tok after)
                  other -> HU.assertFailure ("expected exactly one Zombie token, got " <> show (length other)),
          -- CR 603.4: with the condition FALSE, the ability does not trigger AT ALL
          -- -- nothing reaches the stack.
          HU.testCase "CR 603.4 with a Zombie out, the upkeep ability does not trigger" $
            let (_, board) = withZombie
                atUpkeep = settle (beginUpkeep board)
             in do
                  HU.assertEqual "nothing on the stack" [] (GameState.stack atUpkeep)
                  HU.assertEqual "no life lost" (Just 20) (S.lifeOf S.alice atUpkeep),
          HU.testCase "CR 603.4 with no Zombie, it triggers and deals 1 to its controller" $
            let (_, board) = withZombie
                killed = case zombies board of
                  tok : _ -> Sba.checkStateBasedActions (Event.destroy tok board)
                  [] -> board
                after = resolveAll (settle (beginUpkeep killed))
             in HU.assertEqual "alice took 1" (Just 19) (S.lifeOf S.alice after),
          -- CR 608.2a: the case that distinguishes an intervening "if" from a plain
          -- condition. The ability triggered legitimately; a Zombie appearing in
          -- RESPONSE makes it do nothing on resolution.
          HU.testCase "CR 608.2a a Zombie made in response makes the trigger resolve doing nothing" $
            let (_, board) = withZombie
                killed = case zombies board of
                  tok : _ -> Sba.checkStateBasedActions (Event.destroy tok board)
                  [] -> board
                onStack = settle (beginUpkeep killed)
                -- The Zombie arrives under BOB's control, which is exactly the
                -- point: CR 603.4's clause is "no Zombies on the battlefield", not
                -- "no Zombies you control".
                responded = snd (S.addToken (zombieTokenOf cards) S.bob onStack)
                after = resolveAll responded
             in do
                  HU.assertBool "the trigger really was on the stack" (not (null (GameState.stack onStack)))
                  HU.assertEqual "no damage on resolution" (Just 20) (S.lifeOf S.alice after)
        ]

-- The 2/2 black Zombie Sarcomancy's own ETB mints, read back out of the card data
-- so the "in response" fixture makes the same object the card would.
zombieTokenOf :: Cards.Cards -> Card.Type.Card
zombieTokenOf cards =
  let created effect = case effect of
        Effect.Create _ card _ -> Just card
        _ -> Nothing
      abilityEffects = concatMap (Modal.allEffects . TriggeredAbility.modal) (Card.Type.triggeredAbilities (Printing.card (Cards.sarcomancyPrinting cards)))
   in case Maybe.mapMaybe created abilityEffects of
        card : _ -> card
        [] -> Printing.card (Cards.pikerPrinting cards)
```

Add imports to `TriggerSpec.hs`: `Pawl.Modal` as `Modal`, `Pawl.Type.Card` as `Card.Type`, `Pawl.Type.Color` as `Color`, `Pawl.Type.Effect` as `Effect`, `Pawl.Type.TriggeredAbility` as `TriggeredAbility`.

Add to `CardSpec.hs`'s `lintTests` list:

```haskell
      HU.testCase "the reserved you slot is never a declared target slot" $
        let offenders =
              filter
                (Map.member Binding.you . Card.allTargetSpecs . Printing.card)
                (Cards.allPrintings cards)
         in HU.assertEqual "no card names the you slot" [] (map (Card.Type.name . Printing.card) offenders),
```

Add to `CodecSpec.hs`'s "P4 runtime types" group:

```haskell
          HU.testCase "a TriggeredAbility with an intervening if round-trips" $
            let ability =
                  TriggeredAbility.MkTriggeredAbility
                    { TriggeredAbility.condition = TriggerCondition.SelfEnters,
                      TriggeredAbility.modal = Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1),
                      TriggeredAbility.intervening = Just (StateCondition.NoPermanentsOfSubtype Subtype.Zombie)
                    }
             in roundTrip "ta" Codec.triggeredAbilityToJson Codec.jsonToTriggeredAbility ability,
```

with `Pawl.Type.TriggeredAbility` imported.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `TriggeredAbility.intervening` is not a field; `Binding.you` and `Cards.sarcomancyPrinting` are not in scope.

- [ ] **Step 3: Add the field and the reserved "you" slot**

In `source/library/Pawl/Type/TriggeredAbility.hs`, add the field:

```haskell
    -- CR 603.4: an intervening "if" clause. The SAME predicate vocabulary a CR
    -- 603.8 state trigger uses, with two customers: checked when the trigger event
    -- occurs (the ability does not trigger AT ALL if it is false) and checked
    -- AGAIN on resolution (CR 608.2a removes the ability from the stack if it has
    -- become false). Nothing for every ability without one.
    intervening :: Maybe StateCondition
```

with `import Pawl.Type.StateCondition (StateCondition)`. Note this is the field a hand-built `MkTriggeredAbility` must now supply; the codebase builds them positionally in `Codec` only, which Step 5 updates.

In `source/library/Pawl/Binding.hs`, add beside `triggerSource`:

```haskell
-- CR 109.5: the reserved slot under which a triggered ability's CONTROLLER is
-- bound ("you"), so a targetless self-referential clause -- Sarcomancy's "deals 1
-- damage to you" -- is a slot read rather than a new opcode. No card's
-- targetSpecs may name it: "you" is not a target.
you :: SlotName
you = SlotName.MkSlotName (Text.pack "you")

setYou :: PlayerId -> Map SlotName Binding -> Map SlotName Binding
setYou pid = Map.insert you (Binding.empty {Binding.target = Just (Recipient.ToPlayer pid)})
```

with `import Pawl.Type.PlayerId (PlayerId)`.

In `source/library/Pawl/Engine.hs`, stamp it in `placeOne` alongside the source slot:

```haskell
Binding.setYou controller (Binding.setTriggerSource srcId (Map.union (PendingTrigger.bindings pending) (Binding.fromChoices chosen Map.empty Nothing chosenModes)))
```

- [ ] **Step 4: Check the condition at both sites**

In `source/library/Pawl/Event.hs`, filter the gather (CR 603.4):

```haskell
gatherTriggers :: [GameEvent] -> GameState -> ([PendingTrigger], Seq.Seq DelayedTrigger)
gatherTriggers events gs =
  let (fromDelayed, surviving) = delayedPending events gs
      all_ = eventTriggers events gs ++ stateTriggers gs ++ fromDelayed
   in (filter (interveningHolds gs) all_, surviving)

-- CR 603.4: "the ability doesn't trigger at all" when its intervening "if" is
-- false as the trigger event occurs. Checked HERE, at the gather -- not at
-- placement -- because "doesn't trigger" must be indistinguishable from "no
-- ability existed", including to the CR 117.5 settle loop's re-run flag.
interveningHolds :: GameState -> PendingTrigger -> Bool
interveningHolds gs pending =
  case TriggeredAbility.intervening (PendingTrigger.ability pending) of
    Nothing -> True
    Just cond -> stateHolds (PendingTrigger.controller pending) cond gs
```

In `source/library/Pawl/Stack.hs`, gate the `OfTrigger` branch (CR 608.2a):

```haskell
        Source.OfTrigger srcId ability ->
          -- CR 608.2a: an intervening "if" is checked AGAIN as the ability
          -- resolves; if it is no longer true the ability is removed from the
          -- stack and none of its effects happen. Object.owner is the ability's
          -- controller (Engine.placeOne stamps it), which is who "you" means.
          case TriggeredAbility.intervening ability of
            Just cond
              | not (Event.stateHolds (Object.owner obj) cond gs) ->
                  State.modify' (Resolve.cease oid)
            _ ->
              let chosen = Binding.modesOf (Object.bindings obj)
                  modal = TriggeredAbility.modal ability
               in Resolve.resolveEffects oid srcId (Modal.modesEffects chosen modal) (Modal.modesTargetSpecs chosen modal)
```

- [ ] **Step 5: Add the codec arm and the card**

In `source/library/Pawl/Codec.hs`, replace `triggeredAbilityToJson` / `jsonToTriggeredAbility`. The field is omitted when `Nothing`, so every card file that predates P4 renders byte-identically:

```haskell
triggeredAbilityToJson :: TriggeredAbility.TriggeredAbility CardT.Card -> Value
triggeredAbilityToJson ta =
  Object
    ( [ (Text.pack "condition", triggerConditionToJson (TriggeredAbility.condition ta)),
        (Text.pack "modal", modalToJson (TriggeredAbility.modal ta))
      ]
        ++ ( case TriggeredAbility.intervening ta of
               Nothing -> []
               Just c -> [(Text.pack "intervening", stateConditionToJson c)]
           )
    )

jsonToTriggeredAbility :: Value -> Either Text (TriggeredAbility.TriggeredAbility CardT.Card)
jsonToTriggeredAbility value = do
  ps <- Json.asObject value
  c <- Json.field (Text.pack "condition") ps >>= jsonToTriggerCondition
  m <- Json.field (Text.pack "modal") ps >>= jsonToModal
  i <- maybeFrom jsonToStateCondition (getOpt (Text.pack "intervening") ps)
  pure
    TriggeredAbility.MkTriggeredAbility
      { TriggeredAbility.condition = c,
        TriggeredAbility.modal = m,
        TriggeredAbility.intervening = i
      }
```

Create `data/cards/sarcomancy.json` (one line, plus the trailing newline):

```json
{"name":"Sarcomancy","manaCost":[{"type":"OfType","value":{"type":"Colored","value":{"type":"Black"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Enchantment"}],"subtypes":[]},"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[{"condition":{"type":"SelfEnters"},"modal":{"modes":[{"effects":[{"type":"Create","value":[{"type":"Literal","value":1},{"name":"Zombie","manaCost":null,"typeLine":{"supertypes":[],"types":[{"type":"Creature"}],"subtypes":[{"type":"Zombie"}]},"power":{"type":"Literal","value":2},"toughness":{"type":"Literal","value":2},"keywords":[],"staticAbilities":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"colorIndicator":[{"type":"Black"}]}]}],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}}},{"condition":{"type":"StepBegins","value":[{"type":"Beginning","value":{"type":"Upkeep"}},{"type":"ControllersTurn"}]},"modal":{"modes":[{"effects":[{"type":"DealDamage","value":["you",{"type":"Literal","value":1}]}],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"intervening":{"type":"NoPermanentsOfSubtype","value":{"type":"Zombie"}}}],"castingPermissions":[]}
```

In `source/test-suite/Pawl/Cards.hs`, register `sarcomancyPrinting` (load `"sarcomancy"`) and add it to `allPrintings`.

- [ ] **Step 6: Run the full suite, clean**

Run: `cabal clean && cabal build all --enable-tests --enable-benchmarks && cabal test && cabal bench`
Expected: PASS, warning-free. The clean build is the definitive warning check — incremental builds hide warnings from unchanged modules.

- [ ] **Step 7: Commit**

```bash
git add source/library/Pawl/Type/TriggeredAbility.hs source/library/Pawl/Binding.hs source/library/Pawl/Event.hs source/library/Pawl/Engine.hs source/library/Pawl/Stack.hs source/library/Pawl/Codec.hs data/cards/sarcomancy.json source/test-suite/Pawl/TriggerSpec.hs source/test-suite/Pawl/Cards.hs source/test-suite/Pawl/CardSpec.hs source/test-suite/Pawl/CodecSpec.hs
hooky fix && git add -u && hooky run
git commit -m "feat(m4.5-p4): CR 603.4/608.2a intervening if at both check sites (Sarcomancy)"
```

---

## Exit criterion

All four gate cards pass gameplay-level tests, including the centerpiece ordering-and-counting scenario; `GameState.zoneChanges` and `GameState.damageEvents` are gone and no reader clears the log; `grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-21-p4-event-history-triggers.md` reaches `0`; `cabal build all --enable-tests --enable-benchmarks` is warning-clean after `cabal clean`; `hooky run` passes.

Closing the phase (the `docs/workflow.md` step-5 session, not a task here): append the completion entry to `docs/progress.md`, **replace** `CLAUDE.md`'s status bullet, tick the umbrella spec's §3 row for P4 and update its §4 ordering note to say P6 and P7 are unblocked. git-bugs `b998924` (OfAbility LKI) and `6afb561` (M3f replacement seam) stay **open and unretired** — P4 builds the substrate the first sits on and does not touch the second; `c7a0077` (Quantity.Bound → SlotName) likewise stays open, since `CreaturesDiedThisTurn` folds the log and needs no binding slot.
