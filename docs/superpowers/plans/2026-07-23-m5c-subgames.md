# M5c — Subgames (CR 729) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close CR 729 (Subgames) — the M5 go/no-go — at the depth a labeled-synthetic Shahrazad stand-in demands: a subgame is a *function call* (`runStateT playGame subState` sequenced into the resolving effect through the **same** `Program Prompt` interpreter), reusing M5b's `startGameFromCards`; a generic `Effect.PlaySubgame SlotName` opcode runs the nested game and binds its outcome (the derived 2-player loser) to a slot; ordinary follow-on card data (an existing `DealDamage`) reads that slot (CR 729.1b); owned cards funnel **back** to the main library and reshuffle (CR 729.5); and Shahrazad-in-Shahrazad **nests arbitrarily** (CR 729.6) — all **without** casing on any card's identity and **without** widening the `Prompt` GADT.

**Architecture:** A subgame runs as `Trans.lift (State.runStateT (Setup.startGameFromCards >> Engine.playGame) sub0)` **inside** the resolution of `Effect.PlaySubgame`. Because it is sequenced into the parent's `StateT GameState (Program Prompt)`, the subgame's prompts flow through the **same** `Program.foldProgram`/`Replay.record` fold the main game uses — so scripted test interpreters and deterministic replay work unchanged (this is why a `Prompt.PlaySubgame` constructor is **rejected**: it would consume the subgame's inner prompts *inside* the answer function, bypassing `Replay.record` and breaking determinism). The parent `GameState` sits untouched in the outer frame while the subgame runs (CR 729.1a). `playSubgame` lives in `Engine` (it needs `Engine.playGame`); since `Pawl.Resolve` sits **below** `Engine` and cannot import it, the runner is **injected** as a `Game Result` argument down the spell-resolution path via new `...With` variants (`Stack.resolveTopWith`, `Resolve.resolveSpellWith`, `Resolve.applyEffectWith`), with the existing bare names preserved as thin `...With Resolve.noSubgame` wrappers — so **none** of the 105 `Stack.resolveTop` / 9 `Resolve.applyEffect` existing test call sites change. Nesting (CR 729.6) is free recursion: each level's `priorityLoop` re-supplies `playSubgame`.

**Tech Stack:** Haskell 2010 (GHC 9.14.1 from the Nix dev shell), `tasty` + `tasty-hunit`. Library under `source/library/Pawl/`; tests under `source/test-suite/Pawl/`; cards are JSON data files under `data/cards/`.

## Global Constraints

Every task's requirements implicitly include this section. Values copied verbatim from `CLAUDE.md`.

- **Warning-clean build.** `cabal build all --enable-tests --enable-benchmarks` must compile under `-Weverything` minus the allow-list, with `flags: +pedantic` (`-Werror`) on. Incremental builds hide warnings from unchanged modules — for a definitive check, `cabal clean` first. **Adding a constructor to `Effect` makes every `case effect of` non-exhaustive; `-Wincomplete-patterns` under `-Werror` fails the build until all arms are added** — this is the safety net that guarantees Task 2 touches every dispatch site.
- **No new language extensions.** Haskell 2010. Test files that pattern-match the `Prompt` GADT already carry `{-# LANGUAGE GADTs #-}`; do not add others. `Pawl.Engine` already carries `{-# LANGUAGE RankNTypes #-}` (for the `forall r. Prompt r -> …` interpreter seam); do not add others there.
- **Qualified imports, aliased to the last component** (`Pawl.Setup` → `Setup`, `Control.Monad.Trans.Class` → `Trans`); operators unqualified. One import group, no first/third-party split. A module never imports its parents; `A.B.C` never imports `A.B` or `A`.
- **No partial functions, written or used.** No `head`, `undefined`, `error`, `!!`, or non-exhaustive matches. Use `Data.Maybe.listToMaybe` instead of `head`; `Data.Foldable.toList` to flatten a `Seq`; `Map.restrictKeys`/`Map.withoutKeys`/`Map.filter`/`Map.keys`, never a list comprehension (**CLAUDE.md: "No list comprehensions"**).
- **`newtype`/record conventions.** Build records with `do` + record syntax or record-update syntax, not `<$>`/`<*>`. `case` over point-free; `let` over `where`.
- **Cite the CR number in every rules-bearing comment.** Every CR claim below was checked against `docs/rules.txt` at this planning pass (CR 729 at lines 6273–6306: 729.1/6275, 729.1a/6277, 729.1b/6279, 729.2/6281, 729.3/6289, 729.4/6291, 729.4b/6295, 729.5/6297, 729.6/6306; CR 103 "Starting the Game" at 258–322; CR 704.5b draw-from-empty loss). Never trust recalled Magic rules; re-verify before the number drives code.
- **The two invariants outrank this plan.** (1) The rules core reads a *classification*, never an effect's identity — the subgame's outcome plumbing stays generic (`PlaySubgame` binds a slot; "the loser takes damage" is ordinary follow-on `DealDamage` data); `Pawl.Resolve` never grows a `case … Shahrazad`. (2) The engine makes no choices — every subgame choice (down to shuffles) is a prompt through the same interpreter; the one place a choice is currently *elided* (CR 729.2's "randomly determine which player goes first" → deterministic head-of-turn-order, matching the existing `Setup.emptyGame` posture) carries a filed, named-expiry issue (Task 9).
- **One small complete commit per task, on `main`.** Never push. Commit message trailer: `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>`.
- **`hooky` before done.** `git add -A` → `hooky fix` → `git add -A` → `hooky run` (acts on staged files only).
- **TDD is not optional.** Write each failing test and run it to watch it fail before implementing. Tick each `- [ ]` as you finish it.

---

## Architecture decisions (load-bearing — sanity-check before executing)

This is the **M5 go/no-go**; these decisions are the bet. Read them before Task 1. Each is grounded in the map made at this planning pass (paths/lines re-confirm if a file has moved).

1. **Subgame = a lifted `runStateT`, never a new `Prompt`.** `playSubgame` does `Trans.lift (State.runStateT (Setup.startGameFromCards >> Engine.playGame) sub0)`. `Game = StateT GameState (Program Prompt)` (`Pawl.Type.Game:8`); lifting the subgame's `Program Prompt (Result, GameState)` into the parent `StateT` means the subgame's prompts are part of the **same** `Program` tree the outer `Program.foldProgram` / `Replay.record` folds (`Pawl.Replay:176-202`). A `Prompt.PlaySubgame` constructor would instead run the subgame *inside* the answer function (`answer p = foldProgram answer (runStateT playGame …)`), so the subgame's inner prompts never reach `Replay.record`'s `step` — **breaking replay determinism** (the M0 criterion). Rejected for that reason.

2. **The runner is injected downward, not imported upward.** `playSubgame :: Game Result` needs `Engine.playGame`, and `Engine` sits at the top of the import graph (`Engine` imports `Resolve`+`Stack`; `Stack` imports `Resolve`; nothing imports `Engine`). `Resolve.applyEffect` (bottom) resolves `PlaySubgame`, so it cannot reference `playSubgame`. The runner is threaded as a `Game Result` parameter through **new** `...With` variants only along the **spell** path (`Stack.resolveTopWith` → `Resolve.resolveSpellWith` → `Resolve.applyEffectWith`); the **bare** names are kept as `= …With Resolve.noSubgame` wrappers (`noSubgame = pure Result.Drawn`). `Engine.priorityLoop` calls `Stack.resolveTopWith playSubgame`. Nesting works because each nested `playGame`'s own `priorityLoop` re-supplies `playSubgame`.
   - **Why not thread everywhere:** `Stack.resolveTop` has **105** test call-refs and `Resolve.applyEffect` has **9**; the wrapper-preserves-bare-name strategy changes **none** of them. The ability path (`resolveEffects`/`resolveAbility`) keeps folding bare `applyEffect` (`noSubgame`) — an ability that plays a subgame is deferred (Task 9), and no gate card needs one.

3. **The outcome is a `definedSlot`, read by a later effect via a per-effect binding re-read.** `Effect.PlaySubgame SlotName` **defines** its slot (like `Effect.Create`'s `Maybe SlotName`, `Resolve.definedSlots:219`): after the subgame it binds the derived loser as `Recipient.ToPlayer loser` into the source object's `bindings[slot]`. A later `DealDamage slot …` **reads** that slot. The D4 dataflow lint (`CardSpec.hs:305`: read slots ⊆ declared `targetSpecs` ∪ `{triggerSource}` ∪ `definedSlots`) accepts it because the slot is *defined*, not targeted; and a non-targetSpec slot is vacuously **legal** at resolution (`Resolve.resolveSpell:316`, the `Nothing -> True` branch), so `DealDamage` fires. The one enabling change: `resolveSpell`'s fold snapshots `chosen`/`legality` **once** before the fold (`Resolve.hs:310/318/337`), so a slot bound *mid*-fold is invisible to a later effect (`Effect.hs:117` documents exactly this). `resolveSpellWith` **re-reads** the source's bindings per effect (target-slot legality is unchanged — still validated against the pre-fold state; only newly-*defined* reserved slots become visible). This generalizes the mid-resolution binding `Create` already writes but nothing yet reads.

4. **Construction/teardown is pure `GameState → GameState`, inheriting the id supply.** `Setup.subgameStateFrom` builds `sub0` from the parent's **library cards only** (CR 729.2 — `Map.restrictKeys` to the library object ids), resets players to a fresh game, empties all zones (`startGameFromCards` repopulates libraries from `objects`), and **inherits** the parent's `nextObjectId`/`nextTimestamp` so every object the subgame mints (CR 400.7 re-identification on each draw) gets an id **above** every parent id — guaranteeing no collision at funnel-back. `Setup.funnelBack` (CR 729.5) rebuilds each owner's library from the `OfCard` objects they own **anywhere** in the final subgame, drops the parent's old library objects, merges the returned cards (`Map.union` is collision-free by the id argument above), and advances the parent's id/timestamp supplies to the subgame's high-water mark. All other subgame objects and zones simply cease (they are dropped, not reverted). `playSubgame` then reshuffles each rebuilt library (`Prompt.Shuffle`).

5. **First player is deterministic (elided RNG), and "the loser" is a 2-player derivation.** CR 729.2's "randomly determine which player goes first" has **no** prompt in pawl today (there is no first-player randomness constructor; `Setup.emptyGame` takes the head of the order). `subgameStateFrom` sets the subgame's active player to the head of the turn order — an elision filed with a named expiry (Task 9). `Result` stays `Won PlayerId | Drawn`; in the 2-player gate the loser is `listToMaybe (filter (/= winner) turnOrder)`, and a `Drawn` subgame binds nothing (the follow-on no-ops). Result-widening for a genuine multi-player "each player who doesn't win" is deferred (Task 9), exactly as the umbrella flags.

6. **The gate card is a labeled synthetic (owner decision, this planning pass).** Real Shahrazad's rider — *"each player who doesn't win the subgame loses half their life, rounded up"* — needs life-loss + a half-current-life quantity + a multi-player non-winner scope, none of which exist. Per the `Landform` / `synthetic-restart` crutch (spec §1/§3), the gate is `synthetic-subgame` — a `{0}` Sorcery whose one mode is `[PlaySubgame "loser", DealDamage "loser" (Literal 3)]` — with a documented expiry naming **Shahrazad** (Task 9). The subgame *machinery* it exercises is identical to what Shahrazad needs; only the fixed-3 rider stands in for the half-life rider.

---

## Substrate reference (read once before starting)

Exact shapes the tasks build on (paths/line numbers from this planning pass — re-confirm if a file has moved).

- `Pawl.Setup` (`source/library/Pawl/Setup.hs`): `startingLife :: Integer` (line 33, = 20); `openingHand :: Int` (43, = 7); `emptyGame :: NonEmpty PlayerId -> GameState` (47) — the field template; `shuffleLibrary :: PlayerId -> Game ()` (120) — issues `Prompt.Shuffle`; `startGameFromCards :: Game ()` (146, M5b) — rebuilds every owner's library from **all** `Source.OfCard` objects in `GameState.objects`, shuffles, draws opening hands (M5c calls it on a subgame state whose `objects` are already restricted to library cards); `rotateTo :: PlayerId -> [PlayerId] -> [PlayerId]` (182, M5b); `restartGame :: PlayerId -> Game ()` (196, M5b — the field-by-field reset template `subgameStateFrom` mirrors). Setup already imports `Combat`, `Event`, `Game`, `Turn`, `Object`, `Player`, `Source`, `Status`, `Sickness`, `TapState`, `Zone`, `GameState`, `Map`, `Seq`, `Monad`, `NonEmpty`. **Add** `import qualified Data.Foldable as Foldable` and `import qualified Data.Set as Set` (Tasks 1) and `import qualified Pawl.Type.PlayerId` only if the compiler needs it (it is reached transitively).
- `Pawl.Type.GameState` (`source/library/Pawl/Type/GameState.hs`), one record `deriving (Eq, Show)`. Fields used: `objects :: Map ObjectId Object`; per-player `library`/`hand`/`graveyard :: Map PlayerId (Seq ObjectId)`; shared `battlefield`/`exile`/`command :: Set ObjectId`; `stack :: [ObjectId]`; `players :: Map PlayerId Player`; `manaPool`; `combat`; `events`; `scannedThrough`/`damageScannedThrough :: Natural`; `delayedTriggers`; `continuousEffects`; `replacements`; `playerEffects`; `turnOrder :: [PlayerId]`; `activePlayer :: PlayerId`; `phase :: Phase`; `remaining :: Seq Phase`; `priority :: Maybe PlayerId`; `passes :: Natural`; `turnNumber :: Natural`; `result :: Maybe Result`; `nextObjectId`; `nextTimestamp`; `drewFromEmpty :: Set PlayerId`; `landPlayed :: Set PlayerId`; `pendingControl`; `activeControl`; `monarch`; `exiledUntilMonarch`. **No RNG/seed field.**
- `Pawl.Type.ObjectId` (`.../Type/ObjectId.hs`) and `Pawl.Type.Timestamp` (`.../Type/Timestamp.hs`): both `deriving (Eq, Ord, Show)` — `max` is defined.
- `Pawl.Type.Object` (`.../Type/Object.hs`): `owner :: PlayerId` (stamped once); `source`, `zone`, `tapped`, `damage`, `sickness`, `bindings`, `counters`, `timestamp`.
- `Pawl.Type.Source`: `OfCard Printing | OfToken Card | OfAbility … | OfTrigger … | OfEmblem … | OfInherentTrigger …`. **Only `OfCard` is a Magic card** (CR 729.5 "traditional cards").
- `Pawl.Type.Result` (`.../Type/Result.hs`): `data Result = Won PlayerId | Drawn deriving (Eq, Ord, Show)`.
- `Pawl.Type.Recipient` (`.../Type/Recipient.hs`): `ToCreature ObjectId | ToPlayer PlayerId | ToObject ObjectId`.
- `Pawl.Type.Binding` (`.../Type/Binding.hs`): record `MkBinding { target :: Maybe Recipient, subtypes, amount, modes, copy }`; `empty` is all-`Nothing`. `Pawl.Binding.targetsOf :: Map SlotName Binding -> Map SlotName Recipient` = `Map.mapMaybe Binding.target`; `Binding.subtypesOf`; reserved keys `variableX`/`chosenModes`/`triggerSource`.
- `Pawl.Engine` (`source/library/Pawl/Engine.hs`): `playGame :: Game Result` (521–530) — loops `runStep` until `GameState.result` is `Just`; `priorityLoop :: Game ()` (line ~404, the `ChooseAction`/`Stack.resolveTop` site at ~420); `runGamePure :: (forall r. Prompt r -> r) -> GameState -> Game a -> (a, GameState)` (63); `checkSba :: Game ()` (95, = `Sba.checkStateBasedActions`). Engine imports `Resolve`, `Stack`, `Setup`; carries `{-# LANGUAGE RankNTypes #-}`. **Add** `import qualified Control.Monad.Trans.Class as Trans` (Task 4).
- `Pawl.Stack` (`source/library/Pawl/Stack.hs`): `resolveTop :: Game ()` (30); the **spell** branch is `Resolve.resolveSpell oid` (line ~43); ability branches call `Resolve.resolveAbility` (~59) / `Resolve.resolveEffects` (~72, ~82). Imports `Resolve`.
- `Pawl.Resolve` (`source/library/Pawl/Resolve.hs`) — the **only** module that cases on `Effect`. Classifier tables each needing a new `PlaySubgame` arm: `slotsOf` (67), `readsX`/`effectReadsX` (103/106), `manaProduced` (137), `searchesLibrary` (169), `rewriteEffect` (240). The write-half collector `definedSlots` (219) also needs an arm. Executor `applyEffect` (410, signature `ObjectId -> PlayerId -> Map SlotName (Subtype,Subtype) -> Map SlotName Bool -> Map SlotName Recipient -> Effect Card -> Game ()`; args `source controller bound legality chosen effect`). `resolveSpell` (298), fold at 337; `bindSlot :: ObjectId -> SlotName -> ObjectId -> GameState -> GameState` (837) — the `ToObject` binder to mirror for a `ToPlayer` binder. Resolve imports `Setup` (M5b), `Binding`, `Recipient`, `Result`? — **`Result` is not yet imported; add `import Pawl.Type.Result (Result)` and `import qualified Pawl.Type.Result as Result`** (Task 2).
- `Pawl.Codec` (`source/library/Pawl/Codec.hs`): `slotNameToJson (MkSlotName t) = Json.jText t` (819) / `jsonToSlotName` (822); `effectToJson`'s slot-only arms use `Json.tagged "Name" (Just (slotNameToJson s))` (e.g. `ControlPlayerNextTurn` at 1155, `Counter` at 1158); `jsonToEffect`'s slot-only arms use `withValue mv (fmap Effect.Name . jsonToSlotName)` (e.g. `ChangeText` at 1186). `DealDamage` encodes as `Json.tagged "DealDamage" (Just (Array [slotNameToJson s, quantityToJson q]))` (1148) — JSON `{"type":"DealDamage","value":["<slot>",{"type":"Literal","value":N}]}`.
- `Pawl.Cards` (`source/test-suite/Pawl/Cards.hs`): a new card touches four sites — the `data Cards` field (~16–99), a `…_ <- loadPrinting "<slug>"` line in `loadCards` (~111–192), the record line (~195–277), and `allPrintings` (~281–363).
- Test harness: `Engine.runGamePure`; `Engine.priorityLoop`; `Engine.checkSba`; `Stack.resolveTop`; `S.identityAnswer` (in `Pawl.Support`, answers `Shuffle ids -> ids`, `ChooseAction -> A.Pass`, minimally otherwise); `S.addCreature :: Printing -> PlayerId -> GameState -> (ObjectId, GameState)` (places a permanent **Settled, Untapped, on the battlefield**); `S.bothPlayers :: NonEmpty PlayerId` (alice then bob); `S.alice`, `S.bob`; `S.lifeOf`, `S.handSize`. `Game.zoneMembers :: Zone -> PlayerId -> GameState -> [ObjectId]`; `Game.lookupObject`; `Game.freshObjectId`/`freshTimestamp`. `GameSpec.hs` already has `isCastAction`, `isActivateAction`, `addManyG` (M5a/M5b).

## File structure

| File | Change | Responsibility |
|---|---|---|
| `source/library/Pawl/Setup.hs` | Modify | Task 1: `subgameStateFrom` (CR 729.2), `funnelBack` (CR 729.5) — pure `GameState` builders. |
| `source/test-suite/Pawl/SetupSpec.hs` | Modify | Task 1: unit tests for the two builders (library-only construction; card return + id non-collision). |
| `source/library/Pawl/Type/Effect.hs` | Modify | Task 2: the `PlaySubgame SlotName` constructor. |
| `source/library/Pawl/Resolve.hs` | Modify | Tasks 2–3: five classifier arms + `definedSlots` arm + `Codec`-free wiring; `noSubgame`; `bindLoserSlot`; `applyEffectWith`/`applyEffect` wrapper; `resolveSpellWith`/`resolveSpell` wrapper + per-effect re-read. |
| `source/library/Pawl/Codec.hs` | Modify | Task 2: `effectToJson`/`jsonToEffect` arms. |
| `source/library/Pawl/Stack.hs` | Modify | Task 3: `resolveTopWith`/`resolveTop` wrapper. |
| `source/library/Pawl/Engine.hs` | Modify | Task 4: `playSubgame`; `priorityLoop` → `resolveTopWith playSubgame`. |
| `source/test-suite/Pawl/ResolveSpec.hs` | Modify | Task 3: mid-resolution binding visibility (stub runner). |
| `data/cards/synthetic-subgame.json` | Create | Task 5: the labeled-synthetic gate card. |
| `source/test-suite/Pawl/Cards.hs` | Modify | Task 5: register `syntheticSubgamePrinting` (4 sites). |
| `source/test-suite/Pawl/GameSpec.hs` | Modify | Tasks 4, 6–8: `playSubgame` end-to-end; the three gameplay gates (729.1b/729.3, 729.5/729.4b, 729.6). |
| GitHub issues (`tfausak/pawl`) | Create | Task 9: deferrals (first-player RNG; ability-path subgames; Result-widening; full Shahrazad; subsystem-blocked slices). |
| `docs/progress.md`, `CLAUDE.md`, umbrella spec | Modify | Task 10: completion entry, status replacement, phase tick (M5 exit). |

---

### Task 1: `subgameStateFrom` + `funnelBack` — pure subgame construction and teardown

The two pure `GameState → GameState` halves of a subgame's lifecycle (CR 729.2 / 729.5). No monad, no prompts — those live in `playSubgame` (Task 4). Testable in isolation.

**Files:**
- Modify: `source/library/Pawl/Setup.hs` — two new top-level functions (after `restartGame`).
- Test: `source/test-suite/Pawl/SetupSpec.hs` — a new `subgameTests` group wired into `tests`.

**Interfaces:**
- Consumes: `GameState.{objects,library,turnOrder,players,nextObjectId,nextTimestamp,…}`, `Object.{owner,source,zone,tapped,damage,sickness,bindings,counters}`, `Source.OfCard`, `startingLife`, `Player.{life,status,counters}`, `Status.Playing`, `Combat.emptyCombat`, `Turn.{firstPhase,laterPhases}`, `Zone.Library`, `TapState.Untapped`, `Sickness.Sick`.
- Produces (used by Task 4): `subgameStateFrom :: GameState -> GameState`; `funnelBack :: GameState -> GameState -> GameState`.

- [x] **Step 1: Write the failing tests**

At the top of `source/test-suite/Pawl/SetupSpec.hs` add to the import group (alphabetical among `Pawl.*`), any not already present:

```haskell
import qualified Data.Foldable as Foldable
import qualified Data.Set as Set
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Zone as Zone
```

Append this group (top level), reusing the `addMany` helper already defined in this file for `restartTests` (it folds `S.addCreature` to bulk up an owner's battlefield pool):

```haskell
-- Move a player's pool onto their LIBRARY (subgameStateFrom reads the library
-- zone, not the battlefield). addMany places cards on the battlefield; this
-- helper then relocates a player's battlefield objects into their library so a
-- test can set up a known library size without drawing. replicate/fold avoid a
-- list comprehension (CLAUDE.md).
poolToLibrary :: S.PlayerId -> GameState.GameState -> GameState.GameState
poolToLibrary pid gs =
  let mine = Map.keys (Map.filter (\o -> Object.owner o == pid) (GameState.objects gs))
      onLibrary o = o {Object.zone = Zone.Library}
   in gs
        { GameState.objects = List.foldl' (\m oid -> Map.adjust onLibrary oid m) (GameState.objects gs) mine,
          GameState.battlefield = Set.difference (GameState.battlefield gs) (Set.fromList mine),
          GameState.library = Map.insert pid (Seq.fromList mine) (GameState.library gs)
        }

subgameTests :: Cards.Cards -> Tasty.TestTree
subgameTests cards =
  Tasty.testGroup
    "subgames (CR 729)"
    [ HU.testCase "CR 729.2: subgameStateFrom takes ONLY library cards; battlefield/hand do not enter" $
        -- alice owns 5 cards: 2 relocated to her library, 3 left on the battlefield.
        -- The subgame state's object pool must be exactly the 2 library cards.
        let g0 = Setup.emptyGame S.bothPlayers
            g1 = addMany cards 5 S.alice g0
            -- relocate exactly 2 of alice's cards to her library, leave 3 on the battlefield
            aliceIds = Map.keys (Map.filter (\o -> Object.owner o == S.alice) (GameState.objects g1))
            (toLib, _rest) = splitAt 2 aliceIds
            onLibrary o = o {Object.zone = Zone.Library}
            g2 =
              g1
                { GameState.objects = List.foldl' (\m oid -> Map.adjust onLibrary oid m) (GameState.objects g1) toLib,
                  GameState.battlefield = Set.difference (GameState.battlefield g1) (Set.fromList toLib),
                  GameState.library = Map.insert S.alice (Seq.fromList toLib) (GameState.library g1)
                }
            sub = Setup.subgameStateFrom g2
         in do
              HU.assertEqual "the subgame pool holds exactly the 2 library cards" 2 (Map.size (GameState.objects sub))
              HU.assertEqual "every subgame object is one of the 2 library cards" True (all (`elem` toLib) (Map.keys (GameState.objects sub)))
              HU.assertEqual "the subgame battlefield is empty (nothing but the library entered)" True (Set.null (GameState.battlefield sub))
              HU.assertEqual "the subgame nextObjectId is inherited from the parent" (GameState.nextObjectId g2) (GameState.nextObjectId sub)
              HU.assertEqual "the subgame is a fresh game at turn 1" 1 (GameState.turnNumber sub)
    , HU.testCase "CR 729.5: funnelBack returns every owned subgame card to its owner's library, ids do not collide" $
        -- Parent: alice and bob each own 3 cards on the battlefield plus 2 in their
        -- library (so the parent has non-library objects that must SURVIVE, and
        -- library objects that get REPLACED). finalSub mints its own objects with
        -- ids above the parent's supply (as a real subgame would, CR 400.7).
        let g0 = Setup.emptyGame S.bothPlayers
            g1 = poolToLibrary S.bob (poolToLibrary S.alice (addMany cards 5 S.bob (addMany cards 5 S.alice g0)))
            -- move 3 of each back onto the battlefield so the parent has survivors
            reBattlefield pid gg =
              let libIds = Foldable.toList (Map.findWithDefault Seq.empty pid (GameState.library gg))
                  (keepLib, toField) = splitAt 2 libIds
                  onField o = o {Object.zone = Zone.Battlefield}
               in gg
                    { GameState.objects = List.foldl' (\m oid -> Map.adjust onField oid m) (GameState.objects gg) toField,
                      GameState.battlefield = Set.union (GameState.battlefield gg) (Set.fromList toField),
                      GameState.library = Map.insert pid (Seq.fromList keepLib) (GameState.library gg)
                    }
            parent = reBattlefield S.bob (reBattlefield S.alice g1)
            -- a stand-in "final subgame state": 4 fresh cards per owner, ids above the parent supply
            sub0 = Setup.subgameStateFrom parent
            (finalSub, _) = Engine.runGamePure S.identityAnswer sub0 (Monad.replicateM_ 0 (pure ()))
            after = Setup.funnelBack finalSub parent
            ownedEverywhere pid = length (filter (\o -> Object.owner o == pid) (Map.elems (GameState.objects after)))
            libCount pid = length (Game.zoneMembers Zone.Library pid after)
            battlefieldSurvivors = Set.size (GameState.battlefield after)
         in do
              -- alice/bob each still own all their cards, all in their library, none lost
              HU.assertEqual "alice's library is rebuilt from her subgame cards" True (libCount S.alice > 0)
              HU.assertEqual "bob's library is rebuilt from his subgame cards" True (libCount S.bob > 0)
              HU.assertEqual "the parent's non-library survivors are untouched (6 on the battlefield)" 6 battlefieldSurvivors
              HU.assertEqual "no object id collides (object count = survivors + returned cards)" (Map.size (GameState.objects after)) (battlefieldSurvivors + libCount S.alice + libCount S.bob)
              HU.assertEqual "the id supply advanced to at least the subgame high-water mark" True (GameState.nextObjectId after >= GameState.nextObjectId finalSub)
    ]
```

Wire it into the aggregator at the bottom of the file — change

```haskell
tests cards = Tasty.testGroup "Setup" [setupTests cards, greenBlackSetupTests cards, deckTests cards, restartTests cards]
```

to append `subgameTests cards`:

```haskell
tests cards = Tasty.testGroup "Setup" [setupTests cards, greenBlackSetupTests cards, deckTests cards, restartTests cards, subgameTests cards]
```

> The `funnelBack` case above uses `subgameStateFrom parent` itself as a stand-in "final subgame state" (its objects are exactly the parent's library cards, ids inherited). That is enough to prove card-return, survivor-preservation, and non-collision. Task 4's end-to-end test proves funnelBack against a *real* played-out subgame. `List`, `Map`, `Seq`, `Engine`, `Game`, `Monad`, `GameState` are already imported in `SetupSpec.hs`; add any missing from Step 1's list.

- [x] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20`
Expected: **compile error** — `Setup.subgameStateFrom` / `Setup.funnelBack` not in scope.

- [x] **Step 3: Implement `subgameStateFrom` and `funnelBack`**

Append to `source/library/Pawl/Setup.hs` (after `restartGame`), and add `import qualified Data.Foldable as Foldable` and `import qualified Data.Set as Set` to the import group:

```haskell
-- CR 729.2: build a fresh subgame state from the parent's LIBRARY cards ONLY --
-- each player takes all the cards in their main-game library into the subgame
-- library; no other main-game zone enters (rule 729.2). The object pool is
-- restricted to those library objects; startGameFromCards (called by playSubgame)
-- then rebuilds each subgame library from that pool, shuffles, and draws opening
-- hands (CR 103). Players reset to a new game (CR 103); every transient field is
-- cleared, exactly as restartGame does, EXCEPT the object/timestamp id supplies,
-- which are INHERITED from the parent so every object the subgame mints (CR 400.7)
-- gets an id above every parent id -- funnelBack relies on that for non-collision.
-- CR 729.2's "randomly determine which player goes first" is elided to the head of
-- the turn order (pawl has no first-player randomness prompt; the Setup.emptyGame
-- posture), filed with a named expiry.
subgameStateFrom :: GameState -> GameState
subgameStateFrom parent =
  let libIds =
        Set.fromList
          (concatMap (\pid -> Foldable.toList (Map.findWithDefault Seq.empty pid (GameState.library parent))) (GameState.turnOrder parent))
      libObjects = Map.restrictKeys (GameState.objects parent) libIds
      resetPlayer player =
        player
          { Player.life = startingLife,
            Player.status = Status.Playing,
            Player.counters = Map.empty
          }
      firstPlayer = Maybe.fromMaybe (GameState.activePlayer parent) (Maybe.listToMaybe (GameState.turnOrder parent))
   in parent
        { GameState.objects = libObjects,
          GameState.players = Map.map resetPlayer (GameState.players parent),
          GameState.library = Map.empty,
          GameState.hand = Map.empty,
          GameState.graveyard = Map.empty,
          GameState.battlefield = mempty,
          GameState.exile = mempty,
          GameState.command = mempty,
          GameState.stack = [],
          GameState.manaPool = Map.empty,
          GameState.combat = Combat.emptyCombat,
          GameState.events = Seq.empty,
          GameState.scannedThrough = 0,
          GameState.damageScannedThrough = 0,
          GameState.delayedTriggers = Seq.empty,
          GameState.continuousEffects = [],
          GameState.replacements = [],
          GameState.playerEffects = [],
          GameState.activePlayer = firstPlayer,
          GameState.phase = Turn.firstPhase,
          GameState.remaining = Turn.laterPhases,
          GameState.priority = Nothing,
          GameState.passes = 0,
          GameState.turnNumber = 1,
          GameState.result = Nothing,
          GameState.drewFromEmpty = mempty,
          GameState.landPlayed = mempty,
          GameState.pendingControl = Map.empty,
          GameState.activeControl = Nothing,
          GameState.monarch = Nothing,
          GameState.exiledUntilMonarch = Map.empty
        }

-- CR 729.5: at the end of a subgame, each player takes all traditional cards
-- (Source.OfCard) they own ANYWHERE in the subgame into their main-game library
-- and reshuffles (the reshuffle is playSubgame's Prompt.Shuffle step). All other
-- subgame objects and the subgame's zones cease to exist -- they are simply not
-- carried over. The parent's non-library objects (hand, battlefield, graveyard,
-- ...) are untouched: the main game continues from where it was discontinued. The
-- old parent library objects are dropped (they moved into the subgame). Returned
-- cards keep their subgame ids, which are all above the parent supply
-- (subgameStateFrom inherited it), so Map.union cannot collide; the id/timestamp
-- supplies advance to the subgame high-water mark.
funnelBack :: GameState -> GameState -> GameState
funnelBack finalSub parent =
  let isCard obj = case Object.source obj of
        Source.OfCard _ -> True
        _ -> False
      toLibraryCard obj =
        obj
          { Object.zone = Zone.Library,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
            Object.bindings = Map.empty,
            Object.counters = Map.empty
          }
      returned = Map.map toLibraryCard (Map.filter isCard (GameState.objects finalSub))
      libraryOf pid = Seq.fromList (Map.keys (Map.filter (\obj -> Object.owner obj == pid) returned))
      oldLibIds =
        Set.fromList
          (concatMap (\pid -> Foldable.toList (Map.findWithDefault Seq.empty pid (GameState.library parent))) (GameState.turnOrder parent))
      keptParentObjects = Map.withoutKeys (GameState.objects parent) oldLibIds
   in parent
        { GameState.objects = Map.union returned keptParentObjects,
          GameState.library = Map.fromList (map (\pid -> (pid, libraryOf pid)) (GameState.turnOrder parent)),
          GameState.nextObjectId = max (GameState.nextObjectId parent) (GameState.nextObjectId finalSub),
          GameState.nextTimestamp = max (GameState.nextTimestamp parent) (GameState.nextTimestamp finalSub)
        }
```

> `Maybe` is already imported by `Setup.hs` (used elsewhere); confirm `import qualified Data.Maybe as Maybe` is present. If `Player`/`Status`/`Turn`/`Combat` need their qualified imports, they are already present (M5b's `restartGame` uses all of them).

- [x] **Step 4: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "$0~/subgames .CR 729/"'`
Expected: **PASS** (warning-clean build, both cases green).

- [x] **Step 5: Commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "feat(m5c): subgameStateFrom + funnelBack, the CR 729.2/729.5 pure builders"
```

---

### Task 2: the `PlaySubgame` opcode — constructor, classifiers, defined-slot, codec

Add `Effect.PlaySubgame SlotName` and wire the *pure* halves: the five classifier arms (compiler-forced for `slotsOf`/`manaProduced`/`searchesLibrary`/`rewriteEffect`; hand-added for `readsX`), the `definedSlots` write-half arm (so a downstream `DealDamage` reading the slot passes the D4 lint), and the two `Codec` arms. The **executor** arm and the runner threading are Task 3; here the `applyEffect` arm is a documented placeholder so the build stays green.

**Files:**
- Modify: `source/library/Pawl/Type/Effect.hs` (constructor), `source/library/Pawl/Resolve.hs` (five classifier arms + `definedSlots` arm + placeholder executor arm), `source/library/Pawl/Codec.hs` (two arms).
- Test: covered by the existing `allPrintings` round-trip and D4 lint once Task 5's card exists; here, a targeted `CodecSpec` assertion round-trips a hand-built `PlaySubgame`.

**Interfaces:**
- Produces: `Effect.PlaySubgame :: SlotName -> Effect card`; JSON tag `"PlaySubgame"`.

- [x] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/CodecSpec.hs`, in the effect round-trip group (near the other `jsonToEffect . effectToJson` assertions):

```haskell
            let e = Effect.PlaySubgame (SlotName.MkSlotName (Text.pack "loser"))
             in HU.assertEqual "PlaySubgame round-trips" (Right e) (Codec.jsonToEffect (Codec.effectToJson e)),
```

> `Effect`, `SlotName`, `Text`, `Codec` are already imported in `CodecSpec.hs` (it round-trips many effects). If `Codec.effectToJson`/`jsonToEffect` are not directly exposed under those names, mirror the file's existing effect round-trip idiom exactly.

- [x] **Step 2: Run the test to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20`
Expected: **compile error** — `Effect.PlaySubgame` is not a constructor.

- [x] **Step 3: Add the constructor**

In `source/library/Pawl/Type/Effect.hs`, add after `ExileUntilMonarch` (the current last constructor, ~line 202), before the `deriving`:

```haskell
  | -- CR 729.1/729.1b: play a Magic subgame, then bind its outcome (the derived
    -- loser) into this slot for a later effect to read. This slot is DEFINED here
    -- (like Create's minted-token slot), not a cast-time target -- the loser is
    -- determined only when the subgame ends, so the following effect reads it
    -- through the per-effect binding re-read in resolveSpellWith. Generic: the
    -- engine reaches subgames through this opcode, never Shahrazad's identity.
    PlaySubgame SlotName
```

- [x] **Step 4: Add the five classifier arms and the `definedSlots` arm**

In `source/library/Pawl/Resolve.hs`:

- `slotsOf` (line ~67): `PlaySubgame` READS no target slot — it *defines* one. Add next to `Effect.Create {} -> Set.empty`:

```haskell
  Effect.PlaySubgame _ -> Set.empty
```

- `effectReadsX` (line ~106): add next to `Effect.ExileAllGraveyards -> False`:

```haskell
      Effect.PlaySubgame _ -> False
```

- `manaProduced` (line ~137): add next to `Effect.ExileAllGraveyards -> Nothing`:

```haskell
  Effect.PlaySubgame _ -> Nothing
```

- `searchesLibrary` (line ~169): add next to `Effect.ExileAllGraveyards -> False`:

```haskell
  Effect.PlaySubgame _ -> False
```

- `rewriteEffect` (line ~240): add next to `Effect.ExileAllGraveyards -> effect`:

```haskell
  Effect.PlaySubgame _ -> effect
```

- `definedSlots` (line ~219–224): extend the `bound` matcher so `PlaySubgame slot` contributes its slot (the write-half, so a downstream `DealDamage slot` passes the D4 lint):

```haskell
  let bound effect = case effect of
        Effect.Create _ _ mSlot -> mSlot
        Effect.PlaySubgame slot -> Just slot
        _ -> Nothing
```

- [x] **Step 5: Add the placeholder executor arm**

In `source/library/Pawl/Resolve.hs`, add to `applyEffect`'s `case effect of` (next to the `Effect.RestartGame` arm) a **placeholder** — the real subgame-running arm is Task 3, but the build needs exhaustiveness now:

```haskell
  -- CR 729.1: play a subgame. Wired to the injected runner in resolveSpellWith
  -- (Task 3); the bare applyEffect path is the no-subgame default (an ability
  -- that plays a subgame is deferred). Placeholder no-op until Task 3.
  Effect.PlaySubgame _ -> pure ()
```

- [x] **Step 6: Add the two `Codec` arms**

In `source/library/Pawl/Codec.hs`:

- `effectToJson`, next to `Effect.ControlPlayerNextTurn s -> …` (~1155):

```haskell
  Effect.PlaySubgame s -> Json.tagged (Text.pack "PlaySubgame") (Just (slotNameToJson s))
```

- `jsonToEffect`, next to the `"ChangeText"` arm (~1186):

```haskell
    "PlaySubgame" -> withValue mv (fmap Effect.PlaySubgame . jsonToSlotName)
```

- [x] **Step 7: Run the test to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "$0~/PlaySubgame round-trips/"'`
Expected: **PASS** — warning-clean build (all `case effect of` sites exhaustive), the round-trip green.

- [x] **Step 8: Commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "feat(m5c): PlaySubgame opcode — constructor, classifiers, defined-slot, codec"
```

---

### Task 3: run the subgame — the runner injection and the executor arm

Thread a `Game Result` runner down the spell path and give `PlaySubgame` its real executor: run the runner, derive the 2-player loser from the `Result`, bind it into the slot, and make `resolveSpellWith` re-read bindings per effect so a later `DealDamage` sees the binding (CR 729.1b). All bare names stay as `…With noSubgame` wrappers, so no existing call site changes.

**Files:**
- Modify: `source/library/Pawl/Resolve.hs` — `noSubgame`, `bindLoserSlot`, `applyEffectWith`/`applyEffect`, `resolveSpellWith`/`resolveSpell` (+ per-effect re-read); imports.
- Test: `source/test-suite/Pawl/ResolveSpec.hs` — a spell whose effects are `[PlaySubgame slot, DealDamage slot 3]`, run through `resolveSpellWith` with a **stub** runner returning `Won alice`, deals 3 to bob.

**Interfaces:**
- Consumes: `applyEffect` executor site; `Binding`, `Recipient.ToPlayer`, `Result`.
- Produces: `noSubgame :: Game Result`; `applyEffectWith :: Game Result -> ObjectId -> PlayerId -> Map SlotName (Subtype,Subtype) -> Map SlotName Bool -> Map SlotName Recipient -> Effect Card -> Game ()`; `resolveSpellWith :: Game Result -> ObjectId -> Game ()`; unchanged `applyEffect`/`resolveSpell` signatures.

- [x] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/ResolveSpec.hs`, in the same `testGroup` list that holds the M5b `RestartGame` resolution test. It hand-builds a spell object on the stack whose chosen mode's effects are `[PlaySubgame "loser", DealDamage "loser" (Literal 3)]`, then runs `resolveSpellWith` with a stub runner that returns `Won alice` (no real subgame). The loser is bob; bob must lose 3 life. Add `import qualified Pawl.Type.Result as Result` if absent; the file already imports the ability/spell-construction machinery used by the M5b test (`Object`, `Source`, `Modal`, `Mode`, `ModeSelection`, `ModeIndex`, `Effect`, `Binding`, `Quantity.Type`, `Zone`, `TapState`, `Sickness`, `Seq`, `Map`, `Set`, `Engine`, `Game`, `GameState`, `Resolve`).

```haskell
      HU.testCase "CR 729.1b: PlaySubgame binds the loser, a later DealDamage reads it (mid-resolution binding visible)" $
        let g0 = Setup.emptyGame S.bothPlayers
            slot = SlotName.MkSlotName (Text.pack "loser")
            -- a stub runner: no real subgame, just report alice won -> loser = bob.
            stubRunner = pure (Result.Won S.alice)
            -- hand-build alice's spell on the stack: one chosen mode (index 0),
            -- effects [PlaySubgame slot, DealDamage slot (Literal 3)], no targets.
            (spellId, g1) = Game.freshObjectId g0
            (ts, g2) = Game.freshTimestamp g1
            card =
              -- a minimal synthetic card whose spell has the two effects above;
              -- mirror the file's existing synthetic-card construction idiom.
              Support.instantCardWith
                (Seq.fromList [Effect.PlaySubgame slot, Effect.DealDamage slot (Quantity.Type.Literal 3)])
                Map.empty
            spellObj =
              Object.MkObject
                { Object.owner = S.alice,
                  Object.source = Source.OfToken card,
                  Object.zone = Zone.Stack,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled,
                  Object.bindings = Binding.fromChoices Map.empty Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
                  Object.counters = Map.empty,
                  Object.timestamp = ts
                }
            g3 = g2 {GameState.objects = Map.insert spellId spellObj (GameState.objects g2), GameState.stack = spellId : GameState.stack g2}
            after = snd (Engine.runGamePure S.identityAnswer g3 (Resolve.resolveSpellWith stubRunner spellId))
         in HU.assertEqual "bob (the derived loser) lost 3 life to the follow-on DealDamage" (Just 17) (S.lifeOf S.bob after)
```

> **Card-construction note.** `Source.OfToken card` lets the test give the stack object a `Card` with arbitrary effects without a JSON file (the M4c token idiom; `Game.cardOf` returns a token's embedded card). If `Support` has no `instantCardWith` helper, build the `Card` inline the way the nearest existing `ResolveSpec` synthetic-card test does (a `Sorcery`/`Instant` `MkCard` with `spell = Modal.MkModal (Seq.singleton (Mode.MkMode <effects> <targetSpecs>)) (ModeSelection.ChooseExactly 1)`), and file the tiny helper under `Support` if it is reused. The **only** load-bearing part is that the stack object's chosen mode carries `[PlaySubgame slot, DealDamage slot (Literal 3)]`.

- [x] **Step 2: Run the test to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20`
Expected: **compile error** — `Resolve.resolveSpellWith` not in scope.

- [x] **Step 3: Add `noSubgame`, `bindLoserSlot`, and the imports**

In `source/library/Pawl/Resolve.hs`, add to the import group:

```haskell
import Pawl.Type.Result (Result)
import qualified Pawl.Type.Result as Result
```

Add two top-level helpers (near `bindSlot`, ~line 837):

```haskell
-- The default runner for every resolution that is NOT a subgame-bearing spell:
-- there is no nested game, so a PlaySubgame effect resolves as a draw and binds
-- nothing. The ability path (resolveEffects) and every direct test caller take
-- this; a subgame played from an ABILITY is deferred (no gate card needs one).
noSubgame :: Game Result
noSubgame = pure Result.Drawn

-- CR 729.1b: bind the subgame's derived loser (a player) into `slot` on the
-- resolving object, so a later effect (DealDamage) can read it. Mirrors bindSlot,
-- but the recipient is a player (ToPlayer), not an object.
bindLoserSlot :: ObjectId -> SlotName -> PlayerId -> GameState -> GameState
bindLoserSlot holder slot loser gs =
  let binding = Binding.empty {Binding.target = Just (Recipient.ToPlayer loser)}
      put obj = obj {Object.bindings = Map.insert slot binding (Object.bindings obj)}
   in gs {GameState.objects = Map.adjust put holder (GameState.objects gs)}
```

> `Binding` (the `Pawl.Type.Binding` module, for `empty`/`target`) and `Recipient` are already imported by `Resolve.hs` (it constructs recipients elsewhere). Confirm the aliases; `Binding.empty` is `Pawl.Type.Binding.empty`.

- [x] **Step 4: Split `applyEffect` into `applyEffectWith` + wrapper**

In `source/library/Pawl/Resolve.hs`, rename the executor. Change the header line

```haskell
applyEffect :: ObjectId -> PlayerId -> Map.Map SlotName (Subtype, Subtype) -> Map.Map SlotName Bool -> Map.Map SlotName Recipient -> Effect Card.Type.Card -> Game ()
applyEffect source controller bound legality chosen effect = case effect of
```

to add the runner as the first parameter:

```haskell
-- The subgame-runner-aware executor. `runSubgame` is the injected Game Result
-- that PLAYS a nested game (Engine.playSubgame); the bare applyEffect below
-- passes noSubgame. Only the PlaySubgame arm consults it.
applyEffectWith :: Game Result -> ObjectId -> PlayerId -> Map.Map SlotName (Subtype, Subtype) -> Map.Map SlotName Bool -> Map.Map SlotName Recipient -> Effect Card.Type.Card -> Game ()
applyEffectWith runSubgame source controller bound legality chosen effect = case effect of
```

Replace the placeholder `Effect.PlaySubgame _ -> pure ()` arm (Task 2 Step 5) with the real executor:

```haskell
  Effect.PlaySubgame slot -> do
    -- CR 729.1/729.5: run the nested game to completion (the runner does the
    -- construction, play, funnel-back, and reshuffle); then bind its outcome.
    -- CR 729.1b: the loser is the 2-player derivation from the Result; a Drawn
    -- subgame binds nothing (the follow-on then no-ops). Multi-player "each
    -- player who doesn't win" and a widened Result are deferred.
    result <- runSubgame
    order <- State.gets GameState.turnOrder
    case result of
      Result.Won winner -> case Maybe.listToMaybe (filter (/= winner) order) of
        Just loser -> State.modify' (bindLoserSlot source slot loser)
        Nothing -> pure ()
      Result.Drawn -> pure ()
```

Add the bare wrapper immediately after the whole `applyEffectWith` definition (so every existing 6-argument caller is unchanged):

```haskell
-- The no-subgame executor (the ability path and every direct caller): a
-- PlaySubgame resolves as a draw here (see noSubgame).
applyEffect :: ObjectId -> PlayerId -> Map.Map SlotName (Subtype, Subtype) -> Map.Map SlotName Bool -> Map.Map SlotName Recipient -> Effect Card.Type.Card -> Game ()
applyEffect = applyEffectWith noSubgame
```

> `Maybe.listToMaybe`, `State.gets`, `State.modify'`, `filter`, `(/=)` are already in scope in `Resolve.hs`.

- [x] **Step 5: Split `resolveSpell` into `resolveSpellWith` + wrapper, with the per-effect binding re-read**

In `source/library/Pawl/Resolve.hs`, replace `resolveSpell`'s definition (lines ~298–338). Rename it to `resolveSpellWith` taking the runner, and change the effect fold from a snapshot `mapM_` into a per-effect loop that **re-reads** the source's live bindings (so a slot `PlaySubgame` binds mid-fold is visible to the next effect). Keep the fizzle logic and the pre-fold `legalSlot` closure exactly as they are. The changed body:

```haskell
-- CR 608.2b/608.2c, extended for CR 729.1b: resolve a spell, re-reading the
-- resolving object's bindings before EACH effect so a slot DEFINED mid-resolution
-- (PlaySubgame's loser; a Create's minted token) is visible to a later effect.
-- Target-slot legality is still fixed at the START of resolution (the pre-fold
-- `gs`); only newly-defined reserved slots (never targets) newly appear, and a
-- reserved slot is vacuously legal (legalSlot's Nothing branch). `runSubgame` is
-- the injected nested-game runner.
resolveSpellWith :: Game Result -> ObjectId -> Game ()
resolveSpellWith runSubgame oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just obj -> case Game.cardOf oid gs of
      Nothing -> pure ()
      Just card ->
        let specs = Card.modesTargetSpecs (Binding.modesOf (Object.bindings obj)) card
            chosen = Binding.targetsOf (Object.bindings obj)
            legalSlot slot recipient = case Map.lookup slot specs of
              Nothing -> True
              Just spec -> Target.stillLegal oid recipient spec gs
            legality = Map.mapWithKey legalSlot chosen
            targeted = Map.restrictKeys legality (Map.keysSet specs)
            fizzles = not (Map.null specs) && not (or (Map.elems targeted))
         in if fizzles
              then Event.changeZone oid Zone.Graveyard
              else do
                let effectController = Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf oid gs)
                Monad.forM_ (effectsOf oid gs) $ \eff -> do
                  -- Re-read the live bindings for THIS effect: a prior PlaySubgame
                  -- may have bound its loser slot. Target legality is recomputed
                  -- with the same pre-fold `legalSlot` (targets unchanged; the new
                  -- reserved slot is vacuously legal).
                  bindingsNow <- State.gets (\g -> maybe (Object.bindings obj) Object.bindings (Game.lookupObject oid g))
                  let chosenNow = Binding.targetsOf bindingsNow
                      legalityNow = Map.mapWithKey legalSlot chosenNow
                  applyEffectWith runSubgame oid effectController (Binding.subtypesOf bindingsNow) legalityNow chosenNow eff
                Event.changeZone oid Zone.Graveyard

-- The no-subgame spell resolver (Stack's default path and every direct caller).
resolveSpell :: ObjectId -> Game ()
resolveSpell = resolveSpellWith noSubgame
```

> Preserve the existing comments on `legalSlot`/`fizzles`/`effectController` from the original `resolveSpell` (CR 608.2b/405.4 citations) — copy them verbatim into `resolveSpellWith`; the snippet above abbreviates them. `Monad.forM_` is `Control.Monad.forM_` — already imported (Resolve uses `Monad.mapM_`).

- [x] **Step 6: Run the test to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "$0~/PlaySubgame binds the loser/"'`
Expected: **PASS** — warning-clean; bob is at 17 life.

Then run the **full suite** once here (the per-effect re-read touches the hot spell path):

Run: `cabal test`
Expected: **all pass** — the re-read is behavior-preserving (no existing card reads a mid-resolution binding; target legality is unchanged). If any spell test reddened, the re-read changed target behavior — STOP and inspect `legalityNow` (it must equal the old `legality` for every targetSpec slot).

- [x] **Step 7: Commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "feat(m5c): inject a subgame runner; PlaySubgame binds the loser, follow-on reads it (CR 729.1b)"
```

---

### Task 4: `playSubgame` — the real nested game, wired into the priority loop

Give the runner a body: construct the subgame (CR 729.2), run `startGameFromCards >> playGame` as a **lifted** `runStateT` (the function call, sharing the interpreter), funnel cards back (CR 729.5), and reshuffle. Then wire `Engine.priorityLoop` to supply it via `Stack.resolveTopWith`.

**Files:**
- Modify: `source/library/Pawl/Stack.hs` — `resolveTopWith`/`resolveTop` wrapper.
- Modify: `source/library/Pawl/Engine.hs` — `playSubgame`; `priorityLoop` uses `resolveTopWith playSubgame`; import `Trans`.
- Test: `source/test-suite/Pawl/GameSpec.hs` — `playSubgame` on a crafted 2-player state returns `Won alice` (bob decks, CR 729.3) and funnels every card back (CR 729.5).

**Interfaces:**
- Consumes: `Engine.playGame`, `Setup.{subgameStateFrom,startGameFromCards,funnelBack,shuffleLibrary}`, `Resolve.{resolveSpellWith,resolveTop}`.
- Produces: `Stack.resolveTopWith :: Game Result -> Game ()`; unchanged `Stack.resolveTop`; `Engine.playSubgame :: Game Result`.

- [x] **Step 1: Split `Stack.resolveTop` into `resolveTopWith` + wrapper**

In `source/library/Pawl/Stack.hs`, change `resolveTop`'s definition (line ~30) to take a runner and forward it to the **spell** branch only; keep the ability branches on the bare (no-subgame) resolvers. Rename the header:

```haskell
-- The runner-aware resolve-the-top-of-stack: a resolving SPELL may play a subgame
-- (CR 729), so the spell branch takes the injected runner; abilities do not (an
-- ability-driven subgame is deferred). Engine.priorityLoop supplies playSubgame.
resolveTopWith :: Game Result -> Game ()
resolveTopWith runSubgame = do
```

In the body, change **only** the spell branch — the line that currently reads `else Resolve.resolveSpell oid` (line ~43) becomes:

```haskell
            else Resolve.resolveSpellWith runSubgame oid
```

Leave the ability branches (`Resolve.resolveAbility …`, `Resolve.resolveEffects …`) unchanged. Add the bare wrapper after the definition:

```haskell
-- The no-subgame resolve-top (every existing caller and test): a resolving spell
-- with a PlaySubgame effect would draw. Engine's live loop uses resolveTopWith.
resolveTop :: Game ()
resolveTop = resolveTopWith Resolve.noSubgame
```

Add to `Stack.hs`'s import group (it needs the `Result` type for the signature and `Resolve.noSubgame`):

```haskell
import Pawl.Type.Result (Result)
```

> `Resolve` is already imported by `Stack.hs`; `Resolve.noSubgame`/`resolveSpellWith` come from Task 3.

- [x] **Step 2: Write the failing test (playSubgame end-to-end)**

Add to `source/test-suite/Pawl/GameSpec.hs`, in `ruleTests`. It builds a parent where alice's library has 8 Mountains and bob's has 3, runs `Engine.playSubgame` directly, and asserts the subgame result feeds back correctly: alice wins (bob decks on the opening draw, CR 729.3), and every card funnels back into a library (CR 729.5). Add `import qualified Pawl.Type.Result as Result` if absent. Reuse `addManyG` (already in `GameSpec.hs`) plus a local `poolToLibraryG` mirroring Task 1's helper.

```haskell
      HU.testCase "CR 729.2/729.3/729.5: playSubgame runs a nested game, bob decks, cards funnel back" $
        let g0 = Setup.emptyGame S.bothPlayers
            -- alice: 8 library cards; bob: 3 (fewer than seven -> loses, CR 729.3)
            g1 = poolToLibraryG S.bob (poolToLibraryG S.alice (addManyG cards 3 S.bob (addManyG cards 8 S.alice g0)))
            (result, after) = Engine.runGamePure S.identityAnswer g1 Engine.playSubgame
            libCount pid = length (Game.zoneMembers Zone.Library pid after)
         in do
              HU.assertEqual "CR 729.3: bob has fewer than 7 cards, so alice wins the subgame" (Result.Won S.alice) result
              HU.assertEqual "CR 729.5: alice's cards funnel back into her main-game library" 8 (libCount S.alice)
              HU.assertEqual "CR 729.5: bob's cards funnel back into his main-game library" 3 (libCount S.bob)
              HU.assertEqual "the main game resumes with no result recorded" Nothing (GameState.result after)
```

Add the local helper near the test (mirror of Task 1's `poolToLibrary`, adapted to `GameSpec`'s imports):

```haskell
poolToLibraryG :: S.PlayerId -> GameState.GameState -> GameState.GameState
poolToLibraryG pid gs =
  let mine = Map.keys (Map.filter (\o -> Object.owner o == pid) (GameState.objects gs))
      onLibrary o = o {Object.zone = Zone.Library}
   in gs
        { GameState.objects = List.foldl' (\m oid -> Map.adjust onLibrary oid m) (GameState.objects gs) mine,
          GameState.battlefield = Set.difference (GameState.battlefield gs) (Set.fromList mine),
          GameState.library = Map.insert pid (Seq.fromList mine) (GameState.library gs)
        }
```

> `Object`, `Zone`, `List`, `Set`, `Seq`, `Map`, `Game`, `Engine`, `GameState` are imported in `GameSpec.hs` (M5a/M5b added most); add any missing. **Do not run yet** — `Engine.playSubgame` does not exist.

- [x] **Step 3: Run the test to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20`
Expected: **compile error** — `Engine.playSubgame` not in scope.

- [x] **Step 4: Implement `playSubgame` and wire `priorityLoop`**

In `source/library/Pawl/Engine.hs`, add to the import group:

```haskell
import qualified Control.Monad.Trans.Class as Trans
```

Add the runner (near `playGame`, ~line 521):

```haskell
-- CR 729: play a subgame as a FUNCTION CALL. Construct the subgame from the
-- parent's library cards (CR 729.2, subgameStateFrom), then run its setup and
-- whole game as `runStateT (startGameFromCards >> playGame)` LIFTED into the
-- parent's StateT -- so the subgame's prompts flow through the SAME Program
-- interpreter and Replay fold (untagged; the design). The parent GameState sits
-- untouched in the outer frame while the subgame runs (CR 729.1a). At the end,
-- funnel each owner's cards back to their main-game library (CR 729.5) and
-- reshuffle (Prompt.Shuffle). A subgame within a subgame (CR 729.6) is free: the
-- nested playGame's own priorityLoop re-supplies playSubgame.
playSubgame :: Game Result
playSubgame = do
  parent <- State.get
  let sub0 = Setup.subgameStateFrom parent
  (result, finalSub) <- Trans.lift (State.runStateT (Setup.startGameFromCards >> playGame) sub0)
  State.modify' (Setup.funnelBack finalSub)
  order <- State.gets GameState.turnOrder
  Monad.forM_ order Setup.shuffleLibrary
  pure result
```

Then change `priorityLoop`'s resolve site. Find `Stack.resolveTop` in `priorityLoop` (line ~420) and change it to:

```haskell
                          Stack.resolveTopWith playSubgame
```

> `State.runStateT` yields `Program Prompt (Result, GameState)`; `Trans.lift` lifts that base action into `Game`. `Setup` is already imported by `Engine`; `Monad`, `State`, `GameState` are in scope.

- [x] **Step 5: Run the test to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "$0~/playSubgame runs a nested game/"'`
Expected: **PASS** — alice wins, both libraries rebuilt, no main-game result.

- [x] **Step 6: Commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "feat(m5c): playSubgame — a subgame is a lifted runStateT playGame (CR 729.1a/729.2/729.5)"
```

---

### Task 5: the `synthetic-subgame` gate card

A labeled-synthetic `{0}` Sorcery (the `Landform` / `synthetic-restart` crutch) whose one mode is `[PlaySubgame "loser", DealDamage "loser" (Literal 3)]`. Documented expiry names **Shahrazad** (Task 9). The `{0}` cost lets the gate cast it with no lands.

**Files:**
- Create: `data/cards/synthetic-subgame.json`.
- Modify: `source/test-suite/Pawl/Cards.hs` — register at four sites.

**Interfaces:**
- Produces: `Cards.syntheticSubgamePrinting :: Cards.Cards -> Printing.Printing`.

- [x] **Step 1: Create the card file**

Write `data/cards/synthetic-subgame.json`:

```json
{
  "activatedAbilities": [],
  "castingPermissions": [],
  "keywords": [],
  "manaCost": [],
  "name": "Synthetic Subgame",
  "power": null,
  "replacementEffects": [],
  "spell": {
    "modes": [
      {
        "effects": [
          { "type": "PlaySubgame", "value": "loser" },
          { "type": "DealDamage", "value": [ "loser", { "type": "Literal", "value": 3 } ] }
        ],
        "targetSpecs": []
      }
    ],
    "selection": { "type": "ChooseExactly", "value": 1 }
  },
  "staticAbilities": [],
  "toughness": null,
  "triggeredAbilities": [],
  "typeLine": {
    "subtypes": [],
    "supertypes": [],
    "types": [ { "type": "Sorcery" } ]
  }
}
```

> `manaCost: []` is a payable `{0}` (an *absent* `mana` on an ability is the unpayable case, CR 118.6 — but a spell's `manaCost: []` is a legal empty cost). `PlaySubgame` DEFINES `"loser"`; `DealDamage` READS it — the D4 lint accepts this because `Resolve.definedSlots` (Task 2) reports `"loser"`.

- [x] **Step 2: Register the printing at all four `Cards.hs` sites**

In `source/test-suite/Pawl/Cards.hs`:

1. Record field (in `data Cards`, after `syntheticRestartPrinting :: Printing.Printing,`):

```haskell
    syntheticSubgamePrinting :: Printing.Printing,
```

2. Load line in `loadCards` (after `syntheticRestartPrinting_ <- loadPrinting "synthetic-restart"`):

```haskell
  syntheticSubgamePrinting_ <- loadPrinting "synthetic-subgame"
```

3. Record construction line (after `syntheticRestartPrinting = syntheticRestartPrinting_,`):

```haskell
        syntheticSubgamePrinting = syntheticSubgamePrinting_,
```

4. `allPrintings` (after `syntheticRestartPrinting cards,`):

```haskell
    syntheticSubgamePrinting cards,
```

- [x] **Step 3: Run the round-trip / D4-lint tests**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "$0~/round-trip/ || $0~/allPrintings/ || $0~/honest/ || $0~/declared target slot/"'`
Expected: **PASS** — the printing parses, appears in `allPrintings`, JSON round-trips, and the D4 lint accepts the `PlaySubgame`-defined / `DealDamage`-read slot. If the D4 lint reddens, the `definedSlots` arm (Task 2 Step 4) is missing or wrong.

> If unsure which suite owns these, run the full suite: `cabal test`. It must stay green.

- [x] **Step 4: Commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "feat(m5c): synthetic-subgame gate card (labeled synthetic, expires -> Shahrazad)"
```

---

### Task 6: gameplay gate — casting the subgame spell feeds the loser to the parent (CR 729.1b/729.3)

The first headline gate. alice casts the `synthetic-subgame` sorcery **through the real priority loop**; it plays a nested game in which bob decks (CR 729.3), alice wins, and the follow-on `DealDamage` deals 3 to bob (CR 729.1b) — the derived loser. This is strictly more end-to-end than Task 3 (a stub runner) or Task 4 (`playSubgame` in isolation): it goes through activation, the stack, resolution, and the injected `playSubgame`.

**Files:**
- Modify: `source/test-suite/Pawl/GameSpec.hs` — one answerer `subgameAnswer`, one test case in `ruleTests`.

**Interfaces:**
- Consumes: `Engine.{runGamePure,priorityLoop}`, `Setup.emptyGame`, `S.addCreature`, `Cards.{syntheticSubgamePrinting,mountainPrinting}`, `isCastAction`, `poolToLibraryG`, `S.identityAnswer`.
- Produces: `subgameAnswer :: Prompt.Prompt r -> r`.

- [x] **Step 1: Add the casting answerer**

Append to `source/test-suite/Pawl/GameSpec.hs` (next to the other answerers, e.g. after `restartAnswer`):

```haskell
-- CR 729 gate strategy. Whoever has priority casts the only castable spell on the
-- board -- the synthetic subgame sorcery ({0}, in alice's hand) -- and otherwise
-- passes. Inside the subgame the libraries are Mountains (lands are PLAYED, not
-- cast), so no cast is available there and everyone passes to termination (bob
-- decks). Because subgame prompts are UNTAGGED, the same answerer serves both
-- games. Non-ChooseAction prompts (Shuffle during setup, etc.) delegate to
-- identityAnswer.
subgameAnswer :: Prompt.Prompt r -> r
subgameAnswer p = case p of
  Prompt.ChooseAction _ _ actions ->
    case filter isCastAction actions of
      cast : _ -> cast
      [] -> A.Pass
  _ -> S.identityAnswer p
```

- [x] **Step 2: Add the gate test case to `ruleTests`**

Insert into `ruleTests` (comma-separated). The sorcery is placed into alice's **hand** (a spell is cast from hand); bob's library is a 3-card pool (he decks in the subgame). A helper `handCard` puts a printing into a player's hand as a fresh object — use the file's existing hand-placement idiom (the M5a `handBobBolt` pattern) generalized, or inline the object construction:

```haskell
      HU.testCase "CR 729.1b/729.3 gameplay: alice casts a subgame spell, bob decks, bob takes 3" $
        -- alice has the {0} subgame sorcery in hand and an 8-card library; bob has
        -- a 3-card library (decks in the subgame, CR 729.3). alice casts through
        -- the priority loop; the subgame resolves alice the winner; the follow-on
        -- DealDamage hits bob (the loser) for 3.
        let g0 = Setup.emptyGame S.bothPlayers
            g1 = poolToLibraryG S.bob (poolToLibraryG S.alice (addManyG cards 3 S.bob (addManyG cards 8 S.alice g0)))
            (spellId, g2) = handCard (Cards.syntheticSubgamePrinting cards) S.alice g1
            gStart =
              g2
                { GameState.activePlayer = S.alice,
                  GameState.phase = Phase.PrecombatMain,
                  GameState.priority = Just S.alice
                }
            after = snd (Engine.runGamePure subgameAnswer gStart Engine.priorityLoop)
         in do
              HU.assertEqual "CR 729.1b: bob (the subgame loser) took 3 from the follow-on DealDamage" (Just 17) (S.lifeOf S.bob after)
              HU.assertEqual "alice, the winner, is untouched" (Just 20) (S.lifeOf S.alice after)
              HU.assertEqual "the subgame spell resolved and left the stack" [] (GameState.stack after)
              HU.assertEqual "the main game did not end" Nothing (GameState.result after)
              HU.assertEqual "the subgame spell is gone from alice's hand (cast)" False (Game.lookupObject spellId after /= Nothing && elem spellId (Game.zoneMembers Zone.Hand S.alice after))
```

Add the `handCard` helper near the answerer (mirror `handBobBolt` from M5a; it mints a hand object for the given player):

```haskell
-- Put one copy of a printing into a player's hand as a fresh object; return its id.
handCard :: Printing.Printing -> S.PlayerId -> GameState.GameState -> (Object.ObjectId, GameState.GameState)
handCard printing pid gs =
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.hand = Map.insertWith (\new old -> new <> old) pid (Seq.singleton oid) (GameState.hand gs2)
          }
      )
```

> `Printing`, `Object`, `Source.OfCard`, `Zone.Hand`, `TapState`, `Sickness`, `Game.freshObjectId`/`freshTimestamp` are all imported in `GameSpec.hs` (M5a's `handBobBolt` uses them). If `handBobBolt` already generalizes to any printing, reuse it instead of adding `handCard`. `Phase.PrecombatMain` is used by the M5a/M5b gates in this file.

- [x] **Step 3: Run the gate, then prove it has teeth**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "$0~/alice casts a subgame spell/"'`
Expected: **PASS.**

Then a **falsification check** — temporarily break the loser derivation in `source/library/Pawl/Resolve.hs`'s `PlaySubgame` arm by binding the **winner** instead of the loser (`Just loser` → bind `winner`): re-run the gate and expect **FAIL** (bob at 20, alice at 17). **Revert** exactly, confirm `git diff source/library/Pawl/Resolve.hs` is empty, and re-run: expect **PASS**. This proves the gate depends on the CR 729.1b outcome plumbing.

> Do not leave the mutation in. If the gate does not fail under it, the follow-on is not reading the bound loser — investigate before proceeding.

- [x] **Step 4: Commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "test(m5c): gameplay gate — subgame outcome feeds the parent effect (CR 729.1b/729.3)"
```

---

### Task 7: gameplay gate — funnel-back and counter isolation (CR 729.5/729.4b)

The subgame's cards return to their owners' main libraries and reshuffle (CR 729.5), the main-game board survives, and a main-game **player counter** is unaffected by the subgame while subgame counters cease (CR 729.4b). Reuses Task 6's `subgameAnswer`.

**Files:**
- Modify: `source/test-suite/Pawl/GameSpec.hs` — one test case in `ruleTests`.

**Interfaces:**
- Consumes: everything from Task 6 plus `Effect`/`GainPlayerCounters` machinery indirectly — here just set a counter on the parent player directly via `Player.counters` and assert it survives.

- [x] **Step 1: Write the failing test**

Insert into `ruleTests`. bob starts with a poison counter in the **main** game; after the subgame he still has exactly it (CR 729.4b: main-game counters are outside the subgame and untouched). Every owned card is back in a library and the main-game battlefield survivors are intact (CR 729.5). Add `import qualified Pawl.Type.PlayerCounterKind as PlayerCounterKind` if absent (poison's kind); confirm the kind constructor name against `Pawl.Type.PlayerCounterKind` (P10).

```haskell
      HU.testCase "CR 729.5/729.4b gameplay: cards funnel back, main-game board survives, main-game counters untouched" $
        let g0 = Setup.emptyGame S.bothPlayers
            -- a survivor on the main battlefield that must remain after the subgame
            (survivorId, g1) = S.addCreature (Cards.mountainPrinting cards) S.alice g0
            g2 = poolToLibraryG S.bob (poolToLibraryG S.alice (addManyG cards 3 S.bob (addManyG cards 8 S.alice g1)))
            (_spellId, g3) = handCard (Cards.syntheticSubgamePrinting cards) S.alice g2
            -- give bob a main-game poison counter (CR 729.4b: outside the subgame)
            g4 = g3 {GameState.players = Map.adjust (\pl -> pl {Player.counters = Map.insert PlayerCounterKind.Poison 1 (Player.counters pl)}) S.bob (GameState.players g3)}
            gStart = g4 {GameState.activePlayer = S.alice, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}
            after = snd (Engine.runGamePure subgameAnswer gStart Engine.priorityLoop)
            bobPoison = Map.findWithDefault 0 PlayerCounterKind.Poison (Player.counters (Map.findWithDefault (error "bob") S.bob (GameState.players after)))
         in do
              HU.assertEqual "CR 729.4b: bob's main-game poison counter is untouched by the subgame" 1 bobPoison
              HU.assertEqual "CR 729.5: alice's library holds her 8 subgame cards again" 8 (length (Game.zoneMembers Zone.Library S.alice after))
              HU.assertEqual "CR 729.5: bob's library holds his 3 subgame cards again" 3 (length (Game.zoneMembers Zone.Library S.bob after))
              HU.assertEqual "the main-game survivor is still on the battlefield" True (Set.member survivorId (GameState.battlefield after))
```

> **Do not use `error` in library code** — the `Map.findWithDefault (error "bob") …` above is test-only sugar for "bob is always present"; if hlint or the no-partial rule flags it in tests too, replace with `Maybe.fromMaybe 0 (fmap (Map.findWithDefault 0 PlayerCounterKind.Poison . Player.counters) (Map.lookup S.bob (GameState.players after)))`. Confirm `PlayerCounterKind.Poison`'s exact constructor name (P10's `Pawl.Type.PlayerCounterKind`); adjust if it differs.

- [x] **Step 2: Run the test to verify it passes; prove teeth**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "$0~/cards funnel back/"'`
Expected: **PASS.**

Falsification: temporarily make `Setup.funnelBack` reset players (add `GameState.players = Map.map (\pl -> pl {Player.counters = Map.empty}) (GameState.players parent)` to its record update) — re-run and expect **FAIL** on the poison assertion (proving 729.4b is load-bearing: funnelBack must NOT touch the parent's players). **Revert** exactly, confirm the diff is empty, re-run: **PASS**.

- [x] **Step 3: Commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "test(m5c): gameplay gate — funnel-back and main-game counter isolation (CR 729.5/729.4b)"
```

---

### Task 8: gameplay gate — Shahrazad-in-Shahrazad nests arbitrarily (CR 729.6)

The go/no-go falsifier the whole milestone exists to survive: a subgame that itself resolves a subgame. alice's **main-game library** contains a copy of the `synthetic-subgame` sorcery, so inside the subgame she draws and casts it, opening a **sub-subgame**; every level plays out (bob decks at each level), the recursion unwinds, and the top-level main game resumes intact — proving nesting is free recursion (no `GameState` stack field).

**Files:**
- Modify: `source/test-suite/Pawl/GameSpec.hs` — one test case in `ruleTests`.

**Interfaces:**
- Consumes: everything from Task 6.

- [x] **Step 1: Write the failing test**

Insert into `ruleTests`. The construction: alice's library is `[synthetic-subgame] ++ 7 Mountains` (8 cards, ≥7 so she does not deck); bob's library is 3 Mountains (decks at every level). In the level-1 subgame, alice draws the nested subgame sorcery in her opening hand and — via `subgameAnswer`, which casts any castable spell — casts it, spawning a level-2 sub-subgame where bob decks again; that unwinds, then bob decks in level 1, alice wins level 1, and the main-game follow-on hits bob for 3. The single load-bearing assertion is **termination with the right top-level outcome**: nesting completed without a stack field.

```haskell
      HU.testCase "CR 729.6 gameplay: a subgame nests a subgame; nesting terminates and the main game resumes" $
        let g0 = Setup.emptyGame S.bothPlayers
            -- alice's library: one nested subgame sorcery + 7 Mountains (>= 7, no deck-out);
            -- she will draw and cast the nested sorcery inside the level-1 subgame.
            (_nestedId, g1) = libraryCard (Cards.syntheticSubgamePrinting cards) S.alice g0
            g2 = poolToLibraryG S.bob (addToLibraryG cards 7 S.alice (addManyG cards 3 S.bob g1))
            (_spellId, g3) = handCard (Cards.syntheticSubgamePrinting cards) S.alice g2
            gStart = g3 {GameState.activePlayer = S.alice, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}
            after = snd (Engine.runGamePure subgameAnswer gStart Engine.priorityLoop)
         in do
              -- If nesting had not terminated, runGamePure would not return.
              HU.assertEqual "CR 729.6: the top-level main game resumed with no result" Nothing (GameState.result after)
              HU.assertEqual "CR 729.1b: bob took 3 from the level-1 subgame's follow-on" (Just 17) (S.lifeOf S.bob after)
              HU.assertEqual "the top-level subgame spell left the stack" [] (GameState.stack after)
```

Add the two small library helpers near `handCard` (one puts a single printing into a player's library; one appends `n` Mountains to a player's library) — mirror `handCard`/`poolToLibraryG`:

```haskell
-- Put one printing into a player's library as a fresh object; return its id.
libraryCard :: Printing.Printing -> S.PlayerId -> GameState.GameState -> (Object.ObjectId, GameState.GameState)
libraryCard printing pid gs =
  let (oid, gs1) = handCard printing pid gs
      onLibrary o = o {Object.zone = Zone.Library}
   in ( oid,
        gs1
          { GameState.objects = Map.adjust onLibrary oid (GameState.objects gs1),
            GameState.hand = Map.adjust (Seq.filter (/= oid)) pid (GameState.hand gs1),
            GameState.library = Map.insertWith (\new old -> old <> new) pid (Seq.singleton oid) (GameState.library gs1)
          }
      )

-- Append n Mountains to a player's library.
addToLibraryG :: Cards.Cards -> Int -> S.PlayerId -> GameState.GameState -> GameState.GameState
addToLibraryG cards n pid gs =
  List.foldl' (\g _ -> snd (libraryCard (Cards.mountainPrinting cards) pid g)) gs (replicate n ())
```

> **Determinism caveat.** This scenario relies on alice drawing the nested sorcery into her level-1 opening hand. `startGameFromCards` shuffles via `Prompt.Shuffle`, and `S.identityAnswer` (which `subgameAnswer` delegates to) answers `Shuffle ids -> ids` — an **identity** shuffle — so the library order is deterministic and the nested sorcery's position is fixed. If the nested sorcery does not land in the opening 7 under the identity shuffle, adjust the library so it does (e.g. make it the first library object, or shrink alice's library to exactly 7 + the sorcery = 8 with the sorcery first). Verify by running; if the nested cast does not fire, the nested sorcery was not drawn — reorder, do not weaken the assertion.

- [x] **Step 2: Run the test — verify it terminates and passes; prove teeth**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "$0~/a subgame nests a subgame/"'`
Expected: **PASS** (and, critically, it **returns** — non-termination would hang the suite).

Falsification: this gate's teeth are termination + the level-1 follow-on. To prove the nested cast actually happened (not just a flat subgame), temporarily point `subgameAnswer`'s cast branch at `A.Pass` (never cast): re-run and expect the level-1 follow-on assertion to change (bob no longer takes damage, since alice never casts anything anywhere) — confirming the assertions depend on the spell being cast at every level. **Revert** exactly.

- [x] **Step 3: Commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "test(m5c): gameplay gate — arbitrary subgame nesting terminates (CR 729.6)"
```

---

### Task 9: file the deferrals and cite them inline

Per `CLAUDE.md` ("file the issue, cite it inline"), every M5c elision gets a GitHub issue carrying status, rationale, and expiry trigger, with a code-site comment stating only what is *not* implemented plus `(#N)`.

**Files:**
- Create: GitHub issues in `tfausak/pawl`.
- Modify: `source/library/Pawl/Setup.hs` (first-player elision comment), `source/library/Pawl/Resolve.hs` (ability-path + Result-widening + full-Shahrazad citations at the `PlaySubgame` arm).

- [x] **Step 1: Check for duplicates, then file the issues**

```bash
gh issue list --repo tfausak/pawl --search "subgame"
```

File these (adjust wording if a duplicate exists). Record each returned number.

1. First-player RNG elision (CR 729.2 / CR 103.4):

```bash
gh issue create --repo tfausak/pawl \
  --title "Subgame first player is deterministic (CR 729.2 'randomly determine which player goes first')" \
  --label elision --label rules-correctness \
  --body "Status: elided in M5c. Setup.subgameStateFrom sets the subgame's active player to the head of the turn order, matching the existing Setup.emptyGame posture -- pawl has no first-player randomness prompt (the Prompt GADT has no such constructor; only Shuffle carries randomness). CR 729.2 (and CR 103.4) call for randomly determining the starting player. Expiry: fires when a first-player randomness prompt lands (a shared need with CR 103.4 for main games). Cited at Setup.subgameStateFrom."
```

2. Ability-path subgames:

```bash
gh issue create --repo tfausak/pawl \
  --title "Subgame from an activated/triggered ability (PlaySubgame on the ability path)" \
  --label gap --label expires:card-driven \
  --body "Status: deferred. M5c injects the subgame runner only down the SPELL path (Stack.resolveTopWith -> Resolve.resolveSpellWith -> applyEffectWith); the ability path (resolveEffects/resolveAbility) folds the bare applyEffect, whose PlaySubgame arm uses noSubgame (resolves as a draw, binds nothing). No real card plays a subgame from an ability (Shahrazad is a sorcery). Expiry: card-driven. Cited at the PlaySubgame arm in Pawl.Resolve."
```

3. Result widening for multi-player "each non-winner":

```bash
gh issue create --repo tfausak/pawl \
  --title "Subgame outcome is a 2-player loser derivation (Result not widened for 'each player who doesn't win')" \
  --label gap --label rules-correctness \
  --body "Status: deferred. PlaySubgame derives the single loser as listToMaybe (filter (/= winner) turnOrder) and a Drawn subgame binds nothing. CR 729.1b's real customer (Shahrazad: 'each player who doesn't win ... loses half their life') needs the full non-winner SET and a per-player half-life-rounded-up quantity. Pawl.Type.Result stays Won PlayerId | Drawn. Expiry: card-driven (full Shahrazad). Cited at the PlaySubgame arm in Pawl.Resolve."
```

4. Full Shahrazad (retires the synthetic gate):

```bash
gh issue create --repo tfausak/pawl \
  --title "Full Shahrazad (CR 729): the half-life-rounded-up rider retires the synthetic-subgame gate" \
  --label gap --label rules-correctness --label expires:card-driven \
  --body "Status: deferred; M5c's gate is a labeled synthetic ('Synthetic Subgame', data/cards/synthetic-subgame.json), the Landform crutch. Shahrazad is the only real CR 729 card; its rider -- 'each player who doesn't win the subgame loses half their life, rounded up' -- needs a life-loss opcode, a half-current-life-rounded-up Quantity, and the multi-player non-winner scope (see the Result-widening issue). The subgame MACHINERY (PlaySubgame, funnelBack, nesting) is real and faithful; only the fixed-3 DealDamage rider stands in for the half-life rider. When built, this retires syntheticSubgamePrinting in favor of a real Shahrazad card + test. Also parked: CR 727.6 (restarting a subgame) rides this machinery. Expiry: card-driven (Shahrazad). Cited at the PlaySubgame arm in Pawl.Resolve."
```

5. Subsystem-blocked slices:

```bash
gh issue create --repo tfausak/pawl \
  --title "Subgame subsystem-blocked slices (CR 729.2a-c / 729.4a / 729.5 second sentence / 729.5a-c)" \
  --label gap \
  --body "Status: deferred, subsystem-blocked (not scheduled). Nontraditional/supplementary decks (729.2a/729.5a), Vanguard (729.2b/729.5b), Commander (729.2c/729.5c), and cards brought INTO a subgame from a main game plus the main-game leave-the-zone triggers they queue until the main game resumes (729.4a, 729.5 second sentence) all ride subsystems M5 does not build (nontraditional cards, Vanguard, Commander, outside-the-game). CR 723.4 subgame prompt tagging / game-context is an M7/interpreter concern (subgame prompts are untagged today). Cited at Setup.funnelBack / Engine.playSubgame."
```

- [x] **Step 2: Add the inline citations**

In `source/library/Pawl/Setup.hs`, extend `subgameStateFrom`'s first-player comment to cite the elision issue (replace `#N1`):

```haskell
-- ... CR 729.2's "randomly determine which player goes first" is elided to the
-- head of the turn order (pawl has no first-player randomness prompt) (#N1).
```

In `source/library/Pawl/Resolve.hs`, extend the `PlaySubgame` arm's comment to cite the ability-path, Result-widening, and full-Shahrazad issues (replace `#N2`/`#N3`/`#N4`):

```haskell
  -- ... Multi-player "each player who doesn't win" and a widened Result are
  -- deferred (#N3); this arm runs only on the SPELL path -- an ability-driven
  -- subgame is deferred (#N2). The fixed follow-on stands in for Shahrazad's
  -- half-life rider (#N4), which retires the synthetic gate.
```

In `source/library/Pawl/Engine.hs`, add one line at `playSubgame` citing the subsystem-blocked slice (replace `#N5`):

```haskell
-- ... Nontraditional/Vanguard/Commander subgame movement and cards brought into
-- a subgame are subsystem-blocked (#N5).
```

- [x] **Step 3: Build (comment-only) and commit**

```bash
cabal build all --enable-tests --enable-benchmarks
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "docs(m5c): file CR 729 deferrals (first-player, ability-path, Result, full Shahrazad, subsystems), cite inline"
```

---

### Task 10: close-out — verify, record, and land M5 (exit criterion)

Land the phase — and the M5 milestone — per `docs/workflow.md`: definitive warning-clean build, full suite, invariant/rules audit, then the three docs updates and the M5 exit note.

**Files:**
- Modify: `docs/progress.md` (append the M5c entry), `CLAUDE.md` (replace the status bullet), `docs/superpowers/specs/2026-07-23-m5-player-control-restart-subgames-design.md` (tick M5c; note M5 exit).

- [x] **Step 1: Definitive warning-clean build**

```bash
cabal clean
cabal build all --enable-tests --enable-benchmarks
```
Expected: **no warnings, no errors** (a clean build defeats incremental warning-hiding — the new `PlaySubgame` arms in unchanged-looking modules are re-checked).

- [x] **Step 2: Full test suite green**

```bash
cabal test
```
Expected: **all pass**, including Tasks 1–8. Do not proceed if anything is red.

- [x] **Step 3: Invariant + rules audit**

Confirm by inspection (note anything material in the commit body):
- **Invariant 1:** `Pawl.Resolve` cases only on the `PlaySubgame` *classification*; there is no `case … Shahrazad`. The outcome plumbing is generic — `PlaySubgame` binds a slot; "the loser takes damage" is ordinary `DealDamage` data. `playSubgame`/`subgameStateFrom`/`funnelBack` read `Source.OfCard` (a classification), never a card name.
- **Invariant 2:** every subgame choice is a prompt through the same interpreter (untagged; shuffles remain `Prompt.Shuffle`). The one elision (first-player RNG) carries an expiry-tagged issue (Task 9) cited inline.
- **Rules re-check** against `docs/rules.txt`: CR 729.1 (6275), 729.1a (6277), 729.1b (6279), 729.2 (6281), 729.3 (6289), 729.4 (6291), 729.4b (6295), 729.5 (6297), 729.6 (6306); CR 103 (258); CR 704.5b draw-from-empty. Delegate this citation pass to a cheap model if using subagents (Haiku, per `docs/workflow.md`).

- [x] **Step 4: Append the M5c completion entry to `docs/progress.md`**

Add after the M5b entry (matching the surrounding bullet style). Replace `#Nk` with the Task 9 issue numbers:

```markdown
- **M5c is complete** (Subgames — CR 729 — the M5 go/no-go, and the M5 exit).
  **Gate: a labeled-synthetic "Synthetic Subgame" sorcery** (the `Landform` crutch;
  documented expiry → **Shahrazad**, #N4) whose one mode is
  `[PlaySubgame "loser", DealDamage "loser" (Literal 3)]`. The decision it proves,
  the day-one suspended-continuation bet (design.md §2.1/§3): **a subgame is a
  function call.** `Engine.playSubgame` runs
  `Trans.lift (runStateT (startGameFromCards >> playGame) sub0)` — the nested game
  sequenced into the parent's `StateT GameState (Program Prompt)`, so its prompts
  flow through the **same** `Program`/`Replay` fold the main game uses (untagged;
  scripted interpreters and deterministic replay work unchanged — a
  `Prompt.PlaySubgame` was **rejected** precisely because it would bypass
  `Replay.record` and break determinism). The parent state sits untouched in the
  outer frame while the subgame runs (CR 729.1a); **nesting (CR 729.6) is free
  recursion** — each level's `priorityLoop` re-supplies the runner, no `GameState`
  stack field. **Runner injection:** `playSubgame` lives in `Engine` (it needs
  `playGame`) and is threaded **down** the spell path as a `Game Result` through
  new `Stack.resolveTopWith` → `Resolve.resolveSpellWith` → `Resolve.applyEffectWith`
  (bare names kept as `…With Resolve.noSubgame` wrappers, so **none** of the 105
  `resolveTop` / 9 `applyEffect` existing call sites changed) — the inversion that
  lets the bottom-layer resolver reach the top-layer loop without a cycle.
  **Outcome plumbing (CR 729.1b):** `Effect.PlaySubgame SlotName` **defines** its
  slot (the `Create` pattern, via `Resolve.definedSlots`), binding the derived
  2-player loser (`ToPlayer`); a later `DealDamage` reads it — enabled by
  `resolveSpellWith` **re-reading** the resolving object's bindings per effect
  (target legality still fixed at resolution start; only a newly-*defined* reserved
  slot, always vacuously legal, becomes visible), generalizing the mid-resolution
  binding `Create` writes but nothing yet read. **Construction/teardown:**
  `Setup.subgameStateFrom` (CR 729.2 — library cards only, players reset, id supply
  **inherited** so subgame ids never collide at return) and `Setup.funnelBack`
  (CR 729.5 — each owner's `OfCard` objects return to their main library, the
  parent's non-library board untouched, ids merged collision-free, supplies advanced);
  `playSubgame` reshuffles (`Prompt.Shuffle`). **Falls out for free:** CR 729.3
  (a <7-card library decks in the subgame's opening draw → loses at the first SBA,
  reusing the draw-from-empty path) and CR 729.4b (main-game player counters are
  outside the subgame — `funnelBack` never touches the parent's players; subgame
  counters cease when the subgame state is dropped). **Added:** `Setup.subgameStateFrom`/
  `funnelBack`; `Engine.playSubgame`; one opcode `Effect.PlaySubgame SlotName`
  (five classifier arms + a `definedSlots` arm + two `Codec` arms + `applyEffectWith`
  executor + `noSubgame` + `bindLoserSlot`); the `resolveSpellWith`/`resolveTopWith`
  runner-carrying variants; the `synthetic-subgame` card. **Deferred:** subgame
  first-player RNG (#N1, elision), ability-path subgames (#N2), Result-widening for
  multi-player non-winners (#N3), full Shahrazad's half-life rider (#N4,
  card-driven), and the subsystem-blocked slices — nontraditional/Vanguard/Commander
  movement, cards brought into a subgame, and subgame prompt tagging (#N5).
  **M5 exits here:** control (M5a), restart (M5b), and subgames (M5c) close
  design.md §3's "nightmares"; the closed half is functionally complete for its
  flagged surface. Spec (umbrella) and plan kept as reference:
  `docs/superpowers/specs/2026-07-23-m5-player-control-restart-subgames-design.md`
  and `docs/superpowers/plans/2026-07-23-m5c-subgames.md`.
```

- [x] **Step 5: Replace the `CLAUDE.md` status bullet**

In `CLAUDE.md`, the "Current work and tracking" first bullet currently ends with the M5b-landed / "M5c … is the next phase to plan" framing. **Replace** the tail (do not append a second status bullet) so it reads that M5c has landed and **M5 is complete**, keeping the M0–M5b history intact:

> ... **M5c (Subgames, CR 729 — the M5 go/no-go) has landed, and with it M5 is
> complete.** A labeled-synthetic "Synthetic Subgame" sorcery (documented expiry →
> Shahrazad) drives a gameplay gate over a new `Effect.PlaySubgame SlotName` opcode:
> a subgame is `Trans.lift (runStateT (startGameFromCards >> playGame) sub0)`
> sequenced into the resolving effect, sharing the one `Program`/`Replay`
> interpreter (a `Prompt.PlaySubgame` was rejected for breaking replay); the runner
> is injected down the spell path (`resolveTopWith`/`resolveSpellWith`/
> `applyEffectWith`, bare names kept as `noSubgame` wrappers) since `Resolve` sits
> below `Engine`. `PlaySubgame` defines a slot binding the derived loser (CR 729.1b,
> read via a per-effect binding re-read in `resolveSpellWith`); `Setup.subgameStateFrom`/
> `funnelBack` build from the library only (CR 729.2) and return owned cards (CR
> 729.5) with an inherited id supply; nesting (CR 729.6) is free recursion. Deferred:
> subgame first-player RNG, ability-path subgames, Result-widening, full Shahrazad,
> and the subsystem-blocked movement slices. With control, restart, and subgames
> closed, **the closed half is functionally complete for its flagged surface; M6
> (the transpiler) is next.** The umbrella spec is
> `docs/superpowers/specs/2026-07-23-m5-player-control-restart-subgames-design.md`.

Keep the existing pointers to `docs/progress.md` and the umbrella spec intact.

- [x] **Step 6: Tick M5c and note the M5 exit in the umbrella spec**

In `docs/superpowers/specs/2026-07-23-m5-player-control-restart-subgames-design.md`, mark the **M5c** row of the §3 phase table as landed: change `| **M5c** |` to `| **M5c ✅** |` (per §6). Optionally add a one-line note under §5 that the exit criterion is met (all three phases have a passing gameplay-level gate).

- [x] **Step 7: Commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "docs(m5c): completion entry, CLAUDE.md status, umbrella tick — M5 complete"
```

- [x] **Step 8: Confirm the plan is fully executed**

```bash
grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-23-m5c-subgames.md
```
Expected: **`0`** (every step checked off). Confirm `git log --oneline -10` shows the ten M5c commits on `main`.

---

## Self-review notes (for the executor)

- **Spec coverage (umbrella §3 M5c row + §3 notes).** `runStateT playGame` inside the effect → Tasks 3–4; `startGameFromCards` from the library only (CR 729.2) → Task 1 (`subgameStateFrom`); generic `PlaySubgame` binding the outcome to a slot → Tasks 2–3; ordinary follow-on reading the slot (CR 729.1b) → Task 3 + the gate card (Task 5) + Task 6; funnel-back (CR 729.5) → Task 1 (`funnelBack`) + Task 7; arbitrary nesting (CR 729.6) → Task 8; untagged subgame prompts (tests work directly) → the `subgameAnswer` reuse in Tasks 6–8; CR 729.3 short-deck loss → Tasks 4/6; CR 729.4b counter isolation → Task 7; deferrals (first-player RNG, ability-path, Result-widening, full Shahrazad, subsystem-blocked) → Task 9; M5 exit criterion → Task 10.
- **The go/no-go bet, made explicit.** The architecture section is the review surface. The two decisions that could sink the phase — (a) subgame-as-lifted-`runStateT` vs. a `Prompt` (chosen for replay determinism), and (b) runner injection via `…With` wrappers vs. threading through all 105+ call sites (chosen for zero churn) — are stated with their falsifiers. If either looks wrong, STOP and say so before Task 3 (CLAUDE.md: "If the plan looks wrong, stop and say so").
- **The per-effect binding re-read is load-bearing and must be behavior-preserving.** Task 3 Step 6 runs the **full** suite immediately after the re-read lands, because it touches the hot spell path. It is provably equivalent for existing cards (none read a mid-resolution binding; target legality is recomputed against the same pre-fold state). A red existing test there means the re-read changed target behavior — fix the re-read, never weaken the other test.
- **These gate tests have explicit teeth.** Tasks 6–8 each include a reverted mutation proving the assertion depends on the rule under test (the M5a/M5b discipline for characterization-style gates). Confirm `git diff` on library files is empty after each. Never weaken an assertion or delete a test to make a check pass (CLAUDE.md).
- **Type consistency.** `subgameStateFrom :: GameState -> GameState`; `funnelBack :: GameState -> GameState -> GameState`; `playSubgame :: Game Result`; `noSubgame :: Game Result`; `Effect.PlaySubgame :: SlotName -> Effect card`; `applyEffectWith :: Game Result -> …`; `resolveSpellWith :: Game Result -> ObjectId -> Game ()`; `Stack.resolveTopWith :: Game Result -> Game ()`; `bindLoserSlot :: ObjectId -> SlotName -> PlayerId -> GameState -> GameState`; `Cards.syntheticSubgamePrinting :: Cards -> Printing`; `subgameAnswer :: Prompt.Prompt r -> r`. The bare `applyEffect`/`resolveSpell`/`resolveTop` keep their **existing** signatures (they are `…With noSubgame` wrappers) — this is the whole point of the wrapper strategy and why no existing call site changes.
- **No new opcode is missed.** Adding `Effect.PlaySubgame` makes all five classifier `case`s (`slotsOf`/`effectReadsX`/`manaProduced`/`searchesLibrary`/`rewriteEffect` — `effectReadsX` is exhaustive per-constructor with no `_ ->` fallthrough, so it is forced too) and both `Codec` `case`s non-exhaustive; `-Wincomplete-patterns` under `-Werror` fails until each arm exists (Task 2). `definedSlots` and `bindsSeveralTokens` use a `_ -> …` fallthrough, so only `definedSlots` needs a hand-added arm (Task 2 Step 4); the compiler will not remind you. The `allPrintings` round-trip (Task 5) is the codec arms' behavioral gate; the D4 lint (Task 5) is the `definedSlots` arm's gate.
- **Helper duplication is intentional.** `poolToLibrary` (SetupSpec) vs. `poolToLibraryG` (GameSpec), and `addMany`/`addManyG`, are structurally identical per-file helpers (the documented exception is that `Support` fixtures are shared; group-local helpers stay with their group, CLAUDE.md). `handCard`/`libraryCard`/`addToLibraryG` are GameSpec-local; if `handBobBolt` already generalizes, reuse it instead.
