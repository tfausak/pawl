# M5b — Restarting the Game (CR 727) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close CR 727 (Restarting the Game) at the *rebuild* depth its labeled-synthetic gate demands — a shared `startGameFromCards` primitive that builds a game from an existing object pool (ownership preserved), a `restartGame` routine keyed off a generic nullary `RestartGame` opcode, and a `synthetic-restart` gate card that proves at gameplay level that restart rebuilds from the game's *actual cards* with the *restart's controller* as starting player (CR 727.1a/727.2/727.4) — **without** wiring live multi-turn `playGame` resumption (deferred by decision) and **without** casing on any card's identity.

**Architecture:** Restart is **replace-in-place**: it rebuilds the single `GameState` slot from the objects already in the game, not from `Deck` definitions and not via `Setup.emptyGame` + `Setup.newGame` (which would mint fresh objects and pick the wrong starting player — the milestone's stated falsifier). Two new **`Setup`** functions do the work: `startGameFromCards :: Game ()` (reused verbatim by M5c subgames) and `restartGame :: PlayerId -> Game ()`. A new **nullary** `Effect.RestartGame` opcode (shaped like `ExileAllGraveyards`/`BecomeMonarch` — targetless, game-wide) resolves in `Pawl.Resolve.applyEffect` by calling `Setup.restartGame controller`; the starting player is the resolving controller (CR 727.1a), which `applyEffect` already holds, so no target slot is needed. A `synthetic-restart` artifact (the `Landform` labeled-synthetic-crutch pattern) carries an activated ability whose only effect is `RestartGame`, and drives the gameplay-level gate through the real priority loop.

**Tech Stack:** Haskell 2010 (GHC 9.14.1 from the Nix dev shell), `tasty` + `tasty-hunit`. Library under `source/library/Pawl/`; tests under `source/test-suite/Pawl/`; cards are JSON data files under `data/cards/`.

## Global Constraints

Every task's requirements implicitly include this section. Values copied verbatim from `CLAUDE.md`.

- **Warning-clean build.** `cabal build all --enable-tests --enable-benchmarks` must compile under `-Weverything` minus the allow-list, with `flags: +pedantic` (`-Werror`) on. Incremental builds hide warnings from unchanged modules — for a definitive check, `cabal clean` first. **Adding a constructor to `Effect` makes every `case effect of` non-exhaustive; `-Wincomplete-patterns` under `-Werror` will fail the build until all arms are added** — this is the safety net that guarantees Task 3 touches every dispatch site.
- **No new language extensions.** Haskell 2010. `NamedFieldPuns` is permitted where it improves clarity but is not required here. Test files that pattern-match the `Prompt` GADT already carry `{-# LANGUAGE GADTs #-}`; do not add others.
- **Qualified imports, aliased to the last component** (`Pawl.Setup` → `Setup`, `Data.List` → `List`); operators unqualified. One import group, no first/third-party split. A module never imports its parents; `A.B.C` never imports `A.B` or `A`.
- **No partial functions, written or used.** No `head`, `undefined`, `error`, `!!`, or non-exhaustive matches. Use `Data.Maybe.listToMaybe` instead of `head`. `Map.filter`/`Map.keys`/`map`, never a list comprehension (**CLAUDE.md: "No list comprehensions"**).
- **`newtype`/record conventions.** Build records with `do` + record syntax or record-update syntax, not `<$>`/`<*>`. `case` over point-free; `let` over `where`.
- **Cite the CR number in every rules-bearing comment.** Every CR claim below was checked against `docs/rules.txt` at this planning pass (CR 727 at lines 6246–6265; CR 103 "Starting the Game" at lines 258–322; CR 103.5 opening hands at 296; CR 103.8a starting player skips first draw at 318). Never trust recalled Magic rules; re-verify before the number drives code.
- **The two invariants outrank this plan.** (1) The rules core reads a *classification*, never an effect's identity — restart is a routine keyed off a generic `RestartGame` opcode; `Pawl.Resolve` never grows a `case … Karn`. (2) The engine makes no choices — the starting player is fixed by rule (CR 727.1a: the controller), not prompted; shuffles remain `Prompt.Shuffle`. Nothing is elided.
- **One small complete commit per task, on `main`.** Never push. Commit message trailer: `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>`.
- **`hooky` before done.** `git add -A` → `hooky fix` → `git add -A` → `hooky run` (acts on staged files only).
- **TDD is not optional.** Write each failing test and run it to watch it fail before implementing. Tick each `- [ ]` as you finish it.

---

## Scope decision (locked)

**Rebuild-only.** M5b builds the rebuild primitive and proves it with a labeled-synthetic gate. It does **not** wire a restart that fires *during a live `playGame`* to cleanly abandon the current turn and resume into the new turn 1 — that needs loop-bail guards in `priorityLoop`/`runStep` and is a separate control-flow concern that pairs with full Karn Liberated. It is **deferred and filed as an issue in Task 6**. The milestone falsifier ("restart ≠ fresh setup: rebuild from actual cards, starting player = controller") is entirely about the rebuild, which the gate proves via resolution + assertion — exactly how M5a's Mindslaver gate proved control via `priorityLoop`/`handoffTurn` segments without playing a whole game.

## Substrate reference (read once before starting)

Exact shapes the tasks build on (paths/line numbers from this planning pass — re-confirm if the file has moved):

- `Pawl.Setup` (`source/library/Pawl/Setup.hs`): `startingLife :: Integer` = 20 (line 33); `openingHand :: Int` = 7 (line 43); `emptyGame :: NonEmpty PlayerId -> GameState` (47) — the field template for a fresh `GameState`; `createCard` (95) — the per-object shape (`owner`, `source = Source.OfCard printing`, `zone = Zone.Library`, `tapped = Untapped`, `damage = 0`, `sickness = Sick`, `bindings = Map.empty`, `counters = Map.empty`, fresh `timestamp`); `shuffleLibrary :: PlayerId -> Game ()` (120) — issues `Prompt.Shuffle`; `newGame` (133). Setup already imports `Combat`, `Event`, `Game`, `Turn`, `Object`, `Player`, `Source`, `Status`, `Sickness`, `TapState`, `Zone`, `GameState`, `Map`, `Seq`, `Monad`, `NonEmpty`.
- `Pawl.Type.GameState` (`source/library/Pawl/Type/GameState.hs`): one record, `deriving (Eq, Show)`. Fields used below: `objects :: Map ObjectId Object`, per-player `library`/`hand`/`graveyard :: Map PlayerId (Seq ObjectId)`, shared `battlefield`/`exile`/`command :: Set ObjectId`, `stack :: [ObjectId]`, `players :: Map PlayerId Player`, `manaPool`, `combat`, `events`, `scannedThrough`/`damageScannedThrough :: Natural`, `delayedTriggers`, `continuousEffects`, `replacements`, `playerEffects`, `turnOrder :: [PlayerId]`, `activePlayer :: PlayerId`, `phase :: Phase`, `remaining :: Seq Phase`, `priority :: Maybe PlayerId`, `passes :: Natural`, `turnNumber :: Natural`, `result :: Maybe Result`, `nextObjectId`, `nextTimestamp`, `drewFromEmpty :: Set PlayerId`, `landPlayed :: Set PlayerId`, `pendingControl`, `activeControl`, `monarch`, `exiledUntilMonarch`. **No RNG/seed field** — randomness is `Prompt.Shuffle`.
- `Pawl.Type.Object` (`source/library/Pawl/Type/Object.hs`): `owner :: PlayerId` (stamped once, never changes), `source`, `zone`, `tapped`, `damage`, `sickness`, `bindings`, `counters`, `timestamp`.
- `Pawl.Type.Source` (`source/library/Pawl/Type/Source.hs`): `OfCard Printing | OfToken Card | OfAbility ObjectId (ActivatedAbility Card) | OfTrigger ObjectId (TriggeredAbility Card) | OfEmblem Card | OfInherentTrigger PlayerId (TriggeredAbility Card)`. **Only `OfCard` is a Magic card** (CR 727.2 / 111.7).
- `Pawl.Type.Player` (`source/library/Pawl/Type/Player.hs`): `MkPlayer { life :: Integer, status :: Status, counters :: Map PlayerCounterKind Natural }`. `Pawl.Type.Status`: `Playing | Departed Departure`.
- `Pawl.Turn` (`source/library/Pawl/Turn.hs`): `firstPhase :: Phase` = `Phase.Beginning BeginningStep.Untap`; `laterPhases :: Seq Phase`. Both reused verbatim by `Setup.emptyGame`.
- `Pawl.Type.Effect` (`source/library/Pawl/Type/Effect.hs`): `data Effect card = … deriving (Eq, Ord, Show)`. Nullary game-wide precedents: `ExileAllGraveyards` (constructor at line 58), `BecomeMonarch MonarchTarget` (192). Last constructor `ExileUntilMonarch SlotName` (198), `deriving` at 199.
- `Pawl.Resolve` (`source/library/Pawl/Resolve.hs`) — the **only** module that cases on `Effect`. Six `case effect of` tables that each need a new arm: `slotsOf` (67), `readsX`/`effectReadsX` (104), `manaProduced` (135), `searchesLibrary` (166), `rewriteEffect` (236), `applyEffect` (405). Existing nullary arms to copy: `Effect.ExileAllGraveyards -> Set.empty` / `-> False` / `-> Nothing` / `-> False` / `-> effect`, and the `applyEffect` body at line 507. `applyEffect`'s signature: `ObjectId -> PlayerId -> Map SlotName (Subtype,Subtype) -> Map SlotName Bool -> Map SlotName Recipient -> Effect Card -> Game ()`; its second argument is `controller` (the resolving controller = CR 727.1a's starting player). **`Pawl.Resolve` does not currently import `Pawl.Setup`; add it** — `Setup` does not import `Resolve` (nor do `Event`/`Combat`/`Turn`, its deps), so there is no cycle.
- `Pawl.Codec` (`source/library/Pawl/Codec.hs`): `effectToJson` — nullary pattern `Effect.ExileAllGraveyards -> nullary (Text.pack "ExileAllGraveyards")` (~1153); `jsonToEffect` — `"ExileAllGraveyards" -> Right Effect.ExileAllGraveyards` (~1188). A new opcode needs both arms or the `allPrintings` round-trip test fails. **CR 118.6 (Codec.hs:1299):** an absent/`null` `mana` field decodes to `Nothing` (an *unpayable* cost), never `{0}` — so an activatable ability must use `"mana": []` (a payable `{0}`), not `null`.
- Card data (`data/cards/*.json`): tagged JSON unions (`{"type": …, "value": …}`); nullary effects are `{"type": "Name"}` with no `"value"`. `mindslaver.json` is the structural template for an artifact with an activated ability. Synthetic cards use a `synthetic-` slug prefix (`synthetic-modal-activator.json` etc.).
- `Pawl.Cards` (`source/test-suite/Pawl/Cards.hs`) — hand-written registry. Adding a card touches four sites: the `data Cards` record field (~16–99), a `…_ <- loadPrinting "<slug>"` line in `loadCards` (~111–192), the `… = …_,` record line (~195–277), and `allPrintings` (~281–363, required or the honesty/round-trip test flags it).
- Test harness: `Engine.runGamePure :: (forall r. Prompt r -> r) -> GameState -> Game a -> (a, GameState)`; `Engine.priorityLoop :: Game ()`; `Engine.checkSba :: Game ()`; `Stack.resolveTop :: Game ()`; `S.identityAnswer` (answers `Prompt.Shuffle ids -> ids`, `ChooseAction -> A.Pass`, everything else minimally); `S.addCreature :: Printing -> PlayerId -> GameState -> (ObjectId, GameState)` (places a permanent **Settled, Untapped, on the battlefield** under the player); `S.bothPlayers :: NonEmpty PlayerId` = alice then bob; `S.alice`, `S.bob`; `S.lifeOf :: PlayerId -> GameState -> Maybe Integer`; `S.handSize :: PlayerId -> GameState -> Int`. `isActivateAction :: A.Action -> Bool` already exists in `GameSpec.hs` (added in M5a).

## File structure

| File | Change | Responsibility |
|---|---|---|
| `source/library/Pawl/Setup.hs` | Modify | Tasks 1–2: `startGameFromCards`, `rotateTo`, `restartGame`. |
| `source/test-suite/Pawl/SetupSpec.hs` | Modify | Tasks 1–2: `startGameFromCards` and `restartGame` unit tests, including the CR 727.1a/727.2/727.3/727.4 falsifiers. |
| `source/library/Pawl/Type/Effect.hs` | Modify | Task 3: the `RestartGame` nullary constructor. |
| `source/library/Pawl/Resolve.hs` | Modify | Task 3: six dispatch arms + the `Setup` import + the `applyEffect` executor arm. |
| `source/library/Pawl/Codec.hs` | Modify | Task 3: `effectToJson`/`jsonToEffect` arms. |
| `source/test-suite/Pawl/ResolveSpec.hs` | Modify | Task 3: resolve a hand-built `RestartGame` ability end-to-end. |
| `data/cards/synthetic-restart.json` | Create | Task 4: the labeled-synthetic gate card. |
| `source/test-suite/Pawl/Cards.hs` | Modify | Task 4: register `syntheticRestartPrinting` (4 sites). |
| `source/test-suite/Pawl/GameSpec.hs` | Modify | Task 5: gameplay-level headline gate through `priorityLoop`. |
| GitHub issues (`tfausak/pawl`) | Create | Task 6: deferrals (live-loop re-entry; full Karn 727.5/727.5a). |
| `docs/progress.md`, `CLAUDE.md`, umbrella spec | Modify | Task 7: completion entry, status replacement, phase tick. |

---

### Task 1: `startGameFromCards` — build a game from an existing object pool

The reusable primitive (M5c's subgames call it verbatim). It rebuilds every player's library from the **objects already in the game** — each player's owned *cards*, wherever they sit — then shuffles and draws opening hands. Non-card objects (abilities on the stack, tokens, emblems, triggers) are **not** Magic cards (CR 727.2 / 111.7) and cease to exist. This is deliberately *not* `Setup.newGame` (which mints fresh objects from `Deck` definitions).

**Files:**
- Modify: `source/library/Pawl/Setup.hs` — one new top-level function.
- Test: `source/test-suite/Pawl/SetupSpec.hs` — a new `restartTests` group wired into `tests`.

**Interfaces:**
- Consumes: `GameState.{objects,turnOrder,…}`, `Object.{owner,source,zone,tapped,damage,sickness,bindings,counters}`, `Source.OfCard`, `Zone.Library`, `TapState.Untapped`, `Sickness.Sick`, `shuffleLibrary`, `openingHand`, `Event.drawCard`.
- Produces (used by Task 2 and by M5c): `startGameFromCards :: Game ()`.

- [x] **Step 1: Write the failing test**

Add a `restartTests` group and wire it into `SetupSpec.tests`. First, at the top of `source/test-suite/Pawl/SetupSpec.hs` add these imports to the import group (alphabetical among the `Pawl.*` imports):

```haskell
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Pawl.Event as Event
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.Object as Object
```

Then append the group (top level) and update the aggregator. The helper `addMany` folds `S.addCreature` (which places each object on the battlefield) to build a pool of owned cards:

```haskell
-- Add n Mountains to pid's battlefield, discarding the ids (used to bulk up a
-- pool of owned cards). replicate n () avoids a list comprehension (CLAUDE.md).
addMany :: Cards.Cards -> Int -> S.PlayerId -> GameState.GameState -> GameState.GameState
addMany cards n pid gs =
  List.foldl' (\g _ -> snd (S.addCreature (Cards.mountainPrinting cards) pid g)) gs (replicate n ())

restartTests :: Cards.Cards -> Tasty.TestTree
restartTests cards =
  Tasty.testGroup
    "restart (CR 727)"
    [ HU.testCase "startGameFromCards: libraries are rebuilt from the existing owned cards, hands drawn" $
        -- alice and bob each own 8 cards, all currently on the battlefield. After
        -- startGameFromCards each has a 7-card hand and a 1-card library, the
        -- battlefield is empty, and ownership is unchanged (CR 727.2 / 103.5).
        let g0 = Setup.emptyGame S.bothPlayers
            g1 = addMany cards 8 S.alice g0
            g2 = addMany cards 8 S.bob g1
            after = snd (Engine.runGamePure S.identityAnswer g2 Setup.startGameFromCards)
            libSize pid = length (Game.zoneMembers Zone.Library pid after)
         in do
              HU.assertEqual "alice drew a 7-card opening hand" 7 (S.handSize S.alice after)
              HU.assertEqual "bob drew a 7-card opening hand" 7 (S.handSize S.bob after)
              HU.assertEqual "alice's library holds the remaining owned card" 1 (libSize S.alice)
              HU.assertEqual "bob's library holds the remaining owned card" 1 (libSize S.bob)
              HU.assertEqual "the battlefield is empty after the rebuild" True (Set.null (GameState.battlefield after))
              HU.assertEqual "every rebuilt object is owned by alice or bob (ownership preserved)" True (all (\o -> Object.owner o == S.alice || Object.owner o == S.bob) (Map.elems (GameState.objects after)))
    ]
```

And change the aggregator at the bottom of the file from

```haskell
tests cards = Tasty.testGroup "Setup" [setupTests cards, greenBlackSetupTests cards, deckTests cards]
```

to

```haskell
tests cards = Tasty.testGroup "Setup" [setupTests cards, greenBlackSetupTests cards, deckTests cards, restartTests cards]
```

> `S.PlayerId` may not be re-exported by `Support`; if the compiler cannot find it, import `Pawl.Type.PlayerId (PlayerId)` and write `PlayerId` in `addMany`'s signature instead. `Game.zoneMembers`, `GameState`, `Zone`, `Map` are already imported in `SetupSpec.hs`.

- [x] **Step 2: Run the test to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20`
Expected: **compile error** — `Setup.startGameFromCards` is not in scope (not yet defined).

- [x] **Step 3: Implement `startGameFromCards`**

Append to `source/library/Pawl/Setup.hs` (after `newGame`):

```haskell
-- CR 727.2 / 729.2: build every player's library from an EXISTING object pool --
-- each player's owned CARDS, wherever they currently sit -- then shuffle and draw
-- opening hands (CR 103.5). This is deliberately NOT newGame: it reuses the real
-- objects (ownership preserved, CR 727.2) instead of minting fresh ones from Deck
-- definitions. Only Magic cards survive: an ability on the stack, a token, an
-- emblem, or a trigger is not a card (CR 727.2 / 111.7) and ceases to exist.
-- Shared by restart (CR 727) and, later, subgames (CR 729).
startGameFromCards :: Game ()
startGameFromCards = do
  gs <- State.get
  let owners = GameState.turnOrder gs
      isCard obj = case Object.source obj of
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
      cards = Map.map toLibraryCard (Map.filter isCard (GameState.objects gs))
      libraryOf pid = Seq.fromList (Map.keys (Map.filter (\obj -> Object.owner obj == pid) cards))
  State.put
    gs
      { GameState.objects = cards,
        GameState.library = Map.fromList (map (\pid -> (pid, libraryOf pid)) owners),
        GameState.hand = Map.empty,
        GameState.graveyard = Map.empty,
        GameState.battlefield = mempty,
        GameState.exile = mempty,
        GameState.command = mempty,
        GameState.stack = []
      }
  Monad.forM_ owners $ \pid -> do
    shuffleLibrary pid
    Monad.replicateM_ openingHand (Event.drawCard pid)
```

- [x] **Step 4: Run the test to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "$0~/startGameFromCards/"'`
Expected: **PASS** (warning-clean build, the case passes).

- [x] **Step 5: Commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "feat(m5b): startGameFromCards builds a game from an existing object pool (CR 727.2/729.2)"
```

---

### Task 2: `restartGame` — the CR 727 replace-in-place routine

`restartGame starter` ends the current game and begins a new one in the same `GameState` slot: every card returns to its owner's new library (CR 727.2), the starting player is `starter` (CR 727.1a) so the turn order is rotated to begin with them (CR 103), players reset to 20 life / Playing / no counters, all transient game state clears, and the state settles to *just before the first turn's untap step* (CR 727.4): phase `Beginning Untap`, no priority, turn 1. The object/timestamp id supplies are preserved so reused cards keep unique ids.

**Files:**
- Modify: `source/library/Pawl/Setup.hs` — `rotateTo` + `restartGame`.
- Test: `source/test-suite/Pawl/SetupSpec.hs` — four cases added to `restartTests` (727.1a, 727.2, 727.4, 727.3).

**Interfaces:**
- Consumes: `startGameFromCards`, `startingLife`, `Player.{life,status,counters}`, `Status.Playing`, `Combat.emptyCombat`, `Turn.{firstPhase,laterPhases}`, all `GameState` transient fields.
- Produces: `rotateTo :: PlayerId -> [PlayerId] -> [PlayerId]`; `restartGame :: PlayerId -> Game ()`.

- [x] **Step 1: Write the failing tests**

Add these four cases to the `restartTests` list in `source/test-suite/Pawl/SetupSpec.hs` (comma-separated after the Task 1 case). They need `Pawl.Sba`, `Pawl.Type.Status`, `Pawl.Type.Result`, and `Data.Maybe` imported — add to the import group any of these not already present:

```haskell
import qualified Data.Maybe as Maybe
import qualified Pawl.Sba as Sba
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Status as Status
```

The cases:

```haskell
    , HU.testCase "CR 727.1a: the starting player is the restart's controller, at the head of the turn order" $
        -- Two restarts of the same board, controlled by different players: the
        -- active player and the head of the turn order follow the controller.
        let g0 = addMany cards 8 S.bob (addMany cards 8 S.alice (Setup.emptyGame S.bothPlayers))
            byBob = snd (Engine.runGamePure S.identityAnswer g0 (Setup.restartGame S.bob))
            byAlice = snd (Engine.runGamePure S.identityAnswer g0 (Setup.restartGame S.alice))
         in do
              HU.assertEqual "bob restarted: bob is the new active player" S.bob (GameState.activePlayer byBob)
              HU.assertEqual "bob restarted: bob heads the turn order" (Just S.bob) (Maybe.listToMaybe (GameState.turnOrder byBob))
              HU.assertEqual "alice restarted: alice is the new active player" S.alice (GameState.activePlayer byAlice)
              HU.assertEqual "alice restarted: alice heads the turn order" (Just S.alice) (Maybe.listToMaybe (GameState.turnOrder byAlice))
    , HU.testCase "CR 727.2: every owned card returns to its owner (library or hand), regardless of prior zone" $
        -- alice owns 8 cards, one on the battlefield; bob owns 8, one moved to his
        -- graveyard. CR 400.7 gives drawn cards FRESH ids (Event.changeZone mints a
        -- new object on a zone change), so a specific pre-restart ObjectId need not
        -- survive an opening draw -- CR 727.2 preserves OWNERSHIP, not object ids.
        -- Assert on per-owner counts: after the restart every owned card is in that
        -- owner's library or hand, none on the battlefield or in a graveyard, and
        -- bob's graveyard card is proven to return by his count staying 8.
        let g0 = Setup.emptyGame S.bothPlayers
            (_aId, g1) = S.addCreature (Cards.mountainPrinting cards) S.alice g0
            (bId, g2) = S.addCreature (Cards.mountainPrinting cards) S.bob g1
            g3 = addMany cards 7 S.alice (addMany cards 7 S.bob g2)
            -- move bob's card to his graveyard, to prove zone-independence.
            g4 = snd (Engine.runGamePure S.identityAnswer g3 (Event.changeZone bId Zone.Graveyard))
            after = snd (Engine.runGamePure S.identityAnswer g4 (Setup.restartGame S.alice))
            ownedCount pid = length (filter (\o -> Object.owner o == pid) (Map.elems (GameState.objects after)))
            libHandCount pid = length (Game.zoneMembers Zone.Library pid after) + length (Game.zoneMembers Zone.Hand pid after)
         in do
              HU.assertEqual "alice still owns all 8 of her cards" 8 (ownedCount S.alice)
              HU.assertEqual "bob still owns all 8 of his cards (incl. the one from his graveyard)" 8 (ownedCount S.bob)
              HU.assertEqual "all of alice's cards are in her library or hand" 8 (libHandCount S.alice)
              HU.assertEqual "all of bob's cards are in his library or hand" 8 (libHandCount S.bob)
              HU.assertEqual "no card is left on the battlefield" True (Set.null (GameState.battlefield after))
              HU.assertEqual "no graveyard survives the restart" True (all null (Map.elems (GameState.graveyard after)))
    , HU.testCase "CR 727.4: the restart settles just before the first untap step, no priority, turn 1, life reset" $
        let g0 = addMany cards 8 S.bob (addMany cards 8 S.alice (Setup.emptyGame S.bothPlayers))
            after = snd (Engine.runGamePure S.identityAnswer g0 (Setup.restartGame S.bob))
         in do
              HU.assertEqual "phase is the first turn's untap step" Turn.firstPhase (GameState.phase after)
              HU.assertEqual "no player holds priority" Nothing (GameState.priority after)
              HU.assertEqual "it is turn 1" 1 (GameState.turnNumber after)
              HU.assertEqual "the stack is empty" [] (GameState.stack after)
              HU.assertEqual "alice is back to 20 life" (Just 20) (S.lifeOf S.alice after)
              HU.assertEqual "bob is back to 20 life" (Just 20) (S.lifeOf S.bob after)
    , HU.testCase "CR 727.3: a player owning fewer than seven cards loses at the next SBA check" $
        -- bob owns only 3 cards; drawing an opening hand of 7 draws from an empty
        -- library, flagging drewFromEmpty, so the existing SBA path makes bob lose
        -- and alice win. (In live play this fires at the first upkeep, CR 727.3;
        -- here it is asserted at the next explicit SBA check.)
        let g0 = addMany cards 3 S.bob (addMany cards 8 S.alice (Setup.emptyGame S.bothPlayers))
            afterRestart = snd (Engine.runGamePure S.identityAnswer g0 (Setup.restartGame S.alice))
            afterSba = snd (Engine.runGamePure S.identityAnswer afterRestart Engine.checkSba)
         in do
              HU.assertEqual "bob drew from an empty library during the opening draw" True (Set.member S.bob (GameState.drewFromEmpty afterRestart))
              HU.assertEqual "CR 727.3: bob loses, alice wins at the SBA check" (Just (Result.Won S.alice)) (GameState.result afterSba)
    ]
```

> `Engine.checkSba` is defined in `Pawl.Engine` (`checkSba = Sba.checkStateBasedActions`); import `Pawl.Sba` only if you prefer `Sba.checkStateBasedActions` directly. `Game.lookupObject`, `Game.zoneMembers`, `Map`, `Zone` are already imported.

- [x] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20`
Expected: **compile error** — `Setup.restartGame` (and `rotateTo`) not in scope.

- [x] **Step 3: Implement `rotateTo` and `restartGame`**

Append to `source/library/Pawl/Setup.hs` (after `startGameFromCards`):

```haskell
-- CR 103 / 727.1a: put `starter` at the head of the turn order, preserving the
-- cyclic order ("the game's default turn order begins with the starting player
-- and proceeds clockwise"). Total: a `starter` not in the order leaves it as-is.
rotateTo :: PlayerId -> [PlayerId] -> [PlayerId]
rotateTo starter order = case break (== starter) order of
  (before, after) -> after ++ before

-- CR 727: restart the game in place. CR 727.1: the current game immediately ends
-- and a new game begins per CR 103, with the CR 727.1a exception -- the starting
-- player is `starter` (the controller of the restarting ability), so the turn
-- order is rotated to begin with them. CR 727.2: every card returns to its
-- owner's new library via startGameFromCards, built from the ACTUAL object pool
-- (never emptyGame+newGame, which would lose the real cards and pick the wrong
-- starting player). CR 727.4: the effect finishes resolving just before the first
-- turn's untap step, with no player holding priority -- phase = firstPhase,
-- priority = Nothing, turn 1. The object and timestamp id supplies are preserved
-- so reused cards keep unique ids; startGameFromCards rebuilds objects and zones.
restartGame :: PlayerId -> Game ()
restartGame starter = do
  State.modify' $ \gs ->
    let resetPlayer player =
          player
            { Player.life = startingLife,
              Player.status = Status.Playing,
              Player.counters = Map.empty
            }
     in gs
          { GameState.players = Map.map resetPlayer (GameState.players gs),
            GameState.manaPool = Map.empty,
            GameState.combat = Combat.emptyCombat,
            GameState.events = Seq.empty,
            GameState.scannedThrough = 0,
            GameState.damageScannedThrough = 0,
            GameState.delayedTriggers = Seq.empty,
            GameState.continuousEffects = [],
            GameState.replacements = [],
            GameState.playerEffects = [],
            GameState.turnOrder = rotateTo starter (GameState.turnOrder gs),
            GameState.activePlayer = starter,
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
  startGameFromCards
```

> This does not set `objects`/`library`/`hand`/`graveyard`/`battlefield`/`exile`/`command`/`stack` — `startGameFromCards` (called last) rebuilds all of them from the still-intact `objects`. `nextObjectId`/`nextTimestamp` are intentionally left untouched (preserved).

- [x] **Step 4: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "$0~/restart .CR 727/"'`
Expected: **PASS** — all four cases green, warning-clean build.

- [x] **Step 5: Commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "feat(m5b): restartGame rebuilds the game in place (CR 727.1a/727.2/727.3/727.4)"
```

---

### Task 3: the `RestartGame` opcode — wire the generic effect to the routine

A **nullary** `Effect.RestartGame` opcode (targetless, game-wide — the `ExileAllGraveyards`/`BecomeMonarch` shape). It resolves by calling `Setup.restartGame controller`: CR 727.1a's starting player is the resolving controller, which `applyEffect` already holds, so no target slot exists. The engine reaches restart through this generic opcode, never through a card's identity.

**Files:**
- Modify: `source/library/Pawl/Type/Effect.hs` (constructor), `source/library/Pawl/Resolve.hs` (import + six arms), `source/library/Pawl/Codec.hs` (two arms).
- Test: `source/test-suite/Pawl/ResolveSpec.hs` — resolve a hand-built `RestartGame` ability end-to-end.

**Interfaces:**
- Consumes: `Setup.restartGame`, the `applyEffect` `controller` argument.
- Produces: `Effect.RestartGame :: Effect card`; JSON tag `"RestartGame"`.

- [x] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/ResolveSpec.hs`, in the same `testGroup` list that holds the existing CR 723.1 installation test (the M5a `installControlBy` case). This resolves a hand-built ability whose only effect is `RestartGame`, owned by bob, on a board where alice owns a card — and asserts the game restarted with bob as the starting player and alice's card preserved. Reuse the file's existing ability-construction imports (it already imports `Object`, `Source`, `ActivatedAbility`, `Modal`, `Mode`, `Cost.Type`, `ManaCost`, `Binding`, `Zone`, `TapState`, `Sickness`, `Seq`, `Set`, `Engine`, `Stack`, `Game`, `GameState`); it needs `Effect` and `Setup` — add `import qualified Pawl.Setup as Setup` and confirm `import qualified Pawl.Type.Effect as Effect` are present, plus `import qualified Data.Maybe as Maybe`, `import qualified Pawl.Type.Object as Object`.

```haskell
      HU.testCase "CR 727.1a: resolving a RestartGame ability restarts with its controller as starting player" $
        let g0 = Setup.emptyGame S.bothPlayers
            -- alice owns a card on the battlefield; it must survive the restart.
            (aliceId, g1) = S.addCreature (Cards.mountainPrinting cards) S.alice g0
            -- bob owns 8 cards (enough for a full opening hand, no CR 727.3 loss).
            g2 = addMany cards 8 S.bob g1
            g3 = addMany cards 7 S.alice g2
            -- Hand-build bob's ability object on the stack: one mode, effect
            -- RestartGame, no targets. Object.owner = bob is the resolving
            -- controller (Resolve.hs), which restartGame uses as the starter.
            (abilId, g4) = Game.freshObjectId g3
            (ts, g5) = Game.freshTimestamp g4
            ability =
              ActivatedAbility.MkActivatedAbility
                { ActivatedAbility.cost =
                    Cost.Type.MkCost
                      { Cost.Type.mana = Just (ManaCost.MkManaCost []),
                        Cost.Type.components = []
                      },
                  ActivatedAbility.modal =
                    Modal.MkModal
                      (Seq.singleton (Mode.MkMode (Seq.singleton Effect.RestartGame) Map.empty))
                      (ModeSelection.ChooseExactly 1)
                }
            abilObj =
              Object.MkObject
                { Object.owner = S.bob,
                  Object.source = Source.OfAbility aliceId ability,
                  Object.zone = Zone.Stack,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled,
                  Object.bindings = Binding.fromChoices Map.empty Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
                  Object.counters = Map.empty,
                  Object.timestamp = ts
                }
            g6 = g5 {GameState.objects = Map.insert abilId abilObj (GameState.objects g5), GameState.stack = abilId : GameState.stack g5}
            after = snd (Engine.runGamePure S.identityAnswer g6 Stack.resolveTop)
         in do
              HU.assertEqual "the game restarted with bob as the starting player (CR 727.1a)" S.bob (GameState.activePlayer after)
              HU.assertEqual "alice's card survived the restart, still owned by alice (CR 727.2)" (Just S.alice) (Object.owner <$> Game.lookupObject aliceId after)
              HU.assertEqual "the resolving ability object ceased to exist (not a card)" Nothing (Game.lookupObject abilId after),
```

> `addMany`, `ModeSelection`, `ModeIndex`, `Mode`, `Modal` mirror the M5a `installControlBy` helper in this file — if any import is missing, add it aliased to its last component. `Binding.fromChoices` here supplies empty target/subtype maps, no X, and the single chosen mode (index 0), matching the M5a usage. If `addMany` is not visible from `SetupSpec`, define a local copy at the top of `ResolveSpec.hs` identical to Task 1's.

> **Plan-bug fix applied during execution:** the "alice's card survived" line above (`Game.lookupObject aliceId after`) does not hold as written. CR 400.7 mints a fresh object id on the opening-hand draw's zone change (`Event.changeZone`); `aliceId` is the first fresh id allotted (the smallest), so it sorts first in `libraryOf`'s ascending `Map.keys` order and is drawn — never the one card left undrawn in the post-restart library. `SetupSpec.hs`'s own CR 727.2 test (`restartTests`, "every owned card returns to its owner…") already documents and works around this with an ownership-count assertion instead of a specific-id lookup. `ResolveSpec.hs`'s implementation follows the same idiom: `HU.assertEqual "alice's 8 cards all survived the restart, still hers (CR 727.2)" 8 (length (filter (\o -> Object.owner o == S.alice) (Map.elems (GameState.objects after))))`, replacing the `Just S.alice` / `Game.lookupObject aliceId after` line shown above. Per CLAUDE.md ("a test failing against correct code is a plan bug: fix the plan's test, not the engine"), the engine's CR 400.7 re-identification is untouched; only this assertion changed.

- [x] **Step 2: Run the test to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20`
Expected: **compile error** — `Effect.RestartGame` is not a constructor of `Effect`.

- [x] **Step 3: Add the `RestartGame` constructor**

In `source/library/Pawl/Type/Effect.hs`, add a new constructor. Place it immediately after `ExileAllGraveyards` (line ~58) with a rules comment:

```haskell
  | -- CR 727.1/727.1a: restart the game. Targetless and game-wide (the
    -- ExileAllGraveyards / BecomeMonarch shape); the starting player of the new
    -- game is the resolving controller, so no target slot is needed.
    RestartGame
```

- [x] **Step 4: Add the six `Resolve` dispatch arms and the `Setup` import**

In `source/library/Pawl/Resolve.hs`:

First, add the import (in the import group, aliased to last component):

```haskell
import qualified Pawl.Setup as Setup
```

Then add one arm to each of the five classification tables (copy the `ExileAllGraveyards` value in each — `RestartGame` reads no slot, no X, no mana, no library search, and has no rewritable land-type word):

- in `slotsOf` (next to `Effect.ExileAllGraveyards -> Set.empty`): `Effect.RestartGame -> Set.empty`
- in `readsX`/`effectReadsX` (next to `Effect.ExileAllGraveyards -> False`): `Effect.RestartGame -> False`
- in `manaProduced` (next to `Effect.ExileAllGraveyards -> Nothing`): `Effect.RestartGame -> Nothing`
- in `searchesLibrary` (next to `Effect.ExileAllGraveyards -> False`): `Effect.RestartGame -> False`
- in `rewriteEffect` (next to `Effect.ExileAllGraveyards -> effect`): `Effect.RestartGame -> effect`

Then add the executor arm to `applyEffect` (next to the `Effect.ExileAllGraveyards -> do …` arm at ~507):

```haskell
  -- CR 727.1/727.1a: restart the game. The starting player of the new game is
  -- this ability's controller (CR 727.1a), which applyEffect already holds as
  -- `controller`; the rebuild lives in Setup (game construction). The engine
  -- reaches it through a generic opcode, never Karn's identity.
  Effect.RestartGame -> Setup.restartGame controller
```

- [x] **Step 5: Add the two `Codec` arms**

In `source/library/Pawl/Codec.hs`:

- in `effectToJson`, next to `Effect.ExileAllGraveyards -> nullary (Text.pack "ExileAllGraveyards")`:

```haskell
  Effect.RestartGame -> nullary (Text.pack "RestartGame")
```

- in `jsonToEffect`, next to `"ExileAllGraveyards" -> Right Effect.ExileAllGraveyards`:

```haskell
    "RestartGame" -> Right Effect.RestartGame
```

- [x] **Step 6: Run the test to verify it passes**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "$0~/resolving a RestartGame ability/"'`
Expected: **PASS** — warning-clean build (all `case effect of` sites now exhaustive), the resolution test green.

If the build reports an **import cycle** between `Pawl.Resolve` and `Pawl.Setup`, STOP — this plan verified there is none (Setup and its deps do not import Resolve). A cycle means an unrelated import was added; do not paper over it by moving `restartGame`.

- [x] **Step 7: Commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "feat(m5b): RestartGame opcode resolves to Setup.restartGame (CR 727.1a)"
```

---

### Task 4: the `synthetic-restart` gate card

A labeled-synthetic artifact (the `Landform` crutch pattern) with an activated ability whose only effect is `RestartGame`. Its documented expiry names **Karn Liberated** (Task 6 files the issue). Ability cost is `{0}` + tap + sacrifice (`"mana": []`, payable — an absent/`null` mana would be *unpayable* per CR 118.6), so the gate test can activate it with no lands.

**Files:**
- Create: `data/cards/synthetic-restart.json`.
- Modify: `source/test-suite/Pawl/Cards.hs` — register at four sites.

**Interfaces:**
- Produces: `Cards.syntheticRestartPrinting :: Cards.Cards -> Printing.Printing`.

- [x] **Step 1: Create the card file**

Write `data/cards/synthetic-restart.json`:

```json
{
  "activatedAbilities": [
    {
      "cost": {
        "components": [
          { "type": "TapThis" },
          { "type": "SacrificeThis" }
        ],
        "mana": []
      },
      "modal": {
        "modes": [
          {
            "effects": [ { "type": "RestartGame" } ],
            "targetSpecs": []
          }
        ],
        "selection": { "type": "ChooseExactly", "value": 1 }
      }
    }
  ],
  "castingPermissions": [],
  "keywords": [],
  "manaCost": [ { "type": "Generic", "value": 2 } ],
  "name": "Restart",
  "power": null,
  "replacementEffects": [],
  "spell": {
    "modes": [ { "effects": [], "targetSpecs": [] } ],
    "selection": { "type": "ChooseExactly", "value": 1 }
  },
  "staticAbilities": [],
  "toughness": null,
  "triggeredAbilities": [],
  "typeLine": {
    "subtypes": [],
    "supertypes": [],
    "types": [ { "type": "Artifact" } ]
  }
}
```

- [x] **Step 2: Register the printing at all four `Cards.hs` sites**

In `source/test-suite/Pawl/Cards.hs`:

1. Add the record field (in `data Cards`, after `mindslaverPrinting :: Printing.Printing,`):

```haskell
    syntheticRestartPrinting :: Printing.Printing,
```

2. Add the load line in `loadCards` (after `mindslaverPrinting_ <- loadPrinting "mindslaver"`):

```haskell
  syntheticRestartPrinting_ <- loadPrinting "synthetic-restart"
```

3. Add the record construction line (after `mindslaverPrinting = mindslaverPrinting_,`):

```haskell
        syntheticRestartPrinting = syntheticRestartPrinting_,
```

4. Add it to `allPrintings` (after `mindslaverPrinting cards,`):

```haskell
    syntheticRestartPrinting cards,
```

- [x] **Step 3: Run the round-trip / honesty tests to verify the card loads and encodes**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "$0~/round-trip/ || $0~/allPrintings/ || $0~/honest/"'`
Expected: **PASS** — the new printing parses, appears in `allPrintings`, and JSON round-trips (this is the codec arms' TDD gate). If any round-trip case fails, the `RestartGame` codec arm (Task 3 Step 5) or the card JSON is wrong — fix it, do not skip the card.

> If unsure which suite owns the round-trip, run the full suite: `cabal test`. It must stay green.

- [x] **Step 4: Commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "feat(m5b): synthetic-restart gate card (labeled synthetic, expires -> Karn Liberated)"
```

---

### Task 5: gameplay-level gate — activating the synthetic restart rebuilds the game

The headline gate (CR 727.1/727.2/727.4). bob controls the `synthetic-restart` artifact; bob activates it through the **real priority loop**; it resolves, restarts the game, and the resulting state is a valid new game with bob as starting player, both players at 20 life with 7-card hands, every card back in its owner's library or hand, and the state settled just before the first untap step. This is strictly more end-to-end than Task 3's `resolveTop` test: it goes through activation, the stack, and priority.

**Files:**
- Modify: `source/test-suite/Pawl/GameSpec.hs` — one new answerer `restartAnswer`, one test case in `ruleTests`.

**Interfaces:**
- Consumes: `Engine.{runGamePure,priorityLoop}`, `Setup.emptyGame`, `S.addCreature`, `Cards.{syntheticRestartPrinting,pikerPrinting,mountainPrinting}`, `isActivateAction` (already in `GameSpec`), `S.identityAnswer`, `Turn.firstPhase`.
- Produces: `restartAnswer :: Prompt.Prompt r -> r`.

- [x] **Step 1: Add the activation answerer**

Append to `source/test-suite/Pawl/GameSpec.hs` (next to the other answerers, e.g. after `gateAnswer`). Add `import qualified Data.List as List`, `import qualified Data.Set as Set`, `import qualified Data.Maybe as Maybe`, `import qualified Pawl.Turn as Turn`, `import qualified Pawl.Type.Object as Object` to the import group if not present:

```haskell
-- CR 727 gate strategy. Whoever has priority activates the only activation on the
-- board -- the synthetic restart artifact (bob controls it) -- and otherwise
-- passes. Once the artifact is sacrificed as a cost there is no further
-- activation, so this fires exactly once; after the restart the artifact is in a
-- library, so no player can activate anything and everyone passes to termination.
-- Non-ChooseAction prompts (Shuffle during the rebuild, etc.) delegate to
-- identityAnswer.
restartAnswer :: Prompt.Prompt r -> r
restartAnswer p = case p of
  Prompt.ChooseAction _ _ actions ->
    case filter isActivateAction actions of
      activation : _ -> activation
      [] -> A.Pass
  _ -> S.identityAnswer p
```

> A local `addManyG` helper builds the card pools (mirror of Task 1's `addMany`); add it near the answerer:
>
> ```haskell
> addManyG :: Cards.Cards -> Int -> S.PlayerId -> GameState.GameState -> GameState.GameState
> addManyG cards n pid gs =
>   List.foldl' (\g _ -> snd (S.addCreature (Cards.mountainPrinting cards) pid g)) gs (replicate n ())
> ```
>
> (If `S.PlayerId` is not exported by `Support`, import `Pawl.Type.PlayerId (PlayerId)` and use `PlayerId`.)

- [x] **Step 2: Add the gate test case to `ruleTests`**

Insert into the `ruleTests` list in `source/test-suite/Pawl/GameSpec.hs` (comma-separated):

```haskell
      HU.testCase "CR 727.1/727.2/727.4 gameplay: bob activates a restart and the game rebuilds from its own cards" $
        -- bob controls the synthetic restart artifact and owns 8 cards total;
        -- alice owns 8. Both start with reduced life on a populated board. bob
        -- activates the artifact through the priority loop; it resolves, restarts
        -- the game, and the result is a valid new game with bob as starter.
        let g0 = Setup.emptyGame S.bothPlayers
            (_restartId, g1) = S.addCreature (Cards.syntheticRestartPrinting cards) S.bob g0
            (_aPiker, g2) = S.addCreature (Cards.pikerPrinting cards) S.alice g1
            -- fill each owner's pool to >= 7 cards so opening hands draw without a
            -- CR 727.3 loss (the restart artifact + 7 mountains = 8 for bob).
            g3 = addManyG cards 7 S.bob (addManyG cards 7 S.alice g2)
            gStart =
              g3
                { GameState.activePlayer = S.bob,
                  GameState.phase = Phase.PrecombatMain,
                  GameState.priority = Just S.bob
                }
            after = snd (Engine.runGamePure restartAnswer gStart Engine.priorityLoop)
         in do
              HU.assertEqual "CR 727.1a: bob is the new active player" S.bob (GameState.activePlayer after)
              HU.assertEqual "CR 727.1a: the turn order begins with bob" (Just S.bob) (Maybe.listToMaybe (GameState.turnOrder after))
              HU.assertEqual "both players reset to 20 life (alice)" (Just 20) (S.lifeOf S.alice after)
              HU.assertEqual "both players reset to 20 life (bob)" (Just 20) (S.lifeOf S.bob after)
              HU.assertEqual "CR 103.5: alice drew a 7-card opening hand" 7 (S.handSize S.alice after)
              HU.assertEqual "CR 103.5: bob drew a 7-card opening hand" 7 (S.handSize S.bob after)
              HU.assertEqual "CR 727.4: settled at the first untap step" Turn.firstPhase (GameState.phase after)
              HU.assertEqual "CR 727.2: the battlefield is empty (every card returned to a library)" True (Set.null (GameState.battlefield after))
              HU.assertEqual "the game did not end -- the new game is live" Nothing (GameState.result after)
```

> `Phase.PrecombatMain` and the `Phase` import are already used by the M5a gate in this file. If `Set`/`Maybe`/`Turn`/`List` were not previously imported, they were added in Step 1.

- [x] **Step 3: Run the test to verify it fails first, then passes**

Because the machinery is now built, this characterization gate is expected to **pass on first run**. To honor TDD for a gate over already-built code, first prove it has teeth:

Run the gate: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "$0~/bob activates a restart/"'`
Expected: **PASS.**

Then a **falsification check** — temporarily break `restartGame`'s starting-player rule in `source/library/Pawl/Setup.hs` by ignoring `starter`:

```haskell
            GameState.activePlayer = NonEmpty.head (NonEmpty.fromList (GameState.turnOrder gs)),
```

(i.e. keep the old active player instead of `starter`). Re-run the gate: expected **FAIL** on the "bob is the new active player" assertion. **Revert** `Setup.hs` exactly and confirm `git diff source/library/Pawl/Setup.hs` is empty, then re-run: expected **PASS**. This proves the gate depends on CR 727.1a.

> Do not leave the mutation in. If the gate does *not* fail under the mutation, the test is not exercising the starting-player rule — investigate before proceeding.

- [x] **Step 4: Commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "test(m5b): gameplay gate — activating a restart rebuilds the game (CR 727.1/727.2/727.4)"
```

---

### Task 6: file the deferrals and cite them inline

Per `CLAUDE.md` ("file the issue, cite it inline"), each thing M5b elides gets a GitHub issue carrying status, rationale, and expiry trigger, with a code-site comment stating only what is *not* implemented plus `(#N)`. Two deferrals have M5b code sites: (a) **live `playGame` re-entry** after an in-play restart (the rebuild-only decision), and (b) **full Karn Liberated** (CR 727.5/727.5a exemption + put-onto-battlefield rider), which retires the synthetic gate.

**Files:**
- Create: two GitHub issues in `tfausak/pawl`.
- Modify: `source/library/Pawl/Resolve.hs` — one comment at the `RestartGame` arm citing both issues.

**Interfaces:** none (comments + issues only).

- [x] **Step 1: Check for duplicates, then file the live-loop-reentry issue**

```bash
gh issue list --repo tfausak/pawl --search "restart"
```

If nothing covers it, file:

```bash
gh issue create \
  --repo tfausak/pawl \
  --title "Live playGame re-entry after an in-game restart (CR 727.4): abandon the current turn, resume into the new turn 1" \
  --label gap --label rules-correctness \
  --body "Status: deferred; not implemented in M5b.

M5b built the CR 727 rebuild primitive (Setup.restartGame / startGameFromCards) and a nullary RestartGame opcode, proven by the synthetic-restart gate at gameplay level. What is NOT wired: a RestartGame that resolves DURING a live Engine.playGame does not cleanly abandon the rest of the current turn and resume into the rebuilt turn 1. restartGame rebuilds the GameState in place, but the surrounding runStep/priorityLoop that invoked the resolution keep executing on the fresh state (e.g. advance would pop the fresh 'remaining' and skip the untap step's turn-based actions). The gate resolves the ability and asserts the rebuilt state; it does not drive a multi-turn continuation.

To finish (CR 727.4 'finishes resolving just before the first turn's untap step; no player has priority'): a transient restart signal on GameState, with bail-out guards in Engine.priorityLoop (return without granting further priority) and Engine.runStep (skip advance/SBA and let the next playGame iteration run the fresh untap step). This is the control-flow half of restart-in-place; it pairs naturally with full Karn Liberated integration.

Expiry trigger: milestone/card-driven (fires with full Karn Liberated, or whenever a played game must survive a restart). Cited at the RestartGame arm in Pawl.Resolve."
```

Record the number `N1`.

- [x] **Step 2: File the full-Karn-Liberated issue**

```bash
gh issue create \
  --repo tfausak/pawl \
  --title "Full Karn Liberated (CR 727.5/727.5a): the restart exemption + put-onto-battlefield rider retires the synthetic-restart gate" \
  --label gap --label rules-correctness --label expires:card-driven \
  --body "Status: deferred; M5b's gate is a labeled synthetic ('Restart', data/cards/synthetic-restart.json), the Landform crutch pattern.

Karn Liberated is the only real CR 727 card, and it intrinsically bundles machinery M5b does not build: CR 727.5 -- an effect may EXEMPT designated cards from the restart, leaving them in exile across the new game (they are not in their owner's deck as the new game begins); and Karn's rider, put the exempted cards onto the battlefield under its controller when the restart effect finishes resolving (CR 727.4). startGameFromCards currently funnels EVERY owned card into a library; the exemption needs a set of exempted object ids that skip the rebuild and stay in exile, plus the put-onto-battlefield instruction as ordinary follow-on vocabulary.

When built, this retires the synthetic-restart gate card and its accessor (Cards.syntheticRestartPrinting) in favor of a real Karn Liberated card + test.

Also parked here for M5c: CR 727.6 (restarting a SUBGAME leaves the main game unaffected; main-game references to the subgame winner/loser now refer to the restarted subgame) rides the subgame machinery M5c builds. And CR 727.2's 'cards brought in from outside the game' (Living Wish) needs a sideboard/outside-the-game subsystem -- subsystem-blocked, not scheduled.

Expiry trigger: card-driven (Karn Liberated). Cited at the RestartGame arm in Pawl.Resolve."
```

Record the number `N2`.

- [x] **Step 3: Add the inline citation at the `RestartGame` arm**

In `source/library/Pawl/Resolve.hs`, extend the `RestartGame` arm's comment (from Task 3 Step 4) to cite both issues (replace `#N1`/`#N2` with the real numbers):

```haskell
  -- CR 727.1/727.1a: restart the game. The starting player of the new game is
  -- this ability's controller (CR 727.1a), which applyEffect already holds as
  -- `controller`; the rebuild lives in Setup (game construction). The engine
  -- reaches it through a generic opcode, never Karn's identity.
  -- Not implemented: live playGame re-entry after an in-game restart (#N1);
  -- the CR 727.5/727.5a exemption + put-onto-battlefield rider of full Karn
  -- Liberated (#N2), which retires the synthetic-restart gate.
  Effect.RestartGame -> Setup.restartGame controller
```

- [x] **Step 4: Build (comment-only) and commit**

```bash
cabal build all --enable-tests --enable-benchmarks
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "docs(m5b): defer live restart re-entry (#N1) and full Karn (#N2), cite at the opcode"
```

---

### Task 7: close-out — verify, record, and tick M5b

Land the phase per `docs/workflow.md`: definitive warning-clean build, full test suite, invariant/rules audit, then the three docs updates.

**Files:**
- Modify: `docs/progress.md` (append the M5b entry), `CLAUDE.md` (replace the status bullet), `docs/superpowers/specs/2026-07-23-m5-player-control-restart-subgames-design.md` (tick M5b in the §3 table).

- [x] **Step 1: Definitive warning-clean build**

```bash
cabal clean
cabal build all --enable-tests --enable-benchmarks
```
Expected: **no warnings, no errors** (a clean build defeats incremental warning-hiding — the `RestartGame` arms in unchanged-looking modules are re-checked).

- [x] **Step 2: Full test suite green**

```bash
cabal test
```
Expected: **all pass**, including the Task 1–5 additions. Do not proceed if anything is red.

- [x] **Step 3: Invariant + rules audit**

Confirm by inspection (note anything material in the commit body):
- **Invariant 1:** `Pawl.Resolve` still cases only on the `RestartGame` *classification*; there is no `case … Karn` or card-identity match. `restartGame`/`startGameFromCards` read `Source.OfCard` (a classification), never a card name.
- **Invariant 2:** no choice was elided — the starting player is fixed by CR 727.1a (the controller), not prompted; shuffles remain `Prompt.Shuffle`. The two deferrals carry expiry-tagged issues (Task 6) cited inline.
- **Rules re-check** against `docs/rules.txt`: CR 727.1 (6248), 727.1a (6250), 727.2 (6252), 727.3 (6255), 727.4 (6257), 727.5/727.5a (6259/6261), 727.6 (6263); CR 103.5 opening hands (296). Delegate this citation pass to a cheap model if using subagents (Haiku, per `docs/workflow.md`).

- [x] **Step 4: Append the M5b completion entry to `docs/progress.md`**

Add after the M5a entry (matching the surrounding bullet style):

```markdown
- **M5b is complete** (Restarting the Game — CR 727 — the second M5 phase;
  the lower-risk of the two game-lifecycle phases, and the one that introduces
  the primitive M5c reuses). **Gate: a labeled-synthetic "Restart" artifact**
  (the `Landform` crutch pattern; documented expiry → **Karn Liberated**, #N2)
  whose activated ability's only effect is the new nullary `Effect.RestartGame`
  opcode. At gameplay level, bob activates it through the real priority loop; it
  resolves to `Setup.restartGame controller` and rebuilds the single `GameState`
  slot. The decision it proves: **restart is replace-in-place, not fresh setup**
  — `startGameFromCards` rebuilds every player's library from the *actual object
  pool* (each owner's `Source.OfCard` objects, wherever they sat; non-cards
  cease), ownership preserved (CR 727.2), and the starting player is the
  *restart's controller* (CR 727.1a), rotated to the head of the turn order (CR
  103) — both of which `Setup.emptyGame` + `Setup.newGame` would get wrong. The
  edges: **727.4** — the effect settles just before the first untap step (phase
  `Beginning Untap`, no priority, turn 1); **727.3** — a player owning fewer than
  seven cards draws from an empty library and loses at the next SBA check, reusing
  the existing draw-from-empty path. **Added:** `Setup.startGameFromCards`
  (reused verbatim by M5c) and `Setup.restartGame`/`rotateTo`; one nullary
  `Effect.RestartGame` opcode (six `Resolve` dispatch arms + two `Codec` arms +
  the `applyEffect` executor calling `Setup.restartGame`); the `synthetic-restart`
  card. **Deferred:** live `playGame` re-entry after an in-game restart (#N1);
  full Karn Liberated's CR 727.5/727.5a exemption + put-onto-battlefield rider
  (#N2, card-driven), which retires the synthetic gate; CR 727.6 subgame-restart
  (rides M5c) and CR 727.2's outside-the-game cards (subsystem-blocked), noted in
  #N2.
```

Replace `#N1`/`#N2` with the Task 6 issue numbers.

- [x] **Step 5: Replace the `CLAUDE.md` status bullet**

In `CLAUDE.md`, the "Current work and tracking" first bullet currently ends with the M5a-landed / "M5b … is the next phase to plan" framing. **Replace** the tail (do not append a second status bullet) so it reads that M5b has landed and M5c is next — keeping the M0–M5a history summary intact:

> ... **M5b (Restarting the Game, CR 727) has landed** — a labeled-synthetic
> "Restart" artifact drives a gameplay-level gate over a new nullary
> `Effect.RestartGame` opcode that resolves to `Setup.restartGame`, rebuilding
> the game in place from its actual cards (CR 727.2) with the restart's
> controller as starting player (CR 727.1a), settling just before the first
> untap step (CR 727.4); it introduces the shared `startGameFromCards` primitive
> that M5c reuses. Deferred: live `playGame` re-entry after an in-game restart,
> and full Karn Liberated (CR 727.5/727.5a). **M5c (Subgames, CR 729 — the M5
> go/no-go) is the next phase to plan.** The umbrella spec is
> `docs/superpowers/specs/2026-07-23-m5-player-control-restart-subgames-design.md`.

Keep the existing pointers to `docs/progress.md` and the umbrella spec intact.

- [x] **Step 6: Tick M5b in the umbrella spec**

In `docs/superpowers/specs/2026-07-23-m5-player-control-restart-subgames-design.md`, mark the **M5b** row of the §3 phase table as landed: change `| **M5b** |` to `| **M5b ✅** |` (per §6: record the completion in `progress.md` and tick the phase here).

- [ ] **Step 7: Commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git add -A
git commit -m "docs(m5b): completion entry, CLAUDE.md status, umbrella tick"
```

- [ ] **Step 8: Confirm the plan is fully executed**

```bash
grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-23-m5b-restarting-the-game.md
```
Expected: **`0`** (every step checked off). Confirm `git log --oneline -7` shows the seven M5b commits on `main`.

---

## Self-review notes (for the executor)

- **Spec coverage (umbrella §3 M5b row + §3 notes).** `startGameFromCards` primitive → Task 1; `restartGame` routine + CR 727.1a starting player + CR 727.2 ownership/actual-cards + CR 727.4 timing + CR 727.3 short-deck loss → Task 2; the generic `RestartGame` opcode (routine keyed off a generic effect, not Karn's identity) → Task 3; the labeled-synthetic gate card with expiry → Task 4; the gameplay-level gate → Task 5; deferrals (CR 727.5/727.5a full Karn; outside-the-game Living Wish; CR 727.6 subgame-restart; live-loop re-entry) → Task 6; exit criterion/close-out → Task 7.
- **Rebuild-only scope is intentional.** Live `playGame` resumption after an in-game restart is deferred (#N1) by explicit decision; the gate proves the milestone falsifier (rebuild ≠ fresh setup) without it. If a task seems to require touching `Engine.priorityLoop`/`runStep`, STOP — that is out of M5b's scope.
- **These gate tests over correct machinery pass on first run.** Task 5's falsification check proves the gate's teeth via a *reverted* mutation; confirm `git diff` on library files is empty afterward. Never weaken an assertion or delete a test to make a check pass (CLAUDE.md).
- **Type consistency.** `startGameFromCards :: Game ()`, `restartGame :: PlayerId -> Game ()`, `rotateTo :: PlayerId -> [PlayerId] -> [PlayerId]`, `Effect.RestartGame :: Effect card`, `Cards.syntheticRestartPrinting :: Cards -> Printing`, the answerers `restartAnswer :: Prompt.Prompt r -> r` — used consistently across tasks. `addMany`/`addManyG` are structurally identical helpers (SetupSpec/ResolveSpec vs GameSpec); the difference is only which file they live in.
- **No new opcode is missed.** Adding `Effect.RestartGame` makes six `Resolve` `case`s and both `Codec` `case`s non-exhaustive; `-Wincomplete-patterns` under `-Werror` fails the build until each arm exists (Task 3). The `allPrintings` round-trip (Task 4) is the codec arms' behavioral gate.
