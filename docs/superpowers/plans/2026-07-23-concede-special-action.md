# Concede — the special action (CR 104.3a) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A player may concede at any priority they hold; a Mindslaver-controlled player may concede themselves while their controller cannot concede for them (CR 723.6).

**Architecture:** A new `Prompt.Concede :: PlayerId -> Prompt Concession` that deliberately carries **no `Decider`**, polled in `Engine.priorityLoop` immediately before the existing `ChooseAction`. The departure machinery moves out of `Pawl.Sba` into a new `Pawl.Departure`, because CR 104.3a is immediate while CR 104.3b is a state-based action.

**Tech Stack:** Haskell 2010, GHC 9.14.1, `tasty` + `tasty-hunit`. No new dependencies.

**Spec:** `docs/superpowers/specs/2026-07-23-concede-special-action-design.md`

## Global Constraints

- Haskell 2010. **No language extensions** beyond those a file already has. `GADTs`, `RankNTypes`, `NamedFieldPuns` only where already present.
- Build must be **warning-clean**: `cabal build all --enable-tests --enable-benchmarks`. The `pedantic` flag makes every warning an error.
- **No partial functions.** No `head`, `undefined`, `error`, or non-exhaustive matches.
- **No explicit export lists.** `module Pawl.Foo where`.
- **Qualified imports aliased to the last component.** When two modules share a last component, the logic module keeps the bare name and the type module gets `.Type` — e.g. `Pawl.Departure` as `Departure`, `Pawl.Type.Departure` as `Departure.Type`. This mirrors `Expiry` / `Expiry.Type` in `Pawl.ExpirySpec`.
- **One type per module** under `Pawl.Type.<TypeName>`; logic elsewhere.
- **No boolean blindness** — a sum type, never a `Bool`.
- **Derive at least `Eq` and `Show`.**
- Constructors are **non-punning** with a `Mk` prefix only for single-constructor types. `Concession = Concedes | Continues` is a multi-constructor sum and takes no prefix, exactly like `Departure = Lost | Conceded | Drew`.
- **Every rules claim checked against `docs/rules.txt`**, with the CR number in the comment. Never recall a rule.
- After every task: `hooky fix`, then `git add`, then `hooky run` must pass. `hooky` acts on **staged** files only.
- `cabal-gild` regenerates `other-modules` / `exposed-modules` from the `-- cabal-gild: discover` directives. Adding a module requires running it (via `hooky fix`, or directly as `cabal-gild --io pawl.cabal` if `hooky` skips).

**Rule texts you will need (already verified against `docs/rules.txt` — do not re-derive, but do cite):**

- **CR 104.3a** — "A player can concede the game at any time. A player who concedes leaves the game immediately. That player loses the game."
- **CR 104.3b** — "If a player's life total is 0 or less, that player loses the game the next time a player would receive priority. (This is a state-based action…)"
- **CR 104.2a** — "A player still in the game wins the game if that player's opponents have all left the game. This happens immediately and overrides all effects that would preclude that player from winning the game."
- **CR 405.6g** — "A player may concede the game at any time. That player leaves the game immediately. See rule 104.3a."
- **CR 723.6** — the controller of a controlled player cannot make them concede; the controlled player may still concede themselves.

---

## File Structure

**Created:**

- `source/library/Pawl/Departure.hs` — who is still in the game, and what happens when someone leaves. Owns `stillPlaying`, `depart`, `outcomeAfterLeaving`, `leaveGame`.
- `source/library/Pawl/Type/Concession.hs` — the `Concession` answer type.
- `source/test-suite/Pawl/DepartureSpec.hs` — covers `Pawl.Departure`.

**Modified:**

- `source/library/Pawl/Sba.hs` — loses `stillPlaying` and `depart`; delegates to `Pawl.Departure`.
- `source/library/Pawl/Target.hs`, `source/library/Pawl/Combat.hs`, `source/library/Pawl/Engine.hs` — re-point 5 `Sba.stillPlaying` call sites.
- `source/library/Pawl/Type/Prompt.hs` — the `Concede` constructor.
- `source/library/Pawl/Type/Response.hs` — the `Conceded` arm.
- `source/library/Pawl/Replay.hs` — `encode`, `decode`, `defaultAnswer` arms.
- `source/library/Pawl/Engine.hs` — the poll, and deletion of the stale CR 723.6 guard comment.
- `source/test-suite/Pawl/Support.hs`, `CastSpec.hs`, `GameSpec.hs`, `ReplaySpec.hs`, `source/benchmark/Main.hs` — a `Concede` arm in each exhaustive answer function.
- `source/test-suite/Main.hs` — wire `DepartureSpec`.

---

## Task 1: Extract `Pawl.Departure`

Pure refactor plus one genuinely new function (`leaveGame`). No existing behaviour changes.

**Files:**

- Create: `source/library/Pawl/Departure.hs`
- Create: `source/test-suite/Pawl/DepartureSpec.hs`
- Modify: `source/library/Pawl/Sba.hs`
- Modify: `source/library/Pawl/Target.hs:80,84`
- Modify: `source/library/Pawl/Combat.hs:62`
- Modify: `source/library/Pawl/Engine.hs:91,424`
- Modify: `source/test-suite/Main.hs`

**Interfaces:**

- Consumes: nothing from earlier tasks.
- Produces: `Departure.stillPlaying :: GameState -> [PlayerId]`, `Departure.depart :: Departure -> PlayerId -> GameState -> GameState`, `Departure.outcomeAfterLeaving :: [PlayerId] -> GameState -> Maybe Result`, `Departure.leaveGame :: Departure -> PlayerId -> Game ()`. Task 3 calls `leaveGame`.

- [ ] **Step 1: Write the failing test**

Create `source/test-suite/Pawl/DepartureSpec.hs`:

```haskell
-- Covers Pawl.Departure: who is still in the game, and the CR 104.2a/104.3
-- consequences of leaving it.
module Pawl.DepartureSpec where

import qualified Pawl.Departure as Departure
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.Departure as Departure.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Status as Status
import qualified Data.Map.Strict as Map
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

statusOf :: S.PlayerIdAlias -> GameState.GameState -> Maybe Status.Status
statusOf pid gs = fmap Player.status (Map.lookup pid (GameState.players gs))

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Departure"
    [ HU.testCase "CR 104.3a a conceding player leaves immediately, with Conceded as the reason" $
        let gs = Setup.emptyGame S.bothPlayers
            after = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.alice)
         in HU.assertEqual "alice departed by conceding" (Just (Status.Departed Departure.Type.Conceded)) (statusOf S.alice after),
      HU.testCase "CR 104.2a the last player standing wins, without waiting for a state-based action check" $
        -- leaveGame settles the outcome itself. Nothing runs an SBA pass here.
        let gs = Setup.emptyGame S.bothPlayers
            after = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.alice)
         in HU.assertEqual "bob wins on the spot" (Just (Result.Won S.bob)) (GameState.result after),
      HU.testCase "an already-decided result is not overwritten" $
        let gs = (Setup.emptyGame S.bothPlayers) {GameState.result = Just Result.Drawn}
            after = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.alice)
         in HU.assertEqual "the first result stands" (Just Result.Drawn) (GameState.result after),
      HU.testCase "stillPlaying omits a departed player" $
        let gs = Setup.emptyGame S.bothPlayers
            after = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.alice)
         in HU.assertEqual "only bob remains" [S.bob] (Departure.stillPlaying after)
    ]
```

`S.PlayerIdAlias` is a placeholder for the real type — use `PlayerId.PlayerId`, importing `qualified Pawl.Type.PlayerId as PlayerId`. Replace the two occurrences accordingly.

Wire it into `source/test-suite/Main.hs`: add `import qualified Pawl.DepartureSpec` alongside the other spec imports, and add `Pawl.DepartureSpec.tests` to the `testTree` list. Match the exact style of the neighbouring entries — read the file first.

- [ ] **Step 2: Run it and watch it fail**

```bash
cabal build pawl-test-suite --enable-tests
```

Expected: FAIL — `Could not find module 'Pawl.Departure'`. That is the correct failure; the module does not exist yet.

- [ ] **Step 3: Create `Pawl.Departure`**

Create `source/library/Pawl/Departure.hs`:

```haskell
module Pawl.Departure where

import Control.Applicative ((<|>))
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import Pawl.Type.Departure (Departure)
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Player as Player
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.Result (Result)
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Status as Status

-- CR 104.2a / 104.3: who is still in the game, and what happens when someone
-- leaves it.
--
-- Split out of Pawl.Sba because leaving is not always a state-based action. CR
-- 104.3b (life <= 0) is one and arrives through the SBA pass; CR 104.3a (concede)
-- is IMMEDIATE and Pawl.Engine reaches it directly. Having Engine call into
-- Pawl.Sba for something the rules say is not a state-based action would misstate
-- the rules in the module graph.
stillPlaying :: GameState -> [PlayerId]
stillPlaying gs =
  let isPlaying entry = Player.status (snd entry) == Status.Playing
   in map fst (filter isPlaying (Map.toList (GameState.players gs)))

-- Mark a player as having left, with the reason they left. Pure, because the SBA
-- pass folds it over several players before recomputing the outcome once.
depart :: Departure -> PlayerId -> GameState -> GameState
depart reason pid gs =
  let lose p = p {Player.status = Status.Departed reason}
   in gs {GameState.players = Map.adjust lose pid (GameState.players gs)}

-- CR 104.2a: "A player still in the game wins the game if that player's opponents
-- have all left the game."
--
-- `gs` is the state AFTER the departures have been applied, so the survivors are
-- `stillPlaying gs`. `leaving` is who just left, and is needed only to tell
-- "nobody is playing because they all left at once" (a draw) from "nobody was
-- playing to begin with" (no result at all).
outcomeAfterLeaving :: [PlayerId] -> GameState -> Maybe Result
outcomeAfterLeaving leaving gs = case stillPlaying gs of
  [winner] -> Just (Result.Won winner)
  [] -> if null leaving then Nothing else Just Result.Drawn
  _ -> Nothing

-- CR 104.3a: leave the game IMMEDIATELY, and settle CR 104.2a right now rather
-- than at the next state-based action check -- which is the whole distinction
-- between 104.3a and 104.3b. An already-decided result wins, matching the
-- `outcome <|> existing` precedence Pawl.Sba's pass uses.
leaveGame :: Departure -> PlayerId -> Game ()
leaveGame reason pid = State.modify' $ \gs ->
  let departed = depart reason pid gs
   in departed {GameState.result = outcomeAfterLeaving [pid] departed <|> GameState.result departed}
```

- [ ] **Step 4: Run the new tests**

```bash
cabal test 2>&1 | grep -E "Departure|^All [0-9]+|FAIL"
```

Expected: the four `Departure` cases PASS. The rest of the suite may not build yet — that is Step 5.

- [ ] **Step 5: Delete the old definitions from `Pawl.Sba` and delegate**

In `source/library/Pawl/Sba.hs`:

1. Delete `stillPlaying` (lines 35–38) and `depart` (lines 53–55) entirely.
2. Change the import `import qualified Pawl.Type.Departure as Departure` to **two** imports:

```haskell
import qualified Pawl.Departure as Departure
import qualified Pawl.Type.Departure as Departure.Type
```

3. In `performStateBasedActions`, rewrite these three lines:

```haskell
  let leaving = filter (losesNow destroyed) (stillPlaying destroyed)
      departed = foldr depart destroyed leaving
      remaining = stillPlaying departed
```

to:

```haskell
  let leaving = filter (losesNow destroyed) (Departure.stillPlaying destroyed)
      departed = foldr (Departure.depart Departure.Type.Lost) destroyed leaving
```

Note `remaining` is deleted — `outcomeAfterLeaving` recomputes it.

4. Replace the inline `outcome` binding:

```haskell
      outcome = case remaining of
        [winner] -> Just (Result.Won winner)
        [] -> if null leaving then Nothing else Just Result.Drawn
        _ -> Nothing
```

with:

```haskell
      outcome = Departure.outcomeAfterLeaving leaving departed
```

5. Remove any import that is now unused. `-Werror=unused-imports` will name them; expect `Pawl.Type.Result` and possibly `Pawl.Type.Status` to survive (`losesNow` still uses `Status.Playing`) — let the compiler decide, do not guess.

- [ ] **Step 6: Re-point the five `Sba.stillPlaying` call sites**

Each file gains `import qualified Pawl.Departure as Departure` (none of the three currently imports anything named `Departure`, so there is no collision) and changes `Sba.stillPlaying` to `Departure.stillPlaying`:

- `source/library/Pawl/Target.hs:80` and `:84`
- `source/library/Pawl/Combat.hs:62`
- `source/library/Pawl/Engine.hs:91` and `:424`

If removing the last `Sba.` use from a file makes its `Pawl.Sba` import unused, delete that import — the compiler will say so.

- [ ] **Step 7: Build clean and run the whole suite**

```bash
cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -iE "error|warning"
cabal test 2>&1 | grep -E "^All [0-9]+|FAIL"
```

Expected: no warnings, and **935 tests pass** (931 before, plus the 4 new `Departure` cases). If any pre-existing test fails, the extraction changed behaviour — stop and diagnose rather than editing the test.

- [ ] **Step 8: Commit**

```bash
git add -A source/ pawl.cabal
hooky fix && git add -A source/ pawl.cabal && hooky run
git commit -m "refactor(departure): extract Pawl.Departure from Pawl.Sba (#133)

Leaving the game is not always a state-based action: CR 104.3b (life <= 0) is
one, but CR 104.3a (concede) is immediate. Keeping the departure machinery in
Pawl.Sba would force Pawl.Engine to call into the SBA module for something the
rules say is not an SBA.

depart gains its reason, which is why Departure.Conceded has never been
constructed. outcomeAfterLeaving lifts Sba's inline CR 104.2a outcome verbatim,
including the everyone-left-at-once draw arm. leaveGame is new: the immediate
door that settles the outcome on the spot rather than at the next SBA pass.

No behaviour change; five stillPlaying call sites re-point."
```

---

## Task 2: The `Concession` channel

Types and interpreters only. Nothing polls the new prompt yet, so there is still no behaviour change.

**Files:**

- Create: `source/library/Pawl/Type/Concession.hs`
- Modify: `source/library/Pawl/Type/Prompt.hs`
- Modify: `source/library/Pawl/Type/Response.hs`
- Modify: `source/library/Pawl/Replay.hs`
- Modify: `source/test-suite/Pawl/ReplaySpec.hs`
- Modify: `source/test-suite/Pawl/Support.hs`, `source/test-suite/Pawl/CastSpec.hs`, `source/test-suite/Pawl/GameSpec.hs`, `source/benchmark/Main.hs`

**Interfaces:**

- Consumes: nothing from Task 1.
- Produces: `Concession.Concession = Concedes | Continues`; `Prompt.Concede :: PlayerId -> Prompt Concession`; `Response.Conceded Concession`. Task 3 prompts with `Prompt.Concede`.

- [ ] **Step 1: Write the failing test**

In `source/test-suite/Pawl/ReplaySpec.hs`, add to the existing round-trip group (match the file's surrounding style — read it first):

```haskell
          -- #133: the concede channel round-trips like every other prompt. Note
          -- the prompt takes a PlayerId and NO Decider (CR 723.6).
          HU.testCase "Concede round-trips both ways" $
            let p = Prompt.Concede S.alice
             in do
                  HU.assertEqual "concedes" (Just Concession.Concedes) (Replay.decode p (Replay.encode p Concession.Concedes))
                  HU.assertEqual "continues" (Just Concession.Continues) (Replay.decode p (Replay.encode p Concession.Continues)),
          HU.testCase "a short transcript defaults a Concede to Continues" $
            HU.assertEqual "least eventful" Concession.Continues (Replay.defaultAnswer (Prompt.Concede S.alice)),
```

Add `import qualified Pawl.Type.Concession as Concession` to `ReplaySpec.hs`.

- [ ] **Step 2: Run it and watch it fail**

```bash
cabal build pawl-test-suite --enable-tests
```

Expected: FAIL — `Could not find module 'Pawl.Type.Concession'` and `Not in scope: data constructor 'Prompt.Concede'`.

- [ ] **Step 3: Create the `Concession` type**

Create `source/library/Pawl/Type/Concession.hs`:

```haskell
module Pawl.Type.Concession where

-- CR 104.3a: a player's answer when asked whether they concede.
--
-- A sum type rather than a Bool: this is an outcome ("I am leaving the game"),
-- not a predicate. See Pawl.Type.Prompt's Concede constructor for why the ask
-- exists at all and why it carries no Decider.
data Concession
  = Concedes
  | Continues
  deriving (Eq, Ord, Show)
```

- [ ] **Step 4: Add the `Concede` prompt**

In `source/library/Pawl/Type/Prompt.hs`, add the import `import Pawl.Type.Concession (Concession)` and add this constructor immediately after `ChooseAction` (they are asked back to back, so they should read together):

```haskell
  -- CR 104.3a: "A player can concede the game at any time. A player who concedes
  -- leaves the game immediately." Asked before ChooseAction wherever a player
  -- would receive priority.
  --
  -- This constructor deliberately carries NO Decider, and is the only one that
  -- does not. That asymmetry IS the CR 723.6 mechanism: a controller may not make
  -- a controlled player concede, but the controlled player may still concede
  -- themselves -- so the ask must reach the true player, and there must be
  -- nowhere to put a controller. Routing concede through ChooseAction (as an
  -- Action constructor) would hand the controller exactly the power CR 723.6
  -- forbids, and leave the controlled player with no channel at all.
  --
  -- "At any time" is narrowed to "at each priority grant" (#TBD-ELISION).
  Concede :: PlayerId -> Prompt Concession
```

Leave the literal text `#TBD-ELISION` in place; Task 4 replaces it with the real issue number.

- [ ] **Step 5: Add the `Response` arm**

In `source/library/Pawl/Type/Response.hs`, add `import Pawl.Type.Concession (Concession)` and this arm (placement: immediately after `ChoseAction`, matching the prompt order):

```haskell
  | -- CR 104.3a: whether a player conceded when asked, serialized so a
    -- DecisionLog replays the concession deterministically.
    Conceded Concession
```

- [ ] **Step 6: Add the three `Replay` arms**

In `source/library/Pawl/Replay.hs`, add `import qualified Pawl.Type.Concession as Concession`, then:

In `encode`, after the `Prompt.ChooseAction` arm:

```haskell
  Prompt.Concede _ -> Response.Conceded answer
```

In `decode`, after the `Prompt.ChooseAction` arm:

```haskell
  Prompt.Concede _ -> case response of
    Response.Conceded concession -> Just concession
    _ -> Nothing
```

In `defaultAnswer`, alongside the other fallbacks:

```haskell
  -- CR 104.3a: not conceding is always legal and is the least eventful fallback
  -- when a transcript runs short.
  Prompt.Concede _ -> Concession.Continues
```

- [ ] **Step 7: Add the arm to every exhaustive answer function**

```bash
cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -E "incomplete-patterns" -A 4
```

`-Werror=incomplete-patterns` names every function that needs an arm. Expect them in `source/test-suite/Pawl/Support.hs` (`identityAnswer`, `castAnswer`, `aggressiveAnswer`, `playLandAnswer`, `randomAnswer`), `source/test-suite/Pawl/CastSpec.hs`, `source/test-suite/Pawl/GameSpec.hs`, `source/test-suite/Pawl/ReplaySpec.hs`, and `source/benchmark/Main.hs`. Functions ending in `_ -> S.identityAnswer p` need nothing.

In each, add:

```haskell
  Prompt.Concede _ -> Concession.Continues
```

and the `Pawl.Type.Concession` import. For `randomAnswer`, whose result is in `State Random.StdGen`, use `pure Concession.Continues`.

**Every one of these answers `Continues`.** A test answer function that conceded would end the game before the behaviour it is testing. Task 3 introduces the one function that ever answers `Concedes`.

- [ ] **Step 8: Build clean and run the suite**

```bash
cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -iE "error|warning"
cabal test 2>&1 | grep -E "Concede|^All [0-9]+|FAIL"
```

Expected: no warnings; **937 tests pass** (935 plus the 2 new `ReplaySpec` cases). No existing test changes behaviour, because nothing prompts `Concede` yet.

- [ ] **Step 9: Commit**

```bash
git add -A source/ pawl.cabal
hooky fix && git add -A source/ pawl.cabal && hooky run
git commit -m "feat(concede): the Concession channel -- prompt, response, replay (#133)

Prompt.Concede carries a PlayerId and NO Decider. It is the only constructor in
the GADT without one, and that asymmetry is the CR 723.6 mechanism: a controller
cannot be routed a concession because the type has nowhere to put one.

Concession is a sum type, not a Bool -- conceding is an outcome, not a predicate.

Nothing polls the prompt yet, so no behaviour changes. Every answer function
answers Continues; a test answerer that conceded would end the game before the
behaviour under test."
```

---

## Task 3: Poll it in `Engine.priorityLoop`

Where the behaviour arrives.

**Files:**

- Modify: `source/library/Pawl/Engine.hs`
- Modify: `source/test-suite/Pawl/GameSpec.hs`

**Interfaces:**

- Consumes: `Departure.leaveGame` (Task 1); `Prompt.Concede`, `Concession.Concedes`, `Concession.Continues` (Task 2).
- Produces: nothing further.

- [ ] **Step 1: Write the failing tests**

In `source/test-suite/Pawl/GameSpec.hs`, append a new group and wire it into the file's `tests` list alongside `restartReentryTests cards`:

```haskell
-- #133 / CR 104.3a. Concede is a special action, not a card, so the gate is
-- gameplay-level. The central case is CR 723.6: a Mindslaver controller may not
-- concede for the player they control, but that player may still concede
-- themselves -- which is why Prompt.Concede carries no Decider.

-- Concedes for exactly one player, continues for everyone else, and otherwise
-- passes. The PlayerId the prompt carries is the TRUE player: if the engine ever
-- routed this through Decide.deciderFor, this answerer would concede for the
-- wrong person and the CR 723.6 test below would fail.
concedeAnswer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
concedeAnswer who p = case p of
  Prompt.Concede asked -> if asked == who then Concession.Concedes else Concession.Continues
  _ -> S.identityAnswer p

concedeTests :: Cards.Cards -> Tasty.TestTree
concedeTests cards =
  Tasty.testGroup
    "concede (CR 104.3a)"
    [ HU.testCase "CR 104.3a/104.2a a concede ends the game immediately, opponent wins" $
        let gs = (Setup.emptyGame S.bothPlayers) {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice}
            after = S.runPure (concedeAnswer S.alice) gs Engine.runStep
         in HU.assertEqual "bob wins" (Just (Result.Won S.bob)) (GameState.result after),
      HU.testCase "the conceding player departs as Conceded, not Lost" $
        let gs = (Setup.emptyGame S.bothPlayers) {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice}
            after = S.runPure (concedeAnswer S.alice) gs Engine.runStep
         in HU.assertEqual "reason recorded" (Just (Status.Departed Departure.Type.Conceded)) (fmap Player.status (Map.lookup S.alice (GameState.players after))),
      HU.testCase "CR 723.6 a controlled player concedes themselves; their controller cannot do it for them" $
        -- alice controls bob (Mindslaver). Every ChooseAction for bob is answered
        -- by alice. The concede ask is NOT: it reaches bob, and bob takes it.
        -- If Prompt.Concede carried a Decider, this would be alice's call.
        let gs =
              (Setup.emptyGame S.bothPlayers)
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.bob,
                  GameState.activeControl = Just (Decider.MkDecider S.alice)
                }
            after = S.runPure (concedeAnswer S.bob) gs Engine.runStep
         in do
              HU.assertEqual "bob left by his own concession" (Just (Status.Departed Departure.Type.Conceded)) (fmap Player.status (Map.lookup S.bob (GameState.players after)))
              HU.assertEqual "alice wins" (Just (Result.Won S.alice)) (GameState.result after),
      HU.testCase "CR 104.3a concede does not use the stack: a spell on it never resolves" $
        -- A Lightning Bolt is on the stack targeting nothing in particular. alice
        -- concedes at her priority; the game ends without the stack resolving.
        let (base, spellId) = S.handOne (Cards.lightningBoltPrinting cards) (Setup.emptyGame S.bothPlayers)
            gs =
              base
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.stack = [spellId]
                }
            after = S.runPure (concedeAnswer S.alice) gs Engine.runStep
         in do
              HU.assertEqual "bob wins" (Just (Result.Won S.bob)) (GameState.result after)
              HU.assertEqual "the spell never left the stack" [spellId] (GameState.stack after)
    ]
```

Imports to add to `GameSpec.hs` if absent: `Pawl.Type.Concession as Concession`, `Pawl.Type.Departure as Departure.Type`, `Pawl.Type.Status as Status`. `Decider`, `Player`, `Result`, `Phase`, `Map`, `PlayerId` are already imported — verify rather than assume.

`S.handOne` puts one card in a player's hand and returns its id; check its exact signature in `Pawl.Support` and adjust the binding if it differs from `(GameState, ObjectId)`.

- [ ] **Step 2: Run and watch them fail**

```bash
cabal test 2>&1 | grep -E "concede \(CR|expected|but got|FAIL"
```

Expected: all four FAIL. `GameState.result` will be `Nothing` because nothing ever asks `Prompt.Concede`, so `concedeAnswer` is never consulted.

- [ ] **Step 3: Poll the prompt**

In `source/library/Pawl/Engine.hs`, add the imports:

```haskell
import qualified Pawl.Departure as Departure
import qualified Pawl.Type.Concession as Concession
import qualified Pawl.Type.Departure as Departure.Type
```

(`Pawl.Departure` is already imported from Task 1 — do not duplicate it.)

Then, inside `priorityLoop`'s inner `loop`, replace this:

```haskell
              Just p -> do
                -- CR 723.6: concede is not implemented — there is no Concede
                -- action or concede channel. When it is added it must read the
                -- true player `p`, never `decider`: a controller cannot concede
                -- on the controlled player's behalf (CR 104.3a). (#133)
                let decider = Decide.deciderFor p gs
                    actions = Action.legalActions p gs
                chosen <- Trans.lift (Program.prompt (Prompt.ChooseAction decider p actions))
```

with this:

```haskell
              Just p -> do
                -- CR 104.3a: asked before anything else, and keyed to `p` -- the
                -- TRUE player, never `Decide.deciderFor p`. Prompt.Concede carries
                -- no Decider precisely so this cannot be got wrong (CR 723.6): a
                -- controller may not concede for the player they control, though
                -- that player may still concede themselves.
                concession <- Trans.lift (Program.prompt (Prompt.Concede p))
                case concession of
                  Concession.Concedes -> do
                    -- CR 104.3a: leaves the game IMMEDIATELY. Not a state-based
                    -- action (that is CR 104.3b), so it does not wait for a settle,
                    -- and nothing goes on the stack -- there is nothing to respond
                    -- to. leaveGame settles CR 104.2a on the spot; the loop's own
                    -- `finished` check then unwinds on the next iteration.
                    Departure.leaveGame Departure.Type.Conceded p
                    State.modify' (\g -> g {GameState.priority = Just (nextStillPlaying g p)})
                    loop
                  Concession.Continues -> do
                    let decider = Decide.deciderFor p gs
                        actions = Action.legalActions p gs
                    chosen <- Trans.lift (Program.prompt (Prompt.ChooseAction decider p actions))
```

The whole remaining body of the `Just p` branch (the `case chosen of …` block) must be indented one level deeper to sit under `Concession.Continues`. Do this carefully — it is the largest mechanical edit in the plan. Verify with `cabal build` before running tests; a mis-indent shows up as a parse error, not a test failure.

Note `nextStillPlaying` already exists in `Pawl.Engine`; it takes the state and the current player. Confirm its exact signature at its definition before using it.

- [ ] **Step 4: Run the new tests, then the whole suite**

```bash
cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -iE "error|warning"
cabal test 2>&1 | grep -E "concede \(CR|^All [0-9]+|FAIL"
```

Expected: no warnings; the four concede cases PASS; **941 tests pass**.

If a pre-existing test now fails, the likely cause is an answer function from Task 2 that answers `Concedes`. Fix the answerer, never the assertion.

- [ ] **Step 5: Check the benchmark still measures a game**

```bash
cabal bench 2>&1 | tail -12
```

Expected: three benchmarks that still differ from each other. Prompt volume roughly doubles, so absolute times may rise; what must not happen is the three collapsing to the same number, which would mean a benchmark answerer is conceding immediately.

- [ ] **Step 6: Commit**

```bash
git add -A source/
hooky fix && git add -A source/ && hooky run
git commit -m "feat(concede): poll the concede channel at every priority grant (#133)

CR 104.3a: a player may concede at any priority they hold. The ask is keyed to
the true player and precedes ChooseAction, so a player may concede regardless of
what else they could do.

CR 723.6 is the point of the design and the central test: alice controls bob via
Mindslaver, every ChooseAction for bob is answered by alice, and bob still
concedes himself. Prompt.Concede carries no Decider, so alice has no way to
answer it.

CR 104.3a is immediate, not a state-based action (CR 104.3b), so leaveGame
settles CR 104.2a on the spot. Nothing goes on the stack -- there is nothing to
respond to -- and a spell on the stack when a player concedes never resolves.

The stale guard comment at the ChooseAction site is deleted: the type now
enforces what it asked readers to remember."
```

---

## Task 4: File the elision and close out

**Files:**

- Modify: `source/library/Pawl/Type/Prompt.hs` (replace the `#TBD-ELISION` placeholder)
- Modify: `docs/superpowers/specs/2026-07-23-concede-special-action-design.md` (status line)

- [ ] **Step 1: File the elision issue**

```bash
gh issue create --label elision --label rules-correctness --label expires:card-driven \
  --title "CR 104.3a: concede is offered only at one's own priority, not \"at any time\"" \
  --body 'Status: elided. Pawl.Engine.priorityLoop polls Prompt.Concede before each ChooseAction, so a player may concede only at a priority they themselves hold. CR 104.3a says "at any time" — including while an opponent holds priority.

Why it is mostly fine: a client can record the intent the moment the player expresses it and hand the buffered concession to the engine when that player next holds priority. The channel needs no change to support that.

Why it is NOT merely cosmetic latency, and must not be filed as such: settleForPriority runs state-based actions before every priority grant. If the opponent is themselves about to lose inside that window — at 1 life under an upkeep trigger, or about to draw from an empty library — then conceding immediately makes that opponent the winner (CR 104.2a), while conceding at the conceder'"'"'s next priority lets the opponent depart first and the conceder wins, or the game is drawn. The race decides the game, not just its timing.

A second, cheaper consideration: Replay.replay is positional, so narrowing or widening the polling frequency later invalidates transcripts recorded at the old frequency. Free today (nothing persists a transcript, #126); not free once save/replay ships.

Expiry trigger: a card or subsystem requiring concession outside one'"'"'s own priority, or any client that surfaces the race above.

Design: docs/superpowers/specs/2026-07-23-concede-special-action-design.md section 7.'
```

- [ ] **Step 2: Replace the placeholder with the real number**

In `source/library/Pawl/Type/Prompt.hs`, change `(#TBD-ELISION)` to `(#N)` using the number the previous step printed. Confirm none remain:

```bash
grep -rn "TBD-ELISION" source/ && echo "STILL PRESENT — fix before committing"
```

- [ ] **Step 3: Update the spec's status line**

Change line 3 of `docs/superpowers/specs/2026-07-23-concede-special-action-design.md` from `**Status:** design approved 2026-07-23, not yet implemented.` to `**Status:** implemented 2026-07-23.`

- [ ] **Step 4: Final verification**

```bash
cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -iE "error|warning"
cabal test 2>&1 | grep -E "^All [0-9]+|FAIL"
grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-23-concede-special-action.md
```

Expected: no warnings, 941 tests pass, and the step count reaches `0`.

- [ ] **Step 5: Commit and close**

```bash
git add -A source/ docs/
hooky fix && git add -A source/ docs/ && hooky run
git commit -m "docs(concede): file the \"at any time\" elision, mark the spec implemented (#133)"
gh issue close 133 --comment "Implemented across the four tasks of docs/superpowers/plans/2026-07-23-concede-special-action.md. Prompt.Concede carries no Decider, which makes the CR 723.6 violation unrepresentable rather than something a comment warns about; the central test is a Mindslaver-controlled player conceding themselves while their controller answers every other prompt for them. Departure machinery extracted to Pawl.Departure because CR 104.3a is immediate where CR 104.3b is a state-based action. The \"at any time\" narrowing is filed separately with the SBA race recorded."
```

---

## Self-Review

**Spec coverage.** §1 (why `Action.Concede` fails) → Task 3's CR 723.6 test. §2 (the channel, no `Decider`) → Task 2 Step 4. §3 (where polled) → Task 3 Step 3. §4 (`Pawl.Departure`) → Task 1. §5 (unwinding via `result`, subgames) → relies on existing behaviour; the Task 3 tests cover the unwind. §6 (cost) → Task 2 Step 7. §7 (the elision) → Task 4 Step 1. §8 (out of scope) → nothing to do. §9 (tests) → Task 3 Step 1, all four. §10 (exit criterion) → Task 4 Step 4.

**Known soft spots for the implementer:**

- Task 1 Step 1's test file uses `S.PlayerIdAlias` as a stand-in; Step 1 says to substitute the real `PlayerId.PlayerId`. Do not leave it.
- Task 3 Step 3 requires re-indenting a large block. Build before testing.
- Test counts (935 / 937 / 941) assume the suite is at 931 when Task 1 begins. If it is not, the deltas (+4, +2, +4) still hold.
