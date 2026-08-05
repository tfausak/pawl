# CR 104.4b Mandatory-Loop Draw Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `Engine.playGame` ends a game in `Result.Drawn` (CR 104.4b) once its events have repeated far past any point at which a player could have chosen otherwise, instead of spinning forever.

**Architecture:** A new `GameState.lastChoice` timestamp records when a player was last offered an optional action; a single `Game.choose` wrapper around `Program.prompt` is the only thing that writes it, so the reset cannot be forgotten at a new prompt site. A `checkMandatoryLoop` guard at the heads of `playGame`'s and `priorityLoop`'s loops draws the game when `nextTimestamp - lastChoice` reaches a fixed limit. One labeled synthetic creature plus the pool's Aether Flash builds the proving loop.

**Tech Stack:** Haskell (GHC 9.14.1 from the Nix flake), `cabal`, the `tasty` test suite via `Pawl.Spec`, `hooky` for format/lint.

**Spec:** `docs/superpowers/specs/2026-08-04-cr-104-4b-mandatory-loop-draw-design.md`. Read it before Task 1.

## Global Constraints

- **The build must be warning-free.** `cabal build all` — the suites break separately from the library, so build `all`.
- **One build at a time.** `jobs: $ncpus` already saturates the machine. Never run two builds or a build and a test concurrently.
- **Run the suite as** `cabal test --test-options '--timeout 1s --hide-successes'`. Never pipe its output — that stalls a ~30s suite for minutes.
- **`hooky fix` then `hooky run`, on staged files only.** `git add` first; `hooky fix` reformats, so `git add` again before `hooky run`.
- **Never trust recalled Magic rules.** Every CR claim is checked against `docs/rules.txt`, grepped by rule number, and the rule number goes in the code comment.
- **Constructors take a `Mk` prefix.** Language extensions come from the allowlist in `.hlint.yaml`. No `fromIntegral`/`toEnum` — convert through `Pawl.Extra.*`.
- **The rules core must never case on an effect's identity.** Nothing in this plan does; keep it that way.
- **Do not edit this plan** to make a check pass. If it looks wrong, stop and say so.

The two CR texts this plan rests on, verbatim from `docs/rules.txt`:

> 104.3a A player can concede the game at any time. A player who concedes leaves the game immediately. That player loses the game.

> 104.4b If a game that's not using the limited range of influence option (including a two-player game) somehow enters a "loop" of mandatory actions, repeating a sequence of events with no way to stop, the game is a draw. Loops that contain an optional action don't result in a draw.

## File Structure

| File | Responsibility | Task |
|---|---|---|
| `source/libraries/types/Pawl/Types/GameState.hs` | new `lastChoice` field | 1 |
| `source/libraries/engine/Pawl/Engine/Game.hs` | the `choose` choke point | 1 |
| `source/libraries/engine/Pawl/Engine/Setup.hs` | initialize `lastChoice`, and set it at the two subgame seams | 1 |
| `source/libraries/engine/Pawl/Engine/{Engine,Activate,Cast,Combat,Cost,Damage,Mana,Mulligan,Replacement,Resolve,Sba}.hs` | rewire prompt sites to `Game.choose` | 1 |
| `source/libraries/engine/Pawl/Engine/Engine.hs` | `mandatoryLoopLimit`, `checkMandatoryLoop`, and the two loop heads | 2 |
| `data/cards/synthetic-recursion.json` | the labeled synthetic half of the proving board | 3 |
| `source/test-suite/Pawl/GameSpec.hs` | all tests (it already covers `Pawl.Engine.Game` and `Pawl.Engine.Engine`) | 1, 2, 3 |

---

### Task 1: `GameState.lastChoice` and the `Game.choose` choke point

**Files:**
- Modify: `source/libraries/types/Pawl/Types/GameState.hs` (field, near `nextTimestamp` at :146)
- Modify: `source/libraries/engine/Pawl/Engine/Game.hs` (add `choose`, after `freshTimestamp` at :50-53)
- Modify: `source/libraries/engine/Pawl/Engine/Setup.hs` (`emptyGame` around :89, `subgameStateFrom` around :436, `funnelBack`)
- Modify: every engine module holding a `Trans.lift (Program.prompt …)` — `Engine.hs`, `Activate.hs`, `Cast.hs`, `Combat.hs`, `Cost.hs`, `Damage.hs`, `Mana.hs`, `Mulligan.hs`, `Replacement.hs`, `Resolve.hs`, `Sba.hs`
- Test: `source/test-suite/Pawl/GameSpec.hs`

**Interfaces:**
- Produces: `GameState.lastChoice :: Timestamp.Timestamp`; `Pawl.Engine.Game.choose :: Prompt.Prompt a -> Game.Type.Game a`
- Consumes: nothing from earlier tasks

- [ ] **Step 1: Write the two failing tests**

Add to `source/test-suite/Pawl/GameSpec.hs`. Wire `lastChoiceSpec` into the module's aggregate `tests` list beside the other `*Spec` groups, following the shape of the neighbouring `restartReentrySpec` entry (it takes `s` and `registry` the same way).

```haskell
-- CR 104.4b's "loops that contain an optional action": what makes an action
-- optional is that the player had more than one answer. GameState.lastChoice is
-- how the engine remembers when that last happened, and Pawl.Engine.Game.choose
-- is the only thing that writes it.
lastChoiceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lastChoiceSpec s registry = Spec.describe s "lastChoice (CR 104.4b)" $ do
  Spec.it s "a priority round whose only action is Pass leaves it alone" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, after) = S.runPure S.identityAnswer (passOnlyBoard piker) Engine.priorityLoop
    Spec.assertEqWith s "no player was offered a choice" (GameState.lastChoice after) (Timestamp.MkTimestamp 0)

  Spec.it s "a priority round offering a mana ability moves it" $ do
    -- The board differs by ONE permanent: a Mountain, whose CR 605.1a mana
    -- ability is a second entry on alice's menu. That is the optional action.
    mountain <- S.printingOf s registry "Mountain"
    let (_, after) = S.runPure S.identityAnswer (passOnlyBoard mountain) Engine.priorityLoop
    Spec.assertBool s (GameState.lastChoice after > Timestamp.MkTimestamp 0) "alice was offered a choice"

-- alice, active, in her precombat main phase with an empty stack and nothing
-- scheduled after it; `only` is the one permanent she controls. A vanilla
-- creature offers her nothing, so her menu is exactly [Pass]; a land offers its
-- mana ability.
passOnlyBoard :: Printing.Printing -> GameState.GameState
passOnlyBoard only =
  let base = Setup.emptyGame S.bothPlayers
      (_, gs1) = S.addCreature only S.alice base
   in gs1
        { GameState.phase = Phase.PrecombatMain,
          GameState.remaining = Seq.empty,
          GameState.lastChoice = Timestamp.MkTimestamp 0
        }
```

Add whatever imports the module is missing for this — it already imports `Seq`, `Setup`, `GameState`, `Phase` and `S`; `Timestamp` and `Printing` may need adding.

- [ ] **Step 2: Run the tests to verify they fail**

```
cabal build all
cabal test --test-options '--timeout 1s --hide-successes'
```

Expected: a compile error, `GameState.lastChoice` is not a field of `GameState`.

- [ ] **Step 3: Add the field**

In `source/libraries/types/Pawl/Types/GameState.hs`, directly after `nextTimestamp` (:146):

```haskell
    -- | CR 104.4b: the timestamp as of the last time a player was offered an
    -- OPTIONAL action. The gap between this and nextTimestamp is how many
    -- events have happened with no player able to decide anything, which is
    -- Engine.checkMandatoryLoop's heuristic for a loop of mandatory actions.
    --
    -- Written only by Pawl.Engine.Game.choose, so a new prompt site cannot
    -- forget to reset it. Concede is deliberately not one of its callers: CR
    -- 104.3a lets a player concede at any time, so if conceding counted, no
    -- loop would ever be mandatory.
    lastChoice :: Timestamp.Timestamp,
```

- [ ] **Step 4: Initialize it, including at both subgame seams**

In `source/libraries/engine/Pawl/Engine/Setup.hs`:

- `emptyGame` (near :89, beside `GameState.nextTimestamp = Timestamp.MkTimestamp 0`):
  ```haskell
          GameState.lastChoice = Timestamp.MkTimestamp 0,
  ```
- `subgameStateFrom` — set the subgame's marker to the subgame's own starting `nextTimestamp`, whatever that line computes it to be. A copied parent marker would make the subgame draw itself for events at another level.
- `funnelBack` (near :436, the line that maxes `nextTimestamp`) — set the parent's `lastChoice` to that same merged `nextTimestamp`. A whole subgame's worth of events is not a stretch during which the parent's players could not act.

- [ ] **Step 5: Add the choke point**

In `source/libraries/engine/Pawl/Engine/Game.hs`, after `freshTimestamp` (:53). The module needs imports for `Control.Monad.Trans.Class as Trans`, `Control.Monad.Trans.State.Strict as State`, `Pawl.Types.Game as Game.Type`, `Pawl.Types.Program as Program` and `Pawl.Types.Prompt as Prompt` — all in the `types` sublibrary, so no dependency edge inverts.

```haskell
-- Ask a player something they could answer more than one way, and record that
-- they were asked (GameState.lastChoice). CR 104.4b's second sentence is why
-- this is a funnel rather than a note at each site: a loop containing an
-- optional action is not a draw, and this is the engine's whole definition of
-- an optional action.
--
-- THREE prompt sites deliberately stay bare `Trans.lift (Program.prompt …)`:
--
--   * Prompt.Concede, because CR 104.3a lets a player concede at any time, in
--     or out of a loop. If it reset the marker, no loop would ever be
--     mandatory and this whole mechanism would be dead code.
--   * Prompt.ChooseAction when Pass is the only legal action. Passing is not a
--     decision. Engine.priorityLoop makes that call, since only it knows the
--     menu.
--   * Prompt.Shuffle and Prompt.RandomFirstPlayer, which ask for randomness
--     rather than for a choice (CR 701.20, CR 729.2). A loop that reshuffles
--     every cycle is still a loop of mandatory actions.
--
-- Every other site already elides its prompt when the answer is forced, so only
-- the branch that genuinely asks reaches this.
choose :: Prompt.Prompt a -> Game.Type.Game a
choose p = do
  State.modify' (\gs -> gs {GameState.lastChoice = GameState.nextTimestamp gs})
  Trans.lift (Program.prompt p)
```

- [ ] **Step 6: Rewire every prompt site**

Mechanically replace `Trans.lift (Program.prompt X)` with `Game.choose X` at every site listed below, dropping one layer of parentheses. Each module already imports `Pawl.Engine.Game as Game`; add the import where it does not. Remove any `Program`/`Trans` import that becomes unused — the build is warning-free, so the compiler will tell you.

| Module | Sites |
|---|---|
| `Replacement.hs` | `ChooseReplacement`, `ChooseCopyTarget`, `ChooseEntryOption`, `ChooseColor`, `ChooseBasicLandType`, `ChooseOpponent`, `ChooseCardName`, `OrderDamage` |
| `Mana.hs` | `ChooseManaYield`, `ChooseManaSource`, and the bound `prompt` at :712 |
| `Engine.hs` | `ChooseDiscard` (:235), `ChooseModes` (:481), `ChooseTargets` (:489), `OrderTriggers` (:538) |
| `Activate.hs` | `ChooseModes`, `ChooseX`, `ChooseTargets` |
| `Cast.hs` | `CastWhileSearching`, `ChooseEntwine`, `ChooseModes`, `ChooseCost`, `ChooseX`, `ChooseTargets` |
| `Cost.hs` | `ChooseSacrifices`, `ChooseDiscard` |
| `Sba.hs` | `ChooseLegend` |
| `Mulligan.hs` | the bound `ask` at :98, `DeclareMulligan`, `Bottom` |
| `Resolve.hs` | `ChooseOptional`, the bound `ask` at :878, `SearchLibrary`, `ChooseDiscard`, `ChooseSacrifices`, `ChooseBoundToken`, `ChooseAttachment`, `ChooseProliferate` |
| `Combat.hs` | `ChooseDefender`, `ChooseAttackTarget`, `DeclareAttackers`, `DeclareBlockers` |
| `Damage.hs` | `AssignCombatDamage` (:251) |

Left bare, as the comment in Step 5 says: `Engine.hs`'s `Concede` (:669) and `RandomFirstPlayer` (:1238), `Mulligan.hs`'s `Shuffle`, and `Resolve.hs`'s `Shuffle`.

- [ ] **Step 7: Make `ChooseAction` conditional**

In `Engine.hs`'s `priorityLoop`, replace the unconditional prompt at :698:

```haskell
                            -- CR 104.4b: a menu that is only Pass offers no
                            -- optional action, so it does not break a loop of
                            -- mandatory actions. Still ASKED -- the interpreter
                            -- and the Replay fold see every priority grant --
                            -- just not recorded as a choice.
                            answered <-
                              if length actions > 1
                                then Game.choose (Prompt.ChooseAction decider p actions)
                                else Trans.lift (Program.prompt (Prompt.ChooseAction decider p actions))
```

- [ ] **Step 8: Run the tests to verify they pass**

```
cabal build all
cabal test --test-options '--timeout 1s --hide-successes'
```

Expected: PASS, and the suite count is 2 higher than before. Nothing else should have changed — if another spec's prompt count moved, `choose` was wired into a site that used to be elided.

- [ ] **Step 9: Commit**

```bash
git add source/libraries/types/Pawl/Types/GameState.hs source/libraries/engine/Pawl/Engine/ source/test-suite/Pawl/GameSpec.hs
hooky fix
git add source/libraries/types/Pawl/Types/GameState.hs source/libraries/engine/Pawl/Engine/ source/test-suite/Pawl/GameSpec.hs
hooky run
git commit -m "Record when a player was last offered a choice (#338)"
```

---

### Task 2: `checkMandatoryLoop` and the two loop heads

**Files:**
- Modify: `source/libraries/engine/Pawl/Engine/Engine.hs` (`priorityLoop` :645, `playGame` :1197)
- Test: `source/test-suite/Pawl/GameSpec.hs`

**Interfaces:**
- Consumes: `GameState.lastChoice`, `Pawl.Engine.Game.choose` (Task 1)
- Produces: `Engine.mandatoryLoopLimit :: Natural.Natural`; `Engine.checkMandatoryLoop :: Game.Type.Game ()`

- [ ] **Step 1: Write the failing tests**

Add to `source/test-suite/Pawl/GameSpec.hs` and wire `mandatoryLoopSpec` into the aggregate `tests` list.

```haskell
-- CR 104.4b, the guard itself. The gameplay proof that it is reached from a real
-- loop is in Pawl.GameSpec's "a mandatory loop" group; these three pin the
-- boundary, which a board-level test cannot do precisely.
mandatoryLoopSpec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
mandatoryLoopSpec s = Spec.describe s "the mandatory-loop guard (CR 104.4b)" $ do
  Spec.it s "one event short of the limit is not a draw" $ do
    let gs = atGap (Engine.mandatoryLoopLimit - 1)
        (_, after) = S.runPure S.identityAnswer gs Engine.checkMandatoryLoop
    Spec.assertEqWith s "still being played" (GameState.result after) Nothing

  Spec.it s "the limit is a draw" $ do
    let gs = atGap Engine.mandatoryLoopLimit
        (_, after) = S.runPure S.identityAnswer gs Engine.checkMandatoryLoop
    Spec.assertEqWith s "CR 104.4b" (GameState.result after) (Just Result.Drawn)

  Spec.it s "a game that already ended keeps the result it had" $ do
    -- A draw must never overwrite a win. The guard fires on a state that could
    -- only be reached by a loop, and a won game is not looping.
    let gs = (atGap Engine.mandatoryLoopLimit) {GameState.result = Just (Result.Won S.alice)}
        (_, after) = S.runPure S.identityAnswer gs Engine.checkMandatoryLoop
    Spec.assertEqWith s "alice still won" (GameState.result after) (Just (Result.Won S.alice))

  Spec.it s "playGame draws instead of looping" $ do
    -- Proves the guard is WIRED, not merely correct: playGame's loop head is
    -- what reads it. The board is empty, so nothing but the guard can end this.
    let (result, _) = Engine.runGamePure S.identityAnswer (atGap Engine.mandatoryLoopLimit) Engine.playGame
    Spec.assertEqWith s "CR 104.4b" result Result.Drawn

-- An otherwise-idle two-player game in which `n` events have happened since any
-- player was last offered a choice.
atGap :: Natural -> GameState.GameState
atGap n =
  (Setup.emptyGame S.bothPlayers)
    { GameState.nextTimestamp = Timestamp.MkTimestamp n,
      GameState.lastChoice = Timestamp.MkTimestamp 0
    }
```

- [ ] **Step 2: Run the tests to verify they fail**

```
cabal build all
cabal test --test-options '--timeout 1s --hide-successes'
```

Expected: a compile error, `Engine.mandatoryLoopLimit` and `Engine.checkMandatoryLoop` are not in scope.

- [ ] **Step 3: Write the guard**

In `source/libraries/engine/Pawl/Engine/Engine.hs`, above `priorityLoop`:

```haskell
-- CR 104.4b: how many events may happen with no player able to decide anything
-- before the game is declared a loop of mandatory actions.
--
-- A HEURISTIC, and deliberately a crude one: detecting such a loop in general is
-- the halting problem, so the question this answers is not "is this a loop?" but
-- "has this gone on so long that no real game would have?". The margin is what
-- makes it safe. GameState.nextTimestamp advances on the events CR 104.4b names
-- -- an object entering a zone (CR 613.7d) and a continuous effect beginning (CR
-- 613.7a) -- and a quiet game issues about one per turn, so a game that ends
-- slowly by decking out sits three orders of magnitude under this. A two-card
-- recursion loop issues several per cycle and reaches it in a few hundred.
--
-- Not configurable. There is one caller and one sensible value, and a tuning
-- knob threaded through GameState would be a second thing to keep right.
mandatoryLoopLimit :: Natural.Natural
mandatoryLoopLimit = 1000

-- CR 104.4b: the game is a draw. Never overwrites a result the game already has
-- -- a won game is not looping, and CR 104.4a's simultaneous loss is a different
-- draw with its own path (Departure.leaveGame).
checkMandatoryLoop :: Game ()
checkMandatoryLoop = State.modify' $ \gs ->
  let gap = Timestamp.unwrap (GameState.nextTimestamp gs) - Timestamp.unwrap (GameState.lastChoice gs)
   in if Maybe.isNothing (GameState.result gs) && gap >= mandatoryLoopLimit
        then gs {GameState.result = Just Result.Drawn}
        else gs
```

`Timestamp.unwrap` returns a `Natural`, and `lastChoice` is never above `nextTimestamp` (the only writer copies it), so the subtraction cannot underflow.

- [ ] **Step 4: Wire it into both loop heads**

`playGame` (:1198):

```haskell
playGame =
  let loop = do
        checkMandatoryLoop
        outcome <- State.gets GameState.result
```

`priorityLoop`'s inner `loop` (:645):

```haskell
  let loop = do
        checkMandatoryLoop
        finished <- State.gets (Maybe.isJust . GameState.result)
```

Both already read `GameState.result` immediately after, so the draw unwinds along the path a concession or a deck-out already takes. Add whatever imports `Engine.hs` is missing (`Result`, `Timestamp`, `Natural`).

- [ ] **Step 5: Run the tests to verify they pass**

```
cabal build all
cabal test --test-options '--timeout 1s --hide-successes'
```

Expected: PASS, suite count 4 higher.

- [ ] **Step 6: Commit**

```bash
git add source/libraries/engine/Pawl/Engine/Engine.hs source/test-suite/Pawl/GameSpec.hs
hooky fix
git add source/libraries/engine/Pawl/Engine/Engine.hs source/test-suite/Pawl/GameSpec.hs
hooky run
git commit -m "Draw a game stuck in a loop of mandatory actions (#338)"
```

---

### Task 3: the proving board

**Files:**
- Create: `data/cards/synthetic-recursion.json`
- Test: `source/test-suite/Pawl/GameSpec.hs`

**Interfaces:**
- Consumes: `Engine.mandatoryLoopLimit`, `Engine.checkMandatoryLoop` (Task 2)
- Produces: nothing later tasks read

Why a synthetic. The canonical board is Worldgorger Dragon reanimated by Animate Dead, and Animate Dead needs an Aura that enchants a card in a graveyard, rewrites its own enchant clause as it enters, re-attaches itself, and sacrifices the creature when it leaves — none of which pawl has. Endless Whispers, the other textbook loop, needs a static ability that grants a triggered ability, and `Pawl.Types.Modification` has no such arm. So the fixture is real Aether Flash plus one labeled synthetic, following `synthetic-restart.json` and `synthetic-subgame.json`. Task 4 files the issue that retires it.

- [ ] **Step 1: Write the card**

`data/cards/synthetic-recursion.json` — a 1/1 creature reading "When this creature dies, return it to the battlefield." `SelfDies` binds CR 400.7e's `became` slot (`Pawl.Engine.Event.eventBindings`), which is the graveyard incarnation the `MoveToZone` names. `MoveToZone`'s JSON is two elements: the CR 110.5b default entry riders and the absent rebind slot are both elided by `Pawl.Codec.Effect.toJson`.

```json
{
  "faces": [
    {
      "manaCost": [
        {
          "type": "Generic",
          "value": 1
        },
        {
          "type": "OfType",
          "value": {
            "type": "Colored",
            "value": {
              "type": "Black"
            }
          }
        }
      ],
      "name": "Synthetic Recursion",
      "power": {
        "type": "Literal",
        "value": 1
      },
      "toughness": {
        "type": "Literal",
        "value": 1
      },
      "triggeredAbilities": [
        {
          "condition": {
            "type": "SelfDies"
          },
          "modal": {
            "modes": [
              {
                "effects": [
                  {
                    "type": "MoveToZone",
                    "value": [
                      "became",
                      {
                        "type": "Battlefield"
                      }
                    ]
                  }
                ]
              }
            ]
          }
        }
      ],
      "typeLine": {
        "types": [
          {
            "type": "Creature"
          }
        ]
      }
    }
  ]
}
```

Write it with sorted keys and pretty-printed (`jq -S . < f > f.tmp && mv f.tmp f`): `Pawl.CardsSpec`'s "each committed file re-parses to its compiled card" compares `Json.sortKeys` of both sides, and keys the codec omits when empty must be **absent**, not present-and-empty.

- [ ] **Step 2: Run the suite to confirm the card round-trips**

```
cabal build all
cabal test --test-options '--timeout 1s --hide-successes'
```

Expected: PASS. `S.allPrintings` sweeps `data/cards/`, so the new file gets whole-pool codec coverage for free. A failure here means the JSON does not match the codec's canonical emission — fix the JSON, not the codec.

- [ ] **Step 3: Write the two failing gameplay tests**

Add to `source/test-suite/Pawl/GameSpec.hs` and wire `mandatoryLoopBoardSpec` into the aggregate `tests` list.

```haskell
-- CR 104.4b at gameplay level. Aether Flash deals 2 to each creature that
-- enters; Synthetic Recursion is a 1/1 that returns itself when it dies. It
-- enters, takes lethal damage (CR 704.5g), dies, returns, enters. Nothing in the
-- cycle is optional and nothing in it makes progress: a loop of mandatory
-- actions, repeating a sequence of events with no way to stop.
--
-- Synthetic Recursion is a LABELED CRUTCH (#N1): the canonical board is
-- Worldgorger Dragon reanimated by Animate Dead, and neither card is authorable
-- yet.
mandatoryLoopBoardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mandatoryLoopBoardSpec s registry = Spec.describe s "a mandatory loop (CR 104.4b)" $ do
  Spec.it s "a loop nobody can interrupt is a draw" $ do
    flash <- S.printingOf s registry "Aether Flash"
    recursion <- S.printingOf s registry "Synthetic Recursion"
    let result = runConcedingAt 10000 (loopBoard flash recursion Nothing)
    Spec.assertEqWith s "CR 104.4b" result Result.Drawn

  Spec.it s "a loop containing an optional action is not" $ do
    -- CR 104.4b's second sentence. The board differs by ONE permanent: a
    -- Mountain alice controls, whose mana ability is on her menu every time she
    -- gets priority. That resets the marker each round, so the loop -- which is
    -- otherwise identical, and still runs -- is never declared a draw. It ends
    -- because alice eventually concedes (CR 104.3a), which is the only way to
    -- get a terminating test out of a game the rules say does not end.
    flash <- S.printingOf s registry "Aether Flash"
    recursion <- S.printingOf s registry "Synthetic Recursion"
    mountain <- S.printingOf s registry "Mountain"
    let result = runConcedingAt 200 (loopBoard flash recursion (Just mountain))
    Spec.assertEqWith s "alice conceded, so bob won -- no draw" result (Result.Won S.bob)

-- alice, active, in her precombat main phase: bob's Aether Flash, alice's
-- Synthetic Recursion entering (so CR 603.6a's event is there for Aether Flash
-- to see), and optionally one more permanent under alice. Empty hands, empty
-- libraries, nothing scheduled after this phase -- so the loop is the only thing
-- that can happen and nothing else can end the game.
--
-- Seeded ten events short of the limit rather than starting from zero: the
-- mechanism is the same at any gap, and this keeps the test at a handful of
-- cycles instead of a few hundred.
loopBoard :: Printing.Printing -> Printing.Printing -> Maybe Printing.Printing -> GameState.GameState
loopBoard flash recursion extra =
  let base = Setup.emptyGame S.bothPlayers
      (_, gs1) = S.addCreature flash S.bob base
      gs2 = case extra of
        Nothing -> gs1
        Just printing -> snd (S.addCreature printing S.alice gs1)
      (_, gs3) = S.entersWithTrigger recursion S.alice gs2
   in gs3
        { GameState.phase = Phase.PrecombatMain,
          GameState.remaining = Seq.empty,
          GameState.nextTimestamp = Timestamp.MkTimestamp (Engine.mandatoryLoopLimit - 10),
          GameState.lastChoice = Timestamp.MkTimestamp 0
        }

-- Play the game out, with alice conceding once she has been asked `limit` times.
-- The concession is a backstop for the test that expects NO draw: without it
-- that game would run forever, which is exactly what CR 104.4b says should
-- happen to a loop containing an optional action.
runConcedingAt :: Int -> GameState.GameState -> Result.Result
runConcedingAt limit gs =
  let answer :: Prompt.Prompt r -> State.State Int r
      answer p = case p of
        Prompt.Concede pid
          | pid == S.alice -> do
              asked <- State.get
              State.put (asked + 1)
              pure (if asked >= limit then Concession.Concedes else Concession.Continues)
        _ -> pure (S.identityAnswer p)
      ((result, _), _) = State.runState (Engine.runGame answer gs Engine.playGame) 0
   in result
```

- [ ] **Step 4: Run the tests to verify they fail for the right reason**

```
cabal build all
cabal test --test-options '--timeout 1s --hide-successes'
```

Both should now PASS, since Task 2 already built the guard. **This is the one place in the plan where a passing test is not enough** — the second test is the discriminator and must be shown to discriminate. Verify by temporarily raising `mandatoryLoopLimit` to a value the first test cannot reach (say `100000`) and re-running: the first test must then FAIL by timing out or by returning `Won bob`, proving it was the guard that drew it and not something else on the board. Put the limit back to `1000` afterwards.

If instead the first test times out with the real limit, the loop is not looping — check that `S.entersWithTrigger` put the CR 603.6a event where `Engine.placePendingTriggers` finds it, and that `MoveToZone` returned the creature under its owner's control.

- [ ] **Step 5: Commit**

```bash
git add data/cards/synthetic-recursion.json source/test-suite/Pawl/GameSpec.hs
hooky fix
git add data/cards/synthetic-recursion.json source/test-suite/Pawl/GameSpec.hs
hooky run
git commit -m "Prove the CR 104.4b draw with a board that loops (#338)"
```

---

### Task 4: retire the stale comments, file the follow-up, open the PR

**Files:**
- Modify: `source/libraries/engine/Pawl/Engine/Engine.hs` (`cleanupException`'s comment at :1150-1154, `playGame`'s at :1173-1196)
- Modify: `source/test-suite/Pawl/GameSpec.hs` (replace `#N1` with the real issue number)

**Interfaces:**
- Consumes: everything from Tasks 1-3

- [ ] **Step 1: Rewrite the two stale comments**

Both currently say pawl has no loop detector and cite #338/#484 as open. `cleanupException`'s tail (:1150-1154) reads:

> It is NOT bounded in general, and Magic does not bound it either: an ability that triggers at the beginning of each cleanup step loops forever, and CR 104.4b's draw is the rules' answer to that rather than a bound on the loop. pawl does not detect such a loop (#484), which is the same engine-liveness gap #338 opened from the turn side.

Replace the last sentence with a statement that `playGame` and `priorityLoop` now apply CR 104.4b's draw through `checkMandatoryLoop`, so an unbounded cleanup chain ends in a draw rather than hanging. `playGame`'s comment (:1173-1196) keeps its library-and-draw-step argument — that argument is still the reason ordinary games end, and the guard is the backstop for the ones it does not cover — but its "so there is still no progress bound behind it (#338)" must go.

Per `CLAUDE.md`, a comment citing an issue dies in the commit that closes the issue. A comment citing the test that *proves* a behavior is a different genre and outlives it — point at `Pawl.GameSpec`'s "a mandatory loop (CR 104.4b)" group.

- [ ] **Step 2: File the follow-up issue**

```bash
gh issue create \
  --label gap --label expires:card-driven \
  --title "Retire the Synthetic Recursion gate: prove CR 104.4b's draw with Worldgorger Dragon and Animate Dead" \
  --body "..."
```

The body states: CR 104.4b's draw is implemented and proven, but by `data/cards/synthetic-recursion.json` rather than by a printing. The canonical board is Worldgorger Dragon reanimated by Animate Dead. What stands in the way — Animate Dead needs an Aura that enchants a card in a graveyard, an entry that rewrites its own enchant clause and re-attaches, and a leave-the-battlefield sacrifice; Worldgorger needs a mass exile of the other permanents you control and their return on leave. Endless Whispers is the alternative and needs a `Pawl.Types.Modification` arm that grants a triggered ability. Expiry trigger — card-driven: the first pair of real printings that forms a mandatory loop. Note that #484 was closed as a duplicate of #338 and that this is what is left of both.

Then replace `#N1` in `GameSpec.hs` with the number it returns.

- [ ] **Step 3: Final verification**

```
cabal build all
cabal test --test-options '--timeout 1s --hide-successes'
```

Expected: warning-free build, suite green, count 8 higher than the branch point. Record the before → after counts for the PR body.

- [ ] **Step 4: Self-review the branch**

Per `CLAUDE.md`, before opening the PR: re-check every CR citation added or touched against `docs/rules.txt` (104.3a, 104.4b, 110.5b, 400.7e, 603.6a, 605.1a, 613.7a, 613.7d, 704.5g, 729.2), and re-read every comment the change touched for prose the rewrite made wrong. Scale the effort to the diff — the comment rewrites in Task 4 and the `choose` doc comment are where defects hide here. Fix findings on the branch.

- [ ] **Step 5: Commit and open the PR**

```bash
git add -- source/libraries/engine/Pawl/Engine/Engine.hs source/test-suite/Pawl/GameSpec.hs
hooky fix
git add -- source/libraries/engine/Pawl/Engine/Engine.hs source/test-suite/Pawl/GameSpec.hs
hooky run
git commit -m "Retire the comments that said pawl cannot detect a loop (#338)"
git push -u origin 338-loop-of-mandatory-actions-draws
gh pr create --draft --title "Draw a game stuck in a loop of mandatory actions" --body "..."
```

Stage explicit paths, never `git add -A`: other sessions share this checkout.

The PR body carries the case for merging — what changed and why with `Closes #338` as plain text (backticks break the link); the CR citations, each checked against `rules.txt`; the design calls and the alternatives rejected (a configurable limit, repeated-state detection, checking inside `freshTimestamp`, and why the literal "any prompt" heuristic could never fire); how it was verified (warning-free build, `hooky run` clean, suite count before → after, and that the discriminating test was shown to discriminate in Task 3 Step 4); an explicit "no" on whether the rules core now cases on an effect's identity; and what was deferred (the synthetic, with the new issue number).

- [ ] **Step 6: Mark it ready for review, report, and stop**

```bash
gh pr ready
```

Do not wait for CI. Do not start another unit.

---

## Self-Review

**Spec coverage.** `lastChoice` → Task 1 Step 3. Subgame seams → Task 1 Step 4. `choose` and its three exclusions → Task 1 Steps 5-7. `mandatoryLoopLimit`, `checkMandatoryLoop`, both loop heads → Task 2. The synthetic and the two gameplay tests → Task 3. The rejected alternatives and the out-of-scope note surface in the PR body → Task 4 Step 5. The comment cleanup and the follow-up issue → Task 4 Steps 1-2.

**One deviation from the spec, deliberate:** the spec named only Concede, a Pass-only `ChooseAction` and `RandomFirstPlayer` as exclusions. `Prompt.Shuffle` is added to that list, for the same reason `RandomFirstPlayer` is on it — it asks for randomness, not for a choice, and a loop that reshuffles every cycle is still a loop of mandatory actions.

**Type consistency.** `lastChoice`, `choose`, `mandatoryLoopLimit`, `checkMandatoryLoop`, `loopBoard`, `passOnlyBoard`, `atGap` and `runConcedingAt` are spelled the same everywhere they appear. `Timestamp.unwrap` yields `Natural`, which is what `mandatoryLoopLimit` is and what `GameState.nextTimestamp`'s payload is.
