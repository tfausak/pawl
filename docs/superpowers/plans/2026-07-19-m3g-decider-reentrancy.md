# M3g The Payoff Pair (Decider + Re-entrancy) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make Mindslaver control another player (CR 723 — a turn-scheduled control store that `deciderFor` reads, routing the controlled player's decisions to the controller while every resource stays the controlled player's) and Panglacial Wurm castable from the library mid-search (re-entrancy — `Cast.castSpell` invoked while another ability is resolving), the two seams the day-one substrate was built for.

**Architecture:** Two independent phases. **Phase 1 (Mindslaver)** adds a `PlayerTarget` spec, a `ControlPlayerNextTurn` opcode that installs `GameState.pendingControl` (a `Map PlayerId Decider`), promotion to `GameState.activeControl` in `Engine.handoffTurn` (auto-expiring next turn), `Decide.deciderFor` reading that store, and the mana half of activation costs (`AbilityCost.mana`, forced by Mindslaver's `{4}`). **Phase 2 (Panglacial)** adds a `CastingPermission` classification on `Card`, a `CastWhileSearching` prompt loop, and — because `Cast`/`Mana` import `Resolve` (so `Resolve` cannot call them) — the re-entrant cast is orchestrated in `Stack.resolveTop`, which offers the cast before resolving any ability whose effects search a library (gated by a `Resolve.searchesLibrary` classifier).

**Tech Stack:** Haskell 2010 (GHC 9.14.1), `tasty` (`tasty-hunit` + `tasty-quickcheck`), Cabal. Boot libraries only.

## Global Constraints

Copied from the spec and `CLAUDE.md`; every task implicitly includes these:

- **Haskell 2010, no language extensions** beyond `GADTs`, `RankNTypes`, `NamedFieldPuns`. No `LambdaCase`, no `OverloadedStrings`, **no list comprehensions**.
- **No explicit export lists** (`module Pawl.Foo where`).
- **One type per module** under `Pawl.Type.<TypeName>` (type + instances only); logic in other `Pawl.*` modules. A module never imports its parents. Add a new `Pawl.Type.*` file and run `cabal-gild` (via `hooky fix`) — the `exposed-modules` field is `discover`-generated. A new `Pawl.*Spec` goes in the test-suite `other-modules` list.
- **Qualified imports, aliased to the last component** (`Data.List` → `List`); operators unqualified; one import group. `A.B.C` must not import `A.B` or `A`.
- **No partial functions** — `Maybe`/`Either`, never `head`/`error`/non-exhaustive matches. In tests, an empty-list case uses `HU.assertFailure`, never `head`/`error`.
- **`newtype`/record + `Mk`-prefixed, non-punning constructors**; build records with `do`/record syntax. Sum-type data constructors (like `PlayerTarget`, `ControlPlayerNextTurn`, `CastFromLibraryWhileSearching`, `Artifact`) take no `Mk` prefix.
- **Prefer explicit:** `case` over point-free; `let` over `where`; `$` over parens, `.` over chained `$`; `Text` not `String`; arbitrary-precision numbers.
- **No boolean blindness** (`CastingPermission` is a sum type, never a `Bool` field on `Card`); **derive at least `Eq` and `Show`** (and `Ord` on anything a `Card`/`Action`/`Source` transitively contains — those derive `Ord`).
- **Warning-clean** under `-Weverything` minus the allow-list; the `pedantic` flag makes any warning a build failure (including `-Wincomplete-patterns` on a match missing a new constructor — notably `Card.isPermanentType` on the new `CardType.Artifact`, and every answerer/`Replay` function on the new `Prompt.CastWhileSearching` — and `-Wmissing-fields` on a record missing a new field — every `MkCard` for `castingPermissions`, every `MkAbilityCost` for `mana`, every full `MkGameState` for `pendingControl`/`activeControl`). Build `all`: `cabal build all --enable-tests --enable-benchmarks`. When in doubt, `cabal clean` first — incremental builds hide warnings.
- **The two invariants:** the rules core never `case`s on an effect's or a card's *identity*. `Pawl.Resolve` is the **sole** `case`-on-`Effect` home (`slotsOf`, `manaProduced`, `rewriteEffect`, `matchesCriterion`, `searchesLibrary`, `applyEffect` — all grow a `ControlPlayerNextTurn` and/or `searchesLibrary` arm this milestone); `Pawl.Event` the sole `case`-on-`ReplacementEffect`/`TriggerCondition` home; **`Pawl.Cast` the sole reader of `CastingPermission`** (a membership test, `permitsCastWhileSearching`). `Stack.resolveTop` dispatches on the `Source` classification and asks `Resolve.searchesLibrary`, never a card's identity. `Decide.deciderFor` reads a control store keyed by player, never a card. The engine makes no player choice except where the rules leave one, eliding only indistinguishable options (with a documented expiry).
- **Every rules claim cited** against `docs/rules.txt` in a code comment. Never trust recalled Magic rules. (Numbers verified for this plan: control applies to the next turn actually taken and lasts the whole turn, ending at the next turn's start CR 723.1; multiple controls overwrite, last wins CR 723.1a; skipped turns wait CR 723.1b; only control of the player changes, objects keep their controllers CR 723.3; the controller uses only the controlled player's resources CR 723.5a; a mana ability may be activated mid-resolution CR 605.3a; a player casts a spell only if a rule/effect allows it CR 601.3; new object on zone change CR 400.7; an ability ceases CR 608.2n; the effect source is the source permanent CR 608.2g; target player is CR 115; the legend rule CR 704.5j is ELIDED — Mindslaver is singleton and sacrificed as a cost — and must land suppressible, Mirror Gallery; CR 723.4 information visibility is a PlayerView concern, deferred.)
- **After each task:** `cabal build all --enable-tests --enable-benchmarks` warning-free, `cabal test`, `git add -A` (stage explicit paths under a shared worktree) then `hooky fix` && `git add -A` && `hooky run`, HLint clean. Commit directly to `main`, one small complete commit per task.

**Commit message footer** (every commit):

```
Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
```

**Cards (Scryfall-verified 2026-07-19):**
- **Mindslaver** — `{6}` Legendary Artifact — "{4}, {T}, Sacrifice Mindslaver: You control target player during that player's next turn. (You see all cards that player could see and make all decisions for them.)" The parenthetical's "see all cards" is CR 723.4 (deferred — no PlayerView yet); "make all decisions" is the `deciderFor` routing.
- **Panglacial Wurm** — `{5}{G}{G}` Creature — Wurm, 9/5 — "Trample / While you're searching your library, you may cast this card from your library." Trample is M2c; the 9/5 body needs no opcode; only the casting permission is new.

**Module-graph fact (why Phase 2 orchestrates in `Stack`, not `Resolve`):** `Pawl.Cast` imports `Pawl.Resolve`, and `Pawl.Mana` imports `Pawl.Resolve` (for `manaProduced`). So `Resolve` sits *below* both `Cast` and `Mana` and cannot call them — the re-entrant cast (which needs `Cast.castSpell` and mana payment) cannot live inside `Resolve.applyEffect`. `Pawl.Stack` imports `Resolve` and can also import `Cast` (nothing imports `Stack` except `Engine`), so it is the lowest layer that can offer the cast. `Stack.resolveTop` keeps the resolving object on the stack until the end (only `resolveSpell`/`cease` remove it), so a Panglacial cast during that window lands *on top* of the still-resolving ability — exactly the ruling's sequence.

---

## Phase 1 — Mindslaver and the Decider (Tasks 1–5)

### Task 1: `CardType.Artifact` + `TargetSpec.PlayerTarget` + the player-target arm

**Files:**
- Modify: `source/library/Pawl/Type/CardType.hs` (add `Artifact`)
- Modify: `source/library/Pawl/Card.hs` (`isPermanentType` gains the `Artifact` arm)
- Modify: `source/library/Pawl/Type/TargetSpec.hs` (add `PlayerTarget`)
- Modify: `source/library/Pawl/Target.hs` (`legalRecipients` gains the `PlayerTarget` arm)
- Test: `source/test-suite/Pawl/TargetSpec.hs`

**Interfaces:**
- Produces: `CardType.Artifact`; `TargetSpec.PlayerTarget`; `Target.legalRecipients TargetSpec.PlayerTarget gs = Set.fromList (map Recipient.ToPlayer (Sba.stillPlaying gs))`.

- [x] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/TargetSpec.hs` (in its test group; add imports `qualified Pawl.Type.TargetSpec as TargetSpec`, `qualified Pawl.Type.Recipient as Recipient`, `qualified Pawl.Setup as Setup`, `qualified Pawl.Support as S`, `qualified Pawl.Target as Target`, `qualified Data.Set as Set` if absent — mirror the file's existing style):

```haskell
      HU.testCase "CR 115: PlayerTarget is exactly the players still in the game" $
        let gs = Setup.emptyGame S.bothPlayers
            expected = Set.fromList [Recipient.ToPlayer S.alice, Recipient.ToPlayer S.bob]
         in HU.assertEqual "both players, no creatures" expected (Target.legalRecipients TargetSpec.PlayerTarget gs),
```

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "PlayerTarget is exactly"'`
Expected: FAIL to compile — `TargetSpec.PlayerTarget` not in scope.

- [x] **Step 3: Add `Artifact` to `CardType` and its permanent arm**

In `source/library/Pawl/Type/CardType.hs`, add the constructor (after `Enchantment`):

```haskell
  | -- CR 301: an artifact, a permanent type. Mindslaver is the first (M3g).
    Artifact
```

In `source/library/Pawl/Card.hs`, `isPermanentType` gains the arm (CR 301.1 — an artifact is a permanent):

```haskell
  CardType.Artifact -> True
```

- [x] **Step 4: Add `PlayerTarget` to `TargetSpec`**

In `source/library/Pawl/Type/TargetSpec.hs`, add the constructor (after `LandTarget`):

```haskell
  | -- CR 115: "target player" -- a player still in the game. The players-only
    -- restriction AnyTarget does not express (Mindslaver, M3g).
    PlayerTarget
```

- [x] **Step 5: Add the `PlayerTarget` arm to `legalRecipients`**

In `source/library/Pawl/Target.hs`, the `case spec of` gains (the `players` binding is already in scope, computed for `AnyTarget`):

```haskell
        TargetSpec.PlayerTarget -> Set.fromList players
```

- [x] **Step 6: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS.

- [x] **Step 7: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Add CardType.Artifact + TargetSpec.PlayerTarget (CR 115/301)"
```

---

### Task 2: Activation-cost mana — `AbilityCost.mana` (`{4}` for Mindslaver)

**Files:**
- Modify: `source/library/Pawl/Type/AbilityCost.hs` (`newtype` → `data`, add `mana`)
- Modify: `source/library/Pawl/Card.hs` (seed `mana = Nothing` at Llanowar Elves, Evolving Wilds, Prodigal Sorcerer)
- Modify: `source/library/Pawl/Activate.hs` (`activatable` checks mana; `activateAbility` pays it)
- Modify: any test hand-building an `AbilityCost.MkAbilityCost` (build lists them via `-Wmissing-fields`)
- Test: `source/test-suite/Pawl/ActivateSpec.hs`

**Interfaces:**
- Consumes: `Mana.canPay`, `Mana.payCost` (M3e, in `Pawl.Mana`).
- Produces: `AbilityCost.MkAbilityCost { mana :: Maybe ManaCost, additional :: [AdditionalCost] }`; `Activate.activatable` now also requires the mana payable; `Activate.activateAbility` pays the mana (CR 602.1b) before the additional costs.

- [x] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/ActivateSpec.hs` (add imports as the compiler flags — `qualified Pawl.Type.AbilityCost as AbilityCost`, `qualified Pawl.Type.ManaCost as ManaCost`, `qualified Pawl.Type.ManaSymbol as ManaSymbol`, `qualified Pawl.Activate as Activate`, `qualified Pawl.Support as S`, `qualified Data.Map.Strict as Map`). This asserts an ability with a `{2}` mana cost is not activatable with only one Mountain:

```haskell
      HU.testCase "CR 602.1b: an activation with a mana cost needs the mana" $
        let gs = S.mountainsInPlay 1
            (srcId, gs1) = S.addCreature Card.pikerPrinting S.alice gs
            costlyAbility =
              ActivatedAbility.MkActivatedAbility
                { ActivatedAbility.cost =
                    AbilityCost.MkAbilityCost
                      { AbilityCost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 2]),
                        AbilityCost.additional = []
                      },
                  ActivatedAbility.effects = [],
                  ActivatedAbility.targetSpecs = Map.empty
                }
         in HU.assertBool "one Mountain cannot pay {2}" (not (Activate.activatable S.alice srcId costlyAbility gs1)),
```

(Add imports `qualified Pawl.Type.ActivatedAbility as ActivatedAbility`, `qualified Pawl.Card as Card` if absent. `S.mountainsInPlay 1` gives alice one untapped Mountain; `S.addCreature` adds the Piker as a settled source.)

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "needs the mana"'`
Expected: FAIL to compile — `AbilityCost.mana` not a field (still a `newtype` with only `additional`).

- [x] **Step 3: Promote `AbilityCost` to `data` with a `mana` field**

Replace `source/library/Pawl/Type/AbilityCost.hs`:

```haskell
module Pawl.Type.AbilityCost where

import Pawl.Type.AdditionalCost (AdditionalCost)
import Pawl.Type.ManaCost (ManaCost)

-- The cost of an activated ability (CR 602.1). A mana part (CR 602.1b) plus the
-- non-mana additional costs. Nothing = no mana symbol in the cost (every M3e
-- ability); Mindslaver's {4} is the first Just.
data AbilityCost = MkAbilityCost
  { mana :: Maybe ManaCost,
    additional :: [AdditionalCost]
  }
  deriving (Eq, Ord, Show)
```

- [x] **Step 4: Seed `mana = Nothing` at every existing `MkAbilityCost`**

`-Wmissing-fields` lists them. In `source/library/Pawl/Card.hs`, at Llanowar Elves (`llanowarElvesPrinting`), Evolving Wilds (`evolvingWildsPrinting`), and Prodigal Sorcerer (`prodigalSorcererPrinting`), change each `AbilityCost.MkAbilityCost {AbilityCost.additional = [...]}` to `AbilityCost.MkAbilityCost {AbilityCost.mana = Nothing, AbilityCost.additional = [...]}`. Fix any test that hand-builds an `MkAbilityCost` the same way (the build names them).

- [x] **Step 5: Check + pay the mana in `Activate`**

In `source/library/Pawl/Activate.hs`, extend `activatable` (add the mana conjunct after the `tapSicknessOk` line):

```haskell
    && maybe True (\c -> Mana.canPay pid c gs) (AbilityCost.mana (ActivatedAbility.cost ability))
```

In `activateAbility`, replace the final `else` branch's cost payment (currently a single `State.modify'` folding `payAdditional`) with mana-then-additional payment:

```haskell
    else do
      State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.targets = chosen}) abilId (GameState.objects g)})
      let additional = AbilityCost.additional (ActivatedAbility.cost ability)
          payAll g = List.foldl' (payAdditional srcId) g additional
      case AbilityCost.mana (ActivatedAbility.cost ability) of
        Nothing -> State.modify' payAll
        Just cost -> do
          g1 <- State.get
          case Mana.payCost pid cost g1 of
            -- activatable pre-checks canPay, so within the source elision this is
            -- unreachable; reject-not-repair if a distinguishable source ever makes
            -- payment fail (git-bug 65ce714).
            Nothing -> State.put gs
            Just paid -> State.put (payAll paid)
```

(`Mana`, `List`, `AbilityCost`, `Map` are already imported in `Activate`.)

- [x] **Step 6: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — the M3e activation tests (Llanowar, Prodigal, Evolving Wilds, all `mana = Nothing`) stay green.

- [x] **Step 7: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Add the mana half of activation costs: AbilityCost.mana (CR 602.1b)"
```

---

### Task 3: The control store — `pendingControl` / `activeControl`, `deciderFor`, and turn-start promotion

**Files:**
- Modify: `source/library/Pawl/Type/GameState.hs` (add `pendingControl`, `activeControl`)
- Modify: `source/library/Pawl/Setup.hs` (`emptyGame` seeds both) and `source/test-suite/Pawl/Support.hs` (`oneMountainState` seeds both)
- Modify: `source/library/Pawl/Decide.hs` (`deciderFor` reads `activeControl`)
- Modify: `source/library/Pawl/Engine.hs` (`handoffTurn` promotes)
- Test: `source/test-suite/Pawl/DecideSpec.hs` (new — wire into `Main.hs` and the test-suite `other-modules`)

**Interfaces:**
- Produces: `GameState.pendingControl :: Map PlayerId Decider`; `GameState.activeControl :: Maybe Decider`; `Decide.deciderFor pid gs` returns the active-control decider when `pid` is the active player and `activeControl` is set, else `MkDecider pid`; `Engine.handoffTurn` sets `activeControl = Map.lookup newActive pendingControl` and deletes that pending entry.

- [x] **Step 1: Write the failing test**

Create `source/test-suite/Pawl/DecideSpec.hs`:

```haskell
module Pawl.DecideSpec where

import qualified Data.Map.Strict as Map
import qualified Pawl.Decide as Decide
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.Decider as Decider
import qualified Pawl.Type.GameState as GameState
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Pawl.Decide"
    [ HU.testCase "CR 722: with no control, a player decides for themselves" $
        let gs = Setup.emptyGame S.bothPlayers
         in HU.assertEqual "alice decides for alice" (Decider.MkDecider S.alice) (Decide.deciderFor S.alice gs),
      HU.testCase "CR 723.3: an active controlled player's decisions route to the controller" $
        let gs = (Setup.emptyGame S.bothPlayers) {GameState.activePlayer = S.bob, GameState.activeControl = Just (Decider.MkDecider S.alice)}
         in do
              HU.assertEqual "bob's decisions route to alice" (Decider.MkDecider S.alice) (Decide.deciderFor S.bob gs)
              HU.assertEqual "alice still decides for herself" (Decider.MkDecider S.alice) (Decide.deciderFor S.alice gs),
      HU.testCase "control applies only to the active player" $
        let gs = (Setup.emptyGame S.bothPlayers) {GameState.activePlayer = S.alice, GameState.activeControl = Just (Decider.MkDecider S.alice), GameState.pendingControl = Map.empty}
         in HU.assertEqual "bob is not active, so unaffected" (Decider.MkDecider S.bob) (Decide.deciderFor S.bob gs)
    ]
```

Wire `Pawl.DecideSpec.tests` into `source/test-suite/Main.hs`'s `testTree` and add `Pawl.DecideSpec` to the test-suite `other-modules` list in `pawl.cabal`.

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "route to the controller"'`
Expected: FAIL to compile — `GameState.activeControl`, `GameState.pendingControl` not fields.

- [x] **Step 3: Add the two fields to `GameState`**

In `source/library/Pawl/Type/GameState.hs`, add `import Pawl.Type.Decider (Decider)` and the fields (after `landPlayed`):

```haskell
    -- CR 723.1: pending player-controlling effects, keyed by the player to be
    -- controlled. Map.insert overwrites (CR 723.1a, last created wins). Promoted
    -- to activeControl at the actual start of that player's turn (CR 723.1b).
    pendingControl :: Map PlayerId Decider,
    -- CR 723.1/723.3: the decider controlling the ACTIVE player this turn, if any.
    -- Only the active player is ever controlled during their turn, so one Maybe
    -- suffices. Overwritten every turn start, so control ends at the next turn's
    -- beginning (CR 723.1).
    activeControl :: Maybe Decider
```

(`Map` and `PlayerId` are already imported in `GameState`.)

- [x] **Step 4: Seed both fields at every full `MkGameState`**

`-Wmissing-fields` lists them. In `source/library/Pawl/Setup.hs` (`emptyGame`) and `source/test-suite/Pawl/Support.hs` (`oneMountainState`), add next to `landPlayed`:

```haskell
          GameState.pendingControl = Map.empty,
          GameState.activeControl = Nothing,
```

- [x] **Step 5: Make `deciderFor` read `activeControl`**

Replace `source/library/Pawl/Decide.hs`:

```haskell
module Pawl.Decide where

import Pawl.Type.Decider (Decider)
import qualified Pawl.Type.Decider as Decider
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import Pawl.Type.PlayerId (PlayerId)

-- Who actually decides for a player (CR 722/723). A player controlled during
-- their turn (CR 723.1) has their decisions made by the controller; everyone
-- else decides for themselves. The active-player guard is what makes a single
-- activeControl Maybe correct: only the active player is ever controlled during
-- their controlled turn (CR 723.3).
deciderFor :: PlayerId -> GameState -> Decider
deciderFor pid gs = case GameState.activeControl gs of
  Just decider | pid == GameState.activePlayer gs -> decider
  _ -> Decider.MkDecider pid
```

- [x] **Step 6: Promote pending control at turn start in `handoffTurn`**

In `source/library/Pawl/Engine.hs`, replace `handoffTurn`:

```haskell
handoffTurn :: Game ()
handoffTurn = State.modify' $ \gs ->
  let newActive = nextInOrder (GameState.turnOrder gs) (GameState.activePlayer gs)
   in gs
        { GameState.activePlayer = newActive,
          GameState.turnNumber = GameState.turnNumber gs + 1,
          GameState.phase = Turn.firstPhase,
          GameState.remaining = Turn.laterPhases,
          -- CR 723.1/723.1b: the new active player's pending control (if any)
          -- becomes this turn's active control; overwriting activeControl every
          -- turn is what ends a prior control at the next turn's start (CR 723.1).
          GameState.activeControl = Map.lookup newActive (GameState.pendingControl gs),
          GameState.pendingControl = Map.delete newActive (GameState.pendingControl gs)
        }
```

(`Map` is already imported in `Engine`.)

- [x] **Step 7: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS. The whole suite stays green — `activeControl` is `Nothing` everywhere, so `deciderFor` is unchanged in behavior.

- [x] **Step 8: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Add the turn-scheduled control store: pendingControl/activeControl + deciderFor (CR 723.1)"
```

---

### Task 4: `ControlPlayerNextTurn` opcode + Mindslaver; resolve through the stack; promote

**Files:**
- Modify: `source/library/Pawl/Type/Effect.hs` (add `ControlPlayerNextTurn`)
- Modify: `source/library/Pawl/Resolve.hs` (`slotsOf`/`manaProduced`/`rewriteEffect`/`applyEffect` arms)
- Modify: `source/library/Pawl/Card.hs` (add `mindslaverPrinting`; register in `allPrintings`)
- Test: `source/test-suite/Pawl/ResolveSpec.hs`

**Interfaces:**
- Consumes: `resolveEffects`/`resolveAbility`/`OfAbility` (M3e/M3f), `PlayerTarget` (Task 1), `AbilityCost.mana` (Task 2), `pendingControl` (Task 3).
- Produces: `Effect.ControlPlayerNextTurn SlotName`; `applyEffect … (Effect.ControlPlayerNextTurn slot)` installs `pendingControl[target] = MkDecider controller` when the slot holds a legal `ToPlayer target`; `Card.mindslaverPrinting`. Mindslaver's ability: `cost = MkAbilityCost (Just {4}) [TapSelf, SacrificeSelf]`, `effects = [ControlPlayerNextTurn "target"]`, `targetSpecs = { "target" ↦ PlayerTarget }`.

- [x] **Step 1: Write the failing test**

Add to the `Resolve` group in `source/test-suite/Pawl/ResolveSpec.hs`. Hand-build Mindslaver's ability on the stack (targeting bob), resolve it, assert `pendingControl`, then `handoffTurn` and assert `activeControl` + `deciderFor` + expiry (add imports as the compiler flags — `qualified Pawl.Type.Effect as Effect`, `qualified Pawl.Type.ActivatedAbility as ActivatedAbility`, `qualified Pawl.Type.AbilityCost as AbilityCost`, `qualified Pawl.Type.AdditionalCost as AdditionalCost`, `qualified Pawl.Type.ManaCost as ManaCost`, `qualified Pawl.Type.ManaSymbol as ManaSymbol`, `qualified Pawl.Type.SlotName as SlotName`, `qualified Pawl.Type.Recipient as Recipient`, `qualified Pawl.Type.Source as Source`, `qualified Pawl.Type.Decider as Decider`, `qualified Pawl.Type.Object as Object`, `qualified Pawl.Type.TapState as TapState`, `qualified Pawl.Type.Sickness as Sickness`, `qualified Pawl.Type.Zone as Zone`, `qualified Pawl.Type.TargetSpec as TargetSpec`, `qualified Pawl.Stack as Stack`, `qualified Pawl.Engine as Engine`, `qualified Pawl.Decide as Decide`, `qualified Data.Text as Text`):

```haskell
      HU.testCase "CR 723.1: Mindslaver's ability installs pending control, promoted next turn" $
        let g0 = Setup.emptyGame S.bothPlayers
            (srcId, g1) = S.addCreature Card.mindslaverPrinting S.alice g0
            slot = SlotName.MkSlotName (Text.pack "target")
            ability =
              ActivatedAbility.MkActivatedAbility
                { ActivatedAbility.cost =
                    AbilityCost.MkAbilityCost
                      { AbilityCost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 4]),
                        AbilityCost.additional = [AdditionalCost.TapSelf, AdditionalCost.SacrificeSelf]
                      },
                  ActivatedAbility.effects = [Effect.ControlPlayerNextTurn slot],
                  ActivatedAbility.targetSpecs = Map.singleton slot TargetSpec.PlayerTarget
                }
            (abilId, g2) = Game.freshObjectId g1
            (ts, g3) = Game.freshTimestamp g2
            abilObj =
              Object.MkObject
                { Object.owner = S.alice,
                  Object.source = Source.OfAbility srcId ability,
                  Object.zone = Zone.Stack,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled,
                  Object.targets = Map.singleton slot (Recipient.ToPlayer S.bob),
                  Object.chosenSubtypes = Map.empty,
                  Object.timestamp = ts
                }
            g4 = g3 {GameState.objects = Map.insert abilId abilObj (GameState.objects g3), GameState.stack = abilId : GameState.stack g3}
            resolved = snd (Engine.runGamePure S.identityAnswer g4 Stack.resolveTop)
            bobsTurn = snd (Engine.runGamePure S.identityAnswer resolved Engine.handoffTurn)
            afterBob = snd (Engine.runGamePure S.identityAnswer bobsTurn Engine.handoffTurn)
         in do
              HU.assertEqual "control pending for bob" (Just (Decider.MkDecider S.alice)) (Map.lookup S.bob (GameState.pendingControl resolved))
              HU.assertEqual "promoted on bob's turn" (Just (Decider.MkDecider S.alice)) (GameState.activeControl bobsTurn)
              HU.assertEqual "bob's decisions route to alice" (Decider.MkDecider S.alice) (Decide.deciderFor S.bob bobsTurn)
              HU.assertEqual "control expired after bob's turn" (Decider.MkDecider S.bob) (Decide.deciderFor S.bob afterBob)
    ,
```

(`addCreature` puts Mindslaver on the battlefield; the ability is hand-built and resolved directly, skipping activation — the effect reads the *controller* (alice, the ability object's owner), never the source permanent, so the un-sacrificed Mindslaver does not matter here. `TargetSpec` and `Map` are already imported in `ResolveSpec`.)

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "installs pending control"'`
Expected: FAIL to compile — `Effect.ControlPlayerNextTurn`, `Card.mindslaverPrinting` not in scope.

- [x] **Step 3: Add the opcode**

In `source/library/Pawl/Type/Effect.hs`, add the constructor:

```haskell
  | -- CR 723.1: "you control target player during that player's next turn."
    -- Installs pending control keyed to the slot's chosen player, with the
    -- ability's controller as the decider. Mindslaver's exact shape.
    ControlPlayerNextTurn SlotName
```

- [x] **Step 4: Add the four `Resolve` arms**

In `source/library/Pawl/Resolve.hs`, add `import qualified Pawl.Type.Decider as Decider`. Add arms:

`slotsOf`: `Effect.ControlPlayerNextTurn slot -> Set.singleton slot`
`manaProduced`: `Effect.ControlPlayerNextTurn _ -> Nothing`
`rewriteEffect`: `Effect.ControlPlayerNextTurn _ -> effect`
`applyEffect` (install pending control; `controller` is the ability's controller):

```haskell
  Effect.ControlPlayerNextTurn slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just (Recipient.ToPlayer target), True) ->
          -- CR 723.1: schedule control of `target` by this ability's controller
          -- (CR 723.5). Map.insert overwrites a prior pending control (CR 723.1a).
          gs {GameState.pendingControl = Map.insert target (Decider.MkDecider controller) (GameState.pendingControl gs)}
        -- Not a player recipient or an illegal slot (CR 608.2b): no-op.
        _ -> gs
```

(`Recipient`, `Map`, `GameState`, `State` are already imported in `Resolve`.)

- [x] **Step 5: Add Mindslaver and register it**

In `source/library/Pawl/Card.hs`, add (match the file's existing import aliases; `CardType.Artifact`, `Supertype.Legendary`, `TargetSpec.PlayerTarget` are all in scope after Task 1):

```haskell
-- Mindslaver: {6}, Legendary Artifact, "{4}, {T}, Sacrifice Mindslaver: You
-- control target player during that player's next turn." Scryfall-verified
-- 2026-07-19. Legendary is represented but the CR 704.5j legend-rule SBA is
-- ELIDED (M3g keeps it singleton; it is sacrificed as a cost). The parenthetical
-- "see all cards" is CR 723.4 (deferred, no PlayerView).
mindslaverPrinting :: Printing.Printing
mindslaverPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Mindslaver",
            Card.manaCost = Just (ManaCost.MkManaCost [ManaSymbol.Generic 6]),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.singleton Supertype.Legendary,
                  TypeLine.types = Set.singleton CardType.Artifact,
                  TypeLine.subtypes = Set.empty
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty,
            Card.staticAbilities = [],
            Card.effects = [],
            Card.activatedAbilities =
              [ ActivatedAbility.MkActivatedAbility
                  { ActivatedAbility.cost =
                      AbilityCost.MkAbilityCost
                        { AbilityCost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 4]),
                          AbilityCost.additional = [AdditionalCost.TapSelf, AdditionalCost.SacrificeSelf]
                        },
                    ActivatedAbility.effects = [Effect.ControlPlayerNextTurn (SlotName.MkSlotName (Text.pack "target"))],
                    ActivatedAbility.targetSpecs = Map.singleton (SlotName.MkSlotName (Text.pack "target")) TargetSpec.PlayerTarget
                  }
              ],
            Card.replacementEffects = [],
            Card.triggeredAbilities = [],
            Card.targetSpecs = Map.empty
          }
    }
```

Add `mindslaverPrinting` to `allPrintings` (append to the current tail — after `panglacialWurmPrinting` if Task 6 has landed, else after the last existing entry; order within the list does not matter).

- [x] **Step 6: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — the ability resolves, installs pending control, promotes on bob's turn, and expires the turn after.

- [x] **Step 7: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "ControlPlayerNextTurn opcode + Mindslaver: control installs + promotes (CR 723.1)"
```

---

### Task 5: The routing falsifier — the controller decides, the controlled player's resources move

**Files:**
- Test: `source/test-suite/Pawl/EngineSpec.hs` (the priority-loop integration gate)

**Interfaces:**
- Consumes: everything from Tasks 1–4, plus `Engine.priorityLoop`, `Cast`/`Lightning Bolt` (M3a).
- Produces: no library change — a gameplay-level gate proving CR 723.3/723.5/723.5a.

- [x] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/EngineSpec.hs`. It builds bob's main phase under alice's control, with a decider-and-player-branching interpreter that makes bob Lightning Bolt himself only because alice decides for him (add imports as the compiler flags — `qualified Pawl.Type.Decider as Decider`, `qualified Pawl.Type.Recipient as Recipient`, `qualified Pawl.Type.Prompt as Prompt`, `qualified Pawl.Type.Action as A`, `qualified Pawl.Type.Phase as Phase`, `qualified Pawl.Type.Zone as Zone`, `qualified Pawl.Type.GameState as GameState`, `qualified Pawl.Game as Game`, `qualified Pawl.Type.Source as Source`, `qualified Pawl.Type.Printing as Printing`, `qualified Pawl.Type.Card as Card.Type`, `qualified Data.Set as Set`, `qualified Data.Map.Strict as Map`, `qualified Data.Text as Text`):

```haskell
      HU.testCase "CR 723.3/723.5: alice decides for bob, but bob's resources move" $
        -- bob's main phase, controlled by alice, with a Mountain and a Bolt.
        let g0 = Setup.emptyGame S.bothPlayers
            (mtnId, g1) = S.addCreature Card.mountainPrinting S.bob g0
            (boltId, g2) = handBobBolt g1
            g3 =
              g2
                { GameState.activePlayer = S.bob,
                  GameState.phase = Phase.PrecombatMain,
                  GameState.priority = Just S.bob,
                  GameState.activeControl = Just (Decider.MkDecider S.alice)
                }
            after = snd (Engine.runGamePure slaveAnswer g3 Engine.priorityLoop)
            boltInBobGrave =
              length
                (filter
                   (\o -> namedIs (Text.pack "Lightning Bolt") o)
                   (map (\i -> Game.lookupObject i after) (Game.zoneMembers Zone.Graveyard S.bob after)))
         in do
              HU.assertEqual "bob took 3 from his own Bolt" (Just 17) (S.lifeOf S.bob after)
              HU.assertEqual "bob's Bolt is in bob's graveyard" 1 boltInBobGrave
              HU.assertEqual "the Mountain (bob's) is tapped" 1 (S.tappedCount S.bob after)
    ,
```

Add these group-local helpers to `EngineSpec.hs` (a `bob`-in-hand builder — `Support`'s `handOne` hardcodes alice — a name check, and the branching interpreter):

```haskell
-- One Lightning Bolt in bob's hand.
handBobBolt :: GameState.GameState -> (GameState.GameState, ObjectId.ObjectId)
handBobBolt gs =
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = S.bob,
            Object.source = Source.OfCard Card.lightningBoltPrinting,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.targets = Map.empty,
            Object.chosenSubtypes = Map.empty,
            Object.timestamp = ts
          }
   in (gs2 {GameState.objects = Map.insert oid obj (GameState.objects gs2), GameState.hand = Map.insert S.bob (Seq.singleton oid) (GameState.hand gs2)}, oid)

namedIs :: Text.Text -> Maybe Object.Object -> Bool
namedIs wanted mo = case mo of
  Just o -> case Object.source o of
    Source.OfCard printing -> Card.Type.name (Printing.card printing) == wanted
    Source.OfAbility _ _ -> False
    Source.OfTrigger _ _ -> False
  Nothing -> False

-- The controller's strategy: when asked to decide for bob (the CONTROLLED player,
-- routed because the prompt's Decider is alice), cast the Bolt at bob; otherwise
-- pass. A naive engine that ignored control would send the prompt with Decider =
-- bob, this interpreter would pass, and bob's life would stay 20 -- the falsifier.
slaveAnswer :: Prompt.Prompt r -> r
slaveAnswer p = case p of
  Prompt.ChooseAction (Decider.MkDecider d) player actions ->
    if player == S.bob && d == S.alice
      then case filter isCast actions of
        h : _ -> h
        [] -> A.Pass
      else A.Pass
  Prompt.ChooseTargets _ _ _ sets ->
    Map.mapMaybe
      (\s -> if Set.member (Recipient.ToPlayer S.bob) s then Just (Recipient.ToPlayer S.bob) else Set.lookupMin s)
      sets
  Prompt.Shuffle ids -> ids
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter S.isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
  Prompt.SearchLibrary {} -> Nothing

isCast :: A.Action -> Bool
isCast a = case a of
  A.Cast _ -> True
  _ -> False
```

(Add imports `qualified Pawl.Type.Object as Object`, `qualified Pawl.Type.ObjectId as ObjectId`, `qualified Pawl.Type.TapState as TapState`, `qualified Pawl.Type.Sickness as Sickness`, `qualified Pawl.Type.Subtype as Subtype`, `qualified Data.Sequence as Seq` if absent. `slaveAnswer` covers exactly the prompts that exist at Task 5; Task 7 adds the `CastWhileSearching` arm to it when that prompt is introduced.)

- [x] **Step 2: Run test to verify it fails, then passes**

If Task 4 is complete this should PASS immediately (the routing already works). Confirm it is a genuine gate by temporarily hardcoding `Decide.deciderFor pid _ = Decider.MkDecider pid` (ignoring control): the test must then FAIL (bob's life stays 20, no Bolt cast). Restore `deciderFor`.

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "alice decides for bob"'`
Expected: PASS (and FAIL under the hardcoded-`deciderFor` falsifier check).

- [x] **Step 3: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Gate: Mindslaver routes bob's decisions to alice while bob's resources move (CR 723.3)"
```

---

## Phase 2 — Panglacial Wurm and re-entrancy (Tasks 6–8)

### Task 6: `CastingPermission` + `Card.castingPermissions` + the enumerator + `searchesLibrary` + Panglacial

**Files:**
- Create: `source/library/Pawl/Type/CastingPermission.hs`
- Modify: `source/library/Pawl/Type/Card.hs` (add `castingPermissions`)
- Modify: `source/library/Pawl/Card.hs` (seed `castingPermissions = []` at every printing; add `panglacialWurmPrinting`; register)
- Modify: `source/library/Pawl/Cast.hs` (`permitsCastWhileSearching`, `castableWhileSearching`)
- Modify: `source/library/Pawl/Resolve.hs` (`searchesLibrary`)
- Modify: any hand-built `Card.Type.MkCard` in tests (build lists them via `-Wmissing-fields`)
- Test: `source/test-suite/Pawl/CastSpec.hs`

**Interfaces:**
- Produces: `CastingPermission.CastFromLibraryWhileSearching`; `Card.castingPermissions :: [CastingPermission]`; `Cast.permitsCastWhileSearching :: Card.Card -> Bool`; `Cast.castableWhileSearching :: PlayerId -> GameState -> [ObjectId]` (library cards with the permission, affordable, and fillable — deliberately NOT timing-gated); `Resolve.searchesLibrary :: Effect -> Bool`; `Card.panglacialWurmPrinting`.

- [x] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/CastSpec.hs` (add imports `qualified Pawl.Cast as Cast`, `qualified Pawl.Support as S` if absent):

```haskell
      HU.testCase "Panglacial Wurm in the library is castable-while-searching with mana" $
        let base = S.landsInPlay Card.forestPrinting 7
            (_, gs) = S.addLibraryCard Card.panglacialWurmPrinting S.alice base
         in HU.assertEqual "one castable-while-searching option" 1 (length (Cast.castableWhileSearching S.alice gs)),
      HU.testCase "with too little mana, Panglacial is not castable-while-searching" $
        let base = S.landsInPlay Card.forestPrinting 3
            (_, gs) = S.addLibraryCard Card.panglacialWurmPrinting S.alice base
         in HU.assertEqual "unaffordable, so no options" 0 (length (Cast.castableWhileSearching S.alice gs)),
```

(`S.landsInPlay Card.forestPrinting 7` gives alice seven untapped Forests; Panglacial costs `{5}{G}{G}` = 7 mana.)

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "castable-while-searching with mana"'`
Expected: FAIL to compile — `Cast.castableWhileSearching`, `Card.panglacialWurmPrinting` not in scope.

- [x] **Step 3: Create `CastingPermission`**

`source/library/Pawl/Type/CastingPermission.hs`:

```haskell
module Pawl.Type.CastingPermission where

-- CR 113.6 / 601.3: a static permission to cast a card from a zone or under a
-- condition it normally could not. Classified by the permission pattern (the M3f
-- TriggerCondition shape). CastFromLibraryWhileSearching = Panglacial Wurm: "while
-- you're searching your library, you may cast this from your library." A general
-- "cast from the top of your library" (Garruk's Horde) is a future permission.
-- Only Pawl.Cast reads it (a membership test, permitsCastWhileSearching).
data CastingPermission = CastFromLibraryWhileSearching
  deriving (Eq, Ord, Show)
```

- [x] **Step 4: Add the `castingPermissions` field to `Card`**

In `source/library/Pawl/Type/Card.hs`, add `import Pawl.Type.CastingPermission (CastingPermission)` and the field (after `triggeredAbilities`, before `targetSpecs`):

```haskell
    -- CR 601.3: this card's casting permissions -- zone/condition exceptions to
    -- normal timing. Read directly from the card (NOT the projection): the
    -- permission functions in the library (CR 113.6), where the CR 613 layer
    -- system does not reach. Empty for all but Panglacial Wurm.
    castingPermissions :: [CastingPermission],
```

- [x] **Step 5: Add `searchesLibrary` to `Resolve`**

In `source/library/Pawl/Resolve.hs`, add (casing on `Effect` is Resolve's charter):

```haskell
-- CR 601.3 (Panglacial): does this effect search a library? The classification
-- Stack asks before resolving, to offer the cast-while-searching opportunity.
-- Search searches the controller's own library; every other effect does not.
searchesLibrary :: Effect -> Bool
searchesLibrary effect = case effect of
  Effect.Search _ -> True
  Effect.DealDamage _ _ -> False
  Effect.ModifyTarget {} -> False
  Effect.ChangeText _ -> False
  Effect.AddMana _ -> False
  Effect.ExileAllGraveyards -> False
  Effect.ControlPlayerNextTurn _ -> False
```

- [x] **Step 6: Add the classifier and enumerator to `Cast`**

In `source/library/Pawl/Cast.hs`, add `import qualified Pawl.Type.CastingPermission as CastingPermission`. Add:

```haskell
-- CR 601.3 (Panglacial): may this card be cast from the library while its
-- controller searches their own library? A membership test on the card's casting
-- permissions -- Cast is the sole reader of CastingPermission, and this is a
-- classification, never card identity.
permitsCastWhileSearching :: Card.Type.Card -> Bool
permitsCastWhileSearching card =
  elem CastingPermission.CastFromLibraryWhileSearching (Card.Type.castingPermissions card)

-- The library cards this player may cast while searching their own library:
-- permitted, affordable (Mana.canPay), and with a fillable target set (Cast
-- .targetable). Deliberately omits timingOk -- the permission IS the CR 601.3
-- timing exception (the ruling: "follows all normal rules ... except for timing").
castableWhileSearching :: PlayerId -> GameState -> [ObjectId]
castableWhileSearching pid gs =
  let permitted oid = case Game.cardOf oid gs of
        Nothing -> False
        Just card -> permitsCastWhileSearching card
      affordable oid = case costOf oid gs of
        Nothing -> False
        Just cost -> Mana.canPay pid cost gs
   in filter (\oid -> permitted oid && affordable oid && targetable oid gs) (Game.zoneMembers Zone.Library pid gs)
```

(`Card.Type`, `Mana`, `Game`, `Zone`, `costOf`, `targetable`, `PlayerId`, `GameState`, `ObjectId` are already in scope in `Cast`.)

- [x] **Step 7: Seed `castingPermissions = []` everywhere, then add Panglacial**

`-Wmissing-fields` lists every printing. Add `Card.castingPermissions = []` (after `triggeredAbilities`) at every printing in `source/library/Pawl/Card.hs` and every hand-built `Card.Type.MkCard` in tests. Then add:

```haskell
-- Panglacial Wurm: {5}{G}{G}, Creature - Wurm, 9/5, "Trample / While you're
-- searching your library, you may cast this card from your library."
-- Scryfall-verified 2026-07-19. Trample is M2c; the body needs no opcode. The
-- permission is read from the card in the library (not projected).
panglacialWurmPrinting :: Printing.Printing
panglacialWurmPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Panglacial Wurm",
            Card.manaCost =
              Just (ManaCost.MkManaCost [ManaSymbol.Generic 5, ManaSymbol.OfType (ManaType.Colored Color.Green), ManaSymbol.OfType (ManaType.Colored Color.Green)]),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.empty
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 9)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 5)),
            Card.keywords = Set.singleton Keyword.Trample,
            Card.staticAbilities = [],
            Card.effects = [],
            Card.activatedAbilities = [],
            Card.replacementEffects = [],
            Card.triggeredAbilities = [],
            Card.castingPermissions = [CastingPermission.CastFromLibraryWhileSearching],
            Card.targetSpecs = Map.empty
          }
    }
```

(Add `import qualified Pawl.Type.CastingPermission as CastingPermission` to `Card.hs`. Match existing aliases: `Power`, `Toughness`, `Quantity`, `Keyword`, `ManaType`, `Color`, `ManaSymbol`, `ManaCost`. Register `panglacialWurmPrinting` in `allPrintings`.)

- [x] **Step 8: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS.

- [x] **Step 9: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "CastingPermission + castableWhileSearching + searchesLibrary + Panglacial Wurm (CR 601.3)"
```

---

### Task 7: The `CastWhileSearching` prompt, its serialization, and the cast loop

**Files:**
- Modify: `source/library/Pawl/Type/Prompt.hs` (add `CastWhileSearching`)
- Modify: `source/library/Pawl/Type/Response.hs` (add `CastWhileSearched`)
- Modify: `source/library/Pawl/Replay.hs` (`encode`/`decode`/`defaultAnswer` arms)
- Modify: `source/test-suite/Pawl/Support.hs` (the five answerers gain the arm)
- Modify: `source/library/Pawl/Cast.hs` (`castWhileSearching` loop)
- Test: `source/test-suite/Pawl/CastSpec.hs`

**Interfaces:**
- Produces: `Prompt.CastWhileSearching :: Decider -> PlayerId -> [ObjectId] -> Prompt (Maybe ObjectId)`; `Response.CastWhileSearched (Maybe ObjectId)`; `Cast.castWhileSearching :: PlayerId -> Game ()` (loops: offer the castable-while-searching options, cast the chosen card via `castSpell`, repeat until declined or none remain).

- [x] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/CastSpec.hs`. Drive the loop directly with a scripted interpreter that casts the one option (add imports `qualified Pawl.Engine as Engine`, `qualified Pawl.Type.Prompt as Prompt`, `qualified Data.Text as Text` if absent):

```haskell
      HU.testCase "castWhileSearching casts Panglacial from the library onto the stack" $
        let base = S.landsInPlay Card.forestPrinting 7
            (_, gs) = S.addLibraryCard Card.panglacialWurmPrinting S.alice base
            after = snd (Engine.runGamePure castFirstOption gs (Cast.castWhileSearching S.alice))
            onStack = length (filter (nameOnStack (Text.pack "Panglacial Wurm") after) (GameState.stack after))
         in do
              HU.assertEqual "Panglacial is on the stack" 1 onStack
              HU.assertEqual "Panglacial left the library" 0 (S.countByName (Text.pack "Panglacial Wurm") S.alice after)
              HU.assertEqual "seven Forests tapped to pay {5}{G}{G}" 7 (S.tappedCount S.alice after)
    ,
```

Add group-local helpers to `CastSpec.hs`:

```haskell
-- Casts the first offered option, then declines (the loop re-offers until empty).
castFirstOption :: Prompt.Prompt r -> r
castFirstOption p = case p of
  Prompt.CastWhileSearching _ _ options -> case options of
    oid : _ -> Just oid
    [] -> Nothing
  Prompt.Shuffle ids -> ids
  Prompt.ChooseTargets _ _ _ sets -> Map.mapMaybe Set.lookupMin sets
  Prompt.ChooseAction {} -> A.Pass
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter S.isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
  Prompt.SearchLibrary {} -> Nothing

nameOnStack :: Text.Text -> GameState.GameState -> ObjectId.ObjectId -> Bool
nameOnStack wanted gs oid = case Game.lookupObject oid gs of
  Just o -> case Object.source o of
    Source.OfCard printing -> Card.Type.name (Printing.card printing) == wanted
    Source.OfAbility _ _ -> False
    Source.OfTrigger _ _ -> False
  Nothing -> False
```

(Add imports as the compiler flags — `qualified Pawl.Type.Action as A`, `qualified Pawl.Type.Subtype as Subtype`, `qualified Pawl.Type.GameState as GameState`, `qualified Pawl.Type.Object as Object`, `qualified Pawl.Type.ObjectId as ObjectId`, `qualified Pawl.Type.Source as Source`, `qualified Pawl.Type.Printing as Printing`, `qualified Pawl.Type.Card as Card.Type`, `qualified Pawl.Game as Game`, `qualified Data.Set as Set`, `qualified Data.Map.Strict as Map`.)

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "casts Panglacial from the library onto the stack"'`
Expected: FAIL to compile — `Prompt.CastWhileSearching`, `Cast.castWhileSearching` not in scope.

- [x] **Step 3: Add the prompt**

In `source/library/Pawl/Type/Prompt.hs`, add the constructor to the `Prompt` GADT:

```haskell
  -- The re-entrant cast opportunity during a library search (Panglacial Wurm).
  -- The [ObjectId] is the searcher's library cards castable-while-searching (the
  -- engine pre-filters to permitted, affordable, fillable). Nothing = decline /
  -- done. Offered in a loop before the search finds (per the ruling), so multiple
  -- copies may be cast. CR 605.3a permits mana activation to pay.
  CastWhileSearching :: Decider -> PlayerId -> [ObjectId] -> Prompt (Maybe ObjectId)
```

- [x] **Step 4: Add the response and its serialization**

In `source/library/Pawl/Type/Response.hs`, add the constructor:

```haskell
  | -- CR 601.3 (Panglacial): the library card cast while searching (Nothing =
    -- declined), serialized so a DecisionLog replays the re-entrant cast.
    CastWhileSearched (Maybe ObjectId)
```

In `source/library/Pawl/Replay.hs`, add arms:

`encode`: `Prompt.CastWhileSearching {} -> Response.CastWhileSearched answer`
`decode`:
```haskell
  Prompt.CastWhileSearching {} -> case response of
    Response.CastWhileSearched found -> Just found
    _ -> Nothing
```
`defaultAnswer`: `Prompt.CastWhileSearching {} -> Nothing` (declining is always legal — the least eventful fallback when a transcript runs short).

- [x] **Step 5: Add the arm to every exhaustive `Prompt` match**

The new constructor forces an arm on **every** `case p of` over `Prompt` that lacks a catch-all `_` wildcard. Under the `pedantic` flag `-Wincomplete-patterns` is an error, so **the build enumerates every site exactly** — build, read the warning list, and add the arm to each. Add `Prompt.CastWhileSearching {} -> Nothing` to non-monadic answerers, and `Prompt.CastWhileSearching {} -> pure Nothing` to monadic ones (those returning `State.State … r`).

Known homes (let the build confirm the full set — do not rely on this list being complete): `source/test-suite/Pawl/Support.hs` (`identityAnswer`, `castAnswer`, `aggressiveAnswer`, `playLandAnswer`, and the monadic `randomAnswer`); the group-local answerers in `EngineSpec.hs` (`slaveAnswer`, Task 5), `GameSpec.hs` (`recordingAnswer` — monadic), `CastSpec.hs` (`liar`, `discardLastAnswer`, `hackAnswer`), `ResolveSpec.hs` (`findFirst`, `findNothing`, `boltAnswer`, `atBob`), and any full answerer in `ActivateSpec.hs`/`CombatSpec.hs`. (`Pawl.Replay`'s `encode`/`decode`/`defaultAnswer` are already handled in Step 4. The Task 7/8 test interpreters `castFirstOption`/`castPanglacial` are written *with* the arm, so they are already complete.)

- [x] **Step 6: Add the `castWhileSearching` loop to `Cast`**

In `source/library/Pawl/Cast.hs`, add `import qualified Control.Monad.Trans.Class as Trans` and `import qualified Pawl.Type.Program as Program` (if absent) and `import qualified Pawl.Type.Prompt as Prompt` (if absent):

```haskell
-- CR 601.3 (Panglacial): while a player searches their own library, offer them
-- the chance to cast a castable-while-searching card from it, before any card is
-- found (per the ruling). Loops so multiple copies may be cast (also per the
-- ruling); each cast removes a card from the library, so castableWhileSearching
-- shrinks and the loop terminates. castSpell is the re-entrant call -- casting
-- mid-resolution, the whole point.
castWhileSearching :: PlayerId -> Game ()
castWhileSearching pid = do
  gs <- State.get
  case castableWhileSearching pid gs of
    [] -> pure ()
    options -> do
      let decider = Decide.deciderFor pid gs
      choice <- Trans.lift (Program.prompt (Prompt.CastWhileSearching decider pid options))
      case choice of
        Nothing -> pure ()
        Just oid ->
          -- Reject-not-repair: an option not in the offered set is a no-op that
          -- ends the loop, never a repair.
          if elem oid options
            then do
              castSpell pid oid
              castWhileSearching pid
            else pure ()
```

- [x] **Step 7: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — Panglacial is cast onto the stack, leaves the library, and seven Forests are tapped.

- [x] **Step 8: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "CastWhileSearching prompt + serialization + the re-entrant cast loop (CR 601.3)"
```

---

### Task 8: Wire the cast loop into resolution — cast Panglacial during Evolving Wilds' search

**Files:**
- Modify: `source/library/Pawl/Stack.hs` (`resolveTop` offers the cast before an ability that searches)
- Test: `source/test-suite/Pawl/StackSpec.hs` (the re-entrancy gate + negative control)

**Interfaces:**
- Consumes: `Cast.castWhileSearching` (Task 7), `Resolve.searchesLibrary` (Task 6), `Activate.activateAbility` (M3e).
- Produces: `Stack.resolveTop`'s `OfAbility` arm runs `Cast.castWhileSearching (Object.owner obj)` before `resolveAbility` when any of the ability's effects `searchesLibrary`.

- [x] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/StackSpec.hs`. Activate Evolving Wilds and, during its search, cast Panglacial; assert the interleave and the eventual 9/5 (add imports as the compiler flags — `qualified Pawl.Activate as Activate`, `qualified Pawl.Projection as Projection`, `qualified Pawl.Engine as Engine`, `qualified Pawl.Stack as Stack`, `qualified Pawl.Type.Prompt as Prompt`, `qualified Pawl.Type.Phase as Phase`, `qualified Pawl.Type.GameState as GameState`, `qualified Data.List as List`, `qualified Data.Text as Text`, `qualified Data.Map.Strict as Map`, `qualified Data.Set as Set`):

```haskell
      HU.testCase "CR 601.3: cast Panglacial during Evolving Wilds' search, then it resolves 9/5" $
        let g0 = Setup.emptyGame S.bothPlayers
            (ewId, g1) = S.addCreature Card.evolvingWildsPrinting S.alice g0
            g2 = List.foldl' (\g _ -> snd (S.addCreature Card.forestPrinting S.alice g)) g1 [1 .. (7 :: Int)]
            (_, g3) = S.addLibraryCard Card.panglacialWurmPrinting S.alice g2
            g4 = g3 {GameState.activePlayer = S.alice, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}
         in case Projection.abilitiesOf ewId g4 of
              ewAbility : _ ->
                let action = do
                      Activate.activateAbility S.alice ewId ewAbility
                      Stack.resolveTop -- Evolving Wilds' ability: cast Panglacial, then search + shuffle + cease
                      Stack.resolveTop -- Panglacial resolves onto the battlefield
                    after = snd (Engine.runGamePure castPanglacial g4 action)
                 in do
                      HU.assertEqual "Panglacial is a 9/5 on the battlefield" 1 (S.countOnBattlefieldByName (Text.pack "Panglacial Wurm") S.alice after)
                      HU.assertEqual "Panglacial left the library" 0 (S.countByName (Text.pack "Panglacial Wurm") S.alice after)
                      HU.assertEqual "seven Forests tapped for {5}{G}{G}" 7 (S.tappedCount S.alice after)
              [] -> HU.assertFailure "Evolving Wilds should have an activated ability",
      HU.testCase "declining the cast resolves the search normally, Panglacial stays" $
        let g0 = Setup.emptyGame S.bothPlayers
            (ewId, g1) = S.addCreature Card.evolvingWildsPrinting S.alice g0
            g2 = List.foldl' (\g _ -> snd (S.addCreature Card.forestPrinting S.alice g)) g1 [1 .. (7 :: Int)]
            (_, g3) = S.addLibraryCard Card.panglacialWurmPrinting S.alice g2
            g4 = g3 {GameState.activePlayer = S.alice, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}
         in case Projection.abilitiesOf ewId g4 of
              ewAbility : _ ->
                let after = snd (Engine.runGamePure S.identityAnswer g4 (do Activate.activateAbility S.alice ewId ewAbility; Stack.resolveTop))
                 in HU.assertEqual "Panglacial still in the library" 1 (S.countByName (Text.pack "Panglacial Wurm") S.alice after)
              [] -> HU.assertFailure "Evolving Wilds should have an activated ability"
    ,
```

Add the group-local interpreter to `StackSpec.hs` (casts the first CastWhileSearching option, fails to find with the search):

```haskell
castPanglacial :: Prompt.Prompt r -> r
castPanglacial p = case p of
  Prompt.CastWhileSearching _ _ options -> case options of
    oid : _ -> Just oid
    [] -> Nothing
  Prompt.SearchLibrary {} -> Nothing
  Prompt.Shuffle ids -> ids
  Prompt.ChooseTargets _ _ _ sets -> Map.mapMaybe Set.lookupMin sets
  Prompt.ChooseAction {} -> A.Pass
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter S.isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
```

(Add imports `qualified Pawl.Type.Action as A`, `qualified Pawl.Type.Subtype as Subtype` if absent.)

- [x] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "cast Panglacial during"'`
Expected: FAIL — without the `Stack` wiring, `CastWhileSearching` is never prompted, so Panglacial stays in the library and never reaches the battlefield.

- [x] **Step 3: Wire the cast offer into `resolveTop`**

In `source/library/Pawl/Stack.hs`, add `import qualified Control.Monad as Monad`, `import qualified Pawl.Cast as Cast`, and `import qualified Pawl.Type.ActivatedAbility as ActivatedAbility`. Replace the `OfAbility` arm:

```haskell
        Source.OfAbility srcId ability -> do
          -- CR 601.3 (Panglacial): before resolving an ability that searches a
          -- library, offer its controller the chance to cast a
          -- castable-while-searching card from their library. The ability is still
          -- on the stack, so a cast lands on top of it (the ruling's sequence).
          -- Offered at resolution start, not per-Search-effect within a
          -- multi-effect ability -- exact intra-resolution interleaving is a named
          -- expiry (spec section 7); Evolving Wilds' only effect is the search.
          Monad.when (any Resolve.searchesLibrary (ActivatedAbility.effects ability)) $
            Cast.castWhileSearching (Object.owner obj)
          Resolve.resolveAbility oid srcId ability
```

(`Object.owner obj` is the ability's controller; `obj` is already bound in `resolveTop`.)

- [x] **Step 4: Run tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — Panglacial is cast during the search, the search finishes, and on the next resolution Panglacial enters as a 9/5; the negative control leaves it in the library.

- [x] **Step 5: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Re-entrancy gate: cast Panglacial during Evolving Wilds' search (CR 601.3/605.3a)"
```

---

## Phase 3 — record the milestone (Task 9)

### Task 9: Mark M3g complete in `CLAUDE.md`

**Files:**
- Modify: `CLAUDE.md` (add the M3g current-work note)
- Modify: `docs/design.md` (optional: tick the M3g row if the split table tracks completion)

**Interfaces:** none — documentation only.

- [x] **Step 1: Add the M3g completion note**

In `CLAUDE.md`, after the M3f note in the "Current work and tracking" list, add a bullet in the same voice summarizing M3g: the payoff pair (Decider CR 723 via `pendingControl`/`activeControl` read by `deciderFor`, promoted in `handoffTurn`; Mindslaver), re-entrancy (Panglacial cast from library mid-search, orchestrated in `Stack.resolveTop` because `Cast`/`Mana` sit above `Resolve`), the new types (`CardType.Artifact`, `TargetSpec.PlayerTarget`, `Effect.ControlPlayerNextTurn`, `CastingPermission`, `AbilityCost.mana`, `Prompt.CastWhileSearching`), and the named elisions (legend rule CR 704.5j — singleton, must land suppressible; CR 723.4 visibility — no PlayerView; CR 723.2 limited-duration control). Cite the spec and plan paths as the other milestones do.

- [x] **Step 2: Verify the plan is fully ticked**

Run: `grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-19-m3g-decider-reentrancy.md`
Expected: `0` (every step checked). If not, finish the unchecked steps — never edit the plan to satisfy the grep.

- [x] **Step 3: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "Record M3g complete: the payoff pair (Decider CR 723 + re-entrancy)"
```
