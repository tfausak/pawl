# M2d castable black/green decks — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make M2c's Typhoid Rats and War Mammoth castable by giving players real mono-color decks, so a random game exercises the deathtouch SBA, trample assignment, and their CR 702.2c interaction through actual casting and combat.

**Architecture:** Add `Swamp`/`Forest` basic-land subtypes and printings; model a deck as a `Map Printing Natural` multiset behind a `Deck` newtype; refactor setup so `newGame`/`playFrom` take an explicit `NonEmpty (PlayerId, Deck)` matchup (no privileged default deck); run the property suite over two matchups, `redRed` (unchanged behavior) and `greenBlack` (alice green, bob black). No new rules, zero opcodes.

**Tech Stack:** Haskell 2010 (GHC 9.14.1), `tasty` + `tasty-hunit` + `tasty-quickcheck`, `containers` (`Data.Map.Strict`).

**Spec:** `docs/superpowers/specs/2026-07-17-m2d-castable-decks-design.md`

## Global Constraints

- Haskell 2010, **no language extensions** beyond the `GADTs`/`RankNTypes` already present. No `LambdaCase`, `OverloadedStrings`, etc.
- Build must be **warning-clean** under `-Weverything` minus the allow-list. `-Werror` is not set — treat any warning as a failure. Verify with a **clean** build (incremental builds hide warnings): `rm -rf dist-newstyle/build/aarch64-osx/ghc-9.14.1/pawl-0.2026.7.16/{b,t,build}` then `cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -c "warning:"` must print `0`.
- **No partial functions** written or used. No `head`/`error`/`undefined`/non-exhaustive matches.
- **Qualified imports**, aliased to the last component (`Data.Map.Strict` → `Map`, `Pawl.Type.Deck` → `Deck`). Operators unqualified.
- **`newtype` non-punning** with a `Mk` constructor: `newtype Deck = MkDeck …`.
- **One type per module** under `Pawl.Type.<Name>`; logic lives in other `Pawl.*` modules.
- **`Text` not `String`**; arbitrary-precision numbers (`Natural`, `Integer`).
- **No list comprehensions.** Use `map`/`replicate`/`Monad.forM_`.
- New `exposed-modules` come from the `-- cabal-gild: discover` directive — add the file and run `cabal-gild` via `hooky fix`; never hand-edit the field.
- Each task is **one small complete commit on `main`**. After each task: `cabal build` warning-free, `hooky fix` then `git add -A` then `hooky run` passes, HLint applied, `cabal test` green.
- This milestone adds **no new comprehensive rules**; the only CR already relied on is 305.6 (basic-land intrinsic mana ability from subtype), already cited in `Pawl.Mana`. No `rules.txt` re-verification needed beyond that.

---

## Task 1: Swamp and Forest subtypes, mana, and basic-land printings

Adds the two land types and their printings. Self-contained: no setup signatures change, so the suite stays green throughout.

**Files:**
- Modify: `source/library/Pawl/Type/Subtype.hs`
- Modify: `source/library/Pawl/Mana.hs:38-51` (`subtypeMana`)
- Modify: `source/library/Pawl/Card.hs` (add two printings)
- Test: `source/test-suite/Main.hs`

**Interfaces:**
- Produces: `Subtype.Swamp`, `Subtype.Forest`; `Card.swampPrinting :: Printing.Printing`, `Card.forestPrinting :: Printing.Printing`; `Mana.subtypeMana Subtype.Swamp == Just (ManaType.Colored Color.Black)`, `Mana.subtypeMana Subtype.Forest == Just (ManaType.Colored Color.Green)`.

- [x] **Step 1: Write the failing test**

Add this test group to `source/test-suite/Main.hs` (place the function near `deckTests`), and register `basicLandTests` in the top-level test list (the list at lines ~100-124, e.g. right after `m2cCardTests`):

```haskell
basicLandTests :: Tasty.TestTree
basicLandTests =
  Tasty.testGroup
    "BasicLand"
    [ HU.testCase "CR 305.6 a Swamp's intrinsic ability is black mana" $
        HU.assertEqual
          "black"
          (Just (ManaType.Colored Color.Black))
          (Mana.subtypeMana Subtype.Swamp),
      HU.testCase "CR 305.6 a Forest's intrinsic ability is green mana" $
        HU.assertEqual
          "green"
          (Just (ManaType.Colored Color.Green))
          (Mana.subtypeMana Subtype.Forest),
      HU.testCase "swampPrinting is a basic Swamp land" $
        let c = Printing.card Card.swampPrinting
         in do
              HU.assertBool "land" (Card.isLand c)
              HU.assertBool
                "swamp subtype"
                (Set.member Subtype.Swamp (TypeLine.subtypes (Card.Type.typeLine c))),
      HU.testCase "forestPrinting is a basic Forest land" $
        let c = Printing.card Card.forestPrinting
         in do
              HU.assertBool "land" (Card.isLand c)
              HU.assertBool
                "forest subtype"
                (Set.member Subtype.Forest (TypeLine.subtypes (Card.Type.typeLine c)))
    ]
```

- [x] **Step 2: Run the test to verify it fails**

Run: `cabal build all --enable-tests 2>&1 | grep -iE "not in scope|Swamp|Forest"`
Expected: compile failure — `Subtype.Swamp`, `Card.swampPrinting`, etc. not in scope.

- [x] **Step 3: Add the subtypes**

In `source/library/Pawl/Type/Subtype.hs`, add `Swamp` and `Forest` to the `data Subtype` enumeration (keep the existing `deriving (Eq, Ord, Show)`):

```haskell
data Subtype
  = Mountain
  | Swamp
  | Forest
  | Goblin
  | Warrior
  | Human
  | Bird
  | Ogre
  | Centaur
  | Cat
  | Dinosaur
  | Beast
  | Rat
  | Elephant
  deriving (Eq, Ord, Show)
```

- [x] **Step 4: Add the mana mappings**

In `source/library/Pawl/Mana.hs`, add two arms to `subtypeMana` (it is the only exhaustive `case` over `Subtype` in the library, so this is the only match to extend):

```haskell
subtypeMana subtype = case subtype of
  Subtype.Mountain -> Just (ManaType.Colored Color.Red)
  Subtype.Swamp -> Just (ManaType.Colored Color.Black)
  Subtype.Forest -> Just (ManaType.Colored Color.Green)
  Subtype.Goblin -> Nothing
  Subtype.Warrior -> Nothing
  Subtype.Human -> Nothing
  Subtype.Bird -> Nothing
  Subtype.Ogre -> Nothing
  Subtype.Centaur -> Nothing
  Subtype.Cat -> Nothing
  Subtype.Dinosaur -> Nothing
  Subtype.Beast -> Nothing
  Subtype.Rat -> Nothing
  Subtype.Elephant -> Nothing
```

- [x] **Step 5: Add the basic-land printings**

In `source/library/Pawl/Card.hs`, add `swampPrinting` and `forestPrinting`, each shaped exactly like `mountainPrinting` (mana ability granted from the subtype by CR 305.6, so not stored on the card):

```haskell
-- The Swamp's black mana ability is granted from its subtype by CR 305.6, so it
-- is derived by the engine, not stored on the card.
swampPrinting :: Printing.Printing
swampPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Swamp",
            Card.manaCost = Nothing,
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.singleton Supertype.Basic,
                  TypeLine.types = Set.singleton CardType.Land,
                  TypeLine.subtypes = Set.singleton Subtype.Swamp
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty
          }
    }

-- The Forest's green mana ability is granted from its subtype by CR 305.6, so it
-- is derived by the engine, not stored on the card.
forestPrinting :: Printing.Printing
forestPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Forest",
            Card.manaCost = Nothing,
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.singleton Supertype.Basic,
                  TypeLine.types = Set.singleton CardType.Land,
                  TypeLine.subtypes = Set.singleton Subtype.Forest
                },
            Card.power = Nothing,
            Card.toughness = Nothing,
            Card.keywords = Set.empty
          }
    }
```

- [x] **Step 6: Run the test to verify it passes**

Run: `cabal test 2>&1 | grep -iE "BasicLand|FAIL|PASS|error"`
Expected: the four `BasicLand` cases pass; whole suite green.

- [x] **Step 7: Clean-build warning check**

Run: `rm -rf dist-newstyle/build/aarch64-osx/ghc-9.14.1/pawl-0.2026.7.16/{b,t,build} && cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -c "warning:"`
Expected: `0`

- [x] **Step 8: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git add -A
git commit -m "Add Swamp/Forest subtypes, mana, and basic-land printings

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 2: Deck multiset and the explicit-matchup setup refactor

Introduces `Deck`, the three named decks, `deckSize`/`mirror`, and rewrites `newGame`/`playFrom` to take `NonEmpty (PlayerId, Deck)`. Updates every red-red call site (library, benchmark, tests) at once — this is one atomic compile unit. **Behavior-preserving:** every game still runs red-red, so all existing assertions still hold.

**Files:**
- Create: `source/library/Pawl/Type/Deck.hs`
- Modify: `source/library/Pawl/Setup.hs` (decks, `deckSize`, `mirror`, `newGame`)
- Modify: `source/library/Pawl/Engine.hs:268-270` (`playFrom`)
- Modify: `source/benchmark/Main.hs:84,91,97`
- Modify: `source/test-suite/Main.hs` (matchup bindings; every `Setup.newGame bothPlayers`/`Engine.playFrom bothPlayers`; `deckTests`)

**Interfaces:**
- Produces: `Deck.Deck`, `Deck.MkDeck :: Map Printing Natural -> Deck`; `Setup.redDeck`, `Setup.greenDeck`, `Setup.blackDeck :: Deck`; `Setup.deckSize :: Deck -> Natural`; `Setup.mirror :: Deck -> NonEmpty PlayerId -> NonEmpty (PlayerId, Deck)`; `Setup.newGame :: NonEmpty (PlayerId, Deck) -> Game ()`; `Engine.playFrom :: NonEmpty (PlayerId, Deck) -> Game Result`.
- Consumes: Task 1's `Card.swampPrinting`, `Card.forestPrinting`.
- Test-local: `redRed`, `greenBlack :: NonEmpty (PlayerId, Deck.Deck)` (defined in the test suite).

- [x] **Step 1: Write the failing test (new `deckTests`)**

Replace the existing `deckTests` group in `source/test-suite/Main.hs` (currently at ~line 1102-1116, the one asserting `length Setup.deckList`) with this. Add `import qualified Pawl.Type.Deck as Deck` to the import block:

```haskell
deckTests :: Tasty.TestTree
deckTests =
  Tasty.testGroup
    "Deck"
    [ HU.testCase "the red deck is 60 cards" $
        HU.assertEqual "size" 60 (Setup.deckSize Setup.redDeck),
      HU.testCase "the green deck is 60 cards" $
        HU.assertEqual "size" 60 (Setup.deckSize Setup.greenDeck),
      HU.testCase "the black deck is 60 cards" $
        HU.assertEqual "size" 60 (Setup.deckSize Setup.blackDeck),
      HU.testCase "red deck composition" $
        let Deck.MkDeck m = Setup.redDeck
         in do
              HU.assertEqual "mountains" (Just 36) (Map.lookup Card.mountainPrinting m)
              HU.assertEqual "pikers" (Just 16) (Map.lookup Card.pikerPrinting m)
              HU.assertEqual "maidens" (Just 8) (Map.lookup Card.birdMaidenPrinting m),
      HU.testCase "green deck composition" $
        let Deck.MkDeck m = Setup.greenDeck
         in do
              HU.assertEqual "forests" (Just 36) (Map.lookup Card.forestPrinting m)
              HU.assertEqual "mammoths" (Just 24) (Map.lookup Card.warMammothPrinting m),
      HU.testCase "black deck composition" $
        let Deck.MkDeck m = Setup.blackDeck
         in do
              HU.assertEqual "swamps" (Just 36) (Map.lookup Card.swampPrinting m)
              HU.assertEqual "rats" (Just 24) (Map.lookup Card.typhoidRatsPrinting m),
      HU.testCase "36 Mountains per player after a red-red setup" $
        HU.assertEqual "mountains" 36 (countByName (Text.pack "Mountain") alice setupState),
      HU.testCase "8 Bird Maidens per player after a red-red setup" $
        HU.assertEqual "maidens" 8 (countByName (Text.pack "Bird Maiden") alice setupState),
      HU.testCase "16 Pikers per player after a red-red setup" $
        HU.assertEqual "pikers" 16 (countByName (Text.pack "Goblin Piker") bob setupState)
    ]
```

- [x] **Step 2: Run to verify it fails**

Run: `cabal build all --enable-tests 2>&1 | grep -iE "not in scope|deckSize|redDeck|Deck"`
Expected: compile failure — `Deck`, `Setup.redDeck`, `Setup.deckSize`-as-function, etc. not in scope.

- [x] **Step 3: Create the `Deck` type**

Create `source/library/Pawl/Type/Deck.hs`:

```haskell
module Pawl.Type.Deck where

import Data.Map.Strict (Map)
import Numeric.Natural (Natural)
import Pawl.Type.Printing (Printing)

-- A deck is a multiset of printings: a shuffle erases any order among the cards,
-- so counts are the honest model. `Printing` and everything beneath it derive
-- `Ord`, so it is a lawful `Map` key.
newtype Deck = MkDeck (Map Printing Natural)
  deriving (Eq, Show)
```

Run `hooky fix` (or `cabal-gild`) so the `-- cabal-gild: discover` directive picks up the new module — do not hand-edit `exposed-modules`.

- [x] **Step 4: Add decks, `deckSize`, `mirror`, and a deck-expander to `Setup`**

In `source/library/Pawl/Setup.hs`: add imports `import qualified Pawl.Type.Deck as Deck` and (if not already present) `import qualified Numeric.Natural as Natural` — actually use `Numeric.Natural (Natural)` for the signature. Replace the `deckList`/`deckSize` block (lines ~33-48) with:

```haskell
-- Every deck is 36 land + 24 creature = 60, so two players conserve 120 objects
-- in any matchup. redDeck is M0's deck; greenDeck and blackDeck make M2c's
-- trampler and deathtoucher castable (git-bug 14138aa). Each deck is mono-color,
-- which is what keeps Mana.payCost's source elision legitimate.
redDeck :: Deck.Deck
redDeck =
  Deck.MkDeck $
    Map.fromList
      [ (Card.mountainPrinting, 36),
        (Card.pikerPrinting, 16),
        (Card.birdMaidenPrinting, 8)
      ]

greenDeck :: Deck.Deck
greenDeck =
  Deck.MkDeck $
    Map.fromList
      [ (Card.forestPrinting, 36),
        (Card.warMammothPrinting, 24)
      ]

blackDeck :: Deck.Deck
blackDeck =
  Deck.MkDeck $
    Map.fromList
      [ (Card.swampPrinting, 36),
        (Card.typhoidRatsPrinting, 24)
      ]

deckSize :: Deck.Deck -> Natural
deckSize (Deck.MkDeck m) = sum (Map.elems m)

-- Pair every player with one deck, for a symmetric (mirror) matchup.
mirror :: Deck.Deck -> NonEmpty.NonEmpty PlayerId -> NonEmpty.NonEmpty (PlayerId, Deck.Deck)
mirror deck order = NonEmpty.map (\pid -> (pid, deck)) order
```

Add `import Numeric.Natural (Natural)` to the import list. `Card.warMammothPrinting`/`typhoidRatsPrinting` already exist from M2c; `Card` is already imported.

- [x] **Step 5: Rewrite `newGame` to take a matchup**

In `source/library/Pawl/Setup.hs`, replace `newGame` (lines ~124-128) with:

```haskell
-- Build each player's library from their deck's multiset, shuffle, draw.
createDeck :: PlayerId -> Deck.Deck -> Game ()
createDeck pid (Deck.MkDeck m) =
  Monad.forM_ (Map.toList m) $ \(printing, n) ->
    Monad.replicateM_ (fromIntegral n) (createCard pid printing)

newGame :: NonEmpty.NonEmpty (PlayerId, Deck.Deck) -> Game ()
newGame matchup = Monad.forM_ (NonEmpty.toList matchup) $ \(pid, deck) -> do
  createDeck pid deck
  shuffleLibrary pid
  Monad.replicateM_ openingHand (drawCard pid)
```

`createCard`, `shuffleLibrary`, `drawCard`, `emptyGame` are unchanged. `fromIntegral :: Natural -> Int` for `replicateM_`.

- [x] **Step 6: Change `Engine.playFrom`**

In `source/library/Pawl/Engine.hs`, add `import qualified Pawl.Type.Deck as Deck`, and change `playFrom` (lines ~268-270):

```haskell
playFrom :: NonEmpty.NonEmpty (PlayerId, Deck.Deck) -> Game Result
playFrom matchup = do
  Setup.newGame matchup
  playGame
```

(`emptyGame` is still called by callers with `NonEmpty PlayerId`; `playFrom`'s body no longer needs the raw order.)

- [x] **Step 7: Update the benchmark call sites**

In `source/benchmark/Main.hs`, change the three runners (lines 84, 91, 97) to pass a red mirror. Each currently ends `… (Engine.playFrom players))`; make it:

```haskell
   in fst (Engine.runGamePure alwaysPass (Setup.emptyGame players) (Engine.playFrom (Setup.mirror Setup.redDeck players)))
```

Apply the same `Setup.mirror Setup.redDeck players` wrap in `casting` (line 91) and `fighting` (line 97). `Setup` is already imported.

- [x] **Step 8: Add matchup bindings and update every red-red call site in the tests**

In `source/test-suite/Main.hs`, define (near `bothPlayers`, ~line 1217):

```haskell
redRed :: NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck)
redRed = Setup.mirror Setup.redDeck bothPlayers
```

Then update each call site so the suite compiles (behavior unchanged — still red-red):
- line ~892 `castGameState`: `Engine.playFrom bothPlayers` → `Engine.playFrom redRed`.
- line ~1065 `bobDiscardChoice`: `Setup.newGame bothPlayers` → `Setup.newGame redRed`.
- line ~1314 `setupState`: `Setup.newGame bothPlayers` → `Setup.newGame redRed`.
- line ~1353 (replay goldfish runner): `Engine.playFrom bothPlayers` → `Engine.playFrom redRed`.
- line ~1377 (`landState`/playLand runner): `Engine.playFrom bothPlayers` → `Engine.playFrom redRed`.
- line ~1408 `replayTests`: `game = Engine.playFrom bothPlayers` → `game = Engine.playFrom redRed`.
- line ~1480 `runRandomGame`: `game = Engine.playFrom bothPlayers` → `game = Engine.playFrom redRed`.
- line ~1527 (life-change scenario runner): `Setup.newGame bothPlayers` → `Setup.newGame redRed`.

`Setup.emptyGame bothPlayers` calls stay unchanged everywhere (they still take `NonEmpty PlayerId`).

- [x] **Step 9: Run tests to verify green**

Run: `cabal test 2>&1 | grep -iE "Deck|FAIL|error|passed"`
Expected: the new `Deck` cases pass; all previously-passing groups still pass.

- [x] **Step 10: Clean-build warning check**

Run: `rm -rf dist-newstyle/build/aarch64-osx/ghc-9.14.1/pawl-0.2026.7.16/{b,t,build} && cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -c "warning:"`
Expected: `0`

- [x] **Step 11: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git add -A
git commit -m "Model decks as multisets; make setup take an explicit matchup

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 3: Run the property suite over both matchups

Parameterizes `runRandomGame` by matchup and asserts every property across `{ redRed, greenBlack }`. Adds a green-black setup composition test.

**Files:**
- Modify: `source/test-suite/Main.hs` (`greenBlack`, `matchups`, `runRandomGame`, `propertyTests`, `someLifeChanged`, a new `greenBlackSetup` + test)

**Interfaces:**
- Consumes: Task 2's `redRed`, `Setup.greenDeck`, `Setup.blackDeck`, `Engine.playFrom :: NonEmpty (PlayerId, Deck) -> Game Result`.
- Produces (test-local): `greenBlack`, `matchups :: [NonEmpty (PlayerId, Deck.Deck)]`, `runRandomGame :: NonEmpty (PlayerId, Deck.Deck) -> Int -> GameState`.

- [x] **Step 1: Write the failing test (green-black setup) and generalize the properties**

In `source/test-suite/Main.hs`, add near `redRed`:

```haskell
greenBlack :: NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck)
greenBlack = (alice, Setup.greenDeck) NonEmpty.:| [(bob, Setup.blackDeck)]

matchups :: [NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck)]
matchups = [redRed, greenBlack]
```

Add a green-black setup fixture + test (register `greenBlackSetupTests` in the top-level test list):

```haskell
greenBlackSetup :: GameState.GameState
greenBlackSetup =
  Program.foldProgram
    identityAnswer
    (State.execStateT (Setup.newGame greenBlack) (Setup.emptyGame bothPlayers))

greenBlackSetupTests :: Tasty.TestTree
greenBlackSetupTests =
  Tasty.testGroup
    "GreenBlackSetup"
    [ HU.testCase "alice's green deck deals 36 Forests" $
        HU.assertEqual "forests" 36 (countByName (Text.pack "Forest") alice greenBlackSetup),
      HU.testCase "bob's black deck deals 36 Swamps" $
        HU.assertEqual "swamps" 36 (countByName (Text.pack "Swamp") bob greenBlackSetup),
      HU.testCase "green-black setup conserves 120 objects" $
        HU.assertEqual "count" 120 (Game.objectCount greenBlackSetup)
    ]
```

- [x] **Step 2: Run to verify it fails**

Run: `cabal build all --enable-tests 2>&1 | grep -iE "not in scope|greenBlack"`
Expected: compile failure — `greenBlack` / `matchups` not yet used by `runRandomGame`, and the new group not registered.
(After adding the group to the top-level list, the failure narrows to the property changes below.)

- [x] **Step 3: Parameterize `runRandomGame`**

Replace `runRandomGame` (lines ~1478-1483) with a matchup-taking version:

```haskell
runRandomGame :: NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> Int -> GameState.GameState
runRandomGame matchup s =
  let start = Setup.emptyGame (NonEmpty.map fst matchup)
      game = Engine.playFrom matchup
      (_, final) = State.evalState (Program.foldProgramM randomAnswer (State.runStateT game start)) (Random.mkStdGen s)
   in final
```

- [x] **Step 4: Run each property over both matchups**

Replace `propertyTests` (lines ~1490-1516) so each property is conjoined across `matchups` (no list comprehension — use `map`):

```haskell
propertyTests :: Tasty.TestTree
propertyTests =
  Tasty.testGroup
    "Properties"
    [ QC.testProperty "conservation: 120 objects at end" $ \s ->
        QC.conjoin (map (\m -> Game.objectCount (runRandomGame m s) QC.=== 120) matchups),
      QC.testProperty "every game terminates with a result" $ \s ->
        QC.conjoin (map (\m -> QC.property (Maybe.isJust (GameState.result (runRandomGame m s)))) matchups),
      QC.testProperty "at least 120 ids were minted" $ \s ->
        QC.conjoin (map (\m -> QC.property (nextIdOf (runRandomGame m s) >= 120)) matchups),
      QC.testProperty "no mana floats at the end" $ \s ->
        QC.conjoin (map (\m -> GameState.manaPool (runRandomGame m s) QC.=== Map.empty) matchups),
      QC.testProperty "life never increases" $ \s ->
        QC.conjoin
          ( map
              (\m -> QC.property (all (\pl -> Player.life pl <= Setup.startingLife) (Map.elems (GameState.players (runRandomGame m s)))))
              matchups
          ),
      QC.testProperty "combat happens: some seed changes a life total" $
        QC.once $
          QC.property $
            any someLifeChanged [1 .. 100 :: Int]
    ]
```

- [x] **Step 5: Fix `someLifeChanged` for the new `runRandomGame` arity**

Update `someLifeChanged` (lines ~1519-1522) to pass a matchup (red-red keeps its existing meaning):

```haskell
someLifeChanged :: Int -> Bool
someLifeChanged s =
  let moved pl = Player.life pl /= Setup.startingLife
   in any moved (Map.elems (GameState.players (runRandomGame redRed s)))
```

- [x] **Step 6: Run tests to verify green**

Run: `cabal test 2>&1 | grep -iE "Properties|GreenBlackSetup|FAIL|error"`
Expected: all properties pass across both matchups; `GreenBlackSetup` passes.

- [x] **Step 7: Clean-build warning check**

Run: `rm -rf dist-newstyle/build/aarch64-osx/ghc-9.14.1/pawl-0.2026.7.16/{b,t,build} && cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -c "warning:"`
Expected: `0`

- [x] **Step 8: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git add -A
git commit -m "Run the property suite over red-red and green-black matchups

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 4: Castability asserted through the stack

A deterministic test that actually casts War Mammoth off Forests and Typhoid Rats off Swamps (mana → cast → resolve), proving the behavior M2d newly enables. Fixtures never proved castability.

**Files:**
- Modify: `source/test-suite/Main.hs` (generalize `mountainsInPlay`; add `landsInPlay`, `handOne`, `resolvedCreature`, `castabilityTests`)

**Interfaces:**
- Consumes: Task 1's `Card.forestPrinting`, `Card.swampPrinting`; existing `Card.warMammothPrinting`, `Card.typhoidRatsPrinting`, `Cast.castSpell`, `Stack.resolveTop`, `creaturesInPlay`.

- [x] **Step 1: Write the failing test**

In `source/test-suite/Main.hs`, add (and register `castabilityTests` in the top-level test list):

```haskell
-- alice controls n untapped basic lands of one printing, nothing else.
landsInPlay :: Printing.Printing -> Int -> GameState.GameState
landsInPlay land n =
  let add gs _ =
        let (oid, gs1) = Game.freshObjectId gs
            obj =
              Object.MkObject
                { Object.owner = alice,
                  Object.source = Source.OfCard land,
                  Object.zone = Zone.Battlefield,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled
                }
         in gs1
              { GameState.objects = Map.insert oid obj (GameState.objects gs1),
                GameState.battlefield = Set.insert oid (GameState.battlefield gs1)
              }
   in List.foldl' add (Setup.emptyGame bothPlayers) [1 .. n]

-- Put one card of a printing into alice's hand in a main phase with priority.
handOne :: Printing.Printing -> GameState.GameState -> (GameState.GameState, ObjectId.ObjectId)
handOne printing base =
  let (oid, gs1) = Game.freshObjectId base
      obj =
        Object.MkObject
          { Object.owner = alice,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled
          }
   in ( gs1
          { GameState.objects = Map.insert oid obj (GameState.objects gs1),
            GameState.hand = Map.insert alice (Seq.singleton oid) (GameState.hand gs1),
            GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = alice,
            GameState.priority = Just alice
          },
        oid
      )

-- Cast `creature` off `nLands` copies of `land`, then resolve it.
resolvedCreature :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
resolvedCreature land creature nLands =
  let (base, oid) = handOne creature (landsInPlay land nLands)
      afterCast = snd (Engine.runGamePure identityAnswer base (Cast.castSpell alice oid))
   in Stack.resolveTop afterCast

castabilityTests :: Tasty.TestTree
castabilityTests =
  Tasty.testGroup
    "Castability"
    [ HU.testCase "War Mammoth is cast off four Forests and resolves onto the battlefield" $
        let gs = resolvedCreature Card.forestPrinting Card.warMammothPrinting 4
         in do
              HU.assertEqual "stack empty" 0 (length (GameState.stack gs))
              HU.assertEqual "one creature in play" 1 (creaturesInPlay alice gs),
      HU.testCase "Typhoid Rats is cast off one Swamp and resolves onto the battlefield" $
        let gs = resolvedCreature Card.swampPrinting Card.typhoidRatsPrinting 1
         in do
              HU.assertEqual "stack empty" 0 (length (GameState.stack gs))
              HU.assertEqual "one creature in play" 1 (creaturesInPlay alice gs)
    ]
```

- [x] **Step 2: DRY — redefine `mountainsInPlay` in terms of `landsInPlay`**

Replace the body of the existing `mountainsInPlay` (lines ~1119-1135) with a one-liner so there is a single land-placement helper:

```haskell
-- alice controls n untapped Mountains on the battlefield, nothing else.
mountainsInPlay :: Int -> GameState.GameState
mountainsInPlay = landsInPlay Card.mountainPrinting
```

- [x] **Step 3: Run to verify the new tests pass**

Run: `cabal test 2>&1 | grep -iE "Castability|Cast\b|Stack|FAIL|error"`
Expected: both `Castability` cases pass; the pre-existing `Cast`/`Stack` groups (which use `mountainsInPlay`) still pass.

- [x] **Step 4: Clean-build warning check**

Run: `rm -rf dist-newstyle/build/aarch64-osx/ghc-9.14.1/pawl-0.2026.7.16/{b,t,build} && cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -c "warning:"`
Expected: `0`

- [x] **Step 5: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git add -A
git commit -m "Assert War Mammoth and Typhoid Rats cast and resolve through the stack

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 5: Green-black engagement guard

A `QC.once` property that at least one green-black seed sends a creature to a graveyard — cheap insurance that combat and the SBA actually engage under random play, so the matchup cannot silently no-op.

**Files:**
- Modify: `source/test-suite/Main.hs` (`creatureDied` + one property in `propertyTests`)

**Interfaces:**
- Consumes: Task 3's `runRandomGame`, `greenBlack`; existing `Game.zoneMembers`, `Zone.Graveyard`, `Card.isCreature`.

- [x] **Step 1: Write the failing test**

Add to `source/test-suite/Main.hs`:

```haskell
-- Did some green-black seed put a creature into a graveyard? In green-black the
-- only way a creature dies is combat (trade, deathtouch SBA, or trample), so
-- this fails only if combat never engages across all these seeds.
creatureDied :: Int -> Bool
creatureDied s =
  let gs = runRandomGame greenBlack s
      isDeadCreature oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard printing -> Card.isCreature (Printing.card printing)
      inGrave pid = any isDeadCreature (Game.zoneMembers Zone.Graveyard pid gs)
   in any inGrave [alice, bob]
```

And add this case to the `propertyTests` list (after "combat happens"):

```haskell
      QC.testProperty "green-black: some seed sends a creature to the graveyard" $
        QC.once $
          QC.property $
            any creatureDied [1 .. 100 :: Int]
```

- [x] **Step 2: Run to verify it passes**

Run: `cabal test 2>&1 | grep -iE "graveyard|Properties|FAIL"`
Expected: the new property passes (some seed in 1..100 has a creature death).
Note: if it unexpectedly fails, that is a real signal combat is not engaging in green-black — investigate rather than widening the seed range blindly. Do not weaken the assertion.

- [x] **Step 3: Clean-build warning check**

Run: `rm -rf dist-newstyle/build/aarch64-osx/ghc-9.14.1/pawl-0.2026.7.16/{b,t,build} && cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -c "warning:"`
Expected: `0`

- [x] **Step 4: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git add -A
git commit -m "Guard that green-black combat engages: some seed kills a creature

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 6: Record M2d complete

Update the milestone tracking and close the git-bug. No code.

**Files:**
- Modify: `CLAUDE.md` (milestone list)

- [x] **Step 1: Add the M2d tracking bullet**

In `CLAUDE.md`, under "Current work and tracking", add a bullet after the M2c one and adjust the "Current work is M3" bullet to note M2d landed first:

```markdown
- **M2d is complete** (M2c's black/green creatures are castable: `Swamp`/`Forest`
  basic lands, a `Deck` multiset (`Map Printing Natural`), and setup taking an
  explicit `NonEmpty (PlayerId, Deck)` matchup. The property suite runs over two
  matchups — red-red (unchanged) and green-black (alice green, bob black) — giving
  the 704.5h deathtouch SBA, trample assignment, and their CR 702.2c interaction
  random-game coverage; a deterministic test casts each card through the stack.
  No new rules, zero opcodes. `git-bug 14138aa` is closed). Spec and plan kept as
  reference: `docs/superpowers/specs/2026-07-17-m2d-castable-decks-design.md` and
  `docs/superpowers/plans/2026-07-17-m2d-castable-decks.md`.
```

- [x] **Step 2: Close the git-bug**

Run: `git-bug bug status close 14138aa`
Expected: the bug's status becomes `closed` (`git-bug bug` no longer lists it as open).

- [x] **Step 3: Commit**

```bash
git add CLAUDE.md
git commit -m "Record M2d complete: castable black/green decks (git-bug 14138aa)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Self-Review

**Spec coverage:**
- Subtypes + mana + basic-land printings (spec §1, §4) → Task 1. ✓
- `Deck` multiset newtype, three decks, `deckSize` (spec §2) → Task 2. ✓
- Explicit `NonEmpty (PlayerId, Deck)` matchup, `newGame`/`playFrom`, `mirror`, all red-red call sites (spec §3) → Task 2. ✓
- Property suite over both matchups; deck + setup composition tests (spec §5) → Tasks 2 (deckTests) + 3 (properties, green-black setup). ✓
- Castability asserted through the stack (spec §5) → Task 4. ✓
- Green-black engagement guard (spec §5) → Task 5. ✓
- Preserved: `payCost`/`tapForMana` elisions (mono-color decks), synthetic 702.2c fixture untouched (spec §6) → no task removes them; Task 4/combat fixtures leave M2c's fixtures in place. ✓
- Expiries + deferrals (spec §7, §8) → documentation only, captured in spec; Task 6 records the milestone. ✓

**Placeholder scan:** No TBD/TODO/"handle edge cases"/"similar to Task N". Every code step shows complete code. ✓

**Type consistency:** `Deck.MkDeck`/`Deck.Deck`, `Setup.deckSize :: Deck -> Natural`, `Setup.mirror`, `newGame :: NonEmpty (PlayerId, Deck) -> Game ()`, `playFrom :: NonEmpty (PlayerId, Deck) -> Game Result`, `runRandomGame :: NonEmpty (PlayerId, Deck.Deck) -> Int -> GameState` are used consistently across Tasks 2-5. `redRed`/`greenBlack`/`matchups` names match between definition (Tasks 2, 3) and use. `landsInPlay`/`handOne`/`resolvedCreature` defined and used within Task 4. ✓
