# M2a Keywords Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Creatures stop being interchangeable — a flier a Piker cannot block, a Sentry that cannot attack, a Centaur that attacks without tapping — completing milestone **M2a** from `docs/superpowers/specs/2026-07-16-m2a-keywords-design.md`.

**Architecture:** Builds on the completed M1b engine. `Pawl.Type.Keyword` is a closed enum citing rule 702; `Card` carries `keywords :: Set Keyword`; the closed half asks `Game.keywordsOf`/`hasKeyword` and **never** reads `Card.keywords` directly (the `controllerOf` pattern). Four of the five keywords are a clause inside a function `Pawl.Combat` already has. Flying and reach land on CR 509.1's structure: evasion is a **restriction checked against the whole declaration** (509.1b), so M1b's per-pair `Map.filterWithKey` is replaced by `legalBlockDeclaration`, and an illegal declaration is rejected wholesale. **Zero opcodes. No prompt changes.**

**Tech Stack:** GHC 9.14.1, Cabal. Library depends only on GHC boot libraries (`base`, `containers`, `text`, `transformers`) — no new dependencies. Tests use `tasty`/`tasty-hunit`/`tasty-quickcheck`; benchmark uses `tasty-bench`.

## Starting point (already exists — do not recreate)

M1b is complete and all 137 tests pass. The engine, the `Program`/`Prompt` seam, `changeZone`, mana, casting, the stack, combat declaration, simultaneous combat damage, SBAs, replay, the single-file test suite (`source/test-suite/Main.hs`) and benchmark (`source/benchmark/Main.hs`) all exist. This plan **modifies** them.

Read the spec before starting. The invariants it turns on:

1. **Casing on a `Keyword` is NOT a violation of the closed/open invariant.** Rule 702 is the rulebook; a keyword is a numbered rule, not an effect. `case keyword of Flying -> …` is the same kind of act as casing on `Phase`. See spec §1 before "fixing" this.
2. **The closed half asks `Game.keywordsOf`, never `Card.keywords`.** Layer 6 grants abilities at M3; this is `controllerOf`'s situation exactly.
3. **Evasion is a restriction on the DECLARATION, not a filter on candidates.** See Task 5 and spec §3. Menace is one punchlist entry away and breaks the pair-shaped version.

## Global Constraints

Every task's requirements implicitly include all of these:

- **Warning-clean:** library, test suite, and benchmark compile under `-Weverything` minus the allow-list in `pawl.cabal`. A warning is a failure. `-Wunused-matches` is active: prefix genuinely unused binders with `_`. `-Wmonomorphism-restriction` is active: a `let` without a signature that needs one is a failure — annotate it. Check with a **clean** build (`rm -rf dist-newstyle/build/aarch64-osx/ghc-9.14.1/pawl-0.2026.7.16/{b,t,build}`) — incremental builds hide warnings.
- **Boot libraries only** in the library (`base`, `containers`, `text`, `transformers`). No new dependencies.
- **Haskell 2010.** The only permitted extensions are `GADTs` and `RankNTypes`, per-file.
- **Non-punning constructors:** `Mk` prefix on newtypes and single-constructor records. Multi-constructor ADTs are written plainly.
- **One type per module** under `Pawl.Type.<TypeName>` — **type and instances only**. Cross-type logic lives in other `Pawl.*` modules. A module never imports its parents.
- **Derive at least `Eq` and `Show`.** `Card` derives `Ord`, so everything inside it must too.
- **No partial functions.** `Maybe`/`Either`, never `head`/`error`/non-exhaustive matches.
- **No list comprehensions, no backtick-infixed functions, no point-free where a `case` reads better.** `.hlint.yaml` already ignores the hints that contradict these; if HLint suggests one anyway, restructure rather than suppress.
- **Imports:** qualified, aliased to the last component; one import group; operators unqualified.
- **Tests:** all in `source/test-suite/Main.hs`, appended to the `testTree` list.
- **Commits:** directly to `main`, one small complete commit per task.
- **Module discovery:** after adding a library module run `cabal-gild --mode format --input pawl.cabal --output pawl.cabal`; never hand-edit `exposed-modules`.
- **Per-task gate:** `cabal build`, then `hooky fix` and `hooky run` must pass before committing. `hooky` acts on **staged** files — `git add -A` first, or it reports "hooks skipped".

**Verification commands:** build `cabal build`; all targets `cabal build all --enable-tests --enable-benchmarks`; test `cabal test`; one group `cabal test --test-options='-p "/Pattern/"'`; bench `cabal bench`; lint `hooky fix` then `hooky run`.

**Do not pipe `cabal test` into `head`** — closing the pipe early wedges the process and looks like a hang. Redirect to a file and grep it.

## File structure

**Created (library):**

| File | Responsibility |
|---|---|
| `source/library/Pawl/Type/Keyword.hs` | the CR 702 citation enum |

**Modified (library):** `Pawl/Type/Card.hs`, `Pawl/Type/Subtype.hs`; `Pawl/Card.hs`, `Pawl/Game.hs`, `Pawl/Combat.hs`, `Pawl/Setup.hs`.

**Modified (suites):** `source/test-suite/Main.hs`.

**Untouched, deliberately:** `Pawl/Type/Prompt.hs`, `Pawl/Type/Response.hs`, `Pawl/Replay.hs`, and all eight interpreters. M2a is additive across the decision seam, unlike M1b's Task 5. If a task appears to need a prompt change, stop — that is the corner the spec's §3 exists to avoid.

## Task ordering rationale

- **Task 1 lands all the data before anything consumes it** — the type, the field, the query, the subtypes and all five printings. The seam cannot be positively tested without a card that carries a keyword, and hand-setting `Card.keywords` on a Piker would test the field rather than a card.
- **Tasks 2–4 are the three cheap shapes**, each a clause in an existing function, each independently rejectable: unary legality (defender), an action modifier (vigilance), a state override (haste).
- **Task 5 is the risky one.** It is where CR 509.1b lives, and per-pair filtering is the natural (wrong) way to write it — it is also what M1b currently does.

---

### Task 1: The keyword seam and the five printings

`Keyword`, `Card.keywords`, `Game.keywordsOf`/`hasKeyword`, four new subtypes, five new printings. Nothing consumes them yet.

**Files:**
- Create: `source/library/Pawl/Type/Keyword.hs`
- Modify: `source/library/Pawl/Type/Card.hs`, `source/library/Pawl/Type/Subtype.hs`, `source/library/Pawl/Card.hs`, `source/library/Pawl/Game.hs`, `source/test-suite/Main.hs`

**Interfaces:**
- Consumes: `Pawl.Game.cardOf` (exists), M1b's `addPiker` test fixture.
- Produces:
  - `data Keyword = Defender | Flying | Haste | Reach | Vigilance` (Eq, Ord, Show)
  - `Card` gains `keywords :: Set Keyword`
  - `Pawl.Game.keywordsOf :: ObjectId -> GameState -> Set Keyword`
  - `Pawl.Game.hasKeyword :: Keyword -> ObjectId -> GameState -> Bool`
  - `Pawl.Card.birdMaidenPrinting`, `nimbleBirdstickerPrinting`, `ogreSentryPrinting`, `windseekerCentaurPrinting`, `goblinChariotPrinting :: Printing`
  - test fixture `addCreature :: Printing -> PlayerId -> GameState -> (ObjectId, GameState)`

All five cards are verified against Scryfall (`api.scryfall.com/cards/named?exact=…`). Do not "correct" them from memory:

| Card | Cost | P/T | Type line | Rules text |
|---|---|---|---|---|
| Bird Maiden | `{2}{R}` | 1/2 | Creature — Human Bird | Flying |
| Nimble Birdsticker | `{2}{R}` | 2/3 | Creature — Goblin | Reach |
| Ogre Sentry | `{1}{R}` | 3/3 | Creature — Ogre Warrior | Defender |
| Windseeker Centaur | `{1}{R}{R}` | 2/2 | Creature — Centaur | Vigilance |
| Goblin Chariot | `{2}{R}` | 2/2 | Creature — Goblin Warrior | Haste |

- [ ] **Step 1: Write the failing test**

Add the import to `source/test-suite/Main.hs` (keep the block alphabetically sorted — `hooky fix` enforces it):

```haskell
import qualified Pawl.Type.Keyword as Keyword
```

Add this fixture next to `addPiker`, and **redefine `addPiker` in terms of it** so the existing call sites keep working unchanged:

```haskell
-- Any printing, on the battlefield under pid's control, untapped and Settled.
addCreature :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
addCreature printing pid gs =
  let (oid, gs1) = Game.freshObjectId gs
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Battlefield,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled
          }
   in ( oid,
        gs1
          { GameState.objects = Map.insert oid obj (GameState.objects gs1),
            GameState.battlefield = Set.insert oid (GameState.battlefield gs1)
          }
      )

addPiker :: PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
addPiker = addCreature Card.pikerPrinting
```

Delete `addPiker`'s old body (the one that inlines `Object.MkObject`) and its now-stale comment about Task 2 adding a `damage` field.

Add this group, and add `keywordTests` to the `testTree` list:

```haskell
-- The printings M2a adds, paired with the single keyword each must carry.
m2aPrintings :: [(Printing.Printing, Keyword.Keyword)]
m2aPrintings =
  [ (Card.birdMaidenPrinting, Keyword.Flying),
    (Card.nimbleBirdstickerPrinting, Keyword.Reach),
    (Card.ogreSentryPrinting, Keyword.Defender),
    (Card.windseekerCentaurPrinting, Keyword.Vigilance),
    (Card.goblinChariotPrinting, Keyword.Haste)
  ]

keywordTests :: Tasty.TestTree
keywordTests =
  let gs0 = Setup.emptyGame bothPlayers
      -- Each M2a printing carries exactly its one keyword and no other.
      carriesOnly (printing, keyword) =
        let (oid, gs) = addCreature printing alice gs0
            name = Text.unpack (Card.Type.name (Printing.card printing))
         in HU.testCase (name ++ " carries exactly " ++ show keyword) $ do
              HU.assertEqual "keywords" (Set.singleton keyword) (Game.keywordsOf oid gs)
              HU.assertBool "hasKeyword" (Game.hasKeyword keyword oid gs)
   in Tasty.testGroup
        "Keyword"
        ( map carriesOnly m2aPrintings
            ++ [ HU.testCase "a Piker has no keywords" $
                   let (oid, gs) = addPiker alice gs0
                    in do
                         HU.assertEqual "none" Set.empty (Game.keywordsOf oid gs)
                         HU.assertBool "no flying" (not (Game.hasKeyword Keyword.Flying oid gs)),
                 HU.testCase "a Mountain has no keywords" $
                   let gs = mountainsInPlay 1
                    in case Game.zoneMembers Zone.Battlefield alice gs of
                         [] -> HU.assertFailure "fixture should have one Mountain"
                         oid : _ -> HU.assertEqual "none" Set.empty (Game.keywordsOf oid gs),
                 HU.testCase "an unknown id has no keywords" $
                   HU.assertEqual "none" Set.empty (Game.keywordsOf (ObjectId.MkObjectId 999) gs0),
                 -- Flying is on Bird Maiden and NOT on Nimble Birdsticker. If this
                 -- passes while the reach case above also passes, the two keywords
                 -- are genuinely distinct rather than one flag.
                 HU.testCase "reach is not flying" $
                   let (oid, gs) = addCreature Card.nimbleBirdstickerPrinting alice gs0
                    in HU.assertBool "no flying" (not (Game.hasKeyword Keyword.Flying oid gs))
               ]
        )
```

Add the card-data group, and add `m2aCardTests` to `testTree`:

```haskell
redCost :: [ManaSymbol.ManaSymbol] -> Maybe ManaCost.ManaCost
redCost symbols = Just (ManaCost.MkManaCost symbols)

m2aCardTests :: Tasty.TestTree
m2aCardTests =
  let card = Printing.card
      red = ManaSymbol.OfType (ManaType.Colored Color.Red)
   in Tasty.testGroup
        "M2aCards"
        [ HU.testCase "Bird Maiden is a {2}{R} 1/2 Human Bird with flying" $ do
            HU.assertEqual "name" (Text.pack "Bird Maiden") (Card.Type.name (card Card.birdMaidenPrinting))
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 2, red]) (Card.Type.manaCost (card Card.birdMaidenPrinting))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 1))) (Card.Type.power (card Card.birdMaidenPrinting))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness (card Card.birdMaidenPrinting))
            HU.assertEqual
              "subtypes"
              (Set.fromList [Subtype.Human, Subtype.Bird])
              (TypeLine.subtypes (Card.Type.typeLine (card Card.birdMaidenPrinting))),
          HU.testCase "Nimble Birdsticker is a {2}{R} 2/3 Goblin with reach" $ do
            HU.assertEqual "name" (Text.pack "Nimble Birdsticker") (Card.Type.name (card Card.nimbleBirdstickerPrinting))
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 2, red]) (Card.Type.manaCost (card Card.nimbleBirdstickerPrinting))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (card Card.nimbleBirdstickerPrinting))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 3))) (Card.Type.toughness (card Card.nimbleBirdstickerPrinting)),
          HU.testCase "Ogre Sentry is a {1}{R} 3/3 Ogre Warrior with defender" $ do
            HU.assertEqual "name" (Text.pack "Ogre Sentry") (Card.Type.name (card Card.ogreSentryPrinting))
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 1, red]) (Card.Type.manaCost (card Card.ogreSentryPrinting))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 3))) (Card.Type.power (card Card.ogreSentryPrinting))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 3))) (Card.Type.toughness (card Card.ogreSentryPrinting)),
          HU.testCase "Windseeker Centaur is a {1}{R}{R} 2/2 Centaur with vigilance" $ do
            HU.assertEqual "name" (Text.pack "Windseeker Centaur") (Card.Type.name (card Card.windseekerCentaurPrinting))
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 1, red, red]) (Card.Type.manaCost (card Card.windseekerCentaurPrinting))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (card Card.windseekerCentaurPrinting))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness (card Card.windseekerCentaurPrinting)),
          HU.testCase "Goblin Chariot is a {2}{R} 2/2 Goblin Warrior with haste" $ do
            HU.assertEqual "name" (Text.pack "Goblin Chariot") (Card.Type.name (card Card.goblinChariotPrinting))
            HU.assertEqual "cost" (redCost [ManaSymbol.Generic 2, red]) (Card.Type.manaCost (card Card.goblinChariotPrinting))
            HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (card Card.goblinChariotPrinting))
            HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness (card Card.goblinChariotPrinting)),
          HU.testCase "all five are creatures and none is a land" $
            HU.assertBool "creatures" $
              all
                (\(p, _) -> Card.isCreature (card p) && not (Card.isLand (card p)))
                m2aPrintings
        ]
```

- [ ] **Step 2: Run to verify it fails**

Run: `cabal test 2>&1 > /tmp/t.txt; grep -m3 error /tmp/t.txt`
Expected: FAIL — `Pawl.Type.Keyword` not found.

- [ ] **Step 3: Create `Pawl.Type.Keyword`**

`source/library/Pawl/Type/Keyword.hs`:

```haskell
module Pawl.Type.Keyword where

-- CR 702. A keyword is a CITATION, not an effect: rule 702 is part of the
-- comprehensive rules, the same as rule 506 or rule 302.
--
-- Casing on this is NOT a violation of the closed/open invariant. That invariant
-- forbids the rules core casing on the IDENTITY OF AN EFFECT; a keyword is a
-- numbered rule, so `case keyword of Flying -> ...` is the same kind of act as
-- casing on Phase. The test is "is it in the rulebook?" -- Flying is 702.9;
-- Goblin Piker is not in the rulebook. See the M2a spec, section 1, before
-- "fixing" this into a classification.
--
-- Constructors are ordered by RULE NUMBER, not by arrival, so this type stays
-- diffable against rule 702 itself. Five, because five have consumers -- M2b
-- inserts FirstStrike (702.7) and DoubleStrike (702.4); M2c inserts Deathtouch
-- (702.2) and Trample (702.19).
--
-- Grows a parameterized constructor at the punchlist: Landwalk Subtype (702.14),
-- and later Protection Quality (702.16) and Ward Cost (702.21). A `data`, not an
-- enum, so that is an addition rather than a reshape.
data Keyword
  = Defender -- 702.3
  | Flying -- 702.9
  | Haste -- 702.10
  | Reach -- 702.17
  | Vigilance -- 702.20
  deriving (Eq, Ord, Show)
```

- [ ] **Step 4: Add the field to `Card`, and the subtypes**

`source/library/Pawl/Type/Card.hs` — replace the whole file:

```haskell
module Pawl.Type.Card where

import Data.Set (Set)
import Data.Text (Text)
import Pawl.Type.Keyword (Keyword)
import Pawl.Type.ManaCost (ManaCost)
import Pawl.Type.Power (Power)
import Pawl.Type.Toughness (Toughness)
import Pawl.Type.TypeLine (TypeLine)

data Card = MkCard
  { name :: Text,
    -- Nothing, not a zero cost: CR 202.1, a land has no mana cost at all.
    manaCost :: Maybe ManaCost,
    typeLine :: TypeLine,
    -- Only creatures have these.
    power :: Maybe Power,
    toughness :: Maybe Toughness,
    -- CR 702. A Set because CR 702.9c and 702.3c say multiple instances are
    -- redundant -- a per-keyword fact, true of everything through M2c, and NOT
    -- true out in the tail (two Wards both trigger; Rampage stacks).
    --
    -- The closed half must read this through Pawl.Game.keywordsOf, never
    -- directly: layer 6 grants and removes abilities at M3.
    keywords :: Set Keyword
  }
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/Subtype.hs` — replace the whole file:

```haskell
module Pawl.Type.Subtype where

-- Grows: other land types, other creature types, …
data Subtype
  = Mountain
  | Goblin
  | Warrior
  | Human
  | Bird
  | Ogre
  | Centaur
  deriving (Eq, Ord, Show)
```

- [ ] **Step 5: Add `keywords` to the existing printings**

In `source/library/Pawl/Card.hs`, add `Card.keywords = Set.empty,` to **both** `Card.MkCard` records — `mountainPrinting`'s and `pikerPrinting`'s — after their `Card.toughness` line. These are the only two `Card.MkCard` sites in the repo; `-Werror` will confirm.

- [ ] **Step 6: Add the five printings**

Append to `source/library/Pawl/Card.hs`. Add the import first:

```haskell
import qualified Pawl.Type.Keyword as Keyword
```

```haskell
-- The M2a keyword cards. Each is mono-red, castable from the 36-Mountain mana
-- base, and genuinely vanilla-plus-one-keyword -- its entire behavior is its type
-- line, cost, P/T and one rule 702 citation, so all of them need zero opcodes.
--
-- Every one verified against Scryfall (api.scryfall.com/cards/named?exact=...).
-- The dumps in _scratch/ are other projects' working data: fine for FINDING a
-- candidate, never for confirming one.

-- Bird Maiden: {2}{R}, Creature - Human Bird, 1/2, Flying.
-- The cheapest vanilla red flier that exists -- there is none at {1}{R}. Its 1/2
-- body is deliberate: distinguishable from a Piker's 2/1 by P/T alone, and it
-- trades with one.
birdMaidenPrinting :: Printing.Printing
birdMaidenPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Bird Maiden",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 2,
                      ManaSymbol.OfType (ManaType.Colored Color.Red)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.fromList [Subtype.Human, Subtype.Bird]
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 1)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 2)),
            Card.keywords = Set.singleton Keyword.Flying
          }
    }

-- Nimble Birdsticker: {2}{R}, Creature - Goblin, 2/3, Reach.
-- A Goblin with reach, which is faintly ridiculous and entirely real. It is the
-- FALSIFIER for flying: it blocks a flier without having flying, so any
-- implementation that asks "does the blocker have flying?" fails against it.
nimbleBirdstickerPrinting :: Printing.Printing
nimbleBirdstickerPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Nimble Birdsticker",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 2,
                      ManaSymbol.OfType (ManaType.Colored Color.Red)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.singleton Subtype.Goblin
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 2)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 3)),
            Card.keywords = Set.singleton Keyword.Reach
          }
    }

-- Ogre Sentry: {1}{R}, Creature - Ogre Warrior, 3/3, Defender.
-- A 3/3 on purpose: a defender that died to everything would let "a creature with
-- defender may still block" pass vacuously.
ogreSentryPrinting :: Printing.Printing
ogreSentryPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Ogre Sentry",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 1,
                      ManaSymbol.OfType (ManaType.Colored Color.Red)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.fromList [Subtype.Ogre, Subtype.Warrior]
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 3)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 3)),
            Card.keywords = Set.singleton Keyword.Defender
          }
    }

-- Windseeker Centaur: {1}{R}{R}, Creature - Centaur, 2/2, Vigilance.
-- Chosen over Yotian Soldier ({3}, 1/4, also vigilance): the Soldier is an
-- ARTIFACT creature, which would drag the artifact card type and colorless
-- casting into the milestone that is supposed to be proving one thing.
windseekerCentaurPrinting :: Printing.Printing
windseekerCentaurPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Windseeker Centaur",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 1,
                      ManaSymbol.OfType (ManaType.Colored Color.Red),
                      ManaSymbol.OfType (ManaType.Colored Color.Red)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.singleton Subtype.Centaur
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 2)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 2)),
            Card.keywords = Set.singleton Keyword.Vigilance
          }
    }

-- Goblin Chariot: {2}{R}, Creature - Goblin Warrior, 2/2, Haste.
goblinChariotPrinting :: Printing.Printing
goblinChariotPrinting =
  Printing.MkPrinting
    { Printing.card =
        Card.MkCard
          { Card.name = Text.pack "Goblin Chariot",
            Card.manaCost =
              Just
                ( ManaCost.MkManaCost
                    [ ManaSymbol.Generic 2,
                      ManaSymbol.OfType (ManaType.Colored Color.Red)
                    ]
                ),
            Card.typeLine =
              TypeLine.MkTypeLine
                { TypeLine.supertypes = Set.empty,
                  TypeLine.types = Set.singleton CardType.Creature,
                  TypeLine.subtypes = Set.fromList [Subtype.Goblin, Subtype.Warrior]
                },
            Card.power = Just (Power.MkPower (Quantity.Literal 2)),
            Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 2)),
            Card.keywords = Set.singleton Keyword.Haste
          }
    }
```

- [ ] **Step 7: Add the query to `Pawl.Game`**

In `source/library/Pawl/Game.hs`, add the import:

```haskell
import Pawl.Type.Keyword (Keyword)
```

Append, next to `controllerOf` — these belong together as "facts about an object":

```haskell
-- The keywords an object currently has (CR 702). Empty when the id is unknown.
--
-- A function, not a field read, and that is the whole point. Today this is
-- provably Card.keywords of the object's printing -- nothing in M2a grants or
-- removes an ability -- so reading the field directly from Pawl.Combat would
-- compile and pass every test. It would also be wrong in a dozen call sites at
-- once the moment Magical Hack and Humility arrive.
--
-- EXPIRES at M3: layer 6 grants and removes abilities, at which point this
-- consults the layer system. Everything that needs a keyword calls this and never
-- Card.keywords, so that change is one function rather than every call site. Same
-- move as controllerOf, and as M1a's Mana.manaSources.
keywordsOf :: ObjectId -> GameState -> Set Keyword
keywordsOf oid gs = case cardOf oid gs of
  Nothing -> Set.empty
  Just card -> Card.keywords card

hasKeyword :: Keyword -> ObjectId -> GameState -> Bool
hasKeyword keyword oid gs = Set.member keyword (keywordsOf oid gs)
```

- [ ] **Step 8: Sync, build, test**

Run: `cabal-gild --mode format --input pawl.cabal --output pawl.cabal`, then `cabal build all --enable-tests --enable-benchmarks`, then `cabal test --test-options='-p "/Keyword/"'` and `cabal test --test-options='-p "/M2aCards/"'`.
Expected: PASS (9 cases, then 6 cases).

Then `cabal test`. Expected: all pass — nothing consumes keywords yet, so M1b's 137 are untouched.

- [ ] **Step 9: Lint and commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git commit -m "Add the keyword seam and the five M2a printings"
```

---

### Task 2: Defender

CR 702.3b, in `canAttack`. The unary shape.

**Files:**
- Modify: `source/library/Pawl/Combat.hs`, `source/test-suite/Main.hs`

**Interfaces:**
- Consumes: Task 1's `Game.hasKeyword`, `Card.ogreSentryPrinting`, `addCreature`.
- Produces: test fixture `combatBoardOf :: [Printing] -> [Printing] -> (GameState, [ObjectId], [ObjectId])`

- [ ] **Step 1: Write the failing test**

Add this fixture, and **redefine `combatBoard` in terms of it** so M1b's existing call sites keep working unchanged:

```haskell
-- alice is active with one Settled creature per printing in `mine`; bob defends
-- with one per printing in `theirs`. Returns their ids alongside the state, in
-- the order the printings were given.
combatBoardOf :: [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
combatBoardOf mine theirs =
  let addAll pid ps gs =
        List.foldl'
          (\(ids, g) p -> let (oid, g1) = addCreature p pid g in (ids ++ [oid], g1))
          ([], gs)
          ps
      (ours, gs1) = addAll alice mine (Setup.emptyGame bothPlayers)
      (yours, gs2) = addAll bob theirs gs1
   in ( gs2
          { GameState.activePlayer = alice,
            GameState.phase = Phase.Combat CombatStep.DeclareAttackers
          },
        ours,
        yours
      )

combatBoard :: Int -> Int -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
combatBoard a b = combatBoardOf (replicate a Card.pikerPrinting) (replicate b Card.pikerPrinting)
```

Delete `combatBoard`'s old body (the one with `addN`).

Add this group, and add `defenderTests` to `testTree`:

```haskell
defenderTests :: Tasty.TestTree
defenderTests =
  Tasty.testGroup
    "Defender"
    [ HU.testCase "CR 702.3b a creature with defender can't attack" $
        let (gs, mine, _) = combatBoardOf [Card.ogreSentryPrinting] [Card.pikerPrinting]
         in case mine of
              [] -> HU.assertFailure "fixture should have one creature"
              oid : _ -> HU.assertBool "can't attack" (not (Combat.canAttack alice oid gs)),
      HU.testCase "CR 702.3b a creature with defender is not offered as a legal attacker" $
        let (gs, _, _) = combatBoardOf [Card.ogreSentryPrinting] [Card.pikerPrinting]
         in HU.assertEqual "none" [] (Combat.legalAttackers alice gs),
      HU.testCase "CR 702.3b defender does not stop it blocking" $
        -- 702.3b says "can't attack" and nothing else. A defender that could not
        -- block would be a Wall in the pre-2004 sense, and that is not the rule.
        let (gs, _, theirs) = combatBoardOf [Card.pikerPrinting] [Card.ogreSentryPrinting]
         in case theirs of
              [] -> HU.assertFailure "fixture should have one blocker"
              oid : _ -> HU.assertBool "may block" (Combat.canBlock bob oid gs),
      HU.testCase "a creature without defender is still offered" $
        -- The control. If defender were implemented as "nothing may attack", the
        -- test above would pass and this one would fail.
        let (gs, mine, _) = combatBoardOf [Card.pikerPrinting] [Card.pikerPrinting]
         in HU.assertEqual "one" mine (Combat.legalAttackers alice gs),
      HU.testCase "CR 702.3b a defender is skipped but its neighbor still attacks" $
        let (gs, mine, _) = combatBoardOf [Card.ogreSentryPrinting, Card.pikerPrinting] [Card.pikerPrinting]
         in case mine of
              [_, piker] -> HU.assertEqual "only the piker" [piker] (Combat.legalAttackers alice gs)
              _ -> HU.assertFailure "fixture should have two creatures"
    ]
```

- [ ] **Step 2: Run to verify it fails**

Run: `cabal test --test-options='-p "/Defender/"' 2>&1 > /tmp/t.txt; grep -m3 -E "FAIL|error" /tmp/t.txt`
Expected: FAIL — the Ogre Sentry is offered as a legal attacker.

- [ ] **Step 3: Implement**

In `source/library/Pawl/Combat.hs`, add the import:

```haskell
import qualified Pawl.Type.Keyword as Keyword
```

Add one clause to `canAttack`, immediately after the `isCreatureObject` line:

```haskell
canAttack :: PlayerId -> ObjectId -> GameState -> Bool
canAttack pid oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj ->
    Game.controllerOf oid gs == Just pid
      && GameState.activePlayer gs == pid
      && Object.zone obj == Zone.Battlefield
      && Object.tapped obj == TapState.Untapped
      && Object.sickness obj == Sickness.Settled
      && isCreatureObject oid gs
      -- CR 702.3b: a creature with defender can't attack. It may still block --
      -- 702.3b says nothing about blocking.
      && not (Game.hasKeyword Keyword.Defender oid gs)
```

- [ ] **Step 4: Run to verify it passes**

Run: `cabal test --test-options='-p "/Defender/"' 2>&1 > /tmp/t.txt; grep -E "^All|FAIL" /tmp/t.txt`
Expected: PASS (5 cases).

Then `cabal test`. Expected: all pass.

- [ ] **Step 5: Lint and commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git commit -m "Add defender"
```

---

### Task 3: Vigilance

CR 702.20b, in `declareAttackers`. The action-modifier shape.

**Files:**
- Modify: `source/library/Pawl/Combat.hs`, `source/test-suite/Main.hs`

**Interfaces:**
- Consumes: Task 1's `Game.hasKeyword`, `Card.windseekerCentaurPrinting`; Task 2's `combatBoardOf`.
- Produces: test helper `tapStateOf :: ObjectId -> GameState -> Maybe TapState`

- [ ] **Step 1: Write the failing test**

Add this helper and group, and add `vigilanceTests` to `testTree`:

```haskell
tapStateOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe TapState.TapState
tapStateOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)

vigilanceTests :: Tasty.TestTree
vigilanceTests =
  Tasty.testGroup
    "Vigilance"
    [ HU.testCase "CR 702.20b attacking doesn't tap a creature with vigilance, but does tap its neighbor" $
        -- Both creatures in ONE declaration, so a blanket "nothing taps" bug
        -- cannot pass: the Piker must still tap.
        let (gs, mine, _) = combatBoardOf [Card.windseekerCentaurPrinting, Card.pikerPrinting] [Card.pikerPrinting]
            after = snd (Engine.runGamePure aggressiveAnswer gs (Combat.declareAttackers alice))
         in case mine of
              [centaur, piker] -> do
                HU.assertEqual "both attacking" 2 (length (declaredAttackers after))
                HU.assertEqual "the centaur is untapped" (Just TapState.Untapped) (tapStateOf centaur after)
                HU.assertEqual "the piker is tapped" (Just TapState.Tapped) (tapStateOf piker after)
              _ -> HU.assertFailure "fixture should have two attackers",
      HU.testCase "CR 702.20b vigilance still attacks" $
        -- Vigilance is not a legality question: the creature is declared as an
        -- attacker exactly as normal. It simply skips CR 508.1f's tap.
        let (gs, mine, _) = combatBoardOf [Card.windseekerCentaurPrinting] [Card.pikerPrinting]
            after = snd (Engine.runGamePure aggressiveAnswer gs (Combat.declareAttackers alice))
         in HU.assertEqual "attacking" mine (declaredAttackers after),
      HU.testCase "CR 702.20b an untapped vigilant attacker can still be blocked" $
        -- It is attacking, so it is in the Combat record, tapped or not.
        let (gs, mine, theirs) = combatBoardOf [Card.windseekerCentaurPrinting] [Card.pikerPrinting]
            steps = do
              Combat.declareAttackers alice
              Combat.declareBlockers
            after = snd (Engine.runGamePure aggressiveAnswer gs steps)
         in case mine of
              [] -> HU.assertFailure "fixture should have an attacker"
              attacker : _ -> HU.assertEqual "blocked" (Set.fromList theirs) (Combat.blockersOf attacker after)
    ]
```

- [ ] **Step 2: Run to verify it fails**

Run: `cabal test --test-options='-p "/Vigilance/"' 2>&1 > /tmp/t.txt; grep -m3 -E "FAIL|expected" /tmp/t.txt`
Expected: FAIL — the centaur is tapped.

- [ ] **Step 3: Implement**

In `source/library/Pawl/Combat.hs`, replace `declareAttackers`' `tapIt` binding:

```haskell
            -- CR 508.1f: declaring an attacker taps it -- unless it has vigilance
            -- (CR 702.20b), which does not change WHETHER it attacks, only what
            -- attacking does to it.
            tapIt g oid =
              if Game.hasKeyword Keyword.Vigilance oid g
                then g
                else g {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects g)}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cabal test --test-options='-p "/Vigilance/"' 2>&1 > /tmp/t.txt; grep -E "^All|FAIL" /tmp/t.txt`
Expected: PASS (3 cases).

Then `cabal test`. Expected: all pass — M1b's "CR 508.1f declaring an attacker taps it" uses Pikers and is unaffected.

- [ ] **Step 5: Lint and commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git commit -m "Add vigilance"
```

---

### Task 4: Haste

CR 702.10b, in `canAttack`. The state-override shape.

**Files:**
- Modify: `source/library/Pawl/Combat.hs`, `source/test-suite/Main.hs`

**Interfaces:**
- Consumes: Task 1's `Game.hasKeyword`, `Card.goblinChariotPrinting`; Task 2's `combatBoardOf`.
- Produces: test fixture `justArrived :: GameState -> GameState`

- [ ] **Step 1: Write the failing test**

Add this fixture and group, and add `hasteTests` to `testTree`:

```haskell
-- Re-sicken alice's creatures, as though they had just resolved this turn.
justArrived :: GameState.GameState -> GameState.GameState
justArrived gs =
  let sicken o = if Object.owner o == alice then o {Object.sickness = Sickness.Sick} else o
   in gs {GameState.objects = Map.map sicken (GameState.objects gs)}

hasteTests :: Tasty.TestTree
hasteTests =
  Tasty.testGroup
    "Haste"
    [ HU.testCase "CR 702.10b a creature with haste attacks the turn it arrives" $
        let (gs, _, _) = combatBoardOf [Card.goblinChariotPrinting] [Card.pikerPrinting]
            after = snd (Engine.runGamePure aggressiveAnswer (justArrived gs) (Combat.declareAttackers alice))
         in HU.assertEqual "attacks" 1 (length (declaredAttackers after)),
      HU.testCase "CR 302.6 the same creature without haste cannot" $
        -- The control. Goblin Chariot and Goblin Piker are both 2/2-ish Goblin
        -- Warriors; the ONLY difference the engine can see is the keyword.
        let (gs, _, _) = combatBoardOf [Card.pikerPrinting] [Card.pikerPrinting]
            after = snd (Engine.runGamePure aggressiveAnswer (justArrived gs) (Combat.declareAttackers alice))
         in HU.assertEqual "cannot attack" [] (declaredAttackers after),
      HU.testCase "CR 702.10b haste is not needed once the creature has settled" $
        let (gs, mine, _) = combatBoardOf [Card.pikerPrinting] [Card.pikerPrinting]
            after = snd (Engine.runGamePure aggressiveAnswer gs (Combat.declareAttackers alice))
         in HU.assertEqual "attacks" mine (declaredAttackers after),
      HU.testCase "CR 702.10b a hasty creature and a sick one, in the same declaration" $
        -- Both sick; only the Chariot may attack. A blanket "sickness ignored"
        -- bug would let both through.
        let (gs, mine, _) = combatBoardOf [Card.goblinChariotPrinting, Card.pikerPrinting] [Card.pikerPrinting]
            after = snd (Engine.runGamePure aggressiveAnswer (justArrived gs) (Combat.declareAttackers alice))
         in case mine of
              [chariot, _] -> HU.assertEqual "only the chariot" [chariot] (declaredAttackers after)
              _ -> HU.assertFailure "fixture should have two creatures"
    ]
```

- [ ] **Step 2: Run to verify it fails**

Run: `cabal test --test-options='-p "/Haste/"' 2>&1 > /tmp/t.txt; grep -m3 -E "FAIL|expected" /tmp/t.txt`
Expected: FAIL — the Goblin Chariot does not attack.

- [ ] **Step 3: Implement**

In `source/library/Pawl/Combat.hs`, replace `canAttack`'s sickness clause:

```haskell
      -- CR 302.6, relaxed by CR 702.10b: a creature with haste can attack even if
      -- it hasn't been controlled continuously since its controller's most recent
      -- turn began.
      && (Object.sickness obj == Sickness.Settled || Game.hasKeyword Keyword.Haste oid gs)
```

Note CR 702.10c — haste also frees activated abilities with the tap symbol from CR 302.6 — is **not** implemented. There are no activated abilities, so it has no consumer; unlike M1b's CR 704.5f, omitting it makes nothing partial. EXPIRES at M4.

- [ ] **Step 4: Run to verify it passes**

Run: `cabal test --test-options='-p "/Haste/"' 2>&1 > /tmp/t.txt; grep -E "^All|FAIL" /tmp/t.txt`
Expected: PASS (4 cases).

Then `cabal test`. Expected: all pass — M1b's summoning-sickness tests use Pikers, which have no haste.

- [ ] **Step 5: Lint and commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git commit -m "Add haste"
```

---

### Task 5: Flying, reach, and CR 509.1b declaration legality

**This is the risky task.** Evasion is a restriction on the whole declaration, and per-pair filtering is the natural way to write it — and is what M1b currently does.

**Files:**
- Modify: `source/library/Pawl/Combat.hs`, `source/test-suite/Main.hs`

**Interfaces:**
- Consumes: Task 1's `Game.hasKeyword`, `Card.birdMaidenPrinting`, `Card.nimbleBirdstickerPrinting`; Task 2's `combatBoardOf`.
- Produces:
  - `Pawl.Combat.evasionAllows :: ObjectId -> ObjectId -> GameState -> Bool`
  - `Pawl.Combat.legalBlockDeclaration :: PlayerId -> Map ObjectId ObjectId -> GameState -> Bool`

`canBlock` and `legalBlockers` are **unchanged** — they are CR 509.1a, which is about the blocker alone. `Prompt.DeclareBlockers` is **unchanged**.

- [ ] **Step 1: Write the failing test**

Add this helper and group, and add `evasionTests` to `testTree`:

```haskell
-- Declare attackers with everything, then hand back the state and the ids.
attacking :: [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
attacking mine theirs =
  let (gs, ours, yours) = combatBoardOf mine theirs
      after = snd (Engine.runGamePure aggressiveAnswer gs (Combat.declareAttackers alice))
   in (after, ours, yours)

evasionTests :: Tasty.TestTree
evasionTests =
  Tasty.testGroup
    "Evasion"
    [ HU.testCase "CR 702.9b a declaration in which a ground creature blocks a flier is illegal" $
        let (gs, mine, theirs) = attacking [Card.birdMaidenPrinting] [Card.pikerPrinting]
         in case (mine, theirs) of
              (a : _, b : _) ->
                HU.assertBool "illegal" (not (Combat.legalBlockDeclaration bob (Map.singleton b a) gs))
              _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 702.17b a reach creature may block a flier" $
        -- THE FALSIFIER. Fails against any implementation that asks "does the
        -- blocker have flying?"
        let (gs, mine, theirs) = attacking [Card.birdMaidenPrinting] [Card.nimbleBirdstickerPrinting]
         in case (mine, theirs) of
              (a : _, b : _) ->
                HU.assertBool "legal" (Combat.legalBlockDeclaration bob (Map.singleton b a) gs)
              _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 702.9b a flier may block a ground creature" $
        -- The asymmetry: 702.9b's second sentence. Fails if flying is implemented
        -- as a symmetric predicate.
        let (gs, mine, theirs) = attacking [Card.pikerPrinting] [Card.birdMaidenPrinting]
         in case (mine, theirs) of
              (a : _, b : _) ->
                HU.assertBool "legal" (Combat.legalBlockDeclaration bob (Map.singleton b a) gs)
              _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 702.9b a flier may block a flier" $
        let (gs, mine, theirs) = attacking [Card.birdMaidenPrinting] [Card.birdMaidenPrinting]
         in case (mine, theirs) of
              (a : _, b : _) ->
                HU.assertBool "legal" (Combat.legalBlockDeclaration bob (Map.singleton b a) gs)
              _ -> HU.assertFailure "fixture should have an attacker and a blocker",
      HU.testCase "CR 509.1a a ground creature is still a legal blocker while a flier attacks" $
        -- 509.1a is about the blocker ALONE: it can block SOMETHING. This test
        -- fails if evasion is wrongly implemented as a filter on the candidates.
        let (gs, _, theirs) = attacking [Card.birdMaidenPrinting] [Card.pikerPrinting]
         in HU.assertEqual "still offered" theirs (Combat.legalBlockers bob gs),
      HU.testCase "CR 509.1b an illegal declaration is rejected WHOLE, not repaired" $
        -- aggressiveAnswer blocks the first attacker with EVERYTHING, so bob
        -- declares the reach creature (legal) AND the Piker (illegal) on the
        -- flier. Neither may block. A per-pair filter would drop the Piker and
        -- let the Birdsticker's block stand -- which is what M1b does today, and
        -- is unsound: under menace, dropping one blocker from a pair manufactures
        -- an illegal single block.
        let (gs, _, _) = combatBoardOf [Card.birdMaidenPrinting] [Card.nimbleBirdstickerPrinting, Card.pikerPrinting]
            steps = do
              Combat.declareAttackers alice
              Combat.declareBlockers
            after = snd (Engine.runGamePure aggressiveAnswer gs steps)
         in case Map.keys (Combat.Type.attackers (GameState.combat after)) of
              [] -> HU.assertFailure "fixture should have an attacker"
              a : _ -> HU.assertEqual "nobody blocks" Set.empty (Combat.blockersOf a after),
      HU.testCase "CR 509.1b a wholly legal declaration is accepted" $
        -- The control for the test above: with only the reach creature, the same
        -- interpreter produces a legal declaration and the block stands.
        let (gs, _, theirs) = combatBoardOf [Card.birdMaidenPrinting] [Card.nimbleBirdstickerPrinting]
            steps = do
              Combat.declareAttackers alice
              Combat.declareBlockers
            after = snd (Engine.runGamePure aggressiveAnswer gs steps)
         in case Map.keys (Combat.Type.attackers (GameState.combat after)) of
              [] -> HU.assertFailure "fixture should have an attacker"
              a : _ -> HU.assertEqual "the reach creature blocks" (Set.fromList theirs) (Combat.blockersOf a after),
      HU.testCase "CR 509.1a a Mountain is not a legal blocker, flier or no flier" $
        -- The classification, from the other side: `canBlock` asks
        -- is-it-a-creature, never which card it is. M1b tests "a land may not
        -- attack" but never that a land may not BLOCK, so this closes a real gap
        -- rather than restating one.
        let (gs, mine, _) = attacking [Card.birdMaidenPrinting] []
            withLand = snd (addCreature Card.mountainPrinting bob gs)
         in case mine of
              [] -> HU.assertFailure "fixture should have an attacker"
              _ : _ -> HU.assertEqual "no legal blockers" [] (Combat.legalBlockers bob withLand),
      HU.testCase "CR 702.9b a flier connects past an untapped ground creature, in a real combat" $
        -- The integration case, and it is precise rather than vacuous. WITH
        -- flying: nothing may block, bob takes 1, and both creatures live.
        -- WITHOUT flying: the Piker blocks, bob takes 0, and the two TRADE (Bird
        -- Maiden is 1/2, Piker is 2/1). All three assertions distinguish them.
        let (gs, _, _) = combatBoardOf [Card.birdMaidenPrinting] [Card.pikerPrinting]
            after = Sba.checkStateBasedActions (fightWith aggressiveAnswer gs)
         in do
              HU.assertEqual "bob took 1" (Just 19) (lifeOf bob after)
              HU.assertEqual "the flier lives" 1 (creaturesInPlay alice after)
              HU.assertEqual "the would-be blocker lives" 1 (creaturesInPlay bob after)
    ]
```

- [ ] **Step 2: Run to verify it fails**

Run: `cabal test --test-options='-p "/Evasion/"' 2>&1 > /tmp/t.txt; grep -m3 -E "FAIL|error" /tmp/t.txt`
Expected: FAIL — `Combat.legalBlockDeclaration` not in scope.

- [ ] **Step 3: Implement the restriction check**

In `source/library/Pawl/Combat.hs`, add the import:

```haskell
import Data.Map.Strict (Map)
```

Append:

```haskell
-- CR 702.9b: a creature with flying can't be blocked except by creatures with
-- flying and/or reach (CR 702.17b).
--
-- Note the asymmetry, which is easy to get backwards: 702.9b's second sentence
-- says a creature with flying CAN block a creature with or without flying.
-- Flying restricts being blocked, never blocking. The question is asked of the
-- ATTACKER first, and only then of the blocker.
evasionAllows :: ObjectId -> ObjectId -> GameState -> Bool
evasionAllows blocker attacker gs =
  not (Game.hasKeyword Keyword.Flying attacker gs)
    || Game.hasKeyword Keyword.Flying blocker gs
    || Game.hasKeyword Keyword.Reach blocker gs

-- CR 509.1b: the defending player checks each creature for RESTRICTIONS, and if
-- any are disobeyed the DECLARATION is illegal.
--
-- The unit of legality is the whole declaration, not the pair, and that is not a
-- stylistic choice. Menace (CR 702.111, one punchlist entry away) says a creature
-- can't be blocked except by TWO OR MORE creatures -- a constraint on the SET
-- blocking an attacker, which no per-pair predicate can express. Only flying and
-- reach are pairwise; designing to them would be designing to the case that
-- misleads. See the M2a spec, section 3.
--
-- A conjunction of independent restriction checks, because CR 509.1b says
-- different evasion abilities are cumulative: an attacker with flying AND shadow
-- admits only blockers that answer both.
--
-- CR 509.1c REQUIREMENTS ("must block if able") are NOT implemented, and are not
-- a check but a maximization: 509.1c demands the declaration obey the maximum
-- possible number of requirements achievable without disobeying any restriction.
-- Nothing in M2a creates a requirement, so that maximum is trivially zero. This
-- function is named for restrictions so requirements arrive as a SECOND function
-- rather than as a surprise inside this one. EXPIRES with the first requirement,
-- which also invalidates declareBlockers' fallback -- see there.
legalBlockDeclaration :: PlayerId -> Map ObjectId ObjectId -> GameState -> Bool
legalBlockDeclaration pid declaration gs =
  let attackers = Map.keys (Combat.attackers (GameState.combat gs))
      candidates = legalBlockers pid gs
      -- CR 509.1a: the blocker must be one this player could block with at all,
      -- and the attacker must actually be attacking.
      wellFormed blocker attacker = List.elem blocker candidates && List.elem attacker attackers
      ok (blocker, attacker) = wellFormed blocker attacker && evasionAllows blocker attacker gs
   in all ok (Map.toList declaration)
```

- [ ] **Step 4: Replace the filter with whole-declaration validation**

In `source/library/Pawl/Combat.hs`, replace the body of `declareBlockers`' inner `Monad.unless`:

```haskell
      Monad.unless (null candidates) $ do
        let decider = Decide.deciderFor pid gs
        chosen <- Trans.lift (Program.prompt (Prompt.DeclareBlockers decider pid candidates attacking))
        -- CR 509.1b: an illegal declaration is illegal AS A WHOLE. It is NOT
        -- filtered down to its legal entries -- that is unsound, not merely
        -- inelegant: under menace, dropping one blocker from a pair leaves an
        -- illegal single block, so the filter would manufacture the illegality it
        -- was meant to remove. M1b settled the identical question for CR 510.1e:
        -- "checks the assignment AS A WHOLE, so this cannot be repaired by
        -- filtering the way a discard can."
        --
        -- This is NOT CR 733's rewind. An enforcing engine never offers an
        -- illegal declaration, so only a broken interpreter arrives here, and
        -- re-prompting a pure `Prompt r -> r` returns the identical wrong answer.
        --
        -- Declining to block is always legal today, so "no blocks" is a legal
        -- state to fall back to. EXPIRES with CR 509.1c requirements: once
        -- something must block, "no blocks" can itself be illegal and this
        -- fallback stops being available.
        gs1 <- State.get
        Monad.when (legalBlockDeclaration pid chosen gs1) $ do
          let add m (b, a) = Map.insertWith Set.union a (Set.singleton b) m
              merged = List.foldl' add (Combat.blockers (GameState.combat gs1)) (Map.toList chosen)
          State.modify' $ \g -> g {GameState.combat = (GameState.combat g) {Combat.blockers = merged}}
```

The `legal`/`accepted` bindings are deleted. `legalBlockDeclaration` subsumes them: its `wellFormed` clause is exactly M1b's `legal`, now applied to the declaration rather than used to filter it.

- [ ] **Step 5: Run to verify it passes**

Run: `cabal test --test-options='-p "/Evasion/"' 2>&1 > /tmp/t.txt; grep -E "^All|FAIL" /tmp/t.txt`
Expected: PASS (9 cases).

If "an illegal declaration is rejected WHOLE" fails while the others pass, the filter is still in place. Fix `declareBlockers`, not the test.

Then `cabal test`. Expected: all pass. M1b's `Declare` and `CombatDamage` groups use Pikers only, so every declaration in them is legal and unaffected.

- [ ] **Step 6: Lint and commit**

```bash
git add -A
hooky fix
git add -A
hooky run
git commit -m "Add flying and reach on CR 509.1b declaration legality"
```

---

### Task 6: Bird Maiden joins the deck

The keyword that is visible across a whole random game.

**Files:**
- Modify: `source/library/Pawl/Setup.hs`, `source/test-suite/Main.hs`

**Interfaces:**
- Consumes: everything above.
- Produces: no new library interfaces.

- [ ] **Step 1: Write the failing test**

Add these cases to the existing `deckTests` group:

```haskell
      HU.testCase "8 Bird Maidens per player" $
        HU.assertEqual "maidens" 8 (countByName (Text.pack "Bird Maiden") alice setupState),
      HU.testCase "16 Pikers per player" $
        -- Bird Maiden REPLACES Pikers rather than joining them: the list stays at
        -- 60, so conservation stays at 120 and M1b's property is untouched.
        HU.assertEqual "pikers" 16 (countByName (Text.pack "Goblin Piker") bob setupState),
```

The existing "24 Pikers per player" case is replaced by the 16-Piker case above. That is the deck change landing, not a regression.

- [ ] **Step 2: Run to verify it fails**

Run: `cabal test --test-options='-p "/Deck/"' 2>&1 > /tmp/t.txt; grep -m3 -E "FAIL|expected" /tmp/t.txt`
Expected: FAIL — 0 Bird Maidens, 24 Pikers.

- [ ] **Step 3: Implement**

In `source/library/Pawl/Setup.hs`, replace `deckList`:

```haskell
-- 36 Mountain / 16 Goblin Piker / 8 Bird Maiden: enough lands to cast reliably,
-- enough creatures that a random game exercises casting and combat.
--
-- Bird Maiden REPLACES Pikers rather than joining them, so the list stays at 60
-- and conservation stays at 120 objects. It is the only M2a printing in the deck:
-- flying is the one keyword whose effect is visible across a whole random game,
-- and a printing is not a deck-list entry -- the other four are exercised by
-- fixtures, which is cheaper and gives a deck-composition bug nowhere to hide.
deckList :: [Printing]
deckList =
  replicate 36 Card.mountainPrinting
    ++ replicate 16 Card.pikerPrinting
    ++ replicate 8 Card.birdMaidenPrinting
```

- [ ] **Step 4: Run to verify it passes**

Run: `cabal test --test-options='-p "/Deck/"' 2>&1 > /tmp/t.txt; grep -E "^All|FAIL" /tmp/t.txt`
Expected: PASS (5 cases).

- [ ] **Step 5: Run the properties**

Run: `cabal test --test-options='-p "/Properties/"' 2>&1 > /tmp/t.txt; grep -E "^All|FAIL" /tmp/t.txt`
Expected: PASS (6 properties). **M2a retires no property** — it is the first milestone since M0 that does not.

If "conservation: 120 objects at end" fails, the deck list is not 60 cards. Fix the deck, not the property: the constant is the point.

Then `cabal test` and `cabal bench`. Expected: all pass; the benchmark reports `goldfish 2p`, `casting 2p` and `fighting 2p`.

- [ ] **Step 6: Full verification**

```bash
rm -rf dist-newstyle/build/aarch64-osx/ghc-9.14.1/pawl-0.2026.7.16/{b,t,build}
cabal build all --enable-tests --enable-benchmarks 2>&1 | grep -c "warning:"
```

Expected: `0`.

Then confirm the replay criterion explicitly — the transcript now carries games in which fliers attack and ground creatures fail to block them:

```bash
cabal test --test-options='-p "/Replay/"'
cabal test --test-options='-p "/CombatReplay/"'
```

Expected: PASS. `Prompt` did not change in M2a, so this should be untouched; if it is not, something added a prompt.

- [ ] **Step 7: Commit and close the milestone**

```bash
git add -A
hooky fix
git add -A
hooky run
git commit -m "Add Bird Maiden to the deck"
git-bug bug new -t "M2a — the keyword seam" -m "Complete."
```

---

## Self-review notes (soft spots flagged, not hidden)

1. **Task 5 is the risk, and CR 509.1b is why.** The whole correctness argument is that `legalBlockDeclaration` judges the declaration rather than the pair. The "rejected WHOLE, not repaired" test is the only thing guarding it, and per-pair filtering is both the natural way to write it and what M1b does today. Do not "simplify" the validator into a filter.
2. **The interpreter is blind to evasion legality, and this is a real wrinkle.** `Prompt.DeclareBlockers` carries the candidates and the attackers but no keywords, and `Decider` is only a `PlayerId` — so an interpreter cannot tell which blocks are legal without a `GameState` it does not have. A real client has the state and can call `legalBlockDeclaration`; the test interpreters cannot. Consequence: once Bird Maidens are in the deck (Task 6), `randomAnswer` will sometimes declare a ground creature against a flier and have the whole declaration rejected. That is correct engine behavior and harmless — blocks still happen whenever the first attacker is a Piker, which is most of the time at 16 Pikers to 8 Maidens — but it is why the spec's "fliers get through" property is **not** implemented here: it would pass whether flying worked or the declaration was merely rejected. Task 5's last case tests the same thing deterministically and distinguishes those two outcomes (bob takes 1 and both creatures live, versus bob takes 0 and they trade). Flagged for M2b/M3, when a client with state exists.
3. **`Card` gains a field, so both `Card.MkCard` sites must be updated** — `mountainPrinting` and `pikerPrinting`. They are the only two in the repo. `-Werror` finds them; the churn is expected.
4. **`addPiker` and `combatBoard` are re-expressed in terms of new general fixtures** (`addCreature`, `combatBoardOf`) rather than duplicated. Every M1b call site keeps working unchanged, which is the point — if any M1b test needs editing, the refactor is wrong.
5. **Defender's and haste's tests both rely on a control case** (a Piker doing the opposite), because a blanket bug — "nothing may attack", "sickness ignored" — passes the positive case alone.
6. **`evasionAllows` takes `blocker` before `attacker`**, matching `legalBlockDeclaration`'s `Map` key order (blocker → attacker). The rules read attacker-first ("a creature with flying can't be blocked except by…"), so the argument order is deliberately the opposite of the prose. Getting them backwards type-checks and silently inverts the rule; the "a flier may block a ground creature" test is what catches it.
7. **The spec's §5 classification test is discharged by the card choices, not by a dedicated test**, and this is worth stating because it looks like a gap. The cards were picked so that the engine's *only* visible difference is a rule 702 citation: **Nimble Birdsticker is a Goblin** and blocks a flier, while **Goblin Piker is a Goblin** and cannot (Task 5); **Goblin Chariot is a Goblin Warrior** and attacks the turn it arrives, while **Goblin Piker is a Goblin Warrior** and cannot (Task 4). Same subtypes, same color, same mana base — different keyword, different legality. An implementation that cased on card identity would have to case on cards it cannot tell apart. That is a stronger statement of the invariant than any single assertion, and it is why the fixtures use those pairs rather than more convenient ones.
8. **Task 6 changes a deck constant that a test asserts.** `deckTests`' "24 Pikers per player" becomes 16, and 8 Bird Maidens join. That is the deck change landing and the plan says so explicitly — but `Setup.deckSize`, `openingHand`, and conservation's 120 must all be untouched. If any of those needs editing, the deck change is wrong, not the test.
