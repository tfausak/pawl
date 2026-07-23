# M4.5 P9 — Target-filter predicate language Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the whole family of hand-carved classification-as-data enums
(`TargetSpec`, `CardCriterion`, `PermanentCriterion`, `SpellCriterion`, and
`Affected`'s dynamic sets) with one first-order, statically-analyzable
object-predicate language — `Pawl.Type.Filter` — evaluated by a single generic
matcher, proven by Doom Blade, Terror, and Reprisal.

**Architecture:** `Filter` is closed-half *vocabulary* (it cases on
*characteristics* — card type, colour, subtype, power, controller — never on an
effect's identity). One evaluator, `Filter.matches :: Context -> View -> Filter
-> Bool`, folds a `Filter` over a `View` (the characteristics accessors) supplied
by whichever source fits the subject's zone: a projected battlefield/stack object
(`Projection.viewOfObject`) or a printed card off the battlefield
(`Projection.viewOfCard`). `TargetSpec` becomes a closed `Pool` + an open
`Maybe Filter` + a self-exclusion flag; the three criterions collapse to `Filter`;
`Affected`'s dynamic sets become `Matching Filter`.

**Tech Stack:** Haskell 2010 (GHC 9.14.1, no extensions beyond `GADTs`/`RankNTypes`
in the suspension core and `NamedFieldPuns` where it clarifies), `tasty`
(`tasty-hunit` + `tasty-quickcheck`), cards loaded as JSON data from
`data/cards/`.

## Global Constraints

Copied from CLAUDE.md and the spec — every task's requirements implicitly include
these:

- **The invariant:** the rules core never cases on an effect's *identity*, only on
  *classifications*. `Filter` atoms case on characteristics (a `CardType`, a
  `Color`, a keyword-style axis) — that is the same legitimate act as casing on a
  `CardType`; it is not a violation. The `matches` fold must never learn which
  spell produced a `Filter`.
- **Haskell 2010, no language extensions** unless unavoidable. No `LambdaCase`,
  `OverloadedStrings`. `NamedFieldPuns` permitted for record-heavy clarity.
- **One type per module** under `Pawl.Type.<TypeName>` (type + instances only).
  Logic in other `Pawl.*` modules. A module never imports its parents; a sibling
  `Pawl.Type.*` import is fine. Evaluator-local records may live in a logic module
  (precedent: `Gathered` inside `Pawl.Projection`).
- **No explicit export lists** (`module Pawl.Foo where`).
- **Qualified imports**, aliased to the last component (`Data.Set` → `Set`);
  operators unqualified. `A.B.C` must not import `A.B` or `A`.
- **`newtype` + smart constructors, non-punning:** constructors take a `Mk`
  prefix (`MkView`, `MkContext`). Never pun type and constructor names.
- **No partial functions.** `Maybe`/`Either`, never `head`/`error`/`undefined`/
  non-exhaustive matches.
- **No boolean blindness** — a custom sum type beats a bare `Bool` (this is why
  self-exclusion is a `Pawl.Type.Exclusion` value, not a `Bool`).
- **Derive at least `Eq` and `Show`.** `Filter`, `Pool`, `Exclusion`,
  `PlayerRelation` also derive `Ord` (they sit inside `Set`/`Map` keys and
  `Ord`-deriving `Card`).
- **`Text` not `String`; arbitrary-precision numbers** (`Integer`, not `Int`).
- **Prefer explicit:** `case` over point-free; `do` + record syntax to build
  records; `let` over `where`; `$` over parens, `.` over chained `$`.
- **No API stability obligations.** Delete `CardCriterion` / `PermanentCriterion`
  / `SpellCriterion` outright; no compat shims or re-exports.
- **Build must be warning-clean** under `-Weverything` minus the `pawl.cabal`
  allow-list; `+pedantic` makes any warning a `-Werror` failure. Suites break
  separately from the library, so always build `all`.
- **Every rules claim checked against `docs/rules.txt`**, with the CR number cited
  in the code comment. Never trust recalled Magic rules. The CR numbers this spec
  already verified (2026-07-22): 105.2, 105.2c, 109.2, 109.5, 110.1, 110.4, 112.1,
  115.1a, 115.4, 205.3, 205.4, 205.4c, 208.1, 601.2c, 608.2b, 611.2c, 613.1b,
  700.2c, 701.23a.
- **Cards verified live against Scryfall (2026-07-22), not the vendored dump:**
  Doom Blade `{1}{B}` "Destroy target nonblack creature."; Terror `{1}{B}`
  "Destroy target nonartifact, nonblack creature. It can't be regenerated.";
  Reprisal `{1}{W}` "Destroy target creature with power 4 or greater. It can't be
  regenerated."
- **TDD is not optional:** write each failing test and run it to watch it fail
  before implementing. Never weaken an assertion or delete a test to pass a check.
  If the plan looks wrong, stop and say so.
- **Never edit the plan to make a progress check pass.** The progress gate is
  `grep -c -- '- \[ \] \*\*Step' <this-plan>` reaching `0`.

## Toolchain commands (used verbatim throughout)

- Build: `cabal build all --enable-tests --enable-benchmarks`
- Definitive warning check (incremental builds hide warnings): `cabal clean` first,
  then the build above.
- Full test suite: `cabal test`
- One tasty group by name pattern: `cabal test --test-options='-p "<pattern>"'`
  (e.g. `-p "Pawl.Filter"`). The pattern matches the test-tree path.
- Format + lint (staged files only — `git add -A` first): `hooky fix` then
  `git add -A` again then `hooky run`.
- After adding a `data/cards/*.json` file, it is picked up only once registered in
  `source/test-suite/Pawl/Cards.hs` (three sites: record field, `loadPrinting`
  binding, `allPrintings` list).
- New library modules and new `Pawl.*Spec` modules are auto-discovered by the
  `-- cabal-gild: discover` directives in `pawl.cabal`; run `cabal-gild` (via
  `hooky fix`) — never hand-edit `exposed-modules`/`other-modules`. A new spec
  module ALSO needs manual wiring into `source/test-suite/Main.hs`.

---

## File Structure

**New library modules**

- `source/library/Pawl/Type/PlayerRelation.hs` — `data PlayerRelation = You |
  Opponent`. Who an object's controller is relative to the evaluation's
  perspective.
- `source/library/Pawl/Type/Filter.hs` — `data Filter` — the recursive-but-finite
  object predicate (atoms + `And`/`Or`/`Not`). One type; the core vocabulary.
- `source/library/Pawl/Type/Pool.hs` — `data Pool` — the closed set of target
  recipient kinds (which objects are candidates, and how they're referenced).
- `source/library/Pawl/Type/Exclusion.hs` — `data Exclusion = IncludesSource |
  ExcludesSource` — whether a target slot admits its own source ("another").
- `source/library/Pawl/Filter.hs` — the evaluator (logic, not a type module):
  the `View` record, the `Context` newtype, and `matches`. Depends only on the
  `Pawl.Type.*` leaves — **must not import `Pawl.Projection`** (cycle).

**Reshaped library modules**

- `source/library/Pawl/Type/TargetSpec.hs` — from a 12-constructor enum to
  `data TargetSpec = MkTargetSpec Pool (Maybe Filter) Exclusion`.
- `source/library/Pawl/Type/Affected.hs` — from six constructors to
  `TheseObjects (Set ObjectId) | Matching Filter`.
- `source/library/Pawl/Type/Effect.hs` — `Search` carries `Filter`, not
  `CardCriterion`.
- `source/library/Pawl/Target.hs` — `legalRecipients`/`selfExcludes` over the new
  `TargetSpec`; new private `basePool`.
- `source/library/Pawl/Projection.hs` — new `viewOfObject`, `viewOfCard`, and a
  private `viewOfCharacteristics`; `affects` rewritten to call `Filter.matches`.
- `source/library/Pawl/Cost.hs`, `source/library/Pawl/Replacement.hs`,
  `source/library/Pawl/PlayerEffect.hs`, `source/library/Pawl/Resolve.hs` — their
  criterion matchers become `Filter.matches`.
- `source/library/Pawl/Codec.hs` — new `filterToJson`/`jsonToFilter`,
  `poolToJson`, `exclusionToJson`, `playerRelationToJson` and inverses; reshaped
  `targetSpecToJson`/`jsonToTargetSpec`, `affectedToJson`/`jsonToAffected`,
  `Search` arm; deleted `cardCriterion*`/`permanentCriterion*`/`spellCriterion*`.

**Deleted library modules**

- `source/library/Pawl/Type/CardCriterion.hs`
- `source/library/Pawl/Type/PermanentCriterion.hs`
- `source/library/Pawl/Type/SpellCriterion.hs`

**New test module**

- `source/test-suite/Pawl/FilterSpec.hs` — unit tests for `Filter.matches` (pure,
  hand-built `View`s) and the codec round-trip. Wired into `Main.hs`.

**Card data (JSON) migrated** — every file whose `targetSpecs`/`Search`/criterion
tag changes shape. Enumerated per task.

---

## Task 1: The `Filter` predicate type and its evaluator

**Files:**
- Create: `source/library/Pawl/Type/PlayerRelation.hs`
- Create: `source/library/Pawl/Type/Filter.hs`
- Create: `source/library/Pawl/Filter.hs`
- Create/Test: `source/test-suite/Pawl/FilterSpec.hs`
- Modify: `source/test-suite/Main.hs` (import + `testTree` entry)

**Interfaces:**
- Produces:
  - `Pawl.Type.PlayerRelation.PlayerRelation = You | Opponent` (`Eq, Ord, Show`).
  - `Pawl.Type.Filter.Filter` with constructors `HasCardType CardType`,
    `HasSupertype Supertype`, `HasColor Color`, `HasSubtype Subtype`,
    `PowerAtLeast Integer`, `ControlledBy PlayerRelation`, `And [Filter]`,
    `Or [Filter]`, `Not Filter` (`Eq, Ord, Show`).
  - `Pawl.Filter.View = MkView { cardTypes :: Set CardType, supertypes :: Set
    Supertype, colors :: Set Color, subtypes :: Set Subtype, power :: Maybe
    Integer, controller :: Maybe PlayerId }` (`Eq, Show`).
  - `Pawl.Filter.Context` — `newtype Context = MkContext { perspective :: Maybe
    PlayerId }` (`Eq, Show`).
  - `Pawl.Filter.matches :: Context -> View -> Filter -> Bool`.

- [ ] **Step 1: Write the failing evaluator tests**

Create `source/test-suite/Pawl/FilterSpec.hs`. It heads with a comment listing the
modules it covers (`Pawl.Type.Filter`, `Pawl.Type.PlayerRelation`, `Pawl.Filter`),
exposes `tests :: TestTree`, and takes no `Cards` argument (pure).

```haskell
-- Covers Pawl.Type.Filter, Pawl.Type.PlayerRelation, Pawl.Filter.
module Pawl.FilterSpec where

import qualified Data.Set as Set
import qualified Pawl.Filter as Filter
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.Filter as Filter.Type
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.PlayerRelation as PlayerRelation
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Supertype as Supertype
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- A projected black creature controlled by player 0.
blackCreature :: Filter.View
blackCreature =
  Filter.MkView
    { Filter.cardTypes = Set.singleton CardType.Creature,
      Filter.supertypes = Set.empty,
      Filter.colors = Set.singleton Color.Black,
      Filter.subtypes = Set.singleton Subtype.Zombie,
      Filter.power = Just 2,
      Filter.controller = Just (PlayerId.MkPlayerId 0)
    }

-- A colourless (devoid) creature with power 5, no controller recorded.
devoidBigCreature :: Filter.View
devoidBigCreature =
  Filter.MkView
    { Filter.cardTypes = Set.singleton CardType.Creature,
      Filter.supertypes = Set.empty,
      Filter.colors = Set.empty,
      Filter.subtypes = Set.empty,
      Filter.power = Just 5,
      Filter.controller = Nothing
    }

self :: Filter.Context
self = Filter.MkContext (Just (PlayerId.MkPlayerId 0))

other :: Filter.Context
other = Filter.MkContext (Just (PlayerId.MkPlayerId 1))

noPerspective :: Filter.Context
noPerspective = Filter.MkContext Nothing

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Pawl.Filter"
    [ HU.testCase "HasCardType matches when present" $
        HU.assertBool "creature" (Filter.matches self blackCreature (Filter.Type.HasCardType CardType.Creature)),
      HU.testCase "HasCardType fails when absent" $
        HU.assertBool "not land" (not (Filter.matches self blackCreature (Filter.Type.HasCardType CardType.Land))),
      HU.testCase "HasColor matches Black creature" $
        HU.assertBool "black" (Filter.matches self blackCreature (Filter.Type.HasColor Color.Black)),
      HU.testCase "Not HasColor Black is Doom Blade's narrowing" $ do
        HU.assertBool "black is illegal" (not (Filter.matches self blackCreature (Filter.Type.Not (Filter.Type.HasColor Color.Black))))
        HU.assertBool "devoid is legal" (Filter.matches self devoidBigCreature (Filter.Type.Not (Filter.Type.HasColor Color.Black))),
      HU.testCase "And [] is the trivial predicate (matches everything)" $
        HU.assertBool "trivial" (Filter.matches self blackCreature (Filter.Type.And [])),
      HU.testCase "Terror: And of two negated atoms" $ do
        let terror = Filter.Type.And [Filter.Type.Not (Filter.Type.HasColor Color.Black), Filter.Type.Not (Filter.Type.HasCardType CardType.Artifact)]
        HU.assertBool "black creature fails" (not (Filter.matches self blackCreature terror))
        HU.assertBool "devoid creature passes" (Filter.matches self devoidBigCreature terror),
      HU.testCase "Or matches when either arm matches" $
        HU.assertBool "creature or enchantment" (Filter.matches self blackCreature (Filter.Type.Or [Filter.Type.HasCardType CardType.Creature, Filter.Type.HasCardType CardType.Enchantment])),
      HU.testCase "PowerAtLeast compares projected power" $ do
        HU.assertBool "power 2 < 4" (not (Filter.matches self blackCreature (Filter.Type.PowerAtLeast 4)))
        HU.assertBool "power 5 >= 4" (Filter.matches self devoidBigCreature (Filter.Type.PowerAtLeast 4)),
      HU.testCase "PowerAtLeast is False when power is Nothing" $ do
        let noPower = blackCreature {Filter.power = Nothing}
        HU.assertBool "no power" (not (Filter.matches self noPower (Filter.Type.PowerAtLeast 1))),
      HU.testCase "ControlledBy You holds for own object" $
        HU.assertBool "you" (Filter.matches self blackCreature (Filter.Type.ControlledBy PlayerRelation.You)),
      HU.testCase "ControlledBy You fails from an opponent's perspective" $
        HU.assertBool "not you" (not (Filter.matches other blackCreature (Filter.Type.ControlledBy PlayerRelation.You))),
      HU.testCase "ControlledBy Opponent holds across differing players" $
        HU.assertBool "opponent" (Filter.matches other blackCreature (Filter.Type.ControlledBy PlayerRelation.Opponent)),
      HU.testCase "ControlledBy is False when the object has no controller" $
        HU.assertBool "no controller" (not (Filter.matches self devoidBigCreature (Filter.Type.ControlledBy PlayerRelation.Opponent))),
      HU.testCase "ControlledBy is False when the context has no perspective" $
        HU.assertBool "no perspective" (not (Filter.matches noPerspective blackCreature (Filter.Type.ControlledBy PlayerRelation.You)))
    ]
```

Note the alias `Filter.Type` for `Pawl.Type.Filter` is a deliberate exception to
"alias to the last component" (the last component `Filter` already names the
evaluator module); flag it with a one-line comment in the import group.

- [ ] **Step 2: Wire FilterSpec into Main.hs**

In `source/test-suite/Main.hs` add `import qualified Pawl.FilterSpec` (alphabetical
position) and add `FilterSpec.tests` to the `testTree` list (it is card-free, like
`BindingSpec`/`DecideSpec`/`JsonSpec` — no `cards` argument).

- [ ] **Step 3: Run the tests to verify they fail (do not compile yet)**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Pawl.Type.Filter`, `Pawl.Type.PlayerRelation`, `Pawl.Filter`
not found / not in scope.

- [ ] **Step 4: Create `Pawl.Type.PlayerRelation`**

```haskell
module Pawl.Type.PlayerRelation where

-- Who an object's controller is, relative to the perspective the evaluation
-- carries (the source's controller when targeting; the effect's controller for a
-- continuous effect). CR 109.5 fixes "you" as the object's controller and, by
-- negation, "an opponent" as any other player still in the game.
data PlayerRelation
  = You
  | Opponent
  deriving (Eq, Ord, Show)
```

- [ ] **Step 5: Create `Pawl.Type.Filter`**

```haskell
module Pawl.Type.Filter where

import Pawl.Type.CardType (CardType)
import Pawl.Type.Color (Color)
import Pawl.Type.PlayerRelation (PlayerRelation)
import Pawl.Type.Subtype (Subtype)
import Pawl.Type.Supertype (Supertype)

-- A first-order, non-recursive-in-meaning-but-finitely-recursive-in-structure
-- predicate over one object, expressed as data and evaluated by one generic
-- matcher (Pawl.Filter.matches) that never learns which effect produced it. Its
-- atoms case on CHARACTERISTICS (card type, supertype, colour, subtype, power,
-- controller) exactly as the rules already case on a CardType -- casing on a
-- characteristic classification is legitimate; the invariant forbids only casing
-- on an EFFECT's identity, which this type never does.
--
-- Flat, not layered: the atoms and the And/Or/Not combinators are sibling arms of
-- one type, mirroring Pawl.Type.Quantity's flat `Plus Quantity Quantity`
-- alongside its leaf arms. A Simple/combinator split would buy an enforceable
-- normal form only if it also restricted the recursion (CNF/DNF); unrestricted it
-- guarantees nothing the flat type does not.
--
-- `And []` is the trivial predicate -- the identity that matches everything -- so
-- a bare "target creature" (no narrowing) needs no separate "always" arm.
data Filter
  = HasCardType CardType -- CR 205 / 300: the object's card types include this one.
  | HasSupertype Supertype -- CR 205.4: the object's supertypes include this one.
  | HasColor Color -- CR 105.2: the object's colours include this one.
  | HasSubtype Subtype -- CR 205.3: the object's subtypes include this one.
  | PowerAtLeast Integer -- CR 208.1: the object's power is >= this literal.
  | ControlledBy PlayerRelation -- CR 109.5: controller relates thus to the perspective.
  | And [Filter]
  | Or [Filter]
  | Not Filter
  deriving (Eq, Ord, Show)
```

- [ ] **Step 6: Create the `Pawl.Filter` evaluator**

`View`/`Context` are evaluator-local records in a logic module (precedent:
`Gathered` in `Pawl.Projection`). This module must NOT import `Pawl.Projection`
— the two view *builders* live in `Pawl.Projection` (Task 3) to keep the
dependency one-way (`Projection` → `Filter`).

```haskell
module Pawl.Filter where

import qualified Data.Set as Set
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.Filter as Filter
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.PlayerRelation as PlayerRelation
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Supertype as Supertype

-- The characteristics a Filter atom consults. Supplied by the projection on the
-- battlefield/stack, and by the printed card off the battlefield (the two
-- builders live in Pawl.Projection). `power` and `controller` are Nothing off the
-- battlefield -- a card in a library has neither under the rules that matter here
-- -- so PowerAtLeast / ControlledBy are vacuously False there, which no search
-- filter uses.
data View = MkView
  { cardTypes :: Set.Set CardType.CardType,
    supertypes :: Set.Set Supertype.Supertype,
    colors :: Set.Set Color.Color,
    subtypes :: Set.Set Subtype.Subtype,
    power :: Maybe Integer,
    controller :: Maybe PlayerId.PlayerId
  }
  deriving (Eq, Show)

-- The perspective the match is relative to: who counts as "you" (CR 109.5). For a
-- target it is the targeting source's controller; for a continuous effect's set,
-- the effect's controller; Nothing when no player frames the match (an
-- off-battlefield search, whose filters never reference a player).
newtype Context = MkContext
  { perspective :: Maybe PlayerId.PlayerId
  }
  deriving (Eq, Show)

-- The one generic matcher. A pure fold over the Filter tree; it never inspects
-- the object's identity, only the View's characteristics.
matches :: Context -> View -> Filter.Filter -> Bool
matches context view filter = case filter of
  Filter.HasCardType t -> Set.member t (cardTypes view)
  Filter.HasSupertype s -> Set.member s (supertypes view)
  Filter.HasColor c -> Set.member c (colors view)
  Filter.HasSubtype s -> Set.member s (subtypes view)
  Filter.PowerAtLeast n -> case power view of
    Nothing -> False
    Just p -> p >= n
  Filter.ControlledBy relation -> case (controller view, perspective context) of
    (Just c, Just p) -> case relation of
      PlayerRelation.You -> c == p
      PlayerRelation.Opponent -> c /= p
    _ -> False
  Filter.And fs -> all (matches context view) fs
  Filter.Or fs -> any (matches context view) fs
  Filter.Not f -> not (matches context view f)
```

- [ ] **Step 7: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks` then
`cabal test --test-options='-p "Pawl.Filter"'`
Expected: PASS (all FilterSpec cases green). Then run the full `cabal test` to
confirm nothing else broke.

- [ ] **Step 8: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(m4.5-p9): the Filter predicate type and its generic evaluator"
```

---

## Task 2: JSON codec for `Filter`, `PlayerRelation`

**Files:**
- Modify: `source/library/Pawl/Codec.hs` (add `filterToJson`/`jsonToFilter`,
  `playerRelationToJson`/`jsonToPlayerRelation`)
- Test: `source/test-suite/Pawl/CodecSpec.hs` (round-trip cases)

**Interfaces:**
- Consumes: the recursive-sum codec pattern already used by `quantityToJson` /
  `jsonToQuantity` (Codec.hs:820-839) — `Plus` encodes its operands as a JSON
  `Array` and decodes by recursing. Existing atom codecs `cardTypeToJson`,
  `supertypeToJson`, `colorToJson`, `subtypeToJson` and their `jsonTo*` inverses
  (used by the `TypeLine`, `PermanentCriterion`, `SpellCriterion` codecs).
- Produces: `Codec.filterToJson :: Filter.Filter -> Json.Value`,
  `Codec.jsonToFilter :: Json.Value -> Either Text Filter.Filter`,
  `Codec.playerRelationToJson`, `Codec.jsonToPlayerRelation`.

- [ ] **Step 1: Confirm the atom codecs exist**

Run: `grep -nE 'cardTypeToJson|jsonToCardType|supertypeToJson|jsonToSupertype|colorToJson|jsonToColor|subtypeToJson|jsonToSubtype' source/library/Pawl/Codec.hs`
Expected: all eight present. If `cardTypeToJson`/`supertypeToJson` (or inverses)
are missing, add them first mirroring `colorToJson`/`jsonToColor`, and note it in
the commit. Do not proceed until every atom the `Filter` codec needs exists.

- [ ] **Step 2: Write the failing round-trip tests**

In `source/test-suite/Pawl/CodecSpec.hs`, add a `HU.testCase` in the appropriate
group. Cover every atom plus a nested recursive value (so `And`/`Or`/`Not`
recursion is exercised), and `PlayerRelation`.

```haskell
    HU.testCase "Filter round-trips including nested And/Or/Not" $ do
      let doomBlade = Filter.Not (Filter.HasColor Color.Black)
          terror = Filter.And [Filter.Not (Filter.HasColor Color.Black), Filter.Not (Filter.HasCardType CardType.Artifact)]
          reprisal = Filter.PowerAtLeast 4
          basicLand = Filter.And [Filter.HasCardType CardType.Land, Filter.HasSupertype Supertype.Basic]
          angelicEdict = Filter.Or [Filter.HasCardType CardType.Creature, Filter.HasCardType CardType.Enchantment]
          controlled = Filter.ControlledBy PlayerRelation.Opponent
          bySubtype = Filter.HasSubtype Subtype.Wall
      mapM_
        (roundTrip "filter" Codec.filterToJson Codec.jsonToFilter)
        [doomBlade, terror, reprisal, basicLand, angelicEdict, controlled, bySubtype],
    HU.testCase "PlayerRelation round-trips" $
      mapM_
        (roundTrip "relation" Codec.playerRelationToJson Codec.jsonToPlayerRelation)
        [PlayerRelation.You, PlayerRelation.Opponent]
```

Add the needed imports (`Pawl.Type.Filter as Filter`, `Pawl.Type.PlayerRelation as
PlayerRelation`, and `Pawl.Type.Supertype`, `Pawl.Type.Subtype` if not present).

- [ ] **Step 3: Run to verify failure**

Run: `cabal test --test-options='-p "Codec"'`
Expected: FAIL — `Codec.filterToJson` / `Codec.jsonToFilter` /
`Codec.playerRelationToJson` not in scope.

- [ ] **Step 4: Add the `PlayerRelation` codec**

Mirror the nullary pattern (`nullary` / `decodeNullary`, Codec.hs:92-100):

```haskell
playerRelationToJson :: PlayerRelation.PlayerRelation -> Value
playerRelationToJson r = nullary . Text.pack $ case r of
  PlayerRelation.You -> "You"
  PlayerRelation.Opponent -> "Opponent"

jsonToPlayerRelation :: Value -> Either Text PlayerRelation.PlayerRelation
jsonToPlayerRelation =
  decodeNullary
    (Text.pack "PlayerRelation")
    [ (Text.pack "You", PlayerRelation.You),
      (Text.pack "Opponent", PlayerRelation.Opponent)
    ]
```

- [ ] **Step 5: Add the `Filter` codec (recursive, mirroring `quantityToJson`)**

```haskell
filterToJson :: Filter.Filter -> Value
filterToJson filter = case filter of
  Filter.HasCardType t -> Json.tagged (Text.pack "HasCardType") (Just (cardTypeToJson t))
  Filter.HasSupertype s -> Json.tagged (Text.pack "HasSupertype") (Just (supertypeToJson s))
  Filter.HasColor c -> Json.tagged (Text.pack "HasColor") (Just (colorToJson c))
  Filter.HasSubtype s -> Json.tagged (Text.pack "HasSubtype") (Just (subtypeToJson s))
  Filter.PowerAtLeast n -> Json.tagged (Text.pack "PowerAtLeast") (Just (Json.jInt n))
  Filter.ControlledBy r -> Json.tagged (Text.pack "ControlledBy") (Just (playerRelationToJson r))
  Filter.And fs -> Json.tagged (Text.pack "And") (Just (Array (map filterToJson fs)))
  Filter.Or fs -> Json.tagged (Text.pack "Or") (Just (Array (map filterToJson fs)))
  Filter.Not f -> Json.tagged (Text.pack "Not") (Just (filterToJson f))

jsonToFilter :: Value -> Either Text Filter.Filter
jsonToFilter value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("HasCardType", Just v) -> Filter.HasCardType <$> jsonToCardType v
    ("HasSupertype", Just v) -> Filter.HasSupertype <$> jsonToSupertype v
    ("HasColor", Just v) -> Filter.HasColor <$> jsonToColor v
    ("HasSubtype", Just v) -> Filter.HasSubtype <$> jsonToSubtype v
    ("PowerAtLeast", Just v) -> Filter.PowerAtLeast <$> Json.asInteger v
    ("ControlledBy", Just v) -> Filter.ControlledBy <$> jsonToPlayerRelation v
    ("And", Just (Array vs)) -> Filter.And <$> traverse jsonToFilter vs
    ("Or", Just (Array vs)) -> Filter.Or <$> traverse jsonToFilter vs
    ("Not", Just v) -> Filter.Not <$> jsonToFilter v
    _ -> Left (Text.pack "unknown Filter: " <> t)
```

(Confirm the exact helper names — `Json.jInt` vs `Json.jInteger`, `Json.asInteger`
— against the existing `quantityToJson`/`jsonToQuantity`; use whatever those use.)

- [ ] **Step 6: Run to verify pass**

Run: `cabal test --test-options='-p "Codec"'`
Expected: PASS. Then full `cabal test`.

- [ ] **Step 7: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(m4.5-p9): JSON codec for Filter and PlayerRelation"
```

---

## Task 3: The two `View` builders in `Pawl.Projection`

**Files:**
- Modify: `source/library/Pawl/Projection.hs` (add `viewOfObject`, `viewOfCard`,
  private `viewOfCharacteristics`, private `printedSupertypes`)
- Test: `source/test-suite/Pawl/ProjectionSpec.hs`

**Interfaces:**
- Consumes: `Projection.project :: ObjectId -> GameState ->
  ProjectedCharacteristics`; `PC.cardTypes/colors/subtypes/power`;
  `Projection.controllerOf :: ObjectId -> GameState -> Maybe PlayerId`;
  `Projection.baseColorsOf :: Card -> Set Color` (printed colours, devoid→empty);
  `Game.cardOf :: ObjectId -> GameState -> Maybe Card`;
  `TypeLine.supertypes/types/subtypes`; `Card.typeLine`; `Filter.MkView`.
- Produces:
  - `Projection.viewOfObject :: ObjectId -> GameState -> Filter.View` (full
    projection — targeting, cost, replacement, spell).
  - `Projection.viewOfCard :: Card -> Filter.View` (printed card, off battlefield —
    search).

- [ ] **Step 1: Write the failing builder tests**

In `source/test-suite/Pawl/ProjectionSpec.hs`, add cases proving `viewOfObject`
reads the projection and `viewOfCard` reads the printed card. Use existing
`S.addCreature`/board fixtures and a printed `Card` from `cards`.

```haskell
    HU.testCase "viewOfObject reads a projected creature's characteristics" $ do
      let (oid, gs) = S.addPiker cards (S.newGame ...)  -- use the module's existing board setup
          view = Projection.viewOfObject oid gs
      HU.assertBool "is a creature" (Set.member CardType.Creature (Filter.cardTypes view))
      HU.assertEqual "controller" (Just S.alice) (Filter.controller view),
    HU.testCase "viewOfCard reads a printed basic land's supertypes off the battlefield" $ do
      let card = Printing.card (Cards.mountainPrinting cards)
          view = Projection.viewOfCard card
      HU.assertBool "is a land" (Set.member CardType.Land (Filter.cardTypes view))
      HU.assertBool "is basic" (Set.member Supertype.Basic (Filter.supertypes view))
      HU.assertEqual "no power off battlefield" Nothing (Filter.power view)
      HU.assertEqual "no controller off battlefield" Nothing (Filter.controller view)
```

Match the existing `ProjectionSpec` board-construction idiom (read the file's top
fixtures first; use the same `S.` helpers already imported there). Use a real
basic land the loader already registers (`Cards.mountainPrinting` or equivalent —
confirm the field name in `source/test-suite/Pawl/Cards.hs`).

- [ ] **Step 2: Run to verify failure**

Run: `cabal test --test-options='-p "Projection"'`
Expected: FAIL — `Projection.viewOfObject` / `Projection.viewOfCard` not in scope.

- [ ] **Step 3: Implement the builders in `Pawl.Projection`**

Add `import qualified Pawl.Filter as Filter` (this is the one-way dependency
`Projection` → `Filter`; `Filter` never imports `Projection`). Place the builders
near `project`/`controllerOf`.

```haskell
-- The characteristics view of a battlefield/stack object: its projection (CR 613
-- layer system, so a colour-changer or type-changer is seen), its printed
-- supertypes (supertypes are not projected -- CR 205.4a basic-ness is read from
-- the printed type line), and its projected controller (CR 613.1b; Nothing when
-- the id is unknown -- e.g. a source that has left the battlefield).
viewOfObject :: ObjectId.ObjectId -> GameState.GameState -> Filter.View
viewOfObject oid gs = viewOfCharacteristics oid (project oid gs) (controllerOf oid gs) gs

-- The characteristics view of a PRINTED card off the battlefield (a card in a
-- library/graveyard/hand being matched by a search). No projection exists off the
-- battlefield, so every axis is read from the printed card: types/supertypes/
-- subtypes from the type line, colours from baseColorsOf (devoid -> empty), and
-- power/controller are Nothing (a card in a library has neither under the rules
-- that matter here). This is what lets a Filter read an object's colour outside
-- the battlefield without a projection that does not exist there.
viewOfCard :: Card.Type.Card -> Filter.View
viewOfCard card =
  let typeLine = Card.Type.typeLine card
   in Filter.MkView
        { Filter.cardTypes = TypeLine.types typeLine,
          Filter.supertypes = TypeLine.supertypes typeLine,
          Filter.colors = baseColorsOf card,
          Filter.subtypes = TypeLine.subtypes typeLine,
          Filter.power = Nothing,
          Filter.controller = Nothing
        }

-- Shared assembly: fill a View from a projection's characteristics plus the
-- printed supertypes (not projected) and a supplied controller.
viewOfCharacteristics :: ObjectId.ObjectId -> ProjectedCharacteristics -> Maybe PlayerId.PlayerId -> GameState.GameState -> Filter.View
viewOfCharacteristics oid pc controller gs =
  Filter.MkView
    { Filter.cardTypes = PC.cardTypes pc,
      Filter.supertypes = printedSupertypes oid gs,
      Filter.colors = PC.colors pc,
      Filter.subtypes = PC.subtypes pc,
      Filter.power = PC.power pc,
      Filter.controller = controller
    }

-- CR 205.4a: supertypes are read from the printed type line (no modelled effect
-- changes a supertype). Empty when the object has no underlying card.
printedSupertypes :: ObjectId.ObjectId -> GameState.GameState -> Set.Set Supertype.Supertype
printedSupertypes oid gs = case Game.cardOf oid gs of
  Nothing -> Set.empty
  Just card -> TypeLine.supertypes (Card.Type.typeLine card)
```

Use whatever qualified aliases the module already uses (`Card.Type`, `PC`,
`TypeLine`, `Game`, `ObjectId`, `PlayerId`, `Supertype`, `Set`). Note the existing
`isBasic` helper (Projection.hs:191-194) now duplicates `printedSupertypes`;
replace `isBasic`'s body with `Set.member Supertype.Basic (printedSupertypes oid
gs)` so there is one source of printed supertypes (still used by `affects` until
Task 9 reworks it).

- [ ] **Step 4: Run to verify pass**

Run: `cabal test --test-options='-p "Projection"'`
Expected: PASS. Then full `cabal test` and a warning-clean
`cabal build all --enable-tests --enable-benchmarks`.

- [ ] **Step 5: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(m4.5-p9): viewOfObject and viewOfCard project the Filter View"
```

---

## Task 4: Reshape `TargetSpec` to pool + filter + exclusion (closes #40)

This is the pivotal, atomic task: the `TargetSpec` type changes shape, so its type,
codec, consumers, every card JSON that declares a target, and every Haskell test
fixture must land together for a green build. It is one large but coherent commit.
Doom Blade, Lightning Bolt, Murder, and the whole existing target test suite are
the safety net — they must stay green with identical behaviour.

**Files:**
- Create: `source/library/Pawl/Type/Pool.hs`, `source/library/Pawl/Type/Exclusion.hs`
- Modify: `source/library/Pawl/Type/TargetSpec.hs`
- Modify: `source/library/Pawl/Target.hs` (`legalRecipients`, `selfExcludes`, new
  private `basePool`)
- Modify: `source/library/Pawl/Codec.hs` (`targetSpecToJson`/`jsonToTargetSpec`;
  add `poolToJson`/`jsonToPool`, `exclusionToJson`/`jsonToExclusion`)
- Modify: the card JSON files listed in Step 6
- Modify: the test fixtures listed in Step 7

**Interfaces:**
- Produces:
  - `Pawl.Type.Pool.Pool = Creatures | Players | AnyTarget | Permanents | Spells |
    SpellsAndPermanents` (`Eq, Ord, Show`).
  - `Pawl.Type.Exclusion.Exclusion = IncludesSource | ExcludesSource`
    (`Eq, Ord, Show`).
  - `Pawl.Type.TargetSpec.TargetSpec = MkTargetSpec Pool (Maybe Filter) Exclusion`
    (`Eq, Ord, Show`).
  - `Target.legalRecipients :: ObjectId -> TargetSpec -> GameState -> Set
    Recipient` (signature unchanged); `Target.selfExcludes :: TargetSpec -> Bool`.

**Design note (why an `Exclusion` field):** the spec's 2-field sketch
(`MkTargetSpec Pool (Maybe Filter)`) has nowhere for "another" to live, yet
§3/§4a require `selfExcludes` to keep returning `True` for the old
`NonlandPermanentTarget`. Self-exclusion is a property of the *target slot*, not
the object predicate, so it is a third field (a sum type, not a `Bool` — no boolean
blindness), and `selfExcludes` reads it.

- [ ] **Step 1: Write the failing behaviour tests**

Add to `source/test-suite/Pawl/ResolveSpec.hs`'s `targetTests` a test asserting the
new shape reproduces `NonblackCreatureTarget`'s behaviour, plus that a bare
`Creatures` pool with `Nothing` narrows nothing, plus that `ExcludesSource` drops
the source. Read the file's existing `targetTests` fixtures first and reuse them.

```haskell
    HU.testCase "MkTargetSpec Creatures (Just (Not (HasColor Black))) excludes a black creature" $ do
      let spec = TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Not (Filter.HasColor Color.Black))) Exclusion.IncludesSource
          -- board with one black creature (blackOid) and one devoid/nonblack creature (plainOid):
          legal = Target.legalRecipients S.noSource spec gs
      HU.assertBool "black creature illegal" (not (Set.member (Recipient.ToCreature blackOid) legal))
      HU.assertBool "nonblack creature legal" (Set.member (Recipient.ToCreature plainOid) legal),
    HU.testCase "MkTargetSpec Creatures Nothing narrows nothing" $ do
      let spec = TargetSpec.MkTargetSpec Pool.Creatures Nothing Exclusion.IncludesSource
      HU.assertEqual "all creatures legal" expectedAllCreatures (Target.legalRecipients S.noSource spec gs),
    HU.testCase "selfExcludes reads the Exclusion field" $ do
      HU.assertBool "excludes" (Target.selfExcludes (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Not (Filter.HasCardType CardType.Land))) Exclusion.ExcludesSource))
      HU.assertBool "includes" (not (Target.selfExcludes (TargetSpec.MkTargetSpec Pool.Creatures Nothing Exclusion.IncludesSource)))
```

Add imports for `Pawl.Type.Pool as Pool`, `Pawl.Type.Exclusion as Exclusion`,
`Pawl.Type.Filter as Filter`, `Pawl.Type.PlayerRelation` as needed. Construct
`blackOid`/`plainOid`/`gs`/`expectedAllCreatures` with the same helpers the
surrounding `targetTests` already use.

- [ ] **Step 2: Run to verify failure**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Pool`, `Exclusion` not found; `MkTargetSpec` arity mismatch.

- [ ] **Step 3: Create `Pawl.Type.Pool` and `Pawl.Type.Exclusion`**

```haskell
module Pawl.Type.Pool where

-- CR 115: the closed set of recipient kinds a target slot may draw from, fixing
-- both WHICH objects are candidates and HOW they are referenced
-- (Recipient.ToCreature / ToPlayer / ToObject). Closed-half vocabulary, like the
-- old TargetSpec enum -- it grows only when the rules define a new kind of
-- targetable object, never per card.
data Pool
  = Creatures -- CR 115.1a: creatures on the battlefield (ToCreature).
  | Players -- CR 115: players still in the game (ToPlayer).
  | AnyTarget -- CR 115.4: creatures + players (planeswalkers/battles absent).
  | Permanents -- CR 110.1: permanents on the battlefield (ToObject).
  | Spells -- CR 112.1: spells on the stack (ToObject).
  | SpellsAndPermanents -- CR 115: stack objects + battlefield permanents (ToObject).
  deriving (Eq, Ord, Show)
```

```haskell
module Pawl.Type.Exclusion where

-- CR 601.2c: whether a target slot admits its own source as a legal target.
-- "another" excludes it; the default includes it. A property of the slot, not of
-- the object predicate, so it lives here rather than as a Filter atom.
data Exclusion
  = IncludesSource
  | ExcludesSource
  deriving (Eq, Ord, Show)
```

- [ ] **Step 4: Reshape `Pawl.Type.TargetSpec`**

Replace the whole 12-constructor enum with:

```haskell
module Pawl.Type.TargetSpec where

import Pawl.Type.Exclusion (Exclusion)
import Pawl.Type.Filter (Filter)
import Pawl.Type.Pool (Pool)

-- What a target slot may hold: a closed Pool of candidate recipients (CR 115),
-- narrowed by an open Filter (Nothing = the whole pool, e.g. bare "target
-- creature"), with an Exclusion saying whether the source itself is a legal
-- target ("another"). This retires the whole hand-carved family of colour- and
-- type-restricted specs (#40): each is now one data value.
data TargetSpec = MkTargetSpec Pool (Maybe Filter) Exclusion
  deriving (Eq, Ord, Show)
```

- [ ] **Step 5: Rewrite `Target.legalRecipients` and `selfExcludes`**

First `Read source/library/Pawl/Target.hs` (lines ~1-175). The current
`legalRecipients` has one `case spec of` arm per old constructor; each arm builds a
base recipient set then, for restricted specs, narrows it. Refactor so the base set
comes from a `basePool` helper (extract the *exact* set-construction expressions
already in the old arms — `CreatureTarget`'s creatures, `AnyTarget`'s
creatures+players, `SpellTarget`'s stack spells, `SpellOrPermanentTarget`'s
spells+permanents, `LandTarget`'s battlefield permanents) and the narrowing is
`Filter.matches`.

```haskell
legalRecipients :: ObjectId.ObjectId -> TargetSpec.TargetSpec -> GameState.GameState -> Set.Set Recipient.Recipient
legalRecipients source spec gs =
  let TargetSpec.MkTargetSpec pool restriction _ = spec
      context = Filter.MkContext (Projection.controllerOf source gs)
      keep recipient = case recipient of
        Recipient.ToPlayer _ -> True -- a Filter ranges over objects; it never narrows a player.
        Recipient.ToCreature oid -> narrows oid
        Recipient.ToObject oid -> narrows oid
      narrows oid = case restriction of
        Nothing -> True
        Just f -> Filter.matches context (Projection.viewOfObject oid gs) f
   in Set.filter keep (basePool pool gs)

-- The closed part: build the pool's base recipient set over zones, tagging each
-- with how it is referenced. Reuses the exact per-zone member expressions from the
-- old per-constructor arms.
basePool :: Pool.Pool -> GameState.GameState -> Set.Set Recipient.Recipient
basePool pool gs = case pool of
  Pool.Creatures -> creatureRecipients gs
  Pool.Players -> playerRecipients gs
  Pool.AnyTarget -> Set.union (creatureRecipients gs) (playerRecipients gs)
  Pool.Permanents -> permanentRecipients gs
  Pool.Spells -> spellRecipients gs
  Pool.SpellsAndPermanents -> Set.union (spellRecipients gs) (permanentRecipients gs)
```

Define `creatureRecipients`, `playerRecipients`, `permanentRecipients`,
`spellRecipients` from the existing arm bodies (creatures = battlefield objects
that are creatures via `Projection.isCreatureOf`, tagged `Recipient.ToCreature`;
players = `Sba.stillPlaying` tagged `Recipient.ToPlayer`; permanents = battlefield
members tagged `Recipient.ToObject`; spells = `GameState.stack` spells via
`Game.isSpell` tagged `Recipient.ToObject`). Preserve every existing predicate
exactly — do not "improve" the zone logic.

```haskell
selfExcludes :: TargetSpec.TargetSpec -> Bool
selfExcludes spec =
  let TargetSpec.MkTargetSpec _ _ exclusion = spec
   in case exclusion of
        Exclusion.ExcludesSource -> True
        Exclusion.IncludesSource -> False
```

`stillLegal`, `legalSets`, `legalSetsExcluding`, `fillableModes` keep their bodies
(they call `legalRecipients`/`selfExcludes`).

- [ ] **Step 6: Migrate the target-declaring card JSON**

The card `targetSpecs` entry changes from a nullary tag to the product shape
`{"pool": <pool>, "filter": <filter | omitted>, "exclusion": <exclusion>}` (the
`filter` key is omitted when `Nothing`). First find every file:

Run: `grep -rln '"targetSpecs"' data/cards/` and, for each, look at its `"spec"`
tag. Apply this exact mapping (from spec §4a, verified against the current data):

| Old `"spec"` tag | New `"spec"` object |
|---|---|
| `{"type":"AnyTarget"}` | `{"pool":{"type":"AnyTarget"},"exclusion":{"type":"IncludesSource"}}` |
| `{"type":"CreatureTarget"}` | `{"pool":{"type":"Creatures"},"exclusion":{"type":"IncludesSource"}}` |
| `{"type":"PlayerTarget"}` | `{"pool":{"type":"Players"},"exclusion":{"type":"IncludesSource"}}` |
| `{"type":"SpellTarget"}` | `{"pool":{"type":"Spells"},"exclusion":{"type":"IncludesSource"}}` |
| `{"type":"SpellOrPermanentTarget"}` | `{"pool":{"type":"SpellsAndPermanents"},"exclusion":{"type":"IncludesSource"}}` |
| `{"type":"LandTarget"}` | `{"pool":{"type":"Permanents"},"filter":{"type":"HasCardType","value":{"type":"Land"}},"exclusion":{"type":"IncludesSource"}}` |
| `{"type":"ArtifactTarget"}` | `{"pool":{"type":"Permanents"},"filter":{"type":"HasCardType","value":{"type":"Artifact"}},"exclusion":{"type":"IncludesSource"}}` |
| `{"type":"CreatureOrEnchantmentTarget"}` | `{"pool":{"type":"Permanents"},"filter":{"type":"Or","value":[{"type":"HasCardType","value":{"type":"Creature"}},{"type":"HasCardType","value":{"type":"Enchantment"}}]},"exclusion":{"type":"IncludesSource"}}` |
| `{"type":"NonlandPermanentTarget"}` | `{"pool":{"type":"Permanents"},"filter":{"type":"Not","value":{"type":"HasCardType","value":{"type":"Land"}}},"exclusion":{"type":"ExcludesSource"}}` |
| `{"type":"WallTarget"}` | `{"pool":{"type":"Creatures"},"filter":{"type":"HasSubtype","value":{"type":"Wall"}},"exclusion":{"type":"IncludesSource"}}` |
| `{"type":"NonblackCreatureTarget"}` | `{"pool":{"type":"Creatures"},"filter":{"type":"Not","value":{"type":"HasColor","value":{"type":"Black"}}},"exclusion":{"type":"IncludesSource"}}` |
| `{"type":"OpponentCreatureTarget"}` | `{"pool":{"type":"Creatures"},"filter":{"type":"ControlledBy","value":{"type":"Opponent"}},"exclusion":{"type":"IncludesSource"}}` |

Known files (verify with the grep — apply to *every* occurrence, several files hold
`CreatureTarget`): `lightning-bolt.json` (AnyTarget), `doom-blade.json`
(NonblackCreatureTarget), `murder.json` (CreatureTarget), `master-thief.json`
(ArtifactTarget), `hag-of-inner-weakness.json` (OpponentCreatureTarget),
`chaos-charm.json` (WallTarget), `landform.json` (LandTarget),
`angelic-edict.json` (CreatureOrEnchantmentTarget), `aether-channeler.json`
(NonlandPermanentTarget), `synthetic-modal-trigger.json` (NonlandPermanentTarget,
possibly ×N), plus all `CreatureTarget` (~15), `PlayerTarget` (~3), and the single
`SpellTarget` / `SpellOrPermanentTarget` cards. Confirm the exact inner tag strings
`{"type":"Creature"}` / `{"type":"Land"}` / `{"type":"Black"}` / `{"type":"Wall"}`
match what `cardTypeToJson`/`colorToJson`/`subtypeToJson` actually emit (Step 1 of
Task 2 confirmed the codecs; encode one value in `cabal repl` if unsure).

- [ ] **Step 7: Update the codec and the Haskell test fixtures**

Codec — `Read source/library/Pawl/Codec.hs:663-694` and `:1141-1155`, then:

```haskell
poolToJson :: Pool.Pool -> Value
poolToJson p = nullary . Text.pack $ case p of
  Pool.Creatures -> "Creatures"
  Pool.Players -> "Players"
  Pool.AnyTarget -> "AnyTarget"
  Pool.Permanents -> "Permanents"
  Pool.Spells -> "Spells"
  Pool.SpellsAndPermanents -> "SpellsAndPermanents"

jsonToPool :: Value -> Either Text Pool.Pool
jsonToPool =
  decodeNullary
    (Text.pack "Pool")
    [ (Text.pack "Creatures", Pool.Creatures),
      (Text.pack "Players", Pool.Players),
      (Text.pack "AnyTarget", Pool.AnyTarget),
      (Text.pack "Permanents", Pool.Permanents),
      (Text.pack "Spells", Pool.Spells),
      (Text.pack "SpellsAndPermanents", Pool.SpellsAndPermanents)
    ]

exclusionToJson :: Exclusion.Exclusion -> Value
exclusionToJson e = nullary . Text.pack $ case e of
  Exclusion.IncludesSource -> "IncludesSource"
  Exclusion.ExcludesSource -> "ExcludesSource"

jsonToExclusion :: Value -> Either Text Exclusion.Exclusion
jsonToExclusion =
  decodeNullary
    (Text.pack "Exclusion")
    [ (Text.pack "IncludesSource", Exclusion.IncludesSource),
      (Text.pack "ExcludesSource", Exclusion.ExcludesSource)
    ]

targetSpecToJson :: TargetSpec.TargetSpec -> Value
targetSpecToJson (TargetSpec.MkTargetSpec pool restriction exclusion) =
  let base =
        [ (Text.pack "pool", poolToJson pool),
          (Text.pack "exclusion", exclusionToJson exclusion)
        ]
      withFilter = case restriction of
        Nothing -> base
        Just f -> base <> [(Text.pack "filter", filterToJson f)]
   in Object withFilter

jsonToTargetSpec :: Value -> Either Text TargetSpec.TargetSpec
jsonToTargetSpec value = do
  ps <- Json.asObject value
  pool <- Json.field (Text.pack "pool") ps >>= jsonToPool
  exclusion <- Json.field (Text.pack "exclusion") ps >>= jsonToExclusion
  restriction <- case Json.optField (Text.pack "filter") ps of
    Nothing -> Right Nothing
    Just v -> Just <$> jsonToFilter v
  pure (TargetSpec.MkTargetSpec pool restriction exclusion)
```

(Use the exact `Json.asObject`/`field`/`optField`/`Object` names the module already
uses — mirror how `Json.tag` is written.) `targetSpecsToJson`/`jsonToTargetSpecs`
(the slot map) keep their structure and just call the two functions above.

Delete the old `TargetSpec` round-trip cases and add new ones in `CodecSpec.hs`
(cover a bare pool, a filtered pool, and an `ExcludesSource` value). Then rewrite
every Haskell test fixture that constructed an old `TargetSpec.<Ctor>` value —
files and lines from the survey: `ColorSpec.hs` (137,145,171,185),
`CodecSpec.hs` (358-366,379-382,401), `ActivateSpec.hs` (185,221),
`CardSpec.hs` (224,228,267,443,455,465,470,499,528), `ManaSpec.hs` (166,176),
`ResolveSpec.hs` (85-98,109-119,124,128,138-139,143,151,159,167,183,187,450,656),
`ModalSpec.hs` (193,200) — mapping each to its `MkTargetSpec Pool (Maybe Filter)
Exclusion` value per the table in Step 6.

- [ ] **Step 8: Build, run the whole suite, verify green**

Run: `cabal clean && cabal build all --enable-tests --enable-benchmarks` (clean
build so no warning is hidden), then `cabal test`.
Expected: warning-clean build; entire suite green — the new target tests pass and
every pre-existing target/card/codec test still passes with identical behaviour.

- [ ] **Step 9: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(m4.5-p9): TargetSpec becomes pool + Maybe Filter + exclusion (closes #40)"
```

- [ ] **Step 10: Close issue #40**

```bash
gh issue close 40 --comment "Closed by M4.5 P9: the TargetSpec family collapses to Pool + Maybe Filter + Exclusion; every restricted spec is now one Filter data value. Doom Blade / Terror / Reprisal are the gates."
```

---

## Task 5: Gate cards Terror and Reprisal

**Files:**
- Create: `data/cards/terror.json`, `data/cards/reprisal.json`
- Modify: `source/test-suite/Pawl/Cards.hs` (three sites each: record field,
  `loadPrinting` binding, `allPrintings` entry)
- Test: add gate tests (in `ResolveSpec.hs`'s `targetTests`, or a small dedicated
  group) proving Terror's `And`-of-negations and Reprisal's `PowerAtLeast`.

**Interfaces:**
- Consumes: `TargetSpec.MkTargetSpec`, `Pool.Creatures`, `Filter.And`, `Filter.Not`,
  `Filter.HasColor`, `Filter.HasCardType`, `Filter.PowerAtLeast`,
  `Exclusion.IncludesSource`; `Cards.terrorPrinting`, `Cards.reprisalPrinting`.

- [ ] **Step 1: Write the failing gate tests**

Terror proves `And` of two negated atoms (colour + card type); Reprisal proves the
projected-power comparison changing with a pump/shrink. Model on the Doom Blade
tests in `ColorSpec.hs` and the target tests in `ResolveSpec.hs`.

```haskell
    HU.testCase "Terror: a nonblack nonartifact creature is legal, a black one is not" $ do
      -- board: a black creature (blackOid) and a nonblack, nonartifact creature (plainOid)
      let spec = ... Cards.terrorPrinting ... -- the spell's target slot, loaded from JSON
          legal = Target.legalRecipients S.noSource spec gs
      HU.assertBool "black illegal" (not (Set.member (Recipient.ToCreature blackOid) legal))
      HU.assertBool "plain legal" (Set.member (Recipient.ToCreature plainOid) legal),
    HU.testCase "Reprisal: legality tracks projected power across a pump" $ do
      -- a power-2 creature is illegal; pumped to >=4 (via S.withEffect / +X/+X) it becomes legal
      HU.assertBool "power 2 illegal" (not (Set.member (Recipient.ToCreature smallOid) legalBefore))
      HU.assertBool "pumped legal" (Set.member (Recipient.ToCreature smallOid) legalAfter)
```

Pull the actual `TargetSpec` out of the loaded printing's spell (`Modal.modes` →
`Mode.targetSpecs` → the `"target"` slot) so the test exercises the JSON, not a
hand-built value. Use the existing `S.` pump helper (`S.withEffect` with a power
`Modification`) to raise/lower power for the Reprisal case.

- [ ] **Step 2: Run to verify failure**

Run: `cabal test --test-options='-p "Reprisal"'` (and `-p "Terror"`)
Expected: FAIL — `Cards.terrorPrinting` / `Cards.reprisalPrinting` not in scope.

- [ ] **Step 3: Create `data/cards/terror.json`**

Copy `data/cards/doom-blade.json` (same cost `{1}{B}`, same `Destroy "target"`
effect, same `Instant` type line, same `ChooseExactly 1` selection). Set the
`targetSpecs` `"spec"` to Terror's filter:

```json
{ "slot": "target", "spec": {
  "pool": {"type":"Creatures"},
  "filter": {"type":"And","value":[
    {"type":"Not","value":{"type":"HasColor","value":{"type":"Black"}}},
    {"type":"Not","value":{"type":"HasCardType","value":{"type":"Artifact"}}}
  ]},
  "exclusion": {"type":"IncludesSource"}
} }
```

Terror's card text ends "It can't be regenerated." Regeneration is not modelled
(no regeneration shield to suppress — §6), so that clause is a no-op and is omitted
from the card data; the omission is tracked by the regeneration issue filed in
Step 6. JSON has no comment syntax, so the citation lives in the gate test's
comment, not the data file.

- [ ] **Step 4: Create `data/cards/reprisal.json`**

Copy the Doom Blade shape but cost `{1}{W}` (`Generic 1` + `OfType Colored White`)
and the power filter:

```json
{ "slot": "target", "spec": {
  "pool": {"type":"Creatures"},
  "filter": {"type":"PowerAtLeast","value":4},
  "exclusion": {"type":"IncludesSource"}
} }
```

Same "can't be regenerated" omission as Terror.

- [ ] **Step 5: Register both cards in `Cards.hs`**

`Read source/test-suite/Pawl/Cards.hs` and add, for each card, in alphabetical
position: the `Cards` record field (`terrorPrinting :: Printing.Printing`), the
`loadPrinting` binding (`terrorPrinting_ <- loadPrinting "terror"`), the
`MkCards {...}` assignment, and the `allPrintings` list entry
(`terrorPrinting cards,`). Same four edits for `reprisal` / `"reprisal"`.

- [ ] **Step 6: File the regeneration elision issue and cite it**

```bash
gh issue create --title "Regeneration ('can't be regenerated') not modelled" \
  --label elision --label expires:card-driven \
  --body "Terror and Reprisal (M4.5 P9 gates) end 'It can't be regenerated.' No regeneration shield is modelled, so the clause is a no-op today and is omitted from their card data. Reintroduce as a regeneration-replacement concern when a card forces regeneration to exist. Cited in the P9 gate tests."
```

Put `-- regeneration clause omitted; not modelled (#N)` in the Terror/Reprisal test
comment, using the issue number `gh` returns.

- [ ] **Step 7: Run to verify pass, then full suite**

Run: `cabal test --test-options='-p "Terror"'`, `-p "Reprisal"`, then `cabal test`.
Expected: PASS across the board.

- [ ] **Step 8: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "test(m4.5-p9): Terror and Reprisal gate the And-of-negations and power axes"
```

---

## Task 6: `CardCriterion` → `Filter` (Effect.Search; reads a printed card off the battlefield)

**Files:**
- Delete: `source/library/Pawl/Type/CardCriterion.hs`
- Modify: `source/library/Pawl/Type/Effect.hs` (`Search Filter`)
- Modify: `source/library/Pawl/Resolve.hs` (`matchesCriterion` → `Filter.matches`
  over `viewOfCard`; the `Search` arm)
- Modify: `source/library/Pawl/Codec.hs` (delete `cardCriterion*`; `Search` arm
  carries `filterToJson`/`jsonToFilter`)
- Modify: `data/cards/evolving-wilds.json` (the `Search` value)
- Test: `source/test-suite/Pawl/ResolveSpec.hs` (basic-land-search gate)

**Interfaces:**
- Consumes: `Effect.Search Filter`; `Projection.viewOfCard`; `Filter.matches`.
- Produces: search filters an off-battlefield library card through the *printed*
  view — reading an object's characteristics (colour included) where no
  projection exists, one of the capabilities P9 (#5) establishes.

- [ ] **Step 1: Write the failing basic-land-search test**

Prove a `Filter`-driven `Search` finds a basic land in the library through the
printed-card view (a card in a library has no projection). Use Evolving Wilds'
ability (the `Search` producer) with `S.addLibraryCard` fixtures — a basic land is
found, a nonland is not.

```haskell
    HU.testCase "Search (And [HasCardType Land, HasSupertype Basic]) finds a basic land in the library" $ do
      -- library holds a Mountain (basic land) and a Piker (creature); activating
      -- Evolving Wilds' search offers only the Mountain.
      ...
      HU.assertBool "mountain offered" (... Mountain is a search candidate ...)
      HU.assertBool "piker not offered" (not (... Piker is a candidate ...))
```

Base this on how `resolveTests` already exercises `Effect.Search` (search the file
for the existing Evolving Wilds / `SearchLibrary` prompt test) and assert the
candidate set now derives from `Filter.matches`.

- [ ] **Step 2: Run to verify failure**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL to compile once the type is deleted / signature changes (this test
is the driver; it may also fail on the assertion first — confirm it exercises the
new path).

- [ ] **Step 3: Delete `CardCriterion`; make `Search` carry `Filter`**

Delete `source/library/Pawl/Type/CardCriterion.hs`. In
`source/library/Pawl/Type/Effect.hs` change `import Pawl.Type.CardCriterion
(CardCriterion)` to `import Pawl.Type.Filter (Filter)` and the constructor `Search
CardCriterion` to `Search Filter`. Update the doc comment to cite CR 701.23a and
that the criterion is now a `Filter` over the printed-card view.

- [ ] **Step 4: Update `Resolve`**

Replace `matchesCriterion :: CardCriterion -> Card -> Bool` with the generic call
at the `Search` arm: for each library card, `Filter.matches searchContext
(Projection.viewOfCard card) filter`, where `searchContext = Filter.MkContext
Nothing` (a search has no perspective; its filters never reference a player).
Delete the now-dead `matchesCriterion` and its `CardCriterion` import.

- [ ] **Step 5: Update the codec**

Delete `cardCriterionToJson`/`jsonToCardCriterion` and their `CardCriterion`
import. Change the `Search` arms:

```haskell
-- encoder
Effect.Search f -> Json.tagged (Text.pack "Search") (Just (filterToJson f))
-- decoder
"Search" -> withValue mv (fmap Effect.Search . jsonToFilter)
```

- [ ] **Step 6: Migrate `evolving-wilds.json`**

Change the search effect value from `{"type":"BasicLandCard"}` to the filter
`{"type":"And","value":[{"type":"HasCardType","value":{"type":"Land"}},{"type":"HasSupertype","value":{"type":"Basic"}}]}`:

```json
"effects": [ { "type": "Search", "value":
  {"type":"And","value":[
    {"type":"HasCardType","value":{"type":"Land"}},
    {"type":"HasSupertype","value":{"type":"Basic"}}
  ]}
} ]
```

(CR 205.4c / 701.23a: a basic land card is one with the `Land` type and the `Basic`
supertype.)

- [ ] **Step 7: Run to verify pass, then full suite**

Run: `cabal test --test-options='-p "Search"'` then `cabal clean && cabal build all
--enable-tests --enable-benchmarks && cabal test`.
Expected: green, warning-clean.

- [ ] **Step 8: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(m4.5-p9): Effect.Search carries a Filter over the printed-card view"
```

---

## Task 7: `PermanentCriterion` → `Filter`

**Files:**
- Delete: `source/library/Pawl/Type/PermanentCriterion.hs`
- Modify: `source/library/Pawl/Cost.hs` (`matchesCriterion` → `Filter.matches` over
  `viewOfObject`; `sacrificeCandidates`)
- Modify: `source/library/Pawl/Replacement.hs` (`matchesPermanent` →
  `Filter.matches`)
- Modify: `source/library/Pawl/Codec.hs` (delete `permanentCriterion*`; any
  carrying type — e.g. a `CostComponent`/`Replacement` field — now holds `Filter`)
- Modify: the card JSON using a permanent criterion (Fireblast's Sacrifice-a-
  Mountain cost; any others found by grep)
- Test: `source/test-suite/Pawl/CostSpec.hs`, `source/test-suite/Pawl/ReplacementSpec.hs`

**Interfaces:**
- Consumes: `Projection.viewOfObject`; `Filter.matches`.
- Produces: `Cost` and `Replacement` narrow permanents through the same evaluator
  (retires the #111 duplicate-matcher pair — both now call `Filter.matches`
  directly, no cross-module cycle).

- [ ] **Step 1: Find the carriers and producers**

Run: `grep -rn 'PermanentCriterion' source/ data/` — enumerate every field that
holds a `PermanentCriterion` (e.g. a `Sacrifice` cost component, a replacement
pattern) and every card JSON that names one (Fireblast's Mountain sacrifice per
CostSpec). Each carrier's type changes `PermanentCriterion` → `Filter`; each JSON
tag migrates per the mapping below.

- [ ] **Step 2: Write the failing tests**

In `CostSpec.hs`, assert Fireblast's sacrifice cost now selects Mountains via a
`Filter` (`HasSubtype Mountain`), reusing the existing `fireblastTests` board;
in `ReplacementSpec.hs`, assert the replacement's permanent match runs through
`Filter.matches`. Model on the existing `PermanentCriterion.PermanentOfSubtype
Subtype.Mountain` usage (CostSpec ~141-143) — the new value is
`Filter.HasSubtype Subtype.Mountain`.

- [ ] **Step 3: Run to verify failure**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — mismatched constructor / not-in-scope once producers change.

- [ ] **Step 4: Delete the type; change the carriers to `Filter`**

Delete `source/library/Pawl/Type/PermanentCriterion.hs`. In every carrier type
found in Step 1, replace the `PermanentCriterion` field with `Filter` (import
`Pawl.Type.Filter`).

- [ ] **Step 5: Rewrite the matchers**

`Cost.matchesCriterion gs criterion oid` becomes a direct
`Filter.matches (Filter.MkContext Nothing) (Projection.viewOfObject oid gs) filter`
(a sacrifice/cost has no targeting perspective; its filters here never reference a
player). `sacrificeCandidates` keeps filtering `Projection.controls pid gs` by that
predicate. In `Replacement`, replace `matchesPermanent` the same way and delete the
`#111` "deliberate duplicate to avoid a Cost→Replacement cycle" comment — both now
depend only on the lower `Pawl.Filter`.

- [ ] **Step 6: Update the codec and migrate the JSON**

Delete `permanentCriterionToJson`/`jsonToPermanentCriterion`; the carrying field's
codec now uses `filterToJson`/`jsonToFilter`. JSON mapping (spec §4b):

| Old tag | New `Filter` JSON |
|---|---|
| `{"type":"AnyPermanent"}` | `{"type":"And","value":[]}` |
| `{"type":"CreaturePermanent"}` | `{"type":"HasCardType","value":{"type":"Creature"}}` |
| `{"type":"PermanentOfSubtype","value":<subtype>}` | `{"type":"HasSubtype","value":<subtype>}` |

Apply to Fireblast's card JSON (and any others from Step 1's grep).

- [ ] **Step 7: Run to verify pass, then full suite**

Run: `cabal test --test-options='-p "Cost"'`, `-p "Replacement"`, then
`cabal clean && cabal build all --enable-tests --enable-benchmarks && cabal test`.
Expected: green, warning-clean.

- [ ] **Step 8: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(m4.5-p9): Cost and Replacement narrow permanents through Filter"
```

---

## Task 8: `SpellCriterion` → `Filter`

**Files:**
- Delete: `source/library/Pawl/Type/SpellCriterion.hs`
- Modify: `source/library/Pawl/PlayerEffect.hs` (`matchesSpell` → `Filter.matches`
  over `viewOfObject`; the carrying `PlayerEffect` constructors)
- Modify: `source/library/Pawl/Codec.hs` (delete `spellCriterion*`; carriers hold
  `Filter`)
- Modify: the card JSON naming a spell criterion (Thalia; Sapphire Medallion; any
  others by grep)
- Test: `source/test-suite/Pawl/PlayerEffectSpec.hs`

**Interfaces:**
- Consumes: `Projection.viewOfObject`; `Filter.matches`.
- Produces: cost-adjustment spell matching runs through the generic evaluator.

- [ ] **Step 1: Find carriers and producers**

Run: `grep -rn 'SpellCriterion' source/ data/`. Note every `PlayerEffect`
constructor field holding a `SpellCriterion` (the cost-increase / cost-reduce arms)
and every card JSON tag.

- [ ] **Step 2: Write the failing test**

In `PlayerEffectSpec.hs`, assert Thalia's "noncreature spell" match now derives from
`Filter.Not (Filter.HasCardType CardType.Creature)` and a colour match from
`Filter.HasColor c`, reusing the existing spell-on-stack fixtures
(`S.spellOnStack`).

- [ ] **Step 3: Run to verify failure**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — not-in-scope / arity mismatch once carriers change.

- [ ] **Step 4: Delete the type; change carriers to `Filter`; rewrite `matchesSpell`**

Delete `source/library/Pawl/Type/SpellCriterion.hs`. In `PlayerEffect`, change the
carrier fields to `Filter` and rewrite `matchesSpell criterion oid gs` to
`Filter.matches (Filter.MkContext (Projection.controllerOf oid gs)) (Projection.viewOfObject oid gs) filter`.
(A cost-adjustment filter today references only card type / colour, not a player;
supply the spell's own controller as perspective so a future `ControlledBy` filter
is well-defined — verify no existing behaviour depends on perspective being absent.)

- [ ] **Step 5: Update the codec and migrate the JSON**

Delete `spellCriterionToJson`/`jsonToSpellCriterion`; carriers use
`filterToJson`/`jsonToFilter`. JSON mapping (spec §4b):

| Old tag | New `Filter` JSON |
|---|---|
| `{"type":"NoncreatureSpell"}` | `{"type":"Not","value":{"type":"HasCardType","value":{"type":"Creature"}}}` |
| `{"type":"SpellOfColor","value":<color>}` | `{"type":"HasColor","value":<color>}` |

Apply to Thalia's / Sapphire Medallion's (and any other) card JSON.

- [ ] **Step 6: Run to verify pass, then full suite**

Run: `cabal test --test-options='-p "PlayerEffect"'` then `cabal clean && cabal
build all --enable-tests --enable-benchmarks && cabal test`.
Expected: green, warning-clean.

- [ ] **Step 7: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(m4.5-p9): spell cost-adjustment matches through Filter"
```

---

## Task 9: `Affected` dynamic sets → `Matching Filter`

**Files:**
- Modify: `source/library/Pawl/Type/Affected.hs` (two arms)
- Modify: `source/library/Pawl/Projection.hs` (`affects` rewritten to
  `Filter.matches`; the self-exclusion of `OtherNonAuraEnchantments` is retired —
  see the design note)
- Modify: `source/library/Pawl/Codec.hs` (`affectedToJson`/`jsonToAffected`)
- Test: `source/test-suite/Pawl/ProjectionSpec.hs`, `CodecSpec.hs`,
  `source/test-suite/Pawl/Support.hs` (the six-arm helper at ~375-380)

**Interfaces:**
- Produces: `Affected = TheseObjects (Set ObjectId) | Matching Filter`
  (`Eq, Ord, Show`); `Projection.affects` evaluates `Matching` against the partial
  projection via `Filter.matches`.

**Design note (self-exclusion):** the spec's 2-arm `Affected` cannot carry the
"each other" flag that `OtherNonAuraEnchantments` used (`oid /= source` in the old
`affects`). That variant has **no live producer and no behavioural test** (only
tests/codec exercise it). So it migrates to `Matching (And [HasCardType
Enchantment, Not (HasSubtype Aura)])` *without* self-exclusion, and the deferral is
issue-tracked (Step 5). Only untested, producerless behaviour changes. `CreaturesOfColor`/`AllNonbasicLands`/`AllLands` likewise have no live card producer;
`AllCreatures` and `TheseObjects` are the ones tests build.

- [ ] **Step 1: Write the failing tests**

In `ProjectionSpec.hs`, assert `Matching (HasCardType Creature)` applies to
battlefield creatures against the partial projection (reuse the existing
`AllCreatures` anthem/continuous-effect test, re-expressed). In `CodecSpec.hs`,
round-trip `Matching` values (a plain atom and a nested `And`/`Not`) plus
`TheseObjects`.

```haskell
    -- CodecSpec
    HU.testCase "Affected round-trips (TheseObjects and Matching)" $
      mapM_
        (roundTrip "affected" Codec.affectedToJson Codec.jsonToAffected)
        [ Affected.TheseObjects (Set.fromList [ObjectId.MkObjectId 1, ObjectId.MkObjectId 2]),
          Affected.Matching (Filter.HasCardType CardType.Creature),
          Affected.Matching (Filter.And [Filter.HasCardType CardType.Land, Filter.Not (Filter.HasSupertype Supertype.Basic)])
        ]
```

- [ ] **Step 2: Run to verify failure**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Affected.Matching` not found; the six-arm patterns are non-exhaustive.

- [ ] **Step 3: Reshape `Pawl.Type.Affected`**

```haskell
module Pawl.Type.Affected where

import Data.Set (Set)
import Pawl.Type.Filter (Filter)
import Pawl.Type.ObjectId (ObjectId)

-- Which objects a continuous effect applies to.
data Affected
  = TheseObjects (Set ObjectId) -- CR 611.2c: locked at begin -- a fixed id set, NOT a predicate.
  | Matching Filter -- dynamic: any object matching, re-derived each projection against the PARTIAL projection (CR 613: layers apply in order).
  deriving (Eq, Ord, Show)
```

- [ ] **Step 4: Rewrite `Projection.affects`**

`Read source/library/Pawl/Projection.hs:155-195` (the `Gathered` record, `affects`,
`isBasic`) and `:340-355` (`affectsBase`) and `:596-612` (`projectFrom`'s call).
Replace the six-arm `affects` body:

```haskell
-- CR 611.2c / 613: does the effect apply to `oid`, given the partial projection
-- built by the layers below this one? A fixed set is a membership test; a dynamic
-- set is a Filter evaluated against the PARTIAL characteristics, so a layer-4 type
-- change is visible to a later layer. The affected-set filter carries no
-- perspective (no affected-set filter references a player).
affects :: ObjectId -> Affected.Affected -> ProjectedCharacteristics -> GameState -> Bool
affects oid a partial gs = case a of
  Affected.TheseObjects s -> Set.member oid s
  Affected.Matching f ->
    Set.member oid (GameState.battlefield gs)
      && Filter.matches (Filter.MkContext Nothing) (viewOfCharacteristics oid partial Nothing gs) f
```

The `source`/`gSource` argument existed *only* for `OtherNonAuraEnchantments`
self-exclusion (per the module's own comment); drop it from `affects` and its two
call sites (`projectFrom` ~604, `affectsBase` ~350). If `gSource` on `Gathered`
becomes wholly unused after this, remove the field and its `gather`/
`counterGathered` producers; if any other reader remains, leave it. Verify with
`grep -n gSource source/library/Pawl/Projection.hs` before deleting. Keep the
`isBasic`/`printedSupertypes` unification from Task 3 (still used elsewhere, or now
dead — remove `isBasic` if `affects` was its only caller).

- [ ] **Step 5: File the Opalescence self-exclusion issue and update the fixture**

```bash
gh issue create --title "Affected 'each other' self-exclusion not carried by Matching Filter" \
  --label gap --label expires:card-driven \
  --body "M4.5 P9 collapses Affected's dynamic sets to `Matching Filter`, which has no home for the 'other'/self-exclusion the old OtherNonAuraEnchantments arm applied (oid /= source). No card produces a self-excluding affected set today (Opalescence is unimplemented and this arm had no behavioural test). Reintroduce self-exclusion for affected sets when a producer (Opalescence) forces it — likely a slot-style Exclusion on the continuous effect, mirroring TargetSpec's Exclusion."
```

Update the six-arm helper in `source/test-suite/Pawl/Support.hs` (~375-380) and any
test constructing `AllCreatures`/`AllLands`/etc. to the `Matching <Filter>` form per
spec §4c (`AllCreatures` → `Matching (HasCardType Creature)`; `AllLands` →
`Matching (HasCardType Land)`; `AllNonbasicLands` → `Matching (And [HasCardType
Land, Not (HasSupertype Basic)])`; `CreaturesOfColor c` → `Matching (And
[HasCardType Creature, HasColor c])`; `OtherNonAuraEnchantments` → `Matching (And
[HasCardType Enchantment, Not (HasSubtype Aura)])`).

- [ ] **Step 6: Update the codec**

```haskell
affectedToJson :: Affected.Affected -> Value
affectedToJson a = case a of
  Affected.TheseObjects ids -> Json.tagged (Text.pack "TheseObjects") (Just (setTo objectIdToJson ids))
  Affected.Matching f -> Json.tagged (Text.pack "Matching") (Just (filterToJson f))

jsonToAffected :: Value -> Either Text Affected.Affected
jsonToAffected value = do
  (t, mv) <- Json.tag value
  case Text.unpack t of
    "TheseObjects" -> withValue mv (fmap Affected.TheseObjects . setFrom jsonToObjectId)
    "Matching" -> withValue mv (fmap Affected.Matching . jsonToFilter)
    _ -> Left (Text.pack "unknown Affected: " <> t)
```

- [ ] **Step 7: Run to verify pass, then full suite**

Run: `cabal test --test-options='-p "Projection"'`, `-p "Codec"`, then
`cabal clean && cabal build all --enable-tests --enable-benchmarks && cabal test`.
Expected: green, warning-clean. Pay attention to `DamageSpec`/`ExpirySpec`/
`CombatSpec`/`ProjectionSpec` which build `Affected` values (survey Task-2 report).

- [ ] **Step 8: Format, lint, commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(m4.5-p9): Affected dynamic sets collapse to Matching Filter"
```

---

## Task 10: Rescope #38/#39, docs, and the exit-criterion sweep

**Files:**
- Modify: the code sites of `StateCondition` (#38) and `CountSpec` (#39) comments
- Modify: `CLAUDE.md` (status bullet — replace, not append)
- Modify: `docs/progress.md` (milestone completion entry)

- [ ] **Step 1: Rescope #38 and #39 (do NOT close them)**

The spec (§7) keeps `StateCondition` and `CountSpec` — they add a second concept
(scope + aggregation + threshold comparison) deferred to a later count/compare
phase. Move their expiry off "P9":

```bash
gh issue comment 38 --body "Re-scoped by M4.5 P9 (not closed): the count/compare concept (scope + aggregation + threshold) is deferred to a later phase (P9b or card-driven). P9 built the per-object Filter this will reuse, but not the aggregation. Expiry trigger moves from 'milestone P9' to the deferred count/compare phase."
gh issue comment 39 --body "Re-scoped by M4.5 P9 (not closed): CountSpec's aggregation-and-threshold concept is deferred to the count/compare phase; P9 delivered only the per-object Filter. Expiry moves off P9."
```

If either issue carries a `expires:milestone` label pinned to P9, adjust per the
repo's labelling convention (leave `expires:card-driven` if that is the truer
trigger). Update the in-code comments at the `StateCondition`/`CountSpec` sites to
point at the deferred phase (grep for the `#38`/`#39` citations; change only the
expiry wording, keep the citation).

- [ ] **Step 2: Verify the exit criterion end-to-end**

Confirm each clause of spec §8 holds:
- Doom Blade, Terror, Reprisal cast/resolve with legal sets from `Filter.matches`.
- The basic-land search and a `PermanentCriterion` consumer (Fireblast) match
  through the same evaluator.
- `Pawl.Type.TargetSpec` is `Pool + Maybe Filter + Exclusion`, no per-card variant.
- `CardCriterion`, `PermanentCriterion`, `SpellCriterion` modules are gone:
  `ls source/library/Pawl/Type/ | grep -iE 'CardCriterion|PermanentCriterion|SpellCriterion'` returns nothing.
- `Affected`'s dynamic sets are `Matching Filter`.
- #40 closed, #38/#39 re-scoped.

Run the definitive checks:
```bash
cabal clean && cabal build all --enable-tests --enable-benchmarks
cabal test
git add -A && hooky fix && git add -A && hooky run
```
Expected: warning-clean build, green suite, clean hooks.

- [ ] **Step 3: Update `CLAUDE.md` status bullet (replace) and `docs/progress.md`**

Replace the `Status:` bullet's P9 line in `CLAUDE.md` — mark P9 landed (Filter
predicate language; `TargetSpec` family retired; three criterions merged;
`Affected` dynamic sets re-expressed; #40 closed; #38/#39 re-scoped), and note
P10 (player counters) and P11 (Command zone) remain. Do not append history — that
goes in `docs/progress.md`. Add the P9 completion entry to `docs/progress.md`
(gate cards Doom Blade / Terror / Reprisal; the decision it proved — one predicate
language subsumes the whole classification family across projected and printed
subjects; the types added: `Filter`, `Pool`, `Exclusion`, `PlayerRelation`, and the
`Pawl.Filter` evaluator).

- [ ] **Step 4: Commit the docs**

```bash
git add -A && hooky run
git commit -m "docs(m4.5-p9): completion note, CLAUDE.md status, #38/#39 rescope"
```

- [ ] **Step 5: Close the P9 phase issue #5**

`#5` ("M4.5 P9 — Target-filter predicate language") is the phase tracking issue,
the P9 analogue of P8's #4. Close it last, once the exit criterion holds and the
completion is recorded:

```bash
gh issue close 5 --comment "Closed by M4.5 P9: the Filter predicate language ships with Doom Blade / Terror / Reprisal as gates. TargetSpec family retired (#40 closed); CardCriterion / PermanentCriterion / SpellCriterion merged into Filter; Affected dynamic sets are Matching Filter; off-battlefield characteristics read the printed card via viewOfCard. #38/#39 re-scoped to the deferred count/compare phase, not closed."
```

---

## Self-Review

**Spec coverage:**
- §1 `Filter` type + `PlayerRelation` → Task 1. Flat-not-layered, `And []` trivial,
  `PowerAtLeast` literal `Integer` → Task 1 code + comment.
- §2 characteristics `View`, `viewOfObject`/`viewOfCard`, partial-vs-full → Tasks 1
  (View/matches) + 3 (builders) + 9 (`affects` uses the partial view).
  Off-battlefield colour (a capability of the P9 phase, #5) → Task 3 `viewOfCard`
  + Task 6 search.
- §3 evaluation context (perspective; self-exclusion stays a slot property) →
  Task 1 `Context` + Task 4 `Exclusion`/`selfExcludes`.
- §4a `TargetSpec` → pool + filter (+ exclusion) and the full mapping table →
  Task 4. #40 closed → Task 4 Step 10.
- §4b three criterions → `Filter` → Tasks 6 (`CardCriterion`), 7
  (`PermanentCriterion`), 8 (`SpellCriterion`).
- §4c `Affected` dynamic → `Matching Filter` → Task 9.
- §5 gate cards + two non-target tests → Task 5 (Terror/Reprisal), Task 6
  (basic-land search), Task 7 (Fireblast permanent criterion); Doom Blade preserved
  through Task 4 (ColorSpec) and re-tested in Task 5.
- §6 regeneration out of scope → Task 5 Step 6 (issue + cited note).
- §7 #38/#39 re-scoped not closed → Task 10 Step 1.
- §8 exit criterion → Task 10 Step 2.

**Deviations from the spec's sketches (deliberate, justified):**
- `Pool` and `Exclusion` are their own `Pawl.Type.*` modules (spec sketched `Pool`
  inside `Pawl.Type.TargetSpec`) — required by the one-type-per-module rule;
  precedent is `Quantity` importing `CountSpec`.
- `TargetSpec` gains a third field `Exclusion` (spec sketched two) — the 2-field
  sketch has nowhere for §3/§4a's self-exclusion to live; a sum type avoids boolean
  blindness.
- `viewOfObject`/`viewOfCard` live in `Pawl.Projection`, not `Pawl.Filter` (spec
  put `viewOfCard` in `Pawl.Filter`) — `viewOfCard` needs `baseColorsOf` and
  `viewOfObject` needs `project`, both in `Projection`; siting them there keeps the
  dependency one-way and avoids a `Filter` ↔ `Projection` cycle. `Context` drops
  the unused `source` field (self-exclusion is never a `Filter` atom, so `matches`
  never reads it) — YAGNI + newtype rules.
- `Affected` self-exclusion is retired with an issue (Task 9) rather than preserved,
  because a 2-arm `Affected` cannot carry it and the arm has no producer/test.

**Type consistency check:** `MkTargetSpec Pool (Maybe Filter) Exclusion`,
`Filter.matches :: Context -> View -> Filter -> Bool`, `Filter.MkContext (Maybe
PlayerId)`, `Filter.MkView {cardTypes,supertypes,colors,subtypes,power,controller}`,
`Projection.viewOfObject :: ObjectId -> GameState -> Filter.View`,
`Projection.viewOfCard :: Card -> Filter.View`, `Affected.Matching Filter`,
`Effect.Search Filter` — used identically across all tasks.

**Placeholder scan:** the only intentionally-abbreviated spots are board-setup
`...` inside test snippets (Tasks 3, 4, 5, 6) where each step instructs the
implementer to read and reuse the surrounding spec file's *existing* fixtures
rather than invent a board; the assertions themselves are concrete. Every
production-code block is complete.
