# M4.5 P3b — Characteristic-defined P/T, the freeze, and the switch Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Finish CR 613 layer 7 — characteristic-defined power/toughness (7a) recomputed on every projection and copied as an *ability*, the CR 608.2h freeze that resolution-created continuous effects owe (the 7b gap), and P/T switching (7d).

**Architecture:** `Quantity` grows `Star`, `Plus` and `Count CountSpec` — a quantity that counts game state. The same counting quantity then behaves in two opposite ways, which is the phase's whole point: as a **characteristic-defining ability** it lives unevaluated in `ProjectedCharacteristics.characteristicPT` (seeded from `Card.characteristicPT`, copied by `copiableCharacteristics`, cleared by layer-6 `LoseAllAbilities`) and is evaluated **in place** at `Layer.CharacteristicPT` on every projection; as a **resolution-created continuous effect** it is evaluated once and rewritten to a `Literal` by `Projection.freezeQuantities`, called from `Resolve`'s `ModifyTarget` store path. `Modification.SwitchPowerToughness` adds the last sublayer. No new opcode, no new prompt, no change to `Object`, `GameState`, or the event pipeline.

**Tech Stack:** Haskell 2010 (GHC 9.14.1 from the Nix flake), `tasty` + `tasty-hunit`, the hand-rolled `Pawl.Json`/`Pawl.Codec` card codec.

**Spec:** `docs/superpowers/specs/2026-07-21-p3b-characteristic-defined-pt-design.md`. Read §2 before Task 1, §5 before Task 3, and §8 before Task 8.

## Global Constraints

Every task's requirements implicitly include all of these. They come from `CLAUDE.md` and are not negotiable.

- **Haskell 2010, no language extensions** unless there is no alternative. `NamedFieldPuns` is permitted; `GADTs`/`RankNTypes` only in the suspension core. Test modules that already carry `{-# LANGUAGE GADTs #-}` / `{-# LANGUAGE RankNTypes #-}` keep them.
- **No explicit export lists.** `module Pawl.Foo where`.
- **One type per module** under `Pawl.Type.<TypeName>` (type + instances only). A module never imports its parents.
- **Qualified imports aliased to the last component** (`Data.List` → `List`, `Pawl.Type.CountSpec` → `CountSpec`). One import group, alphabetical. The one documented exception is `Pawl.Support` as `S` in the test suite.
- **No partial functions**, written or used. No `head`, `error`, `undefined`, or non-exhaustive matches.
- **`newtype` liberally, non-punning constructors** (`MkFoo`). Build records with `do` + record syntax.
- **Prefer explicit:** `case` over point-free; one equation with a `case` over multiple clauses; `let` over `where`; `$` over parens, `.` over chained `$`. No list comprehensions. No backtick-infixed functions.
- **`Text` not `String`.** Arbitrary-precision numbers (`Integer`, `Natural`).
- **No boolean blindness** — a custom sum type beats a bare `Bool`.
- **Derive at least `Eq` and `Show`.** Types that ride a `Set`/`Map` key, a `Binding`, or `ProjectedCharacteristics` also derive `Ord`.
- **Every rules claim is checked against `docs/rules.txt`** and the rule number is cited in the code comment. Never trust recalled Magic rules.
- **Build must be warning-clean.** `cabal build all --enable-tests --enable-benchmarks` with `flags: +pedantic` (which is `-Werror`). Incremental builds hide warnings from unchanged modules; `cabal clean` first when a definitive check is needed.
- **Before every commit:** `git add <the paths this task names>`, then `hooky fix`, then `git add` again, then `hooky run`. `hooky` acts on **staged** files only; if you skip the `git add`, it reports "hooks skipped" and checks nothing.
- **TDD is not optional.** Write the failing test and actually run it to watch it fail before implementing.
- **Never edit this plan, weaken an assertion, or delete a test to make a check pass.** If the plan looks wrong, stop and say so.
- **One small complete commit per task, on `main`.** Commit messages end with the `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>` trailer.
- **Concurrent sessions share this checkout.** Stage the explicit paths each task names; do not blanket-stage foreign files that appear in `git status`.

## File Structure

**Library — one new module:**

| File | Responsibility |
|---|---|
| `source/library/Pawl/Type/CountSpec.hs` | **new.** What a counting quantity counts. Quarantined card-shaped growth; P9 retires the whole type. |

**Library — modified:**

| File | Responsibility after this plan |
|---|---|
| `source/library/Pawl/Type/Quantity.hs` | gains `Star`, `Plus Quantity Quantity`, `Count CountSpec` |
| `source/library/Pawl/Quantity.hs` | `evaluate` gains the "you" `PlayerId`; the three new arms; `substituteStar`; `typesInAllGraveyards` |
| `source/library/Pawl/Type/Card.hs` | gains `characteristicPT :: Maybe Quantity` |
| `source/library/Pawl/Type/ProjectedCharacteristics.hs` | gains `characteristicPT :: Maybe (Quantity, Quantity)` |
| `source/library/Pawl/Type/Modification.hs` | gains `SwitchPowerToughness` (layer 7d) |
| `source/library/Pawl/Type/Subtype.hs` | gains `Lhurgoyf` (205.3m) and `Arcane` (205.3k) |
| `source/library/Pawl/Projection.hs` | seeds `PC.characteristicPT`; folds layer 7a in place; `LoseAllAbilities` clears the CDA; `layer`/`applyModification` for the switch; new `freezeQuantities` |
| `source/library/Pawl/Resolve.hs` | `ModifyTarget` freezes at store time against the source and its controller |
| `source/library/Pawl/Codec.hs` | JSON for every type above |

**Data — new card files** under `data/cards/`: `tarmogoyf.json`, `inner-calm-outer-strength.json`, `twisted-image.json`. **No existing card file changes** — `characteristicPT` is omitted when `Nothing`, the `copyOnEnter`/`colorIndicator` precedent.

**Test suite:**

| File | Responsibility |
|---|---|
| `source/test-suite/Pawl/PowerToughnessSpec.hs` | **new.** The phase's own spec, the `ColorSpec` precedent: the CDA seed, layer 7a, the 611.2-scope guard, the freeze, the switch |
| `source/test-suite/Pawl/CoreSpec.hs` | the `Quantity` group grows the new arms; its four existing `evaluate` call sites gain the "you" argument |
| `source/test-suite/Pawl/CopySpec.hs` | gains the Clone-of-Tarmogoyf group (it owns `copyNewest`/`resolveAndSettle`) |
| `source/test-suite/Pawl/Support.hs` | gains `addGraveyardCard` |
| `source/test-suite/Pawl/Cards.hs` | three new printing fields, loads, and `allPrintings` entries |
| `source/test-suite/Main.hs` | wires `PowerToughnessSpec.tests` into `testTree` |

New `Pawl.*Spec` files must be added to the test-suite `other-modules` list in `pawl.cabal` — that field is generated by a `-- cabal-gild: discover` directive, so add the file and let `hooky fix` regenerate it rather than hand-editing.

---

### Task 1: The counting quantity

`Quantity` grows the three arms the numeric tower has promised since M4a, and `evaluate` gains the "you" player that a player-scoped count needs. No card uses any of it yet.

**Files:**
- Create: `source/library/Pawl/Type/CountSpec.hs`
- Modify: `source/library/Pawl/Type/Quantity.hs`
- Modify: `source/library/Pawl/Quantity.hs`
- Modify: `source/library/Pawl/Codec.hs`
- Modify: `source/library/Pawl/Projection.hs` (call sites only)
- Modify: `source/library/Pawl/Resolve.hs` (call sites only)
- Test: `source/test-suite/Pawl/CoreSpec.hs`

**Interfaces:**
- Produces: `Pawl.Type.CountSpec.CountSpec` = `CardTypesInAllGraveyards | CardsInYourHand`; `Pawl.Type.Quantity.Quantity` gains `Star`, `Plus Quantity Quantity`, `Count CountSpec`; `Pawl.Quantity.evaluate :: GameState -> ObjectId -> Maybe PlayerId -> Quantity -> Maybe Integer`; `Pawl.Quantity.substituteStar :: Quantity -> Quantity -> Quantity`; `Pawl.Codec.countSpecToJson` / `jsonToCountSpec`.

- [ ] **Step 1: Write the failing tests**

In `source/test-suite/Pawl/CoreSpec.hs`, replace the whole `quantityTests` list with this (the four existing cases gain the new `Nothing` argument; five cases are new):

```haskell
quantityTests :: Cards.Cards -> Tasty.TestTree
quantityTests cards =
  Tasty.testGroup
    "Quantity"
    [ HU.testCase "a literal evaluates to itself" $
        HU.assertEqual
          "literal"
          (Just 2)
          (Quantity.evaluate (Setup.emptyGame S.bothPlayers) (ObjectId.MkObjectId 0) Nothing (Quantity.Type.Literal 2)),
      HU.testCase "a literal may be negative" $
        HU.assertEqual
          "negative"
          (Just (-1))
          (Quantity.evaluate (Setup.emptyGame S.bothPlayers) (ObjectId.MkObjectId 0) Nothing (Quantity.Type.Literal (-1))),
      HU.testCase "evaluate reads X from the object's binding environment" $
        let (oid, gs) = withBoundAmount cards (Just 5)
         in HU.assertEqual "X = 5" (Just 5) (Quantity.evaluate gs oid Nothing Quantity.Type.X),
      HU.testCase "evaluate X is Nothing when no amount was bound" $
        let (oid, gs) = withBoundAmount cards Nothing
         in HU.assertEqual "unbound X" Nothing (Quantity.evaluate gs oid Nothing Quantity.Type.X),
      HU.testCase "CR 208.2 Star alone is not evaluable -- it is notation, resolved at the seed" $
        HU.assertEqual
          "Star"
          Nothing
          (Quantity.evaluate (Setup.emptyGame S.bothPlayers) (ObjectId.MkObjectId 0) Nothing Quantity.Type.Star),
      HU.testCase "CR 208.2 Plus adds, so 1+* composes without a new case" $
        HU.assertEqual
          "1 + 2"
          (Just 3)
          ( Quantity.evaluate
              (Setup.emptyGame S.bothPlayers)
              (ObjectId.MkObjectId 0)
              Nothing
              (Quantity.Type.Plus (Quantity.Type.Literal 1) (Quantity.Type.Literal 2))
          ),
      HU.testCase "Plus is Nothing when either side is unevaluable" $
        HU.assertEqual
          "1 + Star"
          Nothing
          ( Quantity.evaluate
              (Setup.emptyGame S.bothPlayers)
              (ObjectId.MkObjectId 0)
              Nothing
              (Quantity.Type.Plus (Quantity.Type.Literal 1) Quantity.Type.Star)
          ),
      HU.testCase "substituteStar replaces Star everywhere, including inside Plus" $
        HU.assertEqual
          "1 + Literal 7"
          (Quantity.Type.Plus (Quantity.Type.Literal 1) (Quantity.Type.Literal 7))
          ( Quantity.substituteStar
              (Quantity.Type.Literal 7)
              (Quantity.Type.Plus (Quantity.Type.Literal 1) Quantity.Type.Star)
          ),
      HU.testCase "Count CardsInYourHand is Nothing with no 'you'" $
        HU.assertEqual
          "no player"
          Nothing
          ( Quantity.evaluate
              (Setup.emptyGame S.bothPlayers)
              (ObjectId.MkObjectId 0)
              Nothing
              (Quantity.Type.Count CountSpec.CardsInYourHand)
          ),
      HU.testCase "Count CardsInYourHand counts that player's hand" $
        let (gs, _) = S.handOne (Cards.pikerPrinting cards) (Setup.emptyGame S.bothPlayers)
         in HU.assertEqual
              "one card"
              (Just 1)
              (Quantity.evaluate gs (ObjectId.MkObjectId 0) (Just S.alice) (Quantity.Type.Count CountSpec.CardsInYourHand)),
      HU.testCase "Count CardTypesInAllGraveyards counts DISTINCT card types, not cards" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, one) = S.addGraveyardCard (Cards.pikerPrinting cards) S.alice gs0
            (_, two) = S.addGraveyardCard (Cards.warMammothPrinting cards) S.bob one
            (_, three) = S.addGraveyardCard (Cards.lightningBoltPrinting cards) S.alice two
         in do
              HU.assertEqual
                "two creatures in two graveyards is one type"
                (Just 1)
                (Quantity.evaluate two (ObjectId.MkObjectId 0) Nothing (Quantity.Type.Count CountSpec.CardTypesInAllGraveyards))
              HU.assertEqual
                "adding an instant makes two"
                (Just 2)
                (Quantity.evaluate three (ObjectId.MkObjectId 0) Nothing (Quantity.Type.Count CountSpec.CardTypesInAllGraveyards))
    ]
```

Add these imports to `source/test-suite/Pawl/CoreSpec.hs` (alphabetically, in the single import group):

```haskell
import qualified Pawl.Type.CountSpec as CountSpec
```

`S.addGraveyardCard` does not exist yet. Add it — and `addHandCard` beside it — to `source/test-suite/Pawl/Support.hs`, immediately after `addLibraryCard` (which they mirror — same object shape, a different zone and index).

**`addHandCard` is not redundant with `handOne`.** `handOne` (`Support.hs:439`) uses `Map.insert alice (Seq.singleton oid)`, which **replaces** alice's hand — calling it twice leaves one card, not two. It also sets the phase, active player and priority, which is what makes a cast possible. So the idiom for a multi-card hand is `handOne` **first** (for the card being cast, and for the phase setup), then `addHandCard` for each additional card:

```haskell
-- One card of a printing in pid's graveyard.
addGraveyardCard :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
addGraveyardCard printing pid gs =
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Graveyard,
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
            GameState.graveyard = Map.insertWith (Seq.><) pid (Seq.singleton oid) (GameState.graveyard gs2)
          }
      )

-- One more card of a printing in pid's hand, APPENDED (contrast handOne, which
-- replaces the hand and sets up the phase for a cast).
addHandCard :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
addHandCard printing pid gs =
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.hand = Map.insertWith (Seq.><) pid (Seq.singleton oid) (GameState.hand gs2)
          }
      )
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cabal test 2>&1 | tail -40`
Expected: FAIL — compilation errors, `Data constructor not in scope: Quantity.Type.Star`, `Module 'Pawl.Type.CountSpec' does not exist`, and `evaluate` applied to too many arguments.

- [ ] **Step 3: Create `Pawl.Type.CountSpec`**

Create `source/library/Pawl/Type/CountSpec.hs`:

```haskell
module Pawl.Type.CountSpec where

-- What a Quantity.Count counts. A first-order, analyzable classification, never
-- a predicate function -- the TargetSpec.WallTarget posture: one hand-carved
-- variant per card, specific before general.
--
-- Deliberately its own type rather than flat arms on Quantity: Quantity is a
-- small, closed numeric-tower type (CR 107.3, 208.2), and card-shaped growth
-- belongs somewhere P9's criterion/filter language can retire WHOLESALE.
--
-- EXPIRES at P9.
--
-- Both inhabitants read only zone membership and PRINTED card types, never the
-- projection -- Pawl.Quantity cannot import Pawl.Projection (Projection imports
-- Quantity), and a count evaluated inside the layer fold would recurse into the
-- fold that called it. A count over projected state ("lands you control") is a
-- named deferral in the P3b spec, section 8.
data CountSpec
  = -- CR 208.2a: Tarmogoyf. The number of DISTINCT card types among the cards in
    -- every graveyard -- a count of types, not of objects.
    CardTypesInAllGraveyards
  | -- CR 608.2h: Inner Calm, Outer Strength. The size of the "you" player's hand,
    -- where "you" is supplied by the caller (the resolving spell's controller, or
    -- the object's own controller for a characteristic-defining ability).
    CardsInYourHand
  deriving (Eq, Ord, Show)
```

- [ ] **Step 4: Grow `Quantity`**

In `source/library/Pawl/Type/Quantity.hs`, add the import and the three constructors. The module comment's "Grows:" paragraph is now partly cashed — update it to say so:

```haskell
module Pawl.Type.Quantity where

import Pawl.Type.CountSpec (CountSpec)
```

Replace the "Grows:" comment paragraph with:

```haskell
-- Grows further: Half (Little Girl), Infinite (Mox Lotus). Star, Plus and Count
-- landed at P3b. Plus is binary and recursive so composition covers the awkward
-- printed values without new cases: 1+* is Plus (Literal 1) Star.
```

and add these arms after the `X` arm:

```haskell
  | -- CR 208.2 / 208.2a: the star printed in a power/toughness box, standing for
    -- a value a characteristic-defining ability defines. NOTATION, not a value:
    -- Quantity.evaluate returns Nothing for it. Projection.baseCharacteristics
    -- resolves it at the seed by substituting Card.characteristicPT, so a Star
    -- never survives into a projection.
    Star
  | -- CR 208.2: composition, so a printed 1+* needs no constructor of its own.
    Plus Quantity Quantity
  | -- A quantity that counts game state (CR 208.2a, CR 608.2h). See CountSpec
    -- for why the payload is its own type.
    Count CountSpec
```

- [ ] **Step 5: Teach `Pawl.Quantity` the new arms and the "you" player**

Replace `source/library/Pawl/Quantity.hs` in full:

```haskell
module Pawl.Quantity where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Binding as Binding
import qualified Pawl.Game as Game
import qualified Pawl.Type.Card as Card
import Pawl.Type.CardType (CardType)
import qualified Pawl.Type.CountSpec as CountSpec
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.Quantity (Quantity)
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.TypeLine as TypeLine
import qualified Pawl.Type.Zone as Zone

-- Nothing when the value cannot be determined.
--
-- `you` is the player a player-scoped count is relative to (CR 608.2h). The
-- caller supplies it because this module cannot ask the projection who controls
-- what: Pawl.Projection imports Pawl.Quantity, so the arrow only points one way.
-- Resolve passes the resolving spell's controller; Projection passes the
-- object's own controller.
evaluate :: GameState -> ObjectId -> Maybe PlayerId -> Quantity -> Maybe Integer
evaluate gs oid you quantity = case quantity of
  Quantity.Literal n -> Just n
  Quantity.ManaValue -> fmap manaValueOf (Game.cardOf oid gs)
  -- CR 601.2b: read the chosen X from the source object's binding environment.
  Quantity.X -> case Game.lookupObject oid gs of
    Nothing -> Nothing
    Just obj -> fmap toInteger (Binding.amountOf Binding.variableX (Object.bindings obj))
  -- CR 208.2: a bare star has no value of its own. The projection substitutes
  -- the object's characteristic-defining quantity for it at the seed
  -- (Projection.baseCharacteristics), so reaching this arm means the star was
  -- never resolved -- honestly Nothing, not a hole.
  Quantity.Star -> Nothing
  Quantity.Plus a b -> case (evaluate gs oid you a, evaluate gs oid you b) of
    (Just x, Just y) -> Just (x + y)
    _ -> Nothing
  Quantity.Count spec -> countOf gs you spec

-- The one place a CountSpec is interpreted.
countOf :: GameState -> Maybe PlayerId -> CountSpec.CountSpec -> Maybe Integer
countOf gs you spec = case spec of
  -- CR 208.2a: Tarmogoyf counts card TYPES, so this is the size of the union,
  -- not the number of cards.
  CountSpec.CardTypesInAllGraveyards -> Just (toInteger (Set.size (typesInAllGraveyards gs)))
  CountSpec.CardsInYourHand -> case you of
    Nothing -> Nothing
    Just pid -> Just (toInteger (length (Game.zoneMembers Zone.Hand pid gs)))

-- The distinct card types among the cards in every player's graveyard. Reads the
-- PRINTED type line (Game.cardOf), never the projection: nothing projects a
-- graveyard card today, and a projected read here would recurse into the layer
-- fold that calls this. Expiry in the P3b spec, section 8.
typesInAllGraveyards :: GameState -> Set CardType
typesInAllGraveyards gs =
  let ids = concatMap Foldable.toList (Map.elems (GameState.graveyard gs))
      typesOf oid = case Game.cardOf oid gs of
        Nothing -> Set.empty
        Just card -> TypeLine.types (Card.typeLine card)
   in Set.unions (map typesOf ids)

-- CR 208.2: resolve a printed star to the quantity a characteristic-defining
-- ability supplies, recursing through Plus so 1+* becomes 1+<the count>.
substituteStar :: Quantity -> Quantity -> Quantity
substituteStar star quantity = case quantity of
  Quantity.Star -> star
  Quantity.Plus a b -> Quantity.Plus (substituteStar star a) (substituteStar star b)
  Quantity.Literal _ -> quantity
  Quantity.ManaValue -> quantity
  Quantity.X -> quantity
  Quantity.Count _ -> quantity

-- CR 202.3: the mana value is the total amount of mana in the cost -- each
-- generic symbol contributes its number, each colored/typed symbol contributes
-- one. A land has no mana cost (CR 202.1), so its mana value is 0.
manaValueOf :: Card.Card -> Integer
manaValueOf card = case Card.manaCost card of
  Nothing -> 0
  Just (ManaCost.MkManaCost symbols) -> sum (map symbolValue symbols)

symbolValue :: ManaSymbol.ManaSymbol -> Integer
symbolValue symbol = case symbol of
  ManaSymbol.Generic n -> toInteger n
  ManaSymbol.OfType _ -> 1
  -- CR 202.3b: off the stack a variable's contribution to mana value is 0.
  ManaSymbol.Variable -> 0
```

- [ ] **Step 6: Update the six library call sites**

In `source/library/Pawl/Resolve.hs`, every `Quantity.evaluate gs source quantity` becomes `Quantity.evaluate gs source (Just controller) quantity`. There are six, at lines 322, 435, 446, 460, 479 and 511. Run this to find them all and confirm the count:

```bash
grep -n "Quantity.evaluate gs source quantity" source/library/Pawl/Resolve.hs | wc -l
```

Expected: `6`.

In `source/library/Pawl/Projection.hs`, the four call sites inside `applyModification` and `baseCharacteristics` gain the affected object's own controller. In `applyModification`:

```haskell
  Modification.SetBasePowerToughness p t ->
    pc
      { PC.power = setPT (PC.power pc) (Quantity.evaluate gs oid (controllerOf oid gs) p),
        PC.toughness = setPT (PC.toughness pc) (Quantity.evaluate gs oid (controllerOf oid gs) t)
      }
  Modification.ModifyPowerToughness p t ->
    pc
      { PC.power = addPT (PC.power pc) (Quantity.evaluate gs oid (controllerOf oid gs) p),
        PC.toughness = addPT (PC.toughness pc) (Quantity.evaluate gs oid (controllerOf oid gs) t)
      }
```

and in `baseCharacteristics`:

```haskell
        PC.power = case Card.Type.power card of
          Nothing -> Nothing
          Just (Power.MkPower q) -> Quantity.evaluate gs oid (controllerOf oid gs) q,
        PC.toughness = case Card.Type.toughness card of
          Nothing -> Nothing
          Just (Toughness.MkToughness q) -> Quantity.evaluate gs oid (controllerOf oid gs) q,
```

`controllerOf` is defined further down the same module; Haskell does not care about definition order. It reads `GameState.continuousEffects` directly and never calls `project`, so this introduces no recursion.

Also replace the stale note above `applyModification` — the `-- When X lands, Resolve must freeze ...` sentence is cashed in Task 6 — with:

```haskell
-- Apply one modification to characteristics-in-progress. THE ONE applier
-- (Resolve : Effect :: Projection : Modification). P/T quantities are evaluated
-- here against the CURRENT state, which is correct for a static ability's
-- continuous effect (CR 604.2 -- Opalescence's mana value is re-read per affected
-- object every projection). A continuous effect created by a spell's RESOLUTION
-- must not be re-read (CR 608.2h / 611.2d); it is frozen to Literals at store
-- time by Resolve, via freezeQuantities.
```

- [ ] **Step 7: Add the codec arms**

In `source/library/Pawl/Codec.hs`, add a `CountSpec` codec beside the other nullary codecs (put it immediately after the `CounterKind` pair, around line 158):

```haskell
countSpecToJson :: CountSpec.CountSpec -> Value
countSpecToJson s = nullary . Text.pack $ case s of
  CountSpec.CardTypesInAllGraveyards -> "CardTypesInAllGraveyards"
  CountSpec.CardsInYourHand -> "CardsInYourHand"

jsonToCountSpec :: Value -> Either Text CountSpec.CountSpec
jsonToCountSpec =
  decodeNullary
    (Text.pack "CountSpec")
    [ (Text.pack "CardTypesInAllGraveyards", CountSpec.CardTypesInAllGraveyards),
      (Text.pack "CardsInYourHand", CountSpec.CardsInYourHand)
    ]
```

and replace the two `Quantity` functions:

```haskell
quantityToJson :: Quantity.Quantity -> Value
quantityToJson q = case q of
  Quantity.Literal n -> Json.tagged (Text.pack "Literal") (Just (Json.jInt n))
  Quantity.ManaValue -> nullary (Text.pack "ManaValue")
  Quantity.X -> nullary (Text.pack "X")
  Quantity.Star -> nullary (Text.pack "Star")
  Quantity.Plus a b -> Json.tagged (Text.pack "Plus") (Just (Array [quantityToJson a, quantityToJson b]))
  Quantity.Count s -> Json.tagged (Text.pack "Count") (Just (countSpecToJson s))

jsonToQuantity :: Value -> Either Text Quantity.Quantity
jsonToQuantity value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Literal", Just v) -> Quantity.Literal <$> Json.asInteger v
    ("ManaValue", _) -> Right Quantity.ManaValue
    ("X", _) -> Right Quantity.X
    ("Star", _) -> Right Quantity.Star
    ("Plus", Just (Array [x, y])) -> Quantity.Plus <$> jsonToQuantity x <*> jsonToQuantity y
    ("Count", Just v) -> Quantity.Count <$> jsonToCountSpec v
    _ -> Left (Text.pack "unknown Quantity: " <> t)
```

Add the import `import qualified Pawl.Type.CountSpec as CountSpec` in alphabetical position.

- [ ] **Step 8: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -20`
Expected: PASS, warning-free.

- [ ] **Step 9: Commit**

```bash
git add source/library/Pawl/Type/CountSpec.hs source/library/Pawl/Type/Quantity.hs source/library/Pawl/Quantity.hs source/library/Pawl/Codec.hs source/library/Pawl/Projection.hs source/library/Pawl/Resolve.hs source/test-suite/Pawl/CoreSpec.hs source/test-suite/Pawl/Support.hs pawl.cabal
hooky fix
git add source/library/Pawl/Type/CountSpec.hs source/library/Pawl/Type/Quantity.hs source/library/Pawl/Quantity.hs source/library/Pawl/Codec.hs source/library/Pawl/Projection.hs source/library/Pawl/Resolve.hs source/test-suite/Pawl/CoreSpec.hs source/test-suite/Pawl/Support.hs pawl.cabal
hooky run
git commit -m "$(cat <<'EOF'
feat(m4.5-p3b): Quantity.Star/Plus/Count, the counting quantity (CR 208.2, 608.2h)

Cashes the growth Quantity's own module comment has named since M4a. Count
carries its own quarantined CountSpec rather than flat arms, so P9's filter
language retires the card-shaped growth wholesale. evaluate gains the "you"
player a player-scoped count needs -- supplied by the caller, since
Pawl.Quantity cannot import Pawl.Projection.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 2: The CDA as a copiable characteristic, and Tarmogoyf

The card gains the ability; the projection seeds it as a pair of **unevaluated** quantities. Layer 7a is not wired yet, so Tarmogoyf still projects no P/T — which is exactly CR 613.4a and is what this task asserts.

**Files:**
- Modify: `source/library/Pawl/Type/Subtype.hs`
- Modify: `source/library/Pawl/Type/Card.hs`
- Modify: `source/library/Pawl/Type/ProjectedCharacteristics.hs`
- Modify: `source/library/Pawl/Projection.hs`
- Modify: `source/library/Pawl/Codec.hs`
- Create: `data/cards/tarmogoyf.json`
- Modify: `source/test-suite/Pawl/Cards.hs`
- Modify: `source/test-suite/Main.hs`
- Create: `source/test-suite/Pawl/PowerToughnessSpec.hs`
- Test: `source/test-suite/Pawl/PowerToughnessSpec.hs`

**Interfaces:**
- Consumes: Task 1's `Quantity.Star` / `Plus` / `Count`, `Quantity.substituteStar`, `Codec.quantityToJson` / `jsonToQuantity`.
- Produces: `Card.characteristicPT :: Maybe Quantity`; `ProjectedCharacteristics.characteristicPT :: Maybe (Quantity, Quantity)`; `Projection.seedCharacteristicPT :: Card -> Maybe (Quantity, Quantity)`; `Subtype.Lhurgoyf`; `Cards.tarmogoyfPrinting`; `PowerToughnessSpec.tests :: Cards.Cards -> Tasty.TestTree`.

- [ ] **Step 0: Pin Tarmogoyf against Scryfall**

Look up Tarmogoyf on Scryfall and confirm, verbatim: its mana cost, its type line, its printed power and toughness box, and its oracle text. The vendored MTGJSON dump is a candidate source only (`card-data-source`), and the P3b spec deliberately omits mana costs because two extraction windows disagreed on another card's. Write the confirmed values into the JSON in Step 4. Expected (confirm, do not assume): `{1}{G}`, `Creature — Lhurgoyf`, `*/1+*`, "Tarmogoyf's power is equal to the number of card types among cards in all graveyards and its toughness is equal to that number plus 1."

- [ ] **Step 1: Write the failing test**

Create `source/test-suite/Pawl/PowerToughnessSpec.hs`:

```haskell
-- Covers: Pawl.Projection (CR 613 layer 7 -- 7a characteristic-defined P/T, the
-- CR 608.2h freeze that 7b's stored effects owe, and 7d P/T switching), Pawl.Quantity
-- (the counting quantity) and the P3b gates (Tarmogoyf, Inner Calm Outer Strength,
-- Twisted Image). Gameplay-level: each card is cast or resolved through the stack and
-- the resulting game state is asserted on.
module Pawl.PowerToughnessSpec where

import qualified Pawl.Cards as Cards
import qualified Pawl.Projection as Projection
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.CountSpec as CountSpec
import qualified Pawl.Type.ProjectedCharacteristics as PC
import qualified Pawl.Type.Quantity as Quantity.Type
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "PowerToughness"
    [ HU.testCase "CR 604.3 the seed carries the CDA as QUANTITIES, with the printed star substituted" $
        -- CR 707.2a: a copy acquires the ABILITY, so what the seed (and therefore
        -- the copiable value) holds must be unevaluated. Tarmogoyf's printed box is
        -- */1+*, so the pair is <count> and 1+<count>.
        let gs0 = Setup.emptyGame S.bothPlayers
            (goyfId, gs) = S.addCreature (Cards.tarmogoyfPrinting cards) S.alice gs0
            count = Quantity.Type.Count CountSpec.CardTypesInAllGraveyards
         in HU.assertEqual
              "the CDA pair"
              (Just (count, Quantity.Type.Plus (Quantity.Type.Literal 1) count))
              (PC.characteristicPT (Projection.baseCharacteristics goyfId gs)),
      HU.testCase "CR 613.4a no P/T value exists before layer 7a applies one" $
        -- The seed evaluates the printed Star, which is deliberately Nothing.
        let gs0 = Setup.emptyGame S.bothPlayers
            (goyfId, gs) = S.addCreature (Cards.tarmogoyfPrinting cards) S.alice gs0
            seeded = Projection.baseCharacteristics goyfId gs
         in do
              HU.assertEqual "no seeded power" Nothing (PC.power seeded)
              HU.assertEqual "no seeded toughness" Nothing (PC.toughness seeded),
      HU.testCase "an ordinary card has no CDA" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (pikerId, gs) = S.addPiker cards S.alice gs0
         in HU.assertEqual "none" Nothing (PC.characteristicPT (Projection.baseCharacteristics pikerId gs))
    ]
```

Wire it into `source/test-suite/Main.hs`: add `import qualified Pawl.PowerToughnessSpec as PowerToughnessSpec` in alphabetical position, and add `PowerToughnessSpec.tests cards,` to `testTree` immediately after `ProjectionSpec.tests cards,`.

- [ ] **Step 2: Run the test to verify it fails**

Run: `cabal test 2>&1 | tail -30`
Expected: FAIL — `Cards.tarmogoyfPrinting` is not in scope, and `PC.characteristicPT` does not exist.

- [ ] **Step 3: Add the two fields and the subtype**

In `source/library/Pawl/Type/Subtype.hs`, add after `Shapeshifter`:

```haskell
  | Lhurgoyf -- CR 205.3m (a creature type; Tarmogoyf's printed type)
```

In `source/library/Pawl/Type/Card.hs`, add the import `import Pawl.Type.Quantity (Quantity)` and this field after `colorIndicator`:

```haskell
    -- CR 604.3 / 208.2a: this card's characteristic-defining P/T ability -- the
    -- quantity a printed star (Quantity.Star) in its power/toughness box stands
    -- for. Nothing for every card without a star. A CDA is an ABILITY, not a
    -- number: the projection seeds it unevaluated so a copy acquires the ability
    -- (CR 707.2a) and layer 7a recomputes it on every projection. Read through
    -- Pawl.Projection, never directly.
    characteristicPT :: Maybe Quantity,
```

In `source/library/Pawl/Type/ProjectedCharacteristics.hs`, add the import `import Pawl.Type.Quantity (Quantity)` and this field after `toughness`:

```haskell
    -- CR 613.4a layer 7a: the object's characteristic-defining P/T, as the pair of
    -- UNEVALUATED quantities (power, toughness) with the printed star already
    -- substituted. Seeded from the card, so it rides copiableCharacteristics and a
    -- Clone acquires the ability rather than the number (CR 707.2a); emptied by
    -- LoseAllAbilities at layer 6, which is BEFORE 7a.
    characteristicPT :: Maybe (Quantity, Quantity),
```

- [ ] **Step 4: Seed it in the projection, and write the card**

In `source/library/Pawl/Projection.hs`, add this function immediately above `baseCharacteristics`:

```haskell
-- CR 208.2 / 604.3: the card's characteristic-defining P/T as a pair of
-- quantities, with the printed star resolved to what the CDA counts. Nothing
-- unless the card declares a CDA *and* has a printed power and toughness box for
-- the star to sit in (CR 208.1) -- a card with one and not the other is
-- malformed data, and yields no CDA rather than a partial one.
seedCharacteristicPT :: Card.Type.Card -> Maybe (Quantity.Type.Quantity, Quantity.Type.Quantity)
seedCharacteristicPT card =
  case (Card.Type.characteristicPT card, Card.Type.power card, Card.Type.toughness card) of
    (Just star, Just (Power.MkPower p), Just (Toughness.MkToughness t)) ->
      Just (Quantity.substituteStar star p, Quantity.substituteStar star t)
    _ -> Nothing
```

In `baseCharacteristics`, add `PC.characteristicPT = Nothing,` to the no-card record (after `PC.toughness = Nothing,`) and `PC.characteristicPT = seedCharacteristicPT card,` to the card record (after the `PC.toughness = ...` field).

In `source/library/Pawl/Codec.hs`, add `Subtype.Lhurgoyf -> "Lhurgoyf"` to `subtypeToJson` and `(Text.pack "Lhurgoyf", Subtype.Lhurgoyf)` to `jsonToSubtype`. Then append the new optional field to `cardToJson`, **after** the `colorIndicator` block so every existing file stays byte-identical:

```haskell
        ++ ( case CardT.characteristicPT c of
               Nothing -> []
               Just q -> [(Text.pack "characteristicPT", quantityToJson q)]
           )
```

and in `jsonToCard`, add the decode line after `colorIndicator` and the field to the record:

```haskell
  characteristicPT <- maybeFrom jsonToQuantity (getOpt (Text.pack "characteristicPT") ps)
```

```haskell
        CardT.characteristicPT = characteristicPT
```

Create `data/cards/tarmogoyf.json` as a single line (match Step 0's confirmed values; this is the expected shape):

```json
{"name":"Tarmogoyf","manaCost":[{"type":"Generic","value":1},{"type":"OfType","value":{"type":"Colored","value":{"type":"Green"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Creature"}],"subtypes":[{"type":"Lhurgoyf"}]},"power":{"type":"Star"},"toughness":{"type":"Plus","value":[{"type":"Literal","value":1},{"type":"Star"}]},"keywords":[],"staticAbilities":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"characteristicPT":{"type":"Count","value":{"type":"CardTypesInAllGraveyards"}}}
```

In `source/test-suite/Pawl/Cards.hs`, add `tarmogoyfPrinting :: Printing.Printing,` to the record (after `aphoticWispsPrinting`, which needs a trailing comma added), `tarmogoyfPrinting_ <- loadPrinting "tarmogoyf"` to `loadCards`, `tarmogoyfPrinting = tarmogoyfPrinting_` to the returned record, and `tarmogoyfPrinting cards` to `allPrintings`. It is a **deterministic fixture**: in `allPrintings` for the M3.5 honesty round-trip, in **no** deck.

- [ ] **Step 5: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -20`
Expected: PASS. The `Cards` round-trip test proves `tarmogoyf.json` decodes and re-encodes byte-identically; every other card file is unchanged.

Confirm no existing card file drifted:

```bash
git status --short data/cards/
```

Expected: only `?? data/cards/tarmogoyf.json`.

- [ ] **Step 6: Commit**

```bash
git add source/library/Pawl/Type/Subtype.hs source/library/Pawl/Type/Card.hs source/library/Pawl/Type/ProjectedCharacteristics.hs source/library/Pawl/Projection.hs source/library/Pawl/Codec.hs data/cards/tarmogoyf.json source/test-suite/Pawl/Cards.hs source/test-suite/Pawl/PowerToughnessSpec.hs source/test-suite/Main.hs pawl.cabal
hooky fix
git add source/library/Pawl/Type/Subtype.hs source/library/Pawl/Type/Card.hs source/library/Pawl/Type/ProjectedCharacteristics.hs source/library/Pawl/Projection.hs source/library/Pawl/Codec.hs data/cards/tarmogoyf.json source/test-suite/Pawl/Cards.hs source/test-suite/Pawl/PowerToughnessSpec.hs source/test-suite/Main.hs pawl.cabal
hooky run
git commit -m "$(cat <<'EOF'
feat(m4.5-p3b): seed the CDA as unevaluated quantities, add Tarmogoyf (CR 604.3)

Card.characteristicPT says what a printed star counts; the projection seeds
ProjectedCharacteristics.characteristicPT as the pair (power, toughness) with
the star substituted -- QUANTITIES, not numbers, so the value rides
copiableCharacteristics and a copy acquires the ability (CR 707.2a).

Layer 7a is not wired yet, so Tarmogoyf projects no P/T at all. That is CR
613.4a, not a gap, and the test asserts it.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 3: Layer 7a, folded in place

**Files:**
- Modify: `source/library/Pawl/Projection.hs`
- Test: `source/test-suite/Pawl/PowerToughnessSpec.hs`

**Interfaces:**
- Consumes: Task 2's `PC.characteristicPT`; Task 1's `Quantity.evaluate`.
- Produces: `Projection.applyCharacteristicPT :: GameState -> ObjectId -> ProjectedCharacteristics -> ProjectedCharacteristics`; Tarmogoyf now projects a real P/T through `Projection.powerOf` / `toughnessOf`.

- [ ] **Step 1: Write the failing tests**

Append these three cases to the `PowerToughness` list in `source/test-suite/Pawl/PowerToughnessSpec.hs` (add a comma after the last existing case):

```haskell
      HU.testCase "CR 613.4a Tarmogoyf's P/T is recomputed, not fixed at entry" $
        -- THE FALSIFIER for evaluating a printed * once, at the seed or at entry:
        -- nothing touches the Goyf, and its P/T moves because a graveyard did.
        -- Empty graveyards -> 0 card types -> 0/1. Fog resolves and is put into
        -- its owner's graveyard (CR 608.2m), adding the Instant type.
        --
        -- Fog, NOT Lightning Bolt: Bolt targets, S.identityAnswer would aim it at
        -- the only creature on the board, and 3 damage would kill the 0/1 Goyf
        -- being measured. Fog has no target and no effect outside combat.
        let base = S.landsInPlay (Cards.forestPrinting cards) 1
            (goyfId, board) = S.addCreature (Cards.tarmogoyfPrinting cards) S.alice base
            (gs, fogId) = S.handOne (Cards.fogPrinting cards) board
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice fogId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "before: no card types in any graveyard, so 0 power" (Just 0) (Projection.powerOf goyfId board)
              HU.assertEqual "before: 0+1 toughness" (Just 1) (Projection.toughnessOf goyfId board)
              HU.assertEqual "after: one card type (Instant), so 1 power" (Just 1) (Projection.powerOf goyfId after)
              HU.assertEqual "after: 1+1 toughness" (Just 2) (Projection.toughnessOf goyfId after),
      HU.testCase "CR 208.2a 2007-10-01 the CDA works in all zones, and a Goyf in a graveyard counts itself" $
        -- Gatherer ruling on Tarmogoyf (WotC, 2007-10-01): "The ability that
        -- defines Tarmogoyf's power and toughness works in all zones, not just
        -- the battlefield. If Tarmogoyf is in your graveyard, it will count
        -- itself." CR 604.3 says a CDA functions in all zones, and CR 208.2a
        -- repeats it for P/T. This is the assertion that a gather-based
        -- implementation cannot make: gather only walks the battlefield.
        let gs0 = Setup.emptyGame S.bothPlayers
            (goyfId, gs) = S.addGraveyardCard (Cards.tarmogoyfPrinting cards) S.alice gs0
         in do
              HU.assertEqual "the Goyf in the graveyard is a creature card, so power 1" (Just 1) (Projection.powerOf goyfId gs)
              HU.assertEqual "1+1 toughness" (Just 2) (Projection.toughnessOf goyfId gs),
      HU.testCase "CR 613.4a/613.4c layer 7a runs before 7c, so a counter adds to the CDA" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, withCard) = S.addGraveyardCard (Cards.lightningBoltPrinting cards) S.alice gs0
            (goyfId, board) = S.addCreature (Cards.tarmogoyfPrinting cards) S.alice withCard
            gs = S.addCounter CounterKind.PlusOnePlusOne 1 goyfId board
         in do
              HU.assertEqual "1 card type + 1 counter" (Just 2) (Projection.powerOf goyfId gs)
              HU.assertEqual "1+1 toughness + 1 counter" (Just 3) (Projection.toughnessOf goyfId gs)
```

Add these imports to `source/test-suite/Pawl/PowerToughnessSpec.hs`:

```haskell
import qualified Pawl.Cast as Cast
import qualified Pawl.Engine as Engine
import qualified Pawl.Stack as Stack
import qualified Pawl.Type.CounterKind as CounterKind
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cabal test --test-options='-p "$0~/PowerToughness/"' 2>&1 | tail -30`
Expected: FAIL — `Just 0 /= Nothing` (and the same for toughness). Layer 7a is not applied, so the Goyf still projects nothing.

- [ ] **Step 3: Apply the CDA at layer 7a**

In `source/library/Pawl/Projection.hs`, add this function immediately above `projectFrom`:

```haskell
-- CR 613.4a layer 7a: apply the object's own characteristic-defining P/T ability.
-- Read from the PARTIAL projection (post-layer-6), so LoseAllAbilities can strip
-- it first; evaluated against the CURRENT state, so it recomputes on every
-- projection.
--
-- Folded IN PLACE rather than emitted as a synthetic Gathered the way
-- counterGathered emits layer-7c counters, for three reasons:
--
--   * gather runs BEFORE the fold and has no partial to read, so a pre-gathered
--     CDA could never be removed by Humility at layer 6;
--   * CR 604.3 (and CR 208.2a for P/T specifically) says a CDA functions in ALL
--     zones. gather walks the battlefield only; projectFrom is not zone-scoped, so
--     in-place gets all-zones behaviour for free -- a Tarmogoyf in a graveyard has
--     a power, and counts itself;
--   * a CDA has no source object and no timestamp, so it has nothing to sort on
--     under CR 613.7 and does not belong in the candidate list at all.
--
-- setPT (not a bare assignment) so an unevaluable quantity leaves the value
-- alone, the powerOf posture used throughout this module.
applyCharacteristicPT :: GameState -> ObjectId -> ProjectedCharacteristics -> ProjectedCharacteristics
applyCharacteristicPT gs oid pc = case PC.characteristicPT pc of
  Nothing -> pc
  Just (p, t) ->
    let you = controllerOf oid gs
     in pc
          { PC.power = setPT (PC.power pc) (Quantity.evaluate gs oid you p),
            PC.toughness = setPT (PC.toughness pc) (Quantity.evaluate gs oid you t)
          }
```

and replace `projectFrom`:

```haskell
-- Project one object against a PRECOMPUTED candidate list. gather is
-- oid-independent, so a whole-board sweep gathers once and folds each object
-- (projectAll) instead of re-gathering per object.
--
-- Layer 7a is ALWAYS in the layer list, even when no gathered effect lives there:
-- an object's own characteristic-defining ability is not a gathered candidate
-- (see applyCharacteristicPT). For an object with no CDA the extra pass is an
-- identity function over an empty candidate filter.
projectFrom :: [Gathered] -> ObjectId -> GameState -> ProjectedCharacteristics
projectFrom cands oid gs =
  let layers = Set.toAscList (Set.insert Layer.CharacteristicPT (Set.fromList (map gLayer cands)))
      applyLayer partial lyr =
        let seeded =
              if lyr == Layer.CharacteristicPT
                then applyCharacteristicPT gs oid partial
                else partial
            here = filter (\c -> gLayer c == lyr && affects (gSource c) oid (gAffected c) seeded gs) cands
            -- CR 613.7 timestamp order within a layer. EXPIRES: CR 613.8b dependency
            -- (a same-layer effect that changes which objects another applies to)
            -- would override this. Deferred -- no M3c card falsifies it; existence
            -- dependencies are handled by staticAbilitiesLive. git-bug f90e0c4.
            ordered = List.sortOn gTimestamp here
            step pc c = applyModification gs oid (gModification c) pc
         in List.foldl' step seeded ordered
   in List.foldl' applyLayer (copiableCharacteristics oid gs) layers
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -20`
Expected: PASS, warning-free, with no regressions in `ProjectionSpec` or `CopySpec`.

- [ ] **Step 5: Commit**

```bash
git add source/library/Pawl/Projection.hs source/test-suite/Pawl/PowerToughnessSpec.hs
hooky fix
git add source/library/Pawl/Projection.hs source/test-suite/Pawl/PowerToughnessSpec.hs
hooky run
git commit -m "$(cat <<'EOF'
feat(m4.5-p3b): fold layer 7a in place, so Tarmogoyf recomputes (CR 613.4a)

The CDA is read from the PARTIAL projection at Layer.CharacteristicPT, which
is always in the layer list. Not a synthetic Gathered like counterGathered:
gather has no partial (so Humility could not strip it), gather is
battlefield-only (CR 604.3 says a CDA functions in all zones -- a Goyf in a
graveyard has a power, and counts itself), and a CDA has no timestamp to sort
on under CR 613.7.

Transcribes Tarmogoyf's 2007-10-01 all-zones ruling as a test.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 4: Layer 6 strips the CDA

CR 604.3 makes a characteristic-defining ability an ability, so `LoseAllAbilities` must remove it. **Read the P3b spec §5's "One honest negative result" before writing this**: the change is required by the CR but is *unobservable* in the current pool, and the test must say so in its own comment rather than pretend otherwise.

**Files:**
- Modify: `source/library/Pawl/Projection.hs`
- Test: `source/test-suite/Pawl/PowerToughnessSpec.hs`

**Interfaces:**
- Consumes: Task 3's layer-7a fold.
- Produces: no new names; `Modification.LoseAllAbilities` now clears `PC.characteristicPT`.

- [ ] **Step 1: Write the test**

Append to the `PowerToughness` list:

```haskell
      HU.testCase "CR 604.3 Humility removes the CDA, and a Humility'd Tarmogoyf is 1/1" $
        -- NON-DISTINGUISHING BY CONSTRUCTION, and deliberately kept anyway.
        -- Humility is layer 6 (LoseAllAbilities) AND layer 7b (base P/T 1/1), and
        -- 7b overwrites 7a either way -- so this test passes whether or not
        -- LoseAllAbilities clears characteristicPT. It is here because "a
        -- Humility'd Tarmogoyf is 1/1" is a real ruling worth pinning, not because
        -- it proves the clearing.
        --
        -- What WOULD distinguish: a "loses all abilities" card that does not also
        -- set P/T. The Aura family (Darksteel Mutation and kin) is blocked on
        -- Attach; Soul Sculptor needs layer-4 card-type REPLACEMENT; Dress Down
        -- needs Flash, a beginning-of-end-step trigger (P4) and Sacrifice. See the
        -- P3b spec, section 8.
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, withBolt) = S.addGraveyardCard (Cards.lightningBoltPrinting cards) S.alice gs0
            (goyfId, board) = S.addCreature (Cards.tarmogoyfPrinting cards) S.alice withBolt
            gs = S.withHumility cards board
         in do
              HU.assertEqual "1 power" (Just 1) (Projection.powerOf goyfId gs)
              HU.assertEqual "1 toughness" (Just 1) (Projection.toughnessOf goyfId gs),
      HU.testCase "CR 604.3 LoseAllAbilities clears the CDA from the projected characteristics" $
        -- The clearing itself, asserted directly on the projection rather than
        -- through P/T -- the only channel through which it IS observable today.
        let gs0 = Setup.emptyGame S.bothPlayers
            (goyfId, board) = S.addCreature (Cards.tarmogoyfPrinting cards) S.alice gs0
            gs = S.withHumility cards board
         in HU.assertEqual "no CDA survives layer 6" Nothing (PC.characteristicPT (Projection.project goyfId gs))
```

- [ ] **Step 2: Run the tests to verify the second one fails**

Run: `cabal test --test-options='-p "$0~/PowerToughness/"' 2>&1 | tail -30`
Expected: the "1/1" case PASSES (it is non-distinguishing, as documented); the "clears the CDA" case FAILS with `Just (...) /= Nothing`.

- [ ] **Step 3: Clear it**

In `source/library/Pawl/Projection.hs`, replace the `LoseAllAbilities` arm of `applyModification`:

```haskell
  -- CR 604.3: a characteristic-defining ability IS a static ability, so losing
  -- all abilities loses it too. Layer 6, which is BEFORE 7a -- the reason the CDA
  -- is folded in place from the partial rather than gathered up front.
  Modification.LoseAllAbilities ->
    pc
      { PC.keywords = Set.empty,
        PC.characteristicPT = Nothing,
        PC.activatedAbilities = [],
        PC.replacementEffects = [],
        PC.triggeredAbilities = []
      }
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -20`
Expected: PASS, warning-free.

- [ ] **Step 5: Commit**

```bash
git add source/library/Pawl/Projection.hs source/test-suite/Pawl/PowerToughnessSpec.hs
hooky fix
git add source/library/Pawl/Projection.hs source/test-suite/Pawl/PowerToughnessSpec.hs
hooky run
git commit -m "$(cat <<'EOF'
feat(m4.5-p3b): LoseAllAbilities strips the CDA at layer 6 (CR 604.3)

A characteristic-defining ability is a static ability, so Humility removes it
-- at layer 6, before 7a, which is why the CDA is folded from the partial.

Kept honest: the P/T channel is NON-DISTINGUISHING, because every
LoseAllAbilities source in the pool also sets base P/T at 7b. The test says so
and names what would retire it (Dress Down, or Soul Sculptor). The clearing is
asserted directly on the projection, the one channel where it shows.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 5: A Clone of Tarmogoyf acquires the ability, not the number

This pays the bill P2 deferred by name: *"7b/CDA P/T-setting in copiable values (rides P3b, Tarmogoyf)"*. It lives in `CopySpec` because that module already owns `copyNewest` and `resolveAndSettle`.

**Files:**
- Test: `source/test-suite/Pawl/CopySpec.hs`

**Interfaces:**
- Consumes: Task 3's layer-7a fold; `Cards.tarmogoyfPrinting`; `CopySpec.copyNewest`, `CopySpec.resolveAndSettle`, `CopySpec.cloneOnBattlefield`.
- Produces: no new names. **No library change is expected** — if this task needs one, the seed in Task 2 is wrong; stop and say so.

- [ ] **Step 1: Write the test**

Append to the `Copy` list in `source/test-suite/Pawl/CopySpec.hs` (add a comma after the last existing case):

```haskell
      HU.testCase "Clone of Tarmogoyf copies the ABILITY, so both recompute (CR 707.2a)" $
        -- THE FALSIFIER for snapshotting the NUMBER: CR 707.2a says a copy
        -- acquires the abilities of the object it copies, because those values are
        -- derived from its rules text. Seeding the CDA as an evaluated integer
        -- would freeze the Clone at the graveyards' contents at the moment it
        -- entered -- P2's deferred bill, paid here.
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, withBolt) = S.addGraveyardCard (Cards.lightningBoltPrinting cards) S.alice gs0
            (goyfId, board) = S.addCreature (Cards.tarmogoyfPrinting cards) S.alice withBolt
            (_, staged) = S.spellOnStack (Cards.clonePrinting cards) S.alice board
            resolved = resolveAndSettle copyNewest staged
            -- A second card type reaches a graveyard AFTER the Clone entered.
            (_, later) = S.addGraveyardCard (Cards.pikerPrinting cards) S.bob resolved
         in case cloneOnBattlefield resolved of
              Nothing -> HU.assertFailure "Clone did not reach the battlefield"
              Just cloneId -> do
                HU.assertEqual "at entry the Clone is the Goyf's 1/2" (Just 1) (Projection.powerOf cloneId resolved)
                HU.assertEqual "at entry, toughness 1+1" (Just 2) (Projection.toughnessOf cloneId resolved)
                HU.assertEqual "the source moves to 2" (Just 2) (Projection.powerOf goyfId later)
                HU.assertEqual "and so does the COPY" (Just 2) (Projection.powerOf cloneId later)
                HU.assertEqual "the copy's toughness moves too" (Just 3) (Projection.toughnessOf cloneId later)
```

- [ ] **Step 2: Run the test**

Run: `cabal test --test-options='-p "$0~/Copy/"' 2>&1 | tail -30`
Expected: PASS on the first run — the seed in Task 2 was built for exactly this, and `copiableCharacteristics` carries `PC.characteristicPT` for free.

This is the one task in this plan whose test is expected to pass without a preceding implementation step. That is the point: it is a **regression gate** on a decision already made, not new behaviour. If it fails, do not patch it here — the seed is wrong and Task 2 must be revisited.

- [ ] **Step 3: Commit**

```bash
git add source/test-suite/Pawl/CopySpec.hs
hooky fix
git add source/test-suite/Pawl/CopySpec.hs
hooky run
git commit -m "$(cat <<'EOF'
test(m4.5-p3b): a Clone of Tarmogoyf copies the ability, not the number

Pays P2's named deferral ("7b/CDA P/T-setting in copiable values (rides P3b,
Tarmogoyf)"). CR 707.2a: a copy acquires the ABILITIES of the object it
copies, because those values derive from its rules text. A card type reaches a
graveyard after the Clone entered, and BOTH creatures move.

Passes without a library change by construction -- characteristicPT was seeded
as unevaluated quantities in Task 2, so it rides copiableCharacteristics.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 6: The CR 608.2h freeze, and Inner Calm, Outer Strength

**Files:**
- Modify: `source/library/Pawl/Type/Subtype.hs`
- Modify: `source/library/Pawl/Projection.hs`
- Modify: `source/library/Pawl/Resolve.hs`
- Modify: `source/library/Pawl/Codec.hs`
- Create: `data/cards/inner-calm-outer-strength.json`
- Modify: `source/test-suite/Pawl/Cards.hs`
- Test: `source/test-suite/Pawl/PowerToughnessSpec.hs`

**Interfaces:**
- Consumes: Task 1's `Quantity.Count` and `evaluate`.
- Produces: `Projection.freezeQuantities :: GameState -> ObjectId -> Maybe PlayerId -> Modification -> Modification`; `Subtype.Arcane`; `Cards.innerCalmPrinting`.

- [ ] **Step 0: Read the verified card data (already pinned)**

**The controller verified this card against the Scryfall API on 2026-07-21. Do not re-fetch; use these values.**

- Mana cost **`{2}{G}`**, type line **`Instant — Arcane`** (CR 205.3k), no printed P/T.
- Oracle text: **"Target creature gets +X/+X until end of turn, where X is the number of cards in your hand."**

The `{2}{G}` cost is what the Step 1 test's four Forests are sized for: three to cast this, one for the Giant Growth cast that follows.

**It has zero Gatherer rulings** — so there is no ruling date to put in the freeze test's name, and nothing to transcribe. Do not go looking.

- [ ] **Step 1: Write the failing tests**

Append to the `PowerToughness` list. The `landsInPlay` count below assumes a `{2}{G}` cost — **adjust it to whatever Step 0 confirmed**:

```haskell
      HU.testCase "CR 608.2h a resolved pump is FROZEN and does not shrink with the hand" $
        -- THE FALSIFIER for re-evaluating a stored quantity: CR 608.2h says the
        -- answer is determined only once, when the effect is applied. Alice
        -- resolves the pump with two cards left in hand (+2/+2), then casts one of
        -- them -- her hand is now one card, and the pump must NOT follow it down.
        let base = S.landsInPlay (Cards.forestPrinting cards) 4
            (pikerId, board) = S.addPiker cards S.alice base
            -- handOne FIRST (it replaces the hand and sets up the phase), then
            -- addHandCard for the extras.
            (h1, icId) = S.handOne (Cards.innerCalmPrinting cards) board
            (ggId, h2) = S.addHandCard (Cards.giantGrowthPrinting cards) S.alice h1
            (_, gs) = S.addHandCard (Cards.forestPrinting cards) S.alice h2
            -- Casting Inner Calm moves it from hand to the stack, leaving two.
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice icId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
            -- Now the hand shrinks. Giant Growth is only CAST, not resolved, so it
            -- contributes no pump of its own -- the only thing that changed is the
            -- number Inner Calm counted.
            shrunk = snd (Engine.runGamePure S.identityAnswer after (Cast.castSpell S.alice ggId))
         in do
              HU.assertEqual "two cards left in hand at resolution" 2 (S.handSize S.alice after)
              HU.assertEqual "the 2/1 Piker is pumped to 4" (Just 4) (Projection.powerOf pikerId after)
              HU.assertEqual "and to 3 toughness" (Just 3) (Projection.toughnessOf pikerId after)
              HU.assertEqual "the hand is down to one card" 1 (S.handSize S.alice shrunk)
              HU.assertEqual "THE FREEZE: still +2, not +1" (Just 4) (Projection.powerOf pikerId shrunk)
              HU.assertEqual "and still +2 toughness" (Just 3) (Projection.toughnessOf pikerId shrunk),
      HU.testCase "CR 611.2 the freeze does NOT reach a static ability's continuous effect" $
        -- Opalescence's SetBasePowerToughness carries ManaValue, and CR 611.2 scopes
        -- the freeze to effects created by a spell's RESOLUTION. A static ability's
        -- effect is regenerated from the permanent every projection and evaluated
        -- per AFFECTED object. Freezing it would evaluate ManaValue against
        -- Opalescence itself, making Bad Moon a 4/4 instead of a 2/2.
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, withOpal) = S.addCreature (Cards.opalescencePrinting cards) S.alice gs0
            (moonId, gs) = S.addCreature (Cards.badMoonPrinting cards) S.alice withOpal
         in do
              HU.assertEqual "Bad Moon's own mana value is 2, not Opalescence's 4" (Just 2) (Projection.powerOf moonId gs)
              HU.assertEqual "and its toughness is 2" (Just 2) (Projection.toughnessOf moonId gs)
```

- [ ] **Step 2: Run the tests to verify the first fails**

Run: `cabal test --test-options='-p "$0~/PowerToughness/"' 2>&1 | tail -30`
Expected: the Opalescence case PASSES (it is a regression guard on existing behaviour); the freeze case FAILS — `Cards.innerCalmPrinting` is not in scope.

- [ ] **Step 3: Add `freezeQuantities`**

In `source/library/Pawl/Projection.hs`, add this beside `rewriteModification` (which `Resolve` already delegates to for CR 612):

```haskell
-- CR 608.2h / 611.2d: evaluate a modification's quantities ONCE and rewrite them
-- to Literals. Called by Resolve when a spell's resolution STORES a continuous
-- effect -- "if an effect requires information from the game ... the answer is
-- determined only once, when the effect is applied."
--
-- `oid` is the SOURCE (the resolving spell), not the affected object: the source
-- is what holds a chosen X in its bindings, and `you` is the source's controller,
-- whose hand a player-scoped count counts.
--
-- Deliberately NOT applied to a static ability's effect: CR 611.2 scopes 611.2a-d
-- to "a continuous effect generated by the resolution of a spell or ability", and
-- a static ability's effect (CR 604.2) is regenerated every projection and
-- evaluated per affected object -- Opalescence's mana value must keep moving.
--
-- Cases on Modification, so it lives HERE (Projection is the sole home), the same
-- standing rewriteModification has. An unevaluable quantity is left alone.
freezeQuantities :: GameState -> ObjectId -> Maybe PlayerId.PlayerId -> Modification -> Modification
freezeQuantities gs oid you m =
  let freeze q = case Quantity.evaluate gs oid you q of
        Nothing -> q
        Just n -> Quantity.Type.Literal n
   in case m of
        Modification.SetBasePowerToughness p t -> Modification.SetBasePowerToughness (freeze p) (freeze t)
        Modification.ModifyPowerToughness p t -> Modification.ModifyPowerToughness (freeze p) (freeze t)
        -- Every other modification carries no quantity to freeze; named
        -- explicitly per Modification's exhaustiveness discipline.
        Modification.GainKeyword _ -> m
        Modification.LoseAllAbilities -> m
        Modification.SetLandSubtype _ -> m
        Modification.AddLandSubtype _ -> m
        Modification.AddCardType _ -> m
        Modification.ChangeSubtypeWord _ _ -> m
        Modification.SetController _ -> m
        Modification.SetColor _ -> m
```

- [ ] **Step 4: Call it from `Resolve`**

In `source/library/Pawl/Resolve.hs`, replace the `ModifyTarget` arm's body (lines 338–352) with:

```haskell
          Just target ->
            -- CR 611.2c: the affected set is locked to this one object now.
            -- CR 608.2h / 611.2d: and so is the VALUE -- "the answer is determined
            -- only once, when the effect is applied". The quantities are frozen to
            -- Literals against the SOURCE (which holds a chosen X) and the source's
            -- CONTROLLER (whose hand a player-scoped count counts), never against
            -- the target. See the P3b spec, section 2.4.
            let (ts, gs1) = Game.freshTimestamp gs
                frozen = Projection.freezeQuantities gs source (Just controller) modification
                eff =
                  ContinuousEffect.MkContinuousEffect
                    { ContinuousEffect.source = source,
                      ContinuousEffect.timestamp = ts,
                      ContinuousEffect.duration = duration,
                      ContinuousEffect.modification = frozen,
                      ContinuousEffect.affected = Affected.TheseObjects (Set.singleton target)
                    }
             in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}
```

- [ ] **Step 5: Add the subtype, the codec arm and the card**

In `source/library/Pawl/Type/Subtype.hs`, add after `Lhurgoyf`:

```haskell
  | Arcane -- CR 205.3k (a spell type; Inner Calm, Outer Strength's)
```

In `source/library/Pawl/Codec.hs`, add `Subtype.Arcane -> "Arcane"` to `subtypeToJson` and `(Text.pack "Arcane", Subtype.Arcane)` to `jsonToSubtype`.

Create `data/cards/inner-calm-outer-strength.json` as a single line (**mana cost per Step 0**):

```json
{"name":"Inner Calm, Outer Strength","manaCost":[{"type":"Generic","value":2},{"type":"OfType","value":{"type":"Colored","value":{"type":"Green"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Instant"}],"subtypes":[{"type":"Arcane"}]},"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"spell":{"modes":[{"effects":[{"type":"ModifyTarget","value":[{"type":"UntilEndOfTurn"},{"type":"ModifyPowerToughness","value":[{"type":"Count","value":{"type":"CardsInYourHand"}},{"type":"Count","value":{"type":"CardsInYourHand"}}]},"target"]}],"targetSpecs":[{"slot":"target","spec":{"type":"CreatureTarget"}}]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[]}
```

In `source/test-suite/Pawl/Cards.hs`, add `innerCalmPrinting` to the record, to `loadCards` (`loadPrinting "inner-calm-outer-strength"`), to the returned record, and to `allPrintings`. Deterministic fixture; no deck.

- [ ] **Step 6: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -20`
Expected: PASS, warning-free. `ProjectionSpec`'s Giant Growth and Humility/Opalescence groups must be unaffected — every quantity they store is already a `Literal`, so freezing them is the identity.

- [ ] **Step 7: Add the wrong-object falsifier**

Append one more case to the `PowerToughness` list:

```haskell
      HU.testCase "CR 608.2h the count is the CASTER's hand, not the target's controller's" $
        -- The second half of the same bug: applyModification used to evaluate a
        -- stored quantity against the AFFECTED object, so a player-scoped count
        -- would read the wrong player. Alice holds two cards after casting; bob
        -- holds none, and it is bob's creature being pumped.
        let base = S.landsInPlay (Cards.forestPrinting cards) 4
            (bobsPiker, board) = S.addPiker cards S.bob base
            (h1, icId) = S.handOne (Cards.innerCalmPrinting cards) board
            (_, h2) = S.addHandCard (Cards.giantGrowthPrinting cards) S.alice h1
            (_, gs) = S.addHandCard (Cards.forestPrinting cards) S.alice h2
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice icId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "bob holds nothing" 0 (S.handSize S.bob after)
              HU.assertEqual "the pump is alice's two, not bob's zero" (Just 4) (Projection.powerOf bobsPiker after)
```

Run: `cabal test --test-options='-p "$0~/PowerToughness/"' 2>&1 | tail -20`
Expected: PASS.

`S.handOne` puts a card in **alice's** hand and `Cast.castSpell S.alice` casts it, so alice is the caster throughout; only the creature belongs to bob. If `S.handOne`'s owner is not alice, read its definition in `source/test-suite/Pawl/Support.hs:439` and adjust the assertion's player, not the test's intent.

- [ ] **Step 8: Commit**

```bash
git add source/library/Pawl/Type/Subtype.hs source/library/Pawl/Projection.hs source/library/Pawl/Resolve.hs source/library/Pawl/Codec.hs data/cards/inner-calm-outer-strength.json source/test-suite/Pawl/Cards.hs source/test-suite/Pawl/PowerToughnessSpec.hs pawl.cabal
hooky fix
git add source/library/Pawl/Type/Subtype.hs source/library/Pawl/Projection.hs source/library/Pawl/Resolve.hs source/library/Pawl/Codec.hs data/cards/inner-calm-outer-strength.json source/test-suite/Pawl/Cards.hs source/test-suite/Pawl/PowerToughnessSpec.hs pawl.cabal
hooky run
git commit -m "$(cat <<'EOF'
feat(m4.5-p3b): freeze a stored effect's quantities at resolution (CR 608.2h)

Closes the latent bug Resolve.hs documented against the WRONG rule: the
comment cited CR 611.2b (the "for as long as" rule) for the freeze, and
asserted it was a no-op "until X exists" -- X has existed since M4a. The right
rules are CR 608.2h and 611.2d.

Projection.freezeQuantities evaluates once and rewrites to Literals, called
from Resolve's ModifyTarget store path against the SOURCE and the source's
CONTROLLER -- also fixing the second half, where applyModification evaluated a
stored quantity against the affected object.

Static abilities are deliberately untouched: CR 611.2 scopes the freeze to
resolution-created effects, so Opalescence's mana value keeps recomputing per
affected object. Guarded by a test that would read 4/4 instead of 2/2 if the
freeze leaked into gather.

Gate: Inner Calm, Outer Strength.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 7: Layer 7d, the switch, and Twisted Image

**Files:**
- Modify: `source/library/Pawl/Type/Modification.hs`
- Modify: `source/library/Pawl/Projection.hs`
- Modify: `source/library/Pawl/Codec.hs`
- Create: `data/cards/twisted-image.json`
- Modify: `source/test-suite/Pawl/Cards.hs`
- Test: `source/test-suite/Pawl/PowerToughnessSpec.hs`

**Interfaces:**
- Consumes: Task 3's layer-7a fold (Tarmogoyf is the asymmetric fixture this task needs); Task 6's `freezeQuantities` (which gains one identity arm).
- Produces: `Modification.SwitchPowerToughness`; `Cards.twistedImagePrinting`. **No new `TargetSpec`** — see Step 0.

- [ ] **Step 0: Read the verified card data (already pinned)**

**The controller verified this card against the Scryfall API on 2026-07-21. Do not re-fetch; use these values.**

- Mana cost `{U}`, type line `Instant`, no printed P/T.
- Oracle text: **"Switch target creature's power and toughness until end of turn.\nDraw a card."**

**This is functional errata, and it removes work from this task.** The New Phyrexia *printed* wording was "target **artifact or** creature's"; WotC dropped the artifact clause, because CR 208.3 says a noncreature permanent has no power or toughness, so switching a noncreature artifact's P/T never did anything.

**Consequence: do NOT add `TargetSpec.ArtifactOrCreatureTarget`.** Twisted Image uses the existing `TargetSpec.CreatureTarget`. Steps 4 and 5 below have been rewritten accordingly — there is no new `TargetSpec` constructor, no new `Pawl.Target` arm, and no new target-spec codec arm in this task.

Twisted Image's three Gatherer rulings, all dated **2021-03-19**, verbatim:

1. *"Effects that switch a creature's power and toughness apply after all other effects, regardless of when those effects began to apply. For instance, if you target a 1/2 creature then give it +2/+0 later in the turn, it's a 2/3 creature, not a 4/1 creature."*
2. *"Because damage remains marked on a creature until the damage is removed as the turn ends, nonlethal damage dealt to a creature may become lethal if you switch its power and toughness during that turn."*
3. *"Switching a creature's power and toughness twice (or any even number of times) effectively returns the creature to the power and toughness it had before any switches."*

Rulings 2 and 3 are transcribed as tests in Step 1. Ruling 1's *"regardless of when those effects began to apply"* clause is the timestamp-independence claim, also transcribed in Step 1 — it is the one genuinely new assertion the rulings added to this task.

- [ ] **Step 1: Write the failing tests**

Append to the `PowerToughness` list:

```haskell
      HU.testCase "CR 613.4d the switch takes the value AFTER layers 7a-7c" $
        -- THE ORDERING FALSIFIER, and the reason Tarmogoyf and Twisted Image are in
        -- the same phase: a symmetric fixture (a +1/+1 counter, Giant Growth)
        -- COMMUTES with the switch and proves nothing. Tarmogoyf's N/N+1 is the
        -- pool's only asymmetric P/T, so it is the only thing that can witness the
        -- order. Two card types in graveyards -> a 2/3 -> switched, a 3/2.
        -- Switching before 7a would switch Nothing/Nothing and the CDA would then
        -- write 2/3 straight back over it.
        let base = S.landsInPlay (Cards.islandPrinting cards) 1
            (_, g1) = S.addGraveyardCard (Cards.lightningBoltPrinting cards) S.alice base
            (_, g2) = S.addGraveyardCard (Cards.pikerPrinting cards) S.alice g1
            (goyfId, board) = S.addCreature (Cards.tarmogoyfPrinting cards) S.alice g2
            (gs, tiId) = S.handOne (Cards.twistedImagePrinting cards) board
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice tiId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertEqual "before: a 2/3" (Just 2) (Projection.powerOf goyfId board)
              HU.assertEqual "before: toughness 3" (Just 3) (Projection.toughnessOf goyfId board)
              HU.assertEqual "after: power is the old toughness" (Just 3) (Projection.powerOf goyfId after)
              HU.assertEqual "after: toughness is the old power" (Just 2) (Projection.toughnessOf goyfId after),
      HU.testCase "CR 613.4d a switched CDA still tracks the graveyards" $
        let base = S.landsInPlay (Cards.islandPrinting cards) 1
            (_, g1) = S.addGraveyardCard (Cards.lightningBoltPrinting cards) S.alice base
            (_, g2) = S.addGraveyardCard (Cards.pikerPrinting cards) S.alice g1
            (goyfId, board) = S.addCreature (Cards.tarmogoyfPrinting cards) S.alice g2
            (gs, tiId) = S.handOne (Cards.twistedImagePrinting cards) board
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice tiId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
            (_, later) = S.addGraveyardCard (Cards.giantGrowthPrinting cards) S.bob after
         in do
              HU.assertEqual "a third card type: 3/4 switched is 4/3, power" (Just 4) (Projection.powerOf goyfId later)
              HU.assertEqual "and toughness" (Just 3) (Projection.toughnessOf goyfId later),
      HU.testCase "CR 613.4d 2021-03-19 the switch applies last regardless of WHEN it began" $
        -- Gatherer ruling on Twisted Image (WotC, 2021-03-19): "Effects that switch
        -- a creature's power and toughness apply after all other effects,
        -- REGARDLESS OF WHEN THOSE EFFECTS BEGAN TO APPLY. For instance, if you
        -- target a 1/2 creature then give it +2/+0 later in the turn, it's a 2/3
        -- creature, not a 4/1 creature."
        --
        -- The switch is installed FIRST (earlier timestamp) and the pump SECOND, so
        -- a timestamp-ordered implementation would switch then pump. Layer order
        -- (CR 613.4c before 613.4d) must beat timestamp order. The pump is +2/+0 --
        -- ASYMMETRIC, per the ruling's own example, because a symmetric one cannot
        -- tell the two orders apart.
        --
        -- Goblin Piker is 2/1. Correct: 7c gives 4/1, 7d switches to 1/4.
        -- Timestamp-ordered: switch gives 1/2, then the pump gives 3/2.
        let gs0 = Setup.emptyGame S.bothPlayers
            (pikerId, board) = S.addPiker cards S.alice gs0
            switched = withEffect pikerId Modification.SwitchPowerToughness board
            gs = withEffect pikerId (Modification.ModifyPowerToughness (Quantity.Type.Literal 2) (Quantity.Type.Literal 0)) switched
         in do
              HU.assertEqual "power is the pumped toughness" (Just 1) (Projection.powerOf pikerId gs)
              HU.assertEqual "toughness is the pumped power" (Just 4) (Projection.toughnessOf pikerId gs),
      HU.testCase "CR 613.4d 2021-03-19 two switches return the object to normal" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (pikerId, board) = S.addPiker cards S.alice gs0
            once = withEffect pikerId Modification.SwitchPowerToughness board
            twice = withEffect pikerId Modification.SwitchPowerToughness once
         in do
              HU.assertEqual "once: the 2/1 is a 1/2" (Just 1) (Projection.powerOf pikerId once)
              HU.assertEqual "twice: back to 2" (Just 2) (Projection.powerOf pikerId twice)
              HU.assertEqual "twice: back to 1 toughness" (Just 1) (Projection.toughnessOf pikerId twice),
      HU.testCase "CR 704.5g 2021-03-19 nonlethal damage becomes lethal after a switch" $
        -- Gatherer ruling on Twisted Image (WotC, 2021-03-19): "Because damage
        -- remains marked on a creature until the damage is removed as the turn
        -- ends, nonlethal damage dealt to a creature may become lethal if you
        -- switch its power and toughness during that turn." Damage marking
        -- (CR 514.2) and the CR 704.5g lethal-damage state-based action have both
        -- existed since M1b; this is the ruling as a scenario.
        --
        -- A 2/3 Tarmogoyf with 2 damage marked survives. Switched to 3/2, the same
        -- 2 damage is lethal.
        let base = S.landsInPlay (Cards.islandPrinting cards) 1
            (_, g1) = S.addGraveyardCard (Cards.lightningBoltPrinting cards) S.alice base
            (_, g2) = S.addGraveyardCard (Cards.pikerPrinting cards) S.alice g1
            (goyfId, g3) = S.addCreature (Cards.tarmogoyfPrinting cards) S.alice g2
            -- Twisted Image draws a card, and this is the one test here that runs
            -- settleForPriority (it needs the SBA sweep). Setup.emptyGame leaves
            -- libraries EMPTY, so without this alice would lose to CR 704.5b
            -- mid-assertion rather than the Goyf dying to CR 704.5g.
            (_, g4) = S.addLibraryCard (Cards.forestPrinting cards) S.alice g3
            board = S.markDamage goyfId 2 g4
            (gs, tiId) = S.handOne (Cards.twistedImagePrinting cards) board
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice tiId))
            after = snd (Engine.runGamePure S.identityAnswer cast (Stack.resolveTop >> Engine.settleForPriority))
         in do
              HU.assertBool "the 2/3 with 2 damage was alive" (Set.member goyfId (GameState.battlefield board))
              HU.assertBool "the switched 3/2 with 2 damage is dead" (not (Set.member goyfId (GameState.battlefield after)))
```

Add to `source/test-suite/Pawl/PowerToughnessSpec.hs` a local `withEffect` helper (the `ColorSpec` idiom) and the imports it needs:

```haskell
-- Append a stored continuous effect affecting exactly `oid`. Object id 996 is a
-- stand-in source: nothing in these tests reads the source's own characteristics.
withEffect :: ObjectId.ObjectId -> Modification.Modification -> GameState.GameState -> GameState.GameState
withEffect oid m gs =
  let (ts, gs1) = Game.freshTimestamp gs
      eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = ObjectId.MkObjectId 996,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.duration = Duration.UntilEndOfTurn,
            ContinuousEffect.modification = m,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
          }
   in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}
```

```haskell
import qualified Data.Set as Set
import qualified Pawl.Game as Game
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.ObjectId as ObjectId
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cabal test --test-options='-p "$0~/PowerToughness/"' 2>&1 | tail -30`
Expected: FAIL — `Data constructor not in scope: Modification.SwitchPowerToughness`, `Cards.twistedImagePrinting` not in scope.

- [ ] **Step 3: Add the modification and the layer op**

In `source/library/Pawl/Type/Modification.hs`, add after `SetColor`:

```haskell
  | -- layer 7d, CR 613.4d: switch this object's power and toughness. Takes the
    -- value of power and applies it to toughness, and vice versa -- so it acts on
    -- whatever 7a, 7b and 7c already produced, not on the printed box. Carries no
    -- payload: two applications return the object to normal for free.
    SwitchPowerToughness
```

In `source/library/Pawl/Projection.hs`:

- `layer`: add `Modification.SwitchPowerToughness -> Layer.SwitchPT`
- `applyModification`: add

```haskell
  -- CR 613.4d: "take the value of power and apply it to the creature's toughness,
  -- and take the value of toughness and apply it to the creature's power."
  Modification.SwitchPowerToughness ->
    pc {PC.power = PC.toughness pc, PC.toughness = PC.power pc}
```

- `freezeQuantities`: add `Modification.SwitchPowerToughness -> m` beside the other no-quantity arms.

`setLandSubtypeEffects`'s `isSet` and `rewriteModification`'s `apply1` both end in a wildcard, so they need no new arm.

- [ ] **Step 4: Add the codec arms**

**No `TargetSpec` change in this task** (Step 0's errata note). `Pawl.Type.TargetSpec` and `Pawl.Target` are untouched.

In `source/library/Pawl/Codec.hs`, add `Modification.SwitchPowerToughness -> nullary (Text.pack "SwitchPowerToughness")` to `modificationToJson` and `"SwitchPowerToughness" -> Right Modification.SwitchPowerToughness` to `jsonToModification`. That is the whole codec change.

- [ ] **Step 5: Add the card**

Create `data/cards/twisted-image.json` as a single line (**mana cost per Step 0**):

```json
{"name":"Twisted Image","manaCost":[{"type":"OfType","value":{"type":"Colored","value":{"type":"Blue"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Instant"}],"subtypes":[]},"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"spell":{"modes":[{"effects":[{"type":"ModifyTarget","value":[{"type":"UntilEndOfTurn"},{"type":"SwitchPowerToughness"},"target"]},{"type":"Draw","value":{"type":"Literal","value":1}}],"targetSpecs":[{"slot":"target","spec":{"type":"CreatureTarget"}}]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[]}
```

In `source/test-suite/Pawl/Cards.hs`, add `twistedImagePrinting` to the record, to `loadCards` (`loadPrinting "twisted-image"`), to the returned record, and to `allPrintings`. Deterministic fixture; no deck.

- [ ] **Step 6: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -20`
Expected: PASS, warning-free.

- [ ] **Step 7: Commit**

```bash
git add source/library/Pawl/Type/Modification.hs source/library/Pawl/Projection.hs source/library/Pawl/Codec.hs data/cards/twisted-image.json source/test-suite/Pawl/Cards.hs source/test-suite/Pawl/PowerToughnessSpec.hs pawl.cabal
hooky fix
git add source/library/Pawl/Type/Modification.hs source/library/Pawl/Projection.hs source/library/Pawl/Codec.hs data/cards/twisted-image.json source/test-suite/Pawl/Cards.hs source/test-suite/Pawl/PowerToughnessSpec.hs pawl.cabal
hooky run
git commit -m "$(cat <<'EOF'
feat(m4.5-p3b): layer 7d P/T switching, gated by Twisted Image (CR 613.4d)

The last sublayer of 7 gets a producer. SwitchPowerToughness carries no
payload, so two applications return the object to normal for free.

The ordering falsifier needs Tarmogoyf: a symmetric fixture (a +1/+1 counter,
Giant Growth) COMMUTES with a switch and proves nothing, so the pool's only
asymmetric P/T is the only thing that can witness that 7d acts on the value
7a/7b/7c produced. That mutual dependency is why 7a and 7d are one phase.

Also transcribes Twisted Image's 2011-01-01 ruling: a 2/3 with 2 damage marked
survives; switched to 3/2 the same damage is lethal (CR 514.2 + 704.5g).

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 8: Close the phase in the docs

**Files:**
- Modify: `docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`
- Modify: `docs/progress.md`
- Modify: `CLAUDE.md`

**Interfaces:**
- Consumes: everything above.
- Produces: no code.

- [ ] **Step 1: Verify the phase is actually complete**

Run:

```bash
cabal clean && cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -20
grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-21-p3b-characteristic-defined-pt.md
```

Expected: a clean warning-free build, all tests passing, and the grep reporting only the steps remaining in this task. Do not proceed on a failing check — report it instead.

- [ ] **Step 2: Update the umbrella spec**

In `docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`:

- §3's **P3b** row: mark it *landed*, point at `docs/superpowers/specs/2026-07-21-p3b-characteristic-defined-pt-design.md`, and widen its scope column from "layer 7a" to "the rest of layer 7 (7a + the CR 608.2h freeze 7b owed + 7d)". Gates: **Tarmogoyf**, **Inner Calm, Outer Strength**, **Twisted Image**. New types: `Quantity.Star`/`Plus`/`Count`, `CountSpec`, `Card.characteristicPT`, `PC.characteristicPT`, `Modification.SwitchPowerToughness`, `Subtype.Lhurgoyf`/`Arcane`. Note the phase adds **no** new `TargetSpec` — Twisted Image's Oracle errata (artifact clause dropped, CR 208.3) meant `CreatureTarget` sufficed.
- §3's note "**P3b revives M4a's deferred numeric tower**": add that `c7a0077` (`Quantity.Bound SlotName`) was **not** retired — no count in the phase needed a binding slot.
- §4's ordering paragraph: P3a and P3b are both landed; **P4** is next.
- §6's tracking bullet on `c7a0077`: record the negative answer.
- Add a line noting **Cluster 1 (layer-system completion) is done**: layers 1 (P2), 2 (P1), 3 (M3d), 4 (M3c), 5 (P3a), 6 (M3b) and 7a/7b/7c/7d all have producers.

- [ ] **Step 3: Add the `progress.md` entry**

Append an **M4.5 P3b is complete** entry to `docs/progress.md`, in the established one-distilled-entry-per-milestone shape used by the P3a entry directly above it. It must record: the three gate cards and what each falsified; the thesis (one counting quantity, two re-read rules); the in-place 7a fold and its three reasons; the CR 611.2b→608.2h/611.2d citation fix and the wrong-object half of that bug; that P2's CDA-in-copiable-values bill is paid; that `LoseAllAbilities` clearing the CDA is **non-distinguishing today** with Dress Down / Soul Sculptor named as the expiry; and every deferral in the spec's §8 table. Note that no git-bug is closed and that `c7a0077` and `f90e0c4` both stay open.

- [ ] **Step 4: Tick `CLAUDE.md`**

In the "Current work and tracking" section of `CLAUDE.md`, extend the M4.5 paragraph: P3b is complete, layer 7 is finished, and **P4 (event history + state/delayed triggers, which gates P6 and P7) is next**.

- [ ] **Step 5: Commit**

```bash
git add docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md docs/progress.md CLAUDE.md
hooky fix
git add docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md docs/progress.md CLAUDE.md
hooky run
git commit -m "$(cat <<'EOF'
docs(m4.5-p3b): completion note, umbrella tick, Cluster 1 closed

Layer 7 now has a producer in every sublayer, which finishes M4.5's
layer-system-completion cluster: 1 (P2), 2 (P1), 3 (M3d), 4 (M3c), 5 (P3a), 6
(M3b), 7a/7b/7c/7d (P3b). P4 is next.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
EOF
)"
```
