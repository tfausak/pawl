# M4.5 P8 — Cost generalization and alternative costs: Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the *payment* half of GAP-Co — give pawl one `Cost` type for spells and activated abilities, a parameterized `CostComponent` vocabulary, the CR 118.6 unpayable/`{0}` distinction carried in the type, printed additional (CR 118.8) and alternative (CR 118.9) costs on `Card`, and one sole casing home — proved by three real gate cards: **Greed**, **Village Rites** and **Fireblast**.

**Architecture:** Two new leaf types, `Pawl.Type.Cost` (`{mana :: Maybe ManaCost, components :: [CostComponent]}`) and `Pawl.Type.CostComponent` (`TapThis | SacrificeThis | PayLife Natural | Sacrifice Natural PermanentCriterion`), replace the retired `Pawl.Type.AbilityCost`/`Pawl.Type.AdditionalCost`. `Pawl.Cost` grows from "one arithmetic step" into the axis's **sole casing home**: `costsFor` (the candidate costs of casting an object, printed first), `total` (CR 601.2f, now `Cost -> Cost`), `canPay`/`canPayComponent` (CR 118.3), `pay` (CR 601.2g/h, monadic, transactional), and the `requiresTapSymbol` classification. `Pawl.Cast` and `Pawl.Activate` become pure consumers — they ask "can this be paid" and "pay it" and learn nothing about which components exist. Two new prompts (`ChooseCost`, `ChooseSacrifices`) carry the two new decisions; `Card` gains `additionalCosts` and `alternativeCosts`.

**Tech Stack:** Haskell 2010 (GHC 9.14.1 from the Nix flake), `tasty` + `tasty-hunit`, the hand-rolled `Pawl.Json`/`Pawl.Codec` card codec, `jq` for the card-corpus migration.

**Spec:** `docs/superpowers/specs/2026-07-22-p8-cost-generalization-design.md`. Read §0 (the five falsifiers) before anything; §2.1–§2.3 before Task 1, §2.5–§2.7 before Task 2, §2.2 and §5's Greed block before Task 3, §2.6–§2.8 and §5's Village Rites block before Task 4, §2.4 and §5's Fireblast block before Task 5, §5's cross-checks before Task 6, §8–§10 before Task 7.

## Global Constraints

Every task's requirements implicitly include all of these. They come from `CLAUDE.md` and are not negotiable.

- **Haskell 2010, no language extensions** unless there is no alternative. `NamedFieldPuns` is permitted; `GADTs`/`RankNTypes` only in the suspension core and in test/benchmark modules that already carry the pragma. A module that already carries a `{-# LANGUAGE … #-}` pragma keeps it.
- **No explicit export lists.** `module Pawl.Foo where`.
- **One type per module** under `Pawl.Type.<TypeName>` (type + instances only). A module never imports its parents; `A.B.C` must not import `A.B` or `A`. A sibling `Pawl.Type.*` import is fine.
- **Qualified imports aliased to the last component** (`Data.Set` → `Set`, `Pawl.Type.CostComponent` → `CostComponent`). One import group, alphabetical. A logic module may import its same-named type module under that alias and *also* import the bare type name unqualified — `Pawl.Mana` already does exactly this (`import Pawl.Type.Mana (Mana)` plus `import qualified Pawl.Type.Mana as Mana`), and `Pawl.Cost` follows it. In `Pawl.Cast`, where `Pawl.Cost` takes the `Cost` alias, the type arrives as the unqualified `import Pawl.Type.Cost (Cost)`. In the **test suite**, where both must be imported at once, the type module takes the `X.Type` alias (`Pawl.Type.Cost as Cost.Type`); `Pawl.Support` is aliased `S`.
- **No partial functions**, written or used. No `head`, `error`, `undefined`, or non-exhaustive matches. **`Natural` subtraction is partial** — it throws on underflow. Every `Natural` comparison in this plan that guards a subtraction stays.
- **`newtype` liberally, non-punning constructors** (`MkFoo`). Build records with `do`/`pure` + record syntax, not `<$>`/`<*>`.
- **Prefer explicit:** `case` over point-free; one equation with a `case` over multiple clauses; `let` over `where`; `$` over parens, `.` over chained `$`. No list comprehensions. No backtick-infixed functions. Named local predicates over lambdas in `filter`.
- **`Text` not `String`.** Arbitrary-precision numbers (`Integer`, `Natural`).
- **No boolean blindness** — a custom sum type beats a bare `Bool`. `Pawl.Type.Payment` is exactly this rule applied to the payment door. `canPay`/`canPayComponent`/`requiresTapSymbol` return `Bool` for the same documented reason `Cast.castable` and `Mana.canPay` already do: one yes/no question with no third state.
- **Derive at least `Eq` and `Show`.** `Cost` and `CostComponent` also derive `Ord`, because `Card` derives `Ord` and will carry them.
- **No API stability obligations.** Rename, reshape, and delete freely; never add a compat shim or keep an old name.
- **Every rules claim is checked against `docs/rules.txt`** and the rule number is cited in the code comment. Never trust recalled Magic rules — **including the citations written in this plan**. If `rules.txt` disagrees with a citation below, `rules.txt` wins: fix the citation and say so in the completion note.
- **Build must be warning-clean.** `cabal build all --enable-tests --enable-benchmarks` with `flags: +pedantic` (which is `-Werror`). Incremental builds hide warnings from unchanged modules; `cabal clean` first when a definitive check is needed.
- **Import lists are not spelled out** in every snippet below. When a step's code names `Monad.when`, `Set.*`, `Map.*`, `List.*`, `Maybe.*` or a `Pawl.*` module, add the qualified import (aliased per the rule above, one alphabetical group). GHC names every missing one. Equally, when a step *deletes* the last use of an import, delete the import — `-Wunused-imports` is an error here.
- **New library modules** go under `source/library/` and are picked up by the `-- cabal-gild: discover` directive — add the file and run `hooky fix`; never hand-edit `exposed-modules`. **New test modules** are discovered the same way into the test-suite `other-modules`, and must additionally be wired into `source/test-suite/Main.hs`'s `testTree`.
- **Before every commit:** `git add <the paths this task names>`, then `hooky fix`, then `git add -u`, then `hooky run`. `hooky` acts on **staged** files only; if you skip the `git add`, it reports "hooks skipped" and checks nothing.
- **TDD is not optional.** Write the failing test and actually run it to watch it fail before implementing. For a task whose first test names a module or constructor that does not exist yet, "fails" means the **build** fails with a specific `Not in scope` / `Could not find module` error — record that as the observed failure, then implement.
- **Never edit this plan, weaken an assertion, or delete a test to make a check pass.** If the plan looks wrong, stop and say so.
- **One small complete commit per task, on `main`.** Commit messages end with the `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>` trailer.
- **Concurrent sessions share this checkout.** Stage the explicit paths each task names; do not blanket-stage foreign files that appear in `git status`.
- **The card corpus is committed pretty-printed with `jq -S .`** and `Pawl.CardsSpec.checkFile` compares *up to key order*. `jq -S .` is byte-idempotent on every current file — verified while this plan was written — so a `jq -S` rewrite of a file changes only what the filter changed.

## Card verification (already done — do not re-fetch)

All three gate cards were fetched live from the Scryfall API during the design pass. Use these values verbatim; no network access is needed during execution.

| Card | Cost | Type line | P/T | Oracle text |
|---|---|---|---|---|
| Greed | `{3}{B}` | Enchantment | — | "{B}, Pay 2 life: Draw a card." |
| Village Rites | `{B}` | Instant | — | "As an additional cost to cast this spell, sacrifice a creature.\nDraw two cards." |
| Fireblast | `{4}{R}{R}` | Instant | — | "You may sacrifice two Mountains rather than pay this spell's mana cost.\nFireblast deals 4 damage to any target." |

Gatherer rulings, verbatim. Scryfall returned **none** for Greed and **none** for Fireblast. Village Rites has exactly one, and it is the specification of two of this phase's tests — do not paraphrase it into a test name:

**Village Rites**
> - *"You must sacrifice exactly one creature to cast this spell; you can't cast it without sacrificing a creature, and you can't sacrifice additional creatures."*

**None of the three is added to any deck in `Pawl.Cards`.** They are deterministic fixtures, exactly as Master Thief, Thalia and Silence are — adding them to a deck would perturb `PropertySpec`'s card-backed conservation counts for no gain. They *are* added to the `Cards` record, `loadCards` and `allPrintings`, which `Pawl.CardSpec`'s directory-agreement test (`source/test-suite/Pawl/CardSpec.hs:198`, "the data/cards directory and Cards.allPrintings agree, by slug") requires.

## Six deliberate departures from the spec

State these in the completion note (Task 7). Departure 1 is a **correction** — the spec is wrong there, and following it would ship a rules bug. The rest are refinements.

1. **Activated-ability costs do NOT route through `Pawl.Cost.total`.** Spec §2.9 says they should, on the argument that "no P7 vocabulary taxes an activation today, so nothing changes observably". That argument is false, and the falsifier is already in the card pool. `PlayerEffect.costAdjustments` calls `PlayerEffect.matchesSpell`, which classifies an **object** — `SpellCriterion.NoncreatureSpell` is `not (Set.member CardType.Creature (Projection.cardTypesOf oid gs))`, with no check that the object is a spell. Mindslaver is a noncreature *permanent*, so with Thalia on the battlefield `Cost.total` would raise Mindslaver's activation cost from `{4}` to `{5}` — Thalia taxes noncreature **spells** (CR 613.11 via her own text), never abilities. Routing an ability cost through `total` is therefore a regression, not a no-op. `Pawl.Activate` calls `Cost.canPay`/`Cost.pay` on the ability's **printed** cost, `#90` stays open as *both* a door gap and a producer gap, and Task 2 adds the Thalia × Mindslaver regression test that pins it shut. Fixing this properly needs a spell-vs-ability discriminator on the criterion side, which is P7's surface, not P8's.
2. **`Pawl.Cost.pay` is transactional.** Spec §2.8 says "in `Cast.castSpell` payment precedes the zone change, so rejection needs no restore". That is not enough: `pay` spends mana and pays earlier components *before* it reaches a `ChooseSacrifices` prompt, so a rejected answer would leave lands tapped and a creature sacrificed with no spell cast. `pay` captures the entry state and restores it itself on `Unpaid`, so `Unpaid` is always a complete no-op at both call sites. `Activate.activateAbility`'s existing `State.put gs` restore still runs on top of it (it also has to remove the ability object from the stack).
3. **`Pawl.Cost` gains `substituteX`, `hasVariable`, `unpayable` and `firstOffered`**, which the spec does not name. `substituteX`/`hasVariable` lift `Mana.substituteX` and the `{X}` test to the whole `Cost`, so `Pawl.Cast` stops importing `ManaCost`/`ManaSymbol` and the CR 601.2b X=0 floor is stated once. `unpayable`/`firstOffered` give the nine `ChooseCost` fallback sites (Replay, five `Pawl.Support` answerers, three benchmark answerers) one total, documented answer instead of nine copies of the same `case`.
4. **`PermanentCriterion` is matched at two sites**, not one: `Pawl.Cost.matchesCriterion` alongside the existing `Pawl.Replacement.matchesPermanent`. Reusing Replacement's would make `Pawl.Cost` import `Pawl.Replacement`, and issue **#72** (CR 614.12b — entry choices are not checked for payable combined costs) is a queued fix that makes `Pawl.Replacement` need `Pawl.Cost`: that is a module cycle waiting to happen. The duplication is a four-line `case` over a three-constructor type, the same shape `genericOf` already has in both `Pawl.Mana` and `Pawl.Cost`. Filed as an issue in Task 7; P9's filter language merges both.
5. **`Pawl.Cost.costsFor` arrives in Task 2 and grows twice**, instead of `Cast.costOf` surviving as a `Maybe Cost` intermediate that Task 5 throws away. Task 2's version returns the printed cost alone; Task 4 appends `Card.additionalCosts`; Task 5 appends the alternatives. `Cast.costOf` is deleted in Task 2.
6. **`Pawl.Cast.payableCost` is a named top-level predicate** rather than three copies of the same `canPay ∘ total ∘ substituteX 0` chain in `castable`, `castableWhileSearching` and `castSpell`.

## File structure

**New library modules.**

| Module | Task | Responsibility |
|---|---|---|
| `source/library/Pawl/Type/CostComponent.hs` | 1 (grows in 3, 4) | the component vocabulary: `TapThis \| SacrificeThis \| PayLife \| Sacrifice` |
| `source/library/Pawl/Type/Cost.hs` | 1 | the one cost shape: `{mana :: Maybe ManaCost, components :: [CostComponent]}` |
| `source/library/Pawl/Type/Payment.hs` | 2 | the payment door's answer: `Paid \| Unpaid` |

**Retired library modules.** `source/library/Pawl/Type/AbilityCost.hs`, `source/library/Pawl/Type/AdditionalCost.hs` (Task 1). Retired function: `Pawl.Cast.costOf` (Task 2). Retired functions: `Pawl.Activate.canPayAdditional`, `Pawl.Activate.payAdditional` (Task 2).

**New test module.** `source/test-suite/Pawl/CostSpec.hs` (Task 2) — near-mirrors `Pawl.Cost` and the three types it cases on; holds every gate card's gameplay-level tests and the unit tests.

**New card data.** `data/cards/greed.json` (3), `data/cards/village-rites.json` (4), `data/cards/fireblast.json` (5).

**Changed library modules.** `Pawl/Type/ActivatedAbility.hs` (1), `Pawl/Type/Card.hs` (4, 5), `Pawl/Type/PermanentCriterion.hs` (4), `Pawl/Type/Prompt.hs` (4, 5), `Pawl/Type/Response.hs` (4, 5), `Pawl/Codec.hs` (1, 3, 4, 5), `Pawl/Cost.hs` (2, 3, 4, 5), `Pawl/Activate.hs` (1, 2), `Pawl/Cast.hs` (2, 5), `Pawl/Replacement.hs` (4), `Pawl/Replay.hs` (4, 5).

**Changed data and harness.** All seven ability-bearing card files (1); `source/test-suite/Pawl/Cards.hs` (3, 4, 5); `source/test-suite/Pawl/Support.hs` (4, 5); `source/benchmark/Main.hs` (4, 5); `source/test-suite/Main.hs` (2).

**Untouched, deliberately.** `Pawl/Projection.hs` (read from, never edited — a cost is not a characteristic), `Pawl/Mana.hs` (pools, production and spending unchanged), `Pawl/Sba.hs` (Greed's death-by-payment runs through the existing CR 704.5a check), `Pawl/PlayerEffect.hs` (P7's `applyAdjustments` and `costAdjustments` are untouched), `Pawl/Type/Layer.hs`, `Pawl/Type/Modification.hs`.

---

### Task 1: `Cost` and `CostComponent` — the type, the codec, and the corpus migration

Card data can *say* the new cost shape, and every existing ability is migrated onto it. Behaviour-identical: every existing test must pass unchanged in meaning, and the only card-file changes are the two mechanical renames.

The load-bearing part of this task is §2.3 of the spec: `AbilityCost.mana`'s `Nothing` used to mean "no mana symbol in the cost" (i.e. `{0}`, payable), and `Cost.mana`'s `Nothing` means CR 118.6's **unpayable**. Every existing ability therefore migrates `Nothing → Just (MkManaCost [])`, in the card files and in the fixtures. Leave a file's `mana` absent and its ability silently becomes unactivatable once Task 2 lands.

**Files:**
- Create: `source/library/Pawl/Type/CostComponent.hs`, `source/library/Pawl/Type/Cost.hs`
- Delete: `source/library/Pawl/Type/AbilityCost.hs`, `source/library/Pawl/Type/AdditionalCost.hs`
- Modify: `source/library/Pawl/Type/ActivatedAbility.hs`, `source/library/Pawl/Codec.hs`, `source/library/Pawl/Activate.hs`
- Modify: `data/cards/drudge-skeletons.json`, `data/cards/evolving-wilds.json`, `data/cards/llanowar-elves.json`, `data/cards/mindslaver.json`, `data/cards/prodigal-sorcerer.json`, `data/cards/reliquary-tower.json`, `data/cards/synthetic-modal-activator.json`
- Test: `source/test-suite/Pawl/CodecSpec.hs`, `source/test-suite/Pawl/CardSpec.hs`, `source/test-suite/Pawl/ActivateSpec.hs`, `source/test-suite/Pawl/ManaSpec.hs`, `source/test-suite/Pawl/ResolveSpec.hs`, `source/test-suite/Pawl/ReplacementSpec.hs`

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: `CostComponent.CostComponent = TapThis | SacrificeThis`; `Cost.MkCost {mana :: Maybe ManaCost, components :: [CostComponent]}`; `ActivatedAbility.cost :: Cost`; `Codec.costToJson`/`jsonToCost`, `Codec.costComponentToJson`/`jsonToCostComponent`.

- [x] **Step 1: Write the failing codec tests**

In `source/test-suite/Pawl/CodecSpec.hs`, **replace** the existing `AbilityCost` round-trip (currently at line ~230, reading `(AbilityCost.MkAbilityCost Nothing [AdditionalCost.TapSelf])`) with the group below, and swap the `Pawl.Type.AbilityCost as AbilityCost` / `Pawl.Type.AdditionalCost as AdditionalCost` imports for `Pawl.Type.Cost as Cost.Type` and `Pawl.Type.CostComponent as CostComponent`:

```haskell
      Tasty.testGroup
        "cost (P8)"
        [ HU.testCase "every CostComponent round-trips" $
            mapM_
              (roundTrip "component" Codec.costComponentToJson Codec.jsonToCostComponent)
              [CostComponent.TapThis, CostComponent.SacrificeThis],
          HU.testCase "a Cost with a mana part and components round-trips" $
            roundTrip
              "cost"
              Codec.costToJson
              Codec.jsonToCost
              Cost.Type.MkCost
                { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 4]),
                  Cost.Type.components = [CostComponent.TapThis, CostComponent.SacrificeThis]
                },
          -- CR 118.5a: {0} is a real, payable cost, and ManaCost's empty list IS
          -- {0}. This is the shape every migrated ability now carries.
          HU.testCase "a {0} cost round-trips as Just an empty ManaCost" $
            roundTrip
              "zero"
              Codec.costToJson
              Codec.jsonToCost
              Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
          -- CR 118.6: an ABSENT mana field is an UNPAYABLE cost, not {0}. This is
          -- the footgun the corpus migration exists to avoid, pinned so a future
          -- card file cannot lose its mana field unnoticed.
          HU.testCase "an omitted mana field decodes to Nothing, not to {0}" $
            let value = Json.Object [(Text.pack "components", Json.Array [])]
             in HU.assertEqual
                  "unpayable"
                  (Right Cost.Type.MkCost {Cost.Type.mana = Nothing, Cost.Type.components = []})
                  (Codec.jsonToCost value)
        ],
```

`Json.Object`/`Json.Array` are the `Pawl.Type.Json` constructors; `CodecSpec` already imports `Pawl.Json as J` and `Pawl.Type.Json` — use whichever alias the file already has for the `Value` constructors, adding the import if it has none.

- [x] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Could not find module 'Pawl.Type.Cost'` (and `Pawl.Type.CostComponent`).

- [x] **Step 3: Create the two types and delete the two they replace**

`source/library/Pawl/Type/CostComponent.hs`:

```haskell
module Pawl.Type.CostComponent where

-- CR 601.2f's list of what a cost's non-mana part can be: "paying mana, tapping
-- permanents, sacrificing permanents, discarding cards, and so on." One
-- component of a Pawl.Type.Cost, alongside its mana part.
--
-- The successor to Pawl.Type.AdditionalCost, whose two nullary inhabitants were
-- named relative to that type ("Self"); here the object a cost is on is "This".
--
-- Open-half card data. Pawl.Cost is the ONLY module that may case on it: the
-- rules core reads the classification (can this be paid? does it require the tap
-- symbol?) and never the identity of a component.
data CostComponent
  = -- CR 107.5: "The tap symbol in an activation cost means 'Tap this
    -- permanent.' A permanent that's already tapped can't be tapped again to pay
    -- the cost." CR 302.6 gates it on summoning sickness.
    TapThis
  | -- CR 701.21a: sacrifice the object the cost is on -- its controller moves it
    -- from the battlefield directly to its owner's graveyard (Mindslaver).
    --
    -- Deliberately NOT `Sacrifice 1 <this permanent>`: CR 602.1a's
    -- self-referential cost names one object and offers no choice, so folding it
    -- into the criterion form would invent a prompt the rules do not have.
    SacrificeThis
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/Cost.hs`:

```haskell
module Pawl.Type.Cost where

import Pawl.Type.CostComponent (CostComponent)
import Pawl.Type.ManaCost (ManaCost)

-- CR 118.1: "a cost is an action or payment necessary to take another action".
-- ONE type for both carriers -- a spell's cost (CR 601.2f) and an activated
-- ability's activation cost (CR 602.1a) -- because the rules make them the same
-- thing: a mana part plus the non-mana components.
--
-- `mana` carries CR 118.6's distinction in the type, and the two cases are NOT
-- interchangeable:
--
--   Nothing              = an UNPAYABLE cost. CR 118.6: "Some objects have no
--                          mana cost. This represents an unpayable cost. ...
--                          attempting to pay an unpayable cost is an illegal
--                          action." This is the same fact Card.manaCost's Maybe
--                          already carries for CR 202.1 (a land), passed straight
--                          through by Pawl.Cost.costsFor.
--   Just (MkManaCost []) = {0}, which is real and payable. CR 118.5: "the action
--                          necessary for a player to pay such a cost is the
--                          player's acknowledgment that they are paying it";
--                          CR 118.5a: an activated ability whose cost is {0} is
--                          still activated the normal way. ManaCost is a list of
--                          symbols and the empty list IS {0}.
--
-- Scryfall spells the difference exactly: Ancestral Vision's mana_cost is '',
-- Ornithopter's is '{0}'.
data Cost = MkCost
  { mana :: Maybe ManaCost,
    components :: [CostComponent]
  }
  deriving (Eq, Ord, Show)
```

Then delete both retired modules:

```bash
git rm source/library/Pawl/Type/AbilityCost.hs source/library/Pawl/Type/AdditionalCost.hs
```

- [x] **Step 4: Point `ActivatedAbility` at the new type**

`source/library/Pawl/Type/ActivatedAbility.hs` — replace the `AbilityCost` import and field:

```haskell
import Pawl.Type.Cost (Cost)
```

```haskell
data ActivatedAbility card = MkActivatedAbility
  { cost :: Cost,
    modal :: Modal card
  }
  deriving (Eq, Ord, Show)
```

Keep the module's existing comment, striking the phrase that named `AbilityCost` and replacing it with: an activation cost is a `Pawl.Type.Cost`, the same type a spell's cost takes (CR 118.1).

- [x] **Step 5: Replace the codec pair**

In `source/library/Pawl/Codec.hs`, delete `additionalCostToJson`/`jsonToAdditionalCost` (lines ~635–646) and `abilityCostToJson`/`jsonToAbilityCost` (lines ~1183–1195) together with the two now-unused imports, and add in their place:

```haskell
-- Tagged rather than bare-nullary from the start: this family grows
-- payload-carrying constructors (PayLife, Sacrifice), so the decoder is written
-- against Json.tag and only gains arms.
costComponentToJson :: CostComponent.CostComponent -> Value
costComponentToJson c = case c of
  CostComponent.TapThis -> nullary (Text.pack "TapThis")
  CostComponent.SacrificeThis -> nullary (Text.pack "SacrificeThis")

jsonToCostComponent :: Value -> Either Text CostComponent.CostComponent
jsonToCostComponent value = do
  (t, _) <- Json.tag value
  case Text.unpack t of
    "TapThis" -> Right CostComponent.TapThis
    "SacrificeThis" -> Right CostComponent.SacrificeThis
    _ -> Left (Text.pack "unknown CostComponent: " <> t)
```

```haskell
costToJson :: Cost.Cost -> Value
costToJson c =
  Object
    [ (Text.pack "mana", maybeTo manaCostToJson (Cost.mana c)),
      (Text.pack "components", listTo costComponentToJson (Cost.components c))
    ]

-- CR 118.6: an ABSENT mana field decodes to Nothing -- an unpayable cost -- and
-- never to {0}. Every ability-bearing card file states its mana part explicitly
-- (`[]` for {0}), so the absent case is only ever reached by a malformed file.
jsonToCost :: Value -> Either Text Cost.Cost
jsonToCost value = do
  ps <- Json.asObject value
  m <- maybeFrom jsonToManaCost (getOpt (Text.pack "mana") ps)
  cs <- listFromDefault jsonToCostComponent (getOpt (Text.pack "components") ps)
  pure Cost.MkCost {Cost.mana = m, Cost.components = cs}
```

Import `Pawl.Type.Cost as Cost` and `Pawl.Type.CostComponent as CostComponent`. Update `activatedAbilityToJson`/`jsonToActivatedAbility` to call `costToJson`/`jsonToCost`.

- [x] **Step 6: Update `Pawl.Activate` mechanically**

This step is a rename only — `Pawl.Activate` keeps its own `canPayAdditional`/`payAdditional` and its `maybe True` mana reading until Task 2. In `source/library/Pawl/Activate.hs`, swap the two imports for `Pawl.Type.Cost as Cost` and `Pawl.Type.CostComponent as CostComponent`, then:

- `tapSicknessOk`: `elem CostComponent.TapThis (Cost.components (ActivatedAbility.cost ability))`
- `canPayAdditional`: retype to `ObjectId -> GameState -> CostComponent.CostComponent -> Bool`, with `CostComponent.TapThis` / `CostComponent.SacrificeThis` arms
- `activatable`: `all (canPayAdditional srcId gs) (Cost.components (ActivatedAbility.cost ability))` and `maybe True (\c -> Mana.canPay pid c gs) (Cost.mana (ActivatedAbility.cost ability))`
- `activateAbility`: `let additional = Cost.components (ActivatedAbility.cost ability)` and `case Cost.mana (ActivatedAbility.cost ability) of`
- `payAdditional`: retype to `ObjectId -> CostComponent.CostComponent -> Game ()`, same two arms

- [x] **Step 7: Migrate the seven card files**

Two mechanical renames per ability cost: `additional` → `components`, `TapSelf`/`SacrificeSelf` → `TapThis`/`SacrificeThis`, and `mana: null` → `mana: []` (CR 118.6 → CR 118.5a, §2.3 of the spec).

```bash
for f in drudge-skeletons evolving-wilds llanowar-elves mindslaver prodigal-sorcerer reliquary-tower synthetic-modal-activator; do
  jq -S '(.activatedAbilities[]?.cost) |= {components: [(.additional[]? | if .type == "TapSelf" then {type: "TapThis"} elif .type == "SacrificeSelf" then {type: "SacrificeThis"} else . end)], mana: (.mana // [])}' \
    "data/cards/$f.json" > "data/cards/$f.json.tmp" && mv "data/cards/$f.json.tmp" "data/cards/$f.json"
done
git diff --stat data/cards/
```

Expected: exactly seven files changed, and `git diff data/cards/` shows only `additional`→`components` blocks, the two tag renames, and four `null`→`[]` mana fields (evolving-wilds, llanowar-elves, prodigal-sorcerer, reliquary-tower).

- [x] **Step 8: Migrate the test fixtures**

Every fixture that built an `AbilityCost` must now build a `Cost`, **and every `Nothing` mana becomes `Just (ManaCost.MkManaCost [])`** — that is the §2.3 migration, not a cosmetic change. Swap the `Pawl.Type.AbilityCost`/`Pawl.Type.AdditionalCost` imports for `Pawl.Type.Cost as Cost.Type` (plus `Pawl.Type.CostComponent as CostComponent` where a component is named) in each file, then:

`source/test-suite/Pawl/ActivateSpec.hs` — three sites (lines ~61, ~130, ~181, ~217):

```haskell
-- theAbility's unreachable fallback (line ~61)
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (singleModeAbility [] Map.empty)
```

```haskell
-- the costly-ability fixture (line ~130)
            costlyAbility =
              ActivatedAbility.MkActivatedAbility
                { ActivatedAbility.cost =
                    Cost.Type.MkCost
                      { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 2]),
                        Cost.Type.components = []
                      },
                  ActivatedAbility.modal = singleModeAbility [] Map.empty
                }
```

```haskell
-- both synthetic GainControl abilities (lines ~181 and ~217)
                { ActivatedAbility.cost = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
```

`source/test-suite/Pawl/ManaSpec.hs` — three sites (lines ~155, ~162, ~172), each currently `AbilityCost.MkAbilityCost {AbilityCost.mana = Nothing, AbilityCost.additional = []}`:

```haskell
                { ActivatedAbility.cost = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
```

`source/test-suite/Pawl/ResolveSpec.hs` — four sites (lines ~350, ~383, ~398, ~442):

```haskell
-- lines ~350, ~383, ~398: the Nothing/[] shape
(Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) [])
```

```haskell
-- line ~442: the Mindslaver-shaped fixture
                    Cost.Type.MkCost
                      { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 4]),
                        Cost.Type.components = [CostComponent.TapThis, CostComponent.SacrificeThis]
                      }
```

`source/test-suite/Pawl/ReplacementSpec.hs` — one site (line ~80):

```haskell
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1))
```

`source/test-suite/Pawl/CardSpec.hs` — the Llanowar Elves assertions (lines ~586–587), which are the §2.3 tripwire and must now assert the migrated values:

```haskell
                  HU.assertEqual "tap cost only" [CostComponent.TapThis] (Cost.Type.components (ActivatedAbility.cost ab))
                  HU.assertEqual "a real {0} mana cost, not an unpayable one (CR 118.5a/118.6)" (Just (ManaCost.MkManaCost [])) (Cost.Type.mana (ActivatedAbility.cost ab))
```

- [x] **Step 9: Run the build and the whole suite**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS, all green. `Pawl.CardsSpec`'s `checkFile` is the migration's assertion — it re-parses every committed file and compares it to the compiled card up to key order, so a mis-migrated file fails there.

- [x] **Step 10: Format, lint, and commit**

```bash
git add source/library/Pawl source/test-suite/Pawl data/cards
hooky fix
git add -u
hooky run
git commit -m "refactor(m4.5-p8): one Cost type for spells and abilities

Pawl.Type.Cost and Pawl.Type.CostComponent replace AbilityCost and
AdditionalCost, retired outright (no consumers, no shims). Cost.mana's
Maybe now carries CR 118.6's unpayable/{0} distinction, so every
existing ability migrates Nothing -> Just (MkManaCost []) in the seven
card files and in every fixture -- CR 118.5a makes {0} real and payable.
Behaviour-identical; the existing suites are the assertion.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 2: `Pawl.Cost` becomes the door — `costsFor`, `total`, `canPay`, `pay`

`Pawl.Cost` stops being a module about one arithmetic step and becomes the axis's sole casing home. `Pawl.Activate` and `Pawl.Cast` both route through it. Still behaviour-identical: no card in the pool has a component beyond `TapThis`/`SacrificeThis`, and no spell has a non-mana cost at all.

Read **departures 1 and 2** at the top of this plan before writing any code in this task.

**Files:**
- Create: `source/library/Pawl/Type/Payment.hs`, `source/test-suite/Pawl/CostSpec.hs`
- Modify: `source/library/Pawl/Cost.hs`, `source/library/Pawl/Activate.hs`, `source/library/Pawl/Cast.hs`, `source/test-suite/Main.hs`

**Interfaces:**
- Consumes: `Cost.MkCost {mana, components}`, `CostComponent.TapThis`/`SacrificeThis` (Task 1).
- Produces: `Payment.Payment = Paid | Unpaid`; `Pawl.Cost.costsFor :: ObjectId -> GameState -> [Cost]`, `total :: PlayerId -> ObjectId -> Cost -> GameState -> Cost`, `canPay :: PlayerId -> ObjectId -> Cost -> GameState -> Bool`, `canPayComponent :: PlayerId -> ObjectId -> CostComponent -> GameState -> Bool`, `pay :: PlayerId -> ObjectId -> Cost -> Game Payment`, `payComponent`, `requiresTapSymbol :: Cost -> Bool`, `substituteX :: Natural -> Cost -> Cost`, `hasVariable :: Cost -> Bool`, `unpayable :: Cost`, `firstOffered :: [Cost] -> Cost`; `Pawl.Cast.payableCost :: PlayerId -> ObjectId -> GameState -> Cost -> Bool`.

- [x] **Step 1: Write the failing tests**

Create `source/test-suite/Pawl/CostSpec.hs`:

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Cost and the three types it cases on (Pawl.Type.Cost,
-- Pawl.Type.CostComponent, Pawl.Type.Payment), plus the two prompts the axis
-- adds. CR 118: what a cost IS, what it takes to pay one, and the alternative
-- and additional costs that change the answer.
--
-- The three gate cards: Greed (an amount-bearing component), Village Rites (a
-- mandatory spell-side additional cost) and Fireblast (an alternative cost with
-- no mana in it at all).
module Pawl.CostSpec where

import qualified Pawl.Activate as Activate
import qualified Pawl.Cards as Cards
import qualified Pawl.Cost as Cost
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.Cost as Cost.Type
import qualified Pawl.Type.CostComponent as CostComponent
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.Payment as Payment
import qualified Pawl.Type.Printing as Printing
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- The single activated ability of a printing. Total: the fallback is unreachable
-- in these fixtures. Duplicated per this suite's convention of group-local
-- helpers (ActivateSpec and ReplacementSpec each carry their own).
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card
theAbility p = case Card.Type.activatedAbilities (Printing.card p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Card.Type.spell (Printing.card p))

doorTests :: Cards.Cards -> Tasty.TestTree
doorTests cards =
  Tasty.testGroup
    "Door"
    [ -- CR 118.3's own second example: "a permanent that's already tapped can't
      -- be tapped to pay a cost" (CR 107.5 says the same for the {T} symbol).
      HU.testCase "CR 107.5 TapThis is payable only while the permanent is untapped" $
        let (oid, gs) = S.addCreature (Cards.prodigalSorcererPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            tapped = S.tapObject oid gs
         in do
              HU.assertBool "untapped pays" (Cost.canPayComponent S.alice oid CostComponent.TapThis gs)
              HU.assertBool "tapped does not" (not (Cost.canPayComponent S.alice oid CostComponent.TapThis tapped)),
      -- CR 701.21a: "A player can't sacrifice something that isn't a permanent,
      -- or something that's a permanent they don't control."
      HU.testCase "CR 701.21a SacrificeThis needs a permanent this player controls" $
        let (onField, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (inHand, gs1) = S.addHandCard (Cards.pikerPrinting cards) S.alice gs0
         in do
              HU.assertBool "a controlled permanent pays" (Cost.canPayComponent S.alice onField CostComponent.SacrificeThis gs1)
              HU.assertBool "a card in hand does not" (not (Cost.canPayComponent S.alice inHand CostComponent.SacrificeThis gs1))
              HU.assertBool "another player's permanent does not" (not (Cost.canPayComponent S.bob onField CostComponent.SacrificeThis gs1)),
      -- CR 118.6 vs CR 118.5a: the distinction the Maybe carries. Nothing is an
      -- unpayable cost; an empty ManaCost is {0} and is payable.
      HU.testCase "CR 118.6 an unpayable cost can never be paid" $
        let gs = S.mountainsInPlay cards 5
         in HU.assertBool
              "Nothing is unpayable"
              (not (Cost.canPay S.alice S.noSource (Cost.Type.MkCost Nothing []) gs)),
      HU.testCase "CR 118.5a a {0} cost is payable" $
        let gs = Setup.emptyGame S.bothPlayers
         in HU.assertBool
              "an empty ManaCost is {0}"
              (Cost.canPay S.alice S.noSource (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) gs),
      -- CR 118.6a: "If an unpayable cost is increased by an effect or an
      -- additional cost is imposed, the cost is still unpayable." total maps over
      -- the Maybe, so there is no special case to get wrong.
      HU.testCase "CR 118.6a Thalia's increase leaves an unpayable cost unpayable" $
        let base = S.mountainsInPlay cards 5
            (_, gs) = S.addCreature (Cards.thaliaPrinting cards) S.alice base
            (bolt, withBolt) = S.addHandCard (Cards.lightningBoltPrinting cards) S.alice gs
         in HU.assertEqual
              "still Nothing"
              Nothing
              (Cost.Type.mana (Cost.total S.alice bolt (Cost.Type.MkCost Nothing []) withBolt)),
      -- The classification Pawl.Activate reads instead of matching a constructor.
      HU.testCase "CR 302.6 requiresTapSymbol classifies a cost, and Greed's counterpart proves it" $
        let elves = ActivatedAbility.cost (theAbility (Cards.llanowarElvesPrinting cards))
            skeletons = ActivatedAbility.cost (theAbility (Cards.drudgeSkeletonsPrinting cards))
         in do
              HU.assertBool "Llanowar Elves' {T} cost requires the tap symbol" (Cost.requiresTapSymbol elves)
              HU.assertBool "Drudge Skeletons' {B} regenerate cost does not" (not (Cost.requiresTapSymbol skeletons)),
      -- Departure 1: Pawl.Activate does NOT route an ability cost through
      -- Cost.total. PlayerEffect.matchesSpell classifies an OBJECT, not a spell,
      -- so a noncreature PERMANENT matches SpellCriterion.NoncreatureSpell --
      -- and Thalia taxes noncreature SPELLS, never abilities. Four Mountains
      -- must still afford Mindslaver's printed {4}; a fifth would be needed if
      -- the tax wrongly reached the activation (#90).
      HU.testCase "CR 613.11 Thalia does not tax a noncreature permanent's activated ability" $
        let base = S.mountainsInPlay cards 4
            (slaver, gs1) = S.addCreature (Cards.mindslaverPrinting cards) S.alice base
            (_, gs2) = S.addCreature (Cards.thaliaPrinting cards) S.alice gs1
         in HU.assertBool
              "four Mountains still pay {4}"
              (Activate.activatable S.alice slaver (theAbility (Cards.mindslaverPrinting cards)) gs2),
      -- Departure 2: an Unpaid payment is a complete no-op, never a partial one.
      HU.testCase "CR 118.6 paying an unpayable cost changes nothing" $
        let gs = S.mountainsInPlay cards 3
            (outcome, after) = S.runPureWith S.identityAnswer gs (Cost.pay S.alice S.noSource (Cost.Type.MkCost Nothing []))
         in do
              HU.assertEqual "Unpaid" Payment.Unpaid outcome
              HU.assertEqual "no land tapped" 0 (S.tappedCount S.alice after)
    ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Pawl.Cost" [doorTests cards]
```

This needs one new `Pawl.Support` helper — `runPure` keeps only the state, and this test asserts on the returned `Payment` too. Add it next to `runPure` in `source/test-suite/Pawl/Support.hs`:

```haskell
-- runPure, keeping the action's RESULT alongside the final state -- the shape a
-- test needs when the door under test answers with a value (Pawl.Cost.pay's
-- Payment) and not only with a board.
runPureWith :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> Game.Type.Game a -> (a, GameState.GameState)
runPureWith answer gs game = Engine.runGamePure answer gs game
```

Wire `CostSpec` into `source/test-suite/Main.hs`: add `import qualified Pawl.CostSpec as CostSpec` and `CostSpec.tests cards,` to `testTree` (immediately after `CastSpec.tests cards,`).

- [x] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Could not find module 'Pawl.Type.Payment'`, plus `Not in scope: Cost.canPayComponent`, `Cost.canPay`, `Cost.requiresTapSymbol`, `Cost.pay`, `S.runPureWith`.

- [x] **Step 3: Create `Pawl.Type.Payment`**

`source/library/Pawl/Type/Payment.hs`:

```haskell
module Pawl.Type.Payment where

-- Whether a cost was paid. CR 601.2h: "Partial payments are not allowed.
-- Unpayable costs can't be paid." -- so the answer is genuinely two-valued, and
-- a sum type rather than a Bool per the house rule against boolean blindness.
--
-- Runtime-only: a Payment is never card data and never serialized.
--
-- Unpaid is always a COMPLETE no-op: Pawl.Cost.pay restores the state it was
-- entered with before returning it, so a caller never has to unwind a partial
-- payment (mana spent, one component paid, the next one rejected).
data Payment
  = Paid
  | Unpaid
  deriving (Eq, Show)
```

- [x] **Step 4: Rewrite `Pawl.Cost`**

Replace the whole of `source/library/Pawl/Cost.hs` above `applyAdjustments` — `applyAdjustments` itself is P7's and is **not touched**, comment included. The module header comment becomes:

```haskell
-- CR 118: what a cost IS, and everything the closed half needs to do with one --
-- what the candidates are (costsFor), what the total is (total, CR 601.2f),
-- whether it can be paid (canPay, CR 118.3) and paying it (pay, CR 601.2g/h).
-- Pawl.Mana keeps pools, production and spending; this module keeps the cost.
--
-- The SOLE casing home for Pawl.Type.CostComponent. Pawl.Cast and Pawl.Activate
-- learn nothing about which components exist: they ask "can this be paid" and
-- "pay it", and read one classification (requiresTapSymbol) for CR 302.6.
module Pawl.Cost where
```

The new bodies:

```haskell
-- CR 118.6: the cost of an object with no mana cost. Also the total answer the
-- ChooseCost fallback needs when no candidate was offered -- a state the engine
-- never produces, because the prompt is issued only with two or more payable
-- candidates, and an answer outside the offered set is rejected anyway.
unpayable :: Cost
unpayable = Cost.MkCost {Cost.mana = Nothing, Cost.components = []}

-- The first offered candidate, or `unpayable` when none was offered. The one
-- total, documented answer every ChooseCost fallback uses.
firstOffered :: [Cost] -> Cost
firstOffered candidates = case candidates of
  c : _ -> c
  [] -> unpayable

-- The candidate costs for CASTING this object (CR 601.2b), printed one first.
-- Empty for anything that is not a card: a token is created onto the
-- battlefield and never cast, and an ability on the stack is not a spell.
--
-- A LAND yields one candidate whose mana part is Nothing -- CR 202.1's "a card's
-- mana cost", absent -- which CR 118.6 makes unpayable, so canPay says False and
-- Cast.castable never offers it. That is the same answer the retired
-- Cast.costOf's Nothing gave, arrived at by the rule instead of by a special
-- case.
costsFor :: ObjectId -> GameState -> [Cost]
costsFor oid gs = case Game.lookupObject oid gs of
  Nothing -> []
  Just obj -> case Object.source obj of
    Source.OfCard printing ->
      let card = Printing.card printing
       in [Cost.MkCost {Cost.mana = Card.manaCost card, Cost.components = []}]
    Source.OfToken _ -> []
    Source.OfAbility _ _ -> []
    Source.OfTrigger _ _ -> []

-- CR 601.2f: "The total cost is the mana cost or alternative cost (as determined
-- in rule 601.2b), plus all additional costs and cost increases, and minus all
-- cost reductions." `cost` arrives with X already substituted, because CR 601.2b
-- precedes 601.2f.
--
-- The mana part alone is adjusted, and the components are carried through
-- untouched: every increase and reduction CR 601.2f describes, and every one P7
-- can express, is an amount of mana (CR 118.7a routes it to the generic
-- component). Nor is the result ever "locked in": CR 601.2f's own last sentence
-- makes the total fixed once determined, but this is recomputed fresh from the
-- current game state on every call (#94).
--
-- CR 118.6a's first sentence needs no special case: "If an unpayable cost is
-- increased by an effect or an additional cost is imposed, the cost is still
-- unpayable" -- fmap over the Maybe leaves Nothing as Nothing.
total :: PlayerId -> ObjectId -> Cost -> GameState -> Cost
total pid oid cost gs =
  cost {Cost.mana = fmap (applyAdjustments (PlayerEffect.costAdjustments pid oid gs)) (Cost.mana cost)}

-- CR 601.2b: substitute the chosen value of X into the mana part. Identity on a
-- Variable-free cost, and on an unpayable one.
substituteX :: Natural -> Cost -> Cost
substituteX x cost = cost {Cost.mana = fmap (Mana.substituteX x) (Cost.mana cost)}

-- Does this cost's mana part contain an {X} (CR 107.3)? What decides whether the
-- caster is asked for a value at CR 601.2b -- a spell with no {X} is not asked.
hasVariable :: Cost -> Bool
hasVariable cost = case Cost.mana cost of
  Nothing -> False
  Just (ManaCost.MkManaCost symbols) -> elem ManaSymbol.Variable symbols

-- CR 302.6 / 107.5: does paying this cost require tapping the object it is on?
-- The CLASSIFICATION Pawl.Activate reads for the summoning-sickness gate, so
-- that this module stays the only one matching a CostComponent constructor.
requiresTapSymbol :: Cost -> Bool
requiresTapSymbol cost = elem CostComponent.TapThis (Cost.components cost)

-- CR 118.3: "A player can't pay a cost without having the necessary resources to
-- pay it fully." The mana part AND every component, measured against the CURRENT
-- state -- before any part of the cost is paid. That is CR-correct rather than
-- convenient: CR 601.2g gives the mana window BEFORE CR 601.2h's payment, so a
-- Mountain tapped for mana is still on the battlefield to be sacrificed
-- afterwards, and sacrificing a permanent never retroactively unmakes the mana
-- it produced.
--
-- CR 118.6: an unpayable cost is never payable ("attempting to pay an unpayable
-- cost is an illegal action").
canPay :: PlayerId -> ObjectId -> Cost -> GameState -> Bool
canPay pid oid cost gs = case Cost.mana cost of
  Nothing -> False
  Just manaCost ->
    Mana.canPay pid manaCost gs
      && all (\component -> canPayComponent pid oid component gs) (Cost.components cost)

canPayComponent :: PlayerId -> ObjectId -> CostComponent -> GameState -> Bool
canPayComponent pid oid component gs = case component of
  -- CR 107.5: "A permanent that's already tapped can't be tapped again to pay
  -- the cost." CR 118.3 gives the same example.
  CostComponent.TapThis -> case Game.lookupObject oid gs of
    Nothing -> False
    Just obj -> Object.zone obj == Zone.Battlefield && Object.tapped obj == TapState.Untapped
  -- CR 701.21a: only a permanent, and only one this player controls.
  CostComponent.SacrificeThis ->
    Set.member oid (GameState.battlefield gs) && Projection.controllerOf oid gs == Just pid

-- CR 601.2g then 601.2h: the mana window first, then the payment. Components are
-- paid in PRINTED order; CR 601.2h lets the player pay in any order, which is an
-- elision here (#N) -- no component in this vocabulary changes another's
-- payability.
--
-- All or nothing. CR 601.2h: "Partial payments are not allowed." The entry state
-- is captured and restored on any rejection, so an Unpaid result is a complete
-- no-op even though paying is monadic and a component may prompt.
pay :: PlayerId -> ObjectId -> Cost -> Game Payment
pay pid oid cost = do
  before <- State.get
  case Cost.mana cost of
    -- CR 118.6: attempting to pay an unpayable cost is an illegal action.
    Nothing -> pure Payment.Unpaid
    Just manaCost -> case Mana.payCost pid manaCost before of
      Nothing -> pure Payment.Unpaid
      Just afterMana -> do
        State.put afterMana
        outcome <- payComponents pid oid (Cost.components cost)
        case outcome of
          Payment.Paid -> pure Payment.Paid
          Payment.Unpaid -> do
            State.put before
            pure Payment.Unpaid

payComponents :: PlayerId -> ObjectId -> [CostComponent] -> Game Payment
payComponents pid oid components = case components of
  [] -> pure Payment.Paid
  component : rest -> do
    outcome <- payComponent pid oid component
    case outcome of
      Payment.Unpaid -> pure Payment.Unpaid
      Payment.Paid -> payComponents pid oid rest

payComponent :: PlayerId -> ObjectId -> CostComponent -> Game Payment
payComponent _ oid component = case component of
  CostComponent.TapThis -> do
    State.modify' (\gs -> gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)})
    pure Payment.Paid
  -- Through Event.sacrifice, the CR 701.21 funnel, and never a direct zone poke:
  -- a cost payment is a game event, so dies-triggers, replacement effects and the
  -- turn history all see it.
  CostComponent.SacrificeThis -> do
    Event.sacrifice oid
    pure Payment.Paid
```

`payComponent`'s first parameter is unused until Task 3 adds `PayLife`; name it `_` now and `pid` then.

- [x] **Step 5: Route `Pawl.Activate` through the door**

In `source/library/Pawl/Activate.hs`, **delete** `canPayAdditional` and `payAdditional` outright, and:

```haskell
-- CR 302.6: a creature's {T}-cost ability can't be activated while summoning
-- sick. Reads projected creature-ness -- a land (Evolving Wilds) is never sick-
-- gated. Asks Pawl.Cost for the CLASSIFICATION rather than matching a component
-- constructor here.
tapSicknessOk :: ObjectId -> ActivatedAbility.ActivatedAbility Card.Card -> GameState -> Bool
tapSicknessOk srcId ability gs =
  let needsTap = Cost.requiresTapSymbol (ActivatedAbility.cost ability)
      isCreature = Set.member CardType.Creature (Projection.cardTypesOf srcId gs)
      settled = case Game.lookupObject srcId gs of
        Just obj -> Object.sickness obj == Sickness.Settled
        Nothing -> False
   in not (needsTap && isCreature && not settled)
```

```haskell
-- CR 602.2/602.5: the ability is a member of the source's abilities
-- (abilitiesFor), it is not a mana ability (mana abilities are handled at
-- payment, not the stack), the whole activation cost is payable (CR 118.3), the
-- {T} sickness gate holds, and enough modes are fillable to satisfy the
-- selection (CR 700.2a/602.2b).
--
-- The cost is the PRINTED one: an activated ability's cost is deliberately not
-- routed through Cost.total (#90).
activatable :: PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card -> GameState -> Bool
activatable pid srcId ability gs =
  Projection.controllerOf srcId gs == Just pid
    && elem ability (abilitiesFor srcId gs)
    && not (Mana.isManaAbility ability)
    && tapSicknessOk srcId ability gs
    && Set.size (Target.fillableModes srcId (ActivatedAbility.modal ability) gs)
      >= fromIntegral (Modal.selectionCount (ActivatedAbility.modal ability))
    && Cost.canPay pid srcId (ActivatedAbility.cost ability) gs
```

and replace `activateAbility`'s whole payment tail (the `let additional = …` block through the end of the function) with:

```haskell
          State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.fromChoices chosen Map.empty Nothing chosenModes}) abilId (GameState.objects g)})
          -- CR 601.2g/h via Pawl.Cost.pay: the mana window, then the components.
          -- activatable pre-checks payability, so within the source elision (#12)
          -- Unpaid is unreachable; reject-not-repair restores the whole
          -- activation -- including the ability object this function put on the
          -- stack -- if it ever is not.
          payment <- Cost.pay pid srcId (ActivatedAbility.cost ability)
          case payment of
            Payment.Paid -> pure ()
            Payment.Unpaid -> State.put gs
```

Delete the now-unused `Pawl.Event`, `Pawl.Type.TapState` and `Control.Monad` imports if GHC reports them unused; keep everything still referenced.

- [x] **Step 6: Route `Pawl.Cast` through the door**

In `source/library/Pawl/Cast.hs`, **delete** `costOf`, add `import Pawl.Type.Cost (Cost)`, and:

```haskell
-- CR 601.2b's X=0 floor measured at CR 601.2f's total: a candidate cost is
-- affordable when it is payable with X=0 (the caster may always choose 0)
-- against the TOTAL cost, not the printed one. Taxing castability without taxing
-- payment lets the player underpay; taxing payment without taxing castability
-- offers a cast that cannot be afforded, and there is no mid-announcement
-- rewind (#56).
payableCost :: PlayerId -> ObjectId -> GameState -> Cost -> Bool
payableCost pid oid gs cost = Cost.canPay pid oid (Cost.total pid oid (Cost.substituteX 0 cost) gs) gs
```

```haskell
-- Affordable and correctly timed, actually in this player's hand, fillable, and
-- not prohibited. CR 601.2b: affordable means at least ONE candidate cost is
-- payable -- a spell may have alternative costs, and only one need be.
castable :: PlayerId -> ObjectId -> GameState -> Bool
castable pid oid gs =
  timingOk pid oid gs
    && elem oid (Game.zoneMembers Zone.Hand pid gs)
    -- CR 601.3: no rule or effect prohibits this player from casting a spell
    -- (Rule of Law, Silence). Gated HERE, upstream of Action.legalActions,
    -- because the engine never offers an illegal action and then rejects it.
    && not (PlayerEffect.prohibitsCasting pid gs)
    && any (payableCost pid oid gs) (Cost.costsFor oid gs)
    && targetable oid gs
```

```haskell
castableWhileSearching :: PlayerId -> GameState -> [ObjectId]
castableWhileSearching pid gs =
  let permitted oid = maybe False permitsCastWhileSearching (Game.cardOf oid gs)
      affordable oid = any (payableCost pid oid gs) (Cost.costsFor oid gs)
      allowed oid = permitted oid && affordable oid && targetable oid gs
   in if PlayerEffect.prohibitsCasting pid gs
        then []
        else filter allowed (Game.zoneMembers Zone.Library pid gs)
```

In `castSpell`, replace the opening `case costOf oid gs of … Just cost -> case Game.cardOf oid gs of` with `case Game.cardOf oid gs of Nothing -> pure (); Just card -> do`, and replace the body from the `let sets = …` line down to the end with:

```haskell
        Monad.when (Set.isSubsetOf chosenModes legal && Set.size chosenModes == fromIntegral count) $ do
          -- CR 601.2b: the cost to be paid is announced after the modes and
          -- before X and targets. One candidate today, so nothing is asked; the
          -- prompt arrives with alternative costs.
          case filter (payableCost pid oid gs) (Cost.costsFor oid gs) of
            [] -> pure ()
            chosenCost : _ -> do
              let sets = Target.legalSetsExcluding oid (Card.modesTargetSpecs chosenModes card) gs
              mAmount <-
                if Cost.hasVariable chosenCost
                  then fmap Just (Trans.lift (Program.prompt (Prompt.ChooseX decider pid oid)))
                  else pure Nothing
              chosen <-
                if Map.null sets
                  then pure Map.empty
                  else Trans.lift (Program.prompt (Prompt.ChooseTargets decider pid oid sets))
              let keysAgree = Map.keysSet chosen == Map.keysSet sets
                  eachLegal = and (Map.intersectionWith Set.member chosen sets)
              Monad.when (keysAgree && eachLegal) $ do
                -- CR 612 binding: choose the basic land types for each
                -- text-change slot. Always answerable (the five basics), so no
                -- castability gate.
                let textSlots = Resolve.textChangeSlots card
                    ask slot = do
                      pair <- Trans.lift (Program.prompt (Prompt.ChooseBasicLandTypes decider pid oid slot))
                      pure (slot, pair)
                bound <- fmap Map.fromList (traverse ask textSlots)
                -- CR 601.2b then 601.2f: substitute X, then compute the total
                -- cost. The object is still in HAND here, one step before 601.2a
                -- moves it to the stack, so a criterion is read against its hand
                -- projection (#89).
                let paidCost = Cost.total pid oid (maybe chosenCost (\x -> Cost.substituteX x chosenCost) mAmount) gs
                payment <- Cost.pay pid oid paidCost
                case payment of
                  Payment.Unpaid -> pure ()
                  Payment.Paid -> do
                    Event.changeZone oid Zone.Stack
                    -- CR 601.2i: the spell has been cast. Emitted here, AFTER
                    -- the last step that can fail, so a rejected announcement
                    -- records nothing.
                    State.modify' (Event.recordEvent (GameEvent.SpellCast pid))
                    moved <- State.get
                    case GameState.stack moved of
                      [] -> pure ()
                      top : _ ->
                        State.put
                          moved
                            { GameState.objects =
                                Map.adjust
                                  (\o -> o {Object.bindings = Binding.fromChoices chosen bound mAmount chosenModes})
                                  top
                                  (GameState.objects moved)
                            }
```

Delete the now-unused `Pawl.Mana`, `Pawl.Type.ManaCost` and `Pawl.Type.ManaSymbol` imports if GHC reports them unused.

- [x] **Step 7: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS. `Pawl.ActivateSpec`, `Pawl.ManaSpec`, `Pawl.CastSpec` and `Pawl.PlayerEffectSpec` are the behaviour-identity tripwires — every one of them exercises a path that now runs through `Pawl.Cost`.

- [x] **Step 8: Confirm the casing surface**

Run:

```bash
grep -rln 'CostComponent\.\(TapThis\|SacrificeThis\)' source/library/ | grep -v 'Pawl/Cost.hs\|Pawl/Codec.hs\|Pawl/Type/'
```

Expected: no output. `Pawl.Activate` must no longer name a component constructor.

- [x] **Step 9: Format, lint, and commit**

```bash
git add source/library/Pawl source/test-suite/Pawl source/test-suite/Main.hs
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p8): Pawl.Cost becomes the payment door

costsFor, total (now Cost -> Cost), canPay/canPayComponent, pay and the
requiresTapSymbol classification, with Pawl.Type.Payment for the answer.
Pawl.Activate and Pawl.Cast both route through it; Cast.costOf and
Activate's two payment helpers are deleted.

pay is transactional: CR 601.2h forbids partial payments, so an Unpaid
result restores the state it was entered with. An ability's cost is NOT
routed through Cost.total -- PlayerEffect.matchesSpell classifies an
object rather than a spell, so Thalia would tax Mindslaver's activation
(#90); the regression is pinned by a test.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 3: `PayLife` and Greed — the amount-bearing component

The first component that carries a number, and the first paid from something other than the board. Greed's falsifier is CR 118.3's own worked example: *"a player with only 1 life can't pay a cost of 2 life."*

**Files:**
- Modify: `source/library/Pawl/Type/CostComponent.hs`, `source/library/Pawl/Codec.hs`, `source/library/Pawl/Cost.hs`
- Create: `data/cards/greed.json`
- Modify: `source/test-suite/Pawl/Cards.hs`
- Test: `source/test-suite/Pawl/CostSpec.hs`, `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Consumes: `Cost.canPayComponent`, `Cost.payComponent`, `Cost.canPay` (Task 2).
- Produces: `CostComponent.PayLife Natural`; `Cards.greedPrinting :: Cards -> Printing`.

- [x] **Step 1: Write the failing tests**

In `source/test-suite/Pawl/CostSpec.hs`, add the group below and register it in `tests` (`Tasty.testGroup "Pawl.Cost" [doorTests cards, greedTests cards]`):

```haskell
-- Greed {3}{B} Enchantment: "{B}, Pay 2 life: Draw a card."
--
-- Scryfall returned no rulings for this card; CR 118.3's own worked example is
-- the specification of the discriminating test.
greedTests :: Cards.Cards -> Tasty.TestTree
greedTests cards =
  let -- alice controls Greed and one untapped Swamp, with three cards in her
      -- library so a draw is never a CR 121.3 loss, and priority in her own
      -- precombat main phase.
      board life =
        let base = S.landsInPlay (Cards.swampPrinting cards) 1
            (greed, gs1) = S.addCreature (Cards.greedPrinting cards) S.alice base
            (_, gs2) = S.addLibraryCard (Cards.pikerPrinting cards) S.alice gs1
            (_, gs3) = S.addLibraryCard (Cards.pikerPrinting cards) S.alice gs2
            (_, gs4) = S.addLibraryCard (Cards.pikerPrinting cards) S.alice gs3
         in ( greed,
              gs4
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice,
                  GameState.players = Map.adjust (\p -> p {Player.life = life}) S.alice (GameState.players gs4)
                }
            )
      isActivate a = case a of
        Action.Type.Activate _ _ -> True
        _ -> False
   in Tasty.testGroup
        "Greed"
        [ HU.testCase "CR 119.4 activating draws a card and subtracts the life" $
            let (greed, gs) = board 20
                activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice greed (theAbility (Cards.greedPrinting cards)))
                resolved = S.runPure S.identityAnswer activated Stack.resolveTop
             in do
                  HU.assertEqual "life 20 - 2" (Just 18) (S.lifeOf S.alice resolved)
                  HU.assertEqual "one card drawn" 1 (S.handSize S.alice resolved)
                  HU.assertEqual "the Swamp is tapped" 1 (S.tappedCount S.alice resolved),
          -- CR 118.3: "A player can't pay a cost without having the necessary
          -- resources to pay it fully. For example, a player with only 1 life
          -- can't pay a cost of 2 life." THE discriminating test: a payability
          -- check that ignores the amount passes the case above and fails here.
          HU.testCase "CR 118.3 at 1 life the ability is not offered" $
            let (greed, gs) = board 1
             in do
                  HU.assertBool
                    "not activatable"
                    (not (Activate.activatable S.alice greed (theAbility (Cards.greedPrinting cards)) gs))
                  HU.assertBool "no Activate action offered" (not (any isActivate (Action.legalActions S.alice gs))),
          HU.testCase "CR 119.4b at 2 life the ability IS offered" $
            let (greed, gs) = board 2
             in HU.assertBool
                  "activatable"
                  (Activate.activatable S.alice greed (theAbility (Cards.greedPrinting cards)) gs),
          -- CR 704.5a: "If a player has 0 or less life, that player loses the
          -- game." Paying life is a real life-total change, and a cost may
          -- legally kill its payer.
          HU.testCase "CR 704.5a paying the last 2 life is legal and loses the game" $
            let (greed, gs) = board 2
                activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice greed (theAbility (Cards.greedPrinting cards)))
                settled = S.settleSba activated
             in do
                  HU.assertEqual "life 0" (Just 0) (S.lifeOf S.alice activated)
                  HU.assertEqual
                    "alice has lost"
                    (Just (Status.Departed Departure.Lost))
                    (fmap Player.status (Map.lookup S.alice (GameState.players settled))),
          -- Greed has no {T} in its cost, so CR 302.6 never applies -- the
          -- counterpart to Llanowar Elves, whose cost is Just [] plus TapThis.
          HU.testCase "CR 302.6 Greed's cost requires no tap symbol" $
            HU.assertBool
              "no {T}"
              (not (Cost.requiresTapSymbol (ActivatedAbility.cost (theAbility (Cards.greedPrinting cards)))))
        ]
```

New imports for `CostSpec`: `Pawl.Action as Action`, `Pawl.Stack as Stack`, `Pawl.Type.Action as Action.Type`, `Pawl.Type.Departure as Departure`, `Pawl.Type.Phase as Phase`, `Pawl.Type.Player as Player`, `Pawl.Type.Status as Status`, `Data.Map.Strict as Map`.

In `source/test-suite/Pawl/CodecSpec.hs`, extend the `"every CostComponent round-trips"` list with `CostComponent.PayLife 2`.

- [x] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Not in scope: data constructor 'CostComponent.PayLife'`, and `Cards.greedPrinting` is not a field of `Cards`.

- [x] **Step 3: Add the constructor**

In `source/library/Pawl/Type/CostComponent.hs`, after `SacrificeThis`:

```haskell
  | -- CR 119.4 / Greed: pay this much life. "If a cost or effect allows a player
    -- to pay an amount of life greater than 0, the player may do so only if
    -- their life total is greater than or equal to the amount of the payment. If
    -- a player pays life, the payment is subtracted from their life total; in
    -- other words, the player loses that much life."
    --
    -- A Natural and not a Quantity: a Quantity's evaluation needs a binding
    -- environment, which a cost has no access to at CR 601.2f time, and no card
    -- in the pool pays a variable amount of life (#N).
    PayLife Natural
```

Add `import Numeric.Natural (Natural)`.

- [x] **Step 4: Add the codec arms**

In `source/library/Pawl/Codec.hs`:

```haskell
  CostComponent.PayLife n -> Json.tagged (Text.pack "PayLife") (Just (natTo n))
```

```haskell
jsonToCostComponent :: Value -> Either Text CostComponent.CostComponent
jsonToCostComponent value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("TapThis", _) -> Right CostComponent.TapThis
    ("SacrificeThis", _) -> Right CostComponent.SacrificeThis
    ("PayLife", Just v) -> fmap CostComponent.PayLife (natFrom v)
    _ -> Left (Text.pack "unknown CostComponent: " <> t)
```

- [x] **Step 5: Add the payability and payment arms**

In `source/library/Pawl/Cost.hs`, `canPayComponent`:

```haskell
  -- CR 119.4: payable only if the life total is at least the amount. CR 119.4b:
  -- "Players can always pay 0 life, no matter what their ... life total is" --
  -- which falls out of >= rather than needing a case.
  CostComponent.PayLife n -> case Map.lookup pid (GameState.players gs) of
    Nothing -> False
    Just player -> Player.life player >= toInteger n
```

and `payComponent` (whose first parameter becomes `pid`):

```haskell
  -- CR 119.4: "the payment is subtracted from their life total; in other words,
  -- the player loses that much life." A direct subtraction, and the CR 704.5a
  -- state-based action that may follow is the existing one in Pawl.Sba.
  CostComponent.PayLife n -> do
    State.modify' (\gs -> gs {GameState.players = Map.adjust (\p -> p {Player.life = Player.life p - toInteger n}) pid (GameState.players gs)})
    pure Payment.Paid
```

`Player.life` is an `Integer`, so this subtraction is total.

- [x] **Step 6: Add the card file**

`data/cards/greed.json` (write it, then normalize with `jq -S . data/cards/greed.json > tmp && mv tmp data/cards/greed.json`):

```json
{
  "activatedAbilities": [
    {
      "cost": {
        "components": [
          {
            "type": "PayLife",
            "value": 2
          }
        ],
        "mana": [
          {
            "type": "OfType",
            "value": {
              "type": "Colored",
              "value": {
                "type": "Black"
              }
            }
          }
        ]
      },
      "modal": {
        "modes": [
          {
            "effects": [
              {
                "type": "Draw",
                "value": {
                  "type": "Literal",
                  "value": 1
                }
              }
            ],
            "targetSpecs": []
          }
        ],
        "selection": {
          "type": "ChooseExactly",
          "value": 1
        }
      }
    }
  ],
  "castingPermissions": [],
  "keywords": [],
  "manaCost": [
    {
      "type": "Generic",
      "value": 3
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
  "name": "Greed",
  "power": null,
  "replacementEffects": [],
  "spell": {
    "modes": [
      {
        "effects": [],
        "targetSpecs": []
      }
    ],
    "selection": {
      "type": "ChooseExactly",
      "value": 1
    }
  },
  "staticAbilities": [],
  "toughness": null,
  "triggeredAbilities": [],
  "typeLine": {
    "subtypes": [],
    "supertypes": [],
    "types": [
      {
        "type": "Enchantment"
      }
    ]
  }
}
```

- [x] **Step 7: Register the printing**

In `source/test-suite/Pawl/Cards.hs`: add `greedPrinting :: Printing.Printing` to the `Cards` record (keeping the field order the file already uses — append after `silencePrinting`), `greedPrinting_ <- loadPrinting "greed"` to `loadCards`, `greedPrinting = greedPrinting_,` to the record it builds, and `greedPrinting cards,` to `allPrintings`. Do **not** add it to any deck.

- [x] **Step 8: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS. A missing `allPrintings` registration fails `Pawl.CardSpec`'s directory-agreement test, not the new group.

- [x] **Step 9: Format, lint, and commit**

```bash
git add source/library/Pawl source/test-suite/Pawl data/cards/greed.json
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p8): CostComponent.PayLife, and Greed

The first component carrying an amount, and the first paid from
something other than the board. CR 118.3's own worked example is the
discriminating test: at 1 life the ability is not offered; at 2 it is,
and paying costs alice the game through the existing CR 704.5a
state-based action.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 4: `Sacrifice`, `ChooseSacrifices`, `Card.additionalCosts` — and Village Rites

The criterion-bearing component, the first cost prompt, and the first cost that reaches a **spell**. Village Rites' one ruling is the specification of two tests.

**Files:**
- Modify: `source/library/Pawl/Type/CostComponent.hs`, `source/library/Pawl/Type/PermanentCriterion.hs`, `source/library/Pawl/Type/Card.hs`, `source/library/Pawl/Type/Prompt.hs`, `source/library/Pawl/Type/Response.hs`, `source/library/Pawl/Codec.hs`, `source/library/Pawl/Cost.hs`, `source/library/Pawl/Replacement.hs`, `source/library/Pawl/Replay.hs`
- Create: `data/cards/village-rites.json`
- Modify: `source/test-suite/Pawl/Cards.hs`, `source/test-suite/Pawl/Support.hs`, `source/benchmark/Main.hs`
- Test: `source/test-suite/Pawl/CostSpec.hs`, `source/test-suite/Pawl/CodecSpec.hs`, `source/test-suite/Pawl/ReplaySpec.hs`

**Interfaces:**
- Consumes: `Cost.costsFor`, `Cost.canPayComponent`, `Cost.payComponent`, `Cast.payableCost` (Task 2).
- Produces: `CostComponent.Sacrifice Natural PermanentCriterion`; `PermanentCriterion.PermanentOfSubtype Subtype`; `Card.additionalCosts :: [CostComponent]`; `Prompt.ChooseSacrifices :: Decider -> PlayerId -> ObjectId -> [ObjectId] -> Natural -> Prompt (Set ObjectId)`; `Response.ChoseSacrifices (Set ObjectId)`; `Cost.matchesCriterion :: GameState -> PermanentCriterion -> ObjectId -> Bool`; `Cards.villageRitesPrinting`.

- [x] **Step 1: Write the failing tests**

In `source/test-suite/Pawl/CostSpec.hs`, add the group below plus the two helpers, and register it in `tests`:

```haskell
-- Every answer the engine asked for, in order -- so a test can assert that a
-- prompt WAS raised (the engine did not decide) or was NOT (the choice was
-- forced and correctly elided). The Pawl.ReplacementSpec shape.
answersFor :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> Game.Type.Game a -> [Response.Response]
answersFor answer gs game = snd (Replay.record answer gs game)

wasAskedToSacrifice :: [Response.Response] -> Bool
wasAskedToSacrifice responses =
  let isSacrifice r = case r of
        Response.ChoseSacrifices _ -> True
        _ -> False
   in any isSacrifice responses

-- Village Rites {B} Instant: "As an additional cost to cast this spell,
-- sacrifice a creature. Draw two cards."
--
-- Its one ruling: "You must sacrifice exactly one creature to cast this spell;
-- you can't cast it without sacrificing a creature, and you can't sacrifice
-- additional creatures."
villageRitesTests :: Cards.Cards -> Tasty.TestTree
villageRitesTests cards =
  let -- alice controls one untapped Swamp and `n` Pikers, holds one Village
      -- Rites, and has three cards in her library so the draw is never a CR
      -- 121.3 loss.
      board n =
        let base = S.landsInPlay (Cards.swampPrinting cards) 1
            addPiker (ids, gs) _ = let (oid, gs') = S.addPiker cards S.alice gs in (ids ++ [oid], gs')
            (pikers, withPikers) = List.foldl' addPiker ([], base) [1 .. n]
            (rites, gs1) = S.addHandCard (Cards.villageRitesPrinting cards) S.alice withPikers
            (_, gs2) = S.addLibraryCard (Cards.pikerPrinting cards) S.alice gs1
            (_, gs3) = S.addLibraryCard (Cards.pikerPrinting cards) S.alice gs2
            (_, gs4) = S.addLibraryCard (Cards.pikerPrinting cards) S.alice gs3
         in ( rites,
              pikers,
              gs4
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            )
   in Tasty.testGroup
        "Village Rites"
        [ HU.testCase "CR 118.8 the additional cost is paid and the spell resolves" $
            let (rites, pikers, gs) = board 1
                cast = S.runPure S.identityAnswer gs (Cast.castSpell S.alice rites)
                resolved = S.runPure S.identityAnswer cast Stack.resolveTop
             in do
                  HU.assertEqual "no creature left on the battlefield" 0 (S.creaturesInPlay S.alice resolved)
                  HU.assertBool
                    "the Piker that was on the battlefield is the one in the graveyard"
                    (all (\oid -> elem oid (Game.zoneMembers Zone.Graveyard S.alice resolved)) pikers)
                  HU.assertEqual "two cards drawn" 2 (S.handSize S.alice resolved),
          -- The ruling's second clause, and CR 601.2f's placement of an
          -- additional cost INSIDE the total cost: an implementation that pays
          -- additional costs after announcement offers this cast.
          HU.testCase "CR 601.2f with no creature to sacrifice the spell is not castable" $
            let (rites, _, gs) = board 0
             in do
                  HU.assertBool "not castable" (not (Cast.castable S.alice rites gs))
                  HU.assertEqual "and not offered" [] (filter (isCastOf rites) (Action.legalActions S.alice gs)),
          -- The cost payment went through Event.sacrifice, the CR 701.21 funnel,
          -- so the turn history saw it. A direct zone poke passes both cases
          -- above and fails this one. The settle/resolve shape is
          -- Pawl.TriggerSpec's historyTests, verbatim.
          HU.testCase "CR 608.2i Khabál Ghoul counts a creature sacrificed to pay a cost" $
            let (rites, _, gs0) = board 1
                (ghoul, gs1) = S.addCreature (Cards.khabalGhoulPrinting cards) S.alice gs0
                cast = S.runPure S.identityAnswer gs1 (Cast.castSpell S.alice rites)
                endStep = Phase.Ending EndingStep.EndStep
                beginEndStep gs = Event.recordEvent (GameEvent.StepBegan endStep S.alice) (gs {GameState.phase = endStep})
                settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority
                resolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop
                atEnd = resolveAll (settle (beginEndStep (settle cast)))
                counters = maybe 0 (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject ghoul atEnd)
             in HU.assertEqual "one +1/+1 counter for the sacrificed Piker" 1 counters,
          -- CR 701.21a lets the player choose which of their permanents dies, so
          -- two candidates is a real choice; one is not, and where the rules
          -- leave nothing to ask, don't prompt.
          HU.testCase "CR 701.21a two creatures raise ChooseSacrifices; one elides it" $
            let (ritesTwo, _, twoPikers) = board 2
                (ritesOne, _, onePiker) = board 1
                askedTwo = answersFor S.identityAnswer twoPikers (Cast.castSpell S.alice ritesTwo)
                askedOne = answersFor S.identityAnswer onePiker (Cast.castSpell S.alice ritesOne)
             in do
                  HU.assertBool "asked with two" (wasAskedToSacrifice askedTwo)
                  HU.assertBool "not asked with one" (not (wasAskedToSacrifice askedOne)),
          -- CR 115.1 makes a target only what the word "target" names: a
          -- sacrifice choice is not one, so it must not travel as a target.
          HU.testCase "CR 115.1 the sacrifice choice is not a target choice" $
            let (rites, _, gs) = board 2
                asked = answersFor S.identityAnswer gs (Cast.castSpell S.alice rites)
                isTargets r = case r of
                  Response.ChoseTargets _ -> True
                  _ -> False
             in HU.assertBool "no ChooseTargets was raised" (not (any isTargets asked))
        ]

isCastOf :: ObjectId.ObjectId -> Action.Type.Action -> Bool
isCastOf oid a = case a of
  Action.Type.Cast o -> o == oid
  _ -> False
```

Also add a unit case to `doorTests` for the criterion-bearing arm — this is what covers `PermanentOfSubtype` before Fireblast produces one:

```haskell
      -- CR 701.21a: enough controlled permanents matching the criterion.
      HU.testCase "CR 118.3 a Sacrifice component counts matching permanents this player controls" $
        let gs = S.mountainsInPlay cards 2
            two = CostComponent.Sacrifice 2 (PermanentCriterion.PermanentOfSubtype Subtype.Mountain)
            three = CostComponent.Sacrifice 3 (PermanentCriterion.PermanentOfSubtype Subtype.Mountain)
            islands = CostComponent.Sacrifice 1 (PermanentCriterion.PermanentOfSubtype Subtype.Island)
         in do
              HU.assertBool "two Mountains pay for two" (Cost.canPayComponent S.alice S.noSource two gs)
              HU.assertBool "but not for three" (not (Cost.canPayComponent S.alice S.noSource three gs))
              HU.assertBool "and not for an Island" (not (Cost.canPayComponent S.alice S.noSource islands gs))
              HU.assertBool "and bob controls none of them" (not (Cost.canPayComponent S.bob S.noSource two gs)),
```

In `source/test-suite/Pawl/CodecSpec.hs`: extend the `"every CostComponent round-trips"` list with `CostComponent.Sacrifice 2 (PermanentCriterion.PermanentOfSubtype Subtype.Mountain)`, extend the existing `PermanentCriterion` coverage with `PermanentCriterion.PermanentOfSubtype Subtype.Mountain`, and add:

```haskell
          HU.testCase "a Card carrying an additional cost round-trips" $
            let base = Printing.card (Cards.lightningBoltPrinting cards)
                c = base {CardT.additionalCosts = [CostComponent.Sacrifice 1 PermanentCriterion.CreaturePermanent]}
             in roundTrip "card" Codec.cardToJson Codec.jsonToCard c,
          -- Byte-stability: an empty list must not appear in the rendered JSON,
          -- or every committed card file changes. The posture colorIndicator,
          -- delayedAbilities and playerAbilities already take.
          HU.testCase "an empty additionalCosts list is omitted from the JSON" $
            let base = Printing.card (Cards.lightningBoltPrinting cards)
             in do
                  HU.assertEqual "the fixture really has none" [] (CardT.additionalCosts base)
                  case J.asObject (Codec.cardToJson base) of
                    Left err -> HU.assertFailure (Text.unpack err)
                    Right pairs -> HU.assertBool "key absent" (notElem (Text.pack "additionalCosts") (map fst pairs)),
```

In `source/test-suite/Pawl/ReplaySpec.hs`, add to `combatReplayTests`:

```haskell
          HU.testCase "ChooseSacrifices records and replays a Set ObjectId" $
            let p = Prompt.ChooseSacrifices decider S.alice oid [oid, ObjectId.MkObjectId 8] 1
                answer = Set.singleton (ObjectId.MkObjectId 8)
             in HU.assertEqual "round trip" (Just answer) (Replay.decode p (Replay.encode p answer)),
          HU.testCase "defaultAnswer sacrifices the first `count` offered, in order" $
            HU.assertEqual
              "the ascending prefix"
              (Set.singleton oid)
              (Replay.defaultAnswer (Prompt.ChooseSacrifices decider S.alice oid [oid, ObjectId.MkObjectId 8] 1)),
```

- [x] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Not in scope: data constructor 'CostComponent.Sacrifice'`, `'PermanentCriterion.PermanentOfSubtype'`, `'Prompt.ChooseSacrifices'`, `'Response.ChoseSacrifices'`, and `additionalCosts` is not a field of `Card`.

- [x] **Step 3: Add the two constructors**

In `source/library/Pawl/Type/CostComponent.hs`:

```haskell
  | -- CR 701.21a / CR 601.2f's "sacrificing permanents": sacrifice this many
    -- permanents matching the criterion (Village Rites' one creature,
    -- Fireblast's two Mountains). The player chooses which (CR 701.21a), so this
    -- is a prompt and never an engine pick.
    --
    -- The criterion is matched against the PROJECTION, never a printed
    -- characteristic: Blood Moon makes a nonbasic land a Mountain, and it may be
    -- sacrificed as one.
    Sacrifice Natural PermanentCriterion
```

with `import Pawl.Type.PermanentCriterion (PermanentCriterion)`.

In `source/library/Pawl/Type/PermanentCriterion.hs`:

```haskell
  | -- CR 205.3: a permanent with this subtype (Fireblast's Mountains). Matched
    -- through the projection, so a type-changing effect is seen.
    PermanentOfSubtype Subtype
```

with `import Pawl.Type.Subtype (Subtype)`, and extend the module comment: the growth path it already describes, one constructor further along; P9 still merges it with `CardCriterion`.

- [x] **Step 4: Add the two prompt constructors and their transcript arms**

`source/library/Pawl/Type/Prompt.hs`:

```haskell
  -- CR 701.21a: which permanents to sacrifice to pay a cost. The ObjectId is the
  -- spell being cast or the permanent whose ability is being activated; the
  -- [ObjectId] is the payer's permanents matching the component's criterion (the
  -- engine pre-filters, in ascending order); the Natural is how many. The answer
  -- is a Set because one permanent cannot be sacrificed twice for one payment.
  --
  -- Deliberately NOT Prompt.ChooseTargets or the TargetSpec machinery: CR 115.1
  -- makes a target only what the word "target" names, and conflating the two
  -- would let shroud, hexproof and "becomes the target" triggers observe a
  -- sacrifice choice. Its shape mirrors ChooseDiscard (candidates plus a count).
  --
  -- Asked ONLY when there is a choice -- more candidates than the count. Exactly
  -- as many as the count is forced, and where the rules leave nothing to ask,
  -- don't prompt.
  ChooseSacrifices :: Decider -> PlayerId -> ObjectId -> [ObjectId] -> Natural -> Prompt (Set ObjectId)
```

`source/library/Pawl/Type/Response.hs`:

```haskell
  | -- CR 701.21a: the permanents a player chose to sacrifice to pay a cost,
    -- serialized so a DecisionLog replays the payment deterministically.
    ChoseSacrifices (Set ObjectId)
```

`source/library/Pawl/Replay.hs` — one arm in each of the three functions:

```haskell
  Prompt.ChooseSacrifices {} -> Response.ChoseSacrifices answer
```

```haskell
  Prompt.ChooseSacrifices {} -> case response of
    Response.ChoseSacrifices ids -> Just ids
    _ -> Nothing
```

```haskell
  -- The first `count` candidates, which the engine offers in ascending order --
  -- a legal answer whenever the prompt was legal to ask, and the least eventful
  -- fallback when a transcript runs short.
  Prompt.ChooseSacrifices _ _ _ candidates count -> Set.fromList (take (fromIntegral count) candidates)
```

- [x] **Step 5: Answer the new prompt in all eight interpreters**

Add the same arm to the five answerers in `source/test-suite/Pawl/Support.hs` (`identityAnswer`, `castAnswer`, `aggressiveAnswer`, `playLandAnswer`, and `randomAnswer` with `pure`) and the three in `source/benchmark/Main.hs` (`alwaysPass`, `castAnswer`, `fightAnswer`):

```haskell
  Prompt.ChooseSacrifices _ _ _ candidates count -> Set.fromList (take (fromIntegral count) candidates)
```

```haskell
  -- randomAnswer only
  Prompt.ChooseSacrifices _ _ _ candidates count -> pure (Set.fromList (take (fromIntegral count) candidates))
```

- [x] **Step 6: Add `Card.additionalCosts` and its codec**

`source/library/Pawl/Type/Card.hs`, after `castingPermissions`:

```haskell
    -- CR 118.8: this card's printed additional costs -- "a cost listed in a
    -- spell's rules text ... that its controller must pay at the same time they
    -- pay the spell's mana cost" (Village Rites). Empty for every other
    -- printing.
    --
    -- Read DIRECTLY from the card and never through the projection, the
    -- castingPermissions precedent: a cost is consulted while the object is in
    -- hand, where the CR 613 layer system does not reach.
    --
    -- CR 118.8d: this does not change the card's mana cost. Card.manaCost, and
    -- every reader of mana value, is unaffected.
    additionalCosts :: [CostComponent],
```

with `import Pawl.Type.CostComponent (CostComponent)`.

`source/library/Pawl/Codec.hs` — in `cardToJson`'s optional tail:

```haskell
        ++ ( if null (CardT.additionalCosts c)
               then []
               else [(Text.pack "additionalCosts", listTo costComponentToJson (CardT.additionalCosts c))]
           )
```

and in `jsonToCard`:

```haskell
  additionalCosts <- listFromDefault jsonToCostComponent (getOpt (Text.pack "additionalCosts") ps)
```

```haskell
        CardT.additionalCosts = additionalCosts,
```

Add the `PermanentOfSubtype` arm to the criterion codec, which must be restructured from `decodeNullary` to a tagged decode now that the family carries a payload:

```haskell
permanentCriterionToJson :: PermanentCriterion.PermanentCriterion -> Value
permanentCriterionToJson c = case c of
  PermanentCriterion.AnyPermanent -> nullary (Text.pack "AnyPermanent")
  PermanentCriterion.CreaturePermanent -> nullary (Text.pack "CreaturePermanent")
  PermanentCriterion.PermanentOfSubtype s -> Json.tagged (Text.pack "PermanentOfSubtype") (Just (subtypeToJson s))

jsonToPermanentCriterion :: Value -> Either Text PermanentCriterion.PermanentCriterion
jsonToPermanentCriterion value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("AnyPermanent", _) -> Right PermanentCriterion.AnyPermanent
    ("CreaturePermanent", _) -> Right PermanentCriterion.CreaturePermanent
    ("PermanentOfSubtype", Just v) -> fmap PermanentCriterion.PermanentOfSubtype (jsonToSubtype v)
    _ -> Left (Text.pack "unknown PermanentCriterion: " <> t)
```

and the `Sacrifice` arms to the component codec:

```haskell
  CostComponent.Sacrifice n c_ -> Json.tagged (Text.pack "Sacrifice") (Just (Array [natTo n, permanentCriterionToJson c_]))
```

```haskell
    ("Sacrifice", Just (Array [n, c_])) -> do
      count <- natFrom n
      criterion <- jsonToPermanentCriterion c_
      pure (CostComponent.Sacrifice count criterion)
```

- [x] **Step 7: Keep `Pawl.Replacement.matchesPermanent` total**

`source/library/Pawl/Replacement.hs`:

```haskell
  PermanentCriterion.PermanentOfSubtype s -> Set.member s (Projection.subtypesOf oid gs)
```

Leave a comment at this site: `PermanentCriterion` is now matched here **and** in `Pawl.Cost.matchesCriterion`; the two are not shared because `Pawl.Cost` importing `Pawl.Replacement` would collide with #72's queued fix (which needs `Pawl.Replacement` to consult costs). Cite `(#N)` — Task 7 replaces it.

- [x] **Step 8: Add the payability and payment arms, and the criterion matcher**

`source/library/Pawl/Cost.hs`:

```haskell
-- Which permanents a criterion admits, matched through the PROJECTION and never
-- against printed characteristics: a card type is CR 613.1d layer 4 and a
-- subtype is layer 4 too, so Blood Moon changes the answer.
--
-- The sibling of Pawl.Replacement.matchesPermanent, deliberately not shared with
-- it: Pawl.Cost importing Pawl.Replacement would become a module cycle the
-- moment CR 614.12b's payable-cost check lands there (#N). P9's filter language
-- merges both.
matchesCriterion :: GameState -> PermanentCriterion -> ObjectId -> Bool
matchesCriterion gs criterion oid = case criterion of
  PermanentCriterion.AnyPermanent -> True
  PermanentCriterion.CreaturePermanent -> Projection.isCreatureOf oid gs
  PermanentCriterion.PermanentOfSubtype subtype -> Set.member subtype (Projection.subtypesOf oid gs)

-- The permanents this player may sacrifice for a criterion, ascending -- the
-- order ChooseSacrifices offers them in, which is what makes both the elision
-- test and the transcript fallback deterministic.
sacrificeCandidates :: PlayerId -> PermanentCriterion -> GameState -> [ObjectId]
sacrificeCandidates pid criterion gs =
  List.sort (filter (matchesCriterion gs criterion) (Projection.controls pid gs))
```

`canPayComponent`:

```haskell
  -- CR 701.21a: this player must control at least `n` matching permanents.
  -- CR 118.10's "each payment of a cost applies to only one spell, ability, or
  -- effect" is not enforced across two components of ONE cost (#N).
  CostComponent.Sacrifice n criterion ->
    length (sacrificeCandidates pid criterion gs) >= fromIntegral n
```

`payComponent`:

```haskell
  -- CR 701.21a: the player chooses which of their permanents dies, so this is a
  -- prompt. Elided only when forced -- exactly as many candidates as the count.
  -- Three payable Mountains and a count of two is a real choice and IS asked:
  -- Mountains differ in tap state, counters and attached auras, so "they are all
  -- the same" is not a claim this engine may make.
  --
  -- Reject-not-repair: an answer that is not a size-`n` subset of the offered
  -- candidates makes the whole payment Unpaid, which pay's restore turns into a
  -- no-op.
  CostComponent.Sacrifice n criterion -> do
    gs <- State.get
    let candidates = sacrificeCandidates pid criterion gs
        decider = Decide.deciderFor pid gs
    chosen <-
      if length candidates <= fromIntegral n
        then pure (Set.fromList candidates)
        else Trans.lift (Program.prompt (Prompt.ChooseSacrifices decider pid oid candidates n))
    if Set.isSubsetOf chosen (Set.fromList candidates) && Set.size chosen == fromIntegral n
      then do
        Monad.mapM_ Event.sacrifice (Set.toAscList chosen)
        pure Payment.Paid
      else pure Payment.Unpaid
```

Finally, teach `costsFor` about the new field:

```haskell
    Source.OfCard printing ->
      let card = Printing.card printing
       in [Cost.MkCost {Cost.mana = Card.manaCost card, Cost.components = Card.additionalCosts card}]
```

- [x] **Step 9: Add the card file and register the printing**

`data/cards/village-rites.json` (normalize with `jq -S .` afterwards):

```json
{
  "activatedAbilities": [],
  "additionalCosts": [
    {
      "type": "Sacrifice",
      "value": [
        1,
        {
          "type": "CreaturePermanent"
        }
      ]
    }
  ],
  "castingPermissions": [],
  "keywords": [],
  "manaCost": [
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
  "name": "Village Rites",
  "power": null,
  "replacementEffects": [],
  "spell": {
    "modes": [
      {
        "effects": [
          {
            "type": "Draw",
            "value": {
              "type": "Literal",
              "value": 2
            }
          }
        ],
        "targetSpecs": []
      }
    ],
    "selection": {
      "type": "ChooseExactly",
      "value": 1
    }
  },
  "staticAbilities": [],
  "toughness": null,
  "triggeredAbilities": [],
  "typeLine": {
    "subtypes": [],
    "supertypes": [],
    "types": [
      {
        "type": "Instant"
      }
    ]
  }
}
```

Register `villageRitesPrinting` in `source/test-suite/Pawl/Cards.hs` exactly as Greed was registered (record field, `loadPrinting "village-rites"`, record build, `allPrintings`). No deck.

- [x] **Step 10: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS.

- [x] **Step 11: Format, lint, and commit**

```bash
git add source/library/Pawl source/test-suite/Pawl source/test-suite/Main.hs source/benchmark/Main.hs data/cards/village-rites.json
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p8): spell-side additional costs, and Village Rites

CostComponent.Sacrifice with a PermanentCriterion (which gains
PermanentOfSubtype), Card.additionalCosts, and Prompt.ChooseSacrifices
-- a Set, elided only when the candidates exactly equal the count, and
deliberately not a target (CR 115.1).

CR 601.2f puts an additional cost inside the total cost, so Village
Rites with no creature is not castable at all. The payment runs through
Event.sacrifice, the CR 701.21 funnel, so Khabál Ghoul counts the
creature at end of turn.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 5: `Card.alternativeCosts`, `ChooseCost` — and Fireblast

The seam. An alternative cost is *not* a different mana cost: Fireblast's contains no mana at all, so the printed `{4}{R}{R}` is unaffordable while the spell is castable and deals 4.

**Files:**
- Modify: `source/library/Pawl/Type/Card.hs`, `source/library/Pawl/Type/Prompt.hs`, `source/library/Pawl/Type/Response.hs`, `source/library/Pawl/Codec.hs`, `source/library/Pawl/Cost.hs`, `source/library/Pawl/Cast.hs`, `source/library/Pawl/Replay.hs`
- Create: `data/cards/fireblast.json`
- Modify: `source/test-suite/Pawl/Cards.hs`, `source/test-suite/Pawl/Support.hs`, `source/benchmark/Main.hs`
- Test: `source/test-suite/Pawl/CostSpec.hs`, `source/test-suite/Pawl/CodecSpec.hs`, `source/test-suite/Pawl/ReplaySpec.hs`

**Interfaces:**
- Consumes: `Cost.costsFor`, `Cast.payableCost`, `Cost.firstOffered`, `Cost.unpayable` (Tasks 2 and 4).
- Produces: `Card.alternativeCosts :: [Cost]`; `Prompt.ChooseCost :: Decider -> PlayerId -> ObjectId -> [Cost] -> Prompt Cost`; `Response.ChoseCost Cost`; `Cards.fireblastPrinting`.

- [x] **Step 1: Write the failing tests**

In `source/test-suite/Pawl/CostSpec.hs`, add the helper and the group, and register the group in `tests`:

```haskell
wasAskedToChooseCost :: [Response.Response] -> Bool
wasAskedToChooseCost responses =
  let isCost r = case r of
        Response.ChoseCost _ -> True
        _ -> False
   in any isCost responses

-- Fireblast {4}{R}{R} Instant: "You may sacrifice two Mountains rather than pay
-- this spell's mana cost. Fireblast deals 4 damage to any target."
--
-- Scryfall returned no rulings for this card.
fireblastTests :: Cards.Cards -> Tasty.TestTree
fireblastTests cards =
  let -- alice controls `n` Mountains (all tapped when `tap` is True) and holds
      -- one Fireblast, with priority in her own precombat main phase.
      board n tap =
        let base = S.landsInPlay (Cards.mountainPrinting cards) n
            tapAll gs = List.foldl' (flip S.tapObject) gs (Set.toList (GameState.battlefield gs))
            tapped = if tap then tapAll base else base
            (fireblast, gs1) = S.addHandCard (Cards.fireblastPrinting cards) S.alice tapped
         in ( fireblast,
              gs1
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            )
   in Tasty.testGroup
        "Fireblast"
        [ -- The headline test: the printed cost is unaffordable and the spell is
          -- castable anyway. Kills "castability is mana affordability" and "an
          -- alternative cost is a different ManaCost" at once.
          HU.testCase "CR 118.9 two TAPPED Mountains and an empty pool still cast it, and it deals 4" $
            let (fireblast, gs) = board 2 True
                cast = S.runPure S.identityAnswer gs (Cast.castSpell S.alice fireblast)
                resolved = S.runPure S.identityAnswer cast Stack.resolveTop
             in do
                  HU.assertBool "castable" (Cast.castable S.alice fireblast gs)
                  HU.assertEqual "both Mountains sacrificed" 0 (length (Game.zoneMembers Zone.Battlefield S.alice resolved))
                  HU.assertEqual "alice took 4 (identityAnswer targets the lowest recipient)" (Just 16) (S.lifeOf S.alice resolved),
          -- CR 118.9b: an alternative cost is optional, so a player who can
          -- afford both is really choosing.
          HU.testCase "CR 118.9b both costs payable raises ChooseCost; one payable elides it" $
            let (both, sixUntapped) = board 6 False
                (onlyAlternative, twoTapped) = board 2 True
                askedBoth = answersFor S.identityAnswer sixUntapped (Cast.castSpell S.alice both)
                askedOne = answersFor S.identityAnswer twoTapped (Cast.castSpell S.alice onlyAlternative)
             in do
                  HU.assertBool "asked when both are payable" (wasAskedToChooseCost askedBoth)
                  HU.assertBool "not asked when only one is" (not (wasAskedToChooseCost askedOne)),
          -- CR 118.9a: "Only one alternative cost can be applied to any one spell
          -- as it's being cast" -- the list-of-candidates shape itself. The
          -- printed cost is offered FIRST.
          HU.testCase "CR 118.9a costsFor offers the printed cost first, then each alternative" $
            let (fireblast, gs) = board 2 True
                candidates = Cost.costsFor fireblast gs
                red = ManaSymbol.OfType (ManaType.Colored Color.Red)
             in do
                  HU.assertEqual "two candidates" 2 (length candidates)
                  HU.assertEqual
                    "the printed one first"
                    [Just (ManaCost.MkManaCost [ManaSymbol.Generic 4, red, red]), Just (ManaCost.MkManaCost [])]
                    (map Cost.Type.mana candidates),
          -- CR 701.21a again, on the alternative's own component.
          HU.testCase "CR 701.21a three Mountains raise ChooseSacrifices; exactly two elide it" $
            let (three, threeMountains) = board 3 True
                (two, twoMountains) = board 2 True
                askedThree = answersFor S.identityAnswer threeMountains (Cast.castSpell S.alice three)
                askedTwo = answersFor S.identityAnswer twoMountains (Cast.castSpell S.alice two)
             in do
                  HU.assertBool "asked with three" (wasAskedToSacrifice askedThree)
                  HU.assertBool "not asked with exactly two" (not (wasAskedToSacrifice askedTwo)),
          HU.testCase "CR 118.3 one Mountain pays neither cost" $
            let (fireblast, gs) = board 1 True
             in HU.assertBool "not castable" (not (Cast.castable S.alice fireblast gs))
        ]
```

In `source/test-suite/Pawl/CodecSpec.hs`:

```haskell
          HU.testCase "a Card carrying an alternative cost round-trips" $
            let base = Printing.card (Cards.lightningBoltPrinting cards)
                alt =
                  Cost.Type.MkCost
                    { Cost.Type.mana = Just (ManaCost.MkManaCost []),
                      Cost.Type.components = [CostComponent.Sacrifice 2 (PermanentCriterion.PermanentOfSubtype Subtype.Mountain)]
                    }
                c = base {CardT.alternativeCosts = [alt]}
             in roundTrip "card" Codec.cardToJson Codec.jsonToCard c,
          HU.testCase "an empty alternativeCosts list is omitted from the JSON" $
            let base = Printing.card (Cards.lightningBoltPrinting cards)
             in do
                  HU.assertEqual "the fixture really has none" [] (CardT.alternativeCosts base)
                  case J.asObject (Codec.cardToJson base) of
                    Left err -> HU.assertFailure (Text.unpack err)
                    Right pairs -> HU.assertBool "key absent" (notElem (Text.pack "alternativeCosts") (map fst pairs)),
```

In `source/test-suite/Pawl/ReplaySpec.hs`, add to `combatReplayTests`:

```haskell
          HU.testCase "ChooseCost records and replays a Cost" $
            let printed = Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 4])) []
                alternative = Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) [CostComponent.SacrificeThis]
                p = Prompt.ChooseCost decider S.alice oid [printed, alternative]
             in HU.assertEqual "round trip" (Just alternative) (Replay.decode p (Replay.encode p alternative)),
          HU.testCase "defaultAnswer takes the first offered cost (the printed one)" $
            let printed = Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 4])) []
                alternative = Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) [CostComponent.SacrificeThis]
             in HU.assertEqual
                  "the printed one"
                  printed
                  (Replay.defaultAnswer (Prompt.ChooseCost decider S.alice oid [printed, alternative])),
```

- [x] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Not in scope: data constructor 'Prompt.ChooseCost'`, `'Response.ChoseCost'`, and `alternativeCosts` is not a field of `Card`.

- [x] **Step 3: Add `Card.alternativeCosts` and its codec**

`source/library/Pawl/Type/Card.hs`, after `additionalCosts`:

```haskell
    -- CR 118.9: this card's printed alternative costs -- "a cost listed in a
    -- spell's text ... that its controller MAY pay rather than paying the
    -- spell's mana cost" (Fireblast). Empty for every other printing.
    --
    -- A LIST because a card may print more than one, not because more than one
    -- may be paid: CR 118.9a says "only one alternative cost can be applied to
    -- any one spell as it's being cast", which is what makes
    -- Pawl.Cost.costsFor's list a list of CANDIDATES the caster picks from.
    --
    -- Each carries its OWN mana part, which is how CR 118.6a's second sentence
    -- ("if an alternative cost is applied to an unpayable cost ... the
    -- alternative cost may be paid") falls out of the shape. Fireblast's is
    -- Just [] -- a real, taxable {0}, not Nothing.
    --
    -- CR 118.9c: this does not change the card's mana cost.
    --
    -- Printed-only: an effect that GRANTS an alternative cost has no carrier
    -- here (#N). CR 118.9's first sentence is "Some SPELLS have alternative
    -- costs", so this lives on Card and never on ActivatedAbility -- a rules
    -- fact, not an elision.
    alternativeCosts :: [Cost],
```

with `import Pawl.Type.Cost (Cost)`.

`source/library/Pawl/Codec.hs` — the optional tail of `cardToJson`:

```haskell
        ++ ( if null (CardT.alternativeCosts c)
               then []
               else [(Text.pack "alternativeCosts", listTo costToJson (CardT.alternativeCosts c))]
           )
```

and in `jsonToCard`:

```haskell
  alternativeCosts <- listFromDefault jsonToCost (getOpt (Text.pack "alternativeCosts") ps)
```

```haskell
        CardT.alternativeCosts = alternativeCosts,
```

- [x] **Step 4: Add the `ChooseCost` prompt and its transcript arms**

`source/library/Pawl/Type/Prompt.hs`:

```haskell
  -- CR 601.2b: "If the spell has alternative or additional costs that will be
  -- paid as it's being cast ... the player announces their intentions to pay any
  -- or all of those costs." Issued after the modes and before X and targets, at
  -- 601.2b's own position. The ObjectId is the spell; the [Cost] is the PAYABLE
  -- candidates (the engine pre-filters: each candidate from Pawl.Cost.costsFor,
  -- run through total, then tested with canPay at the CR 601.2b X=0 floor).
  --
  -- CR 118.9b makes an alternative cost optional, so a player who can afford both
  -- is genuinely choosing. Asked ONLY when two or more candidates are payable;
  -- one is forced, and where the rules leave nothing to ask, don't prompt.
  ChooseCost :: Decider -> PlayerId -> ObjectId -> [Cost] -> Prompt Cost
```

with `import Pawl.Type.Cost (Cost)`.

`source/library/Pawl/Type/Response.hs`:

```haskell
  | -- CR 601.2b: the cost a caster announced they would pay, serialized so a
    -- DecisionLog replays an alternative-cost cast deterministically.
    ChoseCost Cost
```

`source/library/Pawl/Replay.hs`:

```haskell
  Prompt.ChooseCost {} -> Response.ChoseCost answer
```

```haskell
  Prompt.ChooseCost {} -> case response of
    Response.ChoseCost cost -> Just cost
    _ -> Nothing
```

```haskell
  -- The first offered candidate is the PRINTED cost (Pawl.Cost.costsFor puts it
  -- first) -- the least eventful fallback when a transcript runs short, since it
  -- sacrifices nothing. Cost.firstOffered keeps this total for the empty list the
  -- engine never produces.
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered candidates
```

with `import qualified Pawl.Cost as Cost` in `Pawl.Replay`.

- [x] **Step 5: Answer the new prompt in all eight interpreters**

Add to the five answerers in `source/test-suite/Pawl/Support.hs` and the three in `source/benchmark/Main.hs` (importing `Pawl.Cost as Cost` in each file):

```haskell
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered candidates
```

```haskell
  -- randomAnswer only
  Prompt.ChooseCost _ _ _ candidates -> pure (Cost.firstOffered candidates)
```

- [x] **Step 6: Teach `costsFor` about the alternatives**

`source/library/Pawl/Cost.hs`:

```haskell
    Source.OfCard printing ->
      let card = Printing.card printing
          printed = Cost.MkCost {Cost.mana = Card.manaCost card, Cost.components = Card.additionalCosts card}
          -- CR 118.9d in one line: "If an alternative cost is being paid to cast
          -- a spell, any additional costs, cost increases, and cost reductions
          -- that affect that spell are applied to that alternative cost." An
          -- alternative replaces only the MANA cost; every additional cost still
          -- applies. The increases and reductions are Pawl.Cost.total's job,
          -- called on whichever candidate is chosen.
          withAdditional alternative =
            alternative {Cost.components = Cost.components alternative ++ Card.additionalCosts card}
       in printed : map withAdditional (Card.alternativeCosts card)
```

- [x] **Step 7: Issue the prompt in `Cast.castSpell`**

In `source/library/Pawl/Cast.hs`, replace Task 2's `case filter … of [] -> pure (); chosenCost : _ -> do` block opener with:

```haskell
          -- CR 601.2b: the cost to be paid is announced after the modes and
          -- before X and targets. Only PAYABLE candidates are offered (CR
          -- 118.9b makes an alternative optional, so a player who can afford both
          -- is really choosing); one payable candidate is forced and unprompted.
          -- Reject-not-repair: an answer outside the offered set makes the whole
          -- cast a no-op.
          let payable = filter (payableCost pid oid gs) (Cost.costsFor oid gs)
          Monad.unless (null payable) $ do
            chosenCost <- case payable of
              [only] -> pure only
              _ -> Trans.lift (Program.prompt (Prompt.ChooseCost decider pid oid payable))
            Monad.when (elem chosenCost payable) $ do
```

with the rest of the block (the `let sets = …` line onward) re-indented one level under it.

- [x] **Step 8: Add the card file and register the printing**

`data/cards/fireblast.json` (normalize with `jq -S .` afterwards):

```json
{
  "activatedAbilities": [],
  "alternativeCosts": [
    {
      "components": [
        {
          "type": "Sacrifice",
          "value": [
            2,
            {
              "type": "PermanentOfSubtype",
              "value": {
                "type": "Mountain"
              }
            }
          ]
        }
      ],
      "mana": []
    }
  ],
  "castingPermissions": [],
  "keywords": [],
  "manaCost": [
    {
      "type": "Generic",
      "value": 4
    },
    {
      "type": "OfType",
      "value": {
        "type": "Colored",
        "value": {
          "type": "Red"
        }
      }
    },
    {
      "type": "OfType",
      "value": {
        "type": "Colored",
        "value": {
          "type": "Red"
        }
      }
    }
  ],
  "name": "Fireblast",
  "power": null,
  "replacementEffects": [],
  "spell": {
    "modes": [
      {
        "effects": [
          {
            "type": "DealDamage",
            "value": [
              "target",
              {
                "type": "Literal",
                "value": 4
              }
            ]
          }
        ],
        "targetSpecs": [
          {
            "slot": "target",
            "spec": {
              "type": "AnyTarget"
            }
          }
        ]
      }
    ],
    "selection": {
      "type": "ChooseExactly",
      "value": 1
    }
  },
  "staticAbilities": [],
  "toughness": null,
  "triggeredAbilities": [],
  "typeLine": {
    "subtypes": [],
    "supertypes": [],
    "types": [
      {
        "type": "Instant"
      }
    ]
  }
}
```

Register `fireblastPrinting` in `source/test-suite/Pawl/Cards.hs` (record field, `loadPrinting "fireblast"`, record build, `allPrintings`). No deck.

- [x] **Step 9: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS.

If the headline case's life assertion disagrees, **do not weaken it** — read what `S.identityAnswer`'s `ChooseTargets` picked (`Map.mapMaybe Set.lookupMin`, and `Recipient.ToCreature` sorts before `Recipient.ToPlayer`, with `alice = MkPlayerId 0` before `bob`) and correct the expected value to what the rules say that target's life should be, stating the correction in the completion note.

- [x] **Step 10: Format, lint, and commit**

```bash
git add source/library/Pawl source/test-suite/Pawl source/benchmark/Main.hs data/cards/fireblast.json
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p8): printed alternative costs, and Fireblast

Card.alternativeCosts, Cost.costsFor (printed candidate first, each
alternative carrying the card's additional costs per CR 118.9d), and
Prompt.ChooseCost, offered only when two or more candidates are payable
(CR 118.9b) and never combined (CR 118.9a).

Two TAPPED Mountains and an empty mana pool cast a {4}{R}{R} spell that
deals 4: an alternative cost is not a different mana cost, and
castability is not mana affordability.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 6: The two cross-checks — Blood Moon and Thalia against Fireblast

Tests only. Both cross a P8 mechanism with an already-landed one: `PermanentOfSubtype` against the layer system (P3a/M3c's layer 4), and the alternative's `Just []` against P7's cost modification. If either fails, it is a bug in Tasks 4–5, not a plan bug — fix the engine, not the test.

**Files:**
- Test: `source/test-suite/Pawl/CostSpec.hs`

**Interfaces:**
- Consumes: everything from Tasks 1–5. Produces nothing new.

- [x] **Step 1: Write the failing tests**

Add the group and register it in `tests`:

```haskell
-- The two cross-checks: Fireblast's alternative cost against the projection
-- (Blood Moon, CR 613 layer 4) and against P7's cost modification (Thalia, CR
-- 118.9d).
crossCheckTests :: Cards.Cards -> Tasty.TestTree
crossCheckTests cards =
  let withPriority gs =
        gs
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
   in Tasty.testGroup
        "CrossChecks"
        [ -- Blood Moon: "Nonbasic lands are Mountains." Evolving Wilds is a
          -- nonbasic land, so layer 4 makes it a Mountain and it may be
          -- sacrificed to Fireblast's alternative. The pair is what
          -- discriminates: WITHOUT Blood Moon the same board has one Mountain
          -- and the spell is not castable.
          --
          -- Blood Moon affects only NONBASIC lands, which is why the second
          -- permanent is Evolving Wilds and not an Island.
          HU.testCase "CR 613.1d PermanentOfSubtype reads the projection, not the printed type line" $
            let base = S.landsInPlay (Cards.mountainPrinting cards) 1
                (wilds, gs1) = S.addCreature (Cards.evolvingWildsPrinting cards) S.alice base
                (fireblast, gs2) = S.addHandCard (Cards.fireblastPrinting cards) S.alice gs1
                withoutMoon = withPriority gs2
                (_, gs3) = S.addCreature (Cards.bloodMoonPrinting cards) S.alice gs2
                withMoon = withPriority gs3
                cast = S.runPure S.identityAnswer withMoon (Cast.castSpell S.alice fireblast)
             in do
                  HU.assertBool
                    "without Blood Moon, Evolving Wilds is not a Mountain and one Mountain is not two"
                    (not (Cast.castable S.alice fireblast withoutMoon))
                  HU.assertBool "with Blood Moon it is castable" (Cast.castable S.alice fireblast withMoon)
                  HU.assertBool "and Evolving Wilds was sacrificed as a Mountain" (not (Set.member wilds (GameState.battlefield cast))),
          -- CR 118.9d: "If an alternative cost is being paid to cast a spell,
          -- any additional costs, cost increases, and cost reductions that
          -- affect that spell are applied to that alternative cost." Fireblast
          -- is an instant, so Thalia's noncreature tax reaches it, and the
          -- alternative's ABSENT mana component is a real, taxable {0} raised to
          -- {1}. This is the test that requires Just [] rather than Nothing.
          HU.testCase "CR 118.9d Thalia raises the alternative cost's {0} to {1}" $
            let tapAll gs = List.foldl' (flip S.tapObject) gs (Set.toList (GameState.battlefield gs))
                twoTapped = tapAll (S.landsInPlay (Cards.mountainPrinting cards) 2)
                (_, taxedTwo) = S.addCreature (Cards.thaliaPrinting cards) S.alice twoTapped
                (fireblastTwo, gsTwo) = S.addHandCard (Cards.fireblastPrinting cards) S.alice taxedTwo
                -- The same board plus one UNTAPPED Mountain, which can pay the {1}.
                (_, threeMountains) = S.addCreature (Cards.mountainPrinting cards) S.alice twoTapped
                (_, taxedThree) = S.addCreature (Cards.thaliaPrinting cards) S.alice threeMountains
                (fireblastThree, gsThree) = S.addHandCard (Cards.fireblastPrinting cards) S.alice taxedThree
                alternativeOf oid gs = case Cost.costsFor oid gs of
                  _ : alt : _ -> Just (Cost.Type.mana (Cost.total S.alice oid alt gs))
                  _ -> Nothing
             in do
                  HU.assertEqual
                    "the alternative's {0} is taxed to {1}"
                    (Just (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])))
                    (alternativeOf fireblastTwo (withPriority gsTwo))
                  HU.assertBool
                    "with nothing untapped the taxed alternative is unpayable, so Fireblast is not castable"
                    (not (Cast.castable S.alice fireblastTwo (withPriority gsTwo)))
                  HU.assertBool
                    "a third, untapped Mountain pays the {1} and it is castable again"
                    (Cast.castable S.alice fireblastThree (withPriority gsThree))
        ]
```

- [x] **Step 2: Run the tests**

Run: `cabal test`
Expected: PASS, if Tasks 4 and 5 are correct. A failure here is an **engine** bug: fix `Pawl.Cost` (or whatever the failure names) and say so in the completion note; never adjust an assertion to match the code.

- [x] **Step 3: Format, lint, and commit**

```bash
git add source/test-suite/Pawl/CostSpec.hs
hooky fix
git add -u
hooky run
git commit -m "test(m4.5-p8): Fireblast crossed with Blood Moon and with Thalia

PermanentOfSubtype reads the projection -- Blood Moon makes Evolving
Wilds a Mountain and it may be sacrificed as one, while the same board
without Blood Moon cannot cast the spell at all. And CR 118.9d: Thalia
taxes the alternative cost, so its absent mana component has to be a
real, taxable {0} (Just []) and not an unpayable Nothing.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 7: Close — issues filed and cited, docs updated, exit criterion verified

Every `(#N)` placeholder the earlier tasks left in the code is replaced here with a real issue number. **`grep -rn '(#N)' source/` must return nothing when this task is done.**

**Files:**
- Modify: every source file carrying a `(#N)` placeholder
- Modify: `docs/progress.md`, `CLAUDE.md`, `docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`

- [x] **Step 1: File the new issues**

Run each `gh issue create` and record the number it prints. Labels come from CLAUDE.md's set: `elision`, `gap`, `rules-correctness`, `bug`, `expires:milestone`, `expires:card-driven`. The first twelve rows are the spec's §8 table; the last two are this plan's own findings.

| # | Title | Labels | Body must carry |
|---|---|---|---|
| A | Flashback and casting from the graveyard have no carrier | `gap`, `expires:card-driven` | CR 702.34a is two static abilities, only one of which is a cost: it also needs a `CastFromGraveyard` permission and an "exile it instead of putting it anywhere else it would leave the stack" replacement on P5's machinery. `Card.alternativeCosts` covers only the cost half. Expires on a flashback card (Deep Analysis, Firebolt). |
| B | Which cost was paid is not recorded | `gap`, `expires:card-driven` | Nothing downstream reads it — Fireblast's alternative has no payoff and no replacement — so `Cast.castSpell` discards the chosen `Cost` after paying it. Adjacent to #94. Expires on the same flashback card, which needs "if the flashback cost was paid". |
| C | Optional additional costs and kicker (CR 118.8a–b) are unrepresentable | `gap`, `expires:card-driven` | CR 118.8a lets a controller announce "any or all" of a spell's additional costs; `Pawl.Cost.costsFor` announces one whole candidate cost, never a subset of optional components, and `CostComponent` has no optionality. Expires on a kicker or buyback card. |
| D | Effect-granted alternative costs have no carrier | `gap`, `expires:card-driven` | `Card.alternativeCosts` is printed-only. "You may cast this without paying its mana cost" granted by another effect is a P7-shaped player permission with no producer. CR 118.6a's second sentence is the rule it would exercise. Expires on an Omniscience-adjacent card. |
| E | CR 118.10: two components of one cost may count the same permanent | `rules-correctness`, `expires:card-driven` | "Each payment of a cost applies to only one spell, ability, or effect." `Pawl.Cost.canPay` checks each component's payability independently, so a cost with two `Sacrifice` components could count one permanent twice. No card in the pool has two object-consuming components. Expires on one that does. |
| F | CR 601.2h's player-chosen payment order is elided | `elision`, `expires:card-driven` | `Pawl.Cost.pay` fixes the order as mana-then-printed-order. CR 601.2g puts the mana window first regardless, and no component in this vocabulary changes another's payability, so the order is unobservable. Expires on a component whose payability depends on another's completion (a `{T}` of another permanent). |
| G | CR 118.13 hybrid and Phyrexian symbol choices are unrepresentable | `gap`, `expires:card-driven` | `ManaSymbol` has three inhabitants and none is payable two ways, so CR 118.13a's announcement-time choice has nothing to choose. Expires on a hybrid or Phyrexian card. |
| H | CR 118.12 costs paid during resolution have no seam | `gap`, `expires:card-driven` | "[Do something]. If [a player] [does/doesn't/can't], [effect]" makes the action a cost paid **when the spell resolves**, which is not an announcement cost and does not travel through `Pawl.Cost.pay`. Expires on an unless- or if-you-do card. |
| I | Discard-as-cost and exile-from-zone components do not exist | `gap`, `expires:card-driven` | VOCAB on the axis P8 built: each is one `CostComponent` constructor, one `canPayComponent` arm and one `payComponent` arm. CR 601.2f names discarding cards explicitly. Expires on the card that prints one. |
| J | A variable (`Quantity`) life payment is unrepresentable | `gap`, `expires:card-driven` | `CostComponent.PayLife` takes a `Natural`. A `Quantity` needs a binding environment, which CR 601.2f runs before. Expires on a "pay X life" card. |
| K | CR 118.6a's alternative-cost-on-an-unpayable-cost path has no producer | `gap`, `expires:card-driven` | The mechanism is built — each alternative carries its own mana part, independent of the printed one — but no card in the pool has both no mana cost and an alternative cost. Expires on Ancestral Vision or Evermind. |
| L | CR 118.8c's hidden-zone "if able" exception is not implemented | `gap`, `expires:card-driven` | No effect instructs a player to cast a spell "if able", so the exception for a mandatory additional cost involving cards of a stated quality in a hidden zone has nothing to except. Expires on such an effect. |
| M | `PermanentCriterion` is matched at two sites | `elision`, `expires:milestone` | `Pawl.Cost.matchesCriterion` and `Pawl.Replacement.matchesPermanent` both case on the family. Sharing one would make `Pawl.Cost` import `Pawl.Replacement`, which becomes a module cycle the moment #72's CR 614.12b payable-cost check lands there. Four lines, the same shape `genericOf` already duplicates between `Pawl.Mana` and `Pawl.Cost`. Expires on **P9**, whose filter language merges `PermanentCriterion`, `CardCriterion` and `SpellCriterion` (siblings of #38/#39/#40). |
| N | `Prompt.ChooseSacrifices` cannot express CR 118.10's per-component scoping | `gap`, `expires:card-driven` | The prompt offers the candidates for **one** component and is issued once per component, so two components of one cost each see the full candidate list. The payload would need to carry what an earlier component already consumed. The prompt-side face of E. Expires with E. |

- [x] **Step 2: Comment on #90 rather than re-filing it**

Departure 1 changes what #90 says. Add a comment:

```bash
gh issue comment 90 --body "P8 deliberately did NOT give this a door. Routing an activated ability's cost through Pawl.Cost.total would be a rules REGRESSION, not a no-op: PlayerEffect.matchesSpell classifies an OBJECT, not a spell (SpellCriterion.NoncreatureSpell is 'not a creature card type on the projection'), so a noncreature PERMANENT matches it -- and Thalia, Guardian of Thraben would tax Mindslaver's {4} activation to {5}. Thalia taxes noncreature SPELLS. Pawl.Activate therefore calls Cost.canPay/Cost.pay on the ability's PRINTED cost, and Pawl.CostSpec pins the regression with a Thalia x Mindslaver case. Fixing this needs a spell-vs-ability discriminator on the criterion side (P7's surface), not a call-site change."
```

- [x] **Step 3: Sweep the `(#N)` placeholders**

Run: `grep -rn '(#N)' source/`

Replace each hit with the matching issue number, keeping the comment to what is *not* implemented and **never writing an expiry trigger into the comment** (that lives in the issue):

| File | Placeholder | Issue |
|---|---|---|
| `Pawl/Cost.hs`, `pay`'s comment | the fixed payment order | F |
| `Pawl/Cost.hs`, `canPayComponent`'s `Sacrifice` arm | CR 118.10 across two components | E |
| `Pawl/Cost.hs`, `matchesCriterion`'s comment | the two matching sites | M |
| `Pawl/Replacement.hs`, `matchesPermanent`'s new arm | the same, from the other side | M |
| `Pawl/Type/CostComponent.hs`, on `PayLife` | the `Natural`, not a `Quantity` | J |
| `Pawl/Type/Card.hs`, on `alternativeCosts` | printed-only, no granting effect | D |

Add citations at three more sites this plan does not pre-place — write them now:

- `Pawl/Type/Prompt.hs`, on `ChooseSacrifices`: the prompt is per-component and cannot express what an earlier component consumed (N).
- `Pawl/Cast.hs`, in `castSpell` where `chosenCost` is discarded after payment: which cost was paid is not recorded (B).
- `Pawl/Type/CostComponent.hs`, on the type's own comment: discard- and exile-as-cost components do not exist (I).

Confirm (do not re-file): `#89` already covers the hand-projection reading in `castSpell` and its comment is still accurate; `#94`, `#91`, `#88`, `#56` and `#12` are untouched and still open; `#38`/`#39`/`#40` are cited by M and are **not** retired.

Run: `grep -rn '(#N)' source/`
Expected: no output.

- [x] **Step 4: Verify the exit criterion mechanically**

Run each and confirm the expected result:

```bash
# The FIRST INVARIANT's audit: Pawl.Cost is the sole rules home of casing on this
# axis. Pawl.Type.* modules only declare, Pawl.Codec only encodes.
grep -rln 'CostComponent\.\(TapThis\|SacrificeThis\|PayLife\|Sacrifice\)' source/library/ \
  | grep -v 'Pawl/Cost.hs\|Pawl/Codec.hs\|Pawl/Type/'                  # no output

# The projection is read from and never edited: a cost is not a characteristic.
# HEAD~6 is the commit before Task 1 (six task commits are in at this point).
git diff --stat HEAD~6 -- source/library/Pawl/Projection.hs source/library/Pawl/Type/Layer.hs \
  source/library/Pawl/Type/Modification.hs source/library/Pawl/Sba.hs \
  source/library/Pawl/PlayerEffect.hs                                   # no output

# Pawl.Mana keeps pools, production and spending: only its callers changed.
git diff --stat HEAD~6 -- source/library/Pawl/Mana.hs                   # no output

# The retired types are gone, name and all.
grep -rn 'AbilityCost\|AdditionalCost\|TapSelf\|SacrificeSelf' source/ data/   # no output

# Every ability-bearing card file states its mana part (CR 118.6's footgun).
grep -rn '"mana": null' data/                                           # no output

grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-22-p8-cost-generalization.md   # counts down to 0
cabal clean && cabal build all --enable-tests --enable-benchmarks       # warning-free
cabal test                                                              # all green
git add -A && hooky run                                                 # passes
cabal bench                                                             # three timings, no large regression
```

`HEAD~6` assumes the six task commits above are the only commits since Task 1 began; if a fix-up commit landed in between, adjust the ref to the commit before Task 1.

**Watch the benchmark.** `Cast.castable` now walks `Cost.costsFor` per card in hand and runs `Cost.canPay` per candidate, and `Cost.canPay` calls `Mana.canPay` exactly where it did before. The added work is a list build and a `null`-components `all`, so the move should be within the suite's own run-to-run noise; if `cabal bench` shows more, say so plainly in the completion note rather than rounding it away. `#66` still makes all three benchmarks execute the identical game, so the aggregate is the only honest reading.

- [x] **Step 5: Append the `docs/progress.md` completion entry**

One entry, in the file's established voice, recording what P8 *established* — not what is left. It must state:

- **the three gate cards and what each falsified** — Greed: CR 118.3's own worked example, a payability check that ignores the amount passes at 20 life and fails at 1, and at 2 life the payment is legal and *loses the game* (CR 704.5a), which is what proves paying life is a real life-total change; Village Rites: CR 601.2f puts an additional cost **inside** the total cost, so with no creature the spell is not castable at all, and the payment runs through `Event.sacrifice` so Khabál Ghoul counts it; Fireblast: two **tapped** Mountains and an empty pool cast a `{4}{R}{R}` spell that deals 4 — "castability is mana affordability" and "an alternative cost is a different `ManaCost`" both die at once;
- **the structural fact the phase rests on** — CR 118.6's unpayable/`{0}` distinction is carried in the type, and every existing ability migrated `Nothing → Just (MkManaCost [])` in the card files and in the fixtures. Name the two cross-checks that cash it: Blood Moon proves `PermanentOfSubtype` reads the projection, and Thalia proves the alternative's `Just []` is a real, taxable `{0}` (CR 118.9d);
- **the correction to the spec** — §2.9's "ability costs route through `Cost.total` too, nothing changes observably" is false. `PlayerEffect.matchesSpell` classifies an object, so Thalia would tax Mindslaver's activation. Ability costs are paid at their printed value, #90 was commented rather than closed, and a regression test pins it;
- **what was added** — `Pawl.Type.Cost`, `Pawl.Type.CostComponent`, `Pawl.Type.Payment`; `Pawl.Cost`'s `costsFor`, `total` (now `Cost -> Cost`), `canPay`, `canPayComponent`, `pay`, `payComponents`, `payComponent`, `requiresTapSymbol`, `substituteX`, `hasVariable`, `matchesCriterion`, `sacrificeCandidates`, `unpayable`, `firstOffered`; `Card.additionalCosts` and `Card.alternativeCosts`; `PermanentCriterion.PermanentOfSubtype`; `Prompt.ChooseCost`/`ChooseSacrifices` and their responses; `Pawl.Cast.payableCost`; three card files; `Pawl.CostSpec`;
- **what was retired** — `Pawl.Type.AbilityCost`, `Pawl.Type.AdditionalCost`, `Pawl.Cast.costOf`, `Pawl.Activate.canPayAdditional`/`payAdditional`;
- **the six departures** listed at the top of this plan, with their reasons — especially that `Cost.pay` is transactional because a rejected `ChooseSacrifices` would otherwise leave mana spent and a creature dead;
- **two prompts were added, each elided only when forced** — `ChooseCost` when one candidate is payable, `ChooseSacrifices` when the candidates exactly equal the count; three payable Mountains and a count of two is a real choice and is asked. The one genuine elision is CR 601.2h's payment order (issue F);
- **tracking** — closes **#4** (M4.5 P8) and the *payment* half of GAP-Co, which P7's spec left explicitly open; #90 commented, not closed; #38/#39/#40 cited and not retired; fourteen new issues filed (A–N above) with their real numbers;
- the final suite count, that the build is warning-clean on a from-scratch `cabal clean` build, and the benchmark comparison with `#66` noted;
- the spec and plan paths, kept as reference.

- [x] **Step 6: Replace the `CLAUDE.md` status bullet**

**Replace, never append** — milestone history goes in `progress.md`. The new bullet says M0–M4h plus M4.5 P1–P8 are complete, that P8 closed the *payment* half of **GAP-Co** with Greed, Village Rites and Fireblast, and that **P9 (filters), P10 (player counters) and P11 (Command zone) remain, none blocking another**. Keep it to the same length as the bullet it replaces.

- [x] **Step 7: Update the umbrella spec**

`docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`:
- §3's P8 row (line 111): mark it landed, with a pointer to `docs/superpowers/specs/2026-07-22-p8-cost-generalization-design.md`. **Correct the row's own gate-card guess** — it proposed "a pay-life ability + a flashback card"; flashback needs a `CastFromGraveyard` permission and a P5 replacement as well as a cost, so the alternative-cost seam landed on **Fireblast** instead, and flashback is deferred (issue A).
- §4's ordering paragraph (line 219): P8 landed; **P9, P10 and P11 remain**, none blocking another.
- The "Notes the phase specs must not lose" bullet on GAP-Co (§3, "GAP-Co is split across two phases"): record that **both halves are now closed** — modification by P7, payment by P8.

- [x] **Step 8: Commit**

```bash
git add docs/progress.md CLAUDE.md docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md source/library/Pawl
hooky fix
git add -u
hooky run
git commit -m "docs(m4.5-p8): completion note, umbrella tick, CLAUDE.md status

Fourteen deferrals filed as issues and cited at their code sites; #4 and
the payment half of GAP-Co closed, #90 commented rather than closed (an
ability cost is deliberately not routed through Cost.total), and
#38/#39/#40 cited not retired. The umbrella's P8 gate-card guess is
corrected: flashback needs a permission and a P5 replacement as well as
a cost, so the alternative-cost seam landed on Fireblast.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

- [x] **Step 9: Close the milestone issue**

```bash
gh issue close 4 --comment "Landed. See docs/progress.md for the completion entry and docs/superpowers/plans/2026-07-22-p8-cost-generalization.md for the executed plan."
```

- [x] **Step 10: Confirm the plan is complete**

Run: `grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-22-p8-cost-generalization.md`
Expected: `0`.

---

## Spec coverage map

| Spec section | Where it lands |
|---|---|
| §0 the five falsifiers | Greed at 1 life → Task 3; Village Rites with no creature → Task 4; Fireblast with two tapped Mountains → Task 5; Fireblast under Thalia → Task 6; Ancestral Vision ≠ Ornithopter → Task 1 (the type) and Task 2 (`canPay`'s `Nothing` arm) |
| §2.1 one `Cost` for spells and abilities; `AbilityCost` retired | Task 1 |
| §2.2 `CostComponent`'s four inhabitants; why `SacrificeThis` is not `Sacrifice 1 this`; why `PayLife` takes a `Natural` | Task 1 (two), Task 3 (`PayLife`), Task 4 (`Sacrifice`) |
| §2.3 the CR 118.6 unpayable/`{0}` distinction, and the `Nothing → Just []` migration | Task 1 (type, codec, seven card files, six fixture files), Task 2 (`canPay`'s `Nothing` arm, the CR 118.6a `total` test) |
| §2.4 `costsFor` and CR 118.9d; `Card`'s two new fields | Task 2 (printed only), Task 4 (`additionalCosts`), Task 5 (`alternativeCosts`, the CR 118.9d fold) |
| §2.5 `total` over the new shape; P7's `applyAdjustments` untouched | Task 2, verified by Task 7 Step 4's `PlayerEffect.hs` diff check |
| §2.6 payability, CR 118.3, and the four-row table | Task 2 (`TapThis`, `SacrificeThis`), Task 3 (`PayLife`), Task 4 (`Sacrifice`) |
| §2.6 criteria matched through the projection | Task 4 (`matchesCriterion`), Task 6 (Blood Moon proves it) |
| §2.7 payment order, the sacrifice funnel, life as a direct subtraction | Task 2 (`pay`'s mana-then-components order), Task 3 (life), Task 4 (`Event.sacrifice` and the Khabál Ghoul test) |
| §2.8 the two prompts, their elisions, and reject-not-repair | Task 4 (`ChooseSacrifices`), Task 5 (`ChooseCost`); the transactional restore is departure 2, Task 2 |
| §2.9 the read sites (`Cast`, `Activate`) | Task 2, with §2.9's ability-cost claim corrected — departure 1 |
| §2.10 `PermanentCriterion` gains one constructor | Task 4 |
| §2.11 serialization, replay, card files | Tasks 1, 3, 4, 5 (codecs and card data); Tasks 4 and 5 (`Replay` arms, fallbacks, eight interpreters) |
| §3 the two invariants | Task 2 Step 8 and Task 7 Step 4's first grep (the casing surface); the no-choice half is Tasks 4 and 5's prompts plus their elision tests |
| §4 what the phase does not touch | Task 7 Step 4's second and third diff checks |
| §5 Greed's four tests | Task 3 |
| §5 Village Rites' four tests | Task 4 |
| §5 Fireblast's five tests | Tasks 5 (three) and 6 (Blood Moon, Thalia) |
| §5 migration coverage | Task 1 Step 9 (`ManaSpec`, `ActivateSpec`, `CodecSpec`, `CardSpec` as tripwires) |
| §5 codec | Tasks 1, 3, 4, 5 (unit arms); the three card files via `CardsSpec.checkFile` as soon as each is registered |
| §6 module and type inventory | Tasks 1, 2, 4, 5 |
| §7 the phase's own ordering | Tasks 1–6, in that order, with §7's step 1 as Task 1, step 2 as Task 2, steps 3–5 as Tasks 3–5, and step 6 as Task 6 |
| §8 the twelve deferrals with named expiries | Task 7 Steps 1 and 3, plus two of this plan's own (M, N) |
| §9 tracking | Task 7 Steps 2 and 5–7 and 9 |
| §10 exit criterion | Task 7 Step 4 |
