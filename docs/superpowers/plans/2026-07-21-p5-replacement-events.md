# M4.5 P5 — Replacement events, and the choice the fold cannot make: Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the engine's four separate, pure, choiceless replacement paths (zone-change fold, prevention filter, regeneration map, as-enters drain) with **one** monadic path over **one** proposed-event vocabulary, driven by CR 616.1's re-collecting loop and a real `ChooseReplacement` prompt.

**Architecture:** `ProposedEvent` names six replaceable *event classes*; `ReplacementEffect` names *(event class, rewrite shape)* pairs with the scoping carried as pattern **data**; a new `Pawl.Replacement` module is the sole home of casing on either, and implements CR 616.1's loop — collect (minus CR 614.5's already-applied set), bucket by 616.1a–e, choose (prompting only when the choice is real), apply, repeat. Every change-and-emit funnel (`changeZone`, `destroy`, `createTokens`, the new `putCounters`, `Damage.applyDamage`) raises its proposed event through that loop, which forces those funnels — and `Sba.performStateBasedActions` above them — to become `Game`-monadic.

**Tech Stack:** Haskell 2010 (GHC 9.14.1 from the Nix flake), `tasty` + `tasty-hunit`, the hand-rolled `Pawl.Json`/`Pawl.Codec` card codec.

**Spec:** `docs/superpowers/specs/2026-07-21-p5-replacement-events-design.md`. Read §2.1–2.3 before Task 1, §2.7 before Task 2, §2.4 before Task 3, §2.6 before Task 4, §2.5 before Tasks 7–8, §8 before Task 10.

## Global Constraints

Every task's requirements implicitly include all of these. They come from `CLAUDE.md` and are not negotiable.

- **Haskell 2010, no language extensions** unless there is no alternative. `NamedFieldPuns` is permitted; `GADTs`/`RankNTypes` only in the suspension core and in test modules that already carry the pragma. Modules that already carry a `{-# LANGUAGE … #-}` pragma keep it.
- **No explicit export lists.** `module Pawl.Foo where`.
- **One type per module** under `Pawl.Type.<TypeName>` (type + instances only). A module never imports its parents; `A.B.C` must not import `A.B` or `A`.
- **Qualified imports aliased to the last component** (`Data.Sequence` → `Seq`, `Pawl.Type.ProposedEvent` → `ProposedEvent`). One import group, alphabetical. The one documented exception is `Pawl.Support` as `S` in the test suite.
- **No partial functions**, written or used. No `head`, `error`, `undefined`, or non-exhaustive matches.
- **`newtype` liberally, non-punning constructors** (`MkFoo`). Build records with `do`/`pure` + record syntax, not `<$>`/`<*>`.
- **Prefer explicit:** `case` over point-free; one equation with a `case` over multiple clauses; `let` over `where`; `$` over parens, `.` over chained `$`. No list comprehensions. No backtick-infixed functions.
- **`Text` not `String`.** Arbitrary-precision numbers (`Integer`, `Natural`).
- **No boolean blindness** — a custom sum type beats a bare `Bool`.
- **Derive at least `Eq` and `Show`.** A type that rides a `Set`/`Map` key, a `Binding`, a `Card`, or `ProjectedCharacteristics` also derives `Ord`.
- **No API stability obligations.** Rename, reshape, and delete freely; never add a compat shim or keep an old name.
- **Every rules claim is checked against `docs/rules.txt`** and the rule number is cited in the code comment. Never trust recalled Magic rules.
- **Build must be warning-clean.** `cabal build all --enable-tests --enable-benchmarks` with `flags: +pedantic` (which is `-Werror`). Incremental builds hide warnings from unchanged modules; `cabal clean` first when a definitive check is needed.
- **Import lists are not spelled out** in every snippet below. When a step's code names `Monad.void`/`Monad.when`/`Monad.replicateM`, `Maybe.isJust`/`Maybe.catMaybes`, `Set.*`, `List.*`, `Ord.Down` or a `Pawl.Type.*` module, add the qualified import (aliased to the last component, one alphabetical group). GHC names every missing one.
- **New library modules** go under `source/library/` and are picked up by the `-- cabal-gild: discover` directive — add the file and run `hooky fix`; never hand-edit `exposed-modules`. **New test modules must be added to the test-suite `other-modules` list** by the same `cabal-gild` discovery, and wired into `source/test-suite/Main.hs`'s `testTree`.
- **Before every commit:** `git add <the paths this task names>`, then `hooky fix`, then `git add` again, then `hooky run`. `hooky` acts on **staged** files only; if you skip the `git add`, it reports "hooks skipped" and checks nothing.
- **TDD is not optional.** Write the failing test and actually run it to watch it fail before implementing.
- **Never edit this plan, weaken an assertion, or delete a test to make a check pass.** If the plan looks wrong, stop and say so.
- **One small complete commit per task, on `main`.** Commit messages end with the `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>` trailer.
- **Concurrent sessions share this checkout.** Stage the explicit paths each task names; do not blanket-stage foreign files that appear in `git status`.

## Card verification (already done — do not re-fetch)

All four new gate cards were verified live against the Scryfall API while this plan was written. Use these values verbatim; no network access is needed during execution.

| Card | Cost | Type line | P/T | Oracle text |
|---|---|---|---|---|
| Hardened Scales | `{G}` | Enchantment | — | "If one or more +1/+1 counters would be put on a creature you control, that many plus one +1/+1 counters are put on it instead." |
| Corpsejack Menace | `{2}{B}{G}` | Creature — Fungus | 4/4 | "If one or more +1/+1 counters would be put on a creature you control, twice that many +1/+1 counters are put on it instead." |
| Doubling Season | `{4}{G}` | Enchantment | — | "If an effect would create one or more tokens under your control, it creates twice that many of those tokens instead. / If an effect would put one or more counters on a permanent you control, it puts twice that many of those counters on that permanent instead." |
| Primal Plasma | `{3}{U}` | Creature — Elemental Shapeshifter | `*`/`*` | "As this creature enters, it becomes your choice of a 3/3 creature, a 2/2 creature with flying, or a 1/6 creature with defender." |

Note **Corpsejack Menace is `Creature — Fungus`** (one subtype, not two) and **4/4**.

None of these four is added to any deck in `Pawl.Cards`. They are deterministic fixtures, like Clone and Tarmogoyf — adding them would perturb `PropertySpec`'s card-backed conservation counts for no gain.

## Three deliberate departures from the spec

State these in the completion note (Task 10). They are corrections, not drift.

1. **CR 614.5's identity is `(source, effect value)`, not `(source, index)`.** The spec's §2.4 step 1 says "(source, index)". Index identity makes the phase's own centerpiece unreachable: a Clone applies its `EntryR AsCopy` (index 0 of its one-element list), which replaces its copiable snapshot with a Primal Plasma's — whose `EntryR (ChoiceOf …)` is then *also* index 0 of the new list. The already-applied set would swallow the newly-acquired ability and CR 616.2 would never fire. Value identity keeps every §5 scenario correct (two Hardened Scales are two *sources*; Doubling Season's two clauses are two *values*). Its only cost — a single source with two textually identical replacement abilities gets one opportunity instead of two — has no producer and gets an issue in Task 10.
2. **The data-file migrations land with the opcode that needs them, not all in Task 1.** `rest-in-peace.json` migrates in Task 1 (the `ReplacementEffect` reshape forces it); `fog.json` in Task 4, `drudge-skeletons.json` in Task 5, `clone.json` in Task 7 — each with the task that introduces `Effect.Replace` / `WouldBeDestroyed` / `EntryR AsCopy`. Every task therefore ends with a green suite, which §7's "every task is one commit that leaves the suite green" requires.
3. **Four internal types the spec's §6 inventory does not list** — `Pawl.Type.ProposedEvent` is listed, but the loop also needs `Pawl.Type.CandidateId`, `Pawl.Type.ReplacementCandidate` and `Pawl.Type.ReplacementBucket`. They land in Task 3, under the one-type-per-module rule, rather than as anonymous tuples inside `Pawl.Replacement`.

## File structure

**New library modules.** All under `source/library/`.

| Module | Responsibility | Task |
|---|---|---|
| `Pawl/Type/ControllerRelation.hs` | `Yours` / `Anyones`, relative to the effect source's controller | 1 |
| `Pawl/Type/PermanentCriterion.hs` | `AnyPermanent` / `CreaturePermanent` (P9 absorbs it) | 1 |
| `Pawl/Type/ZoneChangePattern.hs` | which zone changes a redirect intercepts | 1 |
| `Pawl/Type/CounterPattern.hs` | which counter placements a scaling replacement intercepts | 1 |
| `Pawl/Type/TokenPattern.hs` | which token creations a scaling replacement intercepts | 1 |
| `Pawl/Type/DamagePattern.hs` | which damage events a prevention intercepts | 1 |
| `Pawl/Type/Scaling.hs` | `Multiply` / `AddMore` — the count rewrite | 1 |
| `Pawl/Type/EntryOption.hs` | one shape an "as it enters" choice offers (P/T + keywords) | 1 |
| `Pawl/Type/EntryRewrite.hs` | `AsCopy` / `ChoiceOf [EntryOption]` | 1 |
| `Pawl/Type/DamageRewrite.hs` | `PreventAll` | 1 |
| `Pawl/Type/DestructionRewrite.hs` | `Regenerate` | 1 |
| `Pawl/Type/ProposedEvent.hs` | the six replaceable event classes | 3 |
| `Pawl/Type/CandidateId.hs` | CR 614.5's one-opportunity identity | 3 |
| `Pawl/Type/ReplacementCandidate.hs` | one applicable-or-not effect instance | 3 |
| `Pawl/Type/ReplacementBucket.hs` | CR 616.1a–e, `Ord`-ascending | 3 |
| `Pawl/Replacement.hs` | **the CR 616.1 loop** — sole home of casing on `ProposedEvent` and `ReplacementEffect` | 3 |
| `Pawl/Type/Uses.hs` | `Unlimited` / `Once` (CR 614.3) | 4 |
| `Pawl/Type/ActiveReplacement.hs` | a floating replacement: effect + source + timestamp + duration + uses | 4 |

**Deleted library modules.** `Pawl/Type/Prevention.hs` and `Pawl/Type/ActivePrevention.hs` (Task 4).

**New test module.** `source/test-suite/Pawl/ReplacementSpec.hs` — near-mirrors `Pawl.Replacement`; holds §5 scenarios 1–16 and 18–20 and 22. Scenario 17 (Rest in Peace) stays in `EventSpec` and scenario 21 (Fog) stays in `ResolveSpec`/`DamageSpec` as regressions on the new path.

**New card data.** `data/cards/hardened-scales.json` and `corpsejack-menace.json` (Task 6), `primal-plasma.json` (Task 8), `doubling-season.json` (Task 9).

**Reshaped card data.** `rest-in-peace.json` (Task 1), `fog.json` (Task 4), `drudge-skeletons.json` (Task 5), `clone.json` (Task 7).

---

### Task 1: The replacement vocabulary — types, codec, and the Rest in Peace migration

`ReplacementEffect` grows from one card-shaped constructor into six event-class arms with pattern data. The existing pure zone-change fold is adapted to the new shape and stays pure; no CR 616 loop yet.

**Files:**
- Create: `source/library/Pawl/Type/ControllerRelation.hs`, `PermanentCriterion.hs`, `ZoneChangePattern.hs`, `CounterPattern.hs`, `TokenPattern.hs`, `DamagePattern.hs`, `Scaling.hs`, `EntryOption.hs`, `EntryRewrite.hs`, `DamageRewrite.hs`, `DestructionRewrite.hs` (all under `source/library/Pawl/Type/`)
- Modify: `source/library/Pawl/Type/ReplacementEffect.hs` (whole file)
- Modify: `source/library/Pawl/Codec.hs` (add 11 codec pairs; replace `replacementEffectToJson`/`jsonToReplacementEffect` at lines 954–963)
- Modify: `source/library/Pawl/Projection.hs:646-654` (`replacementsAffecting` returns pairs)
- Modify: `source/library/Pawl/Event.hs:171` and `:296-309` (`applyReplacements`/`applyOne`)
- Modify: `data/cards/rest-in-peace.json`
- Test: `source/test-suite/Pawl/EventSpec.hs:31-43`, `source/test-suite/Pawl/CodecSpec.hs:178`, `source/test-suite/Pawl/ProjectionSpec.hs:387`

**Interfaces:**
- Produces: `ReplacementEffect = ZoneChangeR ZoneChangePattern Zone | EntryR EntryRewrite | DamageR DamagePattern DamageRewrite | DestructionR DestructionRewrite | CounterR CounterPattern Scaling | TokenR TokenPattern Scaling`, deriving `(Eq, Ord, Show)`; `Projection.replacementsAffecting :: GameState -> [(ObjectId, ReplacementEffect)]`; codec pairs `<x>ToJson`/`jsonTo<X>` for all 11 new types.

- [x] **Step 1: Write the failing codec round-trip test**

In `source/test-suite/Pawl/CodecSpec.hs`, replace the `RedirectZoneChange` round-trip at line 178 with this one (keep the surrounding `testCase` scaffolding style used by its neighbours; add `import qualified Pawl.Type.ControllerRelation as ControllerRelation`, `CounterPattern`, `CounterKind`, `EntryOption`, `EntryRewrite`, `Keyword`, `PermanentCriterion`, `Scaling`, `ZoneChangePattern` as needed):

```haskell
      HU.testCase "a ZoneChangeR replacement round-trips" $
        let re =
              ReplacementEffect.ZoneChangeR
                ZoneChangePattern.MkZoneChangePattern
                  { ZoneChangePattern.whenDestination = Zone.Graveyard,
                    ZoneChangePattern.whoseObject = ControllerRelation.Anyones
                  }
                Zone.Exile
         in HU.assertEqual "preserved" (Right re) (Codec.jsonToReplacementEffect (Codec.replacementEffectToJson re)),
      HU.testCase "a CounterR replacement round-trips (pattern and scaling are data)" $
        let re =
              ReplacementEffect.CounterR
                CounterPattern.MkCounterPattern
                  { CounterPattern.whichKind = Just CounterKind.PlusOnePlusOne,
                    CounterPattern.whose = ControllerRelation.Yours,
                    CounterPattern.onWhat = PermanentCriterion.CreaturePermanent
                  }
                (Scaling.AddMore 1)
         in HU.assertEqual "preserved" (Right re) (Codec.jsonToReplacementEffect (Codec.replacementEffectToJson re)),
      HU.testCase "an EntryR ChoiceOf replacement round-trips (P/T and keywords)" $
        let re =
              ReplacementEffect.EntryR
                ( EntryRewrite.ChoiceOf
                    [ EntryOption.MkEntryOption {EntryOption.power = 3, EntryOption.toughness = 3, EntryOption.keywords = Set.empty},
                      EntryOption.MkEntryOption {EntryOption.power = 1, EntryOption.toughness = 6, EntryOption.keywords = Set.singleton Keyword.Defender}
                    ]
                )
         in HU.assertEqual "preserved" (Right re) (Codec.jsonToReplacementEffect (Codec.replacementEffectToJson re)),
```

- [x] **Step 2: Run it to watch it fail**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -30`
Expected: FAIL to compile — `Not in scope: data constructor 'ReplacementEffect.ZoneChangeR'` (and the new `Pawl.Type.*` modules are missing).

- [x] **Step 3: Add the eleven leaf type modules**

`source/library/Pawl/Type/ControllerRelation.hs`:

```haskell
module Pawl.Type.ControllerRelation where

-- CR 614.1 / 109.5: whose object a replacement's pattern admits, relative to the
-- controller of the effect's SOURCE (that is what "you" means on a permanent's
-- static ability). Hardened Scales says "a creature you control" (Yours); Rest in
-- Peace's redirect has no controller clause at all (Anyones).
--
-- P9's filter language absorbs this; it is here so the two gate cards can be
-- distinguished by DATA rather than by a constructor apiece.
data ControllerRelation
  = Yours
  | Anyones
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/PermanentCriterion.hs`:

```haskell
module Pawl.Type.PermanentCriterion where

-- CR 614.1: which permanents a replacement's pattern admits. Hardened Scales
-- scopes to creatures; Doubling Season's counter clause to any permanent.
--
-- The sibling of Pawl.Type.CardCriterion, deliberately NOT merged with it: P9
-- merges both into one filter language, and merging them here would be building
-- half of P9 with one customer.
data PermanentCriterion
  = AnyPermanent
  | CreaturePermanent
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/ZoneChangePattern.hs`:

```haskell
module Pawl.Type.ZoneChangePattern where

import Pawl.Type.ControllerRelation (ControllerRelation)
import Pawl.Type.Zone (Zone)

-- CR 614.1a: which zone changes a redirect intercepts. Rest in Peace is
-- (Graveyard, Anyones) -- any object that would be put into a graveyard from
-- anywhere. `whenDestination` is compared against the event's CURRENT
-- destination, which is why a redirect whose output no longer matches its own
-- trigger destination cannot re-fire even before CR 614.5 is consulted.
data ZoneChangePattern = MkZoneChangePattern
  { whenDestination :: Zone,
    whoseObject :: ControllerRelation
  }
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/CounterPattern.hs`:

```haskell
module Pawl.Type.CounterPattern where

import Pawl.Type.ControllerRelation (ControllerRelation)
import Pawl.Type.CounterKind (CounterKind)
import Pawl.Type.PermanentCriterion (PermanentCriterion)

-- CR 122.6 / 614.1: which counter placements a scaling replacement intercepts.
-- Hardened Scales is (Just PlusOnePlusOne, Yours, CreaturePermanent); Doubling
-- Season's counter clause is (Nothing, Yours, AnyPermanent). `whichKind =
-- Nothing` means ANY kind, never "no kind" -- the two cards differ by data, and
-- neither is a constructor.
data CounterPattern = MkCounterPattern
  { whichKind :: Maybe CounterKind,
    whose :: ControllerRelation,
    onWhat :: PermanentCriterion
  }
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/TokenPattern.hs`:

```haskell
module Pawl.Type.TokenPattern where

import Pawl.Type.ControllerRelation (ControllerRelation)

-- CR 111.1 / 614.1: which token creations a scaling replacement intercepts.
-- Doubling Season's token clause is Yours ("under your control"). No card
-- criterion: nothing in the pool scopes token doubling by what the token IS.
newtype TokenPattern = MkTokenPattern
  { whose :: ControllerRelation
  }
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/DamagePattern.hs`:

```haskell
module Pawl.Type.DamagePattern where

import Pawl.Type.DamageKind (DamageKind)

-- CR 615.1: which damage events a prevention intercepts. Fog is (Just Combat).
-- Nothing means any kind. CR 615's shields that name a SOURCE, an AMOUNT, or a
-- RECIPIENT are P9's to add; this carries the minimum Fog needs.
newtype DamagePattern = MkDamagePattern
  { whichKind :: Maybe DamageKind
  }
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/Scaling.hs`:

```haskell
module Pawl.Type.Scaling where

import Numeric.Natural (Natural)

-- CR 614.1: how a counting replacement rewrites the count. Corpsejack Menace and
-- Doubling Season are Multiply 2 ("twice that many"); Hardened Scales is AddMore
-- 1 ("that many plus one"). The difference between those cards is a NUMBER, which
-- is the whole point of this type existing instead of two effect constructors.
data Scaling
  = Multiply Natural
  | AddMore Natural
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/EntryOption.hs`:

```haskell
module Pawl.Type.EntryOption where

import Data.Set (Set)
import Pawl.Type.Keyword (Keyword)

-- CR 208.2b / 614.1c: one of the shapes an "as this creature enters, it becomes
-- your choice of ..." ability offers. Primal Plasma's three are (3,3,{}),
-- (2,2,{Flying}) and (1,6,{Defender}).
--
-- The keywords are UNIONED into the object's copiable snapshot, never assigned
-- over it. That is pinned by Primal Plasma's own Gatherer ruling: a Clone of a
-- 2/2-flying Plasma that picks the third option is "1/6 with flying AND
-- defender". P/T, by contrast, is SET (CR 707.2's "abilities that set power and
-- toughness").
data EntryOption = MkEntryOption
  { power :: Integer,
    toughness :: Integer,
    keywords :: Set Keyword
  }
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/EntryRewrite.hs`:

```haskell
module Pawl.Type.EntryRewrite where

import Pawl.Type.EntryOption (EntryOption)

-- CR 614.1c: how an "as this permanent enters" replacement modifies the entry.
-- AsCopy is Clone (CR 707.5, and a real "may" -- declining is legal); ChoiceOf
-- is Primal Plasma (CR 208.2b). Both write into the object's COPIABLE snapshot,
-- which is what makes CR 707.2 fall out with no further machinery: the rule says
-- copiable values are the printed values as modified by copy effects and by
-- "as ... enters" abilities that set power and toughness.
data EntryRewrite
  = AsCopy
  | ChoiceOf [EntryOption]
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/DamageRewrite.hs`:

```haskell
module Pawl.Type.DamageRewrite where

-- CR 615.1: how a prevention rewrites a damage event. PreventAll cancels it
-- outright (Fog) -- CR 615.6, a prevented event never happens. CR 615.7's shared
-- N-damage shield and the prevent-the-next-N shape are card-driven, not
-- structure-blocked (#58).
data DamageRewrite = PreventAll
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/DestructionRewrite.hs`:

```haskell
module Pawl.Type.DestructionRewrite where

-- CR 614.8 / 701.19a: how a replacement rewrites a would-be-destroyed event.
-- Regenerate is "instead, tap it, remove all damage from it, and remove it from
-- combat" -- the destruction itself does not happen, so nothing downstream of it
-- (a put-into-graveyard, and therefore Rest in Peace) ever runs.
data DestructionRewrite = Regenerate
  deriving (Eq, Ord, Show)
```

- [x] **Step 4: Reshape `ReplacementEffect`**

Replace the whole of `source/library/Pawl/Type/ReplacementEffect.hs`:

```haskell
module Pawl.Type.ReplacementEffect where

import Pawl.Type.CounterPattern (CounterPattern)
import Pawl.Type.DamagePattern (DamagePattern)
import Pawl.Type.DamageRewrite (DamageRewrite)
import Pawl.Type.DestructionRewrite (DestructionRewrite)
import Pawl.Type.EntryRewrite (EntryRewrite)
import Pawl.Type.Scaling (Scaling)
import Pawl.Type.TokenPattern (TokenPattern)
import Pawl.Type.ZoneChangePattern (ZoneChangePattern)
import Pawl.Type.Zone (Zone)

-- CR 614.1a: a replacement effect, classified by the EVENT CLASS it intercepts
-- and the REWRITE SHAPE it applies. One arm per replaceable event class -- the
-- arm count tracks the ~40 classes the comprehensive rules define, never the card
-- pool. Rest in Peace is DATA (`ZoneChangeR (MkZoneChangePattern Graveyard
-- Anyones) Exile`), not a constructor; so is Fog, so is regeneration, so is
-- Hardened Scales. The scenario the first invariant forbids --
-- `case effect of RedirectZoneChange Graveyard Exile -> restInPeace` -- is no
-- longer expressible.
--
-- A (effect, event) pair whose arms disagree simply does not apply, so the type
-- rules out "redirect a damage event" without a validity pass.
--
-- EntryR and DestructionR carry NO pattern: both are self-only in the pool today
-- (CR 614.1c's "[this permanent] enters as"; CR 201.5/201.5c make "regenerate
-- this creature" name the ability's own source). CR 614.1d's other-objects form
-- ("[Objects] enter the battlefield ...", Essence of the Wild) has no producer,
-- so the field appears when a card needs it rather than as speculative structure.
--
-- Only Pawl.Replacement may case on this for RULES purposes; Pawl.Codec also
-- cases on every constructor, but only as the JSON data boundary.
data ReplacementEffect
  = ZoneChangeR ZoneChangePattern Zone
  | EntryR EntryRewrite
  | DamageR DamagePattern DamageRewrite
  | DestructionR DestructionRewrite
  | CounterR CounterPattern Scaling
  | TokenR TokenPattern Scaling
  deriving (Eq, Ord, Show)
```

- [x] **Step 5: Add the codec pairs**

In `source/library/Pawl/Codec.hs`, add the eleven import lines (alphabetically, alongside the existing `Pawl.Type.*` imports): `ControllerRelation`, `CounterPattern`, `DamagePattern`, `DamageRewrite`, `DestructionRewrite`, `EntryOption`, `EntryRewrite`, `PermanentCriterion`, `Scaling`, `TokenPattern`, `ZoneChangePattern`. Then add, next to the existing `preventionToJson` block:

```haskell
controllerRelationToJson :: ControllerRelation.ControllerRelation -> Value
controllerRelationToJson r = nullary . Text.pack $ case r of
  ControllerRelation.Yours -> "Yours"
  ControllerRelation.Anyones -> "Anyones"

jsonToControllerRelation :: Value -> Either Text ControllerRelation.ControllerRelation
jsonToControllerRelation =
  decodeNullary
    (Text.pack "ControllerRelation")
    [ (Text.pack "Yours", ControllerRelation.Yours),
      (Text.pack "Anyones", ControllerRelation.Anyones)
    ]

permanentCriterionToJson :: PermanentCriterion.PermanentCriterion -> Value
permanentCriterionToJson c = nullary . Text.pack $ case c of
  PermanentCriterion.AnyPermanent -> "AnyPermanent"
  PermanentCriterion.CreaturePermanent -> "CreaturePermanent"

jsonToPermanentCriterion :: Value -> Either Text PermanentCriterion.PermanentCriterion
jsonToPermanentCriterion =
  decodeNullary
    (Text.pack "PermanentCriterion")
    [ (Text.pack "AnyPermanent", PermanentCriterion.AnyPermanent),
      (Text.pack "CreaturePermanent", PermanentCriterion.CreaturePermanent)
    ]

damageRewriteToJson :: DamageRewrite.DamageRewrite -> Value
damageRewriteToJson r = nullary . Text.pack $ case r of
  DamageRewrite.PreventAll -> "PreventAll"

jsonToDamageRewrite :: Value -> Either Text DamageRewrite.DamageRewrite
jsonToDamageRewrite =
  decodeNullary (Text.pack "DamageRewrite") [(Text.pack "PreventAll", DamageRewrite.PreventAll)]

destructionRewriteToJson :: DestructionRewrite.DestructionRewrite -> Value
destructionRewriteToJson r = nullary . Text.pack $ case r of
  DestructionRewrite.Regenerate -> "Regenerate"

jsonToDestructionRewrite :: Value -> Either Text DestructionRewrite.DestructionRewrite
jsonToDestructionRewrite =
  decodeNullary (Text.pack "DestructionRewrite") [(Text.pack "Regenerate", DestructionRewrite.Regenerate)]

scalingToJson :: Scaling.Scaling -> Value
scalingToJson s = case s of
  Scaling.Multiply n -> Json.tagged (Text.pack "Multiply") (Just (natTo n))
  Scaling.AddMore n -> Json.tagged (Text.pack "AddMore") (Just (natTo n))

jsonToScaling :: Value -> Either Text Scaling.Scaling
jsonToScaling value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Multiply", Just v) -> fmap Scaling.Multiply (natFrom v)
    ("AddMore", Just v) -> fmap Scaling.AddMore (natFrom v)
    _ -> Left (Text.pack "unknown Scaling: " <> t)

entryOptionToJson :: EntryOption.EntryOption -> Value
entryOptionToJson o =
  Object
    [ (Text.pack "power", Json.jInt (EntryOption.power o)),
      (Text.pack "toughness", Json.jInt (EntryOption.toughness o)),
      (Text.pack "keywords", setTo keywordToJson (EntryOption.keywords o))
    ]

jsonToEntryOption :: Value -> Either Text EntryOption.EntryOption
jsonToEntryOption value = do
  ps <- Json.asObject value
  p <- Json.field (Text.pack "power") ps >>= Json.asInteger
  t <- Json.field (Text.pack "toughness") ps >>= Json.asInteger
  ks <- Json.field (Text.pack "keywords") ps >>= setFrom jsonToKeyword
  pure
    EntryOption.MkEntryOption
      { EntryOption.power = p,
        EntryOption.toughness = t,
        EntryOption.keywords = ks
      }

entryRewriteToJson :: EntryRewrite.EntryRewrite -> Value
entryRewriteToJson r = case r of
  EntryRewrite.AsCopy -> nullary (Text.pack "AsCopy")
  EntryRewrite.ChoiceOf options -> Json.tagged (Text.pack "ChoiceOf") (Just (listTo entryOptionToJson options))

jsonToEntryRewrite :: Value -> Either Text EntryRewrite.EntryRewrite
jsonToEntryRewrite value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("AsCopy", _) -> Right EntryRewrite.AsCopy
    ("ChoiceOf", Just v) -> fmap EntryRewrite.ChoiceOf (listFrom jsonToEntryOption v)
    _ -> Left (Text.pack "unknown EntryRewrite: " <> t)

zoneChangePatternToJson :: ZoneChangePattern.ZoneChangePattern -> Value
zoneChangePatternToJson p =
  Object
    [ (Text.pack "whenDestination", zoneToJson (ZoneChangePattern.whenDestination p)),
      (Text.pack "whoseObject", controllerRelationToJson (ZoneChangePattern.whoseObject p))
    ]

jsonToZoneChangePattern :: Value -> Either Text ZoneChangePattern.ZoneChangePattern
jsonToZoneChangePattern value = do
  ps <- Json.asObject value
  d <- Json.field (Text.pack "whenDestination") ps >>= jsonToZone
  w <- Json.field (Text.pack "whoseObject") ps >>= jsonToControllerRelation
  pure
    ZoneChangePattern.MkZoneChangePattern
      { ZoneChangePattern.whenDestination = d,
        ZoneChangePattern.whoseObject = w
      }

counterPatternToJson :: CounterPattern.CounterPattern -> Value
counterPatternToJson p =
  Object
    [ (Text.pack "whichKind", maybeTo counterKindToJson (CounterPattern.whichKind p)),
      (Text.pack "whose", controllerRelationToJson (CounterPattern.whose p)),
      (Text.pack "onWhat", permanentCriterionToJson (CounterPattern.onWhat p))
    ]

jsonToCounterPattern :: Value -> Either Text CounterPattern.CounterPattern
jsonToCounterPattern value = do
  ps <- Json.asObject value
  k <- Json.field (Text.pack "whichKind") ps >>= maybeFrom jsonToCounterKind
  w <- Json.field (Text.pack "whose") ps >>= jsonToControllerRelation
  o <- Json.field (Text.pack "onWhat") ps >>= jsonToPermanentCriterion
  pure
    CounterPattern.MkCounterPattern
      { CounterPattern.whichKind = k,
        CounterPattern.whose = w,
        CounterPattern.onWhat = o
      }

tokenPatternToJson :: TokenPattern.TokenPattern -> Value
tokenPatternToJson p =
  Object [(Text.pack "whose", controllerRelationToJson (TokenPattern.whose p))]

jsonToTokenPattern :: Value -> Either Text TokenPattern.TokenPattern
jsonToTokenPattern value = do
  ps <- Json.asObject value
  w <- Json.field (Text.pack "whose") ps >>= jsonToControllerRelation
  pure TokenPattern.MkTokenPattern {TokenPattern.whose = w}

damagePatternToJson :: DamagePattern.DamagePattern -> Value
damagePatternToJson p =
  Object [(Text.pack "whichKind", maybeTo damageKindToJson (DamagePattern.whichKind p))]

jsonToDamagePattern :: Value -> Either Text DamagePattern.DamagePattern
jsonToDamagePattern value = do
  ps <- Json.asObject value
  k <- Json.field (Text.pack "whichKind") ps >>= maybeFrom jsonToDamageKind
  pure DamagePattern.MkDamagePattern {DamagePattern.whichKind = k}
```

If `jsonToDamageKind` does not yet exist next to `damageKindToJson` (line ~680), add it:

```haskell
jsonToDamageKind :: Value -> Either Text DamageKind.DamageKind
jsonToDamageKind =
  decodeNullary
    (Text.pack "DamageKind")
    [ (Text.pack "Combat", DamageKind.Combat),
      (Text.pack "Noncombat", DamageKind.Noncombat)
    ]
```

Then replace `replacementEffectToJson`/`jsonToReplacementEffect` (lines 954–963) with:

```haskell
replacementEffectToJson :: ReplacementEffect.ReplacementEffect -> Value
replacementEffectToJson re = case re of
  ReplacementEffect.ZoneChangeR p z ->
    Json.tagged (Text.pack "ZoneChangeR") (Just (Array [zoneChangePatternToJson p, zoneToJson z]))
  ReplacementEffect.EntryR r ->
    Json.tagged (Text.pack "EntryR") (Just (entryRewriteToJson r))
  ReplacementEffect.DamageR p r ->
    Json.tagged (Text.pack "DamageR") (Just (Array [damagePatternToJson p, damageRewriteToJson r]))
  ReplacementEffect.DestructionR r ->
    Json.tagged (Text.pack "DestructionR") (Just (destructionRewriteToJson r))
  ReplacementEffect.CounterR p s ->
    Json.tagged (Text.pack "CounterR") (Just (Array [counterPatternToJson p, scalingToJson s]))
  ReplacementEffect.TokenR p s ->
    Json.tagged (Text.pack "TokenR") (Just (Array [tokenPatternToJson p, scalingToJson s]))

jsonToReplacementEffect :: Value -> Either Text ReplacementEffect.ReplacementEffect
jsonToReplacementEffect value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("ZoneChangeR", Just (Array [p, z])) -> do
      pattern_ <- jsonToZoneChangePattern p
      dest <- jsonToZone z
      pure (ReplacementEffect.ZoneChangeR pattern_ dest)
    ("EntryR", Just v) -> fmap ReplacementEffect.EntryR (jsonToEntryRewrite v)
    ("DamageR", Just (Array [p, r])) -> do
      pattern_ <- jsonToDamagePattern p
      rewrite <- jsonToDamageRewrite r
      pure (ReplacementEffect.DamageR pattern_ rewrite)
    ("DestructionR", Just v) -> fmap ReplacementEffect.DestructionR (jsonToDestructionRewrite v)
    ("CounterR", Just (Array [p, s])) -> do
      pattern_ <- jsonToCounterPattern p
      scaling <- jsonToScaling s
      pure (ReplacementEffect.CounterR pattern_ scaling)
    ("TokenR", Just (Array [p, s])) -> do
      pattern_ <- jsonToTokenPattern p
      scaling <- jsonToScaling s
      pure (ReplacementEffect.TokenR pattern_ scaling)
    _ -> Left (Text.pack "unknown ReplacementEffect: " <> t)
```

- [x] **Step 6: Carry the source through `replacementsAffecting`**

A `ControllerRelation` needs the effect's source, which a bare `[ReplacementEffect]` cannot supply. In `source/library/Pawl/Projection.hs`, replace `replacementsAffecting` (lines 646–654):

```haskell
-- CR 614.6: every replacement effect active on the battlefield, PAIRED WITH ITS
-- SOURCE -- a ControllerRelation pattern (CR 109.5's "you") is unanswerable
-- without it. Short-circuits when no permanent has one in its base card, so an
-- ordinary zone change (a draw, a land entering) does NOT project the whole board.
--
-- The short-circuit reads BASE cards while the result reads the PROJECTION, which
-- is sound only because the one way to acquire a replacement effect you were not
-- printed with is `EntryR AsCopy` -- and a card with that arm is itself a base
-- card with a replacement effect, so it keeps `baseHas` true for its own object.
replacementsAffecting :: GameState -> [(ObjectId, ReplacementEffect)]
replacementsAffecting gs =
  let onBattlefield = Set.toList (GameState.battlefield gs)
      baseHas oid = case Game.cardOf oid gs of
        Nothing -> False
        Just card -> not (null (Card.Type.replacementEffects card))
      forOne oid = map (\re -> (oid, re)) (replacementsOf oid gs)
   in if not (any baseHas onBattlefield)
        then []
        else concatMap forOne onBattlefield
```

- [x] **Step 7: Adapt the pure zone-change fold**

In `source/library/Pawl/Event.hs`, change line 171 to `resolved = applyReplacements gs (Projection.replacementsAffecting gs) proposed`, and replace `applyReplacements`/`applyOne` (lines 296–309):

```haskell
-- CR 614: rewrite the proposed zone change by each active replacement, paired
-- with its source. CR 614.5: applied left-to-right, each seeing the running
-- event; a ZoneChangeR's output destination no longer matches its own
-- `whenDestination`, so it cannot re-fire.
--
-- This pure fold is a TRANSITIONAL shape. It cannot ask a question and it invents
-- an order when two replacements apply -- CR 616.1's own violation, which this
-- phase exists to retire. Pawl.Replacement replaces it entirely; nothing else
-- about it is meant to survive.
applyReplacements :: GameState -> [(ObjectId, ReplacementEffect)] -> ZoneChange -> ZoneChange
applyReplacements gs res zc = List.foldl' (applyOne gs) zc res

applyOne :: GameState -> ZoneChange -> (ObjectId, ReplacementEffect) -> ZoneChange
applyOne gs zc (src, re) = case re of
  ReplacementEffect.ZoneChangeR pat toDest ->
    if ZoneChange.to zc == ZoneChangePattern.whenDestination pat
      && matchesController gs src (ZoneChangePattern.whoseObject pat) (ZoneChange.object zc)
      then zc {ZoneChange.to = toDest}
      else zc
  -- CR 614.1: an effect whose event class is not a zone change never applies to
  -- one. The type is what rules this out; these arms only make the case total.
  ReplacementEffect.EntryR _ -> zc
  ReplacementEffect.DamageR _ _ -> zc
  ReplacementEffect.DestructionR _ -> zc
  ReplacementEffect.CounterR _ _ -> zc
  ReplacementEffect.TokenR _ _ -> zc

-- CR 109.5 / 614.1: does `oid` satisfy this pattern's controller relation, read
-- against the controller of the effect's SOURCE? Anyones always does.
matchesController :: GameState -> ObjectId -> ControllerRelation -> ObjectId -> Bool
matchesController gs src rel oid = case rel of
  ControllerRelation.Anyones -> True
  ControllerRelation.Yours -> Projection.controllerOf oid gs == Projection.controllerOf src gs
```

Add `import Pawl.Type.ControllerRelation (ControllerRelation)`, `import qualified Pawl.Type.ControllerRelation as ControllerRelation` and `import qualified Pawl.Type.ZoneChangePattern as ZoneChangePattern` to `Pawl.Event`.

- [x] **Step 8: Migrate `rest-in-peace.json`**

Replace the `replacementEffects` value in `data/cards/rest-in-peace.json` (leave every other key byte-identical):

```json
"replacementEffects":[{"type":"ZoneChangeR","value":[{"whenDestination":{"type":"Graveyard"},"whoseObject":{"type":"Anyones"}},{"type":"Exile"}]}]
```

- [x] **Step 9: Update the three existing tests that name the old constructor**

`source/test-suite/Pawl/EventSpec.hs`, replace the three `applyReplacements` cases at lines 31–43:

```haskell
    [ HU.testCase "CR 614.1a a graveyard-bound move is redirected to exile" $
        let (_, gs) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            rip = ReplacementEffect.ZoneChangeR (ZoneChangePattern.MkZoneChangePattern Zone.Graveyard ControllerRelation.Anyones) Zone.Exile
            proposed = ZoneChange.MkZoneChange {ZoneChange.object = ObjectId.MkObjectId 5, ZoneChange.from = Zone.Battlefield, ZoneChange.to = Zone.Graveyard}
         in HU.assertEqual "redirected to exile" Zone.Exile (ZoneChange.to (Event.applyReplacements gs [(ObjectId.MkObjectId 0, rip)] proposed)),
      HU.testCase "CR 614.5 the redirect does not re-apply (exile is not graveyard)" $
        let (_, gs) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            rip = ReplacementEffect.ZoneChangeR (ZoneChangePattern.MkZoneChangePattern Zone.Graveyard ControllerRelation.Anyones) Zone.Exile
            proposed = ZoneChange.MkZoneChange {ZoneChange.object = ObjectId.MkObjectId 5, ZoneChange.from = Zone.Battlefield, ZoneChange.to = Zone.Graveyard}
            pairs = [(ObjectId.MkObjectId 0, rip), (ObjectId.MkObjectId 0, rip)]
         in HU.assertEqual "exile, applied once" Zone.Exile (ZoneChange.to (Event.applyReplacements gs pairs proposed)),
      HU.testCase "a non-graveyard move is untouched" $
        let (_, gs) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            rip = ReplacementEffect.ZoneChangeR (ZoneChangePattern.MkZoneChangePattern Zone.Graveyard ControllerRelation.Anyones) Zone.Exile
            proposed = ZoneChange.MkZoneChange {ZoneChange.object = ObjectId.MkObjectId 5, ZoneChange.from = Zone.Stack, ZoneChange.to = Zone.Battlefield}
         in HU.assertEqual "battlefield unchanged" Zone.Battlefield (ZoneChange.to (Event.applyReplacements gs [(ObjectId.MkObjectId 0, rip)] proposed)),
```

Add `import qualified Pawl.Type.ControllerRelation as ControllerRelation` and `import qualified Pawl.Type.ZoneChangePattern as ZoneChangePattern` to `EventSpec`.

`source/test-suite/Pawl/ProjectionSpec.hs:387` — replace `[ReplacementEffect.RedirectZoneChange Zone.Graveyard Zone.Exile]` with `[ReplacementEffect.ZoneChangeR (ZoneChangePattern.MkZoneChangePattern Zone.Graveyard ControllerRelation.Anyones) Zone.Exile]`, importing the two modules. That assertion reads `Projection.replacementsOf`, which still returns `[ReplacementEffect]` — unchanged. If the assertion instead reads `replacementsAffecting`, wrap the expectation as a `[(ObjectId, ReplacementEffect)]` pair list.

- [x] **Step 10: Build and run the suite**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -20`
Expected: PASS, whole suite green (670+ cases), no warnings.

- [x] **Step 11: Commit**

```bash
git add source/library/Pawl/Type/ControllerRelation.hs source/library/Pawl/Type/PermanentCriterion.hs source/library/Pawl/Type/ZoneChangePattern.hs source/library/Pawl/Type/CounterPattern.hs source/library/Pawl/Type/TokenPattern.hs source/library/Pawl/Type/DamagePattern.hs source/library/Pawl/Type/Scaling.hs source/library/Pawl/Type/EntryOption.hs source/library/Pawl/Type/EntryRewrite.hs source/library/Pawl/Type/DamageRewrite.hs source/library/Pawl/Type/DestructionRewrite.hs source/library/Pawl/Type/ReplacementEffect.hs source/library/Pawl/Codec.hs source/library/Pawl/Event.hs source/library/Pawl/Projection.hs data/cards/rest-in-peace.json source/test-suite/Pawl/EventSpec.hs source/test-suite/Pawl/CodecSpec.hs source/test-suite/Pawl/ProjectionSpec.hs pawl.cabal
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p5): event-class-general ReplacementEffect vocabulary

Rest in Peace stops being a constructor and becomes data. Six arms, one
per replaceable event class (CR 614.1a); the scoping a card needs rides
as pattern data. The zone-change fold is adapted and stays pure.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 2: The monadic ripple — every funnel becomes `Game`

**The one task that carries the risk.** Zero behaviour change; the whole existing suite must stay green. Nothing new is added — the five change-and-emit funnels and the state-based-action pass simply stop being pure `GameState -> GameState` and become `Game`, because in Task 3 they will each need to prompt.

**Files:**
- Modify: `source/library/Pawl/Event.hs` (`placeObject`, `changeZone`, `destroy`, `regenerate`, `counter`, `sacrifice`, `createToken`, `drawCard`)
- Modify: `source/library/Pawl/Damage.hs:157-174` (`applyDamage`), `:217-220` (`dealWave`)
- Modify: `source/library/Pawl/Sba.hs:101-183` (`checkStateBasedActions`, `performStateBasedActions`)
- Modify: `source/library/Pawl/Resolve.hs` (lines 300, 308, 387, 464, 481, 491, 500, 509, 521, 533, 556–572, 612, 690–696)
- Modify: `source/library/Pawl/Engine.hs:93`, `:96`, `:141-142`, `:393-395`, `:436`
- Modify: `source/library/Pawl/Stack.hs:42`
- Modify: `source/library/Pawl/Cast.hs:202-213`
- Modify: `source/library/Pawl/Activate.hs:133-149`
- Modify: `source/library/Pawl/Setup.hs:123-124`
- Test: `source/test-suite/Pawl/Support.hs` (add `runPure`, `settleSba`), and every spec that calls a now-monadic function

**Interfaces:**
- Consumes: nothing from Task 1 beyond a compiling tree.
- Produces: `Event.placeObject :: PlayerId -> (Timestamp -> Object) -> Zone -> Game ObjectId`; `Event.changeZone :: ObjectId -> Zone -> Game ()`; `Event.destroy :: ObjectId -> Game ()`; `Event.regenerate :: ObjectId -> Game ()`; `Event.counter :: ObjectId -> Game ()`; `Event.sacrifice :: ObjectId -> Game ()`; `Event.createToken :: PlayerId -> Card -> Game ()`; `Event.drawCard :: PlayerId -> Game ()`; `Damage.applyDamage :: [DamageEvent] -> Game ()`; `Sba.performStateBasedActions :: Game Bool`; `Sba.checkStateBasedActions :: Game ()`; `Resolve.putTapped :: ObjectId -> Game ()`; test helpers `S.runPure :: (forall r. Prompt r -> r) -> GameState -> Game a -> GameState` and `S.settleSba :: GameState -> GameState`.

- [x] **Step 1: Add the two test helpers first (they are what keeps the test ripple mechanical)**

In `source/test-suite/Pawl/Support.hs`, next to `fightWith`:

```haskell
-- Run a Game action purely under an answerer and keep only the final state. The
-- shape every direct-call test needs now that the change-and-emit funnels are
-- monadic (P5): `Event.destroy oid gs` becomes
-- `S.runPure S.identityAnswer gs (Event.destroy oid)`.
runPure :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> Game.Type.Game a -> GameState.GameState
runPure answer gs game = snd (Engine.runGamePure answer gs game)

-- One CR 704 state-based-action pass, run purely. The direct replacement for the
-- pre-P5 pure `Sba.checkStateBasedActions gs`.
settleSba :: GameState.GameState -> GameState.GameState
settleSba gs = runPure identityAnswer gs Sba.checkStateBasedActions
```

Add `import qualified Pawl.Sba as Sba` to `Support.hs`.

- [x] **Step 2: Run the build to confirm the helpers compile against the still-pure engine**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20`
Expected: FAIL — `Couldn't match type 'GameState -> GameState' with 'StateT GameState …'` on `settleSba`. That is the failing check for this task: it is red precisely because `Sba.checkStateBasedActions` is still pure.

- [x] **Step 3: Make `Pawl.Event`'s funnels monadic**

`source/library/Pawl/Event.hs`. Add `import qualified Control.Monad.Trans.State.Strict as State` and `import Pawl.Type.Game (Game)`. Rewrite these seven definitions (every comment above each one is kept verbatim; only the body and signature change):

```haskell
placeObject :: PlayerId -> (Timestamp.Timestamp -> Object.Object) -> Zone -> Game ObjectId
placeObject pid mkObj dest = do
  gs <- State.get
  let (newId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj = markCopyOnEnter dest (mkObj ts)
      gs3 = gs2 {GameState.objects = Map.insert newId obj (GameState.objects gs2)}
  State.put (Game.insertIntoZone dest pid newId gs3)
  pure newId

changeZone :: ObjectId -> Zone -> Game ()
changeZone oid requestedDest = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just obj -> do
      let pid = Object.owner obj
          fromZone = Object.zone obj
          snapshot = Projection.project oid gs
          proposed = ZoneChange.MkZoneChange oid fromZone requestedDest
          resolved = applyReplacements gs (Projection.replacementsAffecting gs) proposed
          dest = ZoneChange.to resolved
          mkObj ts = obj {Object.zone = dest, Object.tapped = TapState.Untapped, Object.damage = 0, Object.sickness = Sickness.Sick, Object.bindings = Map.empty, Object.counters = Map.empty, Object.timestamp = ts}
          gs1 = Game.removeFromZones pid oid gs
          gs2 = gs1 {GameState.objects = Map.delete oid (GameState.objects gs1)}
      State.put gs2
      newId <- placeObject pid mkObj dest
      State.modify' (recordEvent (GameEvent.Moved (ZoneChange.MkZoneChange newId fromZone dest) snapshot))

destroy :: ObjectId -> Game ()
destroy oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just _ ->
      if Projection.hasKeyword Keyword.Indestructible oid gs
        then pure ()
        else case Map.lookup oid (GameState.regenerationShields gs) of
          Just n | n > 0 -> regenerate oid
          _ -> changeZone oid Zone.Graveyard

regenerate :: ObjectId -> Game ()
regenerate oid = State.modify' $ \gs ->
  let shields = Map.update (\n -> if n <= 1 then Nothing else Just (n - 1)) oid (GameState.regenerationShields gs)
      healTap obj = obj {Object.damage = 0, Object.tapped = TapState.Tapped}
      gs1 =
        gs
          { GameState.regenerationShields = shields,
            GameState.objects = Map.adjust healTap oid (GameState.objects gs)
          }
   in removeFromCombat oid gs1

counter :: ObjectId -> Game ()
counter oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just _ -> changeZone oid Zone.Graveyard

sacrifice :: ObjectId -> Game ()
sacrifice oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just obj -> case Object.zone obj of
      Zone.Battlefield -> changeZone oid Zone.Graveyard
      Zone.Library -> pure ()
      Zone.Hand -> pure ()
      Zone.Graveyard -> pure ()
      Zone.Stack -> pure ()
      Zone.Exile -> pure ()

createToken :: PlayerId -> Card -> Game ()
createToken controller card = do
  let mkObj ts =
        Object.MkObject
          { Object.owner = controller,
            Object.source = Source.OfToken card,
            Object.zone = Zone.Battlefield,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
  newId <- placeObject controller mkObj Zone.Battlefield
  placed <- State.get
  let snapshot = Projection.project newId placed
  State.modify' (recordEvent (GameEvent.Moved (ZoneChange.MkZoneChange newId Zone.Battlefield Zone.Battlefield) snapshot))

drawCard :: PlayerId -> Game ()
drawCard pid = do
  gs <- State.get
  case Game.zoneMembers Zone.Library pid gs of
    [] -> State.put gs {GameState.drewFromEmpty = Set.insert pid (GameState.drewFromEmpty gs)}
    top : _ -> changeZone top Zone.Hand
```

- [x] **Step 4: Make `Damage.applyDamage` monadic**

`source/library/Pawl/Damage.hs`, replacing lines 157–174 and 217–220 (keeping the comments):

```haskell
applyDamage :: [DamageEvent.DamageEvent] -> Game ()
applyDamage events = do
  gs <- State.get
  let kept = Event.applyPreventions (GameState.preventions gs) events
      markOne g ev = case DamageEvent.target ev of
        Recipient.ToCreature oid ->
          let hurt obj = obj {Object.damage = Object.damage obj + DamageEvent.amount ev}
           in g {GameState.objects = Map.adjust hurt oid (GameState.objects g)}
        Recipient.ToPlayer pid ->
          let drain player = player {Player.life = Player.life player - toInteger (DamageEvent.amount ev)}
           in g {GameState.players = Map.adjust drain pid (GameState.players g)}
        Recipient.ToObject _ -> g
      marked = List.foldl' markOne gs kept
  State.put (List.foldl' (\g ev -> Event.recordEvent (GameEvent.DamageDealt ev) g) marked kept)

dealWave :: (ObjectId -> Bool) -> Game ()
dealWave assigns = do
  assignment <- gatherCombatDamage assigns
  applyDamage assignment
```

- [x] **Step 5: Make `Sba` monadic**

`source/library/Pawl/Sba.hs`, replacing lines 98–183. The classification is still computed once against the pre-pass state (CR 704.4 simultaneity); only the *moves* become monadic binds:

```haskell
-- CR 704.3 says to repeat until no state-based action is performed. One pass is
-- enough in M1b: a creature dying cannot cause another SBA, because nothing
-- gains or loses life when a creature dies. Revisit when it can.
checkStateBasedActions :: Game ()
checkStateBasedActions = Monad.void performStateBasedActions

-- One SBA pass, also reporting whether any state-based action was PERFORMED (a
-- creature buried or a player departed). CR 704.4: the caller repeats the check
-- while that flag is True. The flag lets the CR 117.5 settle loop (Engine) decide
-- whether to repeat WITHOUT a deep GameState comparison.
--
-- Monadic since P5: CR 704.5f's put-into-graveyard and CR 704.5g's destruction
-- both go through funnels that can now raise a CR 616 replacement prompt (a
-- creature dying with two applicable death-replacements genuinely must ask its
-- controller which to apply). M3g's decider re-entrancy already permits prompting
-- from inside the settle loop.
performStateBasedActions :: Game Bool
performStateBasedActions = do
  gs <- State.get
  let -- CR 704.5f/g are checked against the state BEFORE any of them apply: SBAs
      -- are simultaneous. Project the whole board once (one gather) and judge each
      -- object against it, rather than re-projecting per object.
      pcs = Projection.projectAll gs
      classify oid = case Map.lookup oid pcs of
        Nothing -> Nothing
        Just pc
          -- CR 704.5f wins when both apply: toughness <= 0 is a put-into-graveyard.
          | zeroToughness pc -> Just False
          | destroyedBySba gs pc oid -> Just True
          | otherwise -> Nothing
      onBattlefield = Set.toList (GameState.battlefield gs)
      toGraveyard = filter (\oid -> classify oid == Just False) onBattlefield
      toDestroy = filter (\oid -> classify oid == Just True) onBattlefield
      -- CR 704.5q / 122.3: a permanent with both a +1/+1 and a -1/-1 counter has N
      -- of each removed (N = min). Computed against the SAME pre-pass state as the
      -- bury/destroy classification, which is what makes the ordering immaterial:
      -- net P/T is preserved, so it can neither cause nor prevent a death.
      annihilateOne oid = case Game.lookupObject oid gs of
        Nothing -> Nothing
        Just obj ->
          let cs = Object.counters obj
              plus = Map.findWithDefault 0 CounterKind.PlusOnePlusOne cs
              minus = Map.findWithDefault 0 CounterKind.MinusOneMinusOne cs
              n = min plus minus
           in if n > 0 then Just (oid, n) else Nothing
      annihilations = Maybe.mapMaybe annihilateOne onBattlefield
      -- CR 704.5h's window is "since the last SBA check", so the watermark is the
      -- log length AS THIS PASS BEGAN: every 704.5h victim was computed from that
      -- same pre-pass state, and the Moved events this pass itself appends carry
      -- no damage. The record is never removed.
      watermark = fromIntegral (Seq.length (GameState.events gs))
  -- CR 704.5f: a plain put-into-graveyard (regeneration cannot save it).
  Monad.mapM_ (\oid -> Event.changeZone oid Zone.Graveyard) toGraveyard
  -- CR 704.5g/h: destruction through the funnel (regeneration may replace it).
  Monad.mapM_ Event.destroy toDestroy
  destroyed <- State.get
  let leaving = filter (losesNow destroyed) (stillPlaying destroyed)
      departed = foldr depart destroyed leaving
      remaining = stillPlaying departed
      -- CR 704.5d: a token in any zone other than the battlefield ceases to exist.
      -- Computed from the post-bury state so a token that just died (now in the
      -- graveyard) or was redirected (Rest in Peace -> exile) is removed here; its
      -- move already emitted a zone-change event, so a future dies-trigger still
      -- sees it (CR 111.7's parenthetical). Keyed to "not on the battlefield",
      -- never to a specific zone, so exile is caught too.
      isVanishingToken oid = case Game.lookupObject oid departed of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfToken _ -> Object.zone obj /= Zone.Battlefield
          _ -> False
      vanishing = filter isVanishingToken (Map.keys (GameState.objects departed))
      ceaseToExist g oid = case Game.lookupObject oid g of
        Nothing -> g
        Just obj ->
          let g1 = Game.removeFromZones (Object.owner obj) oid g
           in g1 {GameState.objects = Map.delete oid (GameState.objects g1)}
      vanished = List.foldl' ceaseToExist departed vanishing
      removeN n c = let c' = c - n in if c' == 0 then Nothing else Just c'
      balance g (oid, n) =
        let strip obj = obj {Object.counters = Map.update (removeN n) CounterKind.MinusOneMinusOne (Map.update (removeN n) CounterKind.PlusOnePlusOne (Object.counters obj))}
         in g {GameState.objects = Map.adjust strip oid (GameState.objects g)}
      outcome = case remaining of
        [winner] -> Just (Result.Won winner)
        [] -> if null leaving then Nothing else Just Result.Drawn
        _ -> Nothing
      drained = vanished {GameState.damageScannedThrough = watermark}
      balanced = List.foldl' balance drained annihilations
      -- A state-based action was performed iff a creature was buried or destroyed
      -- (a regenerated creature still counts, which the CR 704.4 settle loop
      -- re-checks and -- because the regen healed the damage -- terminates), a
      -- player left, or a token ceased to exist.
      acted = not (null toGraveyard) || not (null toDestroy) || not (null leaving) || not (null vanishing) || not (null annihilations)
  State.put balanced {GameState.result = outcome <|> GameState.result balanced}
  pure acted
```

Add `import qualified Control.Monad as Monad`, `import qualified Control.Monad.Trans.State.Strict as State` and `import Pawl.Type.Game (Game)` to `Pawl.Sba`.

- [x] **Step 6: Follow the funnels through the six calling modules**

Each of these is the same rewrite: a `State.modify' (f … gs)` or a `List.foldl'` over `GameState` becomes a monadic bind or `Monad.mapM_`. Work them in this order and rebuild after each file.

`source/library/Pawl/Stack.hs:42` — `then Event.changeZone oid Zone.Battlefield`.

`source/library/Pawl/Setup.hs:124` — `drawCard pid = Event.drawCard pid`.

`source/library/Pawl/Engine.hs`:
- `:93` — `checkSba = Sba.checkStateBasedActions`
- `:96` — `drawFor pid = Event.drawCard pid`
- `:141-142` — replace `toGraveyard`/`State.modify'` with `Monad.mapM_ (\oid -> Event.changeZone oid Zone.Graveyard) toDiscard`
- `:393-395` — `acted <- Sba.performStateBasedActions` (drop the `gs <- State.get` / `State.put gs'` pair around it)
- `:436` — the `Play` arm:
  ```haskell
                  Action.Type.Play oid -> do
                    Event.changeZone oid Zone.Battlefield
                    State.modify' (\g -> g {GameState.landPlayed = Set.insert p (GameState.landPlayed g), GameState.passes = 0, GameState.priority = Just p})
                    settleForPriority
                    loop
  ```

`source/library/Pawl/Cast.hs:202-213`:
```haskell
              Just paid -> do
                State.put paid
                Event.changeZone oid Zone.Stack
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

`source/library/Pawl/Activate.hs:133-149` — `payAdditional` becomes monadic and `payAll` becomes a `mapM_`:
```haskell
          State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.fromChoices chosen Map.empty Nothing chosenModes}) abilId (GameState.objects g)})
          let additional = AbilityCost.additional (ActivatedAbility.cost ability)
              payAll = Monad.mapM_ (payAdditional srcId) additional
          case AbilityCost.mana (ActivatedAbility.cost ability) of
            Nothing -> payAll
            Just cost -> do
              g1 <- State.get
              case Mana.payCost pid cost g1 of
                -- activatable pre-checks canPay, so within the source elision this is
                -- unreachable; reject-not-repair if a distinguishable source ever makes
                -- payment fail (#12).
                Nothing -> State.put gs
                Just paid -> do
                  State.put paid
                  payAll

-- Pay one additional cost against the source permanent.
payAdditional :: ObjectId -> AdditionalCost.AdditionalCost -> Game ()
payAdditional srcId c = case c of
  AdditionalCost.TapSelf ->
    State.modify' (\gs -> gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) srcId (GameState.objects gs)})
  AdditionalCost.SacrificeSelf -> Event.changeZone srcId Zone.Graveyard
```

`source/library/Pawl/Resolve.hs` — thirteen sites. Each `State.modify' $ \gs -> case … of (Just recipient, True) -> … Event.f target gs; _ -> gs` becomes:
```haskell
  Effect.Destroy slot -> do
    gs <- State.get
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> case recipientObject recipient of
        Nothing -> pure ()
        -- CR 701.8: destroy through the single funnel -- indestructible (CR
        -- 702.12b) and regeneration (CR 701.19a) are Event.destroy's to decide.
        Just target -> Event.destroy target
      -- Illegal slot (CR 608.2b) or a non-object recipient: no-op.
      _ -> pure ()
```
Apply that same shape to `Sacrifice` (`Event.sacrifice target`), `MoveToZone` (`Event.changeZone target zone`), `Counter` (`Event.counter target`). The folds become `Monad.mapM_`:
- `:464` `ExileAllGraveyards` → `Monad.mapM_ (\c -> Event.changeZone c Zone.Exile) gyCards`
- `:509` `Draw` → `Monad.replicateM_ (fromInteger n) (Event.drawCard controller)`
- `:521` `Mill` → `Monad.mapM_ (\c -> Event.changeZone c Zone.Graveyard) topN`
- `:533` `Discard` → `bury cs = Monad.mapM_ (\c -> Event.changeZone c Zone.Graveyard) cs`
- `:556` `Create` → `Monad.replicateM_ (fromInteger n) (Event.createToken controller card)`
- `:387` `DealDamage` → `Damage.applyDamage [ … ]` (drop the trailing `gs`), which forces the whole `DealDamage` arm out of `State.modify'` into a `do` block reading `gs <- State.get` first
- `:300`, `:308` `resolveSpell`'s two burials → `Event.changeZone oid Zone.Graveyard`
- `:690-696` `putTapped`:
  ```haskell
  putTapped :: ObjectId -> Game ()
  putTapped cardId = do
    before <- State.get
    Event.changeZone cardId Zone.Battlefield
    moved <- State.get
    case newestBattlefieldOf cardId before moved of
      Nothing -> pure ()
      Just newId ->
        State.put moved {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) newId (GameState.objects moved)}
  ```
  and its caller at `:453` becomes `Just cardId -> putTapped cardId`.

- [x] **Step 7: Build the library alone and fix until clean**

Run: `cabal build 2>&1 | tail -40`
Expected: PASS with no warnings. (Build the library before the tests — the type errors are the map of what is left.)

- [x] **Step 8: Follow the ripple into the test suite**

Purely mechanical, no assertion changes. In each spec, wrap a direct funnel call in `S.runPure S.identityAnswer <state> (…)` and replace `Sba.checkStateBasedActions <state>` with `S.settleSba <state>`. The sites, by file:

| File | Calls to wrap |
|---|---|
| `DamageSpec.hs` | 22 × `Sba.checkStateBasedActions`, 2 × `Event.changeZone`, 4 × `Damage.applyDamage` |
| `ResolveSpec.hs` | 11 × `Sba.checkStateBasedActions`, 4 × `Event.changeZone`, 2 × `Event.drawCard`, 2 × `Damage.applyDamage` |
| `EventSpec.hs` | 7 × `Event.changeZone`, 6 × `Event.destroy`, 3 × `Event.counter`, 3 × `Sba.checkStateBasedActions`, 1 × `Event.createToken` |
| `TriggerSpec.hs` | 8 × `Event.destroy`, 6 × `Event.sacrifice`, 4 × `Event.changeZone`, 1 × `Sba.checkStateBasedActions` |
| `CombatSpec.hs` | 5 × `Sba.checkStateBasedActions` |
| `GameSpec.hs` | 4 × `Event.changeZone` |
| `ProjectionSpec.hs` | 2 × `Sba.checkStateBasedActions` |
| `ModalSpec.hs`, `CastSpec.hs` | 2 × `Event.changeZone` each |
| `ActivateSpec.hs` | 2 × `Event.destroy` |
| `CopySpec.hs` | 1 × `Event.destroy` (line 139) |

Worked example, `EventSpec.hs`'s Rest in Peace case:

```haskell
      HU.testCase "CR 614: with Rest in Peace out, a creature sent to the graveyard is exiled" $
        let (_, g0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (piker, g1) = S.addPiker cards S.bob g0
            after = S.runPure S.identityAnswer g1 (Event.changeZone piker Zone.Graveyard)
            inExile = Set.size (GameState.exile after)
            gyCount = length (Game.zoneMembers Zone.Graveyard S.bob after)
         in do
              HU.assertEqual "exiled, not in graveyard" 0 gyCount
              HU.assertEqual "one object in exile" 1 inExile,
```

Each spec that gains `S.runPure` needs `{-# LANGUAGE RankNTypes #-}` only if it defines its own rank-2 answerer; passing `S.identityAnswer` needs no pragma. `EventSpec.hs`, `TriggerSpec.hs`, `GameSpec.hs` and `ModalSpec.hs` may need `{-# LANGUAGE RankNTypes #-}` added at the top — add it only where GHC asks.

- [x] **Step 9: Run the full suite — this is the task's gate**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -20`
Expected: PASS, the **same** case count as before this task, zero failures. Any behavioural difference here is a bug in this task, not a milestone landing — the exit criterion for Task 2 is literally "nothing changed".

- [x] **Step 10: Confirm the benchmark still builds and runs**

Run: `cabal bench 2>&1 | tail -15`
Expected: three timings, statistically indistinguishable from before (the monad adds an unused `Program` layer on a path that never suspends).

- [x] **Step 11: Commit**

```bash
git add source/library/Pawl/Event.hs source/library/Pawl/Damage.hs source/library/Pawl/Sba.hs source/library/Pawl/Resolve.hs source/library/Pawl/Engine.hs source/library/Pawl/Stack.hs source/library/Pawl/Cast.hs source/library/Pawl/Activate.hs source/library/Pawl/Setup.hs source/test-suite/Pawl
hooky fix
git add -u
hooky run
git commit -m "refactor(m4.5-p5): the change-and-emit funnels become monadic

changeZone, destroy, counter, sacrifice, createToken, drawCard,
applyDamage and performStateBasedActions all become Game. Zero
behaviour change -- this is the substrate the CR 616 loop needs, landed
before any card can observe it.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 3: `Pawl.Replacement` — the CR 616.1 loop, with `WouldChangeZone` wired

**Files:**
- Create: `source/library/Pawl/Type/ProposedEvent.hs`, `CandidateId.hs`, `ReplacementCandidate.hs`, `ReplacementBucket.hs`
- Create: `source/library/Pawl/Replacement.hs`
- Create: `source/test-suite/Pawl/ReplacementSpec.hs`
- Modify: `source/library/Pawl/Type/Prompt.hs` (add `ChooseReplacement`), `Response.hs` (add `ChoseReplacement`), `source/library/Pawl/Replay.hs` (three arms)
- Modify: `source/library/Pawl/Event.hs` (`changeZone` calls the loop; delete `applyReplacements`, `applyOne`, `matchesController`)
- Modify: `source/test-suite/Pawl/Support.hs` (five answerers), `source/test-suite/Pawl/EventSpec.hs` (drop the three `applyReplacements` unit cases — the function is gone), `source/test-suite/Pawl/CopySpec.hs` (`copyNewest` falls through, no change needed)
- Modify: `source/test-suite/Main.hs`, `pawl.cabal`

**Interfaces:**
- Consumes: `ReplacementEffect` and its patterns (Task 1); `Event.changeZone :: ObjectId -> Zone -> Game ()` (Task 2).
- Produces: `Replacement.applyReplacements :: ProposedEvent -> Game (Maybe ProposedEvent)`; `Replacement.resolveZoneChange :: ZoneChange -> Game (Maybe ZoneChange)`; `Prompt.ChooseReplacement :: Decider -> PlayerId -> [ObjectId] -> Prompt Natural`; `Response.ChoseReplacement Natural`.

- [x] **Step 1: Write the failing test**

Create `source/test-suite/Pawl/ReplacementSpec.hs`:

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Replacement (the CR 616.1 loop, its buckets and its prompt) and
-- the funnels that raise proposed events through it. Gameplay-level throughout:
-- put a board together, cast or resolve, assert on game state.
module Pawl.ReplacementSpec where

import qualified Data.Set as Set
import qualified Pawl.Cards as Cards
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Replay as Replay
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.Game as Game.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Response as Response
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Every answer the engine asked for, in order -- so a test can assert that a
-- prompt WAS raised (the engine did not decide) or was NOT (the choice was
-- indistinguishable and correctly elided).
answersFor :: (forall r. S.PromptOf r) -> GameState.GameState -> Game.Type.Game a -> [Response.Response]
answersFor answer gs game = snd (Replay.record answer gs game)

wasAskedToReplace :: [Response.Response] -> Bool
wasAskedToReplace = any isReplacement
  where
    isReplacement r = case r of
      Response.ChoseReplacement _ -> True
      _ -> False

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Pawl.Replacement"
    [ HU.testCase "CR 614.5 two Rest in Peaces redirect once, not twice" $
        let (_, g0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (_, g1) = S.addCreature (Cards.restInPeacePrinting cards) S.alice g0
            (piker, g2) = S.addPiker cards S.bob g1
            after = S.runPure S.identityAnswer g2 (Event.changeZone piker Zone.Graveyard)
         in do
              HU.assertEqual "not in a graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.bob after))
              HU.assertEqual "exactly one object in exile" 1 (Set.size (GameState.exile after)),
      HU.testCase "CR 616.1 value-equal candidates elide the prompt (nothing to choose)" $
        let (_, g0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (_, g1) = S.addCreature (Cards.restInPeacePrinting cards) S.alice g0
            (piker, g2) = S.addPiker cards S.bob g1
            asked = answersFor S.identityAnswer g2 (Event.changeZone piker Zone.Graveyard)
         in HU.assertBool "no ChooseReplacement was raised" (not (wasAskedToReplace asked))
    ]
```

`S.PromptOf` does not exist; write the signature inline instead as
`answersFor :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> Game.Type.Game a -> [Response.Response]`
with `import qualified Pawl.Type.Prompt as Prompt`.

Register it: add `ReplacementSpec.tests cards` to `testTree` in `source/test-suite/Main.hs` (after `CopySpec.tests cards`) with the matching `import qualified Pawl.ReplacementSpec as ReplacementSpec`, and let `hooky fix` add `Pawl.ReplacementSpec` to the test-suite `other-modules` via `cabal-gild`.

- [x] **Step 2: Run it to watch it fail**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20`
Expected: FAIL — `Not in scope: data constructor 'Response.ChoseReplacement'`.

- [x] **Step 3: Add the four loop types**

`source/library/Pawl/Type/ProposedEvent.hs`:

```haskell
module Pawl.Type.ProposedEvent where

import Numeric.Natural (Natural)
import Pawl.Type.Card (Card)
import Pawl.Type.CounterKind (CounterKind)
import Pawl.Type.DamageEvent (DamageEvent)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.ZoneChange (ZoneChange)

-- CR 614.6: an event as it WOULD happen -- the thing a replacement effect
-- rewrites. Deliberately distinct from Pawl.Type.GameEvent: a GameEvent is
-- HISTORY (it carries a CR 608.2h last-known-information snapshot and exists only
-- after the fact), while a ProposedEvent exists only while it is being replaced,
-- and the one that survives the CR 616.1 loop is the one that actually happens.
--
-- WouldEnter is raised only for BATTLEFIELD entries (CR 614.1c-d apply nowhere
-- else) and is NESTED inside whatever caused the entry -- CR 616.1g's containment
-- ("one effect may apply to an event, and another to an event contained within
-- the first"), expressed as call nesting rather than as a field.
--
-- Six arms, not the ~40 replaceable event classes the rules define: each of the
-- rest is one more arm plus the funnel that raises it -- vocabulary on a finished
-- axis, which is what "the closed half can genuinely be finished" means here.
data ProposedEvent
  = WouldChangeZone ZoneChange
  | WouldEnter ObjectId
  | WouldDealDamage DamageEvent
  | WouldBeDestroyed ObjectId
  | WouldPutCounters ObjectId CounterKind Natural
  | WouldCreateTokens PlayerId Card Natural
  deriving (Eq, Show)
```

`source/library/Pawl/Type/CandidateId.hs`:

```haskell
module Pawl.Type.CandidateId where

import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.ReplacementEffect (ReplacementEffect)
import Pawl.Type.Timestamp (Timestamp)

-- CR 614.5's identity: "A replacement effect doesn't invoke itself repeatedly; it
-- gets only one opportunity to affect an event or any modified events that may
-- replace that event." This is what counts as ONE effect for that rule. Without
-- it Hardened Scales and Corpsejack Menace re-fire on each other's output
-- forever -- it is a termination condition, not an optimization.
--
-- A permanent's static replacement ability is identified by (source, effect
-- VALUE), NOT (source, list index). Index identity would break CR 616.2, the rule
-- this phase exists to get right: a Clone applies its own `EntryR AsCopy` (index
-- 0 of its one-element list), which replaces its copiable snapshot with a Primal
-- Plasma's -- whose `EntryR (ChoiceOf ...)` is then ALSO index 0. The
-- newly-acquired ability would be mistaken for the one already used, and the
-- Gatherer ruling's board state would be unreachable.
--
-- Two Doubling Seasons are still two opportunities: different SOURCES. The cost
-- of value identity is that a single source carrying two TEXTUALLY IDENTICAL
-- replacement abilities gets one opportunity instead of two; no card in the pool
-- does that (#N).
--
-- A floating replacement is identified by (source, timestamp): GameState's
-- timestamp counter is monotone, so two Fogs are two instances even from one
-- source object.
data CandidateId
  = OfPermanent ObjectId ReplacementEffect
  | OfFloating ObjectId Timestamp
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/ReplacementCandidate.hs`:

```haskell
module Pawl.Type.ReplacementCandidate where

import Pawl.Type.CandidateId (CandidateId)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.ReplacementEffect (ReplacementEffect)

-- One replacement effect instance as the CR 616.1 loop sees it: what it does,
-- whose it is (CR 109.5's "you", which every ControllerRelation pattern reads),
-- and which instance it is (CR 614.5). `source` is derivable from `identity` but
-- is kept explicit -- every applicability test and the ChooseReplacement payload
-- read it directly.
data ReplacementCandidate = MkReplacementCandidate
  { identity :: CandidateId,
    effect :: ReplacementEffect,
    source :: ObjectId
  }
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/ReplacementBucket.hs`:

```haskell
module Pawl.Type.ReplacementBucket where

-- CR 616.1a-e: the five ordered buckets the affected player picks from. The
-- HIGHEST non-empty bucket wins, and Ord here is ascending in the CR's own order,
-- so "highest non-empty" is the minimum present.
--
-- Only CopyOnEntry (616.1c) and Other (616.1e) have producers. The other three
-- are classification with a documented absence, not machinery pretending to
-- exist: SelfReplacement is CR 614.15 self-replacement effects (#N),
-- ControlOnEntry is CR 616.1b's "enters under your control instead" (#N), and
-- BackFaceOnEntry is CR 616.1d, which needs transform (CR 701.27) first (#N).
data ReplacementBucket
  = SelfReplacement -- CR 616.1a
  | ControlOnEntry -- CR 616.1b
  | CopyOnEntry -- CR 616.1c
  | BackFaceOnEntry -- CR 616.1d
  | Other -- CR 616.1e
  deriving (Eq, Ord, Show)
```

- [x] **Step 4: Add the prompt, the response, and the replay arms**

`source/library/Pawl/Type/Prompt.hs`, appended to the GADT:

```haskell
  -- CR 616.1: with two or more applicable replacement or prevention effects in
  -- the highest non-empty bucket, the affected object's controller (or its owner,
  -- or the affected player) chooses which to apply NEXT -- and then the process
  -- repeats over what is applicable now (616.1f), so this is asked once per
  -- iteration, not once per event. The [ObjectId] is each candidate's SOURCE, in
  -- the engine's canonical order (battlefield ascending, then the floating
  -- store); the answer is an index into it.
  --
  -- Positional, and carrying exactly the caveat #61 records for OrderTriggers: a
  -- source with two DISTINCT applicable replacement abilities would put two
  -- different effects on the wire as identical entries. That is reachable in a way
  -- it is not for triggers -- Doubling Season has two replacement abilities -- but
  -- they are in different EVENT CLASSES and so are never candidates for the same
  -- event. A single source with two same-class applicable replacements needs a
  -- discriminator alongside the source (#N).
  --
  -- Asked ONLY when the bucket holds two or more candidates that are not all equal
  -- as values: with one there is nothing to choose, and among equal values every
  -- order yields the same board (each still gets its own CR 614.5 opportunity).
  ChooseReplacement :: Decider -> PlayerId -> [ObjectId] -> Prompt Natural
```

`source/library/Pawl/Type/Response.hs`, appended:

```haskell
  | -- CR 616.1: the index of the replacement effect a player chose to apply next,
    -- serialized so a DecisionLog replays a replacement race deterministically.
    ChoseReplacement Natural
```

`source/library/Pawl/Replay.hs`:
- `encode`: `Prompt.ChooseReplacement {} -> Response.ChoseReplacement answer`
- `decode`: `Prompt.ChooseReplacement {} -> case response of Response.ChoseReplacement n -> Just n; _ -> Nothing`
- `defaultAnswer`:
  ```haskell
  -- CR 616.1: index 0 is always a legal answer (the bucket is non-empty when this
  -- is asked), and is the least eventful fallback when a transcript runs short.
  Prompt.ChooseReplacement {} -> 0
  ```

`source/test-suite/Pawl/Support.hs` — add `Prompt.ChooseReplacement {} -> 0` to `identityAnswer`, `castAnswer`, `aggressiveAnswer` and `playLandAnswer`, and `Prompt.ChooseReplacement {} -> pure 0` to `randomAnswer`.

- [x] **Step 5: Write `Pawl.Replacement`**

Create `source/library/Pawl/Replacement.hs`:

```haskell
-- CR 616.1's loop: the SOLE home of casing on ProposedEvent and
-- ReplacementEffect, a fourth sole-casing home beside Pawl.Resolve (Effect),
-- Pawl.Event (TriggerCondition / StateCondition) and Pawl.Projection
-- (Modification). Pawl.Codec also cases on ReplacementEffect, but only as the
-- JSON data boundary, never to decide game behaviour.
--
-- Read CR 616.1 literally: it is not an ordering prompt, it is a LOOP. Choose one
-- applicable effect from the highest non-empty of five ordered buckets, apply it,
-- then "this process is repeated (taking into account only replacement or
-- prevention effects that would now be applicable) until there are no more left
-- to apply" (616.1f). CR 616.2 adds that an effect can BECOME applicable because
-- another one modified the event. A foldl' over a list computed once is
-- structurally incapable of either.
--
-- This module must NOT import Pawl.Event: Event raises proposed events through
-- this loop, so the dependency runs one way only. That is also why the entry
-- copy-target legal set lives here rather than in Pawl.Target (Task 7) -- Target
-- imports Pawl.Sba, which imports Pawl.Event.
module Pawl.Replacement where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import Numeric.Natural (Natural)
import qualified Pawl.Decide as Decide
import qualified Pawl.Projection as Projection
import Pawl.Type.CandidateId (CandidateId)
import qualified Pawl.Type.CandidateId as CandidateId
import qualified Pawl.Type.ControllerRelation as ControllerRelation
import qualified Pawl.Type.DamageEvent as DamageEvent
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Program as Program
import Pawl.Type.ProposedEvent (ProposedEvent)
import qualified Pawl.Type.ProposedEvent as ProposedEvent
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
import Pawl.Type.ReplacementBucket (ReplacementBucket)
import qualified Pawl.Type.ReplacementBucket as ReplacementBucket
import Pawl.Type.ReplacementCandidate (ReplacementCandidate)
import qualified Pawl.Type.ReplacementCandidate as ReplacementCandidate
import Pawl.Type.ReplacementEffect (ReplacementEffect)
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import Pawl.Type.ZoneChange (ZoneChange)
import qualified Pawl.Type.ZoneChange as ZoneChange
import qualified Pawl.Type.ZoneChangePattern as ZoneChangePattern
import Data.Set (Set)
import qualified Data.Set as Set

-- CR 614: settle a proposed zone change. Nothing means the move does not happen.
-- The typed door Pawl.Event uses, so Event never cases on a ProposedEvent.
resolveZoneChange :: ZoneChange -> Game (Maybe ZoneChange)
resolveZoneChange zc = do
  outcome <- applyReplacements (ProposedEvent.WouldChangeZone zc)
  pure (outcome >>= asZoneChange)

asZoneChange :: ProposedEvent -> Maybe ZoneChange
asZoneChange event = case event of
  ProposedEvent.WouldChangeZone zc -> Just zc
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldBeDestroyed _ -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing

-- CR 616.1's loop. `Nothing` means the event DOES NOT HAPPEN -- CR 615.6's
-- prevented damage, CR 701.19a's replaced destruction. A rewrite that cancels an
-- event has already performed its own consequences by the time it returns
-- Nothing.
applyReplacements :: ProposedEvent -> Game (Maybe ProposedEvent)
applyReplacements = loop Set.empty

loop :: Set CandidateId -> ProposedEvent -> Game (Maybe ProposedEvent)
loop applied event = do
  gs <- State.get
  -- Step 1, from scratch each iteration: collect against the CURRENT state, minus
  -- CR 614.5's already-applied set. Re-collecting is what makes CR 616.2 work --
  -- an effect that only became applicable because of the last application is
  -- picked up here.
  let unused candidate = not (Set.member (ReplacementCandidate.identity candidate) applied)
      fresh = filter unused (applicable gs event)
  case highestBucket fresh of
    -- CR 616.1f: no candidate remains, so the loop ends and the surviving event
    -- is what happens (CR 614.6).
    [] -> pure (Just event)
    bucket -> do
      picked <- choose gs event bucket
      case picked of
        -- Unreachable: highestBucket returns [] for an empty input, so `bucket` is
        -- non-empty and `choose` always picks. Total rather than partial.
        Nothing -> pure (Just event)
        Just candidate -> do
          outcome <- apply candidate event
          case outcome of
            Nothing -> pure Nothing
            Just rewritten -> loop (Set.insert (ReplacementCandidate.identity candidate) applied) rewritten

-- Every replacement effect instance in the game, in the engine's canonical order:
-- battlefield permanents ascending by id, each permanent's own effects in printed
-- order. That order is what the ChooseReplacement prompt indexes into.
collect :: GameState -> [ReplacementCandidate]
collect gs =
  let fromPermanent entry =
        ReplacementCandidate.MkReplacementCandidate
          { ReplacementCandidate.identity = CandidateId.OfPermanent (fst entry) (snd entry),
            ReplacementCandidate.effect = snd entry,
            ReplacementCandidate.source = fst entry
          }
   in map fromPermanent (Projection.replacementsAffecting gs)

applicable :: GameState -> ProposedEvent -> [ReplacementCandidate]
applicable gs event = filter (applies gs event) (collect gs)

-- CR 614.1: does this instance apply to this proposed event? The arms must agree
-- on the EVENT CLASS -- which the type already rules out for the impossible pairs
-- -- and the pattern must admit the event's subject.
applies :: GameState -> ProposedEvent -> ReplacementCandidate -> Bool
applies gs event candidate =
  let src = ReplacementCandidate.source candidate
   in case (ReplacementCandidate.effect candidate, event) of
        (ReplacementEffect.ZoneChangeR pat _, ProposedEvent.WouldChangeZone zc) ->
          ZoneChange.to zc == ZoneChangePattern.whenDestination pat
            && matchesController gs src (ZoneChangePattern.whoseObject pat) (ZoneChange.object zc)
        -- Tasks 4, 5, 6, 7 and 9 fill these in as each funnel starts raising its
        -- event; until then no such ProposedEvent is ever constructed.
        (ReplacementEffect.ZoneChangeR _ _, _) -> False
        (ReplacementEffect.EntryR _, _) -> False
        (ReplacementEffect.DamageR _ _, _) -> False
        (ReplacementEffect.DestructionR _, _) -> False
        (ReplacementEffect.CounterR _ _, _) -> False
        (ReplacementEffect.TokenR _ _, _) -> False

-- CR 109.5 / 614.1: does `oid` satisfy this pattern's controller relation, read
-- against the controller of the effect's SOURCE? Anyones always does.
matchesController :: GameState -> ObjectId -> ControllerRelation.ControllerRelation -> ObjectId -> Bool
matchesController gs src rel oid = case rel of
  ControllerRelation.Anyones -> True
  ControllerRelation.Yours -> Projection.controllerOf oid gs == Projection.controllerOf src gs

-- CR 616.1a-e: take the HIGHEST non-empty bucket. Ord on ReplacementBucket is
-- ascending in the CR's own order, so that is the minimum present; the fold seeds
-- from Other (the largest) so it needs no partial `minimum`.
highestBucket :: [ReplacementCandidate] -> [ReplacementCandidate]
highestBucket candidates =
  let bucketed = map (\c -> (bucketOf (ReplacementCandidate.effect c), c)) candidates
      best = List.foldl' min ReplacementBucket.Other (map fst bucketed)
   in map snd (filter (\entry -> fst entry == best) bucketed)

-- CR 616.1a-e: which bucket an effect falls in.
bucketOf :: ReplacementEffect -> ReplacementBucket
bucketOf re = case re of
  -- CR 616.1e. Everything with a producer today except the copy-on-entry case.
  ReplacementEffect.ZoneChangeR _ _ -> ReplacementBucket.Other
  ReplacementEffect.EntryR _ -> ReplacementBucket.Other
  ReplacementEffect.DamageR _ _ -> ReplacementBucket.Other
  ReplacementEffect.DestructionR _ -> ReplacementBucket.Other
  ReplacementEffect.CounterR _ _ -> ReplacementBucket.Other
  ReplacementEffect.TokenR _ _ -> ReplacementBucket.Other

-- CR 616.1: "the affected object's controller (or its owner if it has no
-- controller) or the affected player chooses one to apply."
--
-- TWO ELISIONS, both of them choices the rules make indistinguishable:
--
--   * ONE candidate -- there is nothing to choose, and where the rules leave
--     nothing to ask, don't prompt.
--   * several candidates EQUAL AS VALUES -- each still gets its own CR 614.5
--     opportunity, so every order produces the same board. Only the PROMPT is
--     elided, never an application.
--
-- Anything else prompts. The pure fold has silently picked list order since M3f;
-- that is the second-invariant violation this phase exists to retire, and unlike
-- an elision it carried no expiry because nothing detected it.
choose :: GameState -> ProposedEvent -> [ReplacementCandidate] -> Game (Maybe ReplacementCandidate)
choose gs event candidates = case candidates of
  [] -> pure Nothing
  first : rest ->
    if all (\c -> ReplacementCandidate.effect c == ReplacementCandidate.effect first) rest
      then pure (Just first)
      else case chooserOf gs event of
        -- No chooser: the affected object is gone. Apply the canonical first
        -- rather than prompt nobody -- and in particular make no choice on behalf
        -- of a player who is not there to make it.
        Nothing -> pure (Just first)
        Just pid -> do
          let decider = Decide.deciderFor pid gs
          answer <- Trans.lift (Program.prompt (Prompt.ChooseReplacement decider pid (map ReplacementCandidate.source candidates)))
          -- Reject-not-repair, as payment and Engine.permute already do: an
          -- out-of-range index leaves the canonical first standing rather than
          -- dropping the event or crashing.
          pure (Just (at candidates answer first))

-- Total index into a list, with a fallback.
at :: [a] -> Natural -> a -> a
at xs i fallback = case drop (fromIntegral i) xs of
  h : _ -> h
  [] -> fallback

-- CR 616.1 / 108.4: who decides. Projection.controllerOf already falls back to
-- the owner (CR 108.4), so "or its owner if it has no controller" is free.
--
-- CR 616.1's APNAP clause -- "If two or more players have to make these choices at
-- the same time, choices are made in APNAP order (see rule 101.4)" -- has no
-- producer: one proposed event has exactly one affected object and therefore one
-- chooser, and the damage batch runs each event's loop independently (#N).
chooserOf :: GameState -> ProposedEvent -> Maybe PlayerId
chooserOf gs event = case event of
  ProposedEvent.WouldChangeZone zc -> Projection.controllerOf (ZoneChange.object zc) gs
  ProposedEvent.WouldEnter oid -> Projection.controllerOf oid gs
  ProposedEvent.WouldDealDamage de -> case DamageEvent.target de of
    Recipient.ToPlayer pid -> Just pid
    Recipient.ToCreature oid -> Projection.controllerOf oid gs
    Recipient.ToObject oid -> Projection.controllerOf oid gs
  ProposedEvent.WouldBeDestroyed oid -> Projection.controllerOf oid gs
  ProposedEvent.WouldPutCounters oid _ _ -> Projection.controllerOf oid gs
  ProposedEvent.WouldCreateTokens pid _ _ -> Just pid

-- CR 614.6: apply one chosen effect. Nothing means the event does not happen.
apply :: ReplacementCandidate -> ProposedEvent -> Game (Maybe ProposedEvent)
apply candidate event =
  case (ReplacementCandidate.effect candidate, event) of
    (ReplacementEffect.ZoneChangeR _ toDest, ProposedEvent.WouldChangeZone zc) ->
      pure (Just (ProposedEvent.WouldChangeZone zc {ZoneChange.to = toDest}))
    -- Unreachable: `applies` excluded every mismatched pair, and the remaining
    -- rewrite shapes arrive with their funnels in Tasks 4-9. Total, never partial.
    _ -> pure (Just event)
```

- [x] **Step 6: Point `Event.changeZone` at the loop and delete the fold**

In `source/library/Pawl/Event.hs`, delete `applyReplacements`, `applyOne` and `matchesController`, drop the now-unused `ControllerRelation`/`ZoneChangePattern`/`ReplacementEffect`/`List` imports, add `import qualified Pawl.Replacement as Replacement` and `import qualified Pawl.Type.ProposedEvent as ProposedEvent`, and rewrite `changeZone`:

```haskell
changeZone :: ObjectId -> Zone -> Game ()
changeZone oid requestedDest = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just obj -> do
      let pid = Object.owner obj
          fromZone = Object.zone obj
          -- CR 608.2h: last known information -- the object as it exists in the
          -- zone it is LEAVING, projected against the PRE-MOVE state. (The
          -- benchmark note about the strict snapshot field is unchanged.)
          snapshot = Projection.project oid gs
      -- CR 614.4: replacements exist before the event, so the loop reads them from
      -- the PRE-MOVE state. CR 614.6: the modified event is what actually happens.
      resolved <- Replacement.resolveZoneChange (ZoneChange.MkZoneChange oid fromZone requestedDest)
      case resolved of
        -- CR 614.6: nothing survived the loop, so no zone change happens. No
        -- producer today -- no card in the pool cancels a zone change outright --
        -- but Maybe is what "the event does not happen" means on this path.
        Nothing -> pure ()
        Just settled -> do
          let dest = ZoneChange.to settled
              mkObj ts = obj {Object.zone = dest, Object.tapped = TapState.Untapped, Object.damage = 0, Object.sickness = Sickness.Sick, Object.bindings = Map.empty, Object.counters = Map.empty, Object.timestamp = ts}
          State.modify' (\g -> (Game.removeFromZones pid oid g) {GameState.objects = Map.delete oid (GameState.objects (Game.removeFromZones pid oid g))})
          newId <- placeObject pid mkObj dest
          -- CR 603.2g: record the RESOLVED event, carrying the NEW object's id --
          -- what an enters trigger scans.
          State.modify' (recordEvent (GameEvent.Moved (ZoneChange.MkZoneChange newId fromZone dest) snapshot))
```

The doubled `Game.removeFromZones` above is deliberately not written that way — use a `let` binding instead:

```haskell
          State.modify' $ \g ->
            let g1 = Game.removeFromZones pid oid g
             in g1 {GameState.objects = Map.delete oid (GameState.objects g1)}
```

- [x] **Step 7: Retire the three `EventSpec` unit cases — porting the one whose coverage is not otherwise carried**

`Event.applyReplacements` no longer exists, so the three `testCase`s at `source/test-suite/Pawl/EventSpec.hs:31-43` cannot stay in that shape. Two of them are now covered gameplay-level: "a graveyard-bound move is redirected to exile" by `EventSpec`'s existing Rest in Peace cases, and "the redirect does not re-apply" by `ReplacementSpec`'s "two Rest in Peaces redirect once". The third — a move whose destination the pattern does not match is untouched — is **not** covered anywhere else, so it is PORTED, not deleted. Delete the three unit cases, remove the now-unused `ReplacementEffect`/`ControllerRelation`/`ZoneChangePattern`/`ObjectId` imports, and add to `source/test-suite/Pawl/ReplacementSpec.hs`:

```haskell
      HU.testCase "CR 614.1a a move whose destination the pattern misses is untouched" $
        let (_, g0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (piker, g1) = S.addPiker cards S.bob g0
            -- Rest in Peace watches graveyard-bound moves only; a bounce to hand
            -- is not one, so the loop finds no candidate and the move stands.
            after = S.runPure S.identityAnswer g1 (Event.changeZone piker Zone.Hand)
         in do
              HU.assertEqual "in bob's hand" 1 (length (Game.zoneMembers Zone.Hand S.bob after))
              HU.assertEqual "nothing was exiled" 0 (Set.size (GameState.exile after)),
```

- [x] **Step 8: Build and run**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -20`
Expected: PASS. In particular the existing Rest in Peace gameplay cases (§5 scenario 17) are green *through the new loop*, and the two new `ReplacementSpec` cases pass.

- [x] **Step 9: Commit**

```bash
git add source/library/Pawl/Replacement.hs source/library/Pawl/Type/ProposedEvent.hs source/library/Pawl/Type/CandidateId.hs source/library/Pawl/Type/ReplacementCandidate.hs source/library/Pawl/Type/ReplacementBucket.hs source/library/Pawl/Type/Prompt.hs source/library/Pawl/Type/Response.hs source/library/Pawl/Replay.hs source/library/Pawl/Event.hs source/test-suite/Pawl/ReplacementSpec.hs source/test-suite/Pawl/Support.hs source/test-suite/Pawl/EventSpec.hs source/test-suite/Main.hs pawl.cabal
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p5): Pawl.Replacement implements the CR 616.1 loop

Collect (minus CR 614.5's applied set), bucket by 616.1a-e, choose --
prompting via ChooseReplacement only when the choice is real -- apply,
repeat (616.1f). Zone changes are the first class wired through it; the
pure foldl' is gone.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 4: Prevention folded in — `Effect.Replace`, `GameState.replacements`, `Uses`

**Files:**
- Create: `source/library/Pawl/Type/Uses.hs`, `source/library/Pawl/Type/ActiveReplacement.hs`
- Delete: `source/library/Pawl/Type/Prevention.hs`, `source/library/Pawl/Type/ActivePrevention.hs`
- Modify: `source/library/Pawl/Type/GameState.hs` (`preventions` → `replacements`), `Effect.hs` (`Prevent` → `Replace`), `Pawl/Replacement.hs`, `Pawl/Event.hs` (delete `applyPreventions`/`cancels`, rename `dropEndOfTurnPreventions`), `Pawl/Damage.hs`, `Pawl/Resolve.hs`, `Pawl/Engine.hs:185`, `Pawl/Setup.hs:73`, `Pawl/Codec.hs`
- Modify: `data/cards/fog.json`
- Test: `source/test-suite/Pawl/Support.hs` (`addPrevention` → `addReplacement`, `oneMountainState`), `DamageSpec.hs:155-170`, `ResolveSpec.hs:452-464`, `ReplacementSpec.hs` (scenario 22)

**Interfaces:**
- Consumes: `Replacement.applyReplacements` (Task 3); `Damage.applyDamage :: [DamageEvent] -> Game ()` (Task 2).
- Produces: `Uses = Unlimited | Once`; `ActiveReplacement` with fields `effect`/`source`/`timestamp`/`duration`/`uses`; `GameState.replacements :: [ActiveReplacement]`; `Effect.Replace Duration Uses ReplacementEffect`; `Replacement.resolveDamage :: DamageEvent -> Game (Maybe DamageEvent)`; `Event.dropEndOfTurnReplacements :: GameState -> GameState`; `S.addReplacement :: ActiveReplacement -> GameState -> GameState`.

- [x] **Step 1: Write the failing tests (§5 scenarios 21 and 22)**

Scenario 21 already exists as `ResolveSpec.hs`'s "CR 615 Fog prevents combat damage but not spell damage (the gate)"; change only its shield-count assertion from `GameState.preventions` to `GameState.replacements`. Add scenario 22 to `source/test-suite/Pawl/ReplacementSpec.hs`:

```haskell
      HU.testCase "CR 615.10 Fog prevents both attackers' damage in one batch" $
        let base = S.landsInPlay (Cards.forestPrinting cards) 1
            (victimA, g1) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            (victimB, g2) = S.addCreature (Cards.pikerPrinting cards) S.bob g1
            (g3, fogId) = S.handOne (Cards.fogPrinting cards) g2
            resolved = S.runPure S.identityAnswer g3 (Cast.castSpell S.alice fogId >> Stack.resolveTop)
            batch =
              [ DamageEvent.MkDamageEvent victimA (Recipient.ToCreature victimA) 2 False DamageKind.Combat,
                DamageEvent.MkDamageEvent victimB (Recipient.ToCreature victimB) 2 False DamageKind.Combat
              ]
            after = S.runPure S.identityAnswer resolved (Damage.applyDamage batch)
         in do
              HU.assertEqual "the first attacker's damage was prevented" (Just 0) (S.damageOf victimA after)
              HU.assertEqual "and so was the second's, independently" (Just 0) (S.damageOf victimB after)
              HU.assertEqual "no damage event was recorded at all" [] (S.damageEventsOf after)
```

- [x] **Step 2: Run to watch it fail**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20`
Expected: FAIL to compile — `GameState.replacements` is not a field of `GameState`.

- [x] **Step 3: Add `Uses` and `ActiveReplacement`, delete the two prevention types**

`source/library/Pawl/Type/Uses.hs`:

```haskell
module Pawl.Type.Uses where

-- CR 614.3: floating replacement and prevention effects "last until they're used
-- up or their duration has expired". Regeneration is CR 701.19a's "the NEXT time
-- this permanent would be destroyed this turn" (Once); Fog watches every combat
-- damage event for its whole duration (Unlimited).
--
-- A sum, not a Bool and not a counter: "used up" is a rules concept, and CR
-- 615's prevent-the-next-N shape (which would be a counted arm here) has no
-- producer in the pool.
data Uses
  = Unlimited
  | Once
  deriving (Eq, Ord, Show)
```

`source/library/Pawl/Type/ActiveReplacement.hs`:

```haskell
module Pawl.Type.ActiveReplacement where

import Pawl.Type.Duration (Duration)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.ReplacementEffect (ReplacementEffect)
import Pawl.Type.Timestamp (Timestamp)
import Pawl.Type.Uses (Uses)

-- CR 614.3 / 615.3: a floating, resolution-generated replacement effect, held in
-- GameState.replacements. The event-pipeline analog of ContinuousEffect: the
-- projection re-derives a permanent's static replacement abilities live, while
-- these are stored because the object that made them may be long gone.
--
-- `duration` decides when cleanup drops it (CR 514.2). `uses` is CR 614.3's
-- "until they're used up". `source` and `timestamp` are new here and are exactly
-- the two fields #58 recorded as missing: CR 615.13's "prevented" triggers and CR
-- 615.7's multi-source choice are no longer STRUCTURALLY blocked, only
-- card-blocked.
--
-- `timestamp` doubles as this instance's CR 614.5 identity (Pawl.Type.CandidateId):
-- GameState.nextTimestamp is monotone, so no two floating replacements share one.
data ActiveReplacement = MkActiveReplacement
  { effect :: ReplacementEffect,
    source :: ObjectId,
    timestamp :: Timestamp,
    duration :: Duration,
    uses :: Uses
  }
  deriving (Eq, Ord, Show)
```

Delete `source/library/Pawl/Type/Prevention.hs` and `source/library/Pawl/Type/ActivePrevention.hs`.

- [x] **Step 4: Reshape `GameState` and `Effect`**

`source/library/Pawl/Type/GameState.hs`, replacing the `preventions` field (lines 57–60):

```haskell
    -- CR 614.3 / 615.3: floating replacement effects from resolutions (Fog's
    -- prevention, Drudge Skeletons' regeneration shield), each with a duration
    -- cleanup consults (CR 514.2) and a use count (CR 614.3). The event-pipeline
    -- analog of continuousEffects; a permanent's STATIC replacement abilities are
    -- not here -- the projection re-derives those live. Pawl.Replacement reads it.
    replacements :: [ActiveReplacement],
```

with `import Pawl.Type.ActiveReplacement (ActiveReplacement)` replacing the `ActivePrevention` import. Leave `regenerationShields` in place; Task 5 removes it.

`source/library/Pawl/Setup.hs:73` — `emptyGame` builds the record literally, so change `GameState.preventions = [],` to `GameState.replacements = [],` there too. (`regenerationShields` stays until Task 5.)

`source/library/Pawl/Type/Effect.hs`, replacing the `Prevent` constructor (lines 111–115):

```haskell
  | -- CR 614.3 / 615.3: install a floating replacement effect for a duration, with
    -- a use count. Fog is
    -- `Replace UntilEndOfTurn Unlimited (DamageR (MkDamagePattern (Just Combat)) PreventAll)`;
    -- Drudge Skeletons' ability is
    -- `Replace UntilEndOfTurn Once (DestructionR Regenerate)`.
    --
    -- ONE opcode for both, where M3f/M4d had `Prevent` and `RegenerateSelf`: the
    -- difference between a Fog and a regeneration shield is which event class the
    -- payload names, which is data. Targetless (a floating replacement watches a
    -- CLASS of events, not a chosen object) and unprompted. Resolve stores it into
    -- GameState.replacements with this effect's SOURCE (CR 608.2g) and a fresh
    -- timestamp; Pawl.Replacement applies it.
    Replace Duration Uses ReplacementEffect
```

with `import Pawl.Type.ReplacementEffect (ReplacementEffect)` and `import Pawl.Type.Uses (Uses)` replacing the `Prevention` import.

- [x] **Step 5: Teach the loop about damage, uses, and the floating store**

`source/library/Pawl/Replacement.hs`:

Add to `collect` so it reads both sources:

```haskell
collect :: GameState -> [ReplacementCandidate]
collect gs =
  let fromPermanent entry =
        ReplacementCandidate.MkReplacementCandidate
          { ReplacementCandidate.identity = CandidateId.OfPermanent (fst entry) (snd entry),
            ReplacementCandidate.effect = snd entry,
            ReplacementCandidate.source = fst entry
          }
      fromFloating active =
        ReplacementCandidate.MkReplacementCandidate
          { ReplacementCandidate.identity = CandidateId.OfFloating (ActiveReplacement.source active) (ActiveReplacement.timestamp active),
            ReplacementCandidate.effect = ActiveReplacement.effect active,
            ReplacementCandidate.source = ActiveReplacement.source active
          }
   in map fromPermanent (Projection.replacementsAffecting gs)
        ++ map fromFloating (GameState.replacements gs)
```

Add the damage arm to `applies` (above the catch-alls):

```haskell
        (ReplacementEffect.DamageR pat _, ProposedEvent.WouldDealDamage de) ->
          case DamagePattern.whichKind pat of
            -- CR 615.1: no kind named means every damage event.
            Nothing -> True
            Just kind -> DamageEvent.kind de == kind
```

Add the damage arm to `apply`, and the CR 614.3 use-consumption:

```haskell
    -- CR 615.6: a prevented event never happens -- it is not marked, not drained,
    -- and never recorded, so no deathtouch bit exists for the CR 704.5h SBA to read.
    (ReplacementEffect.DamageR _ DamageRewrite.PreventAll, ProposedEvent.WouldDealDamage _) -> do
      consume (ReplacementCandidate.identity candidate)
      pure Nothing
```

and make the `ZoneChangeR` arm consume too, so every application goes through one place:

```haskell
    (ReplacementEffect.ZoneChangeR _ toDest, ProposedEvent.WouldChangeZone zc) -> do
      consume (ReplacementCandidate.identity candidate)
      pure (Just (ProposedEvent.WouldChangeZone zc {ZoneChange.to = toDest}))
```

plus:

```haskell
-- CR 614.3: a floating replacement whose `uses` is Once is spent by being
-- applied. A permanent's STATIC replacement ability has no use count at all --
-- it is re-derived from the battlefield every iteration -- so only the floating
-- store is touched here.
consume :: CandidateId -> Game ()
consume identity_ = case identity_ of
  CandidateId.OfPermanent _ _ -> pure ()
  CandidateId.OfFloating src ts ->
    State.modify' $ \gs ->
      let spent active =
            ActiveReplacement.source active == src
              && ActiveReplacement.timestamp active == ts
              && ActiveReplacement.uses active == Uses.Once
       in gs {GameState.replacements = filter (not . spent) (GameState.replacements gs)}

-- CR 615: settle one proposed damage event. Nothing means it does not happen.
resolveDamage :: DamageEvent.DamageEvent -> Game (Maybe DamageEvent.DamageEvent)
resolveDamage de = do
  outcome <- applyReplacements (ProposedEvent.WouldDealDamage de)
  pure (outcome >>= asDamageEvent)

asDamageEvent :: ProposedEvent -> Maybe DamageEvent.DamageEvent
asDamageEvent event = case event of
  ProposedEvent.WouldDealDamage de -> Just de
  ProposedEvent.WouldChangeZone _ -> Nothing
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldBeDestroyed _ -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
```

- [x] **Step 6: Route the damage batch through the loop**

`source/library/Pawl/Damage.hs`, replacing `applyDamage`:

```haskell
-- CR 120.3e / 120.3a: mark damage on creatures, drain life from players -- AND
-- record each event into GameState.events. The change-and-emit funnel for
-- combat's two waves and resolving effects alike.
--
-- CR 615 / 616: EACH event in the batch runs its OWN CR 616.1 loop, and the
-- survivors are applied together. Simultaneity is preserved as a SCHEDULING
-- property; the loop's unit stays one event, uniform with the other five classes.
-- That is what CR 614.5 ("one opportunity to affect AN EVENT") and CR 615.10
-- ("applies separately to damage from other applicable events that would happen
-- at the same time") both describe.
--
-- What this shape cannot express is CR 615.7's SHARED N-damage shield -- one
-- resource allocated across several simultaneous events, with the recipient
-- choosing which it covers. No such shield exists in the pool (Fog is
-- unlimited-for-a-duration, not N-damage), so it stays card-driven (#58).
applyDamage :: [DamageEvent.DamageEvent] -> Game ()
applyDamage events = do
  survivors <- fmap Maybe.catMaybes (Monad.mapM Replacement.resolveDamage events)
  let markOne g ev = case DamageEvent.target ev of
        Recipient.ToCreature oid ->
          let hurt obj = obj {Object.damage = Object.damage obj + DamageEvent.amount ev}
           in g {GameState.objects = Map.adjust hurt oid (GameState.objects g)}
        Recipient.ToPlayer pid ->
          let drain player = player {Player.life = Player.life player - toInteger (DamageEvent.amount ev)}
           in g {GameState.players = Map.adjust drain pid (GameState.players g)}
        Recipient.ToObject _ -> g
  -- CR 608.2i: each surviving event is RECORDED, not enqueued. Sba consumes by
  -- bumping GameState.damageScannedThrough; the record survives the check.
  State.modify' (\gs -> List.foldl' (\g ev -> Event.recordEvent (GameEvent.DamageDealt ev) g) (List.foldl' markOne gs survivors) survivors)
```

Add `import qualified Data.Maybe as Maybe` and `import qualified Pawl.Replacement as Replacement` to `Pawl.Damage`.

- [x] **Step 7: Delete the prevention path and rename the cleanup**

`source/library/Pawl/Event.hs`: delete `applyPreventions` and `cancels` (lines 89–102), and replace `dropEndOfTurnPreventions` (lines 104–108):

```haskell
-- CR 514.2: at cleanup, drop until-end-of-turn floating replacements (the
-- event-pipeline analog of Projection.dropEndOfTurnEffects). Indefinite ones stay.
-- This one function retires BOTH pre-P5 cleanups: a regeneration shield is now an
-- UntilEndOfTurn/Once entry in the same store, so CR 701.19a's "this turn" falls
-- out of CR 514.2 rather than needing its own sweep.
dropEndOfTurnReplacements :: GameState -> GameState
dropEndOfTurnReplacements gs =
  let keep active = ActiveReplacement.duration active /= Duration.UntilEndOfTurn
   in gs {GameState.replacements = filter keep (GameState.replacements gs)}
```

`source/library/Pawl/Engine.hs:185` — `State.modify' Event.dropEndOfTurnReplacements` (leave the `Event.clearRegenerationShields` line; Task 5 deletes it).

- [x] **Step 8: Swap the opcode in `Pawl.Resolve` and `Pawl.Codec`**

`source/library/Pawl/Resolve.hs` — in `slotsOf`, `readsX`, `manaProduced`, `searchesLibrary` and `rewriteEffect`, replace the `Effect.Prevent _ _` arm with `Effect.Replace {}` (same result in each: `Set.empty` / `False` / `Nothing` / `False` / `effect`). Then replace the `Effect.Prevent` executor arm (lines 594–598):

```haskell
  Effect.Replace duration uses re ->
    -- CR 614.3 / 615.3: install the floating replacement; Pawl.Replacement
    -- consults it at every funnel until cleanup drops it (CR 514.2) or its use is
    -- spent. Targetless and unprompted. CR 608.2g: the SOURCE is this effect's
    -- source, which is what CR 615.13's "prevented" triggers will read (#58).
    State.modify' $ \gs ->
      let (ts, gs1) = Game.freshTimestamp gs
          active =
            ActiveReplacement.MkActiveReplacement
              { ActiveReplacement.effect = re,
                ActiveReplacement.source = source,
                ActiveReplacement.timestamp = ts,
                ActiveReplacement.duration = duration,
                ActiveReplacement.uses = uses
              }
       in gs1 {GameState.replacements = active : GameState.replacements gs1}
```

`source/library/Pawl/Codec.hs` — delete `preventionToJson`/`jsonToPrevention` and its import; add `usesToJson`/`jsonToUses`:

```haskell
usesToJson :: Uses.Uses -> Value
usesToJson u = nullary . Text.pack $ case u of
  Uses.Unlimited -> "Unlimited"
  Uses.Once -> "Once"

jsonToUses :: Value -> Either Text Uses.Uses
jsonToUses =
  decodeNullary
    (Text.pack "Uses")
    [ (Text.pack "Unlimited", Uses.Unlimited),
      (Text.pack "Once", Uses.Once)
    ]
```

and swap the `Effect` arms:

```haskell
  Effect.Replace d u re -> Json.tagged (Text.pack "Replace") (Just (Array [durationToJson d, usesToJson u, replacementEffectToJson re]))
```

```haskell
    ("Replace", Just (Array [d, u, re])) -> do
      duration <- jsonToDuration d
      uses <- jsonToUses u
      effect <- jsonToReplacementEffect re
      pure (Effect.Replace duration uses effect)
```

- [x] **Step 9: Migrate `fog.json`**

Replace Fog's single effect (leave every other key byte-identical):

```json
{"type":"Replace","value":[{"type":"UntilEndOfTurn"},{"type":"Unlimited"},{"type":"DamageR","value":[{"whichKind":{"type":"Combat"}},{"type":"PreventAll"}]}]}
```

- [x] **Step 10: Update the test fixtures**

`source/test-suite/Pawl/Support.hs`:
- replace `addPrevention` with

  ```haskell
  -- Seed a floating replacement directly into GameState (bypasses casting the
  -- spell that would install it; use when a test needs one active without a
  -- resolution).
  addReplacement :: ActiveReplacement.ActiveReplacement -> GameState.GameState -> GameState.GameState
  addReplacement active gs =
    gs {GameState.replacements = active : GameState.replacements gs}
  ```
- in `oneMountainState`, replace `GameState.preventions = []` with `GameState.replacements = []`
- swap the `ActivePrevention` import for `ActiveReplacement`

`source/test-suite/Pawl/DamageSpec.hs:155-170` — build the shield with the new type and rename the cleanup call:

```haskell
      HU.testCase "CR 615 a prevention drops combat damage but spares Noncombat" $
        let base = Setup.emptyGame S.bothPlayers
            (victim, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice base
            shield =
              ActiveReplacement.MkActiveReplacement
                { ActiveReplacement.effect = ReplacementEffect.DamageR (DamagePattern.MkDamagePattern (Just DamageKind.Combat)) DamageRewrite.PreventAll,
                  ActiveReplacement.source = victim,
                  ActiveReplacement.timestamp = Timestamp.MkTimestamp 900,
                  ActiveReplacement.duration = Duration.UntilEndOfTurn,
                  ActiveReplacement.uses = Uses.Unlimited
                }
            withShield = S.addReplacement shield gs0
            combat = S.runPure S.identityAnswer withShield (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False DamageKind.Combat])
            spell = S.runPure S.identityAnswer withShield (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False DamageKind.Noncombat])
         in do
              HU.assertEqual "combat damage prevented -- none marked" (Just 0) (S.damageOf victim combat)
              HU.assertEqual "combat damage prevented -- no event recorded" [] (S.damageEventsOf combat)
              HU.assertEqual "noncombat damage still dealt" (Just 2) (S.damageOf victim spell),
      HU.testCase "CR 514.2 an until-end-of-turn replacement wears off at cleanup" $
        let base = Setup.emptyGame S.bothPlayers
            shield =
              ActiveReplacement.MkActiveReplacement
                { ActiveReplacement.effect = ReplacementEffect.DamageR (DamagePattern.MkDamagePattern (Just DamageKind.Combat)) DamageRewrite.PreventAll,
                  ActiveReplacement.source = ObjectId.MkObjectId 900,
                  ActiveReplacement.timestamp = Timestamp.MkTimestamp 900,
                  ActiveReplacement.duration = Duration.UntilEndOfTurn,
                  ActiveReplacement.uses = Uses.Unlimited
                }
            dropped = Event.dropEndOfTurnReplacements (S.addReplacement shield base)
         in HU.assertEqual "no replacements remain" [] (GameState.replacements dropped)
```

`source/test-suite/Pawl/ResolveSpec.hs:460` — `HU.assertEqual "Fog installed one replacement" 1 (length (GameState.replacements resolved))`.

- [x] **Step 11: Build and run**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -20`
Expected: PASS. §5 scenarios 21 and 22 green; `grep -rn "Prevention" source/` returns nothing.

- [x] **Step 12: Commit**

```bash
git add source/library/Pawl/Type/Uses.hs source/library/Pawl/Type/ActiveReplacement.hs source/library/Pawl/Type/GameState.hs source/library/Pawl/Type/Effect.hs source/library/Pawl/Replacement.hs source/library/Pawl/Event.hs source/library/Pawl/Damage.hs source/library/Pawl/Resolve.hs source/library/Pawl/Engine.hs source/library/Pawl/Codec.hs data/cards/fog.json source/test-suite/Pawl pawl.cabal
git rm source/library/Pawl/Type/Prevention.hs source/library/Pawl/Type/ActivePrevention.hs
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p5): prevention folds into the one replacement path

Effect.Prevent and Pawl.Type.Prevention are gone; Fog is
Replace UntilEndOfTurn Unlimited (DamageR (Just Combat) PreventAll).
Each event of a damage batch runs its own CR 616.1 loop against the same
pre-damage state (CR 615.10), with the batch preserved as scheduling.
ActiveReplacement carries the source and timestamp #58 wanted.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 5: Regeneration folded in — `WouldBeDestroyed` and `Uses = Once`

**Files:**
- Modify: `source/library/Pawl/Game.hs` (gains `removeFromCombat`), `Pawl/Event.hs` (`destroy` raises the event; delete `regenerate`, `removeFromCombat`, `clearRegenerationShields`), `Pawl/Replacement.hs`, `Pawl/Type/GameState.hs` (delete `regenerationShields`), `Pawl/Type/Effect.hs` (delete `RegenerateSelf`), `Pawl/Resolve.hs` (six case tables + the executor arm), `Pawl/Engine.hs:186`, `Pawl/Setup.hs:74`, `Pawl/Codec.hs`
- Modify: `data/cards/drudge-skeletons.json`
- Test: `source/test-suite/Pawl/Support.hs` (`addRegenShield` reimplemented, `oneMountainState`), `EventSpec.hs:131-160`, `ActivateSpec.hs:144`, `TriggerSpec.hs:266-270`, `ReplacementSpec.hs` (scenarios 18–20)

**Interfaces:**
- Consumes: `ActiveReplacement`, `Uses`, `Replacement.consume` (Task 4).
- Produces: `Game.removeFromCombat :: ObjectId -> GameState -> GameState`; `Replacement.resolveDestruction :: ObjectId -> Game Bool` (True = the destruction happens).

- [x] **Step 1: Write the failing tests (§5 scenarios 18, 19, 20)**

Add to `source/test-suite/Pawl/ReplacementSpec.hs`:

```haskell
      HU.testCase "CR 701.19a Uses=Once: the first destruction is replaced, the second is not" $
        let base = S.landsInPlay (Cards.swampPrinting cards) 1
            (skel, g1) = S.addCreature (Cards.drudgeSkeletonsPrinting cards) S.alice base
            -- Activate {B}: regenerate this creature, and resolve it.
            armed = S.runPure S.identityAnswer g1 (Activate.activateAbility S.alice skel (AbilityName.MkAbilityName 0) >> Stack.resolveTop)
            once = S.runPure S.identityAnswer armed (Event.destroy skel)
            twice = S.runPure S.identityAnswer once (Event.destroy skel)
         in do
              HU.assertBool "survived the first destruction" (Set.member skel (GameState.battlefield once))
              HU.assertEqual "the shield was spent" [] (GameState.replacements once)
              HU.assertBool "the second destruction kills it" (not (Set.member skel (GameState.battlefield twice))),
      HU.testCase "CR 614.8 regeneration replaces the destruction, so Rest in Peace never sees it" $
        let base = S.landsInPlay (Cards.swampPrinting cards) 1
            (_, g1) = S.addCreature (Cards.restInPeacePrinting cards) S.bob base
            (skel, g2) = S.addCreature (Cards.drudgeSkeletonsPrinting cards) S.alice g1
            shielded = S.addRegenShield skel g2
            after = S.runPure S.identityAnswer shielded (Event.destroy skel)
         in do
              HU.assertBool "still on the battlefield" (Set.member skel (GameState.battlefield after))
              HU.assertEqual "nothing was exiled -- the put-into-graveyard never happened" 0 (Set.size (GameState.exile after))
              HU.assertEqual "and nothing reached a graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      HU.testCase "CR 614.7 an event that never happens does not consume a shield" $
        let base = Setup.emptyGame S.bothPlayers
            (myr, g1) = S.addCreature (Cards.darksteelMyrPrinting cards) S.alice base
            shielded = S.addRegenShield myr g1
            after = S.runPure S.identityAnswer shielded (Event.destroy myr)
         in do
              HU.assertBool "the indestructible creature survives" (Set.member myr (GameState.battlefield after))
              HU.assertEqual "the shield is intact" 1 (length (GameState.replacements after))
```

`Activate.activateAbility`'s third argument shape must match its actual signature — read `source/library/Pawl/Activate.hs`'s top-level signature and use whatever ability identifier it takes (the existing `ActivateSpec.hs` regeneration test at line 144 already calls it correctly; copy that call site verbatim rather than guessing).

- [x] **Step 2: Run to watch it fail**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "Pawl.Replacement"' 2>&1 | tail -30`
Expected: FAIL — "the shield was spent" reports the old `regenerationShields` path leaving `GameState.replacements` empty from the start, and "Rest in Peace never sees it" passes only by accident of the hardcoded precedence this task is removing.

- [x] **Step 3: Move `removeFromCombat` down to `Pawl.Game`**

`Pawl.Replacement` performs regeneration, and it must not import `Pawl.Event`. Move the function verbatim (comment included, with the parenthetical about Event's import restriction rewritten) from `source/library/Pawl/Event.hs:213-223` to `source/library/Pawl/Game.hs`:

```haskell
-- CR 701.19a: if a permanent is attacking or blocking, remove it from combat.
-- Edits the GameState.combat maps directly. It lives here, in the lowest layer,
-- because both Pawl.Event and Pawl.Replacement need it and neither may import the
-- other.
removeFromCombat :: ObjectId -> GameState -> GameState
removeFromCombat oid gs =
  let c = GameState.combat gs
      c1 =
        c
          { Combat.attackers = Map.delete oid (Combat.attackers c),
            Combat.blockers = Map.map (Set.delete oid) (Map.delete oid (Combat.blockers c))
          }
   in gs {GameState.combat = c1}
```

with `import qualified Pawl.Type.Combat as Combat` added to `Pawl.Game`.

- [x] **Step 4: Teach the loop about destruction**

`source/library/Pawl/Replacement.hs` — add to `applies` (above the catch-alls):

```haskell
        -- CR 201.5 / 201.5c / 701.19a: "regenerate THIS creature" names the
        -- ability's own source, so a destruction replacement is self-only. CR
        -- 614.1d's other-objects form has no producer.
        (ReplacementEffect.DestructionR _, ProposedEvent.WouldBeDestroyed oid) -> src == oid
```

add to `apply`:

```haskell
    -- CR 701.19a: "the next time [it] would be destroyed this turn, instead
    -- remove all damage marked on it and tap it. If it's attacking or blocking,
    -- remove it from combat." The DESTRUCTION does not happen -- so nothing
    -- downstream of it (a put-into-graveyard, and therefore Rest in Peace's
    -- redirect) ever runs. That nesting was hardcoded in Event.destroy before P5;
    -- it is structural now.
    (ReplacementEffect.DestructionR DestructionRewrite.Regenerate, ProposedEvent.WouldBeDestroyed oid) -> do
      consume (ReplacementCandidate.identity candidate)
      State.modify' $ \gs ->
        let healTap obj = obj {Object.damage = 0, Object.tapped = TapState.Tapped}
         in Game.removeFromCombat oid gs {GameState.objects = Map.adjust healTap oid (GameState.objects gs)}
      pure Nothing
```

(bind the record update in a `let` so the precedence is unambiguous), and add the typed door:

```haskell
-- CR 701.8 / 614.8: settle a proposed destruction. True means the permanent is
-- actually destroyed; False means a replacement took it (regeneration), and that
-- rewrite has already done its own work.
resolveDestruction :: ObjectId -> Game Bool
resolveDestruction oid = do
  outcome <- applyReplacements (ProposedEvent.WouldBeDestroyed oid)
  pure (Maybe.isJust outcome)
```

- [x] **Step 5: Raise the event from `Event.destroy`**

`source/library/Pawl/Event.hs` — delete `regenerate` and `removeFromCombat`, delete `clearRegenerationShields`, and rewrite `destroy` (keeping its comment, with the regeneration sentence rewritten):

```haskell
-- The single destruction funnel (CR 701.8 / 702.12b): every destruction -- the
-- Destroy opcode and the CR 704.5g/h state-based actions -- flows through here.
--
-- CR 702.12b: an indestructible permanent can't be destroyed, and that gate comes
-- BEFORE the replacement loop, which is CR 614.7 -- "an event that would
-- otherwise be replaced ... doesn't happen, so nothing replaces it": a shield is
-- neither applied nor consumed. Otherwise the would-be-destroyed event is offered
-- to CR 616.1; if it survives, the permanent is put into its owner's graveyard via
-- changeZone (so Rest in Peace's redirect and a token's CR 704.5d cease-to-exist
-- still compose). Ungated for CR 701.19c "can't be regenerated" (#42).
destroy :: ObjectId -> Game ()
destroy oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just _ ->
      if Projection.hasKeyword Keyword.Indestructible oid gs
        then pure ()
        else do
          happens <- Replacement.resolveDestruction oid
          Monad.when happens (changeZone oid Zone.Graveyard)
```

`source/library/Pawl/Engine.hs:186` — delete the `State.modify' Event.clearRegenerationShields` line (Task 4's `dropEndOfTurnReplacements` already drops the `UntilEndOfTurn` shields).

- [x] **Step 6: Delete `regenerationShields` and `RegenerateSelf`**

`source/library/Pawl/Type/GameState.hs` — delete the `regenerationShields` field and its comment. `source/library/Pawl/Setup.hs:74` — delete the matching `GameState.regenerationShields = Map.empty,` line from `emptyGame` (and its now-unused `Map` import if nothing else there uses it).

`source/library/Pawl/Type/Effect.hs` — delete the `RegenerateSelf` constructor and its comment.

`source/library/Pawl/Resolve.hs` — delete the `Effect.RegenerateSelf` arm from `slotsOf`, `readsX`, `manaProduced`, `searchesLibrary`, `rewriteEffect`, and from `applyEffect` (lines 599–604).

`source/library/Pawl/Codec.hs` — delete the `Effect.RegenerateSelf` encode arm and the `("RegenerateSelf", _)` decode arm.

- [x] **Step 7: Migrate `drudge-skeletons.json`**

Replace the ability's single effect (leave every other key byte-identical):

```json
{"type":"Replace","value":[{"type":"UntilEndOfTurn"},{"type":"Once"},{"type":"DestructionR","value":{"type":"Regenerate"}}]}
```

- [x] **Step 8: Reimplement `S.addRegenShield` on the new store**

Keeping the name means the twelve existing call sites do not change.

`source/test-suite/Pawl/Support.hs`:

```haskell
-- Seed a regeneration shield directly (bypasses activating a regenerate ability;
-- use when a test needs a shield up without the activation). Since P5 a shield is
-- an ordinary floating replacement: CR 701.19a's "the next time ... this turn" is
-- exactly UntilEndOfTurn + Once.
addRegenShield :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
addRegenShield oid gs =
  let (ts, gs1) = Game.freshTimestamp gs
      active =
        ActiveReplacement.MkActiveReplacement
          { ActiveReplacement.effect = ReplacementEffect.DestructionR DestructionRewrite.Regenerate,
            ActiveReplacement.source = oid,
            ActiveReplacement.timestamp = ts,
            ActiveReplacement.duration = Duration.UntilEndOfTurn,
            ActiveReplacement.uses = Uses.Once
          }
   in addReplacement active gs1
```

and delete `GameState.regenerationShields = Map.empty` from `oneMountainState`.

- [x] **Step 9: Update the four tests that read the old store**

- `EventSpec.hs:131-134` — "a regeneration shield is stored per object" reads a store that no longer exists and asserts nothing the new scenarios do not; delete it.
- `EventSpec.hs:135-139` — "shields are cleared at cleanup (this turn)" is REAL behaviour (CR 701.19a's "this turn"), and Task 4's generic cleanup case does not assert that a *regeneration* shield in particular is `UntilEndOfTurn`. Port it rather than delete it:

  ```haskell
        HU.testCase "CR 701.19a / 514.2 a regeneration shield is dropped at cleanup (this turn)" $
          let base = Setup.emptyGame S.bothPlayers
              (oid, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice base
              cleared = Event.dropEndOfTurnReplacements (S.addRegenShield oid gs0)
           in HU.assertEqual "no shields remain" [] (GameState.replacements cleared),
  ```
- `EventSpec.hs:140-160` — the "consumes a shield and regenerates instead" and "second destroy kills it" cases: keep the assertions about the battlefield, tapped state and damage; replace `Map.lookup oid (GameState.regenerationShields after)` with `HU.assertEqual "shield spent" [] (GameState.replacements after)`.
- `ActivateSpec.hs:144` — `HU.assertEqual "a shield is up after resolving the ability" 1 (length (GameState.replacements resolved))`.
- `TriggerSpec.hs:266-270` — `HU.assertEqual "the shield is untouched" 1 (length (GameState.replacements after))`.

- [x] **Step 10: Build and run**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -20`
Expected: PASS. §5 scenarios 18, 19, 20 green; `grep -rn "regenerationShields\|RegenerateSelf" source/` returns nothing.

- [x] **Step 11: Commit**

```bash
git add source/library/Pawl source/test-suite/Pawl data/cards/drudge-skeletons.json
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p5): regeneration folds into the one replacement path

GameState.regenerationShields and Effect.RegenerateSelf are gone; a
shield is Replace UntilEndOfTurn Once (DestructionR Regenerate). The
nesting Event.destroy used to hardcode -- regeneration wins, so the
put-into-graveyard never happens and Rest in Peace never applies -- is
now structural (CR 614.8), and CR 614.7's untouched shield falls out of
the indestructible gate preceding the loop.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 6: Counters — the two-replacement race (§5 scenarios 1–7)

The gate. Hardened Scales and Corpsejack Menace apply to the *same* event with *different* rewrites, so the outcome depends on the order and the engine must ask.

**Files:**
- Modify: `source/library/Pawl/Type/Subtype.hs` (add `Fungus`), `source/library/Pawl/Codec.hs` (two subtype tables)
- Modify: `source/library/Pawl/Event.hs` (add `putCounters`), `Pawl/Replacement.hs`, `Pawl/Resolve.hs` (the `PutCounters` arm; delete the pure `putCounters` helper)
- Create: `data/cards/hardened-scales.json`, `data/cards/corpsejack-menace.json`
- Modify: `source/test-suite/Pawl/Cards.hs` (two printings)
- Test: `source/test-suite/Pawl/ReplacementSpec.hs`

**Interfaces:**
- Consumes: the loop and `Replacement.consume` (Tasks 3–5).
- Produces: `Event.putCounters :: ObjectId -> CounterKind -> Natural -> Game ()`; `Replacement.resolveCounters :: ObjectId -> CounterKind -> Natural -> Game (Maybe (ObjectId, CounterKind, Natural))`; `Cards.hardenedScalesPrinting`, `Cards.corpsejackMenacePrinting`.

- [x] **Step 1: Write the failing tests**

Add these helpers and seven cases to `source/test-suite/Pawl/ReplacementSpec.hs`:

```haskell
-- alice controls one Forest plus `mine`; bob controls `theirs`; alice holds one
-- Battlegrowth ({G} instant: put a +1/+1 counter on target creature). Returns the
-- state, Battlegrowth's hand id, and the two id lists in the order given.
counterBoard :: Cards.Cards -> [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId], [ObjectId.ObjectId])
counterBoard cards mine theirs =
  let addAll pid ps gs =
        List.foldl'
          (\(ids, g) p -> let (oid, g1) = S.addCreature p pid g in (ids ++ [oid], g1))
          ([], gs)
          ps
      (ours, gs1) = addAll S.alice mine (S.landsInPlay (Cards.forestPrinting cards) 1)
      (yours, gs2) = addAll S.bob theirs gs1
      (gs3, spellId) = S.handOne (Cards.battlegrowthPrinting cards) gs2
   in (gs3, spellId, ours, yours)

-- Aim every target slot at `victim`, and answer a CR 616.1 race by picking the
-- candidate whose SOURCE is `preferred` -- by id, so the assertion does not
-- depend on the engine's canonical candidate order.
raceAnswer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
raceAnswer preferred victim p = case p of
  Prompt.ChooseReplacement _ _ sources -> maybe 0 fromIntegral (List.elemIndex preferred sources)
  Prompt.ChooseTargets _ _ _ sets -> Map.map (const (Recipient.ToCreature victim)) sets
  _ -> S.identityAnswer p

countersOn :: CounterKind.CounterKind -> ObjectId.ObjectId -> GameState.GameState -> Natural
countersOn kind oid gs =
  maybe 0 (Map.findWithDefault 0 kind . Object.counters) (Game.lookupObject oid gs)

castAndResolve :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
castAndResolve answer gs spellId =
  S.runPure answer gs (Cast.castSpell S.alice spellId >> Stack.resolveTop)
```

```haskell
      HU.testCase "CR 616.1 Scales first, then Corpsejack: 1 -> 2 -> 4" $
        let (gs, spellId, mine, _) = counterBoard cards [Cards.hardenedScalesPrinting cards, Cards.corpsejackMenacePrinting cards, Cards.pikerPrinting cards] []
         in case mine of
              scales : _ : piker : _ ->
                let after = castAndResolve (raceAnswer scales piker) gs spellId
                 in HU.assertEqual "(1 + 1) * 2" 4 (countersOn CounterKind.PlusOnePlusOne piker after)
              _ -> HU.assertFailure "fixture did not build three permanents",
      HU.testCase "CR 616.1 Corpsejack first, then Scales: 1 -> 2 -> 3 (same input, different board)" $
        let (gs, spellId, mine, _) = counterBoard cards [Cards.hardenedScalesPrinting cards, Cards.corpsejackMenacePrinting cards, Cards.pikerPrinting cards] []
         in case mine of
              _ : corpsejack : piker : _ ->
                let after = castAndResolve (raceAnswer corpsejack piker) gs spellId
                 in HU.assertEqual "(1 * 2) + 1" 3 (countersOn CounterKind.PlusOnePlusOne piker after)
              _ -> HU.assertFailure "fixture did not build three permanents",
      HU.testCase "CR 616.1 the engine ASKS -- it does not proceed on list order" $
        let (gs, spellId, mine, _) = counterBoard cards [Cards.hardenedScalesPrinting cards, Cards.corpsejackMenacePrinting cards, Cards.pikerPrinting cards] []
         in case mine of
              scales : _ : piker : _ ->
                let asked = answersFor (raceAnswer scales piker) gs (Cast.castSpell S.alice spellId >> Stack.resolveTop)
                 in HU.assertBool "a ChooseReplacement was raised" (wasAskedToReplace asked)
              _ -> HU.assertFailure "fixture did not build three permanents",
      HU.testCase "CR 616.1 one Hardened Scales alone is not asked about (nothing to choose)" $
        let (gs, spellId, mine, _) = counterBoard cards [Cards.hardenedScalesPrinting cards, Cards.pikerPrinting cards] []
         in case mine of
              scales : piker : _ ->
                let after = castAndResolve (raceAnswer scales piker) gs spellId
                    asked = answersFor (raceAnswer scales piker) gs (Cast.castSpell S.alice spellId >> Stack.resolveTop)
                 in do
                      HU.assertEqual "1 + 1" 2 (countersOn CounterKind.PlusOnePlusOne piker after)
                      HU.assertBool "no ChooseReplacement was raised" (not (wasAskedToReplace asked))
              _ -> HU.assertFailure "fixture did not build two permanents",
      HU.testCase "CR 614.5 two Hardened Scales are two instances: 1 -> 2 -> 3, unprompted" $
        let (gs, spellId, mine, _) = counterBoard cards [Cards.hardenedScalesPrinting cards, Cards.hardenedScalesPrinting cards, Cards.pikerPrinting cards] []
         in case mine of
              scales : _ : piker : _ ->
                let after = castAndResolve (raceAnswer scales piker) gs spellId
                    asked = answersFor (raceAnswer scales piker) gs (Cast.castSpell S.alice spellId >> Stack.resolveTop)
                 in do
                      HU.assertEqual "each gets its own opportunity" 3 (countersOn CounterKind.PlusOnePlusOne piker after)
                      HU.assertBool "value-equal candidates elide the prompt" (not (wasAskedToReplace asked))
              _ -> HU.assertFailure "fixture did not build three permanents",
      HU.testCase "CR 614.1 Hardened Scales ignores a -1/-1 counter (whichKind)" $
        let base = S.landsInPlay (Cards.swampPrinting cards) 4
            (scales, g1) = S.addCreature (Cards.hardenedScalesPrinting cards) S.alice base
            (piker, g2) = S.addCreature (Cards.pikerPrinting cards) S.alice g1
            (g3, spellId) = S.handOne (Cards.instillInfectionPrinting cards) g2
            after = castAndResolve (raceAnswer scales piker) g3 spellId
         in HU.assertEqual "one -1/-1 counter, unscaled" 1 (countersOn CounterKind.MinusOneMinusOne piker after),
      HU.testCase "CR 109.5 Corpsejack Menace does not double an opponent's counters" $
        let (gs, spellId, mine, theirs) = counterBoard cards [Cards.corpsejackMenacePrinting cards] [Cards.pikerPrinting cards]
         in case (mine, theirs) of
              (corpsejack : _, piker : _) ->
                let after = castAndResolve (raceAnswer corpsejack piker) gs spellId
                 in HU.assertEqual "not doubled -- ControllerRelation is Yours" 1 (countersOn CounterKind.PlusOnePlusOne piker after)
              _ -> HU.assertFailure "fixture did not build both sides",
```

Note the -1/-1 case needs four Swamps (Instill Infection is `{3}{B}`) rather than the Forest fixture; that is why it builds its board inline.

- [x] **Step 2: Run to watch it fail**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20`
Expected: FAIL — `Cards.hardenedScalesPrinting` is not in scope.

- [x] **Step 3: Add the `Fungus` subtype**

`source/library/Pawl/Type/Subtype.hs` — add `| Fungus -- CR 205.3m (a creature type; Corpsejack Menace's)` at the end. `source/library/Pawl/Codec.hs` — add `Subtype.Fungus -> "Fungus"` to `subtypeToJson` and `(Text.pack "Fungus", Subtype.Fungus)` to `jsonToSubtype`.

- [x] **Step 4: Write the two card files**

`data/cards/hardened-scales.json` (one line, no trailing newline issues — match the existing files' formatting exactly):

```json
{"name":"Hardened Scales","manaCost":[{"type":"OfType","value":{"type":"Colored","value":{"type":"Green"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Enchantment"}],"subtypes":[]},"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[{"type":"CounterR","value":[{"whichKind":{"type":"PlusOnePlusOne"},"whose":{"type":"Yours"},"onWhat":{"type":"CreaturePermanent"}},{"type":"AddMore","value":1}]}],"triggeredAbilities":[],"castingPermissions":[]}
```

`data/cards/corpsejack-menace.json`:

```json
{"name":"Corpsejack Menace","manaCost":[{"type":"Generic","value":2},{"type":"OfType","value":{"type":"Colored","value":{"type":"Black"}}},{"type":"OfType","value":{"type":"Colored","value":{"type":"Green"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Creature"}],"subtypes":[{"type":"Fungus"}]},"power":{"type":"Literal","value":4},"toughness":{"type":"Literal","value":4},"keywords":[],"staticAbilities":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[{"type":"CounterR","value":[{"whichKind":{"type":"PlusOnePlusOne"},"whose":{"type":"Yours"},"onWhat":{"type":"CreaturePermanent"}},{"type":"Multiply","value":2}]}],"triggeredAbilities":[],"castingPermissions":[]}
```

Add `hardenedScalesPrinting` and `corpsejackMenacePrinting` fields to `Pawl.Cards`'s `MkCards` record, `loadPrinting "hardened-scales"` / `"corpsejack-menace"` to `loadCards`, and both to `allPrintings`. Do **not** add either to a deck — `CardSpec`'s slug lint requires the directory and `allPrintings` to agree, and `PropertySpec`'s conservation counts require the decks to stay at 60.

- [x] **Step 5: Add the counter funnel and its loop arms**

`source/library/Pawl/Replacement.hs` — add to `applies`:

```haskell
        (ReplacementEffect.CounterR pat _, ProposedEvent.WouldPutCounters oid kind _) ->
          -- CR 614.1: `whichKind = Nothing` means any kind, never no kind.
          maybe True (== kind) (CounterPattern.whichKind pat)
            && matchesController gs src (CounterPattern.whose pat) oid
            && matchesPermanent gs (CounterPattern.onWhat pat) oid
```

to `apply`:

```haskell
    (ReplacementEffect.CounterR _ scaling, ProposedEvent.WouldPutCounters oid kind n) -> do
      consume (ReplacementCandidate.identity candidate)
      pure (Just (ProposedEvent.WouldPutCounters oid kind (scale scaling n)))
```

and the two helpers plus the typed door:

```haskell
-- CR 614.1: which permanents a pattern admits. P9 generalizes this.
matchesPermanent :: GameState -> PermanentCriterion.PermanentCriterion -> ObjectId -> Bool
matchesPermanent gs crit oid = case crit of
  PermanentCriterion.AnyPermanent -> True
  -- CR 305.2 / 613.1d: creature-ness is the PROJECTED question, so an
  -- Opalescence'd enchantment counts.
  PermanentCriterion.CreaturePermanent -> Projection.isCreatureOf oid gs

-- CR 614.1: apply a scaling to a count. "That many plus one" and "twice that
-- many" are the same operation with different data.
scale :: Scaling.Scaling -> Natural -> Natural
scale s n = case s of
  Scaling.Multiply m -> n * m
  Scaling.AddMore m -> n + m

-- CR 122.6: settle a proposed counter placement. Nothing means none are put on.
resolveCounters :: ObjectId -> CounterKind.CounterKind -> Natural -> Game (Maybe (ObjectId, CounterKind.CounterKind, Natural))
resolveCounters oid kind n = do
  outcome <- applyReplacements (ProposedEvent.WouldPutCounters oid kind n)
  pure (outcome >>= asCounters)

asCounters :: ProposedEvent -> Maybe (ObjectId, CounterKind.CounterKind, Natural)
asCounters event = case event of
  ProposedEvent.WouldPutCounters oid kind n -> Just (oid, kind, n)
  ProposedEvent.WouldChangeZone _ -> Nothing
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldBeDestroyed _ -> Nothing
  ProposedEvent.WouldCreateTokens {} -> Nothing
```

`source/library/Pawl/Event.hs` — add the funnel next to `destroy`:

```haskell
-- The single counter funnel (CR 122.6). Before P5 the PutCounters opcode edited
-- Object.counters in place with no funnel at all, so there was nothing for a
-- replacement to intercept.
--
-- CR 122.6 makes this the right single seam: "Some spells and abilities refer to
-- counters being put on an object. This refers to putting counters on that object
-- while it's on the battlefield and also to an object that's given counters as it
-- enters the battlefield." A zero count after the loop puts nothing on.
putCounters :: ObjectId -> CounterKind.CounterKind -> Natural -> Game ()
putCounters oid kind n = do
  resolved <- Replacement.resolveCounters oid kind n
  case resolved of
    Nothing -> pure ()
    Just (target, settledKind, settledCount) ->
      Monad.when (settledCount > 0) $
        State.modify' $ \gs ->
          let bump obj = obj {Object.counters = Map.insertWith (+) settledKind settledCount (Object.counters obj)}
           in gs {GameState.objects = Map.adjust bump target (GameState.objects gs)}
```

`source/library/Pawl/Resolve.hs` — delete the pure `putCounters` helper (lines 682–686) and rewrite the executor arm:

```haskell
  Effect.PutCounters kind quantity slot -> do
    gs <- State.get
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> case recipientObject recipient of
        Nothing -> pure () -- a player recipient takes no counters
        Just target -> case Quantity.evaluate gs source (Just controller) quantity of
          Nothing -> pure () -- unevaluable quantity: no-op (the powerOf posture)
          -- CR 122.6: through the single funnel, so CR 614's counter replacements
          -- (Hardened Scales, Doubling Season) get their opportunity.
          Just n -> Monad.when (n > 0) (Event.putCounters target kind (fromInteger n))
      _ -> pure () -- illegal slot at resolution (CR 608.2b): no-op
```

- [x] **Step 6: Run the seven scenarios**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "Pawl.Replacement"' 2>&1 | tail -30`
Expected: PASS, all seven.

- [x] **Step 7: Run the whole suite**

Run: `cabal test 2>&1 | tail -20`
Expected: PASS. In particular the existing `+1/+1`/`-1/-1` counter tests in `ResolveSpec` and the CR 704.5q annihilation SBA are unchanged — with no replacement source on the board the loop is a no-op that adds one short-circuited `replacementsAffecting` call.

- [x] **Step 8: Commit**

```bash
git add source/library/Pawl data/cards/hardened-scales.json data/cards/corpsejack-menace.json source/test-suite/Pawl pawl.cabal
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p5): the counter funnel and the two-replacement race

Event.putCounters is the CR 122.6 seam PutCounters never had. Hardened
Scales and Corpsejack Menace apply to the same event with different
rewrites, so the board depends on the order -- and the engine asks
instead of inventing one. CR 614.5 keeps the mutual re-fire from
diverging, per instance rather than per value.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 7: The entry loop and the copy fold — `WouldEnter`, `EntryR AsCopy`, CR 614.13a

P2's mark-then-drain seam is retired. The copy choice now happens *inside* the zone change, before the `Moved` event exists — so no trigger scan and no state-based action can observe the interim object, and P2's observable-equivalence argument is discharged rather than re-inherited.

**Files:**
- Modify: `source/library/Pawl/Replacement.hs` (batch parameter, `runEntry`, `legalCopyTargets`, `EntryR AsCopy`)
- Modify: `source/library/Pawl/Event.hs` (`changeZone` runs the entry loop; delete `markCopyOnEnter`, `cardOfObject`)
- Modify: `source/library/Pawl/Engine.hs` (delete `drainAsEntersChoices`, `drainOneCopy`, `applyCopyChoice`; `settleForPriority` drops `drained`)
- Modify: `source/library/Pawl/Binding.hs` (delete `asEntersPending`, `markPending`, `clearPending`, `pendingCopy`)
- Modify: `source/library/Pawl/Target.hs` (delete `legalCopyTargets`)
- Modify: `source/library/Pawl/Type/Card.hs` (delete `copyOnEnter`), `source/library/Pawl/Codec.hs`, `source/library/Pawl/Type/Prompt.hs` (`ChooseCopyTarget`'s comment)
- Modify: `data/cards/clone.json`
- Test: `CopySpec.hs`, `BindingSpec.hs:60-65`, `CardsSpec.hs:29-32`, `CodecSpec.hs:228-231`, `CardSpec.hs:153`, `ResolveSpec.hs:238`, `ReplacementSpec.hs` (scenario 16)

**Interfaces:**
- Consumes: the loop (Task 3).
- Produces: `Replacement.applyReplacementsIn :: Set ObjectId -> ProposedEvent -> Game (Maybe ProposedEvent)`; `Replacement.runEntry :: Set ObjectId -> ObjectId -> Game ()`; `Replacement.legalCopyTargets :: Set ObjectId -> ObjectId -> GameState -> [ObjectId]`.

- [x] **Step 1: Write the failing test (§5 scenario 16)**

Add to `source/test-suite/Pawl/ReplacementSpec.hs`:

```haskell
      HU.testCase "CR 707.5 declining the copy leaves a 0/0 that dies (CR 704.5f)" $
        let base = S.landsInPlay (Cards.islandPrinting cards) 4
            (_, withPiker) = S.addPiker cards S.alice base
            (gs, cloneId) = S.handOne (Cards.clonePrinting cards) withPiker
            -- S.identityAnswer declines ChooseCopyTarget (Clone's own "may").
            resolved = S.runPure S.identityAnswer gs (Cast.castSpell S.alice cloneId >> Stack.resolveTop >> Engine.settleForPriority)
            named = filter (\oid -> fmap Card.Type.name (Game.cardOf oid resolved) == Just (Text.pack "Clone")) (Set.toList (GameState.battlefield resolved))
         in HU.assertEqual "the 0/0 Clone is gone" [] named,
      HU.testCase "CR 614.12a the copy choice is locked in BEFORE the enters event exists" $
        let base = S.landsInPlay (Cards.islandPrinting cards) 4
            (piker, withPiker) = S.addPiker cards S.alice base
            (gs, cloneId) = S.handOne (Cards.clonePrinting cards) withPiker
            -- No settle: the choice must already be made when resolveTop returns.
            resolved = S.runPure (copyOf piker) gs (Cast.castSpell S.alice cloneId >> Stack.resolveTop)
            named = filter (\oid -> fmap Card.Type.name (Game.cardOf oid resolved) == Just (Text.pack "Clone")) (Set.toList (GameState.battlefield resolved))
         in case named of
              [] -> HU.assertFailure "Clone did not reach the battlefield"
              clone : _ -> HU.assertEqual "already a 2/1, with no settle run" (Just 2) (Projection.powerOf clone resolved)
```

with the answerer

```haskell
-- Copy `wanted` when it is offered, decline otherwise.
copyOf :: ObjectId.ObjectId -> Prompt.Prompt r -> r
copyOf wanted p = case p of
  Prompt.ChooseCopyTarget _ _ _ legal -> if List.elem wanted legal then Just wanted else Nothing
  _ -> S.identityAnswer p
```

- [x] **Step 2: Run to watch it fail**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test --test-options='-p "before the enters event"' 2>&1 | tail -20`
Expected: FAIL — the Clone is still a 0/0 (`Just 0`), because the choice is drained at the settle boundary, not at entry.

- [x] **Step 3: Thread the CR 614.13a batch through the loop**

`source/library/Pawl/Replacement.hs` — replace the entry point and give `loop` and `apply` the batch:

```haskell
applyReplacements :: ProposedEvent -> Game (Maybe ProposedEvent)
applyReplacements = applyReplacementsIn Set.empty

-- CR 614.13a: `batch` is the set of ids entering the battlefield AT THE SAME TIME
-- as the object this loop is about -- "You can't choose the object that will
-- become that permanent or any other object entering the battlefield at the same
-- time as that object." Clone's own ruling restates it: "If Clone somehow enters
-- at the same time as another creature, Clone can't become a copy of that
-- creature."
--
-- Empty for every event class but a nested entry, and empty even for a lone entry
-- (nothing else is entering). IMPLEMENTED BUT UNTESTED: no real card in reach
-- puts two copy-choosers onto the battlefield simultaneously (#N).
applyReplacementsIn :: Set ObjectId -> ProposedEvent -> Game (Maybe ProposedEvent)
applyReplacementsIn batch = loop batch Set.empty
```

`loop` gains `batch` as its first argument and passes it to `apply` and to its own recursive call. `apply` gains it as its first argument.

- [x] **Step 4: Make `EntryR AsCopy` a real replacement**

`source/library/Pawl/Replacement.hs`:

`bucketOf` — split the `EntryR` arm:

```haskell
  -- CR 616.1c: "an effect that would cause an object to become a copy of another
  -- object as it enters" is its own, HIGHER bucket. That is what makes the
  -- centerpiece work: a Clone's copy applies before the Primal Plasma choice it
  -- thereby acquires (616.1e), not after.
  ReplacementEffect.EntryR EntryRewrite.AsCopy -> ReplacementBucket.CopyOnEntry
  ReplacementEffect.EntryR (EntryRewrite.ChoiceOf _) -> ReplacementBucket.Other
```

`applies` — replace the `EntryR` catch-all with the self-only rule:

```haskell
        -- CR 614.1c: "as [this permanent] enters" is the entering object's OWN
        -- ability, so an entry replacement is self-only. CR 614.1d's
        -- other-objects form (Essence of the Wild) has no producer.
        (ReplacementEffect.EntryR _, ProposedEvent.WouldEnter oid) -> src == oid
```

`apply` — the copy arm:

```haskell
    -- CR 707.5 / 614.1c / 614.12a: the entering object's controller chooses a
    -- permanent to copy, and its copiable characteristics are stamped as this
    -- object's copy snapshot. Writing to the COPIABLE layer (CR 613.1a's layer-1
    -- base) is what makes CR 707.2 fall out for free: a later Clone of this object
    -- copies the stamped values with no further machinery.
    --
    -- Clone's "may" is real: Nothing leaves the object as its printed self (a
    -- 0/0 Shapeshifter, which CR 704.5f then buries).
    (ReplacementEffect.EntryR EntryRewrite.AsCopy, ProposedEvent.WouldEnter oid) -> do
      gs <- State.get
      case Projection.controllerOf oid gs of
        -- Unreachable: the object is materialized on the battlefield before this
        -- loop runs, so controllerOf falls back to its owner. Defensive: make no
        -- unprompted copy choice.
        Nothing -> pure (Just event)
        Just controller -> do
          let decider = Decide.deciderFor controller gs
          chosen <- Trans.lift (Program.prompt (Prompt.ChooseCopyTarget decider controller oid (legalCopyTargets batch oid gs)))
          case chosen of
            Nothing -> pure (Just event)
            Just src2 -> do
              State.modify' $ \g ->
                let stamp o = o {Object.bindings = Binding.setCopy (Projection.copiableCharacteristics src2 g) (Object.bindings o)}
                 in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
              pure (Just event)
```

and the legal set, moved down from `Pawl.Target` (which imports `Pawl.Sba`, which imports `Pawl.Event`, which imports this module — so it cannot live there any more):

```haskell
-- CR 707.5 / 614.13a: the permanents an entering copy may choose. Battlefield
-- creatures other than itself, minus anything entering in the same batch.
legalCopyTargets :: Set ObjectId -> ObjectId -> GameState -> [ObjectId]
legalCopyTargets batch self gs =
  let eligible oid = oid /= self && not (Set.member oid batch) && Projection.isCreatureOf oid gs
   in filter eligible (Set.toAscList (GameState.battlefield gs))
```

plus the entry door:

```haskell
-- CR 614.1c / 614.12: run the entry loop for an object that has just been
-- materialized on the battlefield.
--
-- The object is in GameState.objects and its zone index BEFORE this runs, because
-- CR 614.12 demands it: "check the characteristics of the permanent AS IT WOULD
-- EXIST ON THE BATTLEFIELD, taking into account replacement effects that have
-- already modified how it enters ... and continuous effects that already exist and
-- would apply to the permanent." That is a projection of the object in the state
-- where it has entered, so the cheapest correct implementation is to put it there
-- and project it normally.
--
-- Nothing observes the interim object: this finishes before the Moved event is
-- recorded, so no trigger scan and no state-based action can see it. That is
-- strictly stronger than P2's drain, whose observable-equivalence argument this
-- discharges.
runEntry :: Set ObjectId -> ObjectId -> Game ()
runEntry batch oid = Monad.void (applyReplacementsIn batch (ProposedEvent.WouldEnter oid))
```

- [x] **Step 5: Run the entry loop from `changeZone`, and delete the marker**

`source/library/Pawl/Event.hs` — delete `markCopyOnEnter` and `cardOfObject`; `placeObject`'s `obj` binding becomes `obj = mkObj ts`. In `changeZone`, insert the entry loop between `placeObject` and `recordEvent`:

```haskell
        Just settled -> do
          let dest = ZoneChange.to settled
              mkObj ts = obj {Object.zone = dest, Object.tapped = TapState.Untapped, Object.damage = 0, Object.sickness = Sickness.Sick, Object.bindings = Map.empty, Object.counters = Map.empty, Object.timestamp = ts}
          State.modify' $ \g ->
            let g1 = Game.removeFromZones pid oid g
             in g1 {GameState.objects = Map.delete oid (GameState.objects g1)}
          newId <- placeObject pid mkObj dest
          -- CR 614.1c-d: entry replacements apply to BATTLEFIELD entries and
          -- nowhere else. CR 616.1g: this loop is NESTED inside the zone change,
          -- which is how "an effect may apply to an event contained within another
          -- event" is expressed -- as call nesting, not as a field. A lone entry
          -- has no same-batch siblings (CR 614.13a).
          Monad.when (dest == Zone.Battlefield) (Replacement.runEntry Set.empty newId)
          -- CR 603.2g: record the RESOLVED event, carrying the NEW object's id --
          -- what an enters trigger scans. Recorded LAST, so the entry loop's
          -- choices are locked in before any trigger or SBA can observe the object.
          State.modify' (recordEvent (GameEvent.Moved (ZoneChange.MkZoneChange newId fromZone dest) snapshot))
```

- [x] **Step 6: Retire the drain and the pending marker**

`source/library/Pawl/Engine.hs` — delete `drainAsEntersChoices`, `drainOneCopy` and `applyCopyChoice` (lines 342–379), and simplify `settleForPriority`:

```haskell
-- CR 117.5: each time a player would receive priority, perform state-based
-- actions, then put triggered abilities on the stack, repeating until neither does
-- anything. Then priority is granted (by the caller). The repeat is gated on two
-- cheap booleans -- whether an SBA fired and whether a trigger was placed -- so a
-- settle that changes nothing (the common case) costs one board projection, NOT a
-- deep GameState equality check.
--
-- P5 removed the third gate. The as-enters copy drain used to run here, first,
-- because a copied permanent's characteristics had to be locked in before any SBA
-- or trigger observed it (CR 614.12a). The entry loop now runs inside the zone
-- change itself, before the Moved event exists, so there is nothing left to drain.
settleForPriority :: Game ()
settleForPriority = do
  acted <- Sba.performStateBasedActions
  placed <- placePendingTriggers
  Monad.when (acted || placed) settleForPriority
```

`source/library/Pawl/Binding.hs` — delete `asEntersPending`, `markPending`, `clearPending` and `pendingCopy`. Keep `copySource`, `copyOf` and `setCopy`.

`source/library/Pawl/Target.hs` — delete `legalCopyTargets` and any imports it alone needed.

`source/library/Pawl/Type/Prompt.hs` — update `ChooseCopyTarget`'s comment: replace "Answered at the settle boundary (P2 drain), not at cast -- the choice is made as the object enters." with "Answered inside the zone change that puts the object onto the battlefield (CR 614.12a), before the enters event is recorded -- the choice really is made as the object enters. The legal set excludes anything entering in the same batch (CR 614.13a)."

- [x] **Step 7: Delete `Card.copyOnEnter` and migrate `clone.json`**

`source/library/Pawl/Type/Card.hs` — delete the `copyOnEnter` field and its comment. A card-specific boolean on the card type was the closest thing in the engine to a fused half.

`source/library/Pawl/Codec.hs` — delete the `copyOnEnter` encode clause from `cardToJson` and its binding in `jsonToCard`.

`data/cards/clone.json` — drop `,"copyOnEnter":true` and change `"replacementEffects":[]` to:

```json
"replacementEffects":[{"type":"EntryR","value":{"type":"AsCopy"}}]
```

- [x] **Step 8: Update the five tests that name the retired machinery**

- `CopySpec.hs:66-78` — delete the "marked as-enters-pending" case outright (the marker no longer exists; the replacement assertion is ReplacementSpec's "locked in BEFORE the enters event exists"). Every other CopySpec case keeps its assertions **unchanged** — they are the P2 gate moving onto the new path, and any change to them is a regression, not a landing.
- `CopySpec.hs:58-60` — `resolveAndSettle` still works verbatim (the settle is now a no-op for the copy, but the P2 tests also rely on it for CR 704.5f).
- `BindingSpec.hs:60-65` — delete the three `markPending`/`clearPending`/`pendingCopy` cases.
- `CardsSpec.hs:29-32` — retitle to "clone.json loads as a 0/0 Shapeshifter with an EntryR AsCopy" and assert `HU.assertEqual "entry replacement" [ReplacementEffect.EntryR EntryRewrite.AsCopy] (CardT.replacementEffects c)`.
- `CodecSpec.hs:228-231` — delete the `copyOnEnter` round-trip case.
- `CardSpec.hs:153` and `ResolveSpec.hs:238` — delete the `Card.Type.copyOnEnter = False,` line from each hand-built card record.

- [x] **Step 9: Build and run**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -20`
Expected: PASS. `grep -rn "copyOnEnter\|drainAsEntersChoices\|pendingCopy\|asEntersPending" source/ data/` returns nothing.

- [x] **Step 10: Commit**

```bash
git add source/library/Pawl data/cards/clone.json source/test-suite/Pawl pawl.cabal
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p5): the entry loop retires the as-enters drain

Clone is EntryR AsCopy in its card data; Card.copyOnEnter, the pending
Binding marker and Engine.drainAsEntersChoices are gone. The choice is
made inside the zone change (CR 614.12/614.12a), before the Moved event
exists, so no trigger scan or SBA can see the interim object -- P2's
observable-equivalence argument is discharged, not re-inherited.
CR 614.13a's same-batch exclusion is implemented (untested, #N).

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 8: `ChoiceOf` and Primal Plasma — the centerpiece (§5 scenarios 12–15)

A Clone entering as a copy of a 2/2-flying Primal Plasma must end up **1/6 with flying and defender** if its controller picks the third option. Getting there requires applying the copy first (CR 616.1c's bucket), observing that the object *now has* an ability it did not have an iteration ago, recognising it as newly applicable (CR 616.2), prompting for it, and letting it overwrite P/T while leaving the copied keyword alone. No implementation that computes its candidate list once can produce that board.

**Files:**
- Modify: `source/library/Pawl/Type/Prompt.hs`, `Response.hs`, `source/library/Pawl/Replay.hs`, `source/test-suite/Pawl/Support.hs` (five answerers)
- Modify: `source/library/Pawl/Replacement.hs` (the `ChoiceOf` arm and `applyEntryOption`)
- Modify: `source/library/Pawl/Type/Subtype.hs` (add `Elemental`), `source/library/Pawl/Codec.hs`
- Modify: `source/library/Pawl/Projection.hs` (the `baseCharacteristics` note about `*` P/T is now false)
- Create: `data/cards/primal-plasma.json`; modify `source/test-suite/Pawl/Cards.hs`
- Test: `source/test-suite/Pawl/ReplacementSpec.hs`

**Interfaces:**
- Consumes: `Replacement.runEntry`, `legalCopyTargets`, the batch parameter (Task 7).
- Produces: `Prompt.ChooseEntryOption :: Decider -> PlayerId -> ObjectId -> [EntryOption] -> Prompt Natural`; `Response.ChoseEntryOption Natural`; `Cards.primalPlasmaPrinting`.

- [x] **Step 1: Write the failing tests**

Add to `source/test-suite/Pawl/ReplacementSpec.hs`:

```haskell
-- alice controls `n` untapped Islands in a main phase with priority, holding one
-- card of each printing in `hand`. Returns the state and the hand ids in order --
-- unlike S.handOne, which replaces the whole hand.
blueBoard :: Cards.Cards -> Int -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId])
blueBoard cards n hand =
  let base = S.landsInPlay (Cards.islandPrinting cards) n
      addOne (ids, g) p = let (oid, g1) = S.addHandCard p S.alice g in (ids ++ [oid], g1)
      (held, gs) = List.foldl' addOne ([], base) hand
   in ( gs
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        held
      )

-- Pick entry option `which`, and copy the highest-id legal creature when offered.
enteringAs :: Natural -> Prompt.Prompt r -> r
enteringAs which p = case p of
  Prompt.ChooseEntryOption {} -> which
  Prompt.ChooseCopyTarget _ _ _ legal -> Maybe.listToMaybe (List.sortOn Ord.Down legal)
  _ -> S.identityAnswer p

-- The newest battlefield object whose printed card has this name.
newestNamed :: Text.Text -> GameState.GameState -> Maybe ObjectId.ObjectId
newestNamed wanted gs =
  let named oid = fmap Card.Type.name (Game.cardOf oid gs) == Just wanted
   in Maybe.listToMaybe (List.sortOn Ord.Down (filter named (Set.toList (GameState.battlefield gs))))
```

```haskell
      HU.testCase "CR 208.2b Primal Plasma enters as the 2/2 with flying its controller picked" $
        let (gs, held) = blueBoard cards 4 [Cards.primalPlasmaPrinting cards]
         in case held of
              plasmaCard : _ ->
                let after = S.runPure (enteringAs 1) gs (Cast.castSpell S.alice plasmaCard >> Stack.resolveTop)
                 in case newestNamed (Text.pack "Primal Plasma") after of
                      Nothing -> HU.assertFailure "Primal Plasma did not reach the battlefield"
                      Just plasma -> do
                        HU.assertEqual "power" (Just 2) (Projection.powerOf plasma after)
                        HU.assertEqual "toughness" (Just 2) (Projection.toughnessOf plasma after)
                        HU.assertBool "flying" (Projection.hasKeyword Keyword.Flying plasma after)
              _ -> HU.assertFailure "fixture did not deal a card",
      HU.testCase "CR 616.2 a Clone of a 2/2-flying Plasma that picks 1/6 is 1/6 with flying AND defender" $
        -- THE CENTERPIECE, and the Gatherer ruling verbatim: "it copies the values
        -- determined by its enters-the-battlefield replacement effect, but its
        -- power and toughness are determined by the copy's own
        -- enters-the-battlefield replacement effect."
        let (gs, held) = blueBoard cards 8 [Cards.primalPlasmaPrinting cards, Cards.clonePrinting cards]
         in case held of
              plasmaCard : cloneCard : _ ->
                let withPlasma = S.runPure (enteringAs 1) gs (Cast.castSpell S.alice plasmaCard >> Stack.resolveTop)
                    after = S.runPure (enteringAs 2) withPlasma (Cast.castSpell S.alice cloneCard >> Stack.resolveTop)
                 in case newestNamed (Text.pack "Clone") after of
                      Nothing -> HU.assertFailure "Clone did not reach the battlefield"
                      Just clone -> do
                        HU.assertEqual "power is the CLONE's own choice" (Just 1) (Projection.powerOf clone after)
                        HU.assertEqual "toughness is the CLONE's own choice" (Just 6) (Projection.toughnessOf clone after)
                        HU.assertBool "flying came from the COPY" (Projection.hasKeyword Keyword.Flying clone after)
                        HU.assertBool "defender came from the CHOICE" (Projection.hasKeyword Keyword.Defender clone after)
              _ -> HU.assertFailure "fixture did not deal two cards",
      HU.testCase "CR 616.2 the same Clone picking 3/3 is a 3/3 with flying" $
        let (gs, held) = blueBoard cards 8 [Cards.primalPlasmaPrinting cards, Cards.clonePrinting cards]
         in case held of
              plasmaCard : cloneCard : _ ->
                let withPlasma = S.runPure (enteringAs 1) gs (Cast.castSpell S.alice plasmaCard >> Stack.resolveTop)
                    after = S.runPure (enteringAs 0) withPlasma (Cast.castSpell S.alice cloneCard >> Stack.resolveTop)
                 in case newestNamed (Text.pack "Clone") after of
                      Nothing -> HU.assertFailure "Clone did not reach the battlefield"
                      Just clone -> do
                        HU.assertEqual "3/3" (Just 3) (Projection.powerOf clone after)
                        HU.assertBool "still flying (keywords UNION, never assign)" (Projection.hasKeyword Keyword.Flying clone after)
                        HU.assertBool "no defender" (not (Projection.hasKeyword Keyword.Defender clone after))
              _ -> HU.assertFailure "fixture did not deal two cards",
      HU.testCase "CR 707.2 a Clone of that Clone copies 1/6-flying-defender and then chooses again" $
        let (gs, held) = blueBoard cards 12 [Cards.primalPlasmaPrinting cards, Cards.clonePrinting cards, Cards.clonePrinting cards]
         in case held of
              plasmaCard : cloneA : cloneB : _ ->
                let s1 = S.runPure (enteringAs 1) gs (Cast.castSpell S.alice plasmaCard >> Stack.resolveTop)
                    s2 = S.runPure (enteringAs 2) s1 (Cast.castSpell S.alice cloneA >> Stack.resolveTop)
                    s3 = S.runPure (enteringAs 0) s2 (Cast.castSpell S.alice cloneB >> Stack.resolveTop)
                 in case newestNamed (Text.pack "Clone") s3 of
                      Nothing -> HU.assertFailure "the second Clone did not reach the battlefield"
                      Just clone -> do
                        HU.assertEqual "its OWN choice wins on P/T" (Just 3) (Projection.powerOf clone s3)
                        HU.assertBool "flying and defender rode the copy chain" (Projection.hasKeyword Keyword.Flying clone s3 && Projection.hasKeyword Keyword.Defender clone s3)
              _ -> HU.assertFailure "fixture did not deal three cards",
```

`enteringAs` copies the highest-id legal creature; in the third test the second Clone must copy the *first* Clone, which is the newest creature at that point, so this is deterministic.

- [x] **Step 2: Run to watch it fail**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20`
Expected: FAIL — `Not in scope: 'Cards.primalPlasmaPrinting'`, and `Prompt.ChooseEntryOption` is not a constructor.

- [x] **Step 3: Add the prompt and response**

`source/library/Pawl/Type/Prompt.hs`:

```haskell
  -- CR 208.2b / 614.1c: as an object enters, its controller chooses among the
  -- shapes an "as this creature enters, it becomes your choice of ..." ability
  -- offers (Primal Plasma). The ObjectId is the entering object; the answer is an
  -- index into the offered list.
  --
  -- The chosen shape is written into the object's COPIABLE snapshot (CR 707.2), so
  -- a later Clone copies the choice without any further machinery -- and then, if
  -- it copied the ABILITY too, makes its own choice on top (CR 616.2).
  --
  -- Asked only when two or more options are offered; one option is not a choice.
  ChooseEntryOption :: Decider -> PlayerId -> ObjectId -> [EntryOption] -> Prompt Natural
```

with `import Pawl.Type.EntryOption (EntryOption)`.

`source/library/Pawl/Type/Response.hs`:

```haskell
  | -- CR 208.2b: the index of the entry shape a player chose as an object entered,
    -- serialized so a DecisionLog replays it deterministically.
    ChoseEntryOption Natural
```

`source/library/Pawl/Replay.hs` — `encode`: `Prompt.ChooseEntryOption {} -> Response.ChoseEntryOption answer`; `decode`: the matching `Response.ChoseEntryOption n -> Just n` arm; `defaultAnswer`:

```haskell
  -- CR 208.2b: the first offered shape is always a legal answer (this is asked
  -- only when the list has two or more), and is the least eventful fallback.
  Prompt.ChooseEntryOption {} -> 0
```

`source/test-suite/Pawl/Support.hs` — add `Prompt.ChooseEntryOption {} -> 0` to the four pure answerers and `Prompt.ChooseEntryOption {} -> pure 0` to `randomAnswer`.

- [x] **Step 4: Implement the `ChoiceOf` rewrite**

`source/library/Pawl/Replacement.hs` — add to `apply`:

```haskell
    -- CR 208.2b: "As this creature enters, it becomes your choice of ..." The
    -- chosen shape is written into the COPIABLE snapshot, which is what CR 707.2
    -- means by copiable values being the printed values "as modified by other copy
    -- effects, by its face-down status, and by 'as ... enters' ... abilities that
    -- set power and toughness (and may also set additional characteristics)".
    (ReplacementEffect.EntryR (EntryRewrite.ChoiceOf options), ProposedEvent.WouldEnter oid) -> do
      gs <- State.get
      case options of
        -- Malformed card data: an as-enters choice with nothing to choose from.
        -- No-op rather than a partial function.
        [] -> pure (Just event)
        first : rest -> do
          picked <-
            if null rest
              then -- One option is not a choice; where the rules leave nothing to
                   -- ask, don't prompt.
                pure first
              else case Projection.controllerOf oid gs of
                -- Unreachable: the object is materialized on the battlefield before
                -- this loop runs, so controllerOf falls back to its owner.
                Nothing -> pure first
                Just controller -> do
                  let decider = Decide.deciderFor controller gs
                  answer <- Trans.lift (Program.prompt (Prompt.ChooseEntryOption decider controller oid options))
                  pure (at options answer first)
          consume (ReplacementCandidate.identity candidate)
          State.modify' (applyEntryOption oid picked)
          pure (Just event)
```

and the writer:

```haskell
-- CR 208.2b / 707.2: stamp a chosen entry shape into the object's copiable
-- snapshot. Power and toughness are SET; keywords are UNIONED into whatever is
-- already there.
--
-- The union is pinned by Primal Plasma's own Gatherer ruling and is the detail
-- worth stating twice: "a 1/6 creature with flying and defender" is only
-- reachable if the choice ADDS defender to a snapshot that already carries flying
-- from the copy.
applyEntryOption :: ObjectId -> EntryOption.EntryOption -> GameState -> GameState
applyEntryOption oid option gs =
  let base = Projection.copiableCharacteristics oid gs
      stamped =
        base
          { PC.power = Just (EntryOption.power option),
            PC.toughness = Just (EntryOption.toughness option),
            -- CR 208.2b: the printed star now has a value, so layer 7a's
            -- characteristic-defining pass must not recompute over it.
            PC.characteristicPT = Nothing,
            PC.keywords = Set.union (PC.keywords base) (EntryOption.keywords option)
          }
      write o = o {Object.bindings = Binding.setCopy stamped (Object.bindings o)}
   in gs {GameState.objects = Map.adjust write oid (GameState.objects gs)}
```

- [x] **Step 5: Add the `Elemental` subtype and the card**

`source/library/Pawl/Type/Subtype.hs` — add `| Elemental -- CR 205.3m (a creature type; Primal Plasma's)`. `source/library/Pawl/Codec.hs` — add the two table entries.

`data/cards/primal-plasma.json`:

```json
{"name":"Primal Plasma","manaCost":[{"type":"Generic","value":3},{"type":"OfType","value":{"type":"Colored","value":{"type":"Blue"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Creature"}],"subtypes":[{"type":"Elemental"},{"type":"Shapeshifter"}]},"power":{"type":"Star"},"toughness":{"type":"Star"},"keywords":[],"staticAbilities":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[{"type":"EntryR","value":{"type":"ChoiceOf","value":[{"power":3,"toughness":3,"keywords":[]},{"power":2,"toughness":2,"keywords":[{"type":"Flying"}]},{"power":1,"toughness":6,"keywords":[{"type":"Defender"}]}]}}],"triggeredAbilities":[],"castingPermissions":[]}
```

Add `primalPlasmaPrinting` to `Pawl.Cards`'s record, `loadCards` (`loadPrinting "primal-plasma"`) and `allPrintings`. Do not add it to a deck.

- [x] **Step 6: Correct the now-false `*`-P/T note in `Pawl.Projection`**

`source/library/Pawl/Projection.hs`'s `baseColorsOf` comment (around line 314) ends with "no card in the pool has `*` P/T and is therefore not a licence to seed a dynamic CDA". Primal Plasma now does. Replace that clause with:

```haskell
-- has `*` P/T with NO characteristic-defining ability behind it -- Primal Plasma
-- (P5), whose star is given its value by an as-enters REPLACEMENT (CR 208.2b),
-- not by a CDA. Quantity.evaluate returns Nothing for a bare Star, so such a card
-- projects NO power or toughness until its entry choice applies, where CR 208.2b
-- says to use 0. That is unobservable on the battlefield -- the entry loop always
-- applies the choice before the Moved event exists -- but a Primal Plasma CARD in
-- a hand, library or graveyard reports Nothing where the rule says 0 (#N).
```

- [x] **Step 7: Run the four scenarios, then the whole suite**

Run: `cabal test --test-options='-p "Pawl.Replacement"' 2>&1 | tail -30`
Expected: PASS, including the centerpiece.

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -20`
Expected: PASS.

- [x] **Step 8: Commit**

```bash
git add source/library/Pawl data/cards/primal-plasma.json source/test-suite/Pawl pawl.cabal
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p5): ChoiceOf entry replacements and Primal Plasma

CR 208.2b's as-enters P/T choice writes into the copiable snapshot, so
CR 707.2 falls out for free. The centerpiece composes with no special
case: AsCopy applies from CR 616.1c's bucket, the loop re-collects and
finds the ChoiceOf the object did not have an iteration ago (CR 616.2),
and the choice sets P/T while UNIONING keywords -- 1/6 with flying and
defender, the Gatherer ruling's own words.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 9: Tokens — `WouldCreateTokens` and Doubling Season (§5 scenarios 8–11)

One card, **two** event classes; and CR 616.1g's nesting, which is only assertable now that Task 7 exists.

**Files:**
- Modify: `source/library/Pawl/Event.hs` (`createToken` → `createTokens`), `Pawl/Replacement.hs`, `Pawl/Resolve.hs` (the `Create` arm)
- Create: `data/cards/doubling-season.json`; modify `source/test-suite/Pawl/Cards.hs`
- Test: `source/test-suite/Pawl/ReplacementSpec.hs`, `source/test-suite/Pawl/EventSpec.hs` (the one `Event.createToken` call site)

**Interfaces:**
- Consumes: `Replacement.runEntry` and the batch (Task 7).
- Produces: `Event.createTokens :: PlayerId -> Card -> Natural -> Game [ObjectId]`; `Replacement.resolveTokens :: PlayerId -> Card -> Natural -> Game (Maybe (PlayerId, Card, Natural))`; `Cards.doublingSeasonPrinting`.

- [x] **Step 1: Write the failing tests**

Add to `source/test-suite/Pawl/ReplacementSpec.hs`. These reuse `castAndResolve`, `counterBoard`, `raceAnswer` and `countersOn`, which Task 6 added to this same file — tasks run strictly in order, so they are already there.

```haskell
      HU.testCase "CR 616.1g Doubling Season turns Dragon Fodder's two Goblins into four" $
        let base = S.landsInPlay (Cards.mountainPrinting cards) 2
            (_, g1) = S.addCreature (Cards.doublingSeasonPrinting cards) S.alice base
            (g2, spellId) = S.handOne (Cards.dragonFodderPrinting cards) g1
            after = castAndResolve S.identityAnswer g2 spellId
         in HU.assertEqual "twice that many" 4 (S.countOnBattlefieldByName (Text.pack "Goblin") S.alice after),
      HU.testCase "CR 614.5 two Doubling Seasons are two instances: eight Goblins" $
        let base = S.landsInPlay (Cards.mountainPrinting cards) 2
            (_, g1) = S.addCreature (Cards.doublingSeasonPrinting cards) S.alice base
            (_, g2) = S.addCreature (Cards.doublingSeasonPrinting cards) S.alice g1
            (g3, spellId) = S.handOne (Cards.dragonFodderPrinting cards) g2
            after = castAndResolve S.identityAnswer g3 spellId
         in HU.assertEqual "2 -> 4 -> 8" 8 (S.countOnBattlefieldByName (Text.pack "Goblin") S.alice after),
      HU.testCase "CR 614.1 Doubling Season's OTHER clause doubles counters, not tokens" $
        let (gs, spellId, mine, _) = counterBoard cards [Cards.doublingSeasonPrinting cards, Cards.pikerPrinting cards] []
         in case mine of
              season : piker : _ ->
                let after = castAndResolve (raceAnswer season piker) gs spellId
                 in HU.assertEqual "1 * 2" 2 (countersOn CounterKind.PlusOnePlusOne piker after)
              _ -> HU.assertFailure "fixture did not build two permanents",
      HU.testCase "CR 616.1 Doubling Season racing Hardened Scales: 4 or 3, by the prompt" $
        let (gs, spellId, mine, _) = counterBoard cards [Cards.doublingSeasonPrinting cards, Cards.hardenedScalesPrinting cards, Cards.pikerPrinting cards] []
         in case mine of
              season : scales : piker : _ ->
                let seasonFirst = castAndResolve (raceAnswer season piker) gs spellId
                    scalesFirst = castAndResolve (raceAnswer scales piker) gs spellId
                 in do
                      HU.assertEqual "(1 * 2) + 1" 3 (countersOn CounterKind.PlusOnePlusOne piker seasonFirst)
                      HU.assertEqual "(1 + 1) * 2" 4 (countersOn CounterKind.PlusOnePlusOne piker scalesFirst)
              _ -> HU.assertFailure "fixture did not build three permanents"
```

- [x] **Step 2: Run to watch it fail**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20`
Expected: FAIL — `Not in scope: 'Cards.doublingSeasonPrinting'`.

- [x] **Step 3: Add the token arms to the loop**

`source/library/Pawl/Replacement.hs` — `applies`:

```haskell
        (ReplacementEffect.TokenR pat _, ProposedEvent.WouldCreateTokens pid _ _) ->
          case TokenPattern.whose pat of
            ControllerRelation.Anyones -> True
            -- CR 109.5: "under YOUR control" -- the tokens' controller is the
            -- effect source's controller.
            ControllerRelation.Yours -> Projection.controllerOf src gs == Just pid
```

`apply`:

```haskell
    (ReplacementEffect.TokenR _ scaling, ProposedEvent.WouldCreateTokens pid card n) -> do
      consume (ReplacementCandidate.identity candidate)
      pure (Just (ProposedEvent.WouldCreateTokens pid card (scale scaling n)))
```

and the door:

```haskell
-- CR 111.1: settle a proposed token creation. Nothing means none are created.
resolveTokens :: PlayerId -> Card -> Natural -> Game (Maybe (PlayerId, Card, Natural))
resolveTokens pid card n = do
  outcome <- applyReplacements (ProposedEvent.WouldCreateTokens pid card n)
  pure (outcome >>= asTokens)

asTokens :: ProposedEvent -> Maybe (PlayerId, Card, Natural)
asTokens event = case event of
  ProposedEvent.WouldCreateTokens pid card n -> Just (pid, card, n)
  ProposedEvent.WouldChangeZone _ -> Nothing
  ProposedEvent.WouldEnter _ -> Nothing
  ProposedEvent.WouldDealDamage _ -> Nothing
  ProposedEvent.WouldBeDestroyed _ -> Nothing
  ProposedEvent.WouldPutCounters {} -> Nothing
```

- [x] **Step 4: Turn `createToken` into `createTokens`**

`source/library/Pawl/Event.hs`, replacing `createToken`:

```haskell
-- CR 111.2: create `n` tokens with the given effect-defined characteristics under
-- `controller`'s control (its owner, CR 111.2), summoning-sick (CR 302.6). A token
-- is created from nothing -- it has no prior object to move, so changeZone cannot
-- mint it. Uses from = Battlefield (it appears there; to == from can never read as
-- a leave). Emits the enters event so ETB triggers (CR 603.6a) fire on the same
-- path a resolved permanent uses.
--
-- PLURAL since P5, and that is a rules requirement, not a convenience. CR 614.1
-- replacements scope to the CREATION EVENT, not to each token -- Doubling Season
-- says "if an effect would create ONE OR MORE tokens ... it creates twice that
-- many" -- so the count is settled once, up front. Then every token is
-- materialized, and only then does each run its OWN entry loop (CR 616.1g:
-- "another [effect] may apply to an event contained within the first"; the whole
-- batch is in scope for CR 614.13a).
createTokens :: PlayerId -> Card -> Natural -> Game [ObjectId]
createTokens controller card n = do
  resolved <- Replacement.resolveTokens controller card n
  case resolved of
    Nothing -> pure []
    Just (owner, tokenCard, count) -> do
      let mkObj ts =
            Object.MkObject
              { Object.owner = owner,
                Object.source = Source.OfToken tokenCard,
                Object.zone = Zone.Battlefield,
                Object.tapped = TapState.Untapped,
                Object.damage = 0,
                Object.sickness = Sickness.Sick,
                Object.bindings = Map.empty,
                Object.counters = Map.empty,
                Object.timestamp = ts
              }
      ids <- Monad.replicateM (fromIntegral count) (placeObject owner mkObj Zone.Battlefield)
      Monad.mapM_ (Replacement.runEntry (Set.fromList ids)) ids
      -- A token is created from nothing, so there is no prior incarnation to
      -- snapshot: its last known information IS what it is now (CR 111.3 makes the
      -- creating effect's stated values functionally printed values). Recorded
      -- AFTER every entry loop, so the events describe settled objects.
      Monad.mapM_ recordEntry ids
      pure ids

recordEntry :: ObjectId -> Game ()
recordEntry newId = do
  placed <- State.get
  let snapshot = Projection.project newId placed
  State.modify' (recordEvent (GameEvent.Moved (ZoneChange.MkZoneChange newId Zone.Battlefield Zone.Battlefield) snapshot))
```

- [x] **Step 5: Point `Effect.Create` at it**

`source/library/Pawl/Resolve.hs`, replacing the `Create` arm (lines 547–573):

```haskell
  Effect.Create quantity card mSlot -> do
    gs <- State.get
    case Quantity.evaluate gs source (Just controller) quantity of
      Just n
        | n > 0 -> do
            -- CR 111: create n tokens with these characteristics under the
            -- effect's controller (CR 111.2), through the single funnel -- so CR
            -- 614's token replacements (Doubling Season) get their opportunity.
            minted <- Event.createTokens controller card (fromInteger n)
            case (mSlot, minted) of
              (Nothing, _) -> pure ()
              -- Unreachable: createTokens places every token onto the battlefield
              -- (CR 111.2). Total rather than partial: nothing bound matches "the
              -- token was never named" instead of crashing.
              (Just _, []) -> pure ()
              -- CR 603.7c: bind the minted token into live Object.bindings so a
              -- delayed ability THIS SAME resolution arms can name it. The lint
              -- guarantees the PRINTED quantity is 1 here (#53) -- but a
              -- replacement can now make it more (Doubling Season doubling a Tidal
              -- Wave's Wall), in which case "it" names the first and the rest are
              -- unnamed (#N).
              (Just slot, newId : _) -> State.modify' (bindSlot source slot newId)
      _ -> pure ()
```

`source/test-suite/Pawl/EventSpec.hs` — its one `Event.createToken` call becomes `Event.createTokens … 1` (the returned ids can be ignored with `Monad.void` or bound and discarded).

- [x] **Step 6: Add the card**

`data/cards/doubling-season.json`:

```json
{"name":"Doubling Season","manaCost":[{"type":"Generic","value":4},{"type":"OfType","value":{"type":"Colored","value":{"type":"Green"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Enchantment"}],"subtypes":[]},"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[{"type":"TokenR","value":[{"whose":{"type":"Yours"}},{"type":"Multiply","value":2}]},{"type":"CounterR","value":[{"whichKind":null,"whose":{"type":"Yours"},"onWhat":{"type":"AnyPermanent"}},{"type":"Multiply","value":2}]}]},"triggeredAbilities":[],"castingPermissions":[]}
```

Check that trailing brace count — the `replacementEffects` array closes with `]` before `,"triggeredAbilities"`. Validate with `python3 -m json.tool data/cards/doubling-season.json` before committing.

Add `doublingSeasonPrinting` to `Pawl.Cards`'s record, `loadCards` and `allPrintings`; not to a deck.

- [x] **Step 7: Run the four scenarios, then everything**

Run: `cabal test --test-options='-p "Pawl.Replacement"' 2>&1 | tail -30`
Expected: PASS, all of §5's 1–16, 18–20 and 22.

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test 2>&1 | tail -20`
Expected: PASS.

- [x] **Step 8: Commit**

```bash
git add source/library/Pawl data/cards/doubling-season.json source/test-suite/Pawl pawl.cabal
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p5): token creation joins the replacement path

Event.createTokens settles the COUNT once (CR 614.1 scopes Doubling
Season to the creation event, not to each token), materializes them all,
then runs each token's own entry loop with the batch in scope -- CR
616.1g's containment and CR 614.13a's simultaneity in one shape.
Doubling Season lands whole: a card in the pool modelling one of its two
abilities would be a wrong engine, not a partial one.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 10: Close — issues filed and cited, docs updated, exit criterion verified

Every `(#N)` placeholder Tasks 3–9 left in the code is replaced here with a real issue number. **`grep -rn '(#N)' source/` must return nothing when this task is done.**

**Files:**
- Modify: every source file carrying a `(#N)` placeholder
- Modify: `docs/progress.md` (append one completion entry), `CLAUDE.md` (replace the status bullet), `docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md` (§3's P5 row, §4's ordering note)

- [x] **Step 1: File the ten new issues**

Run each `gh issue create` and record the number it prints. Labels come from CLAUDE.md's set: `elision`, `gap`, `rules-correctness`, `bug`, `expires:milestone`, `expires:card-driven`.

| # | Title | Labels | Body must carry |
|---|---|---|---|
| A | CR 614.15 self-replacement effects have no producer (CR 616.1a's bucket) | `gap`, `expires:card-driven` | The bucket exists in `ReplacementBucket`; nothing classifies into it. Expiry: the first card that replaces part of its own resolution. |
| B | CR 616.1b control-modifying entry replacements have no producer | `gap`, `expires:card-driven` | Needs an "enters under your control instead" replacement. |
| C | CR 616.1d back-face entry replacements have no producer | `gap`, `expires:card-driven` | Needs transform (CR 701.27), which is not modelled. |
| D | CR 616.1's APNAP tie-break is not implemented | `elision`, `expires:card-driven` | One proposed event has one affected object and therefore one chooser; the damage batch resolves each event's loop independently. Expiry: the first event affecting two players' objects at once. |
| E | CR 614.12b: entry choices are not checked for payable combined costs | `gap`, `expires:card-driven` | Neither `AsCopy` nor `ChoiceOf` has a cost. |
| F | CR 614.13a's same-batch entry exclusion is implemented but untested | `gap`, `expires:card-driven` | No real card puts two copy-choosers onto the battlefield simultaneously; a synthetic card to manufacture the case was deliberately not added. |
| G | ChooseReplacement's payload is positional and carries only the source | `elision`, `expires:card-driven` | Sound while no single source has two same-class applicable replacements. Doubling Season has two replacement abilities but in different event classes. Mirrors #61 for triggers. |
| H | CR 614.5 identity is by effect VALUE, so one source's two identical replacements get one opportunity | `rules-correctness`, `expires:card-driven` | Explains why value identity was chosen over index identity (CR 616.2 / the Clone-of-Primal-Plasma centerpiece) and what it costs. |
| I | CR 208.2b: an unapplied `*` P/T projects nothing rather than 0 | `rules-correctness`, `expires:card-driven` | Unobservable on the battlefield (the entry loop always applies first); a Primal Plasma card in hand/library/graveyard reports `Nothing` where the rule says 0. Sibling of #65. |
| J | CR 603.7c: a slot-binding Create can mint several tokens once a replacement scales it | `bug`, `expires:card-driven` | The `#53` lint checks the PRINTED quantity; Doubling Season doubling a Tidal Wave's Wall binds "it" to the first token and leaves the rest unnamed. |

Then update **#58** (CR 615.7 / 615.13) with a comment rather than closing it: `ActiveReplacement` now carries `source` and `timestamp`, so both are card-blocked rather than structure-blocked; CR 615.7's shared N-damage shield additionally needs a cross-event allocation the per-event batch shape cannot express (`Pawl.Damage.applyDamage`'s comment records this).

- [x] **Step 2: Sweep the `(#N)` placeholders**

Run: `grep -rn '(#N)' source/`
Replace each with the number from Step 1: `CandidateId.hs` → H; `ReplacementBucket.hs` → A, B, C; `Prompt.hs`'s `ChooseReplacement` → G; `Replacement.hs`'s `chooserOf` → D, `applyReplacementsIn` → F; `Projection.hs`'s `baseColorsOf` → I; `Resolve.hs`'s `Create` arm → J. E has no natural code site — cite it in the `EntryRewrite` module comment.

Run: `grep -rn '(#N)' source/`
Expected: no output.

- [x] **Step 3: Verify the exit criterion mechanically**

Run each and confirm the expected result:

```bash
grep -rn "Prevention\|ActivePrevention\|regenerationShields\|copyOnEnter\|drainAsEntersChoices\|applyPreventions" source/ data/    # no output
grep -rln "ProposedEvent\.\|ReplacementEffect\." source/library/ | grep -v "Pawl/Replacement.hs\|Pawl/Codec.hs\|Pawl/Type/"        # no output
cabal clean && cabal build all --enable-tests --enable-benchmarks                                                                  # warning-free
cabal test                                                                                                                          # all green
git add -A && hooky run                                                                                                             # passes
cabal bench                                                                                                                         # three timings, no large regression
```

The second grep is the **first invariant's audit**: `Pawl.Replacement` is the sole rules home of casing on either type, `Pawl.Codec` is the JSON boundary, and `Pawl.Type.*` modules only construct. Anything else in the output is a fusion of the two halves and must be fixed, not excused.

- [x] **Step 4: Append the `docs/progress.md` completion entry**

One entry, in the file's established voice, recording what P5 *established* — not what is left. It must state:
- the gate cards and what each falsified (Hardened Scales + Corpsejack Menace: the order decides the board, so a fold has to invent one; Doubling Season: one card, two event classes, plus CR 616.1g nesting; Clone + Primal Plasma: CR 616.2, which no single-pass implementation can produce);
- what was deleted (`Pawl.Type.Prevention`, `ActivePrevention`, `GameState.preventions`, `regenerationShields`, `Card.copyOnEnter`, `Engine.drainAsEntersChoices` and the `Binding` pending marker, `Event.applyReplacements`/`applyPreventions`/`cancels`/`regenerate`/`markCopyOnEnter`, `Target.legalCopyTargets`, `Effect.Prevent`/`RegenerateSelf`);
- what was added (`Pawl.Replacement`; `ProposedEvent`'s six arms; `ReplacementEffect`'s six; `ActiveReplacement`/`Uses`; `Effect.Replace`; `Event.putCounters`/`createTokens`; `Prompt.ChooseReplacement`/`ChooseEntryOption`; the monadic funnels and `Sba.performStateBasedActions :: Game Bool`);
- the **three departures** listed at the top of this plan, with their reasons;
- tracking: `48b17cb`/issue #1 (M3f replacement seam, GAP-R **and** the CR 616 facet) **closed**; #58 **updated, not closed**; ten new issues (A–J above) filed. Note the family resemblance P4's spec drew: P4 retired the *trigger* ordering elision (603.3b), P5 retires the *replacement* ordering elision (616.1) — different rules, different mechanisms, same shape of mistake;
- the final suite count, that the build is warning-clean on a from-scratch `cabal clean` build, and the benchmark comparison;
- the spec and plan paths, kept as reference.

- [x] **Step 5: Replace the `CLAUDE.md` status bullet**

**Replace, never append** — milestone history goes in `progress.md`. The new bullet says M0–M4h plus M4.5 P1–P5 are complete, that P5 closed Cluster 2's second phase (the monadic replacement path and CR 616), and that **P6 (conditional & event durations) is next**, with P6 and P7 both unblocked and P8/P9 still floating. Keep it to the same length as the bullet it replaces.

- [x] **Step 6: Tick the umbrella spec**

`docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`:
- §3's P5 row: mark it landed with a pointer to `docs/superpowers/specs/2026-07-21-p5-replacement-events-design.md`, and **name the two gate cards the umbrella did not anticipate** — Clone + Primal Plasma, which is what actually falsifies a single-pass implementation via CR 616.2 (the spec's §9 asks for exactly this).
- §4's ordering note: P5 landed; P6 and P7 remain unblocked; P8 and P9 still float.
- §6's tracking list: `48b17cb` (GAP-R) and `6afb561` (the M3f replacement seam) are both closed by this phase.

- [x] **Step 7: Commit**

```bash
git add docs/progress.md CLAUDE.md docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md source/library/Pawl
hooky fix
git add -u
hooky run
git commit -m "docs(m4.5-p5): completion note, umbrella tick, CLAUDE.md status

Ten deferrals filed as issues and cited at their code sites; #1 (GAP-R
and the CR 616 facet) closed; #58 updated, not closed. P6 is next.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

- [x] **Step 8: Confirm the plan is complete**

Run: `grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-21-p5-replacement-events.md`
Expected: `0`.

---

## Spec coverage map

| Spec section | Where it lands |
|---|---|
| §2.1 one vocabulary of proposed events | Task 3 (`Pawl.Type.ProposedEvent`) |
| §2.2 one event-class-general `ReplacementEffect` + patterns | Task 1 |
| §2.3 one place effects come from (`GameState.replacements`, `Effect.Replace`, `Uses`) | Task 4 |
| §2.4 the CR 616.1 loop, buckets, applied set, `ChooseReplacement` | Task 3 (with the identity correction) |
| §2.5 the entry loop, materialization, copiable-snapshot writes, CR 614.13a | Tasks 7 and 8 |
| §2.6 damage stays a simultaneous batch | Task 4 |
| §2.7 new funnels and the monadic ripple | Tasks 2 (ripple), 6 (`putCounters`), 9 (`createTokens`) |
| §2.8 serialization and the two deliberate data breaks | Tasks 1, 4, 5, 7 |
| §5 scenarios 1–7 | Task 6 |
| §5 scenarios 8–11 | Task 9 |
| §5 scenarios 12–15 | Task 8 |
| §5 scenario 16 | Task 7 |
| §5 scenario 17 | Task 3 (existing `EventSpec` case, now on the loop) |
| §5 scenarios 18–20 | Task 5 |
| §5 scenarios 21–22 | Task 4 |
| §6 module & type inventory | Tasks 1, 3, 4 (plus the four internal types noted as a departure) |
| §8 deferrals with named expiries | Task 10 |
| §9 tracking | Task 10 |
| §10 exit criterion | Task 10, Step 3 |
