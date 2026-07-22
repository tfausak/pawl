# M4.5 P6 — Conditional and event durations, and the moment a duration begins: Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Split the printed `Duration` from the stored `Expiry`, give a duration a *beginning* (CR 611.2b) and two new shapes — a continuously re-checked condition and an expiry keyed to a future turn — and prove both with **Master Thief** and **Hag of Inner Weakness**.

**Architecture:** `Pawl.Type.Duration` stays the card-data vocabulary and grows `ForAsLongAs StateCondition` and `UntilYourNextTurn`. A new runtime-only `Pawl.Type.Expiry` (`AtCleanup | Never | While PlayerId StateCondition | AtTurnOf PlayerId`) replaces the `duration` field on both carriers (`ContinuousEffect`, `ActiveReplacement`). A new `Pawl.Expiry` module is the **sole home of `case … Expiry`** and owns the whole life cycle: `arm` (printed → stored, CR 109.5's "you" baked in, `Nothing` when CR 611.2b's duration never starts), `dropAtCleanup` (CR 514.2, absorbing and deleting the two existing per-carrier sweeps), `dropAtHandoff` (CR 611.2a, at the turn handoff), and `sweepConditional` (CR 611.2b, first step of the settle loop — it **deletes**, never masks). `StateCondition` gains a third customer (`YouControlSource`) and `Pawl.Target` becomes source-relative so "an opponent controls" can be expressed.

**Tech Stack:** Haskell 2010 (GHC 9.14.1 from the Nix flake), `tasty` + `tasty-hunit`, the hand-rolled `Pawl.Json`/`Pawl.Codec` card codec.

**Spec:** `docs/superpowers/specs/2026-07-22-p6-conditional-event-durations-design.md`. Read §2.1 and §2.3 before Task 1, §2.4's turn-handoff paragraph before Task 2, §2.2 before Task 3, §2.5 before Task 4, §5 before Tasks 5–6, §8–§10 before Task 7.

## Global Constraints

Every task's requirements implicitly include all of these. They come from `CLAUDE.md` and are not negotiable.

- **Haskell 2010, no language extensions** unless there is no alternative. `NamedFieldPuns` is permitted; `GADTs`/`RankNTypes` only in the suspension core and in test modules that already carry the pragma. A module that already carries a `{-# LANGUAGE … #-}` pragma keeps it.
- **No explicit export lists.** `module Pawl.Foo where`.
- **One type per module** under `Pawl.Type.<TypeName>` (type + instances only). A module never imports its parents; `A.B.C` must not import `A.B` or `A`.
- **Qualified imports aliased to the last component** (`Data.Set` → `Set`, `Pawl.Type.Expiry` → `Expiry`). One import group, alphabetical. The one documented exception is `Pawl.Support` as `S` in the test suite. A module may import another module under an alias equal to its own last component (`Pawl.Mana` already imports `Pawl.Type.Mana` as `Mana`); `Pawl.Expiry` importing `Pawl.Type.Expiry` as `Expiry` is that same, established shape.
- **No partial functions**, written or used. No `head`, `error`, `undefined`, or non-exhaustive matches.
- **`newtype` liberally, non-punning constructors** (`MkFoo`). Build records with `do`/`pure` + record syntax, not `<$>`/`<*>`.
- **Prefer explicit:** `case` over point-free; one equation with a `case` over multiple clauses; `let` over `where`; `$` over parens, `.` over chained `$`. No list comprehensions. No backtick-infixed functions. Named local predicates over lambdas in `filter` (the `keep` idiom the deleted sweeps already used).
- **`Text` not `String`.** Arbitrary-precision numbers (`Integer`, `Natural`).
- **No boolean blindness** — a custom sum type beats a bare `Bool`. The one exception this plan makes is `sweepConditional :: Game Bool`, which the spec mandates and which matches the settle loop's existing `Sba.performStateBasedActions :: Game Bool`.
- **Derive at least `Eq` and `Show`.** `Expiry` also derives `Ord`, because `ContinuousEffect` and `ActiveReplacement` both derive `Ord` and carry it.
- **No API stability obligations.** Rename, reshape, and delete freely; never add a compat shim or keep an old name.
- **Every rules claim is checked against `docs/rules.txt`** and the rule number is cited in the code comment. Never trust recalled Magic rules — including the citations written in *this plan*. If `rules.txt` disagrees with a citation below, `rules.txt` wins: fix the citation and say so in the completion note.
- **Build must be warning-clean.** `cabal build all --enable-tests --enable-benchmarks` with `flags: +pedantic` (which is `-Werror`). Incremental builds hide warnings from unchanged modules; `cabal clean` first when a definitive check is needed.
- **Import lists are not spelled out** in every snippet below. When a step's code names `Monad.when`, `Set.*`, `Map.*`, `List.*` or a `Pawl.Type.*` module, add the qualified import (aliased to the last component, one alphabetical group). GHC names every missing one. Equally, when a step *deletes* the last use of an import, delete the import — `-Wunused-imports` is an error here.
- **New library modules** go under `source/library/` and are picked up by the `-- cabal-gild: discover` directive — add the file and run `hooky fix`; never hand-edit `exposed-modules`. **New test modules** are discovered the same way into the test-suite `other-modules`, and must additionally be wired into `source/test-suite/Main.hs`'s `testTree`.
- **Before every commit:** `git add <the paths this task names>`, then `hooky fix`, then `git add` again, then `hooky run`. `hooky` acts on **staged** files only; if you skip the `git add`, it reports "hooks skipped" and checks nothing.
- **TDD is not optional.** Write the failing test and actually run it to watch it fail before implementing. For a task whose first test names a module or constructor that does not exist yet, "fails" means the **build** fails with a specific `Not in scope` / `Could not find module` error — record that as the observed failure, then implement.
- **Never edit this plan, weaken an assertion, or delete a test to make a check pass.** If the plan looks wrong, stop and say so.
- **One small complete commit per task, on `main`.** Commit messages end with the `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>` trailer.
- **Concurrent sessions share this checkout.** Stage the explicit paths each task names; do not blanket-stage foreign files that appear in `git status`.

## Card verification (already done — do not re-fetch)

Both gate cards were verified live against the Scryfall API during the design pass. Use these values verbatim; no network access is needed during execution.

| Card | Cost | Type line | P/T | Oracle text |
|---|---|---|---|---|
| Master Thief | `{2}{U}{U}` | Creature — Human Rogue | 2/2 | "When this creature enters, gain control of target artifact for as long as you control this creature." |
| Hag of Inner Weakness | `{2}{B}` | Creature — Hag Warlock | 2/2 | "At the beginning of your upkeep, target creature an opponent controls gets -2/-1 until your next turn." |

Master Thief's three Gatherer rulings, verbatim (they *are* the specification of tests 2–4; do not paraphrase them into the test names):

> - *"If Master Thief leaves the battlefield, you no longer control it, and its control-change effect ends."*
> - *"If Master Thief ceases to be under your control before its ability resolves, you won't gain control of the targeted artifact at all."*
> - *"If another player gains control of Master Thief, its control-change effect ends. Regaining control of Master Thief won't cause you to regain control of the artifact."*

Hag of Inner Weakness has **no** Gatherer rulings (Scryfall returned an empty set); its tests derive from CR 611.2a and CR 514.2 directly.

**Neither card is added to any deck in `Pawl.Cards`.** They are deterministic fixtures, like Clone and Tarmogoyf — adding them to a deck would perturb `PropertySpec`'s card-backed conservation counts for no gain. They *are* added to the `Cards` record, `loadCards` and `allPrintings`, which the `CardSpec` directory lint requires.

## Four deliberate departures from the spec

State these in the completion note (Task 7). They are corrections and refinements, not drift.

1. **`Expiry.arm` grows its parameters across Tasks 1–3 rather than arriving four-parameter in Task 1.** The spec's §2.3 gives the final signature `arm :: PlayerId -> ObjectId -> Duration -> GameState -> Maybe Expiry`. Introducing it in Task 1, where only `UntilEndOfTurn` and `Indefinite` exist, leaves three unused parameters and `-Weverything` + `-Werror` fails the build. So: Task 1 defines `arm :: Duration -> Maybe Expiry`, Task 2 widens it to `PlayerId -> Duration -> Maybe Expiry`, Task 3 widens it to the spec's final shape. Each widening touches the same three `Resolve` call sites, one line each. `Pawl.Type.Expiry` itself is defined **once**, in Task 1, with all four constructors — `dropAtCleanup` must case on all four to be exhaustive anyway, and `While`/`AtTurnOf` acquire their producers in Tasks 3 and 2.
2. **`Target.legalSets` gains the source parameter too.** The spec's §2.5 names `legalRecipients` and `stillLegal` as "`Pawl.Target`'s two entry points". `legalSets` is a two-line wrapper over `legalRecipients` (test-only consumers today), so it takes the source and passes it through. `legalSetsExcluding` and `fillableModes` already have it.
3. **A controller-relative spec whose source has left the battlefield yields an EMPTY legal set, which is a rules deviation.** `OpponentCreatureTarget` reads `Projection.controllerOf source`; when the source is gone that is `Nothing`, so CR 608.2b's re-check at resolution finds every target illegal and the ability wrongly fizzles. Real rules keep the target legal — the ability's controller is still known even when its source is not. The narrower fix (threading the ability's *controller* rather than its source) is out of scope: it changes a signature four call sites use for a case no card in the pool reaches (nothing in the pool kills a Hag in response to its own trigger). Task 7 files this as a `rules-correctness` issue and cites it at the arm.
4. **Test 9 (codec) is distributed, not a task.** The spec's §5 lists codec round-trips as one numbered test. Under TDD each new codec arm's round-trip lands in the task that adds the arm (Tasks 2, 3, 4), and each card file's round-trip is already enforced for free by `CardsSpec.checkFile` over `Cards.allPrintings` (parse-equality **and** byte-stability) as soon as Tasks 5–6 register the printings. Task 7 verifies the coverage rather than re-adding it.

## File structure

**New library modules.** Both in Task 1.

| Module | Responsibility |
|---|---|
| `source/library/Pawl/Type/Expiry.hs` | the stored, runtime-only expiry: `AtCleanup \| Never \| While PlayerId StateCondition \| AtTurnOf PlayerId` |
| `source/library/Pawl/Expiry.hs` | **sole home of `case … Expiry`**: `arm`, `dropAtCleanup`, `dropAtHandoff`, `sweepConditional` |

**Deleted functions.** `Pawl.Projection.dropEndOfTurnEffects` and `Pawl.Event.dropEndOfTurnReplacements` (both Task 1, absorbed by `Expiry.dropAtCleanup`).

**New test module.** `source/test-suite/Pawl/ExpirySpec.hs` (Task 1) — near-mirrors `Pawl.Expiry`; holds the sweep unit tests and both gate cards' gameplay-level tests (§5's tests 1–8).

**New card data.** `data/cards/master-thief.json` (Task 5), `data/cards/hag-of-inner-weakness.json` (Task 6).

**Changed library modules.** `Pawl/Type/Duration.hs`, `Pawl/Type/StateCondition.hs`, `Pawl/Type/TargetSpec.hs`, `Pawl/Type/Subtype.hs`, `Pawl/Type/ContinuousEffect.hs`, `Pawl/Type/ActiveReplacement.hs`, `Pawl/Event.hs`, `Pawl/Projection.hs`, `Pawl/Stack.hs`, `Pawl/Target.hs`, `Pawl/Resolve.hs`, `Pawl/Engine.hs`, `Pawl/Codec.hs`.

---

### Task 1: `Expiry` — the split, the sole casing home, and one cleanup sweep

Behaviour-neutral by construction: every existing test must still pass unchanged in meaning. The two per-carrier cleanup sweeps become one.

**Files:**
- Create: `source/library/Pawl/Type/Expiry.hs`, `source/library/Pawl/Expiry.hs`
- Create: `source/test-suite/Pawl/ExpirySpec.hs`
- Modify: `source/library/Pawl/Type/ContinuousEffect.hs` (field `duration` → `expiry`), `source/library/Pawl/Type/ActiveReplacement.hs` (same)
- Modify: `source/library/Pawl/Projection.hs:716-722` (delete `dropEndOfTurnEffects`), `source/library/Pawl/Event.hs:86-95` (delete `dropEndOfTurnReplacements`)
- Modify: `source/library/Pawl/Resolve.hs:398`, `:423`, `:591`, `:637`
- Modify: `source/library/Pawl/Engine.hs:174-181`
- Modify: `source/library/Pawl/Type/GameState.hs:54`, `:58` (the two comments that say "duration")
- Modify: `source/test-suite/Main.hs` (wire `ExpirySpec`)
- Test: `source/test-suite/Pawl/ExpirySpec.hs` (new), plus the mechanical rename in `Support.hs`, `DamageSpec.hs`, `EventSpec.hs`, `ProjectionSpec.hs`, `ReplacementSpec.hs`, `ResolveSpec.hs`, `CombatSpec.hs`

**Interfaces:**
- Produces: `Expiry.Expiry = AtCleanup | Never | While PlayerId StateCondition | AtTurnOf PlayerId` deriving `(Eq, Ord, Show)`; `Pawl.Expiry.arm :: Duration -> Maybe Expiry`; `Pawl.Expiry.dropAtCleanup :: GameState -> GameState`; `ContinuousEffect.expiry :: Expiry`; `ActiveReplacement.expiry :: Expiry`.
- Consumes: nothing from earlier tasks.

- [x] **Step 1: Write the failing test**

Create `source/test-suite/Pawl/ExpirySpec.hs`:

```haskell
-- Covers Pawl.Expiry and Pawl.Type.Expiry: the printed Duration -> stored Expiry
-- arming (CR 611.2), the sweeps that end a duration (CR 514.2, 611.2a, 611.2b),
-- and the two gate cards (Master Thief, Hag of Inner Weakness).
module Pawl.ExpirySpec where

import qualified Data.Set as Set
import qualified Pawl.Expiry as Expiry
import qualified Pawl.Game as Game
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Cards as Cards
import qualified Pawl.Type.ActiveReplacement as ActiveReplacement
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.Expiry as Expiry.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- A stored continuous effect with a chosen expiry, over a stand-in target.
-- Object id 998 is the stand-in source (Support.withEffectAt's posture);
-- nothing here reads the source's characteristics.
effectWith :: Expiry.Type.Expiry -> GameState.GameState -> GameState.GameState
effectWith expiry gs =
  let (ts, gs1) = Game.freshTimestamp gs
      eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = ObjectId.MkObjectId 998,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.expiry = expiry,
            ContinuousEffect.modification = Modification.GainKeyword Keyword.Flying,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton (ObjectId.MkObjectId 999))
          }
   in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}

armTests :: Tasty.TestTree
armTests =
  Tasty.testGroup
    "Arm"
    [ HU.testCase "CR 514.2 an until-end-of-turn duration arms to AtCleanup" $
        HU.assertEqual "armed" (Just Expiry.Type.AtCleanup) (Expiry.arm Duration.UntilEndOfTurn),
      HU.testCase "CR 611.2a an indefinite duration arms to Never" $
        HU.assertEqual "armed" (Just Expiry.Type.Never) (Expiry.arm Duration.Indefinite)
    ]

cleanupTests :: Cards.Cards -> Tasty.TestTree
cleanupTests cards =
  Tasty.testGroup
    "DropAtCleanup"
    [ HU.testCase "CR 514.2 cleanup drops an AtCleanup continuous effect and keeps a Never one" $
        let gs0 = Setup.emptyGame S.bothPlayers
            gs1 = effectWith Expiry.Type.Never (effectWith Expiry.Type.AtCleanup gs0)
            after = Expiry.dropAtCleanup gs1
         in do
              HU.assertEqual "two stored before" 2 (length (GameState.continuousEffects gs1))
              HU.assertEqual "one survives" [Expiry.Type.Never] (map ContinuousEffect.expiry (GameState.continuousEffects after)),
      HU.testCase "CR 514.2 the same sweep drops an AtCleanup floating replacement" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (oid, gs1) = S.addPiker cards S.alice gs0
            shielded = S.addRegenShield oid gs1
            after = Expiry.dropAtCleanup shielded
         in do
              HU.assertEqual "one shield before" 1 (length (GameState.replacements shielded))
              HU.assertEqual "none after" [] (map ActiveReplacement.expiry (GameState.replacements after))
    ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Pawl.ExpirySpec" [armTests, cleanupTests cards]
```

Wire it into `source/test-suite/Main.hs`: add `import qualified Pawl.ExpirySpec as ExpirySpec` in alphabetical position (after `EventSpec`), and `ExpirySpec.tests cards,` into `testTree` (after `EventSpec.tests cards,`).

- [x] **Step 2: Run the test to verify it fails**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Could not find module 'Pawl.Expiry'` (and `Pawl.Type.Expiry`).

- [x] **Step 3: Create `Pawl.Type.Expiry`**

`source/library/Pawl/Type/Expiry.hs`:

```haskell
module Pawl.Type.Expiry where

import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.StateCondition (StateCondition)

-- CR 611.2: how long a STORED effect lasts, as the game remembers it. The
-- runtime counterpart of the printed Pawl.Type.Duration, the same way
-- ActiveReplacement is ReplacementEffect's and PendingTrigger is
-- DelayedTrigger's: card data says "until your next turn", the game remembers
-- WHOSE. Never appears in card JSON and has no codec.
--
-- The split is what makes a card-only value in GameState unrepresentable. With
-- one type carrying both, a printed arm that leaked into a stored effect would
-- match no sweep and the effect would last forever -- silently. Only
-- Pawl.Expiry may case on this type.
data Expiry
  = -- CR 514.2: "all 'until end of turn' and 'this turn' effects end" during
    -- the cleanup step.
    AtCleanup
  | -- CR 611.2a: "lasts until the end of the game". No sweep ends it.
    Never
  | -- CR 611.2b: "for as long as ...". The PlayerId is CR 109.5's "you", baked
    -- in by Pawl.Expiry.arm at the moment the effect is stored -- derived from
    -- the effect's controller, never chosen. The duration is ONE continuous
    -- period: once the condition stops holding the effect is DELETED, and a
    -- condition that becomes true again does not bring it back.
    While PlayerId StateCondition
  | -- CR 611.2a: "until your next turn", as a concrete player. Ends as that
    -- player's turn begins.
    AtTurnOf PlayerId
  deriving (Eq, Ord, Show)
```

- [x] **Step 4: Create `Pawl.Expiry` with `arm` and `dropAtCleanup`**

`source/library/Pawl/Expiry.hs`:

```haskell
-- CR 611.2: the life cycle of a stored effect's duration. This module is the
-- ONLY module that may case on Pawl.Type.Expiry -- the standing Pawl.Resolve
-- has over Effect, Pawl.Projection over Modification and Pawl.Event over
-- TriggerCondition. It owns the transformation from the PRINTED Duration to the
-- STORED Expiry (`arm`) and every sweep that ends one, over BOTH carriers:
-- GameState.continuousEffects and GameState.replacements share one expiry
-- vocabulary, so they share one sweep.
module Pawl.Expiry where

import qualified Pawl.Type.ActiveReplacement as ActiveReplacement
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import Pawl.Type.Duration (Duration)
import qualified Pawl.Type.Duration as Duration
import Pawl.Type.Expiry (Expiry)
import qualified Pawl.Type.Expiry as Expiry
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState

-- CR 611.2: the moment a duration BEGINS. Total for the two fixed points; the
-- shapes that can fail to start (CR 611.2b) arrive with their own arms.
arm :: Duration -> Maybe Expiry
arm duration = case duration of
  Duration.UntilEndOfTurn -> Just Expiry.AtCleanup
  Duration.Indefinite -> Just Expiry.Never

-- CR 514.2: during the cleanup step, "all 'until end of turn' and 'this turn'
-- effects end". Delete-and-recompute (design.md 2.5): dropping the stored entry
-- makes the next projection revert -- nothing is explicitly undone. One sweep
-- over both carriers, replacing Projection.dropEndOfTurnEffects and
-- Event.dropEndOfTurnReplacements, which existed only because the two lists
-- lived in two modules.
dropAtCleanup :: GameState -> GameState
dropAtCleanup gs =
  let survives expiry = case expiry of
        Expiry.AtCleanup -> False
        Expiry.Never -> True
        Expiry.While _ _ -> True
        Expiry.AtTurnOf _ -> True
      keepEffect eff = survives (ContinuousEffect.expiry eff)
      keepReplacement active = survives (ActiveReplacement.expiry active)
   in gs
        { GameState.continuousEffects = filter keepEffect (GameState.continuousEffects gs),
          GameState.replacements = filter keepReplacement (GameState.replacements gs)
        }
```

- [x] **Step 5: Rename the field on both carriers**

In `source/library/Pawl/Type/ContinuousEffect.hs`, replace the `duration :: Duration` field with `expiry :: Expiry` (import `Pawl.Type.Expiry`, drop `Pawl.Type.Duration`), and change the comment "`duration` decides when cleanup drops it (CR 514.2)" to "`expiry` decides when a sweep drops it (Pawl.Expiry; CR 514.2, 611.2a, 611.2b)".

In `source/library/Pawl/Type/ActiveReplacement.hs`, same rename and same import swap; change "`duration` decides when cleanup drops it (CR 514.2)" to "`expiry` decides when a sweep drops it (Pawl.Expiry; CR 514.2)". Leave the rest of that module's long comment untouched.

- [x] **Step 6: Delete the two old sweeps and route the three storing sites through `arm`**

Delete `dropEndOfTurnEffects` from `source/library/Pawl/Projection.hs` (lines 716–722) and drop the now-unused `Pawl.Type.Duration` import.

Delete `dropEndOfTurnReplacements` from `source/library/Pawl/Event.hs` (lines 86–95) and drop the now-unused `Pawl.Type.Duration` **and** `Pawl.Type.ActiveReplacement` imports (line 94 was their only use).

In `source/library/Pawl/Resolve.hs`, the `ChangeText` arm at line 423 stores an indefinite effect directly — change `ContinuousEffect.duration = Duration.Indefinite` to `ContinuousEffect.expiry = Expiry.Never` (CR 611.2a; no arming decision to make).

The three arms that carry a *printed* duration go through `arm`. `ModifyTarget` (around line 380) becomes:

```haskell
  Effect.ModifyTarget duration modification slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs
          Just target -> case Expiry.arm duration of
            -- CR 611.2b: the duration never started, so the effect does nothing
            -- and is never stored.
            Nothing -> gs
            Just expiry ->
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
                        ContinuousEffect.expiry = expiry,
                        ContinuousEffect.modification = frozen,
                        ContinuousEffect.affected = Affected.TheseObjects (Set.singleton target)
                      }
               in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}
        -- A modification cannot land on a player (CreatureTarget/LandTarget name
        -- objects) or an illegal slot (CR 608.2b): no-op.
        _ -> gs
```

`Replace` (around line 579) gains the same wrapper, keeping its existing CR 614.3 / 615.3 / 113.7 comment above the `State.modify'`:

```haskell
  Effect.Replace duration uses re ->
    State.modify' $ \gs -> case Expiry.arm duration of
      -- CR 611.2b: the duration never started, so no floating replacement is
      -- installed.
      Nothing -> gs
      Just expiry ->
        let (ts, gs1) = Game.freshTimestamp gs
            active =
              ActiveReplacement.MkActiveReplacement
                { ActiveReplacement.effect = re,
                  ActiveReplacement.source = source,
                  ActiveReplacement.timestamp = ts,
                  ActiveReplacement.expiry = expiry,
                  ActiveReplacement.uses = uses
                }
         in gs1 {GameState.replacements = active : GameState.replacements gs1}
```

`GainControl` (around line 622) gains it too, wrapping the existing body so that on `Nothing` **neither** the effect is stored **nor** the target re-Sicked (CR 302.6 applies only when control actually changed):

```haskell
  Effect.GainControl duration slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs -- a player recipient cannot be controlled
          Just target -> case Expiry.arm duration of
            -- CR 611.2b: the duration never started -- no control effect is
            -- stored, and nothing is re-Sicked, because control never changed.
            Nothing -> gs
            Just expiry ->
              -- CR 613.1b / 611.2c: the new controller is `controller` (this
              -- effect's source's controller), baked in now -- derived, never
              -- chosen. CR 302.6: the new controller has not controlled the
              -- permanent continuously, so it is re-Sicked.
              let (ts, gs1) = Game.freshTimestamp gs
                  eff =
                    ContinuousEffect.MkContinuousEffect
                      { ContinuousEffect.source = source,
                        ContinuousEffect.timestamp = ts,
                        ContinuousEffect.expiry = expiry,
                        ContinuousEffect.modification = Modification.SetController controller,
                        ContinuousEffect.affected = Affected.TheseObjects (Set.singleton target)
                      }
                  sicken o = o {Object.sickness = Sickness.Sick}
               in gs1
                    { GameState.continuousEffects = eff : GameState.continuousEffects gs1,
                      GameState.objects = Map.adjust sicken target (GameState.objects gs1)
                    }
        _ -> gs -- illegal slot at resolution (CR 608.2b): no-op
```

- [x] **Step 7: Collapse the cleanup step to one call**

In `source/library/Pawl/Engine.hs`, the `Phase.Ending EndingStep.Cleanup` arm (lines 174–181) becomes:

```haskell
    Phase.Ending EndingStep.Cleanup -> do
      discardToHandSize active
      -- CR 514.2: damage wears off AND until-end-of-turn effects end,
      -- simultaneously. One sweep over both carriers (Pawl.Expiry).
      State.modify' Damage.removeAllDamage
      State.modify' Expiry.dropAtCleanup
```

Add the `Pawl.Expiry` import; drop `Pawl.Projection` / `Pawl.Event` imports **only** if nothing else in the module uses them (both are used elsewhere — keep them).

- [x] **Step 8: Sweep every remaining `duration`/`dropEndOfTurn` reference**

Run: `grep -rn 'ContinuousEffect\.duration\|ActiveReplacement\.duration\|dropEndOfTurnEffects\|dropEndOfTurnReplacements' source/`

Fix each hit mechanically:

| File:line | Change |
|---|---|
| `source/test-suite/Pawl/Support.hs:338`, `:362`, `:759` | `ContinuousEffect.duration = Duration.UntilEndOfTurn` / `ActiveReplacement.duration = …` → `… .expiry = Expiry.AtCleanup` |
| `source/test-suite/Pawl/DamageSpec.hs:165`, `:182`, `:443` | same field rename |
| `source/test-suite/Pawl/DamageSpec.hs:185` | `Event.dropEndOfTurnReplacements` → `Expiry.dropAtCleanup` |
| `source/test-suite/Pawl/EventSpec.hs:118` | `Event.dropEndOfTurnReplacements` → `Expiry.dropAtCleanup` |
| `source/test-suite/Pawl/ProjectionSpec.hs:64`, `:423` | field rename |
| `source/test-suite/Pawl/ReplacementSpec.hs:416` | field rename |
| `source/test-suite/Pawl/ResolveSpec.hs:890`, `:973`, `:995` | `Projection.dropEndOfTurnEffects` → `Expiry.dropAtCleanup` |
| `source/test-suite/Pawl/CombatSpec.hs:303` | field rename |

`Duration.UntilEndOfTurn` appearing inside an `Effect.…` value (`ResolveSpec.hs:968`, `CombatSpec.hs:279`, `CodecSpec.hs:145`) is **printed card data and stays a `Duration`** — do not touch those.

Also update the two comments in `source/library/Pawl/Type/GameState.hs` (lines 54 and 58) that say "each with a duration cleanup consults" to say "each with an expiry the Pawl.Expiry sweeps consult".

- [x] **Step 9: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — the new `Pawl.ExpirySpec` group green, and **every pre-existing test still green** (this task is behaviour-neutral).

- [x] **Step 10: Commit**

```bash
git add source/library/Pawl/Type/Expiry.hs source/library/Pawl/Expiry.hs source/test-suite/Pawl/ExpirySpec.hs source/library/Pawl source/test-suite pawl.cabal
hooky fix
git add -u
hooky run
git commit -m "refactor(m4.5-p6): split printed Duration from stored Expiry

Pawl.Expiry is the sole home of case ... Expiry and owns one CR 514.2
cleanup sweep over both carriers, absorbing and deleting
Projection.dropEndOfTurnEffects and Event.dropEndOfTurnReplacements.
Behaviour-neutral.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 2: "Until your next turn" — `AtTurnOf`, and the turn handoff

**Files:**
- Modify: `source/library/Pawl/Type/Duration.hs` (add `UntilYourNextTurn`)
- Modify: `source/library/Pawl/Expiry.hs` (`arm` widens; add `dropAtHandoff`)
- Modify: `source/library/Pawl/Resolve.hs` (three `Expiry.arm` call sites)
- Modify: `source/library/Pawl/Engine.hs:408-433` (`handoffTurn`)
- Modify: `source/library/Pawl/Codec.hs:398-410` (`durationToJson`/`jsonToDuration`)
- Test: `source/test-suite/Pawl/ExpirySpec.hs`, `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Consumes: `Expiry.arm`, `Expiry.dropAtCleanup`, `ContinuousEffect.expiry`, `ActiveReplacement.expiry` (Task 1).
- Produces: `Duration.UntilYourNextTurn`; `Expiry.arm :: PlayerId -> Duration -> Maybe Expiry`; `Expiry.dropAtHandoff :: GameState -> GameState`.

- [x] **Step 1: Write the failing tests**

In `source/test-suite/Pawl/ExpirySpec.hs`, add to `armTests`:

```haskell
      HU.testCase "CR 611.2a / 109.5 'until your next turn' bakes the controller" $
        HU.assertEqual "armed" (Just (Expiry.Type.AtTurnOf S.alice)) (Expiry.arm S.alice Duration.UntilYourNextTurn),
```

and change the two existing `armTests` cases to pass a controller (`Expiry.arm S.alice Duration.UntilEndOfTurn`, `Expiry.arm S.alice Duration.Indefinite`).

Add a new group and register it in `tests`:

```haskell
handoffTests :: Tasty.TestTree
handoffTests =
  Tasty.testGroup
    "DropAtHandoff"
    [ HU.testCase "CR 611.2a an AtTurnOf effect ends as that player's turn begins, not before" $
        let gs0 = Setup.emptyGame S.bothPlayers
            -- alice is the active player; the effect ends at ALICE's next turn.
            armed = effectWith (Expiry.Type.AtTurnOf S.alice) gs0
            bobsTurn = S.runPure S.identityAnswer armed Engine.handoffTurn
            alicesTurn = S.runPure S.identityAnswer bobsTurn Engine.handoffTurn
         in do
              HU.assertEqual "alice is active when it is created" S.alice (GameState.activePlayer armed)
              HU.assertEqual "it survives the creating turn's handoff" 1 (length (GameState.continuousEffects bobsTurn))
              HU.assertEqual "bob is active" S.bob (GameState.activePlayer bobsTurn)
              HU.assertEqual "it ends as alice's next turn begins" [] (GameState.continuousEffects alicesTurn),
      HU.testCase "CR 514.2 does not touch an AtTurnOf effect" $
        let gs0 = Setup.emptyGame S.bothPlayers
            armed = effectWith (Expiry.Type.AtTurnOf S.alice) gs0
         in HU.assertEqual "survives cleanup" 1 (length (GameState.continuousEffects (Expiry.dropAtCleanup armed))),
      HU.testCase "CR 611.2a the sweep is scoped to the player whose turn began" $
        let gs0 = Setup.emptyGame S.bothPlayers
            armed = effectWith (Expiry.Type.AtTurnOf S.bob) gs0
            bobsTurn = S.runPure S.identityAnswer armed Engine.handoffTurn
         in HU.assertEqual "bob's turn ends bob's effect" [] (GameState.continuousEffects bobsTurn)
    ]
```

In `source/test-suite/Pawl/CodecSpec.hs`, beside the existing `Duration` round-trips, add:

```haskell
      HU.testCase "Duration.UntilYourNextTurn round-trips" $
        HU.assertEqual "preserved" (Right Duration.UntilYourNextTurn) (Codec.jsonToDuration (Codec.durationToJson Duration.UntilYourNextTurn)),
```

(If `CodecSpec` has no standalone `Duration` group yet, put this case next to the `Effect.ModifyTarget` round-trip at line 145, in the same list.)

- [x] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Data constructor not in scope: Duration.UntilYourNextTurn`, `Expiry.Type.AtTurnOf` applied where `arm` takes one argument, `Variable not in scope: Expiry.dropAtHandoff`.

- [x] **Step 3: Add the printed duration**

In `source/library/Pawl/Type/Duration.hs`, add the constructor and rewrite the stale module comment:

```haskell
module Pawl.Type.Duration where

-- How long a stored continuous effect lasts, as the CARD says it (CR 611.2).
-- PRINTED data: this is what appears in card JSON. The game stores
-- Pawl.Type.Expiry instead, and Pawl.Expiry.arm is the one-way door between
-- them. Static-ability effects carry no Duration -- they last while their
-- source and ability do, which is "while re-derived from the battlefield".
data Duration
  = UntilEndOfTurn -- CR 514.2
  | Indefinite -- CR 611.2a: "lasts until the end of the game" (Magical Hack)
  | -- CR 611.2a: "until your next turn" (Hag of Inner Weakness). "Your" is
    -- resolved to a concrete player by Pawl.Expiry.arm (CR 109.5) -- it cannot
    -- be a PlayerId here, because a printed card does not know one.
    UntilYourNextTurn
  deriving (Eq, Ord, Show)
```

- [x] **Step 4: Widen `arm` and add `dropAtHandoff`**

In `source/library/Pawl/Expiry.hs`:

```haskell
-- CR 611.2: the moment a duration BEGINS. `controller` is the effect's
-- controller, which is CR 109.5's "you" for every duration that names a player.
arm :: PlayerId -> Duration -> Maybe Expiry
arm controller duration = case duration of
  Duration.UntilEndOfTurn -> Just Expiry.AtCleanup
  Duration.Indefinite -> Just Expiry.Never
  Duration.UntilYourNextTurn -> Just (Expiry.AtTurnOf controller)

-- CR 611.2a: "until your next turn" ends as that player's turn begins. Run at
-- the turn handoff, AFTER activePlayer has been updated, so "a turn began and
-- its active player is p" IS "p's next turn began" -- including when p created
-- the effect on their own turn (the handoff is the only caller, so this never
-- runs during the creating turn) and including extra turns. No per-effect
-- watermark is needed and none is stored.
--
-- Dropping here is observably identical to dropping "as the turn begins": CR
-- 500.12 (no game events occur between turns), CR 502.4 (no priority during
-- untap) and CR 704.3 (no state-based-action check without a player about to
-- receive priority) leave nothing that could observe the difference. The first
-- observation point is the upkeep step (CR 503.1).
dropAtHandoff :: GameState -> GameState
dropAtHandoff gs =
  let survives expiry = case expiry of
        Expiry.AtTurnOf pid -> pid /= GameState.activePlayer gs
        Expiry.AtCleanup -> True
        Expiry.Never -> True
        Expiry.While _ _ -> True
      keepEffect eff = survives (ContinuousEffect.expiry eff)
      keepReplacement active = survives (ActiveReplacement.expiry active)
   in gs
        { GameState.continuousEffects = filter keepEffect (GameState.continuousEffects gs),
          GameState.replacements = filter keepReplacement (GameState.replacements gs)
        }
```

Update the three `Expiry.arm duration` call sites in `source/library/Pawl/Resolve.hs` to `Expiry.arm controller duration` (`controller` is already bound in `applyEffect`'s scope at all three).

- [x] **Step 5: Wire the handoff**

In `source/library/Pawl/Engine.hs`, wrap `handoffTurn`'s existing record update in `Expiry.dropAtHandoff`. **Nothing inside the braces changes** — all nine assignments (`activePlayer`, `turnNumber`, `events`, `scannedThrough`, `damageScannedThrough`, `phase`, `remaining`, `activeControl`, `pendingControl`) and their comments stay exactly as they are. Only the two lines around them are new:

```haskell
handoffTurn :: Game ()
handoffTurn = State.modify' $ \gs ->
  let newActive = nextInOrder (GameState.turnOrder gs) (GameState.activePlayer gs)
   in -- CR 611.2a: with activePlayer already advanced, drop every "until your
      -- next turn" effect belonging to the player whose turn just began. The
      -- transition IS the event, known exactly here; see Pawl.Expiry.
      Expiry.dropAtHandoff $
        gs
          { GameState.activePlayer = newActive,
            GameState.turnNumber = GameState.turnNumber gs + 1,
            -- (the CR 608.2i log-clearing comment, unchanged)
            GameState.events = Seq.empty,
            GameState.scannedThrough = 0,
            GameState.damageScannedThrough = 0,
            GameState.phase = Turn.firstPhase,
            GameState.remaining = Turn.laterPhases,
            -- (the CR 723.1/723.1b comment, unchanged)
            GameState.activeControl = Map.lookup newActive (GameState.pendingControl gs),
            GameState.pendingControl = Map.delete newActive (GameState.pendingControl gs)
          }
```

`dropAtHandoff` reads `GameState.activePlayer` from the record it is handed, which is why it must wrap the update rather than precede it.

- [x] **Step 6: Add the codec arm**

In `source/library/Pawl/Codec.hs`, add `Duration.UntilYourNextTurn -> "UntilYourNextTurn"` to `durationToJson`'s case and `(Text.pack "UntilYourNextTurn", Duration.UntilYourNextTurn)` to `jsonToDuration`'s table.

- [x] **Step 7: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — `DropAtHandoff` green, `Arm` green, the codec round-trip green, everything else unchanged.

- [x] **Step 8: Commit**

```bash
git add source/library/Pawl source/test-suite
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p6): 'until your next turn' expires at the turn handoff

CR 611.2a: Duration.UntilYourNextTurn arms to Expiry.AtTurnOf with CR
109.5's 'you' baked in; Engine.handoffTurn sweeps it once activePlayer
has advanced. The transition is the event -- no event-log watermark.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 3: "For as long as" — `YouControlSource`, the arming check, and the conditional sweep

**Files:**
- Modify: `source/library/Pawl/Type/StateCondition.hs` (add `YouControlSource`)
- Modify: `source/library/Pawl/Type/Duration.hs` (add `ForAsLongAs StateCondition`)
- Modify: `source/library/Pawl/Event.hs` (`stateHolds` gains an `ObjectId`; two internal call sites)
- Modify: `source/library/Pawl/Stack.hs:67` (third `stateHolds` call site)
- Modify: `source/library/Pawl/Expiry.hs` (`arm` widens again; add `sweepConditional`)
- Modify: `source/library/Pawl/Resolve.hs` (three `Expiry.arm` call sites)
- Modify: `source/library/Pawl/Engine.hs:336-351` (`settleForPriority`)
- Modify: `source/library/Pawl/Codec.hs` (`stateConditionToJson`/`jsonToStateCondition`; restructure `durationToJson`/`jsonToDuration`)
- Test: `source/test-suite/Pawl/ExpirySpec.hs`, `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Consumes: `Expiry.arm :: PlayerId -> Duration -> Maybe Expiry`, `Expiry.dropAtHandoff` (Task 2).
- Produces: `StateCondition.YouControlSource`; `Duration.ForAsLongAs StateCondition`; `Event.stateHolds :: PlayerId -> ObjectId -> StateCondition -> GameState -> Bool`; `Expiry.arm :: PlayerId -> ObjectId -> Duration -> GameState -> Maybe Expiry`; `Expiry.sweepConditional :: Game Bool`.

- [x] **Step 1: Write the failing tests**

In `source/test-suite/Pawl/ExpirySpec.hs`, add a group (and register it in `tests`):

```haskell
-- A stored continuous effect whose expiry is a live condition over `src`,
-- affecting `target`. The Master Thief shape, hand-built so the sweep can be
-- tested before the card exists.
whileEffect :: ObjectId.ObjectId -> ObjectId.ObjectId -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
whileEffect src target you gs =
  let (ts, gs1) = Game.freshTimestamp gs
      eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = src,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.expiry = Expiry.Type.While you StateCondition.YouControlSource,
            ContinuousEffect.modification = Modification.SetController you,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton target)
          }
   in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}

conditionalTests :: Cards.Cards -> Tasty.TestTree
conditionalTests cards =
  let board =
        let gs0 = Setup.emptyGame S.bothPlayers
            (srcId, gs1) = S.addPiker cards S.alice gs0
            (targetId, gs2) = S.addCreature (Cards.warMammothPrinting cards) S.bob gs1
         in (srcId, targetId, whileEffect srcId targetId S.alice gs2)
   in Tasty.testGroup
        "Conditional"
        [ HU.testCase "CR 611.2b YouControlSource holds while the source is controlled" $
            let (srcId, _, gs) = board
             in HU.assertBool "holds" (Event.stateHolds S.alice srcId StateCondition.YouControlSource gs),
          HU.testCase "CR 613.1b it stops holding when another player gains control of the source" $
            let (srcId, _, gs) = board
                stolen = S.giveControl srcId S.bob gs
             in HU.assertBool "no longer holds" (not (Event.stateHolds S.alice srcId StateCondition.YouControlSource stolen)),
          HU.testCase "CR 400.7 it stops holding when the source leaves the battlefield" $
            let (srcId, _, gs) = board
                gone = S.runPure S.identityAnswer gs (Event.destroy srcId)
             in HU.assertBool "no longer holds" (not (Event.stateHolds S.alice srcId StateCondition.YouControlSource gone)),
          HU.testCase "CR 611.2b arm returns Nothing when the condition is already false" $
            let (srcId, _, gs) = board
                gone = S.runPure S.identityAnswer gs (Event.destroy srcId)
             in HU.assertEqual
                  "never starts"
                  Nothing
                  (Expiry.arm S.alice srcId (Duration.ForAsLongAs StateCondition.YouControlSource) gone),
          HU.testCase "CR 611.2b arm returns a While when the condition holds now" $
            let (srcId, _, gs) = board
             in HU.assertEqual
                  "starts"
                  (Just (Expiry.Type.While S.alice StateCondition.YouControlSource))
                  (Expiry.arm S.alice srcId (Duration.ForAsLongAs StateCondition.YouControlSource) gs),
          HU.testCase "CR 611.2b the sweep DELETES the effect once the condition fails" $
            let (srcId, targetId, gs) = board
                gone = S.runPure S.identityAnswer gs (Event.destroy srcId)
                (changed, swept) = Engine.runGamePure S.identityAnswer gone Expiry.sweepConditional
             in do
                  HU.assertEqual "alice held it while the source stood" (Just S.alice) (Projection.controllerOf targetId gs)
                  HU.assertBool "the sweep reports a change" changed
                  HU.assertEqual "the effect is gone, not masked" [] (GameState.continuousEffects swept)
                  HU.assertEqual "control reverted" (Just S.bob) (Projection.controllerOf targetId swept),
          HU.testCase "CR 611.2b a sweep that changes nothing reports False" $
            let (_, _, gs) = board
                (changed, _) = Engine.runGamePure S.identityAnswer gs Expiry.sweepConditional
             in HU.assertBool "no change" (not changed),
          HU.testCase "CR 704.3 settleForPriority runs the sweep" $
            let (srcId, targetId, gs) = board
                gone = S.runPure S.identityAnswer gs (Event.destroy srcId)
                settled = S.runPure S.identityAnswer gone Engine.settleForPriority
             in HU.assertEqual "control reverted at the settle" (Just S.bob) (Projection.controllerOf targetId settled)
        ]
```

In `source/test-suite/Pawl/CodecSpec.hs`, add:

```haskell
      HU.testCase "StateCondition.YouControlSource round-trips" $
        HU.assertEqual "preserved" (Right StateCondition.YouControlSource) (Codec.jsonToStateCondition (Codec.stateConditionToJson StateCondition.YouControlSource)),
      HU.testCase "Duration.ForAsLongAs round-trips with its condition" $
        let d = Duration.ForAsLongAs StateCondition.YouControlSource
         in HU.assertEqual "preserved" (Right d) (Codec.jsonToDuration (Codec.durationToJson d)),
```

- [x] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Data constructor not in scope: StateCondition.YouControlSource`, `Duration.ForAsLongAs`, `Variable not in scope: Expiry.sweepConditional`, and `Event.stateHolds` applied to four arguments.

- [x] **Step 3: Add the condition and the duration**

In `source/library/Pawl/Type/StateCondition.hs`, add the third arm (keep the existing module comment, but change "Two customers, one vocabulary" to "Three customers, one vocabulary" and name the third):

```haskell
  | -- CR 611.2b: Master Thief, "for as long as you control this creature". The
    -- OBJECT is the effect's own source, supplied by the caller; "you" is the
    -- PlayerId the surrounding Expiry.While carries. Both halves are
    -- load-bearing: the source must still be on the battlefield, and its
    -- PROJECTED controller (CR 613.1b) must be that player. CR 400.7 makes the
    -- first robust for free -- a permanent that dies and returns is a new object
    -- with a new id, so the stored source can never be satisfied by it.
    YouControlSource
```

In `source/library/Pawl/Type/Duration.hs`, add:

```haskell
  | -- CR 611.2b: "for as long as ...". The duration has a BEGINNING as well as
    -- an end -- "if the 'for as long as' duration never starts, the effect does
    -- nothing" -- which is why Pawl.Expiry.arm returns a Maybe.
    ForAsLongAs StateCondition
```

- [x] **Step 4: Give `stateHolds` its source**

In `source/library/Pawl/Event.hs`:

```haskell
-- CR 603.8 / 603.4 / 611.2b: is this state condition currently true, for an
-- ability or effect whose controller is `you` and whose source object is
-- `source`? Reads the PROJECTION -- a subtype is CR 613 layer 4 and control is
-- layer 2, so Blood Moon and Act of Treason both change the answer. This module
-- is the sole home of casing on StateCondition; the two subtype arms ignore
-- `source`.
stateHolds :: PlayerId -> ObjectId -> StateCondition -> GameState -> Bool
stateHolds you source cond gs =
  let hasSubtype subtype oid = Set.member subtype (Projection.subtypesOf oid gs)
   in case cond of
        -- CR 109.5: "you" on a triggered ability's condition means the
        -- ability's controller (at the time it triggered) -- so "you control"
        -- is that player's projected-controlled permanents.
        StateCondition.YouControlNo subtype -> not (any (hasSubtype subtype) (Projection.controls you gs))
        -- Any player's -- the whole battlefield.
        StateCondition.NoPermanentsOfSubtype subtype -> not (any (hasSubtype subtype) (Set.toList (GameState.battlefield gs)))
        -- CR 611.2b / 613.1b / 400.7: the source object is still on the
        -- battlefield AND its projected controller is `you`.
        StateCondition.YouControlSource ->
          Set.member source (GameState.battlefield gs) && Projection.controllerOf source gs == Just you
```

Update its three call sites, each of which already has the source in scope:
- `Event.stateTriggers`'s `live` helper: `stateHolds ctrl cond gs` → `stateHolds ctrl oid cond gs`.
- `Event.interveningHolds`: `stateHolds (PendingTrigger.controller pending) cond gs` → `stateHolds (PendingTrigger.controller pending) (PendingTrigger.source pending) cond gs`.
- `Pawl.Stack.hs:67`: `Event.stateHolds (Object.owner obj) cond gs` → `Event.stateHolds (Object.owner obj) srcId cond gs` (`srcId` is bound by the enclosing `Source.OfTrigger srcId ability` pattern).

- [x] **Step 5: Widen `arm` and add `sweepConditional`**

In `source/library/Pawl/Expiry.hs` (add the `Pawl.Event`, `Pawl.Type.Game`, `Control.Monad.Trans.State.Strict` and `Pawl.Type.ObjectId` imports):

```haskell
-- CR 611.2: the moment a duration BEGINS. `controller` is the effect's
-- controller -- CR 109.5's "you" -- and `source` is the object the effect comes
-- from. Nothing means the duration never started, so per CR 611.2b the effect
-- does nothing and is never stored at all.
--
-- CR 611.2b's second sentence -- "if that duration ends before the moment the
-- effect would first be applied and doesn't begin again during that spell or
-- ability's resolution" -- is vacuous here: this runs once, at the point the
-- effect would be stored, and no opcode both ends and restarts a condition
-- mid-resolution.
arm :: PlayerId -> ObjectId -> Duration -> GameState -> Maybe Expiry
arm controller source duration gs = case duration of
  Duration.UntilEndOfTurn -> Just Expiry.AtCleanup
  Duration.Indefinite -> Just Expiry.Never
  Duration.UntilYourNextTurn -> Just (Expiry.AtTurnOf controller)
  Duration.ForAsLongAs cond ->
    if Event.stateHolds controller source cond gs
      then Just (Expiry.While controller cond)
      else Nothing

-- CR 611.2b: drop every While whose condition has stopped holding. The effect is
-- DELETED, not masked: 611.2b's duration is one continuous period, so an effect
-- that has ended must stay ended even if the condition becomes true again
-- ("Regaining control of Master Thief won't cause you to regain control of the
-- artifact"). Reports whether it changed anything, so Engine.settleForPriority
-- knows to run again.
--
-- CR 704.3 fixes the coarsest moment anything can OBSERVE the condition --
-- "whenever a player would get priority" -- and settleForPriority runs at
-- exactly the points where the board can change, so checking here is
-- indistinguishable from checking continuously.
sweepConditional :: Game Bool
sweepConditional = do
  gs <- State.get
  let survives source expiry = case expiry of
        Expiry.While you cond -> Event.stateHolds you source cond gs
        Expiry.AtCleanup -> True
        Expiry.Never -> True
        Expiry.AtTurnOf _ -> True
      keepEffect eff = survives (ContinuousEffect.source eff) (ContinuousEffect.expiry eff)
      keepReplacement active = survives (ActiveReplacement.source active) (ActiveReplacement.expiry active)
      keptEffects = filter keepEffect (GameState.continuousEffects gs)
      keptReplacements = filter keepReplacement (GameState.replacements gs)
  State.put gs {GameState.continuousEffects = keptEffects, GameState.replacements = keptReplacements}
  pure (keptEffects /= GameState.continuousEffects gs || keptReplacements /= GameState.replacements gs)
```

Update the three `Expiry.arm controller duration` call sites in `source/library/Pawl/Resolve.hs` to `Expiry.arm controller source duration gs`. In the `ModifyTarget` and `GainControl` arms the surrounding `State.modify' $ \gs -> …` already binds `gs`; in the `Replace` arm it does too. `source` is `applyEffect`'s first parameter.

- [x] **Step 6: Put the sweep at the head of the settle loop**

In `source/library/Pawl/Engine.hs`:

```haskell
settleForPriority :: Game ()
settleForPriority = do
  swept <- Expiry.sweepConditional
  acted <- Sba.performStateBasedActions
  placed <- placePendingTriggers
  Monad.when (swept || acted || placed) settleForPriority
```

Extend the existing comment above it: CR 611.2b's condition is checked continuously and CR 704.3 makes "whenever a player would get priority" the coarsest observable moment; the sweep runs **first** because losing control of a permanent changes what the state-based-action check sees, and the loop re-runs whenever anything fired because an SBA can be what falsifies a condition. A game with no `While` stored pays one list scan.

- [x] **Step 7: Add the codec arms**

In `source/library/Pawl/Codec.hs`, `stateConditionToJson` gains `StateCondition.YouControlSource -> nullary (Text.pack "YouControlSource")` and `jsonToStateCondition` gains `("YouControlSource", _) -> Right StateCondition.YouControlSource`.

`Duration` now has a payload-carrying arm, so its pair stops using `nullary . Text.pack $ case` / `decodeNullary`:

```haskell
durationToJson :: Duration.Duration -> Value
durationToJson d = case d of
  Duration.UntilEndOfTurn -> nullary (Text.pack "UntilEndOfTurn")
  Duration.Indefinite -> nullary (Text.pack "Indefinite")
  Duration.UntilYourNextTurn -> nullary (Text.pack "UntilYourNextTurn")
  Duration.ForAsLongAs c -> Json.tagged (Text.pack "ForAsLongAs") (Just (stateConditionToJson c))

jsonToDuration :: Value -> Either Text Duration.Duration
jsonToDuration value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("UntilEndOfTurn", _) -> Right Duration.UntilEndOfTurn
    ("Indefinite", _) -> Right Duration.Indefinite
    ("UntilYourNextTurn", _) -> Right Duration.UntilYourNextTurn
    ("ForAsLongAs", Just v) -> Duration.ForAsLongAs <$> jsonToStateCondition v
    _ -> Left (Text.pack "unknown Duration: " <> t)
```

- [x] **Step 8: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — the `Conditional` group green, the two codec round-trips green, and every existing test (notably `TriggerSpec`'s state-trigger and intervening-"if" groups, which exercise the widened `stateHolds`) still green.

- [x] **Step 9: Commit**

```bash
git add source/library/Pawl source/test-suite
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p6): 'for as long as' durations, armed and swept

CR 611.2b: a duration now has a beginning -- Expiry.arm returns Nothing
when the condition is already false, so the effect is never stored -- and
Expiry.sweepConditional DELETES a While whose condition has failed, at
the head of settleForPriority (CR 704.3). StateCondition gains its third
customer (#38 stands).

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 4: Targeting becomes source-relative — `ArtifactTarget` and `OpponentCreatureTarget`

**Files:**
- Modify: `source/library/Pawl/Type/TargetSpec.hs` (two new arms)
- Modify: `source/library/Pawl/Target.hs` (`legalRecipients`, `stillLegal`, `legalSets`, `selfExcludes`, `legalSetsExcluding`)
- Modify: `source/library/Pawl/Resolve.hs:285`, `:320` (two `stillLegal` call sites)
- Modify: `source/library/Pawl/Codec.hs:585-612`
- Modify: `source/test-suite/Pawl/Support.hs` (add `noSource`)
- Test: `source/test-suite/Pawl/ResolveSpec.hs`, `source/test-suite/Pawl/ColorSpec.hs`, `source/test-suite/Pawl/ModalSpec.hs`, `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Consumes: nothing from Tasks 1–3 (independent of all three).
- Produces: `TargetSpec.ArtifactTarget`, `TargetSpec.OpponentCreatureTarget`; `Target.legalRecipients :: ObjectId -> TargetSpec -> GameState -> Set Recipient`; `Target.stillLegal :: ObjectId -> Recipient -> TargetSpec -> GameState -> Bool`; `Target.legalSets :: ObjectId -> Map SlotName TargetSpec -> GameState -> Map SlotName (Set Recipient)`; `S.noSource :: ObjectId`.

- [x] **Step 1: Write the failing tests**

In `source/test-suite/Pawl/ResolveSpec.hs`, inside `targetTests`, add:

```haskell
      HU.testCase "CR 115.1a ArtifactTarget is the battlefield's projected artifacts" $
        -- boardWithCreatureArtifactLand: alice has a Piker, a Mindslaver
        -- (Legendary Artifact) and a Mountain.
        let gs = S.boardWithCreatureArtifactLand cards
            legal = Target.legalRecipients S.noSource TargetSpec.ArtifactTarget gs
         in do
              HU.assertEqual "exactly the artifact" (Set.singleton (Recipient.ToObject (S.artifactId gs))) legal
              HU.assertBool "no players" (not (Set.member (Recipient.ToPlayer S.alice) legal)),
      HU.testCase "CR 115.1a / 109.5 OpponentCreatureTarget excludes the source's controller's creatures" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (mine, gs1) = S.addPiker cards S.alice gs0
            (theirs, gs2) = S.addCreature (Cards.warMammothPrinting cards) S.bob gs1
            legal = Target.legalRecipients mine TargetSpec.OpponentCreatureTarget gs2
         in do
              HU.assertEqual "only the opponent's creature" (Set.singleton (Recipient.ToCreature theirs)) legal
              HU.assertBool "not the source's controller's own" (not (Set.member (Recipient.ToCreature mine) legal)),
      HU.testCase "CR 613.1b OpponentCreatureTarget follows PROJECTED control, not ownership" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (mine, gs1) = S.addPiker cards S.alice gs0
            (theirs, gs2) = S.addCreature (Cards.warMammothPrinting cards) S.bob gs1
            (alsoTheirs, gs3) = S.addCreature (Cards.typhoidRatsPrinting cards) S.bob gs2
            -- alice steals one of bob's creatures: it stops being "a creature an
            -- opponent controls" for alice's source, and becomes one for bob's.
            stolen = S.giveControl theirs S.alice gs3
         in do
              HU.assertEqual
                "for alice's source, only the creature still under bob's control"
                (Set.singleton (Recipient.ToCreature alsoTheirs))
                (Target.legalRecipients mine TargetSpec.OpponentCreatureTarget stolen)
              HU.assertEqual
                "for bob's source, the two alice now controls"
                (Set.fromList [Recipient.ToCreature mine, Recipient.ToCreature theirs])
                (Target.legalRecipients alsoTheirs TargetSpec.OpponentCreatureTarget stolen),
```

The third case reads control **twice**: `alsoTheirs` (Typhoid Rats, still bob's) is the only legal target for alice's source, and from bob's remaining source the two creatures alice now controls — her own Piker and the Mammoth she stole — are both legal. `OpponentCreatureTarget` does not self-exclude, but neither source is ever in its own legal set here, because a source is always controlled by its own controller.

In `source/test-suite/Pawl/CodecSpec.hs`, add:

```haskell
      HU.testCase "TargetSpec.ArtifactTarget round-trips" $
        HU.assertEqual "preserved" (Right TargetSpec.ArtifactTarget) (Codec.jsonToTargetSpec (Codec.targetSpecToJson TargetSpec.ArtifactTarget)),
      HU.testCase "TargetSpec.OpponentCreatureTarget round-trips" $
        HU.assertEqual "preserved" (Right TargetSpec.OpponentCreatureTarget) (Codec.jsonToTargetSpec (Codec.targetSpecToJson TargetSpec.OpponentCreatureTarget)),
```

- [x] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Data constructor not in scope: TargetSpec.ArtifactTarget`, `TargetSpec.OpponentCreatureTarget`, `Variable not in scope: S.noSource`, and `Target.legalRecipients` applied to three arguments.

- [x] **Step 3: Add the two specs**

In `source/library/Pawl/Type/TargetSpec.hs`:

```haskell
  | -- CR 115.1a: "target artifact" -- a permanent on the battlefield whose
    -- PROJECTED card types (M3c layer 4) include Artifact. Reads the
    -- projection, never Card.typeLine: a permanent made an artifact by a
    -- type-changing effect is a legal target and a printed artifact that lost
    -- the type is not. Master Thief's slot.
    --
    -- The WallTarget posture: one hand-carved variant, specific before general.
    -- P9's criterion/filter language replaces the whole family (#40).
    ArtifactTarget
  | -- CR 115.1a with CR 109.5: "target creature an opponent controls" -- a
    -- creature on the battlefield whose PROJECTED controller (CR 613.1b) is not
    -- the targeting source's controller. The first spec whose legal set depends
    -- on WHO IS CHOOSING, which is what makes Pawl.Target source-relative. Hag
    -- of Inner Weakness's slot. Retired with the rest of the family (#40).
    OpponentCreatureTarget
```

- [x] **Step 4: Make `Pawl.Target` source-relative**

In `source/library/Pawl/Target.hs`:

```haskell
-- CR 115.4: "any target" is a creature, player, planeswalker, or battle; the
-- last two card types do not exist yet, so this is creatures on the
-- battlefield plus players still in the game. No restriction (protection,
-- hexproof, shroud) exists in the pool -- this function is where they will
-- all land.
--
-- `source` is the object the targeting is relative to: the spell object at
-- cast, the source permanent for an ability. Every spec but
-- OpponentCreatureTarget ignores it -- a legal set that depends on WHO IS
-- CHOOSING (CR 109.5) is what forced it in. Self-exclusion ("another") is NOT
-- applied here; that stays in legalSetsExcluding.
legalRecipients :: ObjectId -> TargetSpec -> GameState -> Set Recipient
legalRecipients source spec gs =
```

The `let`-bound `isCreatureId`/`creatures`/`players` helpers and all ten existing `case spec of` arms are **unchanged**; only the parameter and these two arms, appended after `TargetSpec.NonblackCreatureTarget`, are new:

```haskell
        TargetSpec.ArtifactTarget ->
          -- CR 115.1a: a battlefield permanent whose PROJECTED card types (M3c)
          -- include Artifact -- source-blind.
          let isArtifact oid = Set.member CardType.Artifact (Projection.cardTypesOf oid gs)
              matches = filter isArtifact (Set.toList (GameState.battlefield gs))
           in Set.fromList (map Recipient.ToObject matches)
        TargetSpec.OpponentCreatureTarget ->
          -- CR 115.1a / 109.5 / 613.1b: CreatureTarget's set narrowed to
          -- creatures whose PROJECTED controller is not the source's
          -- controller. A source that has left the battlefield has no projected
          -- controller, and this yields the EMPTY set rather than falling back
          -- to last known information (CR 608.2h), so CR 608.2b's re-check
          -- wrongly fizzles an ability whose source died in response (#N).
          let mine = Projection.controllerOf source gs
              theirs recipient = case recipient of
                Recipient.ToCreature oid -> case mine of
                  Nothing -> False
                  Just pid -> Projection.controllerOf oid gs /= Just pid
                Recipient.ToPlayer _ -> False
                Recipient.ToObject _ -> False
           in Set.fromList (filter theirs creatures)
```

`stillLegal` and `legalSets` take the source and pass it through; `legalSetsExcluding` and `fillableModes` already hold one:

```haskell
stillLegal :: ObjectId -> Recipient -> TargetSpec -> GameState -> Bool
stillLegal source recipient spec gs = Set.member recipient (legalRecipients source spec gs)

legalSets :: ObjectId -> Map SlotName TargetSpec -> GameState -> Map SlotName (Set Recipient)
legalSets source specs gs = Map.map (\spec -> legalRecipients source spec gs) specs
```

Update `legalSetsExcluding`'s body to `legalRecipients source spec gs`, and add `TargetSpec.ArtifactTarget -> False` / `TargetSpec.OpponentCreatureTarget -> False` to `selfExcludes` (which stays untouched otherwise — folding self-exclusion into the now-source-aware `legalRecipients` is explicitly not part of this phase).

Update `stillLegal`'s two call sites in `source/library/Pawl/Resolve.hs` (lines 285 and 320) to `Target.stillLegal source recipient spec gs` and `Target.stillLegal srcId recipient spec gs` respectively — check which identifier is in scope in each function (`resolveSpell`'s is the spell object; `resolveEffects`'s is `srcId`) and use it. CR 608.2b's re-check is now controller-relative: a creature that comes under your control in response is no longer a legal target.

- [x] **Step 5: Add the codec arms and the test stand-in**

In `source/library/Pawl/Codec.hs`, add `TargetSpec.ArtifactTarget -> "ArtifactTarget"` and `TargetSpec.OpponentCreatureTarget -> "OpponentCreatureTarget"` to `targetSpecToJson`, and the matching two entries to `jsonToTargetSpec`'s table.

In `source/test-suite/Pawl/Support.hs`, add:

```haskell
-- The source stand-in for a targeting call whose spec is source-blind (every
-- spec but OpponentCreatureTarget). Object id 999 names nothing, the same
-- posture withEffectAt's 998 takes.
noSource :: ObjectId.ObjectId
noSource = ObjectId.MkObjectId 999
```

- [x] **Step 6: Fix every other call site**

Run: `grep -rn 'Target\.legalRecipients\|Target\.stillLegal\|Target\.legalSets ' source/test-suite/`

Pass `S.noSource` at each of the `ColorSpec.hs`, `ResolveSpec.hs` and `ModalSpec.hs` hits — every one of them exercises a source-blind spec, so the stand-in is honest there. Do **not** change `Target.legalSetsExcluding` call sites (they already pass a real source).

- [x] **Step 7: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — the three new targeting cases and the two codec round-trips green, everything else unchanged.

- [x] **Step 8: Commit**

```bash
git add source/library/Pawl source/test-suite
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p6): targeting becomes source-relative

CR 115.1a: ArtifactTarget (Master Thief) and OpponentCreatureTarget (Hag
of Inner Weakness). The second is the first spec whose legal set depends
on who is choosing (CR 109.5), so legalRecipients/stillLegal/legalSets
take the targeting source. Both retired by P9 (#40).

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 5: Master Thief — the conditional gate, and the latch

The gate card for CR 611.2b, and CR 611.2b's own printed example (`rules.txt` line 2907). Its three Gatherer rulings are the specification of tests 2–4.

**Files:**
- Modify: `source/library/Pawl/Type/Subtype.hs` (add `Rogue`), `source/library/Pawl/Codec.hs` (its two arms)
- Create: `data/cards/master-thief.json`
- Modify: `source/test-suite/Pawl/Cards.hs` (record field, `loadCards`, `allPrintings`)
- Test: `source/test-suite/Pawl/ExpirySpec.hs` (§5 tests 1–4), `source/test-suite/Pawl/CardSpec.hs` (card-shape assertion)

**Interfaces:**
- Consumes: `Duration.ForAsLongAs`, `StateCondition.YouControlSource` (Task 3); `TargetSpec.ArtifactTarget` (Task 4); `Expiry.dropAtCleanup`, `Expiry.sweepConditional`.
- Produces: `Subtype.Rogue`; `Cards.masterThiefPrinting :: Cards -> Printing`.

- [x] **Step 1: Write the failing tests**

In `source/test-suite/Pawl/ExpirySpec.hs`, add a group (and register it in `tests`):

```haskell
-- Master Thief {2}{U}{U} Creature -- Human Rogue 2/2: "When this creature
-- enters, gain control of target artifact for as long as you control this
-- creature." CR 611.2b's own printed example; the three assertions below in
-- tests 2-4 are its three Gatherer rulings, verbatim.
masterThiefTests :: Cards.Cards -> Tasty.TestTree
masterThiefTests cards =
  let settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority
      resolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop
      -- bob's Darksteel Myr (Artifact Creature -- Myr, 0/1) is the only artifact
      -- on the board, so the CR 603.3d target choice is forced.
      board =
        let gs0 = Setup.emptyGame S.bothPlayers
            (myrId, gs1) = S.addCreature (Cards.darksteelMyrPrinting cards) S.bob gs0
            (thiefId, gs2) = S.addCreature (Cards.masterThiefPrinting cards) S.alice gs1
            entered = ZoneChange.MkZoneChange thiefId Zone.Stack Zone.Battlefield
            gs3 = S.withEvent (GameEvent.Moved entered (Projection.project thiefId gs2)) gs2
         in (thiefId, myrId, gs3)
      (thief, myr, entering) = board
      stolen = resolveAll (settle entering)
   in Tasty.testGroup
        "MasterThief"
        [ HU.testCase "CR 611.2b it works: the ETB resolves and control of the artifact changes" $
            do
              HU.assertEqual "alice controls the Myr" (Just S.alice) (Projection.controllerOf myr stolen)
              -- CR 302.6: the new controller has not controlled it continuously.
              HU.assertEqual "and it is re-Sicked" (Just Sickness.Sick) (fmap Object.sickness (Game.lookupObject myr stolen)),
          -- Ruling: "If Master Thief leaves the battlefield, you no longer
          -- control it, and its control-change effect ends."
          HU.testCase "CR 611.2b leaving the battlefield ends it" $
            let dead = S.runPure S.identityAnswer stolen (Event.destroy thief)
                swept = settle dead
             in do
                  HU.assertEqual "control reverts at the next settle" (Just S.bob) (Projection.controllerOf myr swept)
                  HU.assertEqual "and stays reverted" (Just S.bob) (Projection.controllerOf myr (settle swept)),
          -- Ruling: "If Master Thief ceases to be under your control before its
          -- ability resolves, you won't gain control of the targeted artifact at
          -- all." CR 704.5g destroys it for lethal damage while the trigger is
          -- on the stack; the trigger still RESOLVES (its target is legal, CR
          -- 608.2b), but the duration never starts.
          HU.testCase "CR 611.2b the duration never starts, so no effect is stored" $
            let onStack = settle entering
                lethal = S.settleSba (S.markDamage thief 2 onStack)
                after = resolveAll lethal
             in do
                  HU.assertBool "the trigger really was on the stack" (not (null (GameState.stack onStack)))
                  HU.assertEqual "Master Thief died before it resolved" Nothing (Game.lookupObject thief after)
                  HU.assertEqual "nothing was stored" [] (GameState.continuousEffects after)
                  HU.assertEqual "control never changed" (Just S.bob) (Projection.controllerOf myr after),
          -- Ruling: "If another player gains control of Master Thief, its
          -- control-change effect ends. Regaining control of Master Thief won't
          -- cause you to regain control of the artifact." THE FALSIFIER: an
          -- implementation that filters the effect out of the projection while
          -- the condition is false, instead of deleting it, fails exactly here.
          HU.testCase "CR 611.2b the latch: regaining the source does not regain the artifact" $
            let taken = S.giveControl thief S.bob stolen
                swept = settle taken
                returned = Expiry.dropAtCleanup swept
                relatched = settle returned
             in do
                  HU.assertEqual "bob has Master Thief" (Just S.bob) (Projection.controllerOf thief taken)
                  HU.assertEqual "so the artifact goes back to its owner" (Just S.bob) (Projection.controllerOf myr swept)
                  HU.assertEqual "at cleanup Master Thief comes home" (Just S.alice) (Projection.controllerOf thief returned)
                  HU.assertEqual "and the artifact does NOT" (Just S.bob) (Projection.controllerOf myr relatched)
        ]
```

In `source/test-suite/Pawl/CardSpec.hs`, add a new group `m45p6CardTests` (registered in `tests`) with:

```haskell
      HU.testCase "Master Thief is a {2}{U}{U} 2/2 Human Rogue whose ETB steals an artifact" $
        let c = Printing.card (Cards.masterThiefPrinting cards)
            blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)
            slot = SlotName.MkSlotName (Text.pack "target")
         in do
              HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, blue, blue])) (Card.Type.manaCost c)
              HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power c)
              HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 2))) (Card.Type.toughness c)
              HU.assertEqual "subtypes" (Set.fromList [Subtype.Human, Subtype.Rogue]) (TypeLine.subtypes (Card.Type.typeLine c))
              case Card.Type.triggeredAbilities c of
                [ab] -> do
                  HU.assertEqual "enters trigger" TriggerCondition.SelfEnters (TriggeredAbility.condition ab)
                  case Foldable.toList (Modal.modes (TriggeredAbility.modal ab)) of
                    [m] -> do
                      HU.assertEqual
                        "one GainControl effect with a conditional duration"
                        [Effect.GainControl (Duration.ForAsLongAs StateCondition.YouControlSource) slot]
                        (Foldable.toList (Mode.effects m))
                      HU.assertEqual
                        "one ArtifactTarget slot"
                        (Map.singleton slot TargetSpec.ArtifactTarget)
                        (Mode.targetSpecs m)
                    _ -> HU.assertFailure "expected exactly one mode"
                _ -> HU.assertFailure "expected exactly one triggered ability",
```

Note the accessors: `CardSpec` imports **`Pawl.Type.Modal` as `Modal`** (the type, giving `Modal.modes`) and `Pawl.Type.Mode` as `Mode`, not `Pawl.Modal` — so `Mode.effects`/`Mode.targetSpecs` over the single mode is the reachable spelling here, not `Modal.allEffects`. Add the `Pawl.Type.Duration`, `Pawl.Type.StateCondition`, `Pawl.Type.TriggerCondition` and `Pawl.Type.TriggeredAbility` imports.

- [x] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Variable not in scope: Cards.masterThiefPrinting`, `Data constructor not in scope: Subtype.Rogue`.

- [x] **Step 3: Add the subtype**

In `source/library/Pawl/Type/Subtype.hs`, append `| Rogue -- CR 205.3m (a creature type; Master Thief's)` **at the end** of the constructor list. Order matters: the JSON renders subtype sets in `Set.toAscList` (declaration) order, so `Human` must precede `Rogue`.

Add the matching arms to `subtypeToJson` and `jsonToSubtype` in `source/library/Pawl/Codec.hs`, in the same position.

- [x] **Step 4: Write the card file**

Create `data/cards/master-thief.json` with exactly this content, on one line, with a trailing newline:

```json
{"name":"Master Thief","manaCost":[{"type":"Generic","value":2},{"type":"OfType","value":{"type":"Colored","value":{"type":"Blue"}}},{"type":"OfType","value":{"type":"Colored","value":{"type":"Blue"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Creature"}],"subtypes":[{"type":"Human"},{"type":"Rogue"}]},"power":{"type":"Literal","value":2},"toughness":{"type":"Literal","value":2},"keywords":[],"staticAbilities":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[{"condition":{"type":"SelfEnters"},"modal":{"modes":[{"effects":[{"type":"GainControl","value":[{"type":"ForAsLongAs","value":{"type":"YouControlSource"}},"target"]}],"targetSpecs":[{"slot":"target","spec":{"type":"ArtifactTarget"}}]}],"selection":{"type":"ChooseExactly","value":1}}}],"castingPermissions":[]}
```

`CardsSpec.checkFile` asserts **byte-stability** — the committed file must equal `Json.render (Codec.printingToJson p) <> "\n"` exactly. If it reports a mismatch, the render is authoritative: canonicalize the file through the codec rather than weakening the assertion.

```bash
cabal repl lib:pawl <<'HS'
import qualified Pawl.Codec as Codec
import qualified Pawl.Json as Json
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
:{
let regen p = do
      c <- TIO.readFile p
      case Json.parse c >>= Codec.jsonToPrinting of
        Left e -> putStrLn (p ++ ": " ++ T.unpack e)
        Right pr -> TIO.writeFile p (Json.render (Codec.printingToJson pr) <> T.pack "\n")
:}
regen "data/cards/master-thief.json"
HS
```

Expected: no output (the file parsed and was re-rendered in place).

- [x] **Step 5: Register the printing**

In `source/test-suite/Pawl/Cards.hs`, add `masterThiefPrinting :: Printing.Printing` to the `Cards` record (at the end, beside `doublingSeasonPrinting`), `masterThiefPrinting_ <- loadPrinting "master-thief"` to `loadCards`, the field to the returned `MkCards`, and `masterThiefPrinting cards,` to `allPrintings`. Do **not** add it to any deck — that would perturb `PropertySpec`'s card-backed conservation counts.

- [x] **Step 6: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — `MasterThief` green (all four cases, the latch included), the `CardSpec` shape assertion green, and `CardsSpec`'s directory lint + byte-stability green.

If the latch case fails with the Myr returning to alice after cleanup, the sweep is masking rather than deleting — fix `Expiry.sweepConditional`, never the test.

- [x] **Step 7: Commit**

```bash
git add source/library/Pawl data/cards/master-thief.json source/test-suite
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p6): Master Thief, CR 611.2b's own worked example

Its three Gatherer rulings are its three tests: leaving the battlefield
ends the effect, a duration that never starts stores nothing, and
regaining the source does not regain the artifact -- the latch an
implementation that masks instead of deleting cannot pass.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 6: Hag of Inner Weakness — the event-relative gate

A digital-only card with real Oracle text, chosen deliberately: every one of the 66 paper "until your next turn" cards drags in machinery this phase does not own (flashback, reanimation, ward, player-scoped restrictions, sagas, loyalty). Recognizability is traded for a gate that tests the axis and not the collateral.

**Files:**
- Modify: `source/library/Pawl/Type/Subtype.hs` (add `Hag`, `Warlock`), `source/library/Pawl/Codec.hs` (their arms)
- Create: `data/cards/hag-of-inner-weakness.json`
- Modify: `source/test-suite/Pawl/Cards.hs`
- Test: `source/test-suite/Pawl/ExpirySpec.hs` (§5 tests 5–8), `source/test-suite/Pawl/CardSpec.hs`

**Interfaces:**
- Consumes: `Duration.UntilYourNextTurn`, `Expiry.dropAtHandoff` (Task 2); `TargetSpec.OpponentCreatureTarget` (Task 4).
- Produces: `Subtype.Hag`, `Subtype.Warlock`; `Cards.hagOfInnerWeaknessPrinting :: Cards -> Printing`.

- [x] **Step 1: Write the failing tests**

In `source/test-suite/Pawl/ExpirySpec.hs`, add a group (and register it in `tests`):

```haskell
-- Hag of Inner Weakness {2}{B} Creature -- Hag Warlock 2/2: "At the beginning of
-- your upkeep, target creature an opponent controls gets -2/-1 until your next
-- turn." No Gatherer rulings exist, so these derive from CR 611.2a and CR 514.2.
hagTests :: Cards.Cards -> Tasty.TestTree
hagTests cards =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      beginUpkeep gs = Event.recordEvent (GameEvent.StepBegan upkeep S.alice) (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice})
      settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority
      resolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop
      handoff gs = S.runPure S.identityAnswer gs Engine.handoffTurn
      -- alice's Hag, and exactly one creature bob controls, so the CR 603.3d
      -- target choice is forced.
      boardWith printing =
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, gs1) = S.addCreature (Cards.hagOfInnerWeaknessPrinting cards) S.alice gs0
            (victimId, gs2) = S.addCreature printing S.bob gs1
         in (victimId, resolveAll (settle (beginUpkeep gs2)))
      (mammoth, afterTrigger) = boardWith (Cards.warMammothPrinting cards)
   in Tasty.testGroup
        "HagOfInnerWeakness"
        [ HU.testCase "CR 613.4c it works: the opponent's 3/3 becomes 1/2" $
            do
              HU.assertEqual "power" (Just 1) (Projection.powerOf mammoth afterTrigger)
              HU.assertEqual "toughness" (Just 2) (Projection.toughnessOf mammoth afterTrigger),
          -- THE FALSIFIER for both "treat it as until end of turn" and any
          -- implementation that expires the effect by scanning the event log for
          -- a matching StepBegan: the effect was CREATED on a turn whose untap
          -- step has already happened, so a log scan kills it the turn it is born.
          HU.testCase "CR 514.2 it survives cleanup and the whole of the opponent's turn" $
            let ended = Expiry.dropAtCleanup afterTrigger
                bobsTurn = handoff ended
             in do
                  HU.assertEqual "still 1/2 in its own turn's end step" (Just 1) (Projection.powerOf mammoth ended)
                  HU.assertEqual "bob is active" S.bob (GameState.activePlayer bobsTurn)
                  HU.assertEqual "still 1/2 throughout bob's turn" (Just 1) (Projection.powerOf mammoth bobsTurn)
                  HU.assertEqual "still 1/2 throughout bob's turn" (Just 2) (Projection.toughnessOf mammoth bobsTurn),
          HU.testCase "CR 611.2a it expires as the controller's next turn begins" $
            let alicesTurn = handoff (handoff (Expiry.dropAtCleanup afterTrigger))
             in do
                  HU.assertEqual "alice is active again" S.alice (GameState.activePlayer alicesTurn)
                  -- Asserted BEFORE the upkeep trigger fires a second time, so
                  -- the two effects can never be confused.
                  HU.assertEqual "back to 3/3" (Just 3) (Projection.powerOf mammoth alicesTurn)
                  HU.assertEqual "back to 3/3" (Just 3) (Projection.toughnessOf mammoth alicesTurn)
                  HU.assertEqual "nothing stored" [] (GameState.continuousEffects alicesTurn),
          HU.testCase "CR 704.5f the modification really applies: a 2/1 becomes 0/0 and dies" $
            let (_, afterPiker) = boardWith (Cards.pikerPrinting cards)
             in HU.assertEqual "bob's Piker is gone" 0 (S.creaturesInPlay S.bob afterPiker)
        ]
```

In `source/test-suite/Pawl/CardSpec.hs`, add to `m45p6CardTests`:

```haskell
      HU.testCase "Hag of Inner Weakness is a {2}{B} 2/2 Hag Warlock with an upkeep -2/-1 trigger" $
        let c = Printing.card (Cards.hagOfInnerWeaknessPrinting cards)
            black = ManaSymbol.OfType (ManaType.Colored Color.Black)
            slot = SlotName.MkSlotName (Text.pack "target")
         in do
              HU.assertEqual "cost" (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, black])) (Card.Type.manaCost c)
              HU.assertEqual "subtypes" (Set.fromList [Subtype.Hag, Subtype.Warlock]) (TypeLine.subtypes (Card.Type.typeLine c))
              case Card.Type.triggeredAbilities c of
                [ab] -> do
                  HU.assertEqual
                    "beginning of your upkeep"
                    (TriggerCondition.StepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn)
                    (TriggeredAbility.condition ab)
                  case Foldable.toList (Modal.modes (TriggeredAbility.modal ab)) of
                    [m] -> do
                      HU.assertEqual
                        "-2/-1 until your next turn"
                        [Effect.ModifyTarget Duration.UntilYourNextTurn (Modification.ModifyPowerToughness (Quantity.Type.Literal (-2)) (Quantity.Type.Literal (-1))) slot]
                        (Foldable.toList (Mode.effects m))
                      HU.assertEqual
                        "one OpponentCreatureTarget slot"
                        (Map.singleton slot TargetSpec.OpponentCreatureTarget)
                        (Mode.targetSpecs m)
                    _ -> HU.assertFailure "expected exactly one mode"
                _ -> HU.assertFailure "expected exactly one triggered ability",
```

- [x] **Step 2: Run the tests to verify they fail**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `Variable not in scope: Cards.hagOfInnerWeaknessPrinting`, `Data constructor not in scope: Subtype.Hag`, `Subtype.Warlock`.

- [x] **Step 3: Add the two subtypes**

Append to `source/library/Pawl/Type/Subtype.hs`, in this order, **after** `Rogue`:

```haskell
  | Hag -- CR 205.3m (a creature type; Hag of Inner Weakness's)
  | Warlock -- CR 205.3m (a creature type; Hag of Inner Weakness's)
```

Add the matching arms to `subtypeToJson` and `jsonToSubtype` in the same positions.

- [x] **Step 4: Write the card file**

Create `data/cards/hag-of-inner-weakness.json` with exactly this content, on one line, with a trailing newline:

```json
{"name":"Hag of Inner Weakness","manaCost":[{"type":"Generic","value":2},{"type":"OfType","value":{"type":"Colored","value":{"type":"Black"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Creature"}],"subtypes":[{"type":"Hag"},{"type":"Warlock"}]},"power":{"type":"Literal","value":2},"toughness":{"type":"Literal","value":2},"keywords":[],"staticAbilities":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[{"condition":{"type":"StepBegins","value":[{"type":"Beginning","value":{"type":"Upkeep"}},{"type":"ControllersTurn"}]},"modal":{"modes":[{"effects":[{"type":"ModifyTarget","value":[{"type":"UntilYourNextTurn"},{"type":"ModifyPowerToughness","value":[{"type":"Literal","value":-2},{"type":"Literal","value":-1}]},"target"]}],"targetSpecs":[{"slot":"target","spec":{"type":"OpponentCreatureTarget"}}]}],"selection":{"type":"ChooseExactly","value":1}}}],"castingPermissions":[]}
```

Then canonicalize it exactly as in Task 5 Step 4 (`regen "data/cards/hag-of-inner-weakness.json"`), expecting no output.

- [x] **Step 5: Register the printing**

In `source/test-suite/Pawl/Cards.hs`, add `hagOfInnerWeaknessPrinting` to the record, `loadCards` (`loadPrinting "hag-of-inner-weakness"`) and `allPrintings`. No deck.

- [x] **Step 6: Run the tests to verify they pass**

Run: `cabal build all --enable-tests --enable-benchmarks && cabal test`
Expected: PASS — `HagOfInnerWeakness` green (all four cases), `CardSpec` and `CardsSpec` green.

If the "survives the opponent's turn" case fails with the creature back at 3/3 immediately, the expiry is being decided by an event-log scan or treated as `AtCleanup` — fix the implementation, never the test.

- [x] **Step 7: Commit**

```bash
git add source/library/Pawl data/cards/hag-of-inner-weakness.json source/test-suite
hooky fix
git add -u
hooky run
git commit -m "feat(m4.5-p6): Hag of Inner Weakness, the event-relative gate

CR 611.2a: -2/-1 until your next turn survives CR 514.2 and the whole of
the opponent's turn, then ends as the controller's next turn begins --
the case a log scan kills on the turn the effect is born.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

### Task 7: Close — issues filed and cited, docs updated, exit criterion verified

Every `(#N)` placeholder Task 4 left in the code is replaced here with a real issue number. **`grep -rn '(#N)' source/` must return nothing when this task is done.**

**Files:**
- Modify: every source file carrying a `(#N)` placeholder
- Modify: `source/library/Pawl/Type/ActiveReplacement.hs` (cite the new untested-path issue), `source/library/Pawl/Type/StateCondition.hs` (#38 already cited — confirm), `source/library/Pawl/Type/TargetSpec.hs` (#40 cited at both new arms — confirm)
- Modify: `docs/progress.md`, `CLAUDE.md`, `docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`

- [x] **Step 1: File the two new issues**

Run each `gh issue create` and record the number it prints. Labels come from CLAUDE.md's set: `elision`, `gap`, `rules-correctness`, `bug`, `expires:milestone`, `expires:card-driven`.

| # | Title | Labels | Body must carry |
|---|---|---|---|
| A | `While` and `AtTurnOf` expiries on a floating replacement are representable but untested | `gap`, `expires:card-driven` | The `Expiry` vocabulary is shared with `ActiveReplacement` by construction, so `Pawl.Expiry`'s three sweeps handle a conditional or turn-relative floating replacement uniformly — but no card in the pool creates one and no test exercises the path. Expiry: the first card with "prevent … for as long as …" or "until your next turn, prevent …" (Morningtide's Light is one). |
| B | A controller-relative target spec whose source has left the battlefield yields an empty legal set | `rules-correctness`, `expires:card-driven` | `Target.legalRecipients`'s `OpponentCreatureTarget` arm reads `Projection.controllerOf source`; when the source is gone that is `Nothing`, so CR 608.2b's re-check at resolution finds every target illegal and the ability wrongly fizzles. CR 608.2h's last known information is not consulted. Unreached today — nothing in the pool kills a Hag of Inner Weakness in response to its own trigger. The fix is to thread the ability's controller rather than its source. |

- [x] **Step 2: Sweep the `(#N)` placeholders and confirm the cited ones**

Run: `grep -rn '(#N)' source/`
Replace the one hit — `Pawl/Target.hs`'s `OpponentCreatureTarget` arm — with issue B's number.

Add issue A's number to `Pawl/Type/ActiveReplacement.hs`'s comment on the `expiry` field: state only what is *not* exercised, plus `(#A)`. **No expiry trigger in the comment** — that lives in the issue.

Confirm (do not re-file): `#38` is cited on `StateCondition`'s module comment and covers the new `YouControlSource` arm; `#40` is cited on both `ArtifactTarget` and `OpponentCreatureTarget`, exactly as `NonblackCreatureTarget` already does.

Run: `grep -rn '(#N)' source/`
Expected: no output.

- [x] **Step 3: Verify the exit criterion mechanically**

Run each and confirm the expected result:

```bash
grep -rn 'dropEndOfTurnEffects\|dropEndOfTurnReplacements\|ContinuousEffect\.duration\|ActiveReplacement\.duration' source/   # no output
grep -rln 'Expiry\.\(AtCleanup\|Never\|While\|AtTurnOf\)' source/library/ | grep -v 'Pawl/Expiry.hs\|Pawl/Type/'              # no output
grep -rn 'Expiry' data/                                                                                                       # no output
grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-22-p6-conditional-event-durations.md                              # counts down to 0
cabal clean && cabal build all --enable-tests --enable-benchmarks                                                             # warning-free
cabal test                                                                                                                    # all green
git add -A && hooky run                                                                                                       # passes
cabal bench                                                                                                                   # three timings, no large regression
```

The second grep is the **first invariant's audit**: `Pawl.Expiry` is the sole rules home of casing on `Expiry`, `Pawl.Type.*` modules only construct, and there is deliberately no `Expiry` codec. Anything else in the output is a widening of the casing surface and must be fixed, not excused. The third grep is the split's audit from the other side: no card data may name a stored expiry.

- [x] **Step 4: Append the `docs/progress.md` completion entry**

One entry, in the file's established voice, recording what P6 *established* — not what is left. It must state:

- the gate cards and what each falsified — **Master Thief** (CR 611.2b's own printed example): the latch, which an implementation that filters the effect out of the projection instead of deleting it cannot pass, plus "the duration never starts" and "leaving the battlefield ends it", all three straight from Gatherer; **Hag of Inner Weakness**: survives CR 514.2 and the whole of the opponent's turn, which neither `UntilEndOfTurn` nor an event-log scan can produce;
- what was added — `Pawl.Type.Expiry`, `Pawl.Expiry` (`arm`, `dropAtCleanup`, `dropAtHandoff`, `sweepConditional`), `Duration.ForAsLongAs`/`UntilYourNextTurn`, `StateCondition.YouControlSource`, `TargetSpec.ArtifactTarget`/`OpponentCreatureTarget`, `Subtype.Rogue`/`Hag`/`Warlock`, the two card files;
- what was deleted — `Projection.dropEndOfTurnEffects`, `Event.dropEndOfTurnReplacements`, and the `duration` field on both carriers;
- **the design's one departure from the umbrella**: event-relative durations are decided at the turn handoff, not by reading P4's event log. The transition *is* the event, known exactly and for free at the one site that performs it, whereas reading `GameEvent.StepBegan` back out of a turn-scoped log needs a per-effect watermark to distinguish "your untap already happened this turn" from "your next turn began" — the exact trap Hag of Inner Weakness sets. Note the three rules that make dropping at the handoff observably identical to dropping "as the turn begins": CR 500.12, CR 502.4, CR 704.3. P6 still reads P4's work through `stateHolds`;
- **the four departures from the spec** listed at the top of this plan, with their reasons;
- **no prompt was added and none elided** — every value this phase computes is derived (CR 109.5's "you" off the effect's controller, the condition off the board), so there is nothing to elide and no expiry to name;
- tracking: no open issue is closed; `#38` and `#40` are cited, not retired; `#58`, `#49`, `#76`, `c7a0077` and `b998924` are untouched; two new issues (A and B above) filed. If any new test settles a permanent held under cross-turn conditional control across an untap step, say so and comment on `#62` — do **not** close it;
- the final suite count, that the build is warning-clean on a from-scratch `cabal clean` build, and the benchmark comparison (noting `#66`, which makes the per-scenario split meaningless — the aggregate is the only honest reading);
- the spec and plan paths, kept as reference.

- [x] **Step 5: Replace the `CLAUDE.md` status bullet**

**Replace, never append** — milestone history goes in `progress.md`. The new bullet says M0–M4h plus M4.5 P1–P6 are complete, that P6 closed GAP-D (conditional and event durations, and the moment a duration begins), and that **P7 (the player projection, Cluster 3) is next**, already unblocked by P4, with P8/P9 still floating. Keep it to the same length as the bullet it replaces.

- [x] **Step 6: Tick the umbrella spec**

`docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`:
- §3's P6 row: mark it landed with a pointer to `docs/superpowers/specs/2026-07-22-p6-conditional-event-durations-design.md`, and **correct the "(rides P4)" claim** — event-relative durations are decided at turn handoff, not by reading the event log; P6 still reads P4's work through `stateHolds`.
- §4's ordering paragraph: P6 landed; **P7 is next** (Cluster 3, the player projection); P8 and P9 still float.

- [x] **Step 7: Commit**

```bash
git add docs/progress.md CLAUDE.md docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md source/library/Pawl
hooky fix
git add -u
hooky run
git commit -m "docs(m4.5-p6): completion note, umbrella tick, CLAUDE.md status

Two deferrals filed as issues and cited at their code sites; #38 and #40
cited, not retired; no issue closed. The umbrella's 'rides P4' claim for
P6 is corrected: the turn handoff is the event. P7 is next.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

- [x] **Step 8: Confirm the plan is complete**

Run: `grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-22-p6-conditional-event-durations.md`
Expected: `0`.

---

## Spec coverage map

| Spec section | Where it lands |
|---|---|
| §2.1 the `Duration` → `Expiry` split, both carriers renamed | Task 1 |
| §2.2 `StateCondition`'s third customer, `stateHolds` gains a source | Task 3 |
| §2.3 `Pawl.Expiry` as the sole casing home; `arm`, `dropAtCleanup` | Task 1 (plus the widenings in Tasks 2–3, departure 1) |
| §2.3 `dropAtHandoff` | Task 2 |
| §2.3 `sweepConditional` | Task 3 |
| §2.4 arming at `Resolve.applyEffect`'s three storing arms | Task 1 (wired), Tasks 2–3 (each new arm) |
| §2.4 cleanup collapses to one call in `Engine.runStep` | Task 1 |
| §2.4 the sweep at the head of `settleForPriority` | Task 3 |
| §2.4 `dropAtHandoff` in `Engine.handoffTurn`, and why it is complete | Task 2 |
| §2.5 source-relative `Pawl.Target`; `ArtifactTarget`, `OpponentCreatureTarget` | Task 4 (plus departures 2 and 3) |
| §2.6 codec: two `Duration` arms, one `StateCondition` arm, two `TargetSpec` arms; no `Expiry` codec | Tasks 2, 3, 4; audited in Task 7 Step 3 |
| §3 the two invariants | Task 7 Step 3's second grep (casing surface); no prompt is added or elided anywhere, recorded in Task 7 Step 4 |
| §5 tests 1–4 (Master Thief, with the rulings verbatim) | Task 5 |
| §5 tests 5–8 (Hag of Inner Weakness) | Task 6 |
| §5 test 9 (codec round-trips) | Tasks 2, 3, 4 (unit arms) and Tasks 5–6 (card files, via `CardsSpec.checkFile`); departure 4 |
| §6 module and type inventory | Tasks 1–4 |
| §8 deferrals with named expiries | Task 7 Steps 1–2 |
| §9 tracking | Task 7 Steps 4–6 |
| §10 exit criterion | Task 7 Step 3 |
