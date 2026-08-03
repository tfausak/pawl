# As-Enters Basic Land Type Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give CR 614.1c's "As this Aura enters, choose a basic land type" a producer — Convincing Mirage — and give `Modification.SetLandSubtype` a sibling that reads that choice off the effect's source instead of off card data.

**Architecture:** The four-piece shape PR #626 built for a colour, retargeted at a subtype: a non-copiable per-object field (`Object.chosenSubtype`), a nullary `EntryRewrite` that prompts for it as the permanent enters, a `Prompt`/`Response` pair with `Replay.hs` plumbing, and a nullary `Modification` that reads the choice off the effect's **source**.

**The headline, and the one place this is not a copy of the colour twin:** CR 305.7's ability strip has *two* implementations — `Projection.applyModification`'s in-fold arm and `Projection.setLandSubtypeEffects`'s `isSet`, feeding the `liveGiven` candidate-list gate — and `isSet` classifies by **constructor**. A new subtype-setting modification missing from `isSet` leaves the two halves of one rule silently disagreeing: the land's own projection is stripped while its static abilities stay in the gather. That is the "one rule, two samplers" shape this codebase has been bitten by before, so it gets its own task, its own failing-test-first commit, and its own paragraph in the PR body.

**Tech Stack:** GHC 9.14.1 via the Nix flake, `cabal`, `tasty`, `cabal-gild`, `hooky`.

**Precedent:** `docs/superpowers/plans/2026-08-03-cr-613-3-cda-precedence.md` Task 5, and `docs/superpowers/specs/2026-08-03-cr-613-3-cda-precedence-design.md` §2.4.

**Issue:** #608

## Global Constraints

- **One PR, small commits.** Every commit builds warning-free and leaves the suite green. Open the PR as a draft only after the last task; mark it ready once self-review findings are pushed.
- **One build at a time.** `direnv exec . cabal build all`, never bare `cabal`, never just the library — the suites break separately. Never poke inside `dist-newstyle`. Never `cabal clean`.
- **Run every tool through direnv:** `direnv exec . cabal build all`, `direnv exec . cabal test`, `direnv exec . hooky fix`.
- **`hooky` acts on staged files.** `git add`, then `hooky fix`, then `git add` again, then `hooky run`.
- **`cabal-gild pawl.cabal` must be run directly** whenever a module is added or deleted. This plan adds **no** module, so it should not be needed.
- **Never trust recalled Magic rules.** Every CR citation this plan writes into a comment was checked against `docs/rules.txt` on 2026-08-03: 105.1, 205.1b, 205.3i, 303.4a, 303.4c, 303.4d, 305.6, 305.7, 400.7, 604.3, 608.2d, 612.1, 613.1c, 613.1d, 613.7a, 613.7d, 614.1c, 614.12, 614.12a, 702.5a, 704.5m, 707.5. Any *other* citation you touch must be re-checked before you leave it there.
- **The rules core must not case on an effect's identity.** `Pawl.Engine.Projection` is the one module allowed to case on `Modification`. Do not add a `case` on `Modification` anywhere else.
- **Fix a stale comment in the commit that falsifies it, never in a closing sweep.** A sweep is how several of PR #626's seventeen findings survived to review. Each task below names the prose it breaks.
- **Never edit this plan, weaken an assertion, or delete a test to make a check pass.** If the plan looks wrong, stop and say so.
- **`git add` explicit paths, never `git add -A`.** Other sessions may share this checkout.
- Build must be warning-free under `-Weverything` (`pawl.cabal`'s `common warnings`).
- Baseline: **2590 passing** on `main`. Report the count after.

## Settled design calls

Three forks were raised during planning and settled by the owner before
implementation; they are recorded here so the code comments can cite a decision
rather than re-argue it.

1. **Rename the pair prompt.** `Prompt.ChooseBasicLandType` (this branch's new
   singular) and `Prompt.ChooseBasicLandTypes` (Magical Hack's existing pair)
   differ by one character, which is a footgun. The pair becomes
   **`ChooseLandTypeSwap`** — named for what it is, CR 612's from/to word
   replacement, rather than for counting its payload — and `Response`'s arm
   becomes `ChoseLandTypeSwap`. Task 1, first, so every later task is written
   against the final name.
2. **An unchosen subtype is a full no-op**, mirroring `Modification.AddChosenColor`
   toward an unchosen colour. The resulting fold/gate divergence is unreachable
   and is documented in place against #391's precedent. **No issue is filed.**
3. **#608's motivating argument is not assertable and no test gestures at it.**
   The Magical Hack race is about what the *player knows* when they choose, not
   about what the board does, and this engine's tests assert on game state. The
   buildable content is the vocabulary plus CR 613.1c/613.1d's layer ordering.
   The owner is recording that on the issue.

## Adding a constructor: how to find every site

`-Weverything` makes every incomplete `case` a build error, so the compiler
enumerates the sites. The totals below were counted by reading on 2026-08-03 —
**the precedent plan's list is wrong in three places, and the corrections matter**:

- **`Modification`** — `Projection.layer`, `Projection.applyModification`,
  `Projection.freezeQuantities`, `Projection.quantitiesOf`,
  `Projection.removesAbilities`, `Projection.modificationWrites`,
  `Projection.rewriteModification`, `Projection.setLandSubtypeEffects`'s local
  `isSet`, `Codec.Modification.toJson`/`fromJson`. **Nine**, two more than the
  colour twin: `rewriteModification` and `isSet` both case on `Modification`, and
  `isSet` is this branch's headline. **`isSet` has a trailing wildcard, so the
  compiler will NOT name it** — that is exactly why Task 7 exists.
- **`EntryRewrite`** — `Replacement.hs`'s apply loop plus its `bucket`
  (~line 604) and its copy-ish predicate (~line 658),
  `Codec.EntryRewrite.toJson`/`fromJson`.
- **`Prompt`** — a GADT, so only *total* deciders are forced. Confirmed by
  reading: `Replay.hs` **3**, `source/benchmark/Main.hs` **3**,
  `source/test-suite/Pawl/Support.hs` **4**,
  `source/test-suite/Pawl/GameSpec.hs` **2**,
  `source/test-suite/Pawl/CastSpec.hs` **3**. Fifteen arms.
  `ColorSpec.hs`, `ReplacementSpec.hs` and `ReplaySpec.hs` fall through to
  `S.identityAnswer` and are **not** forced — the precedent plan named
  `ReplacementSpec.hs` as forced and it is not.
- **`Response`** — `Replay.hs` only. **There is no `Pawl.Codec.Response` and no
  `Pawl.Codec.Object`**; the precedent plan named both and neither exists. Those
  two types have no JSON codec, so Tasks 1–3 have no codec work.

---

## Task 1: Rename the pair prompt to `ChooseLandTypeSwap`

Pure mechanical rename, no behaviour change and no new test. Done first so every
later task is written against the final name. The suite count must not move.

**Files:**
- Modify: `source/libraries/types/Pawl/Types/Prompt.hs`, `.../Response.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Replay.hs`, `.../Resolve.hs`
- Modify: `source/libraries/types/Pawl/Types/Effect.hs` (a comment citation)
- Modify: `source/libraries/engine/Pawl/Engine/Projection.hs` (a comment citation)
- Modify: `source/benchmark/Main.hs`
- Modify: `source/test-suite/Pawl/Support.hs`, `.../GameSpec.hs`, `.../CastSpec.hs`, and any spec citing the old name

**Interfaces:**
- Renames `Prompt.ChooseBasicLandTypes` → `Prompt.ChooseLandTypeSwap` and
  `Response.ChoseBasicLandTypes` → `Response.ChoseLandTypeSwap`. No shim; this
  project has no API-stability obligations.

- [ ] **Step 1: Find every site, code and prose**

```bash
grep -rn 'ChooseBasicLandTypes\|ChoseBasicLandTypes' source/ docs/ data/
```

Note which hits are **code** and which are **comments** — the comment hits are
the stale-cross-reference hazard and are the reason this is a task rather than a
`sed`. Bulk edits leak into prose; check the superset afterwards.

- [ ] **Step 2: Rename, and say why in the constructor's own comment**

`Prompt.hs`: rename the constructor and add to its existing comment block:

```haskell
  -- Named for the CR 612 word REPLACEMENT it performs rather than for its
  -- payload's arity, so it cannot be confused with ChooseBasicLandType below --
  -- the singular as-enters choice, which is a different rule (CR 614.1c), a
  -- different moment (entry, not resolution) and a different subsystem
  -- (Pawl.Engine.Replacement, not Pawl.Engine.Resolve).
  ChooseLandTypeSwap :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> SlotName.SlotName -> Prompt (Subtype.Subtype, Subtype.Subtype)
```

`Response.hs`: `ChoseLandTypeSwap (Subtype.Subtype, Subtype.Subtype)`, with the
same one-line reason.

Then every code site: `Replay.hs` (3), `Resolve.hs` (1 producer, ~line 1044),
`benchmark/Main.hs` (3), `Support.hs`, `GameSpec.hs`, `CastSpec.hs`, and any
spec that names the response.

- [ ] **Step 3: Fix the prose citations**

At minimum — re-derive the list from Step 1 rather than trusting this one:
- `source/libraries/types/Pawl/Types/Effect.hs` (~line 86), which cites the
  prompt by name in the CR 608.2d discussion.
- `source/libraries/engine/Pawl/Engine/Projection.hs`'s `rewriteModification`
  `SetCreatureSubtype` arm (~line 1154), which cites it explaining why `from` is
  never a creature type.
- `source/test-suite/Pawl/ResolveSpec.hs`'s `MagicalHackTiming` group and any
  module header naming the prompt.

- [ ] **Step 4: Verify green and commit**

```bash
direnv exec . cabal build all && direnv exec . cabal test 2>&1 | tail -20
grep -rn 'ChooseBasicLandTypes\|ChoseBasicLandTypes' source/ docs/ data/
```
Expected: warning-free, **2590 passing, count unchanged**, and the grep finds
nothing.

```bash
git commit -m "Rename Magical Hack's prompt to ChooseLandTypeSwap"
```

---

## Task 2: `Object.chosenSubtype`

Storage only. Mirrors `Object.chosenColor` exactly, including the CR 400.7 reset.

**Files:**
- Modify: `source/libraries/types/Pawl/Types/Object.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Event.hs` (the `changeZone` reset)
- Modify: every `MkObject` construction site the build names

**Interfaces:**
- Produces: `Object.chosenSubtype :: Maybe Subtype.Subtype`.

**Design call — a sibling field, not a generalisation.** `chosenColor` and
`chosenSubtype` could be one choice map, and they are not: this codebase's style
is a named field per rules concept (`enteredUnder`, `attachedTo`, `counters` are
separate for the same reason), and a sum-typed value would force every reader to
re-narrow what the type already knows. What would change the call is a **third**
as-enters choice of a third type, or one card choosing two kinds at once.
Neither exists.

- [ ] **Step 1: Add the field**

`source/libraries/types/Pawl/Types/Object.hs`, directly after `chosenColor`:

```haskell
    -- | CR 614.1c: a basic land type this object's controller chose as it
    -- entered ("As this Aura enters, choose a basic land type" -- Convincing
    -- Mirage). Read by Modification.SetLandSubtypeToChosen off the effect's
    -- SOURCE, never off the affected object -- the same direction
    -- Modification.AddChosenColor reads chosenColor above.
    --
    -- A sibling of chosenColor rather than one generalized choice map: the two
    -- carry different types and are read by different modifications, and a
    -- sum-typed value would make every reader re-narrow what the field already
    -- knows.
    --
    -- NOT a copiable value, for chosenColor's reason. CR 707.5: "If the text
    -- that's being copied includes any abilities that replace the
    -- enters-the-battlefield event (such as ... 'as [this] enters' abilities),
    -- those abilities will take effect" -- so a copy runs the copied as-enters
    -- ability and makes its OWN choice.
    --
    -- Per-incarnation state, like damage and counters: reset by changeZone,
    -- because CR 400.7 makes the moved object a new one.
    chosenSubtype :: Maybe Subtype.Subtype,
```

Add `import qualified Pawl.Types.Subtype as Subtype` in alphabetical position if
absent.

- [ ] **Step 2: Let the build enumerate the construction sites**

`direnv exec . cabal build all 2>&1 | grep -B2 -A6 chosenSubtype`

The sites that set `chosenColor` (and so must set this) are `Engine/Monarch.hs`,
`Engine/Engine.hs`, `Engine/Setup.hs`, `Engine/Activate.hs`, `Engine/Event.hs`
(two — the record near line 735 and `mkObj` near line 350), `Engine/Resolve.hs`,
`test-suite/Pawl/CastSpec.hs`, `test-suite/Pawl/GameSpec.hs`,
`test-suite/Pawl/Support.hs`. `Nothing` at each.

- [ ] **Step 3: Reset it on a zone change, and fix the comment above the reset**

`Engine/Event.hs`'s `mkObj` already lists `Object.chosenColor = Nothing`. Add
`Object.chosenSubtype = Nothing` beside it, and **extend the comment above it
(~line 348), which explains the reset in terms of `chosenColor` alone**, to name
both fields. That comment goes stale in this commit; fix it in this commit.

Also re-read `Object.hs`'s `chosenColor` block, which now has a sibling and
should say so in one clause rather than reading as the only such field.

- [ ] **Step 4: Verify green and commit**

Expected: warning-free, **2590 passing, count unchanged**. The field exists;
nothing writes it.

```bash
git commit -m "Record a basic land type chosen as an object enters"
```

---

## Task 3: `Prompt.ChooseBasicLandType` and `Response.ChoseBasicLandType`

**Files:**
- Modify: `source/libraries/types/Pawl/Types/Prompt.hs`, `.../Response.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Replay.hs`
- Modify: `source/benchmark/Main.hs`
- Modify: `source/test-suite/Pawl/Support.hs`, `.../GameSpec.hs`, `.../CastSpec.hs`, `.../ReplaySpec.hs`

**Interfaces:**
- Produces:
  - `Prompt.ChooseBasicLandType :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt Subtype.Subtype`
  - `Response.ChoseBasicLandType :: Subtype.Subtype -> Response`

**No candidate list**, exactly as `Prompt.ChooseColor` carries none: CR 305.6
fixes the five basic land types ("The basic land types are Plains, Island, Swamp,
Mountain, and Forest. If an object uses the words 'basic land type,' it's
referring to one of these subtypes") the way CR 105.1 fixes the five colours, and
no card in the pool narrows them. `ChooseLandTypeSwap` already carries none for
the same reason, so this is the established posture rather than a new one.

- [ ] **Step 1: Add the prompt**

`Prompt.hs`, directly after `ChooseLandTypeSwap` so the two land-type prompts sit
together and a reader meets both at once:

```haskell
  -- | CR 614.1c: as an object enters, its controller chooses ONE basic land
  -- type ("As this Aura enters, choose a basic land type" -- Convincing
  -- Mirage). The ObjectId is the entering object.
  --
  -- Singular, and deliberately not ChooseLandTypeSwap above. That prompt
  -- answers with a PAIR because CR 612's word swap needs two words (Magical
  -- Hack's "one basic land type" and the "another" replacing it); this is a
  -- single choice, made at a different moment (entry, not resolution) and by a
  -- different subsystem (Pawl.Engine.Replacement, not Pawl.Engine.Resolve).
  -- Answering it with a pair and dropping half would be the engine deciding
  -- something no player was asked.
  --
  -- No candidate list: CR 305.6 fixes the five basic land types the way CR
  -- 105.1 fixes the five colours for ChooseColor, and no card in the pool
  -- narrows them. Asked whenever the entering object has a controller to ask --
  -- five types are five distinguishable options, so there is no one-option case
  -- to elide. Replacement's arm has an unreachable no-controller fallback
  -- beside it; see there.
  --
  -- No SlotName, unlike ChooseLandTypeSwap: that prompt names the spell's
  -- text-change slot, and this choice is bound into no slot at all -- it is
  -- written to Object.chosenSubtype on the entering permanent.
  ChooseBasicLandType :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt Subtype.Subtype
```

- [ ] **Step 2: Add the response**

`Response.hs`, after `ChoseLandTypeSwap`:

```haskell
  | -- | CR 614.1c: the basic land type a player chose as an object entered,
    -- serialized so a DecisionLog replays it deterministically. Singular, and
    -- distinct from ChoseLandTypeSwap above for Prompt.ChooseBasicLandType's
    -- reason.
    ChoseBasicLandType Subtype.Subtype
```

There is no `Pawl.Codec.Response`; do not go looking for one.

- [ ] **Step 3: Let the build enumerate the decider sites**

`Replay.hs` gains three arms, matching `ChooseColor`'s exactly:

```haskell
  Prompt.ChooseBasicLandType {} -> Response.ChoseBasicLandType answer
```
```haskell
  Prompt.ChooseBasicLandType {} -> case response of
    Response.ChoseBasicLandType t -> Just t
    _ -> Nothing
```
```haskell
  -- CR 305.6: any of the five basic land types is a legal answer. Mountain is
  -- what the neighbouring ChooseLandTypeSwap arm already falls back to when a
  -- transcript runs short, so the two agree.
  Prompt.ChooseBasicLandType {} -> Subtype.Mountain
```

Then every remaining total decider: `benchmark/Main.hs` (3), `Support.hs` (4),
`GameSpec.hs` (2, `pure`-wrapped), `CastSpec.hs` (3). `Subtype.Mountain` at each.

**Do not** add an arm to `ColorSpec.hs`, `ReplacementSpec.hs` or `ReplaySpec.hs`
deciders — they fall through to `S.identityAnswer`. If the build names one, this
plan miscounted; add it and note it.

- [ ] **Step 4: Add the replay test**

`source/test-suite/Pawl/ReplaySpec.hs` has a `ChooseColor` record-and-replay
test. Add the `ChooseBasicLandType` twin, following that test's actual shape.

- [ ] **Step 5: Verify green and commit**

Expected: warning-free, green, count **2591**.

```bash
git commit -m "Add the CR 614.1c basic-land-type choice prompt"
```

---

## Task 4: `EntryRewrite.ChooseBasicLandType`

**Files:**
- Modify: `source/libraries/types/Pawl/Types/EntryRewrite.hs`
- Modify: `source/libraries/codec/Pawl/Codec/EntryRewrite.hs`, `.../EntryRewriteSpec.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Replacement.hs`

**Interfaces:**
- Produces: `EntryRewrite.ChooseBasicLandType` (nullary), wire tag
  `"ChooseBasicLandType"`.

- [ ] **Step 1: Add the constructor, and fix the header it falsifies**

`EntryRewrite.hs`, after `ChooseColor`:

```haskell
  | -- | CR 614.1c again, with a subtype instead of a colour: "As [this
    -- permanent] enters, choose a basic land type" (Convincing Mirage).
    -- Nullary -- CR 305.6's five basic land types are the offer, and no card
    -- narrows them, so there is nothing to carry.
    --
    -- Written to Object.chosenSubtype rather than into the copiable snapshot
    -- AsCopy and ChoiceOf write to, for ChooseColor's reason: CR 707.5's second
    -- sentence means a copy runs the copied as-enters ability and makes its own
    -- choice, so the subtype is not a copiable value.
    ChooseBasicLandType
```

**The module header enumerates the arms** ("AsCopy is Clone …; ChoiceOf is Primal
Plasma …; ChooseColor is Painter's Servant (CR 614.1c); WithCounters is …").
Add `ChooseBasicLandType is Convincing Mirage (CR 614.1c)` in constructor order,
in this commit.

- [ ] **Step 2: Codec it**

`Codec/EntryRewrite.hs`, beside `ChooseColor`:
```haskell
  EntryRewrite.ChooseBasicLandType -> Common.nullary "ChooseBasicLandType"
```
```haskell
    ("ChooseBasicLandType", _) -> Right EntryRewrite.ChooseBasicLandType
```

`Codec/EntryRewriteSpec.hs`, copying the `ChooseColor` case's exact shape:
```haskell
  Spec.it s "ChooseBasicLandType (Convincing Mirage)" $
    Common.assertJsonCodec s EntryRewrite.toJson EntryRewrite.fromJson EntryRewrite.ChooseBasicLandType "{\"type\":\"ChooseBasicLandType\"}"
```

- [ ] **Step 3: Apply the rewrite**

`Engine/Replacement.hs`, directly after the `EntryRewrite.ChooseColor` arm
(~line 920). Structurally identical:

```haskell
      -- CR 614.1c: Convincing Mirage's "As this Aura enters, choose a basic
      -- land type". Always asked, for ChooseColor's reason: CR 305.6's five
      -- basic land types are always all legal and always distinguishable, so
      -- there is no one-option case to elide.
      --
      -- Written to Object.chosenSubtype, NOT to the copiable snapshot -- see
      -- EntryRewrite.ChooseBasicLandType.
      EntryRewrite.ChooseBasicLandType -> do
        gs <- State.get
        picked <- case Projection.controllerOf oid gs of
          -- Unreachable, and defensive for ChooseColor's reason: the object is
          -- materialized on the battlefield before this loop runs, so
          -- controllerOf falls back to its owner. The same WEAKER fallback
          -- ChooseColor's arm carries -- Mountain is conjured, not an option the
          -- card named -- and it stands only because the branch cannot be
          -- reached.
          Nothing -> pure Subtype.Mountain
          Just controller -> do
            let decider = Decide.deciderFor controller gs
            Trans.lift (Program.prompt (Prompt.ChooseBasicLandType decider controller oid))
        consume (ReplacementCandidate.identity candidate)
        State.modify' $ \g ->
          let stamp o = o {Object.chosenSubtype = Just picked}
           in g {GameState.objects = Map.adjust stamp oid (GameState.objects g)}
        pure (Just event)
```

Then the two other total cases over `EntryRewrite`:
```haskell
  ReplacementEffect.EntryR _ EntryRewrite.ChooseBasicLandType -> ReplacementBucket.Other   -- ~604
  ReplacementEffect.EntryR _ EntryRewrite.ChooseBasicLandType -> False                     -- ~658
```

- [ ] **Step 4: Fix the two counts this arm falsifies, in this commit**

1. The `UnderSourceControl` arm's comment (~line 971): "All four arms above land
   on the object for the same reason -- AsCopy and ChoiceOf in the copiable
   snapshot, ChooseColor in Object.chosenColor, WithCounters through the CR
   122.6 funnel." Now **five**; add `ChooseBasicLandType in Object.chosenSubtype`.
2. `runEntry`'s comment (~line 1178): "every EntryR arm (AsCopy, ChoiceOf,
   ChooseColor, WithCounters, UnderSourceControl) always returns `Just`". Add
   the new arm.

- [ ] **Step 5: Verify green and commit**

Expected: warning-free, green, count **2592**. Nothing produces the rewrite yet.

```bash
git commit -m "Apply the CR 614.1c as-enters basic-land-type choice"
```

---

## Task 5: `Modification.SetLandSubtypeToChosen` — the fold half

The modification, and CR 305.7's **in-fold** strip. `setLandSubtypeEffects`'s
`isSet` is deliberately **left alone** until Task 7, so that task's test can fail
for the right reason first.

CR 305.7: "If an effect sets a land's subtype to one or more of the basic land
types, the land no longer has its old land type. It loses all abilities generated
from its rules text, its old land types, and any copiable effects affecting that
land, and it gains the appropriate mana ability for each new basic land type."

**Files:**
- Modify: `source/libraries/types/Pawl/Types/Modification.hs`
- Modify: `source/libraries/codec/Pawl/Codec/Modification.hs`, `.../ModificationSpec.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Projection.hs`
- Modify: `source/libraries/types/Pawl/Types/ProjectedCharacteristics.hs` (two comments)
- Modify: `source/test-suite/Pawl/Support.hs` (fixtures)
- Modify: `source/test-suite/Pawl/ProjectionSpec.hs`

**Interfaces:**
- Produces: `Modification.SetLandSubtypeToChosen` (nullary), wire tag
  `"SetLandSubtypeToChosen"`; `Projection.setLandSubtypeTo`;
  `S.withChosenSubtype`.

- [ ] **Step 1: Write the failing test**

`source/test-suite/Pawl/ProjectionSpec.hs`, beside the existing
`"CR 305.7 SetLandSubtype sets a Forest to only Mountain"` test (~line 616). No
card needed — this drives the modification directly.

```haskell
  Spec.it s "CR 305.7 SetLandSubtypeToChosen reads the SOURCE's entry choice" $ do
    -- The falsifier for a subtype baked into card data: the modification
    -- carries no subtype at all, and the Island comes off the effect's SOURCE
    -- (Object.chosenSubtype), which is where CR 614.1c's as-enters choice is
    -- written.
    ...
```

**`S.withEffectAt` will not do** — it hard-codes object id 998 as the source, and
this modification reads the source's `chosenSubtype`, so it needs a source that
is a real object. Add a source-taking variant beside it (`withEffectFrom`), with
a comment saying it exists because this is the pool's first modification whose
value is read off its own source. Assert the land ends up with exactly the chosen
subtype.

- [ ] **Step 2: Add the Support fixture**

`source/test-suite/Pawl/Support.hs`, beside `attach`:

```haskell
-- CR 614.1c: stamp the basic land type a permanent's controller would have
-- chosen as it entered, without running the entry loop. A STATE fixture, the
-- shape `attach` and `withEffect` already have -- the cast-it-for-real proof
-- that the choice is actually MADE is Pawl.AuraSpec's whole-card Convincing
-- Mirage test, and this exists so a projection test does not have to cast.
withChosenSubtype :: Subtype.Subtype -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
withChosenSubtype subtype oid gs =
  let set obj = obj {Object.chosenSubtype = Just subtype}
   in gs {GameState.objects = Map.adjust set oid (GameState.objects gs)}
```

- [ ] **Step 3: Add the constructor**

`Modification.hs`, directly after `SetLandSubtype`:

```haskell
  | -- | layer 4, CR 613.1d / 305.7: set this object's land subtype to the basic
    -- land type chosen for THIS effect's SOURCE as that source entered
    -- (Object.chosenSubtype). Convincing Mirage's "enchanted land is the chosen
    -- type".
    --
    -- Payload-free because the subtype is DERIVED at projection time from the
    -- source rather than baked into card data -- the posture
    -- Modification.AddChosenColor takes toward Object.chosenColor, and
    -- SetControllerToSource toward CR 109.5's "you". A static ability's
    -- modification is card data and cannot name a type a player will choose,
    -- which is why this is a second constructor rather than a field on
    -- SetLandSubtype above.
    --
    -- Carries CR 305.7's ability strip in full, exactly as SetLandSubtype does:
    -- Projection.applyModification routes both through setLandSubtypeTo, and
    -- Projection.setLandSubtypeEffects answers True for both, so the fold half
    -- and the candidate-list-gate half of that rule cannot drift apart.
    SetLandSubtypeToChosen
```

- [ ] **Step 4: Factor the strip, then add the arm**

Lift `applyModification`'s existing `SetLandSubtype` body into a named helper so
the two arms cannot diverge, moving the whole existing comment block (the CR
305.7 discussion, the #391 note, the #406 note) with it **verbatim**:

```haskell
-- CR 305.7's strip, shared by the two modifications that set a land's subtype:
-- SetLandSubtype (a type written into card data -- Blood Moon) and
-- SetLandSubtypeToChosen (a type a player chose as the source entered --
-- Convincing Mirage). One body, so the rule cannot be implemented twice and
-- drift.
--
-- <the existing SetLandSubtype comment block, unchanged>
setLandSubtypeTo :: Subtype.Type.Subtype -> ProjectedCharacteristics -> ProjectedCharacteristics
setLandSubtypeTo s pc =
  pc
    { PC.subtypes = Set.insert s (Set.filter (not . Subtype.isLandType) (PC.subtypes pc)),
      PC.keywords = Map.empty,
      PC.characteristicPT = Nothing,
      PC.activatedAbilities = [],
      PC.replacementEffects = [],
      PC.triggeredAbilities = []
    }
```

Then the two arms:

```haskell
        Modification.SetLandSubtype s -> setLandSubtypeTo s pc
        -- CR 305.7 again, with the type read off the SOURCE's own entry choice.
        -- An unchosen source (malformed data, or an entry that never ran the
        -- rewrite) sets nothing and strips nothing rather than guessing a type
        -- -- the posture AddChosenColor takes toward an unchosen colour.
        --
        -- That leaves this arm and setLandSubtypeEffects' gate disagreeing on
        -- such a source: the gate classifies by CONSTRUCTOR and so would still
        -- strip the land's static abilities. Unreachable -- the only producer is
        -- an EntryR whose rewrite always writes the field before the permanent
        -- is on the battlefield to be projected -- and it is the same kind of
        -- base-vs-projection disagreement the gate already documents (#391).
        Modification.SetLandSubtypeToChosen ->
          case Game.lookupObject src gs >>= Object.chosenSubtype of
            Nothing -> pc
            Just s -> setLandSubtypeTo s pc
```

- [ ] **Step 5: The remaining `Modification` cases — but NOT `isSet`**

`direnv exec . cabal build all 2>&1 | grep -A4 "Pattern match"`, and fix:

- `Projection.layer` — `Layer.Type` (CR 613.1d: "Layer 4: Type-changing effects
  are applied").
- `Projection.freezeQuantities` — `Just m`.
- `Projection.quantitiesOf` — `[]`.
- `Projection.removesAbilities` — `False`, citing the neighbouring
  `SetLandSubtype` arm's reason rather than duplicating it.
- `Projection.modificationWrites` — `Set.fromList [Subtypes, Keywords]`.
- `Projection.rewriteModification`'s `apply1` — identity, and say why:
  ```haskell
        -- CR 612.1 rewrites WORDS, and this carries none: the type is read from
        -- the source at projection time, not printed on the card. Identity, the
        -- treatment SetController gets below and for the same reason.
        Modification.SetLandSubtypeToChosen -> acc
  ```
- `Codec.Modification.toJson`/`fromJson` — `Common.nullary` / `Right`.

**`setLandSubtypeEffects`'s `isSet` has a trailing wildcard, so the build will
NOT name it** — leave it to the wildcard here. Task 7's test must fail before it
passes. Do not flip it in this commit.

- [ ] **Step 6: Pin the wire format**

`Codec/ModificationSpec.hs`, copying `AddChosenColor`'s case (~line 117):

```haskell
  -- Nullary: the subtype is read from the effect's source at projection time
  -- (Object.chosenSubtype), so there is nothing on the wire. Convincing Mirage.
  Spec.it s "SetLandSubtypeToChosen" $
    Common.assertJsonCodec s Modification.toJson Modification.fromJson Modification.SetLandSubtypeToChosen "{\"type\":\"SetLandSubtypeToChosen\"}"
```

- [ ] **Step 7: Fix the prose this commit falsifies**

- `Pawl/Types/ProjectedCharacteristics.hs`, two field comments (~81, ~88) naming
  "CR 305.7's SetLandSubtype at layer 4" as the clearer. Name
  `Projection.setLandSubtypeTo` — the helper this commit introduces — which is
  the stabler reference than either constructor.
- `Projection.applyColorDefining`'s CR 604.3a(2) paragraph: "The one pre-layer-5
  modification that touches the map is Modification.SetLandSubtype at
  Layer.Type, and it only ever EMPTIES it (CR 305.7)." Now two modifications,
  still only emptying — the argument holds, the enumeration does not.

- [ ] **Step 8: Verify green and commit**

Expected: warning-free, green, Step 1's test passes, count up by two.

```bash
git commit -m "Set a land's subtype to the one chosen as the source entered"
```

---

## Task 6: Convincing Mirage, and the proving test

**Files:**
- Create: `data/cards/convincing-mirage.json`
- Modify: `source/test-suite/Pawl/AuraSpec.hs`, `.../ManaSpec.hs`, `.../Support.hs`

**Design call — `Pool.Permanents` narrowed by a Land filter, not a new
`Pool.Lands`.** `Pool` has no `Lands` arm and should not grow one for this.
`TargetSpec`'s header records why: the pool/filter split "retires the whole
hand-carved family of colour- and type-restricted specs (#40)", and Blood Moon
already expresses "land" as `Filter.HasCardType Land` in an affected set.
`Pool.Permanents` tags its recipients `ToObject`, which is what
`Affected.Attached` reads through `Recipient.objectOf`.

**Consequence to note, not to fix:** `Sba.stillLegalEnchant` short-circuits to a
`pcs` lookup only for the bare `Pool.Creatures` spec and falls through to the
general `Target.stillAdmitted` for a spec carrying a `Filter`. That is the
function's own documented fallthrough, not a regression; one land-enchanting Aura
does not warrant changing it.

- [ ] **Step 1: Write the card**

`data/cards/convincing-mirage.json` — `{1}{U}`, Enchantment — Aura, "Enchant
land. As this Aura enters, choose a basic land type. Enchanted land is the chosen
type." (Verified on Scryfall, 2026-08-03.) Shape: `unholy-strength.json`'s, with
the enchant spec pointed at lands, Primal Plasma's `EntryR`/`IsSource`
replacement, and Blood Moon's static-ability shape.

```json
{
  "activatedAbilities": [],
  "castingPermissions": [],
  "enchant": {
    "filter": { "type": "HasCardType", "value": { "type": "Land" } },
    "pool": { "type": "Permanents" }
  },
  "keywords": [],
  "manaCost": [
    { "type": "Generic", "value": 1 },
    { "type": "OfType", "value": { "type": "Colored", "value": { "type": "Blue" } } }
  ],
  "name": "Convincing Mirage",
  "power": null,
  "replacementEffects": [
    { "type": "EntryR", "value": [{ "type": "IsSource" }, { "type": "ChooseBasicLandType" }] }
  ],
  "spell": {
    "modes": [{ "effects": [], "targetSpecs": [] }],
    "selection": { "type": "ChooseExactly", "value": 1 }
  },
  "staticAbilities": [
    {
      "affected": { "type": "Attached" },
      "modifications": [{ "type": "SetLandSubtypeToChosen" }]
    }
  ],
  "toughness": null,
  "triggeredAbilities": [],
  "typeLine": {
    "subtypes": [{ "type": "Aura" }],
    "supertypes": [],
    "types": [{ "type": "Enchantment" }]
  }
}
```

**Verify rather than assume:** subtype arrays must be sorted in `Ord`-declaration
order (one entry here, trivially sorted), and `Pawl.Codec.Card` decodes `enchant`
through `TargetSpec.fromJson` — but **no Aura in the pool has used the filter
half before**. If the decoder rejects it, that is a real gap and gets its own
commit, not a workaround. `Pawl.CardSpec`'s "Aura iff enchant" lint is satisfied
in both directions.

- [ ] **Step 2: Verify the card round-trips**

`direnv exec . cabal test 2>&1 | tail -30` — `Pawl.CardsSpec` passes.

- [ ] **Step 3: Write the proving test**

`source/test-suite/Pawl/AuraSpec.hs`, because it needs that file's cast-an-Aura
machinery (`S.handOne`, `Cast.castSpell`, `Stack.resolveTop`, `aimRecipient`);
the Crown of the Ages test (~line 507) is the model.

It must fail if the choice is **ignored** (no type set → still a Mountain,
still `{R}`) *and* if it is **misread** (a hardcoded type → the two halves cannot
both pass). Two answers on one board is what buys the second half.

```haskell
  -- CR 614.1c + CR 305.6/305.7 end to end: cast the Aura, answer its as-enters
  -- prompt, and see the enchanted land tap for the CHOSEN colour. Run twice
  -- with different answers on the same board -- a hardcoded subtype passes one
  -- half and fails the other, which is what makes this prove the choice is READ
  -- rather than merely made.
  Spec.it s "CR 614.1c whole card: Convincing Mirage makes a Mountain the chosen basic land type" $ do
    ...
    Spec.assertEqWith s "before: a plain Mountain" (Projection.subtypesOf landId withAura) $ Set.singleton Subtype.Type.Mountain
    Spec.assertBool s (ManaType.Colored Color.Red `elem` Mana.manaTypesOf landId withAura) "and it taps for red"
    -- CR 305.7: "the land no longer has its old land type"; CR 305.6: a land
    -- with a basic land type has the intrinsic "{T}: Add [mana symbol]".
    Spec.assertEqWith s "choosing Island: only an Island" (Projection.subtypesOf landId asIsland) $ Set.singleton Subtype.Type.Island
    Spec.assertBool s (ManaType.Colored Color.Blue `elem` Mana.manaTypesOf landId asIsland) "so it taps for blue"
    Spec.assertBool s (ManaType.Colored Color.Red `notElem` Mana.manaTypesOf landId asIsland) "and no longer for red"
    Spec.assertEqWith s "choosing Swamp: only a Swamp" (Projection.subtypesOf landId asSwamp) $ Set.singleton Subtype.Type.Swamp
    Spec.assertBool s (ManaType.Colored Color.Black `elem` Mana.manaTypesOf landId asSwamp) "so it taps for black"
```

Verify while writing: the recipient tag (`Pool.Permanents` is documented
`ToObject` — confirm against `aimRecipient`); enough blue mana to cast a
two-drop; that the local decider generalizes (`AuraSpec` has `GADTs`; hoist to a
top-level helper if not, the shape `moveAura` already uses); and that `landId`
survives the cast (CR 400.7 re-mints the *Aura*, not the land).

- [ ] **Step 4: The fold-half strip, on a land with real rules text**

`source/test-suite/Pawl/ManaSpec.hs`, in the CR 305.6/305.7 group (~line 236),
following the Blood Moon'd Reliquary Tower test's exact shape. This proves the
strip reaches an activated ability when the type was *chosen* rather than
printed — and it is fold-half only, so it passes before Task 7.

```haskell
  -- CR 305.7's strip and CR 305.6's hand-back, with the new type CHOSEN rather
  -- than printed. Reliquary Tower's "{T}: Add {C}" is an activated ability, so
  -- this is the sharpest witness that the strip reaches the whole rules text
  -- whichever modification performed it.
  Spec.it s "CR 305.6/305.7 a Convincing Mirage'd Reliquary Tower taps for the chosen colour" $ do
    ...
    Spec.assertBool s (ManaType.Colored Color.White `elem` Mana.manaTypesOf towerId gs) "white available (CR 305.6, from the chosen Plains)"
    Spec.assertBool s (ManaType.Colorless `notElem` Mana.manaTypesOf towerId gs) "colorless gone (the printed {T}: Add {C} was stripped)"
```

- [ ] **Step 5: Fix the haddock this card falsifies, in this commit**

`source/test-suite/Pawl/Support.hs`'s `attach` is **outright false** once this
card exists: "Tagged ToCreature, which is the tag the real attach paths would
store: every Aura in this pool has a Pool.Creatures enchant spec, and Target's
candidates for that pool are ToCreature." Convincing Mirage's enchant spec is
`Pool.Permanents` + a Land filter, whose candidates are `ToObject`. Rewrite: the
tag is what *creature*-enchanting Auras store; it is wrong for Convincing Mirage;
`Affected.Attached` reads through `Recipient.objectOf` and does not care — but
`Sba.stillLegalEnchant` does, so an SBA test involving Convincing Mirage must
cast rather than use this fixture.

Also update `AuraSpec.hs`'s and `ManaSpec.hs`'s module-header covers-lists.

- [ ] **Step 6: Verify green and commit**

```bash
git commit -m "Choose a basic land type as an Aura enters: Convincing Mirage"
```

---

## Task 7: CR 305.7's gate half — `setLandSubtypeEffects`

**The headline.** Failing test first; the failure is the point.

`applyModification` erases the *bearer's own* projection. It cannot reach a
static ability whose effect lands on **other** objects — such an ability has to
be kept out of the gather's candidate list instead, which is
`setLandSubtypeEffects` → `liveGiven` → `staticAbilitiesLive`. `isSet`
classifies by **constructor**, so until it names `SetLandSubtypeToChosen` the two
halves of CR 305.7 disagree: the enchanted land's own subtypes and rules text are
stripped, while its static ability keeps firing at everything else.

Urborg, Tomb of Yawgmoth is the witness already in the pool — "Each land is a
Swamp in addition to its other land types" — and it is what the Blood Moon
version of this test already uses.

**Files:**
- Modify: `source/test-suite/Pawl/ProjectionSpec.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Projection.hs`

- [ ] **Step 1: Write the failing test**

`ProjectionSpec.hs`, beside "CR 305.7/613.8 Blood Moon strips Urborg" (~line 746):

```haskell
  Spec.it s "CR 305.7 Convincing Mirage strips Urborg's static ability, so no land is a Swamp" $ do
    -- The GATE half of CR 305.7, which applyModification cannot do: Urborg's
    -- ability lands on OTHER objects, so it has to be kept out of the candidate
    -- list (Projection.liveGiven, via setLandSubtypeEffects) rather than erased
    -- from Urborg's own projection. Convincing Mirage is the first
    -- CHOSEN-subtype effect to reach that gate and the pool's second static
    -- producer after Blood Moon; classifying by constructor is what makes the
    -- gate and the fold able to disagree, and this is what stops them.
    forest <- S.printingOf s registry "Forest"
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    convincingMirage <- S.printingOf s registry "Convincing Mirage"
    let gs0 = Setup.emptyGame S.bothPlayers
        (forestId, g1) = S.addCreature forest S.alice gs0
        (urborgId, g2) = S.addCreature urborg S.alice g1
        (mirageId, g3) = S.addCreature convincingMirage S.alice g2
        gs = S.withChosenSubtype Subtype.Type.Island mirageId (S.attach mirageId urborgId g3)
    Spec.assertEqWith s "before: Urborg makes the Forest a Swamp too" (Projection.subtypesOf forestId g3) $ Set.fromList [Subtype.Type.Forest, Subtype.Type.Swamp]
    Spec.assertEqWith s "Urborg itself is only an Island now" (Projection.subtypesOf urborgId gs) $ Set.singleton Subtype.Type.Island
    Spec.assertEqWith s "and the Forest is a plain Forest again" (Projection.subtypesOf forestId gs) $ Set.singleton Subtype.Type.Forest
```

Check the "before" assertion against the file's existing Urborg tests — its type
line may already carry subtypes this plan has not read.

- [ ] **Step 2: Run it and watch it fail for the right reason**

`direnv exec . cabal test 2>&1 | grep -A8 "strips Urborg's static ability"`

Expected: the *second* assertion passes (the fold half already strips Urborg's
own projection) and the **third fails** — the Forest is still a Swamp, because
Urborg's static ability is still in the candidate list. That asymmetry is
precisely the two-samplers bug; confirm you have seen it before fixing it.

- [ ] **Step 3: Close the gate**

`Projection.setLandSubtypeEffects`'s local `isSet`:

```haskell
      let isSet m = case m of
            Modification.SetLandSubtype _ -> True
            -- CR 305.7 does not care WHERE the type came from: a type chosen as
            -- the source entered strips the land's rules text exactly as a
            -- printed one does. Answering False here strips the land inside the
            -- fold (applyModification) while leaving its static abilities in the
            -- candidate list -- the two halves of one rule disagreeing, which is
            -- what the "strips Urborg's static ability" test in
            -- Pawl.ProjectionSpec exists to catch.
            Modification.SetLandSubtypeToChosen -> True
```

leaving the existing named-`False` arms and the trailing wildcard alone.

- [ ] **Step 4: Fix the "only Blood Moon" claims, in this commit**

This is the commit that makes Convincing Mirage enter `setLandSubtypeEffects`, so
it is the commit that falsifies these. Cite by function and arm, never by line.
In `source/libraries/engine/Pawl/Engine/Projection.hs`:

1. **`controlGrants`'s `INVARIANT` block (#197)** — "it holds only because no
   SetLandSubtype in the pool … carries a Matching filter with ControlledBy in
   it: the pool's one static example (Blood Moon) has none." The invariant
   **survives**: Convincing Mirage's set is `Affected.Attached`, which carries no
   filter at all and whose `affects` arm never touches `controllerOf`. Say two
   static examples, and why neither forces the thunk.
2. **The `AttachedPlayerControls` arm's comment** — "None does -- the pool's one
   static example is Blood Moon." Survives (Convincing Mirage carries `Attached`,
   **not** `AttachedPlayerControls`), count wrong. Name the distinction
   explicitly; the two arm names differ by one word and this is exactly where a
   reader would conflate them.
3. **`setLandSubtypeEffects`'s `fromPerm` note (#584/#624)** — "Unreachable: the
   pool's only SetLandSubtype is Blood Moon, which selects by card type and
   supertype and so carries no land-type word for a swap to reach." Still
   unreachable, and now for a **second, independent** reason:
   `rewriteAffected`'s `Affected.Attached` arm is a no-op, so a text change could
   not alter Convincing Mirage's affected set even if one reached it. Say both.
4. **`staticAbilitiesLive`'s haddock**, which frames the gate around Blood Moon
   and Ashaya. Add Convincing Mirage as the second producer. Do **not** rewrite
   the Ashaya/#391 or #37 discussion, which is unaffected.
5. **`applyModification`'s `SetLandSubtypeToChosen` arm** — its "the gate
   classifies by CONSTRUCTOR" sentence was written in Task 5 against a gate that
   answered `False` by wildcard. Re-read it now that the gate answers `True` and
   make sure it still says something true.

Then sweep for anything missed:
```bash
grep -rn 'only SetLandSubtype\|one static example' source/
```

- [ ] **Step 5: Verify green and commit**

Expected: warning-free, green.

```bash
git commit -m "Strip a chosen-subtype land's static abilities too (CR 305.7)"
```

---

## Task 8: Self-review and the PR

- [ ] **Step 1: Self-review the branch**

1. **Re-check every CR citation this branch added or touched against
   `docs/rules.txt`**: 105.1, 205.1b, 205.3i, 303.4a, 303.4c, 303.4d, 305.6,
   305.7, 400.7, 604.3, 608.2d, 612.1, 613.1c, 613.1d, 613.7a, 613.7d, 614.1c,
   614.12, 614.12a, 702.5a, 704.5m, 707.5. Do not trust this plan's rendering.
2. **Re-read every comment the branch touched.** Each task named the prose it
   broke and fixed it in place; this pass is for what those lists missed —
   `Prompt.hs`'s two land-type comments, `Object.hs`'s `chosenColor` block,
   `EntryRewrite.hs`'s header, `Replacement.hs`'s arm counts, and every
   `ChooseLandTypeSwap` citation from Task 1.
3. Confirm the diff does **not** make the rules core case on an effect's
   *identity*. `Projection` casing on `Modification` is the sanctioned exception,
   and `setLandSubtypeEffects`'s `isSet` is inside it. The answer is an explicit
   "no".

Fix findings on the branch.

- [ ] **Step 2: Final verification**

```bash
direnv exec . cabal build all 2>&1 | tail -20
direnv exec . cabal test 2>&1 | tail -20
```
Record 2590 → the final count.

- [ ] **Step 3: Open the PR**

Draft first, then ready once findings are pushed and the suite is green. The body
carries: what changed and why with `Closes #608` as **bare text, never in
backticks**; the CR citations, each checked against `rules.txt`; the settled
design calls and the alternatives rejected (the singular prompt and the
`ChooseLandTypeSwap` rename over one near-namesake pair; the sibling field over a
generalized choice map; `Pool.Permanents` + a filter over a new `Pool.Lands`; the
no-op on an unchosen subtype); verification (build warning-free, `hooky run`
clean, 2590 → final, and the two-answer proving test); an explicit "no" on
rules-core-cases-on-identity; and what was deferred.

**Give CR 305.7's gate half its own paragraph.** Two implementations of one rule,
one of them classifying by constructor and behind a wildcard the compiler cannot
police, is how a rule silently half-applies; the `isSet` commit is
failing-test-first precisely so the disagreement is on the record.

State also that #608's motivating argument — the Magical Hack race giving the
player information a cast-time prompt would not — is about what the *player
knows*, not what the board does, is therefore not assertable by this engine's
tests, and that **no test in this branch gestures at it**. The owner is recording
the same on the issue.

Do not wait for CI. Do not start the next unit.
