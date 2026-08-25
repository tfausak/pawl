# Compleated / counter-multiplier ordering: implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Put CR 702.150a's compleated and CR 614.16's counter multipliers in one CR 616.1 pool, so the affected player is asked which applies first and both loyalty totals are reachable.

**Architecture:** Today the entry loop applies an `EntryR WithCounters` row by *performing* a counter placement, which opens a second, nested CR 616.1 loop inside the CR 122.6 funnel; that inner loop is where `CounterR` lives. The rules have no such inner event -- CR 614.1c makes "enters with N counters" a modification of the ENTRY, with the counters materializing once. This hoists the pool up to match: entry rows accumulate a PENDING per-kind count, `CounterR` and a new compleated row rewrite that pending count as siblings under CR 616.1e, and `runEntry` flushes it through the funnel's write-and-emit half once the loop drains. The funnel itself stays for every placement that is not an entry, which CR 614.16 requires.

**Tech Stack:** Haskell, cabal, tasty. `pawl:types` -> `pawl:codec` -> `pawl:engine` -> `pawl:test`.

**Spec:** GitHub issue #1996 and its four comments. The owner's 2026-08-24 design call ("one CR 616.1 pool") is the direction; the 2026-08-25 comment records what a prior research pass established. Read both before starting.

## Global Constraints

- Ground truth is `docs/rules.txt`, grepped by rule number. Never a rule number quoted in this plan or in the issue -- re-check every citation.
- The rules core must never case on an effect's IDENTITY. Keywords are the stated exception (CLAUDE.md), so a `Keyword.Compleated` arm is legitimate; a `case effect of DealDamage{}` is not.
- Constructors take a `Mk` prefix and never pun the type name. One type per module under `Pawl.Types.<TypeName>`.
- Language extensions come from `.hlint.yaml`'s allowlist.
- STAGE, then `hooky fix`, then `git add` again. Adding a module means staging `pawl.cabal` and running `cabal-gild pawl.cabal` directly.
- Commit before mutation testing, so a blocked commit does not destroy the baseline.
- Copy `cabal.project.local` from the primary checkout before the first build in a fresh worktree, or `pedantic`/`-Werror` are off and green means nothing.

## Scope note

This is one unit but a large one. Tasks 1-2 are pure plumbing with no behaviour change and can be reviewed on that basis. Task 3 is the behavioural pivot and the only place existing tests move. Task 4 is the feature. Task 5 is the paperwork the issue owes. Do not reorder: Task 3 without 3's `CounterR` arm reddens Doubling Season everywhere.

---

## Decisions already made, with the reasoning

Recording these so the implementer does not relitigate them.

**Why hoist and not push compleated down into the funnel.** A compleated row expressed against a counter PLACEMENT cannot tell Tamiyo entering from Tamiyo's `+1` loyalty ability resolving: `CounterCause` is `ByEffect | ByPlayer | ByRule` and both are `ByEffect`. Such a row would subtract two every activation. Recovering the distinction means reinventing entry framing one layer down, having already paid for the move. `Scaling` also has no subtracting arm (`Multiply | AddMore | Halve`).

**Why `GameState` and not `Object` for the pending count.** `Pawl.Types.Object` has 39 fields and roughly forty construction sites, several of them fully positional in the test suite (`Pawl/ResolveSpec.hs:840`, `Pawl/CounterspellSpec.hs:171`, `Pawl/ZoneChangeSpec.hs:103`). A new `Map ... Natural` next to `counters :: Map ... Natural` would be absorbed silently in argument order -- the trap CLAUDE.md names, which bit PRs #2009 and #2021. `Pawl.Types.GameState` has three construction sites, all record syntax, and already carries `enteringBeside` (`GameState.hs:307`) with the save-and-restore discipline `runEntry` needs.

**Why a separate map and not `Object.counters`.** Two sites place entry counters BEFORE `runEntry` (`Event.hs:3351` and `Event.hs:4045`), and both carry a comment resting on "no entry replacement in the pool reads a counter the entering object already has". A pending map distinct from `Object.counters` keeps that true by construction; reusing `Object.counters` would break it and make a doubling row scale counters the object already had.

**Multi-kind granularity.** Today each kind is a separate `putOwnCountersIn` call, so a `CounterR` row gets a fresh nested loop per kind and applies once per kind. After the hoist there is one loop, so CR 614.5 consumes the row once and it scales every pending kind it matches in that one application. That reading is closer to CR 614.16's own wording ("one or more counters"), and the two are indistinguishable unless a permanent enters with two kinds one row matches -- no such card is in the pool. File the elision issue in Task 5.

---

### Task 1: A pending entry-counter map on the game state

No behaviour change: the field is written by nobody and read by nobody. Landing it alone keeps the codec churn out of the behavioural diff.

**Files:**
- Modify: `source/libraries/types/Pawl/Types/GameState.hs` (beside `enteringBeside`, line 307)
- Modify: `source/libraries/codec/Pawl/Codec/GameState.hs:88` (decode) and `:152` (assemble)
- Modify: `source/libraries/engine/Pawl/Engine/Setup.hs:131`, `:454`, `:593`
- Modify: `source/libraries/test/Pawl/Support.hs:1603`
- Test: `source/libraries/codec/Pawl/Codec/GameStateSpec.hs`

**Interfaces:**
- Produces: `GameState.enteringCounters :: Map.Map ObjectId.ObjectId (Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural)`

- [ ] **Step 1: Add the field**

In `Pawl.Types.GameState`, immediately after `enteringBeside`:

```haskell
    -- | CR 614.1c: the counters each entering permanent is so far going to enter
    -- WITH, pending. Written by Pawl.Engine.Event's entry rewrites and by the two
    -- doors that dress a permanent before its entry loop, rewritten by the
    -- CounterR and Compleated rows that CR 616.1 orders against each other, and
    -- flushed onto the object by runEntry once the loop drains.
    --
    -- SEPARATE from Object.counters, not folded into it, because the two doors
    -- above run before runEntry and their comments rest on no entry replacement
    -- reading a counter the object already has. A pending map keeps that true:
    -- nothing here is on the object yet.
    --
    -- Keyed by ObjectId rather than held as one map, because an entry rewrite can
    -- reach another entry (SacrificeAnyNumber sacrifices, RunEffects resolves) and
    -- the outer subject's pending count has to survive the inner one.
    --
    -- Empty outside an entry. An id is inserted when the first row gives that
    -- object counters and deleted by the flush.
    enteringCounters :: Map.Map ObjectId.ObjectId (Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural),
```

Add `CounterKind` and `Keyword` imports if the module lacks them; `-Werror` will name them.

- [ ] **Step 2: Build to find every construction site**

Run: `cabal build pawl:types pawl:codec pawl:engine 2>&1 | head -40`
Expected: FAIL, missing-field errors at `Setup.hs:131`, `:454`, `:593`, `Codec/GameState.hs:152`, `Support.hs:1603`. Record the list -- these are the only sites, and all use record syntax, so none can absorb the field positionally.

- [ ] **Step 3: Fill in every site**

`Setup.hs` (three sites) and `Support.hs:1603`:

```haskell
          GameState.enteringCounters = Map.empty,
```

`Codec/GameState.hs`, beside the `enteringBeside` line at 88:

```haskell
  enteringCounters <- Fields.required "enteringCounters" (Common.map ObjectId.codec (Common.map CounterKind.codec Common.natural)) GameState.enteringCounters
```

and at 152:

```haskell
        GameState.enteringCounters = enteringCounters,
```

Check the helper names against the neighbouring lines in that file rather than trusting this snippet -- `Common.map`'s key encoding for a non-text key may need the same treatment `counters` gets in `Pawl.Codec.Object`. Copy whatever that codec does for `Object.counters`, which has the identical inner type.

- [ ] **Step 4: Round-trip test**

In `Pawl.Codec.GameStateSpec`, extend the existing full-state round-trip fixture so `enteringCounters` is non-empty in at least one case -- an empty map round-trips through almost any bug. Follow the file's existing `Common.assertJson` shape.

- [ ] **Step 5: Build and test**

Run: `cabal test 2>&1 | tail -20`
Expected: PASS, suite count up by the round-trip case.

- [ ] **Step 6: Stage, format, commit**

```bash
git add source/libraries/types/Pawl/Types/GameState.hs source/libraries/codec/Pawl/Codec/GameState.hs source/libraries/codec/Pawl/Codec/GameStateSpec.hs source/libraries/engine/Pawl/Engine/Setup.hs source/libraries/test/Pawl/Support.hs
hooky fix
git add -u
git commit -m "Add a pending entry-counter map to the game state"
```

---

### Task 2: Split the funnel, and flush the pending map

Still no behaviour change: nothing writes to `enteringCounters`, so the flush is a no-op on every board. This isolates the funnel surgery from the semantic move.

**Files:**
- Modify: `source/libraries/engine/Pawl/Engine/Event.hs` -- `putCountersIn` (line 2407), `runEntry` (line 2188)

**Interfaces:**
- Consumes: `GameState.enteringCounters` from Task 1
- Produces: `settleCounters :: ObjectId -> CounterKind.CounterKind Keyword.Type.Keyword -> Natural -> Game Natural` -- the write-and-emit half of `putCountersIn`, with NO CR 616.1 loop in front of it. And `flushEnteringCounters :: ObjectId -> Game ()`.

- [ ] **Step 1: Extract `settleCounters` from `putCountersIn`**

`putCountersIn` currently does resolve-then-write-and-emit. Split the second half out verbatim -- the timestamp, the `bump`, the `CountersPut` record, the `settledCount == 0` and missing-object guards -- and have `putCountersIn` call it:

```haskell
putCountersIn :: Set ObjectId -> CounterCause.CounterCause -> ObjectId -> CounterKind.CounterKind Keyword.Type.Keyword -> Natural -> Game Natural
putCountersIn batch cause oid kind n = do
  resolved <- resolveCounters batch cause oid kind n
  case resolved of
    Nothing -> pure 0
    Just (target, settledKind, settledCount) -> settleCounters target settledKind settledCount

-- The WRITE-AND-EMIT half of putCountersIn, with no CR 616.1 loop in front of it:
-- the counters have already been settled and this records them. Its second caller
-- is flushEnteringCounters below, where CR 616.1 ran at the ENTRY level and running
-- it again here would let one row apply twice.
--
-- Not a second funnel. putCountersIn is still the only door that both settles and
-- writes; this is that door's tail, shared rather than copied.
settleCounters :: ObjectId -> CounterKind.CounterKind Keyword.Type.Keyword -> Natural -> Game Natural
settleCounters target settledKind settledCount
  | settledCount == 0 = pure 0
  | otherwise = do
      -- ... the existing body from `gs <- State.get` down, verbatim ...
```

- [ ] **Step 2: Write `flushEnteringCounters`**

Beside `runEntry`:

```haskell
-- CR 614.1c: the counters the entering permanent turned out to be entering WITH,
-- placed once, after CR 616.1's loop has finished deciding how many that is.
--
-- Through settleCounters and NOT putCounters, because the CR 616.1 loop the
-- funnel's door opens has already run at the entry level -- see the CounterR arm
-- of `apply`. Going back through the door would offer every counter-scaling row a
-- second opportunity CR 614.5 has already spent.
--
-- Ascending by kind so the CountersPut events are ordered deterministically; no
-- rule fixes the order, and nothing between them can observe it, since a trigger
-- scan runs only once this whole entry finishes.
--
-- The id is deleted whether or not anything was pending, so a nested entry cannot
-- inherit an outer subject's leftovers.
flushEnteringCounters :: ObjectId -> Game ()
flushEnteringCounters oid = do
  pending <- State.gets (Map.findWithDefault Map.empty oid . GameState.enteringCounters)
  State.modify' (\gs -> gs {GameState.enteringCounters = Map.delete oid (GameState.enteringCounters gs)})
  Monad.mapM_ (\(kind, n) -> Monad.void (settleCounters oid kind n)) (Map.toAscList pending)
```

- [ ] **Step 3: Call it from `runEntry`**

Between the loop and `designateProtector`:

```haskell
  Monad.void (applyReplacementsIn Nothing batch (ProposedEvent.WouldEnter oid))
  flushEnteringCounters oid
  designateProtector oid
```

Before `designateProtector` rather than after, so CR 310.4b's defense counters are on the battle when its protector is chosen -- the same reason the counters were placed inside the loop before.

- [ ] **Step 4: Build and test**

Run: `cabal test 2>&1 | tail -20`
Expected: PASS, suite count UNCHANGED. Nothing writes `enteringCounters` yet, so `flushEnteringCounters` finds nothing on every board. A red suite here means the extraction of `settleCounters` was not verbatim.

- [ ] **Step 5: Commit**

```bash
git add source/libraries/engine/Pawl/Engine/Event.hs
hooky fix
git add -u
git commit -m "Split the counter funnel's write half out, and flush pending entry counters"
```

---

### Task 3: Move entry counters into the entry pool

The behavioural pivot. Every path that gives a permanent counters as it enters stops placing them and starts accumulating them; `CounterR` gains an arm against `WouldEnter` so CR 614.16 still reaches them, now one level up. This lands as ONE task because splitting it reddens every Doubling Season test in between.

**Files:**
- Modify: `source/libraries/engine/Pawl/Engine/Event.hs` -- `apply`'s `WithCounters` (1384), `SacrificeAnyNumber` (~1525), `ReadAhead` (~1570), `Riot` (~1612), `Unleash`/`Bloodthirst` (~1660, ~1677) arms; the pre-entry doors at 3351 and 4045
- Modify: `source/libraries/engine/Pawl/Engine/Replacement.hs` -- `applies` (389), `bucketOfEffect` (920), `readsApplier` (1023)
- Test: `source/libraries/test/Pawl/SagaSpec.hs:179`, `source/libraries/test/Pawl/PlaneswalkerSpec.hs:401`

**Interfaces:**
- Consumes: `GameState.enteringCounters`, `flushEnteringCounters` from Tasks 1-2
- Produces: `addEnteringCounters :: ObjectId -> CounterKind.CounterKind Keyword.Type.Keyword -> Natural -> Game ()`

- [ ] **Step 1: Write `addEnteringCounters`**

Beside `putOwnCountersIn`:

```haskell
-- CR 614.1c: this permanent is going to enter with N more counters of a kind than
-- it was a moment ago. Adds to the pending map rather than to the object -- see
-- GameState.enteringCounters and flushEnteringCounters.
--
-- The replacement for putOwnCountersIn at every ENTRY door. The `batch` those
-- doors used to pass is gone and is not owed: CR 614.12's sibling exclusion is the
-- entry loop's own `notSibling` filter, which now covers these rows too.
addEnteringCounters :: ObjectId -> CounterKind.CounterKind Keyword.Type.Keyword -> Natural -> Game ()
addEnteringCounters oid kind n =
  Monad.when (n > 0) . State.modify' $ \gs ->
    gs
      { GameState.enteringCounters =
          Map.insertWith (Map.unionWith (+)) oid (Map.singleton kind n) (GameState.enteringCounters gs)
      }
```

- [ ] **Step 2: Swap all seven call sites**

In `apply`, each of `putOwnCountersIn batch oid <kind> <n>` becomes `addEnteringCounters oid <kind> <n>` -- dropping the `Monad.void` where the old call's `Natural` was being discarded. Sites: the `WithCounters` arm, `SacrificeAnyNumber`, `ReadAhead`, `Riot`, and the two `Bloodthirst`/`Unleash` arms. At 3351 and 4045 the same swap, dropping `batch`/`siblingsOf oid`.

DO NOT touch the `TurnUpR` `WithCounters` arm near line 2060 -- CR 614.1e's turning face up is not an entry, has no pending map, and keeps the funnel.

- [ ] **Step 3: Give `CounterR` an arm against `WouldEnter`**

In `Replacement.applies`, beside the existing `CounterR`/`WouldPutCounters` arm:

```haskell
  -- CR 614.16 at the ENTRY level: the counters this permanent is entering with are
  -- a placement this row may scale, and CR 616.1 orders it against every other row
  -- modifying the same entry -- CR 702.150a's compleated among them. The pending
  -- count is what it scales; see GameState.enteringCounters.
  --
  -- The CAUSE is synthesized as the one the flush will place under -- CR 122.6a's
  -- default putter, the object's controller -- so matchesPutter still parts
  -- Vorinclex's "if YOU would put" from CR 614.16's "if an effect would put"
  -- (#847). Reading it off the flush rather than off the event is what keeps the
  -- two levels answering alike.
  (ReplacementEffect.CounterR (CounterR.MkCounterR pattern _), ProposedEvent.WouldEnter oid) ->
    case Projection.controllerOf oid gs of
      Nothing -> False
      Just putter ->
        not (Map.null (matchingEnteringCounters gs pattern oid))
          && matchesPutter gs (ReplacementCandidate.source candidate) (CounterPattern.byWhom pattern) (CounterCause.ByEffect putter)
          && matchesPermanent gs (Just oid) (CounterPattern.onWhat pattern) oid
          && matchesController gs (CounterPattern.whose pattern) oid
```

Write two helpers beside it. `enteringCountersOf` is the bare lookup, and Task 4's `admitsEntry` arm uses it too:

```haskell
-- The counters `oid` is so far entering with, empty outside an entry.
enteringCountersOf :: GameState -> ObjectId -> Map.Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural
enteringCountersOf gs oid = Map.findWithDefault Map.empty oid (GameState.enteringCounters gs)

-- Those of them this pattern's kind admits. CR 614.16's rows name no kind
-- (Doubling Season doubles any counter), so Nothing matches every kind.
matchingEnteringCounters :: GameState -> CounterPattern.CounterPattern -> ObjectId -> Map.Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural
matchingEnteringCounters gs pattern oid =
  let pending = enteringCountersOf gs oid
   in case CounterPattern.whichKind pattern of
        Nothing -> pending
        Just kind -> Map.filterWithKey (\k _ -> k == kind) pending
```

Check each helper's real arity against the existing `WouldPutCounters` arm before writing -- the snippet above names `matchesPutter`, `matchesPermanent` and `matchesController` from that arm and their argument order may differ. Both new helpers are exported from `Pawl.Engine.Replacement`, since `Event.apply` calls them.

- [ ] **Step 4: Give `apply` the matching arm**

In `Event.apply`, beside the existing `CounterR` arm:

```haskell
    (ReplacementEffect.CounterR (CounterR.MkCounterR pattern scaling), ProposedEvent.WouldEnter oid) -> do
      Replacement.consume (ReplacementCandidate.identity candidate)
      gs <- State.get
      let scaled = fmap (Replacement.scale scaling) (Replacement.matchingEnteringCounters gs pattern oid)
      State.modify' $ \gs2 ->
        gs2 {GameState.enteringCounters = Map.adjust (Map.union scaled) oid (GameState.enteringCounters gs2)}
      pure (Just event)
```

`Map.union` is left-biased, so the scaled kinds win and any kind the pattern did not match is left as it was.

EVERY pending kind the pattern matches is scaled in ONE application, where the old nested-loop shape gave the row a fresh opportunity per kind. Add the comment saying so and citing the issue Task 5 files.

- [ ] **Step 5: Answer `bucketOfEffect` and `readsApplier`**

Neither needs a new arm -- both dispatch on the effect alone and `CounterR` already answers `ReplacementBucket.Other` (CR 616.1e) and its `readsApplier` value. Confirm by reading both, and record in the PR that you did. If `readsApplier` answers `True` for `CounterR`, `choose`'s indistinguishability test already folds CR 109.5's "you" in, which is what a two-Doubling-Season board needs.

- [ ] **Step 6: Build**

Run: `cabal build pawl:engine 2>&1 | head -40`
Expected: PASS. If `-Werror` names an unmatched `ProposedEvent` in `applies` or `apply`, an arm was missed.

- [ ] **Step 7: Run the suite and read what moved**

Run: `cabal test 2>&1 | tail -40`
Expected: PASS with the count unchanged. The two tests that PROVE this path still work are `Pawl/SagaSpec.hs:179` ("CR 614.16 Doubling Season doubles the entering lore counter but NOT the turn-based one") and `Pawl/PlaneswalkerSpec.hs:401` ("CR 614.16 Doubling Season doubles a planeswalker's starting loyalty"). Both must stay green WITHOUT edits -- they are the regression gate for the whole hoist. `Pawl/SagaSpec.hs:201` (Vorinclex and the turn-based lore counter) exercises CR 714.3c, which is `ByRule` and not an entry, so it must be untouched by this task.

If any test needs its EXPECTATION changed to pass, stop and report rather than editing it.

- [ ] **Step 8: Mutate**

Run: `script/mutate.sh` with the `WithCounters` arm's `addEnteringCounters` call deleted.
Expected: `Pawl/PlaneswalkerSpec.hs:401` reddens on its loyalty assertion. Then restore, and delete the new `CounterR`/`WouldEnter` arm of `applies`.
Expected: the same test reddens, now at 5 rather than 10 -- which is what proves CR 614.16 is reaching the entry pool and not surviving on some other path.

- [ ] **Step 9: Commit**

```bash
git add source/libraries/engine/Pawl/Engine/Event.hs source/libraries/engine/Pawl/Engine/Replacement.hs
hooky fix
git add -u
git commit -m "Hoist entry counters into the entry loop's CR 616.1 pool"
```

---

### Task 4: Compleated becomes a row

**Files:**
- Modify: `source/libraries/types/Pawl/Types/EntryRewrite.hs`
- Modify: `source/libraries/codec/Pawl/Codec/EntryRewrite.hs`
- Modify: `source/libraries/engine/Pawl/Engine/Projection.hs:4039`
- Modify: `source/libraries/engine/Pawl/Engine/Event.hs` (`apply`), `source/libraries/engine/Pawl/Engine/Replacement.hs` (`applies`, `bucketOfEffect`, `readsApplier`, `admitsEntry`)
- Test: `source/libraries/test/Pawl/ManaSpec.hs` (Tamiyo group, from line 5923)

**Interfaces:**
- Consumes: `addEnteringCounters`, `GameState.enteringCounters`, the entry-level `CounterR` arm
- Produces: `EntryRewrite.Compleated Natural.Natural` -- the payload is the number of Phyrexian mana symbols life was paid for, NOT the number of counters to subtract

- [ ] **Step 1: Write the failing test**

In `Pawl.ManaSpec`'s Tamiyo group (from line 5923), following the shape of the
five tests already there. `S.addCreature` puts Doubling Season on the
battlefield and hands back its id, which is what tells the two rows apart in the
CR 616.1 prompt: compleated's source is Tamiyo herself, Doubling Season's is the
enchantment.

Only ONE `ChooseReplacement` is raised. On the loop's first pass nothing is
pending, so neither compleated's `admitsEntry` nor Doubling Season's entry-level
`applies` is satisfied and CR 306.5b's row is the lone candidate; it applies
unprompted, and CR 616.2 makes the other two applicable together on the next
pass. Assert that count -- it is what proves the engine is not inventing a
decision.

```haskell
  -- CR 616.1e: compleated and CR 614.16's multiplier are siblings on ONE event,
  -- not CR 616.1g's nesting -- Tamiyo, Compleated Sage's third Gatherer ruling
  -- says so outright: "Any other replacement effect that would apply to the
  -- number of loyalty counters it enters the battlefield with will apply as
  -- normal." So the controller picks, and the two orders are two boards.
  Spec.it s "CR 616.1e compleated and Doubling Season are ordered against each other" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    tamiyo <- S.printingOf s registry "Tamiyo, Compleated Sage"
    doublingSeason <- S.printingOf s registry "Doubling Season"
    let (seasonId, board) = S.addCreature doublingSeason S.alice (mixedLands forest island 3 2)
        (gs, tamiyoId) = S.handOne tamiyo board
        (askedFirst, compleatedFirst) = castAndResolve (racesCompleated True seasonId) gs tamiyoId
        (_, doubledFirst) = castAndResolve (racesCompleated False seasonId) gs tamiyoId
    Spec.assertEqWith s "CR 702.150a then CR 614.16: 5 less two is 3, doubled is 6" (S.counterOf CounterKind.Loyalty (tamiyoOn compleatedFirst) compleatedFirst) 6
    Spec.assertEqWith s "CR 614.16 then CR 702.150a: 5 doubled is 10, less two is 8" (S.counterOf CounterKind.Loyalty (tamiyoOn doubledFirst) doubledFirst) 8
    Spec.assertEqWith s "CR 306.5b's row had no rival on the first pass, so exactly one order was asked" (length (filter wasReplacementChoice askedFirst)) 1
```

And the answer beside `announcesBoth` at the foot of the module:

```haskell
-- Pay the {G/U/P} with life, and answer the CR 616.1e race by SOURCE -- Doubling
-- Season's row names the enchantment, compleated's names Tamiyo -- so the
-- assertion does not rest on the engine's canonical candidate order.
-- Pawl.ReplacementSpec.raceIsSelf is the same idiom.
racesCompleated :: Bool -> ObjectId.ObjectId -> Prompt.Prompt r -> r
racesCompleated wantCompleated seasonId p = case p of
  Prompt.ChooseReplacement _ _ entries ->
    maybe 0 Int.toNaturalSaturating (List.findIndex ((== wantCompleated) . (/= seasonId) . ReplacementEntry.source) entries)
  _ -> announcesBoth PhyrexianPayment.PaysLife greenMana p
```

`wasReplacementChoice :: Response.Response -> Bool` needs writing beside
`phyrexianAnnouncements` (line 5171), which shows how that module reads a
recorded transcript; `Pawl/ReplacementSpec.hs:3067`'s `wasAskedToReplace` is the
same predicate one module over, and may be copyable outright.

- [ ] **Step 2: Run it to watch it fail**

Run: `cabal test --test-options='-p "Tamiyo"' 2>&1 | tail -30`
Expected: FAIL. Today pawl answers 6 with no prompt at all, so the first assertion passes by accident and the SECOND is the one that reddens -- and the test as written also fails to find a prompt to answer. Both are the divergence. Confirm the failure is the 8 assertion and not a helper that does not compile.

- [ ] **Step 3: Add the constructor**

In `Pawl.Types.EntryRewrite`:

```haskell
  | -- | CR 702.150a: compleated. "If this permanent would enter with one or more
    -- loyalty counters on it and the player who cast it chose to pay life for any
    -- part of its cost represented by Phyrexian mana symbols, it instead enters the
    -- battlefield with that many loyalty counters minus two for each of those mana
    -- symbols."
    --
    -- The payload is the number of PHYREXIAN MANA SYMBOLS life was paid for, not
    -- the number of counters to subtract: rule 702.150a's two is the rule's, so
    -- doubling it here would put a card's number where a rule's belongs.
    --
    -- A ROW rather than arithmetic folded into CR 306.5b's count, so CR 616.1 can
    -- order it against CR 614.16's multipliers -- which is the whole of #1996.
    -- Tamiyo, Compleated Sage's third Gatherer ruling is what makes them siblings
    -- rather than CR 616.1g's nesting: "Any other replacement effect that would
    -- apply to the number of loyalty counters it enters the battlefield with will
    -- apply as normal."
    --
    -- SUBTRACTS from the pending count, never from Object.counters, so nothing
    -- keyed on counter REMOVAL sees it. Rule 702.150a removes nothing; it changes
    -- how many arrive.
    Compleated Natural.Natural
```

- [ ] **Step 4: Teach `Projection` to mint it, and stop baking**

At `Projection.hs:4039`, the loyalty comprehension's `let n = if Map.member ... then minusSaturating ... else base` collapses back to `let n = base`, and a second row joins the list:

```haskell
    -- CR 702.150a: its own row, so CR 616.1 orders it against CR 614.16's
    -- multipliers. Minted only when life was actually paid, because rule 702.150a's
    -- own condition is "chose to pay life" -- a row that applied for zero symbols
    -- would subtract nothing and still cost the controller an ordering prompt.
    <> [ ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource (EntryRewrite.Compleated phyrexianLifePaid))
       | Set.member CardType.Planeswalker (PC.cardTypes pc),
         Map.member Keyword.Type.Compleated (PC.keywords pc),
         phyrexianLifePaid > 0
       ]
```

- [ ] **Step 5: `admitsEntry`, `bucketOfEffect`, `readsApplier`, `apply`**

`admitsEntry` gets rule 702.150a's "would enter with ONE OR MORE loyalty counters" -- the row does not apply when nothing is pending:

```haskell
  EntryRewrite.Compleated _ -> Map.findWithDefault 0 CounterKind.Loyalty (enteringCountersOf gs oid) > 0
```

That condition is also what makes CR 616.2 work: on the first iteration nothing is pending, so only CR 306.5b's row is applicable; it applies, and compleated and Doubling Season become applicable together on the next pass. Confirm this by reading the loop rather than trusting it.

`bucketOfEffect` -> `ReplacementBucket.Other`, with a comment saying CR 616.1a-d name self-replacement, control on entry, copy on entry and back face up, and rule 702.150a is none of them.

`readsApplier` -> `False`: applying it reads the row's payload and the pending count, nothing off the candidate.

`apply`:

```haskell
      -- CR 702.150a: "minus two for each of those mana symbols". Saturating,
      -- because a counter count is a Natural -- rule 702.150a's own "would enter
      -- with one or more" plus admitsEntry's guard is what makes the floor
      -- unreachable on a printed card anyway.
      EntryRewrite.Compleated symbols -> do
        Replacement.consume (ReplacementCandidate.identity candidate)
        State.modify' $ \gs ->
          gs
            { GameState.enteringCounters =
                Map.adjust (Map.adjust (\n -> Natural.minusSaturating n (2 * symbols)) CounterKind.Loyalty) oid (GameState.enteringCounters gs)
            }
        pure (Just event)
```

- [ ] **Step 6: Codec**

`Pawl.Codec.EntryRewrite` uses `Arm.tagged` with a `_ -> Nothing` fallthrough (CLAUDE.md, #1715), so a missing arm compiles silently. Add the arm by hand and add its case to `Pawl.Codec.EntryRewriteSpec` -- follow `Bloodthirst`'s, which carries the same `Natural` payload.

- [ ] **Step 7: Run the test**

Run: `cabal test --test-options='-p "Tamiyo"' 2>&1 | tail -30`
Expected: PASS, both assertions.

- [ ] **Step 8: Full suite**

Run: `cabal test 2>&1 | tail -20`
Expected: PASS. Record the count before -> after for the PR.

Watch the six existing Tamiyo tests especially: one of them casts her paying life and asserts loyalty 3. That must STILL be 3 with no Doubling Season out, and it must now pass through the row rather than the baked subtraction.

- [ ] **Step 9: Mutate**

Commit first. Then, one at a time:
- Change `2 * symbols` to `symbols`. Expected: the new test's "compleated then doubled" assertion reddens at 8 rather than 6, and the existing loyalty-3 test reddens at 4.
- Delete the `phyrexianLifePaid > 0` guard in `Projection`. Expected: a test asserting Tamiyo cast WITHOUT paying life reddens -- if nothing reddens, the elision that no prompt is owed for zero symbols is unproven; say so in the PR.
- Change `admitsEntry`'s `> 0` to `>= 0`. Expected: the loop offers compleated before CR 306.5b's row has run, so the new test reddens.

For each, name the assertion that reddened. A mutation reported only as "went red" is one nobody diagnosed.

- [ ] **Step 10: Commit**

```bash
git add source/libraries/types/Pawl/Types/EntryRewrite.hs source/libraries/codec/Pawl/Codec/EntryRewrite.hs source/libraries/codec/Pawl/Codec/EntryRewriteSpec.hs source/libraries/engine/Pawl/Engine/Projection.hs source/libraries/engine/Pawl/Engine/Event.hs source/libraries/engine/Pawl/Engine/Replacement.hs source/libraries/test/Pawl/ManaSpec.hs
hooky fix
git add -u
git commit -m "Mint compleated as its own CR 616.1-orderable entry row"
```

---

### Task 5: The paperwork the issue owes

**Files:**
- Modify: `source/libraries/types/Pawl/Types/Keyword.hs` (the `Compleated` haddock, ~1152)
- Modify: `source/libraries/engine/Pawl/Engine/Projection.hs` (the `intrinsicReplacementsOf` haddock, ~4021)
- Modify: `source/libraries/engine/Pawl/Engine/Event.hs` (the comments at ~1537, ~1582, ~3344, ~4032 that say counters reach CR 614.16 "through the funnel")
- Modify: GitHub issue #877's body

- [ ] **Step 1: Grep the bare issue number**

Run: `grep -rn "1996" source/ docs/`
Expected: two elision paragraphs. Both assert "no counter-multiplying card is in `data/cards/`" / "no such card is in `data/cards/`", both false since `doubling-season.json` and `vorinclex-monstrous-raider.json` landed, and both now describe behaviour that exists. DELETE both paragraphs -- an elision comment dies in the commit that closes its issue.

Rewrite the surrounding sentences so what remains is true: `Keyword.hs`'s "NOT a minted EntryRewrite row" paragraph is now backwards and must say the opposite, with rule 702.150a's "that many minus two" explained as a rewrite of the PENDING count rather than a placement of its own.

- [ ] **Step 2: Fix the four funnel comments**

Each says the counters go through `putOwnCountersIn` / "CR 122.6's funnel, so CR 614.16 applies". That road is gone for entry counters. Each should now say the counters are accumulated pending and that CR 614.16 reaches them in the ENTRY pool, where CR 616.1 also orders them against compleated.

- [ ] **Step 3: File the multi-kind elision issue**

```bash
gh issue create --label elision --label area:replacement --label expires:card-driven \
  --title "A counter multiplier scales every kind an entering permanent gets at once" \
  --body "..."
```

Body: a dated plain-language summary first (the `issue-body-convention`), then the detail -- before the hoist each kind opened its own CR 616.1 loop so a row applied once per kind; now one application scales every pending kind the row matches, and CR 614.5 spends it once. Indistinguishable while no card in the pool enters with two kinds one pattern matches. Expiry trigger: such a card entering the pool.

Then cite it at the `CounterR`/`WouldEnter` arm of `Event.apply` as `(#N)`.

- [ ] **Step 4: Move census #877's row**

Line 43 of #877's body reads `702.150 Compleated (Tamiyo, Compleated Sage; CR 616.1's ordering against a counter multiplier is #1996)`. Drop the parenthetical's second clause, leaving `702.150 Compleated (Tamiyo, Compleated Sage)`.

- [ ] **Step 5: Check for dependents**

```bash
gh api repos/tfausak/pawl/issues/1996/dependencies/blocking
```

Say in the PR body which issues this unblocked, if any.

- [ ] **Step 6: Self-review, then commit**

Re-check every CR citation added by this branch against `docs/rules.txt` -- 306.5b, 614.1c, 614.5, 614.12, 614.16, 616.1a-g, 616.2, 702.150a, 122.6, 122.6a, 714.3c. Re-read every comment the branch touched. Both reliably catch real defects.

```bash
git add -A
hooky fix
git add -u
git commit -m "Retire the compleated ordering elisions and move the census row"
```

---

## PR body checklist

- What changed and why, with `Closes #1996`.
- The CR citations behind it.
- The design calls: hoist over push-down (with the `CounterCause` argument), `GameState` over `Object` for the pending map (with the positional-record argument), the multi-kind reading.
- Verification: suite count before -> after, the proving test named, and for each mutation the assertion it reddened, named.
- An explicit "no" on whether the rules core cases on an effect's identity -- the new `Keyword.Compleated` and `EntryRewrite.Compleated` arms are rule 702.150a, which CLAUDE.md's keyword exception covers.
- What was deferred: the multi-kind elision issue from Task 5 Step 3.
- Which `-Werror`-invisible sites were read: `Event.eventBindings`' fallthrough, `Filter.boundSlots`, `ZoneTriggerSpec.everyTriggerCondition` and `representativeEvents`, `CardSpec`'s filter and keyword traversals, and `Codec.EntryRewrite`'s `Arm.tagged` fallthrough -- with why each is right as it stands.
