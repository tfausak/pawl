# Finishing CR 500.8's Extra Phases — Implementation Plan

> **For agentic workers:** Steps use checkbox (`- [ ]`) syntax for tracking. Work
> the tasks strictly in order. Never edit this plan, weaken an assertion, or
> delete a test to make a check pass — if the plan looks wrong, stop and say so.

**Goal:** Close #393, #394, #395 and #52 on one branch, so CR 500.8's added
phases are modelled at the width the rule actually has, and CR 612's word swap
reaches a `Filter` carried by an effect.

**Architecture:** `Pawl.Turn` gains one function that answers "where does this
phase end" (`thisPhase`), and both the drop side and the splice side use it.
`Effect.AddCombatAndMainPhase` becomes `Effect.AddPhases [ExtraPhase]`. A history
atom (`Filter.AttackedThisTurn`) and a trigger frequency
(`TriggerCondition.SelfAttacks TriggerFrequency`) both read the turn-scoped
`GameState.events` log. Delayed triggers learn CR 603.7b's stated duration. Four
cards prove it: Relentless Assault, Aurelia the Warleader, Full Throttle, Boil.

**Tech Stack:** Haskell, GHC 9.14.1 from the Nix flake. `cabal build all`,
`cabal test` (tasty), `hooky fix` / `hooky run`.

**Spec:** `docs/superpowers/specs/2026-07-29-cr-500-8-extra-phases-design.md`.
Read it before Task 1; it carries the CR argument each task implements.

## Global Constraints

- **Branch:** `393-finish-cr-500-8-extra-phases`. Never commit to `main`.
- **One build at a time.** `jobs: $ncpus` already saturates the machine. Never
  run two `cabal` invocations concurrently.
- **Every rules claim is checked against `docs/rules.txt`**, by rule number,
  never from memory. Cite the rule number in the code comment.
- **Cards are verified live against Scryfall**, never from the dumps in
  `_scratch/` and never from memory.
- **No unchecked numeric conversions.** `fromIntegral`, `fromInteger`,
  `realToFrac`, `toEnum` are banned by `.hlint.yaml`; go through
  `Pawl.Extra.Int` / `Integer` / `Natural`.
- **Constructors take a `Mk` prefix and never pun the type name.**
- **One type per module** under `Pawl.Type.<TypeName>`.
- **`hooky` acts on staged files only.** `git add` first, then `hooky fix`, then
  `git add` again, then `hooky run`.
- **Adding or deleting a module needs a direct `cabal-gild pawl.cabal`** —
  `hooky fix` skips it.
- **Never write an expiry into a code comment.** A comment states only what is
  not implemented, plus `(#N)`.
- **The rules core must not case on an effect's identity.** New `case effect of`
  work belongs in `Pawl.Resolve` only.
- **Commit after each task.** The branch's internal history is working state;
  merges are squashed.

---

### Task 1: `Turn.thisPhase` — one notion of where a phase ends

**Files:**
- Modify: `source/library/Pawl/Turn.hs` (`dropSkippedCombatSteps`, ~line 76–104)
- Test: `source/test-suite/Pawl/TurnSpec.hs`

**Interfaces:**
- Produces: `Turn.thisPhase :: Phase -> Seq Phase -> (Seq Phase, Seq Phase)` and
  `Turn.lastStepOf :: Phase -> Maybe Phase`. Task 2 consumes `thisPhase`.
- Consumes: nothing.

Behaviour of `dropSkippedCombatSteps` is unchanged on every reachable input.
This task is a refactor plus new unit tests; no card, no gameplay change.

- [ ] **Step 1: Write the failing tests**

Add to `TurnSpec.hs`, in the group that already holds the
`dropSkippedCombatSteps` cases:

```haskell
HU.testCase "CR 511.3 thisPhase inside a combat phase ends at ITS end of combat" $
  let remaining =
        Seq.fromList
          [ Phase.Combat CombatStep.DeclareBlockers,
            Phase.Combat CombatStep.CombatDamage,
            Phase.Combat CombatStep.EndOfCombat,
            Phase.Combat CombatStep.BeginningOfCombat,
            Phase.Combat CombatStep.DeclareAttackers,
            Phase.Combat CombatStep.EndOfCombat,
            Phase.PostcombatMain
          ]
      expected =
        ( Seq.fromList
            [ Phase.Combat CombatStep.DeclareBlockers,
              Phase.Combat CombatStep.CombatDamage,
              Phase.Combat CombatStep.EndOfCombat
            ],
          Seq.fromList
            [ Phase.Combat CombatStep.BeginningOfCombat,
              Phase.Combat CombatStep.DeclareAttackers,
              Phase.Combat CombatStep.EndOfCombat,
              Phase.PostcombatMain
            ]
        )
   in HU.assertEqual "split at the FIRST end of combat, inclusive" expected (Turn.thisPhase (Phase.Combat CombatStep.DeclareAttackers) remaining),
HU.testCase "CR 505.2 thisPhase in a main phase has no steps of its own" $
  let remaining = Seq.fromList [Phase.Combat CombatStep.BeginningOfCombat, Phase.PostcombatMain]
   in HU.assertEqual "empty prefix" (Seq.empty, remaining) (Turn.thisPhase Phase.PrecombatMain remaining),
HU.testCase "thisPhase yields an empty prefix when this phase's final step is gone" $
  let remaining = Seq.fromList [Phase.PostcombatMain, Phase.Ending EndingStep.EndStep]
   in HU.assertEqual "empty prefix" (Seq.empty, remaining) (Turn.thisPhase (Phase.Combat CombatStep.EndOfCombat) remaining),
```

Note the first test's fixture holds **two** combat phases; splitting at the first
end of combat is what leaves the second one whole.

- [ ] **Step 2: Run the tests and watch them fail**

Run: `cabal test 2>&1 | tail -30`
Expected: FAIL — `Variable not in scope: Turn.thisPhase`.

- [ ] **Step 3: Write `lastStepOf` and `thisPhase`**

Add to `Turn.hs`, above `dropSkippedCombatSteps`:

```haskell
-- CR 500: the final step of the phase this one belongs to -- the step whose end
-- ends the phase. CR 511.3 names combat's ("After the end of combat step ends,
-- the combat phase is over"); CR 501.1 lists the beginning phase's three steps,
-- of which draw is the last; CR 512.1 lists the ending phase's two, of which
-- cleanup is the last. A main phase has NO steps at all (CR 505.2), so there is
-- no step whose end ends it -- which is what Nothing says here.
lastStepOf :: Phase -> Maybe Phase
lastStepOf phase = case phase of
  Phase.Beginning _ -> Just (Phase.Beginning BeginningStep.DrawStep)
  Phase.PrecombatMain -> Nothing
  Phase.Combat _ -> Just (Phase.Combat CombatStep.EndOfCombat)
  Phase.PostcombatMain -> Nothing
  Phase.Ending _ -> Just (Phase.Ending EndingStep.Cleanup)

-- Split what is left of the turn into THIS phase's remaining steps and
-- everything after it. The one place that answers where a phase ends, so
-- CR 511.3 is cited once rather than once per caller.
--
-- The final step goes in the PREFIX: it belongs to the phase it ends.
--
-- An absent final step yields an EMPTY prefix, not the whole schedule: if this
-- phase's last step is no longer scheduled then the phase is already over as far
-- as `remaining` shows, and "directly after this phase" (CR 500.8) is the head.
-- Unreachable from either caller -- Combat.skipEmptyCombat runs as the declare
-- attackers step ends, and Resolve's splice runs while the resolving object's
-- phase is current -- so no test can observe the choice; it is written this way
-- because dropping nothing is the safer failure than dropping everything.
thisPhase :: Phase -> Seq Phase -> (Seq Phase, Seq Phase)
thisPhase phase remaining = case lastStepOf phase >>= \s -> Seq.elemIndexL s remaining of
  Nothing -> (Seq.empty, remaining)
  Just i -> Seq.splitAt (i + 1) remaining
```

- [ ] **Step 4: Rewrite `dropSkippedCombatSteps` to use it**

Replace its body, and rewrite its comment. The `Seq.breakl` and the whole
paragraph arguing the "leading run" alternative and the fallback both go; what
stays is why the two steps are dropped and why the positional split is needed.

```haskell
-- CR 508.8 / 500.11: drop the declare blockers and combat damage steps of THE
-- COMBAT PHASE NOW UNDER WAY from what is left of the turn, so it proceeds "as
-- though they didn't exist". Positional, not a filter over the whole schedule:
-- CR 500.8 lets an effect add a second combat phase, and skipping this one says
-- nothing about that one.
--
-- Where this phase ends is Turn.thisPhase's question, not this function's. The
-- end of combat step it splits on is never one of the two steps dropped here, so
-- which side of the split it lands on is unobservable.
--
-- The caller is Combat.skipEmptyCombat, which runs as the declare attackers step
-- ends, so the phase is always a combat phase and its end of combat step is
-- always still scheduled.
dropSkippedCombatSteps :: Phase -> Seq Phase -> Seq Phase
dropSkippedCombatSteps phase remaining =
  let kept p =
        p /= Phase.Combat CombatStep.DeclareBlockers
          && p /= Phase.Combat CombatStep.CombatDamage
      (current, rest) = thisPhase phase remaining
   in Seq.filter kept current <> rest
```

- [ ] **Step 5: Update the caller and the four existing tests**

`source/library/Pawl/Combat.hs:87` passes `GameState.phase gs`:

```haskell
then gs {GameState.remaining = Turn.dropSkippedCombatSteps (GameState.phase gs) (GameState.remaining gs)}
```

The four existing `dropSkippedCombatSteps` cases in `TurnSpec.hs` gain a first
argument of `Phase.Combat CombatStep.DeclareAttackers`. **Their expected values
do not change** — if one does, stop: the refactor was not behaviour-preserving.

Also re-read `Combat.hs`'s `skipEmptyCombat` comment (lines ~75–88) and
`Engine.hs:927`'s comment for prose the rewrite made wrong.

- [ ] **Step 6: Build and test**

Run: `cabal build all 2>&1 | grep -i warn; cabal test 2>&1 | tail -20`
Expected: no warnings; all tests pass, suite count up by 3.

- [ ] **Step 7: Commit**

```bash
git add source/library/Pawl/Turn.hs source/library/Pawl/Combat.hs source/test-suite/Pawl/TurnSpec.hs
hooky fix && git add -u && hooky run
git commit -m "Give Turn one notion of where a phase ends (CR 511.3)"
```

---

### Task 2: `Effect.AddPhases [ExtraPhase]` (#393)

**Files:**
- Create: `source/library/Pawl/Type/ExtraPhase.hs`
- Modify: `source/library/Pawl/Type/Effect.hs` (~line 281–295), `Pawl/Turn.hs`,
  `Pawl/Resolve.hs` (lines 134, 181, 227, 268, 363, 1626–1633), `Pawl/Codec.hs`
  (1533, 1615), `pawl.cabal` (via `cabal-gild`)
- Modify: `data/cards/aggravated-assault.json`
- Test: `source/test-suite/Pawl/TurnSpec.hs`, `source/test-suite/Pawl/CodecSpec.hs`
  (253), `source/test-suite/Pawl/CardSpec.hs` (377)

**Interfaces:**
- Consumes: `Turn.thisPhase` from Task 1.
- Produces: `ExtraPhase.ExtraPhase = ExtraCombat | ExtraMain`;
  `Effect.AddPhases :: [ExtraPhase] -> Effect card`;
  `Turn.splicePhases :: Phase -> [ExtraPhase] -> Seq Phase -> Seq Phase`;
  `Turn.expandExtraPhase :: ExtraPhase -> Seq Phase`. Tasks 5 and 7 build cards
  on `AddPhases`.

- [ ] **Step 1: Write the failing tests**

In `TurnSpec.hs`:

```haskell
HU.testCase "CR 500.8 splicePhases from a MAIN phase goes at the head" $
  let remaining = Seq.fromList [Phase.Combat CombatStep.BeginningOfCombat, Phase.PostcombatMain]
   in HU.assertEqual
        "directly after this phase"
        (Turn.combatAndMainPhase <> remaining)
        (Turn.splicePhases Phase.PrecombatMain [ExtraPhase.ExtraCombat, ExtraPhase.ExtraMain] remaining),
HU.testCase "CR 500.8 splicePhases from INSIDE a combat phase goes after its end of combat" $
  let remaining =
        Seq.fromList
          [ Phase.Combat CombatStep.DeclareBlockers,
            Phase.Combat CombatStep.CombatDamage,
            Phase.Combat CombatStep.EndOfCombat,
            Phase.PostcombatMain
          ]
      expected =
        Seq.fromList
          [ Phase.Combat CombatStep.DeclareBlockers,
            Phase.Combat CombatStep.CombatDamage,
            Phase.Combat CombatStep.EndOfCombat
          ]
          <> Turn.expandExtraPhase ExtraPhase.ExtraCombat
          <> Seq.fromList [Phase.PostcombatMain]
   in HU.assertEqual
        "not inside the current phase"
        expected
        (Turn.splicePhases (Phase.Combat CombatStep.DeclareAttackers) [ExtraPhase.ExtraCombat] remaining),
HU.testCase "CR 500.8 splicePhases inserts a multi-phase list in written order" $
  HU.assertEqual
    "two combat phases, back to back"
    (Turn.expandExtraPhase ExtraPhase.ExtraCombat <> Turn.expandExtraPhase ExtraPhase.ExtraCombat)
    (Turn.splicePhases Phase.PrecombatMain [ExtraPhase.ExtraCombat, ExtraPhase.ExtraCombat] Seq.empty),
```

- [ ] **Step 2: Run and watch them fail**

Run: `cabal test 2>&1 | tail -30`
Expected: FAIL — `ExtraPhase` and `Turn.splicePhases` not in scope.

- [ ] **Step 3: Create `Pawl/Type/ExtraPhase.hs`**

```haskell
module Pawl.Type.ExtraPhase where

-- CR 500.8: one phase an effect adds to a turn. The rule does not fix WHICH
-- phases are added -- "Some effects can add phases to a turn. They do this by
-- adding the phases directly after the specified phase" -- and printed cards
-- vary: Aggravated Assault adds a combat phase and a main phase, Aurelia, the
-- Warleader adds only a combat phase, Full Throttle adds two combat phases.
--
-- A whole phase, never a step: what Pawl.Turn.expandExtraPhase inserts for
-- ExtraCombat is CR 506.1's five steps in order, and for ExtraMain is the single
-- stepless main phase CR 505.2 describes.
data ExtraPhase
  = -- CR 506.1's five steps.
    ExtraCombat
  | -- CR 505.1a: an added main phase is a POSTCOMBAT main phase. "Only the first
    -- main phase of the turn is a precombat main phase ... It is also true of a
    -- turn in which an effect has caused an additional combat phase and an
    -- additional main phase to be created."
    ExtraMain
  deriving (Eq, Ord, Show)
```

Then run `cabal-gild --io pawl.cabal` (a new module; `hooky fix` skips this).

- [ ] **Step 4: Replace `spliceCombatAndMainPhase` with `splicePhases`**

In `Turn.hs`. `combatAndMainPhase` stays — the existing tests name it — and is
redefined in terms of `expandExtraPhase`.

```haskell
-- The steps one added phase expands to. CR 506.1 fixes the combat phase's five
-- and their order; CR 505.2 ("The main phase has no steps") is why a main phase
-- is one element.
expandExtraPhase :: ExtraPhase -> Seq Phase
expandExtraPhase extra = case extra of
  ExtraPhase.ExtraCombat ->
    Seq.fromList
      [ Phase.Combat CombatStep.BeginningOfCombat,
        Phase.Combat CombatStep.DeclareAttackers,
        Phase.Combat CombatStep.DeclareBlockers,
        Phase.Combat CombatStep.CombatDamage,
        Phase.Combat CombatStep.EndOfCombat
      ]
  ExtraPhase.ExtraMain -> Seq.singleton Phase.PostcombatMain

-- CR 500.8: add phases "directly after the specified phase", and the specified
-- phase is always the one the effect resolves in. Where that phase ends is
-- Turn.thisPhase's question -- which is what makes this correct for an effect
-- resolving inside a STEPPED phase, where the head of `remaining` is still this
-- phase's own later steps rather than the next phase.
--
-- The list is inserted as one block, in written order. CR 500.8's "if multiple
-- extra phases are created after the same phase, the most recently created phase
-- will occur first" governs two SEPARATE effects adding phases after the same
-- phase; the phases within one effect's own list are created together and simply
-- run in the order the card writes them (Full Throttle's "two additional combat
-- phases").
splicePhases :: Phase -> [ExtraPhase] -> Seq Phase -> Seq Phase
splicePhases phase extras remaining =
  let (current, rest) = thisPhase phase remaining
   in current <> foldMap expandExtraPhase extras <> rest

-- One whole combat phase followed by one whole main phase -- what Aggravated
-- Assault and Relentless Assault add. Named apart from the splice so a test can
-- say what it expects to be inserted without restating CR 506.1's order.
combatAndMainPhase :: Seq Phase
combatAndMainPhase = foldMap expandExtraPhase [ExtraPhase.ExtraCombat, ExtraPhase.ExtraMain]
```

- [ ] **Step 5: Change the opcode**

In `Effect.hs`, replace `AddCombatAndMainPhase` with `AddPhases [ExtraPhase]`.
Rewrite the comment: it currently argues the nullary shape is right and cites
`#393` for the payload. Both go. Keep the CR 500.8 quote, keep "targetless and
unprompted — CR 500.8 leaves nothing to choose", and point at
`Turn.splicePhases` for what is inserted.

- [ ] **Step 6: Update `Resolve.hs`**

Five classification arms (134, 181, 227, 268, 363) become `Effect.AddPhases _`
with their answers unchanged. The executor at 1626 becomes:

```haskell
  -- CR 500.8: add the phases, directly after the phase this is resolving in.
  -- Turn.splicePhases takes GameState.phase because "directly after this phase"
  -- is not the head of `remaining` when the resolving phase has steps left --
  -- Aurelia, the Warleader's trigger resolves in the declare attackers step,
  -- where this phase's own blockers, damage and end of combat are still ahead.
  Effect.AddPhases extras ->
    State.modify' $ \gs ->
      gs {GameState.remaining = Turn.splicePhases (GameState.phase gs) extras (GameState.remaining gs)}
```

Delete the old comment's CR 307.5 / CR 505.2 argument for why the head is right.
That argument is exactly the assumption this change removes; leaving it would be
prose the rewrite made wrong.

- [ ] **Step 7: Update `Codec.hs`**

Line 1533 goes from `nullary` to a tagged value carrying a list of
`ExtraPhase`; line 1615 decodes it. Follow the encoding an existing
list-carrying effect uses. Add `extraPhaseToJson` / `jsonToExtraPhase` beside
the other small enum codecs.

- [ ] **Step 8: Update the card and the two spec helpers**

`data/cards/aggravated-assault.json`: `{"type": "AddCombatAndMainPhase"}` becomes
`{"type": "AddPhases", "value": [{"type": "ExtraCombat"}, {"type": "ExtraMain"}]}`
(match whatever shape Step 7 chose). `CodecSpec.hs:253` round-trips the new
value. `CardSpec.hs:377`'s exhaustive effect case gains the new arm.

- [ ] **Step 9: Build and test**

Run: `cabal build all 2>&1 | grep -i warn; cabal test 2>&1 | tail -20`
Expected: no warnings; all pass. **The three existing Aggravated Assault
gameplay tests in `TurnSpec.hs` must pass unchanged** — it resolves in a main
phase, where `thisPhase`'s prefix is empty and the splice is still a head-cons.

- [ ] **Step 10: Commit**

```bash
git add -A source/ data/ pawl.cabal
hooky fix && git add -u && hooky run
git commit -m "Give CR 500.8's added phases a payload (#393)"
```

---

### Task 3: `Filter.AttackedThisTurn` + Relentless Assault (#394)

**Files:**
- Modify: `source/library/Pawl/Type/Filter.hs`, `Pawl/Filter.hs` (`View`,
  `playerView`, `matches`), `Pawl/Projection.hs` (the two `View` builders and
  `filterReads`), `Pawl/Codec.hs`
- Create: `data/cards/relentless-assault.json`
- Test: `source/test-suite/Pawl/FilterSpec.hs`, `source/test-suite/Pawl/TurnSpec.hs`,
  `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Consumes: `Effect.AddPhases` from Task 2.
- Produces: `Filter.AttackedThisTurn`; `Filter.View.attackedThisTurn :: Bool`.
  Task 7 reuses the atom for Full Throttle.

- [ ] **Step 1: Write the failing gameplay test**

In `TurnSpec.hs`'s `extraPhaseTests`. The assertion that distinguishes this atom
from "creatures you control" is the **non-attacker left tapped**:

```haskell
HU.testCase "CR 500.8 whole card: Relentless Assault untaps only what ATTACKED" $ do
  -- Two creatures, both tapped: one attacked this turn, one was tapped by
  -- something else. CR 511.3 has already wiped Combat.attackers by the
  -- postcombat main phase, so Filter.IsAttacking would untap neither -- the
  -- turn-scoped event log is what still knows.
  ...
  HU.assertEqual "the attacker untapped" (Just TapState.Untapped) (fmap Object.tapped (Game.lookupObject attacker after))
  HU.assertEqual "the non-attacker stayed tapped" (Just TapState.Tapped) (fmap Object.tapped (Game.lookupObject bystander after))
  HU.assertEqual
    "and the phases went in after this main phase"
    (Turn.combatAndMainPhase <> afterCombat)
    (GameState.remaining after)
```

Build the board with `S.attackTo S.bob` through a real combat phase so the
`AttackerDeclared` events are genuine, then advance to the postcombat main phase
and cast Relentless Assault there. Model the setup on `assaultBoard` /
`runTurn`, already in this file.

- [ ] **Step 2: Run it and watch it fail**

Run: `cabal test 2>&1 | tail -30`
Expected: FAIL — no "Relentless Assault" printing in the registry.

- [ ] **Step 3: Add the atom**

In `Type/Filter.hs`, after `IsAttacking`:

```haskell
  | -- CR 608.2i: the candidate was declared as an attacker earlier THIS TURN --
    -- Relentless Assault's "all creatures that attacked this turn". A look-back
    -- read of the turn-scoped GameEvent log (CR 608.2i: "Some effects look back
    -- in time and require information about previous game states and actions
    -- rather than considering the current game state"), never a stamp on the
    -- object.
    --
    -- NOT a synonym for IsAttacking, and not expressible in terms of it:
    -- Combat.attackers is wiped by Combat.clearCombat as the end of combat step
    -- ends (CR 511.3), so by the time a second main phase resolves this spell the
    -- first combat's attackers are gone from the live record. The event log is
    -- the right footing because it is cleared at turn handoff, which is exactly
    -- "this turn".
    --
    -- DECLARED, like TriggerCondition.SelfAttacks and for the same reason: CR
    -- 508.4 says a creature put onto the battlefield attacking "never attacked".
    --
    -- The fourth atom, after IsAttacking, IsAttachedToCreature and IsToken,
    -- reading something CR 109.3 leaves off the characteristic list. Their
    -- defence covers this one: what happened earlier this turn is a RULES record
    -- the closed half owns outright (CR 608.2i, Pawl.Type.GameEvent), so reading
    -- it is the same kind of act as reading a card type, and casing on an
    -- EFFECT's identity is still what the invariant forbids and still not what
    -- this does.
    AttackedThisTurn
```

- [ ] **Step 4: Add the `View` field and the matcher arm**

In `Pawl/Filter.hs`, a field after `attacking`. **Lazy**, like
`attachedToCreature`, with the reason stated: filling it folds the whole turn's
event log, and nothing forces it unless a `Filter` actually contains the atom.
`playerView` sets it `False` (CR 506.3: only a creature can attack, and a player
is not one). `matches` gets `Filter.AttackedThisTurn -> attackedThisTurn view`.

- [ ] **Step 5: Fill it in `Projection.hs`**

`viewOfCharacteristics` folds `GameState.events gs` for
`GameEvent.AttackerDeclared oid` naming this object. The printed-card builder
(~line 368) sets `False`, with the comment noting a card off the battlefield was
never on it to attack.

`filterReads` gets `Filter.Type.AttackedThisTurn -> Set.empty`, with the
`IsToken`-strength argument: no `Modification` writes the event log, so no CR 613
layer can move a set selected by this atom.

- [ ] **Step 6: Codec + FilterSpec + CodecSpec**

Encode/decode the nullary atom; round-trip it in `CodecSpec`. Add a `FilterSpec`
unit case: a `View` with `attackedThisTurn = True` matches, one with `False` does
not, and a `playerView` does not.

- [ ] **Step 7: Add the card**

Verify against Scryfall first:

```bash
curl -sS -H 'User-Agent: pawl-dev/1.0' \
  'https://api.scryfall.com/cards/named?exact=Relentless%20Assault'
```

`{2}{R}{R}` Sorcery. Effects, in written order:
`Untap (EachMatching AttackedThisTurn)`, then
`AddPhases [ExtraCombat, ExtraMain]`.

The filter is bare `AttackedThisTurn` — the card says "all creatures that
attacked this turn", and CR 506.3 already means only a creature can have been
declared an attacker, so an added `HasCardType Creature` would narrow by
something the card does not print. **Do not** add `ControlledBy You`: the card
does not say "you control", and it really does untap an opponent's attackers.

Model the JSON on `data/cards/aggravated-assault.json` for the effect shapes and
on any existing sorcery for the `spell` block.

- [ ] **Step 8: Add the second gameplay test**

```haskell
HU.testCase "CR 511.3 Relentless Assault still finds attackers after clearCombat" $ do
```

Assert the untap works when cast in the postcombat main phase — i.e. after
`Combat.clearCombat` has run. This is the test that would pass for the wrong
reason if the atom were implemented as `IsAttacking`.

- [ ] **Step 9: Build, test, commit**

```bash
cabal build all 2>&1 | grep -i warn; cabal test 2>&1 | tail -20
git add -A source/ data/
hooky fix && git add -u && hooky run
git commit -m "Add Filter.AttackedThisTurn, with Relentless Assault (#394)"
```

---

### Task 4: `SelfAttacks` gains a frequency

**Files:**
- Create: `source/library/Pawl/Type/TriggerFrequency.hs`
- Modify: `source/library/Pawl/Type/TriggerCondition.hs` (~line 62–77),
  `Pawl/Event.hs` (`matchesTrigger`, the `SelfAttacks` arm ~line 664, plus the
  `AttackerDeclared` arms of the sibling conditions), `Pawl/Codec.hs`,
  `pawl.cabal` (via `cabal-gild`)
- Modify: `data/cards/hanweir-garrison.json`
- Test: `source/test-suite/Pawl/EventSpec.hs` or `TriggerSpec.hs`,
  `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Produces: `TriggerFrequency.TriggerFrequency = EveryTime | FirstTimeEachTurn`;
  `TriggerCondition.SelfAttacks :: TriggerFrequency -> TriggerCondition`. Task 5
  builds Aurelia on it.

- [ ] **Step 1: Write the failing test**

In the spec that already covers `matchesTrigger`:

```haskell
HU.testCase "CR 508.3a SelfAttacks FirstTimeEachTurn matches the first declaration only" $
```

Build a `GameState` whose `events` hold one `AttackerDeclared bearer` (use
`S.withEvents`), assert the condition matches; then a state whose events hold
**two** for the same bearer, assert it does not. Assert `EveryTime` matches in
both. Add a third case: two `AttackerDeclared` events for *different* bearers
still counts as the first for each.

- [ ] **Step 2: Run and watch it fail**

Run: `cabal test 2>&1 | tail -30`
Expected: FAIL — `SelfAttacks` is nullary, so the payload does not typecheck.

- [ ] **Step 3: Create `Pawl/Type/TriggerFrequency.hs`**

```haskell
module Pawl.Type.TriggerFrequency where

-- How often a trigger condition may match within one turn.
--
-- There is NO comprehensive rule for "for the first time each turn". The nearest,
-- CR 603.2h, is about "Do this only once each turn" -- a qualifier on an
-- INSTRUCTION, gated on what the source's controller has done -- which is a
-- different thing from a qualifier on the trigger event. Aurelia, the
-- Warleader's phrase is plain card text narrowing a CR 508.3a trigger event, and
-- this type says so rather than manufacturing a citation.
--
-- Load-bearing, not decoration: Aurelia adds a combat phase when she attacks, so
-- without the narrowing she attacks in the phase she added, adds another, and
-- the turn never ends.
data TriggerFrequency
  = -- The condition matches every time its trigger event occurs (CR 603.2c).
    EveryTime
  | -- The condition matches only the first occurrence in a turn.
    FirstTimeEachTurn
  deriving (Eq, Ord, Show)
```

Run `cabal-gild --io pawl.cabal`.

- [ ] **Step 4: Add the payload and the match**

`TriggerCondition.SelfAttacks TriggerFrequency`, with the existing comment kept
and a paragraph added for the frequency. In `Event.matchesTrigger`, the
`SelfAttacks` arm becomes:

```haskell
  TriggerCondition.SelfAttacks frequency -> case event of
    GameEvent.AttackerDeclared oid ->
      oid == bearer && case frequency of
        TriggerFrequency.EveryTime -> True
        -- The declaration being matched is already in the log when the scan
        -- runs, so "the first time" is "this is the only one so far". The log is
        -- cleared at turn handoff, which is what makes it "each turn".
        --
        -- CR 400.7 mints a new object on a zone change, so a creature that left
        -- and returned is a different id and attacks for the first time again --
        -- which is correct.
        TriggerFrequency.FirstTimeEachTurn -> declarationsOf bearer gs <= 1
    _ -> False
```

with a small local helper counting `AttackerDeclared bearer` in
`GameState.events gs`. Use `Prelude.length` on a filtered list of the `Seq`'s
elements; do **not** reach for a numeric conversion.

- [ ] **Step 5: Codec, card, spec**

Encode the payload; round-trip in `CodecSpec`. `data/cards/hanweir-garrison.json`
gains `"value": {"type": "EveryTime"}` (or whatever shape the codec chose). Its
existing gameplay tests must pass unchanged.

- [ ] **Step 6: Build, test, commit**

```bash
cabal build all 2>&1 | grep -i warn; cabal test 2>&1 | tail -20
git add -A source/ data/ pawl.cabal
hooky fix && git add -u && hooky run
git commit -m "Let SelfAttacks say how often it matches (CR 508.3a)"
```

---

### Task 5: Aurelia, the Warleader — the proof for Tasks 1, 2 and 4

**Files:**
- Modify: `source/library/Pawl/Type/Subtype.hs` and its edit-site set (the
  compiler enumerates them; `Pawl.Mana.subtypeMana` is the easy one to miss)
- Create: `data/cards/aurelia-the-warleader.json`
- Test: `source/test-suite/Pawl/TurnSpec.hs`

**Interfaces:**
- Consumes: `Turn.thisPhase` (Task 1), `Effect.AddPhases` (Task 2),
  `TriggerCondition.SelfAttacks FirstTimeEachTurn` (Task 4).

- [ ] **Step 1: Write the failing tests**

This is the falsifier for Task 1. Assert on the **whole remaining schedule**,
because that is the shape a head-cons gets wrong:

```haskell
HU.testCase "CR 500.8 Aurelia's added combat phase goes AFTER this one, not inside it" $ do
  -- Aurelia's trigger resolves in the declare attackers step, where this combat
  -- phase's own declare blockers, combat damage and end of combat steps are
  -- still in GameState.remaining. A head-cons would splice the new phase in
  -- front of them -- i.e. inside the phase it is supposed to follow.
  ...
  HU.assertEqual
    "this phase's own steps still come first"
    ( Seq.fromList
        [ Phase.Combat CombatStep.DeclareBlockers,
          Phase.Combat CombatStep.CombatDamage,
          Phase.Combat CombatStep.EndOfCombat
        ]
        <> Turn.expandExtraPhase ExtraPhase.ExtraCombat
        <> Seq.fromList [Phase.PostcombatMain, Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup]
    )
    (GameState.remaining after)
```

And the termination test:

```haskell
HU.testCase "Aurelia's second attack adds no third combat phase" $ do
```

Run the whole turn with `runTurn (S.attackTo S.bob)` and assert the phase list
holds exactly **two** combat phases, and that `bob` took damage in each.

- [ ] **Step 2: Run and watch them fail**

Run: `cabal test 2>&1 | tail -30`
Expected: FAIL — no "Aurelia, the Warleader" printing.

- [ ] **Step 3: Add the `Angel` subtype**

Add the constructor in alphabetical position in `Type/Subtype.hs`, then
`cabal build all` and let the exhaustive-case warnings enumerate every site that
needs an arm.

- [ ] **Step 4: Add the card**

Verify against Scryfall first:

```bash
curl -sS -H 'User-Agent: pawl-dev/1.0' \
  'https://api.scryfall.com/cards/named?exact=Aurelia,%20the%20Warleader'
```

`{2}{R}{R}{W}{W}`, Legendary Creature — Angel, 3/4, keywords Flying, Vigilance,
Haste. One triggered ability: condition
`SelfAttacks FirstTimeEachTurn`; effects, in written order,
`Untap (EachMatching (And [HasCardType Creature, ControlledBy You]))` then
`AddPhases [ExtraCombat]`.

Note the difference from Relentless Assault: Aurelia prints "all creatures **you
control**", so `ControlledBy You` belongs here and did not belong there.

- [ ] **Step 5: Build, test, commit**

```bash
cabal build all 2>&1 | grep -i warn; cabal test 2>&1 | tail -20
git add -A source/ data/
hooky fix && git add -u && hooky run
git commit -m "Add Aurelia, the Warleader, splicing a phase from inside combat (#393)"
```

---

### Task 6: Stated-duration delayed triggers (#52)

**Files:**
- Modify: `source/library/Pawl/Type/DelayedTrigger.hs`,
  `Pawl/Type/Effect.hs` (`ArmDelayedTrigger`, ~line 307),
  `Pawl/Resolve.hs` (1280–1300, plus the five classification arms),
  `Pawl/Event.hs` (`delayedPending`, ~1214–1226),
  `Pawl/Expiry.hs` (`dropAtCleanup` 53, `sweepConditional` 87, `dropAtTurnOf` 133),
  `Pawl/Codec.hs`, `Pawl/Departure.hs:136` (comment)
- Modify: `data/cards/tidal-wave.json`
- Test: `source/test-suite/Pawl/EventSpec.hs`, `source/test-suite/Pawl/ExpirySpec.hs`,
  `source/test-suite/Pawl/CodecSpec.hs`, `source/test-suite/Pawl/CardSpec.hs`

**Interfaces:**
- Produces: `Effect.ArmDelayedTrigger :: AbilityName -> Maybe Duration -> Effect card`;
  `DelayedTrigger.expiry :: Maybe Expiry`. Task 7 builds Full Throttle on it.

- [ ] **Step 1: Write the failing tests**

```haskell
HU.testCase "CR 603.7b a delayed trigger with a stated duration stays armed after firing" $
```

Arm one entry with `Just Expiry.AtCleanup`, run `Event.delayedPending` with a
matching event, and assert it appears in the fired list **and** in the surviving
store. Then the same with `expiry = Nothing` and assert it is evicted —
that one pins CR 603.7b's default and must not regress.

```haskell
HU.testCase "CR 514.2 a stated-duration delayed trigger is gone after cleanup" $
```

Assert `Expiry.dropAtCleanup` empties a store holding one `AtCleanup` entry and
leaves a `Never` entry alone.

- [ ] **Step 2: Run and watch them fail**

Run: `cabal test 2>&1 | tail -30`
Expected: FAIL — `DelayedTrigger` has no `expiry` field.

- [ ] **Step 3: Add the field and the payload**

`DelayedTrigger` gains `expiry :: Maybe Expiry`. Rewrite its haddock: the
sentence "A STATED-DURATION delayed ability ('this turn') would fire repeatedly
instead; stated durations are not modelled (#52)" is exactly what this task
removes, and must not survive as stale prose.

`Effect.ArmDelayedTrigger AbilityName (Maybe Duration)`:

```haskell
    -- The Duration is CR 603.7b's "stated duration, such as 'this turn'".
    -- Nothing is that rule's default -- "A delayed triggered ability will
    -- trigger only once -- the next time its trigger event occurs -- unless it
    -- has a stated duration" -- and it is Nothing rather than a Duration arm
    -- meaning "once", because once-ness is not a duration: the rule words it as
    -- the ABSENCE of one.
    ArmDelayedTrigger AbilityName (Maybe Duration)
```

- [ ] **Step 4: Arm the expiry in `Resolve.hs`**

The arming site builds `expiry` with `Expiry.arm controller source d` for a
`Just d`, and `Nothing` for a `Nothing`. Note that `Expiry.arm` itself returns
`Maybe Expiry` for CR 611.2b's never-starting duration — so a `Just d` whose
duration never starts arms nothing at all, and the entry is stored with
`expiry = Nothing`, i.e. as CR 603.7b's one-shot. Write that reasoning at the
site; it is the one place the two `Maybe`s meet and a reader will ask.

- [ ] **Step 5: Keep the survivors in `Event.delayedPending`**

```haskell
   in ( fmap pend (Foldable.toList (Seq.filter fires store)),
        Seq.filter (\entry -> not (fires entry) || Maybe.isJust (DelayedTrigger.expiry entry)) store
      )
```

Rewrite the function's haddock: its opening sentence says each entry that fires
is removed, which is now only half true.

- [ ] **Step 6: Sweep the new store in all three `Expiry` functions**

`dropAtCleanup`, `sweepConditional` and `dropAtTurnOf` each gain
`GameState.delayedTriggers`. An entry with `expiry = Nothing` always survives a
sweep — it is not on a duration, and CR 603.7b's one shot is spent by firing, not
by time. All three, not just `dropAtCleanup`: leaving two stores unswept is a
silent leak waiting for the next card. `dropAtCleanup`'s comment says "One sweep
over three carriers"; that count is now four.

- [ ] **Step 7: Codec, card, spec helpers**

Encode the new payload; round-trip it. `data/cards/tidal-wave.json` gains an
explicit null duration (or the codec's chosen absent-field shape) — its existing
gameplay tests must pass unchanged, which is what pins CR 603.7b's default.
`CardSpec.hs`'s exhaustive effect case gains the new shape.

- [ ] **Step 8: Build, test, commit**

```bash
cabal build all 2>&1 | grep -i warn; cabal test 2>&1 | tail -20
git add -A source/ data/
hooky fix && git add -u && hooky run
git commit -m "Let a delayed trigger carry CR 603.7b's stated duration (#52)"
```

---

### Task 7: Full Throttle — the two-phase payload and the repeating trigger

**Files:**
- Create: `data/cards/full-throttle.json`
- Test: `source/test-suite/Pawl/TurnSpec.hs`

**Interfaces:**
- Consumes: `Effect.AddPhases` (Task 2), `Filter.AttackedThisTurn` (Task 3),
  `Effect.ArmDelayedTrigger _ (Just UntilEndOfTurn)` (Task 6).

- [ ] **Step 1: Write the failing tests**

```haskell
HU.testCase "CR 500.8 whole card: Full Throttle adds two combat phases and no main phase" $
HU.testCase "CR 603.7b whole card: Full Throttle's delayed trigger fires at BOTH added combats" $
```

The second is the one that fails today for a rules reason rather than a missing
card. Assert on tap state at each beginning of combat: a creature that attacked
in the first added combat is untapped again in the second.

- [ ] **Step 2: Run and watch them fail**

Run: `cabal test 2>&1 | tail -30`
Expected: FAIL — no "Full Throttle" printing.

- [ ] **Step 3: Add the card**

Verify against Scryfall first:

```bash
curl -sS -H 'User-Agent: pawl-dev/1.0' \
  'https://api.scryfall.com/cards/named?exact=Full%20Throttle'
```

`{4}{R}{R}` Sorcery. Two clauses:

1. "After this main phase, there are two additional combat phases." →
   `AddPhases [ExtraCombat, ExtraCombat]`.
2. "At the beginning of each combat this turn, untap all creatures that attacked
   this turn." → `ArmDelayedTrigger "eachCombat" (Just UntilEndOfTurn)`, with a
   `delayedAbilities` entry named `eachCombat` whose condition is
   `StepBegins (Combat BeginningOfCombat) EachTurn` and whose effect is
   `Untap (EachMatching AttackedThisTurn)`.

`EachTurn` rather than `ControllersTurn` is right and the turn bound comes from
the duration, not the scope — `TurnScope`'s own haddock already says a delayed
ability's once-ness comes from the store and never from the scope, and CR 514.2
is what ends "this turn". Write that decomposition into the card's test comment.

- [ ] **Step 4: Build, test, commit**

```bash
cabal build all 2>&1 | grep -i warn; cabal test 2>&1 | tail -20
git add -A data/ source/
hooky fix && git add -u && hooky run
git commit -m "Add Full Throttle, two added combat phases and a repeating trigger (#393, #52)"
```

---

### Task 8: `Filter.rewrite` + Boil (#395)

**Files:**
- Modify: `source/library/Pawl/Filter.hs`, `Pawl/Resolve.hs` (`rewriteEffect`,
  320–380). There is **no** `Pawl/ObjectRef.hs` logic module — `objectRefObjects`
  lives in `Resolve.hs:769`, so `rewriteObjectRef` goes there too, beside it.
- Create: `data/cards/boil.json`
- Test: `source/test-suite/Pawl/FilterSpec.hs`, `source/test-suite/Pawl/ResolveSpec.hs`

**Interfaces:**
- Produces: `Filter.rewrite :: [(Subtype, Subtype)] -> Filter -> Filter`.

- [ ] **Step 1: Write the failing gameplay test**

```haskell
HU.testCase "CR 612.1 Magical Hack on Boil moves which lands it destroys" $ do
  -- Magical Hack names Island -> Forest on Boil while Boil is on the stack. CR
  -- 612.1 rewrites "any words or symbols printed on that object", and the word
  -- is inside Boil's effect filter, not inside a Modification.
  ...
  HU.assertEqual "the Forest was destroyed" Nothing (Game.lookupObject forest after)
  HU.assertBool "the Island survived" (Maybe.isJust (Game.lookupObject island after))
```

Plus a `FilterSpec` unit case: `Filter.rewrite [(Island, Forest)]` maps
`HasSubtype Island` to `HasSubtype Forest`, recurses through `And`/`Or`/`Not`,
and leaves every other atom alone.

- [ ] **Step 2: Run and watch them fail**

Run: `cabal test 2>&1 | tail -30`
Expected: FAIL — no "Boil" printing; `Filter.rewrite` not in scope.

- [ ] **Step 3: Write `Filter.rewrite` and `ObjectRef.rewrite`**

```haskell
-- CR 612.1: swap basic-land-type words wherever they appear in a Filter. The
-- shape Projection.rewriteModification already has, for the type this module
-- owns -- so Resolve threads one call per Filter-carrying effect arm rather than
-- learning what is inside each one.
--
-- HasSubtype is the only atom that can carry a basic land type; every other atom
-- names a card type, a colour, a relation or a status, none of which CR 612's
-- word swap reaches.
rewrite :: [(Subtype.Subtype, Subtype.Subtype)] -> Filter.Filter -> Filter.Filter
rewrite pairs f = case f of
  Filter.HasSubtype s -> Filter.HasSubtype (Maybe.fromMaybe s (List.lookup s pairs))
  Filter.And fs -> Filter.And (fmap (rewrite pairs) fs)
  Filter.Or fs -> Filter.Or (fmap (rewrite pairs) fs)
  Filter.Not g -> Filter.Not (rewrite pairs g)
  _ -> f
```

Write the remaining atoms out explicitly rather than using `_ -> f` if the
module's style is exhaustive matching — check the neighbours first; `matches` and
`filterReads` are both exhaustive, so this should be too, and a future atom that
can carry a land type then fails to compile instead of silently going unrewritten.

`rewriteObjectRef` goes in `Resolve.hs` beside `objectRefObjects`: it leaves
`InSlot` alone (a slot names a chosen object, not a word) and rewrites
`EachMatching`'s filter.

- [ ] **Step 4: Thread it through `rewriteEffect`**

The arms that carry a `Filter`, per #395: `Search`, `Destroy`,
`PlayerSacrifices`, `AttachTarget`, `Untap`. Replace the module-level comment
that currently says a `Filter` carried by an effect is NOT rewritten — it is now
false, and it names `#395`, which this closes.

Keep the carve-outs that stay true: a token's or emblem's embedded card is still
not reached (spec section 8), and a `Filter` in a static ability or trigger
condition is out of scope here.

- [ ] **Step 5: Add the card**

Verify against Scryfall first:

```bash
curl -sS -H 'User-Agent: pawl-dev/1.0' 'https://api.scryfall.com/cards/named?exact=Boil'
```

`{3}{R}` Instant, "Destroy all Islands." →
`Destroy (EachMatching (HasSubtype Island)) Regenerable`. Bare `HasSubtype
Island`, with no added `HasCardType Land`: the card says "all Islands", and CR
205.3i is what makes that a land type in the first place.

- [ ] **Step 6: Build, test, commit**

```bash
cabal build all 2>&1 | grep -i warn; cabal test 2>&1 | tail -20
git add -A source/ data/
hooky fix && git add -u && hooky run
git commit -m "Reach a Filter carried by an effect with CR 612's rewrite (#395)"
```

---

### Task 9: Self-review and PR

- [ ] **Step 1: Definitive warning check**

```bash
cabal clean && cabal build all 2>&1 | grep -iE "warn|error"
```

Incremental builds hide warnings from unchanged modules, so this must be a clean
build. Expected: no output.

- [ ] **Step 2: Full suite, with the count**

```bash
cabal test 2>&1 | tail -5
```

Record the before → after count for the PR body. Before this branch: run
`git stash` is **not** needed — take the count from `main`'s last CI or from
`955d476`'s PR body (1607).

- [ ] **Step 3: Re-check every CR citation added on this branch**

```bash
git diff main --unified=0 | grep -oE "CR [0-9]+\.[0-9]+[a-z]?" | sort -u
```

Grep each one out of `docs/rules.txt` and confirm it says what the comment claims.
This reliably catches real defects here. Fix silently — only the end state matters.

- [ ] **Step 4: Re-read every comment the branch touched**

The other reliable defect source: prose the rewrite made wrong. Specifically
re-read `Turn.dropSkippedCombatSteps`, `Combat.skipEmptyCombat`, `Engine.hs:927`,
`Resolve.rewriteEffect`'s header, `Resolve`'s `AddPhases` arm,
`DelayedTrigger`'s haddock, `Event.delayedPending`'s haddock,
`Expiry.dropAtCleanup`'s "three carriers", `Departure.hs:136`, and
`Effect.AddPhases`' and `ArmDelayedTrigger`'s comments.

- [ ] **Step 5: Check for stale issue references**

```bash
git diff main | grep -nE "#(393|394|395|52)"
```

Every in-code comment citing a closed issue must die in this branch. Comments
citing `#399`, `#400`, `#377` are fine — those stay open.

- [ ] **Step 6: Confirm no `priority-*` label was touched, and open the PR**

Open as a **draft**, then mark ready once the self-review's findings are pushed
and the suite is green. The body must carry: what changed and why, with
`Closes #393`, `Closes #394`, `Closes #395`, `Closes #52` as **bare text, not in
backticks** — backticks silently break the link; the CR citations, each checked
against `rules.txt`; the design calls and the alternatives rejected (Full
Throttle vs Aurelia for #393; Savage Beating deferred to #377/#399/#400);
verification (clean build, `hooky run`, suite count before → after, the proving
tests and their red states); an explicit **no** on whether the rules core cases
on an effect's identity; and what was deferred.

Then report the PR and **stop**. Do not wait for CI. Do not start the next unit.

---

## Self-review of this plan

**Spec coverage.** §A → Task 1. §B → Task 2. §C → Task 3. §D → Task 4. §E →
Task 6. §F → Task 8. Cards: Relentless Assault → Task 3, Aurelia → Task 5, Full
Throttle → Task 7, Boil → Task 8. Every spec test is claimed by a task.

**Type consistency.** `Turn.thisPhase` (Tasks 1, 2), `Turn.expandExtraPhase`
(Tasks 2, 5), `Turn.splicePhases` (Tasks 2, 5, 7), `ExtraPhase.ExtraCombat` /
`ExtraMain` (Tasks 2, 5, 7), `Filter.AttackedThisTurn` (Tasks 3, 7),
`TriggerFrequency.FirstTimeEachTurn` (Tasks 4, 5), `DelayedTrigger.expiry`
(Tasks 6, 7), `Filter.rewrite` (Task 8) — each named identically at every site.

**Known softness.** Two places are described rather than coded, because the
existing convention decides them and guessing it here would be worse than reading
it there: the exact JSON tagging for the new codec arms (Task 2 Step 7, Task 4
Step 5, Task 6 Step 7 — follow the neighbouring effect's shape), and the exact
board-setup helpers for the new gameplay tests (Tasks 3, 5, 7, 8 — model on
`assaultBoard` and `runTurn`, already in `TurnSpec.hs`).
