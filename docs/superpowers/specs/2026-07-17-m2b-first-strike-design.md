# M2b first strike — design

Design for milestone **M2b**: first strike and double strike, and the CR 506.1
**conditional turn structure** they force. This is the second slice of
`docs/design.md`'s **M2** ("French vanilla"), split into **M2a** (the keyword
seam), **M2b** (first strike + the two combat damage steps and the CR 508.8 skip)
and **M2c** (deathtouch + trample). See `docs/design.md` §M2b for the brief and
its "Pre-spec notes for M2b" (2026-07-17), which this spec discharges.

M2a made creatures *different from each other*. M2b makes the *turn itself*
different from one game to the next — the first time the shape of a turn depends
on the board — and it does so **still with zero opcodes**. First strike is rule
702.7; double strike is rule 702.4; both are citations, not card text.

This is a types-and-architecture spec, not an implementation plan.

## M2b goal and scope

The same game — 36 Mountain / 16 Goblin Piker / 8 Bird Maiden per player, **still
zero opcodes** — but combat damage stops being a single simultaneous event. Two
things change, and they are one idea:

- **CR 510.4** — if any attacking or blocking creature has first strike or double
  strike, the combat damage step happens **twice**. Double strike deals in
  **both**.
- **CR 508.8** — if no creatures are declared as attackers, the declare blockers
  and combat damage steps are **skipped** (this is the open `git-bug 5f50eec`).

Both are *conditional turn structure*: the sequence of steps in a turn is no
longer fixed. CR 506.1 states them as one sentence, and they want **one
mechanism** — the turn becomes **data**.

Two keywords, chosen because their interaction falsifies the naive
implementation:

| Keyword | CR | Shape |
|---|---|---|
| First strike | 702.7 | splits the combat damage step into two |
| Double strike | 702.4 | deals in **both** steps |

**Double strike is the falsifier.** The obvious implementation of first strike is
"first strikers deal in step one, everyone else deals in step two." Double strike
breaks it by being in *both*: CR 510.4's second step deals damage from "the
remaining attackers and blockers that had neither first strike nor double strike
as the first combat damage step began, **as well as the remaining attackers and
blockers that currently have double strike**." A milestone whose only keyword is
first strike cannot distinguish the right implementation from the wrong one — the
same trap flying-without-reach set in M2a.

**Exit criterion:** a first-striking 2/1 kills a vanilla 2/1 and lives (where two
vanilla 2/1s would trade); a double-striking 2/1 deals four to an unblocked
player; and a turn on which nobody attacks proceeds from declare attackers to end
of combat without granting priority in the two skipped steps — all without any
function asking which card a creature is.

## Conventions

Inherits M0's, M1a's, M1b's and M2a's (`Mk` prefix, boot libraries only,
`Eq`/`Show` everywhere, shape-now-cases-later, every elision names the milestone
that kills it, keyword constructors in CR-number order, test names cite rule
numbers). No additions.

## 1. The turn becomes data

This is the milestone's load-bearing change, and it is the one the design doc
committed to in advance (`docs/design.md` §M2b): *"make the turn data, not a pure
function plus flags."* Everything else in M2b is a consumer of it.

### What is wrong today

Three pieces of `Pawl.Turn` assume the turn is a fixed, unconditional walk:

- `allPhases` (`Turn.hs:9`) is a static list of the twelve steps.
- `next :: Phase -> Maybe Phase` (`Turn.hs:28`) walks that list to find a step's
  successor. Nothing can add to a turn or drop from it.
- `grantsPriority` (`Turn.hs:35`) returns `True` for the declare blockers and
  combat damage steps **unconditionally**.

And `GameState.phase :: Phase` (`GameState.hs:31`) holds a *kind* of step, not an
*occurrence* of one — so "the combat damage step, a second time" is not even
expressible.

The visible consequence is `git-bug 5f50eec`: on every turn where nobody attacks,
`grantsPriority` still returns `True` for declare blockers and combat damage, so
pawl grants two priority rounds that CR 508.8 says to skip. It is unobservable in
game *state* only because casting is sorcery-speed (`Cast.sorcerySpeed`,
`Cast.hs:34`, requires `isMainPhase`), so the extra windows can only be passed —
but the `DecisionLog` already diverges from a faithful engine (§4).

### The shape

Model the **remaining steps of the current turn** as a sequence in `GameState`,
with `allPhases` demoted to the **template** a new turn refills from. `next`'s
static walk is replaced by popping the sequence; skipping and repeating a step
become **edits to the sequence**.

```hs
-- Pawl.Type.GameState gains a field beside `phase`:
--   phase     :: Phase       -- the CURRENT step (unchanged; the occurrence in progress)
--   remaining :: Seq Phase    -- the steps still scheduled this turn, in order
```

`phase` is kept as the current occurrence — deliberately, so the many read sites
(`Action.legalActions` at `Action.hs:30`, `Cast.sorcerySpeed` at `Cast.hs:35`,
`Engine.runStep` at `Engine.hs:224`) do **not** change. `remaining` is the new
part: the *schedule*, which is now data that the engine can drop from and splice
into. `Data.Sequence.Seq` is already a project dependency (`GameState` stores
libraries and hands as `Seq ObjectId`), so this is not a new import surface.

One mechanism then covers everything CR 500 describes:

| CR | Operation | On `remaining` |
|---|---|---|
| 508.8 skip | **drop** | remove the declare blockers and combat damage steps |
| 510.4 second damage step | **splice after** | insert a combat damage step at the head |
| 500.8 extra combat phase (M4) | **splice after** | insert a phase run (not M2b) |

CR 500.11 defines a skip as "proceed past it as though it didn't exist" — which
is exactly *removing it from the schedule*, not *visiting it and doing nothing*.
And CR 500.9's "the most recently created step will occur first" is *insert at the
head of `remaining`* (LIFO) for free, which is why the head, not the tail, is the
splice point.

### The engine loop

`Turn.next` is deleted. `Engine.advance` stops computing a successor and instead
consumes the schedule:

```hs
-- Pawl.Engine
advance :: Game ()
advance = do
  gs <- State.get
  case Seq.viewl (GameState.remaining gs) of
    p Seq.:< rest -> State.put gs {GameState.phase = p, GameState.remaining = rest}
    Seq.EmptyL    -> handoffTurn
```

`handoffTurn` refills: `phase = Turn.firstPhase`, `remaining = <allPhases minus
its head>` (the template, exposed as a total value so no partial `head`/`tail` is
written). `Setup.emptyGame` initializes the same way. `Turn.grantsPriority` is
**unchanged**: the steps it would wrongly grant priority to are no longer *in the
schedule* to be asked about, so the drop subsumes what §M2b's pre-turn-as-data
sketch proposed to do with a shared predicate across `next` and `grantsPriority`.
Turn-as-data makes the fix structural rather than predicated.

### Why not a flag per case

The alternative — a `Bool` on `GameState` for "skip blockers this turn," another
for "second damage step pending" — needs a new flag for every future conditional
and, crucially, **cannot represent an extra combat phase at all** (M4's Relentless
Assault, Aggravated Assault, World at War, Moraug). Turn-as-data represents all of
them as sequence edits. The cards are M4+; the *mechanism* is M2b's to choose, and
it is cheap now and a retrofit later.

### Existence proof at scale

This is not speculative. ygopro-core's engine is a resumable step-machine whose
pending work is a spliceable list of process units (`field.h:197`;
`prior-art-lessons.md` §10.3), and it runs a 13k-card game that way. The design is
load-bearing prior art, not a first-principles bet — cite it, don't re-argue it.

## 2. The two keywords

```hs
-- Pawl.Type.Keyword — CR 702, ordered by rule number.
data Keyword
  = Defender      -- 702.3
  | DoubleStrike  -- 702.4   (new)
  | FirstStrike   -- 702.7   (new)
  | Flying        -- 702.9
  | Haste         -- 702.10
  | Reach         -- 702.17
  | Vigilance     -- 702.20
```

Two constructors inserted **in CR-number order** (the M2a convention that keeps
`Keyword` diffable against rule 702): `DoubleStrike` at 702.4, `FirstStrike` at
702.7. M2a's §1 already settled that casing on a keyword is not a violation of the
closed/open invariant — a keyword is a numbered rule, the same kind of thing as
`Phase`. Nothing about first strike changes that argument; see the note atop
`Pawl.Type.Keyword` before "fixing" a `case keyword of FirstStrike -> …` into a
classification.

Both are read through M2a's projection, never off the card:

```hs
-- Already exist (M2a). No change.
keywordsOf :: ObjectId -> GameState -> Set Keyword
hasKeyword :: Keyword -> ObjectId -> GameState -> Bool
```

Multiple instances of either are **redundant** (CR 702.4e, 702.7d), which the
`Set Keyword` on `Card` already models — the same per-keyword fact M2a relied on,
still true of every keyword through M2c.

## 3. The two combat damage steps (CR 510.4)

This is the heart of the milestone. CR 510.4 (and, identically, CR 702.4b and
702.7b) reads:

> If at least one attacking or blocking creature has first strike or double strike
> **as the combat damage step begins**, the only creatures that assign combat
> damage in that step are those with first strike or double strike. After that
> step, instead of proceeding to the end of combat step, the phase gets a **second
> combat damage step**. The only creatures that assign combat damage in that step
> are the remaining attackers and blockers that **had neither first strike nor
> double strike as the first combat damage step began**, as well as the remaining
> attackers and blockers that **currently have double strike**. After that step,
> the phase proceeds to the end of combat step.

Three facts to encode:

1. **Whether there are two steps** is decided *as the first combat damage step
   begins*, by a keyword question over the current attackers and blockers.
2. **Who deals in the first step**: those with first strike or double strike.
3. **Who deals in the second step**: those that *had neither* first strike nor
   double strike when the first step began (a **snapshot**), *plus* those that
   *currently* have double strike.

### The splice is what gives the second step priority for free

CR 510.3 gives the active player priority after **each** combat damage step, and
CR 510.4 does not remove it. So the two steps are genuinely two steps with a
priority window and a state-based-action check between them — which is exactly
what `Engine.runStep` already does around every step (turn-based action →
`checkSba` → priority → `checkSba` → `advance`). Modelling the second step as a
**spliced step** (§1) rather than a second inner loop inside one step gets the
between-steps priority and SBA check *for free and correctly*. This is the payoff
of turn-as-data, and it is why "deal both waves inside one handler" is wrong: it
would skip the CR 510.3 priority window.

### Routing the waves: a snapshot on `Combat`

The two combat damage steps are the **same kind** of step (CR 506.1 enumerates
*five* step kinds; there is no sixth). A `CombatDamage` occurrence therefore
cannot tell from its `Phase` value alone whether it is the first wave or the
second — and after `advance` consumes the first occurrence, positional lookahead
cannot tell either. The distinction is genuine transient combat state, so it lives
where transient combat state lives — the `Combat` record, which "exists to absorb"
exactly this (M2a's banding note, `Type/Combat.hs`):

```hs
-- Pawl.Type.Combat gains:
--   struckFirst :: Maybe (Set ObjectId)
--     Nothing            -- the first-strike combat damage step has not happened
--     Just participants  -- it has; `participants` are the attackers and blockers
--                           that HAD first strike or double strike as it began
--                           (CR 510.4's snapshot). Reset to Nothing at CR 511.
```

`emptyCombat` sets it to `Nothing`; `clearCombat` (CR 511.3, end of combat) resets
it. A `Maybe (Set …)`, not a bare `Bool` or a bare `Set`: `Nothing` vs `Just`
crisply answers "has the first-strike step happened," with no overloading of the
empty set, and the set it carries *is* CR 510.4's had-first-strike-or-double
snapshot rather than a second thing to store.

`Damage.dealCombatDamage` becomes wave-aware. When a combat damage step runs:

```
strikers = combat participants (attackers ∪ blockers) that currently have
           first strike or double strike     -- keywordsOf projection, read LIVE,
                                                 at the step boundary
case struckFirst of
  Nothing
    | null strikers ->  -- CR 510.4 does not apply: one step, everyone deals
        deal from every participant still on the battlefield
        -- struckFirst stays Nothing; no splice
    | otherwise ->      -- this is the FIRST of two steps
        set struckFirst = Just strikers
        deal from `strikers` (still on the battlefield)
        splice a second combat damage step at the head of `remaining`
  Just snapshot ->      -- this is the SECOND step
        deal from participants still on the battlefield where
          (not member snapshot)  or  hasKeyword DoubleStrike
        -- no further splice
```

The second-step predicate is CR 510.4 read literally: `not (member snapshot)` is
"had neither first strike nor double strike as the first step began" (the
snapshot's complement); `hasKeyword DoubleStrike` is "currently have double
strike." Check it against the four cases:

| Participant | In first step? | In second step? |
|---|---|---|
| vanilla | no (not a striker) | **yes** — `not member` |
| first strike only | yes | no — in snapshot, not double strike |
| double strike | yes | **yes** — `hasKeyword DoubleStrike` |
| first strike **and** double strike | yes | **yes** — `hasKeyword DoubleStrike` |

The last row is why `not (member snapshot)` alone is not enough and the `or
hasKeyword DoubleStrike` clause is load-bearing — a naive "everyone who didn't
strike first" would drop it.

### "Still on the battlefield" is not optional

Both waves deal only from participants **still on the battlefield**, because CR
510.4 says "the *remaining* attackers and blockers." In M1b this never mattered:
one simultaneous wave, gathered before any death. In M2b a creature can die in the
first-strike step — the `checkSba` between the two steps buries it — and it must
not deal again. Concretely: a double striker blocked by a first striker can die in
the first step; it is in the snapshot and has double strike, so the wave predicate
alone would let it deal a second time. The battlefield check stops it, and there
is a test for exactly this (§6). `Combat.attackers`/`Combat.blockers` still list
dead ids (combat records are not pruned on death), and a graveyard object still
carries its power, so the check must be explicit — it is not implied by
`powerOf`.

### Evaluate at the boundary, through the projection

Both the splice decision (`null strikers`) and the two wave predicates are read
from `keywordsOf` **when the step is reached**, never precomputed at combat start.
At M2b this is invisible — nothing changes a creature's keywords mid-combat — but
precomputing is Arena's Zurgo bug (`prior-art-lessons.md` §8.4), and at M3 a
grant/removal in the priority window *between* the two steps changes the answer.
The snapshot on `struckFirst` is captured at the first step's boundary for the
same reason: CR 510.4's "as the first combat damage step began" is a point in
time, and at M3 (layer 6) "had first strike then" and "has first strike now" come
apart. See §7 for the exact M3 expiry.

### What does not change

`Prompt.AssignCombatDamage`, `Damage.attackerAssignment` and
`Damage.blockerAssignment` are M1b's and are reused unchanged per wave — the
assignment *rules* (CR 510.1) are identical in both steps; only the *set of
creatures* assigning differs, and that set is the wave predicate above. The M1b
reject-not-repair discipline for an illegal assignment (CR 510.1e) carries over
verbatim.

Leave a comment at `Damage.attackerAssignment` pointing at the expiry **M2c**
owes: the damage-assignment chooser is still hardcoded as the attacker's
controller (`Game.controllerOf attacker`), which banding (702.22j) and Mindslaver
both falsify — so the two-step restructure does not silently rebuild that
assumption into itself. This is recorded in M2a's deferral list already; M2b only
adds the pointer at the call site.

## 4. The CR 508.8 skip (`git-bug 5f50eec`)

CR 508.8: "If no creatures are declared as attackers or put onto the battlefield
attacking, skip the declare blockers and combat damage steps." With turn-as-data
this is a **drop**, performed as a turn-based action at the declare attackers step,
right after attackers are declared:

```hs
-- Pawl.Engine.runTurnBasedActions, the DeclareAttackers case
Phase.Combat CombatStep.DeclareAttackers -> do
  Combat.declareAttackers active
  -- CR 508.8 / 500.11: if no creatures were declared as attackers, remove the
  -- declare blockers and combat damage steps from the schedule, so the turn
  -- proceeds as though they didn't exist.
  State.modify' skipEmptyCombat   -- drops those steps from `remaining` iff
                                  -- Combat.attackers is empty
```

The condition is `Map.null (Combat.attackers …)` *after* declaration — CR 508.8
keys on "no creatures declared," which includes the case where legal attackers
existed but the active player declared none (`Combat.declareAttackers` already
does not prompt when there are no candidates, and returns with an empty map when
the player declines). "Put onto the battlefield attacking" has no source at M2b
(no such effect exists) and is not implemented; expiry named in §7.

**Observability.** As at M1b/M2a, the skip changes no game *state* this milestone,
because sorcery-speed casting (`Cast.sorcerySpeed`) means the skipped priority
windows can only be passed. What it changes is the `DecisionLog`: a faithful
engine does not ask the active player for an action in those two steps on an
attacker-less turn, and today's pawl asks twice. Since attacker-less turns are the
common case in a random game (the opening turns, any turn with no untapped
creatures), the drop is exercised across the property games in §6 — it is the one
part of M2b that gets random-game coverage without a deck change.

**Drop scope.** M2b has exactly one combat phase, so removing *the* declare
blockers and combat damage steps from `remaining` is unambiguous. With a second
combat phase (M4) the drop must target only the current phase's steps — a
positional concern the sequence model already supports and which §7 records as an
expiry.

## 5. The test cards

Two printings, each **verified against Scryfall**
(`api.scryfall.com/cards/named?exact=…`, not recalled; the dumps under `_scratch/`
are other projects' data and are fine only for *finding* a candidate):

| Card | Cost | P/T | Type | Rules text | Rulings |
|---|---|---|---|---|---|
| **Sabretooth Tiger** | `{2}{R}` | 2/1 | Creature — Cat | First strike | 0 |
| **Ridgetop Raptor** | `{3}{R}` | 2/1 | Creature — Dinosaur Beast | Double strike | 0 |

Both are mono-red, both genuinely vanilla-plus-one-keyword (their entire behavior
is a type line, cost, P/T and one rule 702 citation — zero opcodes), and both are
**2/1**, the same body as a Goblin Piker.

**The constant 2/1 is the point.** Goblin Piker (2/1 vanilla), Sabretooth Tiger
(2/1 first strike) and Ridgetop Raptor (2/1 double strike) form a triple in which
power and toughness are held fixed and *the only thing the engine can see is the
keyword* — the same control M2a built from Goblin Chariot and Goblin Piker. It
makes the falsifiers razor-sharp: when a Tiger beats a Piker but two Pikers trade,
nothing but first strike can be responsible; when an unblocked Raptor deals four
where an unblocked Tiger deals two, nothing but double strike can be.

`Pawl.Type.Subtype` gains `Cat`, `Dinosaur` and `Beast` (enum growth, no shape
change — subtypes are pure data with no engine logic behind them, exactly like
M2a's `Human`/`Bird`/`Ogre`/`Centaur`). No new color: both cards are red, so the
color axis the M2a spec was careful to avoid (its Yotian Soldier note) stays
closed. The obvious wider-known test cards — Boros Swiftblade, Fencing Ace,
Youthful Knight (`docs/design.md` §M2b) — are white or Boros and were the
zero-rulings *examples*, not a mandate; a mono-red pair keeps M2's mana-base
discipline and adds nothing new.

**Neither card joins the deck.** Both are exercised by fixtures only, for two
reasons. First, the scenario tests place creatures directly on the battlefield
(`combatBoardOf` → the fixtures at `Main.hs:320`), so a card need not be castable
to be tested — cost and color are for faithfulness, not for the fixtures.
Second, and unlike M2a's Bird Maiden, the turn-structure change these cards prove
already gets random-game coverage from the deck as it stands: the CR 508.8 skip
(§4) fires on ordinary attacker-less turns. The one thing a first striker in the
deck would additionally exercise is the CR 510.4 splice inside a *random* game —
real coverage, but bought at the price of re-tuning M2a's carefully balanced "36 /
16 / 8" list and its "fliers get through" property, which needs its eight Bird
Maidens. The trade is not worth disturbing a landed property for; the splice is
proven in fixtures, and the deck is left alone. (If a later milestone wants
random-game splice coverage, it adds a first striker then, as its own deliberate
deck change — this note is its expiry.)

**No rulings step.** Both cards carry **zero** Gatherer rulings (Scryfall, checked
2026-07-17) — french-vanilla keywords' oracle is the CR itself, so §4-of-design's
rulings discipline has nothing to transcribe here and first pays off at M3+. Test
names stay CR-numbered, per M2a.

## 6. Testing approach

One growing `testTree` in `source/test-suite/Main.hs`, property- and
integration-style, asserting `GameState` after actions — M0/M1a/M1b/M2a's
convention. First strike and double strike are only observable *after* combat
damage, so unlike M2a (which mostly asserts pre-damage legality) these are
driven **through the engine** so the two-step structure, the between-steps SBA
check, and the splice all run. The combat fixtures (`Main.hs:320`) gain the
`remaining` field alongside the `phase` they already set, so the engine can
`advance` from declare attackers through end of combat.

**Rule-numbered scenarios.** Each names the case an identity-based or single-step
implementation gets wrong:

- *CR 702.7b — first strike kills before dying.* A 2/1 first striker (Sabretooth
  Tiger) blocked by a 2/1 Goblin Piker: the Piker takes lethal in the first-strike
  step and is buried by the SBA between steps, so it never deals — the Tiger
  survives at zero damage. **The falsifier**, and it requires both the two-step
  structure *and* the `checkSba` sitting between the steps.
- *CR 510.2 — the control: two vanilla 2/1s trade.* The same board with a Piker in
  place of the Tiger: both die. Isolates first strike as the sole cause, and keeps
  M1b's simultaneous-damage behavior honest for non-first-strikers.
- *CR 702.4b — double strike deals twice, unblocked.* An unblocked 2/1 double
  striker (Ridgetop Raptor) deals **4** to the defending player (two in each
  step). An unblocked Sabretooth Tiger deals 2. **The double-strike falsifier for
  "deals once."**
- *CR 510.4 — double strike deals in the second step against a survivor.* A 2/1
  double striker blocked by a 3/3 (Ogre Sentry): the Ogre takes 2 in the
  first-strike step (survives), 2 more in the second (dies at 4 ≥ 3); the Ogre's 3
  kills the 2/1. Against a first striker instead, the Ogre survives on 2 marked.
  Isolates *deals-in-both-steps* from *deals-once-first* — the case a survivor
  makes visible and a 1-toughness blocker hides.
- *CR 510.4 — a double striker that died in the first step does not deal again.* A
  2/1 double striker (Ridgetop Raptor) and a 2/1 first striker (Sabretooth Tiger)
  block each other's fight: both have a striking keyword, both deal in the
  first-strike step, both die, and the second step deals nothing. **The "still on
  the battlefield" test** — it fails against a wave predicate that consults only
  the snapshot and double strike.
- *CR 510.4 — the mixed board.* First striker, double striker and a vanilla
  creature attacking together: the first striker's damage lands only in step one,
  the vanilla's only in step two, the double striker's in both. One test, all
  three shapes, so a blanket "first strikers step one / everyone step two" bug
  cannot pass.
- *CR 508.8 — the skipped steps grant no priority.* On a turn where the active
  player declares no attackers, the declare blockers and combat damage steps issue
  **no** `ChooseAction` prompt (asserted by counting prompts / inspecting the
  `DecisionLog`), and the turn still reaches end of combat. This is the
  `git-bug 5f50eec` regression test.
- *CR 508.8 — the control: an attacker keeps the steps.* The same turn with one
  attacker declared runs declare blockers and combat damage normally. So the skip
  cannot be implemented as "always skip."

**The classification test, which is the point of the milestone:** the splice
decision, the wave predicates and the skip all read `keywordsOf` and the attacker
map, and **no call site in `Pawl.Turn`, `Pawl.Engine` or `Pawl.Damage` names a
card**. Asserted the way M2a asserted its seam — by a board the identity-based
implementation would get wrong — here, the mixed-board test above, whose outcome
depends only on which citations the creatures carry.

**Properties.** All of M1b's and M2a's survive and must keep passing: conservation
(120 objects), termination, ids minted ≥ 120, no mana floats, life never
increases, combat happens, and M2a's "fliers get through." M2b retires **none** of
them — the deck is unchanged (§5), so conservation still asserts the constant 120,
and the turn-structure change is additive.

**New:**

- *no priority in a skipped step, across seeds* — over the property games, on
  every turn where no attacker was declared, the `DecisionLog` contains no action
  request timestamped to that turn's declare blockers or combat damage step. This
  is the property form of the CR 508.8 fix, and it is the one M2b change with
  random-game coverage.

## 7. Expiries

Every elision in M2b, and the milestone that kills it. Inherits M2a's table (still
owed) and M1b's beneath it.

| Elision | Why it is legitimate now | Killed by |
|---|---|---|
| `struckFirst` snapshot read from the *live* projection | Nothing changes keywords mid-combat, so "had first strike as the first step began" equals "has it now" | **M3** — layer 6 grants/removals in the CR 510.3 window between the two steps |
| Wave predicate reads keywords live at the second step | Same — no mid-combat change | **M3** — CR 702.4c/d, 702.7c (gaining/losing a strike between steps) |
| `checkSba` between the two damage steps is single-pass | Nothing triggers on a creature's death yet, so one pass reaches a fixed point | **M4** — death triggers make CR 704.3's "repeat until stable" load-bearing here |
| Damage is a direct mutation (M1b's `applyCombatDamage`), not funnelled through a change-and-emit helper | No effect reads a damage *event* yet | **M2c** — deathtouch's "dealt damage by a deathtouch source since the last SBA check" bit is the first damage-event reader; the atom/event funnel earns its first consumer there, and M3 generalizes it to all mutations |
| `skipEmptyCombat` drops *the* combat steps | One combat phase per turn | **M4** — a second combat phase (500.8) requires dropping only the current phase's steps, positionally |
| CR 508.8's "put onto the battlefield attacking" not implemented | No effect creates attacking creatures | **M4+** — the effects that do |
| Damage-assignment chooser hardcoded to the attacker's controller | Nothing inverts it | **M2c/M3** — banding 702.22j (defending player chooses) and Mindslaver (`Decider`) |

### The one that matters most for M3

The `struckFirst` snapshot and the boundary-read projection are the seam M3 will
lean on. M2b **must not** precompute the wave membership at combat start (§3): the
splice decision, the snapshot capture, and the second-step predicate are all read
when their step is reached. Done that way, M3's only change here is that
`keywordsOf` starts consulting the layer system instead of the card — which M2a
already localized to that one function. Precompute it and M3 is a rewrite of the
combat damage step; read it at the boundary and M3 is a one-function change. This
is the whole reason the pre-spec note insisted on it, and it is why the snapshot
is a captured set rather than a recomputed query.

## Explicitly deferred past M2b

- **Deathtouch, trample** — M2c, with their CR 702.2c interaction and the
  damage-event bit that first reads what §7 defers here.
- **An additional combat phase** (CR 500.8) and **event history** ("for each time
  it has attacked this turn," Moraug) — M4. The splice mechanism is M2b's; the
  *cards* and the turn-scoped event log are not.
- **Layer-6 grants/removals of first strike or double strike mid-combat**
  (CR 702.4c/d, 702.7c) — M3. M2b builds the boundary-read seam that makes them a
  local change.
- **The remaining evergreen keywords** — indestructible, intimidate, landwalk,
  lifelink (punchlist); enchant, equip, flash, hexproof, protection, shroud, ward
  (blocked on machinery that does not exist). Unchanged from M2a's triage.
- **Banding (702.22)** and the damage-assignment chooser inversion — a `Decider`
  problem, M3+, per `docs/design.md` §M2b and §7 of the M2a spec.
