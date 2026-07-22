# M4.5 P6 — Conditional and event durations, and the moment a duration begins

*Design pass 2026-07-22. The seventh phase of the M4.5 umbrella
(`docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`), closing
**GAP-D**. The third phase of Cluster 2, following P4's event substrate and P5's
monadic replacement path. Gates: **Master Thief** (CR 611.2b's own worked
example) and **Hag of Inner Weakness**. This spec is implementable; a
`writing-plans` plan follows it.*

*CR citations below **were checked against `docs/rules.txt` during this design
pass**: 109.5, 115.1a, 302.6, 400.7, 500.12, 502.4, 514.2, 603.2b, 603.3a,
603.6a, 608.2b, 611.2a, 611.2b, 611.2c, 613.1b, 704.3, 704.5f, 704.5g. Any number added
later and marked **(verify)** must be checked before it drives code (CLAUDE.md:
never trust recalled Magic rules).*

*Card text and Gatherer rulings for both gate cards **were verified live against
the Scryfall API during this design pass**, not read from the vendored MTGJSON
dump (`card-data-source`). The oracle text and the three Master Thief rulings
quoted in §5 are what Scryfall returned on 2026-07-22.*

## 0. Why this phase, and what it proves

`Pawl.Type.Duration` has exactly two inhabitants, and has had since M3b:

```haskell
data Duration
  = UntilEndOfTurn -- CR 514.2
  | Indefinite     -- CR 611.2a, "lasts until the end of the game"
```

Both are **fixed points**: a stored effect either dies at the next cleanup or
never dies. CR 611.2 describes two further shapes that neither can express.

1. **A continuously re-checked condition** — CR 611.2b's *"for as long as . . .
   ."* The effect ends the moment its condition stops holding, and — this is the
   whole difficulty — **does not come back** if the condition holds again later.
   CR 611.2b also fires *before* the effect exists: *"If the 'for as long as'
   duration never starts, the effect does nothing."* So a duration has a
   **beginning**, and pawl has no such moment today.

2. **An expiry keyed to a future event** — CR 611.2a's *"lasts as long as stated
   by the spell or ability creating it."* "Until your next turn" is not
   expressible as printed card data at all: *"your"* has to become a concrete
   `PlayerId` (CR 109.5) at the moment the effect is created, and the effect has
   to outlive the cleanup step (CR 514.2) that ends every duration pawl
   currently stores.

Both shapes are carried by **two** types, not one: `ContinuousEffect.duration`
and `ActiveReplacement.duration` (the floating replacements P5 built). The axis
is shared, so it is fixed once.

### What this phase is *not*

It is not new *layer* work — `Expiry` is orthogonal to CR 613 and the projection
fold is untouched. It is not the player-scoped continuous-effect axis (P7), the
target-filter language (P9), or the cost seam (P8). It adds **no new opcode**:
both gate cards are existing `Effect` constructors reached with a new `Duration`.

### Gate cards

**Master Thief** — {2}{U}{U}, Creature — Human Rogue, 2/2 (Mirrodin; verified
via Scryfall).

> When this creature enters, gain control of target artifact for as long as you
> control this creature.

This card is not a candidate chosen by convenience: it is **the example the
comprehensive rules themselves print under CR 611.2b** (`rules.txt` line 2907).
Its three Gatherer rulings are, verbatim, the three behaviours this phase must
produce (§5).

**Hag of Inner Weakness** — {2}{B}, Creature — Hag Warlock, 2/2 (Alchemy
Horizons: Baldur's Gate; verified via Scryfall).

> At the beginning of your upkeep, target creature an opponent controls gets
> -2/-1 until your next turn.

A real Magic card with real Oracle text — **not** a synthetic fixture — but a
digital-only one, never printed on paper. That is a deliberate, recorded
trade-off. The paper pool's "until your next turn" cards were enumerated
exhaustively during this design pass (a Scryfall query, 66 paper cards) and
every one of them drags in machinery this phase does not own: flashback and a
new bulk opcode (Mass Diminish), graveyard reanimation plus intra-resolution
"it" threading (Bond of Revival), ward (Mouth of the Storm), player-scoped
restrictions (The Stasis Coffin, Forbidding Spirit, Enter the Infinite — all
P7), sagas, or planeswalker loyalty. Hag of Inner Weakness needs **one** new
`TargetSpec` and nothing else: P4 already built its `StepBegins (Beginning
Upkeep) ControllersTurn` trigger (Sarcomancy uses the identical shape),
`Quantity.Literal` is already signed, and `Modification.ModifyPowerToughness` is
M3b vocabulary. Recognizability is traded for a gate that tests the *axis* and
not the collateral. If a paper card of the same shape is ever implementable
cheaply, it is a strictly additive second test, not a replacement.

### The falsifiers, stated up front

- **Master Thief falsifies a re-evaluated predicate.** The obvious
  implementation stores the effect and filters it out of the projection while
  the condition is false. That is wrong: CR 611.2b's duration is one continuous
  period, so an effect that has ended must stay ended. Gatherer, verbatim:
  *"Regaining control of Master Thief won't cause you to regain control of the
  artifact."* The effect must be **deleted**, not masked.

- **Hag of Inner Weakness falsifies a log-scan.** Its ability triggers on *your*
  upkeep, so the effect is created during a turn whose untap step has **already
  happened**. Any implementation that expires the effect by looking for a
  matching `GameEvent.StepBegan` belonging to that player finds one immediately
  and kills the effect on the turn it was born. It also falsifies treating any
  cross-turn duration as `UntilEndOfTurn`: the -2/-1 has to survive CR 514.2 and
  the whole of the opponent's turn.

## 1. Scope

**In scope.** The `Duration` → `Expiry` split; the arming moment (CR 611.2b);
`StateCondition`'s third customer; a new `Pawl.Expiry` module owning every
`case … Expiry`; three sweep sites; two `TargetSpec` variants and the
source-relative `Target` signature they force; two card data files and their
gameplay-level tests.

**Out of scope.** Everything in §4.

## 2. Architecture

### 2.1 A printed `Duration`, an armed `Expiry`

The single `Duration` type splits in two. This is the same shape the engine
already uses twice — `ReplacementEffect` / `ActiveReplacement` and
`DelayedTrigger` / `PendingTrigger` — where printed card data and the runtime
record that carries it are different types.

```haskell
-- Pawl.Type.Duration -- PRINTED. Appears in card JSON.
data Duration
  = UntilEndOfTurn                -- CR 514.2
  | Indefinite                    -- CR 611.2a
  | ForAsLongAs StateCondition    -- CR 611.2b
  | UntilYourNextTurn             -- CR 611.2a
  deriving (Eq, Ord, Show)

-- Pawl.Type.Expiry -- STORED. Runtime only; never appears in card JSON.
data Expiry
  = AtCleanup                     -- CR 514.2
  | Never                         -- CR 611.2a, "until the end of the game"
  | While PlayerId StateCondition -- CR 611.2b; "you" baked per CR 109.5
  | AtTurnOf PlayerId             -- CR 611.2a; ends as that player's turn begins
  deriving (Eq, Ord, Show)
```

`ContinuousEffect.duration :: Duration` and `ActiveReplacement.duration ::
Duration` both become `expiry :: Expiry`. No record anywhere stores a `Duration`
after this phase; no card JSON anywhere mentions an `Expiry`.

**Why a split and not two more constructors on one type.** `UntilYourNextTurn`
and `AtTurnOf` are genuinely different values: the first is what the card says,
the second is what the game remembers, and the transformation between them
(resolving CR 109.5's "you") is real work that must happen exactly once. A
single type with a card-only arm and a runtime-only arm — the
`Modification.SetController` posture — would make a card-only value that leaked
into `GameState.continuousEffects` a **silent** bug: the sweeps would simply
never match it and the effect would last forever. The split makes that
unrepresentable, and gives CR 611.2b's "does the duration start?" check a
natural home (§2.3). The cost is renaming one field on two records and touching
every construction site, which is four call sites.

### 2.2 The condition vocabulary is `StateCondition`'s third customer

`Pawl.Type.StateCondition` (P4) already describes itself as *"a predicate over
game STATE rather than over an event"* with *"two customers, one vocabulary"* —
a CR 603.8 state trigger's condition and a CR 603.4 intervening "if". A CR
611.2b duration condition is a third predicate of exactly that kind, so it gets
no new type:

```haskell
data StateCondition
  = YouControlNo Subtype           -- CR 603.8  (Barbarian Outcast)
  | NoPermanentsOfSubtype Subtype  -- CR 603.4  (Sarcomancy)
  | YouControlSource               -- CR 611.2b (Master Thief)  <- new
```

`YouControlSource` reads: *the object this effect came from is on the
battlefield, and its **projected** controller (CR 613.1b, P1's layer 2) is the
player named by the surrounding `Expiry.While`.* Both halves are load-bearing
and each matches one Gatherer ruling — battlefield membership is "if Master
Thief leaves the battlefield", controller match is "if another player gains
control of Master Thief". CR 400.7 makes the first robust for free: a Master
Thief that dies and returns is a new object with a new `ObjectId`, so the stored
`source` can never be satisfied by the returning permanent.

`Pawl.Event.stateHolds` **stays the sole home of `case … StateCondition`** and
grows a source parameter:

```haskell
stateHolds :: PlayerId -> ObjectId -> StateCondition -> GameState -> Bool
```

The two existing arms ignore the `ObjectId`; both existing call sites already
have the ability's source in scope. Reusing the type also means one retirement
path rather than two: `StateCondition` is already marked *"retired wholesale by
P9's criterion/filter language (#38)"*, and this arm retires with it.

### 2.3 `Pawl.Expiry`, the sole casing home

A new module `Pawl.Expiry` is **the only module that may `case` on `Expiry`** —
the same standing `Pawl.Resolve` has over `Effect`, `Pawl.Projection` over
`Modification`, and `Pawl.Event` over `TriggerCondition`. It exports four
functions.

```haskell
-- CR 611.2 / 611.2b / 109.5: the moment a duration BEGINS. Nothing means the
-- duration never started, so per CR 611.2b the effect does nothing and is
-- never stored at all.
arm :: PlayerId -> ObjectId -> Duration -> GameState -> Maybe Expiry

-- CR 514.2: cleanup ends every "until end of turn" and "this turn" effect,
-- over BOTH continuousEffects and replacements.
dropAtCleanup :: GameState -> GameState

-- CR 611.2a: "until your next turn" ends as that player's turn begins.
dropAtHandoff :: GameState -> GameState

-- CR 611.2b: drop every While whose condition has stopped holding. Reports
-- whether it changed anything, so the settle loop knows to run again.
sweepConditional :: Game Bool
```

`arm` is total and mechanical:

| `Duration` | `Expiry` |
|---|---|
| `UntilEndOfTurn` | `Just AtCleanup` |
| `Indefinite` | `Just Never` |
| `UntilYourNextTurn` | `Just (AtTurnOf controller)` — CR 109.5 |
| `ForAsLongAs c` | `Just (While controller c)` if the condition holds **now**, else `Nothing` — CR 611.2b |

`dropAtCleanup` **absorbs and deletes** `Projection.dropEndOfTurnEffects` and
`Event.dropEndOfTurnReplacements`. Those two functions exist only because the
two carriers live in two modules; with one `Expiry` vocabulary there is one
sweep over both lists, and the cleanup step calls it once instead of twice.

### 2.4 The three sweep sites

**Arming — `Pawl.Resolve.applyEffect`.** Three arms store a duration today:
`ModifyTarget`, `GainControl`, and `Replace`. Each calls
`Expiry.arm controller source duration gs` and stores nothing on `Nothing`.
Both `controller` (the effect's controller) and `source` (the resolving spell
for a spell, the source permanent for an ability) are already bound in that
function's scope, so no plumbing is added.

**Cleanup — `Pawl.Engine.runStep`, `Ending Cleanup`.** The two existing
`State.modify'` calls collapse to one `State.modify' Expiry.dropAtCleanup`,
still simultaneous with `Damage.removeAllDamage` per CR 514.2.

**Priority — `Pawl.Engine.settleForPriority`.** The conditional sweep becomes
the loop's first step, ahead of state-based actions:

```haskell
settleForPriority = do
  swept  <- Expiry.sweepConditional
  acted  <- Sba.performStateBasedActions
  placed <- placePendingTriggers
  Monad.when (swept || acted || placed) settleForPriority
```

CR 611.2b's condition is checked continuously, and CR 704.3 fixes the coarsest
moment anything can *observe* it: *"whenever a player would get priority."*
`settleForPriority` already runs at exactly the points where the board can
change. Ordering inside the loop is not arbitrary — losing control of a
permanent changes what the SBA check sees, and an SBA can be what falsifies a
condition — so the sweep runs first **and** the loop re-runs whenever anything
fired. The sweep is a list scan that touches the projection only when a `While`
is actually stored, so a game with no conditional effect pays nothing beyond the
scan.

**Turn handoff — `Pawl.Engine.handoffTurn`.** `dropAtHandoff` runs after
`activePlayer` has been updated, dropping every `AtTurnOf p` whose `p` is the
**new** active player.

That single check is complete, and needs no stamp of the turn the effect was
created on. The check never runs during the creating turn — handoff is the only
caller — so "a turn began and its active player is `p`" *is* "`p`'s next turn
began", including when `p` created the effect on their own turn and including
extra turns.

**This is the design's one departure from the umbrella**, which predicted
(§3 row P6, §4) that event-relative durations would ride P4's event log. They do
not: the turn-handoff transition *is* the event, known exactly and for free at
the one site that performs it, whereas reading `GameEvent.StepBegan` back out of
a turn-scoped log would need a per-effect watermark to distinguish "your untap
already happened this turn" from "your next turn began" — the exact trap Hag of
Inner Weakness sets. The `P4 → P6` dependency edge was already marked
**discharged** in the umbrella, so nothing about the phase ordering changes;
§2.3's `stateHolds` reuse still makes P6 a reader of P4's work. §9 records the
amendment.

Dropping at handoff is **observably identical** to dropping "as the turn
begins", by three rules: CR 500.12 (*"No game events can occur between steps,
phases, or turns"*), CR 502.4 (*"No player receives priority during the untap
step"*), and CR 704.3 (no state-based-action check happens without a player
being about to receive priority). Nothing — no spell, no ability, no SBA, no
trigger — can run between `handoffTurn` and the first moment a player could
observe the board, so the two placements are indistinguishable. The first
observation point is the upkeep step (CR 503.1), which is precisely where Hag of
Inner Weakness's next trigger fires.

### 2.5 Targeting becomes source-relative

Two `TargetSpec` variants, both the acknowledged `WallTarget` posture —
specific before general, one hand-carved variant per card, the whole family
retired by P9's criterion language (**#40**):

- **`ArtifactTarget`** — CR 115.1a, "target artifact": a permanent whose
  **projected** card types (M3c layer 4) include Artifact. Master Thief.
- **`OpponentCreatureTarget`** — CR 115.1a with CR 109.5, "target creature an
  opponent controls": a creature on the battlefield whose **projected**
  controller (CR 613.1b) is not the ability's controller. Hag of Inner Weakness.

The second is the first spec whose legal set depends on *who is choosing*, which
forces `Pawl.Target`'s two entry points to become source-relative:

```haskell
legalRecipients :: ObjectId -> TargetSpec -> GameState -> Set Recipient
stillLegal      :: ObjectId -> Recipient -> TargetSpec -> GameState -> Bool
```

The source `ObjectId` is already threaded one level out —
`legalSetsExcluding` takes it, and all three of its callers (`Pawl.Cast`,
`Pawl.Activate`, `Pawl.Engine.placePendingTriggers`) pass the spell or ability
object. This pushes it one level in. `Projection.controllerOf` on that source
gives the ability's controller: CR 603.3a for a triggered ability, CR 109.5 in
general. `stillLegal`'s two call sites in `Pawl.Resolve` have the same `source`
in scope, so CR 608.2b's re-check at resolution is controller-relative too — a
creature that comes under your control in response is no longer a legal target.

`selfExcludes` and `legalSetsExcluding`'s exclusion pass are **left alone**.
Folding self-exclusion into the now-source-aware `legalRecipients` is a
tempting cleanup and is explicitly not part of this phase.

### 2.6 Serialization

`Pawl.Codec` grows `ForAsLongAs` and `UntilYourNextTurn` arms on
`durationToJson` / `jsonToDuration`, a `stateConditionToJson` /
`jsonToStateCondition` arm for `YouControlSource`, and the two `TargetSpec`
arms. **`Expiry` gets no codec at all** — like `Modification.SetController`, it
is constructed at resolution and never round-trips. `GameState` is not
serialized, so there is nothing else to change.

## 3. The two invariants

**The rules core reads a classification, never an effect's identity.** `Expiry`
is a classification of *how long*, and `Pawl.Expiry` is its single casing home.
Nothing in this phase asks which card produced an effect: the sweeps read
`Expiry`, `stateHolds` reads `StateCondition`, the projection is untouched.
`StateCondition` gaining a second reader (`Pawl.Expiry` calling
`Event.stateHolds`) is not a widening of the casing surface — the `case` itself
stays in exactly one function.

**The engine makes no choices.** This phase adds no prompt and elides none.
Every value it computes is derived: the `PlayerId` baked by `arm` is CR 109.5's
"you", read off the effect's controller and never chosen; whether a condition
holds is read off the board. There is no point at which a player could be asked
anything, so there is nothing to elide and no expiry to name.

## 4. What this phase does **not** touch

- **The layer system.** `Expiry` is orthogonal to CR 613; the projection fold,
  timestamps, and dependency ordering are unchanged.
- **Player-scoped durations** — "until your next turn, you have hexproof" and
  every other duration hung on a player rather than an object is **P7**.
- **Conditional or turn-relative *replacements*.** The `Expiry` type is shared
  with `ActiveReplacement` by construction, so `While` and `AtTurnOf` become
  representable there — but no card in the pool creates one and no test covers
  it (§8).
- **Other duration wordings** — "until end of combat", "until end of your next
  turn", "for as long as [some other condition]". Condition and duration breadth
  are VOCAB, card-driven, and grow on the seam this phase builds.
- **Phasing's CR 702.26f interaction** with "for as long as" durations — phasing
  is umbrella backlog.
- **`selfExcludes` / `legalSetsExcluding`** — see §2.5.
- **CR 611.2b's second sentence** is vacuous here and is noted in code rather
  than deferred: *"if that duration ends before the moment the effect would
  first be applied and doesn't begin again during that spell or ability's
  resolution."* `arm` runs once, at the point the effect would be stored, and no
  opcode both ends and restarts a condition mid-resolution, so the
  begins-again-during-resolution case cannot arise.

## 5. Cards and tests

Two new card files, `data/cards/master-thief.json` and
`data/cards/hag-of-inner-weakness.json`. Master Thief is a `SelfEnters`
triggered ability (CR 603.6a) whose single effect is the existing
`Effect.GainControl (ForAsLongAs YouControlSource) "target"` over an
`ArtifactTarget` slot. Hag of Inner Weakness is a `StepBegins (Beginning Upkeep)
ControllersTurn` triggered ability (CR 603.2b, CR 603.3a) whose single effect is
the existing `Effect.ModifyTarget UntilYourNextTurn (ModifyPowerToughness
(Literal (-2)) (Literal (-1))) "target"` over an `OpponentCreatureTarget` slot.

Every test below is **gameplay-level** — it casts or triggers through the stack
and asserts on projected game state, per the umbrella's definition-of-done. All
supporting cards are already in the pool.

### Rulings discipline (design.md §4)

Master Thief's three Gatherer rulings, verified live and quoted verbatim, are
the specification of tests 2–4. They are not paraphrased into the tests; they
are the tests.

> - *"If Master Thief leaves the battlefield, you no longer control it, and its
>   control-change effect ends."*
> - *"If Master Thief ceases to be under your control before its ability
>   resolves, you won't gain control of the targeted artifact at all."*
> - *"If another player gains control of Master Thief, its control-change effect
>   ends. Regaining control of Master Thief won't cause you to regain control of
>   the artifact."*

Hag of Inner Weakness has **no** Gatherer rulings (Scryfall returned an empty
set), so its tests derive from CR 611.2a and CR 514.2 directly.

### Conditional durations — Master Thief

1. **It works.** Master Thief resolves with Darksteel Myr (an opponent's
   Artifact Creature — Myr, 0/1) as the target; the ETB trigger resolves and
   `Projection.controllerOf` reports Master Thief's controller for the Myr. CR
   302.6: the Myr is re-Sicked, as `GainControl` already does.
2. **The duration never starts** (ruling 2, CR 611.2b). With the ETB trigger on
   the stack, the opponent casts Lightning Bolt on Master Thief (2/2), which is
   destroyed by CR 704.5g before the trigger resolves. The
   trigger still resolves — its target is legal (CR 608.2b) — but `arm` returns
   `Nothing` and **no effect is stored**. Control of the Myr never changes, and
   nothing later restores it.
3. **The latch** (ruling 3) — *the falsifier*. Master Thief resolves normally
   and its controller holds the Myr. On the opponent's turn they cast Act of
   Treason on **Master Thief**: control of the Myr reverts to its owner at the
   next settle. At that turn's cleanup, Act of Treason's `AtCleanup` effect ends
   and Master Thief returns to its original controller — and the Myr **stays**
   with its owner. An implementation that filters rather than deletes fails
   exactly here.
4. **Leaving the battlefield ends it** (ruling 1). Master Thief resolves, then
   is destroyed. Control of the Myr reverts at the next settle, permanently.

### Event-relative durations — Hag of Inner Weakness

5. **It works.** The upkeep trigger targets an opponent's War Mammoth (3/3);
   the projection reports 1/2.
6. **It survives cleanup and the opponent's whole turn** — the falsifier for
   both `UntilEndOfTurn` and any log-scan. The Mammoth is still 1/2 in the
   controller's end step (CR 514.2 does not touch it), still 1/2 throughout the
   opponent's turn.
7. **It expires as the controller's next turn begins** (CR 611.2a). At the top
   of that turn the Mammoth is 3/3 again — asserted *before* the upkeep trigger
   fires a second time, so the two effects are never confused.
8. **The modification really applies.** Targeting an opponent's Goblin Piker
   (2/1) makes it 0/0 and it is put into its owner's graveyard by the CR 704.5f
   state-based action at the next settle, from the trigger's resolution alone.

### Codec

9. Round-trip both new card files, and unit-round-trip the two new `Duration`
   arms, `StateCondition.YouControlSource`, and the two new `TargetSpec` arms.
   Per design.md §4 this proves only that a card *says* it; tests 1–8 are what
   prove the engine *does* it.

## 6. Module and type changes (summary)

**New**

- `Pawl.Type.Expiry` — `AtCleanup | Never | While PlayerId StateCondition | AtTurnOf PlayerId`.
- `Pawl.Expiry` — `arm`, `dropAtCleanup`, `dropAtHandoff`, `sweepConditional`; sole home of `case … Expiry`.
- `data/cards/master-thief.json`, `data/cards/hag-of-inner-weakness.json`.

**Changed**

- `Pawl.Type.Duration` — `+ ForAsLongAs StateCondition`, `+ UntilYourNextTurn`.
- `Pawl.Type.StateCondition` — `+ YouControlSource`.
- `Pawl.Type.TargetSpec` — `+ ArtifactTarget`, `+ OpponentCreatureTarget`.
- `Pawl.Type.ContinuousEffect` — `duration :: Duration` → `expiry :: Expiry`.
- `Pawl.Type.ActiveReplacement` — `duration :: Duration` → `expiry :: Expiry`.
- `Pawl.Event` — `stateHolds` gains an `ObjectId`; `dropEndOfTurnReplacements` **deleted**.
- `Pawl.Projection` — `dropEndOfTurnEffects` **deleted**.
- `Pawl.Target` — `legalRecipients` and `stillLegal` gain the source `ObjectId`; two new spec arms.
- `Pawl.Resolve` — three storing arms call `Expiry.arm`; two `stillLegal` call sites pass the source.
- `Pawl.Engine` — `settleForPriority` gains the sweep; `runStep`'s cleanup calls `dropAtCleanup`; `handoffTurn` calls `dropAtHandoff`.
- `Pawl.Codec` — two `Duration` arms, one `StateCondition` arm, two `TargetSpec` arms.

## 7. Ordering within the phase (for the plan)

Substrate before consumers, and each step is a complete commit with its failing
test written first.

1. `Pawl.Type.Expiry`; rename the field on both carriers; `Pawl.Expiry` with
   `arm` (three trivial arms), `dropAtCleanup`, and the two deletions it
   absorbs. Behaviour-neutral: every existing test must still pass.
2. `Duration.UntilYourNextTurn` → `Expiry.AtTurnOf`; `dropAtHandoff` wired into
   `handoffTurn`. Codec arm.
3. `StateCondition.YouControlSource`; `stateHolds` gains its `ObjectId`;
   `Duration.ForAsLongAs`; `arm`'s CR 611.2b check; `sweepConditional` wired
   into `settleForPriority`. Codec arm.
4. `Target` becomes source-relative; `ArtifactTarget`; `OpponentCreatureTarget`.
   Codec arms.
5. Master Thief: card file, then tests 1–4 in order (1, then 4, then 2, then the
   latch — 3 last, since it is the one that needs the other three to be right).
6. Hag of Inner Weakness: card file, then tests 5–8.
7. Codec round-trips (test 9); the `expires:card-driven` issue from §8; docs.

Steps 2 and 3 are independent of each other and both depend only on 1; step 4 is
independent of all three. The plan may reorder within that constraint.

## 8. Deferred, with named expiries

Per CLAUDE.md, each of these is a GitHub issue carrying status, rationale and
expiry trigger, cited at the code site as `(#N)` with **no** expiry written into
the comment.

- **`While` and `AtTurnOf` on `ActiveReplacement` are representable but
  untested.** The `Expiry` vocabulary is shared with floating replacements by
  construction; no card in the pool produces a conditional or turn-relative one,
  so the sweeps handle them uniformly and nothing exercises the path. **New
  issue**, label `expires:card-driven` — the first card with "prevent … for as
  long as …" or "until your next turn, prevent …" (Morningtide's Light is one)
  retires it.
- **Two more hand-carved `TargetSpec` variants** — cite the existing **#40**
  (P9's criterion/filter language) at both, exactly as `NonblackCreatureTarget`
  already does. No new issue.
- **`StateCondition` gains a third arm** — cite the existing **#38** (P9 retires
  the type wholesale). No new issue.
- **Condition and duration vocabulary breadth** — VOCAB, not a gap; no issue
  (census §5, umbrella §1).

## 9. Tracking

- The umbrella's §3 P6 row and §4 "P6 is next" bullet are updated on landing:
  the phase is done, and the **"rides P4"** claim in the P6 row is corrected per
  §2.4 — event-relative durations are decided at turn handoff, not by reading
  the event log. P6 still reads P4's work through `stateHolds`.
- The umbrella's §4 ordering paragraph advances to **P7 is next** (Cluster 3,
  the player projection), which P4 already unblocked.
- No open issue is closed by this phase. **#38** and **#40** are cited, not
  retired; **#58**, **#49**, **#76**, `c7a0077` and `b998924` are untouched.
- `docs/progress.md` gains one distilled entry; `CLAUDE.md`'s status bullet is
  **replaced**, never appended.

## 10. Exit criterion

GAP-D is closed when `Duration` can express a continuously re-checked condition
and a future-event expiry; when `Pawl.Expiry` is the sole casing home for the
stored form and owns all three sweeps; and when both gate cards pass their
gameplay-level tests — in particular test 3, the latch, which is the one an
implementation that masks instead of deletes cannot pass, and test 6, which is
the one an implementation that scans the event log cannot pass.
