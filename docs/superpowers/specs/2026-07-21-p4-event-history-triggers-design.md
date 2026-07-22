# M4.5 P4 — Event history, and the triggers that are not events

*Design pass 2026-07-21. The fifth phase of the M4.5 umbrella
(`docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`), closing
**GAP-T** — turn-scoped event history plus state (CR 603.8) and delayed (CR
603.7) triggered abilities. Cluster 1 (layer-system completion) closed at P3b;
this opens Cluster 2, **the event substrate generalized**. P4 is a substrate and
**gates P6 and P7** (umbrella §4's hard dependency edges), so it is built before
its readers. Gates: **Barbarian Outcast**, **Tidal Wave**, **Khabál Ghoul**,
**Sarcomancy**. This spec is implementable; a `writing-plans` plan follows it.*

*CR citations below **were checked against `docs/rules.txt` during this design
pass**: 101.4, 111.3, 117.5, 400.7, 400.7e, 513.1, 513.1a, 514.1, 603.2, 603.2b,
603.2c, 603.2d, 603.2f, 603.2g, 603.2h, 603.3, 603.3a, 603.3b, 603.3c, 603.3d,
603.4, 603.6a, 603.6c, 603.7, 603.7a, 603.7b, 603.7c, 603.7d, 603.7e, 603.7f,
603.7h, 603.8, 603.9, 603.10, 603.10a, 603.12, 603.12a, 608.2a, 608.2c, 608.2h,
608.2i, 700.4, 701.21, 701.21a. Numbers marked **(verify)** were not, and must be
checked before they drive code (CLAUDE.md: never trust recalled Magic rules).*

*Card text for all four gate cards **was verified live against the Scryfall API
during this design pass**, not read from the vendored MTGJSON dump
(`card-data-source`). The oracle text quoted in §5 is what Scryfall returned.*

## 0. Why this phase, and what it proves

Two facts about the engine as it stands:

1. **Nothing survives being consumed.** `GameState` carries two drain queues —
   `zoneChanges` (drained by `Engine.placePendingTriggers`) and `damageEvents`
   (drained at each SBA check). Each is cleared by its reader. The game therefore
   has no way to answer *"how many times did that happen this turn?"* about
   anything at all.
2. **Every trigger is an event on an object.** `TriggerCondition` has exactly one
   constructor, `SelfEnters`; `Event.matchesTrigger` matches it against a
   `ZoneChange`; `Event.triggersFrom` scans only the *newcomer's own* abilities.
   A trigger that is not an event (CR 603.8) and a trigger that belongs to no
   object (CR 603.7) both have nowhere to exist.

The first want is now recorded for the **third** time. M1b's spec wanted an event
log for deathtouch and Bloodthirst-style cards (*"those want an event log, which
the engine needs anyway once triggers exist"*); M2b's spec wanted turn-scoped
history for Moraug's *"for each time it has attacked this turn"*; design.md §5's
"event substrate" item names all three wants together and the single-pass SBA
loop's admitted debt besides. M3f then built the **funnel** — one change-and-emit
helper that owns the no-op→no-event guard (CR 603.2f/603.2g) — but emitted into a
queue that its reader empties. P4 keeps the funnel and replaces the queues.

**The thesis, in one line:**

> Events are recorded, not consumed; and a trigger's condition is a
> classification over game *state*, not only over the event stream.

Those two halves are one phase because each is the other's falsifier. A log that
is merely a longer queue is not history — Khabál Ghoul proves that by counting
creatures that died at a priority boundary the trigger scan already passed. A
condition language that only matches events cannot express Barbarian Outcast at
all, and a naive fix — polling the condition at every boundary — floods the stack,
which is precisely the failure CR 603.8's second sentence exists to prevent.

### What this phase is *not*

It is not an event **vocabulary** exercise. The log records what its gate cards
need — zone changes, damage, step beginnings — and nothing else. Spell casts,
attacks, life changes and counter placements are **VOCAB** in the census's sense
(§5): one constructor apiece, added by the phase that first needs them. P7's Rule
of Law will add `SpellCast` when it needs a per-turn spell count. **P4 gates P7 by
mechanism, not by vocabulary** — a constructor no gate card exercises is exactly
what design.md §4's central trap warns about.

### Gate cards

Four cards, each one or two clauses, verified against Scryfall:

| Card | Oracle text | Axis it gates |
|---|---|---|
| **Barbarian Outcast** `{1}{R}` Creature — Human Barbarian Beast, 2/2 | "When you control no Swamps, sacrifice this creature." | State trigger (CR 603.8) |
| **Tidal Wave** `{2}{U}` Instant | "Create a 5/5 blue Wall creature token with defender. Sacrifice it at the beginning of the next end step." | Delayed trigger (CR 603.7), object-bound (603.7c) |
| **Khabál Ghoul** `{2}{B}` Creature — Zombie, 1/1 | "At the beginning of each end step, put a +1/+1 counter on this creature for each creature that died this turn." | Turn history (CR 608.2i); step trigger (603.2b) |
| **Sarcomancy** `{B}` Enchantment | "When this enchantment enters, create a 2/2 black Zombie creature token. At the beginning of your upkeep, if there are no Zombies on the battlefield, this enchantment deals 1 damage to you." | Intervening "if" (CR 603.4 / 608.2a) |

CR 603.8's own example text is *"a player controlling no permanents of a
particular card type"* — Barbarian Outcast's exact shape, chosen by the rulebook
to illustrate the rule. Khabál Ghoul introduces **zero** new opcodes: it is
`PutCounters PlusOnePlusOne (Quantity.Count …)`, cashing P3b's numeric tower
against a new `CountSpec` arm. Tidal Wave's token reuses M4g's `Subtype.Wall` and
`Keyword.Defender`. **CR 603.3b trigger ordering needs no fifth card** — it is a
*scenario* over two of these; see §5.

**Rejected gates, and why.** *Dark Depths* is the iconic state trigger but drags
in enters-with-N-counters, a counter-removal activated ability, indestructible,
and a legendary 20/20 token — almost none of it P4's axis. *Ideas Unbound* is a
delayed trigger that references no object, which would leave CR 603.7c (the
trigger that remembers a specific object) unbuilt. *Wound Reflection* ("each
opponent loses life equal to the life they lost this turn") is a richer history
reader but forces a life-change event category and a `LoseLife` opcode with no
second customer in this phase. *Aetherflux Reservoir* ("for each spell you've cast
this turn") is the natural spells-cast-history card but its second ability is a
pay-50-life cost, which is **P8**'s axis.

## 1. Scope

**In scope.** One turn-scoped event log with last-known-information payloads,
replacing both drain queues; per-consumer watermarks; step-beginning events (CR
603.2b); a widened trigger scan (CR 603.6a — *all* permanents are checked, not
only the newcomer); state triggers (CR 603.8) with derived re-arming; delayed
triggers (CR 603.7) declared on the card and armed by an opcode; intervening "if"
(CR 603.4 / 608.2a); the CR 603.3b ordering prompt, retiring M3f's elision; a
`Sacrifice` opcode (CR 701.21); a `CountSpec` arm for creatures that died this
turn.

**Out of scope, deferred with named expiries** (§8). Reflexive triggers (CR
603.12); leaves-the-battlefield *triggers* and the CR 603.10 look-back-in-time
list; stated-duration delayed triggers (603.7b); 603.2d, 603.2h, 603.7h, 603.9;
every event kind no gate card reads.

## 2. Architecture

### 2.1 One turn-scoped log replaces two drain queues

A new `Pawl.Type.GameEvent`, and in `GameState`:

```haskell
-- CR 608.2i: what happened this turn, in order. Appended by the change-and-emit
-- funnels; NEVER cleared by a reader. Cleared with its watermarks at turn handoff.
events :: Seq GameEvent,
-- CR 117.5: how far the trigger scan has consumed. Everything at or after this
-- index is unscanned.
scannedThrough :: Natural,
-- CR 704: how far the state-based-action damage read has consumed.
damageScannedThrough :: Natural,
```

`GameState.zoneChanges` and `GameState.damageEvents` are **removed**. Their
readers — `Engine.placePendingTriggers` and `Sba.performStateBasedActions` — take
the slice from their own watermark to the end and then advance it. Consumption
becomes an index bump; the record stays.

Two consequences worth stating because they are easy to get wrong:

- **Clearing is a turn-handoff act, not a cleanup-step act.** `Engine.handoffTurn`
  resets `events` and both watermarks together, alongside `turnNumber`. Clearing
  at the cleanup step (CR 514.1) would be wrong: cleanup is still part of *this*
  turn, and CR 514.1's discard is itself an event of it. The plan asserts the
  invariant that both watermarks equal the log's length at the moment of clearing
  — an unscanned event discarded at turn handoff is a lost trigger.
- **`Object.damage` is unaffected.** Marked damage is object state (CR 120.3) and
  stays where it is; the log's damage entries are the *event* record the
  deathtouch SBA reads, exactly as `damageEvents` was.

The arms P4 builds:

```haskell
data GameEvent
  = -- CR 400.7: an object moved between zones. Subsumes M3f's ZoneChange; the
    -- snapshot is the moved object as it last existed in `from` (CR 608.2h).
    Moved ZoneChange ProjectedCharacteristics
  | -- CR 510 / 608: damage dealt. Subsumes M2c's DamageEvent.
    DamageDealt DamageEvent
  | -- CR 603.2b: a phase or step began, on whose turn. The event a step trigger
    -- and a delayed "at the beginning of the next end step" both match.
    StepBegan Phase PlayerId
```

`ZoneChange` and `DamageEvent` survive as the payloads they already are; this is
a re-homing, not a redesign of either.

### 2.2 Last-known information is the payload — for semantic reasons

Each `Moved` entry carries a `ProjectedCharacteristics` snapshot of the moved
object **as it last existed in the zone it left**. CR 608.2h states the rule
directly (*"if it's no longer in that zone … the effect uses the object's last
known information"*), and CR 608.2i names the whole category this log serves:
*"Some effects look back in time and require information about previous game
states and actions rather than considering the current game state … they don't
need to be currently in the zone they were in at the time of that previous game
state or action."* Khabál Ghoul is that rule, printed on a card.

The alternative — store ids, re-derive characteristics from the printed card — is
**wrong on the rules**, not merely inconvenient:

- A land animated into a creature that dies *died as a creature*; its printed type
  line says otherwise.
- A **token has no printed card at all** (CR 111.3: its characteristics are the
  ones the creating effect defined). Tidal Wave's Wall token is a token, and
  Khabál Ghoul must count it.

This is the existing `DamageEvent.dealtByDeathtouch` posture — *"captured from the
projection at deal time, not re-derived — last-known information"* — generalized
from one hand-carved bit to the whole record, so the next history card adds no
field. It also pre-builds the substrate CR 603.10's look-back-in-time list will
need (git-bug `b998924`).

**What is deliberately *not* the argument.** `Pawl.Projection` imports
`Pawl.Quantity`, so a `Quantity.Count` cannot consult the projection, and reading
a snapshot happens to sidestep that. That is a **convenience, not a
justification**: the cycle is cuttable — parametrically, exactly as `Effect card`
already cuts the `Effect`/`Card` cycle by its own module comment, or by moving the
count evaluator. The snapshot earns its place on CR 608.2h/608.2i alone, and the
spec records that so a future reader does not "fix" the import graph and conclude
the snapshot was scaffolding.

### 2.3 The classification grows

```haskell
data TriggerCondition
  = SelfEnters                     -- CR 603.6a, existing
  | StepBegins Phase TurnScope     -- CR 603.2b
  | StateIs StateCondition         -- CR 603.8
```

`Pawl.Type.TurnScope` = `EachTurn | ControllersTurn`. Khabál Ghoul is "each end
step" (`EachTurn`); Sarcomancy is "your upkeep" (`ControllersTurn`, relative to
the ability's controller per CR 603.3a). Tidal Wave's delayed ability is
`EachTurn` — "the *next* end step" is any player's, and its once-ness comes from
the delayed store (CR 603.7b), never from the scope.

`Pawl.Type.StateCondition` is a hand-carved classification with two arms —
`YouControlNo Subtype` (Barbarian Outcast) and `NoPermanentsOfSubtype Subtype`
(Sarcomancy) — and a header noting it **EXPIRES at P9**, the posture `CountSpec`
already documents for card-shaped growth that a criterion language will retire
wholesale. Both arms read the **projection**: a subtype is layer 4 and control is
P1's layer 2, so a card that changed either must change the answer. They are
therefore evaluated in `Pawl.Event`, which already imports `Pawl.Projection`, and
never in `Pawl.Quantity`.

**Intervening "if" reuses the same type.** `TriggeredAbility` grows
`intervening :: Maybe StateCondition`. CR 603.4 checks it when the trigger event
occurs — the ability does not trigger at all if false — and CR 608.2a checks it
again on resolution, removing the ability from the stack if it has become false.
One predicate vocabulary, two customers; that reuse is why including 603.4 in this
phase is cheap.

**`CountSpec` grows one arm**, `CreaturesDiedThisTurn`: fold `events`, keep each
`Moved` whose `from` is the battlefield and whose `to` is a graveyard (CR 700.4
defines *dies* as exactly that), and count those whose **snapshot** says creature.

### 2.4 Delayed triggers: declared on the card, armed by an opcode

A delayed ability's payload is abilities-worth of effects, and `Effect` is
documented first-order and non-recursive, with `Effect → TriggeredAbility → Modal
→ Mode → Effect` a genuine module cycle. So the ability is **card data** and the
opcode only **arms** it:

- `Card.delayedAbilities :: [TriggeredAbility Card]`, keyed by a new
  `Pawl.Type.AbilityName` newtype — `newtype AbilityName = MkAbilityName Text`,
  exactly `SlotName`'s shape — named, never positional.
- `Effect.ArmDelayedTrigger AbilityName` — first-order, no new import, no new type
  parameter. It captures the resolving object's current binding environment, which
  is how "that card" / "it" (CR 603.7c) is remembered.
- `Effect.Create` grows a `Maybe SlotName` so the token it mints is bound and
  therefore referable by the ability armed in the same resolution. The binding is
  defined **only for a single-token create**; a `Create` whose `Quantity` exceeds
  one and which also names a slot is a **named deferral**, expiring at the first
  card that must refer back to several tokens at once ("sacrifice *them*" — Salt
  Road Skirmish). The `cardOffends` lint family in `Pawl.CardSpec` rejects that
  combination rather than silently binding one of them.

The store:

```haskell
-- CR 603.7: delayed triggered abilities awaiting their event. Concrete
-- TriggeredAbility Card, exactly as Source.OfTrigger already carries one.
delayedTriggers :: Seq DelayedTrigger
```

carrying `ability`, `source` and `controller` (CR 603.7d–f: for a spell, the
player who controlled it *as it resolved*), and the captured `bindings`. An entry
is removed as it fires — CR 603.7b's "only once, the next time its trigger event
occurs". CR 603.7a's rule that a delayed ability does not trigger on an event that
happened before it was created falls out for free: the arming resolution appends
to the store, and the scan only ever matches events at or after the watermark.

The cost of this shape, stated honestly: a card's text lives in two fields joined
by a name — and the join is what could rot. It does not, because the seam is
already policed: `SlotName`'s own comment records that *"the dataflow lint (test
suite) checks every reference resolves, so a dangling name is a failing test,
never a silent no-op."* `AbilityName` joins that lint family, so an
`ArmDelayedTrigger` naming an ability the card does not declare is a **failing
test**, not a trigger that never fires. The rejected alternatives were a self-recursive `Effect` (structurally
recursive and depth-unbounded, against design.md §1's static-analyzability claim)
and a second type parameter `Effect card ability` (correct, but wide mechanical
churn across library, tests and codec for one opcode).

### 2.5 The scan at the CR 117.5 boundary

`Pawl.Event` remains the **sole** home of `case … TriggerCondition`, and becomes
the sole home of `case … StateCondition`. One function gathers pending triggers
from three sources:

1. **Event-matched.** For each unscanned event, every object **on the
   battlefield** whose projected triggered abilities match. This widens M3f's
   scan, which only ever inspected the newcomer's own abilities: CR 603.6a
   requires that *"all permanents on the battlefield (including the newcomers) are
   checked"*, and a step trigger belongs to a permanent that has nothing to do
   with the event. The battlefield is the only scanned zone; abilities that
   function from a graveyard, hand or exile are a **named deferral**, expiring at
   the first such card.
2. **State-matched (CR 603.8).** Every object whose `StateIs` condition is
   currently true and which has no instance already on the stack. **Armed-ness is
   derived, not stored:** an instance is suppressed while a matching
   `Source.OfTrigger` object sits on the stack. CR 603.8's own words are *"doesn't
   trigger again until the ability has resolved, has been countered, or has
   otherwise left the stack"* — all three are "no longer on the stack", so
   re-arming is free and there is no bookkeeping field to leak. There is no
   triggered-but-not-yet-placed window to worry about: `placePendingTriggers` puts
   them on the stack within the same settle step.
3. **Delayed (CR 603.7).** Store entries whose condition matches an unscanned
   event, removed as they fire.

Intervening "if" filters the gathered list here (CR 603.4). Then the watermark
advances. `settleForPriority`'s existing three-boolean fixpoint guard is
unchanged in shape.

### 2.6 CR 603.3b ordering — the elision dies here

M3f elided the ordering choice with a documented expiry: *"M3f has at most one
trigger controlled by one player, so the ordering is trivial and the
own-order/two-part choice (CR 603.3b) is elided until a second simultaneous
trigger exists."* P4 creates that second trigger in a single line of card text, so
the expiry falls due, and "the engine makes no choices" outranks the plan
(umbrella §2).

`Prompt.OrderTriggers` (with its `Response` arm, codec, and replay round-trip),
asked of a player only when they control **two or more** pending triggers —
where the rules leave nothing to ask, don't prompt. Its payload is that player's
pending triggers as an **indexed list** in a canonical order, each entry carrying
its source `ObjectId`, and the answer is a permutation of those indices; the
ordering is positional by necessity — unlike a target slot, two triggers from one
source are genuinely indistinguishable, so any permutation among identical entries
is equivalent. Validation rejects an answer that is not a permutation of exactly
those indices (reject-not-repair, as payment already does). `Engine.apnapOrder` already
implements CR 101.4's APNAP pass and stays. CR 603.3b's *two-part* process (first
triggers whose condition is not another ability triggering, then the rest) is
vacuous while no condition triggers on another ability triggering; the code
carries the note rather than the machinery.

### 2.7 Serialization

`GameEvent`, `TurnScope`, `StateCondition`, `AbilityName` and `DelayedTrigger`
each need `Pawl.Codec` coverage and a round-trip property, as do the new
`TriggerCondition` arms, the new `Effect` arms, `Card.delayedAbilities`, and
`Prompt.OrderTriggers`/its `Response`. The log and the watermarks are part of
`GameState` and therefore part of replay.

## 3. The two invariants

**The rules core reads a classification, never an identity.** `TriggerCondition`,
`StateCondition` and `GameEvent` are classifications; `Pawl.Event` is their sole
casing home, the same sole-home pattern `Pawl.Resolve` holds for `Effect` and
`Pawl.Projection` for `Modification`. No new module cases on a card.

**The engine makes no choices.** P4 *removes* an elision rather than adding one:
CR 603.3b ordering becomes a prompt. The one place the engine still decides
without asking is the order of triggers within a single player's set when that
player controls exactly one — which is not a choice.

## 4. What this phase does not touch

- **The layer system.** Cluster 1 closed at P3b; P4 adds no `Modification`, no
  layer, and no projection pass. It *reads* the projection (in `StateCondition`
  and when snapshotting) and never writes it.
- **Replacement effects.** CR 614/616 and the monadic path are **P5**. The funnel's
  purity (M3f: `changeZone` stays `GameState -> GameState`) is unchanged.
- **Durations.** Event-relative and conditional durations are **P6**, and they read
  this log — that is the dependency edge, discharged here.
- **Player-scoped continuous effects.** `PlayerEffect` is **P7**.
- **`Object.damage`, `Sickness`, combat.** Untouched apart from `Sba`'s read
  becoming a watermarked slice.

## 5. Cards and tests

Every gate is a **gameplay-level** test: cast or play through the stack and assert
on game state (umbrella §1's definition-of-done), not a unit test of the scan.

**The centerpiece scenario — the four falsifiers that interlock.** Tidal Wave's
delayed sacrifice and Khabál Ghoul's counter both trigger at the beginning of the
*same* end step, under one controller. Therefore:

- the controller must **order** them (CR 603.3b) — an engine that picks silently
  fails the invariant;
- the order **changes the answer**, because CR 608.2h determines the count when the
  effect is applied: sacrifice first and the Ghoul counts the token, count first
  and it does not;
- the thing counted is a **token with no printed card**, so an implementation that
  re-derives card types from print instead of from the snapshot reads zero (§2.2);
- and the deaths being counted happened at a boundary the trigger scan already
  passed, so an implementation that reads a drained queue also reads zero (§2.1).

**Per-card falsifiers.**

- **Barbarian Outcast**, cast with no Swamps in play: exactly **one** trigger
  reaches the stack, not one per priority boundary. This is the flooding falsifier
  CR 603.8's second sentence exists to prevent, and it is the phase's sharpest
  single test. A companion test plays a Swamp first (no trigger), then destroys it
  (trigger).
- **Barbarian Outcast**, re-arm: with the state still true, an instance removed
  from the stack triggers again — the derived armed-ness of §2.5 read forwards.
- **Tidal Wave**: the token is sacrificed at the beginning of the next end step,
  exactly once; a second end step does not re-fire (CR 603.7b). Cast during the
  end step itself, it fires at the *next* one (CR 513.1a's errata is why the
  wording is "the next end step").
- **Tidal Wave**, object gone: if the token has already left the battlefield, the
  delayed ability does nothing (CR 603.7c) and is still consumed.
- **Khabál Ghoul**: creatures that died earlier in the turn are counted; the count
  resets at turn handoff, not at the trigger scan.
- **Sarcomancy**: with its Zombie token alive, the upkeep trigger does not trigger
  at all (CR 603.4); with the token gone, it triggers and deals 1 damage; and with
  a Zombie created **in response** to the trigger, it resolves doing nothing (CR
  608.2a). The third case is the one that distinguishes an intervening "if" from a
  plain condition, and it is the reason both check sites exist.

**Rulings discipline** (design.md §4): each card's Scryfall rulings are read
before its test is written, and any ruling the engine cannot yet honour is
recorded as a named deferral rather than quietly ignored.

## 6. Module & type changes (summary)

**New types.** `Pawl.Type.GameEvent`, `Pawl.Type.TurnScope`,
`Pawl.Type.StateCondition`, `Pawl.Type.AbilityName`, `Pawl.Type.DelayedTrigger`.

**Changed types.** `TriggerCondition` (+`StepBegins`, +`StateIs`);
`TriggeredAbility` (+`intervening`); `Card` (+`delayedAbilities`); `Effect`
(+`ArmDelayedTrigger`, +`Sacrifice SlotName`; `Create` grows a `SlotName`);
`CountSpec` (+`CreaturesDiedThisTurn`); `GameState` (+`events`,
+`scannedThrough`, +`damageScannedThrough`, +`delayedTriggers`; −`zoneChanges`,
−`damageEvents`); `Prompt`/`Response` (+`OrderTriggers`).

**Changed modules.** `Pawl.Event` (the scan; sole casing home for the two new
classifications; funnels append to `events`); `Pawl.Engine` (step-begin emission,
watermarked scan, the ordering prompt, log clearing at turn handoff); `Pawl.Sba`
(watermarked damage read); `Pawl.Resolve` (the two new opcodes; the CR 608.2a
re-check); `Pawl.Quantity` (`CreaturesDiedThisTurn`); `Pawl.Codec`;
`Pawl.Projection` (unchanged in behaviour, read by `StateCondition`).

**One opcode decision recorded.** `Sacrifice SlotName` (CR 701.21/701.21a) serves
both Barbarian Outcast's "this creature" and Tidal Wave's "it", with the trigger's
**source object bound into a reserved slot at placement** so "this creature" is
expressible. The rejected alternative was a targetless `SacrificeSelf` on
`RegenerateSelf`'s precedent *plus* a slotted `Sacrifice` — two opcodes for one
keyword action, and "this creature" recurs far too often to keep paying for it.
Note CR 701.21a explicitly: sacrificing is **not** destroying, so it does not
consult regeneration shields or indestructible.

## 7. Ordering within the phase (for the plan)

Substrate before readers, and the cheapest falsifier first:

1. Verify all four cards against Scryfall (including rulings) and write the card
   fixtures' data.
2. The log: `GameEvent`, `GameState` fields, funnels appending, both readers
   watermarked, clearing at turn handoff. No behaviour change — the existing suite
   is the regression test.
3. `StepBegan` emission and `StepBegins` matching; the widened scan (CR 603.6a).
4. `StateCondition`, state triggers, derived re-arming → **Barbarian Outcast**.
5. `Sacrifice`, source-bound slot → completes Barbarian Outcast.
6. `CountSpec.CreaturesDiedThisTurn` → **Khabál Ghoul**.
7. Delayed triggers: `AbilityName`, `Card.delayedAbilities`,
   `ArmDelayedTrigger`, `Create`'s slot, the store → **Tidal Wave**.
8. `Prompt.OrderTriggers` + codec + replay → the centerpiece scenario.
9. Intervening "if" at both check sites → **Sarcomancy**.

Step 2 is the riskiest and is deliberately behaviour-preserving, so it lands
alone: if the watermark refactor is wrong, the existing suite says so before any
new card exists to confuse the diagnosis.

## 8. Deferred, with named expiries

- **Reflexive triggered abilities (CR 603.12/603.12a).** They follow the delayed
  rules but are created *during* a resolution and checked immediately against
  events earlier in that same resolution. **Expires** at the first "you may … When
  you do, …" card.
- **Leaves-the-battlefield triggers (CR 603.6c) and the look-back-in-time list (CR
  603.10/603.10a).** The log *records* deaths — Khabál Ghoul counts them — but no
  `TriggerCondition` matches a leave. **Expires** at the first dies-trigger card;
  relate to git-bug `b998924` (OfAbility LKI), whose substrate §2.2 builds.
- **Enters-then-dies-same-settle timing gap (CR 603.10, normal clause, not a
  look-back exception).** The scan runs once, at the CR 117.5 priority boundary,
  and derives its candidate set from the battlefield as it then stands. If a
  permanent enters and a state-based action puts it into a graveyard within the
  same settle (before the boundary's scan runs), its id is no longer on the
  battlefield and the scan never sees it, so its enters trigger is lost — even
  though CR 603.10's normal rule says objects existing immediately after the
  event (which this permanent did) are what get checked. Closing this needs the
  scan to evaluate candidates against the state at the time of each event rather
  than at the boundary. **Expires** at the first card that can die on entry (a
  creature entering with toughness 0 or less — e.g. an entering creature under a
  −X/−X effect, or a 0-toughness token).
- **Stated-duration delayed triggers (CR 603.7b).** One-shot only; a delayed
  ability with "this turn" would fire repeatedly until the turn ends. **Expires**
  at the first such card.
- **CR 603.2d** (an effect making an ability trigger additional times), **603.2h**
  ("do this only once each turn"), **603.7h** (a delayed ability keyed to the Nth
  resolution), **603.9** (triggers on a player losing the game). Each **expires**
  at its first card.
- **CR 603.3b's second part** — triggers whose condition is another ability
  triggering. **Expires** at the first such condition.
- **CR 400.7e's** zone-change trigger finding the new object in a public
  destination is honoured only as far as M3f already does (the `Moved` event
  carries the resulting object's id). **Expires** with the LTB pass.
- **Scanned zones other than the battlefield** (§2.5). An ability that functions
  from a graveyard, hand or exile is not scanned. **Expires** at the first such
  card.
- **A `Create` binding more than one token to a slot** (§2.4). **Expires** at the
  first card that refers back to several tokens at once ("sacrifice *them*").
- **Event kinds with no reader**: spell casts, attacks, life changes, counter
  placement. **VOCAB**; each is added by the phase that needs it (`SpellCast` at
  **P7**).
- **Unenforced sacrifice-control restriction (CR 701.21a).** `Event.sacrifice`
  doesn't check "a player can't sacrifice ... a permanent they don't control" --
  not wrong today, since its only caller (`Resolve`'s `Sacrifice` arm) reads
  `Binding.triggerSource` (CR 113.7, a triggered ability's own source), always
  controlled by whoever triggered it. **Expires** at the first effect that can
  name a permanent its controller doesn't control (an opponent-sacrifice edict,
  e.g. Diabolic Edict).
- **Trigger control read at the scan boundary, not at the trigger moment (CR
  603.3a).** `Event.eventTriggers` reads a triggered ability's controller via
  `Projection.controllerOf` at the CR 117.5 scan boundary, not at the moment the
  underlying event fired. Carried forward from M3f; unobservable today because
  nothing changes control between an event and the boundary. **Expires** at the
  first effect that can change control between an event and the boundary.
- **State-trigger non-termination if all modes are unfillable.** A state trigger
  whose modes are all unfillable would be removed from the stack (CR 603.3c) and
  re-trigger on the next settle pass while its condition still held, looping
  forever. No card in the pool can do that -- Barbarian Outcast's single mode has
  no target slots and is always fillable. **Expires** at the first state-triggered
  card whose modes can all be unfillable.
- **Two identical state triggers on one source conflated by Source equality.**
  `Event.stateTriggers`' suppression check compares `Object.source obj ==
  Source.OfTrigger srcId ab`; if a single source ever carried two textually
  identical `StateIs` abilities, this would conflate them into one value and
  suppress the second as though it were an instance of the first. No card in the
  pool has two identical state triggers on one source. **Expires** at the first
  such card.
- **Partial CR 514.3 cleanup-step handling.** `Engine.advance` settles once more
  at turn end so cleanup's turn-based-action events are scanned before
  `handoffTurn` clears the log, but does not build CR 514.3a's extra cleanup step
  and priority round -- a trigger placed here resolves at the next turn's first
  priority instead of during that cleanup. **Expires** at the first card whose
  triggered ability fires on a cleanup-step event and must resolve during that
  cleanup.
- **Delayed ability's intervening "if" pruned before it is checked (CR 603.4 /
  603.7b).** `Event.delayedPending` removes a fired entry from the delayed-trigger
  store based on the event match alone, before `gatherTriggers`'s CR 603.4
  intervening-"if" filter ever runs; if the condition is false, the entry is
  dropped from the pending list but has already spent its CR 603.7b single shot,
  instead of remaining armed for the trigger event's next occurrence.
  Unreachable today: Tidal Wave's delayed ability, the only one in the pool, has
  no `intervening`. **Expires** at the first delayed ability with an intervening
  "if".

## 9. Tracking

P4 is the umbrella's GAP-T. It does not close any standalone git-bug outright;
it **builds the substrate** two open ones sit on:

- `b998924` (OfAbility LKI) — §2.2's snapshot is the mechanism its fix will use;
  stays open, unretired.
- `6afb561` (M3f replacement seam, CR 616 ordering + choice-bearing replacements)
  → **P5**, untouched here. Note the family resemblance: P4 retires the *trigger*
  ordering elision (603.3b), P5 retires the *replacement* ordering elision (616).
  They are different rules and different mechanisms.

`c7a0077` (Quantity.Bound → SlotName) stays open and unretired: like P3b's two
arms, `CreaturesDiedThisTurn` needs no binding slot — it folds the log.

The umbrella's §3 table row for P4 should be marked landed, and its §4 ordering
note updated to say P6 and P7 are unblocked, when this phase completes.

## 10. Exit criterion

All four gate cards pass gameplay-level tests, including the centerpiece
ordering-and-counting scenario; the two drain queues are gone and no reader
clears the log; `cabal build` is warning-clean and `hooky run` passes. At that
point the event substrate is general enough that P6's event-relative durations
and P7's per-turn restrictions each add a *reader*, not a mechanism — which is
the whole reason P4 precedes them.
