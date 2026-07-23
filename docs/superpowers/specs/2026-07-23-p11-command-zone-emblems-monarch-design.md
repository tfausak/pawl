# M4.5 P11 — Command zone, emblems, and the monarch

*Design written 2026-07-23. Closes **GAP-Z** (the Command zone, with emblems as
its first resident) and the **monarch** customer of **GAP-S**. This is the last
remaining M4.5 phase. Umbrella:
`docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md` §3, row P11.
Issue: `tfausak/pawl#7`. Census: `docs/mtgish-gap-census.md` §3.4/§3.5.*

## 0. Why this phase, and what it proves

pawl's `Zone` has six members (Library, Hand, Graveyard, Battlefield, Stack,
Exile). The comprehensive rules define **seven** game zones (CR 400.1); the
**Command** zone is absent. The Command zone is "a game area reserved for certain
specialized objects that have an overarching effect on the game, yet are not
permanents and cannot be destroyed" (CR 408.1). Its first resident outside the
casual variants is the **emblem** (CR 408.2 / 114).

This phase closes two axes that are, structurally, **separable** — the umbrella
bundles them only because each is small:

- **The Command zone + emblems (GAP-Z).** An emblem is an object with one or more
  abilities and *no other characteristics* (CR 114.1/114.3), living in the command
  zone, whose abilities function *there* (CR 114.4 / 113.6p). The single structural
  claim GAP-Z must prove is that a **static ability functions from outside the
  battlefield** — that pawl's continuous-effect projection can gather a static
  ability whose source is in the command zone, not on the battlefield.
- **The monarch (a customer of GAP-S).** The monarch is a *player designation*
  (CR 725.1), and it needs **nothing** from the command zone — it is a
  player-state axis in its own right. Its structural novelty is CR 725.2's **two
  inherent triggered abilities that have no source**: pawl has never carried an
  ability that belongs to no object.

The two are driven by one real gate card (Palace Jailer, monarch) and one labeled
synthetic source (an emblem-granting fixture), because — see §0's note on the
emblem source — the real card pool has no clean non-planeswalker emblem source.

### What this phase is *not*

It is **not** the whole of the command zone's eventual tenancy. Commander,
Planechase, Vanguard, Archenemy and Conspiracy Draft all seat cards in the command
zone (CR 408.3); **all are out of scope** (design.md §6: "draft / Conspiracy
(second VM)"; the other variants are unroadmapped casual formats). It is **not**
command-zone *casting* (Commander tax, CR 903.8) — no gate card needs it. It is
**not** dungeon/plane/scheme/vanguard/conspiracy card residents (CR 309–315),
each a separate subsystem.

On the GAP-S side it is **not** day/night (the umbrella's "designer-picks" — a
separate subsystem with no P11 gate card), **not** the Ring / Ring-bearer (its own
subsystem, census §3.4 backlog), **not** the initiative/undercity, venture, speed,
or experience/rad counters. The monarch alone is the GAP-S customer this phase
carries; the rest ride P10's player-state substrate and P4's triggers when a card
demands them.

### The emblem source, and why it is synthetic

Nearly every emblem in Magic is created by a **planeswalker ultimate**. pawl does
not model planeswalkers (a whole card type — loyalty counters, loyalty abilities,
the planeswalker uniqueness rule), and modelling one would dwarf GAP-Z, which the
umbrella calls "small." The only tournament-legal *non*-planeswalker emblem sources
are **the Ring** (a deferred subsystem) and draft-only **Conspiracy** cards (out of
scope). So P11 drives the emblem gate with a **labeled synthetic source** — a
test-only fixture that resolves `Effect.CreateEmblem` — carrying a *real,
recognizable* static ability (an Elspeth-style anthem, "creatures you control get
+1/+1"). This is the project's sanctioned "synthetic fixture as a labeled, expiring
crutch" pattern (memory `tests-prefer-real-cards`; cf. the synthetic GainControl of
issue #82). Its expiry: it is retired when planeswalkers or the Ring land and a
real card can mint the emblem. An issue carries that expiry (§8).

### Gate cards

- **Palace Jailer** — `{2}{W}{W}`, Creature — Human Soldier, 2/2.
  - "When this creature enters, you become the monarch."
  - "When this creature enters, exile target creature an opponent controls until
    an opponent becomes the monarch."
- **Synthetic emblem source** — a fixture (not a real card) whose resolution is
  `Effect.CreateEmblem` with one static ability, "creatures you control get
  +1/+1."

Palace Jailer's cost, type line, P/T and Oracle text were read from Scryfall on
2026-07-23 (Conspiracy: Take the Crown and its reprints), not recalled — the
recalled stats were wrong (1/1 for {3}{W}), which is precisely the failure mode
CLAUDE.md warns against. The plan re-reads the printed text at implementation time.

### The falsifiers, stated up front

- **The monarch's inherent triggers belong to no object.** CR 725.2: "These
  triggered abilities have no source." A test asserts that with a monarch and *no
  permanents on the battlefield at all*, the end-step draw still fires — it cannot
  be hung on a bearer.
- **The steal trigger hands the crown to the damaging creature's controller, not
  the ability's controller.** CR 725.2: "its controller becomes the monarch." The
  ability is *controlled by the current monarch* (CR 725.2) but makes a *different*
  player the monarch. A test has player B's creature deal combat damage to monarch
  A and asserts **B** — the creature's controller — becomes the monarch.
- **The end-step draw fires only on the monarch's own end step.** A test with the
  monarch as the non-active player asserts *no* draw at the active player's end
  step, and a draw at the monarch's.
- **Palace Jailer's exile ends *exactly* when an opponent becomes the monarch —
  not at end of turn, not never.** The duration is a designation-change condition,
  not a fixed point. A test keeps the creature exiled across a turn boundary while
  the caster stays monarch, then returns it the instant an opponent takes the crown
  (proving the P6 Expiry, not an `UntilEndOfTurn` stand-in).
- **An emblem's static ability radiates from the command zone, not the
  battlefield.** A test wipes the battlefield (every permanent destroyed), then
  creates a fresh token, and asserts the emblem's anthem still buffs it — the
  emblem is untouched by a battlefield sweep because it is not a permanent
  (CR 114.5) and not on the battlefield.

## 1. Scope

**Adds (GAP-Z):** `Zone.Command`; a `command` object collection on `GameState`; an
emblem object (`Source.OfEmblem`); `Effect.CreateEmblem`; a command-zone pass in
the continuous-effect projection so an emblem's static ability functions from the
command zone.

**Adds (monarch / GAP-S):** a `monarch :: Maybe PlayerId` designation on
`GameState`; `Effect.BecomeMonarch` (with a `MonarchTarget` naming who); a
`GameEvent.BecameMonarch`; the two inherent sourceless triggered abilities of
CR 725.2, carried by a new `Source.OfInherentTrigger`; a new `TriggerCondition`
for "a creature dealt combat damage to the monarch"; a monarch-keyed `Expiry`
condition for Palace Jailer's exile.

**Adds (cards/tests):** Palace Jailer (`data/cards/palace-jailer.json`); a
synthetic emblem-source fixture; their gameplay-level tests; codec/setup wiring.

**Does not add:** command-zone casting; Commander/Planechase/Vanguard/Archenemy/
Conspiracy residents or the second-VM draft; dungeon/plane/scheme/vanguard cards;
day/night; the Ring; the initiative; venture; speed; experience/rad counters; the
counter→layer-6 path (P10-deferred); CR 725.4 monarch-reassignment on a player
leaving the game (§8).

## 2. Architecture

### 2.1 `Zone.Command` and `GameState.command`

`Zone` gains a seventh constructor, `Command` (CR 400.1). Zones in pawl are stored
as per-zone collections on `GameState` keyed into the shared `objects` map;
`command` is a `Set ObjectId` (shared across players, like `battlefield`/`exile`;
CR 400.1 — the command zone is not per-player). The `Zone` value on an emblem
`Object` is `Command`; every existing case-on-`Zone` site (codec, setup,
zone-change) gains the `Command` arm.

### 2.2 The emblem: `Source.OfEmblem Card`

CR 109.1 lists an emblem as one of the kinds of object; CR 109.4c: "An emblem is
controlled by the player who puts it into the command zone." So an emblem is an
`Object`, exactly as a token is, distinguished by its `Source`. `Source` gains:

```
| -- CR 114: an emblem -- an object in the command zone whose only
  -- characteristics are its abilities (CR 114.3). Like OfToken, its
  -- characteristics ARE a Card (name/types/cost/color all empty; only the
  -- ability lists populated), so Game.cardOf reads it with no special case.
  -- Unlike a token it is never a permanent (CR 114.5) and never on the
  -- battlefield, so no P/T / SBA / combat / counter machinery touches it.
  OfEmblem Card
```

Reusing `Card` (as `OfToken Card` already does) means the projection, `cardOf`,
and the ability machinery read an emblem uniformly. The emblem's `Object` carries
the standard per-incarnation fields; all are inert for an emblem (it is never
tapped, damaged, or countered), which is harmless — the fields exist, nothing reads
them for a command-zone object. `owner` and control are the creating player
(CR 114.2).

### 2.3 `Effect.CreateEmblem`

A new opcode:

```
| -- CR 114.2: "[Player] gets an emblem with [ability]." Puts an emblem owned and
  -- controlled by the resolving controller into the command zone. Targetless: the
  -- beneficiary is always the resolving controller (CR 114.2), the same shape as
  -- P10's GainPlayerCounters. The abilities are carried as a Card so the emblem
  -- reuses the whole ability pipeline.
  CreateEmblem Card
```

Resolution mints a fresh `ObjectId`, builds an `Object` with `source = OfEmblem
card`, `zone = Command`, `owner = controller`, a fresh timestamp, and inserts it
into `objects` and `command`. Timestamp matters: an emblem's static continuous
effect shares the emblem's entry timestamp (CR 613.7a), read by the projection when
ordering layers.

### 2.4 The projection gathers command-zone static abilities

`Pawl.Projection.gather` today walks `GameState.battlefield` for permanents' static
abilities (`fromPermanent`). It gains a symmetric pass over `GameState.command`:

```
fromEmblem emblemId = ... (Card.staticAbilities card), timestamped by the emblem's
                      Object.timestamp, gAffected from each StaticAbility.affected
```

structurally identical to `fromPermanent` minus the `SetLandSubtype` liveness
machinery (an emblem has no basic-land-type text to rewrite and cannot be stripped
of abilities — nothing in scope removes an emblem's abilities). The gathered
effects join `stored ++ static_ ++ counterGathered`. This is the **sole** GAP-Z
structural change to the rules core; everything else is zone plumbing. An emblem's
anthem (`affected = ControlledBy <controller>`, a layer-7c `ModifyPowerToughness`)
then folds onto the controller's creatures through the same path as any battlefield
anthem, with the emblem's timestamp.

### 2.5 The monarch designation: `GameState.monarch`

`GameState` gains `monarch :: Maybe PlayerId`. `Nothing` until a player becomes the
monarch (CR 725.1); at most one player at a time (CR 725.3). No monarch at game
start.

### 2.6 `Effect.BecomeMonarch` and `MonarchTarget`

Becoming the monarch is an effect (Palace Jailer's "you become the monarch" and the
steal trigger's "its controller becomes the monarch"). The two name *different*
players, so the opcode carries a small target:

```
data MonarchTarget = TheController        -- CR: "you become the monarch"
                   | ControllerOfSource   -- CR 725.2: "its controller becomes the monarch"
```

`Effect.BecomeMonarch MonarchTarget`. Resolution sets `monarch = Just p`, where `p`
is the resolving controller (`TheController`) or the controller of the object bound
as the trigger's source (`ControllerOfSource`, read from the steal trigger's
binding for the damaging creature). CR 725.3 is automatic: the previous monarch
ceases to be simply because `monarch` is overwritten. Setting the monarch **emits a
`GameEvent.BecameMonarch p`** (§2.8), which is what the steal trigger's duration and
any future monarch trigger key off.

*Why an enum and not a general player-spec:* pawl has no general "which player" spec
for effects yet (P10's player effects are targetless; #120 tracks the targeted
case). The two monarch cases are the entire need this phase; a two-constructor
`MonarchTarget` is the minimal honest shape. Generalizing to a player-spec axis is
deferred to whatever card first needs it.

### 2.7 The two inherent triggers: `Source.OfInherentTrigger`

CR 725.2's two abilities "have no source and are controlled by the player who was
the monarch at the time the abilities triggered." pawl's on-stack triggered ability
carries its source object (`Source.OfTrigger ObjectId (TriggeredAbility Card)`); an
inherent monarch trigger has no such object. `Source` gains:

```
| -- CR 725.2 (and future sourceless rules triggers): a triggered ability with no
  -- object source, controlled by a specific player baked in at trigger time. The
  -- monarch's two inherent abilities are the only customers today. Parallels
  -- DelayedTrigger, which likewise carries a controller and no live object.
  OfInherentTrigger PlayerId (TriggeredAbility Card)
```

The trigger **scanner** — the site that gathers which abilities may trigger on an
event — appends the monarch's two abilities **only while `monarch = Just p`**, each
as an `OfInherentTrigger p ability`. When there is no monarch they do not exist
(CR 725.1). The abilities:

- **End-step draw.** `condition = StepBegins (Ending End) <controller's turn>`,
  effect `Draw 1`. It fires at the beginning of the monarch's own end step: the
  ability's controller is the monarch, and the scope restricts to that player's
  turn, giving exactly "the monarch's end step" (CR 725.2). It is a genuine
  triggered ability — it uses the stack and can be responded to — not a turn-based
  action.
- **Combat-damage steal.** `condition = CreatureDealtCombatDamageToMonarch` (§2.9),
  effect `BecomeMonarch ControllerOfSource`.

Because the ability is a `TriggeredAbility Card` exactly like any other, it shares
Resolve's executor; only the *gathering* (synthesize from `monarch`) and the
*source* (`OfInherentTrigger`) are new.

### 2.8 The steal trigger condition and `GameEvent.BecameMonarch`

`TriggerCondition` gains a constructor for "a creature dealt combat damage to the
monarch" (CR 725.2). Unlike `SelfDealsCombatDamageToPlayer` (bearer-scoped, P10),
this is **not** bearer-scoped — it fires for *any* creature, and the damaged player
must be the monarch. It rides P4's damage-event history (a `DamageDealt` event
already records source and recipient); the match is "recipient == monarch and
source is a creature," binding the source so `ControllerOfSource` can read it.

`GameEvent` gains `BecameMonarch PlayerId`, emitted by `Effect.BecomeMonarch`. It
is the event Palace Jailer's exile duration keys off, and the substrate for any
future "whenever a player becomes the monarch" trigger.

### 2.9 Palace Jailer's exile duration: a monarch-keyed `Expiry`

"Exile target creature an opponent controls until an opponent becomes the monarch"
is an exile whose reversal is scheduled by a **P6 `Expiry`**, keyed to the monarch
designation. The condition is "an opponent (of the effect's controller) is the
monarch." P6's `Expiry` already carries conditional and event-relative durations;
this adds the monarch-designation predicate to that vocabulary. When the condition
is met, the existing exile→battlefield return path (the same one M4/P-series zone
changes use) brings the creature back under its owner's control. The exile itself
is the existing targeted-exile effect; only the *duration* is new.

CR 725.5 (a static effect keyed to "the monarch" does nothing while there is no
monarch) is not exercised by these gate cards — Palace Jailer establishes a monarch
before its own duration is evaluated — and is noted in §8.

### 2.10 Serialization

Every new type and constructor gets `Pawl.Codec` encode/decode arms as the JSON
data boundary (never to decide behaviour): `Zone.Command`, `Source.OfEmblem`,
`Source.OfInherentTrigger`, `Effect.CreateEmblem`, `Effect.BecomeMonarch`,
`MonarchTarget`, the new `TriggerCondition`, `GameEvent.BecameMonarch`, the monarch
`Expiry` condition, and `GameState.command` + `GameState.monarch`. Setup seeds
`command = Set.empty` and `monarch = Nothing`.

## 3. The two invariants

- **The rules core never cases on an effect's identity.** `Effect.CreateEmblem`
  and `Effect.BecomeMonarch` are opcodes in the open half; the closed half sees
  them only through their classification (a resolution effect). The projection
  gathers an emblem's static abilities by the *same* uniform walk it uses for
  permanents — it does not ask "is this an emblem." The monarch triggers are
  gathered by their *presence* (`monarch = Just p`), not by casing on a card.
- **The engine makes no choices it should not, and asks for none it need not.**
  Palace Jailer's exile targets (a real choice — CR 115). Becoming the monarch is
  automatic (no choice). The end-step draw and the steal are mandatory. No elision
  is introduced.

## 4. What this phase does **not** touch

The battlefield/permanent machinery (SBAs, combat, P/T layers beyond the anthem
fold) is untouched — an emblem never enters it. The layer *system* gains no new
layer and no new `Modification`: an emblem's anthem is an existing layer-7c
`ModifyPowerToughness`; the only change is *where* the projection looks for it
(§2.4). `Player` is untouched — the monarch lives on `GameState`, not `Player`,
because CR 725.3 makes it a single game-wide designation, not a per-player counter
(contrast P10's per-player counter map).

## 5. Cards and tests

### Rulings discipline (design.md §4)

Palace Jailer's printed text is re-read from Scryfall at implementation time; the
monarch reminder text and both "when this creature enters" clauses are transcribed
verbatim into the card JSON, and every CR number in this spec (114, 400.1, 408,
725) is re-checked against `docs/rules.txt` before it drives code.

### Palace Jailer — `data/cards/palace-jailer.json`

`{2}{W}{W}`, Creature — Human Soldier, 2/2, two ETB triggered abilities: (1)
`BecomeMonarch TheController`; (2) exile target creature an opponent controls, with
the monarch-keyed `Expiry`. Tests: the monarch designation is set on ETB; the exile
targets only opponent-controlled creatures (CR 115) and returns exactly on the
opponent-becomes-monarch event; the steal trigger transfers the crown on combat
damage.

### Synthetic emblem source — fixture, not a card file

A test-only source resolving `CreateEmblem` with one static ability ("creatures you
control get +1/+1"). Labeled as a synthetic crutch (§8). Tests: the emblem lands in
the command zone owned/controlled by the resolver; its anthem buffs the resolver's
creatures; the buff survives a battlefield wipe and applies to a token created
afterward; the buff is scoped to the controller's creatures (an opponent's creature
is unaffected).

### Codec

Round-trip tests for every new constructor in §2.10 in the subsystem specs that
mirror the touched modules (`Pawl.ZoneSpec`, `Pawl.SourceSpec`, `Pawl.EffectSpec`,
`Pawl.TriggerConditionSpec`, `Pawl.GameEventSpec`, and the projection/monarch
gameplay tests in their subsystem specs).

## 6. Module and type changes (summary)

- `Pawl.Type.Zone` — add `Command`.
- `Pawl.Type.Source` — add `OfEmblem Card`, `OfInherentTrigger PlayerId
  (TriggeredAbility Card)`.
- `Pawl.Type.Effect` — add `CreateEmblem Card`, `BecomeMonarch MonarchTarget`.
- `Pawl.Type.MonarchTarget` — **new** (`TheController` | `ControllerOfSource`).
- `Pawl.Type.TriggerCondition` — add the "creature dealt combat damage to the
  monarch" constructor.
- `Pawl.Type.GameEvent` — add `BecameMonarch PlayerId`.
- `Pawl.Type.GameState` — add `command :: Set ObjectId`, `monarch :: Maybe
  PlayerId`.
- `Pawl.Type.Expiry` (P6) — add the monarch-designation condition.
- `Pawl.Projection` — add the command-zone `fromEmblem` gather pass.
- The trigger scanner — synthesize the two inherent monarch abilities while
  `monarch = Just p`.
- `Effect` resolution — `CreateEmblem` and `BecomeMonarch` executors; the latter
  emits `BecameMonarch`.
- `Pawl.Codec`, `Pawl.Setup` — the arms and seeds of §2.10.
- Every existing case-on-`Zone` / case-on-`Source` / case-on-`Effect` /
  case-on-`TriggerCondition` / case-on-`GameEvent` site gains its new arm
  (`cabal build` warning-clean enforces exhaustiveness).

## 7. Ordering within the phase (for the plan)

1. `Zone.Command` + `GameState.command` + codec/setup (the zone exists, empty).
2. `Source.OfEmblem` + `Effect.CreateEmblem` + resolution (an emblem can be
   minted, inert).
3. `Projection` command-zone pass (the emblem's anthem functions) + the synthetic
   emblem-source test — **closes GAP-Z**.
4. `GameState.monarch` + `Effect.BecomeMonarch`/`MonarchTarget` +
   `GameEvent.BecameMonarch` + codec/setup.
5. `Source.OfInherentTrigger` + the trigger scanner synthesizing the two abilities;
   the end-step-draw test.
6. The steal `TriggerCondition` + the steal test.
7. Palace Jailer's ETB #1 (become monarch) + the monarch-keyed `Expiry` + ETB #2
   (exile-until) + the Palace Jailer tests — **closes the monarch (GAP-S)**.

Each step is one small complete commit; TDD per CLAUDE.md.

## 8. Deferred, with named expiries

Each becomes a tracked issue at phase kickoff, carrying its status, rationale and
expiry trigger (CLAUDE.md "File the issue, cite it inline"):

- **The synthetic emblem source** — a labeled crutch; expires (`expires:card-driven`)
  when planeswalkers or the Ring land and a real card can mint an emblem.
- **Command-zone casting** (Commander tax, CR 903.8) — no gate card; `expires:card-driven`.
- **CR 725.4 monarch reassignment on a player leaving the game** — pawl is
  multiplayer-capable, but this edge fires only when a player leaves; no gate card.
  `expires:card-driven` (a card/format that removes a player). Related to #87.
- **CR 725.5** (a static effect keyed to "the monarch" with no monarch present) —
  no gate card exercises it; `expires:card-driven`.
- **Day/night** (GAP-S) — separate subsystem, umbrella "designer-picks"; backlog.
- **The Ring / Ring-bearer, initiative, venture, speed, experience/rad counters**
  (GAP-S) — ride P10's substrate + P4's triggers; backlog (census §3.4).
- **Other command-zone residents** (dungeon/plane/scheme/vanguard/conspiracy,
  CR 309–315; Commander/Planechase/Vanguard/Archenemy/Conspiracy variants,
  CR 408.3) — each its own subsystem/format; design.md §6 out-of-scope or backlog.
- **The counter→layer-6 ability-granting path** — already P10-deferred (#116);
  unchanged here.

## 9. Tracking

Umbrella issue `tfausak/pawl#7`. On completion, replace the CLAUDE.md status
bullet (never append) and add the milestone-completion-log entry to
`docs/progress.md`. The deferral issues above are filed at kickoff and cited at
their code sites.

## 10. Exit criterion

P11 lands when: (a) `Zone.Command` exists with an emblem resident whose **static
ability functions from the command zone** (the projection gather pass), proven by
the synthetic-source test surviving a battlefield wipe; and (b) the **monarch**
exists as a `GameState` designation with its two **sourceless** inherent triggers
and Palace Jailer as the real gate — the crown transfers on combat damage, the
monarch draws on their end step, and the exile returns exactly when an opponent
takes the crown. At that point **every** closed-half axis the census flagged GAP
(the eleven M4.5 phases) has an axis in the type system and a real (or, for
emblems, a sanctioned labeled-synthetic) gate card with a passing gameplay-level
test — **M4.5 is complete**, and M5's nightmares (723/727/729) sit on complete
axes (umbrella §5).
