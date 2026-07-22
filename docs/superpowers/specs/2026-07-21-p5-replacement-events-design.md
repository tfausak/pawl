# M4.5 P5 — Replacement events, and the choice the fold cannot make

*Design pass 2026-07-21. The sixth phase of the M4.5 umbrella
(`docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`), closing
**GAP-R** (replacement event coverage) together with the **CR 616
ordering/choice** facet that git-bug `6afb561` documented. The second phase of
Cluster 2, following P4's event substrate. Gates: **Hardened Scales +
Corpsejack Menace**, **Doubling Season + Dragon Fodder**, **Clone + Primal
Plasma**. This spec is implementable; a `writing-plans` plan follows it.*

*CR citations below **were checked against `docs/rules.txt` during this design
pass**: 101.4, 117.5, 122.6, 122.6a, 201.5, 201.5c, 208.2b, 400.7, 608.2h, 613.1a,
614.1, 614.1a, 614.1b, 614.1c, 614.1d, 614.3, 614.4, 614.5, 614.6, 614.7,
614.7a, 614.8, 614.10, 614.11, 614.12, 614.12a, 614.12b, 614.13, 614.13a,
614.13b, 614.15, 614.16, 614.17, 615.1, 615.3, 615.6, 615.7, 615.10, 615.12,
615.13, 616.1, 616.1a, 616.1b, 616.1c, 616.1d, 616.1e, 616.1f, 616.1g, 616.2,
701.19a, 704.5f, 704.5g, 707.2, 707.2a, 707.9a. Numbers marked **(verify)** were not,
and must be checked before they drive code (CLAUDE.md: never trust recalled
Magic rules).*

*Card text and Gatherer rulings for every gate card **were verified live against
the Scryfall API during this design pass**, not read from the vendored MTGJSON
dump (`card-data-source`). The oracle text and rulings quoted in §5 are what
Scryfall returned.*

## 0. Why this phase, and what it proves

Four facts about the engine as it stands:

1. **There are four separate replacement-ish paths, and none of them can ask a
   question.** `Event.applyReplacements` is a pure left-to-right `foldl'` over a
   `ZoneChange`. `Event.applyPreventions` is a pure `filter` over a
   `[DamageEvent]`. Regeneration is a `Map ObjectId Natural` consulted by
   `Event.destroy` with a hardcoded precedence. The as-enters copy choice is a
   marker in `Object.bindings` drained after the fact by
   `Engine.drainAsEntersChoices` — whose own comment already names itself *"the
   narrow first version of P5's monadic replacement engine."*
2. **`ReplacementEffect` and `Prevention` have one constructor each, and both
   are card-shaped.** `RedirectZoneChange Graveyard Exile` *is* Rest in Peace;
   `PreventAllCombatDamage` *is* Fog. The census (§3.1) counts ~40 replaceable
   base event classes behind mtgish's ~130 `Would…` families. pawl has wired two.
3. **CR 616.1 is not an ordering prompt — it is a loop.** Read it literally:
   choose *one* applicable effect from the highest non-empty of five ordered
   buckets, apply it, then *"this process is repeated (taking into account only
   replacement or prevention effects that would now be applicable) until there
   are no more left to apply"* (616.1f). CR 616.2 adds that an effect can
   *become* applicable because another one modified the event. A `foldl'` over a
   list computed once is structurally incapable of either.
4. **The engine makes choices it must not.** With two applicable replacements the
   fold silently picks list order. That is the second invariant violated in the
   plainest possible way, and unlike an elision it carries no expiry because
   nothing detects it.

P5 replaces all four paths with **one** monadic path over **one** proposed-event
vocabulary, driven by the CR 616.1 loop.

### What this phase is *not*

Not the ~40 event classes. It wires **six**, chosen so that every *rewrite shape*
the taxonomy needs has a real producer: redirect (zone change), cancel (damage),
scale (counters, tokens), and modify-how-it-enters (entry). The other ~34 are
**VOCAB** (census §5): each is a new `ProposedEvent` arm plus the funnel that
raises it, riding this axis rather than reshaping it. CR 614.10 skip effects,
CR 614.11 draw replacements, and CR 614.17 "can't" effects are all in that set.

Not a targeting or filter language. The patterns below carry the minimum scoping
their gate cards need. **P9 subsumes them** — every criterion type this phase
adds is marked as P9's to generalize.

### Gate cards

| Card | Verified oracle text | What it falsifies |
|---|---|---|
| **Hardened Scales** `{G}` | "If one or more +1/+1 counters would be put on a creature you control, that many plus one +1/+1 counters are put on it instead." | Paired with Corpsejack Menace, the outcome **depends on the order** — a pure fold has to invent one |
| **Corpsejack Menace** `{2}{B}{G}` | "If one or more +1/+1 counters would be put on a creature you control, twice that many +1/+1 counters are put on it instead." | Same event, different rewrite shape; and without CR 614.5 the loop never terminates |
| **Doubling Season** `{4}{G}` | "If an effect would create one or more tokens under your control, it creates twice that many of those tokens instead. / If an effect would put one or more counters on a permanent you control, it puts twice that many of those counters on that permanent instead." | One card, **two** event classes; and CR 616.1g nesting — creating a token *contains* that token entering |
| **Clone** `{3}{U}` | "You may have this creature enter as a copy of any creature on the battlefield." | A replacement that **asks a question**, which a pure path cannot do |
| **Primal Plasma** `{3}{U}`, Elemental Shapeshifter, `*/*` | "As this creature enters, it becomes your choice of a 3/3 creature, a 2/2 creature with flying, or a 1/6 creature with defender." | CR 208.2b; the choice is **copiable** (CR 707.2), and stacked with Clone it falsifies any single-pass implementation |

The centerpiece is the last pair. Its Gatherer ruling, verified live:

> **Primal Plasma:** "If an object enters the battlefield as a copy of Primal
> Plasma, it copies the values determined by its enters-the-battlefield
> replacement effect, **but its power and toughness are determined by the copy's
> own enters-the-battlefield replacement effect.** This can cause you to have a
> 3/3 creature with flying, or a 1/6 creature with flying and defender, for
> example."

> **Clone:** "Any 'as [this creature] enters' or '[this creature] enters with'
> abilities of the chosen creature will also work."

A Clone entering as a copy of a 2/2-flying Primal Plasma must end up **1/6 with
flying and defender** if its controller picks the third option. Getting there
requires: apply the copy first (616.1c's bucket), observe that the object *now
has* an ability it did not have a moment ago, recognize that ability as newly
applicable (616.2), prompt for it, and let it overwrite P/T while leaving the
copied keyword alone. No implementation that computes its candidate list once can
produce that board state.

## 1. Scope

**In scope.** One `ProposedEvent` vocabulary; one reshaped, event-class-general
`ReplacementEffect`; one monadic `Pawl.Replacement` module implementing CR
616.1's loop with a `ChooseReplacement` prompt; unification of prevention,
regeneration and the as-enters copy seam onto that path; six wired event classes;
the CR 614.13a same-batch exclusion; five new/reshaped card data files.

**Out of scope.** The other ~34 event classes (VOCAB). The filter language (P9).
CR 615.7's shared N-damage shields and CR 615.13's prevented-triggers, which are
card-driven and tracked on #58 — this phase discharges their *structural*
blockers and nothing more. CR 614.15 self-replacement effects, CR 616.1b
control-modifying entry, and CR 616.1d back-face entry: each gets its
classification bucket, none has a producer.

## 2. Architecture

### 2.1 One vocabulary of proposed events

```haskell
-- Pawl.Type.ProposedEvent
data ProposedEvent
  = WouldChangeZone ZoneChange
  | WouldEnter ObjectId
  | WouldDealDamage DamageEvent
  | WouldBeDestroyed ObjectId
  | WouldPutCounters ObjectId CounterKind Natural
  | WouldCreateTokens PlayerId Card Natural
```

Distinct from P4's `GameEvent`, and deliberately so: `GameEvent` is *history* —
it carries a CR 608.2h last-known-information snapshot and exists only after the
fact. `ProposedEvent` is a *pre-event*: it exists only while it is being
replaced, and per CR 614.6 the one that survives the loop is the one that
actually happens.

`WouldEnter` is raised only for battlefield entries (CR 614.1c–d entry
replacements apply nowhere else) and is **nested inside** whatever caused the
entry, which is CR 616.1g's requirement stated structurally:

> "one replacement or prevention effect may apply to an event, and another may
> apply to an event contained within the first event. In this case, the second
> effect can't be chosen until after the first effect has been chosen." *(616.1g,
> whose worked example is Doubling Season creating token copies of Voice of All)*

So `createToken` resolves `WouldCreateTokens` to a count, then each of the N
tokens runs its own `WouldEnter` loop; `changeZone` resolves `WouldChangeZone` to
a destination, then runs `WouldEnter` if that destination is the battlefield.
Containment is expressed as call nesting, not as a field.

### 2.2 One event-class-general replacement effect

```haskell
-- Pawl.Type.ReplacementEffect
data ReplacementEffect
  = ZoneChangeR ZoneChangePattern Zone       -- Rest in Peace
  | EntryR EntryRewrite                      -- Clone, Primal Plasma
  | DamageR DamagePattern DamageRewrite      -- Fog
  | DestructionR DestructionRewrite          -- regeneration (CR 614.8)
  | CounterR CounterPattern Scaling          -- Hardened Scales, Corpsejack, Doubling Season
  | TokenR TokenPattern Scaling              -- Doubling Season

data Scaling = Multiply Natural | AddMore Natural
data EntryRewrite = AsCopy | ChoiceOf [EntryOption]
data DamageRewrite = PreventAll
data DestructionRewrite = Regenerate
```

One arm per replaceable *event class* — the count tracks the ~40, not the card
pool. A `(effect, event)` pair whose arms disagree simply does not apply, so the
type rules out "redirect a damage event" without a validity pass.

`EntryR` and `DestructionR` carry **no pattern**: both are self-only today (CR
201.5 / 201.5c — "regenerate *this creature*" names the ability's own source, not
a class of permanents; CR 614.1c — "*[this permanent]*
enters as"). CR 614.1d's other-objects form ("[Objects] enter the battlefield
…", Essence of the Wild) has no producer, so the field appears when a card needs
it rather than as speculative structure.

The patterns carry exactly what their gate cards scope on:

```haskell
data ControllerRelation = Yours | Anyones   -- relative to the effect's source's controller
data PermanentCriterion = AnyPermanent | CreaturePermanent

data ZoneChangePattern = MkZoneChangePattern { whenDestination :: Zone, whoseObject :: ControllerRelation }
data CounterPattern    = MkCounterPattern { whichKind :: Maybe CounterKind, whose :: ControllerRelation, onWhat :: PermanentCriterion }
data TokenPattern      = MkTokenPattern { whose :: ControllerRelation }
data DamagePattern     = MkDamagePattern { whichKind :: Maybe DamageKind }
```

Doubling Season's counter clause is `MkCounterPattern Nothing Yours AnyPermanent`
(any kind, any permanent you control); Hardened Scales is
`MkCounterPattern (Just PlusOnePlusOne) Yours CreaturePermanent`. The difference
between them is *data*, which is the whole point — neither is a constructor.
**`PermanentCriterion` is P9's to generalize**, and `CardCriterion` is its
sibling; they are not merged here because P9 will merge both.

### 2.3 One place effects come from

`GameState.preventions` and `GameState.regenerationShields` both disappear,
replaced by:

```haskell
-- Pawl.Type.ActiveReplacement (replaces ActivePrevention)
data ActiveReplacement = MkActiveReplacement
  { effect :: ReplacementEffect
  , source :: ObjectId
  , timestamp :: Timestamp
  , duration :: Duration
  , uses :: Uses          -- Pawl.Type.Uses = Unlimited | Once
  }
```

`uses` is CR 614.3's *"last until they're used up or their duration has
expired"*: regeneration is CR 701.19a's "the **next** time [permanent] would be
destroyed this turn", so `Once`; Fog is `Unlimited` for its duration. `source`
and `timestamp` are new, and they are precisely the two fields #58 recorded as
missing — CR 615.13 "prevented" triggers and CR 615.7's multi-source choice are
no longer *structurally* blocked, only card-blocked.

Candidates for a given event are therefore collected from two places: the
projection (`Projection.replacementsAffecting`, a permanent's static replacement
abilities after layer 6, unchanged) and `GameState.replacements` (floating,
resolution-generated). The two opcodes that installed the old shapes,
`Effect.Prevent Duration Prevention` and `Effect.RegenerateSelf`, collapse into
one:

```haskell
Replace Duration Uses ReplacementEffect
```

Fog becomes `Replace UntilEndOfTurn Unlimited (DamageR (MkDamagePattern (Just Combat)) PreventAll)`.
Drudge Skeletons becomes `Replace UntilEndOfTurn Once (DestructionR Regenerate)`.
`Pawl.Type.Prevention` and `Pawl.Type.ActivePrevention` are deleted.

### 2.4 The loop, written from CR 616.1

New module **`Pawl.Replacement`** — the sole home of casing on
`ReplacementEffect` and `ProposedEvent`, a fourth sole-casing home beside
`Pawl.Resolve` (`Effect`), `Pawl.Event` (`TriggerCondition`) and
`Pawl.Projection` (`Modification`). §3 restates why that matters.

```haskell
applyReplacements :: ProposedEvent -> Game ProposedEvent
```

Each iteration, from scratch:

1. **Collect** every active replacement whose arm matches the event's arm, whose
   pattern matches the event, and which has not already been applied to *this*
   event. That last clause is CR 614.5 — *"A replacement effect doesn't invoke
   itself repeatedly; it gets only one opportunity to affect an event or any
   modified events that may replace that event."* It is not an optimization:
   without it Hardened Scales and Corpsejack Menace re-trigger on each other's
   output forever. Identity for the applied-set is the `(source, index)` of the
   effect instance, **not** the effect value — two Doubling Seasons are two
   opportunities.
2. **Bucket** by CR 616.1a–e in order: self-replacement (614.15) → would modify
   under whose control an object enters → would cause an object to become a copy
   as it enters → would cause a card to enter with its back face up → any. Take
   the highest non-empty bucket. Only the third and fifth have producers; the
   others are implemented as classification with a documented absence (§8).
3. **Choose.** If the bucket holds two or more candidates that are not all equal
   as values, prompt. Otherwise apply the single candidate without asking — with
   one candidate there is nothing to choose, and order among identical effects is
   indistinguishable (each still gets its own opportunity per step 1, so the
   *outcome* is invariant; only the prompt is elided).
4. **Apply** the chosen effect, which may itself prompt (`AsCopy`, `ChoiceOf`),
   and record it in the applied-set.
5. **Repeat** until no candidate remains — CR 616.1f. Because step 1 re-collects
   against the *current* state, an effect that only became applicable as a result
   of step 4 is picked up, which is CR 616.2.

The chooser is the affected object's controller — P1's `Object.controller`, read
through `Projection.controllerOf` — or its owner if it has none, or the affected
player. New prompt, modelled on `OrderTriggers`:

```haskell
-- CR 616.1: which applicable replacement/prevention effect to apply next.
ChooseReplacement :: Decider -> PlayerId -> [ObjectId] -> Prompt Natural
```

The `[ObjectId]` is each candidate's **source**, in the engine's canonical order;
the answer is an index into it. Positional, and carrying exactly the caveat #61
records for `OrderTriggers`: a source with two *distinct* applicable replacement
abilities would put two different effects on the wire as identical entries. That
is reachable here in a way it is not for triggers — Doubling Season has two
replacement abilities — but they are in different event classes and so never
candidates for the same event. A single source with two same-class replacements
needs a discriminator alongside the source; that gets its own issue (§8).

**CR 616.1's APNAP clause** — *"If two or more players have to make these choices
at the same time, choices are made in APNAP order (see rule 101.4)"* — has no
producer: one proposed event has one affected object and therefore one chooser,
and §2.6's damage batch resolves each event's loop independently. Documented, not
built (§8).

### 2.5 The entry loop, and why the object is materialized

`changeZone` becomes, in order: take the CR 608.2h snapshot against the pre-move
state (unchanged); run the `WouldChangeZone` loop to settle the destination; drop
the old incarnation; materialize the new one; **if the destination is the
battlefield, run the `WouldEnter` loop**; then record the `Moved` event.

The entering object is materialized in `GameState.objects` and its zone index
*before* the loop runs, because CR 614.12 demands it:

> "To determine which replacement effects apply and how they apply, check the
> characteristics of the permanent **as it would exist on the battlefield**,
> taking into account replacement effects that have already modified how it
> enters the battlefield (see rule 616.1), continuous effects from the
> permanent's own static abilities that would apply to it once it's on the
> battlefield, and continuous effects that already exist and would apply to the
> permanent."

That is a projection of the object, in the state where it has entered — so the
cheapest correct implementation is to put it there and project it normally. This
is the same posture P2 already takes (the pending Clone is a materialized 0/0
until the drain fixes it up), but strictly stronger: the loop finishes **before
the `Moved` event exists**, so no trigger scan and no SBA can observe the interim
object. P2's observable-equivalence argument (its spec §2.4) is discharged rather
than re-inherited, and `Engine.settleForPriority` loses its `drained` re-loop.

Applying an `EntryRewrite` writes into the object's **copiable snapshot** — the
existing `Binding.copy` / `Projection.copiableCharacteristics` machinery, which
already serves as the layer-1 base (CR 613.1a):

- `AsCopy` prompts the existing `ChooseCopyTarget` and stamps the chosen source's
  copiable characteristics. Only the drain *site* moves; `Target.legalCopyTargets`
  and `Binding.setCopy` are reused as-is.
- `ChoiceOf` prompts a new `ChooseEntryOption`, then sets P/T from the chosen
  `EntryOption` and **unions** its keywords into the snapshot.

Writing to the copiable snapshot is what makes CR 707.2 fall out for free — the
rule says copiable values are the printed values *"as modified by other copy
effects, by its face-down status, and by 'as … enters' … abilities that set power
and toughness (and may also set additional characteristics)."* Both rewrites
target exactly that layer, so a later Clone of a Primal Plasma copies the choice
without any further machinery.

And the centerpiece composes without a special case. Clone enters; `AsCopy` is in
bucket 616.1c and applies first, stamping the target's copiable values — which,
for a Primal Plasma, already include *2/2 with flying*. Re-collecting (step 1)
now finds Primal Plasma's own `ChoiceOf`, which the Clone did not have an
iteration ago: CR 616.2. It prompts; choosing the third option sets P/T to 1/6
and unions in defender, leaving the copied flying untouched. **1/6 with flying and
defender** — the ruling's own words.

**Union, not replacement, of keywords** is pinned by that ruling and is the one
detail worth stating twice: "1/6 creature with flying and defender" is only
reachable if `ChoiceOf` adds defender to a snapshot that already carries flying.

**CR 614.13a** — *"You can't choose the object that will become that permanent or
any other object entering the battlefield at the same time as that object"*,
which Clone's ruling restates as "If Clone somehow enters at the same time as
another creature, Clone can't become a copy of that creature" — is implemented by
passing the set of ids entering in the same batch down to the entry loop and
excluding them from `legalCopyTargets`. Cheap, rules-mandated, and **unexercised**:
no real card in reach puts two copy-choosers onto the battlefield simultaneously.
Implemented, with an issue recording that the exclusion has no test (§8) rather
than a synthetic card to manufacture one.

### 2.6 Damage stays a simultaneous batch

`Damage` currently applies preventions as a pure filter over a whole batch of
combat damage. Under the unified path, **each event in the batch runs its own CR
616 loop against the same pre-damage state**, and the survivors are then applied
together. Simultaneity is preserved as a *scheduling* property; the loop's unit
stays one event, uniform with the other five classes.

This is what CR 614.5 ("one opportunity to affect **an event**") and CR 615.10
("applies separately to damage from other applicable events that would happen at
the same time") both describe. What it cannot express is CR 615.7's shared shield
— *"If damage would be dealt to the shielded permanent or player by two or more
applicable sources at the same time, the player or the controller of the
permanent chooses which damage the shield prevents"* — because that is a single
resource allocated across events. No such shield exists in the card pool (Fog is
unlimited-for-a-duration, not N-damage), so this stays on #58 as card-driven.

### 2.7 New funnels, and the monadic ripple

`Effect.PutCounters` currently edits `Object.counters` in place with no funnel at
all, so there is nothing to intercept. P5 adds **`Event.putCounters`**, matching
`changeZone` / `createToken` / `destroy`. CR 122.6 makes this the right single
funnel: *"Some spells and abilities refer to counters being put on an object.
This refers to putting counters on that object while it's on the battlefield and
also to an object that's given counters as it enters the battlefield."*

Because every one of these funnels can now prompt, all four become `Game`-monadic.
That pushes upward through:

- `Sba.performStateBasedActions`, today `GameState -> (Bool, GameState)`, becomes
  `Game Bool`. This is correct, not incidental: a creature dying to CR 704.5g with
  two applicable death-replacements genuinely must ask its controller which to
  apply, and M3g's decider re-entrancy already permits prompting from inside the
  settle loop.
- `Damage`, `Resolve`, `Engine`, `Cast`, `Stack`, `Activate` — roughly fourteen
  `State.modify' (Event.changeZone …)` and `List.foldl'` call sites become
  monadic binds.

This is the pervasive-change cost, structurally the same as P1's `controller`
field, and the reason §7 lands it as its own behavior-preserving task before any
new card appears.

### 2.8 Serialization

Every new type gets a `Codec` pair. Two deliberate breaks with the "existing
files stay byte-identical" precedent, licensed by CLAUDE.md's no-API-stability
rule:

- `Card.copyOnEnter :: Bool` is **deleted**. `clone.json` carries a real
  `EntryR AsCopy` in its `replacementEffects` instead. A card-specific boolean on
  the card type was the closest thing in the engine to a fused half; it goes.
- `rest-in-peace.json`, `fog.json` and `drudge-skeletons.json` are reshaped to
  the new constructors.

## 3. The two invariants

**The rules core reads a classification, never an identity.** The whole phase is
one classification: `ProposedEvent` names *event classes*, `ReplacementEffect`
names *(event class, rewrite shape)* pairs, and the patterns are data. The
scenario the invariant forbids —
`case effect of RedirectZoneChange Graveyard Exile -> restInPeace` — is not
expressible after §2.2, because Rest in Peace is no longer a constructor.
`Pawl.Replacement` is the single new sole-casing home; nothing outside it cases
on either type.

**The engine makes no choices.** This phase exists mostly to *stop* the engine
choosing: the `foldl'`'s list order has silently decided CR 616 since M3f
introduced it. The two elisions it retains are the two the rules make
indistinguishable — a lone candidate (nothing to choose) and a bucket of
value-equal candidates (any order yields the same board, since each still gets
its own opportunity). Every other absence in §8 is a *missing producer*, not an
elided prompt.

## 4. What this phase does not touch

The layer system (Cluster 1, closed at P3b). P4's event log, except that `Moved`
is now recorded after the entry loop rather than before it. Durations (P6). The
player projection (P7). Costs (P8). The filter language (P9) — `PermanentCriterion`
is added knowing P9 will absorb it. Player counters (P10). The Command zone (P11).

## 5. Cards and tests

Every test is gameplay-level: cast or resolve through the stack, assert on game
state. New data files: `hardened-scales.json`, `corpsejack-menace.json`,
`doubling-season.json`, `primal-plasma.json`. Reshaped: `clone.json`,
`rest-in-peace.json`, `fog.json`, `drudge-skeletons.json`.

| # | Scenario | Assertion |
|---|---|---|
| 1 | Hardened Scales + Corpsejack Menace out; cast Battlegrowth on a creature; answer the prompt "Scales first" | **4** `+1/+1` counters (1 → 2 → 4) |
| 2 | Same board, answer "Corpsejack first" | **3** counters (1 → 2 → 3) — the same input, a different board, decided by a player |
| 3 | Same board, no prompt answer recorded | The engine **prompts**; it does not proceed on list order |
| 4 | One Hardened Scales only | **2** counters, no prompt (single candidate, nothing to choose) |
| 5 | Two Hardened Scales | **3** counters (1 → 2 → 3); a prompt is elided as the candidates are value-equal, and CR 614.5 is per *instance* |
| 6 | Hardened Scales + Instill Infection (a `-1/-1` counter) | **1** counter — the pattern's `whichKind` excludes it |
| 7 | Corpsejack Menace + a counter on a permanent an opponent controls | Not doubled — `ControllerRelation` |
| 8 | Doubling Season + Dragon Fodder | **4** Goblin tokens, each having run its own entry loop (CR 616.1g / 614.16) |
| 9 | Two Doubling Seasons + Dragon Fodder | **8** Goblins — 614.5 counts instances, not values |
| 10 | Doubling Season + Battlegrowth | **2** counters — the same card's *other* clause, a different event class |
| 11 | Doubling Season + Hardened Scales + Battlegrowth | **4** or **3** by the prompt — the race across two different cards and two rewrite shapes |
| 12 | Primal Plasma enters; choose "2/2 with flying" | A 2/2 with flying; the printed `*/*` is not what the projection reports |
| 13 | Clone enters copying that Primal Plasma; choose "1/6 with defender" | **1/6, with flying and defender** — the Gatherer ruling verbatim |
| 14 | Same, choose "3/3" | **3/3 with flying** — the ruling's other example |
| 15 | Clone copying the *Clone* from #13 | Copies 1/6-flying-defender, then makes its **own** choice — CR 707.2's chain |
| 16 | Clone declines the copy (`Nothing`) | A 0/0 Shapeshifter, dying to CR 704.5f — the "may" is still a real decline |
| 17 | Rest in Peace out; a creature dies | Exiled, not in the graveyard — regression, now via `WouldChangeZone` |
| 18 | Drudge Skeletons regenerates, then is destroyed twice in a turn | First destruction replaced, second is not — `Uses = Once` |
| 19 | Rest in Peace out; a regenerating creature is destroyed | Regeneration replaces the destruction, so the put-into-graveyard event never happens and Rest in Peace never applies — the nesting `destroy` hardcodes today, now structural |
| 20 | An indestructible creature is destroyed with a shield up | The shield is **not** consumed — CR 614.7, an event that never happens is not replaced |
| 21 | Fog | Combat damage prevented — regression, now via `WouldDealDamage` |
| 22 | Fog + two attackers | Both prevented independently; CR 615.10's per-event application, in one simultaneous batch |

## 6. Module & type changes (summary)

**New modules.** `Pawl.Replacement`. `Pawl.Type.ProposedEvent`,
`Pawl.Type.ActiveReplacement`, `Pawl.Type.Uses`, `Pawl.Type.Scaling`,
`Pawl.Type.EntryRewrite`, `Pawl.Type.EntryOption`, `Pawl.Type.DamageRewrite`,
`Pawl.Type.DestructionRewrite`, `Pawl.Type.ZoneChangePattern`,
`Pawl.Type.CounterPattern`, `Pawl.Type.TokenPattern`,
`Pawl.Type.DamagePattern`, `Pawl.Type.ControllerRelation`,
`Pawl.Type.PermanentCriterion`.

**Deleted.** `Pawl.Type.Prevention`, `Pawl.Type.ActivePrevention`.
`Event.applyReplacements`, `applyPreventions`, `cancels`, `markCopyOnEnter`,
`dropEndOfTurnPreventions`, `clearRegenerationShields`.
`Engine.drainAsEntersChoices`, `drainOneCopy`, `applyCopyChoice`.
`Binding.markPending`, `clearPending`, `pendingCopy`, `asEntersPending`.
`Card.copyOnEnter`.

**Reshaped.** `ReplacementEffect` (§2.2). `GameState`: `preventions` and
`regenerationShields` → `replacements :: [ActiveReplacement]`. `Effect`:
`Prevent` and `RegenerateSelf` → `Replace Duration Uses ReplacementEffect`.
`Prompt`: `+ChooseReplacement`, `+ChooseEntryOption`. `Response`:
`+ChoseReplacement`, `+ChoseEntryOption`. `Event`: `changeZone`, `destroy`,
`createToken` become `Game`; `+putCounters`. `Sba.performStateBasedActions`
becomes `Game Bool`. `Engine.settleForPriority` drops its `drained` term.

## 7. Ordering within the phase (for the plan)

Substrate first: the risky rewrite lands, behavior-preserving, before any new card
appears. Every task is one commit that leaves the suite green.

1. **Types + Codec.** All of §6's new types; `ReplacementEffect` reshaped;
   `rest-in-peace.json`, `fog.json`, `drudge-skeletons.json` migrated. The four
   existing pure paths are adapted to consume the new shapes. No CR 616 yet.
2. **The monadic ripple.** `changeZone` / `destroy` / `createToken` become `Game`;
   `Sba.performStateBasedActions` becomes `Game Bool`; all call sites in `Resolve`,
   `Engine`, `Cast`, `Stack`, `Activate`, `Damage` follow. **Zero behavior change**;
   the whole existing suite must stay green.
3. **`Pawl.Replacement` + the loop.** `applyReplacements`, the 616.1 buckets, the
   applied-set, `ChooseReplacement`, `Response`, decision-log replay. Wire
   `WouldChangeZone` through it. Test 17 stays green; a two-redirect race becomes
   expressible.
4. **Prevention folded in.** `Effect.Replace`, `GameState.replacements`, `Uses`;
   delete `Prevention` / `ActivePrevention` / `applyPreventions`. Tests 21, 22.
5. **Regeneration folded in.** `WouldBeDestroyed`, `Uses = Once`; delete
   `regenerationShields`. Tests 18, 19, 20.
6. **Counters.** `Event.putCounters`, `WouldPutCounters`, `CounterR`, `Scaling`;
   `hardened-scales.json`, `corpsejack-menace.json`. Tests 1–7.
7. **The entry loop + the copy fold.** `WouldEnter`, `EntryR AsCopy`, CR 614.13a's
   exclusion; retire `drainAsEntersChoices`, the `Binding` pending marker and
   `Card.copyOnEnter`. Test 16, and the existing P2 Clone tests move to the new
   path unchanged in their assertions.
8. **`ChoiceOf` + Primal Plasma.** `ChooseEntryOption`, the keyword union,
   `primal-plasma.json`. Tests 12–15 — the centerpiece.
9. **Tokens.** `WouldCreateTokens`, `TokenR`, `doubling-season.json`. Tests 8–11,
   including 616.1g nesting, which is only assertable once task 7 exists.
10. **Close.** Issues filed and updated (§8), umbrella §3/§4 ticked, `progress.md`
    entry, `CLAUDE.md` status bullet replaced.

Doubling Season lands whole at task 9 rather than half at task 6: a card in the
pool that models only one of its two abilities is a wrong engine, not a partial
one. Corpsejack Menace exists to give task 6 a real second counter-replacement.

## 8. Deferred, with named expiries

Each gets a GitHub issue carrying status, rationale and expiry trigger; each code
site carries a comment stating only what is *not* implemented, plus `(#N)`.

| What | Why deferred | Expiry trigger |
|---|---|---|
| CR 614.15 self-replacement effects (616.1a's bucket) | No producer: no card in the pool replaces part of its own resolution | `expires:card-driven` — the first self-replacement card |
| CR 616.1b control-modifying entry | No producer; needs an "enters under your control instead" replacement | `expires:card-driven` |
| CR 616.1d back-face entry | No producer; needs transform (CR 701.27), not modelled | `expires:card-driven` |
| CR 616.1's APNAP tie-break | No producer: one proposed event has one chooser | `expires:card-driven` — the first event affecting two players' objects at once |
| CR 614.12b — a player may not make entry choices whose combined costs are unpayable | No producer: neither `AsCopy` nor `ChoiceOf` has a cost | `expires:card-driven` — the first entry replacement with a cost |
| CR 614.13a same-batch exclusion is **implemented but untested** | No real card puts two copy-choosers onto the battlefield simultaneously | `expires:card-driven` — the first card that does |
| `ChooseReplacement`'s positional payload | Sound while no single source has two same-class applicable replacements. Doubling Season has two replacement abilities but in different classes | `expires:card-driven` — mirrors #61 for triggers |
| CR 615.7 shared N-damage shields, CR 615.13 prevented-triggers | **#58, updated not closed.** `source` and `timestamp` now exist, so both are card-blocked rather than structure-blocked | `expires:card-driven` (unchanged) |
| The other ~34 event classes (draws, life, untap, mill, mana, extra turns, …) | **VOCAB.** Each is one `ProposedEvent` arm plus its funnel | Card-driven, tracked by breadth not by issue |
| CR 614.10 skip effects, CR 614.11 draw replacements, CR 614.17 "can't" effects | Same: vocabulary on this axis. 614.17 in particular is explicitly *not* a replacement effect but "follows similar rules" | `expires:card-driven` |

## 9. Tracking

P5 is the umbrella's GAP-R **and** the CR 616 facet, so it closes both halves of
what §6 of the umbrella tracked:

- `48b17cb` (GAP-R, event coverage) → **closed by this phase**, issue #1.
- `6afb561` (M3f replacement seam: pure/single, CR 616 ordering + choice-bearing
  replacements) → **closed by this phase**. Note the family resemblance P4's spec
  drew: P4 retired the *trigger* ordering elision (603.3b); P5 retires the
  *replacement* ordering elision (616.1). Different rules, different mechanisms,
  same shape of mistake.
- #58 (CR 615.7 / 615.13) → **updated, not closed**; see §8.
- `b998924` (OfAbility LKI) — untouched, stays open.
- `f90e0c4` (topological CR 613.8b applies-to) — untouched; this phase adds no
  layer-fold dependency.

The umbrella's §3 table row for P5 and its §4 ordering note should be updated
when this phase completes: P6 and P7 remain unblocked, P8 and P9 still float.

**Departures from the umbrella, per its §7.** The umbrella named the gate as
"Doubling Season + a two-replacement race". This spec keeps Doubling Season and
makes the race concrete (Hardened Scales + Corpsejack Menace), and adds a second
gate the umbrella did not anticipate: **Clone + Primal Plasma**, which is what
actually falsifies a single-pass implementation via CR 616.2. The umbrella's
three fold-ins are all honored — P2's mark-then-drain seam is retired (§2.5), CR
614.13a's simultaneous-entry exclusion is implemented (§2.5, untested per §8),
and P3b's CR 208.2b as-enters P/T choice lands here (Primal Plasma). Update the
umbrella's P5 row to name the two additional gate cards.

## 10. Exit criterion

All twenty-two scenarios in §5 pass as gameplay-level tests, including the
centerpiece (#13) reproducing the Gatherer ruling's own board state; exactly one
replacement path exists in the engine and it is monadic; `Pawl.Type.Prevention`,
`GameState.regenerationShields`, `Card.copyOnEnter` and
`Engine.drainAsEntersChoices` are gone; `cabal build` is warning-clean and
`hooky run` passes. At that point a new replaceable event is one `ProposedEvent`
arm and one funnel — vocabulary on a finished axis, which is what "the closed
half can genuinely be finished" means for GAP-R.
