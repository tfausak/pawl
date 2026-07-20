# M4e counter target spell — design

Design for milestone **M4e**, the fifth letter of M4 (see the split table in
`docs/design.md` §3): **the first effect that targets the stack to remove a spell
from it.** M3d already let an effect *target* a spell on the stack (Magical Hack's
`SpellOrPermanentTarget`), but only to rewrite its text; the spell still resolved.
M4e proves the stack is a zone an effect can *remove an object from before it
resolves* — CR 701.5, the keyword action **Counter** — and that the CR 608.2b
fizzle already built for M3a's targeting generalizes to a target that lives on the
stack.

One gate card, Scryfall-verified (`api.scryfall.com/cards/named?exact=Cancel`,
fetched 2026-07-19):

| Card | Cost | Type line | P/T | Oracle text |
|---|---|---|---|---|
| **Cancel** | `{1}{U}{U}` | Instant | — | "Counter target spell." |

**The falsifier — Cancel must fizzle when its target has already left the stack.**
A counterspell is cast targeting a spell that is legal at cast time; if that spell
leaves the stack before Cancel resolves (it was itself countered, or otherwise
removed), Cancel's only target is now illegal and Cancel does **not** counter
anything — it goes to its owner's graveyard with no effect (CR 608.2b then 608.2).
An implementation that stored a resolved reference to the target object, or that
"countered whatever is on top," would counter the wrong spell or crash on a
dangling id. The engine already re-validates every filled slot at resolution
(`Resolve.resolveSpell`) and re-judges legality against the current state
(`Target.stillLegal`); M4e's job is to route a new `SpellTarget` spec and a
`Counter` opcode through that existing seam, not to rebuild it.

This is a types-and-architecture spec, not an implementation plan.

## 0. The core idea

**Countering is the stack's zone change.** A spell on the stack is an object
(`Source.OfCard`); countering it (CR 701.5a) puts it into its owner's graveyard
without resolving. The engine already has the one primitive that does this:
`Event.changeZone oid Graveyard` removes the id from `GameState.stack`
(`Game.removeFromZones`) and mints a fresh graveyard incarnation carrying the
owner forward (CR 400.7). So Counter is, mechanically, a stack→graveyard
`changeZone` — the same shape M4b's `Destroy` reduces to for a non-indestructible
creature, after its own checks.

Three moving parts, all small, all built on existing seams:

1. **A narrower target spec.** `TargetSpec.SpellTarget` (CR 115, "target spell") —
   stack objects that are *spells*, distinct from M3d's `SpellOrPermanentTarget`
   because Cancel cannot target a permanent (nor an ability).
2. **A distinct opcode.** `Effect.Counter SlotName` — Counter is CR 701.5, an entry
   on rule 701's opcode list, and gets its own opcode for the same reason M4b's
   `Destroy` did: the comprehensive rules treat it as a distinct keyword action
   even where its current mechanics coincide with a plain put-into-graveyard.
3. **A named funnel.** `Event.counter :: ObjectId -> GameState -> GameState`,
   mirroring `Event.destroy`: the sole home of the CR 701.5a move, the future home
   of "can't be countered" (CR 701.5) and a distinct "was countered" event.
   Ungated today, exactly as `Event.destroy` is ungated for CR 701.19c.

The §1 invariant holds throughout: every new read is a **classification** —
is-this-stack-object-a-spell (a read of `Object.source`, the same kind of read as
`Card.isPermanent` in `Stack.resolveTop`) and is-this-a-legal-target (the spec).
The rules core never learns Cancel's identity. Cancel is a printing whose effect
list is `[Counter "spell"]` and whose `targetSpecs` is `{"spell" ↦ SpellTarget}`.

## Goal and scope

**Exit criterion.** Deterministic tests demonstrate all of:

- **Cancel counters a spell (CR 701.5, the gate).** One player casts a spell; it
  goes on the stack. The other player casts Cancel targeting it and it resolves
  first (LIFO). The targeted spell is put into **its owner's** graveyard, never
  resolves (a creature spell does **not** become a permanent on the battlefield),
  and the stack is empty afterward. Cancel itself goes to its own controller's
  graveyard (it is an instant, not a permanent).
- **Cancel fizzles when its target has left the stack (CR 608.2b, the falsifier).**
  Two counters race: Cancel A targets spell X; in response, Cancel B also targets
  X. B resolves first and counters X (X → its owner's graveyard). A then resolves
  with its only target — X — no longer on the stack: A **fizzles**, going to its
  controller's graveyard with no counter performed. X is not moved twice; no
  dangling id is dereferenced. (This scenario doubles as proof that a spell can be
  the target of a counter — a counterspell is itself a spell on the stack.)
- **Cancel cannot target a permanent or an ability (CR 115).** `SpellTarget`'s
  legal set excludes battlefield permanents and stack abilities — asserted at the
  targeting layer (`Target.legalRecipients`), so a Cancel offered when only a
  permanent or an activated ability is present has no legal target.
- **Rest in Peace composes (free).** With Rest in Peace on the battlefield, a
  countered spell being put into a graveyard from the stack is redirected to exile
  by M3f's existing `RedirectZoneChange` replacement — no new code. (This mirrors
  M4c's token/RiP composition; asserted as a confirming test, not new machinery.)

The `DecisionLog` replays deterministically. **No new prompt or response type:**
Cancel targets through the existing `ChooseTargets` prompt (M3a), and countering
is unprompted. The honesty round-trip (`jsonToCard . cardToJson ≡ Right`) covers
Cancel via a new `Codec` arm.

**Out of scope (named deferred expiries, §7):** "can't be countered"; conditional
counters ("counter unless pay {N}", Mana Leak/Daze); a distinct "was countered"
event and "whenever a spell is countered" trigger; countering **abilities**
(Stifle); alternative counter destinations (counter-and-exile, to library top);
restricted counters ("counter target spell with mana value N / of a color").

## 1. The target spec — `SpellTarget`

`Pawl.Type.TargetSpec` gains one constructor:

```
| -- CR 115: "target spell" -- an object on the stack that is a spell (a card on
  -- the stack, CR 111.1: Source.OfCard). Narrower than SpellOrPermanentTarget:
  -- Cancel cannot target a permanent or an ability. The first spec that reaches
  -- ONLY the stack.
  SpellTarget
```

`Target.legalRecipients` gains its arm:

```
TargetSpec.SpellTarget ->
  let spells = filter (\oid -> Game.isSpell oid gs) (GameState.stack gs)
   in Set.fromList (map Recipient.ToObject spells)
```

The recipient reuses `Recipient.ToObject` (a spell is an object, named generically
— the same choice `SpellOrPermanentTarget` made). No `Recipient` change.

**The `isSpell` classification.** "Is this stack object a spell?" reads
`Object.source`: `OfCard _ → True`; `OfToken`/`OfAbility`/`OfTrigger → False`
(a token is never on the stack; abilities are not spells, CR 111.1). This is a
**classification, not an identity case** — structurally identical to
`Stack.resolveTop`'s `Card.isPermanent` read and `Mana.isManaAbility`. It lands as
a small pure helper `Game.isSpell :: ObjectId -> GameState -> Bool` (home in
`Pawl.Game`, beside `cardOf`; `Target` is the consumer). Casing on the `Source`
constructor is permitted — it is a classification of the object's *kind*, never of
the card's identity.

Because `SpellTarget`'s legal set is computed from `GameState.stack` in the
*current* state at both cast (M3a's `Target.legalSets`) and resolution
(`Target.stillLegal`), the CR 608.2b fizzle falls out with no new logic: when
Cancel resolves, `stillLegal` re-runs `legalRecipients SpellTarget` against the
now-current stack; a target no longer present is not in the set.

## 2. The opcode — `Effect.Counter SlotName`

`Pawl.Type.Effect` gains one constructor:

```
| -- CR 701.5: counter the slot's target spell -- remove it from the stack and put
  -- it into its owner's graveyard (CR 701.5a) via the Event.counter funnel, so it
  -- does not resolve. Distinct from MoveToZone slot Graveyard the way Destroy is
  -- (M4b): Counter is a keyword action on rule 701's list, and this is the future
  -- home of "can't be countered" (CR 701.5) and a distinct "was countered" event.
  Counter SlotName
```

The five Effect-classifying functions in `Pawl.Resolve` each gain a `Counter`
arm — all five case exhaustively, so the compiler forces each new arm. (`readsX`'s
NOTE warns that a Quantity field's X-ness is *not* compiler-checked, but the arm
itself is still forced; `Counter` has no Quantity, so its arm is simply `False`):

- `slotsOf (Counter slot) = Set.singleton slot`
- `readsX`: `Counter _ → False` (no Quantity)
- `manaProduced (Counter _) = Nothing`
- `searchesLibrary (Counter _) = False`
- `rewriteEffect _ (Counter _) = effect` (identity — no rewritable land-type word)

`applyEffect`'s `Counter` arm (the sole `case effect of` home):

```
Effect.Counter slot ->
  State.modify' $ \gs ->
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> case recipientObject recipient of
        Nothing -> gs                 -- a player recipient can't be countered
        Just target -> Event.counter target gs
      _ -> gs                          -- illegal slot (CR 608.2b) or non-object: no-op
```

Per-slot legality is the same `Map SlotName Bool` M4b's `Destroy`/`MoveToZone`
consume; `recipientObject` (existing) maps `ToObject`/`ToCreature → Just oid`,
`ToPlayer → Nothing`. The arm never touches `GameState.stack` directly — the
funnel does.

## 3. The funnel — `Event.counter`

`Pawl.Event` gains a funnel mirroring `Event.destroy`:

```
-- The single counter funnel (CR 701.5). A countered spell is removed from the
-- stack and put into its owner's graveyard (CR 701.5a) via changeZone -- so Rest
-- in Peace's redirect (graveyard->exile) and CR 400.7 still compose. Ungated
-- today: "can't be countered" (CR 701.5) and a distinct "was countered" event are
-- deferred (spec section 7), exactly as Event.destroy is ungated for CR 701.19c.
counter :: ObjectId -> GameState -> GameState
counter oid gs = case Game.lookupObject oid gs of
  Nothing -> gs
  Just _ -> changeZone oid Zone.Graveyard gs
```

The `lookupObject` guard makes a stale id a no-op (defensive, matching
`Event.destroy`); in practice `applyEffect` only reaches this arm for a target the
608.2b legality pass just confirmed is on the stack. Routing through `changeZone`
(rather than a bare stack edit) is what earns the Rest in Peace composition and
the CR 400.7 owner-relative graveyard placement for free.

**Why a funnel and not an inlined `changeZone`.** Symmetry with `Event.destroy`
(M4b), and a named home for the deferred CR 701.5 gates: "can't be countered"
becomes a guard here, and a "was countered" event (distinct from the plain
stack→graveyard `ZoneChange`) becomes an emission here — neither expressible if
`applyEffect` inlined the move.

## 4. The card and the fixture mana base

Cancel renders to `data/cards/cancel.json` (the M3.5 files-are-source-of-truth
pipeline; the round-trip regenerates and re-parses it). Its `Card`:

- type line: `Instant` (`CardType.Instant`, exists since M3a); no subtype, no
  supertype.
- mana cost: `{1}{U}{U}` (generic + double blue; exercises mixed payment, and the
  double-blue requires an `Island` base — `Subtype.Island` and `island.json` exist
  since M3d).
- one effect: `Counter "spell"`; `targetSpecs`: `{"spell" ↦ SpellTarget}`.

No new `CardType`, `Subtype`, `Supertype`, `Prompt`, or `Response`. The blue
fixture mana base (Islands) is the one M3d established for Magical Hack; the tests
build a scenario with Islands untapped for the Cancel-caster.

## 5. Tests

All deterministic fixtures, blue (no random-game deck — the M3d/M3f/M3g
blue-fixture posture; a counterspell rarely has a legal target under random
priority-passing, so random coverage is low-value and high-cost). Test names carry
their CR numbers.

- **`TargetSpec`/`Target`:** `legalRecipients SpellTarget` over a state with a
  spell, an ability, and a permanent all present returns exactly the spell's
  `ToObject` (CR 115). `Game.isSpell` returns `True` for `OfCard`, `False` for
  `OfAbility`/`OfTrigger`/`OfToken`.
- **Gate (CR 701.5):** scripted `DecisionLog` — P casts a creature spell; opponent
  casts Cancel targeting it; both pass; Cancel resolves and counters. Assert the
  creature is in P's graveyard, absent from the battlefield and the stack; Cancel
  in the opponent's graveyard; stack empty.
- **Falsifier (CR 608.2b):** the racing-counters scenario of §Goal — Cancel A and
  Cancel B both target spell X; B counters X first; A fizzles. Assert X moved once
  (to its owner's graveyard), A in its controller's graveyard, no exception, board
  otherwise unchanged.
- **RiP composition:** with Rest in Peace on the battlefield, a countered spell is
  **exiled** (M3f's redirect), asserted as a confirming test.
- **Round-trip:** `cancel.json` is covered by the `allPrintings` honesty property
  via the new `Codec` arm.

## 6. Module and dependency notes

- `Game.isSpell` lives in `Pawl.Game` (beside `cardOf`); `Target` (which already
  imports `Game`) is the consumer. No new import edge of concern.
- `Event.counter` lives in `Pawl.Event` beside `destroy`/`changeZone`; it calls
  `changeZone` (same module) — no cycle. `Resolve` already imports `Event`.
- `Resolve` stays the sole `case effect of` home; `Event` stays the sole home of
  casing on replacement/trigger classifications; `Target` stays the sole home of
  targeting legality. No new casing homes.

## 7. Named deferred expiries

Each is due with the first real card that needs it:

- **"Can't be countered" (CR 701.5).** `Event.counter` is ungated — no
  `Counterability` argument, no protection-from-countering read. First card:
  e.g. an uncounterable spell, or Great Sable Stag.
- **Conditional counters.** "Counter target spell unless its controller pays {N}"
  (Mana Leak), "unless they pay {3}" (Daze) need a payment sub-prompt at
  resolution. Deferred; `Counter` is unconditional.
- **A distinct "was countered" event and its trigger.** `Event.counter` emits only
  the plain stack→graveyard `ZoneChange`, which matches no current trigger. A
  "whenever a spell is countered" condition and a countered-event value are future.
- **Countering abilities (Stifle).** `SpellTarget` is spells-only. Targeting
  activated/triggered abilities on the stack needs an `AbilityTarget` (or
  `SpellOrAbilityTarget`) and an `Event.counter` that accepts an ability object
  (which *ceases*, CR 701.5b, rather than moving to a graveyard — abilities are not
  cards). Deferred.
- **Alternative counter destinations.** Counter-and-exile, counter-and-put-on-top-
  of-library (Remand, Memory Lapse) need a destination argument on the counter
  action. Deferred; CR 701.5a's owner's-graveyard default is baked in.
- **Restricted counters.** "Counter target spell with mana value N" / "of a chosen
  color" need a predicate over the target spell at cast/resolution. Deferred.

## 8. Invariant check

- **Closed/open separation.** The rules core reads two classifications and no
  identities: `Game.isSpell` (a `Source`-kind read) and `SpellTarget` legality (a
  `TargetSpec` read). `Pawl.Resolve` is the only module that cases on the `Counter`
  opcode. There is no `case card of Cancel -> …` anywhere.
- **First-order, non-recursive DSL.** `Counter SlotName` carries a name, no
  function, no nested effect. It adds no control flow.
- **The engine makes no choice it shouldn't.** Countering is forced (unprompted);
  the only choice is the target, made at cast through the existing `ChooseTargets`
  prompt. No elision is introduced.
