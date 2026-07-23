# M4.5 P10 — Player-counter substrate, poison and energy

*Design written 2026-07-23. Closes **GAP-C** (the player-counter substrate) and
the first two customers of **GAP-S** (poison and energy). Umbrella:
`docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md` §3, row P10.
Issue: `tfausak/pawl#6`. Census: `docs/mtgish-gap-census.md` §3.4/§3.6.*

## 0. Why this phase, and what it proves

Every counter pawl models today lives on an **object**: `Object.counters ::
Map CounterKind Natural`, projected into P/T at layer 7c (M4f). But the
comprehensive rules place counters on **players** too — poison, energy,
experience, rad (CR 122.1, CR 122.1f). `Player` carries only `life` + `status`;
there is nowhere for a poison or energy counter to live.

This phase adds the missing substrate — a counter map on `Player` — and drives
it from two directions that between them exercise every operation the substrate
must support:

- **Poison**, via **infect** (Glistener Elf), *adds* to the map through the
  combat-damage path and *reads* it in a new state-based action (lose at ten,
  CR 704.5c). Infect is the single gate that touches both counter domains at
  once: damage to a *player* becomes poison counters (CR 120.3b), damage to a
  *creature* becomes −1/−1 counters (CR 120.3d) that then feed the existing
  layer-7c projection and the 704.5f/q SBAs for free.
- **Energy**, via **Longtusk Cub**, *adds* to the map through an effect
  ("you get {E}{E}") and *removes* from it through a cost ("Pay {E}{E}: …").
  Energy is the only thing in the gate that **spends** a player counter, so it
  is what proves the map is bidirectional.

### What this phase is *not*

It is **not** the whole of GAP-C. GAP-C also names the *counter→layer-6*
ability-granting path (a flying counter, a deathtouch counter — CR 122.1b), and
that path is **deferred**: no card in the pool needs it, poison and energy grant
no abilities, and folding it in would ship a layer-6 seam no gate card tests
(§8). It is not toxic (CR 702.164) or poisonous (CR 702.70) — infect
alone proves both poison branches, and those two are card-driven (§8). It is not
proliferate, not experience/rad counters (VOCAB once the substrate exists), and
not Two-Headed Giant poison sharing (CR 704.6b / 810 — out of scope, design.md
§6).

### Gate cards

- **Glistener Elf** — `{G}`, Creature — Phyrexian Elf Warrior, 1/1. "Infect."
- **Longtusk Cub** — `{1}{G}`, Creature — Cat, 2/2. "Whenever this creature
  deals combat damage to a player, you get {E}{E} (two energy counters). / Pay
  {E}{E}: Put a +1/+1 counter on this creature."

Both oracle texts were read from Scryfall on 2026-07-23, not recalled.

### The falsifiers, stated up front

- **Poison lands on the damaged player, not the source's controller.** CR 120.3b
  says infect damage "causes that source's controller to *give the player* that
  many poison counters" — the *counter* goes on the player who was dealt damage;
  the controller is merely who performs it. A test asserts Glistener Elf's
  controller gains no poison when it connects.
- **Infect damage causes no life loss and no marked damage.** CR 702.90b/c: the
  poison / −1/−1 counters *replace* the ordinary result. A test asserts the
  damaged player's life is unchanged and the damaged creature has zero marked
  damage.
- **Loss at ten is a state-based action, not "the next time a player would
  receive priority."** CR 104.3d phrases it the second way, but CR 704.5c is the
  operative rule: it is an SBA checked in the settle loop like life ≤ 0. A test
  drives a player to ten poison and asserts they lose at the next SBA check.
- **Energy is spent, not just gained.** A test activates Longtusk Cub's ability,
  asserts the two energy counters are removed, and asserts the ability is
  **unpayable** (CR 118.6 / 107.14) when the player has fewer than two.

## 1. Scope

Adds: a player-counter map; the `Infect` keyword and its damage diversion; the
poison-at-ten SBA; an energy-gain effect; an energy-payment cost component; and a
deals-combat-damage-to-a-player trigger condition. Two cards, their tests, and
the codec/setup wiring.

Does **not** add: the counter→layer-6 path; toxic; poisonous; proliferate;
experience/rad counters; targeted or variable player counters; a CR 614
player-counter replacement funnel; mana of any color (§8).

## 2. Architecture

### 2.1 `Pawl.Type.PlayerCounterKind`, a new type distinct from `CounterKind`

Object counters and player counters are **disjoint domains**. CR 122 has no
counter kind that goes on both: +1/+1, keyword, shield, stun, finality, loyalty,
defense and lore counters are object-only (CR 122.1a–e,g–i); poison, energy,
experience and rad counters are player-only (CR 122.1f,i; CR 107.14). So the
player-counter kind is its own type, not an extension of `CounterKind`:

```haskell
module Pawl.Type.PlayerCounterKind where

data PlayerCounterKind
  = Energy -- CR 107.14
  | Poison -- CR 122.1f
  deriving (Eq, Ord, Show)
```

`Ord` is load-bearing: like `CounterKind`, this is a `Map` key. Constructors are
ordered by rule number so the type stays diffable against the rules, matching
`CounterKind`'s and `Keyword`'s posture. Keeping the two types apart makes
"a +1/+1 counter on a player" and "a poison counter on a creature"
**unrepresentable** — the counter kind can only key the map it belongs to.

Like `CounterKind`, this is a *classification*, not an effect identity: the rules
core reads counts by kind (the CR 704.5c poison SBA; the CR 107.14 energy
payment) and never cases on a card.

### 2.2 `Player` gains a counter map

```haskell
data Player = MkPlayer
  { life :: Integer,
    status :: Status,
    counters :: Map PlayerCounterKind Natural
  }
```

One map for every player-counter kind, mirroring `Object.counters`. `Pawl.Setup`
initializes it empty. A kind absent from the map means zero (the
`Map.findWithDefault 0` convention `Object.counters` already uses). Unlike
object counters — which "cease to exist" when the object changes zones (CR 122.2)
— player counters persist for the whole game; a player never changes zones.

### 2.3 The `Infect` keyword

`Pawl.Type.Keyword` gains `Infect` (CR 702.90), placed by rule number (after
`Fear` 702.36, before `Devoid` 702.114). Casing on it is not an invariant
violation — a keyword is a citation, the same as `Flying` (see the type's own
comment and the M2a spec §1). Infect is a *static ability*: it changes what the
creature's damage **does**, and that change is read at damage-deal time.

### 2.4 `DamageEvent` gains a deal-time infect bit

```haskell
data DamageEvent = MkDamageEvent
  { source :: ObjectId,
    target :: Recipient,
    amount :: Natural,
    dealtByDeathtouch :: Bool,
    dealtByInfect :: Bool, -- CR 702.90d: last-known info
    kind :: DamageKind
  }
```

`dealtByInfect` is captured **when the damage is dealt**, from the projection
(`Projection.hasKeyword Keyword.Infect source`), exactly as `dealtByDeathtouch`
already is — CR 702.90d ("last known information" if the source has left) and CR
702.90e ("no matter what zone"). It is set at all three construction sites so the
behaviour is uniform: the two combat waves in `Pawl.Damage` and the noncombat
`DealDamage` in `Pawl.Resolve`. The gate exercises only the combat path (Glistener
Elf attacking), but setting it everywhere is cheap and correct; a noncombat
infect source is card-driven, not blocked.

### 2.5 The damage diversion, read from the bit, never the keyword

`Pawl.Damage.applyDamage`'s `markOne` fold gains two infect arms, dispatched on
the event's `dealtByInfect` **classification bit** — never on keyword identity,
exactly as the CR 704.5h deathtouch SBA reads `dealtByDeathtouch` rather than
re-deriving it. This keeps `applyDamage` invariant-clean:

- **`ToPlayer` + infect** → add `amount` **`Poison`** counters to that player's
  `counters` map; **no life loss** (CR 120.3b / 702.90b).
- **`ToCreature` + infect** → put `amount` **`MinusOneMinusOne`** counters on the
  creature; **no marked damage** (CR 120.3d / 702.90c).
- otherwise unchanged (mark on creatures, drain from players).

The −1/−1 branch places *object* counters through the ordinary `Object.counters`
edit, so it inherits the whole downstream chain already built: the layer-7c
projection (M4f) lowers the creature's P/T, the CR 704.5f zero-toughness SBA
buries it, and the CR 704.5q / 122.3 annihilation SBA reconciles it against any
+1/+1 counters. Nothing new is needed there.

**Funnel note.** Object `PutCounters` routes through `Event.putCounters` so CR
614 counter-replacements (Doubling Season, Hardened Scales) get their
opportunity. The infect −1/−1 counters are a *consequence of a damage event that
already ran the CR 616 replacement loop* (`Replacement.resolveDamage`), so they
are added directly here; a "would put −1/−1 from infect" sub-replacement is not
in the pool. Energy gain (§2.6) is likewise added directly. A CR 614
player-counter replacement funnel (an energy- or poison-doubling effect) is
deferred (§8).

### 2.6 The energy-gain effect

`Pawl.Type.Effect` gains, mirroring `PutCounters`' general shape for objects:

```haskell
GainPlayerCounters PlayerCounterKind Quantity
```

Targetless — applied to the **resolving controller** ("you get {E}{E}"), like
`Draw`. `Pawl.Resolve.applyEffect` evaluates the `Quantity` and adds that many
counters of the kind to `controller`'s map. Longtusk Cub's trigger resolves as
`GainPlayerCounters Energy (Literal 2)`. General on purpose: it subsumes any
future self-scoped player counter (experience, rad) without a new opcode. A
*targeted* player-counter effect ("target player gets two poison counters",
Phyresis) needs a player recipient and is deferred (§8).

### 2.7 The energy-payment cost component

`Pawl.Type.CostComponent` gains, mirroring `PayLife`'s specific shape:

```haskell
PayEnergy Natural -- CR 107.14 / 118
```

`Pawl.Cost` — the sole module that may case on `CostComponent` — reads it two
ways: **payability** (the paying player has ≥ N energy counters; below that the
cost is unpayable, CR 118.6) and **payment** (remove N energy counters, CR
107.14: "To pay {E}, a player removes one energy counter from themselves").
Longtusk Cub's activated ability carries `PayEnergy 2`.

A `Natural`, not a `Quantity` — the same reasoning as `PayLife`: a `Quantity`
needs a binding environment a cost has no access to at CR 601.2f time, and no
card in the pool pays a variable amount of energy (§8). Energy-specific,
not a general `PayPlayerCounters` — energy is the only player counter ever spent
as a cost (poison is never paid), matching `PayLife`'s specificity over a
hypothetical `PayResource`.

### 2.8 The poison-at-ten SBA

`Pawl.Sba.losesNow` gains the CR 704.5c clause: a player with **ten or more**
poison counters loses the game. It joins the existing disjunction (life ≤ 0, drew
from empty library) in the same function, checked in the same settle loop, so the
poison loss is simultaneous with and ordered exactly like the others. CR 104.3d's
"next time a player would receive priority" phrasing is subsumed by the SBA
framing — SBAs are checked whenever a player would receive priority (CR 704.3).

### 2.9 The deals-combat-damage-to-a-player trigger

`Pawl.Type.TriggerCondition` gains `SelfDealsCombatDamageToPlayer` (CR 603.2 /
509–510). `Pawl.Event.matchesTrigger` gains its arm: it matches a
`GameEvent.DamageDealt ev` where `DamageEvent.source ev == bearer`,
`DamageEvent.kind ev == DamageKind.Combat`, and `DamageEvent.target ev` is a
player (`Recipient.ToPlayer`). It rides P4's event-history substrate — combat
damage already records `DamageDealt` events (`Pawl.Damage.applyDamage`), so no
new recording is needed. Longtusk Cub's triggered ability bears this condition.

Note the pleasing interaction the tests cover: an *infect* creature dealing
combat damage to a player still emits a `DamageDealt` event (the poison is a
consequence of it), so an infect creature with this trigger would still fire —
the diversion is in what the damage *does*, not in whether it *happened*.

### 2.10 Serialization

`Pawl.Codec` gains encode/decode for: `PlayerCounterKind` (both constructors),
the new `Player.counters` field, `Keyword.Infect`, `DamageEvent.dealtByInfect`,
`Effect.GainPlayerCounters`, `CostComponent.PayEnergy`, and
`TriggerCondition.SelfDealsCombatDamageToPlayer`. `Pawl.Codec` cases on every
constructor as the JSON data boundary — that is not the rules core casing on
identity.

## 3. The two invariants

- **The rules core never cases on an effect's identity.** `applyDamage` reads
  `DamageEvent.dealtByInfect` — a classification bit — not `case keyword`, and
  not `case effect`. `Pawl.Cost` reads `CostComponent`'s payability
  classification. `Pawl.Sba` reads a poison *count*. The only new casing homes
  are the sanctioned ones: `Pawl.Resolve` (the effect interpreter),
  `Pawl.Cost` (the cost interpreter), `Pawl.Event` (the trigger matcher), and
  `Pawl.Codec` (the data boundary).
- **The engine makes no player's choice.** Poison and energy gains are forced by
  the rules (no choice to elide). Longtusk Cub's `PayEnergy` cost is a payment
  the *player* chooses to make by activating the ability; the engine only judges
  payability. Nothing here elides a prompt.

## 4. What this phase does **not** touch

- `Pawl.Projection` — player counters are not object characteristics; the layer
  system (CR 613) is untouched. (The −1/−1 counters infect places *are* read by
  the projection, but through the counter path M4f already built.)
- The layer-6 ability system — the counter→layer-6 path is deferred (§8).
- The combat structure — infect changes damage *results*, not assignment.
- `PlayerEffect` (P7) — a player counter is state, not a continuous effect.

## 5. Cards and tests

### Rulings discipline (design.md §4)

Every CR number in this spec is to be re-checked against `docs/rules.txt` before
it drives code, and cited in the code comment. Card text is from Scryfall
(2026-07-23), never recalled (`card-data-source`).

### Glistener Elf — `data/cards/glistener-elf.json`

`{G}` 1/1 with the `Infect` keyword. Tests:

- Attacks an open player → that player gains `amount` poison, loses no life.
- Attacks into a blocker → the blocker gains `amount` −1/−1 counters, has zero
  marked damage; if reduced to zero toughness it is buried by the 704.5f SBA.
- The −1/−1 counters annihilate against +1/+1 counters (704.5q) when both present.
- Ten cumulative poison → the poisoned player loses (704.5c) at the next SBA
  check; nine does not.
- Glistener Elf's *controller* gains no poison (120.3b falsifier).

### Longtusk Cub — `data/cards/longtusk-cub.json`

`{1}{G}` 2/2 Cat with the combat-damage trigger and the pay-energy ability.
Tests:

- Deals combat damage to a player → controller gains two energy (the trigger
  fires; `GainPlayerCounters Energy 2` resolves).
- Deals combat damage to a *creature* (blocked) → no energy (the trigger is
  to-a-player).
- With ≥ 2 energy, the pay ability is payable; activating it removes two energy
  and puts a +1/+1 counter on the Cub.
- With < 2 energy, the pay ability is unpayable (118.6 / 107.14).

### Codec

Round-trip tests for the two card JSONs and for each new constructor / the new
`Player.counters` field.

## 6. Module and type changes (summary)

New:

- `Pawl.Type.PlayerCounterKind` — `Energy | Poison`.
- `data/cards/glistener-elf.json`, `data/cards/longtusk-cub.json`.

Changed:

- `Pawl.Type.Player` — `+ counters :: Map PlayerCounterKind Natural`.
- `Pawl.Type.Keyword` — `+ Infect`.
- `Pawl.Type.DamageEvent` — `+ dealtByInfect :: Bool`.
- `Pawl.Type.Effect` — `+ GainPlayerCounters PlayerCounterKind Quantity`.
- `Pawl.Type.CostComponent` — `+ PayEnergy Natural`.
- `Pawl.Type.TriggerCondition` — `+ SelfDealsCombatDamageToPlayer`.
- `Pawl.Damage` — set `dealtByInfect` at both combat sites; divert infect damage
  in `applyDamage`.
- `Pawl.Resolve` — set `dealtByInfect` on the noncombat `DealDamage` event; apply
  `GainPlayerCounters`; handle the new effect in the projection/legality helpers
  (targetless, like `Draw`/`AffectPlayers`).
- `Pawl.Cost` — payability + payment for `PayEnergy`.
- `Pawl.Sba` — poison-at-ten clause in `losesNow`.
- `Pawl.Event` — `matchesTrigger` arm for `SelfDealsCombatDamageToPlayer`.
- `Pawl.Setup` — initialize `Player.counters` empty.
- `Pawl.Codec` — all of the above.
- Test suite — `Pawl.SbaSpec`, `Pawl.DamageSpec`, `Pawl.CostSpec`,
  `Pawl.EventSpec`, `Pawl.CodecSpec` (and any card-integration spec).

## 7. Ordering within the phase (for the plan)

1. `PlayerCounterKind`; `Player.counters` field; `Setup` init; codec + round-trip.
2. `Keyword.Infect`; `DamageEvent.dealtByInfect`; set the bit at all three sites.
3. Infect diversion in `applyDamage` (player→poison, creature→−1/−1); tests.
4. Poison-at-ten SBA in `losesNow`; tests. **Glistener Elf lands here.**
5. `GainPlayerCounters` effect; apply in `Resolve`; codec; tests.
6. `PayEnergy` cost component; payability + payment in `Cost`; codec; tests.
7. `SelfDealsCombatDamageToPlayer` trigger; `matchesTrigger` arm; codec; tests.
   **Longtusk Cub lands here.**

Each step is one small complete commit on `main`, TDD (failing test first).

## 8. Deferred, with named expiries

Each gets a GitHub issue carrying status, rationale and expiry trigger; the code
site cites only what is not implemented, plus `(#N)`.

- **Counter→layer-6 ability-granting path** (rest of GAP-C) — keyword counters
  (CR 122.1b). No card in the pool; `expires:card-driven`. (issue to file)
- **Toxic** (CR 702.164) — combat damage to a player adds poison *in addition
  to* the damage; a second diversion shape. `expires:card-driven`. (issue to file)
- **Poisonous** (CR 702.70) — a triggered ability giving poison; rides existing
  trigger machinery + this substrate. `expires:card-driven`. (issue to file)
- **Proliferate** (CR 701.27) — a keyword action adding counters, including
  player counters. `expires:card-driven`. (issue to file)
- **Targeted / variable player counters** — "target player gets N poison";
  needs a player recipient on `GainPlayerCounters` (or a sibling). (issue to file)
- **Variable energy cost** — `PayEnergy` is `Natural`; a `Quantity`-valued
  energy cost has no card. `expires:card-driven`. (issue to file)
- **CR 614 player-counter replacement funnel** — an energy/poison-doubling
  replacement; energy gain and infect poison are added directly today.
  `expires:card-driven`. (issue to file)
- **Experience / rad counters** — further player-counter kinds; VOCAB, one
  constructor each once a card wants them. `expires:card-driven`. (issue to file)
- **Mana of any color** — the mechanism that blocked Aether Hub as the energy
  gate; a color-choice prompt, unrelated to counters. `expires:card-driven`.
  (issue to file)
- **Two-Headed Giant poison sharing** (CR 704.6b / 810) — out of scope
  (design.md §6).

## 9. Tracking

Close issue #6 when this phase lands. File the deferral issues above before
citing them in code (CLAUDE.md: file the issue, cite it inline; never write the
expiry into the comment). Add the P10 completion entry to `docs/progress.md`,
and replace — do not append — the status bullet in `CLAUDE.md`.

## 10. Exit criterion

Glistener Elf can be cast, attack, and (a) poison a player to a loss at ten and
(b) shrink and kill a blocker with −1/−1 counters — all with no life loss and no
marked damage. Longtusk Cub can gain energy on connecting and spend it to grow
itself, with the ability unpayable below cost. `Player` carries a counter map;
`PlayerCounterKind` is distinct from `CounterKind`; the poison-at-ten SBA lives
beside life ≤ 0. The build is warning-clean, `hooky run` passes, and every rules
claim is checked against `docs/rules.txt` with the CR number cited in-code.
