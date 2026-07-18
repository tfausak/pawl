# M3b continuous effects — design

Design for milestone **M3b**, the second letter of M3 (see the split table in
`docs/design.md`): **the projection generalized** — continuous effects, the
single-effect layer system, and durations. The axis is the one where
`Game.powerOf` / `toughnessOf` / `keywordsOf` stop being direct reads of the
printing and become a **layer fold** over base characteristics plus continuous
effects. Three gate cards, each Scryfall-verified:

| Card | Cost | Oracle text | Layers | Form |
|---|---|---|---|---|
| **Giant Growth** | `{G}` Instant | "Target creature gets +3/+3 until end of turn." | 7c | stored duration effect |
| **Serpent's Gift** | `{2}{G}` Instant | "Target creature gains deathtouch until end of turn." | 6 | stored duration effect |
| **Humility** | `{2}{W}{W}` Enchantment | "All creatures lose all abilities and have base power and toughness 1/1." | 6 + 7b | static ability |

The falsifier the design names is **grant/remove deathtouch between damage-deal
and SBA check**: `Sba.woundedByDeathtouch`'s live read (its own comment names
this expiry) becomes wrong, so `DamageEvent` grows a **deal-time deathtouch
bit** (CR 702.2c/702.2e). The *mid-step divergence* that makes the live read
observably wrong needs an instant-speed remover and lands at M3e; what M3b
proves is that the bit reflects the **deal-time projection** — Serpent's Gift →
`true`, Humility → `false` — and that this is what the SBA reads.

A second interaction is genuinely load-bearing here and easy to misfile as
M3c's: Humility (remove all abilities) and Serpent's Gift (grant deathtouch)
both apply in **layer 6** to War Mammoth, so CR 613.7 **timestamp** order
decides whether it keeps deathtouch — and both orders are constructible. This is
613.7 (within-layer timestamp), **not** 613.8 (dependency / trial application),
so it is M3b's to land, and it forces timestamps to be real and comparable
across a static source (Humility) and a stored effect (Serpent's Gift).

This is a types-and-architecture spec, not an implementation plan.

## Goal and scope

**Exit criterion.** Deterministic tests demonstrate all of:

- Giant Growth pumps a creature +3/+3, and the effect **wears off at cleanup**
  (CR 514.2), the creature returning to its printed P/T with no code that
  "undoes" anything (delete-and-recompute, design.md §2.5).
- Humility makes every creature **1/1 with no abilities** (layers 6 and 7b), and
  its 7b toughness drop **kills an already-damaged creature** the next SBA check
  (CR 704.5g reads the projected toughness).
- Serpent's Gift grants **deathtouch to War Mammoth**, reproducing M2c's
  deathtouch+trample interaction (CR 702.2c) from real cards — retiring the
  synthetic both-keywords fixture.
- Humility vs. Serpent's Gift resolves by **timestamp in both orders** (CR
  613.7): older Humility then newer grant → War Mammoth has deathtouch; the
  reverse → it does not.

Random green-black games cast Giant Growth and Serpent's Gift under random play
with every M2d/M3a invariant holding; some seed casts each. The `DecisionLog`
replays deterministically with the new stored continuous-effect state.

**Non-goals.**

- **No CR 613.8 dependency / trial application** — M3c, the go/no-go. M3b sorts
  a layer purely by timestamp; no effect changes whether another *applies*,
  *exists*, or *what layer* it is in. "Single-effect" in the split table means
  exactly this: linear layer-ordered fold, no dependency graph.
- **No layer 3 text-changing** (M3d), **no layers 1, 2, 4, 5** (copy, control,
  type, color — no card needs them), **no 7a CDA/Star**, **no 7d P/T switch**.
  The `Layer` type enumerates them for diffability; only 6, 7b, 7c have
  producers.
- **No ETB/LTB triggers.** Static-ability effects (Humility) are **re-derived
  live** from the battlefield each projection, never added or removed by a hook
  — the 603/614 pipeline is M3f. This is why static effects are not stored.
- **No memoized projected state.** Each projection query recomputes the fold;
  the cached lazy projected-state field (design.md §2.5/§2.10, risk register)
  is deferred with an expiry.
- **No white matchup.** Humility is `{2}{W}{W}`; there is no white deck, so
  Humility is a **deterministic fixture only**, never cast in a random game. The
  random-coverage tail trails again, per the M3 note (an M2d-style white/blue
  matchup follows M3, it is not discovered here).
- **No X, no modes, no counterspells, no new card types.** No serialization or
  AST version field (M3d's rewriting or M6's loader bring the first consumer).

## 1. New types

**`Pawl.Type.Timestamp`** — `newtype Timestamp = MkTimestamp Natural`, drawn from
a monotonic **`GameState.nextTimestamp`** counter (shaped like `nextObjectId`).
CR 613.7a: a static ability's timestamp is the timestamp of the object it is on,
which per CR 613.7d is when that object entered the battlefield. So **`Object` grows
`timestamp :: Timestamp`**, assigned when the object is created — in
`Game.changeZone` (a fresh permanent's entry, CR 400.7) and in `Setup`'s initial
placement. Stored continuous effects draw from the **same** counter at creation.
One comparable sequence is what lets Humility (static, timestamp from its own
Object) and Serpent's Gift (stored, timestamp from creation) order against each
other in layer 6.

*Not* the object id reused as a timestamp: id is identity and timestamp is
entry-order, and though both are monotone today, conflating them is exactly the
kind of pun the project's explicit-types rule rejects. A dedicated newtype and
counter say what they mean.

**`Pawl.Type.Layer`** — the CR 613 layer enumeration, ordered by rule number,
`Ord`-derived so the derived order *is* the application order. Complete for
diffability against CR 613 (the `Keyword` posture), but only the sublayers with
producers are exercised:

```haskell
data Layer
  = Copy            -- 613.1a, layer 1
  | Control         -- 613.1b, layer 2
  | Text            -- 613.1c, layer 3
  | Type            -- 613.1d, layer 4
  | Color           -- 613.1e, layer 5
  | Ability         -- 613.1f, layer 6
  | CharacteristicPT -- 613.1g / 613.3, layer 7a (CDAs)
  | SetPT           -- layer 7b
  | ModifyPT        -- layer 7c
  | SwitchPT        -- layer 7d
  deriving (Eq, Ord, Show)
```

`Ord` is the only ordering the type owes: the derived constructor order *is*
CR 613's application order, and that is the sole thing `project` sorts on. No
`Enum`/`Bounded` — nothing enumerates layers or asks for bounds.

**`Pawl.Type.Modification`** — the open-half continuous-effect vocabulary, *its
own leaf family* distinct from `Effect` (design.md's M3g note: "continuous-effect
specifications, classified by layer"). Classification data plus its payload;
`Modification.layer :: Modification -> Layer` is the ABI classification the rules
core asks:

```haskell
data Modification
  = GainKeyword Keyword                        -- layer 6 (Serpent's Gift)
  | LoseAllAbilities                           -- layer 6 (Humility)
  | SetBasePowerToughness Quantity Quantity    -- layer 7b (Humility 1/1)
  | ModifyPowerToughness Quantity Quantity     -- layer 7c (Giant Growth +3/+3)
  deriving (Eq, Ord, Show)
```

`GainKeyword` takes a `Keyword` (closed-half citation — casing on it is not an
invariant violation, per the M2a spec). The P/T constructors take signed
`Quantity` (Giant Growth is `+3/+3`; a future `-1/-1` is the same 7c
constructor). `layer` is a total function; a new constructor without a `layer`
case fails the build (`-Weverything`), the same executor-coverage hygiene the
`Resolve` incomplete-pattern warning gives `Effect`.

**`Pawl.Type.Duration`** — `data Duration = UntilEndOfTurn`, the only stored
duration at M3b. Static abilities carry no `Duration`: they exist while their
source and ability do, which is exactly "while re-derived from the battlefield."
Grows `WhileSourceOnBattlefield`, `UntilYourNextTurn`, etc., as cards need them.

**`Pawl.Type.ContinuousEffect`** — a stored, resolution-generated effect:

```haskell
data ContinuousEffect = MkContinuousEffect
  { source :: ObjectId,          -- the object whose resolution created it
    timestamp :: Timestamp,      -- CR 613.7, from nextTimestamp at creation
    duration :: Duration,        -- CR 514.2 drops UntilEndOfTurn at cleanup
    modification :: Modification, -- classified by layer
    affected :: Affected         -- CR 611.2c fixed set
  }
  deriving (Eq, Ord, Show)
```

**`Pawl.Type.Affected`** — what an effect applies to:

```haskell
data Affected
  = TheseObjects (Set ObjectId)  -- CR 611.2c: locked when the effect began
  | AllCreatures                 -- dynamic, re-evaluated each projection (Humility)
  deriving (Eq, Ord, Show)
```

`TheseObjects` is a resolution effect's fixed set (Giant Growth / Serpent's Gift
lock the target's id at resolution; CR 400.7 means a bounced-and-returned
creature is a new id the effect no longer names — Giant Growth correctly stops).
`AllCreatures` is Humility's dynamic set: any creature currently on the
battlefield, which is why static effects must be live-derived, not a fixed set
captured once.

**`Pawl.Type.StaticAbility`** — a card's printed static continuous ability:

```haskell
data StaticAbility = MkStaticAbility
  { affected :: Affected,
    modification :: Modification
  }
  deriving (Eq, Ord, Show)
```

Humility declares two: `MkStaticAbility AllCreatures LoseAllAbilities` and
`MkStaticAbility AllCreatures (SetBasePowerToughness (Literal 1) (Literal 1))`.

### Growing existing types

- **`Effect`** gains `ModifyTarget Duration Modification SlotName`. Giant Growth
  and Serpent's Gift are the **same opcode**, differing only in the
  `Modification` — the proof that one create-a-duration-effect leaf covers both
  layer 7c and layer 6. `Resolve` reads the slot's target, builds a
  `ContinuousEffect` affecting that one object, and appends it; it constructs the
  record but never cases on `Modification` (§3).
- **`Card`** gains `staticAbilities :: [StaticAbility]`, `[]` for every existing
  printing.
- **`DamageEvent`** gains `dealtByDeathtouch :: Bool`, set at deal time (§4).
- **`TargetSpec`** gains `CreatureTarget` (§4).
- **`GameState`** gains `continuousEffects :: [ContinuousEffect]` and
  `nextTimestamp :: Timestamp`.
- **`Object`** gains `timestamp :: Timestamp`.

## 2. The projection: `Pawl.Projection`

New logic module **`Pawl.Projection`** — the **single legitimate home of
`case`-on-`Modification`**, the continuous-effect analogue of `Resolve`'s
standing over `Effect`. The invariant split is exact:

> `Resolve : Effect  ::  Projection : Modification`

The rules core (`Combat`, `Damage`, `Sba`, `Engine`, `Cast`, `Target`) never
matches a `Modification`; it asks `Modification.layer` and reads the projected
characteristics. `-Weverything`'s incomplete-pattern warning is the applier's
coverage check at compile time.

```haskell
data ProjectedCharacteristics = MkProjectedCharacteristics
  { keywords :: Set Keyword,
    power :: Maybe Integer,
    toughness :: Maybe Integer
  }
  deriving (Eq, Show)

project :: ObjectId -> GameState -> ProjectedCharacteristics
```

`project` runs three steps:

1. **Gather** the applied effects touching this object, from both sources:
   - each `GameState.continuousEffects` entry whose `affected` includes the
     object, tagged with its stored `timestamp`;
   - for every permanent on the battlefield, each of its card's
     `staticAbilities` whose `affected` includes the object, tagged with that
     **permanent's `Object.timestamp`** (CR 613.7a).

   An `affected` membership test: `TheseObjects s` → `Set.member oid s`;
   `AllCreatures` → `oid` is a creature on the battlefield (read through the
   printed type line — layer 4 type-changing does not exist at M3b).

2. **Sort** the gathered effects by `(Modification.layer, timestamp)`. Pure CR
   613.7; no dependency step (that is M3c). A same-layer, same-timestamp tie
   (two Giant Growths, no intervening object) is order-independent here because
   the colliding modifications commute; the resolver does not rely on that and a
   tiebreak lands with 613.8.

3. **Fold** the applier over base characteristics — the printed keywords and the
   evaluated printed P/T (`Quantity.evaluate`, `Nothing` for a land) — the sole
   `case`-on-`Modification`:
   - `LoseAllAbilities` → `keywords := Set.empty`;
   - `GainKeyword k` → `keywords := Set.insert k`;
   - `SetBasePowerToughness p t` → set both, but only for an object that has P/T
     (guard on `Maybe`, the `powerOf` posture; Humility's `AllCreatures` already
     restricts to creatures, so this is belt-and-suspenders);
   - `ModifyPowerToughness dp dt` → add to each, `Nothing` staying `Nothing`
     (adding to a land's absent power is a no-op, never a crash).

**`Game.keywordsOf` / `powerOf` / `toughnessOf` become thin wrappers** over
`project` — **their M2a/M2c/M3a expiries discharged**. Because a code audit
confirms `Combat`, `Damage`, `Sba`, and `Resolve` read power/toughness/keywords
*only* through these three functions (zero direct `Card.power`/`toughness`/
`keywords` outside `Game`/`Card`), continuous effects reach combat, SBAs, and
targeting with **no change to their logic** — the whole payoff of the M2a
projection discipline. `Game.hasKeyword` stays a `Set.member` over `keywordsOf`.

**Perf.** Each query re-runs the fold; combat and SBAs call these functions
often. Acceptable at this scale; the cached lazy projected-state field
(design.md §2.5/§2.10, risk register's thunk-leak line) is the named optimization
and is deferred with an expiry (§6). Placing `project` as the one gather-sort-fold
funnel is what makes that later cache a memoization rather than a rewrite.

## 3. Resolution, static abilities, wear-off

**`Resolve`** (still the sole `case`-on-`Effect`) gains the `ModifyTarget`
branch: read the recipient the named slot was filled with; **evaluate-and-freeze**
the modification's quantities against the current state (CR 611.2b's latch — a
no-op while every `Quantity` is a `Literal`, placed so a future `X` locks its
value at creation rather than drifting); construct a `MkContinuousEffect` with
`affected = TheseObjects (Set.singleton target)`, a fresh timestamp from
`nextTimestamp`, and the card's `Duration`; append it to
`GameState.continuousEffects`. The spell then goes to its owner's graveyard (CR
608.2n), exactly as `DealDamage` does. If the target is now illegal (CR 608.2b),
the effect is a no-op and nothing is appended — the same partial-resolution
posture `DealDamage` already takes.

`Resolve` constructs the `ContinuousEffect` record but does **not** case on its
`Modification` — it stores it opaquely. The two executors stay cleanly split:
`Resolve` turns an `Effect` into stored state; `Projection` turns stored state
into characteristics.

**Static abilities need no resolution path.** Humility is a permanent spell that
resolves onto the battlefield by the ordinary M1a permanent branch; its
`staticAbilities` are gathered live by `project` while it sits there. Nothing is
stored for it, and nothing removes it when it leaves — it simply stops being
gathered. (Humility's own printed reminder that it affects "all creatures" is not
a target; it takes no `targetSpecs`.)

**Wear-off (CR 514.2).** The cleanup step already runs `Damage.removeAllDamage`;
it gains one sibling `State.modify'` dropping every `UntilEndOfTurn` entry from
`GameState.continuousEffects`. Delete-and-recompute: after the drop, the next
`project` reflects the reverted state — Giant Growth's creature is its printed
P/T again — with no explicit undo. CR 514.2 also removes marked damage in the
same turn-based action, so the two are simultaneous, as the rule states.

## 4. Targeting and the deal-time deathtouch bit

**`TargetSpec.CreatureTarget`.** `Target.legalRecipients` gains the creature-only
case (creatures on the battlefield, as `Recipient.ToCreature`); `Target.stillLegal`
requires the target still be a creature on the battlefield. This **falsifies
M3a's targeting gate**: a spell whose only slot's legal set is empty is
uncastable (CR 601.2c), which M3a wrote but could not exercise because
`AnyTarget` always held a living player. Giant Growth with no creature on the
battlefield is now un-castable, and a test asserts it. Giant Growth and Serpent's
Gift both use `CreatureTarget`; Humility does not target.

**The deal-time deathtouch bit.** `DamageEvent` grows `dealtByDeathtouch :: Bool`.
`Damage.applyDamage`, when it constructs an event, sets the bit from
`Game.hasKeyword Keyword.Deathtouch source gs` **at the moment damage is dealt**
— now a projected read, so Serpent's Gift makes it `true` and Humility makes it
`false`. `Sba.woundedByDeathtouch` reads the stored `dealtByDeathtouch`
**instead of** re-querying `hasKeyword` at check time. This is CR 702.2e's
last-known-information made structural: the wound records what the source *was*,
not what the projection later says.

The bit's *deal-time-projection* behavior is fully testable at M3b (Serpent's
Gift → `true`, Humility → `false`; §5). The *divergence* it also protects against
— deathtouch present at deal, absent at check — needs a state change between the
combat-damage deal and the immediately-following SBA check, and combat offers no
such window until an instant-speed remover exists. That falsifier is documented
as an **M3e expiry** (§6), not left implicit; the bit lands now so M3e is a
widening, not a re-opening of `Damage`/`DamageEvent`/`Sba` (the funnel discipline
the M2b and M3a specs insist on).

CR 702.2c (any nonzero deathtouch assignment is lethal, which is trample's excess
calculation) already runs through `Damage.blockerThreshold`; with Serpent's Gift
feeding the projection, War Mammoth's threshold collapses to 1 and the deathtouch
+ trample interaction is exercised from real cards, replacing M2c's synthetic
fixture.

## 5. Setup, decks, and testing

**Decks.** `greenDeck` gains Giant Growth and Serpent's Gift (both green),
recomposed to keep 36 land + 24 spells = 60 so `objectCount` and conservation
are untouched (the M3a posture). War Mammoth stays in `greenDeck`, so a random
green-black game can cast Serpent's Gift on it and produce the 702.2c interaction
under random play. **Humility is not in any deck** (no white matchup); it enters
tests as a deterministic fixture placed directly on the battlefield. `Card`
gains the three printings, all registered in `allPrintings` so the §M3a slot
lint covers Giant Growth and Serpent's Gift (their `ModifyTarget` slot must
equal their `targetSpecs` key set).

**`Setup`** assigns each initially-placed object a `timestamp` from
`nextTimestamp`, and `nextTimestamp`/`continuousEffects` get their empty/initial
values in `emptyGame`.

**Deterministic tests**, CR-numbered, real cards:

- **Giant Growth pump + wear-off** (CR 611.2, 514.2): cast on a creature → +3/+3
  through `project`; at cleanup the effect is dropped and P/T reverts.
- **Humility base P/T and ability loss** (CR 613, layers 7b/6): every creature
  reads 1/1 with empty `keywordsOf`; a printed flier under Humility cannot use
  flying (blocking legality through `keywordsOf`).
- **Humility kills an already-damaged creature** (CR 704.5g after 7b): War
  Mammoth (3/3) marked 2 damage survives; Humility resolves → toughness 1 → the
  next SBA check buries it (2 ≥ 1), driven purely by the projected toughness.
- **Giant Growth on a Humility'd creature = 4/4** (layer order 7b before 7c):
  base set to 1/1, then +3/+3 — asserts the fold applies sublayers in order, not
  by arrival.
- **Serpent's Gift → deathtouch + trample** (CR 702.2c): Serpent's Gift on War
  Mammoth; its combat `DamageEvent.dealtByDeathtouch` is `true`; a blocker's
  lethal threshold collapses to 1 and trample assigns the excess — the M2c
  interaction from real cards.
- **The deathtouch bit under removal** (CR 702.2e / 613 layer 6): Typhoid Rats
  under Humility deals combat damage; its `DamageEvent.dealtByDeathtouch` is
  `false`, so a non-lethally-damaged creature is *not* destroyed by 704.5h.
- **Humility vs. Serpent's Gift, both timestamp orders** (CR 613.7): with
  Humility older, resolve Serpent's Gift after → War Mammoth keeps deathtouch;
  with Serpent's Gift older, Humility after → it does not. The test controls
  order by the sequence of resolutions/placements, reading `nextTimestamp`'s
  monotonicity.
- **The creature-only targeting gate** (CR 601.2c): Giant Growth is not offered
  as a legal action when no creature is on the battlefield; offered when one is.

**Properties** (`runMatch`, both matchups): every M2d/M3a invariant as it stands
— conservation, termination, ids, no floating mana, life never increases (a
Giant Growth or a grant never raises a total), combat happens, green-black
engagement — plus two watched engagement guards: **some green seed casts Giant
Growth** and **some seed casts Serpent's Gift** (so continuous effects cannot
silently never fire while the suite stays green). Replay determinism runs as
before and now covers `continuousEffects` and object `timestamp`s in the
serialized decision path. The benchmark stays on `redDeck` (unchanged — the new
cards are green); throughput is watched for the per-query fold cost, not
asserted.

## 6. What M3b preserves

- **The two invariants.** `Projection` is the continuous-effect executor,
  `Resolve` the one-shot executor; no rules-core module cases on `Modification`
  or `Effect`. No prompt is elided that isn't forced (target choice for Giant
  Growth / Serpent's Gift is always prompted, even a singleton legal set, per
  M3a).
- **Combat / SBA / Damage logic**: unchanged. They read the generalized
  `keywordsOf`/`powerOf`/`toughnessOf`; `applyDamage` gains one field write
  (the bit), not a behavior change; `woundedByDeathtouch` swaps a live read for
  a stored read.
- **M3a's targeting and pay-first postures**: `ChooseTargets`, reject-not-repair,
  the pre-validated-offers elision (expiring at mid-announcement failure, M3g)
  all stand; `CreatureTarget` is a new spec constructor, not a new mechanism.
- **`Damage.legalAssignment` / `blockerThreshold` / `attackerAssignment`**:
  untouched; the `attackerAssignment` chooser expiry (banding/Mindslaver) still
  points at M3g.

### Expiries this milestone opens

- **CR 613.8 dependency**: the layer sort is timestamp-only; trial application
  and the dependency graph are M3c (the go/no-go). The Blood Moon + Urborg and
  Humility + Opalescence test sets must exist before that resolver.
- **The deathtouch bit's mid-step divergence**: deathtouch changing between the
  combat-damage deal and the SBA check is unobservable until an instant-speed
  remover exists — **M3e**. The deal-time capture is in place so that lands as a
  test, not a re-architecture.
- **Memoized projected state**: `project` recomputes per query; the cached lazy
  projected-state field (design.md §2.5/§2.10) arrives when throughput demands
  it, as a memoization of this one funnel.
- **`Layer` / `Modification` / `Duration` / `Affected`** each grow a constructor
  per new continuous shape; the layers without producers (1–5, 7a, 7d) gain
  appliers with their first card.
- **The 611.2b latch is a no-op** until `Quantity` has a non-`Literal`
  constructor (X); the freeze point is placed so it becomes correct without a
  call-site change.
- **`AllCreatures` reads the printed type line**; when layer 4 type-changing
  exists, "creature" becomes a projected question — one function, the same move
  as `keywordsOf`.

### Explicitly deferred past M3b

- **613.8 dependency / trial application** and its test set — M3c.
- **Layer 3 text-changing (Magical Hack)** — M3d, the go/no-go verdict.
- **Activated / triggered abilities, the 603/614 pipeline, the general `Event`
  type** — M3e/M3f. Static effects are live-derived precisely to avoid needing
  ETB/LTB hooks now.
- **A white or blue matchup** for random continuous-effect coverage (Humility,
  the white gates) — the M2d-style tail after M3.
- **X, modes, counterspells, new card types, serialization / AST version field.**

## 7. Convention note

M3b introduces record-heavy code (`ContinuousEffect`, `StaticAbility`,
`ProjectedCharacteristics`, and the several `GameState`/`Object` field additions).
`NamedFieldPuns` is **permitted** where it improves clarity — an amendment to the
"only `GADTs` and `RankNTypes`" rule agreed at this spec's brainstorming. It does
**not** relax the separate non-punning rule for *constructor* names (`MkFoo`
stays): field puns and constructor-name puns are different things. The prose
rules in `CLAUDE.md` (§Code conventions) and `CONTRIBUTING.md` (style) should be
updated to record the amendment when the plan lands.
