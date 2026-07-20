# M4f counters — design

Design for milestone **M4f**, the sixth letter of M4 (see the split table in
`docs/design.md` §3): **counters as persistent permanent state.** Every P/T
modifier so far has been either a printed value (M1a) or a *continuous effect*
with a duration (M3b's Giant Growth, gone at cleanup). A counter is neither: it is
a marker on a permanent (CR 122.1) that modifies P/T (CR 122.1a) and **persists
until the permanent leaves the battlefield** — it is not durational and cleanup
never touches it. M4f proves the engine can carry that state, feed it into the
layer system at the right sublayer, and run the counter-specific state-based
action.

Two gate cards, Scryfall-verified
(`api.scryfall.com/cards/named?exact=…`, fetched 2026-07-20):

| Card | Cost | Type line | Oracle text |
|---|---|---|---|
| **Battlegrowth** | `{G}` | Instant | "Put a +1/+1 counter on target creature." |
| **Instill Infection** | `{3}{B}` | Instant | "Put a -1/-1 counter on target creature.\nDraw a card." |

**The falsifier — CR 704.5q / 122.3 annihilation.** When a permanent has both a
+1/+1 and a -1/-1 counter, N of each are removed as a state-based action (N = the
smaller count). An engine that stored a counter's effect as a single *net* P/T
`Integer` on the object **cannot represent "has both a +1/+1 and a -1/-1 counter"**
— net −1/−1 and net +1/+1-then-−2/−2 are indistinguishable — so 704.5q has nothing
to act on. The gate forces the data model to keep **typed counts per counter
kind**, not a collapsed net delta. A second, sharper falsifier rides Battlegrowth
alone: its +1/+1 must **survive cleanup and the next turn**, where Giant Growth's
+3/+3 (the same layer, 7c) wears off — the line between a counter and a continuous
effect.

**A rules correction this spec makes.** `docs/design.md`'s M4f row says counters
are "layer 7d (below Giant Growth's 7c)." That is a recalled-rules error (the trap
`CLAUDE.md` names). **CR 613.4c**: "Effects *and counters* that modify power and/or
toughness (but don't set …) are applied" — layer **7c**, the *same* sublayer as
Giant Growth. CR 613.4d (7d) is P/T *switching*, unrelated. So M4f adds **no new
`Layer` constructor**; counters reuse `Modification.ModifyPowerToughness` at
`Layer.ModifyPT` (7c). The design-doc row is corrected as part of this milestone.

This is a types-and-architecture spec, not an implementation plan.

## 0. The core idea

**A counter is persistent per-incarnation state on the object, projected as a
layer-7c P/T modification.** Three moving parts, each built on an existing seam:

1. **Storage.** A new `Object.counters :: Map CounterKind Natural`, in the same
   per-incarnation family as `damage`, `sickness`, and `bindings`: reset when the
   object changes zones (CR 122.2 — counters "simply cease to exist"; the CR 400.7
   new-incarnation mechanism that already resets those fields), and **not** cleared
   at cleanup (counters are not "until end of turn").
2. **The opcode.** `Effect.PutCounters CounterKind Quantity SlotName` — put this
   many counters of this kind on the slot's target (CR 122.6). Executed by
   `Resolve.applyEffect` as a direct in-place edit of the target's `counters`
   (`Map.insertWith (+)`); it is **not** a zone change, so it does not route through
   `Event.changeZone`. Reuses `Quantity` (M4a) and the existing `CreatureTarget`
   (M3b).
3. **Projection.** `Projection.gather` emits one synthetic layer-7c
   `ModifyPowerToughness` per battlefield object carrying P/T counters (CR 122.1a),
   folded by the existing 7c code path alongside Giant Growth.

Plus the state-based action: **CR 704.5q / 122.3** annihilation, a new arm in
`Sba.performStateBasedActions`.

The §1 invariant holds throughout. `CounterKind` is a **classification** of a
marker (`PlusOnePlusOne | MinusOneMinusOne`), the same kind of closed-half tag as
`Keyword`; the rules core reads counts by kind and never learns Battlegrowth's or
Instill Infection's identity. Battlegrowth is a printing whose effect list is
`[PutCounters PlusOnePlusOne (Literal 1) "creature"]`; Instill Infection's is
`[PutCounters MinusOneMinusOne (Literal 1) "creature", Draw (Literal 1)]`.
`Pawl.Resolve` remains the sole module that cases on `Effect`; `Pawl.Projection`
remains the sole module that cases on `Modification`.

## Goal and scope

**Exit criterion.** Deterministic tests demonstrate all of:

- **Battlegrowth puts a +1/+1 counter (CR 122.6, the gate).** A player casts
  Battlegrowth targeting a creature; on resolution the creature has one
  `PlusOnePlusOne` counter, and its projected P/T is +1/+1 over base (CR 122.1a /
  613.4c).
- **The counter persists (the persistence falsifier).** After the turn's cleanup
  (CR 514.2) and into the next turn, the +1/+1 is still there — asserted against a
  same-magnitude Giant Growth (`ModifyTarget UntilEndOfTurn …`) on an identical
  creature, which wears off. Same layer (7c), opposite lifetime.
- **A -1/-1 counter can be lethal (CR 704.5f).** Enough `MinusOneMinusOne` counters
  to bring a creature's projected toughness to 0 or less put it into its owner's
  graveyard as a state-based action — a *put-into-graveyard*, ungated by
  indestructible and unsaveable by regeneration (CR 704.5f verbatim). **This
  retires the synthetic `−0/−1` continuous-effect fixture** that M4b and M4d used
  as a stand-in for a real −1/−1 (their named expiry).
- **Annihilation (CR 704.5q / 122.3, the falsifier).** A creature carrying both a
  +1/+1 and a -1/-1 counter has N of each removed by the state-based action.
  Asserted on **counter counts** (net P/T is unchanged by annihilation, so the
  counts are the observable that a net-`Integer` model could not produce). Includes
  the asymmetric case (e.g. two +1/+1 and one -1/-1 → one +1/+1 remains).
- **Counters cease on zone change (CR 122.2).** A counter-bearing creature bounced
  by Unsummon and recast enters with no counters — the per-incarnation reset.
- **Counters stack in layer 7c with a continuous effect.** Battlegrowth's +1/+1 and
  a Giant Growth on the same creature sum to +4/+4 (both 7c, additive) — a
  confirming test that the projection injection folds through the existing path.

The `DecisionLog` replays deterministically. **No new prompt or response type:**
both cards target through the existing `ChooseTargets` prompt (M3a); putting
counters is unprompted (CR 122.6 places them without a choice at these cards'
text). The honesty round-trip (`jsonToCard . cardToJson ≡ Right`) covers both
cards via new `Codec` arms for `PutCounters` and `CounterKind`.

**Out of scope (named deferred expiries, §9):** counter kinds beyond the two P/T
kinds (keyword, charge, loyalty, poison, shield, stun — CR 122.1b–i); a "counter
placed" event and its triggers (CR 122.7, proliferate); replacement effects that
alter counter placement (Doubling Season, CR 122's replacement interactions);
counters entering *with* a permanent as a replacement (CR 122.6a — distinct from
"put on" during resolution); "move a counter" (CR 122.5); the "can't have more than
N counters" SBA (CR 122.4 / 704.5r); and `Quantity.X`-many counters (rides M4a's
`ChooseX` when a card needs it — Battlegrowth and Instill Infection both put a
`Literal` count).

## 1. The counter classification — `CounterKind`

New module `Pawl.Type.CounterKind`:

```
module Pawl.Type.CounterKind where

-- CR 122: a counter is a marker that modifies characteristics or interacts with a
-- rule (CR 122.1). Its KIND is a closed-half classification -- the same posture as
-- Keyword (a citation, not an effect identity): the rules core reads counts by
-- kind (P/T contribution in CR 613.4c; the CR 704.5q annihilation SBA) and never
-- cases on a card. Only the two P/T-modifying kinds exist at M4f (CR 122.1a);
-- keyword/charge/loyalty/poison/shield/stun counters (CR 122.1b-i) are future.
-- Ord is load-bearing: CounterKind is a Map key on Object.counters.
data CounterKind
  = PlusOnePlusOne -- CR 122.1a: +1/+1
  | MinusOneMinusOne -- CR 122.1a: -1/-1
  deriving (Eq, Ord, Show)
```

No `Enum`/`Bounded` — nothing enumerates counter kinds. The two constructors are
diffable against CR 122.1a; new kinds are additive.

## 2. Storage — `Object.counters`

`Pawl.Type.Object` gains one field:

```
-- CR 122.1: counters placed on this permanent, counted per kind. Persistent
-- permanent state -- unlike `damage`, cleanup does NOT clear it (a counter is not
-- an "until end of turn" effect). Per-incarnation: reset by changeZone, because
-- CR 122.2 says counters "simply cease to exist" when an object changes zones
-- (the CR 400.7 new-object mechanism that also resets damage/sickness/bindings).
-- A +1/+1 or -1/-1 count feeds P/T via the projection (CR 122.1a / 613.4c); both
-- kinds present trigger the CR 704.5q annihilation SBA.
counters :: Map CounterKind Natural
```

Every `Object` construction site initializes `counters = Map.empty`. The two that
matter are the ones that mint a fresh incarnation — `Event.changeZone`'s new-zone
object and `Event.createToken`'s new token — which start empty by construction
(CR 122.2 for the former; a token is created without counters unless an effect says
otherwise, CR 122.6a, out of scope). `Object` is not serialized (only `Card` is),
so `counters` needs no `Codec` arm.

**Projection reads only battlefield permanents' counters**, which is complete: CR
122.2 makes counters cease the moment a permanent leaves the battlefield, so no
object in another zone ever carries one. `Projection.gather` already sweeps only
`GameState.battlefield`; §4 rides that sweep.

## 3. The opcode — `Effect.PutCounters CounterKind Quantity SlotName`

`Pawl.Type.Effect` gains one constructor:

```
| -- CR 122.6: put this many counters of this kind on the slot's target permanent.
  -- Battlegrowth = PutCounters PlusOnePlusOne (Literal 1) slot; Instill Infection
  -- = PutCounters MinusOneMinusOne (Literal 1) slot. A counter is persistent
  -- object state, NOT a zone change -- Resolve.applyEffect edits Object.counters
  -- in place (Map.insertWith (+)), never through Event.changeZone. Quantity is how
  -- many (reused from M4a; a future X-counter card rides ChooseX). The counter's
  -- P/T effect is applied by the projection (CR 122.1a / 613.4c), not here.
  PutCounters CounterKind Quantity SlotName
```

The five Effect-classifying functions in `Pawl.Resolve` each gain a `PutCounters`
arm (all case exhaustively, so the compiler forces each):

- `slotsOf (PutCounters _ _ slot) = Set.singleton slot`
- `readsX`: `PutCounters _ quantity _ → quantity == Quantity.Type.X` (Literal here
  → `False`; the `readsX` NOTE warns the Quantity's X-ness is not compiler-forced,
  so this arm must be added by hand, matching the existing `Draw`/`Mill`/`Discard`/
  `Create` arms)
- `manaProduced (PutCounters …) = Nothing`
- `searchesLibrary (PutCounters …) = False`
- `rewriteEffect _ (PutCounters …) = effect` (identity — no rewritable land-type
  word; a text-changer does not reach a counter)

`applyEffect`'s `PutCounters` arm (the sole `case effect of` home), following the
`DealDamage` idiom exactly — `applyEffect`'s first argument is `source` (the
resolving spell/permanent), the same per-slot legality `Map`s M4b's
`Destroy`/`MoveToZone` consume, and `Quantity` evaluated against **`source`** (not
the target), the `Nothing`/`n <= 0` no-op posture:

```
Effect.PutCounters kind quantity slot ->
  State.modify' $ \gs ->
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> case recipientObject recipient of
        Nothing -> gs                    -- a player recipient takes no counters
        Just target -> case Quantity.evaluate gs source quantity of
          Nothing -> gs                  -- unevaluable quantity: no-op (the powerOf posture)
          Just n -> if n <= 0 then gs else putCounters target kind (fromInteger n) gs
      _ -> gs                            -- illegal slot at resolution (CR 608.2b): no-op
```

where `putCounters :: ObjectId -> CounterKind -> Natural -> GameState ->
GameState` adds the count on the target (`Map.insertWith (+)` into
`Object.counters`). Evaluating against `source` matches Blaze's `DealDamage`: X is
bound on the *resolving spell* at cast (M4a's `variableX`), not on the target — so
a future `X`-many-counters card reads it correctly here with no change, exactly as
`DealDamage source X` does.

**Targeting.** Both cards read "target creature" → the existing
`TargetSpec.CreatureTarget` (M3b). No new target spec. Instill Infection's second
line is the existing `Draw (Literal 1)` (M4b), sequenced after the counter in the
effect list; `Resolve` already runs an effect list in order.

## 4. Projection integration — counters as layer 7c

`Projection.gather` gains a third source of `Gathered` (beside stored continuous
effects and permanents' static abilities): one entry per battlefield object whose
net P/T-counter delta is nonzero.

```
-- CR 122.1a / 613.4c: a +1/+1 counter adds +1/+1 and a -1/-1 counter adds -1/-1,
-- in layer 7c. Emit each battlefield object's counters as ONE synthetic 7c
-- ModifyPowerToughness with net delta d = (#PlusOnePlusOne - #MinusOneMinusOne) on
-- each axis, folded by the same path as Giant Growth. Constructed HERE (Projection
-- is the sole home that may name a Modification constructor). Affected is the
-- object itself; timestamp is the object's own (see the commutativity note).
counterGathered :: GameState -> [Gathered]
counterGathered gs = Maybe.mapMaybe fromObject (Set.toList (GameState.battlefield gs))
  where fromObject oid = case Game.lookupObject oid gs of
          Nothing -> Nothing
          Just obj ->
            let cs = Object.counters obj
                plus  = toInteger (Map.findWithDefault 0 CounterKind.PlusOnePlusOne cs)
                minus = toInteger (Map.findWithDefault 0 CounterKind.MinusOneMinusOne cs)
                d = plus - minus
             in if d == 0 then Nothing else Just (MkGathered
                  { gSource = oid, gAffected = Affected.TheseObjects (Set.singleton oid),
                    gLayer = Layer.ModifyPT, gTimestamp = Object.timestamp obj,
                    gModification = Modification.ModifyPowerToughness (Quantity.Literal d) (Quantity.Literal d) })
```

appended in `gather` (`stored ++ static_ ++ counterGathered gs`).

**Why a single net `ModifyPowerToughness` is correct — and why the timestamp is
unobservable.** Layer 7c is, by CR 613.4c, effects and counters that *modify* (not
*set*) P/T — every 7c modification is an addition, and addition is commutative and
associative. So the sum of a creature's counter deltas may be pre-combined into one
`ModifyPowerToughness`, and its order relative to another 7c effect (Giant Growth)
within the layer cannot change the result. The object's own timestamp is supplied
to satisfy `Gathered`'s shape; it is never load-bearing here. This is a **theorem
about layer 7c, not an elision** — no expiry, because 7c can never contain a
non-additive effect (setting is 7b, switching is 7d). `addPT`'s existing
`(Nothing, _) → Nothing` means counters on a non-creature (no base P/T) produce no
P/T, matching CR 122.1a (which speaks only of creatures).

`projectFrom` is unchanged: the injected `Gathered` flows through the existing
`applyLayer`/`affects`/timestamp-sort machinery for `Layer.ModifyPT`.

## 5. The state-based action — CR 704.5q / 122.3 annihilation

`Pawl.Sba.performStateBasedActions` gains an annihilation arm. For each battlefield
permanent with both counter kinds present, remove N of each (N = the smaller
count), editing `Object.counters`, and report that a state-based action was
performed so the CR 704.4 loop repeats.

```
-- CR 704.5q / 122.3: a permanent with both a +1/+1 and a -1/-1 counter has N of
-- each removed (N = min). A counter-count edit, not a bury or a departure -- so it
-- feeds the "performed" flag (CR 704.4 repeats) but never re-fires once balanced.
-- Net P/T is preserved, so annihilation can neither cause nor prevent a death;
-- ordering vs the 704.5f/g bury/destroy step in the same pass is immaterial.
annihilateCounters :: GameState -> (Bool, GameState)
```

integrated into the single pass beside the existing `zeroToughness` (704.5f) and
`destroyedBySba` (704.5g/h) classification, so all state-based actions are checked
against one projection of the board (CR 704.4 simultaneity, the established
posture). The performed-flag disjunction gains this term; a permanent that never
had both kinds is untouched, and one that did is balanced after one application and
will not re-fire.

**Observability.** Because annihilation preserves net P/T, its only observable is
the counter counts themselves — which is precisely the point: a net-`Integer`
model has no counts to change, so 704.5q is the falsifier that forces §2's typed
`Map CounterKind Natural`. The §7 test asserts on `Object.counters`.

## 6. The cards and the fixture mana base

Both render to `data/cards/<slug>.json` (the M3.5 files-are-source-of-truth
pipeline; the round-trip regenerates and re-parses them):

- **Battlegrowth** (`battlegrowth.json`): type line `Instant`; mana cost `{G}`
  (single green — a `Forest` base, which exists); one effect
  `PutCounters PlusOnePlusOne (Literal 1) "creature"`; `targetSpecs`
  `{"creature" ↦ CreatureTarget}`.
- **Instill Infection** (`instill-infection.json`): type line `Instant`; mana cost
  `{3}{B}` (generic + black — a `Swamp` base, which exists); effects
  `[PutCounters MinusOneMinusOne (Literal 1) "creature", Draw (Literal 1)]`;
  `targetSpecs` `{"creature" ↦ CreatureTarget}`.

No new `CardType`, `Subtype`, `Supertype`, `Prompt`, or `Response`. Both are green
and black respectively, matching the existing **green-black matchup** (M2d).

**Test/deck posture.** Deterministic fixtures carry the gate scenarios — the
704.5q annihilation and the both-counters case in particular need both counters on
one creature, which random priority-passing will not reliably produce. In addition,
Battlegrowth swaps 4-for-4 into the green deck and Instill Infection 4-for-4 into
the black deck (each deck stays 60; the card-backed conservation property stays
120 — a counter mints no object), giving random counter-churn coverage of the
projection injection and the persistence path, the M4b/M4d pattern.

## 7. Tests

All gameplay-level (cast/resolve through the stack, assert on game state); test
names carry their CR numbers.

- **`CounterKind`/`Object`:** an object's `counters` starts empty; `changeZone`
  yields a new incarnation with empty `counters` (CR 122.2 unit-level).
- **Gate (CR 122.6 / 122.1a):** cast Battlegrowth on a creature; assert one
  `PlusOnePlusOne` counter and projected P/T +1/+1 over base.
- **Persistence falsifier (CR 514.2 vs 122):** Battlegrowth's +1/+1 remains after
  cleanup and into the next turn, while a same-magnitude Giant Growth on a twin
  creature wears off at cleanup. The counter/continuous-effect line.
- **Lethal -1/-1 (CR 704.5f):** put enough `MinusOneMinusOne` counters (via Instill
  Infection, and/or a scripted second) to bring a small creature's toughness ≤ 0;
  it is put into its owner's graveyard by the state-based action. **Retires the
  synthetic `−0/−1` fixture** (M4b/M4d expiry).
- **Annihilation falsifier (CR 704.5q / 122.3):** a creature with both a +1/+1 and
  a -1/-1 counter has both removed by the SBA (assert `Object.counters`); the
  asymmetric two-plus/one-minus case leaves one +1/+1 (net P/T unchanged
  throughout).
- **Zone-change reset (CR 122.2):** a counter-bearing creature bounced by Unsummon
  and recast enters with no counters.
- **7c stacking:** Battlegrowth's +1/+1 plus a Giant Growth on the same creature →
  +4/+4 (both 7c, additive), confirming the gather injection folds through the
  existing path.
- **Round-trip:** `battlegrowth.json` and `instill-infection.json` are covered by
  the `allPrintings` honesty property via the new `Codec` arms.

## 8. Module and dependency notes

- `Pawl.Type.CounterKind` is a leaf type module (imports nothing project-local);
  `Object`, `Effect`, `Projection`, `Sba`, and `Codec` import it.
- `Object` gains a `Map CounterKind Natural` field — `Object` already imports
  `Data.Map.Strict` and `Numeric.Natural`; add the `CounterKind` import.
- `Projection` is the sole home that names a `Modification` constructor; the
  synthetic `ModifyPowerToughness` in `counterGathered` is constructed there, no
  invariant issue. `Projection` already imports `Game` (`lookupObject`),
  `Object`, `Affected`, `Layer`, `Quantity`, and `Modification`.
- `Sba` edits `Object.counters` directly on `GameState.objects`; it already
  imports `Object`, `Game`, and `Projection`. No new import edge, no cycle.
- `Resolve` stays the sole `case effect of` home (the new `PutCounters` arms);
  `Projection` the sole `case … Modification` home; `Target` unchanged (reuses
  `CreatureTarget`). No new casing homes.
- `Codec` gains arms for `PutCounters` (a tagged object carrying `CounterKind`,
  `Quantity`, `SlotName`) and `CounterKind` (a tagged sum) — the §2.12
  tagged-sum discipline; `Object.counters` is not serialized.

## 9. Named deferred expiries

Each is due with the first real card that needs it:

- **Non-P/T counter kinds (CR 122.1b–i).** Keyword counters (grant a keyword,
  613.1f), charge/fuse counters, loyalty (planeswalkers), poison (player counters —
  not on an object), shield/stun/finality (each a replacement or prevention effect).
  `CounterKind` grows a constructor per kind; only the two P/T kinds affect the
  projection today.
- **A "counter placed" event and its triggers (CR 122.7).** `PutCounters` edits
  `Object.counters` directly, emitting nothing. "When/whenever the Nth [kind]
  counter is put on…" and proliferate need a counter-placement event through an
  emit funnel (the `Event.changeZone` analog). Deferred.
- **Replacements that alter counter placement.** Doubling Season / Hardened Scales
  ("if one or more counters would be put on…") need `PutCounters` to consult
  replacement effects before applying. Deferred; the put is unmediated today.
- **Counters as a permanent enters (CR 122.6a).** Distinct from "put on" during a
  resolution — a replacement on the ETB, with a possible controller choice. A
  token or permanent entering *with* counters is future.
- **Move a counter (CR 122.5).** Remove-from-one, put-on-another, with its "not
  possible" guards. Deferred; no card moves counters yet.
- **"Can't have more than N counters" (CR 122.4 / 704.5r).** A second counter SBA,
  keyed to an ability on the permanent. Deferred; no such card yet.
- **`Quantity.X`-many counters.** `PutCounters` already carries a `Quantity` and
  `readsX` inspects it; the first `X`-counter card rides M4a's `ChooseX` with no
  opcode change.

## 10. Invariant check

- **Closed/open separation.** The rules core reads counts *by kind* — a
  classification (`CounterKind`), the same posture as `Keyword`. `Pawl.Resolve` is
  the only module that cases on `PutCounters`; `Pawl.Projection` the only one that
  cases on the `Modification` it synthesizes; `Pawl.Sba`'s annihilation reads
  counts, never a card. There is no `case card of Battlegrowth → …` anywhere.
- **First-order, non-recursive DSL.** `PutCounters CounterKind Quantity SlotName`
  carries a tag, a number, and a name — no function, no nested effect, no control
  flow.
- **The engine makes no choice it shouldn't.** Putting counters is forced
  (unprompted, CR 122.6); the only choice is the target, made at cast through the
  existing `ChooseTargets` prompt. Annihilation (CR 704.5q) is a forced SBA. No
  elision is introduced.
- **Numeric tower.** Counts are `Natural`; the P/T delta is an `Integer` `Quantity`
  (may go negative from −1/−1 counters), consistent with M1a's `Quantity` posture.
