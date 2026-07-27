# CR 613.8: dependency ordering within a layer

*Design pass 2026-07-26. Closes GitHub issue #11 ("CR 613.8b: topological
applies-to dependency ordering is not implemented"). **No new type, no new
opcode, no new field, and no new card**: one rewrite of
`Projection.projectWith`'s within-layer step. Every rules claim is checked
against `docs/rules.txt` and cited by number.*

## 0. Why this exists

`projectWith` orders same-layer effects by timestamp (CR 613.7) and stops there.
The CR has a second, overriding rule — an effect that changes what another
applies to goes first, regardless of timestamps — and pawl has never had it. The
elision was safe because no card pair in the pool was same-layer *and*
applies-to-coupled. One arrived in #235, and nobody noticed (§5).

## 1. The rules (verbatim ground truth)

> **613.8.** Within a layer or sublayer, determining which order effects are
> applied in is sometimes done using a dependency system. If a dependency exists,
> it will override the timestamp system.

> **613.8a** An effect is said to "depend on" another if (a) it's applied in the
> same layer (and, if applicable, sublayer) as the other effect; (b) applying the
> other would change the text or the existence of the first effect, what it
> applies to, or what it does to any of the things it applies to; and (c) neither
> effect is from a characteristic-defining ability or both effects are from
> characteristic-defining abilities. Otherwise, the effect is considered to be
> independent of the other effect.

> **613.8b** An effect dependent on one or more other effects waits to apply
> until just after all of those effects have been applied. If multiple dependent
> effects would apply simultaneously in this way, they're applied in timestamp
> order relative to each other. If several dependent effects form a dependency
> loop, then this rule is ignored and the effects in the dependency loop are
> applied in timestamp order.

> **613.8c** After each effect is applied, the order of remaining effects is
> reevaluated and may change if an effect that has not yet been applied becomes
> dependent on or independent of one or more other effects that have not yet been
> applied.

Two neighbours this rests on, already implemented:

> **613.6** … If an effect starts to apply in one layer and/or sublayer, it will
> continue to be applied to the same set of objects in each other applicable
> layer and/or sublayer …

> **613.7** Within a layer or sublayer, determining which order effects are
> applied in is usually done using a timestamp system.

## 2. Two gaps, not one

The issue names the reorder. The reorder alone produces the wrong answer, because
of a second divergence nobody had written down:

1. **No reorder.** `ordered = sortOn gTimestamp …` is CR 613.7 and nothing else.
2. **Applicability is judged against the pre-layer snapshot.** Every same-layer
   candidate is tested with `affects … seeded …`, where `seeded` is the
   projection through the layers *below* this one. So an effect can never see
   what a same-layer effect just did — which is precisely the state CR 613.8's
   dependency exists to describe.

Fixing (1) without (2) reorders Liquimetal Coating's effect ahead of March of the
Machines and then asks March's filter about a land that is not an artifact in the
snapshot, so March still does not apply. Both have to move.

## 3. The design

Replace the sort-then-fold with a selection loop over the layer's candidates.
Per object, per layer:

```
pending := the candidates in this layer
while pending is non-empty:
    ready := { e in pending | e depends on no other member of pending }
    batch := if ready is non-empty then ready
             else  { e in pending | e is on a dependency cycle }   -- CR 613.8b
    next  := the earliest-timestamp member of batch                -- CR 613.7
    apply next (if it applies), then remove it from pending
```

The loop clause is the fiddly one. CR 613.8b's last sentence — "If several
dependent effects form a dependency loop, then this rule is ignored and the
effects **in the dependency loop** are applied in timestamp order" — excuses the
loop's own members and nobody else. An effect that merely waits on the loop keeps
waiting, and takes its turn once the loop has unwound. So when `ready` empties,
the fallback is the candidates that sit on a cycle, not everything left: with A
and B in a two-effect loop and C depending on A from outside it, a fallback over
all of `pending` would spend C first if C had the earliest timestamp, at a moment
when A has not applied and C may not even be applicable yet.

`ready` being empty means every remaining candidate has an outgoing edge, and a
finite graph in which every node has one contains a cycle — so the cycle set is
never empty, and the loop always makes progress.

Three properties fall out of the shape:

- **CR 613.8c is free.** Dependencies are recomputed each pass over the loop, so
  "after each effect is applied, the order of remaining effects is reevaluated"
  is not a separate mechanism.
- **It terminates.** Each iteration removes exactly one candidate.
- **CR 613.7 survives underneath.** With no dependencies, `ready` is everything
  and the loop degenerates to timestamp order — the behaviour every existing test
  pins.

**Applicability moves to application time.** `next` is tested against the running
partial rather than `seeded`, which is what lets March see the artifact Liquimetal
Coating just made. CR 613.6's memo is unaffected and in fact becomes
load-bearing here: an effect whose set was fixed in a lower layer is *not*
re-asked, so it can neither depend on anything in this layer nor be changed by
it.

**Depends-on is one comparison.** `a` depends on `b` when applying `b` changes
whether `a` applies:

```
dependsOn a b pc = appliesTo a pc /= appliesTo a (b applied tentatively to pc)
```

The tentative application is discarded; only the answer is kept.

**Most layers keep the old fold.** A layer where nothing is *movable* — where no
candidate has a `Matching` set that reads a projected characteristic — has no
dependency to find, by CR 613.8a's own definition: nothing can change what
anything else applies to. On such a layer CR 613.8 is silent, CR 613.7 timestamp
order stands, and every candidate's answer is invariant as the layer is applied,
so judging applicability against `seeded` gives the same answers as judging it one
at a time. That branch therefore keeps the existing filter-sort-fold verbatim. It
is not a shortcut past the rule; it is the rule where the rule says nothing. It is
also almost every layer of almost every projection, which matters (§7).

Immovability is three cheap facts, not an analysis: a `TheseObjects` set names ids
(CR 611.2c) and an `Attached` one reads the source's own attachment off the game
state (CR 303.4m), neither of which any modification writes; a set CR 613.6
already fixed answers from the memo; and a filter reading no projected aspect at
all (`And []`, `IsSource`, a supertype — CR 205.4a supertypes are never projected)
has nothing in it to change.

## 4. Scope: which of CR 613.8a is implemented

Clause (b) is a disjunction of four things. This implements one of them and
inherits a second.

| 613.8a(b) clause | status |
|---|---|
| "what it applies to" | **implemented** — the comparison above |
| "the existence of the first effect" | already handled, by source-liveness (`staticAbilitiesLive`, CR 305.7), which is the CR 613.8b loop-escape analog and predates this |
| "the text of the first effect" | unreachable: CR 612 text-changing is layer 3, and no layer-3 effect in the pool rewrites another effect's text |
| "what it does to any of the things it applies to" | **not implemented** — no producer. It needs comparing the *result* of `a` before and after `b`, not just its set |

Clause (c), the characteristic-defining-ability exclusion, is **vacuous**: a CDA
is never a gathered candidate. `applyCharacteristicPT` folds an object's own CDA
at layer 7a outside the candidate list, and devoid is seeded before layer 1. So no
pair this loop can see is ever CDA-vs-non-CDA.

**The analysis is per object.** The CR defines the dependency globally — "what it
applies to" is a set over the whole board — while `projectWith` projects one
object and asks "does `a` apply to *this* one". The two agree whenever a
dependency that matters for object O is visible at O, which is every pair the
pool can build: if `b` does not apply to O it cannot change O's characteristics,
so it cannot change whether `a` applies to O. They diverge only when `b` changes
`a`'s set at some *other* object P while both still apply to O and their order
matters — which needs an effect whose filter reads something off P, and nothing
in the vocabulary does. Filed as #236 rather than pretended away.

## 5. The gate cards

**No new card.** The issue's expiry trigger is "the first real same-layer
applies-to card pair", and it has already fired without anyone noticing: the pool
has held one since #235, two PRs ago.

**Liquimetal Coating** — "{T}: Target permanent becomes an artifact in addition to
its other types until end of turn." A stored effect, layer 4 (CR 613.1d),
`Affected.TheseObjects` locked at resolution (CR 611.2c).

**March of the Machines** — "Each noncreature artifact is an artifact creature
with power and toughness each equal to its mana value." A static ability, layer 4
and layer 7b, over `Matching (And [HasCardType Artifact, Not (HasCardType
Creature)])`.

March depends on Liquimetal Coating: applying the Coating's effect makes its
target an artifact, which changes whether March applies to it. The Coating does
not depend on March: a `TheseObjects` set names an object id, and no type change
can move an id in or out of it. One-way, exactly the shape §3 resolves.

The interaction is real play, not a fixture: point a Liquimetal Coating at your
own Forest with a March of the Machines already out.

- **Dependency order** (correct): the Coating applies, the Forest is an artifact,
  March is then asked and says yes — so the Forest is an artifact creature with
  base P/T equal to its mana value. A land has no mana cost, so that is 0/0, and
  CR 704.5f puts it into the graveyard.
- **Timestamp order** (what pawl does today): March is older, so it is asked
  first, against a Forest that is not an artifact yet. It does not apply. The
  Forest ends the turn as a perfectly healthy artifact land.

That difference is the test. The issue's own suggestion, Blood Moon + Conversion,
is the same shape and would work, but Conversion also prints "At the beginning of
your upkeep, sacrifice this enchantment unless you pay {W}{W}" — an optional-cost
upkeep trigger that does not exist yet, and that the dependency system does not
need. Enchanted Evening + Opalescence is the third such pair and needs hybrid mana
(CR 107.4e), which also does not exist yet.

## 6. What this does not change

- **Cross-layer ordering** (CR 613.1) is untouched: layers still apply in order,
  and the loop runs inside one layer.
- **CR 613.6's memo** is untouched; see §3.
- **Existence dependency** stays where it is, in `staticAbilitiesLive`.
- **`ProjectionSpec`'s documenting test** — "CR 613.7 within layer 4, timestamp
  order (EXPIRES at CR 613.8b, #11)" — flips from asserting the timestamp answer
  to asserting the dependency answer, which is what the issue's expiry trigger
  says to do.

## 7. Cost

Measured, `cabal bench`, median of three, before → after:

| benchmark | before | after |
|---|---|---|
| goldfish 2p | 13.2 ms | 13.3 ms |
| casting 2p | 129 ms | 130 ms |
| fighting 2p | 23.5 ms | 23.6 ms |
| fighting 2p aura | 597 ms | 563 ms |

The aura benchmark ends up **faster than before the feature**, which took three
passes to reach and is worth recording because two of them were wrong turns.

1. **No guard: 2.2 s, 3.7x.** The pairwise dependency scan ran on every layer of
   every projection. It is quadratic in a layer's candidates and each comparison
   costs a filter evaluation plus a tentative application, so this is an
   algorithmic regression rather than a constant one.
2. **Guard per layer per object: 676 ms.** §3's immovability test retires the
   scan — the aura deck (Islands, Darksteel Myr, Control Magic) has no `Matching`
   effect anywhere, so it never reaches it. What was left was asking the guard's
   question once per layer per object.
3. **Guard per board: 563 ms.** The question depends only on the candidate list,
   so `projectWith` now binds it — together with the layer list, which was always
   candidates-only and was always being recomputed per object — *before* taking
   the object, and `projectAll` shares that partial application across the board.
   The layer list moving with it is why the total comes out below the starting
   point.

The guard is deliberately coarser than the fold's own `movableReads`: it skips the
CR 613.6 memo test (per object, and it changes as the fold runs) and the filter's
aspects. Both only ever turn a True into a False, so the coarse version
over-admits — which costs the general path where the tight one would have done,
and never a different answer.
