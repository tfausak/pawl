# M4.5 P2 — Copy (layer 1)

*Design pass 2026-07-20. The second phase of the M4.5 umbrella
(`docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`), closing
**GAP-L1** — copy — and it is the **M4.5 go/no-go** (design.md §5 names the layer
system as the architecture's canary, and layer 1 is the one blank layer left after
P1 landed control). Gate: **Clone**. This spec is implementable; a `writing-plans`
plan follows it. All CR numbers are marked **(verify)** and must be checked against
`docs/rules.txt` before they drive code (CLAUDE.md: never trust recalled Magic
rules).*

## 0. Why this phase, and what it proves

`Layer.Copy` (CR 613.1a, layer 1, **verify**) already sits at the top of the layer
enum with **no producer** — the projection folds layers 2–7 (control, text, type,
ability, P/T) but nothing produces a layer-1 copy effect. Copy is the largest
single blank in the layer system (census §3.3): a whole rules section (CR 707,
**verify**), and design.md §5 flags text-changing / the layer system as the canary
that beat XMage.

**The decision it proves:** the immutable-base / projected-fold architecture
(design.md §2.5) can express two things nothing before it has needed —

1. **Copiable values computed *before* every other layer.** A copy does not
   acquire the source's *current* characteristics; it acquires the source's
   **copiable** values (CR 707.2, **verify**), a strict subset that excludes
   counters, P/T pumps, control, and most continuous effects. The naive
   implementation — "copy the source's projected P/T" — is *wrong*, and the gate
   falsifies it directly.
2. **Self-reference.** A copy of a copy (Clone copying Clone) must resolve to the
   underlying creature. In this design self-reference is impossible to loop on,
   because copy is **locked at entry** (§2) and you can only copy something already
   on the battlefield — whose copy is therefore already baked.

If the projected-fold model is going to break, it breaks here. It does not: copy is
one synthesized layer-1 `Modification` folded by the existing path, exactly as
counters are (`counterGathered`). The verdict is **GO**.

**Gate card — Clone** (`{3}{U}` Creature — Shapeshifter, 0/0; verify Scryfall for
exact text/cost/type line): *"You may have Clone enter the battlefield as a copy of
any creature on the battlefield."* Chosen as the canonical layer-1-copy card: it is
a permanent copy (not a copy-*spell*, CR 707.10, which is a different mechanism —
umbrella §1), its copy is **locked as it enters** (CR 707.9a, **verify**), and its
"may" gives the decline path (enter as a 0/0 that dies to a state-based action) for
free.

## 1. Scope

**In scope:** copy as a projected **layer-1** characteristic; the copiable-value
computation (base + layer-1 only); the **snapshot-at-entry** storage that locks the
copy; the as-enters choice, hooked into the **battlefield-entry funnel** and drained
at the settle boundary (§2.4); and the gate + falsifier tests. **Zero new opcodes** —
copy is an intrinsic card property, not an `Effect` (the M4g/M4h "compose under a
wrapper, add no opcode" posture).

**Out of scope (deferred, each with a named expiry — §7):** copy-spell (CR 707.10);
copy-token effects (CR 707.2 token copies); ongoing "becomes a copy" that re-reads
the source (Vesuvan Doppelganger); the general as-enters *replacement* shape (CR
614.12); face-down / morph / manifest (backlog, umbrella §4); copiable values pawl
does not yet project (name, mana cost, color, supertypes) and the 7b/CDA P/T-
setting inclusion of CR 707.2 (rides P3); copy riders ("...with a +1/+1 counter",
"...and gains flying").

## 2. Architecture

### 2.1 Copy is projected in layer 1, snapshotted at entry (design.md §2.5)

A permanent's copy is **base characteristics replaced, in layer 1, by the copiable
values of the copied object** (CR 613.1a). The copy is **locked as the object
enters** (CR 707.9a, **verify**): once Clone enters as a copy of a 2/2 Grizzly
Bears, later pumping, countering, *or destroying* the Bears leaves Clone untouched.

We honor that by **snapshotting the copiable values at entry**, not by storing a
live pointer to the source. A live `ObjectId` re-read would revert Clone to a 0/0
the moment the source left the battlefield (a real, easily-constructed wrong
outcome — Murder the copied creature); the snapshot is correct by construction and
makes copy-of-copy cycles *impossible* (the copied object entered earlier, so its
own snapshot is already baked — nothing recurses at fold time).

The snapshot rides the **`Object.bindings`** map (the D4 named-slot environment),
exactly like an X value or chosen modes — per-incarnation state, **forgotten on a
zone change for free** (CR 122.2/400.7's `changeZone` reset, the same mechanism
that clears counters and targets). No new base `Object` field; nothing to clean up
at cleanup or when the object leaves.

### 2.2 Copy is the fold seed (layer 1), in `Pawl.Projection`

Layer 1 is the *first* layer, and pawl's projection already begins its fold from a
starting value — `baseCharacteristics oid gs` (the printed characteristics). A copy
effect replaces exactly that starting value with the copied object's **copiable
values** (CR 707.2 / 613.1a). So copy is the **fold seed**, not a synthesized
`Modification`:

- **`Projection.copiableCharacteristics :: ObjectId -> GameState ->
  ProjectedCharacteristics`** — the object's layer-1 result: if the object carries a
  copy snapshot in its `bindings` (stamped at entry, §2.4), that snapshot **is** its
  copiable value; otherwise it is `baseCharacteristics oid gs`. Because the snapshot
  was itself computed as a copiable value at entry, a copy of a copy already carries
  the underlying creature — no recursion at fold time, no cycle possible.
- **`Projection.projectFrom` seeds the layer fold with `copiableCharacteristics oid
  gs`** in place of `baseCharacteristics oid gs` (a one-line change). Layers 2–7
  (control, counters, pumps, ability grants) then fold on top exactly as before.

Seeding at layer 1 with *only* base-or-snapshot is what enforces CR 707.2's subset:
counters and pumps are layer 7c, control is layer 2, ability grants are layer 6 —
they fold *after* the seed and so are never part of a copied object's own copiable
value. **This is the falsifier made structural.** `affectsBase` (source-liveness)
keeps reading `baseCharacteristics`, never the copiable seed, so nothing recurses.

No new `Modification` constructor is introduced — copy is not a `case`-on-
`Modification` operation but a replacement of the fold's starting value, so
`Pawl.Projection` stays the sole home of the layer machinery with **no `Codec`
surface** (a `Modification.BecomeCopy` would have forced a dead `ProjectedCharacteristics`
JSON encoding, since it never appears in a card; the seed avoids it entirely).

### 2.3 `ProjectedCharacteristics` gains `Ord`

The snapshot rides a `Binding` field (§2.4), and `Binding` (via `Object`) derives
`Ord`, so **`ProjectedCharacteristics` gains `deriving Ord`**. This follows the
project's "derive `Ord` broadly — it is harmless and occasionally useful" posture;
the old "No Ord: never sorted, never a key" comment is retired. The characteristics
carried are exactly CR 707.2's copiable subset pawl currently projects (keywords,
P/T, card types, subtypes, the three ability lists, `rulesTextActive`); name/mana
cost/color/supertypes are not projected and so not copied (§7).

### 2.4 The as-enters choice: entry funnel + settle-boundary drain (no opcode)

"Enters as a copy" is, in the rules, **a replacement effect from the object's own
static ability** (CR 614.1c; CR 603.6d: "*a static ability—not a triggered
ability—whose effect occurs as part of the event that puts the permanent onto the
battlefield*"), it fires **as the object enters on any path** (CR 113.6h — not only
on spell resolution), and its choice is made **before the object enters** (CR
614.12a). It generates a **copiable** (layer 1a) effect (CR 613.2a). All **verify**.

So the choice does **not** hang off spell resolution — it hangs off the
**battlefield-entry event**, the funnel every entry already flows through
(`Event.changeZone` / `Event.placeObject`, M3f/M4c). But that funnel is *pure* and
the choice needs a **prompt**. Rather than make the whole funnel monadic (that is
P5's general monadic-replacement job — §7), P2 splits it into a pure **mark** and a
monadic **drain**:

- **`Card.copyOnEnter :: Bool`** (new classification). True for Clone. The rules core
  reads the Bool, never Clone's identity.
- **Mark (pure).** `Event.placeObject`, when the entering card is `copyOnEnter`,
  stamps an **"as-enters choice pending"** marker on the new incarnation (a reserved
  `Object.bindings` slot — auto-cleared on a later zone change, like every binding).
  No prompt; `changeZone` stays pure (M3f's SBA-death path depends on that purity).
- **Drain (monadic).** A new pass in the settle loop — running at the CR 117.5
  boundary **before** the M3f trigger scan and the state-based-action check — finds
  every battlefield object still carrying the pending marker and resolves its choice:
  prompt `ChooseCopyTarget`; on `Just chosen`, compute
  `Projection.copiableCharacteristics chosen gs`, write the snapshot into the copy
  binding (`Pawl.Binding.setCopy`), and clear the marker; on `Nothing` (decline),
  just clear the marker.
- **`Prompt.ChooseCopyTarget` / `Response.ChoseCopyTarget (Maybe ObjectId)`** (new
  pair). Legal set = **battlefield creatures** (projected creature-ness,
  `Projection.isCreatureOf`) **excluding the entering object itself** (a 0/0 with
  nothing useful to copy; excluding it removes any self-cycle question — CR wording
  **verify**). `Nothing` is the decline. `Pawl.Target` gains the legal-set helper
  (`legalCopyTargets`).
- **Why this is CR-faithful, not a stopgap.** The choice is attached to the correct
  event (every battlefield entry — a reanimated, blinked, or token Clone all get it,
  which the resolution-tail alternative silently drops), and it is
  **observationally equivalent to CR 614.12a's "before it enters"**
  (`observable-equivalence-is-the-bar`): the drain runs before any player gets
  priority, before triggers, before SBAs, so (a) no one sees the interim 0/0; (b) the
  Clone's own enter triggers (copied from the source) are scanned against the
  already-copied projection; (c) a declined 0/0 dies to the same SBA sweep via
  `Sba.zeroToughness` (CR 704.5f). Cite the rule/timing in the code comment.
- **Replay.** `Response.ChoseCopyTarget` is serialized; the deterministic replay
  answerer picks a fixed legal choice (lowest-id legal creature, so replay exercises
  the copy rather than always declining), the `ChooseX` / `ChooseModes` posture.

This mark-then-drain seam is the **narrow first version of P5's monadic replacement
engine** (§7), exactly as M3f's single pure redirect was the narrow first version
that P5 widens — nothing here is thrown away; the drain folds into the general CR
614.12 / 616 machinery when P5 lands.

### 2.5 Why no `continuousEffects` entry and no new base field

Copy could have been a stored `ContinuousEffect { modification = IsACopyOf src,
duration = Indefinite }`, but a self-sourced indefinite effect must be dropped when
the object leaves — cleanup the store does not do today. The binding snapshot needs
none of that: it is per-incarnation and dies with the incarnation. This is the
`counterGathered` lesson (per-object permanent state → synthesized `Gathered`)
applied a second time; copy is its second customer, not a new problem.

## 3. The two invariants

1. **Classification, not identity.** `Pawl.Projection` stays the sole
   `case`-on-`Modification` home (`BecomeCopy` applied only there). **No new
   `Effect`**, so `Pawl.Resolve`'s `case effect of` is untouched and no new opcode
   classifications (`slotsOf`/`readsX`/`manaProduced`/`searchesLibrary`/
   `rewriteEffect`) are needed. `copyOnEnter` is a Bool the entry funnel and drain
   consult, never a card identity.
2. **The engine makes no choices.** The copy target is a genuine player choice and
   is **prompted** (`ChooseCopyTarget`), including the "may" decline — nothing
   elided.

## 4. Cards and tests

All gameplay-level (cast/resolve through the stack, assert on projected game
state). The gate is a **real** card and — because every supporting card already
exists in the engine — **no synthetic crutch is needed** (the
`tests-prefer-real-cards` ideal).

- **Clone** (real, Scryfall-verified; `data/cards/clone.json`, joins `allPrintings`
  for the honesty round-trip). A **blue deterministic fixture** (the M3d posture —
  no random-game deck entry, keeping CR 400.7 conservation counts undisturbed).
  Gate scenarios:
  1. **Copy a vanilla creature.** Clone copies Goblin Piker; assert it projects
     2/1, is a Creature with the Piker's subtypes, and can be declared as an
     attacker. Passes only because layer 1 replaced Clone's base 0/0.
  2. **Falsifier — copiable, not current.** Put a +1/+1 counter (Battlegrowth, M4f)
     **and** a Giant Growth (M3b) on the source, *then* Clone it; assert the Clone
     projects the source's **base** P/T (2/1), not 3/2 or 5/4. The counter (7c),
     the pump (7c) are excluded from copiable values — the "read current P/T copies
     the wrong thing" falsifier.
  3. **Copy the rules text.** Clone copies Prodigal Sorcerer (M3e); assert the Clone
     has the `{T}: deal 1` activated ability (via the projection) and can activate
     it. Proves copiable values carry abilities, not just P/T.
  4. **Copy of a copy (self-reference).** Clone A copies a Piker; Clone B copies
     Clone A; assert B projects the Piker. Proves the snapshot chains and cannot
     loop (B copies A's already-baked snapshot).
  5. **Source leaves — the snapshot payoff.** Clone copies a Piker; Murder (M4b) the
     Piker; assert the Clone is **still** a 2/1 Piker. This is the outcome a live-
     ObjectId model gets wrong and the snapshot gets right (CR 707.9a lock).
  6. **Decline.** Clone enters choosing to copy nothing; assert it is a 0/0 and dies
     to the state-based-action check (CR 704.5f, `Sba.zeroToughness`).

## 5. Module & type changes (summary)

- `Pawl.Type.ProjectedCharacteristics` — add `Ord` to the deriving; retire the "No
  Ord" comment.
- `Pawl.Type.Binding` — add `copy :: Maybe ProjectedCharacteristics`.
- `Pawl.Binding` — add the `copyOf` projection and `setCopy` write; the reserved
  copy-snapshot slot; and the reserved **as-enters-pending** marker slot with
  `pendingCopy` / `markPending` / `clearPending`.
- `Pawl.Type.Card` — add `copyOnEnter :: Bool`.
- `Pawl.Projection` — add `copiableCharacteristics`; seed `projectFrom` with it.
  No `Modification`, `layer`, `applyModification`, or `Codec` change.
- `Pawl.Event` (`placeObject`) — pure **mark**: stamp the pending marker on a
  `copyOnEnter` object as it enters. No `case effect of` / no prompt.
- The settle loop (`Pawl.Engine` / `Pawl.Stack` — wherever the CR 117.5 boundary
  scans triggers and SBAs) — monadic **drain**: before the trigger scan and SBA
  check, resolve each pending object's `ChooseCopyTarget`, compute the snapshot,
  stamp the copy binding (or clear on decline).
- `Pawl.Type.Prompt` / `Pawl.Type.Response` — add `ChooseCopyTarget` /
  `ChoseCopyTarget`.
- `Pawl.Target` — add `legalCopyTargets` (battlefield creatures excluding self).
- `Pawl.Replay` — deterministic `ChoseCopyTarget` answer.
- `Pawl.Codec` — `Card.copyOnEnter` field only (emitted **when True**, so the 44
  existing card files stay byte-stable; decoded optionally, default `False`). No
  `Modification` arm (§2.2), and bindings are runtime state, never serialized.
  `clone.json` round-trips.
- `data/cards/clone.json` added; `allPrintings` updated.

## 6. Ordering within the phase (for the plan)

Substrate before consumer, falsifier-first: (1) `ProjectedCharacteristics` Ord +
the `Binding.copy` + pending-marker slots + `Pawl.Binding` accessors; (2)
`copiableCharacteristics` + the `projectFrom` seed, tested by projecting a
hand-placed copy binding directly (unit-level, before the card exists); (3)
`Card.copyOnEnter` + `Codec` + `Subtype.Shapeshifter`; (4) `clone.json` +
`Cards.clonePrinting` + round-trip; (5) `ChooseCopyTarget`/`ChoseCopyTarget` +
`Replay` + answerers + `Target.legalCopyTargets`; (6) the pure `placeObject` mark;
(7) the monadic settle-boundary drain + gate scenarios (ordered before
triggers/SBAs). Each is one small complete commit; TDD per CLAUDE.md (write the
failing test, watch it fail, implement).

## 7. Deferred, with named expiries

- **Copiable values pawl does not project** — name, mana cost, color, supertypes.
  Copy sets only the `ProjectedCharacteristics` subset. → the first card that
  observes a copied name/color/cost (color rides **P3**; a name projection is its
  own first-customer card).
- **7b P/T-setting / CDA in copiable values** (CR 707.2's "as modified by effects
  that set P/T"). `copiableCharacteristics` folds layer 1 only. → **P3** (Tarmogoyf
  `*/1+*` / characteristic-defined P/T) and face-down.
- **Ongoing "becomes a copy"** (Vesuvan Doppelganger) — re-reads the source
  continuously rather than locking at entry; the snapshot model handles only the
  locked case. → first such card.
- **Copy-spell** (CR 707.10, Twincast) — copies a stack object, a different
  mechanism (umbrella §1). → its own phase/card.
- **Copy-token effects** (CR 707.2 token copies of a permanent) — mints a token from
  copiable values; rides M4c token minting + this snapshot. → first such card.
- **Copying a permanent's static abilities** (a Clone of Humility / Opalescence).
  `gather` collects static abilities from an object's *printed* card
  (`Game.cardOf`), not its copiable value, so a copied static ability does not yet
  fold. `ProjectedCharacteristics` carries no `staticAbilities` field (statics are
  gathered separately). *Activated / triggered / replacement* abilities **are**
  copied (they live in `ProjectedCharacteristics`, seeded from the source's base).
  No gate scenario copies a static-ability permanent. → first card that copies a
  static-ability permanent.
- **Simultaneous entry of multiple copy-choosers** (CR 614.12b / 614.13, two Clones
  entering at once). The drain resolves pending markers sequentially, each seeing
  the current board; the "can't choose a co-entering object" rule and CR 616
  ordering are not modelled. The gate enters Clones one at a time. → rides the P5
  monadic replacement engine.
- **The general monadic as-enters replacement engine** (CR 614.12 / 616) — multiple
  racing "enters with N counters" / "enters tapped unless..." / "enters as a copy
  **with** riders" replacements, and the CR 616 ordering prompt when several apply to
  one entry. P2's mark-then-drain seam (§2.4) is the **narrow single-choice first
  version**: it attaches to the correct entry event and prompts once per object, but
  does no multi-replacement ordering. The drain **folds into** this engine — the
  `copyOnEnter` classification and the entry-funnel hook are reused, the bespoke
  settle pass is replaced by CR 616 application. → **M4.5 P5** (the monadic
  replacement path; umbrella §3, git-bug `6afb561`).
- **Face-down** (morph / disguise / manifest / cloak) — a face-down *status* plus
  this copy/characteristics machinery. → backlog (umbrella §4).
- **Legend rule for two copies** (CR 704.5j) — stays **elided** as it has since M3g
  (Mindslaver); a Clone of a legendary is a legal fixture but the 704.5j SBA is not
  yet enforced. → the Mirror Gallery / legend-rule card.

## 8. Tracking

On completion, re-point or close git-bug `83f1a55`'s **GAP-L1** facet (this phase;
its GAP-L2 sibling was addressed by P1). Update the M4.5 umbrella (`umbrella §7`) if
the gate or axis shifted. `f90e0c4` (topological CR 613.8b applies-to) may be
revisited as the layer fold grows, but P2 adds no within-layer dependency (a copy's
affected set is always `TheseObjects {self}`), so it stays open and untouched.

## 9. Exit criterion

A game in which Clone is cast, enters as a copy of a creature, and behaves as that
creature — including retaining the copy after the original leaves — completes and
replays deterministically; the six gate scenarios (§4) pass; the build is
warning-clean and `hooky run` is green. With layer 1 filled, the closed-half layer
system is complete for every layer that has a card (design.md §1's "finish the
finite thing"), and the M4.5 go/no-go is **GO**.
