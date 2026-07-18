# M3c dependency (CR 613.8) — design

Design for milestone **M3c**, the third letter of M3 (see the split table in
`docs/design.md`): **CR 613.8 dependency via trial application** — the
go/no-go verdict for the whole continuous-effects approach, and the single
hardest bet in the design. No studied prior-art engine actually built trial
application: Argentum *documented* it in detail and shipped a hardcoded
whitelist that could not even represent Blood Moon + Urborg (`docs/design.md`
risk register; `prior-art-lessons.md` §Decisions, D2). M3c is where pawl finds
out.

Two card pairs are the gate, each Scryfall-verified (AllPrintings via
`api.scryfall.com`, fetched 2026-07-18):

| Card | Cost | Type line | Oracle text |
|---|---|---|---|
| **Blood Moon** | `{2}{R}` | Enchantment | "Nonbasic lands are Mountains." |
| **Urborg, Tomb of Yawgmoth** | — | Legendary Land | "Each land is a Swamp in addition to its other land types." |
| **Opalescence** | `{2}{W}{W}` | Enchantment | "Each other non-Aura enchantment is a creature in addition to its other types and has base power and base toughness each equal to its mana value." |
| **Humility** | `{2}{W}{W}` | Enchantment | "All creatures lose all abilities and have base power and toughness 1/1." (from M3b) |

**Only Blood Moon + Urborg exercises the dependency resolver.** Their
interaction is an *intra-layer-4 existence dependency*: Blood Moon *sets*
Urborg's subtype to Mountain, which by CR 305.7 strips Urborg's rules-text
ability "each land is a Swamp," so whether Urborg's effect **exists** depends on
applying Blood Moon first. Humility + Opalescence, by contrast, is cross-layer
(4 → 6 → 7b) plus a within-7b timestamp race — no same-layer dependency (CR
613.8a requires the same layer). It resolves *correctly under timestamp-only
ordering* and is the broad correctness test of the generalized projection, not a
resolver test. That distinction is the spine of this milestone (§0).

This is a types-and-architecture spec, not an implementation plan.

## 0. The two axes and the phased spine

M3c carries two genuinely distinct axes wearing one letter, and the spec is
organized so the plan lands them as small commits along a seam that also
protects the go/no-go signal:

- **Axis A — layer 4 type-changing.** The M3b projection generalized from
  keywords / P·T to **types and subtypes**, wired through `Sba`, `Target`,
  `Combat`, and mana, ordered **by timestamp only**. Substantial but understood
  machinery — a direct sibling of what M3b did for layers 6/7. No unproven bet.
- **Axis B — CR 613.8 dependency / trial application.** The go/no-go. It needs
  Axis A to *have a witness* (Blood Moon + Urborg is intra-layer-4).

**The spine: land Axis A passing under timestamp-only ordering, then write the
dependency test set (failing), then land Axis B's resolver.** This:

1. satisfies the risk register literally — *the test set exists before the
   resolver*, and incompleteness is a failing test, never a doc footnote;
2. **isolates the go/no-go.** If trial application is a **no-go**, Axis A is
   still sound, committed, and useful; the failure is quarantined to the
   resolver, not tangled through layer 4;
3. is the natural TDD arc, so the plan decomposes into small ordered commits:
   type-projection additions → each consumer rewired → each card as a
   timestamp-only fixture → the failing dependency pairs → the resolver →
   Opalescence's mana-value P/T → the random-coverage tail.

M3c stays **one milestone, one spec**: layer 4 has no independent gate (it exists
to serve the dependency test), and the letters are cross-referenced throughout
`docs/design.md`. The decomposition into small tasks is the plan's job.

## Goal and scope

**Exit criterion.** Deterministic tests demonstrate all of:

- **Blood Moon + Urborg (the resolver), both timestamp orders, same correct
  outcome.** With a basic Forest and a nonbasic land on the battlefield under
  both enchantments: the Forest taps **{G} only** and the nonbasic land taps
  **{R} only** — *regardless of which enchantment entered first* — because
  Urborg depends on Blood Moon (CR 613.8a), so Blood Moon applies first (CR
  613.8b), stripping Urborg's rules-text ability (CR 305.7); Urborg then
  contributes nothing. A timestamp-only fold gets the Forest wrong (it taps
  {B} too) in both orders, for different mechanical reasons — that divergence is
  the falsifier.
- **Humility + Opalescence, both 7b timestamp orders** (CR 613.7, *not* 613.8):
  a real creature is 1/1 with no abilities; Humility itself (made a creature by
  Opalescence's layer 4) is **1/1 or 4/4** depending on the 7b timestamp order
  between Humility's and Opalescence's static effects; Opalescence itself stays
  a non-creature enchantment ("each *other*").
- **Layer-4 singles** end-to-end: Blood Moon alone turns a nonbasic land into a
  Mountain that taps {R}; Urborg alone makes every land a Swamp that taps {B}
  (Urborg itself included); Opalescence alone makes a non-Aura enchantment a
  creature with base P/T equal to its mana value — targetable by a
  `CreatureTarget` spell and destroyable by an SBA.

The `DecisionLog` replays deterministically with the new projected type line and
the resolver's ordering in the projection path.

**Non-goals.**

- **No new resolution opcode.** All three cards are static-ability permanents
  (like Humility): they resolve onto the battlefield by the ordinary M1a
  permanent branch and their `staticAbilities` are gathered live. `Resolve`
  stays untouched; the go/no-go lives entirely in `Projection`.
- **No layers 1, 2, 5, 7a, 7d producers** (copy, control, color, CDA, P/T
  switch). **No layer 3 text-changing** (Magical Hack) — that is M3d, its own
  go/no-go. The `Layer` type already enumerates all of them (M3b, for
  diffability); M3c adds a producer only for **Type** (layer 4).
- **No cross-query memoized projected state.** The design.md §2.5/§2.10 cached
  lazy projected-state field stays deferred with an expiry. M3c adds only an
  *intra-`project`-call* memo, required for termination and to avoid
  re-projecting static-ability sources (§2, §3).
- **No white or blue matchup.** Humility and Opalescence are white; there is no
  white deck, so both are **deterministic fixtures**, never cast in a random
  game (the M3b posture). The Blood Moon + Urborg *dependency* likewise needs
  both on one battlefield → deterministic-fixture territory. Random coverage
  trails again (§5).
- **No X, no modes, no counterspells, no Auras/Equipment (Attach is M4), no
  activated/triggered abilities, no 603/614 pipeline.** No serialization / AST
  version field.

## 1. New and grown types

**`Pawl.Type.Modification`** grows three layer-4 constructors — the open-half
type-changing vocabulary, classified `Layer.Type` by `Projection.layer`:

```haskell
data Modification
  = GainKeyword Keyword                      -- layer 6 (M3b, Serpent's Gift)
  | LoseAllAbilities                         -- layer 6 (M3b, Humility)
  | SetBasePowerToughness Quantity Quantity  -- layer 7b (M3b Humility; M3c Opalescence)
  | ModifyPowerToughness Quantity Quantity   -- layer 7c (M3b, Giant Growth)
  | SetLandSubtype Subtype                    -- NEW layer 4 (Blood Moon -> Mountain)
  | AddLandSubtype Subtype                    -- NEW layer 4 (Urborg -> Swamp)
  | AddCardType CardType                      -- NEW layer 4 (Opalescence -> Creature)
  deriving (Eq, Ord, Show)
```

The three modes map exactly to CR 305.7's two clauses plus a card-type add:

- **`SetLandSubtype`** — CR 305.7 *set*: replace the land's land types with the
  one basic type, **empty the object's projected keywords**, and **suppress the
  object's rules-text static abilities** (the strip; §3). The Mountain mana
  ability is *not* granted explicitly — {Mountain} in the projected subtypes is
  the CR 305.6 R ability (§4). (305.7: "it loses all abilities generated from
  its rules text… and it gains the appropriate mana ability for each new basic
  land type. Note that this doesn't remove any abilities that were granted to
  the land by other effects.")
- **`AddLandSubtype`** — CR 305.7 *add* ("in addition to its own"): keep land
  types and rules text; add the subtype and its mana ability.
- **`AddCardType`** — add a card type (Creature) to an object; CR 305.7 notes
  setting a *land* subtype touches no card types, so a card-type add is a
  separate layer-4 operation. (Opalescence: "is a creature in addition to its
  other types.")

**`Pawl.Type.Quantity`** grows `ManaValue` — evaluated against the *affected*
object's mana cost (CR 202.3). Opalescence's "base power and base toughness each
equal to its mana value" is `SetBasePowerToughness ManaValue ManaValue`. One
new `Quantity.evaluate` case; still a first-order quantity, no card identity.

**`Pawl.Type.Affected`** grows the dynamic, projection-read sets the cards name:

```haskell
data Affected
  = TheseObjects (Set ObjectId)            -- CR 611.2c fixed set (M3b)
  | AllCreatures                           -- Humility (M3b)
  | AllLands                               -- NEW Urborg
  | AllNonbasicLands                       -- NEW Blood Moon
  | AllOtherNonAuraEnchantments ObjectId   -- NEW Opalescence (carries its own id: "each OTHER")
  deriving (Eq, Ord, Show)
```

**Membership for the three new sets reads the *projected* type line, and those
tests are the coupling sites** — "is this a nonbasic land / a creature" is the
layer-4 answer, not the printed one, which is what makes them layer-4-dependent
(§2, §3). `AllOtherNonAuraEnchantments` carries the source id so Opalescence
excludes itself; it excludes Auras correctly today though no Aura exists to
exercise that branch (an expiry, §6).

**`Pawl.Type.ProjectedCharacteristics`** grows the projected type line:

```haskell
data ProjectedCharacteristics = MkProjectedCharacteristics
  { keywords :: Set Keyword,
    power :: Maybe Integer,
    toughness :: Maybe Integer,
    cardTypes :: Set CardType,   -- NEW (projected, layer 4)
    subtypes :: Set Subtype      -- NEW (projected, layer 4)
  }
  deriving (Eq, Show)
```

Supertypes are *not* projected (CR 305.7: setting a land subtype touches no
supertypes; no M3c card changes a supertype), so "nonbasic" reads the printed
`Basic` supertype. `baseCharacteristics` seeds `cardTypes`/`subtypes` from the
printed type line.

**`Pawl.Card`** gains three Scryfall-verified printings, each with
`staticAbilities` and empty `targetSpecs` (none targets):

- **Blood Moon** — `MkStaticAbility AllNonbasicLands (SetLandSubtype Mountain)`.
- **Urborg** — `MkStaticAbility AllLands (AddLandSubtype Swamp)`; a `Legendary
  Land`. The first *land* carrying a static ability.
- **Opalescence** — two abilities sharing the same affected set:
  `AllOtherNonAuraEnchantments <self>` with `AddCardType Creature`, and the same
  set with `SetBasePowerToughness ManaValue ManaValue`.

Supporting `CardType` (Enchantment, Creature, Land), `Subtype` (Mountain, Swamp,
Forest — the basic land types already exist), and `Supertype` (Basic, Legendary)
values are filled in as the cards require. Humility's M3b printing is unchanged.

## 2. The generalized projection

`Pawl.Projection` keeps its `gather → order-within-layer → fold` shape and stays
**the sole `case`-on-`Modification` home** — the resolver included, because
detecting dependencies means trial-applying modifications through
`applyModification`/`affects`, and confining that to `Projection` preserves the
invariant (`Resolve : Effect :: Projection : Modification`). Three changes:

**(1) The applier gains the three layer-4 modifications** (the only new
`case`-on-`Modification` arms):

- `AddLandSubtype s` → `subtypes := Set.insert s subtypes` (and the CR 305.6
  mana ability follows from the projected subtype at the mana call site, §4).
- `AddCardType t` → `cardTypes := Set.insert t cardTypes`.
- `SetLandSubtype s` → `subtypes := Set.singleton s`, `keywords := Set.empty`
  (305.7 rules-text strip of keyword abilities). The *static-ability* strip is
  enforced in `gather` (below), because it governs what this object contributes
  to *other* objects, not its own P/T fold.

**(2) `gather` becomes projection-aware, and its element grows a source id.**
The M3b tuple `(Layer, Timestamp, Modification)` cannot express the existence
dependency the resolver detects (does applying B strip A's *source* ability?),
so the gathered element grows the **source `ObjectId`** — a `GatheredEffect`
carrying `(source, layer, timestamp, modification)`. The source is already in
hand at both gather sites (the static branch's `permId`; a stored effect's
`ContinuousEffect.source`). A battlefield permanent's static abilities are
collected only if that permanent's rules text is still active — i.e. no
`SetLandSubtype` applies to it (CR 305.7). Deciding that means **projecting the
source permanent** — the cross-object coupling: to know whether Urborg still
generates "each land is a Swamp," project Urborg under Blood Moon. Projection
stops being per-object-independent.

- **Termination and cost:** `project` carries an **intra-call memo** keyed by
  `ObjectId`; a source-projection cycle trips a visited-set and the cycle's
  effects fall to the **CR 613.8b loop rule** (timestamp order). This is a
  *within-`project`-call* memo only; across queries M3c still recomputes (the
  cross-query cached field stays deferred, §6, its pressure now noted).

**(3) The within-layer ordering becomes the dependency resolver** (§2.1),
replacing the M3b `List.sortOn timestamp`.

New projected reads join `keywordsOf`/`powerOf`/`toughnessOf`: `subtypesOf`,
`cardTypesOf`, and `isCreatureOf = Set.member Creature . cardTypesOf`.

### 2.1 The resolver — trial application

Within one layer's gathered effects, order them per CR 613.8:

```haskell
-- The application order of one layer's effects, given the partial projection
-- built by lower layers. Detection IS trial re-application: an effect A depends
-- on B iff hypothetically applying B on top of the partial changes A's affected
-- set over the candidate objects OR A's source-ability existence (CR 613.8a).
orderLayer :: GameState -> ProjectedCharacteristics -> [GatheredEffect] -> [GatheredEffect]
```

The algorithm (all four CR 613.8 clauses):

- **613.8a — dependency.** A depends on B iff (a) same layer [true — one layer
  at a time]; (b) applying B would change **A's affected set** over the
  candidate objects, **or the existence** of A's effect (its source ability
  being stripped), **or what A does**; (c) neither is a CDA [true — no CDAs at
  M3c, so clause (c) holds trivially]. Detection is a **single hypothetical
  step**: compute A's parameters against the partial-so-far, then against
  partial-so-far + B, and diff. No full recursive re-projection — the partial is
  the context, which is what bounds the work.
- **613.8b — ordering and loops.** Apply the effects whose dependencies are all
  already applied, **earliest timestamp first**. If every remaining effect
  depends on another remaining one (a dependency loop), ignore 613.8 for the
  loop and apply its members in timestamp order.
- **613.8c — re-evaluation.** After each application, re-evaluate dependencies
  among the remaining effects; they may change.

**Why this is cheap for pawl and was not for Argentum:** the substrate is
immutable and the partial characteristics are memoized, so trial re-application
is a value comparison, not a copy-on-write of mutable game state. This is the
"unclaimed territory the substrate is built for" (risk register). **Completeness
is a tracked metric, not a claim:** the §5 test set is the tripwire, and any
dependency shape the trial step fails to detect must land as a *failing* test.

### 2.2 Worked outcomes

**Blood Moon + Urborg** (both layer 4, both non-CDA):

- Does **Urborg depend on Blood Moon**? Applying Blood Moon sets Urborg's
  subtype to Mountain and (CR 305.7) strips Urborg's rules-text ability → the
  *existence* of Urborg's effect changes. **Yes.**
- Does **Blood Moon depend on Urborg**? Applying Urborg adds Swamp to lands; it
  does not change which lands are *nonbasic* (a supertype fact) or what Blood
  Moon does. **No.**
- One-directional, no loop → Blood Moon applies first in **both** timestamp
  orders (613.8b overrides 613.7). After Blood Moon, Urborg's ability is gone, so
  Urborg contributes nothing. **Result:** nonbasic lands are Mountains ({R}
  only); basic lands are untouched by either ({G} for a Forest). A
  timestamp-only fold never strips Urborg, so a Forest wrongly gains Swamp ({B})
  — the falsifier, and the outcome a naive fold produces differs *between* the
  two orders as well.

**Humility + Opalescence** (no 613.8 dependency — different layers):

- Layer 4: Opalescence makes each *other* non-Aura enchantment a creature →
  Humility becomes a creature; Opalescence does not (excludes itself, and
  nothing else makes it one).
- Layer 6: Humility strips abilities from all creatures. Opalescence is not a
  creature, so its ability survives; Humility's own ability, once it has started
  applying, continues (CR 613.6).
- Layer 7b: two effects touch Humility — Humility's "1/1" and Opalescence's
  "mana value = 4" — ordered by CR 613.7 timestamp (each static effect carries
  its object's timestamp, CR 613.7a). Real creatures get only Humility's 1/1.
- **Result:** real creature → 1/1, no abilities; Humility → 1/1 or 4/4 by 7b
  timestamp; Opalescence → a non-creature enchantment with no P/T. This lands
  in the Axis-A (timestamp-only) phase — it needs no resolver.

*(These outcomes are to be re-derived against `rules.txt` when the tests are
written, per the "never trust recalled Magic rules" discipline; the derivation
above is the design intent, not the citation.)*

## 3. Static abilities, the strip, and wear-off

**No resolution path changes.** Blood Moon, Urborg, and Opalescence resolve onto
the battlefield by the ordinary M1a permanent branch; their `staticAbilities`
are gathered live by `project`. Nothing is stored on resolution and nothing is
removed on departure — they simply stop being gathered, exactly as Humility does
(M3b §3). No `ContinuousEffect` is created for any M3c card (they carry no
`Duration`; none is `UntilEndOfTurn`), so CR 514.2 cleanup wear-off is unchanged.

**The 305.7 strip is a `gather`-time decision, not stored state.** When
collecting permanent P's static abilities, `project` first asks whether P is
under a `SetLandSubtype` (by projecting P); if so, P's rules-text static
abilities are omitted. This is the existence side of the Blood Moon + Urborg
dependency, and the reason `gather` must project sources (§2).

## 4. Integration across the rules core

Per the "everywhere type is consulted" scope, every printed-type read is rewired
to its projected counterpart. Combat/SBA/Damage/Target *logic* is unchanged —
only the source of the type answer moves from the printed card to `Projection`.

- **Mana (the Blood Moon/Urborg observable).** The intrinsic CR 305.6 land tap
  reads `subtypesOf` (projected). {Mountain} → {R}, {Swamp} → {B}, {Forest} →
  {G}; a land with several basic subtypes offers each. Blood Moon'd land taps
  {R}; Urborg'd land taps {B}; Urborg itself taps {B} (its own "each land is a
  Swamp" makes it a Swamp).
- **`Sba`.** 704.5f (zero toughness) and 704.5g (lethal) creature checks read
  `isCreatureOf`/`toughnessOf`. An Opalescence'd enchantment is a real creature
  that dies to lethal damage or to a 0-toughness projection.
- **`Target`.** `CreatureTarget` legality (M3b) reads `isCreatureOf`; an
  Opalescence'd enchantment is a legal creature target and re-validates under CR
  608.2b through the same read.
- **`Combat`.** Declare-attackers / declare-blockers legality and combat damage
  read `isCreatureOf`. Summoning sickness is unchanged in shape — it keys on
  `Object` control-time (CR 302.6), so an enchantment that becomes a creature
  composes without special handling.

## 5. Setup, decks, and testing

**Testing follows the phased spine (§0), test set before resolver:**

**Phase 1 — Axis A, timestamp-only (passing):**

- **Blood Moon alone** (CR 305.7 set, 305.6): a nonbasic land projects to
  subtypes {Mountain} and taps {R}; its printed keywords/abilities are stripped.
- **Urborg alone** (CR 305.7 add, 305.6): every land projects Swamp in addition
  and taps {B}; Urborg itself taps {B}.
- **Opalescence alone** (CR 613 layers 4 + 7b, `Quantity.ManaValue`): a non-Aura
  enchantment projects as a Creature with base P/T = its mana value; it is a
  legal `CreatureTarget` and dies to a lethal-damage SBA.
- **Humility + Opalescence, both 7b timestamp orders** (CR 613.7): real creature
  1/1 no abilities; Humility 1/1 or 4/4 by order; Opalescence a non-creature
  enchantment. Order controlled by placement sequence reading `nextTimestamp`
  monotonicity (M3b's technique).

**Phase 2 — the dependency test set (written here, failing against Phase 1):**

- **Blood Moon + Urborg + a Forest, Blood-Moon-older**, asserting the correct
  outcome: Forest taps {G} only, the nonbasic land taps {R} only.
- **Blood Moon + Urborg + a Forest, Urborg-older**, asserting the *same* correct
  outcome. Fails under the timestamp-only fold (Forest wrongly taps {B}).

  Ported from Argentum's `ClassicLayerScenariosTest.kt` / `LayerSystemTest.kt` as
  the target list, but asserting corrected outcomes — its Blood Moon test asserts
  a documented-wrong one (`prior-art-lessons.md`). Test names cite CR 613.8 +
  305.7.

**Phase 3 — the resolver (makes Phase 2 pass): the go/no-go verdict.** If trial
application cannot be made to pass Phase 2, Phases 1–2 are still sound and
committed; the failure is isolated.

**Setup.** `emptyGame` is unchanged for the new counters (M3b already added
`nextTimestamp`/`continuousEffects`); the fixtures place the enchantments and
lands directly on the battlefield, assigning `Object.timestamp` from
`nextTimestamp` as M3b's setup does.

**Random-game coverage trails again** (per the M3 note, exactly M3b's white-only
Humility posture). Humility/Opalescence are white → deterministic fixtures only.
The Blood Moon + Urborg *dependency* needs both on one battlefield →
deterministic fixtures. Blood Moon (`{2}{R}`) *may* ride `redDeck` and Urborg a
deck to give *single-effect* layer-4 land type-changing random-play coverage (a
land's type and mana color changing), keeping the 36-land/24-spell balance so
`objectCount` and conservation are untouched; the plan settles deck composition.
The fuller tail — a white/blue matchup and reliable multi-enchantment setups —
is explicitly deferred past M3.

**Properties** (`runMatch`, both matchups): every M2d/M3a/M3b invariant as it
stands — conservation, termination, ids, no floating mana, life never increases,
combat happens, green-black engagement. Replay determinism now covers the
projected type line and the resolver's ordering in the serialized path. The
benchmark stays on `redDeck`; throughput is watched for the deepened per-query
projection cost (the coupling), not asserted.

## 6. What M3c preserves, and the expiries it opens

**Preserves:**

- **The two invariants.** `Projection` is the sole `Modification` executor — the
  resolver included; `Resolve` the sole `Effect` executor; no rules-core module
  (`Combat`, `Damage`, `Sba`, `Engine`, `Cast`, `Target`, `Mana`) cases on
  `Modification` or `Effect`. They ask `Projection.layer` / the projected reads.
- **No forced prompt elided.** Target choice for `CreatureTarget` spells stays
  prompted; the resolver makes no player choice.
- **Combat / SBA / Damage / Target logic:** unchanged; only printed→projected
  type reads move.
- **`NamedFieldPuns`** per the M3b amendment; no new convention change.

**Expiries this milestone opens:**

- **Resolver completeness is the tracked metric.** The Phase-2 pairs are the
  tripwire; any dependency shape the trial step fails to detect lands as a
  failing test, never a doc claim (risk register D2 — the Argentum trap).
- **Layers still without producers**: 1 (copy), 2 (control), 5 (color), 7a
  (CDA), 7d (P/T switch) grow appliers with their first card. **Layer 3
  text-changing (Magical Hack) is M3d**, its own go/no-go.
- **Cross-query memoized projected state** (design.md §2.5/§2.10) stays
  deferred; M3c adds only the intra-`project`-call memo. The coupling deepens
  per-query cost, so the cached field's pressure is now noted — it becomes a
  memoization of this one funnel when throughput demands it.
- **`AllOtherNonAuraEnchantments` Aura-exclusion** is correct but untestable
  until an Aura exists (Attach, a 701 keyword action, M4).
- **Supertypes are not projected** — "nonbasic" reads the printed `Basic`
  supertype; when a card changes a supertype (layer 4 supertype-setting), this
  becomes a projected question, one function, the same move as `subtypesOf`.
- **Urborg legend rule (CR 704.5j)** — tests keep it singleton or invoke the
  704.5j elision, à la Mindslaver.
- **Summoning sickness over projected creature-ness** composes today (keys on
  `Object` control-time); an effect that *grants* control or resets the object's
  control-time is the future customer.

**Explicitly deferred past M3c:**

- **Layer 3 text-changing (Magical Hack)** — M3d, the next go/no-go.
- **Activated / triggered abilities, the 603/614 event pipeline** — M3e/M3f.
- **A white or blue matchup** for random continuous-effect coverage — the
  post-M3 tail.
- **X, modes, counterspells, Auras/Equipment, new card types beyond those the
  three cards need, serialization / AST version field.**
