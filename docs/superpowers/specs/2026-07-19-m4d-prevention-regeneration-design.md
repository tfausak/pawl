# M4d prevention & regeneration — design

Design for milestone **M4d**, the fourth letter of M4 (see the split table in
`docs/design.md` §3): **the two remaining shield shapes.** M3f proved a
replacement can *redirect* one event type (a zone change, graveyard→exile) from a
*static* source (Rest in Peace). M4d proves the other two behaviors CR 614/615
describe — **cancel** (damage that would be dealt simply doesn't happen) and
**replace-with-side-effects** (a destruction becomes tap + heal + remove from
combat) — each installed by a *resolving spell or ability* with a *duration*, and
each hooked into a *new funnel* (the damage funnel; a new unified destruction
funnel). It also cashes the expiry M4b opened when it left `Destroy` as "a plain
check-then-move, not yet an interceptable destroy event."

Two gate cards, Scryfall-verified (`api.scryfall.com/cards/named`, fetched
2026-07-19):

| Card | Cost | Type line | P/T | Oracle text |
|---|---|---|---|---|
| **Fog** | `{G}` | Instant | — | "Prevent all combat damage that would be dealt this turn." |
| **Drudge Skeletons** | `{1}{B}` | Creature — Skeleton | 1/1 | "{B}: Regenerate this creature. *(The next time this creature would be destroyed this turn, instead tap it, remove it from combat, and heal all damage on it.)*" |

Each **falsifies its own naive implementation** on the one axis it owns:

- **Fog** cannot be a redirect. M3f's only replacement shape rewrites an event's
  *destination*; there is no destination that means "this damage never happened."
  Fog forces the **cancel** shape — the funnel drops the event, and no life is
  lost, no damage is marked, no deathtouch bit is recorded. And Fog must touch
  *only combat* damage, so it forces the `DamageEvent` to carry whether it is
  combat damage — a Fog on the battlefield must not blunt a Blaze (CR 615, the
  negative test).
- **Drudge Skeletons** cannot be modeled as "the Destroy opcode checks a flag."
  Its regeneration shield must save the creature from **every** destruction — the
  `Destroy` opcode (Murder) *and* the state-based action that buries a creature
  with lethal combat damage (CR 704.5g) — which forces a **single destruction
  funnel** both routes flow through. And the shield is **one-shot** (CR 701.19a
  "the next time"): destroy it, regenerate, destroy it again, and the second
  destruction kills it. A model that saved it permanently, or only against the
  opcode, is wrong.

This is a types-and-architecture spec, not an implementation plan.

## 0. The core idea and the two phases

M4d is one milestone in **two internal phases** (the user's framing; the M4a
two-phase precedent). The phases are independent — neither's funnel depends on the
other's — and are ordered cancel-then-replace: the damage cancel is the simpler
seam and the purer demonstration of "a replacement that isn't a redirect"; the
destruction funnel is a refactor of two existing sites.

The load-bearing insight both phases share is the one M3f established and this
milestone generalizes: **an event is a value; a shield rewrites (or drops) it
before it happens; the funnel is the single place that consults active shields.**
M3f built that for `changeZone`. M4d builds it for the *damage* funnel
(`Damage.applyDamage`, whose own comment already reserves the seam: "the one seam
CR 614's replacement step will hook") and for a *new* destruction funnel
(`Event.destroy`) that unifies the Destroy opcode and the lethal-damage SBA.

**Phase 1 — damage prevention, the cancel shape (gate: Fog).**

1. **Tag combat damage.** `DamageEvent` grows `kind :: DamageKind`
   (`Combat | Noncombat`). Combat damage (`Pawl.Damage`) is `Combat`; a resolving
   `DealDamage` (Blaze/Bolt, `Pawl.Resolve`) is `Noncombat`.
2. **The prevention shield.** A `Prevention` leaf (`PreventAllCombatDamage`), a
   floating store `GameState.preventions`, the `Effect.Prevent Duration Prevention`
   opcode Fog resolves into, and `Event.applyPreventions` — the sole caser on
   `Prevention` — which `Damage.applyDamage` consults before marking or draining.
3. **Expiry.** Cleanup drops `UntilEndOfTurn` preventions
   (`Event.dropEndOfTurnPreventions`), mirroring `Projection.dropEndOfTurnEffects`.

**Phase 2 — regeneration, the replace-with-side-effects shape (gate: Drudge
Skeletons).**

4. **The unified destruction funnel.** `Event.destroy :: ObjectId -> GameState ->
   GameState`, consulting CR 700.4 indestructibility and CR 701.19a regeneration
   shields.
5. **The one-shot shield.** A store `GameState.regenerationShields :: Map ObjectId
   Natural`, the `Effect.RegenerateSelf` opcode Drudge Skeletons' ability resolves
   into, and cleanup clearing it ("this turn").
6. **Rewire the two destroy sites.** The `Destroy` opcode (M4b) and the SBA's
   destruction cases (CR 704.5g/h) route through `Event.destroy`; the SBA's
   toughness-≤-0 case (CR 704.5f) stays a plain put-into-graveyard.

Phase 1 precedes Phase 2 (simpler seam first); within each the gate card is what
the phase is proven against.

## Goal and scope

**Exit criterion.** Deterministic tests demonstrate all of:

- **Fog cancels combat damage (CR 615, the phase-1 gate).** An attacker is blocked
  (or unblocked and hitting a player); Fog is cast and resolves before the combat
  damage step; in the damage step **no** combat damage is marked or drained —
  attacker and blocker both survive, and the defending player's life is unchanged.
  With a first-striker in the fight, **both** the first-strike and regular waves
  are prevented (the shield is "this turn," consulted at each wave).
- **Fog does not prevent noncombat damage (the falsifier).** With Fog resolved,
  a Blaze (`Noncombat`) still deals its X to its target. The `kind` tag is
  load-bearing: comment names that a tag-blind implementation prevents the Blaze.
- **A regeneration shield saves a creature from the Destroy opcode (CR 701.19a,
  the phase-2 gate).** Drudge Skeletons activates `{B}: Regenerate this creature`
  (a shield is installed); Murder (`{1}{B}{B}`, M4b) resolves targeting it; the
  destruction is replaced — the Skeleton stays on the battlefield, now **tapped**,
  with **all damage removed**, and removed from combat if it was in combat. A
  **second** Murder with no shield up kills it (the one-shot falsifier: the shield
  was consumed).
- **A regeneration shield saves a creature from lethal combat damage (CR 704.5g
  via the SBA).** With a shield up, Drudge Skeletons blocks a Goblin Piker (2/1),
  takes 2 (lethal), and the CR 704.5g state-based action's destruction is replaced
  — it survives, tapped, damage cleared. This is the proof that the SBA destruction
  path and the Destroy opcode share **one funnel**.
- **Regeneration intercepts only destruction (the negative test).** A shielded
  Skeleton reduced to toughness ≤ 0 (CR 704.5f) still dies (it is *put into the
  graveyard*, not *destroyed* — the shield does not fire and is not consumed), and
  a shielded Skeleton bounced by Unsummon (M4b) still returns to hand. Regeneration
  is not a general "don't leave the battlefield" effect.

The `DecisionLog` replays deterministically — **no new prompt or response**: Fog is
a targetless instant (existing cast path, no `ChooseTargets`), and Drudge
Skeletons' regeneration is a targetless activated ability acting on its own source
(existing `Activate` path). The honesty round-trip (`jsonToCard . cardToJson ≡
Right`) holds over `allPrintings` including Fog (a `Prevent` opcode carrying a
`Prevention` and a `Duration`) and Drudge Skeletons (an activated ability whose
effect is `RegenerateSelf`, and the `Skeleton` subtype).

**Non-goals** (each a named expiry in §8):

- **CR 701.19c "can't be regenerated" is not built.** No in-scope card says it.
  Because `Event.destroy` is the single destruction chokepoint, the extension is
  local and documented (§3, §8): a future `Regenerability` argument
  (`CanRegenerate | CantRegenerate`) plus a flag on `Effect.Destroy` (per-
  destruction, Wrath of God) or a game-state check (continuous, "creatures can't be
  regenerated this turn") gates the shield branch — shields are then *not applied*
  but *not consumed* (they persist, CR 701.19c). Due with the first card that says
  it.
- **No amount-shields (CR 615.7).** "Prevent the next N damage that would be dealt
  to any target this turn" needs a shield that carries a remaining amount, is
  consumed as damage is prevented, and — when two sources would deal damage at once
  — prompts the shielded player which to prevent. Fog is all-or-nothing over a
  *class* of events, with no amount and nothing to choose. Due with the first
  "prevent the next N" card (Healing Salve's mode).
- **No static-ability prevention (CR 615.10) and no prevention from a source (CR
  615.2 / 609.7).** Fog is a one-shot floating effect over all combat damage; a
  permanent that continuously prevents, or an effect scoped to a source, is future.
- **Prevented events are not retained.** CR 615.13 (abilities that trigger *when*
  damage is prevented) and CR 615.5 (an additional effect referencing the amount
  prevented) both need the prevented event kept; `applyPreventions` drops it. Due
  with the first "when this prevents damage" / "prevent … you gain that much life"
  card.
- **No general "Regenerate target creature."** `RegenerateSelf` covers the
  activated-ability form (the source regenerates itself). A spell that regenerates
  a *chosen* creature is the future `Regenerate SlotName`, targetless-to-targeted,
  additive.
- **No static-ability regeneration (CR 701.19b).** "Creatures you control have
  regeneration" replaces destruction *each* time (not one-shot) via a static
  ability; M4d's shields are all resolution-generated one-shots. Additive to
  `Event.destroy`.
- **No "was destroyed" event.** `Event.destroy` funnels through `changeZone` to the
  graveyard (which emits the ordinary zone change), but does not emit a distinct
  *destruction* event for a trigger that cares specifically about destruction (vs.
  any leave-the-battlefield). Due with the first such trigger.

## 1. New and grown types

**`Pawl.Type.DamageKind`** (new leaf — one type per module). Non-boolean-blind per
the style rule; the `DamageEvent` comment already sanctions growing the payload
("lifelink and M4 combat-damage triggers grow the payload rather than reshape
it").

```haskell
-- Whether a damage event is combat damage (CR 510) or not (a resolving spell or
-- ability, CR 608). Read by Event.applyPreventions (Fog prevents only combat
-- damage) and, later, by combat-damage triggers and lifelink. A Bool would blind
-- the reader to which it is.
data DamageKind = Combat | Noncombat
  deriving (Eq, Ord, Show)
```

**`Pawl.Type.DamageEvent`** grows one field:

```haskell
    -- CR 510 vs 608: combat damage or not. Set at deal time -- Damage tags Combat,
    -- Resolve's DealDamage tags Noncombat. Read by Event.applyPreventions.
    kind :: DamageKind
```

**`Pawl.Type.Prevention`** (new leaf family — its own module). One constructor at
M4d; **only `Pawl.Event` may case on it** (the `ReplacementEffect` / `Modification`
standing). Extensible to the deferred shapes (from-a-source, next-N-amount).

```haskell
-- CR 615.1a: a prevention effect specification -- classified by the damage events
-- it watches and cancels. PreventAllCombatDamage watches every Combat-kind event
-- and drops it (Fog). Its own leaf family, distinct from Effect (one-shot),
-- Modification (continuous, layered), and ReplacementEffect (zone-change redirect).
-- Only Pawl.Event may case on it.
data Prevention = PreventAllCombatDamage
  deriving (Eq, Ord, Show)
```

**`Pawl.Type.ActivePrevention`** (new — mirrors `Pawl.Type.ContinuousEffect`, the
stored-with-a-duration wrapper). A prevention spec plus how long it lasts. No
timestamp (Fog has no ordering interaction; CR 615.7's multi-source choice is
deferred), no source (CR 615.13 "prevented by" triggers are deferred).

```haskell
-- A floating, resolution-generated prevention effect (CR 615.3), held in
-- GameState.preventions. `duration` decides when cleanup drops it (CR 514.2), the
-- prevention analog of ContinuousEffect for the event pipeline rather than the
-- projection.
data ActivePrevention = MkActivePrevention
  { prevention :: Prevention,
    duration :: Duration
  }
  deriving (Eq, Ord, Show)
```

**`Pawl.Type.GameState`** grows two stores:

```haskell
    -- CR 615.3: floating prevention effects from resolutions (Fog), each with a
    -- duration cleanup consults. The event-pipeline analog of continuousEffects
    -- (which the projection consults). Event.applyPreventions reads it.
    preventions :: [ActivePrevention],
    -- CR 701.19a: one-shot regeneration shields, counted per object (activating
    -- twice stacks two; each destruction consumes one). Keyed by the shielded
    -- object's id -- stable across regeneration (the creature stays on the
    -- battlefield). Cleared at cleanup ("this turn"). Event.destroy reads it.
    regenerationShields :: Map ObjectId Natural
```

**`Pawl.Type.Effect`** grows two constructors. `Resolve` remains the sole module
that may `case` on `Effect`; both arms join `slotsOf`, `readsX`, `manaProduced`,
`searchesLibrary`, and `rewriteEffect` (§4).

```haskell
  | -- CR 615.3: install a floating prevention effect for a duration. Fog =
    -- Prevent UntilEndOfTurn PreventAllCombatDamage. Targetless (Fog watches a
    -- class of events, not a chosen object). Parallels ModifyTarget (Duration +
    -- spec) but installs into GameState.preventions, not the layer store. Resolve
    -- stores it; Event.applyPreventions applies it.
    Prevent Duration Prevention
  | -- CR 701.19a/c: install a one-shot regeneration shield on THIS effect's source
    -- permanent (CR 608.2g) -- targetless and self-referential (Drudge Skeletons'
    -- "{B}: Regenerate this creature"). Adds one to GameState.regenerationShields
    -- for the source id. NOT the act of regenerating (701.19c): the shield fires
    -- later, at Event.destroy. A general "Regenerate target creature" is the future
    -- Regenerate SlotName (§8).
    RegenerateSelf
```

**`Pawl.Type.Subtype`** grows `Skeleton` (Drudge Skeletons' creature type),
following M4b's `Myr` — one catalog entry.

**No new `Prompt`, `Response`, `Zone`, `ReplacementEffect`, or `TriggerCondition`.**
Prevention and regeneration each get their own store and their own `Event`
applier; neither is a zone-change replacement (M3f's `ReplacementEffect` stays a
one-constructor zone-change family).

## 2. Phase 1 — damage prevention (the cancel shape)

**Where the cancel happens.** `Damage.applyDamage` is already the sole place damage
is marked/drained and the sole place a `DamageEvent` is recorded — for combat's two
waves and resolving effects alike (its M3f comment names it the seam CR 614 will
hook). Phase 1 inserts the prevention step at the head of that funnel:

```
applyDamage events gs =
  let kept = Event.applyPreventions (GameState.preventions gs) events
   in <mark/drain kept exactly as today, and record only kept into damageEvents>
```

**`Event.applyPreventions :: [ActivePrevention] -> [DamageEvent] -> [DamageEvent]`**
is the sole caser on `Prevention`. For `PreventAllCombatDamage` it drops every
event whose `kind == Combat`; unmatched events pass through. This is the **cancel**
shape: a prevented event *never happens* (CR 615.6) — it is not marked, not
drained, and never enters `GameState.damageEvents`, so no deathtouch bit is
recorded and no CR 704.5h SBA sees it, and no life is lost. Dropping (not zeroing)
the event is what makes "prevented ⇒ never happened" literal.

- **CR 615.4/615.6 ordering is automatic.** Preventions are read from the state at
  the moment damage is applied, and combat damage is gathered-then-applied in one
  `applyDamage` call (CR 510.2 simultaneity), so a Fog that resolved earlier in the
  step is already in `preventions` when the wave lands. There is no "go back in
  time" risk — the shield exists before the event.
- **First strike.** `Damage.dealCombatDamage` calls `applyDamage` once per wave
  (`dealWave`); each wave consults `preventions`, so "all combat damage this turn"
  covers both the first-strike and regular waves and any additional combat phase —
  the duration, not per-wave bookkeeping, is what spans them.
- **Module dependency.** `Pawl.Damage` gains an import of `Pawl.Event`.
  `Pawl.Event` imports only `Pawl.Game` and `Pawl.Projection` (and types); neither
  imports `Pawl.Damage`, so the edge is acyclic. `applyPreventions` is pure, so
  `applyDamage` stays `[DamageEvent] -> GameState -> GameState`.

**The opcode.** `Effect.Prevent Duration Prevention` in `Resolve.applyEffect`
appends `MkActivePrevention prevention duration` to `GameState.preventions`. Fog is
`Prevent UntilEndOfTurn PreventAllCombatDamage`. Targetless and unprompted.

**Expiry.** `Event.dropEndOfTurnPreventions :: GameState -> GameState` filters out
`UntilEndOfTurn` preventions; the cleanup step (`Engine.hs`, the
`Ending Cleanup` arm that already calls `Damage.removeAllDamage` and
`Projection.dropEndOfTurnEffects`) calls it alongside those, so a Fog and an
until-end-of-turn continuous effect wear off together (CR 514.2, simultaneously).

## 3. Phase 2 — regeneration and the unified destruction funnel

**`Event.destroy :: ObjectId -> GameState -> GameState`** is the new single
chokepoint every destruction flows through. In order:

1. **No-op if the object is gone** (`Game.lookupObject`).
2. **CR 700.4 indestructible** (read off the projection, so Humility strips it):
   the permanent can't be destroyed — the destruction event never happens, so the
   shield is neither applied nor consumed (CR 614.7). No-op.
3. **CR 701.19a regeneration shield** present (`regenerationShields` count > 0):
   consume one (decrement, removing the key at zero) and apply the regeneration
   action — set `damage = 0`, set `tapped = Tapped`, and remove the object from
   combat if it is an attacker or blocker (the `Combat` maps in `GameState.combat`).
   The permanent **stays on the battlefield**.
4. **Otherwise**: `changeZone oid Zone.Graveyard` — the M4b behavior, through the
   M3f funnel (so Rest in Peace's redirect and cease-to-exist for a token still
   compose).

Step 3's action is the exact CR 701.19a text — "remove all damage marked on it and
its controller taps it. If it's an attacking or blocking creature, remove it from
combat." Removing from combat requires touching `GameState.combat`; the plan
decides whether that reuses a `Pawl.Combat` helper or edits `Pawl.Type.Combat`'s
maps directly (`Event` must not import `Pawl.Combat` if that would cycle — the
plan checks).

**The `Regenerability` seam (CR 701.19c), documented not built.** `Event.destroy`
takes no regenerability argument in M4d — every caller would pass "can regenerate."
The single-chokepoint shape makes the future change local: an added
`Regenerability` parameter, threaded from a flag on `Effect.Destroy` (Wrath's
"they can't be regenerated") or a game-state check (a continuous "can't be
regenerated this turn"), gates step 3 — when it forbids regeneration, step 3 is
skipped and the shield is left in place (not consumed). §8.

**Rewiring the two destruction sites:**

- **The Destroy opcode** (`Resolve`, M4b) currently checks indestructible then
  `changeZone`s to the graveyard. It becomes a call to `Event.destroy`, which now
  owns the indestructible check. Murder's behavior against Darksteel Myr is
  unchanged (step 2 no-ops); against Drudge Skeletons with a shield, step 3 fires.
- **The state-based actions** (`Sba`). `Sba.creatureDies` today returns one `Bool`
  merging CR 704.5f (toughness ≤ 0), 704.5g (lethal marked damage), and 704.5h
  (deathtouch). Regeneration saves **g and h** (destructions) but **not f** (a
  *put-into-graveyard*, not a destruction — CR 701.19a, and 704.5f is explicitly
  ungated by indestructible for the same reason). So the merged predicate splits by
  *why* a creature dies:
  - **Destruction (704.5g/h, indestructible-gated as today)** → route the creature
    through `Event.destroy` (regeneration-interceptable). The indestructible gate
    stays in the classification so an indestructible creature never enters the
    destruction set (it doesn't die); `Event.destroy`'s own step-2 check is the
    defensive/opcode path.
  - **Zero toughness (704.5f, ungated)** → plain `changeZone oid Graveyard` (the
    M4b path; regeneration cannot save it, and a token still ceases to exist via
    704.5d).

  The `acted` flag (which drives the CR 704.4 / 117.5 settle loop) must count a
  destruction as *performed* even when regeneration saved the creature — the
  destruction SBA did happen. The loop then re-checks and **terminates**: the regen
  action cleared the creature's marked damage, so it no longer meets 704.5g and
  does not re-enter the destruction set. The plan owns the exact `creatureDies`
  refactor (a reason-returning classifier, or two predicates) and the `acted`
  computation.

**The opcode.** `Effect.RegenerateSelf` in `Resolve.applyEffect` increments
`regenerationShields` at the effect's **source id** (the ability's source
permanent, CR 608.2g — already threaded into `applyEffect`). A shield on a
non-existent or non-battlefield source is harmless (nothing will destroy it).

**Expiry.** Regeneration shields are "this turn" (CR 701.19a), so the cleanup step
clears `regenerationShields` entirely (an unused shield does not carry over). This
joins the same `Ending Cleanup` arm as the prevention and damage wear-off.

## 4. Classifications, codec, and invariants

**The five `Resolve` classifications.** Both new opcodes join every classification,
per the D4 dataflow lint:

- `slotsOf`: `Prevent` and `RegenerateSelf` are **targetless** → the empty set
  (like `Search`, `ExileAllGraveyards`, `Create`).
- `readsX`: neither reads X (the reserved X slot is unaffected).
- `manaProduced`: `False` for both (neither is a mana ability).
- `searchesLibrary`: `False` for both.
- `rewriteEffect`: identity on both (a text-changer reaches neither a prevention
  spec nor a self-regeneration — no in-scope card needs it; §8-style deferral).

**The codec.** `Pawl.Codec` gains arms for `Prevent` (serializing the `Duration`
and the nested `Prevention` — `Prevention` gets its own tagged codec, `Duration`'s
exists) and `RegenerateSelf` (nullary), plus a `Subtype.Skeleton` arm. `DamageKind`
and the new `GameState` stores are **not** serialized — only card data is
serialized, and `DamageEvent` / `GameState` are runtime values. The honesty
round-trip iterates `allPrintings`, so Fog and Drudge Skeletons are covered
automatically.

**Invariants preserved.**

- **The engine never cases on a card's identity.** `Prevent`/`RegenerateSelf` carry
  their behavior as data; `Resolve` cases on the `Effect` constructor (permitted),
  never on "is this Fog." `Event.applyPreventions` cases on the `Prevention`
  classification, `Event.destroy` reads the indestructible *classification* and a
  shield *count* — never a card's name.
- **The engine makes no choices it should ask about.** Fog (targetless) and
  regeneration (self, CR 701.19a leaves nothing to choose) are correctly
  unprompted — the application of the elision rule where the rules ask nothing, not
  an elision with an expiry.
- **The single-caser rule holds and grows by one each.** `Resolve` stays the sole
  `case effect of` home; `Event` stays the sole home for the replacement/prevention
  families — now `ReplacementEffect` (M3f), `Prevention` (`applyPreventions`), and
  the regeneration shield (`destroy`). Each event type has its own typed applier in
  `Event`, never a mixed fold.
- **One funnel per mutation.** Damage still flows through `applyDamage`; every
  destruction now flows through `Event.destroy`; every zone move still through
  `changeZone`. Phase 2 *reduces* the number of places "destroy" is expressed from
  two to one.

## 5. Setup, decks, and testing

**Decks.** Fog is green (alice's deck, M2d) and Drudge Skeletons is black (bob's
deck); Murder (`{1}{B}{B}`, M4b) already sits in the black deck as the destroy
source. Following M4b/M4c, the precise interactions land as **deterministic
fixtures**, with **optional random-game coverage** by swapping Fog into the green
deck and Drudge Skeletons into the black deck (deck-preserving swaps that keep each
at 60 and the conservation counts intact) as a fast-follow churn tail — not a gate,
and deferrable exactly as M4b's random matchup was.

**Deterministic fixtures** (the exit criteria of §"Goal and scope"):

- **Fog cancels combat** — attacker + blocker both survive, defender's life
  unchanged, no `damageEvents` recorded for the prevented wave; a first-strike
  variant proves both waves are covered.
- **Fog spares noncombat** — a Blaze still resolves for its damage with Fog active
  (the `kind`-tag falsifier, named in a comment).
- **Regenerate vs. Murder** — shield up → Skeleton survives tapped with damage
  cleared; a second Murder with no shield → it dies (one-shot consumption).
- **Regenerate vs. lethal combat damage** — shield up, Skeleton blocks a Piker,
  takes 2, survives via the CR 704.5g SBA routed through `Event.destroy` (the
  shared-funnel proof).
- **Regeneration's negatives** — a shielded Skeleton at toughness ≤ 0 (704.5f) dies
  anyway; a shielded Skeleton bounced by Unsummon (M4b) still goes to hand
  (regeneration intercepts only destruction).
- **Round-trip** — `allPrintings` (now including Fog and Drudge Skeletons)
  round-trips byte-stable, exercising `Prevent`/`Prevention`/`Duration` and the
  regenerating activated ability.

## 6. What M4d preserves

- **M4a–M4c are untouched behaviorally.** No existing opcode changes shape; the
  `changeZone` funnel and every M4b verb keep their semantics (Destroy's *observable*
  behavior against Darksteel Myr is unchanged — it now merely reaches indestructible
  via `Event.destroy`). `DamageEvent`'s new `kind` field is additive; every existing
  reader ignores it. The M3–M4c suite is the regression net.
- **The go/no-go is behind us.** M4d adds no architectural bet — trial application
  (M3c) and the rewritable AST (M3d) already returned YES, and the event pipeline
  (M3f) already proved shields. This is the third replacement *shape* on a proven
  substrate: cancel and replace-with-side-effects joining redirect.
- **A milestone may retire a debt.** M4b's "Destroy is not yet an interceptable
  destroy event" is cashed here — that is the milestone landing, and the `Destroy`
  opcode's comment pointing at "M4d" is discharged.

## 7. The expiries M4d opens

| Expiry | Retired by |
|---|---|
| **CR 701.19c "can't be regenerated" not built** — `Event.destroy` takes no `Regenerability` arg; step 3 is ungated | the first card that says it (Wrath of God's "can't be regenerated"; a continuous "creatures can't be regenerated this turn") — a `Regenerability` arg + a `Destroy` flag or game-state check |
| **No amount-shields (CR 615.7)** — no "prevent the next N," no per-1 shield reduction, no multi-source choice prompt | the first "prevent the next N damage" card (Healing Salve's mode) |
| **No static / from-a-source prevention (CR 615.10, 615.2)** — Fog is a one-shot floating effect over all combat damage | the first continuously-preventing permanent or source-scoped prevention |
| **Prevented events not retained** — `applyPreventions` drops them | CR 615.13 ("when damage is prevented" triggers) or CR 615.5 ("prevent … gain that much life") |
| **No general "Regenerate target creature"** — only `RegenerateSelf` | the first regeneration spell that targets — the future `Regenerate SlotName` |
| **No static-ability regeneration (CR 701.19b)** — shields are resolution-generated one-shots | the first "creatures you control have regeneration" |
| **No distinct destruction event** — `Event.destroy` funnels to the graveyard via `changeZone`, emitting only the zone change | the first trigger that cares about destruction specifically (vs. any leave-the-battlefield) |
| **`rewriteEffect` identity on `Prevent`/`RegenerateSelf`** — text-changing reaches neither | the first text-changer that must rewrite one |

Spec companion: the implementation plan under
`docs/superpowers/plans/2026-07-19-m4d-prevention-regeneration.md` (written next,
via the writing-plans skill) owns the commit decomposition and the TDD step order.
