# M4.5 P1 — Permanent control (layer 2)

*Design pass 2026-07-20. The first phase of the M4.5 umbrella
(`docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`), closing
**GAP-L2** — permanent control. Gate: **Act of Treason**. This spec is
implementable; a `writing-plans` plan follows it. All CR numbers are marked
**(verify)** and must be checked against `docs/rules.txt` before they drive code
(CLAUDE.md: never trust recalled Magic rules).*

## 0. Why this phase, and what it proves

Today the engine has **no notion of control distinct from ownership**. `Object`
carries only `owner`; `Game.controllerOf` returns `owner` verbatim, with a comment
noting it is a deliberate stand-in that *"EXPIRES at M3 (Mindslaver)"* — a control
change is the first thing that separates the two, and no card before this phase
can. `Layer.Control` (CR 613.1b, **verify**) already exists in the layer enum with
no producer; `Sickness` carries a comment that CR 302.6's *continuous* control is
unmodelled and *"must be re-set when control moves."* P1 cashes all three.

**The decision it proves:** a permanent's controller is a **projected
characteristic** (CR 613 layer 2), computed from base ownership plus control
effects — not a base field — and every "you control" question in the rules core
(who attacks, whose mana, whose priority) reads that projection, never `owner`.
The gate falsifies the naive `controller = owner` identity from both directions:
a player attacks with a creature they do **not** own, and the creature's owner
can **not** use it while it is controlled away.

**Gate card — Act of Treason** (`{2}{R}` Sorcery; verify Scryfall for exact text
and cost): *"Gain control of target creature until end of turn. Untap that
creature. It gains haste until end of turn."* Chosen over **Control Magic**
because Control Magic is an **Aura**, dragging in the Attach/Aura subsystem that
M4.5 deliberately does not scope (umbrella §4). Act of Treason installs a
**temporary** layer-2 control effect and reuses the M3b continuous-effect store
and its CR 514.2 cleanup wear-off — no permanent-attachment machinery.

## 1. Scope

**In scope:** control as a projected layer-2 characteristic; the `GainControl`
opcode (until-end-of-turn control); the `Untap` and reused-haste riders; making
the battlefield's "you control" enumeration controller-based; the CR 302.6
control-change summoning-sickness reset; and the one latent ownership-vs-control
bug the funnel missed (mana routing).

**Out of scope (deferred, each with a named expiry — §7):** Auras / indefinite
control (Control Magic); instant-speed / mid-combat control change; CR 613.8
control *dependency* beyond timestamp last-wins; multiplayer leaves-the-game
reversion; mass or conditional untap; control at base (a permanent entering under
another player's control).

## 2. Architecture

### 2.1 Control is projected, not stored (design.md §2.5)

A permanent's controller is **base `owner`, overridden by layer-2 `SetController`
continuous effects**. It is *not* a base `Object` field: control-until-end-of-turn
is a continuous effect with a duration, and the founding immutable-state story
(§2.5) is "remove the effect and recompute," which a mutable base field would
fight (nothing would revert it at cleanup). So control rides the existing
`GameState.continuousEffects` store and the projection, exactly as Giant Growth's
P/T does.

### 2.2 Control lives in `Pawl.Projection` (the module graph requires it)

`Pawl.Projection` imports `Pawl.Game`, never the reverse, and the standing
invariant is that **only `Pawl.Projection` may `case` on a `Modification`**. Both
facts point the same way:

- **`controllerOf` moves from `Game` to `Projection`.** `Projection.controllerOf ::
  ObjectId -> GameState -> Maybe PlayerId` folds the object's layer-2 `SetController`
  effects (gathered by the existing `gather`, filtered to `Layer.Control`, ordered
  by `Timestamp`, **last write wins**) over base `Object.owner`. `Projection` is the
  sole applier of `SetController` — no `applyModification`/`ProjectedCharacteristics`
  detour is needed, because control affects only the controller, nothing in the P/T
  fold. `Game.controllerOf` is deleted.
- **New `Projection.controls :: PlayerId -> GameState -> [ObjectId]`** — the
  battlefield permanents `pid` controls (`Zone.Battlefield` members whose
  `controllerOf == Just pid`). This is the control-based "your permanents"
  enumerator.

Consumers (`Combat`, `Engine`, `Activate`, `Event`, `Damage`) already import
`Projection`; switching their `Game.controllerOf` calls to `Projection.controllerOf`
and their "you control" battlefield enumerations to `Projection.controls` keeps
`Projection` below its consumers and above `Game`. No cycle.

### 2.3 Battlefield membership becomes controller-based

`Game.zoneMembers Battlefield pid` filters by `Object.owner == pid` today. For the
battlefield that is the wrong axis: "the permanents you control" is a **control**
question (CR 108.4 / 110.2, **verify**). The call sites that mean "you control" —
`Combat.legalAttackers`, `Engine.untapAll` / `settleAll`, activation legality in
`Activate`/`Action` — switch to `Projection.controls`. `Game.zoneMembers` stays
**owner-based** and is retained only for the genuinely owner-relative zones
(Library / Hand / Graveyard / Stack) and any audited owner-relative battlefield
need; the audit (§5) classifies each remaining battlefield caller as owner or
control.

### 2.4 Summoning sickness on a control change (CR 302.6, verify)

Sickness stays the existing per-incarnation `Object.sickness :: Sickness` flag
(`Sick`/`Settled`). P1 adds exactly one write: **`GainControl` sets the target's
`sickness = Sick`** — the new controller has not controlled it continuously since
their most recent turn began. The existing untap-step settle (`Engine.settleAll`,
now iterating `Projection.controls`) then re-settles it on the appropriate
controller's untap step. For an **until-end-of-turn** steal this is sufficient and
correct: the effect wears off at the thief's cleanup (before the owner's next
turn), so on the owner's untap step the creature is again theirs and settles — no
wear-off write needed. (Indefinite control, where the thief keeps and later
settles the creature across turns, is the Auras phase; §7.)

### 2.5 The opcode surface

- **`Effect.GainControl Duration SlotName`** (new). `Resolve.applyEffect` reads the
  effect's **source** controller (CR 611.2c fixes it at creation, **verify**),
  freezes it into a `ContinuousEffect { source = the resolving object, timestamp,
  duration, modification = SetController pid, affected = <the slot's target> }`
  appended to `GameState.continuousEffects`, and sets the target `sickness = Sick`.
  Parallels Mindslaver's `ControlPlayerNextTurn` (a dedicated control opcode that
  bakes the controller), but installs a **permanent-control** continuous effect
  (CR 613 layer 2) rather than **player-control** pending state (CR 723) — the two
  are different mechanisms (umbrella §3, census §3.3). Not a reuse of `ModifyTarget`:
  its `Modification` is static card data and cannot carry a resolution-time
  `PlayerId`.
- **`Effect.Untap SlotName`** (new). `Resolve.applyEffect` sets the target
  `Object.tapped = Untapped` (CR 701.20, **verify**). Single-target; mass/conditional
  untap is future (§7). Real vocabulary breadth (design.md §4's "obvious verbs").
- **`Modification.SetController PlayerId`** (new) at `Layer.Control`. Its sole
  reader/applier is `Projection.controllerOf` (§2.2). Adds a `PlayerId` import to
  `Pawl.Type.Modification`; `Projection.layer` classifies it as `Layer.Control`.
- **Haste rider — reused, not new.** Act of Treason's "gains haste until end of
  turn" is `ModifyTarget UntilEndOfTurn (GainKeyword Haste) slot`, the M3b path
  Serpent's Gift already exercises. Verify `GainKeyword Haste` folds at layer 6 and
  `Combat.canAttack` reads it (it already does: `hasKeyword Keyword.Haste`).

### 2.6 The mana fix

`Mana.hs:132` routes a tapped permanent's produced mana to `Object.owner`, not its
controller — a latent bug the `controllerOf` funnel never covered because that
site reads `owner` raw. P1 switches it to `Projection.controllerOf`, so tapping a
**stolen** mana producer (e.g. a controlled Llanowar Elves) adds mana to the
**thief's** pool (CR 106.4 / 605.3b, **verify**). This is a concrete, testable
control consequence and belongs in the gate suite.

## 3. The two invariants

1. **Classification, not identity.** `Projection` remains the sole `case`-on-
   `Modification` home (`SetController` is applied only there); `Resolve` remains the
   sole `case`-on-`Effect` home (`GainControl`/`Untap` executed only there, with
   their five classifications — `slotsOf`/`readsX`/`manaProduced`/`searchesLibrary`/
   `rewriteEffect`). Control is a classification the core consults via
   `controllerOf`, never a card identity.
2. **The engine makes no choices.** `GainControl`'s new controller is *derived* (the
   source's controller — CR 611.2c), not chosen, so nothing new is prompted.
   Targeting uses the existing `ChooseTargets`.

## 4. Cards and tests

All gameplay-level (cast/resolve through the stack, assert on game state); the gate
is a **real** card, the sickness path a **labeled synthetic crutch** (design.md §4).

- **Act of Treason** (real, Scryfall-verified; `data/cards/act-of-treason.json`,
  joins `allPrintings` for the honesty round-trip). Gate scenarios:
  1. **Steal-and-swing:** control an opponent's creature, then declare it as *your*
     attacker against that opponent — passes only because `Projection.controls` and
     `controllerOf` moved off `owner`; the untap rider clears a tapped target; haste
     clears sickness. Assert damage to the opponent.
  2. **Owner can't use it:** while controlled away, the owner cannot declare it as
     an attacker/blocker or activate it (`Projection.controls` excludes it for them).
  3. **Reversion:** after cleanup the control effect wears off (CR 514.2); the
     creature is the owner's again, and — verified next turn — settles normally.
  4. **Stolen mana:** control a mana producer, tap it, assert the mana is the
     thief's (§2.6).
- **Synthetic — "steal until end of turn, no haste"** (labeled crutch;
  `[GainControl UntilEndOfTurn slot]`, `CreatureTarget`). Asserts the stolen
  creature is `Sick` and **cannot attack this turn** (haste absent → `canAttack`
  false). **Documented expiry:** retired when Control Magic / the Auras phase can
  test control-change sickness with a real indefinite-control card across two turns.
- **Random-game coverage:** Act of Treason is a **red deterministic fixture** (the
  M3d posture — no random-game deck entry), keeping the CR 400.7 conservation
  counts undisturbed; a later fast-follow may add a matchup if control churn wants
  random coverage.

## 5. The owner-vs-control audit

Every current raw-`Object.owner` read is classified; only the control ones change.
Verify each against `rules.txt` when the plan touches it.

- **Switch to control:** `Mana.hs:132` (mana → controller, §2.6); the battlefield
  "you control" enumerations (§2.3).
- **Already funneled (no change beyond the `Game`→`Projection` move):** `Combat`,
  `Event`, `Activate`, `Damage` call the old `Game.controllerOf`; they now call
  `Projection.controllerOf`.
- **Stay owner (confirm, don't change):** graveyard/zone destinations
  (`Event`/`Stack`/`Sba` "to its owner's graveyard", CR 701.6a etc.);
  `Game.removeFromZones` (owner-indexed); base-state object creation
  (`Setup`/`Engine`/`Event` set `owner` on new incarnations — correct; a token's
  owner is its creator, CR 111.2). `Stack.hs:58` (Panglacial searching player) and
  `Resolve.hs:242/264` (a resolving spell's controller) are cast-time casters where
  owner == controller for a stack object; switch to `controllerOf` for uniformity,
  noting it is behavior-preserving today. `Game.hs:40` — read its context and
  classify.

## 6. Module & type changes (summary)

- `Pawl.Type.Effect` — add `GainControl Duration SlotName`, `Untap SlotName`.
- `Pawl.Type.Modification` — add `SetController PlayerId` (import `PlayerId`).
- `Pawl.Projection` — add `controllerOf` (fold layer-2 `SetController`), `controls`;
  `layer (SetController _) = Control`.
- `Pawl.Game` — delete `controllerOf`; keep `zoneMembers` owner-based.
- `Pawl.Resolve` — execute `GainControl` (bake source controller, store effect, set
  `Sick`) and `Untap`; add both to the five classifications.
- `Pawl.Mana` — route produced mana through `Projection.controllerOf`.
- `Pawl.Combat` / `Pawl.Engine` / `Pawl.Activate` — "you control" enumerations →
  `Projection.controls`; `controllerOf` calls → `Projection.controllerOf`.
- `Pawl.Codec` — `GainControl` / `Untap` / `SetController` arms; round-trip covers
  Act of Treason and the synthetic.
- `data/cards/act-of-treason.json` (+ the synthetic fixture) added; `allPrintings`
  updated.

## 7. Deferred, with named expiries

- **Auras / indefinite control (Control Magic).** The whole Attach subsystem;
  retires the §4 synthetic and adds the cross-turn settle path (§2.4). → Auras phase.
- **`zoneMembers`/settle for cross-turn control.** P1's until-EOT steal never
  settles under the thief; indefinite control does. → Auras phase.
- **Instant-speed / mid-combat control change** (Ray of Command): removing a
  creature from combat, or adding it, when control moves mid-step. → first
  instant-speed steal card.
- **CR 613.8 control dependency** beyond timestamp last-wins (multiple simultaneous
  control effects that depend on one another). → ties to open git-bug `f90e0c4`.
- **Multiplayer leaves-the-game reversion** (CR 800.4). → multiplayer control work.
- **Mass / conditional untap** (`Untap` is single-target). → first such card.
- **Control at base** (a permanent entering under a non-owner's control, e.g. cast
  from an opponent's library). → first such card.

## 8. Tracking

On completion, re-point or close git-bug `83f1a55`'s GAP-L2 facet (this phase) and
retire the `Sickness` type's "EXPIRES at M3" comment (now cashed). Leave `f90e0c4`
open (§7). Update the M4.5 umbrella if the gate or axis shifted (umbrella §7).

## 9. Exit criterion

A game in which Act of Treason is cast, a creature changes control, attacks its
former controller, and reverts at end of turn completes and replays
deterministically; the gate and synthetic scenarios (§4) pass; the build is
warning-clean and `hooky run` is green.
