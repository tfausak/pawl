# mtgish → pawl closed-half gap census

*Analysis written 2026-07-20, at M4g's completion (M4 done). Diffs the mtgish
card-parser's structural vocabulary against pawl's closed-half axes to surface
rules machinery with no roadmap home. **A census, not a plan** — nothing here is
committed to the M-path; §4/§6 record recommendations for triage.*

## 0. Purpose

mtgish (`_scratch/mtgish`) parses Oracle text into a structured vocabulary. This
document diffs mtgish's *structural* type system against pawl's closed-half
**classification axes** to find rules machinery the real card pool demands that
pawl's M0–M7 roadmap does not yet name.

**What this is not.** It is not a diff of mtgish's 1,317 `Action` variants against
pawl's 18 `Effect` opcodes. A new *effect* is never a pawl blind spot — the open
half grows forever by design (design.md §1). The blind spots live in the **closed
half**: the mechanisms an effect hangs off. So the useful signal is entirely in
mtgish's *non-Action* enums.

## 1. Method — the fold

mtgish is a **transliteration** engine: its enums are identity-shaped and
un-normalized (385 `Trigger` variants, ~150 `CounterType`s, ~250 `PlayerEffect`s).
Diffing variant-for-variant would just reproduce the over-granularity pawl exists
to reject. So **every mtgish enum is folded to its underlying mechanism class**
before comparison:

| mtgish source (variant count) | folds to → pawl axis |
|---|---|
| `ReplacementAction*` / `ReplacableEvent*` (~50 enums, ~130 `Would…` families) | replacement/prevention **event coverage** |
| `Trigger` (385) | trigger-condition classes (`TriggerCondition`) |
| `Cost` (~200) | cost classes (`AbilityCost` / `AdditionalCost`) |
| `Condition` (243) | predicate / target-filter classes (`TargetSpec`) |
| `StaticLayer1–7Effect` (~60) | layer operations (`Modification`) |
| `PlayerEffect` (~250) | **player-scoped continuous effects** (no pawl axis) |
| `Expiration` (~45) | duration classes (`Duration`) |
| `CounterType` (~150) | counter kinds + counter *substrate* (`CounterKind`) |
| zone-bearing enums (`CardsInHand`, `CardInExile`, `PermanentIsPutIntoTheCommandZone`…) | `Zone` |
| player-state Actions (`BecomeTheMonarch`, `BecomeDay`, `GetEnergy`, venture, ring…) | **new closed-half subsystems** |

**Caveat on rule numbers.** Per CLAUDE.md's "never trust recalled Magic rules",
every CR number below is marked *(verify)* and must be checked against
`docs/rules.txt` before it drives code. Mechanism *names* are reliable; section
numbers are not, until checked.

## 2. Baseline — pawl's closed-half axes today (at M4g, M4 complete)

| Axis | Module | Variants now |
|---|---|---|
| Zones | `Zone` | 6 (Library, Hand, Graveyard, Battlefield, Stack, Exile) |
| Phases / steps | `Phase` + `*Step` | fixed sequence, 5 phases / 12 steps |
| Effects (opcodes) | `Effect` | 18 |
| Trigger conditions | `TriggerCondition` | **1** (`SelfEnters`) |
| Replacement shapes | `ReplacementEffect` | **1** (`RedirectZoneChange`) |
| Prevention shapes | `Prevention` | **1** (`PreventAllCombatDamage`) |
| Layer operations | `Modification` | 8 (layers 3, 4, 6, 7b, 7c) |
| Durations | `Duration` | **2** (`UntilEndOfTurn`, `Indefinite`) |
| Counter kinds | `CounterKind` | 2 (+1/+1, −1/−1) |
| Target specs | `TargetSpec` | 8 (M4g added `WallTarget`) |
| Additional costs | `AdditionalCost` | 2 (`TapSelf`, `SacrificeSelf`) |
| Keywords | `Keyword` | 10 (evergreen combat) |
| Player state | `Player` | `life`, `status` only |

These counts are *expected* to be small — pawl has just completed M4. The census is about
which axes have **no roadmap entry for a whole mechanism class**, not about low
counts on axes that grow through normal M4-tail vocabulary work.

## 3. The census

Verdicts: **COVERED** (implemented) · **PLANNED-M?** (on the roadmap) ·
**VOCAB** (grows through normal M4-tail opcode/condition batches — not a
structural gap) · **GAP** (structural machinery with no roadmap home) ·
**OOS** (design.md §6 out-of-scope).

### 3.1 Replacement / prevention — GAP (breadth), shapes mostly planned

pawl models replacement as **specific typed shapes**: `RedirectZoneChange` (M3f,
Rest in Peace), `PreventAllCombatDamage` (M4d, Fog), plus regeneration shields
(M4d). The roadmap frames M4d as adding "the cancel shape vs. M3f's redirect" —
i.e. the *shape* taxonomy (redirect / cancel / modify) is understood.

The gap mtgish reveals is **event coverage**. Its ~130 `Would…` families fold to
~40 distinct **replaceable base events**:

> damage-would-be-dealt · would-be-destroyed · would-die · would-draw ·
> would-discard · would-lose-life / pay-life / gain-life / reduce-life ·
> would-enter (tapped / with-counters / from-zone) · would-leave-battlefield ·
> would-be-put-into-graveyard · would-be-countered · token-would-be-created ·
> would-produce-mana · would-untap · would-put-counters · would-begin-turn /
> draw-step / extra-turn · would-scry / mill / explore / proliferate / roll-dice /
> flip-coin / search / planeswalk / copy-spell / get-energy · would-lose-the-game

pawl has wired **two** of these (~zone-change, combat-damage) plus regeneration.
The replacement *engine* (`GameState.preventions`, `Event.applyPreventions`, the
`changeZone` funnel carrying replacements) exists — but each new event that must
become interceptable is real closed-half work, and the roadmap only names damage
(M4d). **The under-modeling: `ReplacementEffect` has 1 constructor and `Prevention`
has 1; both are card-specific, not event-class-general.** *(CR 614/615 — verify.)*

→ **GAP-R:** the replacement seam needs a general "would-*event*" vocabulary, not
one constructor per gate card. Highest structural leverage after M4. *Complements
the open git-bug "M3f: replacement seam is pure/single — CR 616 ordering +
choice-bearing replacements deferred": that bug is the seam's **ordering/choice**
facet, this is its **event-coverage** facet; together they are the full story.*

### 3.2 Player-scoped continuous effects & restrictions — GAP (no axis exists)

mtgish's `PlayerEffect` (~250 variants) folds to a mechanism pawl **has no axis
for at all**. Representative classes:

- **Permission grants:** `MayPlayAdditionalLands`, `MayCastSpellsFromGraveyard`,
  `MaySpendManaAsThoughItWasAnyColor`, `HasNoMaximumHandSize`.
- **Prohibitions:** `CantCastSpells`, `CantDrawCards`, `CantGainLife`,
  `CantLoseTheGame`, `CantAttackWithCreatures`, `CantBeTheTargetOf…`.
- **Cost modification:** `DecreaseSpellCost`, `IncreaseSpellCost`,
  `ReduceManaCostOfActivatedAbilities`, `SetMinimumSpellCost`.
- **Value overrides:** `SetMaximumHandSize`, `LifeTotalCantChange`,
  `DamageDoesntCauseLifeLoss`.

pawl's `ContinuousEffect` + `Modification` + `Affected` are **object-oriented** —
every `Modification` constructor edits an object's characteristics (P/T, types,
abilities, subtype words). There is no player-scoped continuous-effect axis, and
`Player` carries only `life` + `status`. This is the single largest unroadmapped
closed-half surface. *(CR 604/611 continuous effects apply to players too —
verify.)*

→ **GAP-P:** a player-scoped continuous-effect / restriction axis (a `PlayerEffect`
sibling to `Modification`, resolved by the projection over players). Big, and
touched by nearly every control/prison/cost card.

**Correction (#98, landed).** This section originally opened with a
"**Turn-structure skips:** `SkipsUntapStep`, `SkipsDrawStep`, `SkipsMainPhase`,
`SkipsCombatPhase`, `SkipsUpkeepStep`" bullet, listing them under GAP-P. That was
**the wrong axis**, and following it would have hung a `SkipsDrawStep` off
`PlayerEffect`. CR 614.1b: *"Effects that use the word 'skip' are replacement
effects. These replacement effects use the word 'skip' to indicate what events,
steps, phases, or turns will be replaced with nothing."* mtgish's own
over-granular `PlayerEffect` grouping is what misled the fold; a skip is P5's
`ReplacementEffect`, not a CR 613.11 rules-modifying continuous effect. Built as
`ReplacementEffect.PhaseR` over a `ProposedEvent.WouldBeginPhase`, raised by
`Engine.runStep`, proved by Eon Hub.

Two neighbours that are genuinely *not* on this axis either, and are not GAP-P's
to absorb: `Engine.skipsDraw` is CR 103.8a's first-draw-step **turn-based rule**
(the rules state it directly rather than creating an effect), and
`Turn.dropSkippedCombatSteps` is CR 508.8's **rule**-driven combat skip. Neither
is an effect of any kind, so both correctly stay plain engine predicates.

### 3.3 Layer operations — GAP (layers 1, 2, 5, and CDA missing)

pawl's layer system exists (`Layer`, `ProjectedCharacteristics`, the projection).
`Modification` implements operations in layers 3, 4, 6, 7b, 7c. mtgish's
`StaticLayer1–7Effect` reveals whole layers pawl has no operation for:

| Layer | mtgish operations | pawl status |
|---|---|---|
| **1 copy** | `IsACopyOf`, `SetCopiablePT`, `AddCopiableAbility`, mutate | **GAP** — no copy mechanism anywhere (Clone, copy-spell). CR 707 (verify). |
| **2 control** | `SetController` | **GAP (deep)** — `Object` has only `owner`, **no `controller` field**, so a changed controller has nowhere to live; `Modification` has no control op. Player-control (Mindslaver, `GameState.pendingControl`, CR 723) is a *different* mechanism and does **not** cover Control Magic / Threaten (CR 613 layer 2 — verify). |
| 3 text | `SetName`, `HasTextOf…` | COVERED for subtype words (`ChangeSubtypeWord`); name-change is VOCAB. |
| 4 type | add/set/remove card/creature/land types | COVERED (the common ops); breadth is VOCAB. |
| **5 color** | `AddColor`, `SetColor` | **GAP** — `Modification` cannot change color; `Color` type exists but no op feeds it. |
| 6 ability | add / lose abilities | COVERED (`GainKeyword`, `LoseAllAbilities`). |
| 7 P/T | set / adjust, **`AdjustPTForEach`** (CDA) | 7b/7c COVERED; **characteristic-defined P/T** ("*/*", Tarmogoyf — count game state) is **GAP** unless `Quantity` already counts (verify `Quantity`). 7d P/T switching also absent. |

→ **GAP-L1** (copy), **GAP-L2** (permanent control), **GAP-L5** (color),
**GAP-L7cda** (characteristic-defined P/T). Copy is the largest — a whole rules
section, and §5 flags the layer system as the canary.

### 3.4 New closed-half subsystems (player-state) — GAP (mostly unroadmapped)

Each of these is a small closed-half rules section the card pool needs; mtgish
carries first-class support (Actions + Expirations + Triggers). pawl has none, and
M5 lists only the "nightmares" (723/727/729/732/733), not these:

| Subsystem | mtgish evidence | pawl / roadmap |
|---|---|---|
| **Monarch** | `BecomeTheMonarch`, `UntilAPlayerBecomesTheMonarch`, `CantBecomeTheMonarch` | GAP *(CR 720ish — verify)* |
| **Day / Night** | `BecomeDay`, `BecomeNight`, `WhenDayBecomesNightOr…` | GAP |
| **Energy** | `GetEnergy`, `PayEnergy`, `EnergyCounter` | GAP (player counter, see 3.6) |
| **Poison / infect / toxic** | `PoisonCounter`, `Poisoned`, `Poisonous` | GAP (player counter + loss-at-10 SBA) |
| **Experience / Rad** | `ExperienceCounter`, `RadCounter`, `Radiation` | GAP (player counters) |
| **The Ring / Ring-bearer** | `TemptWithRing`, `WhenTheRingTemptsAPlayer`, `RingBearer` | GAP |
| **Venture / Dungeon** | `VentureIntoTheDungeon`, `DungeonType`, `WhenAPlayerCompletesADungeon` | GAP *(CR 700+/701.49 — verify)* |
| **Initiative / Undercity** | `TheInitiative` | GAP (shares dungeon substrate) |
| **Emblems** | `Emblem`, `Boon` | GAP — also needs **Command zone** (3.5) |
| **Speed / max speed** | `Speed`, `MaxSpeed`, `SpeedOfPlayer` (Aetherdrift) | GAP (recent; low priority) |

Keyword-ability bookkeeping that is *also* subsystem-shaped (storm count, suspend
time counters, foretell, plot, cascade) is **VOCAB** — it rides existing
counter/trigger/cost machinery once those seams generalize, not a new subsystem.

→ **GAP-S:** a family of small subsystems. Individually cheap; collectively the
largest *count* of unroadmapped closed-half sections. Poison/energy are the
highest-leverage (competitive staples, and they force the player-counter
substrate).

### 3.5 Zones — GAP (Command missing), one implemented-axis under-model

pawl's `Zone` has 6. The comprehensive rules define 7 game zones *(CR 400.1 —
verify)*; **Command** is absent. Command is needed for **emblems** (3.4) and
command-zone casting, even before Commander-format questions.

- **Command** — GAP (small, but blocks emblems).
- **Phased-out** is a *status*, not a zone (phased-out permanents stay on the
  battlefield, CR 702.26 — verify); **phasing** as a mechanism is absent. GAP.
- **Face-down / turned-face-up** (morph, disguise, manifest, cloak) is a status +
  the copy/characteristics machinery — GAP, tied to copy (3.3).
- **Ante** zone — **OOS** (§6).
- **"Outside the game"** (wish targets, `MayPlayLandsFromOutsideTheGame`) — mostly
  **OOS**-adjacent (sideboard); flag as VOCAB-if-ever.

→ **GAP-Z:** add Command; design phasing and face-down as status axes (not zones).

### 3.6 Counters — GAP is the substrate, not the ~150 names

`CounterKind` has 2; mtgish `CounterType` has ~150. **The names are VOCAB** (a
charge counter is a named counter, no new mechanism). The structural gaps:

- **Counters on players** — poison/energy/experience/rad live on *players*.
  `Player` has no counter map; `Object.counters` is per-object. GAP (ties to 3.4).
- **Ability-granting counters** — `FlyingCounter`, `DeathtouchCounter`, etc. must
  feed the *layer 6* ability system, not just P/T. pawl's counter→layer-7c
  projection (M4f) does not generalize to granting keywords. GAP (small).
- **Loyalty / defense / lore counters** — bound to planeswalkers / battles / sagas
  (card types pawl doesn't model yet). VOCAB-gated on those card types.

→ **GAP-C:** player-counter substrate + counter→layer-6 path.

### 3.7 Durations — GAP (conditional & event-relative)

`Duration` has 2. mtgish `Expiration` (~45) folds to classes pawl cannot express:

- **Conditional / "as long as"** — `ForAsLongAsPermanentHasACounter…`,
  `UntilPermanentNoLongerPassesFilter` (CR 611.2 — verify). GAP.
- **Event-triggered expiry** — `UntilAPlayerCastsASpell`, `UntilPlayerPaysMana`,
  `UntilCardIsNoLongerExiled`. GAP.
- **Next-turn-relative** — `UntilPlayersNextTurn`, `UntilEndOfNextTurn`,
  `DuringPlayersNextTurn`. GAP (M4a's Mindslaver control is next-turn-scoped, so a
  seed exists).
- until-end-of-combat, until-end-step — VOCAB (fixed points in the existing
  phase sequence).

→ **GAP-D:** a richer expiry model (predicate-conditional + event-conditional).

### 3.8 Triggers — VOCAB mostly, GAP on delayed / state / per-turn

`TriggerCondition` has 1. mtgish `Trigger` (385) folds to ~50 event classes (ETB,
LTB, dies, attacks, blocks, deals/receives damage, casts, draws, discards,
sacrifices, phase-begin, …). **The bulk is VOCAB** — the trigger *engine*
(`TriggeredAbility`) exists; new conditions arrive with M4-tail cards. Structural
sub-gaps the fold exposes:

- **State-triggered abilities** — "whenever you have 0 cards", "when a player
  controls no permanents" (CR 603.8 — verify): a *continuous condition* check, not
  an event. GAP.
- **Delayed / reflexive triggers** — "when you do, …", "at the beginning of the
  next end step" (CR 603.7 — verify). GAP.
- **Per-turn event counting** — "for the first time each turn", "their Nth spell
  this turn": needs per-turn event counters in `GameState`. GAP (substrate).

→ **GAP-T:** state + delayed triggers + per-turn event counters. The condition
*catalogue* itself is VOCAB.

### 3.9 Targeting / predicates — GAP (enum won't scale)

`TargetSpec` is a closed enum, now 8 variants — and M4g's `WallTarget` (one new
constructor for a single card, Wall of Stone) is itself the tell: targeting grows
one hand-carved variant per card. mtgish's `Condition` (243) and its filter grammar
show targeting needs a **general predicate/filter language** (power ≥ N,
controlled-by, color, has-keyword, in-zone, …). pawl already has `CardCriterion`
(for `Search`); the gap is unifying target selection onto a criterion language
rather than growing `TargetSpec`.

→ **GAP-F:** a target-filter language (likely a `CardCriterion` generalization).
Structural, and upstream of a large fraction of M4-tail cards.

### 3.10 Costs — GAP (generalization + modification)

`AbilityCost` = mana + `[AdditionalCost]`, with `AdditionalCost` a closed 2-variant
enum. mtgish `Cost` (~200) folds to classes pawl can't express: pay-life,
pay-energy, sacrifice(other/N/type), discard, exile-from-zone, tap-others, reveal,
mill-self, return-to-hand, remove/add-counters. Plus two structural mechanisms:

- **Alternative costs** (`CastASpellFor…AlternateCost`, flashback, overload). GAP.
- **Cost reduction / increase** (huge in `PlayerEffect` §3.2). No cost-modification
  mechanism exists. GAP (overlaps GAP-P).

→ **GAP-Co:** generalize `AdditionalCost` to a pay-*X* list; add alternative-cost
and cost-modification seams.

## 4. Ranked GAP list — the actionable output

Ranked by machinery risk × leverage (design.md's own ordering philosophy), for
**your triage** — no roadmap edits made.

> **Superseded as a work list.** Every gap below now has a home in the M4.5
> phase table (umbrella spec §3), and each unlanded phase is a GitHub issue:
> GAP-R→#1 (P5), GAP-D→#2 (P6), GAP-P and GAP-Co's modification half→#3 (P7),
> GAP-Co's payment half→#4 (P8), GAP-F→#5 (P9), GAP-C and GAP-S→#6 (P10),
> GAP-Z→#7 (P11). Five landed: GAP-L2 as P1, GAP-L1 as P2, GAP-L5 as P3a,
> GAP-L7cda as P3b, GAP-T as P4. **This section is kept for its derivation and
> its reasoning about relative risk**, both of which the issue tracker cannot
> hold; for what to do next, read the issues, not this table.

| # | Gap | Why it ranks here |
|---|---|---|
| 1 | **GAP-P** — player-scoped continuous effects/restrictions | Largest unroadmapped surface; blocks control/prison/cost/skip cards; new axis, not a variant. |
| 2 | **GAP-R** — replacement seam *event coverage* | Engine exists; needs generalizing from 2 events to a would-*event* vocabulary. Touches damage, draw, life, ETB, tokens. |
| 3 | **GAP-L1** — copy (layer 1) | Whole CR section; §5's canary is the layer system; Clone + copy-spell + face-down all depend on it. |
| 4 | **GAP-F** — target-filter predicate language | Upstream of most M4-tail cards; prevents a `TargetSpec` explosion mirroring mtgish. |
| 5 | **GAP-T** — state/delayed triggers + per-turn counters | Substrate many triggers silently need; not just more conditions. |
| 6 | **GAP-D** — conditional/event durations | Blocks "as long as" and "until X" continuous effects. |
| 7 | **GAP-L2** — permanent control (layer 2) | Control Magic/Threaten; distinct from Mindslaver player-control. Deeper than a layer op: `Object` has no `controller` field yet. |
| 8 | **GAP-Co** — cost generalization + modification | Overlaps GAP-P (cost reduction); alternative costs are broad. |
| 9 | **GAP-S** — player-state subsystems (poison/energy first) | Each cheap; poison/energy force GAP-C and are competitively central. |
| 10 | **GAP-C** — player-counter substrate + counter→layer-6 | Enabler for GAP-S; small. |
| 11 | **GAP-Z** — Command zone + phasing/face-down status | Command small; phasing is its own mechanism. |
| 12 | **GAP-L5** / **GAP-L7cda** — color change, characteristic-defined P/T | Small, well-scoped layer ops. |

## 5. What is **not** a gap (guard against misreading)

- **The 1,317 `Action` variants** — open half; grows forever; never a gap (§1).
- **~150 counter *names*, ~50 trigger *conditions*, layer-4 type breadth,
  ~200 cost *instances*** — VOCAB; arrive with M4-tail cards on existing seams.
- **On the roadmap already:** numeric tower/X (M4a ✓), zone-verbs (M4b ✓), tokens
  (M4c ✓), prevention+regen *shapes* (M4d ✓), counter-spell (M4e ✓), +1/+1 counters
  (M4f ✓), modal (M4g ✓ — M4 complete); player-control/restart/subgames (M5); LLM
  transpiler (M6). M4g's fast-follow (modality on activated/triggered abilities)
  is VOCAB on the existing `Mode`/`Modal` payload, not a gap.
- **OOS (§6):** ante, dexterity, draft/Conspiracy (second VM), un-set social/
  art-content cards, Contraptions (Attractions substrate only if CR 717 is ever
  scoped).

## 6. Recommendations for triage (no edits made)

1. **Confirm the fold before acting** — the module-level claims are now verified
   (`Object` = owner/no-controller, `Affected` = object-only, `Quantity` =
   Literal/ManaValue/X, `Modification` = 8 ops in layers 3/4/6/7); what remains to
   spot-check is the *CR section numbers*, all marked *(verify)* above, against
   `docs/rules.txt`.
2. **The two that likely deserve roadmap entries now** (they are *axes*, not
   vocabulary, and everything downstream assumes them): **GAP-P** (player
   continuous effects) and **GAP-R** (replacement event coverage). Candidate: new
   M4 letters or an M4.5, in the same "gate card falsifies naive impl" style.
3. **GAP-L1 (copy)** probably wants to be called out beside §5's layer discussion —
   the layer system is the stated canary and copy (layer 1) is currently a blank.
4. **Leave GAP-S subsystems as a backlog** — each is a self-contained mini-section;
   poison + energy first (they also force GAP-C).
5. ~~When moving this to `docs/`, consider filing GAP-P / GAP-R / GAP-L1 as
   git-bugs so they're tracked, not lost in prose.~~ **Done, and superseded:**
   all three were filed, and every gap is now a GitHub issue against its M4.5
   phase — see the note under §4.
