# M4.5 P3a — Color (layer 5)

*Design pass 2026-07-20. The third phase of the M4.5 umbrella
(`docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`), closing
**GAP-L5** — an object's color. The umbrella's P3 bundled two independent axes
(layer-5 color and layer-7a characteristic-defined P/T); this pass **splits it
into P3a (color, this spec) and P3b (CDA P/T)**, per the umbrella's own §7 —
every other phase is one axis plus the card that falsifies its naive
implementation, and P3 was two. Gate: **Doom Blade** against a colour that moves.
This spec is implementable; a `writing-plans` plan follows it.*

*CR citations below **were checked against `docs/rules.txt` during this design
pass**: 105.2, 105.2c, 105.3, 111.3, 202.2, 202.2b–f, 204.1, 204.2, 604.3,
604.3a, 608.2b, 613.1e, 613.2c, 613.3, 613.8a, 702.36b, 702.114a. Numbers marked
**(verify)** were not, and must be checked before they drive code (CLAUDE.md:
never trust recalled Magic rules). Card text was verified against Scryfall on
2026-07-20 — including one correction to this design's own first draft: **Crimson
Wisps has no untap clause**.*

## 0. Why this phase, and what it proves

`Layer.Color` (CR 613.1e, layer 5) sits in the layer enum with **no producer**,
and the gap is deeper than a missing `Modification` arm: **an object has no colour
at all today**. `Pawl.Type.Color` exists but only `ManaType` consumes it — a
*mana* colour, not an *object* colour. `ProjectedCharacteristics` has no `colors`
field, nothing derives colour from a mana cost (CR 202.2), and consequently
nothing in the engine can ask the question that roughly a fifth of Magic's removal
asks first.

**The decision it proves:** colour is a **projected characteristic**, folded like
every characteristic before it — `keywordsOf` (M2a), `controllerOf` (P1),
`copiableCharacteristics` (P2). Nothing may read colour off a printed card. The
naive implementation is *"an object's colours are the coloured symbols in its mana
cost"*, which CR 202.2 endorses as the **base** value and which three separate
things in this phase falsify as the **final** value: a colour-changing effect
(layer 5), devoid (a characteristic-defining ability, CR 702.114a), and a token
(which has no mana cost at all, CR 111.3).

This phase also cashes a debt the design doc has carried since M4: **design.md
§3's M4b table names Doom Blade as the zone-change gate**, and M4b had to
substitute Murder precisely because *"nonblack"* was not expressible. P3a makes
it expressible.

### Gate cards

All six verified on Scryfall 2026-07-20.

| Card | Text | What it gates |
|---|---|---|
| **Doom Blade** `{1}{B}` Instant | "Destroy target nonblack creature." | the **reader**: targeting legality over projected colour |
| **Crimson Wisps** `{R}` Instant | "Target creature becomes red and gains haste until end of turn. Draw a card." | layer 5, the **set** direction (a black creature stops being black) |
| **Aphotic Wisps** `{B}` Instant | "Target creature becomes black and gains fear until end of turn. Draw a card." | layer 5, the **mirror** direction; CR 608.2b; and `Fear` |
| **Bad Moon** `{1}{B}` Enchantment | "Black creatures get +1/+1." | the **in-fold reader**: a colour-restricted affected set |
| **Slaughter Drone** `{1}{B}` Creature — Eldrazi Drone 2/2 | "Devoid. `{C}`: This creature gains deathtouch until end of turn." | base colour: the cost says black, devoid says **colourless** |
| **Dragon Fodder** *(already in the pool)* | "Create two 1/1 red Goblin creature tokens." | token colour (CR 111.3) — a live correctness hole today |

## 1. Scope

**In scope.** An object's colour as a projected characteristic: base derivation
(CR 202.2 mana cost ∪ CR 204.2 colour indicator, CR 702.114a devoid), one layer-5
`Modification`, three readers (targeting, static affected set, blocking), and the
token-colour fix.

**Out of scope, deferred with named expiries** — §7. Notably `AddColor`
(CR 105.3's "in addition" clause), hybrid/Phyrexian symbols (CR 202.2d), layer 7d
P/T switching (P3b's neighbourhood, not colour's), and the general filter language
that retires two of this phase's three readers (P9).

**Not in this milestone by construction.** Protection (CR 702.16), the largest
colour reader in the game, stays where M2 put it: blocked on the Attach/Aura
subsystem and on CR 615 prevention breadth. P3a builds the colour axis protection
will later stand on; it does not build protection.

## 2. Architecture

### 2.1 Colour is a projected characteristic

`ProjectedCharacteristics` gains one field:

```haskell
colors :: Set Color
```

`Set Color`, not a `Colors` sum with a `Colorless` arm: **CR 105.2c — "a colorless
object has no color."** Colourless is the empty set, and "multicoloured" is
cardinality ≥ 2 (CR 105.2b). Encoding colourless as a constructor would create a
sixth pseudo-colour that CR 105.4 explicitly denies, and would make
`Set.member Black` — the only question any reader asks — the wrong shape.

`Pawl.Projection` gains `colorsOf :: ObjectId -> GameState -> Set Color`, the
sole read point, alongside `keywordsOf` / `subtypesOf` / `controllerOf`.

### 2.2 Base colours, and devoid at the seed

`baseCharacteristics` computes:

```
colors = (coloured symbols in the mana cost)      -- CR 202.2, 202.2b
       ∪ (the card's colour indicator)            -- CR 204.2, 202.2e
       , unless Devoid is printed on the card     -- CR 702.114a
         , in which case ∅
```

`ManaSymbol.OfType (ManaType.Colored c)` contributes `c`; `Generic`, `Variable`
and `OfType Colorless` contribute nothing (CR 202.2b: an object with no coloured
symbols is colourless). A land has no mana cost (CR 202.1, **verify**) and is
therefore colourless, which is already correct.

`Card` gains `colorIndicator :: Set Color` (CR 204.1/204.2). Empty for every
existing card.

**Devoid is applied at the seed, not as a layer-5 effect — and that is a
deliberate, argued equivalence, not an oversight.** CR 702.114a makes devoid a
*characteristic-defining ability*, and CR 613.3 says that within layers 2–6, CDAs
are applied **first**, then all other effects in timestamp order. pawl's fold
sorts on `(layer, timestamp)` only; honouring 613.3 literally would mean a
precedence key on `Gathered`. It is not needed, because for **every** case this
engine can reach the two orderings are observably indistinguishable:

- Every layer-5 effect in the vocabulary is `SetColor`, which **replaces** (CR
  105.3). "CDA first, then the replacers" and "CDA before layer 5, then the
  replacers" yield the same final set, always.
- **Copy (P2).** A Clone of a devoid creature snapshots the source's *copiable*
  values (CR 613.2c), which include the printed `Devoid` keyword — so the copy
  recomputes colourless from its own seed. Correct either way.
- **Humility.** `LoseAllAbilities` is layer 6, *after* layer 5, and CR 613.8a
  scopes dependency to effects in the same layer — so a Humility'd devoid creature
  stays colourless under both orderings, which is the real ruling.
- **CR 604.3**: CDAs function in all zones. The seed is computed from the card and
  is therefore zone-independent; a battlefield-only `gather` pass would not be.

This is the same posture `baseCharacteristics` already takes toward a printed `*`
P/T, which it evaluates at the seed rather than in layer 7a. **Named expiry:** the
first card requiring a genuine CDA-vs-timestamp interleave *within* layers 2–6
builds the `Gathered` precedence key, and P3b re-opens the identical question one
sublayer up. The code comment carries this argument with its CR numbers so the
next reader can check it (CLAUDE.md's rules-claim rule).

*(`observable-equivalence-is-the-bar`: internal structure may differ from the CR's
own decomposition where the difference is provably unobservable. The proof is the
four bullets above, not the assertion.)*

### 2.3 Layer 5: one `Modification`

```haskell
| SetColor (Set Color)   -- layer 5, CR 613.1e / 105.3
```

`Projection.layer` maps it to `Layer.Color`; `Projection.applyModification`
replaces `PC.colors` outright. **CR 105.3: "If an effect gives an object a new
color, the new color replaces all previous colors the object had."** So `SetColor`
is genuinely a set, and `SetColor Set.empty` is "becomes colorless" (Moonlace) for
free.

**No `AddColor`.** CR 105.3's parenthetical — "unless the effect said the object
became that color 'in addition' to its other colors" — has no card in the pool, and
adding an unexercised constructor breaks the no-constructor-without-a-card
discipline (design.md §4). Deferred, §7.

Crimson Wisps and Aphotic Wisps each need **zero new opcodes**: both are
`ModifyTarget UntilEndOfTurn (SetColor …)` + `ModifyTarget UntilEndOfTurn
(GainKeyword …)` + `Draw (Literal 1)`, all existing machinery (M3b, M4b).

### 2.4 Three readers

An axis with no consumer is the argument M2 used to *reject* protection. Colour
gets three, deliberately spanning three different closed-half subsystems:

**(a) Targeting — `TargetSpec.NonblackCreatureTarget`.** CR 115.1a; a battlefield
creature whose **projected** colours exclude `Black`. The `WallTarget` posture:
one hand-carved variant, specific-then-general, **expiry → P9's filter language**.
This is the sharpest observable available — target legality is binary, and it
re-checks at resolution (CR 608.2b), which M4e already built.

**(b) The fold itself — `Affected.CreaturesOfColor Color`.** Bad Moon. `affects`
already receives the **partial** projection accumulated through the previous
layers, so a layer-7c modification's affected set is evaluated against the
**layer-5 result** with no new machinery — a genuine cross-layer read that falls
out of the existing fold. **Expiry → P9.**

**(c) Combat — `Keyword.Fear`.** CR 702.36b: "A creature with fear can't be
blocked except by artifact creatures and/or black creatures." One arm on `Keyword`
(a rulebook citation — casing on it is not an invariant violation, per the M2a
spec §1) and one clause in `Combat.canBlock`, beside the existing flying/reach
pair. Unlike (a) and (b), **this reader does not expire** — it is permanent
closed-half machinery, and it reads projected colour *and* projected card type
(artifact) together. Fear is on design.md §3's own M2 punchlist ("indestructible,
intimidate, landwalk, lifelink — same axes, no new machinery"), so this is scoped
work being cashed, not scope creep.

### 2.5 Token colour (CR 111.3)

**CR 111.3: the effect that creates a token defines its characteristics, and those
values "are functionally equivalent to the characteristic values that are printed
on a card."** pawl models a token as a `Card` (M4c, `Source.OfToken Card`) with no
mana cost — so **Dragon Fodder's Goblins project as colourless today**, and its
oracle text says *red*. That is a live correctness bug, not a hypothetical.

`Card.colorIndicator` is exactly the right home: it is the field for "a colour
defined somewhere other than the mana cost" (CR 202.2e). Dragon Fodder's embedded
token card gets `colorIndicator = {Red}`; no new mechanism. The same field is what
CR 204's real no-mana-cost cards (Ancestral Vision and the suspend cycle) will use
when suspend exists — noted, not built (§7).

### 2.6 Serialization

`Card.colorIndicator` serializes **only when non-empty**, exactly as P2's
`copyOnEnter` serializes only when `True`. Every existing `data/cards/*.json` stays
byte-identical, and the M3.5 honesty round-trip (`jsonToCard . cardToJson ≡ Right`)
covers the new field the moment a card populates it.

## 3. The two invariants

1. **Classification, never identity.** `SetColor` is open-half vocabulary cased on
   solely by `Pawl.Projection` (its standing home for `Modification`);
   `NonblackCreatureTarget` and `CreaturesOfColor` are classifications consulted by
   `Pawl.Target` and `Pawl.Projection`; `Devoid` and `Fear` are CR 702 citations.
   No module learns what card it is looking at.
2. **The engine makes no choices.** P3a introduces **no prompt and elides none**.
   Colour is derived, never chosen. (CR 105.4's "choose a color" — Painter's
   Servant, Iona — is a genuine prompt and is deferred, §7.)

## 4. Cards and tests

Every gate is a **gameplay-level** scenario: cast through the stack, assert on
game state (design.md §4).

**Existing fixtures reused:** Typhoid Rats (`{B}` 1/1 deathtouch — black),
Darksteel Myr (artifact creature — the fear-legal blocker), Dragon Fodder,
Lightning Bolt, and the green/red creature pool.

**New card files:** `doom-blade.json`, `crimson-wisps.json`, `aphotic-wisps.json`,
`bad-moon.json`, `slaughter-drone.json`. All are **deterministic fixtures** in
`allPrintings` (for the round-trip) and in **no random-game deck** — the M3d/P1/P2
posture, so CR 400.7 conservation counts stay undisturbed.

**Why Slaughter Drone, and no synthetic.** A search of every devoid creature with
`{B}` in its cost (Scryfall, 26 results) turns up **no vanilla one** — devoid is an
Eldrazi mechanic and every printing carries a rider. Slaughter Drone is the
cheapest rider in the set: **one activated ability**, `{C}: This creature gains
deathtouch until end of turn`, and every piece of it already exists — activated
abilities (M3e), `GainKeyword` (M3b), deathtouch (M2c), and
`ManaType.Colorless` on `ManaSymbol`. Nothing in the pool *produces* `{C}`, so the
ability is faithful card data that is simply never activated — the same
"supported by doing nothing" posture design.md §6 records for Steamflogger Boss's
Contraption clause. **No synthetic crutch is needed for this phase.**

Scryfall's own data corroborates falsifier 1 before a line is written: Slaughter
Drone's `colors` is `[]` while its `color_identity` is `["B"]`.

### The falsifiers

Each kills one specific naive implementation:

| # | Naive implementation | Scenario that kills it |
|---|---|---|
| 1 | `colors = mana-cost symbols` | Slaughter Drone's cost contains `{B}`, but it is **colourless**: Doom Blade destroys it, and Bad Moon does **not** pump it (it stays 2/2). One card, two readers. |
| 2 | "becomes red" **adds** red | Typhoid Rats is 2/2 under Bad Moon and an illegal Doom Blade target. After Crimson Wisps it is a **1/1 red**: out of Bad Moon's affected set *and* a legal Doom Blade target. An `AddColor` implementation fails both assertions. |
| 3 | colour read from the printed card | Same scenario, either direction — the mirror is Aphotic Wisps putting a nonblack creature **into** Bad Moon's set and **out of** Doom Blade's legal set. |
| 4 | a token's colour comes from its mana cost | Dragon Fodder's Goblins are **red**. The load-bearing assertion is Bad Moon **not** pumping them (colourless would also read as nonblack, so the Doom Blade direction proves nothing here). |
| 5 | targets are checked only at cast | **CR 608.2b:** Doom Blade targets a green creature; in response, Aphotic Wisps makes it black; Doom Blade is removed from the stack and the creature lives. |
| 6 | fear reads the *printed* colour | A creature made black by Aphotic Wisps can block a fear attacker; Darksteel Myr blocks it as an **artifact**; a green or red creature cannot (CR 702.36b). |

Falsifier 5 is the reason Aphotic Wisps is in this phase rather than a synthetic
placeholder: it makes a creature **black**, which no other card in scope does, and
so is the only way to reach 608.2b through a colour change.

### Rulings discipline (design.md §4)

When the plan lands each card, pull its Gatherer rulings and transcribe the
Q&A-shaped ones, recording the ruling's date in the test name. Bad Moon's
dynamic-affected-set rulings and the devoid card's colour rulings are the likely
yield.

## 5. Module & type changes (summary)

| Module | Change |
|---|---|
| `Pawl.Type.ProjectedCharacteristics` | `+ colors :: Set Color` |
| `Pawl.Type.Card` | `+ colorIndicator :: Set Color` |
| `Pawl.Type.Modification` | `+ SetColor (Set Color)` — layer 5 |
| `Pawl.Type.Keyword` | `+ Devoid` (702.114), `+ Fear` (702.36) |
| `Pawl.Type.Subtype` | `+ Eldrazi`, `+ Drone` (Slaughter Drone's type line), `+ Goblin` if absent |
| `Pawl.Type.TargetSpec` | `+ NonblackCreatureTarget` |
| `Pawl.Type.Affected` | `+ CreaturesOfColor Color` |
| `Pawl.Projection` | base colours in `baseCharacteristics`; `layer`/`applyModification` for `SetColor`; `affects` for `CreaturesOfColor`; `+ colorsOf` |
| `Pawl.Combat` | `canBlock` gains the CR 702.36b fear clause |
| `Pawl.Target` | `legalRecipients` / `isSelfExcluding` for `NonblackCreatureTarget` |
| `Pawl.Codec` | `colorIndicator` (omitted when empty), `SetColor`, the two keywords, the target spec, the affected set |
| `data/cards/` | 5 new files; `dragon-fodder.json`'s token gains `colorIndicator` |

No new opcode. No new prompt. No change to `Object`, `GameState`, or the event
pipeline.

## 6. Ordering within the phase (for the plan)

Substrate before consumers, and each step's test written and watched to fail first
(CLAUDE.md: TDD is not optional).

1. Pin all six cards' oracle text against Scryfall (the Crimson Wisps correction
   in this spec's header is why this is a task and not an assumption).
2. `PC.colors` + `Card.colorIndicator` + base derivation (mana cost ∪ indicator) +
   `colorsOf` + codec. Test: printed colours of the existing pool.
3. `Keyword.Devoid` at the seed, with the §2.2 equivalence argument in the comment.
4. `Modification.SetColor` + layer 5 in the fold. Test: the projection only.
5. Reader (b): `Affected.CreaturesOfColor` + Bad Moon. The first gameplay-level
   colour assertion, and the only reader that needs no new target spec.
6. Dragon Fodder's token colour → falsifier 4 (needs step 5's reader to be
   observable).
7. Reader (a): `TargetSpec.NonblackCreatureTarget` + Doom Blade + Slaughter Drone
   → falsifier 1.
8. Crimson Wisps → falsifiers 2 and 3.
9. Aphotic Wisps + `Keyword.Fear` → falsifiers 3 (mirror), 5 and 6.
10. Umbrella §3 update (P3 → P3a/P3b), `progress.md` entry, `CLAUDE.md`
    current-work tick.

## 7. Deferred, with named expiries

| Deferred | Expiry — what retires it |
|---|---|
| `AddColor` (CR 105.3 "in addition to its other colors") | the first card with that wording |
| Hybrid / Phyrexian mana symbols (CR 202.2d) | `ManaSymbol` has no hybrid arm; the first hybrid-cost card |
| CR 613.3 CDA-vs-timestamp precedence within layers 2–6 | the first card needing a genuine interleave; **P3b re-opens the same question at layer 7a** |
| Layer 7d P/T switching (Twisted Image) | **P3b** — it is a P/T op, not a colour one |
| Colour of an object **outside** the battlefield read by an effect | **P9** (a graveyard/exile filter that names a colour) |
| The general filter language retiring `NonblackCreatureTarget` and `CreaturesOfColor` | **P9** |
| Colour indicator on real no-mana-cost cards (Ancestral Vision et al.) | the field exists; the cards need **suspend** |
| Devoid acquired by copy or text-change (CR 604.3a(2)) | the first card that grants a CDA that way |
| "Choose a color" as a prompt (CR 105.4 — Painter's Servant, Iona) | the first colour-choosing card |
| Protection (CR 702.16), the largest colour reader | unchanged from M2: the Attach/Aura subsystem plus CR 615 breadth |
| Colour as a copiable value under a *layer-5* CDA other than devoid | the first such card |

## 8. Tracking

- **The umbrella changes.** §3's table splits **P3 → P3a (colour, this spec) /
  P3b (characteristic-defined P/T)**, and §4's ordering note follows. The umbrella
  §7 explicitly authorizes a phase spec that departs from the map to update the
  map; this is that.
- **P3b inherits** two things named here: the CR 613.3 precedence question (one
  sublayer up, at 7a) and layer 7d switching.
- **No git-bug is closed by this phase.** `f90e0c4` (topological CR 613.8b
  applies-to reorder) is untouched — every layer-5 effect here replaces, so
  within-layer ordering is last-wins by timestamp and no same-layer dependency
  arises. `c7a0077` (`Quantity.Bound`) belongs to P3b.

## 9. Exit criterion

An object's colour is a projected characteristic with a base derivation (mana cost,
colour indicator, devoid), a layer-5 operation, and three readers spanning
targeting, the layer fold, and combat — each proved by a gameplay-level scenario in
which a naive implementation fails. Dragon Fodder's Goblins are red. Doom Blade,
named as a gate in design.md §3's M4b table and unusable since, works.
