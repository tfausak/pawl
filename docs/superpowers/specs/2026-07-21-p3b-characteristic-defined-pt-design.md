# M4.5 P3b — Characteristic-defined P/T, the freeze, and the switch (layer 7)

*Design pass 2026-07-21. The fourth phase of the M4.5 umbrella
(`docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`), closing
**GAP-L7cda** — an object's characteristic-defined power and toughness. The
umbrella scoped P3b to layer 7a alone; this pass **widens it to the rest of
layer 7** (7a, the CR 608.2h/611.2d freeze that 7b needs, and 7d P/T switching),
per the umbrella's own §7 and P3a's §8 hand-off, which already assigned 7d to
P3b's neighbourhood. Gate: **Tarmogoyf**. This spec is implementable; a
`writing-plans` plan follows it.*

*CR citations below **were checked against `docs/rules.txt` during this design
pass**: 107.3, 107.3a, 115.1a, 205.3k, 208.1, 208.2, 208.2a, 208.2b, 208.3,
208.4a, 208.4b, 208.5, 604.3, 604.3a, 608.2h, 611.2, 611.2a, 611.2c, 611.2d,
613.1g, 613.2, 613.2c, 613.3, 613.4, 613.4a, 613.4b, 613.4c, 613.4d, 613.6,
613.7, 707.2, 707.2a, 707.2b. Numbers marked **(verify)** were not, and must be
checked before they drive code (CLAUDE.md: never trust recalled Magic rules).
Card text was drawn from the vendored MTGJSON dump as a **candidate search only**
and **must be re-verified against Scryfall** before it lands in a card file
(`card-data-source`); step 1 of §7 is that task.*

## 0. Why this phase, and what it proves

`Layer.CharacteristicPT` (7a) and `Layer.SwitchPT` (7d) both sit in
`Pawl.Type.Layer` with **no producer**. `Quantity` has three arms — `Literal`,
`ManaValue`, `X` — and its own module comment names the missing ones and cites
this phase's gate card by name: *"Grows: Star (a characteristic-defining ability
— Tarmogoyf's power), Plus Quantity Quantity (Tarmogoyf's 1+\*)"*. design.md
§2.12 makes the same promise from the other end: *"Designing for Little Girl gets
you Tarmogoyf for free."* P3b is where the numeric tower's central claim is
cashed.

**The thesis, in one line:**

> A quantity that counts game state, and the two rules that say when it is
> re-read.

That pairing is the phase's sharpest falsifier, and it is why 7a and the 7b
gap-fill belong in one phase rather than two. The *same* counting quantity must
behave in opposite ways depending on where it lives:

| | CR | Gate card | Behaviour proved |
|---|---|---|---|
| **7a** | 613.4a / 604.3 / 208.2a | **Tarmogoyf** `*`/`1+*` | **recomputed** on every projection |
| **7b** | 608.2h / 611.2d | **Inner Calm, Outer Strength** `{1}{G}` | **frozen** once, at resolution |
| **7d** | 613.4d | **Twisted Image** `{U}` | applied **after** 7a/7b/7c |

**The decision it proves:** a characteristic-defining ability is an *ability*,
carried unevaluated through the projection and through a copy — never a number
computed once and stored. CR 707.2a says it outright: *"A copy acquires the
abilities of the object it's copying because those values are derived from its
rules text."* The naive implementation is *"evaluate the printed P/T at the
seed"*, which is what `Projection.baseCharacteristics` does today via
`Quantity.evaluate`, and which three separate things in this phase falsify: a
graveyard that changes after the creature entered, a Clone, and a switch.

### The live bug this phase also closes

`Pawl.Resolve`'s `ModifyTarget` arm carries this comment (`Resolve.hs:340`):

> *"The Modification's quantities are stored as-is (Literals); CR 611.2b's freeze
> is a no-op until X exists, at which point evaluate-and-freeze …"*

Two things are wrong with it. **CR 611.2b is the wrong rule** — 611.2b is the
*"for as long as"* duration rule; the freeze is **CR 611.2d** (variables such as
X) and, more generally, **CR 608.2h** (*"If an effect requires information from
the game … the answer is determined only once, when the effect is applied"*).
And **X has existed since M4a**, so the stated precondition has expired. It is
latent rather than live only because no card in the pool yet stores a non-`Literal`
quantity in a continuous effect.

There is a second half to the same bug: `Projection.applyModification` evaluates
a stored quantity against the **affected object**, so a stored `X` would read the
*target's* binding environment rather than the caster's, and a stored
"cards in your hand" would count the wrong player's hand. Both halves are fixed
by freezing at store time against the source (§3).

### Gate cards

Candidates found by searching the vendored MTGJSON dump; **all three must be
re-verified against Scryfall** before they are written to `data/cards/` (§7 step 1).

| Card | Text | What it gates |
|---|---|---|
| **Tarmogoyf** `{1}{G}` Creature — Lhurgoyf `*/1+*` | "Tarmogoyf's power is equal to the number of card types among cards in all graveyards and its toughness is equal to that number plus 1." | layer **7a**: a dynamic CDA, recomputed and copiable |
| **Inner Calm, Outer Strength** `{1}{G}` Instant — Arcane | "Target creature gets +X/+X until end of turn, where X is the number of cards in your hand." | layer **7b**: CR 608.2h freeze at resolution |
| **Twisted Image** `{U}` Instant | "Switch target artifact or creature's power and toughness until end of turn. Draw a card." | layer **7d**: the switch, applied last |

The three interlock deliberately. Tarmogoyf is the only **asymmetric** P/T source
in the pool (`N`/`N+1`), which is what makes it the ordering falsifier for
Twisted Image — a symmetric fixture (a `+1/+1` counter, Giant Growth) commutes
with the switch and proves nothing (§5, falsifier 5). And Tarmogoyf's count and
Inner Calm's count are the same `Quantity.Count` machinery pointed at the two
opposite re-read rules.

## 1. Scope

**In scope.** The rest of layer 7: characteristic-defined P/T as a projected,
copiable, recomputed characteristic (7a); the CR 608.2h/611.2d freeze that stored
continuous effects owe (the 7b gap-fill); and P/T switching (7d). Plus the
counting quantity both 7a and 7b need.

**Out of scope, deferred with named expiries** — §8. Notably CR 208.2b's
"enters as" P/T choice (a replacement-effect shape → **P5**), counts that read
*projected* state (needs quantity evaluation to move into `Pawl.Projection`), a
one-axis 7b set, and `Quantity.Half`/`Infinite`.

**Not in this milestone by construction.** The general filter/criterion language
that would retire `CountSpec` and `TargetSpec.ArtifactOrCreatureTarget` is
**P9**'s, unchanged. P3b builds the counting axis P9 will generalize; it does not
build P9.

## 2. Architecture

### 2.1 The numeric tower grows

`Pawl.Type.Quantity` gains three arms, and a new sibling module carries the
card-shaped growth:

```haskell
data Quantity
  = Literal Integer | ManaValue | X
  | Star                          -- CR 208.2: the printed *
  | Plus Quantity Quantity        -- CR 208.2: 1+*
  | Count CountSpec               -- what * (or "for each") counts

-- Pawl.Type.CountSpec
data CountSpec                    -- EXPIRES at P9
  = CardTypesInAllGraveyards      -- Tarmogoyf
  | CardsInYourHand               -- Inner Calm, Outer Strength
```

**Why `Count CountSpec` and not flat arms on `Quantity`.** `Quantity` is a small,
deliberately-closed numeric-tower type whose shape is argued in its own module
comment; letting it grow one card-shaped constructor per card muddies that, and
leaves P9 picking arms out of it rather than deleting one type. `CountSpec` is
the `WallTarget` / `NonblackCreatureTarget` posture — specific before general,
quarantined so **P9 retires the whole type at once**.

**Why not a structured `Count zone criterion` now.** P9 *owns* the criterion
language; building half of it here creates two filter languages to reconcile.
And it would not even cover the gate: Tarmogoyf counts distinct **card types**,
not objects, so a "count objects matching a criterion" shape needs a special arm
regardless.

`Pawl.Quantity.evaluate` gains a "you" `PlayerId` for player-scoped counts:

```haskell
evaluate :: GameState -> ObjectId -> Maybe PlayerId -> Quantity -> Maybe Integer
```

- `Plus a b` — `(+)` on both, `Nothing` if either is unevaluable.
- `Count CardTypesInAllGraveyards` — the cardinality of the union of
  `TypeLine.types` over every card in every graveyard. Reads **printed** types
  through `Game.cardOf`, never the projection: nothing projects a graveyard card
  today, and reading the projection here would recurse into the fold that calls
  it. Expiry in §8.
- `Count CardsInYourHand` — the size of `you`'s hand; `Nothing` when `you` is
  `Nothing`.
- `Star` — **`Nothing`**. `Star` is *notation*, not a value: it is resolved at the
  projection seed (§2.2) and never survives into a projection. The `Nothing` is
  the honest answer, not a hole.

Both counts read only zone membership and printed card types, so **quantity
evaluation stays in `Pawl.Quantity`**. A count over *projected* state ("lands you
control" — a Blood Moon'd land is still a land, a stolen land is not yours) would
need `Pawl.Projection`, which `Pawl.Quantity` cannot import without a cycle;
that is a named deferral (§8), not an oversight.

A new pure helper resolves the printed star:

```haskell
substituteStar :: Quantity -> Quantity -> Quantity   -- replace Star with the CDA
```

recursing through `Plus`. So Tarmogoyf's printed toughness `Plus (Literal 1) Star`
becomes `Plus (Literal 1) (Count CardTypesInAllGraveyards)`. This is exactly the
composition `Quantity`'s module comment promised: *"Plus is binary and recursive
so composition covers the awkward printed values without new cases: 1+\* is
Plus (Literal 1) Star."*

### 2.2 The CDA is a copiable characteristic

Two new fields, one on the card and one on the projection:

```haskell
Card.characteristicPT :: Maybe Quantity              -- CR 604.3: what * counts
PC.characteristicPT   :: Maybe (Quantity, Quantity)  -- the CDA, unevaluated
```

`Card.characteristicPT` is `Nothing` for every card in the pool today, and
**serializes only when `Just`** — so every existing `data/cards/*.json` stays
byte-identical, the posture P2's `copyOnEnter` and P3a's `colorIndicator` both
established. Tarmogoyf's is `Just (Count CardTypesInAllGraveyards)`, its printed
`power` is `MkPower Star` and its printed `toughness` is
`MkToughness (Plus (Literal 1) Star)` — the printed P/T box exactly as CR 208.1
and CR 208.2 describe it.

`Projection.baseCharacteristics` seeds the projected field by *substituting*, not
evaluating:

```
PC.characteristicPT = case Card.characteristicPT card of
  Nothing -> Nothing
  Just q  -> Just ( substituteStar q printedPower
                  , substituteStar q printedToughness )
```

so Tarmogoyf's seeded value is a pair of **quantities**, not numbers. Three
properties fall out of that one decision:

- **CR 707.2 for free.** `PC.characteristicPT` rides
  `Projection.copiableCharacteristics` (the layer-1 seed, CR 613.2c), so a Clone
  of Tarmogoyf snapshots the *ability* and keeps recomputing — CR 707.2a, *"a copy
  acquires the abilities of the object it's copying because those values are
  derived from its rules text"*, and CR 707.2b, *"changing the copiable values of
  the original object won't cause the copy to change"* (the ability is copied; the
  number is never a copiable value at all). **This pays the bill P2 deferred by
  name**: *"7b/CDA P/T-setting in copiable values (rides P3b, Tarmogoyf)"*.
- **Layer 6 can strip it.** `Modification.LoseAllAbilities` sets
  `PC.characteristicPT = Nothing`, at layer 6 — *before* 7a. See §5's honest
  negative result on observability.
- **`PC.power` seeds to `Nothing`** for a `*` card, because `evaluate Star` is
  `Nothing`. That is not a gap: it is literally CR 613.4a. No P/T value exists
  until layer 7a applies one.

### 2.3 Layer 7a folds in place, not through `gather`

`Projection.projectFrom`'s layer list always includes `Layer.CharacteristicPT`,
and when the fold reaches it, the object's **own** CDA is read from the *partial*
projection (post-layer-6) and applied before any gathered 7a effects:

```
n_power     = evaluate gs oid you (fst cda)
n_toughness = evaluate gs oid you (snd cda)
```

where `you` is `Projection.controllerOf oid` (P1) — the object's own controller,
since a CDA's "you" is always the object's controller (CR 604.3a(3): a CDA does
not affect other objects). No count in this phase's pool actually reads it —
Tarmogoyf's counts every graveyard — but the parameter must be threaded correctly
now rather than guessed at by the first card that needs it.

**Why in place, and not a synthetic `Gathered` the way `counterGathered` emits
layer-7c counters.** The counter precedent makes the wrong choice look obvious, so
the three reasons are recorded here and in the code comment:

1. **Humility.** `gather` runs *before* the fold and has no partial to read, so a
   pre-gathered CDA could never be stripped at layer 6.
2. **CR 604.3 — CDAs function in all zones**, and CR 208.2a repeats it for P/T
   specifically (*"This ability functions everywhere, even outside the game"*).
   `gather` is battlefield-only; `projectFrom` is not zone-scoped, so the in-place
   fold gets all-zones behaviour for free.
3. **A CDA is not a continuous effect from a source.** It has no source object and
   no timestamp, so it has nothing to sort on under CR 613.7 and does not belong
   in the candidate list at all.

This is precisely what P3a's spec §2.2 and the `Projection.baseColorsOf` code
comment instructed P3b to do, and for the reason they gave: devoid is a
**constant** CDA (a copy snapshot recomputes the same constant, so seeding it is
sound), while characteristic-defined P/T is a **dynamic** CDA — seeding it would
freeze the graveyards' contents into `Binding.copy` at entry, which is the CR
707.2 violation falsifier 2 catches.

**P3b therefore does not reopen P3a's CR 613.3 precedence question.** CR 613.3
scopes CDA-first ordering to layers **2–6**; layer 7's CDA sublayer is 7a, given
its own slot by CR 613.4a. This phase adds no CDA in layers 2–6, so P3a's
deferred `Gathered` precedence key stays deferred, unchanged.

### 2.4 Layer 7b — the CR 608.2h freeze

A new `Pawl.Projection.freezeQuantities` evaluates a `Modification`'s quantities
and rewrites each to a `Literal`, leaving any unevaluable one alone. It lives in
`Projection` because it pattern-matches `Modification`, which `Pawl.Projection`
is the sole home for — symmetric with the existing `rewriteModification` that
`Pawl.Resolve` already delegates to for CR 612 text-changing.

`Resolve`'s `ModifyTarget` arm calls it **at store time**, against the **source
spell and the source's controller**. That is what CR 608.2h requires (*determined
only once, when the effect is applied*) and CR 611.2d repeats for variables; it
also fixes the wrong-object half of the bug in §0, because the source — not the
target — is the object whose bindings hold `X` and whose controller owns the hand
being counted.

**Static abilities are never frozen.** CR 611.2 scopes the whole of 611.2a–611.2d
to *"a continuous effect generated by the resolution of a spell or ability"*. A
static ability's continuous effect (CR 604.2) is regenerated from the permanent
every projection, so Opalescence's `SetBasePowerToughness ManaValue ManaValue`
must keep recomputing per affected object. The freeze therefore lives on
`Resolve`'s store path only, never in `gather`, and the existing M3c
Humility+Opalescence gate is the regression guard (§5, falsifier 4).

CR 611.2c's affected-set lock is already implemented and unchanged; this is the
*value* half of the same "determined once" story.

### 2.5 Layer 7d — the switch

```haskell
| SwitchPowerToughness   -- layer 7d, CR 613.4d
```

`Projection.layer` maps it to `Layer.SwitchPT`; `applyModification` swaps
`PC.power` and `PC.toughness` outright — CR 613.4d: *"Such effects take the value
of power and apply it to the creature's toughness, and take the value of toughness
and apply it to the creature's power."* Two switches return the object to normal,
for free, because each is a separate application.

`TargetSpec.ArtifactOrCreatureTarget` (CR 115.1a) reads **projected** card types,
the `CreatureOrEnchantmentTarget` posture. Expiry → **P9**.

Twisted Image is `ModifyTarget UntilEndOfTurn SwitchPowerToughness slot` +
`Draw (Literal 1)` — **zero new opcodes**, all existing machinery (M3b, M4b).

### 2.6 Serialization

`Card.characteristicPT` is omitted when `Nothing`, so no existing card file
changes. `CountSpec`, the three new `Quantity` arms, `SwitchPowerToughness`,
`ArtifactOrCreatureTarget` and the two new `Subtype` arms all round-trip
through `Pawl.Codec`, and M3.5's honesty property
(`jsonToCard . cardToJson ≡ Right`) covers them the moment a card populates them.

## 3. The two invariants

1. **Classification, never identity.** `SwitchPowerToughness` is open-half
   vocabulary cased on solely by `Pawl.Projection`;
   `Count`/`CountSpec` is a classification evaluated by `Pawl.Quantity`;
   `ArtifactOrCreatureTarget` is a classification consulted by `Pawl.Target`.
   `freezeQuantities` pattern-matches `Modification` **inside `Pawl.Projection`**,
   preserving the sole-casing home, and `Resolve` delegates to it exactly as it
   already delegates `rewriteModification`. No module learns what card it is
   looking at.
2. **The engine makes no choices.** P3b introduces **no prompt and elides none**.
   A characteristic-defined P/T is derived; a count is derived; a switch is
   mandatory. (CR 107.3's *player-chosen* X is M4a's, already a prompt, and
   unchanged here.)

## 4. What this phase does **not** touch

- `Object`, `GameState`, and the event pipeline are unchanged. No new opcode, no
  new prompt, no new zone, no new SBA.
- CR 613.3's layer-2–6 CDA precedence key stays deferred (§2.3).
- git-bug `f90e0c4` (topological CR 613.8b applies-to reorder) is untouched: 7a
  applies at most one CDA per object with nothing to order against it, and 7d's
  switches order last-wins by timestamp with no same-layer dependency.
- git-bug `c7a0077` (`Quantity.Bound SlotName`) is **not** retired — no count in
  this phase needs a binding slot. The umbrella's §6 note relating it to P3b is
  answered in the negative, and §9 records that.

## 5. Cards and tests

Every gate is a **gameplay-level** scenario: cast through the stack, assert on
game state (design.md §4).

**Existing fixtures reused:** Lightning Bolt (an instant reaching a graveyard),
Blaze (a sorcery reaching a graveyard), Clone (P2), Humility + Opalescence (M3c),
Darksteel Myr (an artifact creature, for the switch's target spec), Battlegrowth
(a `+1/+1` counter), and the mono-colour deck pool.

**New card files:** `tarmogoyf.json`, `inner-calm-outer-strength.json`,
`twisted-image.json`. All three are **deterministic fixtures** in `allPrintings`
(for the round-trip) and in **no random-game deck** — the M3d/P1/P2/P3a posture,
so CR 400.7 conservation counts stay undisturbed.

### The falsifiers

Each kills one specific naive implementation.

| # | Naive implementation | Scenario that kills it |
|---|---|---|
| 1 | evaluate `*` once, at entry (what `baseCharacteristics` does today) | Graveyards empty → Tarmogoyf is **0/1**. A Lightning Bolt resolves and is put into its owner's graveyard → the Goyf is **1/2**, with no re-entry and no effect on it. |
| 2 | a copy snapshots the **number** | Clone enters copying Tarmogoyf while the graveyards hold two card types → both are **2/3**. A sorcery then resolves into a graveyard → **both** become 3/4. Snapshotting the value leaves the Clone at 2/3, violating CR 707.2a. |
| 3 | a stored continuous effect re-evaluates its quantity | Resolve Inner Calm, Outer Strength with N cards in hand → the target is **+N/+N**. Cast another spell from hand → the pump must **not** shrink (CR 608.2h). |
| 4 | the freeze is applied to static abilities too | Opalescence's `ManaValue` must keep recomputing per affected object — CR 611.2 scopes the freeze to resolution-created effects. The existing M3c gate is the regression guard. |
| 5 | 7d switches the **printed / base** box | Twisted Image on a 2/3 Tarmogoyf → **3/2**. Switching before 7a switches `Nothing`/`Nothing`, and the CDA then writes 2/3 back over it. A symmetric fixture proves nothing here — a `+1/+1` counter commutes with the switch — so Tarmogoyf's asymmetric `N`/`N+1` is the only asymmetric source in the pool and the two gate cards falsify each other. |
| 6 | the switch is permanent state rather than a layer op | Two Twisted Images on the same creature → back to normal (CR 613.4d, two applications). And a switched Tarmogoyf still tracks the graveyards: 3/2 becomes 4/3 when a fourth card type arrives. |
| 7 | a stored quantity is evaluated against the **target** | Inner Calm, Outer Strength counts the **caster's** hand, not the target's controller's. Cast it at an opponent's creature with differing hand sizes. |

### One honest negative result

Clearing `PC.characteristicPT` on `Modification.LoseAllAbilities` is required by
CR 604.3 (a CDA is a static ability, and Humility removes abilities) but is
**unobservable in the current card pool**: every `LoseAllAbilities` source —
Humility here, and every real "loses all abilities" card — *also* sets base P/T at
layer 7b, which overwrites layer 7a's result either way. A Humility'd Tarmogoyf is
1/1 under both the correct and the incorrect implementation.

The clearing is implemented anyway, because the CR says so and because the code
would otherwise be wrong for a reason no future reader could reconstruct. The test
is written anyway, because "Humility'd Tarmogoyf is 1/1" is a genuine ruling worth
transcribing. But it is **labeled in the test and the code comment as
non-distinguishing**, with the expiry named: **the first "loses all abilities"
card that does not also set power and toughness**.

### Rulings discipline (design.md §4)

When the plan lands each card, pull its Gatherer rulings and transcribe the
Q&A-shaped ones, recording the ruling's date in the test name. The likely yield:
Tarmogoyf's all-zones and "the number changes as cards are put into graveyards"
rulings; Twisted Image's counters-are-applied-before-the-switch ruling; Inner
Calm, Outer Strength's determined-on-resolution ruling.

## 6. Module & type changes (summary)

| Module | Change |
|---|---|
| `Pawl.Type.Quantity` | `+ Star`, `+ Plus Quantity Quantity`, `+ Count CountSpec` |
| `Pawl.Type.CountSpec` | **new module** — `CardTypesInAllGraveyards`, `CardsInYourHand` |
| `Pawl.Quantity` | the three new arms; `evaluate` gains the "you" `PlayerId`; `+ substituteStar` |
| `Pawl.Type.Card` | `+ characteristicPT :: Maybe Quantity` (serialized only when `Just`) |
| `Pawl.Type.ProjectedCharacteristics` | `+ characteristicPT :: Maybe (Quantity, Quantity)` |
| `Pawl.Type.Modification` | `+ SwitchPowerToughness` — layer 7d |
| `Pawl.Type.TargetSpec` | `+ ArtifactOrCreatureTarget` |
| `Pawl.Type.Subtype` | `+ Arcane` (CR 205.3k spell type, Inner Calm's type line); `+ Lhurgoyf` (CR 205.3m creature type, Tarmogoyf's) |
| `Pawl.Projection` | 7a in-place fold in `projectFrom`; seed of `PC.characteristicPT`; `LoseAllAbilities` clears it; `layer`/`applyModification` for `SwitchPowerToughness`; `+ freezeQuantities` |
| `Pawl.Resolve` | `ModifyTarget` freezes at store time against the source and its controller; the `611.2b` citation becomes `611.2d`/`608.2h` |
| `Pawl.Target` | `legalRecipients` / `isSelfExcluding` for `ArtifactOrCreatureTarget` |
| `Pawl.Codec` | the new `Quantity` arms, `CountSpec`, `characteristicPT`, `SwitchPowerToughness`, the target spec, `Arcane` |
| `data/cards/` | 3 new files; no existing file changes |

No new opcode. No new prompt. No change to `Object`, `GameState`, or the event
pipeline.

## 7. Ordering within the phase (for the plan)

Substrate before consumers, and each step's test written and watched to fail
first (CLAUDE.md: TDD is not optional).

1. **Pin all three cards against Scryfall** — text, mana cost, printed P/T, and
   type line. The MTGJSON candidates in this spec are a search result, not a
   source (`card-data-source`).
2. `Quantity.Star` / `Plus` / `Count` + `Pawl.Type.CountSpec` + `evaluate`'s "you"
   parameter + `substituteStar` + codec. Test: evaluation only, including
   `Star ⇒ Nothing` and both counts.
3. `Card.characteristicPT` + the `PC.characteristicPT` seed + codec. Test: the
   seed carries **quantities**, and a `*` card's `PC.power` is `Nothing`.
4. Layer 7a folded in place in `projectFrom` + `tarmogoyf.json` → **falsifier 1**,
   the first gameplay-level assertion.
5. `LoseAllAbilities` clears the CDA + the labeled non-distinguishing Humility
   test.
6. Clone of Tarmogoyf → **falsifier 2** (P2's deferred bill).
7. `Projection.freezeQuantities` + `Resolve`'s store-time freeze +
   `inner-calm-outer-strength.json` → **falsifiers 3, 4 and 7**.
8. `Modification.SwitchPowerToughness` + `TargetSpec.ArtifactOrCreatureTarget` +
   `twisted-image.json` → **falsifiers 5 and 6**.
9. Rulings transcription for all three cards, dated in the test names.
10. Umbrella §3/§4 update, `progress.md` entry, `CLAUDE.md` current-work tick.

## 8. Deferred, with named expiries

| Deferred | Expiry — what retires it |
|---|---|
| Power and toughness counting **different** things (one `Star` per card) | the first card whose two axes count different things |
| `LoseAllAbilities` clearing the CDA is **unobservable** (§5) | the first "loses all abilities" card that does not also set P/T |
| `Count` over **projected** state ("lands you control", "Elves on the battlefield") | **Strength of Cedars** / **Wirewood Pride** — either forces quantity evaluation to move into `Pawl.Projection` (a cycle from `Pawl.Quantity` today) |
| Counting **projected** card types of graveyard cards (§2.1 reads printed types) | the first effect that changes a graveyard card's types |
| `CountSpec` as a whole | **P9**'s criterion / filter language |
| `TargetSpec.ArtifactOrCreatureTarget` | **P9** |
| CR 208.2b "as this creature enters …" P/T choice (a replacement effect that sets copiable values) | **P5**, the replacement-engine phase |
| CR 208.5 "no value → 0" at the read points | the first creature with no P/T value that a reader observes |
| A one-axis 7b set ("its toughness becomes 4") — `SetBasePowerToughness` takes two `Quantity`s, not two `Maybe`s | the first such card |
| CR 208.4b "base power/toughness" readers | the first card that checks base P/T |
| A **dynamic** CDA defining colour or subtype (CR 604.3a(1)) | P3a seeded devoid as a *constant* CDA; the first dynamic one builds the general path |
| CR 613.3 CDA-vs-timestamp precedence within layers 2–6 | unchanged from P3a — this phase adds no layer-2–6 CDA |
| `Quantity.Half` (Little Girl) and `Infinite` (Mox Lotus) | design.md §6's silver-border canaries; still unbuilt |
| git-bug `c7a0077` (`Quantity.Bound SlotName`) | **not retired here** — no count in this phase needs a binding slot |
| Copying a permanent's **static** abilities (a Clone of Humility) | unchanged from P2 — a CDA is not a `StaticAbility` in this model, so this phase neither fixes nor worsens it |

## 9. Tracking

- **The umbrella changes.** §3's P3b row widens from "layer 7a" to "the rest of
  layer 7" — gates **Tarmogoyf** (7a), **Inner Calm, Outer Strength** (7b's CR
  608.2h freeze) and **Twisted Image** (7d) — and §4's ordering note follows. The
  umbrella §7 explicitly authorizes a phase spec that departs from the map to
  update the map; this is that. P3a's §8 had already assigned 7d here.
- **No git-bug is closed by this phase.** `c7a0077` (`Quantity.Bound`) was related
  to P3b by the umbrella §6 and is answered in the negative (§4): no count here
  needs a binding slot, so it stays open. `f90e0c4` is untouched (§4).
- **P2's deferred bill is paid**: *"7b/CDA P/T-setting in copiable values (rides
  P3b, Tarmogoyf)"* — falsifier 2. P2's *other* copy deferrals (static abilities,
  ongoing copy, copy-spell, copy-token, simultaneous entry) are unchanged.
- **After P3b, Cluster 1 (layer-system completion) is done**: layers 1 (P2), 2
  (P1), 3 (M3d), 4 (M3c), 5 (P3a), 6 (M3b) and 7a/7b/7c/7d all have producers.
  The umbrella's next phase is **P4** (event history + state/delayed triggers),
  which gates P6 and P7.

## 10. Exit criterion

Every sublayer of layer 7 has a producer. A `*` power/toughness is a projected
characteristic that recomputes on every projection, survives being copied as an
*ability* rather than a number (CR 707.2a), and can be stripped at layer 6 —
while a continuous effect created by a spell's resolution freezes its quantities
once, at resolution (CR 608.2h), and a static ability and a characteristic-defining
ability do not. Tarmogoyf, named in `Pawl.Type.Quantity`'s own comment and in
design.md §2.12 and §6 as the numeric tower's payoff, works.
