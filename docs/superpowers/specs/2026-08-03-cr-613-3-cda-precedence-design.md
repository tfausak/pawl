# CR 613.3 — characteristic-defining abilities before timestamp order

*Design pass 2026-08-03, closing #35. P3a
(`docs/superpowers/specs/2026-07-20-p3a-color-design.md` §2.2) applied devoid at
the projection **seed** rather than as a layer-5 characteristic-defining ability,
and argued the shortcut sound because every layer-5 effect in the vocabulary was
`SetColor`, which replaces. This pass adds the "in addition" effect that
falsifies that argument (CR 105.3), and moves devoid to where CR 613.3 puts it.
Gate: **Painter's Servant** naming blue against **Slaughter Drone**.*

*CR citations below **were checked against `docs/rules.txt` on 2026-08-03**:
105.1, 105.3, 109.5, 202.2, 204.2, 604.3, 604.3a, 613.1e, 613.3, 613.6, 613.7a,
613.7b, 613.7d, 613.8a, 614.1c, 702.114a, 707.5. Card text was verified against
Scryfall on 2026-08-03.*

## 0. Why this unit, and what it proves

`Projection.baseColorsOf` reads the printed `Devoid` keyword and returns the empty
set, so a devoid object is colourless from the first line of the fold. CR 613.3
instead says devoid applies **at the start of layer 5** — first within that layer,
before every non-CDA colour effect, and *after* layers 2, 3 and 4 have already read
the object's colours.

The P3a spec argued the two orderings observably identical, and listed the two
cases that would end the argument: an effect that interleaves a CDA with timestamp
order inside layers 2–6, and a below-layer-5 affected set keyed on colour. This
pass builds the first.

**The decision it proves:** a characteristic-defining ability is folded **in place**,
never gathered. CR 604.3a(3) says a CDA "does not directly affect the characteristics
of any other objects", so a CDA has no affected set to gather over, no source object
other than itself, and no timestamp to sort on. The `Gathered` precedence key that
P3a §2.2 imagined is therefore the wrong shape; `applyCharacteristicPT`'s existing
7a hook is the right one. Two of its three stated reasons transfer to colour — the
all-zones one (CR 604.3) and the nothing-to-sort-on one (CR 613.7). The third does
**not**: 7a folds in place partly so Humility can strip the CDA at layer 6 first,
and layer 6 comes *after* layer 5, so a Humility'd devoid object stays colourless.
CR 604.3a(3)'s "no affected set to gather over" takes its place at layer 5.

### Gate cards

Verified on Scryfall 2026-08-03.

| Card | Text | What it gates |
|---|---|---|
| **Painter's Servant** `{2}` Artifact Creature — Scarecrow 1/3 | "As this creature enters, choose a color. All cards that aren't on the battlefield, spells, and permanents are the chosen color in addition to their other colors." | the **interleave**: a layer-5 non-CDA effect whose timestamp is *older* than the devoid object |
| **Slaughter Drone** `{1}{B}` Creature — Eldrazi Drone 2/2 | "Devoid. {C}: This creature gains deathtouch until end of turn." | the CDA, on a real card; retires `synthetic-devoid-drone.json` |
| **Red Elemental Blast** `{R}` Instant | "Choose one — Counter target blue spell; or destroy target blue permanent." | the **reader**, on both a permanent and a spell |
| **Indigo Faerie** `{1}{U}` Creature — Faerie Wizard 1/1 | "Flying. {U}: Target permanent becomes blue in addition to its other colors until end of turn." | plain `AddColor`, with no chosen colour behind it |

**Why Painter's Servant and not Indigo Faerie alone.** Only a *static* effect can be
older than the object it colours. CR 613.7a gives Painter's continuous effect its
source permanent's timestamp, and CR 613.7d gives the drone a timestamp when it
enters — so Painter's is strictly earlier, and pure timestamp order would apply
devoid *second*, leaving the drone colourless. An activated ability's effect takes
its timestamp at resolution (CR 613.7b), which is necessarily after its target
exists, so Indigo Faerie can never produce that interleave. It earns its place as
the `AddColor` producer with no chosen colour behind it, not as the gate.

**Why blue and not black.** Slaughter Drone's mana cost is `{B}`. Naming blue keeps
the added colour distinct from the printed one, so a reader that answers "blue"
cannot be answering "black" by accident.

## 1. Scope

**In scope.** Devoid as an in-place layer-5 CDA; CR 105.3's "in addition" clause as
two `Modification` constructors; the as-enters colour choice behind Painter's
Servant; an affected set that reaches the stack; four cards; the synthetic drone's
retirement.

**Out of scope, deferred with named expiries** — §6. Notably a layer-6 *granted*
devoid, Painter's reach into hidden zones, and the chosen-*subtype* family.

## 2. Architecture

### 2.1 Devoid folds in place, at the start of layer 5

`projectWith`'s `applyLayer` already seeds layer 7a with the object's own
characteristic-defining P/T. Layer 5 gains the same shape:

```haskell
seeded = case lyr of
  Layer.CharacteristicPT -> applyCharacteristicPT lyr cands gs oid partial
  Layer.Color            -> applyColorDefining partial
  _                      -> partial
```

`applyColorDefining` clears `PC.colors` when the partial's keywords hold `Devoid`
(CR 702.114a: "Devoid means 'This object is colorless'"). `Layer.Color` joins
`Layer.CharacteristicPT` in the unconditional layer list, since a devoid object with
no layer-5 candidate must still reach the layer. For everything else that pass is a
`Map.member` over an empty candidate filter — the cost 7a already pays.

**Read from the partial, not from the card.** At layer 5 `PC.keywords` holds the
printed keywords and those a copy effect brought in — both arrive in the seed
(`Projection.copiableCharacteristics`, CR 613.1a's copiable value) — and it
structurally *cannot* hold a layer-6 grant, because layer 6 has not been
applied. Those are CR
604.3a(2)'s list of what makes a static ability characteristic-defining: "it is
printed on the card it affects, it was granted to the token it affects by the effect
that created the token, or it was acquired by the object it affects as the result of
a copy effect or text-changing effect" — minus the token clause, which pawl covers at
the seed, and minus the text-change clause, for which pawl has no keyword writer today
(`Layer.Text`'s one modification is `ChangeSubtypeWord`, which touches subtypes alone).
Reading the partial gets the rule right by construction rather than by a test.

Two consequences fall out, both wanted:

- **Layers 2–4 now read the printed colours.** A devoid object with `{B}` in its
  mana cost is black at layers 2, 3 and 4, which is #35's second bullet. §4's T4
  pins it.
- **Humility cannot remove it.** `LoseAllAbilities` is layer 6, after layer 5, so a
  Humility'd devoid object stays colourless — the same answer the seed gave, and
  the real ruling.

### 2.2 Colours off the battlefield

`baseColorsOf` splits in two, because the devoid test now has two callers that must
not drift:

- `printedColorsOf card` — the coloured symbols in the mana cost (CR 202.2) union
  the colour indicator (CR 204.2), with **no** devoid test. Seeds
  `baseCharacteristics`.
- `viewOfCard` keeps applying devoid on top of `printedColorsOf`. CR 604.3 makes a
  CDA function in all zones, and an object off the battlefield never enters the
  fold at all — `viewUpTo` falls back to the printed card there (#160). Without
  this, a devoid card in a graveyard would report its mana cost's colours.

One predicate decides what devoid means, applied once to `PC.keywords` inside the
fold and once to the card's keywords in `viewOfCard`.

### 2.3 Layer 5 gains "in addition"

```haskell
| AddColor (Set.Set Color.Color)   -- layer 5, CR 613.1e / 105.3
| AddChosenColor                   -- layer 5, the colour chosen for the SOURCE
```

CR 105.3: "If an effect gives an object a new color, the new color replaces all
previous colors the object had (unless the effect said the object became that color
'in addition' to its other colors)." `SetColor` is the first half of that sentence
and stays as it is; `AddColor` is the parenthetical, and unions.

`AddChosenColor` is nullary and reads the colour chosen for the effect's **source**.
Card data cannot name the value, so the modification derives it — the same posture
`SetControllerToSource` takes toward CR 109.5's "you", and two constructors for the
same reason that one is `SetController` and the other is not.

### 2.4 The chosen colour

Three additions, mirroring the `ChoiceOf` path Primal Plasma already walks:

- `Object.chosenColor :: Maybe Color.Color`, `Nothing` until an entry rewrite writes
  it.
- `EntryRewrite.ChooseColor`, nullary. CR 614.1c: "Effects that read … 'As [this
  permanent] enters …' … are replacement effects."
- `Prompt.ChooseColor`, carrying no candidate list. CR 105.1 fixes the five colours
  and no card narrows them. Five distinguishable options, so the prompt is always
  asked — never elided.

`Replacement.hs` gains a `ChooseColor` arm beside `ChoiceOf`: prompt the object's
controller, write the colour, consume the candidate.

**Not written into the copiable snapshot**, unlike `EntryOption`'s P/T and keywords.
CR 707.5's second sentence — "If the text that's being copied includes any abilities
that replace the enters-the-battlefield event (such as 'enters with' or 'as [this]
enters' abilities), those abilities will take effect" — means a Clone of Painter's
Servant runs the *copied* as-enters ability and makes its own choice. The chosen
colour is therefore not a copiable value. (`ProjectedCharacteristics` also has no
`staticAbilities` field, so there would be nothing to rewrite.)

### 2.5 An affected set that reaches the stack

`affects`'s `Matching` arm gates on `Set.member oid (GameState.battlefield gs)`.
That gate is load-bearing for every other card in the pool: without it Bad Moon's
"black creatures get +1/+1" would reach creature cards in graveyards. Painter's
Servant is the first card whose affected set is *not* battlefield-scoped — "all
cards that aren't on the battlefield, spells, and permanents".

A new arm, `Affected.MatchingAnywhere`, carries the same `Filter` without the gate.
Additive rather than a zone payload on `Matching`, which would touch every card in
the pool and every existing codec fixture for no gain.

The CR's set is every object in every zone; the fold reaches the battlefield and the
stack, which are the two zones where a projection exists (`viewOfObject`). That a
target filter already reads a projected view of a spell is not a new claim:
`Pool.Spells` and `Pool.SpellsAndPermanents` (Cancel, Magical Hack) match filters
against stack objects today. A card in
a hand, library, graveyard or exile still falls to `viewOfCard` and stays uncoloured
— §6 files it.

The new arm needs the same CR 613.8 treatment as `Matching` in `staticallyMovable`
and `movableReads`: movable exactly when its filter reads a projected aspect.

## 3. Cards

Four added, one deleted. Slaughter Drone is a cost-and-P/T drop-in for
`synthetic-devoid-drone.json` (`{1}{B}` 2/2), so the two specs that use the synthetic
move across unchanged in shape.

| File | Card | New machinery it needs |
|---|---|---|
| `painters-servant.json` | Painter's Servant | `EntryRewrite.ChooseColor`, `Affected.MatchingAnywhere`, `Modification.AddChosenColor` |
| `slaughter-drone.json` | Slaughter Drone | none — `{C}` in a cost already works (`Mana.hs` handles any `ManaSymbol.OfType`) |
| `red-elemental-blast.json` | Red Elemental Blast | none — modal spells, `Effect.Counter` (Cancel) and colour-filtered targets (Doom Blade) all exist |
| `indigo-faerie.json` | Indigo Faerie | `Modification.AddColor` |
| ~~`synthetic-devoid-drone.json`~~ | deleted | — |

## 4. Tests

In `Pawl.ColorSpec`, gameplay-level, plus one projection assertion.

| | Setup | Asserted | What a wrong ordering gives |
|---|---|---|---|
| **T1** *(the gate)* | Painter's Servant enters choosing blue; **then** Slaughter Drone resolves onto the battlefield | Red Elemental Blast's destroy mode kills it | timestamp order → colourless → no legal target |
| **T2** *(the stack)* | Painter's Servant on the battlefield naming blue; Slaughter Drone cast | Red Elemental Blast counters the **spell** | the spell is uncoloured → no legal target |
| **T3** *(plain add)* | Indigo Faerie's `{U}` targets a Slaughter Drone already on the battlefield | Red Elemental Blast destroys it; before the activation it has no legal target | — |
| **T4** *(layers 2–4)* | a Slaughter Drone on the battlefield | `projectUpTo Layer.Color` reports `{Black}`; the full projection reports `∅` | the seed reports `∅` at both |
| **T5** *(regression)* | the two specs that used the synthetic drone | unchanged behaviour on Slaughter Drone | — |

T1 is the one that discriminates CR 613.3 from CR 613.7: Painter's effect carries its
source's timestamp (CR 613.7a) and the drone's is minted on entry (CR 613.7d), so
Painter's is strictly earlier and timestamp order would apply devoid second.

T4 is a projection assertion rather than a gameplay one because no real card pairs a
below-layer-5 effect with a colour-keyed affected set — see §6.

## 5. Alternatives rejected

- **A precedence key on `Gathered`**, which P3a §2.2 named as the machinery this
  would need. A CDA affects only the object it is on (CR 604.3a(3)), so it has no
  affected set, no foreign source, and no timestamp; it does not belong in the
  candidate list at all. Two of `applyCharacteristicPT`'s three reasons for folding
  7a in place hold for layer 5 as written; the Humility one does not (§0), and CR
  604.3a(3) supplies the replacement.
- **Reading devoid from the card at layer 5.** The partial's keywords are already
  CR 604.3a(2)'s exact set, and cannot see a layer-6 grant. Reading the card would
  need a test to exclude what the structure excludes for free.
- **Leaving devoid at the seed.** `AddColor` falsifies the first bullet of P3a's
  argument — "every layer-5 effect in the vocabulary is `SetColor`, which replaces"
  — which is the bullet the whole elision rested on.
- **Writing the chosen colour into the copiable snapshot** (CR 707.5, §2.4).
- **A zone payload on `Affected.Matching`** rather than a new arm (§2.5).

## 6. Deferred

- **A layer-6 *granted* devoid does nothing to colour.** Per CR 604.3a(2) a granted
  devoid is not characteristic-defining, so it should be an ordinary layer-5 colour
  effect timestamped when granted (CR 613.7a) — today it is silently nothing. **No
  card grants devoid**: a Scryfall sweep on 2026-08-03 for cards whose text names
  devoid without having the keyword returns one card, Corrupted Crossroads, and its
  clause is a mana restriction. Building it is a capability no card exercises, so it
  stays filed. The `Keyword.hs` comment's `(#35)` moves to the new number.
  **Expiry: card-driven** — the first card that grants devoid.
- **Painter's Servant does not colour cards in hidden zones.** `viewUpTo` has no
  projection off the battlefield and falls back to the printed card (#160), so the
  "all cards that aren't on the battlefield" clause reaches nothing there. **Expiry:
  the off-battlefield projection sweep** (#160).
- **#35's second bullet has no card end-to-end.** A below-layer-5 effect with a
  colour-keyed affected set exists nowhere in Magic that a Scryfall sweep on
  2026-08-03 could find. This pass makes the answer correct and T4 pins it at the
  projection level; the gameplay-level proof waits on a card that may never print.
  Recorded in the PR body, not as an issue — a correct-but-unexercised path is not a
  deficiency.
- **`AddChosenColor` reads a colour and nothing else.** An as-enters choice of a
  *subtype* stays unexpressible (#608), and no chosen-characteristic family beyond
  colour is built.
