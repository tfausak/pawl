# M4.5 P9 — Target-filter predicate language

*Design pass 2026-07-22. The tenth phase of the M4.5 umbrella
(`docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`), closing
**GAP-F** — the target-filter criterion language the census flagged as the
`TargetSpec` explosion that mirrors mtgish. Gates: **Doom Blade**, **Terror**
and **Reprisal**. This spec is implementable; a `writing-plans` plan follows
it.*

*This phase is **atom-first**: it builds the object-predicate language and its
one generic evaluator, and retires the `TargetSpec` family (#40), merges
`CardCriterion` / `PermanentCriterion` / `SpellCriterion`, and re-expresses
`Affected`'s dynamic sets. It **does not** fold in `StateCondition` (#38) or
`CountSpec` (#39): those add a second concept — a scope, an aggregation, and a
comparison to a threshold — that is deferred to a later phase. See §7.*

*CR citations below **were checked against `docs/rules.txt` during this design
pass**: 105.2, 105.2c, 109.2, 109.5, 110.1, 110.4, 112.1, 115.1a, 115.4, 205.3,
205.4, 205.4c, 208.1, 601.2c, 608.2b, 611.2c, 613.1b, 700.2c, 701.23a. Any number added later
and marked **(verify)** must be checked before it drives code (CLAUDE.md: never
trust recalled Magic rules).*

*Card text for all three gate cards **was verified live against the Scryfall API
during this design pass** (2026-07-22), not read from the vendored MTGJSON dump
(`card-data-source`):*

- *Doom Blade `{1}{B}` — "Destroy target nonblack creature."*
- *Terror `{1}{B}` — "Destroy target nonartifact, nonblack creature. It can't be
  regenerated."*
- *Reprisal `{1}{W}` — "Destroy target creature with power 4 or greater. It can't
  be regenerated."*

## 0. Why this phase, and what it proves

Pawl has grown a whole **family of parallel classification-as-data types**, each
a hand-carved enum that gains one variant per card:

| Type | Subject | Inhabitants today |
|---|---|---|
| `Pawl.Type.TargetSpec` | a target recipient | 11 variants — `NonblackCreatureTarget`, `WallTarget`, `ArtifactTarget`, `OpponentCreatureTarget`, … |
| `Pawl.Type.CardCriterion` | a card off the battlefield | `BasicLandCard` |
| `Pawl.Type.PermanentCriterion` | a battlefield permanent | `AnyPermanent`, `CreaturePermanent`, `PermanentOfSubtype` |
| `Pawl.Type.SpellCriterion` | a spell on the stack | `NoncreatureSpell`, `SpellOfColor` |
| `Pawl.Type.Affected` (part) | a continuous effect's set | `AllCreatures`, `AllLands`, `AllNonbasicLands`, `CreaturesOfColor` |

Every one of these is "a first-order, analyzable predicate over an object,
expressed as data, evaluated through a generic matcher — never by casing on the
object's *identity*." They differ only in **what object** they range over and
**where that object's characteristics come from**. `TargetSpec` proves the
failure mode: it has grown eleven variants, several of them one-off narrowings
(`WallTarget`, `NonblackCreatureTarget`, `ArtifactTarget`), each carrying its own
`Target.legalRecipients` arm. This is the mtgish `TargetSpec` explosion the M4.5
census warns about.

**What P9 establishes.** A single object-predicate language — `Pawl.Type.Filter`
— as first-order, statically-analyzable **data**, with one generic evaluator, and
the demonstration that it subsumes the whole family across every subject shape:
projected battlefield permanents (targeting, `PermanentCriterion`), projected
stack objects (`SpellCriterion`), and printed cards off the battlefield
(`CardCriterion` search). The gate is CR 115.1a targets whose legal set is a
*filtered* narrowing — "nonblack creature", "nonartifact, nonblack creature",
"creature with power 4 or greater" — replacing three hand-carved `TargetSpec`
arms with one data value each.

**The invariant.** `Filter` is closed-half **vocabulary**, not effect identity.
Its atoms case on *characteristics* — card type, colour, subtype, power,
controller — exactly as `Target.legalRecipients` already does
(`Set.member CardType.Creature …`). Casing on a characteristic classification is
the same legitimate act as casing on a `CardType` or a keyword (CLAUDE.md); the
invariant forbids casing on an *effect's* identity, which `Filter` never does —
it is evaluated by one generic matcher that never learns which spell produced it.

## 1. The core type: `Pawl.Type.Filter`

A first-order predicate over one object, as data, recursive-but-finite:

```haskell
module Pawl.Type.Filter where

data Filter
  = HasCardType CardType          -- CR 205 / 300: the object's card types include this one
  | HasSupertype Supertype        -- CR 205.4: the object's supertypes include this one (basic-land search)
  | HasColor Color                -- CR 105.2: the object's colours include this one
  | HasSubtype Subtype            -- CR 205.3: the object's subtypes include this one
  | PowerAtLeast Integer          -- CR 208.1: the object's power is >= this literal
  | ControlledBy PlayerRelation   -- CR 109.5: controller relates thus to the perspective
  | And [Filter]
  | Or [Filter]
  | Not Filter
  deriving (Eq, Ord, Show)
```

with `PlayerRelation` its own module:

```haskell
module Pawl.Type.PlayerRelation where

-- Who an object's controller is, relative to the perspective the evaluation
-- carries (the source's controller when targeting; the effect's controller for
-- a continuous effect). CR 109.5 fixes "you" as the object's controller and, by
-- negation, "an opponent" as any other player still in the game.
data PlayerRelation = You | Opponent
  deriving (Eq, Ord, Show)
```

**Flat, not layered.** Atoms and the `And`/`Or`/`Not` combinators are sibling
arms of one type. This mirrors `Pawl.Type.Quantity`, the project's standing
precedent for "leaves plus a recursive combinator in one flat type": its
recursive `Plus Quantity Quantity` sits alongside the leaf `Literal` / `ManaValue`
/ `Count` arms, with no `SimpleQuantity` split. A split into
`data Filter = Simple SimpleFilter | And … | Or … | Not …` would buy an
enforceable normal form only if it *also* restricted the recursion (CNF/DNF); an
unrestricted split guarantees nothing the flat type does not, while adding a
wrapper constructor, a second one-type-per-module file, and a second codec — for
no consumer that ever wants "atoms but not combinators." The type's doc comment
cites the `Quantity.Plus` precedent so the next reader sees why it is flat.

**`And []` is the trivial predicate** — the identity that matches everything — so
"target creature" (no narrowing) needs no separate "always" constructor.

**`PowerAtLeast` takes a literal `Integer`, not a `Quantity`.** A
`Quantity`-valued comparison ("power greater than the number of cards in your
hand") belongs to the count/compare concept deferred with `StateCondition` /
`CountSpec` (§7). The gate cards compare to printed literals only.

**`Not`, `And`, `Or` compose the gates.** Doom Blade's "nonblack creature" is
`And [HasCardType Creature, Not (HasColor Black)]`; Terror's "nonartifact,
nonblack creature" is `And [HasCardType Creature, Not (HasColor Black), Not
(HasCardType Artifact)]`; a printed "creature or enchantment" (Angelic Edict)
is `Or [HasCardType Creature, HasCardType Enchantment]`.

## 2. The characteristics *view* — one filter, three subject shapes

The family ranges over two kinds of subject that read characteristics from
different places:

- **Projected objects** — battlefield permanents (targeting today via
  `Projection.colorsOf` / `cardTypesOf`; `PermanentCriterion`) and stack objects
  (`SpellCriterion`). Their characteristics are the **projection** (CR 613 layer
  system), so a colour-changer, Blood Moon, or a type-changing effect is seen.
- **Printed cards** — a card in a library, graveyard, or hand being matched by a
  search (`CardCriterion.BasicLandCard`, matched today against a `Card` in
  `Resolve.matchesCriterion`). It has **no projection**; its characteristics are
  the ones printed on the card.

`Filter` evaluation therefore reads through a small **characteristics view** — the
accessors an atom needs — supplied by whichever source fits the subject's zone:

```haskell
-- Pawl.Filter (evaluator module; logic, not a type module)
-- The accessors Filter atoms consult. Supplied by the projection on the
-- battlefield/stack, and by the printed card off the battlefield. `power` and
-- `controller` are Nothing off the battlefield (a card in a library has neither
-- under the rules that matter here), so PowerAtLeast / ControlledBy are
-- vacuously False there — which no search filter uses.
data View = MkView
  { cardTypes :: Set CardType
  , supertypes :: Set Supertype
  , colors :: Set Color
  , subtypes :: Set Subtype
  , power :: Maybe Integer
  , controller :: Maybe PlayerId
  }
```

Two builders: one from the projection (`viewOfObject :: ObjectId -> GameState ->
View`, reusing `Projection.project`), one from a printed card
(`viewOfCard :: Card -> View`). The evaluator is a single fold over the `Filter`
tree against a `View` plus a **context** (§3):

```haskell
matches :: Context -> View -> Filter -> Bool
```

**This is the component that closes #5's "reading an object's colour outside the
battlefield."** Off-battlefield colour resolves to the *printed* card's colour
via `viewOfCard`, never a projection that does not exist off the battlefield. The
distinction is carried by *which builder the caller picks*, so no atom needs a
zone flag.

**It also carries the partial-vs-full projection distinction for free.** A
continuous effect's affected-set filter (§4, e.g. Bad Moon's `CreaturesOfColor`)
is evaluated mid-layer-fold against the **partial** projection accumulated so
far (CR 613: layers apply in order); a target's filter is evaluated against the
**full** projection. The caller supplies whichever `View` is appropriate; the
`matches` fold is agnostic. The known-untested pairing of a colour-reading
affected-set with a sub-layer-5 modification (`Projection.baseColorsOf`'s devoid
seed, #35) is neither fixed nor worsened here — the behaviour is preserved
exactly, still routed through the same partial view.

## 3. The evaluation context — perspective and source

Two atoms are not self-contained. `ControlledBy` needs a **perspective player**
(who counts as "you"): for a target it is the targeting source's controller (CR
109.5); for a continuous effect's set it is the effect's controller. Self-exclusion
("another") needs the **source object**. Both travel in a `Context`:

```haskell
data Context = MkContext
  { perspective :: Maybe PlayerId   -- who "you" is; Nothing when no player frames the match
  , source :: Maybe ObjectId        -- the object the match is relative to (for "another")
  }
```

`ControlledBy You` holds iff `controller view == perspective`; `ControlledBy
Opponent` iff the view has a controller, the context has a perspective, and they
differ (CR 613.1b projected controller; a source that has left the battlefield
has no projected controller, preserving `OpponentCreatureTarget`'s current empty-set
behaviour and its open #85). A search match (`CardCriterion`) passes a context
with no perspective and no source; its filters never reference either.

Self-exclusion is **not** a `Filter` atom. It stays where it is —
`Target.selfExcludes` / `legalSetsExcluding` — because "another" is a property of
a *target slot* (CR 601.2c chooses the slot's targets), not of the object
predicate. §4's `Affected` "each other" is handled the same way, at fold time by
the effect's source, exactly as today.

## 4. What each type becomes

### 4a. `TargetSpec` → pool + filter (closes #40)

`TargetSpec` splits into a *closed* pool classification and an *open* filter:

```haskell
module Pawl.Type.TargetSpec where

-- What a target slot may hold: a closed pool of candidate recipients, narrowed
-- by an open Filter (Nothing = the whole pool, e.g. bare "target creature").
data TargetSpec = MkTargetSpec Pool (Maybe Filter)
  deriving (Eq, Ord, Show)

-- CR 115: the closed set of recipient kinds, fixing both WHICH objects are
-- candidates and HOW they are referenced (Recipient.ToCreature vs ToPlayer vs
-- ToObject). Closed-half vocabulary, like the old TargetSpec enum.
data Pool
  = Creatures            -- CR 115.1a: creatures on the battlefield (ToCreature)
  | Players              -- CR 115: players still in the game (ToPlayer)
  | AnyTarget            -- CR 115.4: creatures + players (planeswalkers/battles absent)
  | Permanents           -- CR 110.1: permanents on the battlefield (ToObject)
  | Spells               -- CR 112.1: spells on the stack (ToObject)
  | SpellsAndPermanents  -- CR 115: stack objects + battlefield permanents (ToObject)
  deriving (Eq, Ord, Show)
```

`Target.legalRecipients` becomes: build the pool's base recipient set (the closed
part — a handful of arms over zones, tagging `ToCreature`/`ToPlayer`/`ToObject`),
then keep the members whose `View` satisfies the `Maybe Filter` under the context
`MkContext (Projection.controllerOf source gs) (Just source)`. Every existing
variant maps mechanically:

| Old `TargetSpec` | New value |
|---|---|
| `AnyTarget` | `MkTargetSpec AnyTarget Nothing` |
| `CreatureTarget` | `MkTargetSpec Creatures Nothing` |
| `PlayerTarget` | `MkTargetSpec Players Nothing` |
| `SpellOrPermanentTarget` | `MkTargetSpec SpellsAndPermanents Nothing` |
| `SpellTarget` | `MkTargetSpec Spells Nothing` |
| `LandTarget` | `MkTargetSpec Permanents (Just (HasCardType Land))` |
| `ArtifactTarget` | `MkTargetSpec Permanents (Just (HasCardType Artifact))` |
| `CreatureOrEnchantmentTarget` | `MkTargetSpec Permanents (Just (Or [HasCardType Creature, HasCardType Enchantment]))` |
| `NonlandPermanentTarget` | `MkTargetSpec Permanents (Just (Not (HasCardType Land)))` + self-exclude |
| `WallTarget` | `MkTargetSpec Creatures (Just (HasSubtype Wall))` |
| `NonblackCreatureTarget` | `MkTargetSpec Creatures (Just (Not (HasColor Black)))` |
| `OpponentCreatureTarget` | `MkTargetSpec Creatures (Just (ControlledBy Opponent))` |

`NonlandPermanentTarget`'s "another" self-exclusion is unchanged — `selfExcludes`
now reads the pool/slot, not a named variant. This **closes #40**.

### 4b. The three criterions → `Filter`

- `CardCriterion.BasicLandCard` → a `Filter` over the **printed-card** view.
  "Basic land card" is `And [HasCardType Land, HasSupertype Basic]` (CR 205.4c /
  701.23a), the one new axis basic-land search needs. `Effect.Search` carries a `Filter`;
  `Resolve.matchesCriterion` becomes `Filter.matches` over `viewOfCard`.
- `PermanentCriterion` (`AnyPermanent` / `CreaturePermanent` /
  `PermanentOfSubtype`) → `And []` / `HasCardType Creature` / `HasSubtype s` over
  the **projected** view. `Cost.matchesCriterion` and `Replacement`'s pattern
  match become `Filter.matches` over `viewOfObject`.
- `SpellCriterion` (`NoncreatureSpell` / `SpellOfColor`) → `Not (HasCardType
  Creature)` / `HasColor c` over the **projected** view.
  `PlayerEffect.matchesSpell` becomes `Filter.matches` over `viewOfObject`.

All three types are deleted; their consumers hold a `Filter`. (`CardCriterion`
and `PermanentCriterion` carry no elision issue — they are the base being
generalized, not something elided. `SpellCriterion`'s own doc comment already
anticipates this merge.)

### 4c. `Affected` dynamic sets → `Matching Filter`

`Affected`'s dynamic, re-derived-every-projection sets collapse to one filtered
arm; the locked id-set arm is untouched:

```haskell
data Affected
  = TheseObjects (Set ObjectId)   -- CR 611.2c: locked at begin — NOT a filter, kept
  | Matching Filter               -- dynamic: any object matching, re-derived each projection
```

`AllCreatures` → `Matching (HasCardType Creature)`; `AllLands` → `Matching
(HasCardType Land)`; `AllNonbasicLands` → `Matching (And [HasCardType Land, Not
(HasSupertype Basic)])`; `CreaturesOfColor c` → `Matching (And [HasCardType Creature,
HasColor c])`; `OtherNonAuraEnchantments` → `Matching (And [HasCardType
Enchantment, Not (HasSubtype Aura)])` with the "each other" self-exclusion applied
by the effect's source at fold time, as today. Evaluated against the **partial**
view per §2.

## 5. Gate cards and tests

Real, recognizable cards (`tests-prefer-real-cards`); each proves one axis of the
filter language across a distinct subject shape:

- **Doom Blade** `{1}{B}` — "Destroy target nonblack creature." Target =
  `MkTargetSpec Creatures (Just (Not (HasColor Black)))`. Proves `Not` + `HasColor`
  over a projected creature, and re-expresses the retired `NonblackCreatureTarget`
  (a black-made creature is not a legal target; a devoid creature with `{B}` in
  its cost is). This is the direct #40 gate.
- **Terror** `{1}{B}` — "Destroy target nonartifact, nonblack creature." Target =
  `MkTargetSpec Creatures (Just (And [Not (HasColor Black), Not (HasCardType
  Artifact)]))`. Proves `And` of two negated atoms across the colour and card-type
  axes.
- **Reprisal** `{1}{W}` — "Destroy target creature with power 4 or greater."
  Target = `MkTargetSpec Creatures (Just (PowerAtLeast 4))`. Proves the
  projected-power comparison; a creature pumped to power ≥ 4 becomes legal and one
  shrunk below 4 stops being legal, read from the projection.

Plus two non-target tests proving the merge holds across the other subject shapes:

- **A basic-land search** (`Effect.Search` with a `Filter` over `viewOfCard`) —
  proves the printed-card view and the basic-supertype atom, and that
  off-battlefield matching reads printed characteristics, not a projection (#5).
- **A `PermanentCriterion` consumer** already in the pool (Fireblast's Sacrifice-a-
  Mountain cost, P8; or a replacement pattern) — proves `Filter.matches` over the
  projected view at a cost/replacement site, so the merge is exercised outside
  targeting.

Each gate is one small commit: the `Filter` type, the `View` + evaluator, the
`TargetSpec` split, then the per-consumer migrations, then the three gate cards,
TDD throughout (each failing test run and watched fail before implementing).

## 6. "Can't be regenerated" is out of scope

Terror and Reprisal both end "It can't be regenerated." Regeneration is not
modeled (there is no regeneration shield to suppress), so that clause is a no-op
today and is **not** part of P9. It is a regeneration-replacement concern for a
later card-driven phase; the gate cards exercise only their *targeting*, and their
card data omits the unmodeled clause with an issue-cited note (a `regeneration`
elision issue is filed and cited at the card site, per CLAUDE.md's file-the-issue
rule).

## 7. Out of scope — the deferred count/compare concept

`StateCondition` (#38) and `CountSpec` (#39) are **not** retired here, against the
issues' current "P9" expiry. They add a second concept on top of the object
predicate: a **scope** (which objects to fold, over which zones, from whose
perspective), an **aggregation** (count of objects; count of *distinct* card types
for Tarmogoyf), and a **comparison to a threshold** ("you control no Swamps" =
count of {Swamps you control} = 0). That concept reuses the P9 `Filter` as its
per-object predicate but is a genuinely separable design — and folding it in would
drag in `Pawl.Event`'s state-condition casing, the trigger / intervening-"if" /
duration triple, and `Quantity.Count`, sprawling the phase.

**Action:** #38 and #39 are **re-scoped, not closed** — their expiry trigger moves
from "milestone P9" to the deferred count/compare phase (a P9b, or a card-driven
trigger if no card forces it sooner). Their code sites keep their comments,
updated to point at the new expiry. #40 (the `TargetSpec` family) **is** closed by
§4a. `SpellCriterion` has no separate issue; its merge rides along in §4b.

Also out of scope: regeneration (§6), any `Quantity`-valued comparison,
`Affected.TheseObjects` (stays a locked id set), and name / mana-value / keyword
filter atoms (added card-driven, each with its own issue).

## 8. Exit criterion

Doom Blade, Terror, and Reprisal cast and resolve with their legal target sets
derived by `Filter.matches`; the basic-land search and the `PermanentCriterion`
consumer match through the same evaluator; `Pawl.Type.TargetSpec` holds a pool +
`Maybe Filter` with no per-card variant; `CardCriterion`, `PermanentCriterion`,
and `SpellCriterion` are deleted; `Affected`'s dynamic sets are `Matching Filter`;
#40 is closed and #38/#39 re-scoped. `cabal build all --enable-tests
--enable-benchmarks` is warning-clean, `cabal test` green, `hooky run` clean.
