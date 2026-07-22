# M4.5 P7 — Player continuous effects, restrictions and cost modification

*Design pass 2026-07-22. The eighth phase of the M4.5 umbrella
(`docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`), closing
**GAP-P** and the *modification* half of **GAP-Co**. The whole of Cluster 3, and
the largest new axis in M4.5. Gates: **Rule of Law**, **Thalia, Guardian of
Thraben**, **Sapphire Medallion**, **Silence** and **Reliquary Tower**. This
spec is implementable; a `writing-plans` plan follows it.*

*CR citations below **were checked against `docs/rules.txt` during this design
pass**: 101.2, 103.5, 109.5, 118.7, 118.7a, 118.7b, 118.7c, 118.7d, 118.7e,
118.7f, 118.7g, 118.9d, 402.1, 402.2, 601.2b, 601.2e, 601.2f, 601.2g, 601.2h,
601.3, 601.3a, 604.1, 604.2, 604.3, 611.1, 611.2c, 611.2d, 611.3, 613.1, 613.7,
613.10, 613.11, 614.1a, 614.1b, 614.1c. Any number added later and marked
**(verify)** must be checked before it drives code (CLAUDE.md: never trust
recalled Magic rules).*

*Card text and Gatherer rulings for all five gate cards **were verified live
against the Scryfall API during this design pass**, not read from the vendored
MTGJSON dump (`card-data-source`). The oracle text and every ruling quoted in §5
is what Scryfall returned on 2026-07-22.*

## 0. Why this phase, and what it proves

Every continuous effect pawl can express edits an **object**. `Modification`'s
ten constructors each change a characteristic of one; `Affected` names a set of
`ObjectId`s; `ContinuousEffect` and `StaticAbility` both pair the two. And
`Player` carries two fields:

```haskell
data Player = MkPlayer
  { life :: Integer,
    status :: Status
  }
```

CR 611.1 says that is one third of what a continuous effect is:

> **611.1.** A continuous effect modifies characteristics of objects, modifies
> control of objects, **or affects players or the rules of the game**, for a
> fixed or indefinite period.

pawl has the first clause (M3b–P3b) and the second (P1). The third has no axis
at all. The census (`docs/mtgish-gap-census.md` §3.2) folds ~250 mtgish
`PlayerEffect` variants onto it — prohibitions, cost modification, value
overrides, permission grants — and calls it "the single largest unroadmapped
closed-half surface." A restriction, a cost increase, or a hand-size override is
a continuous effect with **no object to attach to**, so today there is nowhere
for one to live and no site that would read it.

### The rules put this axis outside the layer system

This is the structural fact the phase is built on, and it is easy to get wrong.
CR 613.1 opens "the values of an **object's** characteristics are determined by
starting with the actual object" — the seven layers are a machine for computing
object characteristics, and nothing else. The effects this phase adds are handled
by two separate rules, *after* that machine has run:

> **613.10.** Some continuous effects affect **players** rather than objects. For
> example, an effect might give a player protection from red. All such effects
> are applied in timestamp order after the determination of objects'
> characteristics.

> **613.11.** Some continuous effects affect **game rules** rather than objects.
> For example, effects may modify a player's maximum hand size, or say that a
> creature must attack this turn if able. These effects are applied after all
> other continuous effects have been applied. **Continuous effects that affect
> the costs of spells or abilities are applied according to the order specified
> in rule 601.2f.** All other such effects are applied in timestamp order.

So P7 is **not** a new layer, not a new `Modification`, and not a new `Layer`
constructor. It is a sibling tier that runs after the layer fold, and
`Pawl.Projection` is untouched by it. Every one of P7's five gate cards is a CR
613.11 rules-modifying effect; the 613.10 player-affecting tier (protection from
red for a *player*) is real, distinct, and deferred (§8).

### What this phase is *not*

It is not the **cost payment** generalization. The umbrella splits GAP-Co
deliberately (§3, "Notes the phase specs must not lose"): cost *modification* —
increases and reductions, a species of rules-modifying continuous effect — is
P7; pay-life, sacrifice-N, discard-as-cost, exile-from-zone and the
alternative-cost seam are **P8**. `AdditionalCost` and `AbilityCost` are not
touched here.

It is not the **filter language**. `SpellCriterion` (§2.3) is hand-carved, one
variant per gate card, and is retired wholesale by P9 alongside `TargetSpec`
(#40), `CountSpec` (#39) and `StateCondition` (#38).

It is not **turn-structure skips**, and this is a correction the census earns.
§3.2 lists `SkipsUntapStep`, `SkipsDrawStep`, `SkipsMainPhase` under
`PlayerEffect` — but CR 614.1b is explicit:

> **614.1b.** Effects that use the word "skip" are **replacement effects**. These
> replacement effects use the word "skip" to indicate what events, steps, phases,
> or turns will be replaced with nothing.

A skip is P5's machinery — a `ReplacementEffect` whose replaced event is a step
beginning — not this axis. P7 says so explicitly so that nobody later builds a
`SkipsDrawStep` constructor in the wrong home. (`Engine.skipsDraw`'s existing
CR 103.7a first-turn skip is a turn-based rule, not an effect, and stays where
it is.)

### Gate cards

Oracle text as Scryfall returned it on 2026-07-22.

| Card | Cost / type | Oracle text | Shape it proves |
|---|---|---|---|
| **Rule of Law** | `{2}{W}` Enchantment | "Each player can't cast more than one spell each turn." | prohibition, symmetric, counted over P4's turn history |
| **Thalia, Guardian of Thraben** | `{1}{W}` Legendary Creature — Human Soldier | "First strike / Noncreature spells cost {1} more to cast." | cost **increase**, filtered on the spell's type |
| **Sapphire Medallion** | `{2}` Artifact | "Blue spells you cast cost {1} less to cast." | cost **reduction**, filtered on colour, scoped to *you* |
| **Silence** | `{W}` Instant | "Your opponents can't cast spells this turn." | **stored** player effect with a duration, scoped to *opponents* |
| **Reliquary Tower** | Land | "You have no maximum hand size. / {T}: Add {C}." | **value override**, read *off* the casting path |

Five cards is more than a phase usually carries, and each one is load-bearing:
they are the four census shapes (prohibition, increase, reduction, value
override) crossed with the two carriers (printed static, stored-with-duration)
and the three scopes (`EachPlayer`, `You`, `Opponents`). Drop any one and a
structural arm of the axis ships untested.

Every gate card is real and recognizable, so no synthetic crutch is needed
(umbrella §1, definition-of-done). Thalia carries first strike, which M2b
already implements; her creature-ness is what makes "noncreature spells,
**including your own**" observable in a game where she is on the battlefield.

### The falsifiers, stated up front

1. **Rule of Law kills any per-effect watermark, counter field, or `Bool`.** Its
   Gatherer ruling: *"Rule of Law looks at the entire turn to see if a player has
   cast a spell, **even if Rule of Law wasn't on the battlefield when that spell
   was cast**. Notably, you can't cast Rule of Law and then cast another spell
   during the same turn."* An implementation that starts counting when the effect
   begins is wrong; the count is a filter over P4's whole turn log. The second
   ruling — *"If you cast a spell that was countered, you can't cast another
   spell during the same turn"* — fixes the counted event as the **cast**, not
   the resolution.
2. **Rule of Law also kills enforcement at cast time.** The engine never *offers*
   an illegal action, so the gate belongs in `Cast.castable`, upstream of
   `Action.legalActions`. And it kills stored state: the effect is re-derived
   live from the battlefield, so Rule of Law leaving mid-turn lifts the
   restriction with nothing to unwind.
3. **Silence kills the `Affected.TheseObjects` habit.** CR 611.2c's first
   sentence freezes a stored effect's object set — which is what every stored
   `ContinuousEffect` in pawl does today. Its third sentence carves out exactly
   this axis: *"A continuous effect generated by the resolution of a spell or
   ability that doesn't modify the characteristics or change the controller of
   any objects modifies the rules of the game, so it **can affect objects that
   weren't affected when that continuous effect began**."* Freeze Silence's set
   and it names opponent spells that do not exist yet — there are none on the
   stack when it resolves — and the card does **literally nothing**. Observable
   in two-player.
4. **Silence also kills an over-broad prohibition.** Its ruling: *"The only thing
   Silence stops is casting spells. Your opponents can still activate abilities,
   including abilities of cards in their hands (like cycling). Their triggered
   abilities work as normal, they can still play lands, and so on."* A
   prohibition that gates `Action.Play` or `Action.Activate` fails this.
5. **Thalia kills taxing one site instead of two.** Tax castability but not
   payment and the player underpays; tax payment but not castability and the
   engine offers a cast that cannot be afforded — and pawl has no
   mid-announcement rewind (#56), so that is a wedged game, not a rejected
   action.
6. **Sapphire Medallion kills "subtract from the mana value."** CR 118.7a: *"Effects
   that reduce a cost by an amount of generic mana affect only the generic mana
   component of that cost. They can't affect the colored or colorless mana
   components."* Its own ruling repeats it. `{1}` off a `{U}` spell leaves `{U}`;
   off `{2}{U}` it gives `{1}{U}`. A mana-value implementation makes the first
   free.
7. **Reliquary Tower kills a cast-only axis**, and separates two constants the
   rules keep apart that `Engine.discardToHandSize` currently conflates: CR 103.5's
   starting hand size and CR 402.2's maximum hand size, both "normally seven."

## 1. Scope

**In scope.** A `PlayerEffect` vocabulary and its two supporting classifications;
two carriers (printed on `Card`, stored on `GameState`); one sole casing home;
the CR 601.2f total-cost computation; one new `Effect` opcode; one new
`GameEvent` kind; six read sites; codec coverage; five gameplay-level gate tests.

**Out of scope, with issues filed (§8).** Cost payment generalization and
alternative costs (P8). Activated-ability cost modification. The CR 613.10
player-affecting tier. CR 118.7b–g's non-generic reductions. CR 601.3a's
quality-changing choices. Player-scoped *permissions*. Turn-structure skips (P5,
per CR 614.1b above).

## 2. Architecture

### 2.1 `PlayerEffect`, the new open-half vocabulary

A leaf family, sibling to `Modification`, classified by nothing but its own
constructors — hand-carved, one variant per gate card, the `StateCondition` /
`TargetSpec.WallTarget` posture of specific-before-general.

```haskell
-- Pawl.Type.PlayerEffect
data PlayerEffect
  = CantCastSpells                            -- Silence
  | CantCastMoreThan Natural                  -- Rule of Law: spells each turn
  | IncreaseSpellCost SpellCriterion Natural  -- Thalia: noncreature, {1}
  | ReduceSpellCost SpellCriterion Natural    -- Sapphire Medallion: blue, {1}
  | NoMaximumHandSize                         -- Reliquary Tower
  deriving (Eq, Ord, Show)
```

**Increase and reduce are separate constructors, not one signed delta.** The
rules treat them as different operations, in two ways a signed integer cannot
express: CR 601.2f applies *all* increases before *any* reduction, and CR 118.7a
gives reductions a restriction increases do not have (generic component only).
Collapsing them would put both rules into arithmetic that cannot state either.

`CantCastMoreThan` carries the limit rather than hardcoding one: Rule of Law and
Arcane Laboratory say one, and the constructor should not have to grow a sibling
for a card that says two.

### 2.2 `PlayerScope`, the affected-players classification

The player-side analogue of `Affected`, and much smaller because CR 109.5 fixes
what "you" means:

```haskell
-- Pawl.Type.PlayerScope
data PlayerScope
  = You        -- Sapphire Medallion, Reliquary Tower
  | Opponents  -- Silence
  | EachPlayer -- Rule of Law, Thalia
  deriving (Eq, Ord, Show)
```

Resolved against the effect's **controller** (CR 109.5: "the words 'you' and
'your' on an object refer to the object's controller … for a static ability,
this is the current controller of the object it's on").

**The scope is always dynamic — never frozen.** This is the CR 611.2c point and
the single most important structural decision in the phase. A stored
`Modification` freezes its set (`Affected.TheseObjects`); a stored `PlayerEffect`
must not, because 611.2c classifies it as a rules modification that "can affect
objects that weren't affected when that continuous effect began." There is
therefore **no stored-set analogue of `TheseObjects`** in this axis, and no
`arm`-style one-way door for scope — `PlayerScope` is the same type on both
carriers. (`Duration` → `Expiry` still applies, unchanged, to the *duration*.)

### 2.3 `SpellCriterion`, the third criterion sibling

```haskell
-- Pawl.Type.SpellCriterion
data SpellCriterion
  = NoncreatureSpell   -- Thalia
  | SpellOfColor Color -- Sapphire Medallion (Blue)
  deriving (Eq, Ord, Show)
```

A third sibling to `CardCriterion` and `PermanentCriterion`, deliberately **not**
merged with either — for exactly the reason `PermanentCriterion`'s own comment
already gives: P9 merges all of them into one filter language, and merging two of
them here would build half of P9 with one customer. Filed as retired-by-P9
alongside its siblings.

Both inhabitants read the **projection**, never the printed card: `NoncreatureSpell`
asks `Projection.cardTypesOf` and `SpellOfColor` asks `Projection.colorsOf`, per
the standing house rule that the closed half never reads a printed characteristic
directly.

### 2.4 Two carriers, printed and stored

The project already runs this split three times — `Duration`/`Expiry`,
`ReplacementEffect`/`ActiveReplacement`, `DelayedTrigger`/`PendingTrigger`. P7 is
the fourth.

**Printed**, a new field on `Card`, sibling to `staticAbilities`,
`replacementEffects` and `triggeredAbilities`:

```haskell
-- Pawl.Type.PlayerStaticAbility
data PlayerStaticAbility = MkPlayerStaticAbility
  { scope :: PlayerScope,
    effect :: PlayerEffect
  }

-- Pawl.Type.Card
  playerAbilities :: [PlayerStaticAbility],
```

CR 604.1/604.2: a static ability's continuous effect is active while its permanent
is on the battlefield, so these are gathered live from the battlefield on every
read and never captured — the same posture `Projection.gather` takes for
`staticAbilities`. Rule of Law, Thalia and Sapphire Medallion each declare one;
Reliquary Tower declares one alongside its mana ability.

**Stored**, a new list on `GameState`, the `ActiveReplacement` record shape:

```haskell
-- Pawl.Type.ActivePlayerEffect
data ActivePlayerEffect = MkActivePlayerEffect
  { source :: ObjectId,
    controller :: PlayerId,
    timestamp :: Timestamp,
    expiry :: Expiry,
    scope :: PlayerScope,
    effect :: PlayerEffect
  }

-- Pawl.Type.GameState
  playerEffects :: [ActivePlayerEffect],
```

**`controller` is stored, and `ContinuousEffect` does not store one.** It has to
be. A stored `Modification` re-reads its source's *projected* controller (CR
613.1b), which works because the source is a permanent. Silence is an instant: by
the time its effect is live, the source is in a graveyard with no controller to
project. "Your opponents" would be unanswerable. So the controller is baked at
creation — the same treatment `Expiry.While` already gives CR 109.5's "you" —
while the *scope* it is fed to stays dynamic per §2.2.

**`timestamp` is stored even though P7 cannot observe it.** CR 613.10 and 613.11
both order by timestamp (CR 613.7), and Reliquary Tower's ruling names the case:
*"If multiple effects modify your hand size, apply them in timestamp order. For
example, if you put Null Profusion … onto the battlefield and then put Reliquary
Tower onto the battlefield, you'll have no maximum hand size. However, if those
permanents enter in the opposite order, your maximum hand size would be two."*
None of P7's five constructors conflict with another, so the ordering is
unobservable here — but stamping at creation is free and retrofitting an order
onto effects already stored is not. Filed with **Null Profusion** as the named
expiry (§8).

### 2.5 `Pawl.PlayerEffect`, the sole casing home

One new module, holding the standing over `PlayerEffect`, `PlayerScope` and
`SpellCriterion` that `Pawl.Projection` holds over `Modification`, `Pawl.Resolve`
over `Effect`, `Pawl.Event` over `TriggerCondition` and `Pawl.Expiry` over
`Expiry`. It gathers both carriers — every battlefield permanent's
`playerAbilities` (CR 604.2), plus `GameState.playerEffects` — resolves each
entry's scope against its controller, and answers **typed questions**:

```haskell
-- CR 601.3: does any effect prohibit this player from casting this spell?
prohibitsCasting :: PlayerId -> GameState -> Bool

-- CR 613.11 / 601.2f: the increases and reductions applying to this spell,
-- kept apart because 601.2f applies them in that order.
costAdjustments :: PlayerId -> ObjectId -> GameState -> ([Natural], [Natural])

-- CR 402.2. Nothing is "no maximum hand size", never a sentinel.
maximumHandSize :: PlayerId -> GameState -> Maybe Natural
```

No `ProjectedPlayer` record. The symmetry with `ProjectedCharacteristics` is
tempting and wrong: `costAdjustments` depends on *which spell is being cast*, so
the cost half of the axis could never be pre-folded into a per-player record, and
a record that carried one real field while the interesting path bypassed it would
be a shape to unpick later. Typed queries now; a record is the natural growth
path when a caller wants several answers at once.

**`prohibitsCasting` deliberately does not take the spell.** Both of P7's
prohibitions are quality-free — "can't cast spells", "can't cast more than one
spell" — so the answer does not depend on *which* spell, and a parameter nothing
reads would be a shape asserting a generality the phase has not built. It grows
an `ObjectId` when CR 601.3a's quality-bearing prohibitions do (§8, Void
Winnower), which is the one deferral in this phase that changes a signature
rather than adding a constructor.

**CR 101.2 is why prohibitions fold as a disjunction**: "When a rule or effect
allows or directs something to happen, and another effect states that it can't
happen, the 'can't' effect takes precedence." One applicable prohibition is
enough; nothing outvotes it.

### 2.6 `Pawl.Cost`, and CR 601.2f

A second new module, owning the total-cost computation and giving P8 a home to
land in. `Pawl.Mana` keeps pools, production and payment; `Pawl.Cost` keeps what
a cost *is*.

```haskell
-- CR 601.2f: the total cost of this spell for this player.
total :: PlayerId -> ObjectId -> ManaCost -> GameState -> ManaCost
```

CR 601.2f, in its stated order:

> The total cost is the mana cost or alternative cost (as determined in rule
> 601.2b), plus all additional costs and cost increases, and minus all cost
> reductions. … If the mana component of the total cost is reduced to nothing by
> cost reduction effects, it is considered to be {0}. It can't be reduced to less
> than {0}.

1. **Start from the mana cost with X already substituted.** CR 601.2b precedes
   601.2f, and `Cast.castSpell` already calls `Mana.substituteX`. Sapphire
   Medallion's own ruling confirms the order: *"If a spell you cast has {X} in its
   mana cost, you choose the value of X before calculating the spell's total
   cost."*
2. **Append every increase** as a `Generic n` symbol.
3. **Apply every reduction to the generic component only** (CR 118.7a), floored at
   zero, never touching an `OfType` or `Variable` symbol. A reduction with no
   generic left to take is simply lost.

601.2f's "reduced to nothing is {0}" needs no special case: `ManaCost` is a list
of symbols and the empty list *is* `{0}` — `Mana.spend` on it is already a no-op.
Nor does the phase need a "mana value is unchanged" rule; Thalia's ruling says
*"The mana value of the spell remains unchanged, no matter what the total cost to
cast it was"*, and pawl computes mana value from the printed cost, so nothing
reads the total.

Reduction *order* is a prompt in the rules — "if multiple cost reductions apply,
the player may apply them in any order" — and an elision here: every reduction P7
can express is an amount of generic mana routed to the same pool by 118.7a, so
the order is unobservable (§8, expiring on a coloured or hybrid reduction, CR
118.7e).

### 2.7 The event kind, and why the count is free

```haskell
-- Pawl.Type.GameEvent
  | SpellCast PlayerId
```

Emitted by `Cast.castSpell`, and the `P4 → P7` edge the umbrella predicted
("each later phase adds the event kind it reads — `SpellCast` at **P7**").

Rule of Law's count is `length (filter (cast by pid) (GameState.events gs))`
against the limit. Three properties come free from P4's substrate and none of
them needs new state:

- P4's log is **cleared at turn handoff, not at cleanup**, so "this turn" is
  exactly the log's own extent — including a spell cast on an *opponent's* turn,
  which is a turn the caster is limited in too.
- The log is **appended by the funnel and never drained by a reader**
  (`scannedThrough` is a watermark, not a consumption), so counting is a fold over
  the whole turn — which is precisely what Rule of Law's ruling demands, and what
  a per-effect watermark would get wrong.
- The event is the **cast**, so a countered spell still counts, per the second
  ruling.

### 2.8 The read sites

Six, and the breadth is the point: an axis that only the casting path consults
has not been shown to be an axis.

| Site | Change |
|---|---|
| `Cast.castable` | gated by `prohibitsCasting`; affordability asks `Cost.total`, not the printed cost |
| `Cast.castableWhileSearching` | the same two changes |
| `Cast.castSpell` | pays `Cost.total`; emits `GameEvent.SpellCast` |
| `Engine.discardToHandSize` | asks `maximumHandSize` (CR 402.2) instead of `Setup.openingHand` |
| `Pawl.Expiry` | a third carrier in the three sweeps |
| `Pawl.Resolve` | the `AffectPlayers` opcode |

**`castableWhileSearching` is not an oversight.** CR 601.3 is one sentence with
two halves — "only if a rule or effect **allows** that player to cast it and no
rule or effect **prohibits** that player from casting it" — and pawl now
implements both: `Cast.permitsCastWhileSearching` is the allow half (Panglacial
Wurm), `PlayerEffect.prohibitsCasting` is the prohibit half. The Panglacial
permission is a *timing* exception only (its ruling: "follows all normal rules …
except for timing"), so a Rule of Law and a Thalia tax both still apply to a
spell cast from the library.

**The prohibition gates casting and nothing else.** Not `Action.Play`, not
`Action.Activate` — Silence's ruling is explicit that lands and activated
abilities are unaffected, and CR 601.3 is a rule about casting spells.

### 2.9 The new opcode

```haskell
-- Pawl.Type.Effect
  | AffectPlayers Duration PlayerScope PlayerEffect
```

Targetless, mirroring `Replace Duration Uses ReplacementEffect`: a rules-modifying
effect watches a *class*, not a chosen object, so there is nothing to target and
nothing to prompt. `Pawl.Resolve` stores it into `GameState.playerEffects` with
the resolving object's source, its controller, a fresh timestamp, and
`Expiry.arm`'s answer — Silence's "this turn" is `Duration.UntilEndOfTurn` →
`Expiry.AtCleanup`, so P6's arm handles it unchanged.

### 2.10 Expiry gains a third carrier

`Pawl.Expiry`'s three sweeps — `dropAtCleanup`, `sweepConditional`,
`dropAtHandoff` — each grow a `keepPlayerEffect` and a third field update. This
is precedented rather than novel: the module's own comment records that its
per-carrier sweeps merged "because the two lists lived in two modules, not
because they differed." A third list that shares the expiry vocabulary shares the
sweep.

`sweepConditional` needs the source and expiry of each entry, both of which
`ActivePlayerEffect` carries, so a `ForAsLongAs` player effect would work — though
no gate card produces one (§8).

### 2.11 Serialization

`PlayerStaticAbility`, `PlayerEffect`, `PlayerScope`, `SpellCriterion` and the
`AffectPlayers` opcode are **card data** and get codecs, exercised by the
round-trip suite. `ActivePlayerEffect` is runtime-only and gets none — the same
standing `Expiry`, `ActiveReplacement` and `PendingTrigger` have. A printed value
that leaked into the store, or a stored value that leaked into card JSON, stays
unrepresentable.

## 3. The two invariants

**The rules core reads a classification, never an identity.** `PlayerEffect` is a
classification the closed half consults, and `Pawl.PlayerEffect` is its sole
casing home on the same pattern as every other leaf family. No read site cases on
a card. The five gate cards reach the engine as data: three `playerAbilities`
entries, one `AffectPlayers` opcode, one `playerAbilities` entry on a land.

**The engine makes no choices.** P7 adds no prompt, and every place it could have
is either forced or elided with a named expiry: CR 601.2f's reduction-order
choice is unobservable while all reductions are generic (§8); a prohibited cast
is removed from `legalActions` rather than offered and rejected, which is the
absence of a choice, not the making of one. The one existing prompt P7 touches —
`Prompt.ChooseDiscard` at cleanup — is asked *less* often, not differently: a
player with no maximum hand size is never asked.

## 4. What this phase does **not** touch

- `Pawl.Projection`, the layer fold, `Layer`, `Modification`, `Affected`,
  `ContinuousEffect`, `StaticAbility` — the axis is a sibling tier per CR
  613.10/613.11, not a layer.
- `Pawl.Type.Player` — the effects are gathered from carriers, not stored on the
  player. `Player` keeps `life` and `status`. (P10's counter substrate is what
  grows it.)
- `AdditionalCost`, `AbilityCost`, `Mana.spend`, `Mana.payCost`'s signature — P8.
- `Pawl.Replacement` — including skips, which are replacement effects (CR 614.1b).
- `Setup.openingHand` itself, which is still CR 103.5's starting hand size and is
  still 7. Only its *misuse* as a maximum hand size goes away.

## 5. Cards and tests

Every gate test is **gameplay-level**: it casts or resolves through the stack and
asserts on game state (umbrella §1). A codec round-trip proves only that a card
*says* something (design.md §4, the central trap) and is never the gate.

### Rulings discipline (design.md §4)

All Oracle text and rulings above and below were fetched live from the Scryfall
API on 2026-07-22. Where a ruling contradicts a plausible implementation, the
ruling is quoted in §0 and drives a test, not a comment.

### Prohibition, whole-turn history — Rule of Law

- Cast Rule of Law; then assert no `Action.Cast` appears in `legalActions` for
  either player for the rest of that turn — the ruling's "you can't cast Rule of
  Law and then cast another spell during the same turn", which is also the
  whole-turn-history falsifier, since the spell that used up the allowance is
  Rule of Law itself, cast *before* the effect existed.
- Advance to the next turn; assert casting is available again — the log clears at
  handoff.
- Cast a spell, counter it, assert the caster still cannot cast — the counted
  event is the cast.
- With Rule of Law on the battlefield and one spell already cast, destroy it and
  assert casting is available again in the same turn — the effect is re-derived
  live, with no stored state to unwind.

### Prohibition, stored with a duration — Silence

- Resolve Silence; assert the opponent has no `Action.Cast` and the controller
  still does — the `Opponents` scope, and the CR 611.2c dynamic-set falsifier:
  the opponent's hand contains cards that were not spells when Silence resolved.
- Assert the opponent still has `Action.Play` for a land and `Action.Activate`
  for a permanent's ability — the ruling's "the only thing Silence stops is
  casting spells."
- Advance past cleanup; assert the opponent can cast again — `Expiry.AtCleanup`.

### Cost increase — Thalia, Guardian of Thraben

- With Thalia on the battlefield, assert a noncreature spell costs one more:
  affordable with the higher amount, *not* offered in `legalActions` with only
  the printed amount available, and leaving the pool short by exactly the
  increase after payment. Both sites, one scenario.
- Assert a creature spell is unaffected — `SpellCriterion` reads the projection.
- Assert Thalia's controller is taxed too — "including your own", `EachPlayer`.

### Cost reduction — Sapphire Medallion

- CR 118.7a, the headline: a `{U}` spell still costs `{U}` — a reduction with no
  generic component to take is lost, not applied to coloured mana.
- A `{2}{U}` spell costs `{1}{U}`.
- A red spell is unaffected — `SpellOfColor Blue`.
- With Thalia *and* Sapphire Medallion both out, a **`{U}`** blue noncreature
  spell still costs `{U}`. This is the CR 601.2f **order** test, and the cost
  must be exactly `{U}`: increase first gives `{1}{U}`, which the reduction then
  takes back to `{U}`; reduce first loses the reduction to CR 118.7a's empty
  generic component and the increase then leaves `{1}{U}`. A spell with a
  generic component would **not** falsify the order — the two orders agree
  wherever the floor does not bind, which is why this test names a cost that has
  no generic mana at all.

### Value override — Reliquary Tower

- With nine cards in hand at cleanup and no Reliquary Tower, assert
  `Prompt.ChooseDiscard` is asked for two.
- With Reliquary Tower on the battlefield, assert the prompt is not asked at all
  and the hand keeps nine cards.

### Codec

Round-trip the four new card-data types and the `AffectPlayers` opcode, and the
five gate cards as whole card definitions.

## 6. Module and type changes (summary)

**New types** — `Pawl.Type.PlayerEffect`, `Pawl.Type.PlayerScope`,
`Pawl.Type.SpellCriterion`, `Pawl.Type.PlayerStaticAbility`,
`Pawl.Type.ActivePlayerEffect`.

**New logic modules** — `Pawl.PlayerEffect` (sole casing home; `prohibitsCasting`,
`costAdjustments`, `maximumHandSize`), `Pawl.Cost` (`total`, CR 601.2f).

**Changed types** — `Card.playerAbilities`; `GameState.playerEffects`;
`Effect.AffectPlayers`; `GameEvent.SpellCast`.

**Changed modules** — `Pawl.Cast` (three sites), `Pawl.Engine`
(`discardToHandSize`), `Pawl.Expiry` (third carrier ×3 sweeps), `Pawl.Resolve`
(new opcode), `Pawl.Codec` (new card data), `Pawl.Event` (the `SpellCast`
constructor's arm in each existing exhaustive match over `GameEvent`).

**New test module** — `Pawl.PlayerEffectSpec`, wired into `Main.hs`'s `testTree`
and the test-suite `other-modules` list.

**Five new card definitions** under the M3.5 cards-as-data layout.

## 7. Ordering within the phase (for the plan)

Substrate before consumer, and each step a complete commit:

1. `PlayerEffect` + `PlayerScope` + `SpellCriterion` + `PlayerStaticAbility`, with
   codecs and round-trips. No reader yet.
2. `Card.playerAbilities`, and `Pawl.PlayerEffect`'s gather over the battlefield.
3. `GameEvent.SpellCast`, emitted by `Cast.castSpell`.
4. `prohibitsCasting` + the `Cast.castable` / `castableWhileSearching` gate →
   **Rule of Law** lands.
5. `Pawl.Cost.total` + `costAdjustments`, and the two cost read sites →
   **Thalia**, then **Sapphire Medallion**.
6. `maximumHandSize` + `Engine.discardToHandSize` → **Reliquary Tower**.
7. `ActivePlayerEffect` + `GameState.playerEffects` + `Expiry`'s third carrier +
   `Effect.AffectPlayers` + `Pawl.Resolve` → **Silence**.

Silence is last deliberately: it is the only step that touches the stored
carrier, and every other gate card proves the axis works from the printed one
first.

## 8. Deferred, with named expiries

Each becomes a GitHub issue carrying status, rationale and expiry trigger, cited
inline at the code site as `(#N)` with no expiry written into the comment
(CLAUDE.md).

| Deferred | Why it is safe | Expires on |
|---|---|---|
| Activated-ability cost modification (`AbilityCost` untouched) | no gate card modifies one; the census's `ReduceManaCostOfActivatedAbilities` class has no producer | a card that taxes or discounts an activation (Rings of Brighthearth-adjacent) — card-driven |
| CR 601.2f's "reductions in any order" prompt | every P7 reduction is an amount of generic mana routed to one pool by CR 118.7a, so order is unobservable | a coloured or hybrid reduction, which CR 118.7e explicitly prompts — card-driven |
| CR 118.7b–g (colored, colorless, hybrid, Phyrexian, snow reductions) | no producer; every reduction in the pool P7 covers is generic | a card whose reduction names a mana type — card-driven |
| CR 613.10's player-affecting tier (protection from red for a *player*) | a genuinely distinct tier from 613.11's rules tier; no producer | a card granting a player protection or shroud — card-driven |
| CR 613.10/613.11 timestamp ordering | none of P7's five constructors conflicts with another, so the order is unobservable; the field is stored so the fix is a sort, not a migration | **Null Profusion** + Reliquary Tower, the pair Reliquary Tower's own ruling names — card-driven |
| CR 601.2f's "locked in" total | pawl recomputes on demand; nothing can change a cost between announcement and payment | an effect that changes a cost mid-announcement — card-driven |
| CR 601.3a's quality-changing choices (Void Winnower) | P7's prohibitions name no spell qualities, so 601.3a is vacuous | **Void Winnower**, 601.3a's own worked example — card-driven |
| Player-scoped permissions (extra lands, cast-from-graveyard) | `Card.castingPermissions` is object-scoped; the player-scoped sibling has no producer | Exploration / Yawgmoth's Will — card-driven |
| A `ForAsLongAs` player effect | `sweepConditional` would handle one, but no gate card produces one | a conditional-duration player effect — card-driven |
| `Cast.castSpell` computes the total cost against an object still in **hand** | CR 601.2f runs after 601.2a moves the spell to the stack; pawl pays first. Newly load-bearing, since the cost now depends on the spell's projected characteristics and a hand object could project differently from a stack one | a card whose type or colour differs between hand and stack — card-driven; adjacent to #56 |
| Turn-structure skips | CR 614.1b makes them replacement effects; the census's placement under `PlayerEffect` is corrected here | a skip card, built on P5 — card-driven |

## 9. Tracking

- Closes **issue #3** (M4.5 P7) and, with it, git-bug `c5a985d` (GAP-P), which
  the umbrella re-pointed here (umbrella §6).
- Closes the *modification* half of GAP-Co. The payment half stays open as
  **#4** (P8).
- Does **not** retire #38/#39/#40 (P9's filter language); `SpellCriterion` joins
  that list as a fourth member.
- Updates the umbrella spec: P7's row ticked, §4's "P7 is next" replaced, and the
  CR 614.1b skips correction recorded so the census's §3.2 placement is not
  followed by a later phase.
- Updates `docs/progress.md` with the completion entry and `CLAUDE.md`'s status
  bullet by **replacement**.

## 10. Exit criterion

The player/rules continuous-effect axis has (a) a classification in the type
system — `PlayerEffect`, scoped by `PlayerScope`, on printed and stored carriers,
with one sole casing home — and (b) five real, recognizable gate cards, each
passing a gameplay-level test, covering four effect shapes, two carriers, three
scopes and two read paths.

At that point Cluster 3 is closed, and M4.5 has P8 (costs), P9 (filters), P10
(player counters) and P11 (Command zone) remaining — of which P8 and P9 float
freely, per umbrella §4.
