# M3e activated abilities (abilities on the stack) — design

Design for milestone **M3e**, the fifth letter of M3 (see the split table in
`docs/design.md`): **abilities on the stack — activation (CR 602), non-mana
costs, and the CR 605 mana-ability classification.** This letter is not a
go/no-go — the M3 verdict already arrived at the end of M3d — but it is the
prerequisite for M3g: Mindslaver and Evolving Wilds both need activation
(`docs/design.md`, M3 ordering note). It validates a seam prior art already
proves works (ygopro's suspension protocol at 13k cards), not a novel bet.

The gate cards, Scryfall-verified (`api.scryfall.com/cards/named`, fetched
2026-07-18):

| Card | Cost | Type line | P/T | Oracle text |
|---|---|---|---|---|
| **Prodigal Sorcerer** | `{2}{U}` | Creature — Human Wizard Sorcerer | 1/1 | "{T}: This creature deals 1 damage to any target." |
| **Evolving Wilds** | — | Land | — | "{T}, Sacrifice this land: Search your library for a basic land card, put it onto the battlefield tapped, then shuffle." |
| **Llanowar Elves** | `{G}` | Creature — Elf Druid | 1/1 | "{T}: Add {G}." |

Prodigal Sorcerer **reuses `DealDamage`** — its activated ability adds no opcode
to the executor, only the machinery to get an ability *onto the stack*. Evolving
Wilds brings a **sacrifice cost** and the **`Search`** opcode. Llanowar Elves is
the third card, added by this spec (not in the `docs/design.md` gate row): it is
the **printed activated mana ability** that exercises the CR 605 classification's
*true* branch — a mana ability must resolve inline and **never touch the stack**.
Without it the classification predicate would have only its false branch tested.

This is a types-and-architecture spec, not an implementation plan.

## 0. The core idea and the phased spine

**M3e makes an activated ability a first-class object on the stack, and adds the
one ABI predicate that keeps mana abilities off it.** Three structural axes, one
predicate:

| Axis | Mechanism | Gate |
|---|---|---|
| **Activation (CR 602)** | an ability becomes a new *kind* of stack object (`Source.OfAbility`), put there by `Action.Activate`, resolved through the existing `Resolve` executor, then **ceasing to exist** (CR 608.2n) rather than being buried | Prodigal Sorcerer |
| **Non-mana costs** | `{T}` (tap the source) and Sacrifice, paid at activation (CR 602.1a / 701.21) | Prodigal Sorcerer (`{T}`); Evolving Wilds (`{T}`, Sacrifice) |
| **The CR 605 classification** | `isManaAbility` — the ABI predicate read at two sites: a mana ability is a mana *source* and is **not** offered as a stack action | Llanowar Elves (true branch); Prodigal Sorcerer / Evolving Wilds (false branch) |

Plus one projection move (`abilitiesOf`, the `keywordsOf` analog) so a Humility'd
Prodigal Sorcerer cannot tap, and one new opcode family (`Search`) for Evolving
Wilds.

**Why the classification is the heart of it.** CR 605.3b: "an activated mana
ability doesn't go on the stack ... it resolves immediately after it is
activated." A naive engine offers *every* activated ability
to the stack — and Llanowar Elves' mana ability, put on the stack, would deadlock
cost payment (you cannot let the stack resolve in the middle of paying for a
spell). The classification is the fork, and it is an ABI predicate three
independent fused engines still had to grow explicitly (`prior-art-lessons.md`
§8.2: Shandalar's `EA_MANA_SOURCE`, Duels'
`COMPARTMENT_ID_TRIGGER_ABILITY_IS_MANA_ABILITY`, and the mana leak §1 names).
pawl adds it as a classification of an ability's *structure* — produces mana and
targets nothing — never of a card's identity.

**The phased spine** (M3c/M3d structure: land the understood machinery, keep the
one interesting bet isolated):

1. **Activation onto the stack + the CR 605 classification.** `Source.OfAbility`,
   `Action.Activate`, `Pawl.Activate`, the ability-object, `{T}` cost, targeting
   at activation (CR 602.2b), resolve-then-cease, and the `isManaAbility`
   predicate with its two read-sites. Gate: **Prodigal Sorcerer** (ability on the
   stack, deals damage, ceases) and **Llanowar Elves** (mana ability, inline, no
   stack). Summoning sickness (CR 302.6) gates the `{T}` cost on creatures.
2. **Sacrifice cost + the `Search` opcode.** `SacrificeSelf`, the `Search` effect
   (library search prompt → put onto battlefield tapped → `Shuffle`). Gate:
   **Evolving Wilds**. Its ability is *not* a mana ability (it fetches a land but
   adds no mana), so it correctly uses the stack — the classification's false
   branch on a card that superficially looks mana-adjacent.
3. **`abilitiesOf` as a projection.** `LoseAllAbilities` (layer 6) strips
   activated abilities; activation legality reads the projection, never the
   printed list. Gate: **Humility** — a Humility'd Prodigal Sorcerer projects no
   abilities and cannot be activated.

M3e stays **one milestone, one spec, one plan**: the phases are ordered commits;
the plan owns the decomposition.

## Goal and scope

**Exit criterion.** Deterministic tests demonstrate all of:

- **Activation on the stack (Phase 1).** Activating Prodigal Sorcerer's `{T}`
  ability taps it, puts an `OfAbility` object on the stack, chooses "any target"
  (CR 602.2b), and on resolution deals 1 damage to that target and the ability
  **ceases to exist** (CR 608.2n) — it is not in any graveyard, and no card
  changed zones. CR 608.2b fizzle: an activation whose sole target became illegal
  ceases with no effect.
- **The mana-ability true branch (Phase 1).** Llanowar Elves' `{T}: Add {G}` is a
  mana source: its `{G}` pays part of a spell's cost, tapping the Elf, with **no
  ability ever on the stack** during payment. The falsifier: an engine that routes
  all activated abilities to the stack puts Llanowar Elves' ability there and
  cannot complete payment.
- **Summoning sickness (Phase 1, CR 302.6).** A freshly-cast Prodigal Sorcerer
  cannot activate its `{T}` ability (sick); after its controller's next untap
  step it can. A freshly-played Evolving Wilds (a land) *can* tap immediately.
  A sick Llanowar Elves is not a mana source.
- **Sacrifice + Search (Phase 2).** Activating Evolving Wilds taps and sacrifices
  it (CR 701.21 — the source goes to its graveyard), the ability goes on the
  stack (it is not a mana ability), and on resolution searches the controller's
  library for a basic-land card, puts it onto the battlefield **tapped**, and
  shuffles. Failure to find (CR 701.23b) is legal: shuffle only.
- **`abilitiesOf` projection (Phase 3).** Under Humility, Prodigal Sorcerer's
  activated ability is absent from `Projection.abilitiesOf`, so no `Activate` is
  offered. The falsifier: reading `Card.activatedAbilities` directly still offers
  it.

The `DecisionLog` replays deterministically with activation, targeting at
activation, cost payment, the search choice, and the mana-ability inline path in
the serialized path.

**Non-goals.**

- **No mana-source choice Prompt.** Llanowar Elves is the first source that makes
  two same-color mana sources *distinguishable* (tapping the Elf spends a
  creature — it cannot then block, attack, or re-tap — while a Forest cannot).
  `Mana.payCost`'s auto-tap elision ("EXPIRES the moment mana sources differ in
  any way a player could care about") is therefore now genuinely *reachable*.
  M3e keeps it valid **by construction**, not by paying the expiry: Llanowar Elves
  stays out of the random decks, and its fixtures are built so every payment taps
  *all* of a color's sources (forced, no choice) or the Elf is the sole source of
  its color. The general "choose which source" Prompt is the milestone's headline
  expiry (§9), tracked in git-bug so it cannot rot.
- **No standalone mana-ability activation.** Tapping a mana ability for floating
  mana with no immediate sink is strictly redundant in M3e (auto-tap at payment
  covers every case), so mana abilities are not offered as standalone `Activate`
  actions. Elision with an expiry (§9): the first card where floating mana matters
  (mana that triggers something, colour-filtering, an empty-pool sink).
- **No nested cast during Search.** M3e's `Search` is **atomic**: no player
  receives priority mid-resolution. Panglacial Wurm ("cast this while searching",
  CR 605.3a mana activation mid-resolution) is M3g's re-entrancy gate (§9).
- **No triggered abilities, no replacement pipeline.** CR 603/614 is M3f. M3e adds
  *activated* abilities only; nothing here emits or intercepts an event.
- **No loyalty abilities.** No planeswalkers exist, so `isManaAbility` omits the
  CR 605.1a loyalty clause safely (§2).
- **No mana in an ability's cost.** None of the three gates has a mana symbol in
  its *ability* cost. `AbilityCost` carries only additional (non-mana) costs;
  a `Maybe ManaCost` field is a named additive future addition (§9), not built now.
- **No last-known-information.** If a source leaves before its ability resolves,
  M3e stores the source id and a `DealDamage` from a vanished source is a no-op
  (the `powerOf` posture). Real CR 608.2g/113.7a LKI is deferred (§9).
- **No X, no modes, no multi-ability targeting subtleties, no activated abilities
  of objects outside the battlefield.** Untouched from M3d.

## 1. New and grown types

**`Pawl.Type.ActivatedAbility`** (new) — CR 602.1's "[cost]: [effect]":

```haskell
data ActivatedAbility = MkActivatedAbility
  { cost :: AbilityCost,
    -- Reuses the Effect vocabulary. Prodigal Sorcerer: [DealDamage slot 1].
    -- Llanowar Elves: [AddMana Green]. Evolving Wilds: [Search ...].
    effects :: [Effect],
    -- The same slot/target machinery as a spell (Card.targetSpecs). Prodigal
    -- Sorcerer: one AnyTarget slot. Llanowar Elves / Evolving Wilds: empty.
    targetSpecs :: Map SlotName TargetSpec
  }
  deriving (Eq, Ord, Show)
```

An ability is **value-typed, not identity-typed**: two abilities with the same
cost and effect are indistinguishable (activating either is the same act, like
tapping either of two Mountains), and abilities that differ are distinguished by
their value. This is why `Action.Activate` carries the ability *value*, not an
index or a name (§3).

**`Pawl.Type.AbilityCost`** (new):

```haskell
data AbilityCost = MkAbilityCost
  { additional :: [AdditionalCost]
    -- A `mana :: Maybe ManaCost` field is the named future addition (§9): no M3e
    -- gate has a mana symbol in its ability cost.
  }
  deriving (Eq, Ord, Show)
```

**`Pawl.Type.AdditionalCost`** (new):

```haskell
data AdditionalCost
  = TapSelf        -- CR 602.1a: the {T} symbol -- tap the source permanent.
  | SacrificeSelf  -- CR 701.21: sacrifice the source permanent.
  deriving (Eq, Ord, Show)
```

Both name **the source permanent**; neither takes an argument. Costs that name
another object, or a quantity, are future.

**`Pawl.Type.Effect`** grows two opcodes; `DealDamage` is reused:

```haskell
data Effect
  = DealDamage SlotName Quantity                 -- M3a (reused by Prodigal Sorcerer)
  | ModifyTarget Duration Modification SlotName  -- M3b/M3c
  | ChangeText SlotName                          -- M3d
  | AddMana ManaType                             -- NEW (Llanowar Elves)
  | Search CardCriterion                         -- NEW (Evolving Wilds)
  deriving (Eq, Ord, Show)
```

`AddMana mt` produces one unit of `mt`. It is executed only by `Mana.tapForMana`
(reading the `manaProduced` classification, §2), **never** on the stack — a mana
ability does not use the stack (CR 605.3b). `Search crit` searches the resolving
ability's controller's library for a card matching `crit`, puts it onto the
battlefield **tapped**, and shuffles (§5). The destination-and-tapped is baked
into the opcode as Evolving Wilds' exact shape; a more general search-then-move
composition is deferred (§9).

**`Pawl.Type.CardCriterion`** (new) — a first-order, analyzable predicate over a
card, as data (not a closure):

```haskell
data CardCriterion
  = BasicLandCard  -- CR 205.4c: a card with the Basic supertype and the Land type.
  deriving (Eq, Ord, Show)
```

Its one inhabitant is Evolving Wilds' target. Evaluated by a classifier reading a
card's projected/base characteristics (§5), never its identity.

**`Pawl.Type.Source`** grows the ability incarnation:

```haskell
data Source
  = OfCard Printing
  | OfAbility ObjectId ActivatedAbility  -- NEW: the source permanent + the ability
  deriving (Eq, Ord, Show)
```

`OfAbility src ab` is the stack object for an activated ability (CR 602.2a: the
ability is put on the stack, independent of its source). `src` is the source
permanent — the effects' source (CR 608.2g: "this creature deals 1 damage" comes
from the permanent), read by the executor. The ability travels *with* the object,
so it resolves even if `src` has left the battlefield (subject to the LKI expiry,
§9). `Game.cardOf` gains `OfAbility _ _ -> Nothing` (an ability is not a card);
consumers already tolerate `Nothing` (the fizzle, the bury).

**`Pawl.Type.Action`** grows activation:

```haskell
data Action
  = Pass
  | Play ObjectId
  | Cast ObjectId
  | Activate ObjectId ActivatedAbility  -- NEW: source permanent + the ability value
  deriving (Eq, Ord, Show)
```

`Activate src ab` carries the **ability value** (not an index), validated by
membership in `Projection.abilitiesOf src gs` — the same content/membership
posture the codebase uses for target recipients (`Set.member`, "by name, never by
position"). The value flows straight into `OfAbility src ab` with no index
indirection; Humility stripping the ability makes the membership check fail for
free (§3, §6).

**`Pawl.Type.Card`** grows the printed abilities:

```haskell
  activatedAbilities :: [ActivatedAbility]  -- CR 602. Empty for all but the gates.
```

**`Pawl.Type.Prompt`** grows the library search:

```haskell
  -- CR 701.23 / 701.23b. The [ObjectId] is the library cards MATCHING the
  -- criterion (the engine pre-filters to legal choices); Nothing is "fail to
  -- find," which the rules always permit for a search of one's own library.
  SearchLibrary :: Decider -> PlayerId -> [ObjectId] -> Prompt (Maybe ObjectId)
```

Reveal/hidden-information nuance is out of scope: a player searching their own
library sees it. Reuses the existing `Shuffle` prompt afterward.

**`Pawl.Card`** gains three printings: **Prodigal Sorcerer** (`{2}{U}`, one
`{T}`-cost ability `[DealDamage slot 1]` with an `AnyTarget` slot), **Llanowar
Elves** (`{G}`, one `{T}`-cost ability `[AddMana Green]`, no slots), and
**Evolving Wilds** (Land, one `{T}`+`SacrificeSelf` ability `[Search
BasicLandCard]`, no slots). Humility is unchanged from M3b/M3c.

## 2. The CR 605 classification — one predicate, two sites

`Resolve.manaProduced :: Effect -> Maybe ManaType` joins `slotsOf` as a
`case effect of` classifier — **`Resolve` stays the sole home of `case effect
of`.** It reads `AddMana mt -> Just mt`; every other opcode `-> Nothing`. This is
the risk register's "produces mana?" ABI bit landed concretely (`docs/design.md`
§7, `prior-art-lessons.md` §1).

`Mana.isManaAbility :: ActivatedAbility -> Bool` — CR 605.1a, minus the loyalty
clause (no planeswalkers):

```haskell
isManaAbility ab =
  not (null (Maybe.mapMaybe Resolve.manaProduced (ActivatedAbility.effects ab)))
    && Map.null (ActivatedAbility.targetSpecs ab)
```

"Could add mana" (any `AddMana` effect) **and** "doesn't target" (no slots). The
predicate is read at exactly two sites, and nowhere else:

- **Site 1 — `Mana.manaTypesOf` (include).** A permanent's mana types are now the
  intrinsic subtype mana (CR 305.6, unchanged) **plus**, for each *projected*
  activated ability that `isManaAbility`, the mana its `AddMana` effects produce.
  So Llanowar Elves is a green source, auto-tapped at payment exactly like a
  Forest. `Mana.tapForMana` taps and adds the produced type; because a mana
  ability produces exactly one type here, the existing single-type elision holds.
- **Site 2 — `Action.legalActions` (exclude).** `Activate` is offered only for
  projected abilities where `not (isManaAbility ab)`. A mana ability never becomes
  a stack action; it is handled at payment. This is the fork: the naive "offer
  every activated ability" would route Llanowar Elves' ability to the stack, a CR
  605.3b violation that deadlocks payment.

`Resolve.applyEffect` gains an `AddMana` arm that is a **documented no-op**: "CR
605 — a mana ability never resolves on the stack; `AddMana` is applied by
`Mana.tapForMana` at payment. Reaching this arm means a mana ability was wrongly
put on the stack — a classification bug." This keeps `applyEffect` total without a
`Resolve → Mana` import cycle (the mana pool primitives live in `Mana`; the
classification lives in `Resolve`; `Mana` imports `Resolve` for the classifier,
never the reverse). `slotsOf` gains `AddMana _ -> Set.empty`.

## 3. Activation and resolution

A new module **`Pawl.Activate`** mirrors `Pawl.Cast`. Its shape follows CR 602.2
(announce → choose targets → pay costs) and the `Cast` conventions
(reject-not-repair, keep priority):

- **`Activate.activatable :: PlayerId -> ObjectId -> ActivatedAbility ->
  GameState -> Bool`** — the enumeration predicate: the player controls the
  source, the ability is a member of `Projection.abilitiesOf src gs` (so a
  Humility'd ability is not activatable), it is `not . isManaAbility` (mana
  abilities are handled at payment), every additional cost is payable
  (`TapSelf` → source untapped and, if a projected creature, not summoning-sick
  per CR 302.6; `SacrificeSelf` → the source is on the battlefield), every target
  slot has a legal recipient (CR 602.2b, reusing `Target.legalSets`), and the
  timing is legal (activated abilities are instant-speed by default, CR 602.3a —
  any time the controller has priority; the empty-stack requirement of sorcery
  speed does *not* apply). `Action.legalActions` maps this over each controlled
  permanent's projected non-mana abilities.
- **`Activate.activateAbility :: PlayerId -> ObjectId -> ActivatedAbility ->
  Game ()`** — mirror of `Cast.castSpell`: mint a fresh ability object on the
  stack (`Game.freshObjectId`, a new `Object` with `source = OfAbility src ab`,
  `zone = Stack`, `owner = the ability's controller`), prompt `ChooseTargets`
  keyed to the new object's id and stamp them (CR 602.2b), then pay the additional
  costs (tap the source, and/or sacrifice it via `Game.changeZone src Graveyard`).
  An illegal target answer makes the whole activation a no-op (reject-not-repair),
  and — like casting — enumeration only offers activations whose costs are payable,
  so payment cannot fail after the prompt (the same elided rewind `Cast` names).
  Keeps priority (CR 117.3c).

`Engine.priorityLoop` gains an `Activate src ab` arm beside `Cast`, structurally
identical: run `Activate.activateAbility`, reset passes, keep priority with the
acting player.

**Resolution.** `Stack.resolveTop` gains an `OfAbility` arm dispatching to
`Resolve.resolveAbility` — the dispatch is on *what kind of stack object* it is (a
classification, exactly like the existing is-it-a-permanent split), never on card
identity:

```haskell
Source.OfAbility src ab -> Resolve.resolveAbility oid src ab
```

**`Resolve.resolveAbility :: ObjectId -> ObjectId -> ActivatedAbility ->
Game ()`** — the CR 608 executor for an ability. It is `Game`-monadic (like
`resolveSpell`, because `Search` prompts at resolution, §5). It reuses
`applyEffect` with the **source permanent** (`src`) as the effect source (so
`DealDamage`'s damage comes from Prodigal Sorcerer, and any deathtouch/keyword
read hits the permanent, CR 608.2g), folding over the ability's effects with the
same per-slot legality and CR 608.2b fizzle as `resolveSpell`. The one difference
from a spell: when resolution finishes, the ability object **ceases to exist**
(CR 608.2n) — removed from the stack and from `GameState.objects` — rather than
`changeZone`-d to a graveyard. An ability has no owner's graveyard to go to; it is
not a card.

Targets are stamped on the ability's own `Object.targets` (the same field a spell
uses) and re-validated at resolution through `Target.stillLegal` (CR 608.2b),
unchanged from M3d. `Object.chosenSubtypes` stays empty (no text-changer among the
gates).

## 4. Costs and summoning sickness

**Paying additional costs.** At activation, each `AdditionalCost` is paid against
the source permanent:

- `TapSelf` — set the source `Tapped`. Enumeration requires it `Untapped`.
- `SacrificeSelf` — `Game.changeZone src Zone.Graveyard` (CR 701.21 puts a
  sacrificed permanent into its owner's graveyard). Enumeration requires it on the
  battlefield.

Both are paid as part of activation and are irrevocable once the ability is on the
stack (there is no cost that fails after the ability is announced, given
enumeration).

**Summoning sickness (CR 302.6).** A creature's activated ability whose cost
includes the tap symbol cannot be activated unless its controller has controlled
it continuously since their most recent turn began. The gate:

- Reads **projected creature-ness** (`Projection.cardTypesOf`): the restriction is
  creature-specific. A land with a `{T}` cost — Evolving Wilds — has **no**
  sickness restriction and can tap the turn it is played.
- Applies only when the cost contains `TapSelf`. An ability with no tap symbol is
  unaffected (none in M3e, but the classification is stated so the plan does not
  bake tap-ness into the sickness read).
- Reuses `Object.sickness` and the existing `settleAll` untap-step transition
  (M1b): a permanent is `Settled` once its controller's untap step has passed.

`Mana.manaSources` gains the same exclusion: a summoning-sick creature whose mana
ability requires `{T}` is not a mana source (a freshly-cast Llanowar Elves cannot
tap for `{G}` this turn). This is the mana-side reading of the same CR 302.6 gate,
so both `Activate` enumeration and mana payment consult one classification.

## 5. Search

The `Search BasicLandCard` opcode, resolving inside `resolveAbility`
(Evolving Wilds' ability is on the stack; it resolves like any non-mana ability):

- **`Resolve.matchesCriterion :: CardCriterion -> Card -> Bool`** — the classifier
  for `CardCriterion`, casing on the criterion (Resolve's charter) and reading a
  card's characteristics via existing helpers (`Card.isLand` plus a Basic-supertype
  read), never its identity. `BasicLandCard` → a card that is a Land with the
  Basic supertype (CR 205.4c / 305.6 subtype list).
- **The prompt.** `applyEffect` for `Search crit` gathers the controller's library
  members matching `crit`, prompts `SearchLibrary decider controller matches`, and
  on `Just found` moves that card onto the battlefield **tapped**
  (`Game.changeZone found Zone.Battlefield` then set `Tapped`), on `Nothing`
  does nothing (fail to find). Then it shuffles the library (the existing
  `Shuffle` prompt over the library members). CR card text: shuffle happens whether
  or not a card was found.
- **Atomicity.** No priority is granted between the search and the shuffle; the
  whole `Search` is one `applyEffect` step. The nested-cast-during-search
  re-entrancy (Panglacial Wurm) is M3g (§9).

`applyEffect` for `Search` needs the monadic prompt, but `applyEffect` is
currently pure (`GameState -> ... -> GameState`). Two options for the plan; the
spec fixes the **first**: give `resolveAbility`/`resolveSpell` a monadic effect
loop for prompting opcodes (`Search`) while keeping pure opcodes pure, OR carry
the search choice as a pre-resolved binding like `chosenSubtypes`. The spec
chooses the **monadic-resolution** path here, because it is the honest shape a
resolving spell/ability needs the moment any resolution prompts (Search is the
first; scry, choose-a-mode, and fetch-to-hand all follow), and the cast-time
binding trick does not generalize to a search whose legal set depends on the
library *at resolution*. `resolveSpell`/`resolveAbility` become `Game`-monadic;
`Stack.resolveTop` and the `priorityLoop` resolution site lift accordingly. This
retires the "pure `Resolve`" posture M3d preserved — a named transition, not a
regression (§9). (Plan note: keep the pure `applyEffect` arms pure and thread only
the prompting arm through the monad, so DealDamage/ModifyTarget/ChangeText are
untouched.)

## 6. `abilitiesOf` — a projection from day one

`Projection.abilitiesOf :: ObjectId -> GameState -> [ActivatedAbility]` — a
permanent's activated abilities after the layer system, the exact `keywordsOf`
move (M2a): start from `Card.activatedAbilities`, and apply the layer-6
`LoseAllAbilities` modification (M3b) — if the projection strips a permanent's
abilities, `abilitiesOf` returns `[]`. Everything that asks "what can this
activate?" reads `abilitiesOf`, never `Card.activatedAbilities`:

- `Action.legalActions` / `Activate.activatable` enumerate over `abilitiesOf`.
- `Activate.activateAbility` validates the chosen ability by membership in
  `abilitiesOf` (§3), so a Humility'd Prodigal Sorcerer's ability — absent from
  the projection — fails the check and cannot be activated.

`LoseAllAbilities` already strips keywords (M3b) and static abilities (M3b/M3c);
M3e extends the same modification to activated abilities, folded at layer 6. No
new modification, no new layer. Humility is `AllCreatures LoseAllAbilities`, so a
land's abilities are untouched — the intrinsic mana ability (CR 305.6, driven by
`subtypesOf`, not `abilitiesOf`) is not in scope for stripping here; a future
"lands lose all abilities" card would need the intrinsic mana ability routed
through `abilitiesOf` too (§9).

## 7. Invariants preserved

- **The two-halves invariant holds through activation.** `Stack.resolveTop`
  dispatches on the `Source` classification (`OfCard` permanent/spell vs
  `OfAbility`) — *what kind of stack object*, never card identity, the same shape
  as the existing is-it-a-permanent split. The CR 605 `isManaAbility` predicate is
  a classification of an ability's *structure*; the CR 302.6 sickness gate and the
  `Search` criterion read projected *characteristics*. No rules-core module cases
  on a card's or an effect's identity: `Resolve` remains the sole home of `case
  effect of` (now `slotsOf`, `manaProduced`, `matchesCriterion`, `rewriteEffect`,
  `applyEffect`).
- **The engine makes no choice it should not, and elides none it should.** Targets
  at activation are prompted (`ChooseTargets`); the search is prompted
  (`SearchLibrary`, including fail-to-find). Two elisions, each with a named
  expiry (§9): the mana-source choice (kept valid by construction) and standalone
  mana-ability activation (redundant in M3e's pool).
- **Value-typed abilities.** `Action.Activate` carries the ability value and
  validates by membership, matching the codebase's content/membership posture for
  targets (never positional). No index, no fragile alignment.
- **Conventions.** One type per module (`ActivatedAbility`, `AbilityCost`,
  `AdditionalCost`, `CardCriterion` each new); `NamedFieldPuns` per the M3b
  amendment; new sum-type constructors take no `Mk`-pun; `Mk`-prefixed record
  constructors for the new records.

## 8. Setup, decks, and testing

Testing follows the phased spine (§0): activation + the classification, then
sacrifice + search, then the projection strip.

**Phase 1 — activation onto the stack + the CR 605 classification.**

- **Prodigal Sorcerer.** On the battlefield and settled: activate `{T}`, assert
  an `OfAbility` object is on the stack and the source is tapped; resolve, assert 1
  damage dealt to the chosen target (a creature and a player, both `AnyTarget`) and
  the ability object is **gone** (not in any graveyard; object count returns).
  CR 608.2b: activate targeting a creature that dies before resolution → the
  ability ceases with no effect. Test names cite CR 602/608.2n/608.2g.
- **Llanowar Elves — the true branch.** Settled Elf: cast a green creature whose
  cost the Elf's `{G}` helps pay; assert the Elf is tapped, the spell is on the
  stack, and **no `OfAbility` object was ever created** (the stack held only the
  spell). Contrast: assert `isManaAbility` is `True` for the Elf's ability and
  `False` for Prodigal Sorcerer's and Evolving Wilds'. Falsifier articulated in a
  comment: routing all abilities to the stack deadlocks this payment.
- **Summoning sickness (CR 302.6).** Freshly-cast Prodigal Sorcerer: `Activate` is
  not offered (sick); after its controller's untap step, it is. Freshly-cast
  Llanowar Elves is not a mana source; after untap, it is. Freshly-played Evolving
  Wilds (Phase 2) *can* tap immediately (a land, no sickness).

**Phase 2 — sacrifice + Search.**

- **Evolving Wilds.** Activate `{T}, Sacrifice`: assert the source is in its
  owner's graveyard and an `OfAbility` object is on the stack (it is **not** a
  mana ability — assert `isManaAbility` `False`, and that it used the stack).
  Resolve: search the library for a basic land, assert it is on the battlefield
  **tapped**, the library is one smaller and shuffled. Fail-to-find (choose
  `Nothing`): the library is only shuffled, nothing enters. Test names cite CR
  701.21/701.23/305.6.

**Phase 3 — `abilitiesOf` projection.**

- **Humility + Prodigal Sorcerer.** With Humility on the battlefield,
  `Projection.abilitiesOf prodigal gs == []`, and `Activate` is not offered for it.
  Falsifier: reading `Card.activatedAbilities` still offers the ability. Also
  assert the Elf under Humility (if a creature-typed mana source is placed) loses
  its mana source status via `abilitiesOf` — the mana-side reading of the same
  strip. Test names cite CR 613 layer 6 / 605.

**Setup and decks.** `emptyGame` unchanged. Fixtures place the gate permanents on
the battlefield (settled or sick as the test requires) and activate through the
stack, assigning timestamps from `nextTimestamp` as M3c/M3d do. All three gate
cards are **deterministic fixtures, out of the random decks** — Prodigal Sorcerer
and Llanowar Elves have no same-color matchup, and keeping Llanowar Elves out of
the random green deck is what preserves the auto-tap elision (§ Non-goals). The
fuller tail (a matchup exercising activation in random games, which needs the
mana-source Prompt anyway) is deferred past M3 (§9).

**Properties** (`runMatch`, both matchups): every M2d/M3a–M3d invariant as it
stands — conservation, termination, ids, no floating mana at end of step, life
never increases, combat happens, green-black engagement. Replay determinism now
covers activation, targeting at activation, cost payment, and the search choice.
The benchmark stays on `redDeck`; throughput is watched for the added
`abilitiesOf`/`isManaAbility` per-enumeration cost, not asserted.

## 9. What M3e preserves, and the expiries it opens

**Preserves:** the two invariants (§7), the numeric/mana/timestamp models, the
M3b–M3d projection shape and source-liveness, the deterministic-fixture posture
for off-color cards.

**Expiries this milestone opens:**

- **The mana-source choice Prompt — the headline expiry.** Llanowar Elves makes
  two same-color sources distinguishable, so `Mana.payCost`'s auto-tap can no
  longer silently choose among them in general. M3e keeps the elision valid *by
  construction* (Elf out of random decks; fixtures tap all of a color's sources or
  make the Elf the sole source). The general "player chooses which sources to tap"
  (CR 601.2g / 602) forces a split of pure `canPay` (affordability, for
  enumeration) from a monadic `payCost` (the choice) and a new Prompt. **Tracked
  in git-bug so it cannot rot**; due when any scenario genuinely offers
  distinguishable same-color sources the engine must resolve — the first random
  deck mixing a creature mana source with lands, or a fixture that needs it.
- **Standalone mana-ability activation.** Tapping a mana ability for floating mana
  with no sink is elided (auto-tap at payment covers M3e). Expires at the first
  card where floating mana matters (a trigger on mana, colour-filtering, an
  empty-pool sink).
- **Nested cast during Search (re-entrancy).** M3e's `Search` is atomic. Panglacial
  Wurm (cast mid-search, CR 605.3a mana activation mid-resolution) is M3g's gate;
  it reopens `Search` to grant priority / accept a nested cast during resolution.
- **Last-known information.** A source that leaves before its ability resolves:
  M3e stores the source id and a `DealDamage` from a vanished source no-ops (the
  `powerOf` posture). CR 608.2g/113.7a real LKI (a snapshot of the source at
  activation) is deferred, pressured first by a real LKI-dependent card and by
  M3f's event pipeline.
- **Mana in an ability's cost.** `AbilityCost` carries only additional costs; a
  `mana :: Maybe ManaCost` field is an additive addition, first needed the moment
  an ability costs mana (abundant at M4).
- **Monadic resolution retires the pure-`Resolve` posture.** `Search` is the first
  opcode that prompts at resolution, so `resolveSpell`/`resolveAbility` become
  `Game`-monadic (§5). This is the transition M3d's "no monadic `Resolve`" expiry
  named; scry, modal choices, and fetch-to-hand ride the same seam afterward.
- **`Search` generality.** M3e's opcode is tutor-to-battlefield-tapped for a
  basic land. Other shapes (search to hand, reveal, other criteria, put untapped,
  optional-vs-mandatory find) generalize `Search`/`CardCriterion`/the destination
  when a second search card disagrees with Evolving Wilds' baked shape.
- **Intrinsic mana ability under `LoseAllAbilities`.** Humility is `AllCreatures`,
  so a land's CR 305.6 mana ability (driven by `subtypesOf`, not `abilitiesOf`) is
  never stripped here. A future card that removes all abilities from a land needs
  the intrinsic mana ability routed through `abilitiesOf` too — a small
  unification, unforced now.
- **`OfAbility` / `OfCard` and the `cardOf` `Nothing` path.** `Game.cardOf`
  returns `Nothing` for an ability; every consumer already tolerates it. A future
  need to read an ability's "characteristics" (a copy of an ability, a triggered
  ability's source characteristics) extends the `Source` reads uniformly.

**Explicitly deferred past M3e:**

- **The M3 remaining letters** — M3f (the 603/614 event pipeline, Rest in Peace),
  M3g (Decider + re-entrancy, Mindslaver + Panglacial Wurm — both build directly
  on this letter's activation).
- **A matchup for random activated-ability coverage** — the post-M3 tail, blocked
  on the mana-source Prompt.
- **X, modes, counterspells, Auras/Equipment (Attach), new card types,
  serialization / AST version field.**
