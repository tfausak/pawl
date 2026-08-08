# Keyword family filters (#522)

## The problem

`Filter.HasKeyword` carries a whole `Keyword` and `Engine.Filter.matches` answers it
with `Set.member k (keywords view)`. For a nullary keyword that is exactly right:
`HasKeyword Flying` is "a creature with flying" (CR 702.9). For a parameterized one it
asks the narrower question -- `HasKeyword (Toxic 2)` matches toxic 2 and not toxic 3,
`HasKeyword (Landwalk (HasSubtype Swamp))` matches swampwalk and not islandwalk. Card
text that narrows by the FAMILY has no spelling.

## What the CR says

The CR makes the family/instance split itself, in the two places the producers come
from:

- CR 702.14a: "Landwalk is a **generic term** that appears within an object's rules
  text as '[type]walk'". The generic term and the written instance are different
  things by the rule's own wording.
- CR 702.164a: "Toxic is a static ability. It is written 'toxic N,' where N is a
  number." The ability is *toxic*; N is how it is written.
- CR 702.1: "the object lists only the name of the ability as a 'keyword'".

So the family atom asks about the ability and the exact atom about the written
instance. Both are rules concepts, not conveniences.

## Both forms have producers

- FAMILY -- **Flensing Raptor** (ONE, `{2}{W}`, 2/2 Phyrexian Bird): "Flying. Toxic 1.
  When this creature enters, another target creature you control with toxic gets +1/+1
  and gains flying until end of turn."
- EXACT -- **Quagmire** (LEG, `{2}{B}` Enchantment): "Creatures with swampwalk can be
  blocked as though they didn't have swampwalk." Swampwalk specifically, not the
  landwalk family.

Quagmire is what forbids collapsing the two into one atom.

## Design

### `Pawl.Types.KeywordFamily`

A new type in the `types` sublibrary, one nullary constructor per PAYLOAD-CARRYING
`Keyword` constructor, each citing its CR 702 subrule:

    data KeywordFamily
      = Hexproof   -- CR 702.11
      | Landwalk   -- CR 702.14
      | Cycling    -- CR 702.29
      | Flashback  -- CR 702.34
      | Morph      -- CR 702.37
      | Entwine    -- CR 702.42
      | Poisonous  -- CR 702.70
      | Crew       -- CR 702.122
      | Toxic      -- CR 702.164

It imports NOTHING. That is load-bearing: `Pawl.Types.Filter` can name it concretely
while keeping its `keyword` parameter for the exact atom, so the existing knot
(`Filter Keyword` / `Cost Keyword`, 123 mentions across 40 files) is untouched.

Nullary keywords deliberately get NO constructor. The two atoms then partition rather
than overlap: "with flying" has exactly one spelling (`HasKeyword Flying`) and "with
toxic" has exactly one (`HasKeywordFamily Toxic`).

### `Pawl.Types.Filter`

    | HasKeywordFamily KeywordFamily.KeywordFamily

Sibling of `HasKeyword`, and payload-free by construction -- which is the property the
rejected designs lacked. There is no `HasKeywordFamily` value that is observably equal
to another and structurally different.

### `Pawl.Engine.Keyword.familyOf`

    familyOf :: Keyword.Keyword -> Maybe KeywordFamily.KeywordFamily

Exhaustive, NO wildcard. A sibling of the classifiers already there
(`castingPermissionsOf`, `morphCost`, `mintsEntryReplacement`). It cannot live in
`Pawl.Types.KeywordFamily` -- that module naming `Keyword` reopens the cycle -- and
`Pawl.Types.*` holds no cross-type logic.

### `Pawl.Engine.Filter.matches`

    Filter.HasKeywordFamily f ->
      any ((Just f ==) . Keyword.familyOf) (keywords view)

Computed, not stored: `ProjectedCharacteristics` gains no field. Correctness over
performance, and a stored set would be one more derived-state sampling hazard.

### Codec

New `Pawl.Codec.KeywordFamily` and its spec; a `HasKeywordFamily` arm in
`Pawl.Codec.Filter` and `Pawl.Codec.FilterSpec`. `pawl.cabal` regenerated with
`cabal-gild pawl.cabal` directly -- two new modules, which `hooky fix` will not pick up.

### The card

`data/cards/flensing-raptor.json`: `{2}{W}`, 2/2 Phyrexian Bird, `Flying`, `Toxic 1`,
and a `SelfEnters` trigger with one target slot narrowed by

    And [HasCardType Creature, ControlledBy You, Not IsSource, HasKeywordFamily Toxic]

and two effects on that same slot, `ObjectRef.InSlot` making "one target, two
modifications" expressible: `ModifyTarget UntilEndOfTurn (ModifyPowerToughness 1 1)`
and `ModifyTarget UntilEndOfTurn (GainKeyword Flying)`. `Not IsSource` is CR 601.2c's
"another".

## Rejected

- **`HasKeyword` carries a payload-free tag, no exact atom.** The prize: it would
  dissolve the `keyword` parameter from `Filter`, `Cost`, `CostComponent` and
  `Keyword`. Quagmire kills it -- `Landwalk (HasSubtype Swamp)` must stay askable.
- **`HasKeyword (KeywordPattern)` with `Exactly kw | Family f`.** The shape that reads
  best. It puts `Keyword` back inside `Filter`, so it needs a SECOND level of
  parametricity to tie the knot, and rewrites all 73 `Filter Keyword` sites.
- **`Toxic (Maybe Natural)`.** `Projection.totalToxic` and
  `Combat.landwalkAllowsGiven` would both have to answer the `Nothing`-vs-`Just 0`
  question CR 702.164b never asks.
- **A per-keyword "ignore the payload" rule inside `matches`.** Makes
  `HasKeyword (Toxic 2)` and `HasKeyword (Toxic 3)` observably the same value.
- **`Data.Data`/`toConstr` instead of a parallel enum.** The designator must be
  payload-free, typed, `Eq`/`Ord`/`Show` and codec-able. `Constr` is none of those, and
  it forces `Data` instances through `Filter`, `Cost` and `CostComponent`.
- **One atom per family in `Filter` (`HasAnyToxic`, ...).** Smallest diff, but the
  concept never gets a name, and `Filter` plus the codec accrete a near-duplicate arm
  per family. Three producers are already named.

## The maintenance cost, stated plainly

`docs/rules.txt` has 194 CR 702 keyword subrules; 64 describe a parameter in their
first subrule alone. `Pawl.Types.Keyword` models 34 of the 194, 9 of them
payload-carrying. So `KeywordFamily` will track roughly a third of `Keyword`'s growth
-- ward N, annihilator N, bushido N, casualty N, and the alternative-cost crowd all
land in it eventually. Nine is a snapshot of pawl's coverage, not a fact about Magic.

This IS the "second keyword enumeration to keep in step with CR 702" that
`Pawl.Types.CounterKind`'s haddock refuses. What makes it payable here and not there:
`familyOf` is exhaustive with no wildcard, so adding a `Keyword` constructor is an
incomplete-patterns error under `-Werror` until the author decides `Just` or `Nothing`.
`CounterKind`'s enum would have been a hand-maintained subset of CR 122.1b's fifteen
keywords with nothing to catch drift.

Representability is not capability: `HasKeywordFamily Crew` is representable and no
card prints it, exactly as `CounterKind.Keyword Defender` is.

**Rule, recorded in `CLAUDE.md`:** a `KeywordFamily` constructor lands with the
payload-carrying `Keyword` constructor, not with the first card that asks for it.
`-Werror` catches the missing `familyOf` arm but not a missing constructor -- an author
can write `Nothing` and move on.

## Verification

- Gameplay test: Flensing Raptor enters; creatures with toxic 1, toxic 2 and toxic 3
  are all legal targets; a vanilla creature and the Raptor itself are not.
- `FilterSpec` unit pair pinning family against exact: `HasKeyword (Toxic 2)` rejects
  toxic 3 while `HasKeywordFamily Toxic` accepts it. This is what keeps Quagmire's
  question askable, and it sits alongside the existing `PowerAtLeast`/`PowerAtMost`
  pinning.
- Mutation: change `familyOf`'s `Toxic` arm to `Nothing` and confirm both the gameplay
  test and the family half of the unit pair fail; change `matches` to ignore the family
  and confirm the exact half fails.
- Repo-wide `ormolu --mode check $(git ls-files '*.hs')` before pushing.

## Out of scope, staying open

- **Quagmire and Staff of the Ages** both need blocking-restriction removal, so
  `Landwalk` gets a family constructor and no producer this cycle.
- **#920** is unblocked, not closed: Backslide still needs the turn-face-down effect.
- **Slaughter Singer** still needs a "whenever another creature you control with toxic
  attacks" `TriggerCondition`; `TriggerCondition` has only `SelfAttacks`.
