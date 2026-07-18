# M3d text-changing (layer 3 / the rewritable AST) — design

Design for milestone **M3d**, the fourth letter of M3 (see the split table in
`docs/design.md`): **layer 3 — the rewritable effect AST**, gated by Magical
Hack. This is the §5 *canary*: XMage lists text-changing effects as "very
unlikely" and ships none of the named ones, because its rules live inside
compiled Java and there is nothing there to rewrite. pawl's cards are a data AST
loaded at runtime (`docs/design.md` §2.7, §5), so text-changing is a tree
transformation on a value already in hand. **The go/no-go verdict for M3 arrives
at the end of M3d** (`docs/design.md`, M3 ordering note): if the effect AST is
not rewritable, we find out at card #3, not card #8,000.

The gate card, Scryfall-verified (`api.scryfall.com/cards/named`, fetched
2026-07-18):

| Card | Cost | Type line | Oracle text |
|---|---|---|---|
| **Magical Hack** | `{U}` | Instant | "Change the text of target spell or permanent by replacing all instances of one basic land type with another. (For example, you may change \"swampwalk\" to \"plainswalk.\" This effect lasts indefinitely.)" |

The reminder text is load-bearing: "swampwalk to plainswalk" is a *rules-text*
change, and CR 612.1 covers "any words or symbols printed on that object …
generally … that object's rules text … and/or the text that appears in its type
line." So M3d is genuinely both halves — the type line *and* the rules text.

Two further cards appear, both already in the repo from M3c: **Blood Moon**
(`{2}{R}` Enchantment, "Nonbasic lands are Mountains.",
`MkStaticAbility AllNonbasicLands (SetLandSubtype Mountain)`) and **Urborg, Tomb
of Yawgmoth** (`AllLands (AddLandSubtype Swamp)`). They are the *ability-AST*
demonstrators — hacking them rewrites a basic-land-type word *inside a static
ability*, the part that beat XMage.

This is a types-and-architecture spec, not an implementation plan.

## 0. The core idea and the phased spine

**M3d is full-depth text-changing decomposed into pieces.** One new layer-3
modification — `ChangeSubtypeWord from to` (the first `Layer.Text` producer) — is
read at **three** points, and that is what proves the canary at full depth:

| Read-point | Where | Mechanism | Gate it lights |
|---|---|---|---|
| **Type line** | within an object's projection fold (layer 3, before layer 4) | rewrite the object's projected `subtypes` | a basic Mountain projects `{Island}` → CR 305.6 taps `{U}` |
| **A source's abilities** | `gather`-time, before the source's effect is folded onto others | rewrite basic-land-type words inside the source's static-ability `Modification`s | hack Blood Moon `SetLandSubtype Mountain` → `SetLandSubtype Island` → nonbasic lands tap `{U}` |
| **A spell's one-shot effects** | resolve-time | rewrite basic-land-type words inside the resolving spell's `Effect`s | a *"target land becomes Swamp"* spell hacked on the stack resolves as **Mountain** |

Read-point 1 is the "ordinary layer 3" XMage already does (it rewrites
structured subtypes/colors fine; `docs/design.md` §5). Read-points 2 and 3 are
the part XMage *cannot* do — rewriting a word *inside an ability or effect*.
Because every consumer (mana via CR 305.6, layer 4, the resolver) already reads a
projection, all three cash out "with zero special cases" — the falsifier
(`docs/design.md` M3d row).

**The phased spine** (M3c's structure: land the understood machinery, write the
go/no-go test failing, then land the bet, isolated):

1. **Layer-3 machinery + type-line rewrite**, unit-tested via directly-built
   effects — no casting, no targeting, no binding. `ChangeSubtypeWord` applier,
   folded before layer 4. Low risk; committed first.
2. **Magical Hack, castable** — the value-choice binding, the "spell or
   permanent" target, the `ChangeText` opcode, the `Indefinite` duration. Cast on
   a basic Mountain → new mana color. The design's *stated* falsifier,
   end-to-end.
3. **The go/no-go — the ability-AST rewrite.** `gather` applies text-changes to a
   source permanent's static-ability `Modification`s. **Test-first (failing),
   then land it**: hack Blood Moon → nonbasic lands tap `{U}`, both timestamp
   orders. Isolated — if it cannot be done, phases 1–2 are still sound, committed,
   and useful; the failure is quarantined here.
4. **The positive spell-on-the-stack rewrite.** The resolver reads *projected*
   effects; targeting reaches stack spells; the fixture spell hacked on the stack
   (`Swamp`→`Mountain`) resolves as Mountain. Plus the CR 400.7 **negative** test
   (hack a permanent spell → the change is lost when it resolves into a new
   object).

M3d stays **one milestone, one spec, one plan**: the phases are the "pieces,"
each a small ordered commit; the plan owns the decomposition.

## Goal and scope

**Exit criterion.** Deterministic tests demonstrate all of:

- **Type line (Phase 1–2).** A basic Mountain under a `ChangeSubtypeWord Mountain
  Island` effect projects subtype `{Island}` and taps **`{U}` only**; via a cast
  Magical Hack (Phase 2) the same holds end-to-end (choose Mountain→Island,
  target the land).
- **Ability AST — the go/no-go (Phase 3).** Blood Moon hacked `Mountain`→`Island`
  (Magical Hack targeting Blood Moon, a permanent): every nonbasic land projects
  `{Island}` and taps **`{U}`**, in **both** timestamp orders of Blood Moon vs.
  the hack. Falsifier: an engine that reads Blood Moon's *printed* ability leaves
  nonbasic lands as Mountains (`{R}`).
- **One-shot effect AST — the stack (Phase 4).** A fixture instant "target land
  becomes a Swamp" (`ModifyTarget (SetLandSubtype Swamp)`) hacked on the stack
  `Swamp`→`Mountain` resolves making the target land a **Mountain** (taps `{R}`),
  because the resolver reads the *projected* effect. The negative CR 400.7 test:
  hacking a *permanent* spell on the stack leaves no effect once it resolves (new
  object, the text-change's fixed set no longer names it).

The `DecisionLog` replays deterministically with the projected type line, the
projected effects, and the cast-time land-type binding in the serialized path.

**Non-goals.**

- **No layer-3 changes beyond basic-land-type words.** Magical Hack changes only
  *basic land type* words (CR 612.2, "used as a land type"). Color words (Sleight
  of Mind), creature types (Artificial Evolution), names (612.6–612.9), and text
  swaps (612.5) are out — each its own future card, no producer here.
- **No landwalk / evasion.** The Oracle example ("swampwalk to plainswalk") is a
  rules-text change on a *keyword ability with a land-type parameter*. Landwalk
  is unimplemented (a `docs/design.md` M2 punchlist keyword) and is its own axis;
  M3d demonstrates rules-text rewriting via **Blood Moon's static ability** and
  the **fixture spell's one-shot effect** instead, both of which stay inside
  M3d/M3c vocabulary. A landwalk creature as a text-change target is a post-M3d
  test (expiry, §8).
- **No real-card positive stack demonstrator.** Real non-Aura instants/sorceries
  that set a land's basic type essentially do not exist (they are Auras, needing
  M4 `Attach`, or `Destroy all [type]` cards needing M4 `Destroy`). The Phase-4
  positive demonstrator is therefore a **labeled synthetic fixture** — the
  project-sanctioned expiring crutch (memory: tests-prefer-real-cards) — with an
  expiry naming the first real card that replaces it (§8).
- **No resolution-time prompting / monadic `Resolve`.** The land-type choice is
  bound **at cast** (Cast is already monadic; `Resolve` stays pure). Legitimate
  only because the choice is indistinguishable between cast and resolution here
  (§3); a documented expiry names the first card that forces a genuine
  resolution-time choice and thus a monadic `Resolve`.
- **No other layers' producers.** Layers 1 (copy), 2 (control), 5 (color), 7a
  (CDA), 7d (P/T switch) still have no producers; the `Layer` enum already lists
  them (M3b, diffability). M3d adds only the **Text** (layer 3) producer.
- **No 603/614 event pipeline, no activated/triggered abilities, no X, no modes,
  no counterspells, no Auras/Equipment, no new card types, no cross-query
  memoized projected state, no AST version field.** Untouched from M3c.

## 1. New and grown types

**`Pawl.Type.Modification`** grows the first layer-3 constructor, classified
`Layer.Text` by `Projection.layer`:

```haskell
data Modification
  = GainKeyword Keyword                      -- layer 6 (M3b)
  | LoseAllAbilities                         -- layer 6 (M3b)
  | SetBasePowerToughness Quantity Quantity  -- layer 7b (M3b/M3c)
  | ModifyPowerToughness Quantity Quantity   -- layer 7c (M3b)
  | SetLandSubtype Subtype                    -- layer 4 (M3c)
  | AddLandSubtype Subtype                    -- layer 4 (M3c)
  | AddCardType CardType                      -- layer 4 (M3c)
  | ChangeSubtypeWord Subtype Subtype         -- NEW layer 3 (Magical Hack: from -> to)
  deriving (Eq, Ord, Show)
```

`ChangeSubtypeWord from to` is CR 612's "replace all instances of one basic land
type with another." Its `from`/`to` are basic land types (CR 305.6 list); the
two are a player choice (§3), stored as `Literal`-style `Subtype` values in the
modification (the binding is baked in, exactly as M3b bakes a `ModifyTarget`'s
`Modification`).

**`Pawl.Type.Effect`** grows the text-changing opcode:

```haskell
data Effect
  = DealDamage SlotName Quantity                 -- M3a
  | ModifyTarget Duration Modification SlotName  -- M3b/M3c
  | ChangeText SlotName                          -- NEW (Magical Hack)
  deriving (Eq, Ord, Show)
```

`ChangeText slot` stores a `ChangeSubtypeWord`-carrying continuous effect
(`Indefinite`) on the object filling `slot`, with the chosen `from`/`to` read
from the cast-time binding (§3). It is the third `Effect` opcode; `Resolve` gains
one arm and `slotsOf` one case (its charter, the D4 dataflow lint). The Phase-4
fixture spell reuses `ModifyTarget UntilEndOfTurn (SetLandSubtype Swamp)` — no
new one-shot vocabulary.

**`Pawl.Type.Duration`** grows `Indefinite`:

```haskell
data Duration
  = UntilEndOfTurn  -- CR 514.2 (M3b)
  | Indefinite      -- NEW: "lasts indefinitely" (Magical Hack); cleanup never drops it
  deriving (Eq, Ord, Show)
```

`dropEndOfTurnEffects` (CR 514.2) already keeps everything that is not
`UntilEndOfTurn`, so `Indefinite` needs no cleanup change. A stored `Indefinite`
effect is locked to `TheseObjects {targetId}` (CR 611.2c); when the target leaves
its zone (CR 400.7) the id names nothing and the effect goes inert — it is never
folded again. It is not garbage-collected from `continuousEffects` (a hygiene
expiry, §8; correctness is unaffected because it is inert).

**`Pawl.Type.TargetSpec`** grows two specs:

```haskell
data TargetSpec
  = AnyTarget               -- M3a
  | CreatureTarget          -- M3b
  | SpellOrPermanentTarget  -- NEW (Magical Hack): a spell on the stack or a permanent
  | LandTarget              -- NEW (the Phase-4 fixture): a land permanent
  deriving (Eq, Ord, Show)
```

`SpellOrPermanentTarget` is CR 115's spell-or-permanent target: any object on the
stack, plus any permanent on the battlefield. `LandTarget` is a land on the
battlefield (projected card-type `Land`), used only by the fixture.

**`Pawl.Type.Recipient`** grows `ToObject`:

```haskell
data Recipient
  = ToCreature ObjectId  -- M1b/M3a
  | ToPlayer PlayerId    -- M1b/M3a
  | ToObject ObjectId    -- NEW: a spell or non-creature permanent named by a text-change
  deriving (Eq, Ord, Show)
```

`ToObject` names any object generically (a stack spell, an enchantment like Blood
Moon, a land). `ToCreature` stays distinct because `ModifyTarget` is
creature-only (M3b) and its arm pattern-matches `ToCreature`; a text-change does
not care about creature-ness, so it uses `ToObject`. (A future unification of the
object-naming recipients is possible but not forced here.)

**`Pawl.Type.Object`** grows a cast-time value-binding store (§3): the chosen
`from`/`to` basic land types, keyed by the same `SlotName` the `ChangeText`
effect references. Shape (a `Map SlotName (Subtype, Subtype)`, or a small
`Binding` record) is a plan detail; the invariant is that it is bound at cast
alongside `Object.targets` and read purely at resolution.

**`Pawl.Type.Prompt`** grows the value choice:

```haskell
ChooseBasicLandTypes :: Decider -> PlayerId -> ObjectId -> SlotName -> Prompt (Subtype, Subtype)
```

CR 601.2b-adjacent (a choice made while casting): pick the `from` and `to` basic
land types for the named slot. Always offered when a `ChangeText` slot exists —
the legal set is the five basic land types, never empty, so unlike targeting it
never gates castability. (See §3 for why cast-time is correct-by-elision.)

**`Pawl.Card`** gains one printing: **Magical Hack** — `manaCost {U}`, type line
Instant, one `ChangeText` effect referencing one slot, that slot's `targetSpecs`
entry `SpellOrPermanentTarget`, empty `staticAbilities`. Plus the **fixture**
land-changer for Phase 4 (a labeled synthetic instant; §7, §8). Blood Moon and
Urborg are unchanged from M3c.

## 2. The projection: text-changing at three read-points

`Pawl.Projection` keeps its `gather → order-within-layer → fold` shape and stays
the sole `case`-on-`Modification` home. `ChangeSubtypeWord` classifies to
`Layer.Text` (`layer`), which is *below* `Layer.Type` in the derived `Ord`, so it
folds before layer 4 automatically.

### 2.1 Read-point 1 — the type line (within the fold)

`applyModification` gains one arm — the only new `case`-on-`Modification` for the
fold:

```haskell
ChangeSubtypeWord from to ->
  if Set.member from (PC.subtypes pc)
    then pc {PC.subtypes = Set.insert to (Set.delete from (PC.subtypes pc))}
    else pc
```

A basic Mountain: `baseCharacteristics` seeds `{Mountain}`; layer 3 rewrites to
`{Island}`; layer 4 (no effect on it) leaves `{Island}`; the mana call site reads
`subtypesOf` → `{Island}` → `{U}`. CR 612.2 ("used as a land type") is satisfied
by construction — this touches only the `subtypes` set, never a name.

### 2.2 Read-point 2 — a source's abilities (gather-time)

This is the go/no-go (Phase 3). When `gather` collects a battlefield permanent
P's static abilities, it first rewrites each ability's `Modification` by the
layer-3 text-changes that affect P — *before* those abilities are folded onto
other objects at layer 4. Precedent: M3c's `staticAbilitiesLive` is already a
`gather`-time read of a layer-4 effect against the source; this is the layer-3
analog.

- `Projection.textChangesAffecting :: ObjectId -> GameState -> [(Subtype, Subtype)]`
  collects every `ChangeSubtypeWord` continuous effect whose `Affected` set
  contains the object (stored resolution effects — the same `affects` /
  `TheseObjects` membership M3c uses; these are not subject to source-liveness,
  which strips static abilities, not stored effects).
  Casing on `Modification` to detect `ChangeSubtypeWord` and pull its fields is
  Projection's charter, exactly like `setLandSubtypeEffects`.
- `Projection.rewriteModification :: [(Subtype, Subtype)] -> Modification -> Modification`
  applies those pairs to a `Modification` — for `SetLandSubtype s` / `AddLandSubtype
  s`, replace `s` with its image if a pair matches; every other constructor is
  untouched (CR 612.3: granted/other modifications carry no rewritable printed
  land-type word here). This is the tree-transform on ability data.
- `gather`'s `fromPermanent` maps `rewriteModification (textChangesAffecting permId
  gs)` over `permId`'s static-ability modifications before building each
  `Gathered`.

Blood Moon hacked `Mountain`→`Island`: `gather` contributes
`AllNonbasicLands (SetLandSubtype Island)`; folding it at layer 4 sets every
nonbasic land's subtype to `{Island}` → `{U}`. **Order-independence:** the hack is
a `TheseObjects {bloodMoonId}` effect; whether Blood Moon or the hack is older
does not change that Blood Moon's *gathered* modification is rewritten before any
per-target fold, so both timestamp orders give `{Island}`. (The within-layer-4
timestamp of Blood Moon's now-`Island` effect versus other layer-4 effects is
unchanged; there is no *layer-3* competitor.)

**Interaction with M3c source-liveness.** `staticAbilitiesLive` /
`setLandSubtypeEffects` only need to know *which lands* a `SetLandSubtype` strips,
evaluated against **base** (printed) characteristics ("nonbasic" is a printed
supertype). Hacking `Mountain`→`Island` changes the *product* subtype, not which
lands are nonbasic, so liveness is unaffected: the strip still hits the same
nonbasic lands; they become Islands rather than Mountains. Source-liveness reads
the *un-rewritten* `SetLandSubtype` predicate (it cares only that it *is* a set
and whom it targets) — no ordering hazard between the layer-3 rewrite and the
layer-4 liveness read. This is stated so the plan does not silently entangle the
two `gather`-time reads.

### 2.3 Read-point 3 — a spell's one-shot effects (resolve-time)

Handled in `Resolve` (§5), because rewriting an `Effect` cases on `Effect` —
`Resolve`'s charter, not `Projection`'s. `Resolve` delegates the *inner*
`Modification` rewrite (a `ModifyTarget`'s `Modification`) back to
`Projection.rewriteModification`. Neither module reaches into the other's
constructor space (§6).

## 3. Binding: the value choice (D4, minimal form)

Magical Hack has the player choose the two basic land types — the first player
choice in the DSL that is neither a target (object) nor mana. This is the D4
"binding" question (`docs/design.md` §2.11/D4, risk register): a first-order
effect referencing a prior choice. The answer is **named binding slots**, not
lambdas.

**Mechanism.** The `ChangeText slot` effect references a `SlotName`. At cast,
`Cast.castSpell` — already monadic and already prompting `ChooseTargets` — also
prompts `ChooseBasicLandTypes` for each `ChangeText` slot and stores the
`(from, to)` pair on the new stack `Object`, keyed by that slot, alongside
`Object.targets`. At resolution, `Resolve` reads the pair purely and constructs
the stored `ChangeSubtypeWord from to` continuous effect on the target.

**Why cast-time is correct here (the elision).** CR timing for Magical Hack's
land-type choice (cast-time vs. as-it-resolves) is to be re-checked against
`rules.txt` when tests are written (the "never trust recalled rules" discipline).
Either way, **the choice is indistinguishable between the two moments**: the legal
set is always the five basic land types (CR 305.6), independent of any game state
that can change between cast and resolution, and no card in the M3d pool can
alter it in that window. Per the engine-makes-no-choices invariant, eliding an
indistinguishable distinction is legitimate *with a documented expiry* (memory:
engine-makes-no-choices). Binding at cast keeps `Resolve` pure (a
`GameState -> GameState` function); the first card whose analogous choice is
genuinely resolution-time-*and*-distinguishable forces a resolution-time prompt
and hence a monadic `Resolve` — that is the named expiry (§8), not M3d's to pay.

This is the closed-half-shaped half of D4 landing concretely: targets were
already slot-bound (`Map SlotName Recipient`); values now bind the same way
(`Map SlotName (Subtype, Subtype)`), keeping binding uniform and first-order.

## 4. Targeting and reaching the stack

`Pawl.Target.legalRecipients` grows the two new specs; the *logic* is a straight
extension of the existing battlefield-creature/player enumeration:

- `SpellOrPermanentTarget` → `ToObject` for every object on the stack
  (`GameState.stack`) plus every permanent on the battlefield. This is the first
  target that reaches the **stack** — M3a/M3b/M3c only ever targeted battlefield
  creatures and players.
- `LandTarget` → `ToObject` for every battlefield object whose projected
  card-types include `Land` (`Projection.cardTypesOf`), so an animated /
  type-changed land is included consistently.

`stillLegal` (CR 608.2b) re-judges through the same sets: a spell that has left
the stack (resolved or countered) or a permanent that has left the battlefield is
illegal, and its id names nothing (CR 400.7). `Cast.targetable` (CR 601.2c) holds
for Magical Hack — the stack or battlefield generally has an object, though an
empty board is a genuine "uncastable" case the property suite may exercise.

**No new projection over stack objects is required for characteristics.**
`project`/`projectFrom` already accept any `ObjectId` and read
`Game.cardOf`/`baseCharacteristics`, which work for stack objects. `gather` and
`projectAll` iterate the battlefield only (their consumers — SBA/combat/mana —
are battlefield questions); the stack's text-change is observed through
`Resolve.effectsOf` at resolution (§5), not through a battlefield sweep.

## 5. The resolver reads projected effects

`Pawl.Resolve` gains read-point 3 and the `ChangeText` executor. It stays the
sole `case`-on-`Effect` home.

- **`Resolve.rewriteEffect :: [(Subtype, Subtype)] -> Effect -> Effect`** — cases
  on `Effect`; for `ModifyTarget dur mod slot` it returns `ModifyTarget dur
  (Projection.rewriteModification pairs mod) slot`; `DealDamage` and `ChangeText`
  carry no rewritable land-type word and pass through. This is the one-shot
  analog of §2.2's ability rewrite, and it *delegates* the `Modification` case to
  Projection — the invariant-preserving split (§6).
- **`Resolve.effectsOf :: ObjectId -> GameState -> [Effect]`** — the resolving
  spell's *projected* effects: `map (rewriteEffect (Projection.textChangesAffecting
  oid gs)) (Card.effects card)`. `resolveSpell` folds `applyEffect` over
  `effectsOf oid gs` instead of `Card.effects card`. Nothing else changes — the
  fizzle (CR 608.2b), the per-slot legality, and the bury (CR 608.2n) are
  untouched.
- **`ChangeText` arm.** On a legal `ToObject` target, mint a fresh timestamp
  (`Game.freshTimestamp`) and store `MkContinuousEffect { source, timestamp,
  duration = Indefinite, modification = ChangeSubtypeWord from to, affected =
  TheseObjects (Set.singleton target) }`, with `(from, to)` read from the
  cast-time binding for the slot. An illegal slot or a missing binding is a no-op
  (the `ModifyTarget` posture). Targeting a *spell* on the stack is legal at cast
  but, when Magical Hack resolves, that spell is usually still on the stack (a
  later object resolves first) — the stored effect names the spell's id, and if
  that spell later resolves into a permanent (CR 400.7 new object) the effect goes
  inert (the Phase-4 negative test).

Worked Phase-4 positive case: the fixture spell (`ModifyTarget UntilEndOfTurn
(SetLandSubtype Swamp) landSlot`) is on the stack; Magical Hack resolves first,
storing `ChangeSubtypeWord Swamp Mountain` on the fixture's id; the fixture then
resolves, and `Resolve.effectsOf` returns `ModifyTarget UntilEndOfTurn
(SetLandSubtype Mountain) landSlot` — the target land becomes a Mountain (`{R}`).
The AST was rewritten and the resolver honored it: the canary, on the stack.

## 6. Invariants preserved

- **The two-halves invariant scales to text-changing — the key design win.** The
  rewrite is split across the two sanctioned modules *by which type it
  destructures*: `Projection.rewriteModification` cases on `Modification` (its
  charter, alongside `layer`/`applyModification`/`setLandSubtypeEffects`);
  `Resolve.rewriteEffect` cases on `Effect` (its charter, alongside
  `slotsOf`/`applyEffect`) and delegates the inner `Modification` to Projection.
  Projection never touches an `Effect` constructor; Resolve never touches a
  `Modification` constructor. No rules-core module (`Combat`, `Damage`, `Sba`,
  `Engine`, `Cast`, `Target`, `Mana`) cases on either — they read `subtypesOf` /
  `cardTypesOf` / `effectsOf` / `layer`.
- **The engine makes no choice it should not, and elides none it should.** The
  land-type pair is prompted (`ChooseBasicLandTypes`); the target is prompted
  (`ChooseTargets`). The only elision is cast-vs-resolution timing, justified by
  indistinguishability with a named expiry (§3, §8).
- **`NamedFieldPuns`** per the M3b amendment; no new convention change. One type
  per module; new constructors take no `Mk`-pun (they are sum-type data
  constructors, not the newtype/record form the naming rule governs).

## 7. Setup, decks, and testing

Testing follows the phased spine (§0): understood machinery first, the go/no-go
test written failing before its resolver, the stack rewrite last.

**Phase 1 — layer-3 machinery + type-line, directly-built effects (passing).**

- `ChangeSubtypeWord Forest Island` on a Forest (via `withEffect` /
  `TheseObjects`, M3c's technique): projects subtypes `{Island}`, taps `{U}`;
  `ChangeSubtypeWord Forest Island` on a Swamp is a no-op (from absent). Layer
  ordering: a `ChangeSubtypeWord` and an `AddLandSubtype` on the same land show
  the text-change folding before the layer-4 add.

**Phase 2 — Magical Hack castable, end-to-end (the stated falsifier).**

- Cast Magical Hack (`{U}`) choosing `Mountain`→`Island`, targeting a basic
  Mountain: after resolution the land taps `{U}`. Exercises the binding
  (`ChooseBasicLandTypes`), `SpellOrPermanentTarget`/`ToObject`, the `ChangeText`
  opcode, `Indefinite`, and instant-speed casting (CR 117.1a, M3a).
- CR 608.2b: Magical Hack whose target left the battlefield before resolution
  fizzles (buried, no effect).

**Phase 3 — the ability-AST rewrite (the go/no-go). Written failing, then landed.**

- Blood Moon on the battlefield + a nonbasic land: hack Blood Moon
  `Mountain`→`Island`; the nonbasic land projects `{Island}`, taps `{U}`. Fails
  before §2.2 (reads Blood Moon's printed Mountain → `{R}`).
- Both timestamp orders (Blood Moon older / the hack older): same `{U}` outcome
  (order-independence, §2.2).
- Urborg variant (optional, cheap): hack Urborg `Swamp`→`Mountain`; every land
  gains Mountain (adds `{R}`) rather than Swamp. Confirms the rewrite hits
  `AddLandSubtype` as well as `SetLandSubtype`.
- Ported target list where Argentum has one; assert corrected outcomes. Test
  names cite CR 612.1/612.2 + 305.6/305.7.

**Phase 4 — the positive spell-on-the-stack rewrite (highest integration).**

- The **fixture** instant "target land becomes a Swamp"
  (`ModifyTarget UntilEndOfTurn (SetLandSubtype Swamp) landSlot`, `LandTarget`) —
  a **labeled synthetic crutch** (memory: tests-prefer-real-cards), commented
  with its expiry (§8). On the stack, hacked `Swamp`→`Mountain` by a Magical Hack
  that resolves first: the fixture then resolves making the target land a
  Mountain (`{R}`). Demonstrates `Resolve.effectsOf` reading the projected AST.
- **CR 400.7 negative test.** Magical Hack targets a *permanent spell* on the
  stack (e.g. a Goblin Piker) and would change a land-type word; when the spell
  resolves into a permanent (a new object), the stored text-change (fixed to the
  spell's id) no longer names it — the permanent enters un-hacked. A real
  correctness test of 400.7 identity, distinct from the positive case.

**Setup.** `emptyGame` unchanged (M3b/M3c added the counters). Fixtures place
enchantments/lands on the battlefield and cast Magical Hack / the fixture through
the stack, assigning `Object.timestamp` from `nextTimestamp` as M3c does.

**Decks / random-game coverage.** Magical Hack is blue; there is no blue matchup,
so it and the fixture are **deterministic fixtures**, never in a random game —
the M3b/M3c white-fixture posture. The fuller tail (a blue matchup for random
text-changing coverage) is deferred past M3 (§8). Blood Moon may still ride
`redDeck` for single-effect layer-4 random coverage (M3c), unchanged; M3d adds no
random-game entrant.

**Properties** (`runMatch`, both matchups): every M2d/M3a/M3b/M3c invariant as it
stands — conservation, termination, ids, no floating mana, life never increases,
combat happens, green-black engagement. Replay determinism now covers the
projected type line, the projected effects, and the cast-time land-type binding.
The benchmark stays on `redDeck`; throughput is watched for the added
`textChangesAffecting` per-projection cost, not asserted.

## 8. What M3d preserves, and the expiries it opens

**Preserves:** the two invariants (§6), the numeric/mana/timestamp models, the
M3c projection shape and source-liveness, the deterministic-fixture posture for
off-color cards.

**Expiries this milestone opens:**

- **Resolution-time choice / monadic `Resolve`.** The land-type binding is
  cast-time by indistinguishability (§3). The first card whose choice is
  genuinely made as it resolves *and* whose legal set can differ from the
  cast-time set forces a resolution-time prompt and a monadic `Resolve`. Tracked
  from here; a documenting test records the elision.
- **The Phase-4 fixture spell** ("target land becomes a Swamp") is a labeled
  synthetic crutch. It expires when a real non-Aura land-type-setting spell lands
  — which needs M4 `Attach` (Auras: Spreading Seas, Sea's Claim) or M4 `Destroy`
  (Boil, Flashfires, Tsunami — hack the type, destroy a different type). The
  positive stack rewrite is then re-tested on a real card.
- **Landwalk as a text-change target.** Magical Hack's own Oracle example
  (swampwalk→plainswalk) is a rules-text change on a landwalk keyword; landwalk is
  unimplemented (a `docs/design.md` M2 punchlist keyword). When landwalk lands,
  its rewrite-under-text-change becomes a test — the same `rewriteModification`
  seam extended to a keyword-with-a-land-type-parameter.
- **Text-changing beyond basic land types.** Color words (Sleight of Mind, layer
  3 with a `Color` producer at layer 5's consumer), creature types (Artificial
  Evolution), names/text-box swaps (CR 612.5–612.9). Each is a new producer /
  read-point off the same `ChangeText` shape.
- **Inert stored `Indefinite` effects are not garbage-collected.** A text-change
  whose target has left its zone lingers in `continuousEffects` (inert;
  correctness unaffected). GC of dead fixed-set effects is a future hygiene pass,
  first pressured when long games accumulate them.
- **`ToObject` vs. `ToCreature` recipient unification.** Kept distinct because
  `ModifyTarget` is creature-only; a future pass may unify object-naming
  recipients once more opcodes target arbitrary objects.
- **Stack objects are not swept by `projectAll`.** Only `Resolve.effectsOf`
  observes a stack spell's text-change. A future need to read a stack object's
  *projected characteristics* in a sweep (e.g. a static ability that reads spells
  on the stack) extends `gather`/`projectAll` to the stack.

**Explicitly deferred past M3d:**

- **The M3 remaining letters** — M3e (activated abilities, CR 605 classification),
  M3f (the 603/614 event pipeline, Rest in Peace), M3g (Decider + re-entrancy,
  Mindslaver + Panglacial Wurm).
- **A blue (or white) matchup** for random continuous-effect / text-changing
  coverage — the post-M3 tail.
- **X, modes, counterspells, Auras/Equipment, new card types, serialization / AST
  version field.**
