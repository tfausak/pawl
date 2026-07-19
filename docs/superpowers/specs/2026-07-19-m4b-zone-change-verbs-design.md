# M4b zone-change verbs — design

Design for milestone **M4b**, the second letter of M4 (see the split table in
`docs/design.md` §3): **the targeted zone-change opcode family**, plus the one
verb that is not a plain move — **Destroy**. M4b's job is to prove that M3f's
`Event.changeZone` funnel — built for Rest in Peace's single graveyard→exile
*redirect* — generalizes into a *targeted opcode family* that any card can drive
to any destination, and to establish that **destroy ≠ move-to-graveyard**.

The funnel already exists and is already general: `Event.changeZone :: ObjectId
-> Zone -> GameState -> GameState` consults active replacements, moves the object
minting a fresh incarnation (CR 400.7), and emits the resolved event for
triggers. It is the *sole* mover in the engine today — used by the SBA bury, the
spell-resolution fizzle, the draw step (`Engine.drawFor`), Search, and
`ExileAllGraveyards`. What is missing is an **opcode** that lets card data name a
target and a destination. M4b adds that opcode family; it builds almost no new
mechanism, which is exactly why it can land breadth (§4 of `design.md`).

The gate card and its falsifying foil, Scryfall-verified
(`api.scryfall.com/cards/named`, fetched 2026-07-19):

| Card | Cost | Type line | P/T | Oracle text |
|---|---|---|---|---|
| **Murder** | `{1}{B}{B}` | Instant | — | "Destroy target creature." |
| **Darksteel Myr** | `{3}` | Artifact Creature — Myr | 0/1 | "Indestructible (Damage and effects that say 'destroy' don't destroy this creature. If its toughness is 0 or less, it still dies.)" |

Murder **falsifies its own naive implementation** on the one axis this letter
owns: an engine that modelled "destroy target creature" as `MoveToZone slot
Graveyard` — routing the target straight through the funnel to its owner's
graveyard — does the wrong thing against Darksteel Myr. Destroy must be its
**own** opcode that consults indestructibility (CR 700.4) *before* moving, and
does nothing when the target can't be destroyed. Darksteel Myr's own reminder
text states the exact SBA scope M4b implements: "destroy" doesn't destroy it, and
combat/deathtouch damage doesn't either — **but toughness 0 or less still kills
it** (it is *put into the graveyard*, CR 704.5f, not "destroyed"). That is the
falsifier for the second reader, too: an indestructible guard that blanketed all
of 704.5 would wrongly keep a 0-toughness Darksteel Myr alive.

The generalization proof rides alongside: **Unsummon** (`{U}` "Return target
creature to its owner's hand") and **Angelic Edict** (`{4}{W}` "Exile target
creature or enchantment") drive the same one targeted-move opcode to two *non*-
graveyard destinations — the concrete demonstration that the funnel generalizes
past RiP's single redirect.

This is a types-and-architecture spec, not an implementation plan.

## 0. The core idea and the ordered spine

M4b is the first **open-half breadth** milestone: it adds five opcodes, one
keyword, and a handful of real cards, and it adds essentially no new *mechanism*
— every mover funnels through the `changeZone` that M3f already built. The one
genuinely new rules fact is **indestructibility** (CR 700.4 / 702.12), which has
two independent readers.

**The ordered spine** (gate first, then additive riders — the plan owns the exact
commit decomposition):

1. **Destroy + Indestructible (the gate).** `Effect.Destroy`,
   `Keyword.Indestructible`, the indestructible guard in the Destroy opcode *and*
   in `Sba.creatureDies` (704.5g/704.5h, not 704.5f). Cards: Murder, Darksteel
   Myr. This is the whole architectural claim of the letter; it lands first.
2. **The targeted move (the generalization proof).** `Effect.MoveToZone SlotName
   Zone` and `TargetSpec.CreatureOrEnchantmentTarget`. Cards: Unsummon (→ Hand),
   Angelic Edict (→ Exile).
3. **The player-zone verbs (breadth).** `Effect.Draw`, `Effect.Mill`,
   `Effect.Discard`, the single-card-draw consolidation, and `Prompt.ChooseDiscard`.
   Cards: Divination, Tome Scour, Mind Rot.

Steps 2 and 3 are additive over step 1 and over each other; they are sequenced by
descending architectural weight (a new keyword-with-SBA, then a new target spec,
then three self-contained verbs), not by dependency.

## Goal and scope

**Exit criterion.** Deterministic tests demonstrate all of:

- **Destroy ≠ move-to-graveyard (the gate, CR 700.4/701.7).** Murder resolves and
  a normal target creature is in its owner's graveyard. Murder resolves against
  **Darksteel Myr** and the game is unchanged — the creature stays on the
  battlefield (the falsifier in a comment: a `MoveToZone slot Graveyard` model
  would have buried it).
- **Indestructible survives combat and deathtouch (CR 704.5g/704.5h).** Darksteel
  Myr (0/1) blocks a 2/2 and survives the lethal combat damage; a deathtouch
  source deals it 1 and it survives. **But** a Darksteel Myr whose toughness is
  reduced to 0 or less (a −1/−1 style effect, or in the test a synthetic
  toughness set) *is* put into the graveyard (CR 704.5f is not guarded).
- **The funnel generalizes to non-graveyard destinations (CR 400.7).** Unsummon
  returns a target creature to its owner's hand (a fresh library/hand-zone object;
  the battlefield incarnation is gone). Angelic Edict exiles a target creature
  *and* — exercising the broadened spec — a target enchantment (Rest in Peace or
  Humility on the board), each landing in the exile zone.
- **Draw, mill, discard (CR 120 / 701.13 / 701.8).** Divination draws its
  controller two cards (hand grows by two, library shrinks by two). Tome Scour
  mills a target player five (top five of that player's library → their
  graveyard). Mind Rot makes a target player discard two, the *discarding* player
  choosing which (CR 701.8a), the chosen two moving to their graveyard.
- **Draw-from-empty is still a loss; mill/discard-from-empty are not.** A Draw that
  exhausts the library marks the CR 121.3 empty-draw and the CR 704.5b loss (the
  existing `drewFromEmpty` path, now shared). A Mill or Discard that outruns the
  zone simply moves fewer (CR 701.13b / 701.8b) with no penalty.

The `DecisionLog` replays deterministically with the new `ChooseDiscard` response
alongside the existing choices.

**Non-goals.**

- **No derived references ("its controller", "its power").** Path to Exile ("its
  controller may search…") and Swords to Plowshares ("its controller gains life
  equal to its power") are exile cards, but both hinge on reading a player or a
  characteristic *off the exiled object* — a derived-reference binding M4b does
  not build (the wall mtg-pure hit). Angelic Edict is the clean targeted-exile
  card precisely because it has no such rider. Deferred with a named expiry (§8).
- **No lifegain / lifeloss opcode.** Swords' "gains life", drain effects, and the
  §4 volume verb "life gain/loss" are a separate open-half verb, not a zone
  change. Deferred to the volume tail.
- **Destroy is not yet an interceptable "destroy event."** M4b's Destroy is
  `check-indestructible-then-changeZone`. Regeneration and prevention (CR 615)
  replace the *destroy* event with "tap, remove from combat, heal" — that is
  M4d's "cancel replacement shape / regeneration's one-shot shield". Destroy
  becoming an emitted, replaceable event is a named expiry (§8), owned by M4d.
- **No "at random" or "reveal" discard.** CR 701.8's default is the discarding
  player's free choice (Mind Rot); Mind Sludge / Hymn-style random or revealed
  discard is a later variant, deferred (§8).
- **No targeted mill/exile via a card characteristic.** Mill and Discard target a
  *player* (a `PlayerTarget` slot). "Exile all creatures with power ≥ 3" and
  friends are set-selection predicates, open-half volume, deferred.
- **No tokens, counters, prevention, counterspells, or modes.** Those are M4c–M4g.
  No zone-change verb here needs them.
- **No new numeric-tower variants.** Draw/Mill/Discard reuse `Quantity`; every
  M4b quantity is a `Literal`. `Quantity.X` in a draw/mill/discard count (Braingeyser,
  Stroke of Genius) rides the M4a `ChooseX` mechanism unchanged, but no M4b card
  exercises it — the executor arms are X-aware (per the M4a `readsX` contract) but
  untested here.

## 1. New and grown types

**`Pawl.Type.Effect`** grows five constructors. `Resolve` remains the sole module
that may `case` on `Effect`; each new arm is added to `slotsOf`, `readsX`,
`manaProduced`, `searchesLibrary`, and `rewriteEffect` (§3, §5).

```haskell
  | -- CR 701.7: destroy the slot's target permanent -- move it to its owner's
    -- graveyard, UNLESS it is indestructible (CR 700.4). NOT a MoveToZone slot
    -- Graveyard: the indestructible check is the whole reason this is its own
    -- opcode (Murder vs Darksteel Myr). A future destroy EVENT (regeneration,
    -- CR 615) is M4d.
    Destroy SlotName
  | -- CR 400.7: move the slot's target object to a zone. Bounce = MoveToZone slot
    -- Hand (owner-relative, changeZone carries Object.owner); targeted exile =
    -- MoveToZone slot Exile. One opcode for every targeted single-object move;
    -- the destination is data. (Graveyard as a destination here would be an
    -- unconditional move, distinct from Destroy -- no M4b card needs it.)
    MoveToZone SlotName Zone
  | -- CR 120: the controller draws this many cards. Targetless (a spell's
    -- controller draws, CR 120.2). Empty-library draw is a loss (CR 121.3 ->
    -- 704.5b), unlike Mill: that is why Draw and Mill are separate opcodes.
    Draw Quantity
  | -- CR 701.13: the slot's target player mills this many (top N of their library
    -- to their graveyard). Milling an empty/short library mills fewer with no
    -- penalty (CR 701.13b) -- the semantic that forbids sharing an opcode with Draw.
    Mill SlotName Quantity
  | -- CR 701.8: the slot's target player discards this many. The DISCARDING player
    -- chooses which (CR 701.8a) via Prompt.ChooseDiscard, routed through
    -- Decide.deciderFor. Discarding from a smaller hand discards all of it (CR
    -- 701.8b).
    Discard SlotName Quantity
```

**`Pawl.Type.Keyword`** grows **`Indestructible`** (CR 702.12), inserted by rule
number between `Haste` (702.10) and `Reach` (702.17) — the type stays diffable
against rule 702. A keyword is a rulebook citation, so reading it in the closed
half is legitimate (the M2a rule); it is read only through the projection
(`Projection.hasKeyword` / `PC.keywords`), never `Card.keywords`, so Humility
(layer 6) strips it for free.

**`Pawl.Type.TargetSpec`** grows **`CreatureOrEnchantmentTarget`** (Angelic Edict,
CR 115): a permanent whose *projected* card types include Creature or Enchantment.
The first spec that admits a non-creature permanent as a target — exercised by
exiling an enchantment (Rest in Peace / Humility exist on the board at M4b).

**`Pawl.Type.Prompt`** grows the discard choice:

```haskell
  -- CR 701.8a: the discarding player chooses which cards leave their hand. The
  -- PlayerId is the discarding (target) player; the [ObjectId] is their current
  -- hand; the Natural is how many to choose (already clamped to hand size, CR
  -- 701.8b). Returns the chosen cards. Asked only when a discard actually
  -- happens with a non-empty hand and a real choice (choosing all of a hand <=
  -- count is forced -- where the rules leave nothing to choose, don't prompt).
  ChooseDiscard :: Decider -> PlayerId -> [ObjectId] -> Natural -> Prompt [ObjectId]
```

**`Pawl.Type.Response`** grows `ChoseDiscard [ObjectId]` for replay round-trip,
mirroring `Searched` / `ChoseX`.

No new `Zone` (all six destinations exist), no new `Recipient` (`ToCreature`,
`ToObject`, `ToPlayer` cover every M4b target), no new `ZoneChange` field.

## 2. Destroy and Indestructible (the gate)

**Two independent readers of one keyword.** CR 700.4 is precise about what
indestructible stops, and the two consumers are unrelated code paths:

1. **The Destroy opcode (CR 701.7 / 700.4).** `applyEffect`'s `Destroy` arm looks
   up the slot's chosen target and its CR 608.2b legality (the same pattern as
   `DealDamage`), resolves the target object, and asks `Projection.hasKeyword
   Keyword.Indestructible target gs`. If indestructible, the effect is a **no-op**
   (CR 700.4: rules and effects can't destroy it). Otherwise `Event.changeZone
   target Graveyard` — the object moves to *its owner's* graveyard (changeZone
   carries `Object.owner`), funnelling through any active replacement (so a Rest
   in Peace on the board would redirect the destroyed creature to exile — the
   funnel composing with M3f for free).

2. **The lethal-damage / deathtouch SBAs (CR 704.5g/704.5h).** `Sba.creatureDies`
   gains an indestructible guard read the same way it reads `cardTypes` and
   `toughness` — off the already-projected `ProjectedCharacteristics` the SBA
   sweep computes once (`Set.member Keyword.Indestructible (PC.keywords pc)`), no
   re-projection. The guard covers **only** the two "destroy" branches:

   ```
   creatureDies = isCreature && case toughness of
     ...
     Just t -> (t <= 0)                              -- 704.5f: NOT guarded (put into gy, not destroyed)
            || (not indestructible && damageLethal)  -- 704.5g: guarded
            || (not indestructible && woundedByDeathtouch)  -- 704.5h: guarded
   ```

   704.5f (toughness ≤ 0) is deliberately outside the guard: Darksteel Myr's own
   reminder text — "If its toughness is 0 or less, it still dies" — is CR 704.5f,
   and it is not a destruction. This is the second falsifier: a blanket guard
   keeps a 0-toughness indestructible creature alive, which is wrong.

**Why Destroy is its own opcode, restated for the record.** `MoveToZone slot
Graveyard` would be an *unconditional* move; Destroy is a *conditional* one gated
on a keyword, and it is the CR 701.7 keyword action other cards (regeneration,
totem armor, indestructible) are defined to interact with. Collapsing it into
MoveToZone is precisely the fusion the gate exists to reject.

## 3. The targeted move and the player-zone verbs

**`MoveToZone SlotName Zone`** — the generalization proof. Its `applyEffect` arm
is the minimal one: look up the chosen recipient and its CR 608.2b legality,
resolve the target object, and `Event.changeZone target zone`. Bounce (`Hand`)
and targeted exile (`Exile`) differ only in the `Zone` literal in the card data.
Owner-relativity ("to its *owner's* hand") is already handled by the funnel;
nothing extra is needed. `CreatureOrEnchantmentTarget` gets a `Target.stillLegal`
arm (projected card types ∩ {Creature, Enchantment} ≠ ∅) shared by cast-time
legality and the CR 608.2b re-check.

**`Draw Quantity`** — the controller draws. The arm evaluates the quantity
(`Quantity.evaluate` against the spell object; a `Literal` for Divination) and
draws that many for `controller` (the resolving spell's controller, already
threaded into `applyEffect`). **Consolidation:** the single-card draw is today
duplicated — `Engine.drawFor` (the draw step) and `Setup.drawCard` (opening
hands) each move the top library card to hand and, for `drawFor`, mark
`drewFromEmpty` on an empty library. The `Draw` opcode is the third consumer, so
this milestone extracts one shared primitive — `drawCard :: PlayerId -> GameState
-> GameState` (move top → hand via `changeZone`, else set `drewFromEmpty`) — that
all three call. Draw N folds it N times; the empty-library loss mark (CR 121.3 →
704.5b) is preserved because the shared primitive owns it.

**`Mill SlotName Quantity`** — the target player mills. The arm resolves the
slot's `ToPlayer target`, evaluates the quantity, takes the top `min(n,
librarySize)` of that player's library (CR 701.13b), and `changeZone`s each to the
graveyard. No `drewFromEmpty` — milling an empty library is not a draw and not a
loss.

**`Discard SlotName Quantity`** — the target player discards, choosing. The arm
resolves `ToPlayer target`, evaluates the quantity, clamps to hand size (CR
701.8b), and — when a genuine choice remains (hand size > count) — prompts
`ChooseDiscard (deciderFor target) target hand n` for the chosen ids, validating
the response is an `n`-subset of the hand; when the whole hand is discarded (hand
≤ count) the choice is forced and is not prompted (the elision-for-
indistinguishable-choices rule). Chosen cards `changeZone` to the target's
graveyard.

## 4. The D4 dataflow lint and the classifications

Each new `Effect` constructor is wired into every classification `Resolve`
exposes; the compiler does not force the `Quantity`-comparing ones (per M4a's
`readsX` note), so the spec enumerates them:

- **`slotsOf`** — `Destroy slot`, `MoveToZone slot _`, `Mill slot _`, `Discard
  slot _` → `{slot}`; `Draw _` → `∅` (targetless). This keeps the D4 lint's
  reads-equal-declares contract honest: Murder/Unsummon/Angelic Edict/Tome
  Scour/Mind Rot each declare exactly their read slot; Divination declares none.
- **`readsX`** — `Draw q`, `Mill _ q`, `Discard _ q` → `q == Quantity.X`;
  `Destroy`, `MoveToZone` → `False`. All `False` for M4b's Literal-only cards, but
  the arms exist so the first `Draw X` card is lint-covered from day one.
- **`manaProduced`** → `Nothing` for all five (none is a mana ability).
- **`searchesLibrary`** → `False` for all five (none searches — the Panglacial
  re-entrancy classification is untouched).
- **`rewriteEffect`** (CR 612 layer-3 text change) → identity for all five: none
  carries a rewritable basic-land-type word (a `Zone` and a `Quantity` are not
  land-type words). MoveToZone's `Zone` is a fixed destination, not text.

## 5. Invariants preserved

- **The two-halves invariant holds.** No closed-half module cases on a card's or
  effect's *identity*. `Resolve` stays the sole home of `case effect of`; `Event`
  stays the sole home of `case` on `ReplacementEffect`/`TriggerCondition`. The new
  keyword `Indestructible` is cased on freely — it is rule 702.12, a citation, the
  same kind of read as `Phase` or `Deathtouch` (the M2a rule). The closed half
  *asks* "is this permanent indestructible?" through the projection; it never
  names Murder or Darksteel Myr.
- **Every mover is the one funnel.** Destroy, MoveToZone, Draw, Mill, and Discard
  all move objects exclusively through `Event.changeZone`, so replacement rewrites
  (CR 614) and enters/leaves triggers (CR 603) compose with each M4b verb for free
  — a Rest in Peace on the board redirects a Murdered or milled or discarded card
  to exile with no M4b code aware of it.
- **The engine makes the right choices and elides only indistinguishable ones.**
  Discard prompts the discarding player (CR 701.8a) via `deciderFor`, and only
  when a real choice remains; a forced full-hand discard is not prompted. Indestructible
  is never a choice. No other M4b verb adds a prompt.
- **Conventions.** One type per module (`Indestructible`, `CreatureOrEnchantment
  Target`, `ChooseDiscard`, `ChoseDiscard`, and the five `Effect` constructors are
  additions to existing types; no new module unless the plan splits the draw
  primitive out). Arbitrary-precision `Natural`/`Integer` (counts are `Natural`
  via `Quantity`). No fixed arity. `Text` not `String`. Derive `Eq`/`Show` (and
  `Ord` where the type is already a map key).

## 6. Setup, decks, and testing

Cards land as **deterministic fixtures** (the M3/M4a posture), each a real,
Scryfall-verified card exercised through the stack at its own speed. Murder and
Unsummon resolve at instant speed (CR 117.1a, M3a's `Cast`); the rest are sorcery
speed.

**Gate tests (step 1).**

- **Destroy a normal creature.** Cast Murder at a vanilla creature; after
  resolution it is in its owner's graveyard, off the battlefield. Cite CR 701.7.
- **Destroy an indestructible creature (the falsifier).** Cast Murder at Darksteel
  Myr; the game is unchanged, Myr on the battlefield, Murder in the graveyard
  (it still resolved and was buried, CR 608.2n; it simply did nothing). Comment:
  a `MoveToZone slot Graveyard` model buries Myr — the whole reason Destroy is a
  distinct opcode.
- **Indestructible survives combat (704.5g).** Darksteel Myr (0/1) blocks a 2/2;
  after combat damage and the SBA sweep, Myr survives with 2 damage marked.
- **Indestructible survives deathtouch (704.5h).** A deathtouch source deals Myr 1;
  the 704.5h SBA does not destroy it.
- **704.5f still kills it (the second falsifier).** Reduce Myr's toughness to 0
  (a −1/−1 effect, or a synthetic toughness-set fixture labeled with its expiry);
  the SBA puts it into the graveyard despite indestructible. Cite CR 704.5f and
  Darksteel Myr's reminder text.

**Move tests (step 2).**

- **Bounce.** Cast Unsummon at a creature; it leaves the battlefield and a fresh
  object is in its owner's hand (CR 400.7 — the battlefield incarnation is gone,
  no marked damage carried).
- **Targeted exile, creature and enchantment.** Cast Angelic Edict at a creature →
  exile; cast it at an enchantment on the board (Rest in Peace or Humility) →
  exile, exercising `CreatureOrEnchantmentTarget` admitting a non-creature.

**Player-zone tests (step 3).**

- **Draw.** Cast Divination; the controller's hand grows by two and library shrinks
  by two. A separate fixture drives Draw against a one-card library to assert the
  CR 121.3 empty mark and the CR 704.5b loss fire through the shared primitive.
- **Mill.** Cast Tome Scour at a player with ≥ 5 library cards → top five to their
  graveyard; a short-library fixture mills fewer with no loss (CR 701.13b).
- **Discard.** Cast Mind Rot at a player with > 2 hand cards → `ChooseDiscard`
  picks two, they land in the graveyard; a fixture with a 2-card hand discards
  both without a prompt (forced), asserting the elision.

**The D4 lint** (a `CardSpec` unit) runs over the whole pool: each M4b card's slot
reads equal its declared `targetSpecs`, Divination declares none, and no card
reads `X` without an `{X}` cost (all M4b Literals). The M3.5 round-trip
(`jsonToCard . cardToJson ≡ Right`) stays green over the grown pool — the five
opcodes, the keyword, the target spec, and the prompt/response are all born
serializable through `Pawl.Codec`.

**Random-game coverage.** The gate and riders land as deterministic fixtures;
following the M2d/M3 pattern, a black/blue random-game deck entry for the
highest-value verbs (Destroy, Draw, bounce) can follow M4b rather than gate it,
so the funnel-through-random-play coverage trails by one step as it has before.

**Properties** (`runMatch`, all matchups): every M2d/M3/M4a invariant as it
stands — conservation under CR 400.7 (each zone change mints one id and retires
one; the counts still balance), termination, id discipline, no floating mana at
end of step. The "life never increases" property is **unaffected** (M4b adds no
lifegain; Draw/Mill/Discard/Destroy/bounce touch no life total — only the
existing draw-from-empty loss path, already covered).

## 7. What M4b preserves

The two invariants (§5); the mana/timestamp/projection models; M3a's targeting and
CR 608.2b re-validation (Destroy and MoveToZone reuse the per-slot legality and
the fizzle); M3b/M3c/M3d's continuous-effect projection (indestructible reads
through it, so Humility strips it); M3e's activation; **M3f's event pipeline**,
which M4b is the first milestone to *stress at breadth* — five verbs riding one
funnel, each composing with replacements and triggers untouched; M3g's control and
re-entrancy; M4a's binding environment (Destroy/MoveToZone read `target` bindings;
Mill/Discard read `PlayerTarget` bindings) and X (the count arms are X-aware); and
the deterministic-fixture posture.

## 8. The expiries M4b opens

- **Destroy as an interceptable event (M4d).** Today Destroy is
  check-indestructible-then-`changeZone`. Regeneration and totem armor (CR 615 /
  702.x) replace the *destroy* event with a shield; prevention is the same cancel
  shape. When M4d builds the CR 615 replacement family, Destroy emits a replaceable
  destroy event and the indestructible check becomes one clause among replacements.
  Documented at the `Destroy` arm.
- **Derived references ("its controller", "its power").** Path to Exile and Swords
  to Plowshares — and every "sacrifice … then draw", "its controller loses life",
  "equal to its power" — need to read a player or characteristic *off a slot's
  object*. That is a new binding kind on M4a's environment (a `Binding` that names
  a derived value, not a chosen one). Due with the first card that needs it; it
  unlocks the two iconic exile spells M4b deferred.
- **Lifegain / lifeloss opcode.** The §4 volume verb "life gain/loss"
  (Swords, drains, lifelink's effect half) is a separate open-half verb. Due with
  its first card.
- **"At random" / "reveal" discard.** CR 701.8's non-default discard modes (Hymn
  to Tourach, Mind Sludge's flavor) need a different chooser or a reveal. Due with
  the first such card.
- **Unconditional move-to-graveyard.** `MoveToZone slot Graveyard` (a "put into
  graveyard" that is *not* a destroy, so indestructible does not stop it — e.g.
  "put target creature into its owner's graveyard") is representable already but
  has no M4b card. It lands with its first real card, distinct from Destroy.
- **X in a draw/mill/discard count.** Braingeyser / Stroke of Genius ("draw X")
  ride M4a's `ChooseX` and the X-aware count arms unchanged; untested at M4b
  because no card here carries `{X}`. Due with the first variable-count card.
- **Set-selection zone changes.** "Exile all creatures", "each player discards
  their hand", board wipes — predicates over sets rather than a single target.
  Open-half volume, deferred.
- **git-bug `65ce714`** (mana-source prompt) stays open, unchanged — no M4b verb
  touches payment.

**Explicitly deferred to M4c–M4g and the volume tail:** tokens (M4c), prevention
and regeneration (M4d, which also reopens Destroy), counter target spell (M4e),
counters (M4f), modal (M4g), lifegain, derived references, and set-selection
effects. No M4b zone-change verb needs any of them.
